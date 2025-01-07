/*
 * meli - imap module.
 *
 * Copyright 2019 Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */
use std::{
    sync::Arc,
    time::{Duration, Instant},
};

use imap_codec::imap_types::search::SearchKey;

use super::*;
use crate::{
    backends::SpecialUsageMailbox,
    error::Result,
    imap::{email::common_attributes, sync::cache::ignore_not_found},
};

/// Arguments for IMAP watching functions
pub struct ImapWatchKit {
    pub conn: ImapConnection,
    pub main_conn: Arc<ConnectionMutex>,
    pub uid_store: Arc<UIDStore>,
}

pub fn poll_with_examine(
    kit: ImapWatchKit,
) -> impl futures::stream::Stream<Item = Result<BackendEvent>> {
    try_fn_stream(|emitter| async move {
        log::trace!("poll with examine");
        let ImapWatchKit {
            mut conn,
            main_conn: _,
            uid_store,
        } = kit;
        conn.connect().await?;
        let mailboxes: HashMap<MailboxHash, ImapMailbox> = {
            let mailboxes_lck = timeout(uid_store.timeout, uid_store.mailboxes.lock()).await?;
            mailboxes_lck.clone()
        };
        loop {
            for (_, mailbox) in mailboxes.clone() {
                if let Some(ev) = examine_updates(mailbox, &mut conn).await? {
                    emitter.emit(ev).await;
                }
            }
            //[ref:FIXME]: make sleep duration configurable
            smol::Timer::after(Duration::from_secs(3 * 60)).await;
        }
    })
}

pub fn idle(kit: ImapWatchKit) -> impl futures::stream::Stream<Item = Result<BackendEvent>> {
    // duration interval to send heartbeat
    const _10_MINS: Duration = Duration::from_secs(10 * 60);
    // duration interval to check other mailboxes for changes
    const _5_MINS: Duration = Duration::from_secs(5 * 60);
    try_fn_stream(|emitter| async move {
        log::trace!("IDLE");
        /* IDLE only watches the connection's selected mailbox. We will IDLE on INBOX
         * and every ~5 minutes wake up and poll the others */
        let ImapWatchKit {
            mut conn,
            main_conn,
            uid_store,
        } = kit;
        conn.connect().await?;
        let mailbox: ImapMailbox = {
            let mut retries = 0;
            loop {
                let inbox = uid_store
                    .mailboxes
                    .lock()
                    .await
                    .values()
                    .find(|f| {
                        f.parent.is_none() && (f.special_usage() == SpecialUsageMailbox::Inbox)
                    })
                    .cloned();
                match inbox {
                    Some(mailbox) => break mailbox,
                    None if retries >= 10 => {
                        return Err(Error::new(
                            "INBOX mailbox not found in local mailbox index. the connection might \
                             have not parsed the IMAP mailboxes correctly",
                        )
                        .set_kind(ErrorKind::TimedOut));
                    }
                    None => {
                        smol::Timer::after(Duration::from_millis(
                            retries * (4 * crate::utils::random::random_u8() as u64),
                        ))
                        .await;
                        retries += 1;
                    }
                }
            }
        };
        let mailbox_hash = mailbox.hash();
        let mut response = Vec::with_capacity(8 * 1024);
        let select_response = conn
            .examine_mailbox(mailbox_hash, &mut response, true)
            .await?;
        {
            let mut mismatch = false;
            {
                let mut uidvalidities = uid_store.uidvalidity.lock().unwrap();

                if let Some(v) = uidvalidities.get(&mailbox_hash) {
                    mismatch = *v != select_response.uidvalidity;
                }
                uidvalidities.insert(mailbox_hash, select_response.uidvalidity);
            }
            if mismatch {
                emitter
                    .emit(
                        RefreshEvent {
                            account_hash: uid_store.account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Rescan,
                        }
                        .into(),
                    )
                    .await;
            }
        }
        let mailboxes: HashMap<MailboxHash, ImapMailbox> = {
            let mailboxes_lck = timeout(uid_store.timeout, uid_store.mailboxes.lock()).await?;
            mailboxes_lck.clone()
        };
        conn.send_command(CommandBody::Idle).await?;
        let mut blockn = ImapBlockingConnection::from(conn);
        let mut watch = Instant::now();
        loop {
            let line = match timeout(Some(_10_MINS), blockn.as_stream()).await {
                Ok(Some(line)) => line,
                Ok(None) => {
                    log::trace!("IDLE connection dropped: {:?}", &blockn.err());
                    return Ok(());
                }
                Err(_) => {
                    /* Timeout */
                    blockn.conn.send_raw(b"DONE").await?;
                    blockn
                        .conn
                        .read_response(&mut response, RequiredResponses::empty())
                        .await?;
                    blockn.conn.send_command(CommandBody::Idle).await?;
                    let mut main_conn_lck = main_conn.lock().await?;
                    main_conn_lck.connect().await?;
                    continue;
                }
            };
            let now = Instant::now();
            if now.duration_since(watch) >= _5_MINS {
                /* Time to poll all inboxes */
                let mut main_conn_lck = main_conn.lock().await?;
                for (_h, mailbox) in mailboxes.clone() {
                    if let Some(ev) = examine_updates(mailbox, &mut main_conn_lck).await? {
                        emitter.emit(ev).await;
                    }
                }
                watch = now;
            }
            if line
                .split_rn()
                .filter(|l| {
                    !l.starts_with(b"+ ")
                        && !l.starts_with(b"* ok")
                        && !l.starts_with(b"* ok")
                        && !l.starts_with(b"* Ok")
                        && !l.starts_with(b"* OK")
                })
                .count()
                == 0
            {
                continue;
            }
            {
                blockn.conn.send_raw(b"DONE").await?;
                blockn
                    .conn
                    .read_response(&mut response, RequiredResponses::empty())
                    .await?;
                for l in line.split_rn().chain(response.split_rn()) {
                    log::trace!("process_untagged {:?}", &l);
                    if l.starts_with(b"+ ")
                        || l.starts_with(b"* ok")
                        || l.starts_with(b"* ok")
                        || l.starts_with(b"* Ok")
                        || l.starts_with(b"* OK")
                    {
                        continue;
                    }
                    blockn.conn.process_untagged(l).await?;
                }
                blockn.conn.send_command(CommandBody::Idle).await?;
            }
        }
    })
}

pub async fn examine_updates(
    mailbox: ImapMailbox,
    conn: &mut ImapConnection,
) -> Result<Option<BackendEvent>> {
    if mailbox.no_select {
        return Ok(None);
    }
    let mailbox_hash = mailbox.hash();
    log::trace!("examining mailbox {} {}", mailbox_hash, mailbox.path());
    if let Some(new_envelopes) = conn.resync(mailbox_hash).await? {
        Ok(new_envelopes
            .into_iter()
            .map(|env| RefreshEvent {
                mailbox_hash,
                account_hash: conn.uid_store.account_hash,
                kind: RefreshEventKind::Create(Box::new(env)),
            })
            .collect::<Vec<_>>()
            .try_into()
            .ok())
    } else {
        let mut response = Vec::with_capacity(8 * 1024);
        let select_response = conn
            .examine_mailbox(mailbox_hash, &mut response, true)
            .await?;
        {
            let mut uidvalidities = conn.uid_store.uidvalidity.lock().unwrap();

            if let Some(v) = uidvalidities.get(&mailbox_hash) {
                if *v != select_response.uidvalidity {
                    return Ok(Some(
                        RefreshEvent {
                            account_hash: conn.uid_store.account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Rescan,
                        }
                        .into(),
                    ));
                }
            } else {
                uidvalidities.insert(mailbox_hash, select_response.uidvalidity);
            }
        }

        let current_exists = mailbox.exists.lock().unwrap().len();
        if mailbox.is_cold() {
            /* Mailbox hasn't been loaded yet */
            let has_list_status: bool = conn
                .uid_store
                .capabilities
                .lock()
                .unwrap()
                .iter()
                .any(|cap| cap.eq_ignore_ascii_case(b"LIST-STATUS"));
            if has_list_status {
                // [ref:TODO]: (#222) imap-codec does not support "LIST Command Extensions" currently.
                conn.send_command_raw(
                    format!(
                        "LIST \"{}\" \"\" RETURN (STATUS (MESSAGES UNSEEN))",
                        mailbox.imap_path()
                    )
                    .as_bytes(),
                )
                .await?;
                conn.read_response(
                    &mut response,
                    RequiredResponses::LIST | RequiredResponses::STATUS,
                )
                .await?;
                log::trace!(
                    "list return status out: {}",
                    String::from_utf8_lossy(&response)
                );
                for l in response.split_rn() {
                    if !l.starts_with(b"*") {
                        continue;
                    }
                    if let Ok(status) = protocol_parser::status_response(l).map(|(_, v)| v) {
                        if Some(mailbox_hash) == status.mailbox {
                            if let Some(total) = status.messages {
                                if let Ok(mut exists_lck) = mailbox.exists.lock() {
                                    exists_lck.clear();
                                    exists_lck.set_not_yet_seen(total);
                                }
                            }
                            if let Some(total) = status.unseen {
                                if let Ok(mut unseen_lck) = mailbox.unseen.lock() {
                                    unseen_lck.clear();
                                    unseen_lck.set_not_yet_seen(total);
                                }
                            }
                            break;
                        }
                    }
                }
            } else {
                conn.send_command(CommandBody::search(None, SearchKey::Unseen.into(), false))
                    .await?;
                conn.read_response(&mut response, RequiredResponses::SEARCH)
                    .await?;
                let unseen_count = protocol_parser::search_results(&response)?.1.len();
                if let Ok(mut exists_lck) = mailbox.exists.lock() {
                    exists_lck.clear();
                    exists_lck.set_not_yet_seen(select_response.exists);
                }
                if let Ok(mut unseen_lck) = mailbox.unseen.lock() {
                    unseen_lck.clear();
                    unseen_lck.set_not_yet_seen(unseen_count);
                }
            }
            mailbox.set_warm(true);
            return Ok(None);
        }

        if select_response.recent > 0 {
            /* UID SEARCH RECENT */
            conn.send_command(CommandBody::search(None, SearchKey::Recent.into(), true))
                .await?;
            conn.read_response(&mut response, RequiredResponses::SEARCH)
                .await?;
            let v = protocol_parser::search_results(response.as_slice()).map(|(_, v)| v)?;
            if v.is_empty() {
                log::trace!(
                    "search response was empty: {}",
                    String::from_utf8_lossy(&response)
                );
                return Ok(None);
            }
            let (required_responses, attributes) = common_attributes();
            conn.send_command(CommandBody::fetch(v.as_slice(), attributes, true)?)
                .await?;
            conn.read_response(&mut response, required_responses)
                .await?;
        } else if select_response.exists > current_exists {
            let min = current_exists.max(1);

            let (required_responses, attributes) = common_attributes();
            conn.send_command(CommandBody::fetch(min.., attributes, false)?)
                .await?;
            conn.read_response(&mut response, required_responses)
                .await?;
        } else {
            return Ok(None);
        }
        log::trace!(
            "fetch response is {} bytes and {} lines",
            response.len(),
            String::from_utf8_lossy(&response).lines().count()
        );
        let (_, mut v, _) = protocol_parser::fetch_responses(&response)?;
        log::trace!("responses len is {}", v.len());
        if v.is_empty() {
            return Ok(None);
        }
        for FetchResponse {
            ref uid,
            ref mut envelope,
            ref mut flags,
            ref references,
            ..
        } in v.iter_mut()
        {
            let uid = uid.unwrap();
            let env = envelope.as_mut().unwrap();
            env.set_hash(generate_envelope_hash(mailbox.imap_path(), &uid));
            if let Some(value) = references {
                env.set_references(value);
            }
            let mut tag_lck = conn.uid_store.collection.tag_index.write().unwrap();
            if let Some((flags, keywords)) = flags {
                env.set_flags(*flags);
                if !env.is_seen() {
                    mailbox.unseen.lock().unwrap().insert_new(env.hash());
                }
                mailbox.exists.lock().unwrap().insert_new(env.hash());
                for f in keywords {
                    let hash = TagHash::from_bytes(f.as_bytes());
                    tag_lck.entry(hash).or_insert_with(|| f.to_string());
                    env.tags_mut().insert(hash);
                }
            }
        }
        {
            conn.uid_store
                .insert_envelopes(mailbox_hash, &v)
                .or_else(ignore_not_found)
                .chain_err_summary(|| {
                    format!(
                        "Could not save envelopes in cache for mailbox {}",
                        mailbox.imap_path()
                    )
                })?;
        }

        let mut events = Vec::with_capacity(v.len());

        for FetchResponse {
            uid,
            envelope,
            message_sequence_number,
            ..
        } in v
        {
            if uid.is_none() || envelope.is_none() {
                continue;
            }
            let uid = uid.unwrap();
            if conn
                .uid_store
                .uid_index
                .lock()
                .unwrap()
                .contains_key(&(mailbox_hash, uid))
            {
                continue;
            }
            let env = envelope.unwrap();
            log::trace!(
                "Create event {} {} {}",
                env.hash(),
                env.subject(),
                mailbox.path(),
            );
            conn.uid_store
                .msn_index
                .lock()
                .unwrap()
                .entry(mailbox_hash)
                .or_default()
                .insert(message_sequence_number, uid);
            conn.uid_store
                .hash_index
                .lock()
                .unwrap()
                .insert(env.hash(), (uid, mailbox_hash));
            conn.uid_store
                .uid_index
                .lock()
                .unwrap()
                .insert((mailbox_hash, uid), env.hash());
            events.push(RefreshEvent {
                account_hash: conn.uid_store.account_hash,
                mailbox_hash,
                kind: Create(Box::new(env)),
            });
        }
        Ok(events.try_into().ok())
    }
}
