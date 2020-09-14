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
use super::*;
use crate::backends::SpecialUsageMailbox;
use std::sync::Arc;

/// Arguments for IMAP watching functions
pub struct ImapWatchKit {
    pub conn: ImapConnection,
    pub main_conn: Arc<FutureMutex<ImapConnection>>,
    pub uid_store: Arc<UIDStore>,
}

pub async fn poll_with_examine(kit: ImapWatchKit) -> Result<()> {
    debug!("poll with examine");
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
            examine_updates(mailbox, &mut conn, &uid_store).await?;
        }
        //FIXME: make sleep duration configurable
        smol::Timer::after(std::time::Duration::from_secs(3 * 60)).await;
    }
}

pub async fn idle(kit: ImapWatchKit) -> Result<()> {
    debug!("IDLE");
    /* IDLE only watches the connection's selected mailbox. We will IDLE on INBOX and every ~5
     * minutes wake up and poll the others */
    let ImapWatchKit {
        mut conn,
        main_conn,
        uid_store,
    } = kit;
    conn.connect().await?;
    let mailbox: ImapMailbox = match uid_store
        .mailboxes
        .lock()
        .await
        .values()
        .find(|f| f.parent.is_none() && (f.special_usage() == SpecialUsageMailbox::Inbox))
        .map(std::clone::Clone::clone)
    {
        Some(mailbox) => mailbox,
        None => {
            return Err(MeliError::new("INBOX mailbox not found in local mailbox index. meli may have not parsed the IMAP mailboxes correctly"));
        }
    };
    let mailbox_hash = mailbox.hash();
    let mut response = String::with_capacity(8 * 1024);
    let select_response = conn
        .select_mailbox(mailbox_hash, &mut response, true)
        .await?
        .unwrap();
    debug!("select response {}", &response);
    {
        let mut uidvalidities = uid_store.uidvalidity.lock().unwrap();

        if let Some(v) = uidvalidities.get(&mailbox_hash) {
            if *v != select_response.uidvalidity {
                if uid_store.keep_offline_cache {
                    #[cfg(not(feature = "sqlite3"))]
                    let mut cache_handle = super::cache::DefaultCache::get(uid_store.clone())?;
                    #[cfg(feature = "sqlite3")]
                    let mut cache_handle = super::cache::Sqlite3Cache::get(uid_store.clone())?;
                    cache_handle.clear(mailbox_hash, &select_response)?;
                }
                conn.add_refresh_event(RefreshEvent {
                    account_hash: uid_store.account_hash,
                    mailbox_hash,
                    kind: RefreshEventKind::Rescan,
                });
                /*
                uid_store.uid_index.lock().unwrap().clear();
                uid_store.hash_index.lock().unwrap().clear();
                uid_store.byte_cache.lock().unwrap().clear();
                */
            }
        } else {
            uidvalidities.insert(mailbox_hash, select_response.uidvalidity);
        }
    }
    let mailboxes: HashMap<MailboxHash, ImapMailbox> = {
        let mailboxes_lck = timeout(uid_store.timeout, uid_store.mailboxes.lock()).await?;
        mailboxes_lck.clone()
    };
    conn.send_command(b"IDLE").await?;
    let mut blockn = ImapBlockingConnection::from(conn);
    let mut beat = std::time::Instant::now();
    let mut watch = std::time::Instant::now();
    /* duration interval before IMAP timeouts */
    const _35_MINS: std::time::Duration = std::time::Duration::from_secs(35 * 60);
    /* duration interval to send heartbeat */
    const _26_MINS: std::time::Duration = std::time::Duration::from_secs(26 * 60);
    /* duration interval to check other mailboxes for changes */
    const _5_MINS: std::time::Duration = std::time::Duration::from_secs(5 * 60);
    while let Some(line) = timeout(Some(_35_MINS), blockn.as_stream()).await? {
        let now = std::time::Instant::now();
        if now.duration_since(beat) >= _26_MINS {
            let mut main_conn_lck = timeout(uid_store.timeout, main_conn.lock()).await?;
            blockn.conn.send_raw(b"DONE").await?;
            blockn
                .conn
                .read_response(&mut response, RequiredResponses::empty())
                .await?;
            blockn.conn.send_command(b"IDLE").await?;
            main_conn_lck.send_command(b"NOOP").await?;
            main_conn_lck
                .read_response(&mut response, RequiredResponses::empty())
                .await?;
            beat = now;
        }
        if now.duration_since(watch) >= _5_MINS {
            /* Time to poll all inboxes */
            let mut conn = timeout(uid_store.timeout, main_conn.lock()).await?;
            for (h, mailbox) in mailboxes.clone() {
                if mailbox_hash == h {
                    continue;
                }
                examine_updates(mailbox, &mut conn, &uid_store).await?;
            }
            watch = now;
        }
        if to_str!(&line)
            .split_rn()
            .filter(|l| {
                !l.starts_with("+ ")
                    && !l.starts_with("* ok")
                    && !l.starts_with("* ok")
                    && !l.starts_with("* Ok")
                    && !l.starts_with("* OK")
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
            for l in to_str!(&line).split_rn() {
                debug!("process_untagged {:?}", &l);
                if l.starts_with("+ ")
                    || l.starts_with("* ok")
                    || l.starts_with("* ok")
                    || l.starts_with("* Ok")
                    || l.starts_with("* OK")
                {
                    debug!("ignore continuation mark");
                    continue;
                }
                blockn.conn.process_untagged(l).await?;
            }
            blockn.conn.send_command(b"IDLE").await?;
        }
    }
    debug!("IDLE connection dropped");
    Err(blockn
        .err()
        .unwrap_or(MeliError::new("Unknown reason.").set_kind(crate::error::ErrorKind::Network))
        .set_summary("IDLE connection dropped".to_string()))
}

pub async fn examine_updates(
    mailbox: ImapMailbox,
    conn: &mut ImapConnection,
    uid_store: &Arc<UIDStore>,
) -> Result<()> {
    if mailbox.no_select {
        return Ok(());
    }
    let mailbox_hash = mailbox.hash();
    debug!("examining mailbox {} {}", mailbox_hash, mailbox.path());
    if let Some(new_envelopes) = conn.resync(mailbox_hash).await? {
        for env in new_envelopes {
            conn.add_refresh_event(RefreshEvent {
                mailbox_hash,
                account_hash: uid_store.account_hash,
                kind: RefreshEventKind::Create(Box::new(env)),
            });
        }
    } else {
        #[cfg(not(feature = "sqlite3"))]
        let mut cache_handle = super::cache::DefaultCache::get(uid_store.clone())?;
        #[cfg(feature = "sqlite3")]
        let mut cache_handle = super::cache::Sqlite3Cache::get(uid_store.clone())?;
        let mut response = String::with_capacity(8 * 1024);
        let select_response = conn
            .examine_mailbox(mailbox_hash, &mut response, true)
            .await?
            .unwrap();
        debug!(&select_response);
        {
            let mut uidvalidities = uid_store.uidvalidity.lock().unwrap();

            if let Some(v) = uidvalidities.get(&mailbox_hash) {
                if *v != select_response.uidvalidity {
                    if uid_store.keep_offline_cache {
                        cache_handle.clear(mailbox_hash, &select_response)?;
                    }
                    conn.add_refresh_event(RefreshEvent {
                        account_hash: uid_store.account_hash,
                        mailbox_hash,
                        kind: RefreshEventKind::Rescan,
                    });
                    /*
                    uid_store.uid_index.lock().unwrap().clear();
                    uid_store.hash_index.lock().unwrap().clear();
                    uid_store.byte_cache.lock().unwrap().clear();
                    */
                    return Ok(());
                }
            } else {
                uidvalidities.insert(mailbox_hash, select_response.uidvalidity);
            }
        }
        if debug!(select_response.recent > 0) {
            /* UID SEARCH RECENT */
            conn.send_command(b"UID SEARCH RECENT").await?;
            conn.read_response(&mut response, RequiredResponses::SEARCH)
                .await?;
            let v = protocol_parser::search_results(response.as_bytes()).map(|(_, v)| v)?;
            if v.is_empty() {
                debug!("search response was empty: {}", response);
                return Ok(());
            }
            let mut cmd = "UID FETCH ".to_string();
            if v.len() == 1 {
                cmd.push_str(&v[0].to_string());
            } else {
                cmd.push_str(&v[0].to_string());
                for n in v.into_iter().skip(1) {
                    cmd.push(',');
                    cmd.push_str(&n.to_string());
                }
            }
            cmd.push_str(" (UID FLAGS RFC822)");
            conn.send_command(cmd.as_bytes()).await?;
            conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
                .await?;
        } else if debug!(select_response.exists > mailbox.exists.lock().unwrap().len()) {
            conn.send_command(
                format!(
                    "FETCH {}:* (UID FLAGS RFC822)",
                    mailbox.exists.lock().unwrap().len()
                )
                .as_bytes(),
            )
            .await?;
            conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
                .await?;
        } else {
            return Ok(());
        }
        debug!(&response);
        let (_, mut v, _) = protocol_parser::fetch_responses(&response)?;
        for FetchResponse {
            ref uid,
            ref mut flags,
            ref mut body,
            ref mut envelope,
            ..
        } in v.iter_mut()
        {
            let uid = uid.unwrap();
            *envelope = Envelope::from_bytes(body.take().unwrap(), flags.as_ref().map(|&(f, _)| f))
                .map(|mut env| {
                    env.set_hash(generate_envelope_hash(&mailbox.imap_path(), &uid));
                    if let Some((_, keywords)) = flags.take() {
                        let mut tag_lck = uid_store.tag_index.write().unwrap();
                        for f in keywords {
                            let hash = tag_hash!(f);
                            if !tag_lck.contains_key(&hash) {
                                tag_lck.insert(hash, f);
                            }
                            env.labels_mut().push(hash);
                        }
                    }
                    env
                })
                .map_err(|err| {
                    debug!("uid {} envelope parse error {}", uid, &err);
                    err
                })
                .ok();
        }
        if uid_store.keep_offline_cache {
            cache_handle.insert_envelopes(mailbox_hash, &v)?;
        }
        'fetch_responses_c: for FetchResponse { uid, envelope, .. } in v {
            let uid = uid.unwrap();
            if uid_store
                .uid_index
                .lock()
                .unwrap()
                .contains_key(&(mailbox_hash, uid))
            {
                continue 'fetch_responses_c;
            }
            if let Some(env) = envelope {
                uid_store
                    .hash_index
                    .lock()
                    .unwrap()
                    .insert(env.hash(), (uid, mailbox_hash));
                uid_store
                    .uid_index
                    .lock()
                    .unwrap()
                    .insert((mailbox_hash, uid), env.hash());
                debug!(
                    "Create event {} {} {}",
                    env.hash(),
                    env.subject(),
                    mailbox.path(),
                );
                if !env.is_seen() {
                    mailbox.unseen.lock().unwrap().insert_new(env.hash());
                }
                mailbox.exists.lock().unwrap().insert_new(env.hash());
                conn.add_refresh_event(RefreshEvent {
                    account_hash: uid_store.account_hash,
                    mailbox_hash,
                    kind: Create(Box::new(env)),
                });
            }
        }
    }
    Ok(())
}
