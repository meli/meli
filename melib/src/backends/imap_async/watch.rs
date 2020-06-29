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
use crate::email::parser::BytesExt;
use crate::email::parser::BytesIterExt;
use std::sync::{Arc, Mutex};

/// Arguments for IMAP watching functions
pub struct ImapWatchKit {
    pub conn: ImapConnection,
    pub main_conn: Arc<FutureMutex<ImapConnection>>,
    pub uid_store: Arc<UIDStore>,
}

macro_rules! exit_on_error {
    ($conn:expr, $mailbox_hash:ident, $thread_id:ident, $($result:expr)+) => {
        $(if let Err(e) = $result {
            *$conn.uid_store.is_online.lock().unwrap() = (
            Instant::now(),
            Err(e.clone()),
        );
            debug!("failure: {}", e.to_string());
            let account_hash = $conn.uid_store.account_hash;
            $conn.add_refresh_event(RefreshEvent {
                account_hash,
                mailbox_hash: $mailbox_hash,
                kind: RefreshEventKind::Failure(e.clone()),
            });
            Err(e)
        } else { Ok(()) }?;)+
    };
}

pub async fn poll_with_examine(kit: ImapWatchKit) -> Result<()> {
    debug!("poll with examine");
    let ImapWatchKit {
        mut conn,
        main_conn,
        uid_store,
    } = kit;
    conn.connect().await?;
    let mut response = String::with_capacity(8 * 1024);
    let thread_id: std::thread::ThreadId = std::thread::current().id();
    loop {
        let mailboxes = uid_store.mailboxes.read()?;
        for mailbox in mailboxes.values() {
            examine_updates(mailbox, &mut conn, &uid_store).await?;
        }
        let mut main_conn = main_conn.lock().await;
        main_conn.send_command(b"NOOP").await?;
        main_conn
            .read_response(&mut response, RequiredResponses::empty())
            .await?;
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
    let thread_id: std::thread::ThreadId = std::thread::current().id();
    let mailbox: ImapMailbox = match uid_store
        .mailboxes
        .read()
        .unwrap()
        .values()
        .find(|f| f.parent.is_none() && (f.special_usage() == SpecialUsageMailbox::Inbox))
        .map(std::clone::Clone::clone)
    {
        Some(mailbox) => mailbox,
        None => {
            let err = MeliError::new("INBOX mailbox not found in local mailbox index. meli may have not parsed the IMAP mailboxes correctly");
            debug!("failure: {}", err.to_string());
            conn.add_refresh_event(RefreshEvent {
                account_hash: uid_store.account_hash,
                mailbox_hash: 0,
                kind: RefreshEventKind::Failure(err.clone()),
            });
            return Err(err);
        }
    };
    let mailbox_hash = mailbox.hash();
    let uidvalidity;
    let mut response = String::with_capacity(8 * 1024);
    exit_on_error!(
        conn,
        mailbox_hash,
        thread_id,
        conn.send_command(format!("SELECT \"{}\"", mailbox.imap_path()).as_bytes())
            .await
        conn.read_response(&mut response, RequiredResponses::SELECT_REQUIRED)
            .await
    );
    debug!("select response {}", &response);
    {
        let mut prev_exists = mailbox.exists.lock().unwrap();
        match protocol_parser::select_response(&response) {
            Ok(ok) => {
                {
                    uidvalidity = ok.uidvalidity;
                    let mut uidvalidities = uid_store.uidvalidity.lock().unwrap();

                    if let Some(v) = uidvalidities.get_mut(&mailbox_hash) {
                        if *v != ok.uidvalidity {
                            conn.add_refresh_event(RefreshEvent {
                                account_hash: uid_store.account_hash,
                                mailbox_hash,
                                kind: RefreshEventKind::Rescan,
                            });
                            prev_exists.clear();
                            /*
                            uid_store.uid_index.lock().unwrap().clear();
                            uid_store.hash_index.lock().unwrap().clear();
                            uid_store.byte_cache.lock().unwrap().clear();
                            */
                            *v = ok.uidvalidity;
                        }
                    } else {
                        conn.add_refresh_event(RefreshEvent {
                            account_hash: uid_store.account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Rescan,
                        });
                        conn.add_refresh_event(RefreshEvent {
                            account_hash: uid_store.account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Failure(MeliError::new(format!(
                                "Unknown mailbox: {} {}",
                                mailbox.path(),
                                mailbox_hash
                            ))),
                        });
                    }
                }
                debug!(&ok);
            }
            Err(e) => {
                debug!("{:?}", e);
                panic!("could not select mailbox");
            }
        };
    }
    exit_on_error!(
        conn,
        mailbox_hash,
        thread_id,
        conn.send_command(b"IDLE").await
    );
    let mut iter = ImapBlockingConnection::from(conn);
    let mut beat = std::time::Instant::now();
    let mut watch = std::time::Instant::now();
    /* duration interval to send heartbeat */
    let _26_mins = std::time::Duration::from_secs(26 * 60);
    /* duration interval to check other mailboxes for changes */
    let _5_mins = std::time::Duration::from_secs(5 * 60);
    while let Some(line) = iter.next() {
        let now = std::time::Instant::now();
        if now.duration_since(beat) >= _26_mins {
            let mut main_conn_lck = main_conn.lock().await;
            exit_on_error!(
                iter.conn,
                mailbox_hash,
                thread_id,
                iter.conn.send_raw(b"DONE").await
                iter.conn.read_response(&mut response, RequiredResponses::empty()).await
                iter.conn.send_command(b"IDLE").await
                main_conn_lck.send_command(b"NOOP").await
                main_conn_lck.read_response(&mut response, RequiredResponses::empty()).await
            );
            beat = now;
        }
        if now.duration_since(watch) >= _5_mins {
            /* Time to poll all inboxes */
            let mut conn = main_conn.lock().await;
            for mailbox in uid_store.mailboxes.read().unwrap().values() {
                exit_on_error!(
                    conn,
                    mailbox_hash,
                    thread_id,
                    examine_updates(mailbox, &mut conn, &uid_store).await
                );
            }
            watch = now;
        }
        *uid_store.is_online.lock().unwrap() = (Instant::now(), Ok(()));
        match protocol_parser::untagged_responses(line.as_slice())
            .map(|(_, v)| v)
            .map_err(MeliError::from)
        {
            Ok(Some(Recent(r))) => {
                let mut conn = main_conn.lock().await;
                /* UID SEARCH RECENT */
                exit_on_error!(
                    conn,
                    mailbox_hash,
                    thread_id,
                    conn.examine_mailbox(mailbox_hash, &mut response, false).await
                    conn.send_command(b"UID SEARCH RECENT").await
                    conn.read_response(&mut response, RequiredResponses::SEARCH).await
                );
                match protocol_parser::search_results_raw(response.as_bytes())
                    .map(|(_, v)| v)
                    .map_err(MeliError::from)
                {
                    Ok(&[]) => {
                        debug!("UID SEARCH RECENT returned no results");
                    }
                    Ok(v) => {
                        exit_on_error!(
                            conn,
                            mailbox_hash,
                            thread_id,
                            conn.send_command(
                                &[&b"UID FETCH"[..], &v.trim().split(|b| b == &b' ').join(b','), &b"(FLAGS RFC822)"[..]]
                                .join(&b' '),
                                ).await
                            conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED).await
                        );
                        debug!(&response);
                        match protocol_parser::uid_fetch_responses(&response) {
                            Ok((_, v, _)) => {
                                let len = v.len();
                                let mut ctr = 0;
                                for UidFetchResponse {
                                    uid, flags, body, ..
                                } in v
                                {
                                    ctr += 1;
                                    if !uid_store
                                        .uid_index
                                        .lock()
                                        .unwrap()
                                        .contains_key(&(mailbox_hash, uid))
                                    {
                                        if let Ok(mut env) = Envelope::from_bytes(
                                            body.unwrap(),
                                            flags.as_ref().map(|&(f, _)| f),
                                        ) {
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
                                            if let Some((_, keywords)) = flags {
                                                let mut tag_lck =
                                                    uid_store.tag_index.write().unwrap();
                                                for f in keywords {
                                                    let hash = tag_hash!(f);
                                                    if !tag_lck.contains_key(&hash) {
                                                        tag_lck.insert(hash, f);
                                                    }
                                                    env.labels_mut().push(hash);
                                                }
                                            }
                                            if !env.is_seen() {
                                                mailbox
                                                    .unseen
                                                    .lock()
                                                    .unwrap()
                                                    .insert_new(env.hash());
                                            }
                                            if uid_store.cache_headers {
                                                cache::save_envelopes(
                                                    uid_store.account_hash,
                                                    mailbox_hash,
                                                    uidvalidity,
                                                    &[(uid, &env)],
                                                )?;
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
                            }
                            Err(e) => {
                                debug!(e);
                            }
                        }
                    }
                    Err(e) => {
                        debug!(
                            "UID SEARCH RECENT err: {}\nresp: {}",
                            e.to_string(),
                            &response
                        );
                    }
                }
            }
            Ok(Some(Expunge(n))) => {
                // The EXPUNGE response reports that the specified message sequence
                // number has been permanently removed from the mailbox. The message
                // sequence number for each successive message in the mailbox is
                // immediately decremented by 1, and this decrement is reflected in
                // message sequence numbers in subsequent responses (including other
                // untagged EXPUNGE responses).
                let mut conn = main_conn.lock().await;
                let deleted_uid = uid_store
                    .msn_index
                    .lock()
                    .unwrap()
                    .entry(mailbox_hash)
                    .or_default()
                    .remove(n);
                debug!("expunge {}, UID = {}", n, deleted_uid);
                let deleted_hash: EnvelopeHash = uid_store
                    .uid_index
                    .lock()
                    .unwrap()
                    .remove(&(mailbox_hash, deleted_uid))
                    .unwrap();
                uid_store.hash_index.lock().unwrap().remove(&deleted_hash);
                conn.add_refresh_event(RefreshEvent {
                    account_hash: uid_store.account_hash,
                    mailbox_hash,
                    kind: Remove(deleted_hash),
                });
            }
            Ok(Some(Exists(n))) => {
                let mut conn = main_conn.lock().await;
                /* UID FETCH ALL UID, cross-ref, then FETCH difference headers
                 * */
                let mut prev_exists = mailbox.exists.lock().unwrap();
                debug!("exists {}", n);
                if n > prev_exists.len() {
                    exit_on_error!(
                        conn,
                        mailbox_hash,
                        thread_id,
                        conn.examine_mailbox(mailbox_hash, &mut response, false).await
                        conn.send_command(
                            &[
                            b"FETCH",
                            format!("{}:{}", prev_exists.len() + 1, n).as_bytes(),
                            b"(UID FLAGS RFC822)",
                            ]
                            .join(&b' '),
                        ).await
                        conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED).await
                    );
                    match protocol_parser::uid_fetch_responses(&response) {
                        Ok((_, v, _)) => {
                            let len = v.len();
                            let mut ctr = 0;
                            'fetch_responses_b: for UidFetchResponse {
                                uid, flags, body, ..
                            } in v
                            {
                                if uid_store
                                    .uid_index
                                    .lock()
                                    .unwrap()
                                    .contains_key(&(mailbox_hash, uid))
                                {
                                    ctr += 1;
                                    continue 'fetch_responses_b;
                                }
                                if let Ok(mut env) = Envelope::from_bytes(
                                    body.unwrap(),
                                    flags.as_ref().map(|&(f, _)| f),
                                ) {
                                    ctr += 1;
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
                                    if let Some((_, keywords)) = flags {
                                        let mut tag_lck = uid_store.tag_index.write().unwrap();
                                        for f in keywords {
                                            let hash = tag_hash!(f);
                                            if !tag_lck.contains_key(&hash) {
                                                tag_lck.insert(hash, f);
                                            }
                                            env.labels_mut().push(hash);
                                        }
                                    }
                                    debug!(
                                        "Create event {} {} {}",
                                        env.hash(),
                                        env.subject(),
                                        mailbox.path(),
                                    );
                                    if !env.is_seen() {
                                        mailbox.unseen.lock().unwrap().insert_new(env.hash());
                                    }
                                    if uid_store.cache_headers {
                                        cache::save_envelopes(
                                            uid_store.account_hash,
                                            mailbox_hash,
                                            uidvalidity,
                                            &[(uid, &env)],
                                        )?;
                                    }
                                    prev_exists.insert_new(env.hash());

                                    conn.add_refresh_event(RefreshEvent {
                                        account_hash: uid_store.account_hash,
                                        mailbox_hash,
                                        kind: Create(Box::new(env)),
                                    });
                                }
                            }
                        }
                        Err(e) => {
                            debug!(e);
                        }
                    }
                }
            }
            Ok(Some(Fetch(msg_seq, flags))) => {
                /* a * {msg_seq} FETCH (FLAGS ({flags})) was received, so find out UID from msg_seq
                 * and send update
                 */
                let mut conn = main_conn.lock().await;
                debug!("fetch {} {:?}", msg_seq, flags);
                exit_on_error!(
                    conn,
                    mailbox_hash,
                    thread_id,
                    conn.examine_mailbox(mailbox_hash, &mut response, false).await
                    conn.send_command(
                        &[
                        b"UID SEARCH ",
                        format!("{}", msg_seq).as_bytes(),
                        ]
                        .join(&b' '),
                    ).await
                    conn.read_response(&mut response, RequiredResponses::SEARCH).await
                );
                match search_results(response.split_rn().next().unwrap_or("").as_bytes())
                    .map(|(_, v)| v)
                {
                    Ok(mut v) => {
                        if let Some(uid) = v.pop() {
                            if let Some(env_hash) = uid_store
                                .uid_index
                                .lock()
                                .unwrap()
                                .get(&(mailbox_hash, uid))
                            {
                                if !flags.0.intersects(crate::email::Flag::SEEN) {
                                    mailbox.unseen.lock().unwrap().insert_new(*env_hash);
                                } else {
                                    mailbox.unseen.lock().unwrap().remove(*env_hash);
                                }
                                conn.add_refresh_event(RefreshEvent {
                                    account_hash: uid_store.account_hash,
                                    mailbox_hash,
                                    kind: NewFlags(*env_hash, flags),
                                });
                            }
                        }
                    }
                    Err(e) => {
                        debug!(&response);
                        debug!(e);
                    }
                }
            }
            Ok(Some(Bye { .. })) => break,
            Ok(None) | Err(_) => {}
        }
    }
    debug!("IDLE connection dropped");
    let err: &str = iter.err().unwrap_or("Unknown reason.");
    main_conn.lock().await.add_refresh_event(RefreshEvent {
        account_hash: uid_store.account_hash,
        mailbox_hash,
        kind: RefreshEventKind::Failure(MeliError::new(format!(
            "IDLE connection dropped: {}",
            &err
        ))),
    });
    Err(MeliError::new(format!("IDLE connection dropped: {}", err)))
}

pub async fn examine_updates(
    mailbox: &ImapMailbox,
    conn: &mut ImapConnection,
    uid_store: &Arc<UIDStore>,
) -> Result<()> {
    let mailbox_hash = mailbox.hash();
    debug!("examining mailbox {} {}", mailbox_hash, mailbox.path());
    let mut response = String::with_capacity(8 * 1024);
    exit_on_error!(
        conn,
        mailbox_hash,
        thread_id,
        conn.examine_mailbox(mailbox_hash, &mut response, true)
            .await
    );
    *uid_store.is_online.lock().unwrap() = (Instant::now(), Ok(()));
    let uidvalidity;
    match protocol_parser::select_response(&response) {
        Ok(ok) => {
            uidvalidity = ok.uidvalidity;
            debug!(&ok);
            {
                let mut uidvalidities = uid_store.uidvalidity.lock().unwrap();

                if let Some(v) = uidvalidities.get_mut(&mailbox_hash) {
                    if *v != ok.uidvalidity {
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
                        *v = ok.uidvalidity;
                    }
                } else {
                    conn.add_refresh_event(RefreshEvent {
                        account_hash: uid_store.account_hash,
                        mailbox_hash,
                        kind: RefreshEventKind::Rescan,
                    });
                    conn.add_refresh_event(RefreshEvent {
                        account_hash: uid_store.account_hash,
                        mailbox_hash,
                        kind: RefreshEventKind::Failure(MeliError::new(format!(
                            "Unknown mailbox: {} {}",
                            mailbox.path(),
                            mailbox_hash
                        ))),
                    });
                }
            }
            let n = ok.exists;
            if ok.recent > 0 {
                {
                    /* UID SEARCH RECENT */
                    exit_on_error!(
                        conn,
                        mailbox_hash,
                        thread_id,
                        conn.send_command(b"UID SEARCH RECENT").await
                        conn.read_response(&mut response, RequiredResponses::SEARCH).await
                    );
                    match protocol_parser::search_results_raw(response.as_bytes())
                        .map(|(_, v)| v)
                        .map_err(MeliError::from)
                    {
                        Ok(&[]) => {
                            debug!("UID SEARCH RECENT returned no results");
                        }
                        Ok(v) => {
                            exit_on_error!(
                                conn,
                                mailbox_hash,
                                thread_id,
                                conn.send_command(
                                &[&b"UID FETCH"[..], &v.trim().split(|b| b == &b' ').join(b','), &b"(FLAGS RFC822)"[..]]
                                    .join(&b' '),
                                    ).await
                                conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED).await
                            );
                            debug!(&response);
                            match protocol_parser::uid_fetch_responses(&response) {
                                Ok((_, v, _)) => {
                                    'fetch_responses_c: for UidFetchResponse {
                                        uid,
                                        flags,
                                        body,
                                        ..
                                    } in v
                                    {
                                        if uid_store
                                            .uid_index
                                            .lock()
                                            .unwrap()
                                            .contains_key(&(mailbox_hash, uid))
                                        {
                                            continue 'fetch_responses_c;
                                        }
                                        if let Ok(mut env) = Envelope::from_bytes(
                                            body.unwrap(),
                                            flags.as_ref().map(|&(f, _)| f),
                                        ) {
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
                                            if let Some((_, keywords)) = flags {
                                                let mut tag_lck =
                                                    uid_store.tag_index.write().unwrap();
                                                for f in keywords {
                                                    let hash = tag_hash!(f);
                                                    if !tag_lck.contains_key(&hash) {
                                                        tag_lck.insert(hash, f);
                                                    }
                                                    env.labels_mut().push(hash);
                                                }
                                            }
                                            if !env.is_seen() {
                                                mailbox
                                                    .unseen
                                                    .lock()
                                                    .unwrap()
                                                    .insert_new(env.hash());
                                            }
                                            if uid_store.cache_headers {
                                                cache::save_envelopes(
                                                    uid_store.account_hash,
                                                    mailbox_hash,
                                                    uidvalidity,
                                                    &[(uid, &env)],
                                                )?;
                                            }
                                            let mut prev_exists = mailbox.exists.lock().unwrap();
                                            prev_exists.insert_new(env.hash());

                                            conn.add_refresh_event(RefreshEvent {
                                                account_hash: uid_store.account_hash,
                                                mailbox_hash,
                                                kind: Create(Box::new(env)),
                                            });
                                        }
                                    }
                                }
                                Err(e) => {
                                    debug!(e);
                                }
                            }
                        }
                        Err(e) => {
                            debug!(
                                "UID SEARCH RECENT err: {}\nresp: {}",
                                e.to_string(),
                                &response
                            );
                        }
                    }
                }
            } else if n > mailbox.exists.lock().unwrap().len() {
                /* UID FETCH ALL UID, cross-ref, then FETCH difference headers
                 * */
                debug!("exists {}", n);
                exit_on_error!(
                    conn,
                    mailbox_hash,
                    thread_id,
                    conn.send_command(
                        &[
                        b"FETCH",
                        format!("{}:{}", mailbox.exists.lock().unwrap().len() + 1, n).as_bytes(),
                        b"(UID FLAGS RFC822)",
                        ]
                        .join(&b' '),
                        ).await
                    conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED).await
                );
                match protocol_parser::uid_fetch_responses(&response) {
                    Ok((_, v, _)) => {
                        'fetch_responses_a: for UidFetchResponse {
                            uid, flags, body, ..
                        } in v
                        {
                            if uid_store
                                .uid_index
                                .lock()
                                .unwrap()
                                .contains_key(&(mailbox_hash, uid))
                            {
                                continue 'fetch_responses_a;
                            }
                            if let Ok(mut env) =
                                Envelope::from_bytes(body.unwrap(), flags.as_ref().map(|&(f, _)| f))
                            {
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
                                if let Some((_, keywords)) = flags {
                                    let mut tag_lck = uid_store.tag_index.write().unwrap();
                                    for f in keywords {
                                        let hash = tag_hash!(f);
                                        if !tag_lck.contains_key(&hash) {
                                            tag_lck.insert(hash, f);
                                        }
                                        env.labels_mut().push(hash);
                                    }
                                }
                                debug!(
                                    "Create event {} {} {}",
                                    env.hash(),
                                    env.subject(),
                                    mailbox.path(),
                                );
                                if !env.is_seen() {
                                    mailbox.unseen.lock().unwrap().insert_new(env.hash());
                                }
                                if uid_store.cache_headers {
                                    cache::save_envelopes(
                                        uid_store.account_hash,
                                        mailbox_hash,
                                        uidvalidity,
                                        &[(uid, &env)],
                                    )?;
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
                    Err(e) => {
                        debug!(e);
                    }
                }
            }
        }
        Err(e) => {
            debug!("{:?}", e);
            panic!("could not select mailbox");
        }
    };
    Ok(())
}
