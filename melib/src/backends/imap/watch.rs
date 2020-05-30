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
    pub account_hash: AccountHash,
    pub conn: ImapConnection,
    pub main_conn: Arc<Mutex<ImapConnection>>,
    pub uid_store: Arc<UIDStore>,
    pub work_context: WorkContext,
}

macro_rules! exit_on_error {
    ($conn:expr, $account_hash:ident, $mailbox_hash:ident, $work_context:ident, $thread_id:ident, $($result:expr)+) => {
        $(if let Err(e) = $result {
            *$conn.uid_store.is_online.lock().unwrap() = (
            Instant::now(),
            Err(e.clone()),
        );
            debug!("failure: {}", e.to_string());
            $work_context.set_status.send(($thread_id, e.to_string())).unwrap();
            $work_context.finished.send($thread_id).unwrap();
            $conn.add_refresh_event(RefreshEvent {
                account_hash: $account_hash,
                mailbox_hash: $mailbox_hash,
                kind: RefreshEventKind::Failure(e.clone()),
            });
            Err(e)
        } else { Ok(()) }?;)+
    };
}

pub fn poll_with_examine(kit: ImapWatchKit) -> Result<()> {
    debug!("poll with examine");
    let ImapWatchKit {
        mut conn,
        main_conn,
        uid_store,
        work_context,
        account_hash,
    } = kit;
    loop {
        if super::try_lock(&uid_store.is_online, Some(std::time::Duration::new(10, 0)))?
            .1
            .is_ok()
        {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    conn.connect()?;
    let mut response = String::with_capacity(8 * 1024);
    let thread_id: std::thread::ThreadId = std::thread::current().id();
    loop {
        work_context
            .set_status
            .send((thread_id, "sleeping...".to_string()))
            .unwrap();
        std::thread::sleep(std::time::Duration::from_millis(5 * 60 * 1000));
        let mailboxes = uid_store.mailboxes.read()?;
        for mailbox in mailboxes.values() {
            work_context
                .set_status
                .send((
                    thread_id,
                    format!("examining `{}` for updates...", mailbox.path()),
                ))
                .unwrap();
            examine_updates(account_hash, mailbox, &mut conn, &uid_store, &work_context)?;
        }
        let mut main_conn = super::try_lock(&main_conn, Some(std::time::Duration::new(10, 0)))?;
        main_conn.send_command(b"NOOP")?;
        main_conn.read_response(&mut response, RequiredResponses::empty())?;
    }
}

pub fn idle(kit: ImapWatchKit) -> Result<()> {
    debug!("IDLE");
    /* IDLE only watches the connection's selected mailbox. We will IDLE on INBOX and every ~5
     * minutes wake up and poll the others */
    let ImapWatchKit {
        mut conn,
        main_conn,
        uid_store,
        work_context,
        account_hash,
    } = kit;
    loop {
        if super::try_lock(&uid_store.is_online, Some(std::time::Duration::new(10, 0)))?
            .1
            .is_ok()
        {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(100));
    }
    conn.connect()?;
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
            work_context
                .set_status
                .send((thread_id, err.to_string()))
                .unwrap();
            conn.add_refresh_event(RefreshEvent {
                account_hash,
                mailbox_hash: 0,
                kind: RefreshEventKind::Failure(err.clone()),
            });
            return Err(err);
        }
    };
    let mailbox_hash = mailbox.hash();
    let mut response = String::with_capacity(8 * 1024);
    exit_on_error!(
        conn,
                        account_hash,
        mailbox_hash,
        work_context,
        thread_id,
        conn.send_command(format!("SELECT \"{}\"", mailbox.imap_path()).as_bytes())
        conn.read_response(&mut response, RequiredResponses::SELECT_REQUIRED)
    );
    debug!("select response {}", &response);
    {
        let mut prev_exists = mailbox.exists.lock().unwrap();
        *prev_exists = match protocol_parser::select_response(&response) {
            Ok(ok) => {
                {
                    let mut uidvalidities = uid_store.uidvalidity.lock().unwrap();

                    if let Some(v) = uidvalidities.get_mut(&mailbox_hash) {
                        if *v != ok.uidvalidity {
                            conn.add_refresh_event(RefreshEvent {
                                account_hash,
                                mailbox_hash,
                                kind: RefreshEventKind::Rescan,
                            });
                            *prev_exists = 0;
                            /*
                            uid_store.uid_index.lock().unwrap().clear();
                            uid_store.hash_index.lock().unwrap().clear();
                            uid_store.byte_cache.lock().unwrap().clear();
                            */
                            *v = ok.uidvalidity;
                        }
                    } else {
                        conn.add_refresh_event(RefreshEvent {
                            account_hash,
                            mailbox_hash,
                            kind: RefreshEventKind::Rescan,
                        });
                        conn.add_refresh_event(RefreshEvent {
                            account_hash,
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
                ok.exists
            }
            Err(e) => {
                debug!("{:?}", e);
                panic!("could not select mailbox");
            }
        };
    }
    exit_on_error!(
        conn,
        account_hash,
        mailbox_hash,
        work_context,
        thread_id,
        conn.send_command(b"IDLE")
    );
    work_context
        .set_status
        .send((thread_id, "IDLEing".to_string()))
        .unwrap();
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
            let mut main_conn_lck =
                super::try_lock(&main_conn, Some(std::time::Duration::new(10, 0)))?;
            exit_on_error!(
                iter.conn,
                        account_hash,
                mailbox_hash,
                work_context,
                thread_id,
                iter.conn.set_nonblocking(true)
                iter.conn.send_raw(b"DONE")
                iter.conn.read_response(&mut response, RequiredResponses::empty())
                iter.conn.send_command(b"IDLE")
                iter.conn.set_nonblocking(false)
                main_conn_lck.send_command(b"NOOP")
                main_conn_lck.read_response(&mut response, RequiredResponses::empty())
            );
            beat = now;
        }
        if now.duration_since(watch) >= _5_mins {
            /* Time to poll all inboxes */
            let mut conn = try_lock(&main_conn, Some(std::time::Duration::new(10, 0)))?;
            for mailbox in uid_store.mailboxes.read().unwrap().values() {
                work_context
                    .set_status
                    .send((
                        thread_id,
                        format!("examining `{}` for updates...", mailbox.path()),
                    ))
                    .unwrap();
                exit_on_error!(
                    conn,
                    account_hash,
                    mailbox_hash,
                    work_context,
                    thread_id,
                    examine_updates(account_hash, mailbox, &mut conn, &uid_store, &work_context,)
                );
            }
            work_context
                .set_status
                .send((thread_id, "done examining mailboxes.".to_string()))
                .unwrap();
            watch = now;
        }
        *uid_store.is_online.lock().unwrap() = (Instant::now(), Ok(()));
        match protocol_parser::untagged_responses(line.as_slice())
            .to_full_result()
            .map_err(MeliError::from)
        {
            Ok(Some(Recent(r))) => {
                let mut conn = super::try_lock(&main_conn, Some(std::time::Duration::new(10, 0)))?;
                work_context
                    .set_status
                    .send((thread_id, format!("got `{} RECENT` notification", r)))
                    .unwrap();
                /* UID SEARCH RECENT */
                exit_on_error!(
                    conn,
                        account_hash,
                    mailbox_hash,
                    work_context,
                    thread_id,
                    conn.examine_mailbox(mailbox_hash, &mut response)
                    conn.send_command(b"UID SEARCH RECENT")
                    conn.read_response(&mut response, RequiredResponses::SEARCH)
                );
                match protocol_parser::search_results_raw(response.as_bytes())
                    .to_full_result()
                    .map_err(MeliError::from)
                {
                    Ok(&[]) => {
                        debug!("UID SEARCH RECENT returned no results");
                    }
                    Ok(v) => {
                        exit_on_error!(
                            conn,
                        account_hash,
                            mailbox_hash,
                            work_context,
                            thread_id,
                            conn.send_command(
                                &[&b"UID FETCH"[..], &v.trim().split(|b| b == &b' ').join(b','), &b"(FLAGS RFC822)"[..]]
                                .join(&b' '),
                                )
                            conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
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
                                    work_context
                                        .set_status
                                        .send((
                                            thread_id,
                                            format!("parsing {}/{} envelopes..", ctr, len),
                                        ))
                                        .unwrap();
                                    ctr += 1;
                                    *mailbox.exists.lock().unwrap() += 1;
                                    if !uid_store.uid_index.lock().unwrap().contains_key(&uid) {
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
                                                .insert(uid, env.hash());
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
                                                *mailbox.unseen.lock().unwrap() += 1;
                                            }

                                            conn.add_refresh_event(RefreshEvent {
                                                account_hash,
                                                mailbox_hash,
                                                kind: Create(Box::new(env)),
                                            });
                                        }
                                    }
                                }
                                work_context
                                    .set_status
                                    .send((thread_id, format!("parsed {}/{} envelopes.", ctr, len)))
                                    .unwrap();
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
                work_context
                    .set_status
                    .send((thread_id, format!("got `{} EXPUNGED` notification", n)))
                    .unwrap();
                debug!("expunge {}", n);
            }
            Ok(Some(Exists(n))) => {
                let mut conn = super::try_lock(&main_conn, Some(std::time::Duration::new(10, 0)))?;
                /* UID FETCH ALL UID, cross-ref, then FETCH difference headers
                 * */
                let mut prev_exists = mailbox.exists.lock().unwrap();
                debug!("exists {}", n);
                work_context
                    .set_status
                    .send((
                        thread_id,
                        format!(
                            "got `{} EXISTS` notification (EXISTS was previously {} for {}",
                            n,
                            *prev_exists,
                            mailbox.path()
                        ),
                    ))
                    .unwrap();
                if n > *prev_exists {
                    exit_on_error!(
                        conn,
                        account_hash,
                        mailbox_hash,
                        work_context,
                        thread_id,
                        conn.examine_mailbox(mailbox_hash, &mut response)
                        conn.send_command(
                            &[
                            b"FETCH",
                            format!("{}:{}", *prev_exists + 1, n).as_bytes(),
                            b"(UID FLAGS RFC822)",
                            ]
                            .join(&b' '),
                        )
                        conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
                    );
                    match protocol_parser::uid_fetch_responses(&response) {
                        Ok((_, v, _)) => {
                            let len = v.len();
                            let mut ctr = 0;
                            'fetch_responses_b: for UidFetchResponse {
                                uid, flags, body, ..
                            } in v
                            {
                                work_context
                                    .set_status
                                    .send((
                                        thread_id,
                                        format!("parsing {}/{} envelopes..", ctr, len),
                                    ))
                                    .unwrap();
                                if uid_store.uid_index.lock().unwrap().contains_key(&uid) {
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
                                    uid_store.uid_index.lock().unwrap().insert(uid, env.hash());
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
                                        *mailbox.unseen.lock().unwrap() += 1;
                                    }
                                    conn.add_refresh_event(RefreshEvent {
                                        account_hash,
                                        mailbox_hash,
                                        kind: Create(Box::new(env)),
                                    });
                                }
                            }
                            work_context
                                .set_status
                                .send((thread_id, format!("parsed {}/{} envelopes.", ctr, len)))
                                .unwrap();
                        }
                        Err(e) => {
                            debug!(e);
                        }
                    }

                    *prev_exists = n;
                } else if n < *prev_exists {
                    *prev_exists = n;
                }
            }
            Ok(Some(Fetch(msg_seq, flags))) => {
                /* a * {msg_seq} FETCH (FLAGS ({flags})) was received, so find out UID from msg_seq
                 * and send update
                 */
                let mut conn = super::try_lock(&main_conn, Some(std::time::Duration::new(10, 0)))?;
                debug!("fetch {} {:?}", msg_seq, flags);
                exit_on_error!(
                    conn,
                    account_hash,
                    mailbox_hash,
                    work_context,
                    thread_id,
                    conn.examine_mailbox(mailbox_hash, &mut response)
                    conn.send_command(
                        &[
                        b"UID SEARCH ",
                        format!("{}", msg_seq).as_bytes(),
                        ]
                        .join(&b' '),
                    )
                    conn.read_response(&mut response, RequiredResponses::SEARCH)
                );
                match search_results(response.split_rn().next().unwrap_or("").as_bytes())
                    .to_full_result()
                {
                    Ok(mut v) => {
                        if let Some(uid) = v.pop() {
                            if let Some(env_hash) = uid_store.uid_index.lock().unwrap().get(&uid) {
                                conn.add_refresh_event(RefreshEvent {
                                    account_hash,
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
        work_context
            .set_status
            .send((thread_id, "IDLEing".to_string()))
            .unwrap();
    }
    debug!("IDLE connection dropped");
    let err: &str = iter.err().unwrap_or("Unknown reason.");
    work_context
        .set_status
        .send((thread_id, "IDLE connection dropped".to_string()))
        .unwrap();
    work_context.finished.send(thread_id).unwrap();
    main_conn.lock().unwrap().add_refresh_event(RefreshEvent {
        account_hash,
        mailbox_hash,
        kind: RefreshEventKind::Failure(MeliError::new(format!(
            "IDLE connection dropped: {}",
            &err
        ))),
    });
    Err(MeliError::new(format!("IDLE connection dropped: {}", err)))
}

pub fn examine_updates(
    account_hash: AccountHash,
    mailbox: &ImapMailbox,
    conn: &mut ImapConnection,
    uid_store: &Arc<UIDStore>,
    work_context: &WorkContext,
) -> Result<()> {
    let thread_id: std::thread::ThreadId = std::thread::current().id();
    let mailbox_hash = mailbox.hash();
    debug!("examining mailbox {} {}", mailbox_hash, mailbox.path());
    let mut response = String::with_capacity(8 * 1024);
    exit_on_error!(
        conn,
        account_hash,
        mailbox_hash,
        work_context,
        thread_id,
        conn.examine_mailbox(mailbox_hash, &mut response)
    );
    *uid_store.is_online.lock().unwrap() = (Instant::now(), Ok(()));
    match protocol_parser::select_response(&response) {
        Ok(ok) => {
            debug!(&ok);
            {
                let mut uidvalidities = uid_store.uidvalidity.lock().unwrap();

                if let Some(v) = uidvalidities.get_mut(&mailbox_hash) {
                    if *v != ok.uidvalidity {
                        conn.add_refresh_event(RefreshEvent {
                            account_hash,
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
                        account_hash,
                        mailbox_hash,
                        kind: RefreshEventKind::Rescan,
                    });
                    conn.add_refresh_event(RefreshEvent {
                        account_hash,
                        mailbox_hash,
                        kind: RefreshEventKind::Failure(MeliError::new(format!(
                            "Unknown mailbox: {} {}",
                            mailbox.path(),
                            mailbox_hash
                        ))),
                    });
                }
            }
            let mut prev_exists = mailbox.exists.lock().unwrap();
            let n = ok.exists;
            if ok.recent > 0 {
                {
                    /* UID SEARCH RECENT */
                    exit_on_error!(
                        conn,
                        account_hash,
                        mailbox_hash,
                        work_context,
                        thread_id,
                        conn.send_command(b"UID SEARCH RECENT")
                        conn.read_response(&mut response, RequiredResponses::SEARCH)
                    );
                    match protocol_parser::search_results_raw(response.as_bytes())
                        .to_full_result()
                        .map_err(MeliError::from)
                    {
                        Ok(&[]) => {
                            debug!("UID SEARCH RECENT returned no results");
                        }
                        Ok(v) => {
                            exit_on_error!(
                                conn,
                                account_hash,
                                mailbox_hash,
                                work_context,
                                thread_id,
                                conn.send_command(
                                &[&b"UID FETCH"[..], &v.trim().split(|b| b == &b' ').join(b','), &b"(FLAGS RFC822)"[..]]
                                    .join(&b' '),
                                    )
                                conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
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
                                        if uid_store.uid_index.lock().unwrap().contains_key(&uid) {
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
                                                .insert(uid, env.hash());
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
                                                *mailbox.unseen.lock().unwrap() += 1;
                                            }
                                            conn.add_refresh_event(RefreshEvent {
                                                account_hash,
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
            } else if n > *prev_exists {
                /* UID FETCH ALL UID, cross-ref, then FETCH difference headers
                 * */
                debug!("exists {}", n);
                exit_on_error!(
                    conn,
                    account_hash,
                    mailbox_hash,
                    work_context,
                    thread_id,
                    conn.send_command(
                        &[
                        b"FETCH",
                        format!("{}:{}", *prev_exists + 1, n).as_bytes(),
                        b"(UID FLAGS RFC822)",
                        ]
                        .join(&b' '),
                        )
                    conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
                );
                match protocol_parser::uid_fetch_responses(&response) {
                    Ok((_, v, _)) => {
                        'fetch_responses_a: for UidFetchResponse {
                            uid, flags, body, ..
                        } in v
                        {
                            if uid_store.uid_index.lock().unwrap().contains_key(&uid) {
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
                                uid_store.uid_index.lock().unwrap().insert(uid, env.hash());
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
                                    *mailbox.unseen.lock().unwrap() += 1;
                                }
                                conn.add_refresh_event(RefreshEvent {
                                    account_hash,
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

                *prev_exists = n;
            } else if n < *prev_exists {
                *prev_exists = n;
            }
        }
        Err(e) => {
            debug!("{:?}", e);
            panic!("could not select mailbox");
        }
    };
    Ok(())
}
