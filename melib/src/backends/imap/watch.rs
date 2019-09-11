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
use std::sync::{Arc, Mutex};

/// Arguments for IMAP watching functions
pub struct ImapWatchKit {
    pub conn: ImapConnection,
    pub main_conn: Arc<Mutex<ImapConnection>>,
    pub hash_index: Arc<Mutex<FnvHashMap<EnvelopeHash, (UID, FolderHash)>>>,
    pub uid_index: Arc<Mutex<FnvHashMap<usize, EnvelopeHash>>>,
    pub folders: FnvHashMap<FolderHash, ImapFolder>,
    pub sender: RefreshEventConsumer,
    pub work_context: WorkContext,
}

macro_rules! exit_on_error {
    ($sender:expr, $folder_hash:ident, $work_context:ident, $thread_id:ident, $($result:expr)+) => {
        $(if let Err(e) = $result {
            debug!("failure: {}", e.to_string());
            $work_context.set_status.send(($thread_id, e.to_string())).unwrap();
            $sender.send(RefreshEvent {
                hash: $folder_hash,
                kind: RefreshEventKind::Failure(e),
            });
            std::process::exit(1);
        })+
    };
}

pub fn poll_with_examine(kit: ImapWatchKit) {
    debug!("poll with examine");
    let ImapWatchKit {
        mut conn,
        main_conn,
        hash_index,
        uid_index,
        folders,
        sender,
        work_context,
    } = kit;
    let mut response = String::with_capacity(8 * 1024);
    let thread_id: std::thread::ThreadId = std::thread::current().id();
    loop {
        work_context
            .set_status
            .send((thread_id, "sleeping...".to_string()))
            .unwrap();
        std::thread::sleep(std::time::Duration::from_millis(5 * 60 * 1000));
        for folder in folders.values() {
            work_context
                .set_status
                .send((
                    thread_id,
                    format!("examining `{}` for updates...", folder.path()),
                ))
                .unwrap();
            examine_updates(
                folder,
                &sender,
                &mut conn,
                &hash_index,
                &uid_index,
                &work_context,
            );
        }
        let mut main_conn = main_conn.lock().unwrap();
        main_conn.send_command(b"NOOP").unwrap();
        main_conn.read_response(&mut response).unwrap();
    }
}

pub fn idle(kit: ImapWatchKit) {
    debug!("IDLE");
    /* IDLE only watches the connection's selected mailbox. We will IDLE on INBOX and every ~5
     * minutes wake up and poll the others */
    let ImapWatchKit {
        mut conn,
        main_conn,
        hash_index,
        uid_index,
        folders,
        sender,
        work_context,
    } = kit;
    let thread_id: std::thread::ThreadId = std::thread::current().id();
    let folder: &ImapFolder = folders
        .values()
        .find(|f| f.parent.is_none() && f.path().eq_ignore_ascii_case("INBOX"))
        .unwrap();
    let folder_hash = folder.hash();
    let mut response = String::with_capacity(8 * 1024);
    exit_on_error!(
        sender,
        folder_hash,
        work_context,
        thread_id,
        conn.read_response(&mut response)
        conn.send_command(format!("SELECT {}", folder.path()).as_bytes())
        conn.read_response(&mut response)
    );
    debug!("select response {}", &response);
    {
        let mut prev_exists = folder.exists.lock().unwrap();
        *prev_exists = match protocol_parser::select_response(&response)
            .to_full_result()
            .map_err(MeliError::from)
        {
            Ok(SelectResponse::Bad(bad)) => {
                debug!(bad);
                panic!("could not select mailbox");
            }
            Ok(SelectResponse::Ok(ok)) => {
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
        sender,
        folder_hash,
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
    /* duration interval to check other folders for changes */
    let _5_mins = std::time::Duration::from_secs(5 * 60);
    loop {
        while let Some(line) = iter.next() {
            let now = std::time::Instant::now();
            if now.duration_since(beat) >= _26_mins {
                exit_on_error!(
                    sender,
                    folder_hash,
                    work_context,
                    thread_id,
                    iter.conn.set_nonblocking(true)
                    iter.conn.send_raw(b"DONE")
                    iter.conn.read_response(&mut response)
                    iter.conn.send_command(b"IDLE")
                    iter.conn.set_nonblocking(false)
                    main_conn.lock().unwrap().send_command(b"NOOP")
                    main_conn.lock().unwrap().read_response(&mut response)
                );
                beat = now;
            }
            if now.duration_since(watch) >= _5_mins {
                /* Time to poll the other inboxes */
                exit_on_error!(
                    sender,
                    folder_hash,
                    work_context,
                    thread_id,
                    iter.conn.set_nonblocking(true)
                    iter.conn.send_raw(b"DONE")
                    iter.conn.read_response(&mut response)
                );
                for (hash, folder) in &folders {
                    if *hash == folder_hash {
                        /* Skip INBOX */
                        continue;
                    }
                    work_context
                        .set_status
                        .send((
                            thread_id,
                            format!("examining `{}` for updates...", folder.path()),
                        ))
                        .unwrap();
                    examine_updates(
                        folder,
                        &sender,
                        &mut iter.conn,
                        &hash_index,
                        &uid_index,
                        &work_context,
                    );
                }
                work_context
                    .set_status
                    .send((thread_id, "done examining mailboxes.".to_string()))
                    .unwrap();
                exit_on_error!(
                    sender,
                    folder_hash,
                    work_context,
                    thread_id,
                    iter.conn.send_command(b"IDLE")
                    iter.conn.set_nonblocking(false)
                    main_conn.lock().unwrap().send_command(b"NOOP")
                    main_conn.lock().unwrap().read_response(&mut response)
                );
                watch = now;
            }
            match protocol_parser::untagged_responses(line.as_slice())
                .to_full_result()
                .map_err(MeliError::from)
            {
                Ok(Some(Recent(r))) => {
                    work_context
                        .set_status
                        .send((thread_id, format!("got `{} RECENT` notification", r)))
                        .unwrap();
                    /* UID SEARCH RECENT */
                    exit_on_error!(
                        sender,
                        folder_hash,
                        work_context,
                        thread_id,
                        iter.conn.set_nonblocking(true)
                        iter.conn.send_raw(b"DONE")
                        iter.conn.read_response(&mut response)
                        iter.conn.send_command(b"UID SEARCH RECENT")
                        iter.conn.read_response(&mut response)
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
                                sender,
                                folder_hash,
                                work_context,
                                thread_id,
                                iter.conn.send_command(
                                    &[b"UID FETCH", v, b"(FLAGS RFC822.HEADER)"]
                                    .join(&b' '),
                                    )
                                iter.conn.read_response(&mut response)
                            );
                            debug!(&response);
                            match protocol_parser::uid_fetch_response(response.as_bytes())
                                .to_full_result()
                                .map_err(MeliError::from)
                            {
                                Ok(v) => {
                                    let len = v.len();
                                    let mut ctr = 0;
                                    for (uid, flags, b) in v {
                                        work_context
                                            .set_status
                                            .send((
                                                thread_id,
                                                format!("parsing {}/{} envelopes..", ctr, len),
                                            ))
                                            .unwrap();
                                        if let Ok(env) = Envelope::from_bytes(&b, flags) {
                                            ctr += 1;
                                            hash_index
                                                .lock()
                                                .unwrap()
                                                .insert(env.hash(), (uid, folder_hash));
                                            uid_index.lock().unwrap().insert(uid, env.hash());
                                            debug!(
                                                "Create event {} {} {}",
                                                env.hash(),
                                                env.subject(),
                                                folder.path(),
                                            );
                                            sender.send(RefreshEvent {
                                                hash: folder_hash,
                                                kind: Create(Box::new(env)),
                                            });
                                        }
                                    }
                                    work_context
                                        .set_status
                                        .send((
                                            thread_id,
                                            format!("parsed {}/{} envelopes.", ctr, len),
                                        ))
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
                    exit_on_error!(
                        sender,
                        folder_hash,
                        work_context,
                        thread_id,
                        iter.conn.send_command(b"IDLE")
                        iter.conn.set_nonblocking(false)
                    );
                }
                Ok(Some(Expunge(n))) => {
                    work_context
                        .set_status
                        .send((thread_id, format!("got `{} EXPUNGED` notification", n)))
                        .unwrap();
                    debug!("expunge {}", n);
                }
                Ok(Some(Exists(n))) => {
                    exit_on_error!(
                        sender,
                        folder_hash,
                        work_context,
                        thread_id,
                        iter.conn.set_nonblocking(true)
                        iter.conn.send_raw(b"DONE")
                        iter.conn.read_response(&mut response)
                    );
                    /* UID FETCH ALL UID, cross-ref, then FETCH difference headers
                     * */
                    let mut prev_exists = folder.exists.lock().unwrap();
                    debug!("exists {}", n);
                    work_context
                        .set_status
                        .send((
                            thread_id,
                            format!(
                                "got `{} EXISTS` notification (EXISTS was previously {} for {}",
                                n,
                                *prev_exists,
                                folder.path()
                            ),
                        ))
                        .unwrap();
                    if n > *prev_exists {
                        exit_on_error!(
                            sender,
                            folder_hash,
                            work_context,
                            thread_id,
                            iter.conn.send_command(
                                &[
                                b"FETCH",
                                format!("{}:{}", *prev_exists + 1, n).as_bytes(),
                                b"(UID FLAGS RFC822.HEADER)",
                                ]
                                .join(&b' '),
                            )
                            iter.conn.read_response(&mut response)
                        );
                        match protocol_parser::uid_fetch_response(response.as_bytes())
                            .to_full_result()
                            .map_err(MeliError::from)
                        {
                            Ok(v) => {
                                let len = v.len();
                                let mut ctr = 0;
                                for (uid, flags, b) in v {
                                    work_context
                                        .set_status
                                        .send((
                                            thread_id,
                                            format!("parsing {}/{} envelopes..", ctr, len),
                                        ))
                                        .unwrap();
                                    if uid_index.lock().unwrap().contains_key(&uid) {
                                        ctr += 1;
                                        continue;
                                    }
                                    if let Ok(env) = Envelope::from_bytes(&b, flags) {
                                        ctr += 1;
                                        hash_index
                                            .lock()
                                            .unwrap()
                                            .insert(env.hash(), (uid, folder_hash));
                                        uid_index.lock().unwrap().insert(uid, env.hash());
                                        debug!(
                                            "Create event {} {} {}",
                                            env.hash(),
                                            env.subject(),
                                            folder.path(),
                                        );
                                        sender.send(RefreshEvent {
                                            hash: folder_hash,
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
                    exit_on_error!(
                        sender,
                        folder_hash,
                        work_context,
                        thread_id,
                        iter.conn.send_command(b"IDLE")
                        iter.conn.set_nonblocking(false)
                    );
                }
                Ok(None) | Err(_) => {}
            }
            work_context
                .set_status
                .send((thread_id, "IDLEing".to_string()))
                .unwrap();
        }
    }
}

fn examine_updates(
    folder: &ImapFolder,
    sender: &RefreshEventConsumer,
    conn: &mut ImapConnection,
    hash_index: &Arc<Mutex<FnvHashMap<EnvelopeHash, (UID, FolderHash)>>>,
    uid_index: &Arc<Mutex<FnvHashMap<usize, EnvelopeHash>>>,
    work_context: &WorkContext,
) {
    let thread_id: std::thread::ThreadId = std::thread::current().id();
    let folder_hash = folder.hash();
    debug!("examining folder {} {}", folder_hash, folder.path());
    let mut response = String::with_capacity(8 * 1024);
    exit_on_error!(
        sender,
        folder_hash,
        work_context,
        thread_id,
        conn.send_command(format!("EXAMINE {}", folder.path()).as_bytes())
        conn.read_response(&mut response)
    );
    match protocol_parser::select_response(&response)
        .to_full_result()
        .map_err(MeliError::from)
    {
        Ok(SelectResponse::Bad(bad)) => {
            debug!(bad);
            panic!("could not select mailbox");
        }
        Ok(SelectResponse::Ok(ok)) => {
            debug!(&ok);
            let mut prev_exists = folder.exists.lock().unwrap();
            let n = ok.exists;
            if ok.recent > 0 {
                {
                    /* UID SEARCH RECENT */
                    exit_on_error!(
                        sender,
                        folder_hash,
                        work_context,
                        thread_id,
                        conn.send_command(b"UID SEARCH RECENT")
                        conn.read_response(&mut response)
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
                                sender,
                                folder_hash,
                                work_context,
                                thread_id,
                                conn.send_command(
                                    &[b"UID FETCH", v, b"(FLAGS RFC822.HEADER)"]
                                    .join(&b' '),
                                    )
                                conn.read_response(&mut response)
                            );
                            debug!(&response);
                            match protocol_parser::uid_fetch_response(response.as_bytes())
                                .to_full_result()
                                .map_err(MeliError::from)
                            {
                                Ok(v) => {
                                    for (uid, flags, b) in v {
                                        if let Ok(env) = Envelope::from_bytes(&b, flags) {
                                            hash_index
                                                .lock()
                                                .unwrap()
                                                .insert(env.hash(), (uid, folder_hash));
                                            uid_index.lock().unwrap().insert(uid, env.hash());
                                            debug!(
                                                "Create event {} {} {}",
                                                env.hash(),
                                                env.subject(),
                                                folder.path(),
                                            );
                                            sender.send(RefreshEvent {
                                                hash: folder_hash,
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
                    sender,
                    folder_hash,
                    work_context,
                    thread_id,
                    conn.send_command(
                        &[
                        b"FETCH",
                        format!("{}:{}", *prev_exists + 1, n).as_bytes(),
                        b"(UID FLAGS RFC822.HEADER)",
                        ]
                        .join(&b' '),
                        )
                    conn.read_response(&mut response)
                );
                match protocol_parser::uid_fetch_response(response.as_bytes())
                    .to_full_result()
                    .map_err(MeliError::from)
                {
                    Ok(v) => {
                        for (uid, flags, b) in v {
                            if let Ok(env) = Envelope::from_bytes(&b, flags) {
                                hash_index
                                    .lock()
                                    .unwrap()
                                    .insert(env.hash(), (uid, folder_hash));
                                uid_index.lock().unwrap().insert(uid, env.hash());
                                debug!(
                                    "Create event {} {} {}",
                                    env.hash(),
                                    env.subject(),
                                    folder.path(),
                                );
                                sender.send(RefreshEvent {
                                    hash: folder_hash,
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
}
