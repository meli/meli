/*
 * meli - imap
 *
 * Copyright 2020 Manos Pitsidianakis
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

use super::ImapConnection;
use crate::backends::imap::protocol_parser::{
    ImapLineSplit, RequiredResponses, UidFetchResponse, UntaggedResponse,
};
use crate::backends::BackendMailbox;
use crate::backends::{
    RefreshEvent,
    RefreshEventKind::{self, *},
};
use crate::email::Envelope;
use crate::error::*;
use std::time::Instant;

impl ImapConnection {
    pub fn process_untagged(&mut self, line: &str) -> Result<bool> {
        macro_rules! try_fail {
            ($mailbox_hash: expr, $($result:expr)+) => {
                $(if let Err(err) = $result {
                    *self.uid_store.is_online.lock().unwrap() = (
                        Instant::now(),
                        Err(err.clone()),
                    );
                    debug!("failure: {}", err.to_string());
                    self.add_refresh_event(RefreshEvent {
                        account_hash: self.uid_store.account_hash,
                        mailbox_hash: $mailbox_hash,
                        kind: RefreshEventKind::Failure(err.clone()),
                    });
                    Err(err)
                } else { Ok(()) }?;)+
            };
        }
        //FIXME
        let mailbox_hash = if let Some(mailbox_hash) = self.current_mailbox {
            mailbox_hash
        } else {
            return Ok(false);
        };
        let mailbox =
            std::clone::Clone::clone(&self.uid_store.mailboxes.read().unwrap()[&mailbox_hash]);

        let mut response = String::with_capacity(8 * 1024);
        let untagged_response =
            match super::protocol_parser::untagged_responses(line.as_bytes()).map(|(_, v)| v) {
                Ok(None) | Err(_) => {
                    return Ok(false);
                }
                Ok(Some(r)) => r,
            };
        match untagged_response {
            UntaggedResponse::Bye { reason } => {
                *self.uid_store.is_online.lock().unwrap() =
                    (std::time::Instant::now(), Err(reason.into()));
            }
            UntaggedResponse::Expunge(n) => {
                let deleted_uid = self
                    .uid_store
                    .msn_index
                    .lock()
                    .unwrap()
                    .entry(mailbox_hash)
                    .or_default()
                    .remove(n);
                debug!("expunge {}, UID = {}", n, deleted_uid);
                let deleted_hash: crate::email::EnvelopeHash = self
                    .uid_store
                    .uid_index
                    .lock()
                    .unwrap()
                    .remove(&(mailbox_hash, deleted_uid))
                    .unwrap();
                self.uid_store
                    .hash_index
                    .lock()
                    .unwrap()
                    .remove(&deleted_hash);
                self.add_refresh_event(RefreshEvent {
                    account_hash: self.uid_store.account_hash,
                    mailbox_hash,
                    kind: Remove(deleted_hash),
                });
            }
            UntaggedResponse::Exists(n) => {
                /* UID FETCH ALL UID, cross-ref, then FETCH difference headers
                 * */
                let mut prev_exists = mailbox.exists.lock().unwrap();
                debug!("exists {}", n);
                if n > prev_exists.len() {
                    try_fail!(
                        mailbox_hash,
                        self.send_command(
                            &[
                            b"FETCH",
                            format!("{}:{}", prev_exists.len() + 1, n).as_bytes(),
                            b"(UID FLAGS RFC822)",
                            ]
                            .join(&b' '),
                        )
                        self.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
                    );
                    match super::protocol_parser::uid_fetch_responses(&response) {
                        Ok((_, v, _)) => {
                            'fetch_responses: for UidFetchResponse {
                                uid, flags, body, ..
                            } in v
                            {
                                if self
                                    .uid_store
                                    .uid_index
                                    .lock()
                                    .unwrap()
                                    .contains_key(&(mailbox_hash, uid))
                                {
                                    continue 'fetch_responses;
                                }
                                if let Ok(mut env) = Envelope::from_bytes(
                                    body.unwrap(),
                                    flags.as_ref().map(|&(f, _)| f),
                                ) {
                                    self.uid_store
                                        .hash_index
                                        .lock()
                                        .unwrap()
                                        .insert(env.hash(), (uid, mailbox_hash));
                                    self.uid_store
                                        .uid_index
                                        .lock()
                                        .unwrap()
                                        .insert((mailbox_hash, uid), env.hash());
                                    if let Some((_, keywords)) = flags {
                                        let mut tag_lck = self.uid_store.tag_index.write().unwrap();
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
                                    prev_exists.insert_new(env.hash());
                                    self.add_refresh_event(RefreshEvent {
                                        account_hash: self.uid_store.account_hash,
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
            UntaggedResponse::Recent(_) => {
                try_fail!(
                    mailbox_hash,
                    self.send_command(b"UID SEARCH RECENT")
                    self.read_response(&mut response, RequiredResponses::SEARCH)
                );
                match super::protocol_parser::search_results_raw(response.as_bytes())
                    .map(|(_, v)| v)
                    .map_err(MeliError::from)
                {
                    Ok(&[]) => {
                        debug!("UID SEARCH RECENT returned no results");
                    }
                    Ok(v) => {
                        try_fail!(
                            mailbox_hash,
                            self.send_command(
                                &[b"UID FETCH", v, b"(FLAGS RFC822)"]
                                .join(&b' '),
                                )
                            self.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
                        );
                        debug!(&response);
                        match super::protocol_parser::uid_fetch_responses(&response) {
                            Ok((_, v, _)) => {
                                for UidFetchResponse {
                                    uid, flags, body, ..
                                } in v
                                {
                                    if !self
                                        .uid_store
                                        .uid_index
                                        .lock()
                                        .unwrap()
                                        .contains_key(&(mailbox_hash, uid))
                                    {
                                        if let Ok(mut env) = Envelope::from_bytes(
                                            body.unwrap(),
                                            flags.as_ref().map(|&(f, _)| f),
                                        ) {
                                            self.uid_store
                                                .hash_index
                                                .lock()
                                                .unwrap()
                                                .insert(env.hash(), (uid, mailbox_hash));
                                            self.uid_store
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
                                                    self.uid_store.tag_index.write().unwrap();
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

                                            mailbox.exists.lock().unwrap().insert_new(env.hash());
                                            self.add_refresh_event(RefreshEvent {
                                                account_hash: self.uid_store.account_hash,
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
            UntaggedResponse::Fetch(msg_seq, flags) => {
                /* a * {msg_seq} FETCH (FLAGS ({flags})) was received, so find out UID from msg_seq
                 * and send update
                 */
                debug!("fetch {} {:?}", msg_seq, flags);
                try_fail!(
                        mailbox_hash,
                self.send_command(
                    &[
                    b"UID SEARCH ",
                    format!("{}", msg_seq).as_bytes(),
                    ]
                    .join(&b' '),
                )
                        self.read_response(&mut response, RequiredResponses::SEARCH)
                    );
                debug!(&response);
                match super::protocol_parser::search_results(
                    response.split_rn().next().unwrap_or("").as_bytes(),
                )
                .map(|(_, v)| v)
                {
                    Ok(mut v) => {
                        if let Some(uid) = v.pop() {
                            let lck = self.uid_store.uid_index.lock().unwrap();
                            let env_hash = lck.get(&(mailbox_hash, uid)).map(|&h| h);
                            drop(lck);
                            if let Some(env_hash) = env_hash {
                                if !flags.0.intersects(crate::email::Flag::SEEN) {
                                    mailbox.unseen.lock().unwrap().insert_new(env_hash);
                                } else {
                                    mailbox.unseen.lock().unwrap().remove(env_hash);
                                }
                                self.add_refresh_event(RefreshEvent {
                                    account_hash: self.uid_store.account_hash,
                                    mailbox_hash,
                                    kind: NewFlags(env_hash, flags),
                                });
                            };
                        }
                    }
                    Err(e) => {
                        debug!(&response);
                        debug!(e);
                    }
                }
            }
        }
        Ok(true)
    }
}
