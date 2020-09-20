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

use super::{ImapConnection, MailboxSelection, UID};
use crate::backends::imap::protocol_parser::{
    generate_envelope_hash, FetchResponse, ImapLineSplit, RequiredResponses, UntaggedResponse,
};
use crate::backends::BackendMailbox;
use crate::backends::{
    RefreshEvent,
    RefreshEventKind::{self, *},
};
use crate::email::Envelope;
use crate::error::*;
use std::convert::TryInto;

impl ImapConnection {
    pub async fn process_untagged(&mut self, line: &[u8]) -> Result<bool> {
        macro_rules! try_fail {
            ($mailbox_hash: expr, $($result:expr $(,)*)+) => {
                $(if let Err(err) = $result {
                    self.uid_store.is_online.lock().unwrap().1 = Err(err.clone());
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
        let mailbox_hash = match self.stream.as_ref()?.current_mailbox {
            MailboxSelection::Select(h) | MailboxSelection::Examine(h) => h,
            MailboxSelection::None => return Ok(false),
        };
        let mailbox =
            std::clone::Clone::clone(&self.uid_store.mailboxes.lock().await[&mailbox_hash]);

        #[cfg(not(feature = "sqlite3"))]
        let mut cache_handle = super::cache::DefaultCache::get(self.uid_store.clone())?;
        #[cfg(feature = "sqlite3")]
        let mut cache_handle = super::cache::Sqlite3Cache::get(self.uid_store.clone())?;
        let mut response = Vec::with_capacity(8 * 1024);
        let untagged_response =
            match super::protocol_parser::untagged_responses(line).map(|(_, v, _)| v) {
                Ok(None) | Err(_) => {
                    return Ok(false);
                }
                Ok(Some(r)) => r,
            };
        match untagged_response {
            UntaggedResponse::Bye { reason } => {
                self.uid_store.is_online.lock().unwrap().1 = Err(reason.into());
            }
            UntaggedResponse::Expunge(n) => {
                if self
                    .uid_store
                    .msn_index
                    .lock()
                    .unwrap()
                    .get(&mailbox_hash)
                    .map(|i| i.len() < n.try_into().unwrap())
                    .unwrap_or(true)
                {
                    debug!(
                        "Received expunge {} but mailbox msn index is {:?}",
                        n,
                        self.uid_store.msn_index.lock().unwrap().get(&mailbox_hash)
                    );
                    return Ok(true);
                }
                let deleted_uid = self
                    .uid_store
                    .msn_index
                    .lock()
                    .unwrap()
                    .entry(mailbox_hash)
                    .or_default()
                    .remove(n.try_into().unwrap());
                debug!("expunge {}, UID = {}", n, deleted_uid);
                let deleted_hash: crate::email::EnvelopeHash = match self
                    .uid_store
                    .uid_index
                    .lock()
                    .unwrap()
                    .remove(&(mailbox_hash, deleted_uid))
                {
                    Some(v) => v,
                    None => return Ok(true),
                };
                self.uid_store
                    .hash_index
                    .lock()
                    .unwrap()
                    .remove(&deleted_hash);
                let mut event: [(UID, RefreshEvent); 1] = [(
                    deleted_uid,
                    RefreshEvent {
                        account_hash: self.uid_store.account_hash,
                        mailbox_hash,
                        kind: Remove(deleted_hash),
                    },
                )];
                if self.uid_store.keep_offline_cache {
                    cache_handle.update(mailbox_hash, &event)?;
                }
                self.add_refresh_event(std::mem::replace(
                    &mut event[0].1,
                    RefreshEvent {
                        account_hash: self.uid_store.account_hash,
                        mailbox_hash,
                        kind: Rescan,
                    },
                ));
            }
            UntaggedResponse::Exists(n) => {
                debug!("exists {}", n);
                try_fail!(
                    mailbox_hash,
                    self.send_command(format!("FETCH {} (UID FLAGS RFC822)", n).as_bytes()).await
                    self.read_response(&mut response, RequiredResponses::FETCH_REQUIRED).await
                );
                match super::protocol_parser::fetch_responses(&response) {
                    Ok((_, v, _)) => {
                        'fetch_responses: for FetchResponse {
                            uid, flags, body, ..
                        } in v
                        {
                            if uid.is_none() || flags.is_none() || body.is_none() {
                                continue;
                            }
                            let uid = uid.unwrap();
                            if self
                                .uid_store
                                .uid_index
                                .lock()
                                .unwrap()
                                .contains_key(&(mailbox_hash, uid))
                            {
                                continue 'fetch_responses;
                            }
                            let env_hash = generate_envelope_hash(&mailbox.imap_path(), &uid);
                            self.uid_store
                                .msn_index
                                .lock()
                                .unwrap()
                                .entry(mailbox_hash)
                                .or_default()
                                .push(uid);
                            if let Ok(mut env) =
                                Envelope::from_bytes(body.unwrap(), flags.as_ref().map(|&(f, _)| f))
                            {
                                env.set_hash(env_hash);
                                self.uid_store
                                    .hash_index
                                    .lock()
                                    .unwrap()
                                    .insert(env_hash, (uid, mailbox_hash));
                                self.uid_store
                                    .uid_index
                                    .lock()
                                    .unwrap()
                                    .insert((mailbox_hash, uid), env_hash);
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
                                mailbox.exists.lock().unwrap().insert_new(env.hash());
                                let mut event: [(UID, RefreshEvent); 1] = [(
                                    uid,
                                    RefreshEvent {
                                        account_hash: self.uid_store.account_hash,
                                        mailbox_hash,
                                        kind: Create(Box::new(env)),
                                    },
                                )];
                                if self.uid_store.keep_offline_cache {
                                    cache_handle.update(mailbox_hash, &event)?;
                                }
                                self.add_refresh_event(std::mem::replace(
                                    &mut event[0].1,
                                    RefreshEvent {
                                        account_hash: self.uid_store.account_hash,
                                        mailbox_hash,
                                        kind: Rescan,
                                    },
                                ));
                            }
                        }
                    }
                    Err(e) => {
                        debug!(e);
                    }
                }
            }
            UntaggedResponse::Recent(_) => {
                try_fail!(
                    mailbox_hash,
                    self.send_command(b"UID SEARCH RECENT").await
                    self.read_response(&mut response, RequiredResponses::SEARCH).await
                );
                match super::protocol_parser::search_results_raw(&response)
                    .map(|(_, v)| v)
                    .map_err(MeliError::from)
                {
                    Ok(&[]) => {
                        debug!("UID SEARCH RECENT returned no results");
                    }
                    Ok(v) => {
                        let command = {
                            let mut iter = v.split(u8::is_ascii_whitespace);
                            let first = iter.next().unwrap_or(v);
                            let mut accum = format!("{}", to_str!(first).trim());
                            for ms in iter {
                                accum = format!("{},{}", accum, to_str!(ms).trim());
                            }
                            format!("UID FETCH {} (FLAGS RFC822)", accum)
                        };
                        try_fail!(
                            mailbox_hash,
                            self.send_command(command.as_bytes()).await
                            self.read_response(&mut response, RequiredResponses::FETCH_REQUIRED).await
                        );
                        debug!(&response);
                        match super::protocol_parser::fetch_responses(&response) {
                            Ok((_, v, _)) => {
                                for FetchResponse {
                                    uid, flags, body, ..
                                } in v
                                {
                                    if uid.is_none() || flags.is_none() || body.is_none() {
                                        continue;
                                    }
                                    let uid = uid.unwrap();
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
                                            let mut event: [(UID, RefreshEvent); 1] = [(
                                                uid,
                                                RefreshEvent {
                                                    account_hash: self.uid_store.account_hash,
                                                    mailbox_hash,
                                                    kind: Create(Box::new(env)),
                                                },
                                            )];
                                            if self.uid_store.keep_offline_cache {
                                                cache_handle.update(mailbox_hash, &event)?;
                                            }
                                            self.add_refresh_event(std::mem::replace(
                                                &mut event[0].1,
                                                RefreshEvent {
                                                    account_hash: self.uid_store.account_hash,
                                                    mailbox_hash,
                                                    kind: Rescan,
                                                },
                                            ));
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
                            to_str!(&response)
                        );
                    }
                }
            }
            UntaggedResponse::Fetch(FetchResponse {
                uid,
                message_sequence_number: msg_seq,
                modseq,
                flags,
                body: _,
                envelope: _,
            }) => {
                if let Some(flags) = flags {
                    let uid = if let Some(uid) = uid {
                        uid
                    } else {
                        try_fail!(
                            mailbox_hash,
                            self.send_command(format!("UID SEARCH {}", msg_seq).as_bytes())
                                .await,
                            self.read_response(&mut response, RequiredResponses::SEARCH)
                                .await,
                        );
                        debug!(to_str!(&response));
                        match super::protocol_parser::search_results(
                            response.split_rn().next().unwrap_or(b""),
                        )
                        .map(|(_, v)| v)
                        {
                            Ok(mut v) if v.len() == 1 => v.pop().unwrap(),
                            Ok(_) => {
                                return Ok(false);
                            }
                            Err(e) => {
                                debug!("SEARCH error failed: {}", e);
                                debug!(to_str!(&response));
                                return Ok(false);
                            }
                        }
                    };
                    debug!("fetch uid {} {:?}", uid, flags);
                    let env_hash = self
                        .uid_store
                        .uid_index
                        .lock()
                        .unwrap()
                        .get(&(mailbox_hash, uid))
                        .copied();
                    if let Some(env_hash) = env_hash {
                        if !flags.0.intersects(crate::email::Flag::SEEN) {
                            mailbox.unseen.lock().unwrap().insert_new(env_hash);
                        } else {
                            mailbox.unseen.lock().unwrap().remove(env_hash);
                        }
                        if let Some(modseq) = modseq {
                            self.uid_store
                                .modseq
                                .lock()
                                .unwrap()
                                .insert(env_hash, modseq);
                        }
                        let mut event: [(UID, RefreshEvent); 1] = [(
                            uid,
                            RefreshEvent {
                                account_hash: self.uid_store.account_hash,
                                mailbox_hash,
                                kind: NewFlags(env_hash, flags),
                            },
                        )];
                        if self.uid_store.keep_offline_cache {
                            cache_handle.update(mailbox_hash, &event)?;
                        }
                        self.add_refresh_event(std::mem::replace(
                            &mut event[0].1,
                            RefreshEvent {
                                account_hash: self.uid_store.account_hash,
                                mailbox_hash,
                                kind: Rescan,
                            },
                        ));
                    };
                }
            }
        }
        Ok(true)
    }
}
