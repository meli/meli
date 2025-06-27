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

use std::convert::{TryFrom, TryInto};

use imap_codec::imap_types::{command::CommandBody, search::SearchKey, sequence::SequenceSet};

use crate::{
    backends::{BackendEvent, BackendMailbox, RefreshEvent, RefreshEventKind::*, TagHash},
    error::*,
    imap::{
        email::common_attributes,
        protocol_parser::{
            generate_envelope_hash, FetchResponse, ImapLineSplit, RequiredResponses,
            UntaggedResponse,
        },
        sync::cache::{ignore_not_found, ImapCache},
        ImapConnection, MailboxSelection, UID,
    },
};

impl ImapConnection {
    pub async fn process_untagged(
        &mut self,
        untagged_response: UntaggedResponse<'_>,
    ) -> Result<Option<BackendEvent>> {
        macro_rules! try_fail {
            ($mailbox_hash: expr, $($result:expr $(,)*)+) => {
                $(if let Err(err) = $result {
                    self.uid_store.is_online.lock().unwrap().1 = Err(err.clone());
                    imap_log!(trace, self, "failure: {}", err.to_string());
                    log::debug!("failure: {}", err.to_string());
                    Err(err)
                } else { Ok(()) }?;)+
            };
        }
        let mailbox_hash = match self.stream.as_ref()?.current_mailbox {
            MailboxSelection::Select {
                mailbox_hash: h, ..
            }
            | MailboxSelection::Examine {
                mailbox_hash: h, ..
            } => h,
            MailboxSelection::None => return Ok(None),
        };
        let mailbox =
            std::clone::Clone::clone(&self.uid_store.mailboxes.lock().await[&mailbox_hash]);

        let mut response = Vec::with_capacity(8 * 1024);
        match untagged_response {
            UntaggedResponse::Bye { reason } => {
                self.uid_store.is_online.lock().unwrap().1 = Err(reason.into());
                Err(reason.into())
            }
            UntaggedResponse::Expunge(n) => {
                if self
                    .uid_store
                    .msn_index
                    .lock()
                    .unwrap()
                    .get(&mailbox_hash)
                    .map(|i| i.len() < TryInto::<usize>::try_into(n).unwrap())
                    .unwrap_or(true)
                {
                    debug!(
                        "Received expunge {} but mailbox msn index is {:?}",
                        n,
                        self.uid_store.msn_index.lock().unwrap().get(&mailbox_hash)
                    );
                    self.send_command(CommandBody::search(
                        None,
                        SearchKey::SequenceSet(SequenceSet::from(..)).into(),
                        true,
                    ))
                    .await?;
                    self.read_response(&mut response, RequiredResponses::SEARCH)
                        .await?;
                    let results = super::protocol_parser::search_results(&response)?
                        .1
                        .into_iter()
                        .collect::<std::collections::BTreeSet<UID>>();
                    {
                        let mut lck = self.uid_store.msn_index.lock().unwrap();
                        let msn_index = lck.entry(mailbox_hash).or_default();
                        msn_index.clear();
                        msn_index.extend(
                            super::protocol_parser::search_results(&response)?
                                .1
                                .into_iter()
                                .enumerate(),
                        );
                    }
                    let mut uid_events = vec![];
                    {
                        let mut mboxes = self.uid_store.mailboxes.lock().await;
                        let deleted_uids_hashes = self
                            .uid_store
                            .uid_index
                            .lock()
                            .unwrap()
                            .iter()
                            .filter(|((mbx, u), _)| *mbx == mailbox_hash && !results.contains(u))
                            .map(|((_, uid), hash)| (*uid, *hash))
                            .collect::<Vec<(UID, crate::email::EnvelopeHash)>>();
                        for (deleted_uid, deleted_hash) in deleted_uids_hashes {
                            self.uid_store
                                .uid_index
                                .lock()
                                .unwrap()
                                .remove(&(mailbox_hash, deleted_uid));
                            {
                                if let Some(mbx) = mboxes.get_mut(&mailbox_hash) {
                                    mbx.exists.lock().unwrap().remove(deleted_hash);
                                    mbx.unseen.lock().unwrap().remove(deleted_hash);
                                }
                            }
                            self.uid_store
                                .hash_index
                                .lock()
                                .unwrap()
                                .remove(&deleted_hash);
                            uid_events.push((
                                deleted_uid,
                                RefreshEvent {
                                    account_hash: self.uid_store.account_hash,
                                    mailbox_hash,
                                    kind: Remove(deleted_hash),
                                },
                            ));
                        }
                    }
                    if let Err(err) = self
                        .uid_store
                        .update(mailbox_hash, &uid_events)
                        .or_else(ignore_not_found)
                    {
                        log::error!(
                            "Could not update cache for mailbox_hash = {:?} uid, events = {:?}: \
                             err = {}",
                            mailbox_hash,
                            uid_events,
                            err
                        );
                    }
                    return Ok(uid_events
                        .into_iter()
                        .map(|(_, ev)| ev)
                        .collect::<Vec<_>>()
                        .try_into()
                        .ok());
                }
                let Some(deleted_uid) = self
                    .uid_store
                    .msn_index
                    .lock()
                    .unwrap()
                    .entry(mailbox_hash)
                    .or_default()
                    .remove(&TryInto::<usize>::try_into(n).unwrap().saturating_sub(1))
                else {
                    return Ok(None);
                };
                imap_log!(trace, self, "expunge {}, UID = {}", n, deleted_uid);
                let Some(deleted_hash) = self
                    .uid_store
                    .uid_index
                    .lock()
                    .unwrap()
                    .remove(&(mailbox_hash, deleted_uid))
                else {
                    return Ok(None);
                };
                {
                    let mut mboxes = self.uid_store.mailboxes.lock().await;
                    if let Some(mbx) = mboxes.get_mut(&mailbox_hash) {
                        mbx.exists.lock().unwrap().remove(deleted_hash);
                        mbx.unseen.lock().unwrap().remove(deleted_hash);
                    }
                }
                self.uid_store
                    .hash_index
                    .lock()
                    .unwrap()
                    .remove(&deleted_hash);
                let pair = [(
                    deleted_uid,
                    RefreshEvent {
                        account_hash: self.uid_store.account_hash,
                        mailbox_hash,
                        kind: Remove(deleted_hash),
                    },
                )];

                if let Err(err) = self
                    .uid_store
                    .update(mailbox_hash, &pair)
                    .or_else(ignore_not_found)
                {
                    log::error!(
                        "Could not update cache for mailbox_hash = {:?} uid = {:?}, events = \
                         {:?}: err = {}",
                        mailbox_hash,
                        deleted_uid,
                        pair,
                        err
                    );
                }
                let [(_, event)] = pair;
                Ok(Some(event.into()))
            }
            UntaggedResponse::Exists(n) => {
                imap_log!(trace, self, "exists {}", n);
                let (required_responses, attributes) = common_attributes();
                try_fail!(
                    mailbox_hash,
                    self.send_command(CommandBody::fetch(n, attributes, false)?).await
                    self.read_response(&mut response, required_responses).await
                );
                let mut v = match super::protocol_parser::fetch_responses(&response) {
                    Ok((_, v, _)) => v,
                    Err(err) => {
                        imap_log!(
                            trace,
                            self,
                            "Error when parsing FETCH response after untagged exists {:?}",
                            err
                        );
                        return Ok(None);
                    }
                };
                imap_log!(trace, self, "responses len is {}", v.len());
                for FetchResponse {
                    ref uid,
                    ref mut envelope,
                    ref mut flags,
                    ref references,
                    ref message_sequence_number,
                    ..
                } in &mut v
                {
                    if uid.is_none() || flags.is_none() || envelope.is_none() {
                        continue;
                    }
                    let uid = uid.unwrap();
                    let env = envelope.as_mut().unwrap();
                    env.set_hash(generate_envelope_hash(mailbox.imap_path(), &uid));
                    if let Some(value) = references {
                        env.set_references(value);
                    }
                    let mut tag_lck = self.uid_store.collection.tag_index.write().unwrap();
                    if let Some((flags, keywords)) = flags {
                        env.set_flags(*flags);
                        if !env.is_seen() {
                            mailbox.unseen.lock().unwrap().insert_new(env.hash());
                        }
                        for f in keywords {
                            let hash = TagHash::from_bytes(f.as_bytes());
                            tag_lck.entry(hash).or_insert_with(|| f.to_string());
                            env.tags_mut().insert(hash);
                        }
                    }
                    mailbox.exists.lock().unwrap().insert_new(env.hash());
                    if !self
                        .uid_store
                        .uid_index
                        .lock()
                        .unwrap()
                        .contains_key(&(mailbox_hash, uid))
                    {
                        self.uid_store
                            .msn_index
                            .lock()
                            .unwrap()
                            .entry(mailbox_hash)
                            .or_default()
                            .insert(message_sequence_number.saturating_sub(1), uid);
                    }
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
                    imap_log!(
                        trace,
                        self,
                        "Create event {} {} {}",
                        env.hash(),
                        env.subject(),
                        mailbox.path(),
                    );
                }
                {
                    if let Err(err) = self
                        .uid_store
                        .insert_envelopes(mailbox_hash, &v)
                        .or_else(ignore_not_found)
                        .chain_err_summary(|| {
                            format!(
                                "Could not save envelopes in cache for mailbox {}",
                                &mailbox.imap_path()
                            )
                        })
                    {
                        imap_log!(info, self, "{}", err);
                    }
                }
                let mut events = vec![];
                for response in v {
                    if let FetchResponse {
                        envelope: Some(envelope),
                        ..
                    } = response
                    {
                        events.push(RefreshEvent {
                            account_hash: self.uid_store.account_hash,
                            mailbox_hash,
                            kind: Create(Box::new(envelope)),
                        });
                    }
                }
                Ok(events.try_into().ok())
            }
            UntaggedResponse::Recent(_) => {
                try_fail!(
                    mailbox_hash,
                    self.send_command(CommandBody::search(None, SearchKey::Recent.into(), true)).await
                    self.read_response(&mut response, RequiredResponses::SEARCH).await
                );
                match super::protocol_parser::search_results_raw(&response)
                    .map(|(_, v)| v)
                    .map_err(Error::from)
                {
                    Ok(&[]) => {
                        imap_log!(trace, self, "UID SEARCH RECENT returned no results");
                        Ok(None)
                    }
                    Ok(v) => {
                        // [ref:FIXME]: use imap_codec types instead of a raw command
                        let command = {
                            let mut iter = v.split(u8::is_ascii_whitespace);
                            let first = iter.next().unwrap_or(v);
                            let mut accum = to_str!(first).trim().to_string();
                            for ms in iter {
                                accum.push(',');
                                accum.push_str(to_str!(ms).trim());
                            }
                            format!(
                                "UID FETCH {accum} (UID FLAGS ENVELOPE BODY.PEEK[HEADER.FIELDS \
                                 (REFERENCES)] BODYSTRUCTURE)"
                            )
                        };
                        try_fail!(
                            mailbox_hash,
                            self.send_command_raw(command.as_bytes()).await,
                            self.read_response(
                                &mut response,
                                RequiredResponses::FETCH_UID
                                    | RequiredResponses::FETCH_FLAGS
                                    | RequiredResponses::FETCH_ENVELOPE
                                    | RequiredResponses::FETCH_REFERENCES
                                    | RequiredResponses::FETCH_BODYSTRUCTURE,
                            )
                            .await,
                        );
                        let mut v = match super::protocol_parser::fetch_responses(&response) {
                            Ok((_, v, _)) => v,
                            Err(err) => {
                                imap_log!(
                                    debug,
                                    self,
                                    "Error when parsing FETCH response after untagged recent {:?}",
                                    err
                                );
                                return Ok(None);
                            }
                        };
                        imap_log!(trace, self, "responses len is {}", v.len());
                        for FetchResponse {
                            ref uid,
                            ref mut envelope,
                            ref mut flags,
                            ref references,
                            ..
                        } in &mut v
                        {
                            if uid.is_none() || flags.is_none() || envelope.is_none() {
                                continue;
                            }
                            let uid = uid.unwrap();
                            let env = envelope.as_mut().unwrap();
                            env.set_hash(generate_envelope_hash(mailbox.imap_path(), &uid));
                            if let Some(value) = references {
                                env.set_references(value);
                            }
                            let mut tag_lck = self.uid_store.collection.tag_index.write().unwrap();
                            if let Some((flags, keywords)) = flags {
                                env.set_flags(*flags);
                                if !env.is_seen() {
                                    mailbox.unseen.lock().unwrap().insert_new(env.hash());
                                }
                                for f in keywords {
                                    let hash = TagHash::from_bytes(f.as_bytes());
                                    tag_lck.entry(hash).or_insert_with(|| f.to_string());
                                    env.tags_mut().insert(hash);
                                }
                            }
                            mailbox.exists.lock().unwrap().insert_new(env.hash());
                        }
                        {
                            if let Err(err) = self
                                .uid_store
                                .insert_envelopes(mailbox_hash, &v)
                                .or_else(ignore_not_found)
                                .chain_err_summary(|| {
                                    format!(
                                        "Could not save envelopes in cache for mailbox {}",
                                        &mailbox.imap_path()
                                    )
                                })
                            {
                                log::info!("{err}");
                            }
                        }
                        let mut events = vec![];
                        for response in v {
                            if let FetchResponse {
                                envelope: Some(envelope),
                                uid: Some(uid),
                                message_sequence_number,
                                ..
                            } = response
                            {
                                if !self
                                    .uid_store
                                    .uid_index
                                    .lock()
                                    .unwrap()
                                    .contains_key(&(mailbox_hash, uid))
                                {
                                    self.uid_store
                                        .msn_index
                                        .lock()
                                        .unwrap()
                                        .entry(mailbox_hash)
                                        .or_default()
                                        .insert(message_sequence_number.saturating_sub(1), uid);
                                }
                                self.uid_store
                                    .hash_index
                                    .lock()
                                    .unwrap()
                                    .insert(envelope.hash(), (uid, mailbox_hash));
                                self.uid_store
                                    .uid_index
                                    .lock()
                                    .unwrap()
                                    .insert((mailbox_hash, uid), envelope.hash());
                                debug!(
                                    "Create event {} {} {}",
                                    envelope.hash(),
                                    envelope.subject(),
                                    mailbox.path(),
                                );
                                events.push(RefreshEvent {
                                    account_hash: self.uid_store.account_hash,
                                    mailbox_hash,
                                    kind: Create(Box::new(envelope)),
                                });
                            }
                        }
                        Ok(events.try_into().ok())
                    }
                    Err(err) => {
                        debug!(
                            "UID SEARCH RECENT err: {}\nresp: {}",
                            err,
                            to_str!(&response)
                        );
                        Ok(None)
                    }
                }
            }
            UntaggedResponse::Fetch(fetch) => {
                let FetchResponse {
                    uid,
                    message_sequence_number: msg_seq,
                    modseq,
                    flags,
                    body: _,
                    references: _,
                    envelope: _,
                    bodystructure: _,
                    raw_fetch_value: _,
                } = *fetch;
                if let Some(flags) = flags {
                    let uid = if let Some(uid) = uid {
                        uid
                    } else {
                        try_fail!(
                            mailbox_hash,
                            self.send_command(CommandBody::search(
                                None,
                                SearchKey::SequenceSet(SequenceSet::try_from(msg_seq)?).into(),
                                true
                            ))
                            .await,
                            self.read_response(&mut response, RequiredResponses::SEARCH)
                                .await,
                        );
                        match super::protocol_parser::search_results(
                            response.split_rn().next().unwrap_or(b""),
                        )
                        .map(|(_, v)| v)
                        {
                            Ok(mut v) if v.len() == 1 => v.pop().unwrap(),
                            Ok(_) => {
                                return Ok(None);
                            }
                            Err(e) => {
                                debug!("SEARCH error failed: {}", e);
                                debug!(to_str!(&response));
                                return Ok(None);
                            }
                        }
                    };
                    debug!("fetch uid {} {:?}", uid, flags);
                    if let Some(env_hash) = {
                        let temp = self
                            .uid_store
                            .uid_index
                            .lock()
                            .unwrap()
                            .get(&(mailbox_hash, uid))
                            .copied();
                        temp
                    } {
                        if !flags.0.intersects(crate::email::Flag::SEEN) {
                            for mbx in self.uid_store.mailboxes.lock().await.values_mut() {
                                if mbx.exists.lock().unwrap().contains(&env_hash) {
                                    mbx.unseen.lock().unwrap().insert_new(env_hash);
                                }
                            }
                        } else {
                            for mbx in self.uid_store.mailboxes.lock().await.values_mut() {
                                mbx.unseen.lock().unwrap().remove(env_hash);
                            }
                        }
                        mailbox.exists.lock().unwrap().insert_new(env_hash);
                        if let Some(modseq) = modseq {
                            self.uid_store
                                .modseq
                                .lock()
                                .unwrap()
                                .insert(env_hash, modseq);
                        }
                        let event: [(UID, RefreshEvent); 1] = [(
                            uid,
                            RefreshEvent {
                                account_hash: self.uid_store.account_hash,
                                mailbox_hash,
                                kind: NewFlags(env_hash, flags),
                            },
                        )];
                        {
                            self.uid_store.update(mailbox_hash, &event)?;
                        }
                        let [(_, ev)] = event;
                        Ok(Some(ev.into()))
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            }
        }
    }
}
