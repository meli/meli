/*
 * melib - IMAP
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

use imap_codec::imap_types::{
    command::FetchModifier,
    fetch::{MacroOrMessageDataItemNames, MessageDataItemName},
    search::SearchKey,
    sequence::SequenceSet,
    status::StatusDataItemName,
};
use indexmap::IndexSet;

use super::*;

pub mod cache;
#[cfg(feature = "sqlite3")]
pub mod sqlite3_cache;

#[cfg(test)]
mod tests;

impl ImapConnection {
    pub async fn resync(&mut self, mailbox_hash: MailboxHash) -> Result<Option<Vec<Envelope>>> {
        if matches!(self.sync_policy, SyncPolicy::None) {
            return Ok(None);
        }

        if self.uid_store.mailbox_state(mailbox_hash)?.is_none() {
            return Ok(None);
        }

        match self.sync_policy {
            SyncPolicy::None => Ok(None),
            SyncPolicy::Basic => self.resync_basic(mailbox_hash).await,
            SyncPolicy::Condstore => self.resync_condstore(mailbox_hash).await,
            SyncPolicy::CondstoreQresync => self.resync_condstoreqresync(mailbox_hash).await,
        }
    }

    /// Re-sync IMAP state by following the strategy described in
    /// [RFC4549](https://datatracker.ietf.org/doc/rfc4549/) "Synchronization Operations for
    /// Disconnected IMAP4 Clients" Section 4.3. Details of "Normal"
    /// Synchronization of a Single Mailbox.
    pub async fn resync_basic(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Option<Vec<Envelope>>> {
        // Plan:
        //
        // 1. Check UIDVALIDITY
        // 2. Discover New Messages and Changes to Old Messages (Section 4.3.1.) i. tag1
        //    UID FETCH <lastseenuid+1>:* <descriptors> (Create events are created from
        //    return value) ii. tag2 UID FETCH 1:<lastseenuid> FLAGS (NewFlags events
        //    must be manually added)
        // 3. Update seen/unseen sets for mailbox
        // 4. Check for removed messages (Remove events must be manually added)
        // 5. Add events
        // 6. Return new envelopes

        // Step 1: Check UIDVALIDITY
        let (Some(cached_uidvalidity), Some(lastseenuid)) = (
            self.uid_store
                .uidvalidity
                .lock()
                .unwrap()
                .get(&mailbox_hash)
                .cloned(),
            self.uid_store
                .lastseenuid
                .lock()
                .unwrap()
                .get(&mailbox_hash)
                .cloned(),
        ) else {
            return Ok(None);
        };
        let mut response = Vec::with_capacity(1024);
        // Fix: was force: true - same redundant-reselect issue as
        // resync_condstore() below, see its comment for the full
        // explanation.
        let select_response = self
            .select_mailbox(mailbox_hash, &mut response, false)
            .await?;
        if select_response.uidvalidity != cached_uidvalidity {
            self.uid_store
                .init_mailbox(mailbox_hash, &select_response)?;
            return Ok(None);
        }

        self.uid_store
            .update_mailbox(mailbox_hash, &select_response)?;

        let (mailbox_path, mailbox_exists, unseen) = {
            let f = &self.uid_store.mailboxes.lock().await[&mailbox_hash];
            (
                f.imap_path().to_string(),
                f.exists.clone(),
                f.unseen.clone(),
            )
        };
        let mut refresh_events = vec![];

        let mut new_envelopes = vec![];
        let mut new_unseen = BTreeSet::default();
        let mut new_seen = BTreeSet::default();
        let mut valid_envs = BTreeSet::default();

        // Step 2. Discover New Messages and Changes to Old Messages

        // Step 2i. Create events

        let (required_responses, attributes) = crate::imap::email::common_attributes();
        self.send_command(CommandBody::fetch(lastseenuid + 1.., attributes, true)?)
            .await?;
        self.read_response(&mut response, required_responses)
            .await?;
        let (_, mut v, _) = protocol_parser::fetch_responses(&response)?;
        //> Also note that a UID range of 559:* always includes the UID of the
        //> last message in the mailbox, even if 559 is higher than any
        //> assigned UID value. This is because the contents of a range are
        //> independent of the order of the range endpoints. Thus, any UID
        //> range with * as one of the endpoints indicates at least one
        //> message (the message with the highest numbered UID), unless the
        //> mailbox is empty.
        //- 6.4.9. UID Command - RFC9051
        v.retain(|f| f.uid != Some(lastseenuid));
        {
            {
                let mut tag_lck = self.uid_store.collection.tag_index.write().unwrap();
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
                    let env_hash = generate_envelope_hash(&mailbox_path, &uid);
                    valid_envs.insert(env_hash);
                    env.set_hash(env_hash);
                    if let Some(value) = references {
                        env.set_references(value);
                    }
                    if let Some((flags, keywords)) = flags {
                        env.set_flags(*flags);
                        if !env.is_seen() {
                            new_unseen.insert(env_hash);
                        } else {
                            new_seen.insert(env_hash);
                        }
                        for f in keywords {
                            let hash = TagHash::from_bytes(f.as_bytes());
                            tag_lck.entry(hash).or_insert_with(|| f.to_string());
                            env.tags_mut().insert(hash);
                        }
                    }
                }
            }

            self.uid_store
                .insert_envelopes(mailbox_hash, &v)
                .chain_err_summary(|| {
                    format!("Could not save envelopes in cache for mailbox {mailbox_path}")
                })?;
        }
        for FetchResponse { envelope, .. } in v {
            let Some(env) = envelope else {
                continue;
            };
            new_envelopes.push(env);
        }

        // Step 2ii. NewFlags events

        let sequence_set = if lastseenuid == 0 {
            SequenceSet::from(..)
        } else {
            SequenceSet::try_from(..=lastseenuid)?
        };
        self.send_command(CommandBody::Fetch {
            sequence_set,
            macro_or_item_names: MacroOrMessageDataItemNames::MessageDataItemNames(vec![
                MessageDataItemName::Flags,
            ]),
            uid: true,
            modifiers: vec![],
        })
        .await?;
        self.read_response(&mut response, RequiredResponses::FETCH_FLAGS)
            .await?;
        let (_, v, _) = protocol_parser::fetch_responses(&response)?;
        {
            let mut env_lck = self.uid_store.envelopes.lock().unwrap();
            let mut tag_lck = self.uid_store.collection.tag_index.write().unwrap();
            for FetchResponse { uid, flags, .. } in v {
                let uid = uid.unwrap();
                let env_hash = generate_envelope_hash(&mailbox_path, &uid);
                let Some(cenv) = env_lck.get_mut(&env_hash) else {
                    continue;
                };
                valid_envs.insert(env_hash);
                if let Some((flags, tags)) = flags {
                    let is_new_flags = !(flags == cenv.inner.flags()
                        && cenv.inner.tags()
                            == &tags
                                .iter()
                                .map(|t| TagHash::from_bytes(t.as_bytes()))
                                .collect::<IndexSet<TagHash>>());
                    cenv.inner.set_flags(flags);
                    if is_new_flags && !cenv.inner.is_seen() {
                        new_unseen.insert(env_hash);
                    } else if is_new_flags {
                        new_seen.insert(env_hash);
                    }
                    cenv.inner.tags_mut().clear();
                    for f in &tags {
                        let hash = TagHash::from_bytes(f.as_bytes());
                        tag_lck.entry(hash).or_insert_with(|| f.to_string());
                        cenv.inner.tags_mut().insert(hash);
                    }
                    if is_new_flags {
                        refresh_events.push((
                            uid,
                            RefreshEvent {
                                mailbox_hash,
                                account_hash: self.uid_store.account_hash,
                                kind: RefreshEventKind::NewFlags(env_hash, (flags, tags)),
                            },
                        ));
                    }
                }
            }
        }

        // Step 3. Update seen/unseen sets for mailbox
        let new_envelopes_hash_set: BTreeSet<_> =
            new_envelopes.iter().map(|env| env.hash()).collect::<_>();
        {
            let mut unseen_lck = unseen.lock().unwrap();
            if unseen_lck.set.is_empty() {
                let new_total = unseen_lck.len() + new_unseen.len();
                unseen_lck.set_not_yet_seen(new_total);
            } else {
                for &seen_env_hash in new_envelopes_hash_set
                    .difference(&new_unseen)
                    .chain(new_seen.iter())
                {
                    unseen_lck.remove(seen_env_hash);
                }

                unseen_lck.insert_set(new_unseen);
            }
        }
        {
            let mut exists_lck = mailbox_exists.lock().unwrap();
            if exists_lck.set.is_empty() {
                let new_total = exists_lck.len() + new_envelopes_hash_set.len();
                exists_lck.set_not_yet_seen(new_total);
            } else {
                exists_lck.insert_set(new_envelopes_hash_set);
            }
        }
        // Step 4. Remove events
        {
            let mut env_lck = self.uid_store.envelopes.lock().unwrap();
            for env_hash in env_lck
                .iter()
                .filter_map(|(h, cenv)| {
                    if cenv.mailbox_hash == mailbox_hash {
                        Some(*h)
                    } else {
                        None
                    }
                })
                .collect::<BTreeSet<EnvelopeHash>>()
                .difference(&valid_envs)
            {
                refresh_events.push((
                    env_lck[env_hash].uid,
                    RefreshEvent {
                        mailbox_hash,
                        account_hash: self.uid_store.account_hash,
                        kind: RefreshEventKind::Remove(*env_hash),
                    },
                ));
                env_lck.remove(env_hash);
            }
        }
        // Step 5. Add events
        self.uid_store.update(mailbox_hash, &refresh_events)?;
        for (_uid, ev) in refresh_events {
            self.add_refresh_event(ev);
        }
        // Step 6. Return new envelopes
        Ok(Some(new_envelopes))
    }

    /// Resync with `CONDSTORE` Extension
    ///
    /// Re-sync IMAP state by following the strategy described in
    /// [RFC4549](https://datatracker.ietf.org/doc/rfc4549/) "Synchronization Operations for
    /// Disconnected IMAP4 Clients", Section 6.1 "CONDSTORE Extension"
    pub async fn resync_condstore(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Option<Vec<Envelope>>> {
        let mut response = Vec::with_capacity(8 * 1024);

        let cached_uidvalidity = self
            .uid_store
            .uidvalidity
            .lock()
            .unwrap()
            .get(&mailbox_hash)
            .cloned();
        let lastseenuid = self
            .uid_store
            .lastseenuid
            .lock()
            .unwrap()
            .get(&mailbox_hash)
            .cloned();
        let cached_highestmodseq = self
            .uid_store
            .highestmodseqs
            .lock()
            .unwrap()
            .get(&mailbox_hash)
            .cloned();

        let (Some(cached_uidvalidity), Some(lastseenuid), Some(cached_highestmodseq)): (
            Option<UID>,
            Option<UID>,
            Option<std::result::Result<ModSequence, ()>>,
        ) = (cached_uidvalidity, lastseenuid, cached_highestmodseq) else {
            // This means the mailbox is not cached.
            return Ok(None);
        };
        let Ok(cached_highestmodseq) = cached_highestmodseq else {
            // No MODSEQ is available for __this__ mailbox, fallback to basic sync
            return self.resync_basic(mailbox_hash).await;
        };

        // 1. check UIDVALIDITY. If fail, discard cache and rebuild
        //
        // Fix: was force: true, which unconditionally re-selects the
        // mailbox even when it's already the connection's current
        // selection. resync() is called from two places: once from
        // ResyncCache (imap/fetch.rs) right after its own init_mailbox()
        // call - that call leaves the connection in EXAMINE state, not
        // SELECT (see init_mailbox()'s own comment below), so force: false
        // doesn't change anything on that path - but also from
        // watch.rs's examine_updates(), which runs periodically on an
        // already-open connection with no preceding re-select. There,
        // force: false lets a real SELECT from a previous cycle be reused
        // instead of re-selecting on every single poll.
        let select_response = self
            .select_mailbox(mailbox_hash, &mut response, false)
            .await?;
        if select_response.uidvalidity != cached_uidvalidity {
            self.uid_store
                .init_mailbox(mailbox_hash, &select_response)?;
            return Ok(None);
        }

        let new_highestmodseq = match select_response.highestmodseq {
            None => return self.resync_basic(mailbox_hash).await,
            Some(Err(_)) => {
                self.uid_store
                    .highestmodseqs
                    .lock()
                    .unwrap()
                    .insert(mailbox_hash, Err(()));
                return self.resync_basic(mailbox_hash).await;
            }
            Some(Ok(v)) => v,
        };

        self.uid_store
            .update_mailbox(mailbox_hash, &select_response)?;

        let (mailbox_path, mailbox_exists, unseen) = {
            let f = &self.uid_store.mailboxes.lock().await[&mailbox_hash];
            (
                f.imap_path().to_string(),
                f.exists.clone(),
                f.unseen.clone(),
            )
        };

        let mut refresh_events = vec![];
        let mut new_envelopes = vec![];
        let mut new_unseen = BTreeSet::default();
        let mut new_seen = BTreeSet::default();
        let mut valid_envs = BTreeSet::default();

        // 1b) Check the mailbox HIGHESTMODSEQ.
        //  If the cached value is the same as the one returned by the server, skip
        // fetching  message flags on step 2-II, i.e., the client only has to
        // find out which messages got  expunged.
        if cached_highestmodseq != new_highestmodseq {
            /* Cache is synced, only figure out which messages got expunged */

            // 2) Fetch the current "descriptors".
            //   I)  Discover new messages.

            //   II) Discover changes to old messages and flags for new messages
            //       using
            //       "FETCH 1:* (FLAGS) (CHANGEDSINCE <cached-value>)" or
            //       "SEARCH MODSEQ <cached-value>".

            // 2. tag1 UID FETCH <lastseenuid+1>:* <descriptors>
            let (required_responses, macro_or_item_names) = crate::imap::email::common_attributes();
            self.send_command(CommandBody::Fetch {
                sequence_set: ((lastseenuid + 1)..).try_into()?,
                macro_or_item_names,
                uid: true,
                modifiers: vec![FetchModifier::ChangedSince(cached_highestmodseq.into())],
            })
            .await?;
            self.read_response(
                &mut response,
                required_responses | RequiredResponses::FETCH_MODSEQ,
            )
            .await?;
            let (_, mut v, _) = protocol_parser::fetch_responses(&response)?;
            //> Also note that a UID range of 559:* always includes the UID of the
            //> last message in the mailbox, even if 559 is higher than any
            //> assigned UID value. This is because the contents of a range are
            //> independent of the order of the range endpoints. Thus, any UID
            //> range with * as one of the endpoints indicates at least one
            //> message (the message with the highest numbered UID), unless the
            //> mailbox is empty.
            //- 6.4.9. UID Command - RFC9051
            v.retain(|f| f.uid != Some(lastseenuid));
            {
                {
                    let mut tag_lck = self.uid_store.collection.tag_index.write().unwrap();
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
                        let env_hash = generate_envelope_hash(&mailbox_path, &uid);
                        env.set_hash(env_hash);
                        if let Some(value) = references {
                            env.set_references(value);
                        }
                        if let Some((flags, keywords)) = flags {
                            env.set_flags(*flags);
                            if !env.is_seen() {
                                new_unseen.insert(env.hash());
                            } else {
                                new_seen.insert(env.hash());
                            }
                            for f in keywords {
                                let hash = TagHash::from_bytes(f.as_bytes());
                                tag_lck.entry(hash).or_insert_with(|| f.to_string());
                                env.tags_mut().insert(hash);
                            }
                        }
                    }
                }
                self.uid_store
                    .insert_envelopes(mailbox_hash, &v)
                    .chain_err_summary(|| {
                        format!("Could not save envelopes in cache for mailbox {mailbox_path}")
                    })?;
            }

            for FetchResponse { envelope, .. } in v {
                let Some(env) = envelope else {
                    continue;
                };
                new_envelopes.push(env);
            }
            // 3. Fetch the bodies of any "interesting" messages that the client doesn't
            //    already have.
            // 3. tag2 UID FETCH 1:<lastseenuid> FLAGS

            let sequence_set = if lastseenuid == 0 {
                (1..).try_into()?
            } else {
                (1..lastseenuid).try_into()?
            };
            self.send_command(CommandBody::Fetch {
                sequence_set,
                macro_or_item_names: MacroOrMessageDataItemNames::MessageDataItemNames(vec![
                    MessageDataItemName::Uid,
                    MessageDataItemName::Flags,
                ]),
                uid: true,
                modifiers: vec![FetchModifier::ChangedSince(cached_highestmodseq.into())],
            })
            .await?;
            self.read_response(
                &mut response,
                RequiredResponses::FETCH_FLAGS | RequiredResponses::FETCH_MODSEQ,
            )
            .await?;
            // 1) update cached flags for old messages;
            let mut env_lck = self.uid_store.envelopes.lock().unwrap();
            let mut tag_lck = self.uid_store.collection.tag_index.write().unwrap();
            let (_, v, _) = protocol_parser::fetch_responses(&response)?;
            {
                for FetchResponse { uid, flags, .. } in v {
                    let uid = uid.unwrap();
                    let env_hash = generate_envelope_hash(&mailbox_path, &uid);
                    let Some(cenv) = env_lck.get_mut(&env_hash) else {
                        continue;
                    };
                    valid_envs.insert(env_hash);
                    if let Some((flags, tags)) = flags {
                        let is_new_flags = !(flags == cenv.inner.flags()
                            && cenv.inner.tags()
                                == &tags
                                    .iter()
                                    .map(|t| TagHash::from_bytes(t.as_bytes()))
                                    .collect::<IndexSet<TagHash>>());
                        cenv.inner.set_flags(flags);
                        if is_new_flags && !cenv.inner.is_seen() {
                            new_unseen.insert(env_hash);
                        } else if is_new_flags {
                            new_seen.insert(env_hash);
                        }
                        cenv.inner.tags_mut().clear();
                        for f in &tags {
                            let hash = TagHash::from_bytes(f.as_bytes());
                            tag_lck.entry(hash).or_insert_with(|| f.to_string());
                            cenv.inner.tags_mut().insert(hash);
                        }
                        if is_new_flags {
                            refresh_events.push((
                                uid,
                                RefreshEvent {
                                    mailbox_hash,
                                    account_hash: self.uid_store.account_hash,
                                    kind: RefreshEventKind::NewFlags(env_hash, (flags, tags)),
                                },
                            ));
                        }
                    }
                }
            }
            self.uid_store
                .highestmodseqs
                .lock()
                .unwrap()
                .insert(mailbox_hash, Ok(new_highestmodseq));
            // Step 3. Update seen/unseen sets for mailbox
            let new_envelopes_hash_set: BTreeSet<_> =
                new_envelopes.iter().map(|env| env.hash()).collect::<_>();
            {
                let mut unseen_lck = unseen.lock().unwrap();
                if unseen_lck.set.is_empty() {
                    let new_total = unseen_lck.len() + new_unseen.len();
                    unseen_lck.set_not_yet_seen(new_total);
                } else {
                    for &seen_env_hash in new_envelopes_hash_set
                        .difference(&new_unseen)
                        .chain(new_seen.iter())
                    {
                        unseen_lck.remove(seen_env_hash);
                    }

                    unseen_lck.insert_set(new_unseen);
                }
            }
            {
                let mut exists_lck = mailbox_exists.lock().unwrap();
                if exists_lck.set.is_empty() {
                    let new_total = exists_lck.len() + new_envelopes_hash_set.len();
                    exists_lck.set_not_yet_seen(new_total);
                } else {
                    exists_lck.insert_set(new_envelopes_hash_set);
                }
            }
        }
        {
            // EXPUNGE events.
            // This should be UID SEARCH 1:<maxuid> but it's difficult to compare to cached
            // UIDs at the point of calling this function
            self.send_command(CommandBody::search(None, SearchKey::All.into(), true))
                .await?;
            self.read_response(&mut response, RequiredResponses::SEARCH)
                .await?;
            // 1) update cached flags for old messages;
            let (_, v) = protocol_parser::search_results(response.as_slice())?;
            for uid in v {
                valid_envs.insert(generate_envelope_hash(&mailbox_path, &uid));
            }
            {
                let mut env_lck = self.uid_store.envelopes.lock().unwrap();
                let olds = env_lck
                    .iter()
                    .filter_map(|(h, cenv)| {
                        if cenv.mailbox_hash == mailbox_hash {
                            Some(*h)
                        } else {
                            None
                        }
                    })
                    .collect::<BTreeSet<EnvelopeHash>>();
                for env_hash in olds.difference(&valid_envs) {
                    refresh_events.push((
                        env_lck[env_hash].uid,
                        RefreshEvent {
                            mailbox_hash,
                            account_hash: self.uid_store.account_hash,
                            kind: RefreshEventKind::Remove(*env_hash),
                        },
                    ));
                    env_lck.remove(env_hash);
                }
                drop(env_lck);
            }
        }
        // Step 5. Add events
        self.uid_store.update(mailbox_hash, &refresh_events)?;
        for (_uid, ev) in refresh_events {
            self.add_refresh_event(ev);
        }
        // Step 6. Return new envelopes
        Ok(Some(new_envelopes))
    }

    /// Resync with `CONDSTORE` and `QRESYNC` Extension
    ///
    /// Re-sync IMAP state by following the strategy described in
    /// [RFC7162](https://datatracker.ietf.org/doc/rfc7162/) "Quick Flag Changes Resynchronization (CONDSTORE) and Quick
    /// Mailbox Resynchronization (QRESYNC)"
    pub async fn resync_condstoreqresync(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Option<Vec<Envelope>>> {
        log::trace!(
            "resync_condstoreqresync: mailbox_hash: {:?}, function unimplemented",
            mailbox_hash
        );
        self.resync_condstore(mailbox_hash).await
    }

    pub async fn init_mailbox(&mut self, mailbox_hash: MailboxHash) -> Result<SelectResponse> {
        let mut response = Vec::with_capacity(8 * 1024);
        let (mailbox_path, mailbox_exists, permissions) = {
            let f = &self.uid_store.mailboxes.lock().await[&mailbox_hash];
            (
                f.imap_path().to_string(),
                f.exists.clone(),
                f.permissions.clone(),
            )
        };

        /* first SELECT the mailbox to get READ/WRITE permissions (because EXAMINE
         * only returns READ-ONLY for both cases) */
        let mut select_response = self
            .select_mailbox(mailbox_hash, &mut response, true)
            .await?;
        {
            {
                let mut uidvalidities = self.uid_store.uidvalidity.lock().unwrap();

                let v = uidvalidities
                    .entry(mailbox_hash)
                    .or_insert(select_response.uidvalidity);
                *v = select_response.uidvalidity;
            }
            {
                if let Some(highestmodseq) = select_response.highestmodseq {
                    let mut highestmodseqs = self.uid_store.highestmodseqs.lock().unwrap();
                    let v = highestmodseqs.entry(mailbox_hash).or_insert(highestmodseq);
                    *v = highestmodseq;
                }
            }
            let mut permissions = permissions.lock().unwrap();
            permissions.create_messages = !select_response.read_only;
            permissions.remove_messages = !select_response.read_only;
            permissions.set_flags = !select_response.read_only;
            permissions.rename_messages = !select_response.read_only;
            permissions.delete_messages = !select_response.read_only;
            {
                let mut mailbox_exists_lck = mailbox_exists.lock().unwrap();
                mailbox_exists_lck.clear();
                mailbox_exists_lck.set_not_yet_seen(select_response.exists);
            }
        }
        if select_response.exists == 0 {
            return Ok(select_response);
        }
        /* reselecting the same mailbox with EXAMINE prevents expunging it */
        self.examine_mailbox(mailbox_hash, &mut response, true)
            .await?;
        if select_response.uidnext == 0 {
            /* UIDNEXT shouldn't be 0, since exists != 0 at this point */
            self.send_command(CommandBody::status(
                mailbox_path,
                [StatusDataItemName::UidNext].as_slice(),
            )?)
            .await?;
            self.read_response(&mut response, RequiredResponses::STATUS)
                .await?;
            let (_, status) = protocol_parser::status_response(response.as_slice())?;
            if let Some(uidnext) = status.uidnext {
                if uidnext == 0 {
                    return Err(Error::new(
                        "IMAP server error: zero UIDNEXT with nonzero exists.",
                    ));
                }
                select_response.uidnext = uidnext;
            } else {
                return Err(Error::new("IMAP server did not reply with UIDNEXT"));
            }
        }
        Ok(select_response)
    }
}
