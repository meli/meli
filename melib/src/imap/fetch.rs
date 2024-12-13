//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use super::*;

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum FetchStage {
    #[default]
    InitialFresh,
    InitialCache {
        max_uid: Option<UID>,
    },
    ResyncCache,
    FreshFetch {
        max_uid: UID,
    },
    Finished,
}

#[derive(Debug)]
pub struct FetchState {
    pub stage: FetchStage,
    pub connection: Arc<ConnectionMutex>,
    pub mailbox_hash: MailboxHash,
    pub uid_store: Arc<UIDStore>,
    pub batch_size: usize,
    pub cache_batch_size: usize,
}

impl FetchState {
    pub async fn chunk(&mut self) -> Result<Vec<Envelope>> {
        let mut resync_payload: Option<Vec<Envelope>> = None;
        loop {
            match self.stage {
                FetchStage::InitialFresh => {
                    let select_response = self
                        .connection
                        .lock()
                        .await?
                        .init_mailbox(self.mailbox_hash)
                        .await?;
                    _ = self
                        .uid_store
                        .update_mailbox(self.mailbox_hash, &select_response);

                    if select_response.exists == 0 {
                        self.stage = FetchStage::Finished;
                        return Ok(Vec::new());
                    }
                    self.stage = FetchStage::FreshFetch {
                        max_uid: select_response.uidnext,
                    };
                    continue;
                }
                FetchStage::InitialCache { max_uid: None } => {
                    let select_response = self
                        .connection
                        .lock()
                        .await?
                        .init_mailbox(self.mailbox_hash)
                        .await?;
                    if let Err(err) = self
                        .uid_store
                        .update_mailbox(self.mailbox_hash, &select_response)
                        .chain_err_summary(|| {
                            format!("Could not update cache for mailbox {}.", self.mailbox_hash)
                        })
                    {
                        (self.uid_store.event_consumer)(self.uid_store.account_hash, err.into());
                    }
                    match self.max_uid() {
                        Ok(Some(max_uid)) => {
                            self.stage = FetchStage::InitialCache {
                                max_uid: Some(max_uid),
                            };
                            continue;
                        }
                        Ok(None) => {}
                        Err(err) => {
                            imap_log!(
                                error,
                                self.connection.lock().await?,
                                "IMAP cache error: could not fetch cache for {}. Reason: {}",
                                self.uid_store.account_name,
                                err
                            );
                            // Try resetting the database
                            if let Err(err) = self.uid_store.reset() {
                                imap_log!(
                                    error,
                                    self.connection.lock().await?,
                                    "IMAP cache error: could not reset cache for {}. Reason: {}",
                                    self.uid_store.account_name,
                                    err
                                );
                            }
                        }
                    }
                    self.stage = FetchStage::InitialFresh;
                    continue;
                }
                FetchStage::InitialCache {
                    max_uid: Some(max_uid),
                } => {
                    match self.cached_envs(max_uid).await {
                        Ok(Some(cached_payload)) => {
                            self.stage = match max_uid.saturating_sub(self.cache_batch_size) {
                                0 => FetchStage::Finished,
                                other => FetchStage::InitialCache {
                                    max_uid: Some(other),
                                },
                            };
                            let (mailbox_exists, unseen) = {
                                let f = &self.uid_store.mailboxes.lock().await[&self.mailbox_hash];
                                (Arc::clone(&f.exists), Arc::clone(&f.unseen))
                            };
                            unseen.lock().unwrap().insert_existing_set(
                                cached_payload
                                    .iter()
                                    .filter_map(|env| {
                                        if !env.is_seen() {
                                            Some(env.hash())
                                        } else {
                                            None
                                        }
                                    })
                                    .collect(),
                            );
                            mailbox_exists.lock().unwrap().insert_existing_set(
                                cached_payload.iter().map(|env| env.hash()).collect::<_>(),
                            );
                            if let Some(mut resync_payload) = resync_payload.take() {
                                resync_payload.extend(cached_payload.into_iter());
                                return Ok(resync_payload);
                            }
                            return Ok(cached_payload);
                        }
                        Err(err) => {
                            imap_log!(
                                error,
                                self.connection.lock().await?,
                                "IMAP cache error: could not fetch cache for {}. Reason: {}",
                                self.uid_store.account_name,
                                err
                            );
                            // Try resetting the database
                            if let Err(err) = self.uid_store.reset() {
                                imap_log!(
                                    error,
                                    self.connection.lock().await?,
                                    "IMAP cache error: could not reset cache for {}. Reason: {}",
                                    self.uid_store.account_name,
                                    err
                                );
                            }
                            self.stage = FetchStage::InitialFresh;
                            continue;
                        }
                        Ok(None) => {
                            self.stage = FetchStage::InitialFresh;
                            continue;
                        }
                    }
                }
                FetchStage::ResyncCache => {
                    let mut conn = self.connection.lock().await?;
                    let select_response = conn.init_mailbox(self.mailbox_hash).await?;
                    match self
                        .uid_store
                        .update_mailbox(self.mailbox_hash, &select_response)
                    {
                        Err(err) if err.kind.is_not_found() => {
                            _ = self
                                .uid_store
                                .init_mailbox(self.mailbox_hash, &select_response);
                        }
                        Err(err) => {
                            (self.uid_store.event_consumer)(
                                self.uid_store.account_hash,
                                err.set_summary(format!(
                                    "Could not update cache for mailbox {}.",
                                    self.mailbox_hash
                                ))
                                .into(),
                            );
                        }
                        Ok(()) => {
                            let mailbox_hash = self.mailbox_hash;
                            let res = conn.resync(mailbox_hash).await;
                            if let Ok(Some(payload)) = res {
                                self.stage = FetchStage::InitialCache { max_uid: None };
                                resync_payload = Some(payload);
                                continue;
                            }
                        }
                    }
                    self.stage = FetchStage::InitialFresh;
                    continue;
                }
                FetchStage::FreshFetch { max_uid } => {
                    let Self {
                        ref mut stage,
                        ref connection,
                        mailbox_hash,
                        ref uid_store,
                        batch_size,
                        cache_batch_size: _,
                    } = self;
                    let mailbox_hash = *mailbox_hash;
                    let mut our_unseen: BTreeSet<EnvelopeHash> = BTreeSet::default();
                    let (mailbox_path, mailbox_exists, no_select, unseen) = {
                        let f = &uid_store.mailboxes.lock().await[&mailbox_hash];
                        (
                            f.imap_path().to_string(),
                            Arc::clone(&f.exists),
                            f.no_select,
                            Arc::clone(&f.unseen),
                        )
                    };
                    if no_select {
                        self.stage = FetchStage::Finished;
                        return Ok(Vec::new());
                    }
                    let mut conn = connection.lock().await?;
                    let mut response = Vec::with_capacity(8 * 1024);
                    let max_uid_left = max_uid;

                    let mut envelopes = Vec::with_capacity(*batch_size);
                    conn.examine_mailbox(mailbox_hash, &mut response, false)
                        .await?;
                    if max_uid_left > 0 {
                        let sequence_set = if max_uid_left == 1 {
                            SequenceSet::from(ONE)
                        } else {
                            let min = max_uid_left.saturating_sub(*batch_size).max(1);
                            let max = max_uid_left;

                            SequenceSet::try_from(min..=max)?
                        };
                        conn.send_command(CommandBody::Fetch {
                            sequence_set,
                            macro_or_item_names: common_attributes(),
                            uid: true,
                        })
                        .await?;
                        conn.read_response(&mut response, RequiredResponses::FETCH_REQUIRED)
                            .await
                            .chain_err_summary(|| {
                                format!(
                                    "Could not parse fetch response for mailbox {}",
                                    mailbox_path
                                )
                            })?;
                        let (_, mut v, _) = protocol_parser::fetch_responses(&response)?;
                        for FetchResponse {
                            ref uid,
                            ref mut envelope,
                            ref mut flags,
                            raw_fetch_value,
                            ref references,
                            ..
                        } in v.iter_mut()
                        {
                            if uid.is_none() || envelope.is_none() || flags.is_none() {
                                imap_log!(
                                    trace,
                                    conn,
                                    "BUG? something in fetch is none. UID: {:?}, envelope: {:?} \
                                     flags: {:?}",
                                    uid,
                                    envelope,
                                    flags
                                );
                                imap_log!(
                                    trace,
                                    conn,
                                    "response was: {}",
                                    String::from_utf8_lossy(&response)
                                );
                                conn.process_untagged(raw_fetch_value).await?;
                                continue;
                            }
                            let uid = uid.unwrap();
                            let env = envelope.as_mut().unwrap();
                            env.set_hash(generate_envelope_hash(&mailbox_path, &uid));
                            if let Some(value) = references {
                                env.set_references(value);
                            }
                            let mut tag_lck = uid_store.collection.tag_index.write().unwrap();
                            if let Some((flags, keywords)) = flags {
                                env.set_flags(*flags);
                                if !env.is_seen() {
                                    our_unseen.insert(env.hash());
                                }
                                for f in keywords {
                                    let hash = TagHash::from_bytes(f.as_bytes());
                                    tag_lck.entry(hash).or_insert_with(|| f.to_string());
                                    env.tags_mut().insert(hash);
                                }
                            }
                        }
                        {
                            let mut uid_store = Arc::clone(&self.uid_store);

                            if let Err(err) = uid_store
                                .insert_envelopes(mailbox_hash, &v)
                                .chain_err_summary(|| {
                                    format!(
                                        "Could not save envelopes in cache for mailbox {}",
                                        mailbox_path
                                    )
                                })
                            {
                                (uid_store.event_consumer)(uid_store.account_hash, err.into());
                            }
                        }

                        for f in v {
                            let FetchResponse {
                                uid: Some(uid),
                                message_sequence_number,
                                envelope: Some(env),
                                ..
                            } = f
                            else {
                                continue;
                            };
                            uid_store
                                .msn_index
                                .lock()
                                .unwrap()
                                .entry(mailbox_hash)
                                .or_default()
                                .insert(message_sequence_number - 1, uid);
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
                            envelopes.push(env);
                        }
                        unseen.lock().unwrap().insert_existing_set(our_unseen);
                        mailbox_exists.lock().unwrap().insert_existing_set(
                            envelopes.iter().map(|env| env.hash()).collect::<_>(),
                        );
                        drop(conn);
                    }
                    if max_uid_left <= 1 {
                        unseen.lock().unwrap().set_not_yet_seen(0);
                        mailbox_exists.lock().unwrap().set_not_yet_seen(0);
                        *stage = FetchStage::Finished;
                    } else {
                        *stage = FetchStage::FreshFetch {
                            max_uid: std::cmp::max(max_uid_left.saturating_sub(*batch_size + 1), 1),
                        };
                    }
                    return Ok(envelopes);
                }
                FetchStage::Finished => {
                    return Ok(vec![]);
                }
            }
        }
    }

    fn load_cache(
        conn: &ImapConnection,
        mailbox_hash: MailboxHash,
        max_uid: UID,
        batch_size: usize,
        select_response: SelectResponse,
    ) -> Option<Result<Vec<EnvelopeHash>>> {
        let mut uid_store = conn.uid_store.clone();
        if let Err(err) = uid_store
            .update_mailbox(mailbox_hash, &select_response)
            .chain_err_summary(|| format!("Could not update cache for mailbox {}.", mailbox_hash))
        {
            (uid_store.event_consumer)(uid_store.account_hash, err.into());
        }
        match uid_store.mailbox_state(mailbox_hash) {
            Err(err) => return Some(Err(err)),
            Ok(Some(_)) => {}
            Ok(None) => {
                return None;
            }
        };
        match uid_store.envelopes(mailbox_hash, max_uid, batch_size) {
            Ok(Some(envs)) => Some(Ok(envs)),
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    }

    async fn cached_envs(&mut self, max_uid: UID) -> Result<Option<Vec<Envelope>>> {
        let Self {
            stage: _,
            ref mut connection,
            mailbox_hash,
            ref uid_store,
            batch_size: _,
            cache_batch_size,
        } = self;
        let mailbox_hash = *mailbox_hash;
        if !uid_store.keep_offline_cache.load(Ordering::SeqCst) {
            return Ok(None);
        }
        {
            let mut conn = connection.lock().await?;
            let select_response = conn.init_mailbox(mailbox_hash).await?;
            match Self::load_cache(
                &conn,
                mailbox_hash,
                max_uid,
                *cache_batch_size,
                select_response,
            ) {
                None => Ok(None),
                Some(Ok(env_hashes)) => {
                    let env_lck = uid_store.envelopes.lock().unwrap();
                    Ok(Some(
                        env_hashes
                            .into_iter()
                            .filter_map(|env_hash| {
                                env_lck.get(&env_hash).map(|c_env| c_env.inner.clone())
                            })
                            .collect::<Vec<Envelope>>(),
                    ))
                }
                Some(Err(err)) => Err(err),
            }
        }
    }

    fn max_uid(&mut self) -> Result<Option<UID>> {
        let mailbox_hash = self.mailbox_hash;
        match self.uid_store.max_uid(mailbox_hash)? {
            None => Ok(None),
            Some(max_uid) => Ok(Some(max_uid)),
        }
    }
}
