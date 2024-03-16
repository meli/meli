/*
 * meli - accounts module.
 *
 * Copyright 2017 Manos Pitsidianakis
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

//! Account management from user configuration.

use std::{
    collections::{BTreeMap, HashMap, HashSet, VecDeque},
    convert::TryFrom,
    fs,
    future::Future,
    io,
    ops::{Index, IndexMut},
    os::unix::fs::PermissionsExt,
    pin::Pin,
    result,
    sync::{Arc, RwLock},
    time::Duration,
};

use futures::{future::FutureExt, stream::StreamExt};
use indexmap::IndexMap;
use melib::{
    backends::*,
    email::*,
    error::{Error, ErrorKind, Result},
    log,
    text::GlobMatch,
    thread::Threads,
    AddressBook, Collection, LogLevel, SortField, SortOrder,
};
use smallvec::SmallVec;

#[cfg(feature = "sqlite3")]
use crate::command::actions::AccountAction;
use crate::{
    conf::{AccountConf, FileMailboxConf},
    jobs::{JobId, JoinHandle},
    types::UIEvent::{self, EnvelopeRemove, EnvelopeRename, EnvelopeUpdate, Notification},
    MainLoopHandler, StatusEvent, ThreadEvent,
};

mod backend_ops;
mod jobs;
mod mailbox;

pub use jobs::*;
pub use mailbox::*;

#[macro_export]
macro_rules! try_recv_timeout {
    ($oneshot:expr) => {{
        const _3_MS: std::time::Duration = std::time::Duration::from_millis(95);
        let now = std::time::Instant::now();
        let mut res = Ok(None);
        while now + _3_MS >= std::time::Instant::now() {
            res = $oneshot.try_recv().map_err(|_| Error::new("canceled"));
            if res.as_ref().map(|r| r.is_some()).unwrap_or(false) || res.is_err() {
                break;
            }
        }
        res
    }};
}

#[macro_export]
macro_rules! is_variant {
    ($n:ident, $($var:tt)+) => {
        #[inline]
        pub fn $n(&self) -> bool {
            matches!(self, Self::$($var)*)
        }
    };
}

#[derive(Clone, Debug, Default)]
pub enum IsOnline {
    #[default]
    Uninit,
    True,
    Err {
        value: Error,
        retries: u64,
    },
}

impl IsOnline {
    is_variant! { is_uninit, Uninit }
    is_variant! { is_true, True }
    is_variant! { is_err, Err { .. } }

    fn set_err(&mut self, err: Error) {
        if let Self::Err { ref mut value, .. } = self {
            *value = err;
        } else {
            *self = Self::Err {
                value: err,
                retries: 1,
            };
        }
    }

    pub fn is_recoverable(err: &Error) -> bool {
        !(err.kind.is_authentication()
            || err.kind.is_configuration()
            || err.kind.is_bug()
            || err.kind.is_external()
            || (err.kind.is_network() && !err.kind.is_network_down())
            || err.kind.is_not_implemented()
            || err.kind.is_not_supported()
            || err.kind.is_protocol_error()
            || err.kind.is_protocol_not_supported()
            || err.kind.is_value_error())
    }
}

#[derive(Debug)]
pub struct Account {
    pub name: String,
    pub hash: AccountHash,
    pub is_online: IsOnline,
    pub mailbox_entries: IndexMap<MailboxHash, MailboxEntry>,
    pub mailboxes_order: Vec<MailboxHash>,
    pub tree: Vec<MailboxNode>,
    pub sent_mailbox: Option<MailboxHash>,
    pub collection: Collection,
    pub address_book: AddressBook,
    pub settings: AccountConf,
    pub backend: Arc<RwLock<Box<dyn MailBackend>>>,

    pub main_loop_handler: MainLoopHandler,
    pub active_jobs: HashMap<JobId, JobRequest>,
    pub active_job_instants: BTreeMap<std::time::Instant, JobId>,
    pub event_queue: VecDeque<(MailboxHash, RefreshEvent)>,
    pub backend_capabilities: MailBackendCapabilities,
}

impl Drop for Account {
    fn drop(&mut self) {
        if let Ok(data_dir) = xdg::BaseDirectories::with_profile("meli", &self.name) {
            if let Ok(data) = data_dir.place_data_file("addressbook") {
                /* place result in cache directory */
                let f = match fs::File::create(data) {
                    Ok(f) => f,
                    Err(e) => {
                        eprintln!("{}", e);
                        return;
                    }
                };
                let metadata = f.metadata().unwrap();
                let mut permissions = metadata.permissions();

                permissions.set_mode(0o600); // Read/write for owner only.
                f.set_permissions(permissions).unwrap();
                let writer = io::BufWriter::new(f);
                if let Err(err) = serde_json::to_writer(writer, &self.address_book) {
                    eprintln!("{}", err);
                };
            };
            /*
            if let Ok(data) = data_dir.place_data_file("mailbox") {
                /* place result in cache directory */
                let f = match fs::File::create(data) {
                    Ok(f) => f,
                    Err(e) => {
                        eprintln!("{}", e);
                        return;
                    }
                };
                let metadata = f.metadata().unwrap();
                let mut permissions = metadata.permissions();

                permissions.set_mode(0o600); // Read/write for owner only.
                f.set_permissions(permissions).unwrap();
                let writer = io::BufWriter::new(f);
                if let Err(err) = bincode::Options::serialize_into(
                    bincode::config::DefaultOptions::new(),
                    writer,
                    &self.collection,
                ) {
                    eprintln!("{}", err);
                };
            };
                */
        }
    }
}

impl Account {
    pub fn new(
        hash: AccountHash,
        name: String,
        mut settings: AccountConf,
        map: &Backends,
        main_loop_handler: MainLoopHandler,
        event_consumer: BackendEventConsumer,
    ) -> Result<Self> {
        let s = settings.clone();
        let backend = map.get(&settings.account().format)(
            settings.account(),
            Box::new(move |path: &str| {
                // disjoint-capture-in-closures
                let _ = &s;
                s.account.subscribed_mailboxes.is_empty()
                    || (s.mailbox_confs.contains_key(path)
                        && s.mailbox_confs[path].mailbox_conf().subscribe.is_true())
                    || s.account
                        .subscribed_mailboxes
                        .iter()
                        .any(|m| path.matches_glob(m))
            }),
            event_consumer,
        )?;

        let data_dir = xdg::BaseDirectories::with_profile("meli", &name).unwrap();
        let mut address_book = AddressBook::with_account(settings.account());

        if let Ok(data) = data_dir.place_data_file("addressbook") {
            if data.exists() {
                let reader = io::BufReader::new(fs::File::open(data).unwrap());
                let result: result::Result<AddressBook, _> = serde_json::from_reader(reader);
                if let Ok(data_t) = result {
                    for (id, c) in data_t.cards {
                        if !address_book.card_exists(id) && !c.external_resource() {
                            address_book.add_card(c);
                        }
                    }
                }
            }
        };

        if settings.conf.search_backend == crate::conf::SearchBackend::Auto {
            if backend.capabilities().supports_search {
                settings.conf.search_backend = crate::conf::SearchBackend::None;
            } else {
                #[cfg(feature = "sqlite3")]
                {
                    settings.conf.search_backend = crate::conf::SearchBackend::Sqlite3;
                }
                #[cfg(not(feature = "sqlite3"))]
                {
                    settings.conf.search_backend = crate::conf::SearchBackend::None;
                }
            }
        }

        let mut active_jobs = HashMap::default();
        let mut active_job_instants = BTreeMap::default();
        if let Ok(mailboxes_job) = backend.mailboxes() {
            if let Ok(online_job) = backend.is_online() {
                let handle = if backend.capabilities().is_async {
                    main_loop_handler
                        .job_executor
                        .spawn_specialized("mailboxes".into(), online_job.then(|_| mailboxes_job))
                } else {
                    main_loop_handler
                        .job_executor
                        .spawn_blocking("mailboxes".into(), online_job.then(|_| mailboxes_job))
                };
                let job_id = handle.job_id;
                active_jobs.insert(job_id, JobRequest::Mailboxes { handle });
                active_job_instants.insert(std::time::Instant::now(), job_id);
                main_loop_handler.send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                    StatusEvent::NewJob(job_id),
                )));
            }
        }

        #[cfg(feature = "sqlite3")]
        if settings.conf.search_backend == crate::conf::SearchBackend::Sqlite3 {
            let db_path = match crate::sqlite3::AccountCache::db_path(&name) {
                Err(err) => {
                    main_loop_handler.send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                        StatusEvent::DisplayMessage(format!(
                            "Error with setting up an sqlite3 search database for account `{}`: {}",
                            name, err
                        )),
                    )));
                    None
                }
                Ok(path) => path,
            };
            if let Some(db_path) = db_path {
                if !db_path.exists() {
                    log::info!(
                        "An sqlite3 search database for account `{}` seems to be missing, a new \
                         one will be created.",
                        name
                    );
                    main_loop_handler.send(ThreadEvent::UIEvent(UIEvent::Action(
                        (name.clone(), AccountAction::ReIndex).into(),
                    )));
                }
            }
        }

        Ok(Self {
            hash,
            name,
            is_online: if !backend.capabilities().is_remote {
                IsOnline::True
            } else {
                IsOnline::Uninit
            },
            mailbox_entries: Default::default(),
            mailboxes_order: Default::default(),
            tree: Default::default(),
            address_book,
            sent_mailbox: Default::default(),
            collection: backend.collection(),
            settings,
            main_loop_handler,
            active_jobs,
            active_job_instants,
            event_queue: VecDeque::with_capacity(8),
            backend_capabilities: backend.capabilities(),
            backend: Arc::new(RwLock::new(backend)),
        })
    }

    fn init(&mut self, mut ref_mailboxes: HashMap<MailboxHash, Mailbox>) -> Result<()> {
        self.backend_capabilities = self.backend.read().unwrap().capabilities();
        let mut mailbox_entries: IndexMap<MailboxHash, MailboxEntry> =
            IndexMap::with_capacity_and_hasher(ref_mailboxes.len(), Default::default());
        let mut mailboxes_order: Vec<MailboxHash> = Vec::with_capacity(ref_mailboxes.len());

        let mut sent_mailbox = None;

        /* Keep track of which mailbox config values we encounter in the actual
         * mailboxes returned by the backend. For each of the actual
         * mailboxes, delete the key from the hash set. If any are left, they
         * are misconfigurations (eg misspelling) and a warning is shown to the
         * user */
        let mut mailbox_conf_hash_set = self
            .settings
            .mailbox_confs
            .keys()
            .cloned()
            .collect::<HashSet<String>>();
        for f in ref_mailboxes.values_mut() {
            if let Some(conf) = self.settings.mailbox_confs.get_mut(f.path()) {
                mailbox_conf_hash_set.remove(f.path());
                conf.mailbox_conf.usage = if f.special_usage() != SpecialUsageMailbox::Normal {
                    Some(f.special_usage())
                } else {
                    let tmp = SpecialUsageMailbox::detect_usage(f.name());
                    if let Some(tmp) = tmp.filter(|&v| v != SpecialUsageMailbox::Normal) {
                        let _ = f.set_special_usage(tmp);
                    }
                    tmp
                };
                match conf.mailbox_conf.usage {
                    Some(SpecialUsageMailbox::Sent) => {
                        sent_mailbox = Some(f.hash());
                    }
                    None => {
                        if f.special_usage() == SpecialUsageMailbox::Sent {
                            sent_mailbox = Some(f.hash());
                        }
                    }
                    _ => {}
                }
                mailbox_entries.insert(
                    f.hash(),
                    MailboxEntry::new(
                        MailboxStatus::None,
                        f.path().to_string(),
                        f.clone(),
                        conf.clone(),
                    ),
                );
            } else {
                let mut new = FileMailboxConf::default();
                new.mailbox_conf.usage = if f.special_usage() != SpecialUsageMailbox::Normal {
                    Some(f.special_usage())
                } else {
                    let tmp = SpecialUsageMailbox::detect_usage(f.name());
                    if let Some(tmp) = tmp.filter(|&v| v != SpecialUsageMailbox::Normal) {
                        let _ = f.set_special_usage(tmp);
                    }
                    tmp
                };
                if new.mailbox_conf.usage == Some(SpecialUsageMailbox::Sent) {
                    sent_mailbox = Some(f.hash());
                }

                mailbox_entries.insert(
                    f.hash(),
                    MailboxEntry::new(MailboxStatus::None, f.path().to_string(), f.clone(), new),
                );
            }
        }

        for missing_mailbox in &mailbox_conf_hash_set {
            log::warn!(
                "Account `{}` mailbox `{}` configured but not present in account's mailboxes. Is \
                 it misspelled?",
                &self.name,
                missing_mailbox,
            );
            self.main_loop_handler
                .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                    StatusEvent::DisplayMessage(format!(
                        "Account `{}` mailbox `{}` configured but not present in account's \
                         mailboxes. Is it misspelled?",
                        &self.name, missing_mailbox,
                    )),
                )));
        }
        if !mailbox_conf_hash_set.is_empty() {
            let mut mailbox_comma_sep_list_string = mailbox_entries
                .values()
                .map(|e| e.name.as_str())
                .fold(String::new(), |mut acc, el| {
                    acc.push('`');
                    acc.push_str(el);
                    acc.push('`');
                    acc.push_str(", ");
                    acc
                });
            mailbox_comma_sep_list_string
                .drain(mailbox_comma_sep_list_string.len().saturating_sub(2)..);
            log::warn!(
                "Account `{}` has the following mailboxes: [{}]",
                &self.name,
                mailbox_comma_sep_list_string,
            );
            self.main_loop_handler
                .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                    StatusEvent::DisplayMessage(format!(
                        "Account `{}` has the following mailboxes: [{}]",
                        &self.name, mailbox_comma_sep_list_string,
                    )),
                )));
        }

        let mut tree: Vec<MailboxNode> = Vec::new();
        for (h, f) in ref_mailboxes.iter() {
            if !f.is_subscribed() {
                /* Skip unsubscribed mailbox */
                continue;
            }
            mailbox_entries.entry(*h).and_modify(|entry| {
                if entry.conf.mailbox_conf.autoload
                    || (entry.ref_mailbox.special_usage() == SpecialUsageMailbox::Inbox
                        || entry.ref_mailbox.special_usage() == SpecialUsageMailbox::Sent)
                {
                    let total = entry.ref_mailbox.count().ok().unwrap_or((0, 0)).1;
                    entry.status = MailboxStatus::Parsing(0, total);
                    if let Ok(mailbox_job) = self.backend.write().unwrap().fetch(*h) {
                        let mailbox_job = mailbox_job.into_future();
                        let handle = if self.backend_capabilities.is_async {
                            self.main_loop_handler
                                .job_executor
                                .spawn_specialized("fetch-mailbox".into(), mailbox_job)
                        } else {
                            self.main_loop_handler
                                .job_executor
                                .spawn_blocking("fetch-mailbox".into(), mailbox_job)
                        };
                        let job_id = handle.job_id;
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                                StatusEvent::NewJob(job_id),
                            )));
                        self.active_jobs.insert(
                            job_id,
                            JobRequest::Fetch {
                                mailbox_hash: *h,
                                handle,
                            },
                        );
                        self.active_job_instants
                            .insert(std::time::Instant::now(), job_id);
                    }
                }
            });
            self.collection.new_mailbox(*h);
        }

        build_mailboxes_order(&mut tree, &mailbox_entries, &mut mailboxes_order);
        self.mailboxes_order = mailboxes_order;
        self.mailbox_entries = mailbox_entries;
        self.tree = tree;
        self.sent_mailbox = sent_mailbox;
        Ok(())
    }

    pub fn reload(&mut self, event: RefreshEvent, mailbox_hash: MailboxHash) -> Option<UIEvent> {
        if !self.mailbox_entries[&mailbox_hash].status.is_available()
            && !self.mailbox_entries[&mailbox_hash].status.is_parsing()
        {
            self.event_queue.push_back((mailbox_hash, event));
            return None;
        }

        {
            //let mailbox: &mut Mailbox =
            // self.mailboxes[idx].as_mut().unwrap().as_mut().unwrap();
            match event.kind {
                RefreshEventKind::Update(old_hash, envelope) => {
                    if !self.collection.contains_key(&old_hash) {
                        return self.reload(
                            RefreshEvent {
                                account_hash: event.account_hash,
                                mailbox_hash: event.mailbox_hash,
                                kind: RefreshEventKind::Create(envelope),
                            },
                            mailbox_hash,
                        );
                    }
                    #[cfg(feature = "sqlite3")]
                    self.update_cached_env(*envelope.clone(), Some(old_hash));
                    self.collection.update(old_hash, *envelope, mailbox_hash);
                    return Some(EnvelopeUpdate(old_hash));
                }
                RefreshEventKind::NewFlags(env_hash, (flags, tags)) => {
                    if !self.collection.contains_key(&env_hash) {
                        return None;
                    }
                    self.collection
                        .envelopes
                        .write()
                        .unwrap()
                        .entry(env_hash)
                        .and_modify(|entry| {
                            entry.tags_mut().clear();
                            entry.tags_mut().extend(
                                tags.into_iter().map(|h| TagHash::from_bytes(h.as_bytes())),
                            );
                            entry.set_flags(flags);
                        });
                    #[cfg(feature = "sqlite3")]
                    if let Some(env) = {
                        let temp = self
                            .collection
                            .envelopes
                            .read()
                            .unwrap()
                            .get(&env_hash)
                            .cloned();

                        temp
                    } {
                        self.update_cached_env(env, None);
                    }
                    self.collection.update_flags(env_hash, mailbox_hash);
                    return Some(EnvelopeUpdate(env_hash));
                }
                RefreshEventKind::Rename(old_hash, new_hash) => {
                    log::trace!("rename {} to {}", old_hash, new_hash);
                    if !self.collection.rename(old_hash, new_hash, mailbox_hash) {
                        return Some(EnvelopeRename(old_hash, new_hash));
                    }
                    #[cfg(feature = "sqlite3")]
                    if let Some(env) = {
                        let temp = self
                            .collection
                            .envelopes
                            .read()
                            .unwrap()
                            .get(&new_hash)
                            .cloned();

                        temp
                    } {
                        self.update_cached_env(env, Some(old_hash));
                    }
                    return Some(EnvelopeRename(old_hash, new_hash));
                }
                RefreshEventKind::Create(envelope) => {
                    let env_hash = envelope.hash();
                    if self.collection.contains_key(&env_hash)
                        && self
                            .collection
                            .get_mailbox(mailbox_hash)
                            .contains(&env_hash)
                    {
                        return None;
                    }
                    let (is_seen, is_draft) =
                        { (envelope.is_seen(), envelope.flags().contains(Flag::DRAFT)) };
                    let (subject, from) = {
                        (
                            envelope.subject().into_owned(),
                            envelope.field_from_to_string(),
                        )
                    };
                    #[cfg(feature = "sqlite3")]
                    if self.settings.conf.search_backend == crate::conf::SearchBackend::Sqlite3 {
                        let handle = self.main_loop_handler.job_executor.spawn_specialized(
                            "sqlite3::insert".into(),
                            crate::sqlite3::AccountCache::insert(
                                (*envelope).clone(),
                                self.backend.clone(),
                                self.name.clone(),
                            ),
                        );
                        self.insert_job(
                            handle.job_id,
                            JobRequest::Generic {
                                name: format!(
                                    "Update envelope {} in sqlite3 cache",
                                    envelope.message_id_display()
                                )
                                .into(),
                                handle,
                                log_level: LogLevel::TRACE,
                                on_finish: None,
                            },
                        );
                    }

                    if self.collection.insert(*envelope, mailbox_hash) {
                        /* is a duplicate */
                        return None;
                    }

                    if self.mailbox_entries[&mailbox_hash]
                        .conf
                        .mailbox_conf
                        .ignore
                        .is_true()
                    {
                        return Some(UIEvent::MailboxUpdate((self.hash, mailbox_hash)));
                    }

                    let thread_hash = self.collection.get_env(env_hash).thread();
                    if self
                        .collection
                        .get_threads(mailbox_hash)
                        .thread_nodes()
                        .contains_key(&thread_hash)
                    {
                        let thread = self.collection.get_threads(mailbox_hash).find_group(
                            self.collection.get_threads(mailbox_hash)[&thread_hash].group,
                        );
                        if self
                            .collection
                            .get_threads(mailbox_hash)
                            .thread_ref(thread)
                            .snoozed()
                        {
                            return Some(UIEvent::MailboxUpdate((self.hash, mailbox_hash)));
                        }
                    }
                    if is_seen || is_draft {
                        return Some(UIEvent::MailboxUpdate((self.hash, mailbox_hash)));
                    }

                    return Some(Notification {
                        title: Some(format!("new e-mail from: {}", from).into()),
                        body: format!(
                            "{}\n{} {}",
                            subject,
                            self.name,
                            self.mailbox_entries[&mailbox_hash].name()
                        )
                        .into(),
                        source: None,
                        kind: Some(crate::types::NotificationType::NewMail),
                    });
                }
                RefreshEventKind::Remove(env_hash) => {
                    if !self.collection.contains_key(&env_hash) {
                        return None;
                    }
                    #[cfg(feature = "sqlite3")]
                    if self.settings.conf.search_backend == crate::conf::SearchBackend::Sqlite3 {
                        let fut = crate::sqlite3::AccountCache::remove(self.name.clone(), env_hash);
                        let handle = self
                            .main_loop_handler
                            .job_executor
                            .spawn_specialized("remove envelope from cache".into(), fut);
                        self.insert_job(
                            handle.job_id,
                            JobRequest::Refresh {
                                mailbox_hash,
                                handle,
                            },
                        );
                    }
                    let thread_hash = self.collection.get_env(env_hash).thread();
                    if !self
                        .collection
                        .get_threads(mailbox_hash)
                        .thread_nodes()
                        .contains_key(&thread_hash)
                    {
                        return None;
                    }
                    let thread_hash = self
                        .collection
                        .get_threads(mailbox_hash)
                        .find_group(self.collection.get_threads(mailbox_hash)[&thread_hash].group);
                    self.collection.remove(env_hash, mailbox_hash);
                    return Some(EnvelopeRemove(env_hash, thread_hash));
                }
                RefreshEventKind::Rescan => {
                    self.watch();
                }
                RefreshEventKind::Failure(err) => {
                    log::trace!("RefreshEvent Failure: {}", err.to_string());
                    while let Some((job_id, _)) =
                        self.active_jobs.iter().find(|(_, j)| j.is_watch())
                    {
                        let job_id = *job_id;
                        let j = self.active_jobs.remove(&job_id);
                        drop(j);
                    }
                    self.watch();
                    return Some(Notification {
                        title: Some("Account watch failed".into()),
                        body: err.to_string().into(),
                        kind: Some(crate::types::NotificationType::Error(err.kind)),
                        source: Some(err),
                    });
                }
                RefreshEventKind::MailboxCreate(_new_mailbox) => {}
                RefreshEventKind::MailboxDelete(_mailbox_hash) => {}
                RefreshEventKind::MailboxRename {
                    old_mailbox_hash: _,
                    new_mailbox: _,
                } => {}
                RefreshEventKind::MailboxSubscribe(_mailbox_hash) => {}
                RefreshEventKind::MailboxUnsubscribe(_mailbox_hash) => {}
            }
        }
        None
    }

    pub fn refresh(&mut self, mailbox_hash: MailboxHash) -> Result<()> {
        if let Some(ref refresh_command) = self.settings.conf().refresh_command {
            let child = std::process::Command::new("sh")
                .args(["-c", refresh_command])
                .stdin(std::process::Stdio::null())
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::piped())
                .spawn()?;
            self.main_loop_handler
                .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                    StatusEvent::DisplayMessage(format!("Running command {}", refresh_command)),
                )));
            self.main_loop_handler
                .send(ThreadEvent::UIEvent(UIEvent::Fork(
                    crate::ForkType::Generic(child),
                )));
            return Ok(());
        }
        let refresh_job = self.backend.write().unwrap().refresh(mailbox_hash);
        if let Ok(refresh_job) = refresh_job {
            let handle = if self.backend_capabilities.is_async {
                self.main_loop_handler
                    .job_executor
                    .spawn_specialized("refresh".into(), refresh_job)
            } else {
                self.main_loop_handler
                    .job_executor
                    .spawn_blocking("refresh".into(), refresh_job)
            };
            self.insert_job(
                handle.job_id,
                JobRequest::Refresh {
                    mailbox_hash,
                    handle,
                },
            );
        }
        Ok(())
    }

    pub fn watch(&mut self) {
        if self.settings.account().manual_refresh
            || matches!(self.is_online, IsOnline::Err { ref value, ..} if !IsOnline::is_recoverable(value))
        {
            return;
        }

        if !self.active_jobs.values().any(|j| j.is_watch()) {
            match self.backend.read().unwrap().watch() {
                Ok(fut) => {
                    let handle = if self.backend_capabilities.is_async {
                        self.main_loop_handler
                            .job_executor
                            .spawn_specialized("watch".into(), fut)
                    } else {
                        self.main_loop_handler
                            .job_executor
                            .spawn_blocking("watch".into(), fut)
                    };
                    self.active_jobs
                        .insert(handle.job_id, JobRequest::Watch { handle });
                }
                Err(e)
                    if e.kind == ErrorKind::NotSupported || e.kind == ErrorKind::NotImplemented => {
                }
                Err(e) => {
                    self.main_loop_handler
                        .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Account `{}` watch action returned error: {}",
                                &self.name, e
                            )),
                        )));
                }
            }
        }
    }

    pub fn len(&self) -> usize {
        self.tree.len()
    }

    pub fn is_empty(&self) -> bool {
        self.tree.is_empty()
    }

    pub fn list_mailboxes(&self) -> Vec<MailboxNode> {
        let mut ret = Vec::with_capacity(self.mailbox_entries.len());
        fn rec(node: &MailboxNode, ret: &mut Vec<MailboxNode>) {
            ret.push(node.clone());
            for c in node.children.iter() {
                rec(c, ret);
            }
        }
        for node in &self.tree {
            rec(node, &mut ret);
        }
        ret
    }

    pub fn mailboxes_order(&self) -> &[MailboxHash] {
        &self.mailboxes_order
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn hash(&self) -> AccountHash {
        self.hash
    }

    pub fn load(&mut self, mailbox_hash: MailboxHash) -> result::Result<(), usize> {
        if mailbox_hash.is_null() {
            return Err(0);
        }
        match self.mailbox_entries[&mailbox_hash].status {
            MailboxStatus::Available | MailboxStatus::Parsing(_, _)
                if self
                    .collection
                    .mailboxes
                    .read()
                    .unwrap()
                    .contains_key(&mailbox_hash) =>
            {
                Ok(())
            }
            MailboxStatus::None => {
                if !self.active_jobs.values().any(|j| j.is_fetch(mailbox_hash)) {
                    let mailbox_job = self.backend.write().unwrap().fetch(mailbox_hash);
                    match mailbox_job {
                        Ok(mailbox_job) => {
                            let mailbox_job = mailbox_job.into_future();
                            let handle = if self.backend_capabilities.is_async {
                                self.main_loop_handler
                                    .job_executor
                                    .spawn_specialized("mailbox_fetch".into(), mailbox_job)
                            } else {
                                self.main_loop_handler
                                    .job_executor
                                    .spawn_blocking("mailbox_fetch".into(), mailbox_job)
                            };
                            self.insert_job(
                                handle.job_id,
                                JobRequest::Fetch {
                                    mailbox_hash,
                                    handle,
                                },
                            );
                        }
                        Err(err) => {
                            self.mailbox_entries
                                .entry(mailbox_hash)
                                .and_modify(|entry| {
                                    entry.status = MailboxStatus::Failed(err);
                                });
                            self.main_loop_handler
                                .send(ThreadEvent::UIEvent(UIEvent::StartupCheck(mailbox_hash)));
                        }
                    }
                }
                self.collection.new_mailbox(mailbox_hash);
                Err(0)
            }
            _ => Err(0),
        }
    }

    pub fn save_special(
        &mut self,
        bytes: &[u8],
        mailbox_type: SpecialUsageMailbox,
        flags: Flag,
    ) -> Result<MailboxHash> {
        let mut saved_at: Option<MailboxHash> = None;
        for mailbox in &[
            self.special_use_mailbox(mailbox_type),
            self.special_use_mailbox(SpecialUsageMailbox::Inbox),
            self.special_use_mailbox(SpecialUsageMailbox::Normal),
        ] {
            if let Some(mailbox_hash) = mailbox {
                if let Err(err) = self.save(bytes, *mailbox_hash, Some(flags)) {
                    log::error!("Could not save in '{}' mailbox: {}.", *mailbox_hash, err);
                } else {
                    saved_at = Some(*mailbox_hash);
                    break;
                }
            } else {
                continue;
            }
        }

        if let Some(mailbox_hash) = saved_at {
            Ok(mailbox_hash)
        } else {
            let file = crate::types::File::create_temp_file(bytes, None, None, Some("eml"), false)?;
            log::trace!("message saved in {}", file.path().display());
            log::info!(
                "Message was stored in {} so that you can restore it manually.",
                file.path().display()
            );
            Err(Error::new(format!(
                "Message was stored in {} so that you can restore it manually.",
                file.path().display()
            ))
            .set_summary("Could not save in any mailbox"))
        }
    }

    pub fn save(
        &mut self,
        bytes: &[u8],
        mailbox_hash: MailboxHash,
        flags: Option<Flag>,
    ) -> Result<()> {
        if self.settings.account.read_only {
            return Err(Error::new(format!(
                "Account {} is read-only.",
                self.name.as_str()
            )));
        }
        let job = self
            .backend
            .write()
            .unwrap()
            .save(bytes.to_vec(), mailbox_hash, flags)?;

        let handle = if self.backend_capabilities.is_async {
            self.main_loop_handler
                .job_executor
                .spawn_specialized("save".into(), job)
        } else {
            self.main_loop_handler
                .job_executor
                .spawn_blocking("save".into(), job)
        };
        self.insert_job(
            handle.job_id,
            JobRequest::SaveMessage {
                bytes: bytes.to_vec(),
                mailbox_hash,
                handle,
            },
        );
        Ok(())
    }

    pub fn send(
        &mut self,
        message: String,
        send_mail: crate::conf::composing::SendMail,
        #[allow(unused_variables)] complete_in_background: bool,
    ) -> Result<Option<JoinHandle<Result<()>>>> {
        use std::{
            io::Write,
            process::{Command, Stdio},
        };

        use crate::conf::composing::SendMail;
        match send_mail {
            SendMail::ShellCommand(ref command) => {
                if command.is_empty() {
                    return Err(Error::new(
                        "send_mail shell command configuration value is empty",
                    ));
                }
                let mut msmtp = Command::new("sh")
                    .args(["-c", command])
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                    .expect("Failed to start mailer command");
                {
                    let stdin = msmtp.stdin.as_mut().expect("failed to open stdin");
                    stdin
                        .write_all(message.as_bytes())
                        .expect("Failed to write to stdin");
                }
                let output = msmtp.wait().expect("Failed to wait on mailer");
                if output.success() {
                    log::trace!("Message sent.");
                } else {
                    let error_message = if let Some(exit_code) = output.code() {
                        format!(
                            "Could not send e-mail using `{}`: Process exited with {}",
                            command, exit_code
                        )
                    } else {
                        format!(
                            "Could not send e-mail using `{}`: Process was killed by signal",
                            command
                        )
                    };
                    log::error!("{}", error_message);
                    return Err(Error::new(error_message).set_summary("Message not sent."));
                }
                Ok(None)
            }
            #[cfg(feature = "smtp")]
            SendMail::Smtp(conf) => {
                let handle = self.main_loop_handler.job_executor.spawn_specialized(
                    "smtp".into(),
                    async move {
                        let mut smtp_connection =
                            melib::smtp::SmtpConnection::new_connection(conf).await?;
                        smtp_connection.mail_transaction(&message, None).await
                    },
                );
                if complete_in_background {
                    self.insert_job(handle.job_id, JobRequest::SendMessageBackground { handle });
                    return Ok(None);
                } else {
                    self.insert_job(handle.job_id, JobRequest::SendMessage);
                }
                Ok(Some(handle))
            }
            SendMail::ServerSubmission => {
                if self.backend_capabilities.supports_submission {
                    let job =
                        self.backend
                            .write()
                            .unwrap()
                            .submit(message.into_bytes(), None, None)?;

                    let handle = if self.backend_capabilities.is_async {
                        self.main_loop_handler
                            .job_executor
                            .spawn_specialized("server_submission".into(), job)
                    } else {
                        self.main_loop_handler
                            .job_executor
                            .spawn_blocking("server_submission".into(), job)
                    };
                    self.insert_job(handle.job_id, JobRequest::SendMessageBackground { handle });
                    return Ok(None);
                }
                Err(Error::new("Server does not support submission.")
                    .set_summary("Message not sent."))
            }
        }
    }

    pub fn send_async(
        &self,
        send_mail: crate::conf::composing::SendMail,
    ) -> impl FnOnce(Arc<String>) -> Pin<Box<dyn Future<Output = Result<()>> + Send>> + Send {
        let capabilities = self.backend_capabilities.clone();
        let backend = self.backend.clone();
        move |message: Arc<String>| -> Pin<Box<dyn Future<Output = Result<()>> + Send>> {
            Box::pin(async move {
                use std::{
                    io::Write,
                    process::{Command, Stdio},
                };

                use crate::conf::composing::SendMail;
                match send_mail {
                    SendMail::ShellCommand(ref command) => {
                        if command.is_empty() {
                            return Err(Error::new(
                                "send_mail shell command configuration value is empty",
                            ));
                        }
                        let mut msmtp = Command::new("sh")
                            .args(["-c", command])
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn()
                            .expect("Failed to start mailer command");
                        {
                            let stdin = msmtp.stdin.as_mut().expect("failed to open stdin");
                            stdin
                                .write_all(message.as_bytes())
                                .expect("Failed to write to stdin");
                        }
                        let output = msmtp.wait().expect("Failed to wait on mailer");
                        if output.success() {
                            log::trace!("Message sent.");
                        } else {
                            let error_message = if let Some(exit_code) = output.code() {
                                format!(
                                    "Could not send e-mail using `{}`: Process exited with {}",
                                    command, exit_code
                                )
                            } else {
                                format!(
                                    "Could not send e-mail using `{}`: Process was killed by \
                                     signal",
                                    command
                                )
                            };
                            log::error!("{}", error_message);
                            return Err(Error::new(error_message).set_summary("Message not sent."));
                        }
                        Ok(())
                    }
                    #[cfg(feature = "smtp")]
                    SendMail::Smtp(conf) => {
                        let mut smtp_connection =
                            melib::smtp::SmtpConnection::new_connection(conf).await?;
                        smtp_connection
                            .mail_transaction(message.as_str(), None)
                            .await
                    }
                    SendMail::ServerSubmission => {
                        if capabilities.supports_submission {
                            let fut = backend.write().unwrap().submit(
                                message.as_bytes().to_vec(),
                                None,
                                None,
                            )?;
                            fut.await?;
                            return Ok(());
                        }
                        Err(Error::new("Server does not support submission.")
                            .set_summary("Message not sent."))
                    }
                }
            })
        }
    }

    pub fn contains_key(&self, h: EnvelopeHash) -> bool {
        self.collection.contains_key(&h)
    }
    pub fn operation(&self, h: EnvelopeHash) -> Result<Box<dyn BackendOp>> {
        let operation = self.backend.read().unwrap().operation(h)?;
        Ok(if self.settings.account.read_only {
            ReadOnlyOp::new(operation)
        } else {
            operation
        })
    }

    pub fn mailbox_operation(
        &mut self,
        op: crate::command::actions::MailboxOperation,
    ) -> Result<()> {
        use crate::command::actions::MailboxOperation;
        if self.settings.account.read_only {
            return Err(Error::new("Account is read-only."));
        }
        match op {
            MailboxOperation::Create(path) => {
                let job = self
                    .backend
                    .write()
                    .unwrap()
                    .create_mailbox(path.to_string())?;
                let handle = if self.backend_capabilities.is_async {
                    self.main_loop_handler
                        .job_executor
                        .spawn_specialized("create_mailbox".into(), job)
                } else {
                    self.main_loop_handler
                        .job_executor
                        .spawn_blocking("create_mailbox".into(), job)
                };
                self.insert_job(handle.job_id, JobRequest::CreateMailbox { path, handle });
                Ok(())
            }
            MailboxOperation::Delete(path) => {
                if self.mailbox_entries.len() == 1 {
                    return Err(Error::new("Cannot delete only mailbox."));
                }

                let mailbox_hash = self.mailbox_by_path(&path)?;
                let job = self.backend.write().unwrap().delete_mailbox(mailbox_hash)?;
                let handle = if self.backend_capabilities.is_async {
                    self.main_loop_handler
                        .job_executor
                        .spawn_specialized("delete_mailbox".into(), job)
                } else {
                    self.main_loop_handler
                        .job_executor
                        .spawn_blocking("delete_mailbox".into(), job)
                };
                self.insert_job(
                    handle.job_id,
                    JobRequest::DeleteMailbox {
                        mailbox_hash,
                        handle,
                    },
                );
                Ok(())
            }
            MailboxOperation::Subscribe(path) => {
                let mailbox_hash = self.mailbox_by_path(&path)?;
                let job = self
                    .backend
                    .write()
                    .unwrap()
                    .set_mailbox_subscription(mailbox_hash, true)?;
                let handle = if self.backend_capabilities.is_async {
                    self.main_loop_handler
                        .job_executor
                        .spawn_specialized("subscribe_mailbox".into(), job)
                } else {
                    self.main_loop_handler
                        .job_executor
                        .spawn_blocking("subscribe_mailbox".into(), job)
                };
                self.insert_job(
                    handle.job_id,
                    JobRequest::SetMailboxSubscription {
                        mailbox_hash,
                        new_value: true,
                        handle,
                    },
                );
                Ok(())
            }
            MailboxOperation::Unsubscribe(path) => {
                let mailbox_hash = self.mailbox_by_path(&path)?;
                let job = self
                    .backend
                    .write()
                    .unwrap()
                    .set_mailbox_subscription(mailbox_hash, false)?;
                let handle = if self.backend_capabilities.is_async {
                    self.main_loop_handler
                        .job_executor
                        .spawn_specialized("unsubscribe_mailbox".into(), job)
                } else {
                    self.main_loop_handler
                        .job_executor
                        .spawn_blocking("unsubscribe_mailbox".into(), job)
                };
                self.insert_job(
                    handle.job_id,
                    JobRequest::SetMailboxSubscription {
                        mailbox_hash,
                        new_value: false,
                        handle,
                    },
                );
                Ok(())
            }
            MailboxOperation::Rename(_, _) => Err(Error::new("Not implemented.")),
            MailboxOperation::SetPermissions(_) => Err(Error::new("Not implemented.")),
        }
    }

    pub fn special_use_mailbox(&self, special_use: SpecialUsageMailbox) -> Option<MailboxHash> {
        let ret = self
            .mailbox_entries
            .iter()
            .find(|(_, f)| f.conf.mailbox_conf().usage == Some(special_use));
        ret.as_ref().map(|ret| ret.1.ref_mailbox.hash())
    }

    /* Call only in Context::is_online, since only Context can launch the watcher
     * threads if an account goes from offline to online. */
    pub fn is_online(&mut self) -> Result<()> {
        let (ret, wait) = match &mut self.is_online {
            IsOnline::Uninit => (Err(Error::new("Attempting connection.")), None),
            IsOnline::True => return Ok(()),
            IsOnline::Err {
                value,
                ref mut retries,
            } => {
                let ret = Err(value.clone());
                if !IsOnline::is_recoverable(value) {
                    return ret;
                }
                let wait = if value.kind.is_timeout()
                    || value.kind.is_network_down()
                    || value.kind.is_oserror()
                {
                    let oldval = *retries;
                    if oldval != 8 {
                        *retries *= 2;
                    }
                    Some(Duration::from_millis(
                        oldval * (4 * u64::from(melib::utils::random::random_u8())),
                    ))
                } else {
                    None
                };

                (ret, wait)
            }
        };
        if !self.active_jobs.values().any(JobRequest::is_online) {
            let online_fut = self.backend.read().unwrap().is_online();
            if let Ok(online_fut) = online_fut {
                use melib::utils::futures::sleep;

                let handle = match (wait, self.backend_capabilities.is_async) {
                    (Some(wait), true) => self.main_loop_handler.job_executor.spawn_specialized(
                        "is_online".into(),
                        async move {
                            sleep(wait).await;
                            online_fut.await
                        },
                    ),
                    (None, true) => self
                        .main_loop_handler
                        .job_executor
                        .spawn_specialized("is_online".into(), online_fut),
                    (Some(wait), false) => self.main_loop_handler.job_executor.spawn_blocking(
                        "is_online".into(),
                        async move {
                            sleep(wait).await;
                            online_fut.await
                        },
                    ),
                    (None, false) => self
                        .main_loop_handler
                        .job_executor
                        .spawn_blocking("is_online".into(), online_fut),
                };
                self.insert_job(handle.job_id, JobRequest::IsOnline { handle });
            }
        }
        ret
    }

    pub fn search(
        &self,
        search_term: &str,
        _sort: (SortField, SortOrder),
        mailbox_hash: MailboxHash,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        let query = melib::search::Query::try_from(search_term)?;
        match self.settings.conf.search_backend {
            #[cfg(feature = "sqlite3")]
            crate::conf::SearchBackend::Sqlite3 => Ok(Box::pin(
                crate::sqlite3::AccountCache::search(self.name.clone(), query, _sort),
            )),
            crate::conf::SearchBackend::Auto | crate::conf::SearchBackend::None => {
                if self.backend_capabilities.supports_search {
                    self.backend
                        .read()
                        .unwrap()
                        .search(query, Some(mailbox_hash))
                } else {
                    use melib::search::QueryTrait;
                    let mut ret = SmallVec::new();
                    let envelopes = self.collection.envelopes.read().unwrap();
                    for &env_hash in self.collection.get_mailbox(mailbox_hash).iter() {
                        if let Some(envelope) = envelopes.get(&env_hash) {
                            if envelope.is_match(&query) {
                                ret.push(env_hash);
                            }
                        }
                    }
                    Ok(Box::pin(async { Ok(ret) }))
                }
            }
        }
    }

    pub fn mailbox_by_path(&self, path: &str) -> Result<MailboxHash> {
        if let Some((mailbox_hash, _)) = self
            .mailbox_entries
            .iter()
            .find(|(_, f)| f.ref_mailbox.path() == path)
        {
            Ok(*mailbox_hash)
        } else {
            Err(Error::new("Mailbox with that path not found."))
        }
    }

    pub fn process_event(&mut self, job_id: &JobId) -> bool {
        self.main_loop_handler
            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                StatusEvent::JobFinished(*job_id),
            )));

        if let Some(mut job) = self.active_jobs.remove(job_id) {
            let job_id = *job_id;
            match job {
                JobRequest::Mailboxes { ref mut handle } => {
                    if let Ok(Some(mailboxes)) = handle.chan.try_recv() {
                        if let Err(err) = mailboxes.and_then(|mailboxes| self.init(mailboxes)) {
                            if !IsOnline::is_recoverable(&err) {
                                self.main_loop_handler.send(ThreadEvent::UIEvent(
                                    UIEvent::Notification {
                                        title: Some(self.name.clone().into()),
                                        source: Some(err.clone()),
                                        body: err.to_string().into(),
                                        kind: Some(crate::types::NotificationType::Error(err.kind)),
                                    },
                                ));
                                self.main_loop_handler.send(ThreadEvent::UIEvent(
                                    UIEvent::AccountStatusChange(
                                        self.hash,
                                        Some(err.to_string().into()),
                                    ),
                                ));
                                self.is_online.set_err(err);
                                self.main_loop_handler
                                    .job_executor
                                    .set_job_success(job_id, false);
                                return true;
                            }
                            let mailboxes_job = self.backend.read().unwrap().mailboxes();
                            if let Ok(mailboxes_job) = mailboxes_job {
                                let handle = if self.backend_capabilities.is_async {
                                    self.main_loop_handler
                                        .job_executor
                                        .spawn_specialized("mailboxes_list".into(), mailboxes_job)
                                } else {
                                    self.main_loop_handler
                                        .job_executor
                                        .spawn_blocking("mailboxes_list".into(), mailboxes_job)
                                };
                                self.insert_job(handle.job_id, JobRequest::Mailboxes { handle });
                            };
                        } else {
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::AccountStatusChange(
                                    self.hash,
                                    Some("Loaded mailboxes.".into()),
                                ),
                            ));
                        }
                    }
                }
                JobRequest::Fetch {
                    mailbox_hash,
                    ref mut handle,
                    ..
                } => {
                    log::trace!("got payload in status for {}", mailbox_hash);
                    match handle.chan.try_recv() {
                        Err(_) => {
                            self.main_loop_handler
                                .job_executor
                                .set_job_success(job_id, false);
                            /* canceled */
                            return true;
                        }
                        Ok(None) => {
                            return true;
                        }
                        Ok(Some((None, _))) => {
                            log::trace!("finished in status for {}", mailbox_hash);
                            self.mailbox_entries
                                .entry(mailbox_hash)
                                .and_modify(|entry| {
                                    entry.status = MailboxStatus::Available;
                                });
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::MailboxUpdate((self.hash, mailbox_hash)),
                            ));
                            return true;
                        }
                        Ok(Some((Some(Err(err)), _))) => {
                            self.main_loop_handler
                                .job_executor
                                .set_job_success(job_id, false);
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(
                                        format!("{}: could not fetch mailbox", &self.name).into(),
                                    ),
                                    source: None,
                                    body: err.to_string().into(),
                                    kind: Some(crate::types::NotificationType::Error(err.kind)),
                                },
                            ));
                            self.mailbox_entries
                                .entry(mailbox_hash)
                                .and_modify(|entry| {
                                    entry.status = MailboxStatus::Failed(err);
                                });
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::MailboxUpdate((self.hash, mailbox_hash)),
                            ));
                            return true;
                        }
                        Ok(Some((Some(Ok(payload)), rest))) => {
                            let handle = if self.backend_capabilities.is_async {
                                self.main_loop_handler
                                    .job_executor
                                    .spawn_specialized("rest_fetch".into(), rest.into_future())
                            } else {
                                self.main_loop_handler
                                    .job_executor
                                    .spawn_blocking("rest_fetch".into(), rest.into_future())
                            };
                            self.insert_job(
                                handle.job_id,
                                JobRequest::Fetch {
                                    mailbox_hash,
                                    handle,
                                },
                            );
                            let envelopes = payload
                                .into_iter()
                                .map(|e| (e.hash(), e))
                                .collect::<HashMap<EnvelopeHash, Envelope>>();
                            if let Some(updated_mailboxes) =
                                self.collection
                                    .merge(envelopes, mailbox_hash, self.sent_mailbox)
                            {
                                for f in updated_mailboxes {
                                    self.main_loop_handler.send(ThreadEvent::UIEvent(
                                        UIEvent::MailboxUpdate((self.hash, f)),
                                    ));
                                }
                            }
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::MailboxUpdate((self.hash, mailbox_hash)),
                            ));
                        }
                    }
                }
                JobRequest::IsOnline { ref mut handle, .. } => {
                    if matches!(self.is_online, IsOnline::Err { ref value, ..} if !IsOnline::is_recoverable(value))
                    {
                        return true;
                    }
                    if let Ok(Some(is_online)) = handle.chan.try_recv() {
                        self.main_loop_handler.send(ThreadEvent::UIEvent(
                            UIEvent::AccountStatusChange(self.hash, None),
                        ));
                        match is_online {
                            Ok(()) => {
                                if matches!(self.is_online, IsOnline::Err { .. })
                                    || matches!(self.is_online, IsOnline::Uninit)
                                {
                                    self.watch();
                                }
                                self.is_online = IsOnline::True;
                                return true;
                            }
                            Err(value) => {
                                self.is_online.set_err(value);
                            }
                        }
                    }
                    let online_job = self.backend.read().unwrap().is_online();
                    if let Ok(online_job) = online_job {
                        let handle = if self.backend_capabilities.is_async {
                            self.main_loop_handler
                                .job_executor
                                .spawn_specialized("is_online".into(), online_job)
                        } else {
                            self.main_loop_handler
                                .job_executor
                                .spawn_blocking("is_online".into(), online_job)
                        };
                        self.insert_job(handle.job_id, JobRequest::IsOnline { handle });
                    };
                }
                JobRequest::Refresh { ref mut handle, .. } => {
                    if matches!(self.is_online, IsOnline::Err { ref value, ..} if !IsOnline::is_recoverable(value))
                    {
                        return true;
                    }
                    match handle.chan.try_recv() {
                        Err(_) => { /* canceled */ }
                        Ok(None) => {}
                        Ok(Some(Ok(()))) => {
                            if matches!(self.is_online, IsOnline::Err { .. }) {
                                self.watch();
                            }
                            self.is_online = IsOnline::True;
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::AccountStatusChange(self.hash, None),
                            ));
                        }
                        Ok(Some(Err(err))) => {
                            self.main_loop_handler
                                .job_executor
                                .set_job_success(job_id, false);
                            self.is_online.set_err(err);
                            _ = self.is_online();
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::AccountStatusChange(self.hash, None),
                            ));
                        }
                    }
                }
                JobRequest::SetFlags {
                    ref mut handle,
                    ref env_hashes,
                    ref mailbox_hash,
                    ref flags,
                } => match handle.chan.try_recv() {
                    Ok(Some(Err(err))) => {
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(format!("{}: could not set flag", &self.name).into()),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(crate::types::NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(()))) => {
                        for env_hash in env_hashes.iter() {
                            if !self.collection.contains_key(&env_hash) {
                                continue;
                            }
                            let mut env_lck = self.collection.envelopes.write().unwrap();
                            env_lck.entry(env_hash).and_modify(|entry| {
                                for op in flags.iter() {
                                    match op {
                                        FlagOp::Set(f) => {
                                            let mut flags = entry.flags();
                                            flags.set(*f, true);
                                            entry.set_flags(flags);
                                        }
                                        FlagOp::UnSet(f) => {
                                            let mut flags = entry.flags();
                                            flags.set(*f, false);
                                            entry.set_flags(flags);
                                        }
                                        FlagOp::SetTag(t) => {
                                            entry
                                                .tags_mut()
                                                .insert(TagHash::from_bytes(t.as_bytes()));
                                        }
                                        FlagOp::UnSetTag(t) => {
                                            entry
                                                .tags_mut()
                                                .remove(&TagHash::from_bytes(t.as_bytes()));
                                        }
                                    }
                                }
                            });
                            #[cfg(feature = "sqlite3")]
                            if let Some(env) = env_lck.get(&env_hash).cloned() {
                                drop(env_lck);
                                self.update_cached_env(env, None);
                            }
                        }
                        for env_hash in env_hashes.iter() {
                            self.collection.update_flags(env_hash, *mailbox_hash);
                            self.main_loop_handler
                                .send(ThreadEvent::UIEvent(UIEvent::EnvelopeUpdate(env_hash)));
                        }
                    }
                    Err(_) | Ok(None) => {}
                },
                JobRequest::SaveMessage {
                    ref mut handle,
                    ref bytes,
                    ..
                } => {
                    if let Ok(Some(Err(err))) = handle.chan.try_recv() {
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                        log::error!("Could not save message: {err}");
                        match crate::types::File::create_temp_file(
                            bytes,
                            None,
                            None,
                            Some("eml"),
                            false,
                        ) {
                            Ok(file) => {
                                log::debug!("message saved in {}", file.path().display());
                                log::info!(
                                    "Message was stored in {} so that you can restore it manually.",
                                    file.path().display()
                                );
                                self.main_loop_handler.send(ThreadEvent::UIEvent(
                                    UIEvent::Notification {
                                        title: Some(
                                            format!("{}: could not save message", &self.name)
                                                .into(),
                                        ),
                                        source: None,
                                        body: format!(
                                            "Message was stored in {} so that you can restore it \
                                             manually.",
                                            file.path().display()
                                        )
                                        .into(),
                                        kind: Some(crate::types::NotificationType::Info),
                                    },
                                ));
                            }
                            Err(err) => log::error!("Could not save message: {err}"),
                        }
                    }
                }
                JobRequest::SendMessage => {}
                JobRequest::SendMessageBackground { ref mut handle, .. } => {
                    if let Ok(Some(Err(err))) = handle.chan.try_recv() {
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some("Could not send message".into()),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(crate::types::NotificationType::Error(err.kind)),
                            }));
                    }
                }
                JobRequest::DeleteMessages { ref mut handle, .. } => {
                    if let Ok(Some(Err(err))) = handle.chan.try_recv() {
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(
                                    format!("{}: could not delete message", &self.name).into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(crate::types::NotificationType::Error(err.kind)),
                            }));
                    }
                }
                JobRequest::CreateMailbox {
                    ref path,
                    ref mut handle,
                    ..
                } => {
                    if let Ok(Some(r)) = handle.chan.try_recv() {
                        match r {
                            Err(err) => {
                                self.main_loop_handler
                                    .job_executor
                                    .set_job_success(job_id, false);
                                self.main_loop_handler.send(ThreadEvent::UIEvent(
                                    UIEvent::Notification {
                                        title: Some(
                                            format!(
                                                "{}: could not create mailbox {}",
                                                &self.name, path
                                            )
                                            .into(),
                                        ),
                                        source: None,
                                        body: err.to_string().into(),
                                        kind: Some(crate::types::NotificationType::Error(err.kind)),
                                    },
                                ));
                            }
                            Ok((mailbox_hash, mut mailboxes)) => {
                                self.main_loop_handler.send(ThreadEvent::UIEvent(
                                    UIEvent::MailboxCreate((self.hash, mailbox_hash)),
                                ));
                                let mut new = FileMailboxConf::default();
                                new.mailbox_conf.subscribe = super::ToggleFlag::InternalVal(true);
                                new.mailbox_conf.usage = if mailboxes[&mailbox_hash].special_usage()
                                    != SpecialUsageMailbox::Normal
                                {
                                    Some(mailboxes[&mailbox_hash].special_usage())
                                } else {
                                    let tmp = SpecialUsageMailbox::detect_usage(
                                        mailboxes[&mailbox_hash].name(),
                                    );
                                    if let Some(tmp) =
                                        tmp.filter(|&v| v != SpecialUsageMailbox::Normal)
                                    {
                                        mailboxes.entry(mailbox_hash).and_modify(|entry| {
                                            let _ = entry.set_special_usage(tmp);
                                        });
                                    }
                                    tmp
                                };
                                /* if new mailbox has parent, we need to update its children
                                 * field */
                                if let Some(parent_hash) = mailboxes[&mailbox_hash].parent() {
                                    self.mailbox_entries
                                        .entry(parent_hash)
                                        .and_modify(|parent| {
                                            parent.ref_mailbox =
                                                mailboxes.remove(&parent_hash).unwrap();
                                        });
                                }
                                let status = MailboxStatus::default();

                                self.mailbox_entries.insert(
                                    mailbox_hash,
                                    MailboxEntry::new(
                                        status,
                                        mailboxes[&mailbox_hash].path().to_string(),
                                        mailboxes.remove(&mailbox_hash).unwrap(),
                                        new,
                                    ),
                                );
                                self.collection
                                    .threads
                                    .write()
                                    .unwrap()
                                    .insert(mailbox_hash, Threads::default());
                                self.collection
                                    .mailboxes
                                    .write()
                                    .unwrap()
                                    .insert(mailbox_hash, Default::default());
                                build_mailboxes_order(
                                    &mut self.tree,
                                    &self.mailbox_entries,
                                    &mut self.mailboxes_order,
                                );
                                //Ok(format!("`{}` successfully created.",
                                // &path))
                            }
                        }
                    }
                }
                JobRequest::DeleteMailbox {
                    mailbox_hash,
                    ref mut handle,
                    ..
                } => {
                    match handle.chan.try_recv() {
                        Err(_) => { /* canceled */ }
                        Ok(None) => {}
                        Ok(Some(Err(err))) => {
                            self.main_loop_handler
                                .job_executor
                                .set_job_success(job_id, false);
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(
                                        format!("{}: could not delete mailbox", &self.name).into(),
                                    ),
                                    source: None,
                                    body: err.to_string().into(),
                                    kind: Some(crate::types::NotificationType::Error(err.kind)),
                                },
                            ));
                        }
                        Ok(Some(Ok(mut mailboxes))) => {
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::MailboxDelete((self.hash, mailbox_hash)),
                            ));
                            if let Some(pos) =
                                self.mailboxes_order.iter().position(|&h| h == mailbox_hash)
                            {
                                self.mailboxes_order.remove(pos);
                            }
                            if let Some(pos) = self.tree.iter().position(|n| n.hash == mailbox_hash)
                            {
                                self.tree.remove(pos);
                            }
                            if self.sent_mailbox == Some(mailbox_hash) {
                                self.sent_mailbox = None;
                            }
                            self.collection
                                .threads
                                .write()
                                .unwrap()
                                .remove(&mailbox_hash);
                            let deleted_mailbox =
                                self.mailbox_entries.remove(&mailbox_hash).unwrap();
                            /* if deleted mailbox had parent, we need to update its children
                             * field */
                            if let Some(parent_hash) = deleted_mailbox.ref_mailbox.parent() {
                                self.mailbox_entries
                                    .entry(parent_hash)
                                    .and_modify(|parent| {
                                        parent.ref_mailbox =
                                            mailboxes.remove(&parent_hash).unwrap();
                                    });
                            }
                            self.collection
                                .mailboxes
                                .write()
                                .unwrap()
                                .remove(&mailbox_hash);
                            build_mailboxes_order(
                                &mut self.tree,
                                &self.mailbox_entries,
                                &mut self.mailboxes_order,
                            );
                            // [ref:FIXME] remove from settings as well

                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(
                                        format!("{}: mailbox deleted successfully", &self.name)
                                            .into(),
                                    ),
                                    source: None,
                                    body: "".into(),
                                    kind: Some(crate::types::NotificationType::Info),
                                },
                            ));
                        }
                    }
                }
                //JobRequest::RenameMailbox,
                JobRequest::SetMailboxPermissions { ref mut handle, .. } => {
                    match handle.chan.try_recv() {
                        Err(_) => { /* canceled */ }
                        Ok(None) => {}
                        Ok(Some(Err(err))) => {
                            self.main_loop_handler
                                .job_executor
                                .set_job_success(job_id, false);
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(
                                        format!(
                                            "{}: could not set mailbox permissions",
                                            &self.name
                                        )
                                        .into(),
                                    ),
                                    source: None,
                                    body: err.to_string().into(),
                                    kind: Some(crate::types::NotificationType::Error(err.kind)),
                                },
                            ));
                        }
                        Ok(Some(Ok(_))) => {
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(
                                        format!(
                                            "{}: mailbox permissions set successfully",
                                            &self.name
                                        )
                                        .into(),
                                    ),
                                    source: None,
                                    body: "".into(),
                                    kind: Some(crate::types::NotificationType::Info),
                                },
                            ));
                        }
                    }
                }
                JobRequest::SetMailboxSubscription {
                    ref mut handle,
                    ref mailbox_hash,
                    ref new_value,
                } => {
                    match handle.chan.try_recv() {
                        Err(_) => { /* canceled */ }
                        Ok(None) => {}
                        Ok(Some(Err(err))) => {
                            self.main_loop_handler
                                .job_executor
                                .set_job_success(job_id, false);
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(
                                        format!(
                                            "{}: could not set mailbox subscription",
                                            &self.name
                                        )
                                        .into(),
                                    ),
                                    source: None,
                                    body: err.to_string().into(),
                                    kind: Some(crate::types::NotificationType::Error(err.kind)),
                                },
                            ));
                        }
                        Ok(Some(Ok(()))) if self.mailbox_entries.contains_key(mailbox_hash) => {
                            self.mailbox_entries.entry(*mailbox_hash).and_modify(|m| {
                                m.conf.mailbox_conf.subscribe = if *new_value {
                                    super::ToggleFlag::True
                                } else {
                                    super::ToggleFlag::False
                                };
                                let _ = m.ref_mailbox.set_is_subscribed(*new_value);
                            });
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(
                                        format!(
                                            "{}: `{}` has been {}subscribed.",
                                            &self.name,
                                            self.mailbox_entries[mailbox_hash].name(),
                                            if *new_value { "" } else { "un" }
                                        )
                                        .into(),
                                    ),
                                    source: None,
                                    body: "".into(),
                                    kind: Some(crate::types::NotificationType::Info),
                                },
                            ));
                        }
                        Ok(Some(Ok(()))) => {}
                    }
                }
                JobRequest::Watch { ref mut handle } => {
                    log::trace!("JobRequest::Watch finished??? ");
                    if let Ok(Some(Err(err))) = handle.chan.try_recv() {
                        if err.kind.is_timeout() {
                            self.watch();
                        } else {
                            self.main_loop_handler
                                .job_executor
                                .set_job_success(job_id, false);
                            // [ref:TODO]: relaunch watch job with ratelimit for failure
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(
                                        format!("{}: watch thread failed", &self.name).into(),
                                    ),
                                    source: None,
                                    body: err.to_string().into(),
                                    kind: Some(crate::types::NotificationType::Error(err.kind)),
                                },
                            ));
                        }
                    }
                }
                JobRequest::Generic {
                    ref name,
                    ref mut handle,
                    ref mut on_finish,
                    log_level,
                } => {
                    match handle.chan.try_recv() {
                        Ok(Some(Err(err))) => {
                            self.main_loop_handler
                                .job_executor
                                .set_job_success(job_id, false);
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(format!("{}: {} failed", &self.name, name,).into()),
                                    source: None,
                                    body: err.to_string().into(),
                                    kind: Some(crate::types::NotificationType::Error(err.kind)),
                                },
                            ));
                        }
                        Ok(Some(Ok(()))) if on_finish.is_none() => {
                            if log_level <= LogLevel::INFO {
                                self.main_loop_handler.send(ThreadEvent::UIEvent(
                                    UIEvent::Notification {
                                        title: Some(
                                            format!("{}: {} succeeded", &self.name, name,).into(),
                                        ),
                                        source: None,
                                        body: "".into(),
                                        kind: Some(crate::types::NotificationType::Info),
                                    },
                                ));
                            }
                        }
                        Err(_) => { /* canceled */ }
                        Ok(Some(Ok(()))) | Ok(None) => {}
                    }
                    if on_finish.is_some() {
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Callback(
                                on_finish.take().unwrap(),
                            )));
                    }
                }
            }
            true
        } else {
            false
        }
    }

    pub fn insert_job(&mut self, job_id: JobId, job: JobRequest) {
        self.active_jobs.insert(job_id, job);
        self.active_job_instants
            .insert(std::time::Instant::now(), job_id);
        self.main_loop_handler
            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                StatusEvent::NewJob(job_id),
            )));
    }

    pub fn cancel_job(&mut self, job_id: JobId) -> Option<JobRequest> {
        if let Some(req) = self.active_jobs.remove(&job_id) {
            self.main_loop_handler
                .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                    StatusEvent::JobCanceled(job_id),
                )));
            Some(req)
        } else {
            None
        }
    }
}

impl Index<&MailboxHash> for Account {
    type Output = MailboxEntry;
    fn index(&self, index: &MailboxHash) -> &MailboxEntry {
        &self.mailbox_entries[index]
    }
}

impl IndexMut<&MailboxHash> for Account {
    fn index_mut(&mut self, index: &MailboxHash) -> &mut MailboxEntry {
        self.mailbox_entries.get_mut(index).unwrap()
    }
}
