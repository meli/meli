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

/*!
 * Account management from user configuration.
 */

use super::{AccountConf, FileMailboxConf};
use crate::jobs::{JobExecutor, JobId, JoinHandle};
use indexmap::IndexMap;
use melib::backends::*;
use melib::email::*;
use melib::error::{ErrorKind, MeliError, Result};
use melib::text_processing::GlobMatch;
use melib::thread::{SortField, SortOrder, Threads};
use melib::AddressBook;
use melib::Collection;
use smallvec::SmallVec;
use std::collections::BTreeMap;
use std::collections::{HashMap, HashSet};

use crate::types::UIEvent::{self, EnvelopeRemove, EnvelopeRename, EnvelopeUpdate, Notification};
use crate::{StatusEvent, ThreadEvent};
use crossbeam::channel::Sender;
use futures::{
    future::FutureExt,
    stream::{Stream, StreamExt},
};
use std::borrow::Cow;
use std::collections::VecDeque;
use std::convert::TryFrom;
use std::fs;
use std::future::Future;
use std::io;
use std::ops::{Index, IndexMut};
use std::os::unix::fs::PermissionsExt;
use std::pin::Pin;
use std::result;
use std::sync::{Arc, RwLock};

#[macro_export]
macro_rules! try_recv_timeout {
    ($oneshot:expr) => {{
        const _3_MS: std::time::Duration = std::time::Duration::from_millis(95);
        let now = std::time::Instant::now();
        let mut res = Ok(None);
        while now + _3_MS >= std::time::Instant::now() {
            res = $oneshot.try_recv().map_err(|_| MeliError::new("canceled"));
            if res.as_ref().map(|r| r.is_some()).unwrap_or(false) || res.is_err() {
                break;
            }
        }
        res
    }};
}

#[derive(Debug, Clone)]
pub enum MailboxStatus {
    Available,
    Failed(MeliError),
    /// first argument is done work, and second is total work
    Parsing(usize, usize),
    None,
}

impl Default for MailboxStatus {
    fn default() -> Self {
        MailboxStatus::None
    }
}

impl MailboxStatus {
    pub fn is_available(&self) -> bool {
        matches!(self, MailboxStatus::Available)
    }

    pub fn is_parsing(&self) -> bool {
        matches!(self, MailboxStatus::Parsing(_, _))
    }
}

#[derive(Debug, Clone)]
pub struct MailboxEntry {
    pub status: MailboxStatus,
    pub name: String,
    pub ref_mailbox: Mailbox,
    pub conf: FileMailboxConf,
}

impl MailboxEntry {
    pub fn status(&self) -> String {
        match self.status {
            MailboxStatus::Available => format!(
                "{} [{} messages]",
                self.name(),
                self.ref_mailbox.count().ok().unwrap_or((0, 0)).1
            ),
            MailboxStatus::Failed(ref e) => e.to_string(),
            MailboxStatus::None => "Retrieving mailbox.".to_string(),
            MailboxStatus::Parsing(done, total) => {
                format!("Parsing messages. [{}/{}]", done, total)
            }
        }
    }

    pub fn name(&self) -> &str {
        if let Some(name) = self.conf.mailbox_conf.alias.as_ref() {
            name
        } else {
            self.ref_mailbox.name()
        }
    }
}

#[derive(Debug)]
pub struct Account {
    pub name: String,
    pub hash: AccountHash,
    pub is_online: Result<()>,
    pub mailbox_entries: IndexMap<MailboxHash, MailboxEntry>,
    pub mailboxes_order: Vec<MailboxHash>,
    pub tree: Vec<MailboxNode>,
    pub sent_mailbox: Option<MailboxHash>,
    pub collection: Collection,
    pub address_book: AddressBook,
    pub settings: AccountConf,
    pub backend: Arc<RwLock<Box<dyn MailBackend>>>,

    pub job_executor: Arc<JobExecutor>,
    pub active_jobs: HashMap<JobId, JobRequest>,
    pub active_job_instants: BTreeMap<std::time::Instant, JobId>,
    pub sender: Sender<ThreadEvent>,
    pub event_queue: VecDeque<(MailboxHash, RefreshEvent)>,
    pub backend_capabilities: MailBackendCapabilities,
}

pub enum JobRequest {
    Mailboxes {
        handle: JoinHandle<Result<HashMap<MailboxHash, Mailbox>>>,
    },
    Fetch {
        mailbox_hash: MailboxHash,
        #[allow(clippy::type_complexity)]
        handle: JoinHandle<(
            Option<Result<Vec<Envelope>>>,
            Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>,
        )>,
    },
    Generic {
        name: Cow<'static, str>,
        logging_level: melib::LoggingLevel,
        handle: JoinHandle<Result<()>>,
        on_finish: Option<crate::types::CallbackFn>,
    },
    IsOnline {
        handle: JoinHandle<Result<()>>,
    },
    Refresh {
        mailbox_hash: MailboxHash,
        handle: JoinHandle<Result<()>>,
    },
    SetFlags {
        env_hashes: EnvelopeHashBatch,
        handle: JoinHandle<Result<()>>,
    },
    SaveMessage {
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        handle: JoinHandle<Result<()>>,
    },
    SendMessage,
    SendMessageBackground {
        handle: JoinHandle<Result<()>>,
    },
    DeleteMessages {
        env_hashes: EnvelopeHashBatch,
        handle: JoinHandle<Result<()>>,
    },
    CreateMailbox {
        path: String,
        handle: JoinHandle<Result<(MailboxHash, HashMap<MailboxHash, Mailbox>)>>,
    },
    DeleteMailbox {
        mailbox_hash: MailboxHash,
        handle: JoinHandle<Result<HashMap<MailboxHash, Mailbox>>>,
    },
    //RenameMailbox,
    SetMailboxPermissions {
        mailbox_hash: MailboxHash,
        handle: JoinHandle<Result<()>>,
    },
    SetMailboxSubscription {
        mailbox_hash: MailboxHash,
        new_value: bool,
        handle: JoinHandle<Result<()>>,
    },
    Watch {
        handle: JoinHandle<Result<()>>,
    },
}

impl Drop for JobRequest {
    fn drop(&mut self) {
        match self {
            JobRequest::Generic { handle, .. } |
            JobRequest::IsOnline { handle, .. } |
            JobRequest::Refresh { handle, .. } |
            JobRequest::SetFlags { handle, .. } |
            JobRequest::SaveMessage { handle, .. } |
            //JobRequest::RenameMailbox,
            JobRequest::SetMailboxPermissions { handle, .. } |
            JobRequest::SetMailboxSubscription { handle, .. } |
            JobRequest::Watch { handle, .. } |
            JobRequest::SendMessageBackground { handle, .. } => {
                handle.cancel();
            }
            JobRequest::DeleteMessages { handle, .. } => {
                handle.cancel();
            }
            JobRequest::CreateMailbox { handle, .. } => {
                handle.cancel();
            }
            JobRequest::DeleteMailbox { handle, .. } => {
                handle.cancel();
            }
            JobRequest::Fetch { handle, .. } => {
                handle.cancel();
            }
            JobRequest::Mailboxes { handle, .. } => {
                handle.cancel();
            }
            JobRequest::SendMessage => {}
        }
    }
}

impl core::fmt::Debug for JobRequest {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            JobRequest::Generic { name, .. } => write!(f, "JobRequest::Generic({})", name),
            JobRequest::Mailboxes { .. } => write!(f, "JobRequest::Mailboxes"),
            JobRequest::Fetch { mailbox_hash, .. } => {
                write!(f, "JobRequest::Fetch({})", mailbox_hash)
            }
            JobRequest::IsOnline { .. } => write!(f, "JobRequest::IsOnline"),
            JobRequest::Refresh { .. } => write!(f, "JobRequest::Refresh"),
            JobRequest::SetFlags { .. } => write!(f, "JobRequest::SetFlags"),
            JobRequest::SaveMessage { .. } => write!(f, "JobRequest::SaveMessage"),
            JobRequest::DeleteMessages { .. } => write!(f, "JobRequest::DeleteMessages"),
            JobRequest::CreateMailbox { .. } => write!(f, "JobRequest::CreateMailbox"),
            JobRequest::DeleteMailbox { mailbox_hash, .. } => {
                write!(f, "JobRequest::DeleteMailbox({})", mailbox_hash)
            }
            //JobRequest::RenameMailbox,
            JobRequest::SetMailboxPermissions { .. } => {
                write!(f, "JobRequest::SetMailboxPermissions")
            }
            JobRequest::SetMailboxSubscription { .. } => {
                write!(f, "JobRequest::SetMailboxSubscription")
            }
            JobRequest::Watch { .. } => write!(f, "JobRequest::Watch"),
            JobRequest::SendMessage => write!(f, "JobRequest::SendMessage"),
            JobRequest::SendMessageBackground { .. } => {
                write!(f, "JobRequest::SendMessageBackground")
            }
        }
    }
}

impl core::fmt::Display for JobRequest {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            JobRequest::Generic { name, .. } => write!(f, "{}", name),
            JobRequest::Mailboxes { .. } => write!(f, "Get mailbox list"),
            JobRequest::Fetch { .. } => write!(f, "Mailbox fetch"),
            JobRequest::IsOnline { .. } => write!(f, "Online status check"),
            JobRequest::Refresh { .. } => write!(f, "Refresh mailbox"),
            JobRequest::SetFlags { env_hashes, .. } => write!(
                f,
                "Set flags for {} message{}",
                env_hashes.len(),
                if env_hashes.len() == 1 { "" } else { "s" }
            ),
            JobRequest::SaveMessage { .. } => write!(f, "Save message"),
            JobRequest::DeleteMessages { env_hashes, .. } => write!(
                f,
                "Delete {} message{}",
                env_hashes.len(),
                if env_hashes.len() == 1 { "" } else { "s" }
            ),
            JobRequest::CreateMailbox { path, .. } => write!(f, "Create mailbox {}", path),
            JobRequest::DeleteMailbox { .. } => write!(f, "Delete mailbox"),
            //JobRequest::RenameMailbox,
            JobRequest::SetMailboxPermissions { .. } => write!(f, "Set mailbox permissions"),
            JobRequest::SetMailboxSubscription { .. } => write!(f, "Set mailbox subscription"),
            JobRequest::Watch { .. } => write!(f, "Background watch"),
            JobRequest::SendMessageBackground { .. } | JobRequest::SendMessage => {
                write!(f, "Sending message")
            }
        }
    }
}

impl JobRequest {
    pub fn is_watch(&self) -> bool {
        matches!(self, JobRequest::Watch { .. })
    }

    pub fn is_fetch(&self, mailbox_hash: MailboxHash) -> bool {
        matches!(self, JobRequest::Fetch {
                 mailbox_hash: h, ..
             } if *h == mailbox_hash)
    }

    pub fn is_online(&self) -> bool {
        matches!(self, JobRequest::IsOnline { .. })
    }
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

#[derive(Serialize, Debug, Clone, Default)]
pub struct MailboxNode {
    pub hash: MailboxHash,
    pub depth: usize,
    pub indentation: u32,
    pub has_sibling: bool,
    pub children: Vec<MailboxNode>,
}

impl Account {
    pub fn new(
        hash: AccountHash,
        name: String,
        mut settings: AccountConf,
        map: &Backends,
        job_executor: Arc<JobExecutor>,
        sender: Sender<ThreadEvent>,
        event_consumer: BackendEventConsumer,
    ) -> Result<Self> {
        let s = settings.clone();
        let backend = map.get(settings.account().format())(
            settings.account(),
            Box::new(move |path: &str| {
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
                    job_executor.spawn_specialized(online_job.then(|_| mailboxes_job))
                } else {
                    job_executor.spawn_blocking(online_job.then(|_| mailboxes_job))
                };
                let job_id = handle.job_id;
                active_jobs.insert(job_id, JobRequest::Mailboxes { handle });
                active_job_instants.insert(std::time::Instant::now(), job_id);
                sender
                    .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                        StatusEvent::NewJob(job_id),
                    )))
                    .unwrap();
            }
        }

        #[cfg(feature = "sqlite3")]
        if settings.conf.search_backend == crate::conf::SearchBackend::Sqlite3 {
            let db_path = match crate::sqlite3::db_path() {
                Err(err) => {
                    sender
                        .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!("Error with setting up an sqlite3 search database for account `{}`: {}", name, err))
                        )))
                        .unwrap();
                    None
                }
                Ok(path) => Some(path),
            };
            if let Some(db_path) = db_path {
                if !db_path.exists() {
                    sender
                        .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!("An sqlite3 search database for account `{}` seems to be missing, a new one must be created with the `reindex` command.", name))
                        )))
                        .unwrap();
                }
            }
        }

        Ok(Account {
            hash,
            name,
            is_online: if !backend.capabilities().is_remote {
                Ok(())
            } else {
                Err(MeliError::new("Attempting connection."))
            },
            mailbox_entries: Default::default(),
            mailboxes_order: Default::default(),
            tree: Default::default(),
            address_book,
            sent_mailbox: Default::default(),
            collection: backend.collection(),
            settings,
            sender,
            job_executor,
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

        /* Keep track of which mailbox config values we encounter in the actual mailboxes returned
         * by the backend. For each of the actual mailboxes, delete the key from the hash set. If
         * any are left, they are misconfigurations (eg misspelling) and a warning is shown to the
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
                    MailboxEntry {
                        ref_mailbox: f.clone(),
                        name: f.path().to_string(),
                        status: MailboxStatus::None,
                        conf: conf.clone(),
                    },
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
                    MailboxEntry {
                        ref_mailbox: f.clone(),
                        name: f.path().to_string(),
                        status: MailboxStatus::None,
                        conf: new,
                    },
                );
            }
        }

        for missing_mailbox in &mailbox_conf_hash_set {
            melib::log(
                format!(
                    "Account `{}` mailbox `{}` configured but not present in account's mailboxes. Is it misspelled?",
                    &self.name, missing_mailbox,
                ),
                melib::WARN,
            );
            self.sender
                .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                    "Account `{}` mailbox `{}` configured but not present in account's mailboxes. Is it misspelled?",
                                    &self.name, missing_mailbox,
                            )),
                )))
                .unwrap();
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
            melib::log(
                format!(
                    "Account `{}` has the following mailboxes: [{}]",
                    &self.name, mailbox_comma_sep_list_string,
                ),
                melib::WARN,
            );
            self.sender
                .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                    StatusEvent::DisplayMessage(format!(
                        "Account `{}` has the following mailboxes: [{}]",
                        &self.name, mailbox_comma_sep_list_string,
                    )),
                )))
                .unwrap();
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
                            self.job_executor.spawn_specialized(mailbox_job)
                        } else {
                            self.job_executor.spawn_blocking(mailbox_job)
                        };
                        let job_id = handle.job_id;
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                                StatusEvent::NewJob(job_id),
                            )))
                            .unwrap();
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
            //let mailbox: &mut Mailbox = self.mailboxes[idx].as_mut().unwrap().as_mut().unwrap();
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
                    if self.settings.conf.search_backend == crate::conf::SearchBackend::Sqlite3 {
                        match crate::sqlite3::remove(old_hash).map(|_| {
                            crate::sqlite3::insert(
                                (*envelope).clone(),
                                self.backend.clone(),
                                self.name.clone(),
                            )
                        }) {
                            Err(err) => {
                                melib::log(
                                    format!(
                                        "Failed to update envelope {} in cache: {}",
                                        envelope.message_id_display(),
                                        err
                                    ),
                                    melib::ERROR,
                                );
                            }
                            Ok(job) => {
                                let handle = self.job_executor.spawn_blocking(job);
                                self.insert_job(
                                    handle.job_id,
                                    JobRequest::Generic {
                                        name: format!(
                                            "Update envelope {} in sqlite3 cache",
                                            envelope.message_id_display()
                                        )
                                        .into(),
                                        handle,
                                        logging_level: melib::LoggingLevel::TRACE,
                                        on_finish: None,
                                    },
                                );
                            }
                        }
                    }
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
                            entry.labels_mut().clear();
                            entry
                                .labels_mut()
                                .extend(tags.into_iter().map(|h| tag_hash!(h)));
                            entry.set_flags(flags);
                        });
                    #[cfg(feature = "sqlite3")]
                    if self.settings.conf.search_backend == crate::conf::SearchBackend::Sqlite3 {
                        match crate::sqlite3::remove(env_hash).map(|_| {
                            crate::sqlite3::insert(
                                self.collection.envelopes.read().unwrap()[&env_hash].clone(),
                                self.backend.clone(),
                                self.name.clone(),
                            )
                        }) {
                            Ok(job) => {
                                let handle = self.job_executor.spawn_blocking(job);
                                self.insert_job(
                                    handle.job_id,
                                    JobRequest::Generic {
                                        name: format!(
                                            "Update envelope {} in sqlite3 cache",
                                            self.collection.envelopes.read().unwrap()[&env_hash]
                                                .message_id_display()
                                        )
                                        .into(),
                                        handle,
                                        logging_level: melib::LoggingLevel::TRACE,
                                        on_finish: None,
                                    },
                                );
                            }
                            Err(err) => {
                                melib::log(
                                    format!(
                                        "Failed to update envelope {} in cache: {}",
                                        self.collection.envelopes.read().unwrap()[&env_hash]
                                            .message_id_display(),
                                        err
                                    ),
                                    melib::ERROR,
                                );
                            }
                        }
                    }
                    self.collection.update_flags(env_hash, mailbox_hash);
                    return Some(EnvelopeUpdate(env_hash));
                }
                RefreshEventKind::Rename(old_hash, new_hash) => {
                    debug!("rename {} to {}", old_hash, new_hash);
                    if !self.collection.rename(old_hash, new_hash, mailbox_hash) {
                        return Some(EnvelopeRename(old_hash, new_hash));
                    }
                    #[cfg(feature = "sqlite3")]
                    if self.settings.conf.search_backend == crate::conf::SearchBackend::Sqlite3 {
                        match crate::sqlite3::remove(old_hash).map(|_| {
                            crate::sqlite3::insert(
                                self.collection.envelopes.read().unwrap()[&new_hash].clone(),
                                self.backend.clone(),
                                self.name.clone(),
                            )
                        }) {
                            Err(err) => {
                                melib::log(
                                    format!(
                                        "Failed to update envelope {} in cache: {}",
                                        &self.collection.envelopes.read().unwrap()[&new_hash]
                                            .message_id_display(),
                                        err
                                    ),
                                    melib::ERROR,
                                );
                            }
                            Ok(job) => {
                                let handle = self.job_executor.spawn_blocking(job);
                                self.insert_job(
                                    handle.job_id,
                                    JobRequest::Generic {
                                        name: format!(
                                            "Update envelope {} in sqlite3 cache",
                                            self.collection.envelopes.read().unwrap()[&new_hash]
                                                .message_id_display()
                                        )
                                        .into(),
                                        handle,
                                        logging_level: melib::LoggingLevel::TRACE,
                                        on_finish: None,
                                    },
                                );
                            }
                        }
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
                        let handle = self.job_executor.spawn_blocking(crate::sqlite3::insert(
                            (*envelope).clone(),
                            self.backend.clone(),
                            self.name.clone(),
                        ));
                        self.insert_job(
                            handle.job_id,
                            JobRequest::Generic {
                                name: format!(
                                    "Update envelope {} in sqlite3 cache",
                                    envelope.message_id_display()
                                )
                                .into(),
                                handle,
                                logging_level: melib::LoggingLevel::TRACE,
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

                    return Some(Notification(
                        Some(format!("new e-mail from: {}", from)),
                        format!(
                            "{}\n{} {}",
                            subject,
                            self.name,
                            self.mailbox_entries[&mailbox_hash].name()
                        ),
                        Some(crate::types::NotificationType::NewMail),
                    ));
                }
                RefreshEventKind::Remove(env_hash) => {
                    if !self.collection.contains_key(&env_hash) {
                        return None;
                    }
                    #[cfg(feature = "sqlite3")]
                    if self.settings.conf.search_backend == crate::conf::SearchBackend::Sqlite3 {
                        if let Err(err) = crate::sqlite3::remove(env_hash) {
                            let envelopes = self.collection.envelopes.read().unwrap();
                            melib::log(
                                format!(
                                    "Failed to remove envelope {} [{}] in cache: {}",
                                    &envelopes[&env_hash].message_id_display(),
                                    env_hash,
                                    err
                                ),
                                melib::ERROR,
                            );
                        }
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
                    debug!("RefreshEvent Failure: {}", err.to_string());
                    while let Some((job_id, _)) =
                        self.active_jobs.iter().find(|(_, j)| j.is_watch())
                    {
                        let job_id = *job_id;
                        let j = self.active_jobs.remove(&job_id);
                        drop(j);
                    }
                    /*
                    context
                        .1
                        .send(ThreadEvent::UIEvent(UIEvent::Notification(
                            Some(format!("{} watcher exited with error", &self.name)),
                            e.to_string(),
                            Some(crate::types::NotificationType::Error(err.kind)),
                        )))
                        .expect("Could not send event on main channel");
                    */
                    self.watch();
                    return Some(Notification(
                        Some("Account watch failed".into()),
                        err.to_string(),
                        Some(crate::types::NotificationType::Error(err.kind)),
                    ));
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
                .args(&["-c", refresh_command])
                .stdin(std::process::Stdio::null())
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::piped())
                .spawn()?;
            self.sender
                .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                    StatusEvent::DisplayMessage(format!("Running command {}", refresh_command)),
                )))
                .unwrap();
            self.sender
                .send(ThreadEvent::UIEvent(UIEvent::Fork(
                    crate::ForkType::Generic(child),
                )))
                .unwrap();
            return Ok(());
        }
        let refresh_job = self.backend.write().unwrap().refresh(mailbox_hash);
        if let Ok(refresh_job) = refresh_job {
            let handle = if self.backend_capabilities.is_async {
                self.job_executor.spawn_specialized(refresh_job)
            } else {
                self.job_executor.spawn_blocking(refresh_job)
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
        if self.settings.account().manual_refresh {
            return;
        }

        if !self.active_jobs.values().any(|j| j.is_watch()) {
            match self.backend.read().unwrap().watch() {
                Ok(fut) => {
                    let handle = if self.backend_capabilities.is_async {
                        self.job_executor.spawn_specialized(fut)
                    } else {
                        self.job_executor.spawn_blocking(fut)
                    };
                    self.active_jobs
                        .insert(handle.job_id, JobRequest::Watch { handle });
                }
                Err(e)
                    if e.kind == ErrorKind::NotSupported || e.kind == ErrorKind::NotImplemented => {
                }
                Err(e) => {
                    self.sender
                        .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Account `{}` watch action returned error: {}",
                                &self.name, e
                            )),
                        )))
                        .unwrap();
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

    pub fn mailboxes_order(&self) -> &Vec<MailboxHash> {
        &self.mailboxes_order
    }
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn hash(&self) -> AccountHash {
        self.hash
    }

    pub fn load(&mut self, mailbox_hash: MailboxHash) -> result::Result<(), usize> {
        if mailbox_hash == 0 {
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
                                self.job_executor.spawn_specialized(mailbox_job)
                            } else {
                                self.job_executor.spawn_blocking(mailbox_job)
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
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::StartupCheck(mailbox_hash)))
                                .unwrap();
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
                    debug!("{:?} could not save msg", err);
                    melib::log(
                        format!("Could not save in '{}' mailbox: {}.", *mailbox_hash, err),
                        melib::ERROR,
                    );
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
            let file = crate::types::create_temp_file(bytes, None, None, false);
            debug!("message saved in {}", file.path.display());
            melib::log(
                format!(
                    "Message was stored in {} so that you can restore it manually.",
                    file.path.display()
                ),
                melib::INFO,
            );
            Err(MeliError::new(format!(
                "Message was stored in {} so that you can restore it manually.",
                file.path.display()
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
        if self.settings.account.read_only() {
            return Err(MeliError::new(format!(
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
            self.job_executor.spawn_specialized(job)
        } else {
            self.job_executor.spawn_blocking(job)
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
        complete_in_background: bool,
    ) -> Result<Option<JoinHandle<Result<()>>>> {
        use crate::conf::composing::SendMail;
        use std::io::Write;
        use std::process::{Command, Stdio};
        debug!(&send_mail);
        match send_mail {
            SendMail::ShellCommand(ref command) => {
                if command.is_empty() {
                    return Err(MeliError::new(
                        "send_mail shell command configuration value is empty",
                    ));
                }
                let mut msmtp = Command::new("sh")
                    .args(&["-c", command])
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
                    melib::log("Message sent.", melib::LoggingLevel::TRACE);
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
                    melib::log(&error_message, melib::LoggingLevel::ERROR);
                    return Err(MeliError::new(error_message).set_summary("Message not sent."));
                }
                Ok(None)
            }
            #[cfg(feature = "smtp")]
            SendMail::Smtp(conf) => {
                let handle = self.job_executor.spawn_specialized(async move {
                    let mut smtp_connection =
                        melib::smtp::SmtpConnection::new_connection(conf).await?;
                    smtp_connection.mail_transaction(&message, None).await
                });
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
                        self.job_executor.spawn_specialized(job)
                    } else {
                        self.job_executor.spawn_blocking(job)
                    };
                    self.insert_job(handle.job_id, JobRequest::SendMessageBackground { handle });
                    return Ok(None);
                }
                Err(MeliError::new("Server does not support submission.")
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
        |message: Arc<String>| -> Pin<Box<dyn Future<Output = Result<()>> + Send>> {
            Box::pin(async move {
                use crate::conf::composing::SendMail;
                use std::io::Write;
                use std::process::{Command, Stdio};
                match send_mail {
                    SendMail::ShellCommand(ref command) => {
                        if command.is_empty() {
                            return Err(MeliError::new(
                                "send_mail shell command configuration value is empty",
                            ));
                        }
                        let mut msmtp = Command::new("sh")
                            .args(&["-c", command])
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
                            melib::log("Message sent.", melib::LoggingLevel::TRACE);
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
                            melib::log(&error_message, melib::LoggingLevel::ERROR);
                            return Err(
                                MeliError::new(error_message).set_summary("Message not sent.")
                            );
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
                        Err(MeliError::new("Server does not support submission.")
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
        Ok(if self.settings.account.read_only() {
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
        if self.settings.account.read_only() {
            return Err(MeliError::new("Account is read-only."));
        }
        match op {
            MailboxOperation::Create(path) => {
                let job = self
                    .backend
                    .write()
                    .unwrap()
                    .create_mailbox(path.to_string())?;
                let handle = if self.backend_capabilities.is_async {
                    self.job_executor.spawn_specialized(job)
                } else {
                    self.job_executor.spawn_blocking(job)
                };
                self.insert_job(handle.job_id, JobRequest::CreateMailbox { path, handle });
                Ok(())
            }
            MailboxOperation::Delete(path) => {
                if self.mailbox_entries.len() == 1 {
                    return Err(MeliError::new("Cannot delete only mailbox."));
                }

                let mailbox_hash = self.mailbox_by_path(&path)?;
                let job = self.backend.write().unwrap().delete_mailbox(mailbox_hash)?;
                let handle = if self.backend_capabilities.is_async {
                    self.job_executor.spawn_specialized(job)
                } else {
                    self.job_executor.spawn_blocking(job)
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
                    self.job_executor.spawn_specialized(job)
                } else {
                    self.job_executor.spawn_blocking(job)
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
                    self.job_executor.spawn_specialized(job)
                } else {
                    self.job_executor.spawn_blocking(job)
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
            MailboxOperation::Rename(_, _) => Err(MeliError::new("Not implemented.")),
            MailboxOperation::SetPermissions(_) => Err(MeliError::new("Not implemented.")),
        }
    }

    pub fn special_use_mailbox(&self, special_use: SpecialUsageMailbox) -> Option<MailboxHash> {
        let ret = self
            .mailbox_entries
            .iter()
            .find(|(_, f)| f.conf.mailbox_conf().usage == Some(special_use));
        ret.as_ref().map(|ret| ret.1.ref_mailbox.hash())
    }

    /* Call only in Context::is_online, since only Context can launch the watcher threads if an
     * account goes from offline to online. */
    pub fn is_online(&mut self) -> Result<()> {
        if !self.backend_capabilities.is_remote && !self.backend_capabilities.is_async {
            return Ok(());
        }

        if self.is_online.is_err()
            && self
                .is_online
                .as_ref()
                .unwrap_err()
                .kind
                .is_authentication()
        {
            return self.is_online.clone();
        }
        if self.is_online.is_ok() {
            return Ok(());
        }
        if !self.active_jobs.values().any(JobRequest::is_online) {
            let online_job = self.backend.read().unwrap().is_online();
            if let Ok(online_job) = online_job {
                let handle = if self.backend_capabilities.is_async {
                    self.job_executor.spawn_specialized(online_job)
                } else {
                    self.job_executor.spawn_blocking(online_job)
                };
                self.insert_job(handle.job_id, JobRequest::IsOnline { handle });
            }
        }
        self.is_online.clone()
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
            crate::conf::SearchBackend::Sqlite3 => crate::sqlite3::search(&query, _sort),
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
            Err(MeliError::new("Mailbox with that path not found."))
        }
    }

    pub fn process_event(&mut self, job_id: &JobId) -> bool {
        self.sender
            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                StatusEvent::JobFinished(*job_id),
            )))
            .unwrap();

        if let Some(mut job) = self.active_jobs.remove(job_id) {
            match job {
                JobRequest::Mailboxes { ref mut handle } => {
                    if let Ok(Some(mailboxes)) = handle.chan.try_recv() {
                        if let Err(err) = mailboxes.and_then(|mailboxes| self.init(mailboxes)) {
                            if err.kind.is_authentication() {
                                self.sender
                                    .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                        Some(format!("{}: authentication error", &self.name)),
                                        err.to_string(),
                                        Some(crate::types::NotificationType::Error(err.kind)),
                                    )))
                                    .expect("Could not send event on main channel");
                                self.is_online = Err(err);
                                return true;
                            }
                            let mailboxes_job = self.backend.read().unwrap().mailboxes();
                            if let Ok(mailboxes_job) = mailboxes_job {
                                let handle = if self.backend_capabilities.is_async {
                                    self.job_executor.spawn_specialized(mailboxes_job)
                                } else {
                                    self.job_executor.spawn_blocking(mailboxes_job)
                                };
                                self.insert_job(handle.job_id, JobRequest::Mailboxes { handle });
                            };
                        } else {
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::AccountStatusChange(
                                    self.hash,
                                    Some("Loaded mailboxes.".into()),
                                )))
                                .unwrap();
                        }
                    }
                }
                JobRequest::Fetch {
                    mailbox_hash,
                    ref mut handle,
                    ..
                } => {
                    debug!("got payload in status for {}", mailbox_hash);
                    match handle.chan.try_recv() {
                        Err(_) => {
                            /* canceled */
                            return true;
                        }
                        Ok(None) => {
                            return true;
                        }
                        Ok(Some((None, _))) => {
                            debug!("finished in status for {}", mailbox_hash);
                            self.mailbox_entries
                                .entry(mailbox_hash)
                                .and_modify(|entry| {
                                    entry.status = MailboxStatus::Available;
                                });
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::MailboxUpdate((
                                    self.hash,
                                    mailbox_hash,
                                ))))
                                .unwrap();
                            return true;
                        }
                        Ok(Some((Some(Err(err)), _))) => {
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!("{}: could not fetch mailbox", &self.name)),
                                    err.to_string(),
                                    Some(crate::types::NotificationType::Error(err.kind)),
                                )))
                                .expect("Could not send event on main channel");
                            self.mailbox_entries
                                .entry(mailbox_hash)
                                .and_modify(|entry| {
                                    entry.status = MailboxStatus::Failed(err);
                                });
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::MailboxUpdate((
                                    self.hash,
                                    mailbox_hash,
                                ))))
                                .unwrap();
                            return true;
                        }
                        Ok(Some((Some(Ok(payload)), rest))) => {
                            let handle = if self.backend_capabilities.is_async {
                                self.job_executor.spawn_specialized(rest.into_future())
                            } else {
                                self.job_executor.spawn_blocking(rest.into_future())
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
                                    self.sender
                                        .send(ThreadEvent::UIEvent(UIEvent::MailboxUpdate((
                                            self.hash, f,
                                        ))))
                                        .unwrap();
                                }
                            }
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::MailboxUpdate((
                                    self.hash,
                                    mailbox_hash,
                                ))))
                                .unwrap();
                        }
                    }
                }
                JobRequest::IsOnline { ref mut handle, .. } => {
                    if let Ok(Some(is_online)) = handle.chan.try_recv() {
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::AccountStatusChange(
                                self.hash, None,
                            )))
                            .unwrap();
                        if is_online.is_ok() {
                            if self.is_online.is_err()
                                && !self
                                    .is_online
                                    .as_ref()
                                    .unwrap_err()
                                    .kind
                                    .is_authentication()
                            {
                                self.watch();
                            }
                            self.is_online = Ok(());
                            return true;
                        }
                        self.is_online = is_online;
                    }
                    let online_job = self.backend.read().unwrap().is_online();
                    if let Ok(online_job) = online_job {
                        let handle = if self.backend_capabilities.is_async {
                            self.job_executor.spawn_specialized(online_job)
                        } else {
                            self.job_executor.spawn_blocking(online_job)
                        };
                        self.insert_job(handle.job_id, JobRequest::IsOnline { handle });
                    };
                }
                JobRequest::Refresh { ref mut handle, .. } => {
                    match handle.chan.try_recv() {
                        Err(_) => { /* canceled */ }
                        Ok(None) => {}
                        Ok(Some(Ok(()))) => {
                            if self.is_online.is_err()
                                && !self
                                    .is_online
                                    .as_ref()
                                    .unwrap_err()
                                    .kind
                                    .is_authentication()
                            {
                                self.watch();
                            }
                            if !(self.is_online.is_err()
                                && self
                                    .is_online
                                    .as_ref()
                                    .unwrap_err()
                                    .kind
                                    .is_authentication())
                            {
                                self.is_online = Ok(());
                                self.sender
                                    .send(ThreadEvent::UIEvent(UIEvent::AccountStatusChange(
                                        self.hash, None,
                                    )))
                                    .unwrap();
                            }
                        }
                        Ok(Some(Err(err))) => {
                            if !err.kind.is_authentication() {
                                let online_job = self.backend.read().unwrap().is_online();
                                if let Ok(online_job) = online_job {
                                    let handle = if self.backend_capabilities.is_async {
                                        self.job_executor.spawn_specialized(online_job)
                                    } else {
                                        self.job_executor.spawn_blocking(online_job)
                                    };
                                    self.insert_job(handle.job_id, JobRequest::IsOnline { handle });
                                };
                            }
                            self.is_online = Err(err);
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::AccountStatusChange(
                                    self.hash, None,
                                )))
                                .unwrap();
                        }
                    }
                }
                JobRequest::SetFlags { ref mut handle, .. } => {
                    if let Ok(Some(Err(err))) = handle.chan.try_recv() {
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                Some(format!("{}: could not set flag", &self.name)),
                                err.to_string(),
                                Some(crate::types::NotificationType::Error(err.kind)),
                            )))
                            .expect("Could not send event on main channel");
                    }
                }
                JobRequest::SaveMessage {
                    ref mut handle,
                    ref bytes,
                    ..
                } => {
                    if let Ok(Some(Err(err))) = handle.chan.try_recv() {
                        melib::log(format!("Could not save message: {}", err), melib::ERROR);
                        let file = crate::types::create_temp_file(bytes, None, None, false);
                        debug!("message saved in {}", file.path.display());
                        melib::log(
                            format!(
                                "Message was stored in {} so that you can restore it manually.",
                                file.path.display()
                            ),
                            melib::INFO,
                        );
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                Some(format!("{}: could not save message", &self.name)),
                                format!(
                                    "Message was stored in {} so that you can restore it manually.",
                                    file.path.display()
                                ),
                                Some(crate::types::NotificationType::Info),
                            )))
                            .expect("Could not send event on main channel");
                    }
                }
                JobRequest::SendMessage => {}
                JobRequest::SendMessageBackground { ref mut handle, .. } => {
                    if let Ok(Some(Err(err))) = handle.chan.try_recv() {
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                Some("Could not send message".to_string()),
                                err.to_string(),
                                Some(crate::types::NotificationType::Error(err.kind)),
                            )))
                            .expect("Could not send event on main channel");
                    }
                }
                JobRequest::DeleteMessages { ref mut handle, .. } => {
                    if let Ok(Some(Err(err))) = handle.chan.try_recv() {
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                Some(format!("{}: could not delete message", &self.name)),
                                err.to_string(),
                                Some(crate::types::NotificationType::Error(err.kind)),
                            )))
                            .expect("Could not send event on main channel");
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
                                self.sender
                                    .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                        Some(format!(
                                            "{}: could not create mailbox {}",
                                            &self.name, path
                                        )),
                                        err.to_string(),
                                        Some(crate::types::NotificationType::Error(err.kind)),
                                    )))
                                    .expect("Could not send event on main channel");
                            }
                            Ok((mailbox_hash, mut mailboxes)) => {
                                self.sender
                                    .send(ThreadEvent::UIEvent(UIEvent::MailboxCreate((
                                        self.hash,
                                        mailbox_hash,
                                    ))))
                                    .unwrap();
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
                                /* if new mailbox has parent, we need to update its children field */
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
                                    MailboxEntry {
                                        name: mailboxes[&mailbox_hash].path().to_string(),
                                        status,
                                        conf: new,
                                        ref_mailbox: mailboxes.remove(&mailbox_hash).unwrap(),
                                    },
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
                                //Ok(format!("`{}` successfully created.", &path))
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
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!("{}: could not delete mailbox", &self.name)),
                                    err.to_string(),
                                    Some(crate::types::NotificationType::Error(err.kind)),
                                )))
                                .expect("Could not send event on main channel");
                        }
                        Ok(Some(Ok(mut mailboxes))) => {
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::MailboxDelete((
                                    self.hash,
                                    mailbox_hash,
                                ))))
                                .unwrap();
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
                            /* if deleted mailbox had parent, we need to update its children field */
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
                            // FIXME remove from settings as well

                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!("{}: mailbox deleted successfully", &self.name)),
                                    String::new(),
                                    Some(crate::types::NotificationType::Info),
                                )))
                                .expect("Could not send event on main channel");
                        }
                    }
                }
                //JobRequest::RenameMailbox,
                JobRequest::SetMailboxPermissions { ref mut handle, .. } => {
                    match handle.chan.try_recv() {
                        Err(_) => { /* canceled */ }
                        Ok(None) => {}
                        Ok(Some(Err(err))) => {
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!(
                                        "{}: could not set mailbox permissions",
                                        &self.name
                                    )),
                                    err.to_string(),
                                    Some(crate::types::NotificationType::Error(err.kind)),
                                )))
                                .expect("Could not send event on main channel");
                        }
                        Ok(Some(Ok(_))) => {
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!(
                                        "{}: mailbox permissions set successfully",
                                        &self.name
                                    )),
                                    String::new(),
                                    Some(crate::types::NotificationType::Info),
                                )))
                                .expect("Could not send event on main channel");
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
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!(
                                        "{}: could not set mailbox subscription",
                                        &self.name
                                    )),
                                    err.to_string(),
                                    Some(crate::types::NotificationType::Error(err.kind)),
                                )))
                                .expect("Could not send event on main channel");
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
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!(
                                        "{}: `{}` has been {}subscribed.",
                                        &self.name,
                                        self.mailbox_entries[mailbox_hash].name(),
                                        if *new_value { "" } else { "un" }
                                    )),
                                    String::new(),
                                    Some(crate::types::NotificationType::Info),
                                )))
                                .expect("Could not send event on main channel");
                        }
                        Ok(Some(Ok(()))) => {}
                    }
                }
                JobRequest::Watch { ref mut handle } => {
                    debug!("JobRequest::Watch finished??? ");
                    if let Ok(Some(Err(err))) = handle.chan.try_recv() {
                        if err.kind.is_timeout() {
                            self.watch();
                        } else {
                            //TODO: relaunch watch job with ratelimit for failure
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!("{}: watch thread failed", &self.name)),
                                    err.to_string(),
                                    Some(crate::types::NotificationType::Error(err.kind)),
                                )))
                                .expect("Could not send event on main channel");
                        }
                    }
                }
                JobRequest::Generic {
                    ref name,
                    ref mut handle,
                    ref mut on_finish,
                    logging_level,
                } => {
                    match handle.chan.try_recv() {
                        Ok(Some(Err(err))) => {
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!("{}: {} failed", &self.name, name,)),
                                    err.to_string(),
                                    Some(crate::types::NotificationType::Error(err.kind)),
                                )))
                                .expect("Could not send event on main channel");
                        }
                        Ok(Some(Ok(()))) if on_finish.is_none() => {
                            if logging_level <= melib::LoggingLevel::INFO {
                                self.sender
                                    .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                        Some(format!("{}: {} succeeded", &self.name, name,)),
                                        String::new(),
                                        Some(crate::types::NotificationType::Info),
                                    )))
                                    .expect("Could not send event on main channel");
                            }
                        }
                        Err(_) => { /* canceled */ }
                        Ok(Some(Ok(()))) | Ok(None) => {}
                    }
                    if on_finish.is_some() {
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::Callback(
                                on_finish.take().unwrap(),
                            )))
                            .unwrap();
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
        self.sender
            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                StatusEvent::NewJob(job_id),
            )))
            .unwrap();
    }

    pub fn cancel_job(&mut self, job_id: JobId) -> Option<JobRequest> {
        if let Some(req) = self.active_jobs.remove(&job_id) {
            self.sender
                .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                    StatusEvent::JobCanceled(job_id),
                )))
                .unwrap();
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

fn build_mailboxes_order(
    tree: &mut Vec<MailboxNode>,
    mailbox_entries: &IndexMap<MailboxHash, MailboxEntry>,
    mailboxes_order: &mut Vec<MailboxHash>,
) {
    tree.clear();
    mailboxes_order.clear();
    for (h, f) in mailbox_entries.iter() {
        if f.ref_mailbox.parent().is_none() {
            fn rec(
                h: MailboxHash,
                mailbox_entries: &IndexMap<MailboxHash, MailboxEntry>,
                depth: usize,
            ) -> MailboxNode {
                let mut node = MailboxNode {
                    hash: h,
                    children: Vec::new(),
                    depth,
                    indentation: 0,
                    has_sibling: false,
                };
                for &c in mailbox_entries[&h].ref_mailbox.children() {
                    if mailbox_entries.contains_key(&c) {
                        node.children.push(rec(c, mailbox_entries, depth + 1));
                    }
                }
                node
            }

            tree.push(rec(*h, mailbox_entries, 0));
        }
    }

    macro_rules! mailbox_eq_key {
        ($mailbox:expr) => {{
            if let Some(sort_order) = $mailbox.conf.mailbox_conf.sort_order {
                (0, sort_order, $mailbox.ref_mailbox.path())
            } else {
                (1, 0, $mailbox.ref_mailbox.path())
            }
        }};
    }
    tree.sort_unstable_by(|a, b| {
        if mailbox_entries[&b.hash]
            .conf
            .mailbox_conf
            .sort_order
            .is_none()
            && mailbox_entries[&b.hash]
                .ref_mailbox
                .path()
                .eq_ignore_ascii_case("INBOX")
        {
            std::cmp::Ordering::Greater
        } else if mailbox_entries[&a.hash]
            .conf
            .mailbox_conf
            .sort_order
            .is_none()
            && mailbox_entries[&a.hash]
                .ref_mailbox
                .path()
                .eq_ignore_ascii_case("INBOX")
        {
            std::cmp::Ordering::Less
        } else {
            mailbox_eq_key!(mailbox_entries[&a.hash])
                .cmp(&mailbox_eq_key!(mailbox_entries[&b.hash]))
        }
    });

    let mut stack: SmallVec<[Option<&MailboxNode>; 16]> = SmallVec::new();
    for n in tree.iter_mut() {
        mailboxes_order.push(n.hash);
        n.children.sort_unstable_by(|a, b| {
            if mailbox_entries[&b.hash]
                .conf
                .mailbox_conf
                .sort_order
                .is_none()
                && mailbox_entries[&b.hash]
                    .ref_mailbox
                    .path()
                    .eq_ignore_ascii_case("INBOX")
            {
                std::cmp::Ordering::Greater
            } else if mailbox_entries[&a.hash]
                .conf
                .mailbox_conf
                .sort_order
                .is_none()
                && mailbox_entries[&a.hash]
                    .ref_mailbox
                    .path()
                    .eq_ignore_ascii_case("INBOX")
            {
                std::cmp::Ordering::Less
            } else {
                mailbox_eq_key!(mailbox_entries[&a.hash])
                    .cmp(&mailbox_eq_key!(mailbox_entries[&b.hash]))
            }
        });
        stack.extend(n.children.iter().rev().map(Some));
        while let Some(Some(next)) = stack.pop() {
            mailboxes_order.push(next.hash);
            stack.extend(next.children.iter().rev().map(Some));
        }
    }
    drop(stack);
    for node in tree.iter_mut() {
        fn rec(
            node: &mut MailboxNode,
            mailbox_entries: &IndexMap<MailboxHash, MailboxEntry>,
            mut indentation: u32,
            has_sibling: bool,
        ) {
            node.indentation = indentation;
            node.has_sibling = has_sibling;
            let mut iter = (0..node.children.len())
                .filter(|i| {
                    mailbox_entries[&node.children[*i].hash]
                        .ref_mailbox
                        .is_subscribed()
                })
                .collect::<SmallVec<[_; 8]>>()
                .into_iter()
                .peekable();
            if has_sibling {
                indentation <<= 1;
                indentation |= 1;
            } else {
                indentation <<= 1;
            }
            while let Some(i) = iter.next() {
                let c = &mut node.children[i];
                rec(c, mailbox_entries, indentation, iter.peek() != None);
            }
        }

        rec(node, mailbox_entries, 0, false);
    }
}
