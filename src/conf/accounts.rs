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
use crate::jobs::{JobChannel, JobExecutor, JobId, JoinHandle};
use indexmap::IndexMap;
use melib::backends::*;
use melib::email::*;
use melib::error::{MeliError, Result};
use melib::text_processing::GlobMatch;
use melib::thread::{SortField, SortOrder, Threads};
use melib::AddressBook;
use melib::Collection;
use smallvec::SmallVec;
use std::collections::BTreeMap;
use std::collections::{HashMap, HashSet};

use crate::types::UIEvent::{self, EnvelopeRemove, EnvelopeRename, EnvelopeUpdate, Notification};
use crate::{StatusEvent, ThreadEvent};
use crossbeam::Sender;
use futures::channel::oneshot;
use futures::future::FutureExt;
pub use futures::stream::Stream;
use futures::stream::StreamExt;
use std::borrow::Cow;
use std::collections::VecDeque;
use std::convert::TryFrom;
use std::fs;
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

#[derive(Debug)]
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
        if let MailboxStatus::Available = self {
            true
        } else {
            false
        }
    }
    pub fn is_parsing(&self) -> bool {
        if let MailboxStatus::Parsing(_, _) = self {
            true
        } else {
            false
        }
    }
}

#[derive(Debug)]
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
            &self.ref_mailbox.name()
        }
    }
}

#[derive(Debug)]
pub struct Account {
    name: String,
    hash: AccountHash,
    pub is_online: Result<()>,
    pub(crate) mailbox_entries: IndexMap<MailboxHash, MailboxEntry>,
    pub(crate) mailboxes_order: Vec<MailboxHash>,
    tree: Vec<MailboxNode>,
    sent_mailbox: Option<MailboxHash>,
    pub(crate) collection: Collection,
    pub(crate) address_book: AddressBook,
    pub(crate) settings: AccountConf,
    pub(crate) backend: Arc<RwLock<Box<dyn MailBackend>>>,

    pub job_executor: Arc<JobExecutor>,
    pub active_jobs: HashMap<JobId, JobRequest>,
    pub active_job_instants: BTreeMap<std::time::Instant, JobId>,
    sender: Sender<ThreadEvent>,
    event_queue: VecDeque<(MailboxHash, RefreshEvent)>,
    pub backend_capabilities: MailBackendCapabilities,
}

pub enum JobRequest {
    Mailboxes(
        JoinHandle,
        oneshot::Receiver<Result<HashMap<MailboxHash, Mailbox>>>,
    ),
    Fetch(
        MailboxHash,
        JoinHandle,
        oneshot::Receiver<(
            Option<Result<Vec<Envelope>>>,
            Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>,
        )>,
    ),
    Generic {
        name: Cow<'static, str>,
        logging_level: melib::LoggingLevel,
        handle: JoinHandle,
        channel: JobChannel<()>,
        on_finish: Option<crate::types::CallbackFn>,
    },
    IsOnline(JoinHandle, oneshot::Receiver<Result<()>>),
    Refresh(MailboxHash, JoinHandle, oneshot::Receiver<Result<()>>),
    SetFlags(EnvelopeHashBatch, JoinHandle, oneshot::Receiver<Result<()>>),
    SaveMessage {
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        handle: JoinHandle,
        channel: oneshot::Receiver<Result<()>>,
    },
    SendMessage,
    SendMessageBackground(JoinHandle, JobChannel<()>),
    CopyTo(MailboxHash, JoinHandle, oneshot::Receiver<Result<Vec<u8>>>),
    DeleteMessages(EnvelopeHashBatch, JoinHandle, oneshot::Receiver<Result<()>>),
    CreateMailbox {
        path: String,
        handle: JoinHandle,
        channel: JobChannel<(MailboxHash, HashMap<MailboxHash, Mailbox>)>,
    },
    DeleteMailbox {
        mailbox_hash: MailboxHash,
        handle: JoinHandle,
        channel: JobChannel<HashMap<MailboxHash, Mailbox>>,
    },
    //RenameMailbox,
    Search(JoinHandle),
    AsBytes(JoinHandle),
    SetMailboxPermissions(MailboxHash, JoinHandle, oneshot::Receiver<Result<()>>),
    SetMailboxSubscription(MailboxHash, JoinHandle, oneshot::Receiver<Result<()>>),
    Watch {
        channel: oneshot::Receiver<Result<()>>,
        handle: JoinHandle,
    },
}

impl Drop for JobRequest {
    fn drop(&mut self) {
        match self {
            JobRequest::Generic { handle, .. } => handle.0.cancel(),
            JobRequest::Mailboxes(h, _) => h.0.cancel(),
            JobRequest::Fetch(_, h, _) => h.0.cancel(),
            JobRequest::IsOnline(h, _) => h.0.cancel(),
            JobRequest::Refresh(_, h, _) => h.0.cancel(),
            JobRequest::SetFlags(_, h, _) => h.0.cancel(),
            JobRequest::SaveMessage { handle, .. } => handle.0.cancel(),
            JobRequest::CopyTo(_, h, _) => h.0.cancel(),
            JobRequest::DeleteMessages(_, h, _) => h.0.cancel(),
            JobRequest::CreateMailbox { handle, .. } => handle.0.cancel(),
            JobRequest::DeleteMailbox { handle, .. } => handle.0.cancel(),
            //JobRequest::RenameMailbox,
            JobRequest::Search(h) => h.0.cancel(),
            JobRequest::AsBytes(h) => h.0.cancel(),
            JobRequest::SetMailboxPermissions(_, h, _) => {
                h.0.cancel();
            }
            JobRequest::SetMailboxSubscription(_, h, _) => {
                h.0.cancel();
            }
            JobRequest::Watch { handle, .. } => handle.0.cancel(),
            JobRequest::SendMessage => {}
            JobRequest::SendMessageBackground(h, _) => {
                h.0.cancel();
            }
        }
    }
}

impl core::fmt::Debug for JobRequest {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            JobRequest::Generic { name, .. } => write!(f, "JobRequest::Generic({})", name),
            JobRequest::Mailboxes(_, _) => write!(f, "JobRequest::Mailboxes"),
            JobRequest::Fetch(hash, _, _) => write!(f, "JobRequest::Fetch({})", hash),
            JobRequest::IsOnline(_, _) => write!(f, "JobRequest::IsOnline"),
            JobRequest::Refresh(_, _, _) => write!(f, "JobRequest::Refresh"),
            JobRequest::SetFlags(_, _, _) => write!(f, "JobRequest::SetFlags"),
            JobRequest::SaveMessage { .. } => write!(f, "JobRequest::SaveMessage"),
            JobRequest::CopyTo(_, _, _) => write!(f, "JobRequest::CopyTo"),
            JobRequest::DeleteMessages(_, _, _) => write!(f, "JobRequest::DeleteMessages"),
            JobRequest::CreateMailbox { .. } => write!(f, "JobRequest::CreateMailbox"),
            JobRequest::DeleteMailbox { mailbox_hash, .. } => {
                write!(f, "JobRequest::DeleteMailbox({})", mailbox_hash)
            }
            //JobRequest::RenameMailbox,
            JobRequest::Search(_) => write!(f, "JobRequest::Search"),
            JobRequest::AsBytes(_) => write!(f, "JobRequest::AsBytes"),
            JobRequest::SetMailboxPermissions(_, _, _) => {
                write!(f, "JobRequest::SetMailboxPermissions")
            }
            JobRequest::SetMailboxSubscription(_, _, _) => {
                write!(f, "JobRequest::SetMailboxSubscription")
            }
            JobRequest::Watch { .. } => write!(f, "JobRequest::Watch"),
            JobRequest::SendMessage => write!(f, "JobRequest::SendMessage"),
            JobRequest::SendMessageBackground(_, _) => {
                write!(f, "JobRequest::SendMessageBackground")
            }
        }
    }
}

impl core::fmt::Display for JobRequest {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            JobRequest::Generic { name, .. } => write!(f, "{}", name),
            JobRequest::Mailboxes(_, _) => write!(f, "Get mailbox list"),
            JobRequest::Fetch(_, _, _) => write!(f, "Mailbox fetch"),
            JobRequest::IsOnline(_, _) => write!(f, "Online status check"),
            JobRequest::Refresh(_, _, _) => write!(f, "Refresh mailbox"),
            JobRequest::SetFlags(batch, _, _) => write!(
                f,
                "Set flags for {} message{}",
                batch.len(),
                if batch.len() == 1 { "" } else { "s" }
            ),
            JobRequest::SaveMessage { .. } => write!(f, "Save message"),
            JobRequest::CopyTo(_, _, _) => write!(f, "Copy message."),
            JobRequest::DeleteMessages(batch, _, _) => write!(
                f,
                "Delete {} message{}",
                batch.len(),
                if batch.len() == 1 { "" } else { "s" }
            ),
            JobRequest::CreateMailbox { path, .. } => write!(f, "Create mailbox {}", path),
            JobRequest::DeleteMailbox { .. } => write!(f, "Delete mailbox"),
            //JobRequest::RenameMailbox,
            JobRequest::Search(_) => write!(f, "Search"),
            JobRequest::AsBytes(_) => write!(f, "Message body fetch"),
            JobRequest::SetMailboxPermissions(_, _, _) => write!(f, "Set mailbox permissions"),
            JobRequest::SetMailboxSubscription(_, _, _) => write!(f, "Set mailbox subscription"),
            JobRequest::Watch { .. } => write!(f, "Background watch"),
            JobRequest::SendMessageBackground(_, _) | JobRequest::SendMessage => {
                write!(f, "Sending message")
            }
        }
    }
}

impl JobRequest {
    pub fn is_watch(&self) -> bool {
        match self {
            JobRequest::Watch { .. } => true,
            _ => false,
        }
    }

    pub fn is_fetch(&self, mailbox_hash: MailboxHash) -> bool {
        match self {
            JobRequest::Fetch(h, _, _) if *h == mailbox_hash => true,
            _ => false,
        }
    }

    pub fn is_online(&self) -> bool {
        match self {
            JobRequest::IsOnline(_, _) => true,
            _ => false,
        }
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
                    return;
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
                if let Err(err) = bincode::serialize_into(writer, &self.collection) {
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
        let mut address_book = AddressBook::with_account(&settings.account());

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

        if ["imap", "jmap", "notmuch"].contains(&settings.account().format()) {
            settings.conf.search_backend = crate::conf::SearchBackend::None;
        }

        let mut active_jobs = HashMap::default();
        let mut active_job_instants = BTreeMap::default();
        if let Ok(mailboxes_job) = backend.mailboxes() {
            if let Ok(online_job) = backend.is_online() {
                let (rcvr, handle, job_id) = if backend.capabilities().is_async {
                    job_executor.spawn_specialized(online_job.then(|_| mailboxes_job))
                } else {
                    job_executor.spawn_blocking(online_job.then(|_| mailboxes_job))
                };
                active_jobs.insert(job_id, JobRequest::Mailboxes(handle, rcvr));
                active_job_instants.insert(std::time::Instant::now(), job_id);
                sender
                    .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                        StatusEvent::NewJob(job_id),
                    )))
                    .unwrap();
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
            collection: Default::default(),
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
                    if tmp != Some(SpecialUsageMailbox::Normal) && tmp != None {
                        let _ = f.set_special_usage(tmp.unwrap());
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
                    if tmp != Some(SpecialUsageMailbox::Normal) && tmp != None {
                        let _ = f.set_special_usage(tmp.unwrap());
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
            mailbox_comma_sep_list_string.drain(mailbox_comma_sep_list_string.len() - 2..);
            melib::log(
                format!(
                    "Account `{}` has the following mailboxes: [{}]",
                    &self.name, mailbox_comma_sep_list_string,
                ),
                melib::WARN,
            );
        }

        let mut tree: Vec<MailboxNode> = Vec::new();
        for (h, f) in ref_mailboxes.iter() {
            if !f.is_subscribed() {
                /* Skip unsubscribed mailbox */
                continue;
            }
            mailbox_entries.entry(*h).and_modify(|entry| {
                if entry.conf.mailbox_conf.autoload
                    || entry.ref_mailbox.special_usage() == SpecialUsageMailbox::Inbox
                {
                    let total = entry.ref_mailbox.count().ok().unwrap_or((0, 0)).1;
                    entry.status = MailboxStatus::Parsing(0, total);
                    if let Ok(mailbox_job) = self.backend.write().unwrap().fetch(*h) {
                        let mailbox_job = mailbox_job.into_future();
                        let (rcvr, handle, job_id) = if self.backend_capabilities.is_async {
                            self.job_executor.spawn_specialized(mailbox_job)
                        } else {
                            self.job_executor.spawn_blocking(mailbox_job)
                        };
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                                StatusEvent::NewJob(job_id),
                            )))
                            .unwrap();
                        self.active_jobs
                            .insert(job_id, JobRequest::Fetch(*h, handle, rcvr));
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
                    {
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
                                        err.to_string()
                                    ),
                                    melib::ERROR,
                                );
                            }
                            Ok(job) => {
                                let (channel, handle, job_id) =
                                    self.job_executor.spawn_blocking(job);
                                self.insert_job(
                                    job_id,
                                    JobRequest::Generic {
                                        name: format!(
                                            "Update envelope {} in sqlite3 cache",
                                            envelope.message_id_display()
                                        )
                                        .into(),
                                        handle,
                                        channel,
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
                    {
                        match crate::sqlite3::remove(env_hash).map(|_| {
                            crate::sqlite3::insert(
                                self.collection.envelopes.read().unwrap()[&env_hash].clone(),
                                self.backend.clone(),
                                self.name.clone(),
                            )
                        }) {
                            Ok(job) => {
                                let (channel, handle, job_id) =
                                    self.job_executor.spawn_blocking(job);
                                self.insert_job(
                                    job_id,
                                    JobRequest::Generic {
                                        name: format!(
                                            "Update envelope {} in sqlite3 cache",
                                            self.collection.envelopes.read().unwrap()[&env_hash]
                                                .message_id_display()
                                        )
                                        .into(),
                                        handle,
                                        channel,
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
                                        err.to_string()
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
                    {
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
                                        err.to_string()
                                    ),
                                    melib::ERROR,
                                );
                            }
                            Ok(job) => {
                                let (channel, handle, job_id) =
                                    self.job_executor.spawn_blocking(job);
                                self.insert_job(
                                    job_id,
                                    JobRequest::Generic {
                                        name: format!(
                                            "Update envelope {} in sqlite3 cache",
                                            self.collection.envelopes.read().unwrap()[&new_hash]
                                                .message_id_display()
                                        )
                                        .into(),
                                        handle,
                                        channel,
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
                    {
                        let (channel, handle, job_id) =
                            self.job_executor.spawn_blocking(crate::sqlite3::insert(
                                (*envelope).clone(),
                                self.backend.clone(),
                                self.name.clone(),
                            ));
                        self.insert_job(
                            job_id,
                            JobRequest::Generic {
                                name: format!(
                                    "Update envelope {} in sqlite3 cache",
                                    envelope.message_id_display()
                                )
                                .into(),
                                handle,
                                channel,
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

                    let thread = {
                        let thread_hash = self.collection.get_env(env_hash).thread();
                        self.collection.get_threads(mailbox_hash).find_group(
                            self.collection.get_threads(mailbox_hash)[&thread_hash].group,
                        )
                    };
                    if self
                        .collection
                        .get_threads(mailbox_hash)
                        .thread_ref(thread)
                        .snoozed()
                    {
                        return Some(UIEvent::MailboxUpdate((self.hash, mailbox_hash)));
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
                    let thread_hash = {
                        let thread_hash = self.collection.get_env(env_hash).thread();
                        self.collection.get_threads(mailbox_hash).find_group(
                            self.collection.get_threads(mailbox_hash)[&thread_hash].group,
                        )
                    };
                    #[cfg(feature = "sqlite3")]
                    {
                        let envelopes = self.collection.envelopes.read();
                        let envelopes = envelopes.unwrap();
                        if let Err(err) = crate::sqlite3::remove(env_hash) {
                            melib::log(
                                format!(
                                    "Failed to remove envelope {} [{}] in cache: {}",
                                    &envelopes[&env_hash].message_id_display(),
                                    env_hash,
                                    err.to_string()
                                ),
                                melib::ERROR,
                            );
                        }
                    }
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
            let (rcvr, handle, job_id) = if self.backend_capabilities.is_async {
                self.job_executor.spawn_specialized(refresh_job)
            } else {
                self.job_executor.spawn_blocking(refresh_job)
            };
            self.insert_job(job_id, JobRequest::Refresh(mailbox_hash, handle, rcvr));
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
                    let (channel, handle, job_id) = if self.backend_capabilities.is_async {
                        self.job_executor.spawn_specialized(fut)
                    } else {
                        self.job_executor.spawn_blocking(fut)
                    };
                    self.active_jobs
                        .insert(job_id, JobRequest::Watch { channel, handle });
                }
                Err(e) => {
                    self.sender
                        .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(e.to_string()),
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
                            let (rcvr, handle, job_id) = if self.backend_capabilities.is_async {
                                self.job_executor.spawn_specialized(mailbox_job)
                            } else {
                                self.job_executor.spawn_blocking(mailbox_job)
                            };
                            self.insert_job(job_id, JobRequest::Fetch(mailbox_hash, handle, rcvr));
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
            if mailbox.is_none() {
                continue;
            }
            let mailbox = mailbox.unwrap();
            if let Err(e) = self.save(bytes, mailbox, Some(flags)) {
                debug!("{:?} could not save msg", e);
                melib::log(
                    format!(
                        "Could not save in '{}' mailbox: {}.",
                        mailbox,
                        e.to_string()
                    ),
                    melib::ERROR,
                );
            } else {
                saved_at = Some(mailbox);
                break;
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
            return Err(MeliError::new(format!(
                "Message was stored in {} so that you can restore it manually.",
                file.path.display()
            ))
            .set_summary("Could not save in any mailbox"));
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
        let (channel, handle, job_id) = self.job_executor.spawn_specialized(job);
        self.insert_job(
            job_id,
            JobRequest::SaveMessage {
                bytes: bytes.to_vec(),
                mailbox_hash,
                handle,
                channel,
            },
        );
        Ok(())
    }

    pub fn send(
        &mut self,
        message: String,
        send_mail: crate::conf::composing::SendMail,
        complete_in_background: bool,
    ) -> Result<Option<(JobId, JoinHandle, JobChannel<()>)>> {
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
                    return Err(
                        MeliError::new(error_message.clone()).set_summary("Message not sent.")
                    );
                }
                Ok(None)
            }
            #[cfg(feature = "smtp")]
            SendMail::Smtp(conf) => {
                let (chan, handle, job_id) = self.job_executor.spawn_specialized(async move {
                    let mut smtp_connection =
                        melib::smtp::SmtpConnection::new_connection(conf).await?;
                    smtp_connection.mail_transaction(&message, None).await
                });
                if complete_in_background {
                    self.insert_job(job_id, JobRequest::SendMessageBackground(handle, chan));
                    return Ok(None);
                } else {
                    self.insert_job(job_id, JobRequest::SendMessage);
                }
                Ok(Some((job_id, handle, chan)))
            }
        }
    }

    pub fn delete(
        &mut self,
        env_hash: EnvelopeHash,
        mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        if self.settings.account.read_only() {
            return Err(MeliError::new(format!(
                "Account {} is read-only.",
                self.name.as_str()
            )));
        }
        self.backend.write().unwrap().delete(env_hash, mailbox_hash)
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
                let (channel, handle, job_id) = if self.backend_capabilities.is_async {
                    self.job_executor.spawn_specialized(job)
                } else {
                    self.job_executor.spawn_blocking(job)
                };
                self.insert_job(
                    job_id,
                    JobRequest::CreateMailbox {
                        path,
                        handle,
                        channel,
                    },
                );
                Ok(())
            }
            MailboxOperation::Delete(path) => {
                if self.mailbox_entries.len() == 1 {
                    return Err(MeliError::new("Cannot delete only mailbox."));
                }

                let mailbox_hash = self.mailbox_by_path(&path)?;
                let job = self.backend.write().unwrap().delete_mailbox(mailbox_hash)?;
                let (channel, handle, job_id) = if self.backend_capabilities.is_async {
                    self.job_executor.spawn_specialized(job)
                } else {
                    self.job_executor.spawn_blocking(job)
                };
                self.insert_job(
                    job_id,
                    JobRequest::DeleteMailbox {
                        mailbox_hash,
                        handle,
                        channel,
                    },
                );
                Ok(())
            }
            MailboxOperation::Subscribe(path) => {
                let mailbox_hash = self.mailbox_by_path(&path)?;
                self.backend
                    .write()
                    .unwrap()
                    .set_mailbox_subscription(mailbox_hash, true)?;
                self.mailbox_entries.entry(mailbox_hash).and_modify(|m| {
                    m.conf.mailbox_conf.subscribe = super::ToggleFlag::True;
                    let _ = m.ref_mailbox.set_is_subscribed(true);
                });

                self.sender
                    .send(ThreadEvent::UIEvent(UIEvent::Notification(
                        None,
                        format!("'`{}` has been subscribed.", &path),
                        Some(crate::types::NotificationType::Info),
                    )))
                    .expect("Could not send event on main channel");
                Ok(())
            }
            MailboxOperation::Unsubscribe(path) => {
                let mailbox_hash = self.mailbox_by_path(&path)?;
                self.backend
                    .write()
                    .unwrap()
                    .set_mailbox_subscription(mailbox_hash, false)?;
                self.mailbox_entries.entry(mailbox_hash).and_modify(|m| {
                    m.conf.mailbox_conf.subscribe = super::ToggleFlag::False;
                    let _ = m.ref_mailbox.set_is_subscribed(false);
                });

                self.sender
                    .send(ThreadEvent::UIEvent(UIEvent::Notification(
                        None,
                        format!("'`{}` has been unsubscribed.", &path),
                        Some(crate::types::NotificationType::Info),
                    )))
                    .expect("Could not send event on main channel");
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
        if let Some(ret) = ret.as_ref() {
            Some(ret.1.ref_mailbox.hash())
        } else {
            None
        }
    }

    /* Call only in Context::is_online, since only Context can launch the watcher threads if an
     * account goes from offline to online. */
    pub fn is_online(&mut self) -> Result<()> {
        if !self.backend_capabilities.is_remote && !self.backend_capabilities.is_async {
            return Ok(());
        }

        let mut timeout = false;
        let mut drain: SmallVec<[std::time::Instant; 16]> = SmallVec::new();
        const ONLINE_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(30);
        for (_instant, _) in self
            .active_job_instants
            .range(..std::time::Instant::now() - ONLINE_TIMEOUT)
        {
            drain.push(*_instant);
        }
        for inst in drain {
            if let Some(j) = self.active_job_instants.remove(&inst) {
                if let Some(req) = self.cancel_job(j) {
                    debug!("timeout for {} {:?}", j, &req);
                    timeout |= !req.is_watch();
                }
            }
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
        if self.is_online.is_ok() && !timeout {
            return Ok(());
        }
        if !self.active_jobs.values().any(JobRequest::is_online) {
            let online_job = self.backend.read().unwrap().is_online();
            if let Ok(online_job) = online_job {
                let (rcvr, handle, job_id) = if self.backend_capabilities.is_async {
                    self.job_executor.spawn_specialized(online_job)
                } else {
                    self.job_executor.spawn_blocking(online_job)
                };
                self.insert_job(job_id, JobRequest::IsOnline(handle, rcvr));
            }
        }
        return self.is_online.clone();
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
            crate::conf::SearchBackend::None => {
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

        if self.active_jobs.contains_key(job_id) {
            match self.active_jobs.remove(job_id).unwrap() {
                JobRequest::Mailboxes(_, ref mut chan) => {
                    if let Some(mailboxes) = chan.try_recv().unwrap() {
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::AccountStatusChange(
                                self.hash,
                            )))
                            .unwrap();
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
                                let (rcvr, handle, job_id) =
                                    self.job_executor.spawn_specialized(mailboxes_job);
                                self.insert_job(job_id, JobRequest::Mailboxes(handle, rcvr));
                            };
                        }
                    }
                }
                JobRequest::Fetch(mailbox_hash, _, ref mut chan) => {
                    let (payload, rest): (Option<Result<Vec<Envelope>>>, _) =
                        chan.try_recv().unwrap().unwrap();
                    debug!("got payload in status for {}", mailbox_hash);
                    if payload.is_none() {
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
                    let (rcvr, handle, job_id) =
                        self.job_executor.spawn_specialized(rest.into_future());
                    self.insert_job(job_id, JobRequest::Fetch(mailbox_hash, handle, rcvr));
                    let payload = payload.unwrap();
                    if let Err(err) = payload {
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
                    let envelopes = payload
                        .unwrap()
                        .into_iter()
                        .map(|e| (e.hash(), e))
                        .collect::<HashMap<EnvelopeHash, Envelope>>();
                    if let Some(updated_mailboxes) =
                        self.collection
                            .merge(envelopes, mailbox_hash, self.sent_mailbox)
                    {
                        for f in updated_mailboxes {
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::MailboxUpdate((self.hash, f))))
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
                JobRequest::IsOnline(_, ref mut chan) => {
                    let is_online = chan.try_recv().unwrap();
                    if let Some(is_online) = is_online {
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::AccountStatusChange(
                                self.hash,
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
                        let (rcvr, handle, job_id) =
                            self.job_executor.spawn_specialized(online_job);
                        self.insert_job(job_id, JobRequest::IsOnline(handle, rcvr));
                    };
                }
                JobRequest::Refresh(_mailbox_hash, _, ref mut chan) => {
                    let r = chan.try_recv().unwrap();
                    match r {
                        Some(Ok(())) => {
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
                                        self.hash,
                                    )))
                                    .unwrap();
                            }
                        }
                        Some(Err(err)) => {
                            if !err.kind.is_authentication() {
                                let online_job = self.backend.read().unwrap().is_online();
                                if let Ok(online_job) = online_job {
                                    let (rcvr, handle, job_id) =
                                        self.job_executor.spawn_specialized(online_job);
                                    self.insert_job(job_id, JobRequest::IsOnline(handle, rcvr));
                                };
                            }
                            self.is_online = Err(err);
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::AccountStatusChange(
                                    self.hash,
                                )))
                                .unwrap();
                        }
                        None => {}
                    }
                }
                JobRequest::SetFlags(_, _, ref mut chan) => {
                    let r = chan.try_recv().unwrap();
                    if let Some(Err(err)) = r {
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
                    ref mut channel,
                    ref bytes,
                    ..
                } => {
                    let r = channel.try_recv().unwrap();
                    if let Some(Err(err)) = r {
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
                JobRequest::SendMessageBackground(_, ref mut chan) => {
                    let r = chan.try_recv().unwrap();
                    if let Some(Err(err)) = r {
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                Some("Could not send message".to_string()),
                                err.to_string(),
                                Some(crate::types::NotificationType::Error(err.kind)),
                            )))
                            .expect("Could not send event on main channel");
                    }
                }
                JobRequest::CopyTo(mailbox_hash, _, ref mut chan) => {
                    if let Err(err) = chan
                        .try_recv()
                        .unwrap()
                        .unwrap()
                        .and_then(|bytes| self.save(&bytes, mailbox_hash, None))
                    {
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                Some(format!("{}: could not save message", &self.name)),
                                err.to_string(),
                                Some(crate::types::NotificationType::Error(err.kind)),
                            )))
                            .expect("Could not send event on main channel");
                    }
                }
                JobRequest::DeleteMessages(_, _, ref mut chan) => {
                    let r = chan.try_recv().unwrap();
                    if let Some(Err(err)) = r {
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
                    ref mut channel,
                    ..
                } => {
                    let r = channel.try_recv().unwrap();
                    if let Some(r) = r {
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
                                    if tmp != Some(SpecialUsageMailbox::Normal) && tmp != None {
                                        mailboxes.entry(mailbox_hash).and_modify(|entry| {
                                            let _ = entry.set_special_usage(tmp.unwrap());
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
                    ref mut channel,
                    ..
                } => {
                    let r = channel.try_recv().unwrap();
                    match r {
                        Some(Err(err)) => {
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!("{}: could not delete mailbox", &self.name)),
                                    err.to_string(),
                                    Some(crate::types::NotificationType::Error(err.kind)),
                                )))
                                .expect("Could not send event on main channel");
                        }
                        Some(Ok(mut mailboxes)) => {
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
                        None => {}
                    }
                }
                //JobRequest::RenameMailbox,
                JobRequest::Search(_) | JobRequest::AsBytes(_) => {}
                JobRequest::SetMailboxPermissions(_, _, ref mut chan) => {
                    let r = chan.try_recv().unwrap();
                    match r {
                        Some(Err(err)) => {
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
                        Some(Ok(_)) => {
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
                        None => {}
                    }
                }
                JobRequest::SetMailboxSubscription(_, _, ref mut chan) => {
                    let r = chan.try_recv().unwrap();
                    match r {
                        Some(Err(err)) => {
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
                        Some(Ok(_)) => {
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!(
                                        "{}: mailbox subscription set successfully",
                                        &self.name
                                    )),
                                    String::new(),
                                    Some(crate::types::NotificationType::Info),
                                )))
                                .expect("Could not send event on main channel");
                        }
                        None => {}
                    }
                }
                JobRequest::Watch {
                    ref mut channel,
                    handle: _,
                } => {
                    debug!("JobRequest::Watch finished??? ");
                    let r = channel.try_recv().unwrap();
                    debug!("JobRequest::Watch {:?}", r);
                    if let Some(Err(err)) = r {
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
                    ref mut channel,
                    handle: _,
                    ref mut on_finish,
                    logging_level,
                } => {
                    let r = channel.try_recv().unwrap();
                    match r {
                        Some(Err(err)) => {
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::Notification(
                                    Some(format!("{}: {} failed", &self.name, name,)),
                                    err.to_string(),
                                    Some(crate::types::NotificationType::Error(err.kind)),
                                )))
                                .expect("Could not send event on main channel");
                        }
                        Some(Ok(())) if on_finish.is_none() => {
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
                        Some(Ok(())) | None => {}
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
                };
                for &c in mailbox_entries[&h].ref_mailbox.children() {
                    if mailbox_entries.contains_key(&c) {
                        node.children.push(rec(c, mailbox_entries, depth + 1));
                    }
                }
                node
            };

            tree.push(rec(*h, &mailbox_entries, 0));
        }
    }

    tree.sort_unstable_by(|a, b| {
        if mailbox_entries[&b.hash]
            .ref_mailbox
            .path()
            .eq_ignore_ascii_case("INBOX")
        {
            std::cmp::Ordering::Greater
        } else if mailbox_entries[&a.hash]
            .ref_mailbox
            .path()
            .eq_ignore_ascii_case("INBOX")
        {
            std::cmp::Ordering::Less
        } else {
            mailbox_entries[&a.hash]
                .ref_mailbox
                .path()
                .cmp(&mailbox_entries[&b.hash].ref_mailbox.path())
        }
    });

    let mut stack: SmallVec<[Option<&MailboxNode>; 16]> = SmallVec::new();
    for n in tree.iter_mut() {
        mailboxes_order.push(n.hash);
        n.children.sort_unstable_by(|a, b| {
            if mailbox_entries[&b.hash]
                .ref_mailbox
                .path()
                .eq_ignore_ascii_case("INBOX")
            {
                std::cmp::Ordering::Greater
            } else if mailbox_entries[&a.hash]
                .ref_mailbox
                .path()
                .eq_ignore_ascii_case("INBOX")
            {
                std::cmp::Ordering::Less
            } else {
                mailbox_entries[&a.hash]
                    .ref_mailbox
                    .path()
                    .cmp(&mailbox_entries[&b.hash].ref_mailbox.path())
            }
        });
        stack.extend(n.children.iter().rev().map(Some));
        while let Some(Some(next)) = stack.pop() {
            mailboxes_order.push(next.hash);
            stack.extend(next.children.iter().rev().map(Some));
        }
    }
}
