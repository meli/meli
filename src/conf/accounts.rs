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
use melib::async_workers::{Async, AsyncBuilder, AsyncStatus, WorkContext};
use melib::backends::{
    BackendOp, Backends, MailBackend, Mailbox, MailboxHash, NotifyFn, ReadOnlyOp, RefreshEvent,
    RefreshEventConsumer, RefreshEventKind, SpecialUsageMailbox,
};
use melib::email::*;
use melib::error::{MeliError, Result};
use melib::text_processing::GlobMatch;
use melib::thread::{SortField, SortOrder, ThreadNode, ThreadNodeHash, Threads};
use melib::AddressBook;
use melib::Collection;
use smallvec::SmallVec;
use std::collections::{HashMap, HashSet};

use crate::types::UIEvent::{self, EnvelopeRemove, EnvelopeRename, EnvelopeUpdate, Notification};
use crate::{StatusEvent, ThreadEvent};
use crossbeam::Sender;
use std::collections::VecDeque;
use std::fs;
use std::io;
use std::ops::{Index, IndexMut};
use std::os::unix::fs::PermissionsExt;
use std::result;
use std::sync::{Arc, RwLock};

pub type Worker = Option<Async<Result<Vec<Envelope>>>>;

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
    pub worker: Worker,
}

impl MailboxEntry {
    pub fn status(&self) -> String {
        match self.status {
            MailboxStatus::Available => self.name().to_string(),
            MailboxStatus::Failed(ref e) => e.to_string(),
            MailboxStatus::None => "Retrieving mailbox".to_string(),
            MailboxStatus::Parsing(done, total) => {
                format!("Parsing messages. [{}/{}]", done, total)
            }
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct Account {
    pub index: usize,
    name: String,
    pub is_online: bool,
    pub(crate) mailbox_entries: HashMap<MailboxHash, MailboxEntry>,
    pub(crate) mailboxes_order: Vec<MailboxHash>,
    tree: Vec<MailboxNode>,
    sent_mailbox: Option<MailboxHash>,
    pub(crate) collection: Collection,
    pub(crate) address_book: AddressBook,
    pub(crate) work_context: WorkContext,
    pub(crate) settings: AccountConf,
    pub(crate) backend: Arc<RwLock<Box<dyn MailBackend>>>,

    sender: Sender<ThreadEvent>,
    event_queue: VecDeque<(MailboxHash, RefreshEvent)>,
    notify_fn: Arc<NotifyFn>,
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
        index: usize,
        name: String,
        mut settings: AccountConf,
        map: &Backends,
        work_context: WorkContext,
        sender: Sender<ThreadEvent>,
        notify_fn: NotifyFn,
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
        )?;
        let notify_fn = Arc::new(notify_fn);

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

        if settings.account().format() == "imap" {
            settings.conf.cache_type = crate::conf::CacheType::None;
        }

        Ok(Account {
            index,
            name,
            is_online: false,
            mailbox_entries: Default::default(),
            mailboxes_order: Default::default(),
            tree: Default::default(),
            address_book,
            sent_mailbox: Default::default(),
            collection: Default::default(),
            work_context,
            settings,
            backend: Arc::new(RwLock::new(backend)),
            notify_fn,
            sender,

            event_queue: VecDeque::with_capacity(8),
        })
    }

    fn init(&mut self) {
        let mut ref_mailboxes: HashMap<MailboxHash, Mailbox> =
            match self.backend.read().unwrap().mailboxes() {
                Ok(f) => f,
                Err(err) => {
                    debug!(&err);
                    return;
                }
            };
        let mut mailbox_entries: HashMap<MailboxHash, MailboxEntry> =
            HashMap::with_capacity_and_hasher(ref_mailboxes.len(), Default::default());
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
                        worker: None,
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
                        worker: None,
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
                if entry.conf.mailbox_conf.autoload {
                    entry.status = MailboxStatus::Parsing(0, 0);
                    entry.worker = Account::new_worker(
                        f.clone(),
                        &mut self.backend,
                        &self.work_context,
                        self.notify_fn.clone(),
                    );
                }
            });
            self.collection.new_mailbox(*h);
        }

        build_mailboxes_order(&mut tree, &mailbox_entries, &mut mailboxes_order);
        self.mailboxes_order = mailboxes_order;
        self.mailbox_entries = mailbox_entries;
        self.tree = tree;
        self.sent_mailbox = sent_mailbox;
    }

    fn new_worker(
        mailbox: Mailbox,
        backend: &Arc<RwLock<Box<dyn MailBackend>>>,
        work_context: &WorkContext,
        notify_fn: Arc<NotifyFn>,
    ) -> Worker {
        let mut mailbox_handle = backend.write().unwrap().get(&mailbox);
        let mut builder = AsyncBuilder::new();
        let our_tx = builder.tx();
        let mailbox_hash = mailbox.hash();
        let priority = match mailbox.special_usage() {
            SpecialUsageMailbox::Inbox => 0,
            SpecialUsageMailbox::Sent => 1,
            SpecialUsageMailbox::Drafts | SpecialUsageMailbox::Trash => 2,
            _ => {
                3 * mailbox
                    .path()
                    .split(if mailbox.path().contains('/') {
                        '/'
                    } else {
                        '.'
                    })
                    .count() as u64
            }
        };

        /* This polling closure needs to be 'static', that is to spawn its own thread instead of
         * being assigned to a worker thread. Otherwise the polling closures could fill up the
         * workers causing no actual parsing to be done. If we could yield from within the worker
         * threads' closures this could be avoided, but it requires green threads.
         */
        builder.set_priority(priority).set_is_static(true);
        let mut w = builder.build(Box::new(move |work_context| {
            let name = format!("Parsing {}", mailbox.path());
            let work = mailbox_handle.work().unwrap();
            work_context.new_work.send(work).unwrap();
            let thread_id = std::thread::current().id();
            work_context.set_name.send((thread_id, name)).unwrap();
            work_context
                .set_status
                .send((thread_id, "Waiting for subworkers..".to_string()))
                .unwrap();

            loop {
                match debug!(mailbox_handle.poll_block()) {
                    Ok(s @ AsyncStatus::Payload(_)) => {
                        our_tx.send(s).unwrap();
                        debug!("notifying for {}", mailbox_hash);
                        notify_fn.notify(mailbox_hash);
                    }
                    Ok(s @ AsyncStatus::Finished) => {
                        our_tx.send(s).unwrap();
                        notify_fn.notify(mailbox_hash);
                        debug!("exiting");
                        work_context.finished.send(thread_id).unwrap();
                        return;
                    }
                    Ok(s) => {
                        our_tx.send(s).unwrap();
                    }
                    Err(_) => {
                        debug!("poll error");
                        return;
                    }
                }
            }
        }));
        if let Some(w) = w.work() {
            work_context.new_work.send(w).unwrap();
        }
        Some(w)
    }
    pub fn reload(&mut self, event: RefreshEvent, mailbox_hash: MailboxHash) -> Option<UIEvent> {
        if !self.mailbox_entries[&mailbox_hash].status.is_available() {
            self.event_queue.push_back((mailbox_hash, event));
            return None;
        }

        let kind = event.kind();
        {
            //let mailbox: &mut Mailbox = self.mailboxes[idx].as_mut().unwrap().as_mut().unwrap();
            match kind {
                RefreshEventKind::Update(old_hash, envelope) => {
                    #[cfg(feature = "sqlite3")]
                    {
                        if let Err(err) = crate::sqlite3::remove(old_hash).and_then(|_| {
                            crate::sqlite3::insert(&envelope, &self.backend, &self.name)
                        }) {
                            melib::log(
                                format!(
                                    "Failed to update envelope {} in cache: {}",
                                    envelope.message_id_display(),
                                    err.to_string()
                                ),
                                melib::ERROR,
                            );
                        }
                    }
                    self.collection.update(old_hash, *envelope, mailbox_hash);
                    return Some(EnvelopeUpdate(old_hash));
                }
                RefreshEventKind::NewFlags(env_hash, (flags, tags)) => {
                    let mut envelopes = self.collection.envelopes.write().unwrap();
                    envelopes.entry(env_hash).and_modify(|entry| {
                        for f in tags {
                            let hash = tag_hash!(f);
                            if !entry.labels().contains(&hash) {
                                entry.labels_mut().push(hash);
                            }
                        }
                        entry.set_flags(flags);
                    });
                    #[cfg(feature = "sqlite3")]
                    {
                        if let Err(err) = crate::sqlite3::remove(env_hash).and_then(|_| {
                            crate::sqlite3::insert(&envelopes[&env_hash], &self.backend, &self.name)
                        }) {
                            melib::log(
                                format!(
                                    "Failed to update envelope {} in cache: {}",
                                    envelopes[&env_hash].message_id_display(),
                                    err.to_string()
                                ),
                                melib::ERROR,
                            );
                        }
                    }
                    return Some(EnvelopeUpdate(env_hash));
                }
                RefreshEventKind::Rename(old_hash, new_hash) => {
                    debug!("rename {} to {}", old_hash, new_hash);
                    self.collection.rename(old_hash, new_hash, mailbox_hash);
                    #[cfg(feature = "sqlite3")]
                    {
                        let envelopes = self.collection.envelopes.read();
                        let envelopes = envelopes.unwrap();
                        if let Err(err) = crate::sqlite3::remove(old_hash).and_then(|_| {
                            crate::sqlite3::insert(&envelopes[&new_hash], &self.backend, &self.name)
                        }) {
                            melib::log(
                                format!(
                                    "Failed to update envelope {} in cache: {}",
                                    &envelopes[&new_hash].message_id_display(),
                                    err.to_string()
                                ),
                                melib::ERROR,
                            );
                        }
                    }
                    return Some(EnvelopeRename(old_hash, new_hash));
                }
                RefreshEventKind::Create(envelope) => {
                    let env_hash = envelope.hash();
                    if self.collection.contains_key(&env_hash)
                        && self.collection[&mailbox_hash].contains(&env_hash)
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
                        if let Err(err) =
                            crate::sqlite3::insert(&envelope, &self.backend, &self.name)
                        {
                            melib::log(
                                format!(
                                    "Failed to insert envelope {} in cache: {}",
                                    envelope.message_id_display(),
                                    err.to_string()
                                ),
                                melib::ERROR,
                            );
                        }
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
                        return Some(UIEvent::MailboxUpdate((self.index, mailbox_hash)));
                    }

                    let thread = {
                        let thread_hash = self.collection.get_env(env_hash).thread();
                        self.collection.threads[&mailbox_hash]
                            .find_group(self.collection.threads[&mailbox_hash][&thread_hash].group)
                    };
                    if self.collection.threads[&mailbox_hash]
                        .thread_ref(thread)
                        .snoozed()
                    {
                        return Some(UIEvent::MailboxUpdate((self.index, mailbox_hash)));
                    }
                    if is_seen || is_draft {
                        return Some(UIEvent::MailboxUpdate((self.index, mailbox_hash)));
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
                RefreshEventKind::Remove(envelope_hash) => {
                    #[cfg(feature = "sqlite3")]
                    {
                        let envelopes = self.collection.envelopes.read();
                        let envelopes = envelopes.unwrap();
                        if let Err(err) = crate::sqlite3::remove(envelope_hash) {
                            melib::log(
                                format!(
                                    "Failed to remove envelope {} [{}] in cache: {}",
                                    &envelopes[&envelope_hash].message_id_display(),
                                    envelope_hash,
                                    err.to_string()
                                ),
                                melib::ERROR,
                            );
                        }
                    }
                    self.collection.remove(envelope_hash, mailbox_hash);
                    return Some(EnvelopeRemove(envelope_hash));
                }
                RefreshEventKind::Rescan => {
                    let handle = Account::new_worker(
                        self.mailbox_entries[&mailbox_hash].ref_mailbox.clone(),
                        &mut self.backend,
                        &self.work_context,
                        self.notify_fn.clone(),
                    );
                    self.mailbox_entries
                        .entry(mailbox_hash)
                        .and_modify(|entry| {
                            entry.worker = handle;
                        });
                }
                RefreshEventKind::Failure(e) => {
                    debug!("RefreshEvent Failure: {}", e.to_string());
                    /*
                    context
                        .1
                        .send(ThreadEvent::UIEvent(UIEvent::Notification(
                            Some(format!("{} watcher exited with error", &self.name)),
                            e.to_string(),
                            Some(crate::types::NotificationType::ERROR),
                        )))
                        .expect("Could not send event on main channel");
                    */
                    self.watch();
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

        let sender_ = self.sender.clone();
        let r = RefreshEventConsumer::new(Box::new(move |r| {
            sender_.send(ThreadEvent::from(r)).unwrap();
        }));
        let mut h = self.backend.write().unwrap().refresh(mailbox_hash, r)?;
        self.work_context.new_work.send(h.work().unwrap()).unwrap();
        Ok(())
    }
    pub fn watch(&self) {
        if self.settings.account().manual_refresh {
            return;
        }

        let sender_ = self.sender.clone();
        let r = RefreshEventConsumer::new(Box::new(move |r| {
            sender_.send(ThreadEvent::from(r)).unwrap();
        }));
        match self
            .backend
            .read()
            .unwrap()
            .watch(r, self.work_context.clone())
        {
            Ok(id) => {
                self.sender
                    .send(ThreadEvent::NewThread(
                        id,
                        format!("watching {}", self.name()).into(),
                    ))
                    .unwrap();
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

    pub fn load(&mut self, mailbox_hash: MailboxHash) -> result::Result<(), usize> {
        if mailbox_hash == 0 {
            return Err(0);
        }
        loop {
            match self
                .mailbox_entries
                .get_mut(&mailbox_hash)
                .unwrap()
                .worker
                .as_mut()
            {
                None => {
                    return match self.mailbox_entries[&mailbox_hash].status {
                        MailboxStatus::Available | MailboxStatus::Parsing(_, _)
                            if self.collection.mailboxes.contains_key(&mailbox_hash) =>
                        {
                            Ok(())
                        }
                        MailboxStatus::None => {
                            let handle = Account::new_worker(
                                self.mailbox_entries[&mailbox_hash].ref_mailbox.clone(),
                                &mut self.backend,
                                &self.work_context,
                                self.notify_fn.clone(),
                            );
                            self.mailbox_entries
                                .entry(mailbox_hash)
                                .and_modify(|entry| {
                                    entry.worker = handle;
                                });
                            self.collection.new_mailbox(mailbox_hash);
                            Err(0)
                        }
                        _ => Err(0),
                    }
                }
                Some(ref mut w) => match debug!(w.poll()) {
                    Ok(AsyncStatus::NoUpdate) => {
                        break;
                    }
                    Ok(AsyncStatus::Payload(payload)) => {
                        debug!("got payload in status for {}", mailbox_hash);
                        if payload.is_err() {
                            self.mailbox_entries
                                .entry(mailbox_hash)
                                .and_modify(|entry| {
                                    entry.status = MailboxStatus::Failed(payload.unwrap_err());
                                });
                            self.sender
                                .send(ThreadEvent::UIEvent(UIEvent::StartupCheck(mailbox_hash)))
                                .unwrap();
                            return Err(0);
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
                                    .send(ThreadEvent::UIEvent(UIEvent::StartupCheck(f)))
                                    .unwrap();
                            }
                        }
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::StartupCheck(mailbox_hash)))
                            .unwrap();
                    }
                    Ok(AsyncStatus::Finished) => {
                        debug!("got finished in status for {}", mailbox_hash);
                        self.mailbox_entries
                            .entry(mailbox_hash)
                            .and_modify(|entry| {
                                entry.status = MailboxStatus::Available;
                                entry.worker = None;
                            });
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::MailboxUpdate((
                                self.index,
                                mailbox_hash,
                            ))))
                            .unwrap();
                    }
                    Ok(AsyncStatus::ProgressReport(n)) => {
                        self.mailbox_entries
                            .entry(mailbox_hash)
                            .and_modify(|entry| match entry.status {
                                MailboxStatus::Parsing(ref mut d, _) => {
                                    *d += n;
                                }
                                _ => {}
                            });
                        //return Err(n);
                    }
                    _ => {
                        break;
                    }
                },
            };
        }
        if self.mailbox_entries[&mailbox_hash].status.is_available()
            || (self.mailbox_entries[&mailbox_hash].status.is_parsing()
                && self.collection.mailboxes.contains_key(&mailbox_hash))
        {
            Ok(())
        } else {
            Err(0)
        }
    }

    pub fn save_special(
        &self,
        bytes: &[u8],
        mailbox_type: SpecialUsageMailbox,
        flags: Flag,
    ) -> Result<()> {
        let mut failure = true;
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
                failure = false;
                break;
            }
        }

        if failure {
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
        Ok(())
    }

    pub fn save(&self, bytes: &[u8], mailbox: &str, flags: Option<Flag>) -> Result<()> {
        if self.settings.account.read_only() {
            return Err(MeliError::new(format!(
                "Account {} is read-only.",
                self.name.as_str()
            )));
        }
        self.backend.write().unwrap().save(bytes, mailbox, flags)
    }

    pub fn delete(&self, env_hash: EnvelopeHash) -> Result<()> {
        if self.settings.account.read_only() {
            return Err(MeliError::new(format!(
                "Account {} is read-only.",
                self.name.as_str()
            )));
        }
        self.backend.write().unwrap().delete(env_hash)
    }

    pub fn contains_key(&self, h: EnvelopeHash) -> bool {
        self.collection.contains_key(&h)
    }
    pub fn operation(&self, h: EnvelopeHash) -> Box<dyn BackendOp> {
        let operation = self.backend.read().unwrap().operation(h);
        if self.settings.account.read_only() {
            ReadOnlyOp::new(operation)
        } else {
            operation
        }
    }

    pub fn thread(&self, h: ThreadNodeHash, f: MailboxHash) -> &ThreadNode {
        &self.collection.threads[&f].thread_nodes()[&h]
    }

    pub fn mailbox_operation(
        &mut self,
        op: crate::execute::actions::MailboxOperation,
    ) -> Result<String> {
        use crate::execute::actions::MailboxOperation;
        if self.settings.account.read_only() {
            return Err(MeliError::new("Account is read-only."));
        }
        match op {
            MailboxOperation::Create(path) => {
                let (mailbox_hash, mut mailboxes) = self
                    .backend
                    .write()
                    .unwrap()
                    .create_mailbox(path.to_string())?;
                self.sender
                    .send(ThreadEvent::UIEvent(UIEvent::MailboxCreate((
                        self.index,
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
                    let tmp = SpecialUsageMailbox::detect_usage(mailboxes[&mailbox_hash].name());
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
                            parent.ref_mailbox = mailboxes.remove(&parent_hash).unwrap();
                        });
                }

                self.mailbox_entries.insert(
                    mailbox_hash,
                    MailboxEntry {
                        name: mailboxes[&mailbox_hash].path().to_string(),
                        status: MailboxStatus::Parsing(0, 0),
                        conf: new,
                        worker: Account::new_worker(
                            mailboxes[&mailbox_hash].clone(),
                            &mut self.backend,
                            &self.work_context,
                            self.notify_fn.clone(),
                        ),
                        ref_mailbox: mailboxes.remove(&mailbox_hash).unwrap(),
                    },
                );
                self.collection
                    .threads
                    .insert(mailbox_hash, Threads::default());
                self.collection
                    .mailboxes
                    .insert(mailbox_hash, Default::default());
                build_mailboxes_order(
                    &mut self.tree,
                    &self.mailbox_entries,
                    &mut self.mailboxes_order,
                );
                Ok(format!("`{}` successfully created.", &path))
            }
            MailboxOperation::Delete(path) => {
                if self.mailbox_entries.len() == 1 {
                    return Err(MeliError::new("Cannot delete only mailbox."));
                }
                let mailbox_hash = if let Some((mailbox_hash, _)) = self
                    .mailbox_entries
                    .iter()
                    .find(|(_, f)| f.ref_mailbox.path() == path)
                {
                    *mailbox_hash
                } else {
                    return Err(MeliError::new("Mailbox with that path not found."));
                };
                let mut mailboxes = self.backend.write().unwrap().delete_mailbox(mailbox_hash)?;
                self.sender
                    .send(ThreadEvent::UIEvent(UIEvent::MailboxDelete((
                        self.index,
                        mailbox_hash,
                    ))))
                    .unwrap();
                if let Some(pos) = self.mailboxes_order.iter().position(|&h| h == mailbox_hash) {
                    self.mailboxes_order.remove(pos);
                }
                if let Some(pos) = self.tree.iter().position(|n| n.hash == mailbox_hash) {
                    self.tree.remove(pos);
                }
                if self.sent_mailbox == Some(mailbox_hash) {
                    self.sent_mailbox = None;
                }
                self.collection.threads.remove(&mailbox_hash);
                /* if deleted mailbox had parent, we need to update its children field */
                if let Some(parent_hash) = self
                    .mailbox_entries
                    .remove(&mailbox_hash)
                    .unwrap()
                    .ref_mailbox
                    .parent()
                {
                    self.mailbox_entries
                        .entry(parent_hash)
                        .and_modify(|parent| {
                            parent.ref_mailbox = mailboxes.remove(&parent_hash).unwrap();
                        });
                }
                self.collection.mailboxes.remove(&mailbox_hash);
                build_mailboxes_order(
                    &mut self.tree,
                    &self.mailbox_entries,
                    &mut self.mailboxes_order,
                );
                // FIXME Kill worker as well

                // FIXME remove from settings as well

                Ok(format!("'`{}` has been deleted.", &path))
            }
            MailboxOperation::Subscribe(path) => {
                let mailbox_hash = if let Some((mailbox_hash, _)) = self
                    .mailbox_entries
                    .iter()
                    .find(|(_, f)| f.ref_mailbox.path() == path)
                {
                    *mailbox_hash
                } else {
                    return Err(MeliError::new("Mailbox with that path not found."));
                };
                self.backend
                    .write()
                    .unwrap()
                    .set_mailbox_subscription(mailbox_hash, true)?;
                self.mailbox_entries.entry(mailbox_hash).and_modify(|m| {
                    m.conf.mailbox_conf.subscribe = super::ToggleFlag::True;
                    let _ = m.ref_mailbox.set_is_subscribed(true);
                });

                Ok(format!("'`{}` has been subscribed.", &path))
            }
            MailboxOperation::Unsubscribe(path) => {
                let mailbox_hash = if let Some((mailbox_hash, _)) = self
                    .mailbox_entries
                    .iter()
                    .find(|(_, f)| f.ref_mailbox.path() == path)
                {
                    *mailbox_hash
                } else {
                    return Err(MeliError::new("Mailbox with that path not found."));
                };
                self.backend
                    .write()
                    .unwrap()
                    .set_mailbox_subscription(mailbox_hash, false)?;
                self.mailbox_entries.entry(mailbox_hash).and_modify(|m| {
                    m.conf.mailbox_conf.subscribe = super::ToggleFlag::False;
                    let _ = m.ref_mailbox.set_is_subscribed(false);
                });

                Ok(format!("'`{}` has been unsubscribed.", &path))
            }
            MailboxOperation::Rename(_, _) => Err(MeliError::new("Not implemented.")),
            MailboxOperation::SetPermissions(_) => Err(MeliError::new("Not implemented.")),
        }
    }

    pub fn special_use_mailbox(&self, special_use: SpecialUsageMailbox) -> Option<&str> {
        let ret = self
            .mailbox_entries
            .iter()
            .find(|(_, f)| f.conf.mailbox_conf().usage == Some(special_use));
        if let Some(ret) = ret.as_ref() {
            Some(ret.1.name())
        } else {
            None
        }
    }

    /* Call only in Context::is_online, since only Context can launch the watcher threads if an
     * account goes from offline to online. */
    pub fn is_online(&mut self) -> Result<()> {
        if !self.is_online {
            self.backend.write().unwrap().connect();
        }

        let ret = self.backend.read().unwrap().is_online();
        if ret.is_ok() != self.is_online && ret.is_ok() {
            self.init();
        }
        self.is_online = ret.is_ok();
        if !self.is_online {
            self.backend.write().unwrap().connect();
        }

        ret
    }

    pub fn search(
        &self,
        search_term: &str,
        sort: (SortField, SortOrder),
        mailbox_hash: MailboxHash,
    ) -> Result<SmallVec<[EnvelopeHash; 512]>> {
        if self.settings.account().format() == "imap" {
            use melib::parsec::Parser;
            let query = melib::search::query().parse(search_term)?.1;
            return self
                .backend
                .read()
                .unwrap()
                .search(query, Some(mailbox_hash));
        }

        #[cfg(feature = "notmuch")]
        {
            if self.settings.account().format() == "notmuch" {
                let backend_lck = self.backend.read().unwrap();
                let b = (*backend_lck).as_any();
                return if let Some(notmuch_backend) = b.downcast_ref::<melib::backends::NotmuchDb>()
                {
                    notmuch_backend.search(search_term)
                } else {
                    Err(MeliError::new(
                        "Internal error: Could not downcast backend to NotmuchDb",
                    ))
                };
            }
        }

        #[cfg(feature = "sqlite3")]
        {
            crate::sqlite3::search(search_term, sort)
        }

        #[cfg(not(feature = "sqlite3"))]
        {
            let mut ret = SmallVec::new();
            let envelopes = self.collection.envelopes.read().unwrap();

            for &env_hash in self.collection[&mailbox_hash].iter() {
                let envelope = &envelopes[&env_hash];
                if envelope.subject().contains(&search_term) {
                    ret.push(env_hash);
                    continue;
                }
                if envelope.field_from_to_string().contains(&search_term) {
                    ret.push(env_hash);
                    continue;
                }
                let op = self.operation(env_hash);
                let body = envelope.body(op)?;
                let decoded = decode_rec(&body, None);
                let body_text = String::from_utf8_lossy(&decoded);
                if body_text.contains(&search_term) {
                    ret.push(env_hash);
                }
            }
            Ok(ret)
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
    mailbox_entries: &HashMap<MailboxHash, MailboxEntry>,
    mailboxes_order: &mut Vec<MailboxHash>,
) {
    tree.clear();
    mailboxes_order.clear();
    for (h, f) in mailbox_entries.iter() {
        if f.ref_mailbox.parent().is_none() {
            fn rec(
                h: MailboxHash,
                mailbox_entries: &HashMap<MailboxHash, MailboxEntry>,
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
