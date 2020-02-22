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

use super::{AccountConf, FileFolderConf};
use fnv::FnvHashMap;
use melib::async_workers::{Async, AsyncBuilder, AsyncStatus, WorkContext};
use melib::backends::{
    BackendOp, Backends, Folder, FolderHash, MailBackend, NotifyFn, ReadOnlyOp, RefreshEvent,
    RefreshEventConsumer, RefreshEventKind, SpecialUsageMailbox,
};
use melib::email::*;
use melib::error::{MeliError, Result};
use melib::text_processing::GlobMatch;
use melib::thread::{SortField, SortOrder, ThreadNode, ThreadNodeHash, Threads};
use melib::AddressBook;
use melib::Collection;
use smallvec::SmallVec;

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

#[derive(Serialize, Debug)]
pub enum MailboxStatus {
    Available,
    Failed(MeliError),
    /// first argument is done work, and second is total work
    Parsing(usize, usize),
    None,
}

impl Default for MailboxStatus {
    fn default() -> Self {
        MailboxStatus::Parsing(0, 0)
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
pub struct FolderEntry {
    pub status: MailboxStatus,
    pub name: String,
    pub ref_folder: Folder,
    pub conf: FileFolderConf,
    pub worker: Worker,
}

impl FolderEntry {
    pub fn status(&self) -> String {
        match self.status {
            MailboxStatus::Available => self.name().to_string(),
            MailboxStatus::Failed(ref e) => e.to_string(),
            MailboxStatus::None => "Not subscribed, is this a bug?".to_string(),
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
    pub(crate) folder_entries: FnvHashMap<FolderHash, FolderEntry>,
    pub(crate) folders_order: Vec<FolderHash>,
    tree: Vec<FolderNode>,
    sent_folder: Option<FolderHash>,
    pub(crate) collection: Collection,
    pub(crate) address_book: AddressBook,
    pub(crate) work_context: WorkContext,
    pub(crate) settings: AccountConf,
    pub(crate) runtime_settings: AccountConf,
    pub(crate) backend: Arc<RwLock<Box<dyn MailBackend>>>,

    sender: Sender<ThreadEvent>,
    event_queue: VecDeque<(FolderHash, RefreshEvent)>,
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
        }
    }
}

#[derive(Serialize, Debug, Clone, Default)]
pub struct FolderNode {
    pub hash: FolderHash,
    pub depth: usize,
    pub children: Vec<FolderNode>,
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
                s.account.subscribed_folders.is_empty()
                    || (s.folder_confs.contains_key(path)
                        && s.folder_confs[path].folder_conf().subscribe.is_true())
                    || s.account
                        .subscribed_folders
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
            folder_entries: Default::default(),
            folders_order: Default::default(),
            tree: Default::default(),
            address_book,
            sent_folder: Default::default(),
            collection: Default::default(),
            work_context,
            runtime_settings: settings.clone(),
            settings,
            backend: Arc::new(RwLock::new(backend)),
            notify_fn,
            sender,

            event_queue: VecDeque::with_capacity(8),
        })
    }

    fn init(&mut self) {
        let mut ref_folders: FnvHashMap<FolderHash, Folder> =
            match self.backend.read().unwrap().folders() {
                Ok(f) => f,
                Err(err) => {
                    debug!(&err);
                    return;
                }
            };
        let mut folder_entries: FnvHashMap<FolderHash, FolderEntry> =
            FnvHashMap::with_capacity_and_hasher(ref_folders.len(), Default::default());
        let mut folders_order: Vec<FolderHash> = Vec::with_capacity(ref_folders.len());

        let mut sent_folder = None;
        for f in ref_folders.values_mut() {
            if let Some(conf) = self.settings.folder_confs.get_mut(f.path()) {
                conf.folder_conf.usage = if f.special_usage() != SpecialUsageMailbox::Normal {
                    Some(f.special_usage())
                } else {
                    let tmp = SpecialUsageMailbox::detect_usage(f.name());
                    if tmp != Some(SpecialUsageMailbox::Normal) && tmp != None {
                        let _ = f.set_special_usage(tmp.unwrap());
                    }
                    tmp
                };
                match conf.folder_conf.usage {
                    Some(SpecialUsageMailbox::Sent) => {
                        sent_folder = Some(f.hash());
                    }
                    None => {
                        if f.special_usage() == SpecialUsageMailbox::Sent {
                            sent_folder = Some(f.hash());
                        }
                    }
                    _ => {}
                }
                folder_entries.insert(
                    f.hash(),
                    FolderEntry {
                        ref_folder: f.clone(),
                        name: f.path().to_string(),
                        status: MailboxStatus::None,
                        conf: conf.clone(),
                        worker: None,
                    },
                );
            } else {
                let mut new = FileFolderConf::default();
                new.folder_conf.usage = if f.special_usage() != SpecialUsageMailbox::Normal {
                    Some(f.special_usage())
                } else {
                    let tmp = SpecialUsageMailbox::detect_usage(f.name());
                    if tmp != Some(SpecialUsageMailbox::Normal) && tmp != None {
                        let _ = f.set_special_usage(tmp.unwrap());
                    }
                    tmp
                };
                if new.folder_conf.usage == Some(SpecialUsageMailbox::Sent) {
                    sent_folder = Some(f.hash());
                }

                folder_entries.insert(
                    f.hash(),
                    FolderEntry {
                        ref_folder: f.clone(),
                        name: f.path().to_string(),
                        status: MailboxStatus::None,
                        conf: new,
                        worker: None,
                    },
                );
            }
        }

        let mut tree: Vec<FolderNode> = Vec::new();
        let mut collection: Collection = Collection::new(Default::default());
        for (h, f) in ref_folders.iter() {
            if !f.is_subscribed() {
                /* Skip unsubscribed folder */
                continue;
            }
            folder_entries.entry(*h).and_modify(|entry| {
                entry.status = MailboxStatus::Parsing(0, 0);
                entry.worker = Account::new_worker(
                    f.clone(),
                    &mut self.backend,
                    &self.work_context,
                    self.notify_fn.clone(),
                );
            });
            collection.mailboxes.insert(*h, Default::default());
            collection.threads.insert(*h, Threads::default());
        }

        build_folders_order(&mut tree, &folder_entries, &mut folders_order);
        self.folders_order = folders_order;
        self.folder_entries = folder_entries;
        self.tree = tree;
        self.sent_folder = sent_folder;
        self.collection = collection;
    }

    fn new_worker(
        folder: Folder,
        backend: &Arc<RwLock<Box<dyn MailBackend>>>,
        work_context: &WorkContext,
        notify_fn: Arc<NotifyFn>,
    ) -> Worker {
        let mut mailbox_handle = backend.write().unwrap().get(&folder);
        let mut builder = AsyncBuilder::new();
        let our_tx = builder.tx();
        let folder_hash = folder.hash();
        let priority = match folder.special_usage() {
            SpecialUsageMailbox::Inbox => 0,
            SpecialUsageMailbox::Sent => 1,
            SpecialUsageMailbox::Drafts | SpecialUsageMailbox::Trash => 2,
            _ => {
                3 * folder
                    .path()
                    .split(if folder.path().contains('/') {
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
            let name = format!("Parsing {}", folder.path());
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
                        debug!("notifying for {}", folder_hash);
                        notify_fn.notify(folder_hash);
                    }
                    Ok(s @ AsyncStatus::Finished) => {
                        our_tx.send(s).unwrap();
                        notify_fn.notify(folder_hash);
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
    pub fn reload(&mut self, event: RefreshEvent, folder_hash: FolderHash) -> Option<UIEvent> {
        if !self.folder_entries[&folder_hash].status.is_available() {
            self.event_queue.push_back((folder_hash, event));
            return None;
        }

        let kind = event.kind();
        {
            //let mailbox: &mut Mailbox = self.folders[idx].as_mut().unwrap().as_mut().unwrap();
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
                    self.collection.update(old_hash, *envelope, folder_hash);
                    return Some(EnvelopeUpdate(old_hash));
                }
                RefreshEventKind::Rename(old_hash, new_hash) => {
                    debug!("rename {} to {}", old_hash, new_hash);
                    self.collection.rename(old_hash, new_hash, folder_hash);
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
                        && self.collection[&folder_hash].contains(&env_hash)
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
                    self.collection.insert(*envelope, folder_hash);
                    if self
                        .sent_folder
                        .as_ref()
                        .map(|h| *h == folder_hash)
                        .unwrap_or(false)
                    {
                        self.collection.insert_reply(env_hash);
                    }

                    if self.folder_entries[&folder_hash]
                        .conf
                        .folder_conf
                        .ignore
                        .is_true()
                    {
                        return Some(UIEvent::MailboxUpdate((self.index, folder_hash)));
                    }

                    let thread = {
                        let thread_hash = self.collection.get_env(env_hash).thread();
                        self.collection.threads[&folder_hash]
                            .find_group(self.collection.threads[&folder_hash][&thread_hash].group)
                    };
                    if self.collection.threads[&folder_hash]
                        .thread_ref(thread)
                        .snoozed()
                    {
                        return Some(UIEvent::MailboxUpdate((self.index, folder_hash)));
                    }
                    if is_seen || is_draft {
                        return Some(UIEvent::MailboxUpdate((self.index, folder_hash)));
                    }

                    return Some(Notification(
                        Some(format!("new e-mail from: {}", from)),
                        format!(
                            "{}\n{} {}",
                            subject,
                            self.name,
                            self.folder_entries[&folder_hash].name()
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
                    self.collection.remove(envelope_hash, folder_hash);
                    return Some(EnvelopeRemove(envelope_hash));
                }
                RefreshEventKind::Rescan => {
                    let handle = Account::new_worker(
                        self.folder_entries[&folder_hash].ref_folder.clone(),
                        &mut self.backend,
                        &self.work_context,
                        self.notify_fn.clone(),
                    );
                    self.folder_entries.entry(folder_hash).and_modify(|entry| {
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
    pub fn refresh(&mut self, folder_hash: FolderHash) -> Result<()> {
        if let Some(ref refresh_command) = self.settings.conf().refresh_command {
            let parts = crate::split_command!(refresh_command);
            let (cmd, args) = (parts[0], &parts[1..]);
            std::process::Command::new(cmd)
                .args(args)
                .stdin(std::process::Stdio::null())
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::piped())
                .spawn()?;
            return Ok(());
        }

        let sender_ = self.sender.clone();
        let r = RefreshEventConsumer::new(Box::new(move |r| {
            sender_.send(ThreadEvent::from(r)).unwrap();
        }));
        let mut h = self.backend.write().unwrap().refresh(folder_hash, r)?;
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

    pub fn list_folders(&self) -> Vec<FolderNode> {
        let mut ret = Vec::with_capacity(self.folder_entries.len());
        fn rec(node: &FolderNode, ret: &mut Vec<FolderNode>) {
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

    pub fn folders_order(&self) -> &Vec<FolderHash> {
        &self.folders_order
    }
    pub fn name(&self) -> &str {
        &self.name
    }

    fn load_mailbox(&mut self, folder_hash: FolderHash, payload: Result<Vec<Envelope>>) {
        if payload.is_err() {
            self.folder_entries.entry(folder_hash).and_modify(|entry| {
                entry.status = MailboxStatus::Failed(payload.unwrap_err());
            });
            self.sender
                .send(ThreadEvent::UIEvent(UIEvent::StartupCheck(folder_hash)))
                .unwrap();
            return;
        }
        let envelopes = payload
            .unwrap()
            .into_iter()
            .map(|e| (e.hash(), e))
            .collect::<FnvHashMap<EnvelopeHash, Envelope>>();
        if let Some(updated_folders) =
            self.collection
                .merge(envelopes, folder_hash, self.sent_folder)
        {
            for f in updated_folders {
                self.sender
                    .send(ThreadEvent::UIEvent(UIEvent::StartupCheck(f)))
                    .unwrap();
            }
        }
        self.sender
            .send(ThreadEvent::UIEvent(UIEvent::StartupCheck(folder_hash)))
            .unwrap();
    }

    pub fn status(&mut self, folder_hash: FolderHash) -> result::Result<(), usize> {
        if folder_hash == 0 {
            return Err(0);
        }
        loop {
            match self
                .folder_entries
                .get_mut(&folder_hash)
                .unwrap()
                .worker
                .as_mut()
            {
                None => {
                    return if self.folder_entries[&folder_hash].status.is_available()
                        || (self.folder_entries[&folder_hash].status.is_parsing()
                            && self.collection.mailboxes.contains_key(&folder_hash))
                    {
                        Ok(())
                    } else {
                        Err(0)
                    };
                }
                Some(ref mut w) => match debug!(w.poll()) {
                    Ok(AsyncStatus::NoUpdate) => {
                        break;
                    }
                    Ok(AsyncStatus::Payload(envs)) => {
                        debug!("got payload in status for {}", folder_hash);
                        self.load_mailbox(folder_hash, envs);
                    }
                    Ok(AsyncStatus::Finished) => {
                        debug!("got finished in status for {}", folder_hash);
                        self.folder_entries.entry(folder_hash).and_modify(|entry| {
                            entry.status = MailboxStatus::Available;
                            entry.worker = None;
                        });
                        self.sender
                            .send(ThreadEvent::UIEvent(UIEvent::MailboxUpdate((
                                self.index,
                                folder_hash,
                            ))))
                            .unwrap();
                    }
                    Ok(AsyncStatus::ProgressReport(n)) => {
                        self.folder_entries.entry(folder_hash).and_modify(|entry| {
                            match entry.status {
                                MailboxStatus::Parsing(ref mut d, _) => {
                                    *d += n;
                                }
                                _ => {}
                            }
                        });
                        //return Err(n);
                    }
                    _ => {
                        break;
                    }
                },
            };
        }
        if self.folder_entries[&folder_hash].status.is_available()
            || (self.folder_entries[&folder_hash].status.is_parsing()
                && self.collection.mailboxes.contains_key(&folder_hash))
        {
            Ok(())
        } else {
            Err(0)
        }
    }

    pub fn save_special(
        &self,
        bytes: &[u8],
        folder_type: SpecialUsageMailbox,
        flags: Flag,
    ) -> Result<()> {
        let mut failure = true;
        for folder in &[
            self.special_use_folder(folder_type),
            self.special_use_folder(SpecialUsageMailbox::Inbox),
            self.special_use_folder(SpecialUsageMailbox::Normal),
        ] {
            if folder.is_none() {
                continue;
            }
            let folder = folder.unwrap();
            if let Err(e) = self.save(bytes, folder, Some(flags)) {
                debug!("{:?} could not save msg", e);
                melib::log(
                    format!("Could not save in '{}' folder: {}.", folder, e.to_string()),
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
            .set_summary("Could not save in any folder"));
        }
        Ok(())
    }

    pub fn save(&self, bytes: &[u8], folder: &str, flags: Option<Flag>) -> Result<()> {
        if self.settings.account.read_only() {
            return Err(MeliError::new(format!(
                "Account {} is read-only.",
                self.name.as_str()
            )));
        }
        self.backend.write().unwrap().save(bytes, folder, flags)
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

    pub fn thread(&self, h: ThreadNodeHash, f: FolderHash) -> &ThreadNode {
        &self.collection.threads[&f].thread_nodes()[&h]
    }

    pub fn folder_operation(
        &mut self,
        op: crate::execute::actions::FolderOperation,
    ) -> Result<String> {
        use crate::execute::actions::FolderOperation;
        if self.settings.account.read_only() {
            return Err(MeliError::new("Account is read-only."));
        }
        match op {
            FolderOperation::Create(path) => {
                let (folder_hash, mut folders) = self
                    .backend
                    .write()
                    .unwrap()
                    .create_folder(path.to_string())?;
                self.sender
                    .send(ThreadEvent::UIEvent(UIEvent::MailboxCreate((
                        self.index,
                        folder_hash,
                    ))))
                    .unwrap();
                let mut new = FileFolderConf::default();
                new.folder_conf.subscribe = super::ToggleFlag::InternalVal(true);
                new.folder_conf.usage =
                    if folders[&folder_hash].special_usage() != SpecialUsageMailbox::Normal {
                        Some(folders[&folder_hash].special_usage())
                    } else {
                        let tmp = SpecialUsageMailbox::detect_usage(folders[&folder_hash].name());
                        if tmp != Some(SpecialUsageMailbox::Normal) && tmp != None {
                            folders.entry(folder_hash).and_modify(|entry| {
                                let _ = entry.set_special_usage(tmp.unwrap());
                            });
                        }
                        tmp
                    };
                /* if new folder has parent, we need to update its children field */
                if let Some(parent_hash) = folders[&folder_hash].parent() {
                    self.folder_entries.entry(parent_hash).and_modify(|parent| {
                        parent.ref_folder = folders.remove(&parent_hash).unwrap();
                    });
                }

                self.folder_entries.insert(
                    folder_hash,
                    FolderEntry {
                        name: folders[&folder_hash].path().to_string(),
                        status: MailboxStatus::Parsing(0, 0),
                        conf: new,
                        worker: Account::new_worker(
                            folders[&folder_hash].clone(),
                            &mut self.backend,
                            &self.work_context,
                            self.notify_fn.clone(),
                        ),
                        ref_folder: folders.remove(&folder_hash).unwrap(),
                    },
                );
                self.collection
                    .threads
                    .insert(folder_hash, Threads::default());
                self.collection
                    .mailboxes
                    .insert(folder_hash, Default::default());
                build_folders_order(
                    &mut self.tree,
                    &self.folder_entries,
                    &mut self.folders_order,
                );
                Ok(format!("`{}` successfully created.", &path))
            }
            FolderOperation::Delete(path) => {
                if self.folder_entries.len() == 1 {
                    return Err(MeliError::new("Cannot delete only mailbox."));
                }
                let folder_hash = if let Some((folder_hash, _)) = self
                    .folder_entries
                    .iter()
                    .find(|(_, f)| f.ref_folder.path() == path)
                {
                    *folder_hash
                } else {
                    return Err(MeliError::new("Mailbox with that path not found."));
                };
                let mut folders = self.backend.write().unwrap().delete_folder(folder_hash)?;
                self.sender
                    .send(ThreadEvent::UIEvent(UIEvent::MailboxDelete((
                        self.index,
                        folder_hash,
                    ))))
                    .unwrap();
                if let Some(pos) = self.folders_order.iter().position(|&h| h == folder_hash) {
                    self.folders_order.remove(pos);
                }
                if let Some(pos) = self.tree.iter().position(|n| n.hash == folder_hash) {
                    self.tree.remove(pos);
                }
                if self.sent_folder == Some(folder_hash) {
                    self.sent_folder = None;
                }
                self.collection.threads.remove(&folder_hash);
                /* if deleted folder had parent, we need to update its children field */
                if let Some(parent_hash) = self
                    .folder_entries
                    .remove(&folder_hash)
                    .unwrap()
                    .ref_folder
                    .parent()
                {
                    self.folder_entries.entry(parent_hash).and_modify(|parent| {
                        parent.ref_folder = folders.remove(&parent_hash).unwrap();
                    });
                }
                self.collection.mailboxes.remove(&folder_hash);
                build_folders_order(
                    &mut self.tree,
                    &self.folder_entries,
                    &mut self.folders_order,
                );
                // FIXME Kill worker as well

                // FIXME remove from settings as well

                Ok(format!("'`{}` has been deleted.", &path))
            }
            FolderOperation::Subscribe(_) => Err(MeliError::new("Not implemented.")),
            FolderOperation::Unsubscribe(_) => Err(MeliError::new("Not implemented.")),
            FolderOperation::Rename(_, _) => Err(MeliError::new("Not implemented.")),
            FolderOperation::SetPermissions(_) => Err(MeliError::new("Not implemented.")),
        }
    }

    pub fn special_use_folder(&self, special_use: SpecialUsageMailbox) -> Option<&str> {
        let ret = self
            .folder_entries
            .iter()
            .find(|(_, f)| f.conf.folder_conf().usage == Some(special_use));
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
        ret
    }

    pub fn search(
        &self,
        search_term: &str,
        sort: (SortField, SortOrder),
        folder_hash: FolderHash,
    ) -> Result<SmallVec<[EnvelopeHash; 512]>> {
        if self.settings.account().format() == "imap" {
            return crate::cache::imap_search(search_term, sort, folder_hash, &self.backend);
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

            for &env_hash in &self.collection[&folder_hash].iter() {
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

impl Index<&FolderHash> for Account {
    type Output = FolderEntry;
    fn index(&self, index: &FolderHash) -> &FolderEntry {
        &self.folder_entries[index]
    }
}

impl IndexMut<&FolderHash> for Account {
    fn index_mut(&mut self, index: &FolderHash) -> &mut FolderEntry {
        self.folder_entries.get_mut(index).unwrap()
    }
}

fn build_folders_order(
    tree: &mut Vec<FolderNode>,
    folder_entries: &FnvHashMap<FolderHash, FolderEntry>,
    folders_order: &mut Vec<FolderHash>,
) {
    tree.clear();
    folders_order.clear();
    for (h, f) in folder_entries.iter() {
        if f.ref_folder.parent().is_none() {
            fn rec(
                h: FolderHash,
                folder_entries: &FnvHashMap<FolderHash, FolderEntry>,
                depth: usize,
            ) -> FolderNode {
                let mut node = FolderNode {
                    hash: h,
                    children: Vec::new(),
                    depth,
                };
                for &c in folder_entries[&h].ref_folder.children() {
                    if folder_entries.contains_key(&c) {
                        node.children.push(rec(c, folder_entries, depth + 1));
                    }
                }
                node
            };

            tree.push(rec(*h, &folder_entries, 0));
        }
    }

    tree.sort_unstable_by(|a, b| {
        if folder_entries[&b.hash]
            .ref_folder
            .path()
            .eq_ignore_ascii_case("INBOX")
        {
            std::cmp::Ordering::Greater
        } else if folder_entries[&a.hash]
            .ref_folder
            .path()
            .eq_ignore_ascii_case("INBOX")
        {
            std::cmp::Ordering::Less
        } else {
            folder_entries[&a.hash]
                .ref_folder
                .path()
                .cmp(&folder_entries[&b.hash].ref_folder.path())
        }
    });

    let mut stack: SmallVec<[Option<&FolderNode>; 16]> = SmallVec::new();
    for n in tree.iter_mut() {
        folders_order.push(n.hash);
        n.children.sort_unstable_by(|a, b| {
            if folder_entries[&b.hash]
                .ref_folder
                .path()
                .eq_ignore_ascii_case("INBOX")
            {
                std::cmp::Ordering::Greater
            } else if folder_entries[&a.hash]
                .ref_folder
                .path()
                .eq_ignore_ascii_case("INBOX")
            {
                std::cmp::Ordering::Less
            } else {
                folder_entries[&a.hash]
                    .ref_folder
                    .path()
                    .cmp(&folder_entries[&b.hash].ref_folder.path())
            }
        });
        stack.extend(n.children.iter().rev().map(Some));
        while let Some(Some(next)) = stack.pop() {
            folders_order.push(next.hash);
            stack.extend(next.children.iter().rev().map(Some));
        }
    }
}
