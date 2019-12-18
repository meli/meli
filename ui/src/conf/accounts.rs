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
    BackendOp, Backends, Folder, FolderHash, FolderOperation, MailBackend, NotifyFn, ReadOnlyOp,
    RefreshEvent, RefreshEventConsumer, RefreshEventKind, SpecialUsageMailbox,
};
use melib::error::{MeliError, Result};
use melib::mailbox::*;
use melib::thread::{SortField, SortOrder, ThreadHash, ThreadNode, Threads};
use melib::AddressBook;
use melib::StackVec;
use text_processing::GlobMatch;

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

macro_rules! mailbox {
    ($idx:expr, $folders:expr) => {
        $folders.get_mut(&$idx).unwrap().unwrap_mut()
    };
}

#[derive(Serialize, Debug)]
pub enum MailboxEntry {
    Available(Mailbox),
    Failed(MeliError),
    /// first argument is done work, and second is total work
    Parsing(Mailbox, usize, usize),
}

impl Default for MailboxEntry {
    fn default() -> Self {
        MailboxEntry::Parsing(Mailbox::default(), 0, 0)
    }
}

impl std::fmt::Display for MailboxEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MailboxEntry::Available(ref m) => m.name().to_string(),
                MailboxEntry::Failed(ref e) => e.to_string(),
                MailboxEntry::Parsing(_, done, total) => {
                    format!("Parsing messages. [{}/{}]", done, total)
                }
            }
        )
    }
}
impl MailboxEntry {
    pub fn is_available(&self) -> bool {
        if let MailboxEntry::Available(_) = self {
            true
        } else {
            false
        }
    }
    pub fn is_parsing(&self) -> bool {
        if let MailboxEntry::Parsing(_, _, _) = self {
            true
        } else {
            false
        }
    }

    pub fn as_result(&self) -> Result<&Mailbox> {
        match self {
            MailboxEntry::Available(ref m) => Ok(m),
            MailboxEntry::Parsing(ref m, _, _) => Ok(m),
            MailboxEntry::Failed(ref e) => Err(MeliError::new(format!(
                "Mailbox is not available: {}",
                e.to_string()
            ))),
        }
    }

    pub fn as_mut_result(&mut self) -> Result<&mut Mailbox> {
        match self {
            MailboxEntry::Available(ref mut m) => Ok(m),
            MailboxEntry::Parsing(ref mut m, _, _) => Ok(m),
            MailboxEntry::Failed(ref e) => Err(MeliError::new(format!(
                "Mailbox is not available: {}",
                e.to_string()
            ))),
        }
    }

    pub fn unwrap_mut(&mut self) -> &mut Mailbox {
        match self {
            MailboxEntry::Available(ref mut m) => m,
            MailboxEntry::Parsing(ref mut m, _, _) => m,
            e => panic!(format!("mailbox is not available! {:#}", e)),
        }
    }

    pub fn unwrap(&self) -> &Mailbox {
        match self {
            MailboxEntry::Available(ref m) => m,
            MailboxEntry::Parsing(ref m, _, _) => m,
            e => panic!(format!("mailbox is not available! {:#}", e)),
        }
    }
}

#[derive(Debug)]
pub struct Account {
    pub index: usize,
    name: String,
    pub is_online: bool,
    pub(crate) folders: FnvHashMap<FolderHash, MailboxEntry>,
    pub(crate) ref_folders: FnvHashMap<FolderHash, Folder>,
    pub(crate) folder_confs: FnvHashMap<FolderHash, FileFolderConf>,
    pub(crate) folders_order: Vec<FolderHash>,
    pub(crate) folder_names: FnvHashMap<FolderHash, String>,
    tree: Vec<FolderNode>,
    sent_folder: Option<FolderHash>,
    pub(crate) collection: Collection,

    pub(crate) address_book: AddressBook,

    pub(crate) workers: FnvHashMap<FolderHash, Worker>,
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
                if let Err(err) = bincode::serialize_into(writer, &self.folders) {
                    eprintln!("{}", err);
                };
            };
        }
    }
}

pub struct MailboxIterator<'a> {
    folders_order: &'a [FolderHash],
    folders: &'a FnvHashMap<FolderHash, MailboxEntry>,
    pos: usize,
}

impl<'a> Iterator for MailboxIterator<'a> {
    type Item = &'a MailboxEntry;

    fn next(&mut self) -> Option<&'a MailboxEntry> {
        if self.pos == self.folders.len() {
            return None;
        }
        let fh = &self.folders_order[self.pos];

        self.pos += 1;
        Some(&self.folders[&fh])
    }
}

#[derive(Serialize, Debug, Default)]
struct FolderNode {
    hash: FolderHash,
    kids: Vec<FolderNode>,
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
                (s.folder_confs.contains_key(path)
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
            folders: Default::default(),
            ref_folders: Default::default(),
            folder_confs: Default::default(),
            folders_order: Default::default(),
            folder_names: Default::default(),
            tree: Default::default(),
            address_book,
            sent_folder: Default::default(),
            collection: Default::default(),
            workers: Default::default(),
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
        let mut folders: FnvHashMap<FolderHash, MailboxEntry> =
            FnvHashMap::with_capacity_and_hasher(ref_folders.len(), Default::default());
        let mut folders_order: Vec<FolderHash> = Vec::with_capacity(ref_folders.len());
        let mut workers: FnvHashMap<FolderHash, Worker> = FnvHashMap::default();
        let mut folder_names = FnvHashMap::default();
        let mut folder_confs = FnvHashMap::default();

        let mut sent_folder = None;
        for f in ref_folders.values_mut() {
            if !((self.settings.folder_confs.contains_key(f.path())
                && self.settings.folder_confs[f.path()]
                    .folder_conf()
                    .subscribe
                    .is_true())
                || self
                    .settings
                    .account
                    .subscribed_folders
                    .iter()
                    .any(|m| f.path().matches_glob(m)))
            {
                /* Skip unsubscribed folder */
                continue;
            }

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
                folder_confs.insert(f.hash(), conf.clone());
            } else {
                let mut new = FileFolderConf::default();
                new.folder_conf.subscribe = super::ToggleFlag::InternalVal(true);
                new.folder_conf.usage = if f.special_usage() != SpecialUsageMailbox::Normal {
                    Some(f.special_usage())
                } else {
                    let tmp = SpecialUsageMailbox::detect_usage(f.name());
                    if tmp != Some(SpecialUsageMailbox::Normal) && tmp != None {
                        let _ = f.set_special_usage(tmp.unwrap());
                    }
                    tmp
                };

                folder_confs.insert(f.hash(), new);
            }
            folder_names.insert(f.hash(), f.path().to_string());
        }

        let mut stack: StackVec<FolderHash> = StackVec::new();
        let mut tree: Vec<FolderNode> = Vec::new();
        let mut collection: Collection = Collection::new(Default::default());
        for (h, f) in ref_folders.iter() {
            if !folder_confs.contains_key(&h) {
                /* Skip unsubscribed folder */
                continue;
            }
            if f.parent().is_none() {
                fn rec(h: FolderHash, ref_folders: &FnvHashMap<FolderHash, Folder>) -> FolderNode {
                    let mut node = FolderNode {
                        hash: h,
                        kids: Vec::new(),
                    };
                    for &c in ref_folders[&h].children() {
                        node.kids.push(rec(c, ref_folders));
                    }
                    node
                };

                tree.push(rec(*h, &ref_folders));
                for &c in f.children() {
                    stack.push(c);
                }
                while let Some(next) = stack.pop() {
                    for c in ref_folders[&next].children() {
                        stack.push(*c);
                    }
                }
            }
            folders.insert(
                *h,
                MailboxEntry::Parsing(Mailbox::new(f.clone(), &FnvHashMap::default()), 0, 0),
            );
            workers.insert(
                *h,
                Account::new_worker(
                    f.clone(),
                    &mut self.backend,
                    &self.work_context,
                    self.notify_fn.clone(),
                ),
            );
            collection.threads.insert(*h, Threads::default());
        }

        tree.sort_unstable_by(|a, b| {
            if ref_folders[&b.hash].path().eq_ignore_ascii_case("INBOX") {
                std::cmp::Ordering::Greater
            } else if ref_folders[&a.hash].path().eq_ignore_ascii_case("INBOX") {
                std::cmp::Ordering::Less
            } else {
                ref_folders[&a.hash]
                    .path()
                    .cmp(&ref_folders[&b.hash].path())
            }
        });

        let mut stack: StackVec<Option<&FolderNode>> = StackVec::new();
        for n in tree.iter_mut() {
            folders_order.push(n.hash);
            n.kids.sort_unstable_by(|a, b| {
                if ref_folders[&b.hash].path().eq_ignore_ascii_case("INBOX") {
                    std::cmp::Ordering::Greater
                } else if ref_folders[&a.hash].path().eq_ignore_ascii_case("INBOX") {
                    std::cmp::Ordering::Less
                } else {
                    ref_folders[&a.hash]
                        .path()
                        .cmp(&ref_folders[&b.hash].path())
                }
            });
            stack.extend(n.kids.iter().rev().map(Some));
            while let Some(Some(next)) = stack.pop() {
                folders_order.push(next.hash);
                stack.extend(next.kids.iter().rev().map(Some));
            }
        }

        self.folders = folders;
        self.ref_folders = ref_folders;
        self.folder_confs = folder_confs;
        self.folders_order = folders_order;
        self.folder_names = folder_names;
        self.tree = tree;
        self.sent_folder = sent_folder;
        self.collection = collection;
        self.workers = workers;
    }

    fn new_worker(
        folder: Folder,
        backend: &Arc<RwLock<Box<dyn MailBackend>>>,
        work_context: &WorkContext,
        notify_fn: Arc<NotifyFn>,
    ) -> Worker {
        let mailbox_handle = backend.write().unwrap().get(&folder);
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
            let mut mailbox_handle = mailbox_handle.clone();
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
        if !self.folders[&folder_hash].is_available() {
            self.event_queue.push_back((folder_hash, event));
            return None;
        }

        let kind = event.kind();
        {
            //let mailbox: &mut Mailbox = self.folders[idx].as_mut().unwrap().as_mut().unwrap();
            match kind {
                RefreshEventKind::Update(old_hash, envelope) => {
                    mailbox!(&folder_hash, self.folders).rename(old_hash, envelope.hash());
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
                    mailbox!(&folder_hash, self.folders).rename(old_hash, new_hash);
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
                        && mailbox!(&folder_hash, self.folders)
                            .envelopes
                            .contains(&env_hash)
                    {
                        return None;
                    }
                    mailbox!(&folder_hash, self.folders).insert(env_hash);
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

                    let ref_folders: FnvHashMap<FolderHash, Folder> =
                        self.backend.read().unwrap().folders().unwrap();
                    let folder_conf = &self.folder_confs[&folder_hash];
                    if folder_conf.folder_conf().ignore.is_true() {
                        return Some(UIEvent::MailboxUpdate((self.index, folder_hash)));
                    }

                    let thread_node = {
                        let thread_hash = &mut self.collection.get_env(env_hash).thread();
                        &self.collection.threads[&folder_hash][&thread_hash]
                    };
                    if thread_node.snoozed() {
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
                            ref_folders[&folder_hash].name(),
                        ),
                        Some(crate::types::NotificationType::NewMail),
                    ));
                }
                RefreshEventKind::Remove(envelope_hash) => {
                    mailbox!(&folder_hash, self.folders).remove(envelope_hash);
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
                    let ref_folders: FnvHashMap<FolderHash, Folder> =
                        self.backend.read().unwrap().folders().unwrap();
                    let handle = Account::new_worker(
                        ref_folders[&folder_hash].clone(),
                        &mut self.backend,
                        &self.work_context,
                        self.notify_fn.clone(),
                    );
                    self.workers.insert(folder_hash, handle);
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
        self.folders.len()
    }
    pub fn is_empty(&self) -> bool {
        self.folders.is_empty()
    }
    pub fn list_folders(&self) -> Vec<Folder> {
        let mut folders = if let Ok(folders) = self.backend.read().unwrap().folders() {
            folders
        } else {
            return Vec::new();
        };
        let folder_confs = &self.folder_confs;
        //debug!("folder renames: {:?}", folder_renames);
        for f in folders.values_mut() {
            if let Some(r) = folder_confs.get(&f.hash()) {
                if let Some(rename) = r.folder_conf().alias() {
                    f.change_name(rename);
                }
            }
        }
        /*
        if let Some(pos) = folders
            .iter()
            .position(|f| f.name().eq_ignore_ascii_case("INBOX"))
        {
            folders.swap(pos, 0);
        }
        */
        let order: FnvHashMap<FolderHash, usize> = self
            .folders_order
            .iter()
            .enumerate()
            .map(|(i, &fh)| (fh, i))
            .collect();
        let mut folders: Vec<Folder> = folders
            .drain()
            .map(|(_, f)| f)
            .filter(|f| {
                self.folders.contains_key(&f.hash())
                    || self
                        .settings
                        .account
                        .subscribed_folders
                        .iter()
                        .any(|m| f.path().matches_glob(m))
            })
            .collect();
        if order.is_empty() {
            return Vec::new();
        }
        folders.sort_unstable_by(|a, b| order[&a.hash()].partial_cmp(&order[&b.hash()]).unwrap());
        folders
    }
    pub fn folders_order(&self) -> &Vec<FolderHash> {
        &self.folders_order
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn workers(&mut self) -> &mut FnvHashMap<FolderHash, Worker> {
        &mut self.workers
    }

    fn load_mailbox(&mut self, folder_hash: FolderHash, payload: Result<Vec<Envelope>>) {
        if payload.is_err() {
            self.folders
                .insert(folder_hash, MailboxEntry::Failed(payload.unwrap_err()));
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
        match self.folders.entry(folder_hash).or_default() {
            MailboxEntry::Failed(_) => {}
            MailboxEntry::Parsing(ref mut m, _, _) | MailboxEntry::Available(ref mut m) => {
                m.merge(&envelopes);
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
            }
        }
        self.sender
            .send(ThreadEvent::UIEvent(UIEvent::StartupCheck(folder_hash)))
            .unwrap();
    }

    pub fn status(&mut self, folder_hash: FolderHash) -> result::Result<(), usize> {
        loop {
            match self.workers.get_mut(&folder_hash).unwrap() {
                None => {
                    return if self.folders[&folder_hash].is_available()
                        || (self.folders[&folder_hash].is_parsing()
                            && self.collection.threads.contains_key(&folder_hash))
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
                        self.folders.entry(folder_hash).and_modify(|f| {
                            let m = if let MailboxEntry::Parsing(m, _, _) = f {
                                std::mem::replace(m, Mailbox::default())
                            } else {
                                return;
                            };
                            *f = MailboxEntry::Available(m);
                        });

                        self.workers.insert(folder_hash, None);
                    }
                    Ok(AsyncStatus::ProgressReport(n)) => {
                        self.folders.entry(folder_hash).and_modify(|f| {
                            if let MailboxEntry::Parsing(_, ref mut d, _) = f {
                                *d += n;
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
        if self.folders[&folder_hash].is_available()
            || (self.folders[&folder_hash].is_parsing()
                && self.collection.threads.contains_key(&folder_hash))
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
    pub fn iter_mailboxes(&self) -> MailboxIterator {
        MailboxIterator {
            folders_order: &self.folders_order,
            folders: &self.folders,
            pos: 0,
        }
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

    pub fn thread(&self, h: ThreadHash, f: FolderHash) -> &ThreadNode {
        &self.collection.threads[&f].thread_nodes()[&h]
    }

    pub fn folder_operation(&mut self, path: &str, op: FolderOperation) -> Result<()> {
        Err(MeliError::new("Not implemented."))
    }

    pub fn folder_confs(&self, folder_hash: FolderHash) -> &FileFolderConf {
        &self.folder_confs[&folder_hash]
    }

    pub fn sent_folder(&self) -> &str {
        let sent_folder = self
            .folder_confs
            .iter()
            .find(|(_, f)| f.folder_conf().usage == Some(SpecialUsageMailbox::Sent));
        if let Some(sent_folder) = sent_folder.as_ref() {
            &self.folder_names[&sent_folder.0]
        } else {
            ""
        }
    }

    pub fn special_use_folder(&self, special_use: SpecialUsageMailbox) -> Option<&str> {
        let ret = self
            .folder_confs
            .iter()
            .find(|(_, f)| f.folder_conf().usage == Some(special_use));
        if let Some(ret) = ret.as_ref() {
            Some(&self.folder_names[&ret.0])
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
    ) -> Result<StackVec<EnvelopeHash>> {
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
            let mut ret = StackVec::new();
            let envelopes = self.collection.envelopes.clone().read();
            let envelopes = envelopes.unwrap();

            for env_hash in self.folders[folder_hash].as_result()?.envelopes {
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
            ret
        }
    }
}

impl Index<FolderHash> for Account {
    type Output = MailboxEntry;
    fn index(&self, index: FolderHash) -> &MailboxEntry {
        &self.folders[&index]
    }
}

impl IndexMut<FolderHash> for Account {
    fn index_mut(&mut self, index: FolderHash) -> &mut MailboxEntry {
        self.folders.get_mut(&index).unwrap()
    }
}

impl Index<usize> for Account {
    type Output = MailboxEntry;
    fn index(&self, index: usize) -> &MailboxEntry {
        &self.folders[&self.folders_order[index]]
    }
}

/// Will panic if mailbox hasn't loaded, ask `status()` first.
impl IndexMut<usize> for Account {
    fn index_mut(&mut self, index: usize) -> &mut MailboxEntry {
        self.folders.get_mut(&self.folders_order[index]).unwrap()
    }
}
