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

use super::AccountConf;
use super::ToggleFlag;
use fnv::FnvHashMap;
use melib::async_workers::{Async, AsyncBuilder, AsyncStatus};
use melib::backends::{
    BackendOp, Backends, Folder, FolderHash, MailBackend, NotifyFn, ReadOnlyOp, RefreshEvent,
    RefreshEventConsumer, RefreshEventKind,
};
use melib::error::{MeliError, Result};
use melib::mailbox::*;
use melib::thread::ThreadHash;
use melib::AddressBook;
use melib::StackVec;

use crate::types::UIEvent::{self, EnvelopeRemove, EnvelopeRename, EnvelopeUpdate, Notification};
use std::collections::VecDeque;
use std::fs;
use std::io;
use std::mem;
use std::ops::{Index, IndexMut};
use std::result;
use std::sync::Arc;

pub type Worker = Option<Async<Result<(FnvHashMap<EnvelopeHash, Envelope>, Mailbox)>>>;

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
    Parsing(usize, usize),
    /// first argument is done work, and second is total work
    Threading(usize, usize),
}

impl std::fmt::Display for MailboxEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                MailboxEntry::Available(ref m) => m.name().to_string(),
                MailboxEntry::Failed(ref e) => e.to_string(),
                MailboxEntry::Parsing(done, total) => {
                    format!("Parsing messages. [{}/{}]", done, total)
                }
                MailboxEntry::Threading(done, total) => {
                    format!("Calculating threads. [{}/{}]", done, total)
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
        if let MailboxEntry::Parsing(_, _) = self {
            true
        } else {
            false
        }
    }
    pub fn unwrap_mut(&mut self) -> &mut Mailbox {
        match self {
            MailboxEntry::Available(ref mut m) => m,
            e => panic!(format!("mailbox is not available! {:#}", e)),
        }
    }
    pub fn unwrap(&self) -> &Mailbox {
        match self {
            MailboxEntry::Available(ref m) => m,
            e => panic!(format!("mailbox is not available! {:#}", e)),
        }
    }
}

#[derive(Debug)]
pub struct Account {
    name: String,
    pub(crate) folders: FnvHashMap<FolderHash, MailboxEntry>,
    pub(crate) folders_order: Vec<FolderHash>,
    folder_names: FnvHashMap<FolderHash, String>,
    tree: Vec<FolderNode>,
    sent_folder: Option<FolderHash>,
    pub(crate) collection: Collection,

    pub(crate) address_book: AddressBook,

    pub(crate) workers: FnvHashMap<FolderHash, Worker>,

    pub(crate) settings: AccountConf,
    pub(crate) runtime_settings: AccountConf,
    pub(crate) backend: Box<dyn MailBackend>,

    event_queue: VecDeque<(FolderHash, RefreshEvent)>,
    notify_fn: Arc<NotifyFn>,
}

impl Drop for Account {
    fn drop(&mut self) {
        //TODO: Avoid panics
        let data_dir = xdg::BaseDirectories::with_profile("meli", &self.name).unwrap();
        if let Ok(data) = data_dir.place_data_file("addressbook") {
            /* place result in cache directory */
            let f = match fs::File::create(data) {
                Ok(f) => f,
                Err(e) => {
                    panic!("{}", e);
                }
            };
            let writer = io::BufWriter::new(f);
            serde_json::to_writer(writer, &self.address_book).unwrap();
        };
        if let Ok(data) = data_dir.place_data_file("mailbox") {
            /* place result in cache directory */
            let f = match fs::File::create(data) {
                Ok(f) => f,
                Err(e) => {
                    panic!("{}", e);
                }
            };
            let writer = io::BufWriter::new(f);
            bincode::serialize_into(writer, &self.folders).unwrap();
        };
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
        name: String,
        mut settings: AccountConf,
        map: &Backends,
        notify_fn: NotifyFn,
    ) -> Self {
        let mut backend = map.get(settings.account().format())(settings.account());
        let mut ref_folders: FnvHashMap<FolderHash, Folder> = backend.folders();
        let mut folders: FnvHashMap<FolderHash, MailboxEntry> =
            FnvHashMap::with_capacity_and_hasher(ref_folders.len(), Default::default());
        let mut folders_order: Vec<FolderHash> = Vec::with_capacity(ref_folders.len());
        let mut workers: FnvHashMap<FolderHash, Worker> = FnvHashMap::default();
        let notify_fn = Arc::new(notify_fn);
        let mut folder_names = FnvHashMap::default();

        let mut sent_folder = None;
        for f in ref_folders.values_mut() {
            let entry = settings
                .folder_confs
                .entry(f.name().to_string())
                .or_default();
            if f.name().eq_ignore_ascii_case("sent") {
                sent_folder = Some(f.hash());
            }
            if (f.name().eq_ignore_ascii_case("junk")
                || f.name().eq_ignore_ascii_case("spam")
                || f.name().eq_ignore_ascii_case("sent")
                || f.name().eq_ignore_ascii_case("trash"))
                && entry.subscribe.is_unset()
            {
                entry.subscribe = ToggleFlag::InternalVal(false);
            }
            folder_names.insert(f.hash(), f.name().to_string());
        }

        let mut stack: StackVec<FolderHash> = StackVec::new();
        let mut tree: Vec<FolderNode> = Vec::new();
        for (h, f) in ref_folders.iter() {
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
            folders.insert(*h, MailboxEntry::Parsing(0, 0));
            workers.insert(
                *h,
                Account::new_worker(f.clone(), &mut backend, notify_fn.clone()),
            );
        }
        tree.sort_unstable_by_key(|f| ref_folders[&f.hash].name());

        let mut stack: StackVec<Option<&FolderNode>> = StackVec::new();
        for n in tree.iter_mut() {
            folders_order.push(n.hash);
            n.kids.sort_unstable_by_key(|f| ref_folders[&f.hash].name());
            stack.extend(n.kids.iter().rev().map(Some));
            while let Some(Some(next)) = stack.pop() {
                folders_order.push(next.hash);
                stack.extend(next.kids.iter().rev().map(Some));
            }
        }

        let data_dir = xdg::BaseDirectories::with_profile("meli", &name).unwrap();
        let address_book = if let Ok(data) = data_dir.place_data_file("addressbook") {
            if data.exists() {
                let reader = io::BufReader::new(fs::File::open(data).unwrap());
                let result: result::Result<AddressBook, _> = serde_json::from_reader(reader);
                if let Ok(data_t) = result {
                    data_t
                } else {
                    AddressBook::new(name.clone())
                }
            } else {
                AddressBook::new(name.clone())
            }
        } else {
            AddressBook::new(name.clone())
        };

        Account {
            name,
            folders,
            folders_order,
            folder_names,
            tree,
            address_book,
            sent_folder,
            collection: Collection::new(Default::default()),
            workers,
            settings: settings.clone(),
            runtime_settings: settings,
            backend,
            notify_fn,

            event_queue: VecDeque::with_capacity(8),
        }
    }
    fn new_worker(
        folder: Folder,
        backend: &mut Box<MailBackend>,
        notify_fn: Arc<NotifyFn>,
    ) -> Worker {
        let mailbox_handle = backend.get(&folder);
        let mut builder = AsyncBuilder::new();
        let tx = builder.tx();
        Some(builder.build(Box::new(move || {
            let mut handle = mailbox_handle.clone();
            let folder = folder.clone();
            let work = handle.work().unwrap();
            work.compute();
            handle.join();
            let envelopes: Result<FnvHashMap<EnvelopeHash, Envelope>> = handle.extract().map(|v| {
                v.into_iter()
                    .map(|e| (e.hash(), e))
                    .collect::<FnvHashMap<EnvelopeHash, Envelope>>()
            });
            let hash = folder.hash();
            if envelopes.is_err() {
                tx.send(AsyncStatus::Payload(Err(envelopes.unwrap_err())));
                notify_fn.notify(hash);
                return;
            }
            let envelopes = envelopes.unwrap();
            let m = Mailbox::new(folder, &envelopes);
            tx.send(AsyncStatus::Payload(Ok((envelopes, m))));
            notify_fn.notify(hash);
        })))
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
                    self.collection.update(old_hash, *envelope, folder_hash);
                    return Some(EnvelopeUpdate(old_hash));
                }
                RefreshEventKind::Rename(old_hash, new_hash) => {
                    debug!("rename {} to {}", old_hash, new_hash);
                    mailbox!(&folder_hash, self.folders).rename(old_hash, new_hash);
                    self.collection.rename(old_hash, new_hash, folder_hash);
                    return Some(EnvelopeRename(old_hash, new_hash));
                }
                RefreshEventKind::Create(envelope) => {
                    let env_hash = envelope.hash();
                    mailbox!(&folder_hash, self.folders).insert(env_hash);
                    self.collection.insert(*envelope, folder_hash);
                    if self
                        .sent_folder
                        .as_ref()
                        .map(|h| *h == folder_hash)
                        .unwrap_or(false)
                    {
                        self.collection.insert_reply(env_hash);
                    }

                    let ref_folders: FnvHashMap<FolderHash, Folder> = self.backend.folders();
                    let folder_conf = &self.settings.folder_confs[&self.folder_names[&folder_hash]];
                    if folder_conf.subscribe.is_false() {
                        return None;
                    }
                    let (_, thread_node) = self.mail_and_thread(env_hash, folder_hash);
                    if thread_node.snoozed() {
                        return None;
                    }
                    let env = self.get_env(&env_hash);
                    return Some(Notification(
                        Some("new mail".into()),
                        format!(
                            "{} {:.15}:\n\nFrom: {:.15}\nSubject: {:.15}",
                            self.name,
                            ref_folders[&folder_hash].name(),
                            env.subject(),
                            env.field_from_to_string(),
                        ),
                    ));
                }
                RefreshEventKind::Remove(envelope_hash) => {
                    mailbox!(&folder_hash, self.folders).remove(envelope_hash);
                    self.collection.remove(envelope_hash, folder_hash);
                    return Some(EnvelopeRemove(envelope_hash));
                }
                RefreshEventKind::Rescan => {
                    let ref_folders: FnvHashMap<FolderHash, Folder> = self.backend.folders();
                    let handle = Account::new_worker(
                        ref_folders[&folder_hash].clone(),
                        &mut self.backend,
                        self.notify_fn.clone(),
                    );
                    self.workers.insert(folder_hash, handle);
                }
            }
        }
        None
    }
    pub fn watch(&self, r: RefreshEventConsumer) {
        self.backend.watch(r).unwrap();
    }

    pub fn len(&self) -> usize {
        self.folders.len()
    }
    pub fn is_empty(&self) -> bool {
        self.folders.is_empty()
    }
    pub fn list_folders(&self) -> Vec<Folder> {
        let mut folders = self.backend.folders();
        if let Some(folder_confs) = self.settings.conf().folders() {
            //debug!("folder renames: {:?}", folder_renames);
            for f in folders.values_mut() {
                if let Some(r) = folder_confs.get(f.name()) {
                    if let Some(rename) = r.rename() {
                        f.change_name(rename);
                    }
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
        let mut folders: Vec<Folder> = folders.drain().map(|(_, f)| f).collect();
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

    fn load_mailbox(
        &mut self,
        folder_hash: FolderHash,
        payload: (Result<(FnvHashMap<EnvelopeHash, Envelope>, Mailbox)>),
    ) {
        if payload.is_err() {
            self.folders
                .insert(folder_hash, MailboxEntry::Failed(payload.unwrap_err()));
            return;
        }
        let (envelopes, mut mailbox) = payload.unwrap();
        self.collection
            .merge(envelopes, folder_hash, &mut mailbox, self.sent_folder);
        self.folders
            .insert(folder_hash, MailboxEntry::Available(mailbox));
    }

    pub fn status(&mut self, folder_hash: FolderHash) -> result::Result<(), usize> {
        match self.workers.get_mut(&folder_hash).unwrap() {
            None => {
                return Ok(());
            }
            Some(ref mut w) if self.folders[&folder_hash].is_parsing() => match w.poll() {
                Ok(AsyncStatus::NoUpdate) => {
                    return Err(0);
                }
                Ok(AsyncStatus::Finished) => {}
                Ok(AsyncStatus::ProgressReport(n)) => {
                    self.folders.entry(folder_hash).and_modify(|f| {
                        if let MailboxEntry::Parsing(ref mut d, _) = f {
                            *d += n;
                        }
                    });
                    return Err(n);
                }
                _ => {
                    return Err(0);
                }
            },
            Some(_) => return Ok(()),
        };
        let m = mem::replace(self.workers.get_mut(&folder_hash).unwrap(), None)
            .unwrap()
            .extract();
        self.workers.insert(folder_hash, None);
        self.load_mailbox(folder_hash, m);
        Ok(())
    }

    pub fn save_draft(&self, draft: Draft) -> Result<()> {
        if self.settings.account.read_only() {
            return Err(MeliError::new(format!(
                "Account {} is read-only.",
                self.name.as_str()
            )));
        }
        let finalize = draft.finalise()?;
        self.backend
            .save(&finalize.as_bytes(), &self.settings.conf.draft_folder)
    }
    pub fn save(&self, bytes: &[u8], folder: &str) -> Result<()> {
        if self.settings.account.read_only() {
            return Err(MeliError::new(format!(
                "Account {} is read-only.",
                self.name.as_str()
            )));
        }
        self.backend.save(bytes, folder)
    }
    pub fn iter_mailboxes(&self) -> MailboxIterator {
        MailboxIterator {
            folders_order: &self.folders_order,
            folders: &self.folders,
            pos: 0,
        }
    }

    pub fn get_env(&self, h: &EnvelopeHash) -> &Envelope {
        &self.collection[h]
    }
    pub fn get_env_mut(&mut self, h: &EnvelopeHash) -> &mut Envelope {
        self.collection.entry(*h).or_default()
    }
    pub fn contains_key(&self, h: EnvelopeHash) -> bool {
        self.collection.contains_key(&h)
    }
    pub fn operation(&self, h: EnvelopeHash) -> Box<BackendOp> {
        for mailbox in self.folders.values() {
            if let MailboxEntry::Available(ref m) = mailbox {
                if m.envelopes.contains(&h) {
                    let operation = self.backend.operation(h, m.folder.hash());
                    if self.settings.account.read_only() {
                        return ReadOnlyOp::new(operation);
                    } else {
                        return operation;
                    }
                }
            }
        }
        debug!("didn't find {}", h);
        debug!(&self.folders);
        debug!(&self.collection.envelopes);
        unreachable!()
    }
    pub fn thread_to_mail_mut(&mut self, h: ThreadHash, f: FolderHash) -> &mut Envelope {
        self.collection
            .envelopes
            .entry(self.collection.threads[&f].thread_to_mail(h))
            .or_default()
    }
    pub fn thread_to_mail(&self, h: ThreadHash, f: FolderHash) -> &Envelope {
        &self.collection.envelopes[&self.collection.threads[&f].thread_to_mail(h)]
    }
    pub fn threaded_mail(&self, h: ThreadHash, f: FolderHash) -> EnvelopeHash {
        self.collection.threads[&f].thread_to_mail(h)
    }
    pub fn mail_and_thread(
        &mut self,
        i: EnvelopeHash,
        f: FolderHash,
    ) -> (&mut Envelope, &ThreadNode) {
        let thread;
        {
            let x = &mut self.collection.envelopes.entry(i).or_default();
            thread = &self.collection.threads[&f][&x.thread()];
        }
        (self.collection.envelopes.entry(i).or_default(), thread)
    }
    pub fn thread(&self, h: ThreadHash, f: FolderHash) -> &ThreadNode {
        &self.collection.threads[&f].thread_nodes()[&h]
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
