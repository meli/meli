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
use crate::StackVec;
use fnv::FnvHashMap;
use melib::async_workers::{Async, AsyncBuilder, AsyncStatus};
use melib::backends::FolderHash;
use melib::error::Result;
use melib::mailbox::backends::{
    Backends, Folder, MailBackend, NotifyFn, RefreshEvent, RefreshEventConsumer, RefreshEventKind,
};
use melib::mailbox::*;
use melib::AddressBook;

use std::fs;
use std::io;
use std::mem;
use std::ops::{Index, IndexMut};
use std::result;
use std::sync::Arc;
use types::UIEvent::{self, EnvelopeRemove, EnvelopeRename, EnvelopeUpdate, Notification};

pub type Worker = Option<Async<Result<Mailbox>>>;

macro_rules! mailbox {
    ($idx:expr, $folders:expr) => {
        $folders
            .get_mut(&$idx)
            .unwrap()
            .as_mut()
            .unwrap()
            .as_mut()
            .unwrap()
    };
}

#[derive(Debug)]
pub struct Account {
    name: String,
    pub(crate) folders: FnvHashMap<FolderHash, Option<Result<Mailbox>>>,
    pub(crate) folders_order: Vec<FolderHash>,
    tree: Vec<FolderNode>,
    sent_folder: Option<usize>,

    pub(crate) address_book: AddressBook,

    pub(crate) workers: FnvHashMap<FolderHash, Worker>,

    pub(crate) settings: AccountConf,
    pub(crate) runtime_settings: AccountConf,
    pub(crate) backend: Box<dyn MailBackend>,
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
    }
}

pub struct MailboxIterator<'a> {
    folders_order: &'a [FolderHash],
    folders: &'a FnvHashMap<FolderHash, Option<Result<Mailbox>>>,
    pos: usize,
}

impl<'a> Iterator for MailboxIterator<'a> {
    type Item = Option<&'a Mailbox>;

    fn next(&mut self) -> Option<Option<&'a Mailbox>> {
        if self.pos == self.folders.len() {
            return None;
        }
        for fh in self.folders_order[self.pos..].iter() {
            if self.pos == self.folders.len() {
                return None;
            }

            self.pos += 1;
            if self.folders[&fh].is_none() {
                return Some(None);
            }
            if let Some(Err(_)) = self.folders[&fh] {
                return Some(None);
            }
            return Some(Some(self.folders[&fh].as_ref().unwrap().as_ref().unwrap()));
        }
        return None;
    }
}

#[derive(Debug, Default)]
struct FolderNode {
    hash: FolderHash,
    kids: Vec<FolderNode>,
}

impl Account {
    pub fn new(name: String, settings: AccountConf, map: &Backends, notify_fn: NotifyFn) -> Self {
        let mut backend = map.get(settings.account().format())(settings.account());
        let mut ref_folders: FnvHashMap<FolderHash, Folder> = backend.folders();
        let mut folders: FnvHashMap<FolderHash, Option<Result<Mailbox>>> =
            FnvHashMap::with_capacity_and_hasher(ref_folders.len(), Default::default());
        let mut folders_order: Vec<FolderHash> = Vec::with_capacity(ref_folders.len());
        let mut workers: FnvHashMap<FolderHash, Worker> = FnvHashMap::default();
        let notify_fn = Arc::new(notify_fn);

        if let Some(folder_confs) = settings.conf().folders() {
            //debug!("folder renames: {:?}", folder_renames);
            for f in ref_folders.values_mut() {
                if let Some(r) = folder_confs.get(&f.name().to_ascii_lowercase()) {
                    if let Some(rename) = r.rename() {
                        f.change_name(rename);
                    }
                }
            }
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
                    let k = FolderNode {
                        hash: c,
                        kids: Vec::new(),
                    };
                    stack.push(c);
                }
                while let Some(next) = stack.pop() {
                    for c in ref_folders[&next].children() {
                        stack.push(*c);
                    }
                }
            }
            folders.insert(*h, None);
            workers.insert(
                *h,
                Account::new_worker(f.clone(), &mut backend, notify_fn.clone()),
            );
        }
        tree.sort_unstable_by_key(|f| ref_folders[&f.hash].name());
        {
            //FIXME: NLL
            let mut stack: StackVec<Option<&FolderNode>> = StackVec::new();
            for n in tree.iter_mut() {
                folders_order.push(n.hash);
                n.kids.sort_unstable_by_key(|f| ref_folders[&f.hash].name());
                stack.extend(n.kids.iter().rev().map(|r| Some(r)));
                while let Some(Some(next)) = stack.pop() {
                    folders_order.push(next.hash);
                    stack.extend(next.kids.iter().rev().map(|r| Some(r)));
                }
            }
        }

        let data_dir = xdg::BaseDirectories::with_profile("meli", &name).unwrap();
        let address_book = if let Ok(data) = data_dir.place_data_file("addressbook") {
            if data.exists() {
                let reader = io::BufReader::new(fs::File::open(data).unwrap());
                let result: result::Result<AddressBook, _> = serde_json::from_reader(reader);
                if let Ok(mut data_t) = result {
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
            tree,
            address_book,
            sent_folder: None,
            workers,
            settings: settings.clone(),
            runtime_settings: settings,
            backend,
            notify_fn,
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
            let envelopes = handle.extract();
            let hash = folder.hash();
            let ret = Mailbox::new(folder, envelopes);
            tx.send(AsyncStatus::Payload(ret));
            notify_fn.notify(hash);
        })))
    }
    pub fn reload(&mut self, event: RefreshEvent, folder_hash: FolderHash) -> Option<UIEvent> {
        let kind = event.kind();
        {
            //let mailbox: &mut Mailbox = self.folders[idx].as_mut().unwrap().as_mut().unwrap();
            match kind {
                RefreshEventKind::Update(old_hash, envelope) => {
                    mailbox!(&folder_hash, self.folders).update(old_hash, *envelope);
                    return Some(EnvelopeUpdate(old_hash));
                }
                RefreshEventKind::Rename(old_hash, new_hash) => {
                    debug!("rename {} to {}", old_hash, new_hash);
                    let mailbox = mailbox!(&folder_hash, self.folders);
                    mailbox.rename(old_hash, new_hash);
                    return Some(EnvelopeRename(mailbox.folder.hash(), old_hash, new_hash));
                }
                RefreshEventKind::Create(envelope) => {
                    debug!("create {}", envelope.hash());
                    let env: &Envelope = mailbox!(&folder_hash, self.folders).insert(*envelope);
                    let ref_folders: FnvHashMap<FolderHash, Folder> = self.backend.folders();
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
    /* This doesn't represent the number of correctly parsed mailboxes though */
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
                if let Some(r) = folder_confs.get(&f.name().to_ascii_lowercase()) {
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

    fn load_mailbox(&mut self, folder_hash: FolderHash, mailbox: Result<Mailbox>) {
        self.folders.insert(folder_hash, Some(mailbox));
        /*
        if self.sent_folder.is_some() && self.sent_folder.unwrap() == index {
            self.folders[index] = Some(mailbox);
            /* Add our replies to other folders */
        for id in (0..self.folders.len()).filter(|i| *i != index) {
        self.add_replies_to_folder(id);
        }
        } else {
        self.folders[index] = Some(mailbox);
        self.add_replies_to_folder(index);
        };
         */
    }

    /*
    fn add_replies_to_folder(&mut self, folder_index: usize) {
        if let Some(sent_index) = self.sent_folder.as_ref() {
            if self.folders[*sent_index]
                .as_ref()
                .map(|v| v.is_ok())
                .unwrap_or(false)
                && self.folders[folder_index]
                    .as_ref()
                    .map(|v| v.is_ok())
                    .unwrap_or(false)
            {
                let (sent, cur) = {
                    let ptr = self.folders.as_mut_ptr();
                    unsafe {
                        use std::slice::from_raw_parts_mut;
                        (
                            from_raw_parts_mut(ptr.offset(*sent_index as isize), *sent_index + 1)
                                [0].as_mut()
                            .unwrap()
                            .as_mut()
                            .unwrap(),
                            from_raw_parts_mut(ptr.offset(folder_index as isize), folder_index + 1)
                                [0].as_mut()
                            .unwrap()
                            .as_mut()
                            .unwrap(),
                        )
                    }
                };
                cur.insert_sent_folder(&sent);
            }
        }
    }
    */

    pub fn status(&mut self, folder_hash: FolderHash) -> result::Result<(), usize> {
        match self.workers.get_mut(&folder_hash).unwrap() {
            None => {
                return Ok(());
            }
            Some(ref mut w) if self.folders[&folder_hash].is_none() => match w.poll() {
                Ok(AsyncStatus::NoUpdate) => {
                    return Err(0);
                }
                Ok(AsyncStatus::Finished) => {}
                Ok(AsyncStatus::ProgressReport(n)) => {
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
        let finalize = draft.finalise()?;
        self.backend
            .save(&finalize.as_bytes(), &self.settings.conf.draft_folder)
    }
    pub fn save(&self, bytes: &[u8], folder: &str) -> Result<()> {
        self.backend.save(bytes, folder)
    }
    pub fn iter_mailboxes<'a>(&'a self) -> MailboxIterator<'a> {
        MailboxIterator {
            folders_order: &self.folders_order,
            folders: &self.folders,
            pos: 0,
        }
    }
}
impl Index<FolderHash> for Account {
    type Output = Result<Mailbox>;
    fn index(&self, index: FolderHash) -> &Result<Mailbox> {
        &self.folders[&index]
            .as_ref()
            .expect("BUG: Requested mailbox that is not yet available.")
    }
}

/// Will panic if mailbox hasn't loaded, ask `status()` first.
impl IndexMut<FolderHash> for Account {
    fn index_mut(&mut self, index: FolderHash) -> &mut Result<Mailbox> {
        self.folders
            .get_mut(&index)
            .unwrap()
            .as_mut()
            .expect("BUG: Requested mailbox that is not yet available.")
    }
}

impl Index<usize> for Account {
    type Output = Result<Mailbox>;
    fn index(&self, index: usize) -> &Result<Mailbox> {
        &self.folders[&self.folders_order[index]]
            .as_ref()
            .expect("BUG: Requested mailbox that is not yet available.")
    }
}

/// Will panic if mailbox hasn't loaded, ask `status()` first.
impl IndexMut<usize> for Account {
    fn index_mut(&mut self, index: usize) -> &mut Result<Mailbox> {
        self.folders
            .get_mut(&self.folders_order[index])
            .unwrap()
            .as_mut()
            .expect("BUG: Requested mailbox that is not yet available.")
    }
}
