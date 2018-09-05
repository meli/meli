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

use async::*;
use conf::AccountConf;
use mailbox::backends::{
    Backends, Folder, MailBackend, RefreshEvent, RefreshEventConsumer, RefreshEventKind,
};
use mailbox::*;
use melib::error::Result;
use std::ops::{Index, IndexMut};
use std::result;
use types::UIEventType::{self, Notification};

pub type Worker = Option<Async<Result<Vec<Envelope>>>>;

#[derive(Debug)]
pub struct Account {
    name: String,
    folders: Vec<Option<Result<Mailbox>>>,

    pub workers: Vec<Worker>,

    sent_folder: Option<usize>,

    pub settings: AccountConf,
    pub runtime_settings: AccountConf,
    pub backend: Box<MailBackend>,
}

impl Account {
    pub fn new(name: String, settings: AccountConf, map: &Backends) -> Self {
        let mut backend = map.get(settings.account().format())(settings.account());
        let ref_folders: Vec<Folder> = backend.folders();
        let mut folders: Vec<Option<Result<Mailbox>>> = Vec::with_capacity(ref_folders.len());
        let mut workers: Vec<Worker> = Vec::new();
        let sent_folder = ref_folders
            .iter()
            .position(|x: &Folder| x.name() == settings.account().sent_folder);
        for f in ref_folders {
            folders.push(None);
            let handle = backend.get(&f);
            workers.push(Some(handle));
        }
        Account {
            name,
            folders,
            workers,
            sent_folder,
            settings: settings.clone(),
            runtime_settings: settings,
            backend,
        }
    }
    pub fn reload(&mut self, event: RefreshEvent, idx: usize) -> Option<UIEventType> {
        let kind = event.kind();
        let mailbox: &mut Mailbox = self.folders[idx].as_mut().unwrap().as_mut().unwrap();
        match kind {
            RefreshEventKind::Update(old_hash, envelope) => {
                mailbox.update(old_hash, envelope);
            }
            RefreshEventKind::Create(envelope) => {
                let env: &Envelope = mailbox.insert(envelope);
                let ref_folders: Vec<Folder> = self.backend.folders();
                return Some(Notification(
                    Some("New mail".into()),
                    format!(
                        "Subject: {:15}:\n{:15}\nFrom: {:15}",
                        ref_folders[idx].name(),
                        env.subject(),
                        env.field_from_to_string()
                    ),
                ));
            }
            RefreshEventKind::Remove(envelope_hash) => {
                mailbox.remove(envelope_hash);
            }
            RefreshEventKind::Rescan => {
                let ref_folders: Vec<Folder> = self.backend.folders();
                let handle = self.backend.get(&ref_folders[idx]);
                self.workers[idx] = Some(handle);
            }
        }
        return None;
    }
    pub fn watch(&self, r: RefreshEventConsumer) -> () {
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
            //eprintln!("folder renames: {:?}", folder_renames);
            for f in &mut folders {
                if let Some(r) = folder_confs.get(&f.name().to_ascii_lowercase()) {
                    if let Some(rename) = r.rename() {
                        f.change_name(rename);
                    }
                }
            }
        }
        folders
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn workers(&mut self) -> &mut Vec<Worker> {
        &mut self.workers
    }

    fn load_mailbox(&mut self, index: usize, envelopes: Result<Vec<Envelope>>) {
        // TODO: Cleanup this function
        let folders = self.backend.folders();
        let folder = &folders[index];
        if self.sent_folder.is_some() {
            let id = self.sent_folder.unwrap();
            if id == index {
                /* ======================== */
                self.folders[index] = Some(Mailbox::new(folder, &None, envelopes));
            /* ======================== */
            } else {
                let (sent, cur) = {
                    let ptr = self.folders.as_mut_ptr();
                    unsafe {
                        use std::slice::from_raw_parts_mut;
                        (
                            from_raw_parts_mut(ptr.offset(id as isize), id + 1),
                            from_raw_parts_mut(ptr.offset(index as isize), index + 1),
                        )
                    }
                };
                let sent_path = &folders[id];
                if sent[0].is_none() {
                    sent[0] = Some(Mailbox::new(sent_path, &None, envelopes.clone()));
                }
                /* ======================== */
                cur[0] = Some(Mailbox::new(folder, &sent[0], envelopes));
                /* ======================== */
            }
        } else {
            /* ======================== */
            self.folders[index] = Some(Mailbox::new(folder, &None, envelopes));
            /* ======================== */
        };
    }

    pub fn status(&mut self, index: usize) -> result::Result<(), usize> {
        match self.workers[index].as_mut() {
            None => {
                return Ok(());
            }
            Some(ref mut w) => match w.poll() {
                Ok(AsyncStatus::NoUpdate) => {
                    return Err(0);
                }
                Ok(AsyncStatus::Finished) => {}
                Ok(AsyncStatus::ProgressReport(n)) => {
                    return Err(n);
                }
                a => {
                    eprintln!("Error: {:?}", a);
                    return Err(0);
                }
            },
        };
        let m = self.workers[index].take().unwrap().extract();
        self.workers[index] = None;
        Ok(self.load_mailbox(index, m))
    }
}

impl Index<usize> for Account {
    type Output = Result<Mailbox>;
    fn index(&self, index: usize) -> &Result<Mailbox> {
        &self.folders[index]
            .as_ref()
            .expect("BUG: Requested mailbox that is not yet available.")
    }
}

/// Will panic if mailbox hasn't loaded, ask `status()` first.
impl IndexMut<usize> for Account {
    fn index_mut(&mut self, index: usize) -> &mut Result<Mailbox> {
        self.folders[index]
            .as_mut()
            .expect("BUG: Requested mailbox that is not yet available.")
    }
}
