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
use conf::AccountSettings;
use mailbox::backends::{Backends, RefreshEventConsumer};
use mailbox::*;
use std::ops::{Index, IndexMut};
use std::result;
use std::mem;

pub struct NewMailEvent {
    pub folder: u64,
    pub index: Vec<usize>,
}

pub type Worker = Option<Async<Result<Vec<Envelope>>>>;

#[derive(Debug)]
pub struct Account {
    name: String,
    folders: Vec<Option<Result<Mailbox>>>,

    pub workers: Vec<Worker>,

    sent_folder: Option<usize>,

    pub settings: AccountSettings,
    pub runtime_settings: AccountSettings,
    pub backend: Box<MailBackend>,
}

impl Account {
    pub fn new(name: String, settings: AccountSettings, map: &Backends) -> Self {
        let mut backend = map.get(settings.format())(&settings);
        let ref_folders: Vec<Folder> = backend.folders();
        let mut folders: Vec<Option<Result<Mailbox>>> = Vec::with_capacity(ref_folders.len());
        let mut workers: Vec<Worker> = Vec::new();
        let sent_folder = ref_folders
            .iter()
            .position(|x: &Folder| x.name() == settings.sent_folder);
        for f in ref_folders {
            folders.push(None);
            let handle = backend.get(&f);
            workers.push(Some(handle));
        }
        Account {
            name: name,
            folders: folders,
            workers: workers,

            sent_folder: sent_folder,

            settings: settings.clone(),
            runtime_settings: settings,
            backend: backend,
        }
    }
    pub fn reload(&mut self, idx: usize) {
        let ref_folders: Vec<Folder> = self.backend.folders();
        let handle = self.backend.get(&ref_folders[idx]);
        self.workers[idx] = Some(handle);
    }
    pub fn watch(&self, r: RefreshEventConsumer) -> () {
        self.backend.watch(r).unwrap();
    }
    /* This doesn't represent the number of correctly parsed mailboxes though */
    pub fn len(&self) -> usize {
        self.folders.len()
    }
    pub fn list_folders(&self) -> Vec<Folder> {
        self.backend.folders()
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn workers(&mut self) -> &mut Vec<Worker> {
        &mut self.workers
    }
    fn load_mailbox(&mut self, index: usize, envelopes: Result<Vec<Envelope>>) -> Option<NewMailEvent> {
        let mut ret: Option<NewMailEvent> = None;

        // TODO: Cleanup this function
        let folders = self.backend.folders();
        let folder = &folders[index];
        if self.sent_folder.is_some() {
            let id = self.sent_folder.unwrap();
            if id == index {
            /* ======================== */
                let old_m = mem::replace(&mut self.folders[index], Some(Mailbox::new(folder, &None, envelopes)));
                if let Some(old_m) = old_m {
                    if self.folders[index].is_some() && old_m.is_ok() {
                        let diff = self.folders[index].as_ref().map(|v| v.as_ref().unwrap().collection.len()).unwrap_or(0).saturating_sub(old_m.as_ref().map(|v| v.collection.len()).unwrap_or(0));
                        if diff > 0 {
                            let mut index = old_m.as_ref().unwrap().collection.iter().zip(&self.folders[index].as_ref().unwrap().as_ref().unwrap().collection).count();
                            ret = Some(NewMailEvent {
                                folder: folder.hash(),
                                index: (index.saturating_sub(1)..(self.folders[index].as_ref().unwrap().as_ref().unwrap().collection.len().saturating_sub(1))).collect(),
                            });

                        }

                    }
                }
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
                let old_m = mem::replace(&mut cur[0], Some(Mailbox::new(folder, &sent[0], envelopes)));
                if let Some(old_m) = old_m {
                    if cur[0].is_some() && old_m.is_ok() {
                        let diff = cur[0].as_ref().map(|v| v.as_ref().unwrap().collection.len()).unwrap_or(0).saturating_sub(old_m.as_ref().map(|v| v.collection.len()).unwrap_or(0));
                        if diff > 0 {
                            let mut index = old_m.as_ref().unwrap().collection.iter().zip(&cur[0].as_ref().unwrap().as_ref().unwrap().collection).count();
                            ret = Some(NewMailEvent {
                                folder: folder.hash(),
                                index: (index.saturating_sub(1)..(cur[0].as_ref().unwrap().as_ref().unwrap().collection.len()).saturating_sub(1)).collect(),
                            });

                        }

                    }
                }
            /* ======================== */
            }
        } else {
            /* ======================== */
                let old_m = mem::replace(&mut self.folders[index], Some(Mailbox::new(folder, &None, envelopes)));
                if let Some(old_m) = old_m {
                    if self.folders[index].is_some() && old_m.is_ok() {
                        let diff = self.folders[index].as_ref().map(|v| v.as_ref().unwrap().collection.len()).unwrap_or(0).saturating_sub(old_m.as_ref().map(|v| v.collection.len()).unwrap_or(0));
                        if diff > 0 {
                            let mut index = old_m.as_ref().unwrap().collection.iter().zip(&self.folders[index].as_ref().unwrap().as_ref().unwrap().collection).count();
                            ret = Some(NewMailEvent {
                                folder: folder.hash(),
                                index: (index.saturating_sub(1)..(self.folders[index].as_ref().unwrap().as_ref().unwrap().collection.len().saturating_sub(1))).collect(),
                            });

                        }

                    }
                }
            /* ======================== */
        };

        ret
    }

    pub fn status(&mut self, index: usize) -> result::Result<Option<NewMailEvent>, usize> {
        match self.workers[index].as_mut() {
            None => {
                return Ok(None);
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
                    eprintln!("{:?}", a);
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
