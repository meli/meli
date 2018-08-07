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
use conf::{AccountSettings, Folder};
use mailbox::backends::{Backends, RefreshEventConsumer};
use mailbox::*;
use std::ops::{Index, IndexMut};
use std::result;

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
    pub fn new(name: String, settings: AccountSettings, backends: &Backends) -> Self {
        let sent_folder = settings
            .folders
            .iter()
            .position(|x| *x.path() == settings.sent_folder);
        let mut folders = Vec::with_capacity(settings.folders.len());
        let mut workers = Vec::new();
        let backend = backends.get(settings.format());
        for f in &settings.folders {
            folders.push(None);
            let mut handle = backend.get(&f);
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
    pub fn watch(&self, r: RefreshEventConsumer) -> () {
        self.backend.watch(r, &self.settings.folders[..]);
    }
    /* This doesn't represent the number of correctly parsed mailboxes though */
    pub fn len(&self) -> usize {
        self.folders.len()
    }
    pub fn list_folders(&self) -> Vec<Folder> {
        self.settings.folders.clone()
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn workers(&mut self) -> &mut Vec<Worker> {
        &mut self.workers
    }
    fn load_mailbox(&mut self, index: usize, envelopes: Result<Vec<Envelope>>) -> () {
        let folder = &self.settings.folders[index];
        if self.sent_folder.is_some() {
            let id = self.sent_folder.unwrap();
            if id == index {
                self.folders[index] = Some(Mailbox::new(folder, &None, envelopes));
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
                let sent_path = &self.settings.folders[id];
                if sent[0].is_none() {
                    sent[0] = Some(Mailbox::new(sent_path, &None, envelopes.clone()));
                }
                cur[0] = Some(Mailbox::new(folder, &sent[0], envelopes));
            }
        } else {
            self.folders[index] = Some(Mailbox::new(folder, &None, envelopes));
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
                    eprintln!("{:?}", a);
                    return Err(0);
                }
            },
        };
        let m = self.workers[index].take().unwrap().extract();
        self.load_mailbox(index, m);
        self.workers[index] = None;
        Ok(())
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
