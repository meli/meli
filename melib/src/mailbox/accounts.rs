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

use mailbox::*;
use mailbox::backends::{RefreshEventConsumer, Backends};
use conf::{AccountSettings, Folder};
use std::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct Account {
    name: String,
    folders: Vec<Option<Result<Mailbox>>>,

    sent_folder: Option<usize>,

    pub settings: AccountSettings,
    pub backend: Box<MailBackend>,
}


impl Account {
    pub fn new(name: String, settings: AccountSettings, backends: &Backends) -> Self {
        let sent_folder = settings
            .folders
            .iter()
            .position(|x| *x.path() == settings.sent_folder);
        let mut folders = Vec::with_capacity(settings.folders.len());
        for _ in 0..settings.folders.len() {
            folders.push(None);
        }
        let backend = backends.get(settings.format());
        Account {
            name: name,
            folders: folders,

            sent_folder: sent_folder,

            settings: settings,
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
}

impl Index<usize> for Account {
    type Output = Option<Result<Mailbox>>;
    fn index(&self, index: usize) -> &Option<Result<Mailbox>> {
        &self.folders[index]
    }
}

impl IndexMut<usize> for Account {
    fn index_mut(&mut self, index: usize) -> &mut Option<Result<Mailbox>> {
        if self.folders[index].is_none() {
            let folder = &self.settings.folders[index];
            let path = folder.path().clone();
            if self.sent_folder.is_some() {
                let id = self.sent_folder.unwrap();
                if id == index {
                    self.folders[index] = Some(Mailbox::new(folder, &None, self.backend.get(&folder)));
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
                        sent[0] = Some(Mailbox::new(sent_path, &None, self.backend.get(&folder)));
                    }
                    cur[0] = Some(Mailbox::new(folder, &sent[0],self.backend.get(folder)));
                }
            } else {
                self.folders[index] = Some(Mailbox::new(folder, &None,self.backend.get(&folder)));
            };
        }
        &mut self.folders[index]
    }
}
