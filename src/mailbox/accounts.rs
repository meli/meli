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
use conf::AccountSettings;
use std::ops::{Index, IndexMut};
#[derive(Debug)]
pub struct Account {
    name: String,
    folders: Vec<Option<Result<Mailbox>>>,

    sent_folder: Option<usize>,

    settings: AccountSettings,
}


impl Account {
    pub fn new(name: String, settings: AccountSettings) -> Self {
        eprintln!("new acc");
        let sent_folder = settings
            .folders
            .iter()
            .position(|x| *x == settings.sent_folder);
        let mut folders = Vec::with_capacity(settings.folders.len());
        for _ in 0..settings.folders.len() {
            folders.push(None);
        }
        Account {
            name: name,
            folders: folders,

            sent_folder: sent_folder,

            settings: settings,
        }
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
            eprintln!("building folder {:?}", self.settings.folders[index]);
            let path = self.settings.folders[index].clone();
            if self.sent_folder.is_some() {
                let id = self.sent_folder.unwrap();
                if id == index {
                    eprintln!("building sent_folder..");
                    self.folders[index] = Some(Mailbox::new(&path, &None));
                    eprintln!("Done!");
                } else {
                    eprintln!(
                        "Now building folder {:?} with sent_folder",
                        self.settings.folders[index]
                    );
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
                    let sent_path = self.settings.folders[id].clone();
                    if sent[0].is_none() {
                        eprintln!("\tbuilding sent_folder..");
                        sent[0] = Some(Mailbox::new(&sent_path, &None));
                        eprintln!("\tDone!");
                    }
                    cur[0] = Some(Mailbox::new(&path, &sent[0]));
                    eprintln!("Done!");
                }
            } else {
                eprintln!(
                    "Now building folder {:?} without sent_folder",
                    self.settings.folders[index]
                );
                self.folders[index] = Some(Mailbox::new(&path, &None));
                eprintln!("Done!");
            };
        }
        &mut self.folders[index]
    }
}
