/*
 * meli - imap module.
 *
 * Copyright 2019 Manos Pitsidianakis
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
use crate::backends::{BackendFolder, Folder, FolderHash, FolderPermissions};
use std::sync::{Arc, Mutex};

#[derive(Debug, Default, Clone)]
pub struct ImapFolder {
    pub(super) hash: FolderHash,
    pub(super) path: String,
    pub(super) name: String,
    pub(super) parent: Option<FolderHash>,
    pub(super) children: Vec<FolderHash>,
    pub no_select: bool,

    pub permissions: Arc<Mutex<FolderPermissions>>,
    pub exists: Arc<Mutex<usize>>,
}

impl BackendFolder for ImapFolder {
    fn hash(&self) -> FolderHash {
        self.hash
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn path(&self) -> &str {
        &self.path
    }

    fn change_name(&mut self, s: &str) {
        self.name = s.to_string();
    }

    fn children(&self) -> &[FolderHash] {
        &self.children
    }

    fn clone(&self) -> Folder {
        Box::new(ImapFolder {
            hash: self.hash,
            path: self.path.clone(),
            name: self.name.clone(),
            parent: self.parent,
            children: self.children.clone(),
            no_select: self.no_select,
            permissions: self.permissions.clone(),
            exists: self.exists.clone(),
        })
    }

    fn parent(&self) -> Option<FolderHash> {
        self.parent
    }

    fn permissions(&self) -> FolderPermissions {
        *self.permissions.lock().unwrap()
    }
}
