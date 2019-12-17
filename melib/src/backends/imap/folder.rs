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
use crate::backends::{BackendFolder, Folder, FolderHash, FolderPermissions, SpecialUsageMailbox};
use crate::error::*;
use std::sync::{Arc, Mutex, RwLock};

#[derive(Debug, Default, Clone)]
pub struct ImapFolder {
    pub(super) hash: FolderHash,
    pub(super) path: String,
    pub(super) name: String,
    pub(super) parent: Option<FolderHash>,
    pub(super) children: Vec<FolderHash>,
    pub usage: Arc<RwLock<SpecialUsageMailbox>>,
    pub no_select: bool,
    pub is_subscribed: bool,

    pub permissions: Arc<Mutex<FolderPermissions>>,
    pub exists: Arc<Mutex<usize>>,
    pub unseen: Arc<Mutex<usize>>,
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
        Box::new(std::clone::Clone::clone(self))
    }

    fn special_usage(&self) -> SpecialUsageMailbox {
        *self.usage.read().unwrap()
    }

    fn parent(&self) -> Option<FolderHash> {
        self.parent
    }

    fn permissions(&self) -> FolderPermissions {
        *self.permissions.lock().unwrap()
    }
    fn is_subscribed(&self) -> bool {
        self.is_subscribed
    }
    fn set_is_subscribed(&mut self, new_val: bool) -> Result<()> {
        self.is_subscribed = new_val;
        // FIXME: imap subscribe
        Ok(())
    }

    fn set_special_usage(&mut self, new_val: SpecialUsageMailbox) -> Result<()> {
        *self.usage.write()? = new_val;
        Ok(())
    }

    fn count(&self) -> Result<(usize, usize)> {
        Ok((*self.unseen.lock()?, *self.exists.lock()?))
    }
}
