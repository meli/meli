/*
 * meli - jmap module.
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

use super::*;
use crate::backends::{FolderPermissions, SpecialUsageMailbox};

#[derive(Debug, Clone)]
pub struct JmapFolder {
    pub name: String,
    pub path: String,
    pub hash: FolderHash,
    pub v: Vec<FolderHash>,
    pub id: String,
    pub is_subscribed: bool,
    pub my_rights: JmapRights,
    pub parent_id: Option<String>,
    pub role: Option<String>,
    pub sort_order: u64,
    pub total_emails: u64,
    pub total_threads: u64,
    pub unread_emails: u64,
    pub unread_threads: u64,
    pub usage: SpecialUsageMailbox,
}

impl BackendFolder for JmapFolder {
    fn hash(&self) -> FolderHash {
        self.hash
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn path(&self) -> &str {
        &self.path
    }

    fn change_name(&mut self, _s: &str) {}

    fn clone(&self) -> Folder {
        Box::new(std::clone::Clone::clone(self))
    }

    fn children(&self) -> &[FolderHash] {
        &self.v
    }

    fn parent(&self) -> Option<FolderHash> {
        None
    }

    fn permissions(&self) -> FolderPermissions {
        FolderPermissions::default()
    }

    fn special_usage(&self) -> SpecialUsageMailbox {
        self.usage
    }
}
