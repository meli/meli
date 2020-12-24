/*
 * meli - mailbox module.
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

#[macro_use]
mod backend;
pub use self::backend::*;

mod stream;
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    fs,
    hash::{Hash, Hasher},
    io::{BufReader, Read},
    path::{Path, PathBuf},
    sync::{Arc, Mutex, RwLock},
};

use futures::stream::Stream;
use smallvec::SmallVec;
pub use stream::*;

use crate::{
    backends::*,
    email::Flag,
    error::{Error, Result},
    utils::shellexpand::ShellExpandTrait,
    AccountSettings, Envelope, EnvelopeHash,
};

/// `BackendOp` implementor for Maildir
#[derive(Debug)]
pub struct MaildirOp {
    hash_index: HashIndexes,
    mailbox_hash: MailboxHash,
    hash: EnvelopeHash,
}

impl Clone for MaildirOp {
    fn clone(&self) -> Self {
        Self {
            hash_index: self.hash_index.clone(),
            mailbox_hash: self.mailbox_hash,
            hash: self.hash,
        }
    }
}

impl MaildirOp {
    pub fn new(hash: EnvelopeHash, hash_index: HashIndexes, mailbox_hash: MailboxHash) -> Self {
        Self {
            hash_index,
            mailbox_hash,
            hash,
        }
    }
    fn path(&self) -> Result<PathBuf> {
        let map = self.hash_index.lock().unwrap();
        let map = &map[&self.mailbox_hash];
        debug!("looking for {} in {} map", self.hash, self.mailbox_hash);
        if !map.contains_key(&self.hash) {
            debug!("doesn't contain it though len = {}\n{:#?}", map.len(), map);
            for e in map.iter() {
                debug!("{:#?}", e);
            }
            return Err(Error::new("File not found"));
        }

        Ok(if let Some(modif) = &map[&self.hash].modified {
            match modif {
                PathMod::Path(ref path) => path.clone(),
                PathMod::Hash(hash) => map[hash].to_path_buf(),
            }
        } else {
            map.get(&self.hash).unwrap().to_path_buf()
        })
    }
}

impl BackendOp for MaildirOp {
    fn as_bytes(&self) -> ResultFuture<Vec<u8>> {
        let _self = self.clone();
        Ok(Box::pin(async move {
            smol::unblock(move || {
                let file = std::fs::OpenOptions::new()
                    .read(true)
                    .write(false)
                    .open(_self.path()?)?;
                let mut buf_reader = BufReader::new(file);
                let mut contents = Vec::new();
                buf_reader.read_to_end(&mut contents)?;
                Ok(contents)
            })
            .await
        }))
    }
}

#[derive(Clone, Debug, Default)]
pub struct MaildirMailbox {
    hash: MailboxHash,
    name: String,
    fs_path: PathBuf,
    path: PathBuf,
    parent: Option<MailboxHash>,
    children: Vec<MailboxHash>,
    pub usage: Arc<RwLock<SpecialUsageMailbox>>,
    pub is_subscribed: bool,
    permissions: MailboxPermissions,
    pub total: Arc<Mutex<usize>>,
    pub unseen: Arc<Mutex<usize>>,
}

impl MaildirMailbox {
    pub fn new(
        path: String,
        file_name: String,
        parent: Option<MailboxHash>,
        children: Vec<MailboxHash>,
        accept_invalid: bool,
        settings: &AccountSettings,
    ) -> Result<Self> {
        let pathbuf = PathBuf::from(&path).expand();
        let mut h = DefaultHasher::new();
        pathbuf.hash(&mut h);

        /* Check if mailbox path (Eg `INBOX/Lists/luddites`) is included in the
         * subscribed mailboxes in user configuration */
        let fname = pathbuf
            .strip_prefix(
                PathBuf::from(&settings.root_mailbox)
                    .expand()
                    .parent()
                    .unwrap_or_else(|| Path::new("/")),
            )
            .ok();

        let read_only = if let Ok(metadata) = std::fs::metadata(&pathbuf) {
            metadata.permissions().readonly()
        } else {
            true
        };

        let ret = Self {
            hash: MailboxHash(h.finish()),
            name: file_name,
            path: fname.unwrap().to_path_buf(),
            fs_path: pathbuf,
            parent,
            children,
            usage: Arc::new(RwLock::new(SpecialUsageMailbox::Normal)),
            is_subscribed: false,
            permissions: MailboxPermissions {
                create_messages: !read_only,
                remove_messages: !read_only,
                set_flags: !read_only,
                create_child: !read_only,
                rename_messages: !read_only,
                delete_messages: !read_only,
                delete_mailbox: !read_only,
                change_permissions: false,
            },
            unseen: Arc::new(Mutex::new(0)),
            total: Arc::new(Mutex::new(0)),
        };
        if !accept_invalid {
            ret.is_valid()?;
        }
        Ok(ret)
    }

    pub fn fs_path(&self) -> &Path {
        self.fs_path.as_path()
    }

    fn is_valid(&self) -> Result<()> {
        let path = self.fs_path();
        let mut p = PathBuf::from(path);
        for d in &["cur", "new", "tmp"] {
            p.push(d);
            if !p.is_dir() {
                return Err(Error::new(format!(
                    "{} is not a valid maildir mailbox",
                    path.display()
                )));
            }
            p.pop();
        }
        Ok(())
    }
}

impl BackendMailbox for MaildirMailbox {
    fn hash(&self) -> MailboxHash {
        self.hash
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn path(&self) -> &str {
        self.path.to_str().unwrap_or_else(|| self.name())
    }

    fn children(&self) -> &[MailboxHash] {
        &self.children
    }

    fn clone(&self) -> Mailbox {
        Box::new(std::clone::Clone::clone(self))
    }

    fn special_usage(&self) -> SpecialUsageMailbox {
        *self.usage.read().unwrap()
    }

    fn parent(&self) -> Option<MailboxHash> {
        self.parent
    }

    fn permissions(&self) -> MailboxPermissions {
        self.permissions
    }
    fn is_subscribed(&self) -> bool {
        self.is_subscribed
    }
    fn set_is_subscribed(&mut self, new_val: bool) -> Result<()> {
        self.is_subscribed = new_val;
        Ok(())
    }

    fn set_special_usage(&mut self, new_val: SpecialUsageMailbox) -> Result<()> {
        *self.usage.write()? = new_val;
        Ok(())
    }

    fn count(&self) -> Result<(usize, usize)> {
        Ok((*self.unseen.lock()?, *self.total.lock()?))
    }
}

pub trait MaildirPathTrait {
    fn flags(&self) -> Flag;
}

impl MaildirPathTrait for Path {
    fn flags(&self) -> Flag {
        let mut flag = Flag::default();
        let path = self.to_string_lossy();
        if !path.contains(":2,") {
            return flag;
        }

        for f in path.chars().rev() {
            match f {
                ',' => break,
                'D' => flag |= Flag::DRAFT,
                'F' => flag |= Flag::FLAGGED,
                'P' => flag |= Flag::PASSED,
                'R' => flag |= Flag::REPLIED,
                'S' => flag |= Flag::SEEN,
                'T' => flag |= Flag::TRASHED,
                _ => {
                    debug!(
                        "DEBUG: in MaildirPathTrait::flags(), encountered unknown flag marker \
                         {:?}, path is {}",
                        f, path
                    );
                }
            }
        }

        flag
    }
}
