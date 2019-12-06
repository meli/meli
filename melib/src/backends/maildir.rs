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

use crate::backends::*;
use crate::email::{Envelope, Flag};
use crate::error::{MeliError, Result};
use crate::shellexpand::ShellExpandTrait;

use memmap::{Mmap, Protection};
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

/// `BackendOp` implementor for Maildir
#[derive(Debug)]
pub struct MaildirOp {
    hash_index: HashIndexes,
    folder_hash: FolderHash,
    hash: EnvelopeHash,
    slice: Option<Mmap>,
}

impl Clone for MaildirOp {
    fn clone(&self) -> Self {
        MaildirOp {
            hash_index: self.hash_index.clone(),
            folder_hash: self.folder_hash,
            hash: self.hash,
            slice: None,
        }
    }
}

impl MaildirOp {
    pub fn new(hash: EnvelopeHash, hash_index: HashIndexes, folder_hash: FolderHash) -> Self {
        MaildirOp {
            hash_index,
            folder_hash,
            hash,
            slice: None,
        }
    }
    fn path(&self) -> PathBuf {
        let map = self.hash_index.lock().unwrap();
        let map = &map[&self.folder_hash];
        debug!("looking for {} in {} map", self.hash, self.folder_hash);
        if !map.contains_key(&self.hash) {
            debug!("doesn't contain it though len = {}\n{:#?}", map.len(), map);
            for e in map.iter() {
                debug!("{:#?}", e);
            }
        }
        if let Some(modif) = &map[&self.hash].modified {
            match modif {
                PathMod::Path(ref path) => path.clone(),
                PathMod::Hash(hash) => map[&hash].to_path_buf(),
            }
        } else {
            map.get(&self.hash).unwrap().to_path_buf()
        }
    }
}

impl<'a> BackendOp for MaildirOp {
    fn description(&self) -> String {
        format!("Path of file: {}", self.path().display())
    }
    fn as_bytes(&mut self) -> Result<&[u8]> {
        if self.slice.is_none() {
            self.slice = Some(Mmap::open_path(self.path(), Protection::Read)?);
        }
        /* Unwrap is safe since we use ? above. */
        Ok(unsafe { self.slice.as_ref().unwrap().as_slice() })
    }
    fn fetch_flags(&self) -> Flag {
        let mut flag = Flag::default();
        let path = self.path();
        let path = path.to_str().unwrap(); // Assume UTF-8 validity
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
                    debug!("DEBUG: in fetch_flags, path is {}", path);
                }
            }
        }

        flag
    }

    fn set_flag(&mut self, envelope: &mut Envelope, f: Flag, value: bool) -> Result<()> {
        let path = self.path();
        let path = path.to_str().unwrap(); // Assume UTF-8 validity
        let idx: usize = path
            .rfind(":2,")
            .ok_or_else(|| MeliError::new(format!("Invalid email filename: {:?}", self)))?
            + 3;
        let mut new_name: String = path[..idx].to_string();
        let mut flags = self.fetch_flags();
        flags.set(f, value);

        if !(flags & Flag::DRAFT).is_empty() {
            new_name.push('D');
        }
        if !(flags & Flag::FLAGGED).is_empty() {
            new_name.push('F');
        }
        if !(flags & Flag::PASSED).is_empty() {
            new_name.push('P');
        }
        if !(flags & Flag::REPLIED).is_empty() {
            new_name.push('R');
        }
        if !(flags & Flag::SEEN).is_empty() {
            new_name.push('S');
        }
        if !(flags & Flag::TRASHED).is_empty() {
            new_name.push('T');
        }
        let old_hash = envelope.hash();
        let new_name: PathBuf = new_name.into();
        let hash_index = self.hash_index.clone();
        let mut map = hash_index.lock().unwrap();
        let map = map.entry(self.folder_hash).or_default();
        map.entry(old_hash).or_default().modified = Some(PathMod::Path(new_name.clone()));

        debug!("renaming {:?} to {:?}", path, new_name);
        fs::rename(&path, &new_name)?;
        debug!("success in rename");
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct MaildirFolder {
    hash: FolderHash,
    name: String,
    fs_path: PathBuf,
    path: PathBuf,
    parent: Option<FolderHash>,
    children: Vec<FolderHash>,
    permissions: FolderPermissions,
}

impl MaildirFolder {
    pub fn new(
        path: String,
        file_name: String,
        parent: Option<FolderHash>,
        children: Vec<FolderHash>,
        settings: &AccountSettings,
    ) -> Result<Self> {
        macro_rules! strip_slash {
            ($v:expr) => {
                if $v.ends_with("/") {
                    &$v[..$v.len() - 1]
                } else {
                    $v
                }
            };
        }
        let pathbuf = PathBuf::from(&path);
        let mut h = DefaultHasher::new();
        pathbuf.hash(&mut h);

        /* Check if folder path (Eg `INBOX/Lists/luddites`) is included in the subscribed
         * mailboxes in user configuration */
        let fname = if let Ok(fname) = pathbuf.strip_prefix(
            PathBuf::from(&settings.root_folder)
                .expand()
                .parent()
                .unwrap_or_else(|| &Path::new("/")),
        ) {
            if fname.components().count() != 0
                && !settings
                    .subscribed_folders
                    .iter()
                    .any(|x| x == strip_slash!(fname.to_str().unwrap()))
            {
                return Err(MeliError::new(format!(
                    "Folder with name `{}` is not included in configured subscribed mailboxes",
                    fname.display()
                )));
            }
            Some(fname)
        } else {
            None
        };

        let read_only = if let Ok(metadata) = std::fs::metadata(&pathbuf) {
            metadata.permissions().readonly()
        } else {
            true
        };

        let ret = MaildirFolder {
            hash: h.finish(),
            name: file_name,
            path: fname.unwrap().to_path_buf(),
            fs_path: pathbuf,
            parent,
            children,
            permissions: FolderPermissions {
                create_messages: !read_only,
                remove_messages: !read_only,
                set_flags: !read_only,
                create_child: !read_only,
                rename_messages: !read_only,
                delete_messages: !read_only,
                delete_mailbox: !read_only,
                change_permissions: false,
            },
        };
        ret.is_valid()?;
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
                return Err(MeliError::new(format!(
                    "{} is not a valid maildir folder",
                    path.display()
                )));
            }
            p.pop();
        }
        Ok(())
    }
}
impl BackendFolder for MaildirFolder {
    fn hash(&self) -> FolderHash {
        self.hash
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn path(&self) -> &str {
        self.path.to_str().unwrap_or(self.name())
    }

    fn change_name(&mut self, s: &str) {
        self.name = s.to_string();
    }

    fn children(&self) -> &[FolderHash] {
        &self.children
    }

    fn clone(&self) -> Folder {
        Box::new(MaildirFolder {
            hash: self.hash,
            name: self.name.clone(),
            fs_path: self.fs_path.clone(),
            path: self.path.clone(),
            children: self.children.clone(),
            parent: self.parent,
            permissions: self.permissions,
        })
    }

    fn parent(&self) -> Option<FolderHash> {
        self.parent
    }

    fn permissions(&self) -> FolderPermissions {
        self.permissions
    }
}
