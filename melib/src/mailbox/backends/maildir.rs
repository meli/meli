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

extern crate fnv;

mod backend;
pub use self::backend::*;

use error::{MeliError, Result};
use mailbox::backends::*;
use mailbox::email::parser;
use mailbox::email::{Envelope, Flag};

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
        eprintln!("looking for {} in {} map", self.hash, self.folder_hash);
        if !map.contains_key(&self.hash) {
            eprintln!("doesn't contain it though len = {}\n{:#?}", map.len(), map);
            for e in map.iter() {
                eprintln!("{:#?}", e);
            }
        }
        map.get(&self.hash).unwrap().clone()
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
    fn fetch_headers(&mut self) -> Result<&[u8]> {
        let raw = self.as_bytes()?;
        let result = parser::headers_raw(raw).to_full_result()?;
        Ok(result)
    }
    fn fetch_body(&mut self) -> Result<&[u8]> {
        let raw = self.as_bytes()?;
        let result = parser::headers_raw(raw).to_full_result()?;
        Ok(result)
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
                _ => eprintln!("DEBUG: in fetch_flags, path is {}", path),
            }
        }

        flag
    }

    fn set_flag(&mut self, envelope: &mut Envelope, f: Flag) -> Result<()> {
        let path = self.path();
        let path = path.to_str().unwrap(); // Assume UTF-8 validity
        let idx: usize = path
            .rfind(":2,")
            .ok_or_else(|| MeliError::new(format!("Invalid email filename: {:?}", self)))?
            + 3;
        let mut new_name: String = path[..idx].to_string();
        let mut flags = self.fetch_flags();
        if !(flags & f).is_empty() {
            return Ok(());
        }
        flags.toggle(f);
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

        fs::rename(&path, &new_name)?;
        let hash = envelope.hash();
        let hash_index = self.hash_index.clone();
        let mut map = hash_index.lock().unwrap();
        let map = map.entry(self.folder_hash).or_default();
        *map.get_mut(&hash).unwrap() = PathBuf::from(new_name);
        Ok(())
    }
}

#[derive(Debug, Default)]
pub struct MaildirFolder {
    hash: FolderHash,
    name: String,
    path: PathBuf,
    children: Vec<usize>,
}

impl MaildirFolder {
    pub fn new(path: String, file_name: String, children: Vec<usize>) -> Result<Self> {
        let pathbuf = PathBuf::from(path);
        let mut h = DefaultHasher::new();
        pathbuf.hash(&mut h);

        let ret = MaildirFolder {
            hash: h.finish(),
            name: file_name,
            path: pathbuf,
            children,
        };
        ret.is_valid()?;
        Ok(ret)
    }
    pub fn path(&self) -> &Path {
        self.path.as_path()
    }
    fn is_valid(&self) -> Result<()> {
        let path = self.path();
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

    fn change_name(&mut self, s: &str) {
        self.name = s.to_string();
    }

    fn children(&self) -> &Vec<usize> {
        &self.children
    }

    fn clone(&self) -> Folder {
        Box::new(MaildirFolder {
            hash: self.hash,
            name: self.name.clone(),
            path: self.path.clone(),
            children: self.children.clone(),
        })
    }
}
