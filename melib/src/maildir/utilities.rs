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

use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    io::{BufReader, Read},
    ops::{Deref, DerefMut},
    path::{Path, PathBuf},
    sync::{Arc, Mutex, RwLock},
};

use super::Configuration;
use crate::{
    backends::prelude::*,
    error::{Error, Result},
    utils::shellexpand::ShellExpandTrait,
};

/// Read a maildir entry into bytes.
#[derive(Debug)]
pub struct MaildirOp {
    pub hash_index: HashIndexes,
    pub mailbox_hash: MailboxHash,
    pub hash: EnvelopeHash,
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

    pub fn path(&self) -> Option<PathBuf> {
        let map = self.hash_index.lock().unwrap();
        let map = map.get(&self.mailbox_hash)?;
        log::trace!("looking for {} in {} map", self.hash, self.mailbox_hash);
        let mut hash = self.hash;
        loop {
            let Some(p) = map.get(&hash) else {
                log::trace!("doesn't contain it though len = {}\n{:#?}", map.len(), map);
                for e in map.iter() {
                    log::debug!("{:#?}", e);
                }
                return None;
            };
            if let Some(ref modif) = p.modified {
                match modif {
                    PathMod::Path(ref path) => return Some(path.to_path_buf()),
                    PathMod::Hash(next_hash) => {
                        hash = *next_hash;
                    }
                }
            } else if p.removed {
                return None;
            } else {
                return Some(p.buf.to_path_buf());
            }
        }
    }

    pub async fn as_bytes(&self) -> Result<Vec<u8>> {
        let _self = self.clone();

        smol::unblock(move || {
            let Some(path) = _self.path() else {
                return Err(Error::new("Not found")
                    .set_summary(format!("Message with hash {} was not found.", _self.hash))
                    .set_kind(ErrorKind::NotFound));
            };
            let file = std::fs::OpenOptions::new()
                .read(true)
                .write(false)
                .open(path)?;
            let mut buf_reader = BufReader::new(file);
            let mut contents = Vec::new();
            buf_reader.read_to_end(&mut contents)?;
            Ok(contents)
        })
        .await
    }
}

#[derive(Clone, Debug, Default)]
pub struct MaildirMailbox {
    pub hash: MailboxHash,
    pub name: String,
    pub fs_path: PathBuf,
    pub path: PathBuf,
    pub parent: Option<MailboxHash>,
    pub children: Vec<MailboxHash>,
    pub usage: Arc<RwLock<SpecialUsageMailbox>>,
    pub is_subscribed: bool,
    pub permissions: MailboxPermissions,
    pub total: Arc<Mutex<usize>>,
    pub unseen: Arc<Mutex<usize>>,
}

impl MaildirMailbox {
    pub fn new(
        given_path: String,
        file_name: String,
        parent: Option<MailboxHash>,
        children: Vec<MailboxHash>,
        accept_invalid: bool,
        config: &Configuration,
    ) -> Result<Self> {
        let (fs_path, suffix) = {
            let mut fs_path = config.path.clone();
            let mut suffix = given_path.clone();
            let root_mailbox_path_str = PathBuf::from(&config.settings.root_mailbox)
                .expand()
                .display()
                .to_string();
            if suffix.starts_with(&root_mailbox_path_str)
                && suffix.get(root_mailbox_path_str.len()..).is_some()
                && suffix[root_mailbox_path_str.len()..].starts_with("/")
            {
                suffix.replace_range(0..=root_mailbox_path_str.len(), "");
            }
            if suffix.starts_with(&config.root_mailbox_name)
                && suffix.get(config.root_mailbox_name.len()..).is_some()
                && suffix[config.root_mailbox_name.len()..].starts_with("/")
            {
                suffix.replace_range(0..=config.root_mailbox_name.len(), "");
            }
            fs_path.push(&suffix);
            if !fs_path.starts_with(&config.path) && fs_path != config.path {
                return Err(Error::new(format!(
                    "Path given, `{}`, is is not included in the root mailbox path `{}`.",
                    &given_path,
                    config.path.display()
                )));
            }
            (fs_path, suffix)
        };

        let hash = {
            let mut h = DefaultHasher::new();
            fs_path.hash(&mut h);
            MailboxHash(h.finish())
        };

        let path = if config.is_root_a_mailbox {
            let mut path = PathBuf::from(&config.root_mailbox_name);
            path.push(suffix);
            path
        } else {
            PathBuf::from(&suffix)
        };

        let read_only = if let Ok(metadata) = std::fs::metadata(&fs_path) {
            metadata.permissions().readonly()
        } else {
            true
        };

        let ret = Self {
            hash,
            name: file_name,
            path,
            fs_path,
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

    pub fn new_root_mailbox(
        given_path: String,
        file_name: String,
        parent: Option<MailboxHash>,
        children: Vec<MailboxHash>,
        accept_invalid: bool,
        settings: &AccountSettings,
    ) -> Result<Self> {
        let fs_path = PathBuf::from(&given_path).expand();
        let hash = {
            let mut h = DefaultHasher::new();
            fs_path.hash(&mut h);
            MailboxHash(h.finish())
        };

        let path = fs_path
            .strip_prefix(
                PathBuf::from(&settings.root_mailbox)
                    .expand()
                    .parent()
                    .unwrap_or_else(|| Path::new("/")),
            )
            .ok()
            .unwrap()
            .to_path_buf();

        let read_only = if let Ok(metadata) = std::fs::metadata(&fs_path) {
            metadata.permissions().readonly()
        } else {
            true
        };

        let ret = Self {
            hash,
            name: file_name,
            path,
            fs_path,
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

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

/// Extension trait for [`Path`] for various maildir file calculations.
pub trait MaildirFilePathExt {
    /// Parses the `,DFPRST` filename suffix of a path into a [`Flag`].
    ///
    /// If the filename does not contain `:2,` or ends with `:2,` an empty
    /// [`Flag`] is returned.
    fn flags(&self) -> Flag;
    /// Calculates a new path for `self` with the flags overwritten by the ones
    /// specified in `flags` argument.
    fn set_flags(&self, flags: Flag, config: &Configuration) -> Result<PathBuf>;
    /// Calculates new path for `self` if it was placed in `dest_dir` taking
    /// into account [`Configuration::rename_regex`].
    fn place_in_dir(&self, dest_dir: &Path, config: &Configuration) -> Result<PathBuf>;
    /// Hashes `self` into an [`EnvelopeHash`].
    fn to_envelope_hash(&self) -> EnvelopeHash;
    /// Checks if file is placed in a `new` directory.
    fn is_in_new(&self) -> bool;
}

impl MaildirFilePathExt for Path {
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
                        "DEBUG: in MaildirFilePathExt::flags(), encountered unknown flag marker \
                         {:?}, path is {}",
                        f, path
                    );
                }
            }
        }

        flag
    }

    fn set_flags(&self, flags: Flag, _config: &Configuration) -> Result<PathBuf> {
        let filename = self
            .file_name()
            .ok_or_else(|| format!("Could not get filename of `{}`", self.display(),))?
            .to_string_lossy()
            .to_string();
        let (idx, append_2): (usize, bool) = if let Some(idx) = filename.rfind(":2,") {
            (idx + 3, false)
        } else {
            log::trace!(
                "Invalid maildir filename: {:?}\nBacktrace:\n{}",
                self,
                std::backtrace::Backtrace::capture()
            );
            (filename.len(), true)
        };
        let mut new_name: String = filename[..idx].to_string();
        if append_2 {
            new_name.push_str(":2,");
        }
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
        let mut new_path: PathBuf = self.into();
        new_path.set_file_name(new_name);
        Ok(new_path)
    }

    fn place_in_dir(&self, dest_dir: &Path, config: &Configuration) -> Result<PathBuf> {
        let mut filename = self
            .file_name()
            .ok_or_else(|| format!("Could not get filename of `{}`", self.display()))?
            .to_string_lossy();
        if !filename.contains(":2,") {
            filename = Cow::Owned(format!("{}:2,", filename));
        }
        let mut new_path = dest_dir.to_path_buf();
        if let Some(ref rename_regex) = config.rename_regex {
            new_path.push(rename_regex.replace_all(&filename, "").as_ref());
        } else {
            new_path.push(filename.as_ref());
        };
        Ok(new_path)
    }

    fn to_envelope_hash(&self) -> EnvelopeHash {
        debug_assert!(self.is_file());
        let mut hasher = DefaultHasher::default();
        self.hash(&mut hasher);
        EnvelopeHash(hasher.finish())
    }

    fn is_in_new(&self) -> bool {
        use std::{ffi::OsStr, path::Component};

        if self.is_dir() {
            false
        } else {
            let mut iter = self.components().rev();
            iter.next();
            iter.next() == Some(Component::Normal(OsStr::new("new")))
        }
    }
}

/// Extension trait for [`Path`] for various maildir mailbox calculations.
pub trait MaildirMailboxPathExt {
    /// Calculate the mailbox hash this path would have.
    ///
    /// - If it's a file, the directory the file is in is used.
    /// - If it's a directory:
    /// - If it's a directory and it's ending in `{cur, new, tmp}`, they are
    ///   popped first.
    fn to_mailbox_hash(&self) -> MailboxHash;
}

impl MaildirMailboxPathExt for Path {
    fn to_mailbox_hash(&self) -> MailboxHash {
        let mut path = self.to_path_buf();
        if path.is_file() {
            path.pop();
        }
        if path.is_dir() && (path.ends_with("cur") || path.ends_with("tmp") | path.ends_with("new"))
        {
            path.pop();
        }

        let mut hasher = DefaultHasher::new();
        path.hash(&mut hasher);
        MailboxHash(hasher.finish())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum PathMod {
    Path(PathBuf),
    Hash(EnvelopeHash),
}

#[derive(Debug, Default)]
pub struct MaildirPath {
    pub buf: PathBuf,
    pub modified: Option<PathMod>,
    pub removed: bool,
}

impl Deref for MaildirPath {
    type Target = PathBuf;

    fn deref(&self) -> &Self::Target {
        assert!(!(self.removed && self.modified.is_none()));
        &self.buf
    }
}

impl DerefMut for MaildirPath {
    fn deref_mut(&mut self) -> &mut Self::Target {
        assert!(!(self.removed && self.modified.is_none()));
        &mut self.buf
    }
}

impl From<PathBuf> for MaildirPath {
    fn from(val: PathBuf) -> Self {
        Self {
            buf: val,
            modified: None,
            removed: false,
        }
    }
}

#[derive(Debug, Default)]
pub struct HashIndex {
    pub index: HashMap<EnvelopeHash, MaildirPath>,
    pub _hash: MailboxHash,
}

impl Deref for HashIndex {
    type Target = HashMap<EnvelopeHash, MaildirPath>;

    fn deref(&self) -> &Self::Target {
        &self.index
    }
}

impl DerefMut for HashIndex {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.index
    }
}

pub type HashIndexes = Arc<Mutex<HashMap<MailboxHash, HashIndex>>>;

pub fn move_to_cur(config: &Configuration, p: &Path) -> Result<PathBuf> {
    let cur = {
        let mut cur = p.to_path_buf();
        cur.pop();
        cur.pop();
        cur.push("cur");
        cur
    };
    let dest_path = p.place_in_dir(&cur, config)?;
    log::trace!("moved to cur: {}", dest_path.display());
    #[cfg(not(test))]
    std::fs::rename(p, &dest_path)?;
    Ok(dest_path)
}
