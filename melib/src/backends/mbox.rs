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

/*!
 * https://wiki2.dovecot.org/MailboxFormat/mbox
 */

use crate::async_workers::{Async, AsyncBuilder, AsyncStatus, WorkContext};
use crate::backends::BackendOp;
use crate::backends::FolderHash;
use crate::backends::{
    BackendFolder, Folder, FolderPermissions, MailBackend, RefreshEvent, RefreshEventConsumer,
    RefreshEventKind, SpecialUsageMailbox,
};
use crate::conf::AccountSettings;
use crate::email::parser::BytesExt;
use crate::email::*;
use crate::error::{MeliError, Result};
use crate::shellexpand::ShellExpandTrait;
use fnv::FnvHashMap;
use libc;
use memmap::{Mmap, Protection};
use nom::{IResult, Needed};
extern crate notify;
use self::notify::{watcher, DebouncedEvent, RecursiveMode, Watcher};
use std::collections::hash_map::DefaultHasher;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::BufReader;
use std::io::Read;
use std::os::unix::io::AsRawFd;
use std::path::{Path, PathBuf};
use std::sync::mpsc::channel;
use std::sync::{Arc, Mutex};

const F_OFD_SETLKW: libc::c_int = 38;

// Open file description locking
// # man fcntl
fn get_rw_lock_blocking(f: &File) {
    let fd: libc::c_int = f.as_raw_fd();
    let mut flock: libc::flock = libc::flock {
        l_type: libc::F_WRLCK as libc::c_short,
        l_whence: libc::SEEK_SET as libc::c_short,
        l_start: 0,
        l_len: 0, /* "Specifying 0 for l_len has the special meaning: lock all bytes starting at the location
                  specified by l_whence and l_start through to the end of file, no matter how large the file grows." */
        l_pid: 0, /* "By contrast with traditional record locks, the l_pid field of that structure must be set to zero when using the commands described below." */
    };
    let ptr: *mut libc::flock = &mut flock;
    let ret_val = unsafe { libc::fcntl(fd, F_OFD_SETLKW, ptr as *mut libc::c_void) };
    debug!(&ret_val);
    assert!(-1 != ret_val);
}

macro_rules! get_path_hash {
    ($path:expr) => {{
        let mut hasher = DefaultHasher::new();
        $path.hash(&mut hasher);
        hasher.finish()
    }};
}

#[derive(Debug)]
struct MboxFolder {
    hash: FolderHash,
    name: String,
    path: PathBuf,
    content: Vec<u8>,
    children: Vec<FolderHash>,
    parent: Option<FolderHash>,
    permissions: FolderPermissions,
}

impl BackendFolder for MboxFolder {
    fn hash(&self) -> FolderHash {
        self.hash
    }

    fn name(&self) -> &str {
        self.name.as_str()
    }

    fn path(&self) -> &str {
        /* We know it's valid UTF-8 because we supplied it */
        self.path.to_str().unwrap()
    }

    fn change_name(&mut self, s: &str) {
        self.name = s.to_string();
    }

    fn clone(&self) -> Folder {
        Box::new(MboxFolder {
            hash: self.hash,
            name: self.name.clone(),
            path: self.path.clone(),
            content: self.content.clone(),
            children: self.children.clone(),
            parent: self.parent,
            permissions: self.permissions,
        })
    }

    fn children(&self) -> &[FolderHash] {
        &self.children
    }

    fn parent(&self) -> Option<FolderHash> {
        self.parent
    }

    fn special_usage(&self) -> SpecialUsageMailbox {
        SpecialUsageMailbox::Normal
    }

    fn permissions(&self) -> FolderPermissions {
        self.permissions
    }
}

/// `BackendOp` implementor for Mbox
#[derive(Debug, Default)]
pub struct MboxOp {
    hash: EnvelopeHash,
    path: PathBuf,
    offset: Offset,
    length: Length,
    slice: Option<Mmap>,
}

impl MboxOp {
    pub fn new(hash: EnvelopeHash, path: &Path, offset: Offset, length: Length) -> Self {
        MboxOp {
            hash,
            path: path.to_path_buf(),
            slice: None,
            offset,
            length,
        }
    }
}

impl BackendOp for MboxOp {
    fn description(&self) -> String {
        String::new()
    }

    fn as_bytes(&mut self) -> Result<&[u8]> {
        if self.slice.is_none() {
            self.slice = Some(Mmap::open_path(&self.path, Protection::Read)?);
        }
        /* Unwrap is safe since we use ? above. */
        Ok(unsafe {
            &self.slice.as_ref().unwrap().as_slice()[self.offset..self.offset + self.length]
        })
    }

    fn fetch_flags(&self) -> Flag {
        let mut flags = Flag::empty();
        let file = match std::fs::OpenOptions::new()
            .read(true)
            .write(true)
            .open(&self.path)
        {
            Ok(f) => f,
            Err(e) => {
                debug!(e);
                return flags;
            }
        };
        get_rw_lock_blocking(&file);
        let mut buf_reader = BufReader::new(file);
        let mut contents = Vec::new();
        if let Err(e) = buf_reader.read_to_end(&mut contents) {
            debug!(e);
            return flags;
        };

        if let Ok(headers) = parser::headers_raw(contents.as_slice()).to_full_result() {
            if let Some(start) = headers.find(b"Status:") {
                if let Some(end) = headers[start..].find(b"\n") {
                    let start = start + b"Status:".len();
                    let status = headers[start..start + end].trim();
                    if status.contains(&b'F') {
                        flags.set(Flag::FLAGGED, true);
                    }
                    if status.contains(&b'A') {
                        flags.set(Flag::REPLIED, true);
                    }
                    if status.contains(&b'R') {
                        flags.set(Flag::SEEN, true);
                    }
                    if status.contains(&b'D') {
                        flags.set(Flag::TRASHED, true);
                    }
                    if status.contains(&b'T') {
                        flags.set(Flag::DRAFT, true);
                    }
                }
            }
            if let Some(start) = headers.find(b"X-Status:") {
                let start = start + b"X-Status:".len();
                if let Some(end) = headers[start..].find(b"\n") {
                    let status = headers[start..start + end].trim();
                    if status.contains(&b'F') {
                        flags.set(Flag::FLAGGED, true);
                    }
                    if status.contains(&b'A') {
                        flags.set(Flag::REPLIED, true);
                    }
                    if status.contains(&b'R') {
                        flags.set(Flag::SEEN, true);
                    }
                    if status.contains(&b'D') {
                        flags.set(Flag::TRASHED, true);
                    }
                    if status.contains(&b'T') {
                        flags.set(Flag::DRAFT, true);
                    }
                }
            }
        }
        flags
    }

    fn set_flag(&mut self, _envelope: &mut Envelope, _flag: Flag, _value: bool) -> Result<()> {
        Ok(())
    }

    fn set_tag(&mut self, _envelope: &mut Envelope, _tag: String, _value: bool) -> Result<()> {
        Err(MeliError::new("mbox doesn't support tags."))
    }
}

pub fn mbox_parse(
    index: Arc<Mutex<FnvHashMap<EnvelopeHash, (Offset, Length)>>>,
    input: &[u8],
    file_offset: usize,
) -> IResult<&[u8], Vec<Envelope>> {
    if input.is_empty() {
        return IResult::Incomplete(Needed::Unknown);
    }
    let mut input = input;
    let mut offset = 0;
    let mut index = index.lock().unwrap();
    let mut envelopes = Vec::with_capacity(32);
    while !input.is_empty() {
        let next_offset: Option<usize> = input.find(b"\n\nFrom ");
        if let Some(len) = next_offset {
            match Envelope::from_bytes(&input[..len], None) {
                Ok(mut env) => {
                    let mut flags = Flag::empty();
                    if env.other_headers().contains_key("Status") {
                        if env.other_headers()["Status"].contains("F") {
                            flags.set(Flag::FLAGGED, true);
                        }
                        if env.other_headers()["Status"].contains("A") {
                            flags.set(Flag::REPLIED, true);
                        }
                        if env.other_headers()["Status"].contains("R") {
                            flags.set(Flag::SEEN, true);
                        }
                        if env.other_headers()["Status"].contains("D") {
                            flags.set(Flag::TRASHED, true);
                        }
                    }
                    if env.other_headers().contains_key("X-Status") {
                        if env.other_headers()["X-Status"].contains("F") {
                            flags.set(Flag::FLAGGED, true);
                        }
                        if env.other_headers()["X-Status"].contains("A") {
                            flags.set(Flag::REPLIED, true);
                        }
                        if env.other_headers()["X-Status"].contains("R") {
                            flags.set(Flag::SEEN, true);
                        }
                        if env.other_headers()["X-Status"].contains("D") {
                            flags.set(Flag::TRASHED, true);
                        }
                        if env.other_headers()["X-Status"].contains("T") {
                            flags.set(Flag::DRAFT, true);
                        }
                    }
                    env.set_flags(flags);
                    index.insert(env.hash(), (offset + file_offset, len));
                    envelopes.push(env);
                }
                Err(_) => {
                    debug!("Could not parse mail at byte offset {}", offset);
                }
            }
            offset += len + 2;
            input = &input[len + 2..];
        } else {
            match Envelope::from_bytes(input, None) {
                Ok(mut env) => {
                    let mut flags = Flag::empty();
                    if env.other_headers().contains_key("Status") {
                        if env.other_headers()["Status"].contains("F") {
                            flags.set(Flag::FLAGGED, true);
                        }
                        if env.other_headers()["Status"].contains("A") {
                            flags.set(Flag::REPLIED, true);
                        }
                        if env.other_headers()["Status"].contains("R") {
                            flags.set(Flag::SEEN, true);
                        }
                        if env.other_headers()["Status"].contains("D") {
                            flags.set(Flag::TRASHED, true);
                        }
                    }
                    if env.other_headers().contains_key("X-Status") {
                        if env.other_headers()["X-Status"].contains("F") {
                            flags.set(Flag::FLAGGED, true);
                        }
                        if env.other_headers()["X-Status"].contains("A") {
                            flags.set(Flag::REPLIED, true);
                        }
                        if env.other_headers()["X-Status"].contains("R") {
                            flags.set(Flag::SEEN, true);
                        }
                        if env.other_headers()["X-Status"].contains("D") {
                            flags.set(Flag::TRASHED, true);
                        }
                        if env.other_headers()["X-Status"].contains("T") {
                            flags.set(Flag::DRAFT, true);
                        }
                    }
                    env.set_flags(flags);
                    index.insert(env.hash(), (offset + file_offset, input.len()));
                    envelopes.push(env);
                }
                Err(_) => {
                    debug!("Could not parse mail at byte offset {}", offset);
                }
            }
            break;
        }
    }
    return IResult::Done(&[], envelopes);
}

type Offset = usize;
type Length = usize;
/// Mbox backend
#[derive(Debug, Default)]
pub struct MboxType {
    path: PathBuf,
    index: Arc<Mutex<FnvHashMap<EnvelopeHash, (Offset, Length)>>>,
    folders: Arc<Mutex<FnvHashMap<FolderHash, MboxFolder>>>,
}

impl MailBackend for MboxType {
    fn is_online(&self) -> bool {
        true
    }
    fn get(&mut self, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        let mut w = AsyncBuilder::new();
        let handle = {
            let tx = w.tx();
            let index = self.index.clone();
            let folder_path = folder.path().to_string();
            let folder_hash = folder.hash();
            let folders = self.folders.clone();
            let closure = move |_work_context| {
                let tx = tx.clone();
                let index = index.clone();
                let file = match std::fs::OpenOptions::new()
                    .read(true)
                    .write(true)
                    .open(&folder_path)
                {
                    Ok(f) => f,
                    Err(e) => {
                        tx.send(AsyncStatus::Payload(Err(MeliError::from(e))))
                            .unwrap();
                        return;
                    }
                };
                get_rw_lock_blocking(&file);
                let mut buf_reader = BufReader::new(file);
                let mut contents = Vec::new();
                if let Err(e) = buf_reader.read_to_end(&mut contents) {
                    tx.send(AsyncStatus::Payload(Err(MeliError::from(e))))
                        .unwrap();
                    return;
                };

                let payload = mbox_parse(index, contents.as_slice(), 0)
                    .to_full_result()
                    .map_err(|e| MeliError::from(e));
                {
                    let mut folder_lock = folders.lock().unwrap();
                    folder_lock
                        .entry(folder_hash)
                        .and_modify(|f| f.content = contents);
                }

                tx.send(AsyncStatus::Payload(payload)).unwrap();
                tx.send(AsyncStatus::Finished).unwrap();
            };
            Box::new(closure)
        };
        w.build(handle)
    }

    fn watch(
        &self,
        sender: RefreshEventConsumer,
        work_context: WorkContext,
    ) -> Result<std::thread::ThreadId> {
        let (tx, rx) = channel();
        let mut watcher = watcher(tx, std::time::Duration::from_secs(10))
            .map_err(|e| e.to_string())
            .map_err(MeliError::new)?;
        for f in self.folders.lock().unwrap().values() {
            watcher
                .watch(&f.path, RecursiveMode::Recursive)
                .map_err(|e| e.to_string())
                .map_err(MeliError::new)?;
            debug!("watching {:?}", f.path.as_path());
        }
        let index = self.index.clone();
        let folders = self.folders.clone();
        let handle = std::thread::Builder::new()
            .name(format!(
                "watching {}",
                self.path.file_name().unwrap().to_str().unwrap()
            ))
            .spawn(move || {
                // Move `watcher` in the closure's scope so that it doesn't get dropped.
                let _watcher = watcher;
                let _work_context = work_context;
                let index = index;
                let folders = folders;
                loop {
                    match rx.recv() {
                        /*
                         * Event types:
                         *
                         * pub enum RefreshEventKind {
                         *     Update(EnvelopeHash, Envelope), // Old hash, new envelope
                         *     Create(Envelope),
                         *     Remove(EnvelopeHash),
                         *     Rescan,
                         * }
                         */
                        Ok(event) => match event {
                            /* Update */
                            DebouncedEvent::NoticeWrite(pathbuf)
                            | DebouncedEvent::Write(pathbuf) => {
                                let folder_hash = get_path_hash!(&pathbuf);
                                let file = match std::fs::OpenOptions::new()
                                    .read(true)
                                    .write(true)
                                    .open(&pathbuf)
                                {
                                    Ok(f) => f,
                                    Err(_) => {
                                        continue;
                                    }
                                };
                                get_rw_lock_blocking(&file);
                                let mut folder_lock = folders.lock().unwrap();
                                let mut buf_reader = BufReader::new(file);
                                let mut contents = Vec::new();
                                if let Err(e) = buf_reader.read_to_end(&mut contents) {
                                    debug!(e);
                                    continue;
                                };
                                if contents
                                    .starts_with(folder_lock[&folder_hash].content.as_slice())
                                {
                                    if let Ok(envelopes) = mbox_parse(
                                        index.clone(),
                                        &contents[folder_lock[&folder_hash].content.len()..],
                                        folder_lock[&folder_hash].content.len(),
                                    )
                                    .to_full_result()
                                    {
                                        for env in envelopes {
                                            sender.send(RefreshEvent {
                                                hash: folder_hash,
                                                kind: RefreshEventKind::Create(Box::new(env)),
                                            });
                                        }
                                    }
                                } else {
                                    sender.send(RefreshEvent {
                                        hash: folder_hash,
                                        kind: RefreshEventKind::Rescan,
                                    });
                                }
                                folder_lock
                                    .entry(folder_hash)
                                    .and_modify(|f| f.content = contents);
                            }
                            /* Remove */
                            DebouncedEvent::NoticeRemove(pathbuf)
                            | DebouncedEvent::Remove(pathbuf) => {
                                if folders
                                    .lock()
                                    .unwrap()
                                    .values()
                                    .any(|f| &f.path == &pathbuf)
                                {
                                    let folder_hash = get_path_hash!(&pathbuf);
                                    sender.send(RefreshEvent {
                                        hash: folder_hash,
                                        kind: RefreshEventKind::Failure(MeliError::new(format!(
                                            "mbox folder {} was removed.",
                                            pathbuf.display()
                                        ))),
                                    });
                                    return;
                                }
                            }
                            DebouncedEvent::Rename(src, dest) => {
                                if folders.lock().unwrap().values().any(|f| &f.path == &src) {
                                    let folder_hash = get_path_hash!(&src);
                                    sender.send(RefreshEvent {
                                        hash: folder_hash,
                                        kind: RefreshEventKind::Failure(MeliError::new(format!(
                                            "mbox folder {} was renamed to {}.",
                                            src.display(),
                                            dest.display()
                                        ))),
                                    });
                                    return;
                                }
                            }
                            /* Trigger rescan of folders */
                            DebouncedEvent::Rescan => {
                                for h in folders.lock().unwrap().keys() {
                                    sender.send(RefreshEvent {
                                        hash: *h,
                                        kind: RefreshEventKind::Rescan,
                                    });
                                }
                                return;
                            }
                            _ => {}
                        },
                        Err(e) => debug!("watch error: {:?}", e),
                    }
                }
            })?;
        Ok(handle.thread().id())
    }
    fn folders(&self) -> Result<FnvHashMap<FolderHash, Folder>> {
        Ok(self
            .folders
            .lock()
            .unwrap()
            .iter()
            .map(|(h, f)| (*h, f.clone() as Folder))
            .collect())
    }
    fn operation(&self, hash: EnvelopeHash) -> Box<dyn BackendOp> {
        let (offset, length) = {
            let index = self.index.lock().unwrap();
            index[&hash]
        };
        Box::new(MboxOp::new(hash, self.path.as_path(), offset, length))
    }

    fn save(&self, _bytes: &[u8], _folder: &str, _flags: Option<Flag>) -> Result<()> {
        unimplemented!();
    }

    fn as_any(&self) -> &dyn::std::any::Any {
        self
    }
}

impl MboxType {
    pub fn new(
        s: &AccountSettings,
        _is_subscribed: Box<dyn Fn(&str) -> bool>,
    ) -> Result<Box<dyn MailBackend>> {
        let path = Path::new(s.root_folder.as_str()).expand();
        if !path.exists() {
            return Err(MeliError::new(format!(
                "\"root_folder\" {} for account {} is not a valid path.",
                s.root_folder.as_str(),
                s.name()
            )));
        }
        let ret = MboxType {
            path,
            ..Default::default()
        };
        let name: String = ret
            .path
            .file_name()
            .map(|f| f.to_string_lossy().into())
            .unwrap_or(String::new());
        let hash = get_path_hash!(&ret.path);

        let read_only = if let Ok(metadata) = std::fs::metadata(&ret.path) {
            metadata.permissions().readonly()
        } else {
            true
        };

        ret.folders.lock().unwrap().insert(
            hash,
            MboxFolder {
                hash,
                path: ret.path.clone(),
                name,
                content: Vec::new(),
                children: Vec::new(),
                parent: None,
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
            },
        );
        /*
        /* Look for other mailboxes */
        let parent_folder = Path::new(path).parent().unwrap();
        let read_dir = std::fs::read_dir(parent_folder);
        if read_dir.is_ok() {
            for f in read_dir.unwrap() {
                if f.is_err() {
                    continue;
                }
                let f = f.unwrap().path();
                if f.is_file() && f != path {
                    let name: String = f
                        .file_name()
                        .map(|f| f.to_string_lossy().into())
                        .unwrap_or(String::new());
                    let hash = get_path_hash!(f);
                    ret.folders.lock().unwrap().insert(
                        hash,
                        MboxFolder {
                            hash,
                            path: f,
                            name,
                            content: Vec::new(),
                            children: Vec::new(),
                            parent: None,
                        },
                    );
                }
            }
        }
        */
        Ok(Box::new(ret))
    }

    pub fn validate_config(s: &AccountSettings) -> Result<()> {
        let path = Path::new(s.root_folder.as_str()).expand();
        if !path.exists() {
            return Err(MeliError::new(format!(
                "\"root_folder\" {} for account {} is not a valid path.",
                s.root_folder.as_str(),
                s.name()
            )));
        }
        Ok(())
    }
}
