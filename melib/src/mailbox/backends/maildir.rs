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

use async::*;
use conf::Folder;
use error::{MeliError, Result};
use mailbox::backends::{
    BackendOp, BackendOpGenerator, MailBackend, RefreshEvent, RefreshEventConsumer,
};
use mailbox::email::parser;
use mailbox::email::{Envelope, Flag};

extern crate notify;

use self::notify::{watcher, DebouncedEvent, RecursiveMode, Watcher};
use std::time::Duration;

use std::sync::mpsc::channel;
//use std::sync::mpsc::sync_channel;
//use std::sync::mpsc::SyncSender;
//use std::time::Duration;
use std::thread;
extern crate crossbeam;
use memmap::{Mmap, Protection};
use std::path::PathBuf;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;

/// `BackendOp` implementor for Maildir
#[derive(Debug, Default)]
pub struct MaildirOp {
    path: String,
    slice: Option<Mmap>,
}

impl Clone for MaildirOp {
    fn clone(&self) -> Self {
        MaildirOp {
            path: self.path.clone(),
            slice: None,
        }
    }
}

impl MaildirOp {
    pub fn new(path: String) -> Self {
        MaildirOp {
            path: path,
            slice: None,
        }
    }
}

impl BackendOp for MaildirOp {
    fn description(&self) -> String {
        format!("Path of file: {}", self.path)
    }
    fn as_bytes(&mut self) -> Result<&[u8]> {
        if self.slice.is_none() {
            self.slice = Some(Mmap::open_path(self.path.to_string(), Protection::Read)?);
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
        let path = PathBuf::from(&self.path);
        let filename = path.file_name().unwrap().to_str().unwrap();
        if !filename.contains(":2,") {
            return flag;
        }

        for f in filename.chars().rev() {
            match f {
                ',' => break,
                'P' => flag |= Flag::PASSED,
                'R' => flag |= Flag::REPLIED,
                'S' => flag |= Flag::SEEN,
                'T' => flag |= Flag::TRASHED,
                'D' => flag |= Flag::DRAFT,
                'F' => flag |= Flag::FLAGGED,
                _ => panic!(),
            }
        }

        flag
    }
}

/// Maildir backend https://cr.yp.to/proto/maildir.html
#[derive(Debug)]
pub struct MaildirType {
    path: String,
    idx: (usize, usize),
}

impl MailBackend for MaildirType {
    fn get(&self, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        self.multicore(4, folder)
    }
    fn watch(&self, sender: RefreshEventConsumer, folders: &[Folder]) -> () {
        let folders = folders.to_vec();

        thread::Builder::new()
            .name("folder watch".to_string())
            .spawn(move || {
                let (tx, rx) = channel();
                let mut watcher = watcher(tx, Duration::from_secs(1)).unwrap();
                for f in folders {
                    if MaildirType::is_valid(&f).is_err() {
                        continue;
                    }
                    let mut p = PathBuf::from(&f.path());
                    p.push("cur");
                    watcher.watch(&p, RecursiveMode::NonRecursive).unwrap();
                    p.pop();
                    p.push("new");
                    watcher.watch(&p, RecursiveMode::NonRecursive).unwrap();
                }
                loop {
                    match rx.recv() {
                        Ok(event) => match event {
                            DebouncedEvent::Create(pathbuf) => {
                                let path = pathbuf.parent().unwrap().to_str().unwrap();

                                let mut hasher = DefaultHasher::new();
                                hasher.write(path.as_bytes());
                                sender.send(RefreshEvent {
                                    folder: format!(
                                        "{}", path
                                    ),
                                    hash: hasher.finish(),
                                });
                            }
                            _ => {}
                        },
                        Err(e) => eprintln!("watch error: {:?}", e),
                    }
                }
            })
            .unwrap();
    }
}

impl MaildirType {
    pub fn new(path: &str, idx: (usize, usize)) -> Self {
        MaildirType {
            path: path.to_string(),
            idx: idx,
        }
    }
    fn is_valid(f: &Folder) -> Result<()> {
        let path = f.path();
        let mut p = PathBuf::from(path);
        for d in &["cur", "new", "tmp"] {
            p.push(d);
            if !p.is_dir() {
                return Err(MeliError::new(format!(
                    "{} is not a valid maildir folder",
                    path
                )));
            }
            p.pop();
        }
        Ok(())
    }
    pub fn multicore(&self, cores: usize, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        
        let mut w = AsyncBuilder::new();
        let handle = {
            let tx = w.tx();
            // TODO: Avoid clone
            let folder = folder.clone();

            
        thread::Builder::new()
            .name(format!("parsing {:?}", folder))
                  .spawn(move ||  {
        MaildirType::is_valid(&folder)?;
        let path = folder.path();
        let mut path = PathBuf::from(path);
        path.push("cur");
        let iter = path.read_dir()?;
        let count = path.read_dir()?.count();
        let mut files: Vec<String> = Vec::with_capacity(count);
        let mut r = Vec::with_capacity(count);
        for e in iter {
            let e = e.and_then(|x| {
                let path = x.path();
                Ok(path.to_str().unwrap().to_string())
            })?;
            files.push(e);
        }
        let mut threads = Vec::with_capacity(cores);
        if !files.is_empty() {
            crossbeam::scope(|scope| {
                let chunk_size = if count / cores > 0 {
                    count / cores
                } else {
                    count
                };
                for chunk in files.chunks(chunk_size) {
                    let mut tx = tx.clone();
                    let s = scope.spawn(move || {
                        let len = chunk.len();
                        let size = if len <= 100 { 100 } else { (len / 100) * 100};
                        let mut local_r: Vec<Envelope> = Vec::with_capacity(chunk.len());
                        for c in chunk.chunks(size) {
                            let len = c.len();
                            for e in c {
                                let e_copy = e.to_string();
                                if let Some(mut e) =
                                    Envelope::from_token(Box::new(BackendOpGenerator::new(Box::new(
                                                    move || Box::new(MaildirOp::new(e_copy.clone())),
                                                    )))) {
                                        if e.populate_headers().is_err() {
                                            continue;
                                        }
                                        local_r.push(e);
                                    }
                            }
                            tx.send(AsyncStatus::ProgressReport(len));
                        }
                        local_r
                    });
                    threads.push(s);
                }
            });
        }
        for t in threads {
            let mut result = t.join();
            r.append(&mut result);
        }
        tx.send(AsyncStatus::Finished);
        Ok(r)
        }).unwrap()
        }; 
        w.build(handle)
    }
}
