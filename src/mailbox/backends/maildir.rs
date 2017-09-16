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

use mailbox::email::Envelope;
use error::{MeliError, Result};
use mailbox::backends::{BackendOp, BackendOpGenerator, MailBackend};
use mailbox::email::parser;

extern crate crossbeam;
use std::path::PathBuf;
use memmap::{Mmap, Protection};


pub struct MaildirType {
    path: String,
}

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
            self.slice = Some(
                Mmap::open_path(self.path.to_string(), Protection::Read).unwrap(),
            );
        }
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
}


impl MailBackend for MaildirType {
    fn get(&self) -> Result<Vec<Envelope>> {
        self.get_multicore(4)
        /*

        MaildirType::is_valid(&self.path)?;
        let mut path = PathBuf::from(&self.path);
        path.push("cur");
        let iter = path.read_dir()?;
        let count = path.read_dir()?.count();
        let mut r = Vec::with_capacity(count);
        for e in iter {
            //eprintln!("{:?}", e);
            let e = e.and_then(|x| {
                let path = x.path();
                Ok(path.to_str().unwrap().to_string())
            })?;
            match Mail::from(&e) {
                Some(e) => {r.push(e);},
                None => {}
            }
        }
        Ok(r)
            */
    }
}

impl MaildirType {
    pub fn new(path: &str) -> Self {
        MaildirType {
            path: path.to_string(),
        }
    }
    fn is_valid(path: &str) -> Result<()> {
        let mut p = PathBuf::from(path);
        for d in &["cur", "new", "tmp"] {
            p.push(d);
            if !p.is_dir() {
                return Err(MeliError::new(
                    format!("{} is not a valid maildir folder", path),
                ));
            }
            p.pop();
        }
        Ok(())
    }
    pub fn get_multicore(&self, cores: usize) -> Result<Vec<Envelope>> {
        MaildirType::is_valid(&self.path)?;
        let mut path = PathBuf::from(&self.path);
        path.push("cur");
        let iter = path.read_dir()?;
        let count = path.read_dir()?.count();
        let mut files: Vec<String> = Vec::with_capacity(count);
        let mut r = Vec::with_capacity(count);
        for e in iter {
            //eprintln!("{:?}", e);
            let e = e.and_then(|x| {
                let path = x.path();
                Ok(path.to_str().unwrap().to_string())
            })?;
            files.push(e);
            /*

            f.read_to_end(&mut buffer)?;
            eprintln!("{:?}", String::from_utf8(buffer.clone()).unwrap());
            let m = match Email::parse(&buffer) {
                Ok((v, rest)) => match rest.len() {
                    0 => v,
                    _ => 
                    { eprintln!("{:?}", String::from_utf8(rest.to_vec()).unwrap());
panic!("didn't parse"); },
                },
                Err(v) => panic!(v),
            };

            r.push(m);
            */
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
                    let s = scope.spawn(move || {
                        let mut local_r: Vec<Envelope> = Vec::with_capacity(chunk.len());
                        for e in chunk {
                            let e_copy = e.to_string();
                            if let Some(e) = Envelope::from(Box::new(BackendOpGenerator::new(
                                Box::new(move || Box::new(MaildirOp::new(e_copy.clone()))),
                            ))) {
                                local_r.push(e);
                            }
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
        Ok(r)
    }
}
