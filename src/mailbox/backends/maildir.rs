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

use mailbox::email::Mail;
use error::{MeliError, Result};
use mailbox::backends::MailBackend;

extern crate crossbeam;
use std::path::PathBuf;


pub struct MaildirType {
    path: String,
}

impl MailBackend for MaildirType {
    fn get(&self) -> Result<Vec<Mail>> {
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
            path: path.to_string()
        }
    }
    fn is_valid(path: &str) -> Result<()> {
        let mut p = PathBuf::from(path);
        for d in &["cur", "new", "tmp"] {
            p.push(d);
            if !p.is_dir() {
                return Err(MeliError::new(format!("{} is not a valid maildir folder", path)));
            }
            p.pop();
        }
        Ok(())
    }
    pub fn get_multicore(&self, cores: usize) -> Result<Vec<Mail>> {
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
                        let mut local_r:Vec<Mail> = Vec::with_capacity(chunk.len());
                        for e in chunk {
                            if let Some(e) =  Mail::from(e) {
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
