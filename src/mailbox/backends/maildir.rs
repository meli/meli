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
//use std::io::prelude::*;
//use std::fs::File;
use std::path::PathBuf;
use mailbox::email::Mail;
use error::{MeliError, Result};

use mailbox::backends::MailBackend;

pub struct MaildirType {
    path: String,
}

impl MailBackend for MaildirType {
    fn get(&self) -> Result<Vec<Mail>> {
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
            match Mail::from(e) {
                Some(e) => {r.push(e);},
                None => {}
            }
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
        Ok(r)
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
        for d in ["cur", "new", "tmp"].iter() {
            p.push(d);
            if !p.is_dir() {
                return Err(MeliError::new(format!("{} is not a valid maildir folder", path)));
            }
            p.pop();
        }
        Ok(())
    }
}
