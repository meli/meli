/*
 * meli - backends module
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
pub mod maildir;

use mailbox::email::Mail;
use error::Result;

use std::fmt;

pub trait MailBackend {
    fn get(&self) -> Result<Vec<Mail>>;
}


/* A BackendOp manages common operations for the various mail backends. They should be created when
 * needed */
pub trait BackendOp: ::std::fmt::Debug + ::std::marker::Send {
    fn description(&self) -> String;
    fn as_bytes(&mut self) -> Result<&[u8]>;
    //fn delete(&self) -> ();
    //fn copy(&self
    fn fetch_headers(&mut self) -> Result<&[u8]>;
    fn fetch_body(&mut self) -> Result<&[u8]>;
}

/* BackendOpGenerator is a wrapper for a closure that returns a BackendOp object */
pub struct BackendOpGenerator(Box<Fn() -> Box<BackendOp>>);
impl BackendOpGenerator {
    pub fn new(b: Box<Fn() -> Box<BackendOp>>) -> Self {
        BackendOpGenerator(b)
    }
    pub fn generate(&self) -> Box<BackendOp> {
        self.0()
    }
}
unsafe impl Send for BackendOpGenerator {}
unsafe impl Sync for BackendOpGenerator {}
impl fmt::Debug for BackendOpGenerator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BackendOpGenerator")
    }
}
    
