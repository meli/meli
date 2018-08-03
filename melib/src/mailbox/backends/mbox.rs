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

use conf::Folder;
use error::Result;
use async::*;
use mailbox::backends::{BackendOp, MailBackend, RefreshEventConsumer};
use mailbox::email::{Envelope, Flag};

/// `BackendOp` implementor for Mbox
#[derive(Debug, Default, Clone)]
pub struct MboxOp {}

impl MboxOp {
    pub fn new(_path: String) -> Self {
        MboxOp {}
    }
}

impl BackendOp for MboxOp {
    fn description(&self) -> String {
        unimplemented!();
    }
    fn as_bytes(&mut self) -> Result<&[u8]> {
        unimplemented!();
    }
    fn fetch_headers(&mut self) -> Result<&[u8]> {
        unimplemented!();
    }
    fn fetch_body(&mut self) -> Result<&[u8]> {
        unimplemented!();
    }
    fn fetch_flags(&self) -> Flag {
        unimplemented!();
    }
}

/// Mbox backend
#[derive(Debug)]
pub struct MboxType {}

impl MailBackend for MboxType {
    fn get(&self, _folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        unimplemented!();
    }
    fn watch(&self, _sender: RefreshEventConsumer, _folders: &[Folder]) -> () {
        unimplemented!();
    }
}

impl MboxType {
    pub fn new(_path: &str) -> Self {
        MboxType {}
    }
}
