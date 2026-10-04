//
// meli
//
// Copyright 2026 Manos Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use crate::{
    contacts::{AddressBookName, Card},
    prelude::*,
};

#[derive(Clone, Debug)]
pub struct ContactBackendCapabilities {
    pub is_async: bool,
    pub is_remote: bool,
    pub extensions: Option<Vec<(String, MailBackendExtensionStatus)>>,
    pub supports_search: bool,
    pub metadata: Option<serde_json::Value>,
    pub can_create_address_book: bool,
    pub max_address_books_per_card: Option<u32>,
}

/// A default for [`ContactBackendCapabilities`] for use in const contexts.
pub const EMPTY_CONTACT_BACKEND_CAPABILITIES: ContactBackendCapabilities =
    ContactBackendCapabilities {
        is_async: false,
        is_remote: false,
        supports_search: false,
        extensions: None,
        metadata: None,
        can_create_address_book: false,
        max_address_books_per_card: None,
    };

impl Default for ContactBackendCapabilities {
    fn default() -> Self {
        EMPTY_CONTACT_BACKEND_CAPABILITIES
    }
}

pub trait ContactBackend: ::std::fmt::Debug + Send + Sync {
    fn capabilities(&mut self) -> ContactBackendCapabilities;
    fn is_online(&mut self) -> ResultFuture<()> {
        Ok(Box::pin(async { Ok(()) }))
    }
    fn address_books(&mut self) -> ResultFuture<Vec<AddressBookName>>;
    fn fetch_book(&mut self, address_book: &AddressBookName) -> ResultFuture<Vec<Card>>;
    fn search(&self, term: &str, address_book: Option<&AddressBookName>)
        -> ResultFuture<Vec<Card>>;
}
