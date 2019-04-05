/*
 * meli - ui crate.
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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
 * User actions that need to be handled by the UI
 */

use components::Component;
pub use melib::mailbox::{SortField, SortOrder};

extern crate uuid;
use uuid::Uuid;

#[derive(Debug)]
pub enum ListingAction {
    SetPlain,
    SetThreaded,
    SetCompact,
}

#[derive(Debug)]
pub enum TabAction {
    TabOpen(Option<Box<Component>>),
    NewDraft(usize),
    Reply((usize, usize, usize), usize), // thread coordinates (account, mailbox, root_set idx) and message idx
    Close,
    Edit((usize, usize, usize), usize), // thread coordinates (account, mailbox, root_set idx) and message idx
    Kill(Uuid),
}

#[derive(Debug)]
pub enum Action {
    Listing(ListingAction),
    ViewMailbox(usize),
    Sort(SortField, SortOrder),
    SubSort(SortField, SortOrder),
    Tab(TabAction),
}
