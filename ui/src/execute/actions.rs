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

use crate::components::Component;
pub use melib::mailbox::{SortField, SortOrder};
use melib::thread::ThreadHash;
use melib::{Draft, EnvelopeHash};

extern crate uuid;
use uuid::Uuid;

#[derive(Debug)]
pub enum ListingAction {
    SetPlain,
    SetThreaded,
    SetCompact,
    Filter(String),
    SetRead,
    SetUnread,
    Delete,
}

#[derive(Debug)]
pub enum TabAction {
    TabOpen(Option<Box<Component>>),
    NewDraft(usize, Option<Draft>),
    Reply((usize, usize, usize), ThreadHash), // thread coordinates (account, mailbox, root_set idx) and thread hash
    Close,
    Edit(usize, EnvelopeHash), // account_position, envelope hash
    Kill(Uuid),
}

#[derive(Debug)]
pub enum MailingListAction {
    ListPost,
    ListArchive,
    ListUnsubscribe,
}

#[derive(Debug)]
pub enum Action {
    Listing(ListingAction),
    ViewMailbox(usize),
    Sort(SortField, SortOrder),
    SubSort(SortField, SortOrder),
    Tab(TabAction),
    ToggleThreadSnooze,
    MailingListAction(MailingListAction),
    SetEnv(String, String),
    PrintEnv(String),
}
