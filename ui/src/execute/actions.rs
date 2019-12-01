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
use melib::backends::FolderOperation;
use melib::thread::ThreadHash;
pub use melib::thread::{SortField, SortOrder};
use melib::{Draft, EnvelopeHash};

extern crate uuid;
use uuid::Uuid;

#[derive(Debug)]
pub enum TagAction {
    Add(String),
    Remove(String),
}

#[derive(Debug)]
pub enum ListingAction {
    SetPlain,
    SetThreaded,
    SetCompact,
    SetConversations,
    Filter(String),
    SetSeen,
    SetUnseen,
    Delete,
    OpenInNewTab,
    Tag(TagAction),
}

#[derive(Debug)]
pub enum TabAction {
    New(Option<Box<dyn Component>>),
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
pub enum ViewAction {
    Pipe(String, Vec<String>),
    SaveAttachment(usize, String),
}

#[derive(Debug)]
pub enum ComposeAction {
    AddAttachment(String),
    RemoveAttachment(usize),
    ToggleSign,
}

#[derive(Debug)]
pub enum AccountAction {
    ReIndex,
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
    View(ViewAction),
    SetEnv(String, String),
    PrintEnv(String),
    Compose(ComposeAction),
    Folder(AccountName, FolderPath, FolderOperation),
    AccountAction(AccountName, AccountAction),
}

type AccountName = String;
type FolderPath = String;
