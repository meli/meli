/*
 * meli
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

//! User actions that need to be handled by the UI

use std::{path::PathBuf, sync::Arc};

use melib::{email::mailto::Mailto, Flag, SortField, SortOrder};

use crate::components::{Component, ComponentId};

#[derive(Debug, Eq, PartialEq)]
pub enum FlagAction {
    Set(Flag),
    Unset(Flag),
}

#[derive(Debug, Eq, PartialEq)]
pub enum TagAction {
    Add(String),
    Remove(String),
}

#[derive(Debug, Eq, PartialEq)]
pub enum ListingAction {
    SetPlain,
    SetThreaded,
    SetCompact,
    SetConversations,
    Search(String),
    Select(String),
    SetSeen,
    SetUnseen,
    SendToTrash,
    CopyTo(MailboxPath),
    CopyToOtherAccount(AccountName, MailboxPath),
    MoveTo(MailboxPath),
    MoveToOtherAccount(AccountName, MailboxPath),
    Import(PathBuf, MailboxPath),
    ExportMbox(Option<melib::mbox::MboxFormat>, PathBuf),
    Delete,
    OpenInNewTab,
    Tag(TagAction),
    Flag(FlagAction),
    ClearSelection,
    ToggleThreadSnooze,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ComposerTabAction {
    DiscardDraft,
    SaveDraft,
    #[cfg(feature = "gpgme")]
    ToggleSign,
    #[cfg(feature = "gpgme")]
    ToggleEncrypt,
    AddAttachment(String),
    AddAttachmentFilePicker(Option<String>),
    AddAttachmentPipe(String),
    RemoveAttachment(usize),
}

#[derive(Debug, PartialEq)]
pub enum TabAction {
    ComposerAction(ComposerTabAction),
    Close,
    Kill(ComponentId),
    New(Option<Box<dyn Component>>),
    ManageMailboxes,
    ManageJobs,
    #[cfg(feature = "cli-docs")]
    Man(crate::manpages::ManPages),
}

#[derive(Debug, Eq, PartialEq)]
pub enum MailingListAction {
    ListPost,
    ListArchive,
    ListUnsubscribe,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ViewAction {
    Pipe(String, Vec<String>),
    Filter(Option<String>),
    SaveAttachment(usize, String),
    PipeAttachment(usize, String, Vec<String>),
    ExportMail(String),
    AddAddressesToContacts,
}

#[derive(Debug, Eq, PartialEq)]
pub enum ComposeAction {
    Mailto(Mailto),
}

#[derive(Debug, Eq, PartialEq)]
pub enum AccountAction {
    ReIndex,
    PrintAccountSetting(String),
}

#[derive(Debug, Eq, PartialEq)]
pub enum MailboxOperation {
    Create(NewMailboxPath),
    Delete(MailboxPath),
    Subscribe(MailboxPath),
    Unsubscribe(MailboxPath),
    Rename(MailboxPath, NewMailboxPath),
    // Placeholder
    SetPermissions(MailboxPath),
}

#[derive(Debug, PartialEq)]
pub enum Action {
    Listing(ListingAction),
    ViewMailbox(usize),
    Sort(SortField, SortOrder),
    SortColumn(usize, SortOrder),
    SubSort(SortField, SortOrder),
    Tab(TabAction),
    MailingListAction(MailingListAction),
    View(ViewAction),
    SetEnv(String, String),
    PrintEnv(String),
    CurrentDirectory,
    ChangeCurrentDirectory(PathBuf),
    Compose(ComposeAction),
    Mailbox(AccountName, MailboxOperation),
    AccountAction(AccountName, AccountAction),
    PrintSetting(String),
    ReloadConfiguration,
    ToggleMouse,
    Quit,
}

impl Action {
    pub fn needs_confirmation(&self) -> bool {
        matches!(
            self,
            Self::Listing(ListingAction::Delete)
                | Self::MailingListAction(_)
                | Self::Mailbox(_, _)
                | Self::Quit
        )
    }
}

type AccountName = String;
type MailboxPath = String;
type NewMailboxPath = String;

macro_rules! impl_into_action {
    ($({$t:ty => $var:tt}),*$(,)?) => {
        $(
            impl From<$t> for Action {
                fn from(v: $t) -> Self {
                    Self::$var(v)
                }
            }
        )*
    };
}
macro_rules! impl_tuple_into_action {
    ($({$a:ty,$b:ty => $var:tt}),*$(,)?) => {
        $(
            impl From<($a,$b)> for Action {
                fn from((a, b): ($a,$b)) -> Self {
                    Self::$var(a.to_string(), b)
                }
            }
        )*
    };
}

impl_into_action!(
    { ListingAction => Listing },
    { TabAction => Tab },
    { MailingListAction => MailingListAction },
    { ViewAction => View },
    { ComposeAction => Compose }
);
impl_tuple_into_action!(
    { AccountName, MailboxOperation => Mailbox },
    { AccountName, AccountAction => AccountAction },
    { Arc<str>, MailboxOperation => Mailbox },
    { Arc<str>, AccountAction => AccountAction },
);
