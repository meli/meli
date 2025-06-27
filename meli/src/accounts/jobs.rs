//
// meli - accounts module.
//
// Copyright 2017 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use std::{borrow::Cow, collections::HashMap};

use melib::{backends::prelude::*, error::Result, LogLevel};

use crate::{is_variant, jobs::JoinHandle, StatusEvent};

pub enum MailboxJobRequest {
    Mailboxes {
        handle: JoinHandle<Result<HashMap<MailboxHash, Mailbox>>>,
    },
    CreateMailbox {
        path: String,
        handle: JoinHandle<Result<(MailboxHash, HashMap<MailboxHash, Mailbox>)>>,
    },
    DeleteMailbox {
        mailbox_hash: MailboxHash,
        handle: JoinHandle<Result<HashMap<MailboxHash, Mailbox>>>,
    },
    RenameMailbox {
        mailbox_hash: MailboxHash,
        new_path: String,
        handle: JoinHandle<Result<Mailbox>>,
    },
    SetMailboxPermissions {
        mailbox_hash: MailboxHash,
        handle: JoinHandle<Result<()>>,
    },
    SetMailboxSubscription {
        mailbox_hash: MailboxHash,
        new_value: bool,
        handle: JoinHandle<Result<()>>,
    },
}

impl std::fmt::Debug for MailboxJobRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::CreateMailbox { .. } => write!(f, "JobRequest::CreateMailbox"),
            Self::DeleteMailbox { mailbox_hash, .. } => {
                write!(f, "JobRequest::DeleteMailbox({mailbox_hash})")
            }
            Self::RenameMailbox {
                mailbox_hash,
                new_path,
                ..
            } => {
                write!(f, "JobRequest::RenameMailbox {mailbox_hash} to {new_path} ")
            }
            Self::SetMailboxPermissions { .. } => {
                write!(f, "JobRequest::SetMailboxPermissions")
            }
            Self::SetMailboxSubscription { .. } => {
                write!(f, "JobRequest::SetMailboxSubscription")
            }
            Self::Mailboxes { .. } => write!(f, "JobRequest::Mailboxes"),
        }
    }
}

impl std::fmt::Display for MailboxJobRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Mailboxes { .. } => write!(f, "Get mailbox list"),
            Self::CreateMailbox { path, .. } => write!(f, "Create mailbox {path}"),
            Self::DeleteMailbox { .. } => write!(f, "Delete mailbox"),
            Self::RenameMailbox { new_path, .. } => write!(f, "Rename mailbox to {new_path}"),
            Self::SetMailboxPermissions { .. } => write!(f, "Set mailbox permissions"),
            Self::SetMailboxSubscription { .. } => write!(f, "Set mailbox subscription"),
        }
    }
}

impl MailboxJobRequest {
    pub fn cancel(&self) -> Option<StatusEvent> {
        match self {
            Self::Mailboxes { handle } => handle.cancel(),
            Self::CreateMailbox { handle, .. } => handle.cancel(),
            Self::DeleteMailbox { handle, .. } => handle.cancel(),
            Self::RenameMailbox { handle, .. } => handle.cancel(),
            Self::SetMailboxPermissions { handle, .. } => handle.cancel(),
            Self::SetMailboxSubscription { handle, .. } => handle.cancel(),
        }
    }
}

pub enum JobRequest {
    Fetch {
        mailbox_hash: MailboxHash,
        #[allow(clippy::type_complexity)]
        handle: JoinHandle<(
            Option<Result<Vec<Envelope>>>,
            BoxStream<'static, Result<Vec<Envelope>>>,
        )>,
    },
    Generic {
        name: Cow<'static, str>,
        log_level: LogLevel,
        handle: JoinHandle<Result<()>>,
        on_finish: Option<crate::types::CallbackFn>,
    },
    IsOnline {
        handle: JoinHandle<Result<()>>,
    },
    Refresh {
        mailbox_hash: MailboxHash,
        handle: JoinHandle<Result<()>>,
    },
    SetFlags {
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: Vec<FlagOp>,
        handle: JoinHandle<Result<()>>,
    },
    SaveMessage {
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        handle: JoinHandle<Result<()>>,
    },
    SendMessage,
    SendMessageBackground {
        handle: JoinHandle<Result<()>>,
    },
    DeleteMessages {
        env_hashes: EnvelopeHashBatch,
        handle: JoinHandle<Result<()>>,
    },
    Watch {
        #[allow(clippy::type_complexity)]
        handle: JoinHandle<(
            Option<Result<BackendEvent>>,
            BoxStream<'static, Result<BackendEvent>>,
        )>,
    },
    Mailbox(MailboxJobRequest),
}

impl std::fmt::Debug for JobRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Generic { name, .. } => write!(f, "JobRequest::Generic({name})"),
            Self::Mailbox(inner) => std::fmt::Debug::fmt(inner, f),
            Self::Fetch { mailbox_hash, .. } => {
                write!(f, "JobRequest::Fetch({mailbox_hash})")
            }
            Self::IsOnline { .. } => write!(f, "JobRequest::IsOnline"),
            Self::Refresh { .. } => write!(f, "JobRequest::Refresh"),
            Self::SetFlags {
                env_hashes,
                mailbox_hash,
                flags,
                ..
            } => f
                .debug_struct(stringify!(JobRequest::SetFlags))
                .field("env_hashes", &env_hashes)
                .field("mailbox_hash", &mailbox_hash)
                .field("flags", &flags)
                .finish(),
            Self::SaveMessage { .. } => write!(f, "JobRequest::SaveMessage"),
            Self::DeleteMessages { .. } => write!(f, "JobRequest::DeleteMessages"),
            Self::Watch { .. } => write!(f, "JobRequest::Watch"),
            Self::SendMessage => write!(f, "JobRequest::SendMessage"),
            Self::SendMessageBackground { .. } => {
                write!(f, "JobRequest::SendMessageBackground")
            }
        }
    }
}

impl std::fmt::Display for JobRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Generic { name, .. } => write!(f, "{name}"),
            Self::Mailbox(inner) => std::fmt::Display::fmt(inner, f),
            Self::Fetch { .. } => write!(f, "Mailbox fetch"),
            Self::IsOnline { .. } => write!(f, "Online status check"),
            Self::Refresh { .. } => write!(f, "Refresh mailbox"),
            Self::SetFlags {
                env_hashes, flags, ..
            } => write!(
                f,
                "Set flags for {} message{}: {:?}",
                env_hashes.len(),
                if env_hashes.len() == 1 { "" } else { "s" },
                flags
            ),
            Self::SaveMessage { .. } => write!(f, "Save message"),
            Self::DeleteMessages { env_hashes, .. } => write!(
                f,
                "Delete {} message{}",
                env_hashes.len(),
                if env_hashes.len() == 1 { "" } else { "s" }
            ),
            Self::Watch { .. } => write!(f, "Background watch"),
            Self::SendMessageBackground { .. } | Self::SendMessage => {
                write!(f, "Sending message")
            }
        }
    }
}

impl JobRequest {
    is_variant! { is_watch, Watch { .. } }
    is_variant! { is_online, IsOnline { .. } }
    is_variant! { is_any_fetch, Fetch { .. } }

    pub fn is_fetch(&self, mailbox_hash: MailboxHash) -> bool {
        matches!(self, Self::Fetch {
                 mailbox_hash: h, ..
             } if *h == mailbox_hash)
    }

    pub fn cancel(&self) -> Option<StatusEvent> {
        match self {
            Self::Generic { handle, .. } => handle.cancel(),
            Self::Mailbox(inner) => inner.cancel(),
            Self::Fetch { handle, .. } => handle.cancel(),
            Self::IsOnline { handle, .. } => handle.cancel(),
            Self::Refresh { handle, .. } => handle.cancel(),
            Self::SetFlags { handle, .. } => handle.cancel(),
            Self::SaveMessage { handle, .. } => handle.cancel(),
            Self::DeleteMessages { handle, .. } => handle.cancel(),
            Self::Watch { handle, .. } => handle.cancel(),
            Self::SendMessage => None,
            Self::SendMessageBackground { handle, .. } => handle.cancel(),
        }
    }
}
