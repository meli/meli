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

pub mod accounts;
pub use self::accounts::Account;
#[macro_use]
mod position;
#[macro_use]
mod cells;
#[macro_use]
mod helpers;
#[macro_use]
mod keys;
pub use self::cells::*;
pub use self::helpers::*;
pub use self::keys::*;
pub use self::position::*;

use super::execute::Action;

use melib::RefreshEvent;
use std;
use std::fmt;
use std::thread;
use uuid::Uuid;

#[derive(Debug)]
pub enum StatusEvent {
    DisplayMessage(String),
    BufClear,
    BufSet(String),
}

/// `ThreadEvent` encapsulates all of the possible values we need to transfer between our threads
/// to the main process.
#[derive(Debug)]
pub enum ThreadEvent {
    ThreadJoin(thread::ThreadId),
    /// User input.
    Input(Key),
    /// A watched folder has been refreshed.
    RefreshMailbox(Box<RefreshEvent>),
    UIEvent(UIEventType),
    //Decode { _ }, // For gpg2 signature check
}

impl From<RefreshEvent> for ThreadEvent {
    fn from(event: RefreshEvent) -> Self {
        ThreadEvent::RefreshMailbox(Box::new(event))
    }
}

#[derive(Debug)]
pub enum ForkType {
    Finished, // Already finished fork, we only want to restore input/output
    Generic(std::process::Child),
    NewDraft(File, std::process::Child),
}

#[derive(Debug)]
pub enum UIEventType {
    Input(Key),
    ExInput(Key),
    RefreshMailbox((usize, usize)),
    //Quit?
    Resize,
    /// Force redraw.
    Fork(ForkType),
    ChangeMailbox(usize),
    ChangeMode(UIMode),
    Command(String),
    Notification(Option<String>, String),
    Action(Action),
    StatusEvent(StatusEvent),
    MailboxUpdate((usize, usize)), // (account_idx, mailbox_idx)
    EntityKill(Uuid),
    StartupCheck,
    RefreshEvent(Box<RefreshEvent>),
}

impl From<RefreshEvent> for UIEvent {
    fn from(event: RefreshEvent) -> Self {
        UIEvent {
            id: 0,
            event_type: UIEventType::RefreshEvent(Box::new(event)),
        }
    }
}

/// An event passed from `State` to its Entities.
#[derive(Debug)]
pub struct UIEvent {
    pub id: u64,
    pub event_type: UIEventType,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UIMode {
    Normal,
    Execute,
    Fork,
}

impl fmt::Display for UIMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                UIMode::Normal => "NORMAL",
                UIMode::Execute => "EX",
                UIMode::Fork => "FORK",
            }
        )
    }
}

/// An event notification that is passed to Entities for handling.
pub struct Notification {
    _title: String,
    _content: String,

    _timestamp: std::time::Instant,
}
