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
extern crate serde;
#[macro_use]
mod helpers;
pub use self::helpers::*;

use super::execute::Action;
use super::terminal::*;

use melib::backends::FolderHash;
use melib::EnvelopeHash;
use melib::RefreshEvent;
use std;
use std::fmt;
use std::ops::Index;
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
    UIEvent(UIEvent),
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
pub enum UIEvent {
    Input(Key),
    ExInput(Key),
    InsertInput(Key),
    RefreshMailbox((usize, FolderHash)), //view has changed to FolderHash mailbox
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
    MailboxUpdate((usize, FolderHash)), // (account_idx, mailbox_idx)
    ComponentKill(Uuid),
    StartupCheck(FolderHash),
    RefreshEvent(Box<RefreshEvent>),
    EnvelopeUpdate(EnvelopeHash),
    EnvelopeRename(FolderHash, EnvelopeHash, EnvelopeHash),
    EnvelopeRemove(EnvelopeHash),
}

impl From<RefreshEvent> for UIEvent {
    fn from(event: RefreshEvent) -> Self {
        UIEvent::RefreshEvent(Box::new(event))
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UIMode {
    Normal,
    Insert,
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
                UIMode::Insert => "INSERT",
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

#[derive(Debug)]
pub(crate) struct StackVec<T: Default + Copy + std::fmt::Debug> {
    len: usize,
    array: [T; 8],
    heap_vec: Vec<T>,
}

impl<T: Default + Copy + std::fmt::Debug> StackVec<T> {
    pub(crate) fn new() -> Self {
        StackVec {
            len: 0,
            array: [T::default(); 8],
            heap_vec: Vec::new(),
        }
    }
    pub(crate) fn push(&mut self, ind: T) {
        if self.len == self.array.len() {
            if self.heap_vec.is_empty() {
                self.heap_vec.reserve(16);
                for _ in 0..8 {
                    self.heap_vec.push(T::default());
                }
            }
            self.heap_vec[0..8].copy_from_slice(&self.array);
            self.heap_vec.push(ind);
        } else if self.len > self.array.len() {
            self.heap_vec.push(ind);
        } else {
            self.array[self.len] = ind;
        }
        self.len += 1;
    }
    pub(crate) fn pop(&mut self) -> Option<T> {
        if self.len == 0 {
            return None;
        }
        if self.len >= self.array.len() {
            self.len -= 1;
            self.heap_vec.pop()
        } else {
            let ret = self.array[self.len - 1];
            self.len = self.len - 1;
            Some(ret)
        }
    }
    pub(crate) fn len(&self) -> usize {
        self.len
    }
    pub(crate) fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl<T: Default + Copy + std::fmt::Debug> Index<usize> for StackVec<T> {
    type Output = T;

    fn index(&self, idx: usize) -> &T {
        if self.len >= self.array.len() {
            &self.heap_vec[idx]
        } else {
            &self.array[idx]
        }
    }
}
