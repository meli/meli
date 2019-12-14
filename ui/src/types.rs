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
use melib::{EnvelopeHash, RefreshEvent};
use nix::unistd::Pid;
use std;
use std::fmt;
use std::thread;
use uuid::Uuid;

#[derive(Debug)]
pub enum StatusEvent {
    DisplayMessage(String),
    BufClear,
    BufSet(String),
    UpdateStatus(String),
}

/// `ThreadEvent` encapsulates all of the possible values we need to transfer between our threads
/// to the main process.
#[derive(Debug)]
pub enum ThreadEvent {
    NewThread(thread::ThreadId, String),
    /// User input.
    Input(Key),
    /// User input and input as raw bytes.
    InputRaw((Key, Vec<u8>)),
    /// A watched folder has been refreshed.
    RefreshMailbox(Box<RefreshEvent>),
    UIEvent(UIEvent),
    /// A thread has updated some of its information
    Pulse,
    //Decode { _ }, // For gpg2 signature check
}

impl From<RefreshEvent> for ThreadEvent {
    fn from(event: RefreshEvent) -> Self {
        ThreadEvent::RefreshMailbox(Box::new(event))
    }
}

#[derive(Debug)]
pub enum ForkType {
    /// Already finished fork, we only want to restore input/output
    Finished,
    /// Embed pty
    Embed(Pid),
    Generic(std::process::Child),
    NewDraft(File, std::process::Child),
}

#[derive(Debug)]
pub enum NotificationType {
    INFO,
    ERROR,
    NewMail,
}

#[derive(Debug)]
pub enum UIEvent {
    Input(Key),
    ExInput(Key),
    InsertInput(Key),
    EmbedInput((Key, Vec<u8>)),
    RefreshMailbox((usize, FolderHash)), //view has changed to FolderHash mailbox
    //Quit?
    Resize,
    /// Force redraw.
    Fork(ForkType),
    ChangeMailbox(usize),
    ChangeMode(UIMode),
    Command(String),
    Notification(Option<String>, String, Option<NotificationType>),
    Action(Action),
    StatusEvent(StatusEvent),
    MailboxUpdate((usize, FolderHash)), // (account_idx, mailbox_idx)
    ComponentKill(Uuid),
    WorkerProgress(FolderHash),
    StartupCheck(FolderHash),
    RefreshEvent(Box<RefreshEvent>),
    EnvelopeUpdate(EnvelopeHash),
    EnvelopeRename(EnvelopeHash, EnvelopeHash), // old_hash, new_hash
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
    /// Forward input to an embed pseudoterminal.
    Embed,
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
                UIMode::Embed => "EMBED",
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

pub mod segment_tree {
    /*! Simple segment tree implementation for maximum in range queries. This is useful if given an
     *  array of numbers you want to get the maximum value inside an interval quickly.
     */
    use melib::StackVec;
    use std::convert::TryFrom;
    use std::iter::FromIterator;

    #[derive(Default, Debug, Clone)]
    pub struct SegmentTree {
        array: StackVec<u8>,
        tree: StackVec<u8>,
    }

    impl From<StackVec<u8>> for SegmentTree {
        fn from(val: StackVec<u8>) -> SegmentTree {
            SegmentTree::new(val)
        }
    }

    impl SegmentTree {
        pub fn new(val: StackVec<u8>) -> SegmentTree {
            if val.is_empty() {
                return SegmentTree {
                    array: val.clone(),
                    tree: val,
                };
            }

            let height = (f64::from(u32::try_from(val.len()).unwrap_or(0)))
                .log2()
                .ceil() as u32;
            let max_size = 2 * (2_usize.pow(height)) - 1;

            let mut segment_tree: StackVec<u8> =
                StackVec::from_iter(core::iter::repeat(0).take(max_size));
            for i in 0..val.len() {
                segment_tree[val.len() + i] = val[i];
            }

            for i in (1..val.len()).rev() {
                segment_tree[i] = std::cmp::max(segment_tree[2 * i], segment_tree[2 * i + 1]);
            }

            SegmentTree {
                array: val,
                tree: segment_tree,
            }
        }

        /// (left, right) is inclusive
        pub fn get_max(&self, mut left: usize, mut right: usize) -> u8 {
            let len = self.array.len();
            debug_assert!(left <= right);
            if right >= len {
                right = len.saturating_sub(1);
            }

            left += len;
            right += len + 1;

            let mut max = 0;

            while left < right {
                if (left & 1) > 0 {
                    max = std::cmp::max(max, self.tree[left]);
                    left += 1;
                }

                if (right & 1) > 0 {
                    right -= 1;
                    max = std::cmp::max(max, self.tree[right]);
                }

                left /= 2;
                right /= 2;
            }
            max
        }
    }

    #[test]
    fn test_segment_tree() {
        let array: StackVec<u8> = [9, 1, 17, 2, 3, 23, 4, 5, 6, 37]
            .into_iter()
            .cloned()
            .collect::<StackVec<u8>>();
        let segment_tree = SegmentTree::from(array.clone());

        assert_eq!(segment_tree.get_max(0, 5), 23);
        assert_eq!(segment_tree.get_max(6, 9), 37);
    }
}
