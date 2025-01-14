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

//! UI types used throughout meli.
//!
//! The [`segment_tree`] module performs maximum range queries.
//! This is used in getting the maximum element of a column within a specific
//! range in e-mail lists.
//! That way a very large value that is not the in the currently displayed page
//! does not cause the column to be rendered bigger than it has to.
//!
//! [`UIMode`] describes the application's... mode. Same as in the modal editor
//! `vi`.
//!
//! [`UIEvent`] is the type passed around
//! [`Component`]'s when something happens.

use std::{borrow::Cow, sync::Arc};

use indexmap::IndexMap;
use melib::{
    backends::{AccountHash, BackendEvent, MailboxHash},
    error::{Error, Result, ResultIntoError},
    log, EnvelopeHash, RefreshEvent, RefreshEventKind, ThreadHash,
};
use nix::{errno::Errno, unistd::Pid};

use super::{
    command::Action,
    jobs::{JobExecutor, JobId, TimerId},
    terminal::*,
};
use crate::components::{Component, ComponentId, ScrollUpdate};

#[macro_use]
mod helpers;
pub use helpers::*;

pub type UIMessage = Box<dyn 'static + std::any::Any + Send + Sync>;

#[derive(Debug)]
pub enum StatusEvent {
    DisplayMessage(String),
    BufClear,
    BufSet(String),
    UpdateStatus(String),
    UpdateSubStatus(String),
    NewJob(JobId),
    JobFinished(JobId),
    JobCanceled(JobId),
    SetMouse(bool),
    ScrollUpdate(ScrollUpdate),
}

/// [`ThreadEvent`] encapsulates all of the possible values we need to transfer
/// between our threads to the main process.
#[derive(Debug)]
pub enum ThreadEvent {
    /// User input and input as raw bytes.
    Input((Key, Vec<u8>)),
    /// A mailbox has changed state.
    MailboxChanges {
        mailbox_hash: MailboxHash,
        account_hash: AccountHash,
        events: Vec<RefreshEventKind>,
    },
    UIEvent(UIEvent),
    /// A thread has updated some of its information
    Pulse,
    JobFinished(JobId),
}

impl ThreadEvent {
    pub fn from_refresh_events<I: std::iter::ExactSizeIterator<Item = RefreshEvent>>(
        events: I,
    ) -> impl Iterator<Item = Self> {
        let events_len = events.len();
        events
            .fold(
                IndexMap::<(AccountHash, MailboxHash), Vec<RefreshEventKind>>::with_capacity(
                    events_len,
                ),
                |mut accum,
                 RefreshEvent {
                     mailbox_hash,
                     account_hash,
                     kind,
                 }| {
                    accum
                        .entry((account_hash, mailbox_hash))
                        .or_default()
                        .push(kind);
                    accum
                },
            )
            .into_iter()
            .map(
                |((account_hash, mailbox_hash), events)| Self::MailboxChanges {
                    account_hash,
                    mailbox_hash,
                    events,
                },
            )
    }
}

impl From<UIEvent> for ThreadEvent {
    fn from(event: UIEvent) -> Self {
        Self::UIEvent(event)
    }
}

#[derive(Debug)]
pub enum ForkedProcess {
    /// Embedded pty
    Embedded {
        id: Cow<'static, str>,
        command: Option<Cow<'static, str>>,
        pid: Pid,
    },
    Generic {
        id: Cow<'static, str>,
        command: Option<Cow<'static, str>>,
        child: std::process::Child,
    },
}

impl ForkedProcess {
    /// Return the process's string identifier.
    #[inline]
    pub fn id(&self) -> &str {
        let (Self::Embedded { id, .. } | Self::Generic { id, .. }) = self;
        id.as_ref()
    }

    /// Return the process's ID.
    pub fn pid(&self) -> Pid {
        match self {
            Self::Embedded { pid, .. } => *pid,
            Self::Generic { child, .. } => nix::unistd::Pid::from_raw(child.id() as libc::pid_t),
        }
    }

    /// Attempt to kill process.
    pub fn kill(&mut self) -> Result<()> {
        match self {
            Self::Generic {
                ref id,
                ref command,
                ref mut child,
            } => {
                let w = child.kill();
                match w {
                    Ok(()) => {
                        log::debug!(
                            "child process {} {} ({}) was killed successfully",
                            id,
                            command
                                .as_deref()
                                .map(Cow::Borrowed)
                                .unwrap_or(Cow::Borrowed("<command unknown>")),
                            child.id(),
                        );
                        Ok(())
                    }
                    err @ Err(_) => err.chain_err_details(|| {
                        format!(
                            "Failed to kill child process {} {} ({})",
                            id,
                            command
                                .as_deref()
                                .map(Cow::Borrowed)
                                .unwrap_or(Cow::Borrowed("<command unknown>")),
                            child.id(),
                        )
                    }),
                }
            }
            Self::Embedded {
                ref id,
                ref command,
                ref pid,
            } => {
                use nix::sys::signal::{kill, Signal};
                match kill(*pid, Some(Signal::SIGTERM)) {
                    Ok(()) | Err(Errno::ESRCH) => Ok(()),
                    err @ Err(_) => err.chain_err_details(|| {
                        format!(
                            "Failed to SIGTERM embedded child process {} {} ({})",
                            id,
                            command
                                .as_deref()
                                .map(Cow::Borrowed)
                                .unwrap_or(Cow::Borrowed("<command unknown>")),
                            pid,
                        )
                    }),
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NotificationType {
    Info,
    Error(melib::error::ErrorKind),
    NewMail,
    SentMail,
    Saved,
}

impl std::fmt::Display for NotificationType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Self::Info => write!(f, "info"),
            Self::Error(melib::error::ErrorKind::None) => write!(f, "error"),
            Self::Error(kind) => write!(f, "error: {}", kind),
            Self::NewMail => write!(f, "new mail"),
            Self::SentMail => write!(f, "sent mail"),
            Self::Saved => write!(f, "saved"),
        }
    }
}

#[derive(Debug)]
pub enum UIEvent {
    Input(Key),
    CmdInput(Key),
    InsertInput(Key),
    EmbeddedInput((Key, Vec<u8>)),
    Resize,
    Fork(ForkedProcess),
    /// Restore input/output, useful after forking to something that captures
    /// stdin/stdout.
    RestoreStandardIO,
    ChangeMailbox(usize),
    ChangeMode(UIMode),
    Command(String),
    Notification {
        title: Option<Cow<'static, str>>,
        body: Cow<'static, str>,
        source: Option<Error>,
        kind: Option<NotificationType>,
    },
    Action(Action),
    StatusEvent(StatusEvent),
    MailboxUpdate((AccountHash, MailboxHash)), // (account_idx, mailbox_idx)
    MailboxDelete((AccountHash, MailboxHash)),
    MailboxCreate((AccountHash, MailboxHash)),
    AccountStatusChange(AccountHash, Option<Cow<'static, str>>),
    ComponentUnrealize(ComponentId),
    BackendEvent(AccountHash, BackendEvent),
    StartupCheck(MailboxHash),
    RefreshEvent(Box<RefreshEvent>),
    EnvelopeUpdate(EnvelopeHash),
    EnvelopeRename(EnvelopeHash, EnvelopeHash), // old_hash, new_hash
    EnvelopeRemove(EnvelopeHash, ThreadHash),
    Contacts(ContactEvent),
    Compose(ComposeEvent),
    FinishedUIDialog(ComponentId, UIMessage),
    IntraComm {
        from: ComponentId,
        to: ComponentId,
        content: UIMessage,
    },
    Callback(CallbackFn),
    GlobalUIDialog {
        value: Box<dyn Component>,
        parent: Option<ComponentId>,
    },
    Timer(TimerId),
    ConfigReload {
        old_settings: Box<crate::conf::Settings>,
    },
    VisibilityChange(bool),
}

pub struct CallbackFn(pub Box<dyn FnOnce(&mut crate::Context) + Send + 'static>);

impl std::fmt::Debug for CallbackFn {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(melib::identify!(CallbackFn)).finish()
    }
}

impl From<RefreshEvent> for UIEvent {
    fn from(event: RefreshEvent) -> Self {
        Self::RefreshEvent(Box::new(event))
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UIMode {
    Normal,
    Insert,
    /// Forward input to an embedded pseudoterminal.
    Embedded,
    Command,
    Fork,
}

impl std::fmt::Display for UIMode {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Self::Normal => "NORMAL",
                Self::Insert => "INSERT",
                Self::Command => "COMMAND",
                Self::Fork => "FORK",
                Self::Embedded => "EMBEDDED",
            }
        )
    }
}

pub mod segment_tree {
    //! Simple segment tree implementation for maximum in range queries. This is
    //! useful if given an array of numbers you want to get the maximum
    //! value inside an interval quickly.

    use std::{convert::TryFrom, iter::FromIterator};

    use smallvec::SmallVec;

    #[derive(Clone, Debug, Default)]
    pub struct SegmentTree {
        pub array: SmallVec<[u8; 1024]>,
        tree: SmallVec<[u8; 1024]>,
    }

    impl From<SmallVec<[u8; 1024]>> for SegmentTree {
        fn from(val: SmallVec<[u8; 1024]>) -> Self {
            Self::new(val)
        }
    }

    impl SegmentTree {
        pub fn new(val: SmallVec<[u8; 1024]>) -> Self {
            if val.is_empty() {
                return Self {
                    array: val.clone(),
                    tree: val,
                };
            }

            let height = (f64::from(u32::try_from(val.len()).unwrap_or(0)))
                .log2()
                .ceil() as u32;
            let max_size = 2 * (2_usize.pow(height));

            let mut segment_tree: SmallVec<[u8; 1024]> =
                SmallVec::from_iter(std::iter::repeat(0).take(max_size));
            for i in 0..val.len() {
                segment_tree[val.len() + i] = val[i];
            }

            for i in (1..val.len()).rev() {
                segment_tree[i] = std::cmp::max(segment_tree[2 * i], segment_tree[2 * i + 1]);
            }

            Self {
                array: val,
                tree: segment_tree,
            }
        }

        /// (left, right) is inclusive
        pub fn get_max(&self, mut left: usize, mut right: usize) -> u8 {
            if self.array.is_empty() {
                return 0;
            }

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

        pub fn update(&mut self, pos: usize, value: u8) {
            let mut ctr = pos + self.array.len();

            // Update leaf node value
            self.tree[ctr] = value;
            while ctr > 1 {
                // move up one level
                ctr >>= 1;

                self.tree[ctr] = std::cmp::max(self.tree[2 * ctr], self.tree[2 * ctr + 1]);
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_segment_tree() {
            let array: SmallVec<[u8; 1024]> = [9, 1, 17, 2, 3, 23, 4, 5, 6, 37]
                .iter()
                .cloned()
                .collect::<SmallVec<[u8; 1024]>>();
            let mut segment_tree = SegmentTree::from(array);

            assert_eq!(segment_tree.get_max(0, 5), 23);
            assert_eq!(segment_tree.get_max(6, 9), 37);

            segment_tree.update(2_usize, 24_u8);

            assert_eq!(segment_tree.get_max(0, 5), 24);
        }
    }
}

#[derive(Debug)]
pub struct RateLimit {
    last_tick: std::time::Instant,
    pub timer: crate::jobs::Timer,
    rate: std::time::Duration,
    pub active: bool,
}

impl RateLimit {
    pub fn new(reqs: u64, millis: u64, job_executor: Arc<JobExecutor>) -> Self {
        Self {
            last_tick: std::time::Instant::now(),
            timer: job_executor.create_timer(
                std::time::Duration::from_secs(0),
                std::time::Duration::from_millis(millis),
            ),
            rate: std::time::Duration::from_millis(millis / reqs),
            active: false,
        }
    }

    pub fn reset(&mut self) {
        self.last_tick = std::time::Instant::now();
        self.active = false;
    }

    pub fn tick(&mut self) -> bool {
        let now = std::time::Instant::now();
        if self.last_tick + self.rate > now {
            self.active = false;
        } else {
            self.timer.rearm();
            self.last_tick = now;
            self.active = true;
        }
        self.active
    }

    #[inline(always)]
    pub fn id(&self) -> TimerId {
        self.timer.id()
    }
}

#[derive(Debug)]
pub enum ContactEvent {
    CreateContacts(Vec<melib::Card>),
}

#[derive(Debug)]
pub enum ComposeEvent {
    SetRecipients(Vec<melib::Address>),
}

#[cfg(test)]
mod tests {
    //use super::*;

    #[test]
    fn test_rate_limit() {
        /*
           let (sender, receiver) =
           crossbeam::channel::bounded(4096 * ::std::mem::size_of::<ThreadEvent>());
           use std::sync::Arc;
           let job_executor = Arc::new(JobExecutor::new(sender));
        /* Accept at most one request per 3 milliseconds */
        let mut rt = RateLimit::new(1, 3, job_executor.clone());
        std::thread::sleep(std::time::Duration::from_millis(2000));
        /* assert that only one request per 3 milliseconds is accepted */
        for _ in 0..5 {
            assert!(rt.tick());
            std::thread::sleep(std::time::Duration::from_millis(1));
            assert!(!rt.tick());
            std::thread::sleep(std::time::Duration::from_millis(1));
            assert!(!rt.tick());
            std::thread::sleep(std::time::Duration::from_millis(1));
            /* How many times was the signal handler called? We've slept for at least 3
             * milliseconds, so it should have been called once */
            let mut ctr = 0;
            while receiver.try_recv().is_ok() {
                ctr += 1;
                println!("got {}", ctr);
            }
            println!("ctr =  {} {}", ctr, ctr == 1);
            assert_eq!(ctr, 1);
        }
        /* next, test at most 100 requests per second */
        let mut rt = RateLimit::new(100, 1000, job_executor.clone());
        for _ in 0..5 {
            let mut ctr = 0;
            for _ in 0..500 {
                if rt.tick() {
                    ctr += 1;
                }
                std::thread::sleep(std::time::Duration::from_millis(2));
            }
            /* around 100 requests should succeed. might be 99 if in first loop, since
             * RateLimit::new() has a delay */
            assert!(ctr > 97 && ctr < 103);
            /* alarm should expire in 1 second */
            std::thread::sleep(std::time::Duration::from_millis(1000));
            /* How many times was the signal handler called? */
            ctr = 0;
            while receiver.try_recv().is_ok() {
                ctr += 1;
            }
            assert_eq!(ctr, 1);
        }
        /* next, test at most 500 requests per second */
        let mut rt = RateLimit::new(500, 1000, job_executor.clone());
        for _ in 0..5 {
            let mut ctr = 0;
            for _ in 0..500 {
                if rt.tick() {
                    ctr += 1;
                }
                std::thread::sleep(std::time::Duration::from_millis(2));
            }
            /* all requests should succeed.  */
            assert!(ctr < 503 && ctr > 497);
            /* alarm should expire in 1 second */
            std::thread::sleep(std::time::Duration::from_millis(1000));
            /* How many times was the signal handler called? */
            ctr = 0;
            while receiver.try_recv().is_ok() {
                ctr += 1;
            }
            assert_eq!(ctr, 1);
        }
        */
    }
}
