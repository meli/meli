/*
 * meli - async module
 *
 * Copyright 2017 Manos Pitsidianakis
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
 * Primitive Async/Wait implementation.
 *
 * To create an Async promise, create an AsyncBuilder. Ask for its channel receiver/sender with
 * `tx` and `rx` methods to pass them in your worker's closure. Build an `Async<T>` with your
 * `JoinHandle<T>`. The thread must communicate with the `Async<T>` object via `AsyncStatus`
 * messages.
 *
 * When `Async<T>` receives `AsyncStatus::Finished` it joins the thread and takes its value which
 * can be extracted with `extract`.
 */

use crossbeam::{
    bounded,
    channel::{Receiver, Sender},
    select,
};
use std::fmt;

#[derive(Clone, Debug)]
pub struct WorkContext {
    pub new_work: Sender<Work>,
    pub set_name: Sender<(std::thread::ThreadId, String)>,
    pub set_status: Sender<(std::thread::ThreadId, String)>,
    pub finished: Sender<std::thread::ThreadId>,
}

pub struct Work {
    priority: u64,
    pub is_static: bool,
    pub closure: Box<dyn FnOnce(WorkContext) -> () + Send + Sync>,
}

impl Ord for Work {
    fn cmp(&self, other: &Work) -> std::cmp::Ordering {
        self.priority.cmp(&other.priority)
    }
}

impl PartialOrd for Work {
    fn partial_cmp(&self, other: &Work) -> Option<std::cmp::Ordering> {
        Some(self.priority.cmp(&other.priority))
    }
}

impl PartialEq for Work {
    fn eq(&self, other: &Work) -> bool {
        self.priority == other.priority
    }
}

impl Eq for Work {}

impl Work {
    pub fn compute(self, work_context: WorkContext) {
        (self.closure)(work_context);
    }
}

impl fmt::Debug for Work {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Work object")
    }
}

/// Messages to pass between `Async<T>` owner and its worker thread.
#[derive(Clone)]
pub enum AsyncStatus<T> {
    NoUpdate,
    Payload(T),
    Finished,
    ///The number may hold whatever meaning the user chooses.
    ProgressReport(usize),
}

impl<T> fmt::Debug for AsyncStatus<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AsyncStatus::NoUpdate => write!(f, "AsyncStatus<T>::NoUpdate"),
            AsyncStatus::Payload(_) => write!(f, "AsyncStatus<T>::Payload(_)"),
            AsyncStatus::Finished => write!(f, "AsyncStatus<T>::Finished"),
            AsyncStatus::ProgressReport(u) => write!(f, "AsyncStatus<T>::ProgressReport({})", u),
        }
    }
}

/// A builder object for `Async<T>`
#[derive(Debug, Clone)]
pub struct AsyncBuilder<T: Send + Sync> {
    tx: Sender<AsyncStatus<T>>,
    rx: Receiver<AsyncStatus<T>>,
    priority: u64,
    is_static: bool,
}

#[derive(Debug)]
pub struct Async<T: Send + Sync> {
    work: Option<Work>,
    active: bool,
    tx: Sender<AsyncStatus<T>>,
    rx: Receiver<AsyncStatus<T>>,
}

impl<T: Send + Sync> Default for AsyncBuilder<T> {
    fn default() -> Self {
        AsyncBuilder::<T>::new()
    }
}

impl<T> AsyncBuilder<T>
where
    T: Send + Sync,
{
    pub fn new() -> Self {
        let (sender, receiver) = bounded(8 * ::std::mem::size_of::<AsyncStatus<T>>());
        AsyncBuilder {
            tx: sender,
            rx: receiver,
            priority: 0,
            is_static: false,
        }
    }
    /// Returns the sender object of the promise's channel.
    pub fn tx(&mut self) -> Sender<AsyncStatus<T>> {
        self.tx.clone()
    }
    /// Returns the receiver object of the promise's channel.
    pub fn rx(&mut self) -> Receiver<AsyncStatus<T>> {
        self.rx.clone()
    }

    pub fn set_priority(&mut self, new_val: u64) -> &mut Self {
        self.priority = new_val;
        self
    }

    pub fn set_is_static(&mut self, new_val: bool) -> &mut Self {
        self.is_static = new_val;
        self
    }

    /// Returns an `Async<T>` object that contains a `Thread` join handle that returns a `T`
    pub fn build(self, work: Box<dyn FnOnce(WorkContext) -> () + Send + Sync>) -> Async<T> {
        Async {
            work: Some(Work {
                priority: self.priority,
                is_static: self.is_static,
                closure: work,
            }),
            tx: self.tx,
            rx: self.rx,
            active: false,
        }
    }
}

impl<T> Async<T>
where
    T: Send + Sync,
{
    pub fn work(&mut self) -> Option<Work> {
        if !self.active {
            self.active = true;
            self.work.take()
        } else {
            None
        }
    }
    /// Returns the sender object of the promise's channel.
    pub fn tx(&mut self) -> Sender<AsyncStatus<T>> {
        self.tx.clone()
    }
    /// Returns the receiver object of the promise's channel.
    pub fn rx(&mut self) -> Receiver<AsyncStatus<T>> {
        self.rx.clone()
    }
    /// Polls worker thread and returns result.
    pub fn poll_block(&mut self) -> Result<AsyncStatus<T>, ()> {
        if !self.active {
            return Ok(AsyncStatus::Finished);
        }

        let rx = &self.rx;
        select! {
            recv(rx) -> r => {
                match r {
                    Ok(p @ AsyncStatus::Payload(_)) => {
                        return Ok(p);
                    },
                    Ok(f @ AsyncStatus::Finished) => {
                        self.active = false;
                        return Ok(f);
                    },
                    Ok(a) => {
                        return Ok(a);
                    }
                    Err(_) => {
                        return Err(());
                    },
                }
            },
        };
    }
    /// Polls worker thread and returns result.
    pub fn poll(&mut self) -> Result<AsyncStatus<T>, ()> {
        if !self.active {
            return Ok(AsyncStatus::Finished);
        }

        let rx = &self.rx;
        select! {
            default => {
                return Ok(AsyncStatus::NoUpdate);
            },
            recv(rx) -> r => {
                match r {
                    Ok(p @ AsyncStatus::Payload(_)) => {
                        return Ok(p);
                    },
                    Ok(f @ AsyncStatus::Finished) => {
                        self.active = false;
                        return Ok(f);
                    },
                    Ok(a) => {
                        return Ok(a);
                    }
                    Err(_) => {
                        return Err(());
                    },
                }
            },
        };
    }
}
