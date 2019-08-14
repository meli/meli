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

use chan;
use std::fmt;
use std::sync::Arc;

#[derive(Clone)]
pub struct Work(pub Arc<Box<dyn Fn() -> () + Send + Sync>>);

impl Work {
    pub fn compute(&self) {
        (self.0)();
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
    tx: chan::Sender<AsyncStatus<T>>,
    rx: chan::Receiver<AsyncStatus<T>>,
}

#[derive(Debug, Clone)]
pub struct Async<T: Send + Sync> {
    value: Option<T>,
    work: Work,
    active: bool,
    tx: chan::Sender<AsyncStatus<T>>,
    rx: chan::Receiver<AsyncStatus<T>>,
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
        let (sender, receiver) = chan::sync(8 * ::std::mem::size_of::<AsyncStatus<T>>());
        AsyncBuilder {
            tx: sender,
            rx: receiver,
        }
    }
    /// Returns the sender object of the promise's channel.
    pub fn tx(&mut self) -> chan::Sender<AsyncStatus<T>> {
        self.tx.clone()
    }
    /// Returns the receiver object of the promise's channel.
    pub fn rx(&mut self) -> chan::Receiver<AsyncStatus<T>> {
        self.rx.clone()
    }
    /// Returns an `Async<T>` object that contains a `Thread` join handle that returns a `T`
    pub fn build(self, work: Box<dyn Fn() -> () + Send + Sync>) -> Async<T> {
        Async {
            work: Work(Arc::new(work)),
            value: None,
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
    /// Consumes `self` and returns the computed value. Will panic if computation hasn't finished.
    pub fn extract(self) -> T {
        self.value.unwrap()
    }
    pub fn work(&mut self) -> Option<Work> {
        if !self.active {
            self.active = true;
            Some(self.work.clone())
        } else {
            None
        }
    }
    /// Returns the sender object of the promise's channel.
    pub fn tx(&mut self) -> chan::Sender<AsyncStatus<T>> {
        self.tx.clone()
    }
    /// Polls worker thread and returns result.
    pub fn poll(&mut self) -> Result<AsyncStatus<T>, ()> {
        if self.value.is_some() {
            return Ok(AsyncStatus::Finished);
        }
        //self.tx.send(true);
        let rx = &self.rx;
        let result: T;
        chan_select! {
            default => {
                return Ok(AsyncStatus::NoUpdate);
            },
            rx.recv() -> r => {
                match r {
                    Some(AsyncStatus::Payload(payload)) => {
                        result = payload;
                    },
                    Some(a) => {
                        return Ok(a);
                    }
                    _ => {
                        return Err(());
                    },
                }
            },
        };
        self.value = Some(result);
        Ok(AsyncStatus::Finished)
    }
    /// Blocks until thread joins.
    pub fn join(&mut self) {
        let result: T;
        let rx = &self.rx;
        loop {
            chan_select! {
                rx.recv() -> r => {
                    match r {
                        Some(AsyncStatus::Payload(payload)) => {
                            result = payload;
                            break;
                        },
                        _ => continue,
                    }
                }

            }
        }
        self.value = Some(result);
    }
}
