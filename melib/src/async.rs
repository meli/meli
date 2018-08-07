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
use std::thread;

/// Messages to pass between `Async<T>` owner and its worker thread.
#[derive(Debug)]
pub enum AsyncStatus {
    NoUpdate,
    Finished,
    ///The number may hold whatever meaning the user chooses.
    ProgressReport(usize),
}

/// A builder object for `Async<T>`
#[derive(Debug)]
pub struct AsyncBuilder {
    tx: chan::Sender<AsyncStatus>,
    rx: chan::Receiver<AsyncStatus>,
}

#[derive(Debug)]
pub struct Async<T> {
    value: Option<T>,
    worker: Option<thread::JoinHandle<T>>,
    tx: chan::Sender<AsyncStatus>,
    rx: chan::Receiver<AsyncStatus>,
}

impl AsyncBuilder {
    pub fn new() -> Self {
        let (sender, receiver) = chan::sync(::std::mem::size_of::<AsyncStatus>());
        AsyncBuilder {
            tx: sender,
            rx: receiver,
        }
    }
    /// Returns the sender object of the promise's channel.
    pub fn tx(&mut self) -> chan::Sender<AsyncStatus> {
        self.tx.clone()
    }
    /// Returns the receiver object of the promise's channel.
    pub fn rx(&mut self) -> chan::Receiver<AsyncStatus> {
        self.rx.clone()
    }
    /// Returns an `Async<T>` object that contains a `Thread` join handle that returns a `T`
    pub fn build<T: Clone>(self, worker: thread::JoinHandle<T>) -> Async<T> {
        Async {
            worker: Some(worker),
            value: None,
            tx: self.tx,
            rx: self.rx,
        }
    }
}

impl<T> Async<T> {
    /// Consumes `self` and returns the computed value. Will panic if computation hasn't finished.
    pub fn extract(self) -> T {
        self.value.unwrap()
    }
    /// Polls worker thread and returns result.
    pub fn poll(&mut self) -> Result<AsyncStatus, ()> {
        if self.value.is_some() {
            return Ok(AsyncStatus::Finished);
        }
        //self.tx.send(true);
        let rx = &self.rx;
        chan_select! {
            default => {
                return Ok(AsyncStatus::NoUpdate);
            },
            rx.recv() -> r => {
                match r {
                    Some(AsyncStatus::Finished) => {
                    },
                    Some(a) => {
                        return Ok(a);
                    }
                    _ => {
                        return Err(());
                    },
                }

            },
        }
        let v = self.worker.take().unwrap().join().unwrap();
        self.value = Some(v);
        return Ok(AsyncStatus::Finished);
    }
}
