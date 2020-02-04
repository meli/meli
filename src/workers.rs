/*
 * meli
 *
 * Copyright 2017-2020 Manos Pitsidianakis
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

/*! Simple blocking job control.
 */
use crate::types::ThreadEvent;
use crossbeam::{
    channel::{bounded, unbounded, Sender},
    select,
};
use fnv::FnvHashMap;
use melib::async_workers::{Work, WorkContext};
use melib::text_processing::Truncate;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;

const MAX_WORKER: usize = 4;

/// Representation of a worker thread for use in `WorkController`. These values are to be displayed
/// to the user.
#[derive(Debug)]
pub struct Worker {
    pub name: String,
    pub status: String,
}

impl From<String> for Worker {
    fn from(val: String) -> Self {
        Worker {
            name: val,
            status: String::new(),
        }
    }
}

pub struct WorkController {
    pub queue: WorkQueue,
    thread_end_tx: Sender<bool>,
    /// Worker threads that take up on jobs from self.queue
    pub threads: Arc<Mutex<FnvHashMap<thread::ThreadId, Worker>>>,
    /// Special function threads that live indefinitely (eg watching a mailbox)
    pub static_threads: Arc<Mutex<FnvHashMap<thread::ThreadId, Worker>>>,
    work_context: WorkContext,
}

impl Drop for WorkController {
    fn drop(&mut self) {
        for _ in 0..self.threads.lock().unwrap().len() {
            self.thread_end_tx.send(true).unwrap();
        }
    }
}

#[derive(Clone)]
pub struct WorkQueue {
    inner: Arc<Mutex<Vec<Work>>>,
    new_jobs_tx: Sender<bool>,
    work_context: WorkContext,
}

impl WorkQueue {
    fn new(new_jobs_tx: Sender<bool>, work_context: WorkContext) -> Self {
        Self {
            inner: Arc::new(Mutex::new(Vec::new())),
            new_jobs_tx,
            work_context,
        }
    }

    /// Blocks the current thread until work is available, then
    /// gets the data required to perform that work.
    ///
    /// # Errors
    /// Returns None if there is no more work in the queue.
    ///
    /// # Panics
    /// Panics if the underlying mutex became poisoned. This is exceedingly
    /// unlikely.
    fn get_work(&self) -> Option<Work> {
        // try to get a lock on the mutex.
        let maybe_queue = self.inner.lock();
        if let Ok(mut queue) = maybe_queue {
            if queue.is_empty() {
                return None;
            } else {
                return Some(queue.swap_remove(0));
            }
        } else {
            // poisoned mutex, some other thread holding the mutex has panicked!
            panic!("WorkQueue::get_work() tried to lock a poisoned mutex");
        }
    }

    // Both the controller (main thread) and workers can use this
    // function to add work to the queue.

    /// Blocks the current thread until work can be added, then
    /// adds that work to the end of the queue.
    /// Returns the amount of work now in the queue.
    ///
    /// # Panics
    /// Panics if the underlying mutex became poisoned. This is exceedingly
    /// unlikely.
    pub fn add_work(&self, work: Work) {
        if work.is_static {
            self.work_context.new_work.send(work).unwrap();
            return;
        }

        // As above, try to get a lock on the mutex.
        if let Ok(mut queue) = self.inner.lock() {
            /* Insert in position that maintains the queue sorted */
            let pos = match queue.binary_search_by(|probe| probe.cmp(&work)) {
                Ok(p) => p,
                Err(p) => p,
            };
            queue.insert(pos, work);

            /* inform threads that new job is available */
            self.new_jobs_tx.send(true).unwrap();
        } else {
            panic!("WorkQueue::add_work() tried to lock a poisoned mutex");
        }
    }
}

impl WorkController {
    pub fn new(pulse: Sender<ThreadEvent>) -> WorkController {
        let (new_jobs_tx, new_jobs_rx) = unbounded();

        /* create a channel for jobs to send new work to Controller thread */
        let (new_work_tx, new_work_rx) = unbounded();

        /* create a channel for jobs to set their names */
        let (set_name_tx, set_name_rx) = unbounded();

        /* create a channel for jobs to set their statuses */
        let (set_status_tx, set_status_rx) = unbounded();

        /* create a channel for jobs to announce their demise */
        let (finished_tx, finished_rx) = unbounded();

        /* each associated thread will hold a copy of this context item in order to communicate
         * with the controller thread */
        let work_context = WorkContext {
            new_work: new_work_tx,
            set_name: set_name_tx,
            set_status: set_status_tx,
            finished: finished_tx,
        };

        let queue: WorkQueue = WorkQueue::new(new_jobs_tx, work_context.clone());
        // Create a SyncFlag to share whether or not there are more jobs to be done.
        let (thread_end_tx, thread_end_rx) = bounded(1);

        let threads_lock: Arc<Mutex<FnvHashMap<thread::ThreadId, Worker>>> =
            Arc::new(Mutex::new(FnvHashMap::default()));

        let static_threads_lock: Arc<Mutex<FnvHashMap<thread::ThreadId, Worker>>> =
            Arc::new(Mutex::new(FnvHashMap::default()));

        let mut threads = threads_lock.lock().unwrap();
        /* spawn worker threads */
        for thread_num in 0..MAX_WORKER {
            /* Each worker thread will wait on two channels: thread_end and new_jobs. thread_end
             * informs the worker that it should quit and new_jobs informs that there is a new job
             * available inside the queue. Only one worker will get each job, and others will
             * go back to waiting on the channels */
            let thread_queue = queue.clone();

            let thread_end_rx = thread_end_rx.clone();
            let new_jobs_rx = new_jobs_rx.clone();
            let new_jobs_rx = new_jobs_rx.clone();

            let work_context = work_context.clone();
            let pulse = pulse.clone();

            let handle = thread::spawn(move || {
                let mut work_done = 0;

                'work_loop: loop {
                    debug!("Waiting for work");
                    select! {
                        recv(thread_end_rx) -> _ => {
                            debug!("received thread_end_rx, quitting");
                            break 'work_loop;
                        },
                        recv(new_jobs_rx) -> _ => {
                            while let Some(work) = thread_queue.get_work() {
                                debug!("Got some work");
                                work.compute(work_context.clone());
                                debug!("finished work");

                                work_done += 1;
                                work_context.set_name.send((std::thread::current().id(), "idle-worker".to_string())).unwrap();
                                work_context.set_status.send((std::thread::current().id(), "inactive".to_string())).unwrap();
                                pulse.send(ThreadEvent::Pulse).unwrap();

                                std::thread::yield_now();
                            }
                            continue 'work_loop;
                        },
                    }
                }

                /* report the amount of work done. */
                debug!("Thread {} did {} jobs.", thread_num, work_done);
            });

            /* add the handle for the newly spawned thread to the list of handles */
            threads.insert(handle.thread().id(), String::from("idle-worker").into());
        }
        /* drop lock */
        drop(threads);

        {
            /* start controller thread */
            let threads_lock = threads_lock.clone();
            let _static_threads_lock = static_threads_lock.clone();
            let thread_queue = queue.clone();
            let threads_lock = threads_lock.clone();
            let thread_end_rx = thread_end_rx.clone();
            let work_context = work_context.clone();

            let handle = thread::spawn(move || 'control_loop: loop {
                select! {
                    recv(thread_end_rx) -> _ => {
                        debug!("received thread_end_rx, quitting");
                        break 'control_loop;
                    },
                    recv(new_work_rx) -> work => {
                        if let Ok(work) = work {
                            if work.is_static {
                                let work_context = work_context.clone();
                                let handle = thread::spawn(move || work.compute(work_context));
                                 _static_threads_lock.lock().unwrap().insert(handle.thread().id(), String::new().into());
                            } else {
                                thread_queue.add_work(work);
                            }
                        }
                    }
                    recv(set_name_rx) -> new_name => {
                        if let Ok((thread_id, mut new_name)) = new_name {
                            new_name.truncate_at_boundary(256);
                            let mut threads = threads_lock.lock().unwrap();
                            let mut static_threads = _static_threads_lock.lock().unwrap();
                            if threads.contains_key(&thread_id) {
                                threads.entry(thread_id).and_modify(|e| e.name = new_name);
                            } else if static_threads.contains_key(&thread_id) {
                                static_threads.entry(thread_id).and_modify(|e| e.name = new_name);
                            } else {
                                 static_threads.insert(thread_id, new_name.into());
                            }
                            pulse.send(ThreadEvent::Pulse).unwrap();
                        }
                    }
                    recv(set_status_rx) -> new_status => {
                        if let Ok((thread_id, mut new_status)) = new_status {
                            new_status.truncate_at_boundary(256);
                            let mut threads = threads_lock.lock().unwrap();
                            let mut static_threads = _static_threads_lock.lock().unwrap();
                            if threads.contains_key(&thread_id) {
                                threads.entry(thread_id).and_modify(|e| e.status = new_status);
                            } else if static_threads.contains_key(&thread_id) {
                                static_threads.entry(thread_id).and_modify(|e| e.status = new_status);
                                debug!(&static_threads[&thread_id]);
                            } else {
                                 static_threads.insert(thread_id, Worker { status: new_status, .. String::new().into() });
                            }
                            pulse.send(ThreadEvent::Pulse).unwrap();
                        }
                    }
                    recv(finished_rx) -> dead_thread_id => {
                        if let Ok(thread_id) = dead_thread_id {
                            let mut threads = threads_lock.lock().unwrap();
                            let mut static_threads = _static_threads_lock.lock().unwrap();
                            if threads.contains_key(&thread_id) {
                                threads.remove(&thread_id);
                            } else if static_threads.contains_key(&thread_id) {
                                static_threads.remove(&thread_id);
                            } else {
                                /* Nothing to do */
                            }
                            pulse.send(ThreadEvent::Pulse).unwrap();
                        }
                    }
                }
            });

            let mut static_threads = static_threads_lock.lock().unwrap();
            static_threads.insert(
                handle.thread().id(),
                "WorkController-thread".to_string().into(),
            );
        }

        WorkController {
            queue,
            thread_end_tx,
            threads: threads_lock,
            static_threads: static_threads_lock,
            work_context,
        }
    }

    pub fn add_static_thread(&mut self, id: std::thread::ThreadId) {
        self.static_threads
            .lock()
            .unwrap()
            .insert(id, String::new().into());
    }

    pub fn get_context(&self) -> WorkContext {
        self.work_context.clone()
    }
}
