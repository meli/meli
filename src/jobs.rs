/*
 * meli - jobs executor
 *
 * Copyright 2020 Manos Pitsidianakis
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

//! Async job executor thread pool
//!
//! ## Usage
//! ```no_run
//!  let (channel, handle, job_id) = job_executor.spawn(job);
//! ```

use melib::error::Result;
use melib::smol;
use std::collections::HashMap;
use std::future::Future;
use std::panic::catch_unwind;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use uuid::Uuid;

use crate::types::{ThreadEvent, UIEvent};
use crossbeam::deque::{Injector, Stealer, Worker};
use crossbeam::sync::{Parker, Unparker};
use crossbeam::Sender;
pub use futures::channel::oneshot;
use std::iter;

type AsyncTask = async_task::Task<()>;

fn find_task<T>(local: &Worker<T>, global: &Injector<T>, stealers: &[Stealer<T>]) -> Option<T> {
    // Pop a task from the local queue, if not empty.
    local.pop().or_else(|| {
        // Otherwise, we need to look for a task elsewhere.
        iter::repeat_with(|| {
            // Try stealing a batch of tasks from the global queue.
            global
                .steal_batch_and_pop(local)
                // Or try stealing a task from one of the other threads.
                .or_else(|| stealers.iter().map(|s| s.steal()).collect())
        })
        // Loop while no task was stolen and any steal operation needs to be retried.
        .find(|s| !s.is_retry())
        // Extract the stolen task, if there is one.
        .and_then(|s| s.success())
    })
}

macro_rules! uuid_hash_type {
    ($n:ident) => {
        #[derive(
            PartialEq, Hash, Eq, Copy, Clone, Ord, PartialOrd, Serialize, Deserialize, Default,
        )]
        pub struct $n(Uuid);

        impl core::fmt::Debug for $n {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{}", self.0.to_string())
            }
        }

        impl core::fmt::Display for $n {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{}", self.0.to_string())
            }
        }

        impl $n {
            pub fn new() -> Self {
                $n(Uuid::new_v4())
            }
            pub fn null() -> Self {
                $n(Uuid::nil())
            }
        }
    };
}
uuid_hash_type!(JobId);

/// A spawned future and its current state.
pub struct MeliTask {
    task: AsyncTask,
    id: JobId,
    timer: bool,
}

#[derive(Debug)]
pub struct JobExecutor {
    global_queue: Arc<Injector<MeliTask>>,
    workers: Vec<Stealer<MeliTask>>,
    sender: Sender<ThreadEvent>,
    parkers: Vec<Unparker>,
    timers: Arc<Mutex<HashMap<Uuid, TimerPrivate>>>,
}

#[derive(Debug, Default)]
struct TimerPrivate {
    /// Interval for periodic timer.
    interval: Duration,
    /// Time until next expiration.
    value: Duration,
    active: bool,
    handle: Option<async_task::JoinHandle<(), ()>>,
}

#[derive(Debug)]
pub struct Timer {
    id: Uuid,
    job_executor: Arc<JobExecutor>,
}

impl Timer {
    pub fn id(&self) -> Uuid {
        self.id
    }

    pub fn rearm(&self) {
        self.job_executor.rearm(self.id);
    }

    pub fn disable(&self) {
        self.job_executor.disable_timer(self.id);
    }

    pub fn set_interval(&self, new_val: Duration) {
        self.job_executor.set_interval(self.id, new_val);
    }
}

impl Drop for Timer {
    fn drop(&mut self) {
        self.disable();
    }
}

impl JobExecutor {
    /// A queue that holds scheduled tasks.
    pub fn new(sender: Sender<ThreadEvent>) -> Self {
        // Create a queue.
        let mut ret = JobExecutor {
            global_queue: Arc::new(Injector::new()),
            workers: vec![],
            parkers: vec![],
            sender,
            timers: Arc::new(Mutex::new(HashMap::default())),
        };
        let mut workers = vec![];
        for _ in 0..num_cpus::get().max(1) {
            let new_worker = Worker::new_fifo();
            ret.workers.push(new_worker.stealer());
            let p = Parker::new();
            ret.parkers.push(p.unparker().clone());
            workers.push((new_worker, p));
        }

        // Reactor thread
        thread::Builder::new()
            .name("meli-reactor".to_string())
            .spawn(move || {
                let ex = smol::Executor::new();

                futures::executor::block_on(ex.run(futures::future::pending::<()>()));
            })
            .unwrap();

        // Spawn executor threads the first time the queue is created.
        for (i, (local, parker)) in workers.into_iter().enumerate() {
            let global = ret.global_queue.clone();
            let stealers = ret.workers.clone();
            thread::Builder::new()
                .name(format!("meli-executor-{}", i))
                .spawn(move || loop {
                    parker.park_timeout(Duration::from_millis(100));
                    let task = find_task(&local, &global, stealers.as_slice());
                    if let Some(meli_task) = task {
                        let MeliTask { task, id, timer } = meli_task;
                        if !timer {
                            debug!("Worker {} got task {:?}", i, id);
                        }
                        let _ = catch_unwind(|| task.run());
                        if !timer {
                            debug!("Worker {} returned after {:?}", i, id);
                        }
                    }
                })
                .unwrap();
        }
        ret
    }

    /// Spawns a future with a generic return value `R`
    pub fn spawn_specialized<F, R>(&self, future: F) -> JoinHandle<R>
    where
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
    {
        let (sender, receiver) = oneshot::channel();
        let finished_sender = self.sender.clone();
        let job_id = JobId::new();
        let injector = self.global_queue.clone();
        // Create a task and schedule it for execution.
        let (task, handle) = async_task::spawn(
            async move {
                let res = future.await;
                let _ = sender.send(res);
                finished_sender
                    .send(ThreadEvent::JobFinished(job_id))
                    .unwrap();
                Ok(())
            },
            move |task| {
                injector.push(MeliTask {
                    task,
                    id: job_id,
                    timer: false,
                })
            },
            (),
        );
        task.schedule();
        for unparker in self.parkers.iter() {
            unparker.unpark();
        }

        JoinHandle {
            inner: handle,
            chan: receiver,
            job_id,
        }
    }

    /// Spawns a future with a generic return value `R` that might block on a new thread
    pub fn spawn_blocking<F, R>(&self, future: F) -> JoinHandle<R>
    where
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
    {
        self.spawn_specialized(smol::unblock(move || futures::executor::block_on(future)))
    }

    pub fn create_timer(self: Arc<JobExecutor>, interval: Duration, value: Duration) -> Timer {
        let id = Uuid::new_v4();
        let timer = TimerPrivate {
            interval,
            value,
            active: true,
            handle: None,
        };
        self.timers.lock().unwrap().insert(id, timer);
        self.arm_timer(id, value);
        Timer {
            id,
            job_executor: self,
        }
    }

    pub fn rearm(&self, timer_id: Uuid) {
        let mut timers_lck = self.timers.lock().unwrap();
        if let Some(timer) = timers_lck.get_mut(&timer_id) {
            if let Some(handle) = timer.handle.take() {
                handle.cancel();
            }
            let value = timer.value;
            drop(timers_lck);
            self.arm_timer(timer_id, value);
        }
    }

    fn arm_timer(&self, id: Uuid, value: Duration) {
        let job_id = JobId::new();
        let sender = self.sender.clone();
        let injector = self.global_queue.clone();
        let timers = self.timers.clone();
        let (task, handle) = async_task::spawn(
            async move {
                let mut value = value;
                loop {
                    smol::Timer::after(value).await;
                    sender
                        .send(ThreadEvent::UIEvent(UIEvent::Timer(id)))
                        .unwrap();
                    if let Some(interval) = timers.lock().unwrap().get(&id).and_then(|timer| {
                        if timer.interval.as_millis() == 0 && timer.interval.as_secs() == 0 {
                            None
                        } else if timer.active {
                            Some(timer.interval)
                        } else {
                            None
                        }
                    }) {
                        value = interval;
                    } else {
                        break;
                    }
                }
            },
            move |task| {
                injector.push(MeliTask {
                    task,
                    id: job_id,
                    timer: true,
                })
            },
            (),
        );
        self.timers.lock().unwrap().entry(id).and_modify(|timer| {
            timer.handle = Some(handle);
            timer.active = true;
        });
        task.schedule();
        for unparker in self.parkers.iter() {
            unparker.unpark();
        }
    }

    fn disable_timer(&self, id: Uuid) {
        let mut timers_lck = self.timers.lock().unwrap();
        if let Some(timer) = timers_lck.get_mut(&id) {
            if let Some(handle) = timer.handle.take() {
                handle.cancel();
            }
            timer.active = false;
        }
    }

    fn set_interval(&self, id: Uuid, new_val: Duration) {
        let mut timers_lck = self.timers.lock().unwrap();
        if let Some(timer) = timers_lck.get_mut(&id) {
            timer.interval = new_val;
        }
    }
}

pub type JobChannel<T> = oneshot::Receiver<T>;

#[derive(Debug)]
/// JoinHandle for the future that allows us to cancel the task.
pub struct JoinHandle<T> {
    pub inner: async_task::JoinHandle<Result<()>, ()>,
    pub chan: JobChannel<T>,
    pub job_id: JobId,
}

impl<T> JoinHandle<T> {
    pub fn cancel(&self) {
        self.inner.cancel()
    }
}

impl<T> std::cmp::PartialEq<JobId> for JoinHandle<T> {
    fn eq(&self, other: &JobId) -> bool {
        self.job_id == *other
    }
}

/*
use std::pin::Pin;
use std::task::{Context, Poll};
impl Future for JoinHandle {
    type Output = Result<()>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match Pin::new(&mut self.inner).poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(output) => Poll::Ready(output.expect("task failed")),
        }
    }
}
*/
