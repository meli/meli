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

use std::{
    borrow::Cow,
    future::Future,
    iter,
    panic::catch_unwind,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
    thread,
    time::Duration,
};

use crossbeam::{
    channel::Sender,
    deque::{Injector, Stealer, Worker},
    sync::{Parker, Unparker},
};
pub use futures::channel::oneshot;
use indexmap::IndexMap;
use melib::{log, smol, utils::datetime, uuid::Uuid, UnixTimestamp};

use crate::types::{StatusEvent, ThreadEvent, UIEvent};

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum IsAsync {
    Async,
    Blocking,
}

type AsyncTask = async_task::Runnable;

#[derive(Clone, Debug)]
struct FinishedTimestamp(Arc<Mutex<UnixTimestamp>>);

impl FinishedTimestamp {
    fn finished(&self) -> Option<UnixTimestamp> {
        match self.0.lock() {
            Ok(v) if *v == 0 => None,
            Ok(v) => Some(*v),
            Err(poison) => {
                let mut guard = poison.into_inner();
                if *guard == 0 {
                    *guard = UnixTimestamp::default();
                }
                Some(*guard)
            }
        }
    }

    fn set_finished(&self, new_value: Option<UnixTimestamp>) {
        let new_value = new_value.unwrap_or_default();
        match self.0.lock() {
            Ok(mut f) => *f = new_value,
            Err(poison) => {
                let mut guard = poison.into_inner();
                *guard = new_value;
            }
        }
    }
}

fn find_task(
    local: &Worker<MeliTask>,
    global: &Injector<MeliTask>,
    stealers: &[Stealer<MeliTask>],
) -> Option<MeliTask> {
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
        #[derive(PartialEq, Hash, Eq, Copy, Clone, Ord, PartialOrd, Serialize, Deserialize)]
        pub struct $n(Uuid);

        impl std::fmt::Debug for $n {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.0.to_string())
            }
        }

        impl std::fmt::Display for $n {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.0.to_string())
            }
        }

        impl Default for $n {
            fn default() -> Self {
                Self::new()
            }
        }

        impl $n {
            pub fn new() -> Self {
                Self(Uuid::new_v4())
            }
            pub fn null() -> Self {
                Self(Uuid::nil())
            }
        }
    };
}
uuid_hash_type!(JobId);
uuid_hash_type!(TimerId);

/// A spawned future and its current state.
pub struct MeliTask {
    task: AsyncTask,
    id: JobId,
    desc: Cow<'static, str>,
    timer: bool,
}

#[derive(Clone, Debug)]
/// A spawned future's metadata for book-keeping.
pub struct JobMetadata {
    id: JobId,
    desc: Cow<'static, str>,
    timer: bool,
    started: UnixTimestamp,
    finished: FinishedTimestamp,
    succeeded: bool,
}

impl JobMetadata {
    pub fn id(&self) -> &JobId {
        &self.id
    }

    pub fn description(&self) -> &str {
        &self.desc
    }

    pub fn is_timer(&self) -> bool {
        self.timer
    }

    pub fn started(&self) -> UnixTimestamp {
        self.started
    }

    pub fn finished(&self) -> Option<UnixTimestamp> {
        self.finished.finished()
    }

    pub fn succeeded(&self) -> bool {
        self.succeeded
    }
}

#[derive(Debug)]
pub struct JobExecutor {
    global_queue: Arc<Injector<MeliTask>>,
    workers: Vec<Stealer<MeliTask>>,
    sender: Sender<ThreadEvent>,
    parkers: Vec<Unparker>,
    timers: Arc<Mutex<IndexMap<TimerId, TimerPrivate>>>,
    pub jobs: Arc<Mutex<IndexMap<JobId, JobMetadata>>>,
}

#[derive(Debug, Default)]
struct TimerPrivate {
    /// Interval for periodic timer.
    interval: Duration,
    /// Time until next expiration.
    value: Duration,
    active: bool,
    handle: Option<async_task::Task<()>>,
    cancel: Arc<AtomicBool>,
}

#[derive(Debug)]
pub struct Timer {
    id: TimerId,
    job_executor: Arc<JobExecutor>,
}

impl Timer {
    pub fn id(&self) -> TimerId {
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
        let mut ret = Self {
            global_queue: Arc::new(Injector::new()),
            workers: vec![],
            parkers: vec![],
            sender,
            timers: Arc::new(Mutex::new(IndexMap::default())),
            jobs: Arc::new(Mutex::new(IndexMap::default())),
        };
        let mut workers = vec![];
        for _ in 0..std::thread::available_parallelism()
            .map(Into::into)
            .unwrap_or(1)
        {
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
                .name(format!("meli-executor-{i}"))
                .spawn(move || loop {
                    parker.park_timeout(Duration::from_millis(100));
                    let task = find_task(&local, &global, stealers.as_slice());
                    if let Some(meli_task) = task {
                        let MeliTask {
                            task,
                            id,
                            timer,
                            desc,
                        } = meli_task;
                        if !timer {
                            log::trace!("Worker {} got task {:?} {:?}", i, desc, id);
                        }
                        let _ = catch_unwind(|| task.run());
                        if !timer {
                            log::trace!("Worker {} returned after {:?} {:?}", i, desc, id);
                        }
                    }
                })
                .unwrap();
        }
        ret
    }

    /// Spawns a future with a generic return value `R`
    #[inline(always)]
    pub fn spawn<F, R>(
        &self,
        desc: Cow<'static, str>,
        future: F,
        is_async: IsAsync,
    ) -> JoinHandle<R>
    where
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
    {
        if matches!(is_async, IsAsync::Async) {
            self.spawn_specialized(desc, future)
        } else {
            self.spawn_blocking(desc, future)
        }
    }

    /// Spawns a future with a generic return value `R`
    #[inline(always)]
    fn spawn_specialized<F, R>(&self, desc: Cow<'static, str>, future: F) -> JoinHandle<R>
    where
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
    {
        let (sender, receiver) = oneshot::channel();
        let finished_sender = self.sender.clone();
        let job_id = JobId::new();
        let injector = self.global_queue.clone();
        // We do not use `AtomicU64` because it's not portable, so ignore the lint.
        #[allow(clippy::mutex_integer)]
        let finished = FinishedTimestamp(Arc::new(Mutex::new(0)));
        let cancel = Arc::new(AtomicBool::new(false));

        self.jobs.lock().unwrap().insert(
            job_id,
            JobMetadata {
                id: job_id,
                desc: desc.clone(),
                started: datetime::now(),
                finished: finished.clone(),
                succeeded: true,
                timer: false,
            },
        );

        // Create a task and schedule it for execution.
        let (handle, task) = {
            let cancel = cancel.clone();
            let finished = finished.clone();
            async_task::spawn(
                async move {
                    let res = future.await;
                    let _ = sender.send(res);
                    if let Ok(mut guard) = finished.0.lock() {
                        *guard = datetime::now();
                    }
                    finished_sender
                        .send(ThreadEvent::JobFinished(job_id))
                        .unwrap();
                },
                move |task| {
                    if cancel.load(Ordering::SeqCst) {
                        return;
                    }
                    let desc = desc.clone();
                    injector.push(MeliTask {
                        task,
                        id: job_id,
                        desc,
                        timer: false,
                    })
                },
            )
        };
        handle.schedule();
        for unparker in self.parkers.iter() {
            unparker.unpark();
        }

        JoinHandle {
            task: Arc::new(Mutex::new(Some(task))),
            cancel,
            finished,
            chan: receiver,
            job_id,
        }
    }

    /// Spawns a future with a generic return value `R` that might block on a
    /// new thread
    #[inline(always)]
    fn spawn_blocking<F, R>(&self, desc: Cow<'static, str>, future: F) -> JoinHandle<R>
    where
        F: Future<Output = R> + Send + 'static,
        R: Send + 'static,
    {
        self.spawn_specialized(
            desc,
            smol::unblock(move || futures::executor::block_on(future)),
        )
    }

    pub fn create_timer(self: Arc<Self>, interval: Duration, value: Duration) -> Timer {
        let timer = TimerPrivate {
            interval,
            cancel: Arc::new(AtomicBool::new(false)),
            value,
            active: true,
            handle: None,
        };
        let id = TimerId::default();
        self.timers.lock().unwrap().insert(id, timer);
        self.arm_timer(id, value);
        Timer {
            id,
            job_executor: self,
        }
    }

    pub fn rearm(&self, timer_id: TimerId) {
        let mut timers_lck = self.timers.lock().unwrap();
        if let Some(timer) = timers_lck.get_mut(&timer_id) {
            let value = timer.value;
            drop(timers_lck);
            self.arm_timer(timer_id, value);
        }
    }

    fn arm_timer(&self, id: TimerId, value: Duration) {
        let job_id = JobId::new();
        let sender = self.sender.clone();
        let injector = self.global_queue.clone();
        let timers = self.timers.clone();
        let cancel = Arc::new(AtomicBool::new(false));
        let (task, handle) = {
            let cancel = cancel.clone();
            async_task::spawn(
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
                    if cancel.load(Ordering::SeqCst) {
                        return;
                    }
                    injector.push(MeliTask {
                        task,
                        id: job_id,
                        desc: Cow::Borrowed("timer"),
                        timer: true,
                    })
                },
            )
        };
        self.timers.lock().unwrap().entry(id).and_modify(|timer| {
            timer.handle = Some(handle);
            timer.cancel = cancel;
            timer.active = true;
        });
        task.schedule();
        for unparker in self.parkers.iter() {
            unparker.unpark();
        }
    }

    fn disable_timer(&self, id: TimerId) {
        let mut timers_lck = self.timers.lock().unwrap();
        if let Some(timer) = timers_lck.get_mut(&id) {
            timer.active = false;
            timer.cancel.store(true, Ordering::SeqCst);
        }
    }

    fn set_interval(&self, id: TimerId, new_val: Duration) {
        let mut timers_lck = self.timers.lock().unwrap();
        if let Some(timer) = timers_lck.get_mut(&id) {
            timer.interval = new_val;
        }
    }

    pub fn set_job_finished(&self, id: JobId) {
        self.jobs.lock().unwrap().entry(id).and_modify(|entry| {
            entry.finished.set_finished(Some(datetime::now()));
        });
    }

    pub fn set_job_success(&self, id: JobId, value: bool) {
        self.jobs.lock().unwrap().entry(id).and_modify(|entry| {
            entry.succeeded = value;
        });
    }
}

pub type JobChannel<T> = oneshot::Receiver<T>;

/// `JoinHandle` for the future that allows us to cancel the task.
#[derive(Debug)]
pub struct JoinHandle<T> {
    pub task: Arc<Mutex<Option<async_task::Task<()>>>>,
    pub chan: JobChannel<T>,
    pub cancel: Arc<AtomicBool>,
    finished: FinishedTimestamp,
    pub job_id: JobId,
}

impl<T> JoinHandle<T> {
    pub fn cancel(&self) -> Option<StatusEvent> {
        let was_active = self.cancel.swap(true, Ordering::SeqCst);
        if was_active {
            self.finished.set_finished(Some(datetime::now()));
            Some(StatusEvent::JobCanceled(self.job_id))
        } else {
            None
        }
    }

    pub fn finished(&self) -> Option<UnixTimestamp> {
        self.finished.finished()
    }

    pub fn is_canceled(&self) -> bool {
        self.cancel.load(Ordering::SeqCst)
    }
}

impl<T> std::cmp::PartialEq<JobId> for JoinHandle<T> {
    fn eq(&self, other: &JobId) -> bool {
        self.job_id == *other
    }
}

impl<T> Drop for JoinHandle<T> {
    fn drop(&mut self) {
        _ = self.cancel();
    }
}
