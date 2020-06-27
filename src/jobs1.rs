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

use melib::smol;
use std::future::Future;
use std::panic::catch_unwind;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use std::thread;
use std::time::Duration;
use uuid::Uuid;

use crate::types::ThreadEvent;
use crossbeam::channel;
use crossbeam::deque::{Injector, Steal, Stealer, Worker};
use crossbeam::sync::{Parker, Unparker};
use crossbeam::Sender;
use futures::channel::oneshot;
use once_cell::sync::Lazy;
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
        #[derive(PartialEq, Hash, Eq, Copy, Clone, Serialize, Deserialize, Default)]
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
            fn new() -> Self {
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
}

#[derive(Debug)]
pub struct JobExecutor {
    active_jobs: Vec<JobId>,
    global_queue: Arc<Injector<MeliTask>>,
    workers: Vec<Stealer<MeliTask>>,
    sender: Sender<ThreadEvent>,
    parkers: Vec<Unparker>,
}

impl JobExecutor {
    /// A queue that holds scheduled tasks.
    pub fn new(sender: Sender<ThreadEvent>) -> Self {
        // Create a queue.
        let mut ret = JobExecutor {
            active_jobs: vec![],
            global_queue: Arc::new(Injector::new()),
            workers: vec![],
            parkers: vec![],
            sender,
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
                smol::run(futures::future::pending::<()>());
            });

        // Spawn executor threads the first time the queue is created.
        for (i, (local, parker)) in workers.into_iter().enumerate() {
            let sender = ret.sender.clone();
            let global = ret.global_queue.clone();
            let stealers = ret.workers.clone();
            thread::Builder::new()
                .name(format!("meli-executor-{}", i))
                .spawn(move || loop {
                    parker.park_timeout(Duration::from_millis(100));
                    let task = find_task(&local, &global, stealers.as_slice());
                    if let Some(meli_task) = task {
                        let MeliTask { task, id } = meli_task;
                        debug!("Worker {} got task {:?}", i, id);
                        let res = catch_unwind(|| task.run());
                        debug!("Worker {} got result {:?}", i, id);
                    }
                });
        }
        ret
    }
    /// Spawns a future on the executor.
    pub fn spawn<F>(&self, future: F) -> JoinHandle
    where
        F: Future<Output = ()> + Send + 'static,
    {
        let job_id = JobId::new();
        let _job_id = job_id.clone();
        let __job_id = job_id.clone();
        let finished_sender = self.sender.clone();
        let injector = self.global_queue.clone();
        // Create a task and schedule it for execution.
        let (task, handle) = async_task::spawn(
            async move {
                let _ = future.await;
                finished_sender
                    .send(ThreadEvent::JobFinished(__job_id))
                    .unwrap();
            },
            move |task| injector.push(MeliTask { task, id: _job_id }),
            (),
        );
        task.schedule();
        for unparker in self.parkers.iter() {
            unparker.unpark();
        }

        // Return a join handle that retrieves the output of the future.
        JoinHandle(handle)
    }

    ///// Spawns a future on the executor.
    pub fn spawn_specialized<F, R>(&self, future: F) -> (oneshot::Receiver<R>, JobId)
    where
        F: Future<Output = R> + Send + 'static,
        R: Send + core::fmt::Debug + 'static,
    {
        let (sender, receiver) = oneshot::channel();
        let finished_sender = self.sender.clone();
        let job_id = JobId::new();
        let _job_id = job_id.clone();
        let __job_id = job_id.clone();
        let injector = self.global_queue.clone();
        // Create a task and schedule it for execution.
        let (task, handle) = async_task::spawn(
            async move {
                let res = future.await;
                sender.send(res).unwrap();
                finished_sender
                    .send(ThreadEvent::JobFinished(__job_id))
                    .unwrap();
            },
            move |task| injector.push(MeliTask { task, id: _job_id }),
            (),
        );
        task.schedule();
        for unparker in self.parkers.iter() {
            unparker.unpark();
        }

        (receiver, job_id)
    }
}

///// Spawns a future on the executor.
//fn spawn<F, R>(future: F) -> JoinHandle<R>
//where
//    F: Future<Output = R> + Send + 'static,
//    R: Send + 'static,
//{
//    // Create a task and schedule it for execution.
//    let (task, handle) = async_task::spawn(future, |t| QUEUE.send(t).unwrap(), ());
//    task.schedule();
//
//    // Return a join handle that retrieves the output of the future.
//    JoinHandle(handle)
//}

/// Awaits the output of a spawned future.
pub struct JoinHandle(async_task::JoinHandle<(), ()>);

impl Future for JoinHandle {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match Pin::new(&mut self.0).poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(output) => Poll::Ready(output.expect("task failed")),
        }
    }
}

/*
fn _test() {
    let executor = JobExecutor::new();
    futures::executor::block_on(async {
        // Spawn a future.
        let handle = executor.spawn(async {
            println!("Running task...");
            panic!();
        });

        // Await its output.
        handle.await;
    });
}
*/
