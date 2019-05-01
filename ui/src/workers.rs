use chan;
use melib::async_workers::Work;
use std;
use std::mem;
use std::thread;

const MAX_WORKER: usize = 4;

pub struct WorkController {
    pub queue: WorkQueue<Work>,
    thread_end_tx: chan::Sender<bool>,
    results: Option<chan::Receiver<bool>>,
    threads: Vec<std::thread::JoinHandle<()>>,
}

impl WorkController {
    pub fn results_rx(&mut self) -> chan::Receiver<bool> {
        self.results.take().unwrap()
    }
}

/*
impl Drop for WorkController {
    fn drop(&mut self) {
        for _ in 0..self.threads.len() {
            self.thread_end_tx.send(true);
        }
        let threads = mem::replace(&mut self.threads, Vec::new());
        for handle in threads {
            handle.join().unwrap();
        }
    }
}
*/

// We need a way to keep track of what work needs to be done.
// This is a multi-source, multi-consumer queue which we call a
// WorkQueue.

// To create this type, we wrap a mutex (std::sync::mutex) around a
// queue (technically a double-ended queue, std::collections::VecDeque).
//
// Mutex stands for MUTually EXclusive. It essentially ensures that only
// one thread has access to a given resource at one time.
use std::sync::Mutex;

// A VecDeque is a double-ended queue, but we will only be using it in forward
// mode; that is, we will push onto the back and pull from the front.
use std::collections::VecDeque;

// Finally we wrap the whole thing in Arc (Atomic Reference Counting) so that
// we can safely share it with other threads. Arc (std::sync::arc) is a lot
// like Rc (std::rc::Rc), in that it allows multiple references to some memory
// which is freed when no references remain, except that it is atomic, making
// it comparitively slow but able to be shared across the thread boundary.
use std::sync::Arc;

// All three of these types are wrapped around a generic type T.
// T is required to be Send (a marker trait automatically implemented when
// it is safe to do so) because it denotes types that are safe to move between
// threads, which is the whole point of the WorkQueue.
// For this implementation, T is required to be Copy as well, for simplicity.

/// A generic work queue for work elements which can be trivially copied.
/// Any producer of work can add elements and any worker can consume them.
/// WorkQueue derives Clone so that it can be distributed among threads.
#[derive(Clone)]
pub struct WorkQueue<T: Send> {
    inner: Arc<Mutex<VecDeque<T>>>,
    new_jobs_tx: chan::Sender<bool>,
}

impl<T: Send> WorkQueue<T> {
    // Creating one of these by hand would be kind of a pain,
    // so let's provide a convenience function.

    /// Creates a new WorkQueue, ready to be used.
    fn new(new_jobs_tx: chan::Sender<bool>) -> Self {
        Self {
            inner: Arc::new(Mutex::new(VecDeque::new())),
            new_jobs_tx,
        }
    }

    // This is the function workers will use to acquire work from the queue.
    // They will call it in a loop, checking to see if there is any work available.

    /// Blocks the current thread until work is available, then
    /// gets the data required to perform that work.
    ///
    /// # Errors
    /// Returns None if there is no more work in the queue.
    ///
    /// # Panics
    /// Panics if the underlying mutex became poisoned. This is exceedingly
    /// unlikely.
    fn get_work(&self) -> Option<T> {
        // Try to get a lock on the Mutex. If this fails, there is a
        // problem with the mutex - it's poisoned, meaning that a thread that
        // held the mutex lock panicked before releasing it. There is no way
        // to guarantee that all its invariants are upheld, so we need to not
        // use it in that case.
        let maybe_queue = self.inner.lock();
        // A lot is going on here. self.inner is an Arc of Mutex. Arc can deref
        // into its internal type, so we can call the methods of that inner
        // type (Mutex) without dereferencing, so this is like
        //      *(self.inner).lock()
        // but doesn't look awful. Mutex::lock() returns a
        // Result<MutexGuard<VecDeque<T>>>.

        // Unwrapping with if let, we get a MutexGuard, which is an RAII guard
        // that unlocks the Mutex when it goes out of scope.
        if let Ok(mut queue) = maybe_queue {
            // queue is a MutexGuard<VecDeque>, so this is like
            //      (*queue).pop_front()
            // Returns Some(item) or None if there are no more items.
            queue.pop_front()

        // The function has returned, so queue goes out of scope and the
        // mutex unlocks.
        } else {
            // There's a problem with the mutex.
            panic!("WorkQueue::get_work() tried to lock a poisoned mutex");
        }
    }

    // Both the controller (main thread) and possibly workers can use this
    // function to add work to the queue.

    /// Blocks the current thread until work can be added, then
    /// adds that work to the end of the queue.
    /// Returns the amount of work now in the queue.
    ///
    /// # Panics
    /// Panics if the underlying mutex became poisoned. This is exceedingly
    /// unlikely.
    pub fn add_work(&self, work: T) -> usize {
        // As above, try to get a lock on the mutex.
        if let Ok(mut queue) = self.inner.lock() {
            // As above, we can use the MutexGuard<VecDeque<T>> to access
            // the internal VecDeque.
            queue.push_back(work);

            self.new_jobs_tx.send(true);
            // Now return the length of the queue.
            queue.len()
        } else {
            panic!("WorkQueue::add_work() tried to lock a poisoned mutex");
        }
    }
}

impl WorkController {
    pub fn new() -> WorkController {
        let (new_jobs_tx, new_jobs_rx) = chan::async();
        // Create a new work queue to keep track of what work needs to be done.
        // Note that the queue is internally mutable (or, rather, the Mutex is),
        // but this binding doesn't need to be mutable. This isn't unsound because
        // the Mutex ensures at runtime that no two references can be used;
        // therefore no mutation can occur at the same time as aliasing.
        let queue: WorkQueue<Work> = WorkQueue::new(new_jobs_tx);

        // Create a MPSC (Multiple Producer, Single Consumer) channel. Every worker
        // is a producer, the main thread is a consumer; the producers put their
        // work into the channel when it's done.
        let (results_tx, results_rx) = chan::async();

        // Create a SyncFlag to share whether or not there are more jobs to be done.
        let (thread_end_tx, thread_end_rx) = chan::sync(::std::mem::size_of::<bool>());

        // This Vec will hold thread join handles to allow us to not exit while work
        // is still being done. These handles provide a .join() method which blocks
        // the current thread until the thread referred to by the handle exits.
        let mut threads = Vec::new();

        for thread_num in 0..MAX_WORKER {
            // Get a reference to the queue for the thread to use
            // .clone() here doesn't clone the actual queue data, but rather the
            // internal Arc produces a new reference for use in the new queue
            // instance.
            let thread_queue = queue.clone();

            // Similarly, create a new transmitter for the thread to use
            let thread_results_tx = results_tx.clone();

            let thread_end_rx = thread_end_rx.clone();
            let new_jobs_rx = new_jobs_rx.clone();

            // thread::spawn takes a closure (an anonymous function that "closes"
            // over its environment). The move keyword means it takes ownership of
            // those variables, meaning they can't be used again in the main thread.
            let handle = thread::spawn(move || {
                // A varaible to keep track of how much work was done.
                let mut work_done = 0;

                'work_loop: loop {
                    // Loop while there's expected to be work, looking for work.
                    chan_select! {
                        thread_end_rx.recv() -> _ => {
                            break 'work_loop;
                        },
                        new_jobs_rx.recv() -> _ => {
                            // If work is available, do that work.
                            while let Some(work) = thread_queue.get_work() {
                                // Do some work.
                                work.compute();

                                // Record that some work was done.
                                work_done += 1;

                                // Send the work and the result of that work.
                                //
                                // Sending could fail. If so, there's no use in
                                // doing any more work, so abort.
                                thread_results_tx.send(true);

                                // Signal to the operating system that now is a good time
                                // to give another thread a chance to run.
                                //
                                // This isn't strictly necessary - the OS can preemptively
                                // switch between threads, without asking - but it helps make
                                // sure that other threads do get a chance to get some work.
                                std::thread::yield_now();
                            }
                            continue 'work_loop;
                        },
                    }
                }

                // Report the amount of work done.
                debug!("Thread {} did {} jobs.", thread_num, work_done);
            });

            // Add the handle for the newly spawned thread to the list of handles
            threads.push(handle);
        }

        WorkController {
            queue,
            thread_end_tx,
            results: Some(results_rx),
            threads,
        }
    }
}
/*
pub fn add_jobkk

    println!("Adding jobs to the queue.");
    // Variables to keep track of the number of jobs we expect to do.
    let mut jobs_remaining = 0;
    let mut jobs_total = 0;

    // Just add some numbers to the queue.
    // These numbers will be passed into fib(), so they need to stay pretty
    // small.
    for work in 0..90 {
        // Add each one several times.
        for _ in 0..100 {
            jobs_remaining = queue.add_work(work);
            jobs_total += 1;
        }
    }


    // Report that some jobs were inserted, and how many are left to be done.
    // This is interesting because the workers have been taking jobs out of the queue
    // the whole time the control thread has been putting them in!
    //
    // Try removing the use of std::thread::yield_now() in the thread closure.
    // You'll probably (depending on your system) notice that the number remaining
    // after insertion goes way up. That's because the operating system is usually
    // (not always, but usually) fairly conservative about interrupting a thread
    // that is actually doing work.
    //
    // Similarly, if you add a call to yield_now() in the loop above, you'll see the
    // number remaining probably drop to 1 or 2. This can also change depending on
    // how optimized the output code is - try `cargo run --release` vs `cargo run`.
    //
    // This inconsistency should drive home to you that you as the programmer can't
    // make any assumptions at all about when and in what order things will happen
    // in parallel code unless you use thread control primatives as demonstrated
    // in this program.
    println!("Total of {} jobs inserted into the queue ({} remaining at this time).",
             jobs_total,
             jobs_remaining);


    // Get completed work from the channel while there's work to be done.
    while jobs_total > 0 {
        match results_rx.recv() {
            // If the control thread successfully receives, a job was completed.
            Ok(_) => { jobs_total -= 1 },
            // If the control thread is the one left standing, that's pretty
            // problematic.
            Err(_) => {panic!("All workers died unexpectedly.");}
        }
    }

*/
