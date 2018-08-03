use chan;
use std::thread;

#[derive(Debug)]
pub enum AsyncStatus {
    NoUpdate,
    Finished,
    ProgressReport(usize),
}

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
    pub fn tx(&mut self) -> chan::Sender<AsyncStatus> {
        self.tx.clone()
    }
    pub fn rx(&mut self) -> chan::Receiver<AsyncStatus> {
        self.rx.clone()
    }
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
    pub fn extract(self) -> T {
        self.value.unwrap()
    }
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
                        eprintln!("async got {:?}", a);
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
        eprintln!("worker joined");
        return Ok(AsyncStatus::Finished);
    }
}

