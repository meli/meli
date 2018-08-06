#[macro_use]
mod cells;
#[macro_use]
mod helpers;
#[macro_use]
mod keys;
#[macro_use]
mod position;
pub use self::cells::*;
pub use self::helpers::*;
pub use self::keys::*;
pub use self::position::*;

use super::execute::Action;

use melib::RefreshEvent;
use std;
use std::thread;
use std::fmt;

/// `ThreadEvent` encapsulates all of the possible values we need to transfer between our threads
/// to the main process.
#[derive(Debug)]
pub enum ThreadEvent {
    ThreadJoin(thread::ThreadId),
    /// User input.
    Input(Key),
    /// A watched folder has been refreshed.
    RefreshMailbox {
        hash: u64,
    },
    UIEvent(UIEventType),
    //Decode { _ }, // For gpg2 signature check
}

impl From<RefreshEvent> for ThreadEvent {
    fn from(event: RefreshEvent) -> Self {
        ThreadEvent::RefreshMailbox { hash: event.hash }
    }
}

#[derive(Debug)]
pub enum ForkType {
    Generic(std::process::Child),
    NewDraft(File, std::process::Child),
}

#[derive(Debug)]
pub enum UIEventType {
    Input(Key),
    ExInput(Key),
    RefreshMailbox((usize, usize)),
    //Quit?
    Resize,
    /// Force redraw.
    Fork(ForkType),
    ChangeMailbox(usize),
    ChangeMode(UIMode),
    Command(String),
    Notification(String),
    EditDraft(File),
    Action(Action),
    StatusNotification(String),
    MailboxUpdate((usize, usize)),

    StartupCheck,
}

/// An event passed from `State` to its Entities.
#[derive(Debug)]
pub struct UIEvent {
    pub id: u64,
    pub event_type: UIEventType,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum UIMode {
    Normal,
    Execute,
    Fork,
}

impl fmt::Display for UIMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                UIMode::Normal => "NORMAL",
                UIMode::Execute => "EX",
                UIMode::Fork => "FORK",
            }
        )
    }
}

/// An event notification that is passed to Entities for handling.
pub struct Notification {
    _title: String,
    _content: String,

    _timestamp: std::time::Instant,
}
