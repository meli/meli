/*
 * meli - ui module.
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
  The UI crate has an Entity-Component-System design. The System part, is also the application's state, so they're both merged in the `State` struct.

  `State` owns all the Entities of the UI, which are currently plain Containers for `Component`s. In the application's main event loop, input is handed to the state in the form of `UIEvent` objects which traverse the entity graph. Components decide to handle each input or not.

  Input is received in the main loop from threads which listen on the stdin for user input, observe folders for file changes etc. The relevant struct is `ThreadEvent`.
  */

#[macro_use]
mod position;
mod cells;
pub mod components;

mod helpers;
pub use helpers::*;

#[macro_use]
mod execute;
use execute::*;
use self::cells::*;
pub use self::components::*;
pub use self::position::*;

extern crate melib;
extern crate mime_apps;
extern crate notify_rust;
#[macro_use]
extern crate chan;
extern crate chan_signal;
extern crate linkify;
extern crate uuid;
use melib::*;

use std::collections::VecDeque;
use std::fmt;
use std::io::Write;
use std::thread;
use std::time;
extern crate fnv;
use self::fnv::FnvHashMap;
extern crate termion;
use termion::event::Key as TermionKey;
use termion::input::TermRead;
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use termion::{clear, cursor, style};

#[macro_use]
extern crate nom;
use chan::Sender;


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

/// A context container for loaded settings, accounts, UI changes, etc.
pub struct Context {
    pub accounts: Vec<Account>,
    settings: Settings,

    runtime_settings: Settings,
    /// Areas of the screen that must be redrawn in the next render
    dirty_areas: VecDeque<Area>,

    /// Events queue that components send back to the state
    replies: VecDeque<UIEvent>,
    _backends: Backends,

    input_thread: chan::Sender<bool>,
}

impl Context {
    pub fn replies(&mut self) -> Vec<UIEvent> {
        self.replies.drain(0..).collect()
    }
    pub fn input_thread(&mut self) -> &mut chan::Sender<bool> {
        &mut self.input_thread
    }
}

/// A State object to manage and own components and entities of the UI. `State` is responsible for
/// managing the terminal and interfacing with `melib`
pub struct State<W: Write> {
    cols: usize,
    rows: usize,

    grid: CellBuffer,
    stdout: Option<termion::screen::AlternateScreen<termion::raw::RawTerminal<W>>>,
    child: Option<ForkType>,
    pub mode: UIMode,
    sender: Sender<ThreadEvent>,
    entities: Vec<Entity>,
    pub context: Context,

    startup_thread: Option<chan::Sender<bool>>,

    threads: FnvHashMap<thread::ThreadId, thread::JoinHandle<()>>,
}

impl<W: Write> Drop for State<W> {
    fn drop(&mut self) {
        // When done, restore the defaults to avoid messing with the terminal.
        write!(
            self.stdout(),
            "{}{}{}{}",
            clear::All,
            style::Reset,
            cursor::Goto(1, 1),
            cursor::Show
        ).unwrap();
        self.flush();
    }
}

impl State<std::io::Stdout> {
    pub fn new(sender: Sender<ThreadEvent>, input_thread: chan::Sender<bool>) -> Self {
        let _stdout = std::io::stdout();
        _stdout.lock();
        let settings = Settings::new();
        let backends = Backends::new();
        let stdout = AlternateScreen::from(_stdout.into_raw_mode().unwrap());

        let termsize = termion::terminal_size().ok();
        let termcols = termsize.map(|(w, _)| w);
        let termrows = termsize.map(|(_, h)| h);
        let cols = termcols.unwrap_or(0) as usize;
        let rows = termrows.unwrap_or(0) as usize;
        let mut accounts: Vec<Account> = settings
            .accounts
            .iter()
            .map(|(n, a_s)| Account::new(n.to_string(), a_s.clone(), &backends))
            .collect();
        accounts.sort_by(|a, b| a.name().cmp(&b.name()));
        let (startup_tx, startup_rx) = chan::async();
        let startup_thread = {
            let sender = sender.clone();
            let startup_rx = startup_rx.clone();

            thread::Builder::new()
                .name("startup-thread".to_string())
                .spawn(move || {
                    let dur = time::Duration::from_millis(100);
                    loop {
                        chan_select! {
                            default => {},
                            startup_rx.recv() -> _ => {
                                sender.send(ThreadEvent::ThreadJoin(thread::current().id()));
                                return;
                            }
                        }
                        sender.send(ThreadEvent::UIEvent(UIEventType::StartupCheck));
                        thread::sleep(dur);
                    }
                }).unwrap()
        };
        let mut s = State {
            cols: cols,
            rows: rows,
            grid: CellBuffer::new(cols, rows, Cell::with_char(' ')),
            stdout: Some(stdout),
            child: None,
            mode: UIMode::Normal,
            sender: sender,
            entities: Vec::with_capacity(1),

            context: Context {
                accounts: accounts,
                _backends: backends,
                settings: settings.clone(),
                runtime_settings: settings,
                dirty_areas: VecDeque::with_capacity(5),
                replies: VecDeque::with_capacity(5),

                input_thread: input_thread,
            },
            startup_thread: Some(startup_tx),
            threads: FnvHashMap::with_capacity_and_hasher(1, Default::default()),
        };
        s.threads.insert(startup_thread.thread().id(), startup_thread);
        write!(
            s.stdout(),
            "{}{}{}",
            cursor::Hide,
            clear::All,
            cursor::Goto(1, 1)
            ).unwrap();
        s.flush();
        for account in &mut s.context.accounts {
            let sender = s.sender.clone();
            account.watch(RefreshEventConsumer::new(Box::new(move |r| {
                sender.send(ThreadEvent::from(r));
            })));
        }
        s
    }
    pub fn join(&mut self, id: thread::ThreadId) {
        let handle = self.threads.remove(&id).unwrap();
        handle.join().unwrap();


    }
    pub fn finish_startup(&mut self) {
        // TODO: Encode startup process with the type system if possible
        if self.startup_thread.is_none() {
            return;
        }
        {
            let tx =  self.startup_thread.take().unwrap();
            tx.send(true);
        }
    }
    pub fn to_main_screen(&mut self) {
        write!(
            self.stdout(),
            "{}{}",
            termion::screen::ToMainScreen,
            cursor::Show
        ).unwrap();
        self.flush();
        self.stdout = None;
        self.context.input_thread.send(false);
    }
    pub fn to_alternate_screen(&mut self) {
        let s = std::io::stdout();
        s.lock();
        self.stdout = Some(AlternateScreen::from(s.into_raw_mode().unwrap()));

        write!(
            self.stdout(),
            "{}{}",
            termion::screen::ToAlternateScreen,
            cursor::Hide
        ).unwrap();
        self.flush();
    }
}
impl<W: Write> State<W> {
    pub fn update_size(&mut self) {
        let termsize = termion::terminal_size().ok();
        let termcols = termsize.map(|(w, _)| w);
        let termrows = termsize.map(|(_, h)| h);
        if termcols.unwrap_or(72) as usize != self.cols
            || termrows.unwrap_or(120) as usize != self.rows
        {
            eprintln!(
                "Size updated, from ({}, {}) -> ({:?}, {:?})",
                self.cols, self.rows, termcols, termrows
            );
        }
        self.cols = termcols.unwrap_or(72) as usize;
        self.rows = termrows.unwrap_or(120) as usize;
        self.grid.resize(self.cols, self.rows, Cell::with_char(' '));

        self.rcv_event(UIEvent {
            id: 0,
            event_type: UIEventType::Resize,
        });
    }

    pub fn redraw(&mut self) {
        for i in 0..self.entities.len() {
            self.draw_entity(i);
        }
        let areas: Vec<Area> = self.context.dirty_areas.drain(0..).collect();
        /* draw each dirty area */
        for a in areas {
            self.draw_area(a);
        }
    }
    fn draw_area(&mut self, area: Area) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        for y in get_y(upper_left)..=get_y(bottom_right) {
            write!(
                self.stdout(),
                "{}",
                cursor::Goto(get_x(upper_left) as u16 + 1, (y + 1) as u16)
            ).unwrap();
            for x in get_x(upper_left)..=get_x(bottom_right) {
                let c = self.grid[(x, y)];

                if c.bg() != cells::Color::Default {
                    write!(self.stdout(), "{}", termion::color::Bg(c.bg().as_termion())).unwrap();
                }
                if c.fg() != cells::Color::Default {
                    write!(self.stdout(), "{}", termion::color::Fg(c.fg().as_termion())).unwrap();
                }
                write!(self.stdout(), "{}", c.ch()).unwrap();
                if c.bg() != cells::Color::Default {
                    write!(
                        self.stdout(),
                        "{}",
                        termion::color::Bg(termion::color::Reset)
                    ).unwrap();
                }
                if c.fg() != cells::Color::Default {
                    write!(
                        self.stdout(),
                        "{}",
                        termion::color::Fg(termion::color::Reset)
                    ).unwrap();
                }
            }
        }
        self.flush();
    }
    pub fn render(&mut self) {
        self.update_size();

        /* draw each entity */
        for i in 0..self.entities.len() {
            self.draw_entity(i);
        }
        let cols = self.cols;
        let rows = self.rows;

        self.draw_area(((0, 0), (cols - 1, rows - 1)));
    }
    pub fn draw_entity(&mut self, idx: usize) {
        let entity = &mut self.entities[idx];
        let upper_left = (0, 0);
        let bottom_right = (self.cols - 1, self.rows - 1);

        if entity.component.is_dirty() {
            entity.component.draw(
                &mut self.grid,
                (upper_left, bottom_right),
                &mut self.context,
            );
        }
    }
    pub fn register_entity(&mut self, entity: Entity) {
        self.entities.push(entity);
    }
    /// Convert user commands to actions/method calls.
    fn parse_command(&mut self, cmd: String) {
        let result = parse_command(&cmd.as_bytes()).to_full_result();

        if let Ok(v) = result {
            self.rcv_event(UIEvent { id: 0, event_type: UIEventType::Action(v) });
        }
    }

    pub fn rcv_event(&mut self, event: UIEvent) {
        match event.event_type {
            // Command type is handled only by State.
            UIEventType::Command(cmd) => {
                self.parse_command(cmd);
                return;
            }
            UIEventType::Fork(child) => {
                self.mode = UIMode::Fork;
                self.child = Some(child);
                self.flush();
                return;
            }
            UIEventType::EditDraft(mut file) => {
                use std::io::Read;
                use std::process::{Command, Stdio};
                let mut output = Command::new("msmtp")
                    .arg("-t")
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                    .expect("failed to execute process");
                {
                    let mut in_pipe = output.stdin.as_mut().unwrap();
                    let mut buf = Vec::new();
                    let mut f = file.file();

                    f.read_to_end(&mut buf).unwrap();
                    in_pipe.write(&buf).unwrap();
                    std::fs::remove_file(file.path()).unwrap();
                }
                output.wait_with_output().expect("Failed to read stdout");

                return;
            }
            UIEventType::Input(Key::Char('t')) => for i in 0..self.entities.len() {
                self.entities[i].rcv_event(
                    &UIEvent {
                        id: 0,
                        event_type: UIEventType::Action(Action::MailListing(
                            MailListingAction::ToggleThreaded,
                        )),
                    },
                    &mut self.context,
                );
            },

            _ => {}
        }
        /* inform each entity */
        for i in 0..self.entities.len() {
            self.entities[i].rcv_event(&event, &mut self.context);
        }

        if !self.context.replies.is_empty() {
            let replies: Vec<UIEvent>= self.context.replies.drain(0..).collect();
            // Pass replies to self and call count on the map iterator to force evaluation
            replies.into_iter().map(|r| self.rcv_event(r)).count();
        }
    }

    /// Tries to load a mailbox's content
    pub fn refresh_mailbox(&mut self, account_idx: usize, folder_idx: usize) {
        let flag = match &mut self.context.accounts[account_idx][folder_idx] {
            Ok(_) => true,
            Err(e) => {
                eprintln!("error {:?}", e);
                false
            }
        };
        if flag {
            self.rcv_event(UIEvent {
                id: 0,
                event_type: UIEventType::RefreshMailbox((account_idx, folder_idx)),
            });
        }
    }
    pub fn try_wait_on_child(&mut self) -> Option<bool> {
        if {
            match self.child {
                Some(ForkType::NewDraft(_, ref mut c)) => {
                    let mut w = c.try_wait();
                    match w {
                        Ok(Some(_)) => true,
                        Ok(None) => false,
                        Err(_) => {
                            return None;
                        }
                    }
                }
                Some(ForkType::Generic(ref mut c)) => {
                    let mut w = c.try_wait();
                    match w {
                        Ok(Some(_)) => true,
                        Ok(None) => false,
                        Err(_) => {
                            return None;
                        }
                    }
                }
                _ => {
                    return None;
                }
            }
        } {
            if let Some(ForkType::NewDraft(f, _)) = std::mem::replace(&mut self.child, None) {
                self.rcv_event(UIEvent {
                    id: 0,
                    event_type: UIEventType::EditDraft(f),
                });
            }
            return Some(true);
        }
        Some(false)
    }
    fn flush(&mut self) {
        self.stdout.as_mut().map(|s| s.flush().unwrap());
    }
    fn stdout(&mut self) -> &mut termion::screen::AlternateScreen<termion::raw::RawTerminal<W>> {
        self.stdout.as_mut().unwrap()
    }
}

#[derive(Debug)]
pub enum Key {
    /// Backspace.
    Backspace,
    /// Left arrow.
    Left,
    /// Right arrow.
    Right,
    /// Up arrow.
    Up,
    /// Down arrow.
    Down,
    /// Home key.
    Home,
    /// End key.
    End,
    /// Page Up key.
    PageUp,
    /// Page Down key.
    PageDown,
    /// Delete key.
    Delete,
    /// Insert key.
    Insert,
    /// Function keys.
    ///
    /// Only function keys 1 through 12 are supported.
    F(u8),
    /// Normal character.
    Char(char),
    /// Alt modified character.
    Alt(char),
    /// Ctrl modified character.
    ///
    /// Note that certain keys may not be modifiable with `ctrl`, due to limitations of terminals.
    Ctrl(char),
    /// Null byte.
    Null,
    /// Esc key.
    Esc,
}

impl From<TermionKey> for Key {
    fn from(k: TermionKey) -> Self {
        match k {
            TermionKey::Backspace => Key::Backspace,
            TermionKey::Left => Key::Left,
            TermionKey::Right => Key::Right,
            TermionKey::Up => Key::Up,
            TermionKey::Down => Key::Down,
            TermionKey::Home => Key::Home,
            TermionKey::End => Key::End,
            TermionKey::PageUp => Key::PageUp,
            TermionKey::PageDown => Key::PageDown,
            TermionKey::Delete => Key::Delete,
            TermionKey::Insert => Key::Insert,
            TermionKey::F(u) => Key::F(u),
            TermionKey::Char(c) => Key::Char(c),
            TermionKey::Alt(c) => Key::Alt(c),
            TermionKey::Ctrl(c) => Key::Ctrl(c),
            TermionKey::Null => Key::Null,
            TermionKey::Esc => Key::Esc,
            _ => Key::Char(' '),
        }
    }
}

/*
 * If we fork (for example start $EDITOR) we want the input-thread to stop reading from stdin. The
 * best way I came up with right now is to send a signal to the thread that is read in the first
 * input in stdin after the fork, and then the thread kills itself. The parent process spawns a new
 * input-thread when the child returns.
 *
 * The main loop uses try_wait_on_child() to check if child has exited.
 */
pub fn get_events(
    stdin: std::io::Stdin,
    mut closure: impl FnMut(Key),
    mut exit: impl FnMut(),
    rx: chan::Receiver<bool>,
) -> () {
    for c in stdin.keys() {
        chan_select! {
            default => {},
            rx.recv() -> val => {
                if let Some(true) = val {
                    exit();
                    return;
                } else if let Some(false) = val {
                    return;
                }
            }


        };
        if let Ok(k) = c {
            closure(Key::from(k));
        }
    }
}
