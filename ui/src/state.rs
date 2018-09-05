/*
 * meli - ui crate.
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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

/*! The application's state.

  The UI crate has an Entity-Component-System design. The System part, is also the application's state, so they're both merged in the `State` struct.

  `State` owns all the Entities of the UI, which are currently plain Containers for `Component`s. In the application's main event loop, input is handed to the state in the form of `UIEvent` objects which traverse the entity graph. Components decide to handle each input or not.

  Input is received in the main loop from threads which listen on the stdin for user input, observe folders for file changes etc. The relevant struct is `ThreadEvent`.
  */

use super::*;
use melib::backends::FolderHash;

use chan::{Receiver, Sender};
use fnv::FnvHashMap;
use std::io::Write;
use std::result;
use std::thread;
use std::time;
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use termion::{clear, cursor, style};

type StateStdout = termion::screen::AlternateScreen<termion::raw::RawTerminal<std::io::Stdout>>;

struct InputHandler {
    rx: Receiver<bool>,
    tx: Sender<bool>,
}

impl InputHandler {
    fn restore(&self, tx: Sender<ThreadEvent>) {
        let stdin = std::io::stdin();
        let rx = self.rx.clone();
        thread::Builder::new()
            .name("input-thread".to_string())
            .spawn(move || {
                get_events(
                    stdin,
                    |k| {
                        tx.send(ThreadEvent::Input(k));
                    },
                    || {
                        tx.send(ThreadEvent::UIEvent(UIEventType::ChangeMode(UIMode::Fork)));
                    },
                    &rx,
                )
            })
            .unwrap();
    }
    fn kill(&self) {
        self.tx.send(false);
    }
}

/// A context container for loaded settings, accounts, UI changes, etc.
pub struct Context {
    pub accounts: Vec<Account>,
    mailbox_hashes: FnvHashMap<FolderHash, (usize, usize)>,
    pub settings: Settings,

    pub runtime_settings: Settings,
    /// Areas of the screen that must be redrawn in the next render
    pub dirty_areas: VecDeque<Area>,

    /// Events queue that components send back to the state
    pub replies: VecDeque<UIEvent>,
    sender: Sender<ThreadEvent>,
    receiver: Receiver<ThreadEvent>,
    input: InputHandler,

    pub temp_files: Vec<File>,
}

impl Context {
    pub fn replies(&mut self) -> Vec<UIEvent> {
        self.replies.drain(0..).collect()
    }
    pub fn input_kill(&self) {
        self.input.kill();
    }
    pub fn restore_input(&self) {
        self.input.restore(self.sender.clone());
    }
    pub fn account_status(&mut self, idx_a: usize, idx_m: usize) -> result::Result<bool, usize> {
        match self.accounts[idx_a].status(idx_m) {
            Ok(()) => Ok(true),
            Err(n) => Err(n),
        }
    }
}

/// A State object to manage and own components and entities of the UI. `State` is responsible for
/// managing the terminal and interfacing with `melib`
pub struct State {
    cols: usize,
    rows: usize,

    grid: CellBuffer,
    stdout: Option<StateStdout>,
    child: Option<ForkType>,
    pub mode: UIMode,
    entities: Vec<Entity>,
    pub context: Context,

    startup_thread: Option<chan::Sender<bool>>,

    threads: FnvHashMap<thread::ThreadId, (chan::Sender<bool>, thread::JoinHandle<()>)>,
}

impl Drop for State {
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

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    pub fn new() -> Self {
        /* Create a channel to communicate with other threads. The main process is the sole receiver.
         * */
        let (sender, receiver) = chan::sync(::std::mem::size_of::<ThreadEvent>());

        /*
         * Create async channel to block the input-thread if we need to fork and stop it from reading
         * stdin, see get_events() for details
         * */
        let input_thread = chan::async();
        let _stdout = std::io::stdout();
        _stdout.lock();
        let backends = Backends::new();
        let settings = Settings::new();
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
                                break;
                            }
                        }
                        sender.send(ThreadEvent::UIEvent(UIEventType::StartupCheck));
                        thread::sleep(dur);
                    }
                    startup_rx.recv();
                    return;
                })
                .unwrap()
        };
        let mut s = State {
            cols,
            rows,
            grid: CellBuffer::new(cols, rows, Cell::with_char(' ')),
            stdout: Some(stdout),
            child: None,
            mode: UIMode::Normal,
            entities: Vec::with_capacity(1),

            context: Context {
                accounts,
                mailbox_hashes: FnvHashMap::with_capacity_and_hasher(1, Default::default()),

                settings: settings.clone(),
                runtime_settings: settings,
                dirty_areas: VecDeque::with_capacity(5),
                replies: VecDeque::with_capacity(5),
                temp_files: Vec::new(),

                sender,
                receiver,
                input: InputHandler {
                    rx: input_thread.1,
                    tx: input_thread.0,
                },
            },
            startup_thread: Some(startup_tx.clone()),
            threads: FnvHashMap::with_capacity_and_hasher(1, Default::default()),
        };
        s.threads.insert(
            startup_thread.thread().id(),
            (startup_tx.clone(), startup_thread),
        );
        write!(
            s.stdout(),
            "{}{}{}",
            cursor::Hide,
            clear::All,
            cursor::Goto(1, 1)
        ).unwrap();
        s.flush();
        eprintln!("DEBUG: inserting mailbox hashes:");
        for (x, account) in s.context.accounts.iter_mut().enumerate() {
            for (y, folder) in account.backend.folders().iter().enumerate() {
                eprintln!("{:?}", folder);
                s.context.mailbox_hashes.insert(folder.hash(), (x, y));
            }
            let sender = s.context.sender.clone();
            account.watch(RefreshEventConsumer::new(Box::new(move |r| {
                sender.send(ThreadEvent::from(r));
            })));
        }
        s.restore_input();
        s
    }
    /*
     * When we receive a folder hash from a watcher thread,
     * we match the hash to the index of the mailbox, request a reload
     * and startup a thread to remind us to poll it every now and then till it's finished.
     */
    pub fn refresh_event(&mut self, event: RefreshEvent) {
        let hash = event.hash();
        if let Some(&(idxa, idxm)) = self.context.mailbox_hashes.get(&hash) {
            if let Some(notification) = self.context.accounts[idxa].reload(event, idxm) {
                self.context.replies.push_back(UIEvent {
                    id: 0,
                    event_type: notification,
                });
            }

            let (startup_tx, startup_rx) = chan::async();
            let startup_thread = {
                let sender = self.context.sender.clone();
                let startup_rx = startup_rx.clone();

                thread::Builder::new()
                    .name("startup-thread".to_string())
                    .spawn(move || {
                        let dur = time::Duration::from_millis(100);
                        loop {
                            chan_select! {
                                default => {},
                                startup_rx.recv() -> _ => {
                                    sender.send(ThreadEvent::UIEvent(UIEventType::MailboxUpdate((idxa,idxm))));
                                    sender.send(ThreadEvent::ThreadJoin(thread::current().id()));
                                    return;
                                }
                            }
                            sender.send(ThreadEvent::UIEvent(UIEventType::StartupCheck));
                            thread::sleep(dur);
                        }
                    })
                    .expect("Failed to spawn startup-thread in hash_to_folder()")
            };
            self.startup_thread = Some(startup_tx.clone());
            self.threads
                .insert(startup_thread.thread().id(), (startup_tx, startup_thread));
        } else {
            eprintln!(
                "BUG: mailbox with hash {} not found in mailbox_hashes.",
                hash
            );
        }
    }

    /// If an owned thread returns a `ThreadEvent::ThreadJoin` event to `State` then it must remove
    /// the thread from its list and `join` it.
    pub fn join(&mut self, id: thread::ThreadId) {
        let (tx, handle) = self.threads.remove(&id).unwrap();
        tx.send(true);
        handle.join().unwrap();
    }

    /// If startup has finished, inform startup thread that it doesn't need to tick us with startup
    /// check reminders and let it die.
    pub fn finish_startup(&mut self) {
        // TODO: Encode startup process with the type system if possible
        if self.startup_thread.is_none() {
            return;
        }
        {
            let tx = self.startup_thread.take().unwrap();
            tx.send(true);
        }
    }

    /// Switch back to the terminal's main screen (The command line the user sees before opening
    /// the application)
    pub fn switch_to_main_screen(&mut self) {
        write!(
            self.stdout(),
            "{}{}",
            termion::screen::ToMainScreen,
            cursor::Show
        ).unwrap();
        self.flush();
        self.stdout = None;
        self.context.input.kill();
    }
    pub fn switch_to_alternate_screen(&mut self) {
        let s = std::io::stdout();
        s.lock();
        self.stdout = Some(AlternateScreen::from(s.into_raw_mode().unwrap()));

        write!(
            self.stdout(),
            "{}{}{}{}",
            termion::screen::ToAlternateScreen,
            cursor::Hide,
            clear::All,
            cursor::Goto(1, 1)
        ).unwrap();
        self.flush();
    }

    pub fn receiver(&self) -> Receiver<ThreadEvent> {
        self.context.receiver.clone()
    }
    pub fn restore_input(&mut self) {
        self.context.restore_input();
    }

    /// On `SIGWNICH` the `State` redraws itself according to the new terminal size.
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

        // Invalidate dirty areas.
        self.context.dirty_areas.clear();
    }

    /// Force a redraw for all dirty components.
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

    /// Draw only a specific `area` on the screen.
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

                if c.bg() != Color::Default {
                    write!(self.stdout(), "{}", termion::color::Bg(c.bg().as_termion())).unwrap();
                }
                if c.fg() != Color::Default {
                    write!(self.stdout(), "{}", termion::color::Fg(c.fg().as_termion())).unwrap();
                }
                write!(self.stdout(), "{}", c.ch()).unwrap();
                if c.bg() != Color::Default {
                    write!(
                        self.stdout(),
                        "{}",
                        termion::color::Bg(termion::color::Reset)
                    ).unwrap();
                }
                if c.fg() != Color::Default {
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

    /// Draw the entire screen from scratch.
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
    fn parse_command(&mut self, cmd: &str) {
        let result = parse_command(&cmd.as_bytes()).to_full_result();

        if let Ok(v) = result {
            self.rcv_event(UIEvent {
                id: 0,
                event_type: UIEventType::Action(v),
            });
        }
    }

    /// The application's main loop sends `UIEvents` to state via this method.
    pub fn rcv_event(&mut self, event: UIEvent) {
        match event.event_type {
            // Command type is handled only by State.
            UIEventType::Command(cmd) => {
                self.parse_command(&cmd);
                return;
            }
            UIEventType::Fork(child) => {
                self.mode = UIMode::Fork;
                self.child = Some(child);
                if let Some(ForkType::Finished) = self.child {
                    /*
                     * Fork has finished in the past.
                     * We're back in the AlternateScreen, but the cursor is reset to Shown, so fix
                     * it.
                     */
                    write!(self.stdout(), "{}", cursor::Hide,).unwrap();
                    self.flush();
                }
                return;
            }
            UIEventType::Input(Key::Char('t')) => for i in 0..self.entities.len() {
                self.entities[i].rcv_event(
                    &UIEvent {
                        id: 0,
                        event_type: UIEventType::Action(Action::Listing(
                            ListingAction::ToggleThreaded,
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
            let replies: Vec<UIEvent> = self.context.replies.drain(0..).collect();
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
        let should_return_flag = match self.child {
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
            Some(ForkType::Finished) => {
                /* Fork has already finished */
                std::mem::replace(&mut self.child, None);
                return None;
            }
            _ => {
                return None;
            }
        };
        if should_return_flag {
            return Some(true);
        }
        Some(false)
    }
    fn flush(&mut self) {
        if let Some(s) = self.stdout.as_mut() {
            s.flush().unwrap();
        }
    }
    fn stdout(&mut self) -> &mut StateStdout {
        self.stdout.as_mut().unwrap()
    }
}
