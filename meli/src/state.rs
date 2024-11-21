/*
 * meli
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

//! The application's state.
//!
//! The UI crate has an [`Box<dyn
//! Component>`](crate::components::Component)-Component-System design. The
//! system part, is also the application's state, so they're both merged in the
//! [`State`] struct.
//!
//! [`State`] owns all the Components of the UI. In the application's main event
//! loop, input is handed to the state in the form of [`UIEvent`] objects which
//! traverse the component graph. Components decide to handle each input or not.
//!
//! Input is received in the main loop from threads which listen on the stdin
//! for user input, observe folders for file changes etc. The relevant struct is
//! [`ThreadEvent`].

use std::{
    borrow::Cow,
    collections::BTreeSet,
    os::fd::{AsRawFd, FromRawFd, OwnedFd},
    path::{Path, PathBuf},
    sync::Arc,
    thread,
};

use crossbeam::channel::{unbounded, Receiver, Sender};
use indexmap::{IndexMap, IndexSet};
use melib::{
    backends::{
        AccountHash, BackendEvent, BackendEventConsumer, Backends, RefreshEvent, RefreshEventKind,
    },
    utils::datetime,
};
use smallvec::SmallVec;

use super::*;
use crate::{
    conf::data_types::SearchBackend,
    jobs::JobExecutor,
    notifications::DisplayMessageBox,
    terminal::{get_events, Screen, Tty},
};

struct InputHandler {
    pipe: (OwnedFd, OwnedFd),
    rx: Receiver<InputCommand>,
    tx: Sender<InputCommand>,
    state_tx: Sender<ThreadEvent>,
    control: std::sync::Weak<()>,
}

impl InputHandler {
    fn restore(&mut self) {
        let working = Arc::new(());
        let control = Arc::downgrade(&working);

        /* Clear channel without blocking. switch_to_main_screen() issues a kill when
         * returning from a fork and there's no input thread, so the newly created
         * thread will receive it and die. */
        //let _ = self.rx.try_iter().count();
        let rx = self.rx.clone();
        let pipe = nix::unistd::dup(self.pipe.0.as_raw_fd())
            .expect("Fatal: Could not dup() input pipe file descriptor");
        let tx = self.state_tx.clone();
        thread::Builder::new()
            .name("input-thread".to_string())
            .spawn(move || {
                let pipe = unsafe { OwnedFd::from_raw_fd(pipe) };
                get_events(
                    |i| {
                        tx.send(ThreadEvent::Input(i)).unwrap();
                    },
                    &rx,
                    &pipe,
                    working,
                )
            })
            .unwrap();
        self.control = control;
    }

    fn kill(&self) {
        let _ = nix::unistd::write(&self.pipe.1, &[1]);
        self.tx.send(InputCommand::Kill).unwrap();
    }

    fn check(&mut self) {
        match self.control.upgrade() {
            Some(_) => {}
            None => {
                log::trace!("restarting input_thread");
                self.restore();
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct MainLoopHandler {
    pub sender: Sender<ThreadEvent>,
    pub job_executor: Arc<JobExecutor>,
}

impl MainLoopHandler {
    #[inline]
    pub fn send(&self, event: ThreadEvent) {
        if let Err(err) = self.sender.send(event) {
            log::error!("Could not send event to main loop: {}", err);
        }
    }
}

/// A context container for loaded settings, accounts, UI changes, etc.
pub struct Context {
    pub accounts: IndexMap<AccountHash, Account>,
    pub settings: Box<Settings>,

    /// Areas of the screen that must be redrawn in the next render
    pub dirty_areas: VecDeque<Area>,

    /// Events queue that components send back to the state
    pub replies: VecDeque<UIEvent>,
    pub realized: IndexMap<ComponentId, Option<ComponentId>>,
    pub unrealized: IndexSet<ComponentId>,
    pub main_loop_handler: MainLoopHandler,
    pub receiver: Receiver<ThreadEvent>,
    input_thread: InputHandler,
    current_dir: PathBuf,
    /// Children processes
    pub children: IndexMap<Cow<'static, str>, Vec<std::process::Child>>,
    pub temp_files: Vec<File>,
}

impl Context {
    pub fn replies(&mut self) -> smallvec::SmallVec<[UIEvent; 8]> {
        self.replies.drain(0..).collect()
    }

    pub fn input_kill(&self) {
        self.input_thread.kill();
    }

    pub fn restore_input(&mut self) {
        self.input_thread.restore();
    }

    pub fn is_online_idx(&mut self, account_pos: usize) -> Result<()> {
        let Self {
            ref mut accounts,
            ref mut replies,
            ..
        } = self;
        let was_online = accounts[account_pos].is_online.is_true();
        let ret = accounts[account_pos].is_online();
        if ret.is_ok() && !was_online {
            log::trace!("inserting mailbox hashes:");
            for mailbox_node in accounts[account_pos].list_mailboxes() {
                log::trace!(
                    "hash & mailbox: {:?} {}",
                    mailbox_node.hash,
                    accounts[account_pos][&mailbox_node.hash].name()
                );
            }
            accounts[account_pos].watch(None);

            replies.push_back(UIEvent::AccountStatusChange(
                accounts[account_pos].hash(),
                None,
            ));
        }
        if ret.is_ok() != was_online {
            replies.push_back(UIEvent::AccountStatusChange(
                accounts[account_pos].hash(),
                None,
            ));
        }
        ret
    }

    pub fn is_online(&mut self, account_hash: AccountHash) -> Result<()> {
        let idx = self.accounts.get_index_of(&account_hash).unwrap();
        self.is_online_idx(idx)
    }

    #[cfg(test)]
    pub fn new_mock(dir: &tempfile::TempDir) -> Self {
        use crate::conf::tests::{ConfigFile, IMAP_CONFIG};

        let (sender, receiver) =
            crossbeam::channel::bounded(32 * ::std::mem::size_of::<ThreadEvent>());
        let job_executor = Arc::new(JobExecutor::new(sender.clone()));
        let input_thread = unbounded();
        let input_thread_pipe = crate::types::pipe().unwrap();
        let backends = Backends::new();
        let config_file = ConfigFile::new(IMAP_CONFIG, dir).unwrap();
        std::env::set_var("MELI_CONFIG", &config_file.path);
        let settings = Box::new(Settings::new().unwrap());
        let accounts = vec![{
            let name = "test".to_string();
            let mut account_conf = crate::conf::AccountConf::default();
            account_conf.conf.format = "maildir".to_string();
            account_conf.account.format = "maildir".to_string();
            account_conf.account.root_mailbox = dir.path().display().to_string();
            let sender = sender.clone();
            let account_hash = AccountHash::from_bytes(name.as_bytes());
            Account::new(
                account_hash,
                name,
                account_conf,
                &backends,
                MainLoopHandler {
                    job_executor: job_executor.clone(),
                    sender: sender.clone(),
                },
                BackendEventConsumer::new(Arc::new(
                    move |account_hash: AccountHash, ev: BackendEvent| {
                        sender
                            .send(ThreadEvent::UIEvent(UIEvent::BackendEvent(
                                account_hash,
                                ev,
                            )))
                            .unwrap();
                    },
                )),
            )
            .unwrap()
        }];
        let accounts = accounts.into_iter().map(|acc| (acc.hash(), acc)).collect();
        let working = Arc::new(());
        let control = Arc::downgrade(&working);
        Self {
            accounts,
            settings,
            dirty_areas: VecDeque::with_capacity(0),
            replies: VecDeque::with_capacity(0),
            realized: IndexMap::default(),
            unrealized: IndexSet::default(),
            temp_files: Vec::new(),
            current_dir: std::env::current_dir().unwrap(),
            children: IndexMap::default(),

            input_thread: InputHandler {
                pipe: input_thread_pipe,
                rx: input_thread.1,
                tx: input_thread.0,
                control,
                state_tx: sender.clone(),
            },
            main_loop_handler: MainLoopHandler {
                job_executor,
                sender,
            },
            receiver,
        }
    }

    pub fn current_dir(&self) -> &Path {
        &self.current_dir
    }
}

/// A State object to manage and own components and components of the UI.
/// `State` is responsible for managing the terminal and interfacing with
/// `melib`
pub struct State {
    screen: Box<Screen<Tty>>,
    draw_rate_limit: RateLimit,
    child: Option<ForkType>,
    pub mode: UIMode,
    overlay: IndexMap<ComponentId, Box<dyn Component>>,
    components: IndexMap<ComponentId, Box<dyn Component>>,
    component_tree: IndexMap<ComponentId, ComponentPath>,
    pub context: Box<Context>,
    timer: thread::JoinHandle<()>,
    message_box: DisplayMessageBox,
}

impl Drop for State {
    fn drop(&mut self) {
        // When done, restore the defaults to avoid messing with the terminal.
        self.screen.switch_to_main_screen();
        use nix::sys::wait::{waitpid, WaitPidFlag};
        for (id, pid, err) in self
            .context
            .children
            .iter_mut()
            .flat_map(|(i, v)| v.drain(..).map(move |v| (i, v)))
            .filter_map(|(id, child)| {
                if let Err(err) = waitpid(
                    nix::unistd::Pid::from_raw(child.id() as i32),
                    Some(WaitPidFlag::WNOHANG),
                ) {
                    Some((id, child.id(), err))
                } else {
                    None
                }
            })
        {
            log::trace!("Failed to wait on subprocess {} ({}): {}", id, pid, err);
        }
        if let Some(ForkType::Embedded { id, command, pid }) = self.child.take() {
            /* Try wait, we don't want to block */
            if let Err(err) = waitpid(pid, Some(WaitPidFlag::WNOHANG)) {
                log::trace!(
                    "Failed to wait on embedded process {} {} ({}): {}",
                    id,
                    if let Some(v) = command.as_ref() {
                        v.as_ref()
                    } else {
                        ""
                    },
                    pid,
                    err
                );
            }
        }
    }
}

impl State {
    pub fn new(
        settings: Option<Settings>,
        sender: Sender<ThreadEvent>,
        receiver: Receiver<ThreadEvent>,
    ) -> Result<Self> {
        // Create async channel to block the input-thread if we need to fork and stop it
        // from reading stdin, see get_events() for details
        let input_thread = unbounded();
        let input_thread_pipe = crate::types::pipe()?;
        let backends = Backends::new();
        let settings = Box::new(if let Some(settings) = settings {
            settings
        } else {
            Settings::new()?
        });

        let (cols, rows) = termion::terminal_size().chain_err_summary(|| {
            "Could not determine terminal size. Are you running this on a tty? If yes, do you need \
             permissions for tty ioctls?"
        })?;
        let (cols, rows) = (cols as usize, rows as usize);

        let job_executor = Arc::new(JobExecutor::new(sender.clone()));
        let accounts = {
            settings
                .accounts
                .iter()
                .map(|(n, a_s)| {
                    let sender = sender.clone();
                    let account_hash = AccountHash::from_bytes(n.as_bytes());
                    Account::new(
                        account_hash,
                        n.to_string(),
                        a_s.clone(),
                        &backends,
                        MainLoopHandler {
                            job_executor: job_executor.clone(),
                            sender: sender.clone(),
                        },
                        BackendEventConsumer::new(Arc::new(
                            move |account_hash: AccountHash, ev: BackendEvent| {
                                sender
                                    .send(ThreadEvent::UIEvent(UIEvent::BackendEvent(
                                        account_hash,
                                        ev,
                                    )))
                                    .unwrap();
                            },
                        )),
                    )
                })
                .collect::<Result<Vec<Account>>>()?
        };
        let accounts = accounts.into_iter().map(|acc| (acc.hash(), acc)).collect();

        let timer = {
            let sender = sender.clone();
            thread::Builder::new().spawn(move || {
                let sender = sender;
                loop {
                    thread::park();

                    sender.send(ThreadEvent::Pulse).unwrap();
                    thread::sleep(std::time::Duration::from_millis(100));
                }
            })
        }?;

        timer.thread().unpark();

        let working = Arc::new(());
        let control = Arc::downgrade(&working);
        let mut screen =
            Box::new(Screen::<Tty>::new(Default::default()).with_cols_and_rows(cols, rows));
        screen
            .tty_mut()
            .set_mouse(settings.terminal.use_mouse.is_true())
            .set_draw_fn(if settings.terminal.use_color() {
                Screen::draw_horizontal_segment
            } else {
                Screen::draw_horizontal_segment_no_color
            });
        let message_box = DisplayMessageBox::new(&screen);
        let mut s = Self {
            screen,
            child: None,
            mode: UIMode::Normal,
            components: IndexMap::default(),
            overlay: IndexMap::default(),
            component_tree: IndexMap::default(),
            timer,
            draw_rate_limit: RateLimit::new(1, 3, job_executor.clone()),
            message_box,
            context: Box::new(Context {
                accounts,
                settings,
                dirty_areas: VecDeque::with_capacity(5),
                replies: VecDeque::with_capacity(5),
                realized: IndexMap::default(),
                unrealized: IndexSet::default(),
                temp_files: Vec::new(),
                current_dir: std::env::current_dir()?,
                children: IndexMap::default(),

                input_thread: InputHandler {
                    pipe: input_thread_pipe,
                    rx: input_thread.1,
                    tx: input_thread.0,
                    control,
                    state_tx: sender.clone(),
                },
                main_loop_handler: MainLoopHandler {
                    job_executor,
                    sender,
                },
                receiver,
            }),
        };
        if s.context.settings.terminal.ascii_drawing {
            s.screen.grid_mut().set_ascii_drawing(true);
            s.screen.overlay_grid_mut().set_ascii_drawing(true);
        }
        if s.context.settings.terminal.use_text_presentation() {
            s.screen.grid_mut().set_force_text_presentation(true);
            s.screen
                .overlay_grid_mut()
                .set_force_text_presentation(true);
        }

        s.screen.switch_to_alternate_screen(&s.context);
        s.screen.do_background_query();
        for i in 0..s.context.accounts.len() {
            if !s.context.accounts[i].backend_capabilities.is_remote {
                s.context.accounts[i].watch(None);
            }
            if s.context.is_online_idx(i).is_ok() && s.context.accounts[i].is_empty() {
                //return Err(Error::new(format!(
                //    "Account {} has no mailboxes configured.",
                //    s.context.accounts[i].name()
                //)));
            }
        }
        s.context.restore_input();
        Ok(s)
    }

    /*
     * When we receive a mailbox hash from a watcher thread,
     * we match the hash to the index of the mailbox, request a reload
     * and startup a thread to remind us to poll it every now and then till it's
     * finished.
     */
    pub fn refresh_event(
        &mut self,
        account_hash: AccountHash,
        mailbox_hash: MailboxHash,
        events: Vec<RefreshEventKind>,
    ) {
        if self.context.accounts[&account_hash]
            .mailbox_entries
            .contains_key(&mailbox_hash)
        {
            if self.context.accounts[&account_hash]
                .load(mailbox_hash, false)
                .is_err()
            {
                self.context.accounts[&account_hash]
                    .event_queue
                    .entry(mailbox_hash)
                    .or_default()
                    .extend(events);
                return;
            }
            let Context {
                ref mut accounts, ..
            } = &mut *self.context;

            if let Some(notifications) = accounts[&account_hash].reload(events, mailbox_hash) {
                for n in notifications {
                    if matches!(n, UIEvent::Notification { .. }) {
                        self.rcv_event(UIEvent::MailboxUpdate((account_hash, mailbox_hash)));
                    }
                    self.rcv_event(n);
                }
            }
        }
    }

    pub fn receiver(&self) -> Receiver<ThreadEvent> {
        self.context.receiver.clone()
    }

    pub fn sender(&self) -> Sender<ThreadEvent> {
        self.context.main_loop_handler.sender.clone()
    }

    pub fn restore_input(&mut self) {
        self.context.restore_input();
    }

    /// On `SIGWINCH` the `State` redraws itself according to the new terminal
    /// size.
    pub fn update_size(&mut self) {
        self.screen.update_size();
        self.rcv_event(UIEvent::Resize);
        self.message_box.set_dirty(true);
        self.message_box.initialised = false;

        // Invalidate dirty areas.
        self.context.dirty_areas.clear();
    }

    /// Force a redraw for all dirty components.
    pub fn redraw(&mut self) {
        if !self.draw_rate_limit.tick() {
            return;
        }

        for i in 0..self.components.len() {
            self.draw_component(i);
        }
        let mut areas: smallvec::SmallVec<[Area; 8]> =
            self.context.dirty_areas.drain(0..).collect();

        let can_draw_above_screen: bool = !matches!(self.mode, UIMode::Embedded | UIMode::Fork);
        if self.message_box.active {
            let now = datetime::now();
            if self
                .message_box
                .expiration_start
                .map(|t| t + 5 < now)
                .unwrap_or(false)
            {
                self.message_box.active = false;
                self.message_box.set_dirty(true);
                self.message_box.initialised = false;
                self.message_box.expiration_start = None;
                areas.push(self.screen.area());
            }
        }

        /* Sort by x_start, ie upper_left corner's x coordinate */
        areas.sort_by(|a, b| a.upper_left().0.partial_cmp(&b.upper_left().0).unwrap());

        if self.message_box.active && can_draw_above_screen {
            /* Check if any dirty area intersects with the area occupied by
             * floating notification box */
            let displ = self.message_box.cached_area();
            let (displ_top, displ_bot) = (displ.upper_left(), displ.bottom_right());
            let mut is_dirty = self.message_box.is_dirty();
            for a in &areas {
                let (top_x, top_y) = a.upper_left();
                let (bottom_x, bottom_y) = a.bottom_right();
                is_dirty |= !(bottom_y < displ_top.1
                    || displ_bot.1 < top_y
                    || bottom_x < displ_top.0
                    || displ_bot.0 < top_x);
            }
            self.message_box.set_dirty(is_dirty);
        }
        /* draw each dirty area */
        let rows = self.screen.area().height();
        for y in 0..rows {
            let mut segment = None;
            for ((x_start, y_start), (x_end, y_end)) in
                areas.iter().map(|a| (a.upper_left(), a.bottom_right()))
            {
                if y < y_start || y > y_end {
                    continue;
                }
                if let Some((x_start, x_end)) = segment.take() {
                    self.screen.draw(x_start..(x_end + 1), y);
                }
                match segment {
                    None => {
                        segment = Some((x_start, x_end));
                    }
                    Some((p_x_start, p_x_end)) if p_x_end < x_start => {
                        self.screen.draw(p_x_start..(p_x_end + 1), y);
                        segment = Some((x_start, x_end));
                    }
                    Some((p_x_start, p_x_end)) if p_x_end < x_end => {
                        self.screen.draw(p_x_start..(p_x_end + 1), y);
                        segment = Some((p_x_end, x_end));
                    }
                    Some((_, ref mut x)) => {
                        *x = x_end;
                    }
                }
            }
            if let Some((x_start, x_end)) = segment {
                self.screen.draw(x_start..(x_end + 1), y);
            }
        }

        if self.message_box.is_dirty() && self.message_box.active && can_draw_above_screen {
            if !self.message_box.is_empty() {
                if !self.message_box.initialised {
                    {
                        /* Clear area previously occupied by floating
                         * notification box */
                        if self.message_box.cached_area().generation()
                            == self.screen.area().generation()
                        {
                            for row in self
                                .screen
                                .grid()
                                .bounds_iter(self.message_box.cached_area())
                            {
                                self.screen.draw(row.cols(), row.row_index());
                            }
                        }
                    }
                }
                let area = self.screen.area();
                self.message_box
                    .draw(self.screen.overlay_grid_mut(), area, &mut self.context);
                for row in self
                    .screen
                    .overlay_grid()
                    .bounds_iter(self.message_box.cached_area())
                {
                    self.screen.draw_overlay(row.cols(), row.row_index());
                }
            }
            self.message_box.set_dirty(false);
        } else if self.message_box.is_dirty() && can_draw_above_screen {
            /* Clear area previously occupied by floating notification box */
            if self.message_box.cached_area().generation() == self.screen.area().generation() {
                for row in self
                    .screen
                    .grid()
                    .bounds_iter(self.message_box.cached_area())
                {
                    self.screen.draw(row.cols(), row.row_index());
                }
            }
            self.message_box.set_dirty(false);
        }

        if let (Some((_, overlay_widget)), true) =
            (self.overlay.get_index_mut(0), can_draw_above_screen)
        {
            let area: Area = self.screen.area();
            {
                let (grid, overlay_grid) = self.screen.grid_and_overlay_grid_mut();
                overlay_grid.copy_area(grid, area, area);
                overlay_widget.draw(overlay_grid, area, &mut self.context);
            }
            for row in self.screen.overlay_grid().bounds_iter(area) {
                self.screen.draw_overlay(row.cols(), row.row_index());
            }
        }
        self.flush();
    }

    /// Draw the entire screen from scratch.
    pub fn render(&mut self) {
        self.screen.update_size();
        self.context.dirty_areas.push_back(self.screen.area());

        self.redraw();
    }

    pub fn draw_component(&mut self, idx: usize) {
        let component = &mut self.components[idx];

        if component.is_dirty() {
            let area = self.screen.area();
            component.draw(self.screen.grid_mut(), area, &mut self.context);
        }
    }

    pub fn can_quit_cleanly(&mut self) -> bool {
        let Self {
            ref mut components,
            ref context,
            ..
        } = self;
        components.values_mut().all(|c| c.can_quit_cleanly(context))
    }

    pub fn register_component(&mut self, component: Box<dyn Component>) {
        component.realize(None, &mut self.context);
        self.components.insert(component.id(), component);
    }

    /// Convert user commands to actions/method calls.
    fn exec_command(&mut self, cmd: Action) {
        match cmd {
            SetEnv(key, val) => {
                // SAFETY: we only modify our environment from the main process/thread.
                std::env::set_var(key.as_str(), val.as_str());
            }
            PrintEnv(key) => {
                self.context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                        std::env::var(key.as_str()).unwrap_or_else(|e| e.to_string()),
                    )));
            }
            ChangeCurrentDirectory(dir) => {
                self.context.current_dir = dir;
                self.context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                        self.context.current_dir.display().to_string(),
                    )));
            }
            CurrentDirectory => {
                self.context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                        self.context.current_dir.display().to_string(),
                    )));
            }
            Mailbox(account_name, op) => {
                if let Some(account) = self
                    .context
                    .accounts
                    .values_mut()
                    .find(|a| a.name() == account_name)
                {
                    if let Err(err) = account.mailbox_operation(op) {
                        self.context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                } else {
                    self.context.replies.push_back(UIEvent::StatusEvent(
                        StatusEvent::DisplayMessage(format!(
                            "Account with name `{}` not found.",
                            account_name
                        )),
                    ));
                }
            }
            #[cfg(feature = "sqlite3")]
            AccountAction(ref account_name, ReIndex) => {
                let account_index = if let Some(a) = self
                    .context
                    .accounts
                    .iter()
                    .position(|(_, acc)| acc.name() == account_name)
                {
                    a
                } else {
                    self.context.replies.push_back(UIEvent::Notification {
                        title: None,
                        source: None,
                        body: format!("Account {account_name} was not found.").into(),
                        kind: Some(NotificationType::Error(ErrorKind::None)),
                    });
                    return;
                };
                if *self.context.accounts[account_index]
                    .settings
                    .conf
                    .search_backend()
                    != SearchBackend::Sqlite3
                {
                    self.context.replies.push_back(UIEvent::Notification {
                        title: None,
                        source: None,
                        body: format!(
                            "Account {account_name} doesn't have an sqlite3 search backend.",
                        )
                        .into(),
                        kind: Some(NotificationType::Error(ErrorKind::None)),
                    });
                    return;
                }
                let account = &self.context.accounts[account_index];
                let (acc_name, backend_mutex): (Arc<str>, Arc<_>) =
                    (Arc::clone(&account.name), account.backend.clone());
                let job = crate::sqlite3::AccountCache::index(
                    acc_name,
                    account.collection.clone(),
                    backend_mutex,
                );
                let handle = self.context.main_loop_handler.job_executor.spawn(
                    "sqlite3::index".into(),
                    job,
                    crate::sqlite3::AccountCache::is_async(),
                );
                self.context.accounts[account_index].active_jobs.insert(
                    handle.job_id,
                    crate::accounts::JobRequest::Generic {
                        name: "Message index rebuild".into(),
                        handle,
                        on_finish: None,
                        log_level: LogLevel::INFO,
                    },
                );
                self.context.replies.push_back(UIEvent::Notification {
                    title: None,
                    source: None,
                    body: "Message index rebuild started.".into(),
                    kind: Some(NotificationType::Info),
                });
            }
            #[cfg(not(feature = "sqlite3"))]
            AccountAction(_, ReIndex) => {
                self.context.replies.push_back(UIEvent::Notification {
                    title: None,
                    source: None,
                    body: "Message index rebuild failed: meli is not built with sqlite3 support."
                        .into(),
                    kind: Some(NotificationType::Error(ErrorKind::None)),
                });
            }
            AccountAction(ref account_name, PrintAccountSetting(ref setting)) => {
                let path = setting.split('.').collect::<SmallVec<[&str; 16]>>();
                if let Some(pos) = self
                    .context
                    .accounts
                    .iter()
                    .position(|(_h, a)| a.name() == account_name)
                {
                    self.context.replies.push_back(UIEvent::StatusEvent(
                        StatusEvent::UpdateStatus(
                            self.context.accounts[pos]
                                .settings
                                .lookup("settings", &path)
                                .unwrap_or_else(|err| err.to_string()),
                        ),
                    ));
                } else {
                    self.context.replies.push_back(UIEvent::Notification {
                        title: None,
                        source: None,
                        body: format!("Account {account_name} was not found.").into(),
                        kind: Some(NotificationType::Error(ErrorKind::None)),
                    });
                }
            }
            PrintSetting(ref setting) => {
                let path = setting.split('.').collect::<SmallVec<[&str; 16]>>();
                self.context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.context
                            .settings
                            .lookup("settings", &path)
                            .unwrap_or_else(|err| err.to_string()),
                    )));
            }
            ToggleMouse => {
                let new_val = !self.screen.tty().mouse();
                self.screen.tty_mut().set_mouse(new_val);
                self.rcv_event(UIEvent::StatusEvent(StatusEvent::SetMouse(new_val)));
            }
            Quit => {
                self.context
                    .main_loop_handler
                    .sender
                    .send(ThreadEvent::Input((
                        self.context.settings.shortcuts.general.quit.clone(),
                        vec![],
                    )))
                    .unwrap();
            }
            #[cfg(feature = "cli-docs")]
            Tab(Man(manpage)) => match manpage
                .read(false)
                .map(|text| crate::manpages::ManPages::remove_markup(&text).unwrap_or(text))
            {
                Ok(m) => self.rcv_event(UIEvent::Action(Tab(New(Some(Box::new(
                    Pager::from_string(
                        m,
                        &self.context,
                        None,
                        None,
                        crate::conf::value(&self.context, "theme_default"),
                    ),
                )))))),
                Err(err) => self.context.replies.push_back(UIEvent::Notification {
                    title: None,
                    body: "Encountered an error while retrieving manual page.".into(),
                    source: Some(err),
                    kind: Some(NotificationType::Error(ErrorKind::Bug)),
                }),
            },
            v => {
                self.rcv_event(UIEvent::Action(v));
            }
        }
    }

    /// The application's main loop sends `UIEvents` to state via this method.
    pub fn rcv_event(&mut self, mut event: UIEvent) {
        if let UIEvent::Input(_) = event {
            if self.message_box.expiration_start.is_none() {
                self.message_box.expiration_start = Some(datetime::now());
            }
        }

        match event {
            // Command type is handled only by State.
            UIEvent::Command(cmd) => {
                match parse_command(cmd.as_bytes()) {
                    Ok(action) => {
                        if action.needs_confirmation() {
                            let new = Box::new(UIConfirmationDialog::new(
                                "Are you sure?",
                                vec![(true, "yes".to_string()), (false, "no".to_string())],
                                true,
                                Some(Box::new(move |id: ComponentId, result: bool| {
                                    Some(UIEvent::FinishedUIDialog(
                                        id,
                                        Box::new(if result { Some(action) } else { None }),
                                    ))
                                })),
                                &self.context,
                            ));

                            self.overlay.insert(new.id(), new);
                        } else if matches!(action, Action::ReloadConfiguration) {
                            let res = Settings::new().and_then(|new_settings| {
                                let old_accounts = self
                                    .context
                                    .settings
                                    .accounts
                                    .keys()
                                    .collect::<std::collections::HashSet<&String>>();
                                let new_accounts = new_settings
                                    .accounts
                                    .keys()
                                    .collect::<std::collections::HashSet<&String>>();
                                if old_accounts != new_accounts {
                                    return Err("cannot reload account configuration changes; \
                                                restart meli instead."
                                        .into());
                                }
                                for (key, acc) in new_settings.accounts.iter() {
                                    if toml::Value::try_from(acc)
                                        != toml::Value::try_from(
                                            &self.context.settings.accounts[key],
                                        )
                                    {
                                        return Err("cannot reload account configuration \
                                                    changes; restart meli instead."
                                            .into());
                                    }
                                }
                                if toml::Value::try_from(&new_settings)
                                    == toml::Value::try_from(&self.context.settings)
                                {
                                    return Err("No changes detected.".into());
                                }
                                Ok(Box::new(new_settings))
                            });
                            match res {
                                Ok(new_settings) => {
                                    let old_settings =
                                        std::mem::replace(&mut self.context.settings, new_settings);
                                    self.context
                                        .replies
                                        .push_back(UIEvent::ConfigReload { old_settings });
                                    self.context.replies.push_back(UIEvent::Resize);
                                }
                                Err(err) => {
                                    self.context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(format!(
                                            "Could not load configuration: {}",
                                            err
                                        )),
                                    ));
                                }
                            }
                        } else {
                            self.exec_command(action);
                        }
                    }
                    Err(err) => {
                        self.context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!("Invalid command `{cmd}`: {err}")),
                        ));
                    }
                }
                return;
            }
            UIEvent::Fork(ForkType::Finished) => {
                /*
                 * Fork has finished in the past.
                 * We're back in the AlternateScreen, but the cursor is reset to Shown, so fix
                 * it.
                write!(self.screen.stdout(), "{}", cursor::Hide,).unwrap();
                self.flush();
                 */
                self.screen.switch_to_main_screen();
                self.screen.switch_to_alternate_screen(&self.context);
                self.context.restore_input();
                return;
            }
            UIEvent::Fork(ForkType::Generic {
                id,
                command: _,
                child,
            }) => {
                self.context.children.entry(id).or_default().push(child);
                return;
            }
            UIEvent::Fork(child) => {
                self.mode = UIMode::Fork;
                self.child = Some(child);
                return;
            }
            UIEvent::BackendEvent(
                account_hash,
                BackendEvent::Notice {
                    ref description,
                    ref content,
                    level,
                },
            ) => {
                log::log!(
                    level.into(),
                    "{}: {}{}{}",
                    self.context.accounts[&account_hash].name(),
                    description.as_str(),
                    if content.is_some() { ": " } else { "" },
                    content.as_ref().map(|s| s.as_str()).unwrap_or("")
                );
                self.rcv_event(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                    description.to_string(),
                )));
                return;
            }
            UIEvent::BackendEvent(account_hash, BackendEvent::AccountStateChange { message }) => {
                self.rcv_event(UIEvent::AccountStatusChange(account_hash, Some(message)));
                return;
            }
            UIEvent::BackendEvent(
                _,
                BackendEvent::Refresh(RefreshEvent {
                    account_hash,
                    mailbox_hash,
                    kind,
                }),
            ) => {
                self.refresh_event(account_hash, mailbox_hash, vec![kind]);
                return;
            }
            UIEvent::ChangeMode(m) => {
                self.context
                    .main_loop_handler
                    .sender
                    .send(ThreadEvent::UIEvent(UIEvent::ChangeMode(m)))
                    .unwrap();
            }
            UIEvent::Timer(id) if id == self.draw_rate_limit.id() => {
                self.draw_rate_limit.reset();
                self.redraw();
                return;
            }
            UIEvent::Input(ref key)
                if *key
                    == self
                        .context
                        .settings
                        .shortcuts
                        .general
                        .info_message_previous =>
            {
                self.message_box.expiration_start = Some(datetime::now());
                self.message_box.active = true;
                self.message_box.initialised = false;
                self.message_box.set_dirty(true);
                self.message_box.pos = self.message_box.pos.saturating_sub(1);
                return;
            }
            UIEvent::Input(ref key)
                if *key == self.context.settings.shortcuts.general.info_message_next =>
            {
                self.message_box.expiration_start = Some(datetime::now());
                self.message_box.active = true;
                self.message_box.initialised = false;
                self.message_box.set_dirty(true);
                self.message_box.pos = std::cmp::min(
                    self.message_box.len().saturating_sub(1),
                    self.message_box.pos + 1,
                );
                return;
            }
            UIEvent::StatusEvent(StatusEvent::DisplayMessage(ref mut msg)) => {
                if self
                    .message_box
                    .try_push(std::mem::take(msg), datetime::now())
                {
                    self.message_box.active = true;
                    self.message_box.initialised = false;
                    self.message_box.set_dirty(true);
                    self.message_box.expiration_start = None;
                    self.message_box.pos = self.message_box.len() - 1;
                    self.redraw();
                }
            }
            UIEvent::FinishedUIDialog(ref id, ref mut results) if self.overlay.contains_key(id) => {
                if let Some(ref mut action @ Some(_)) = results.downcast_mut::<Option<Action>>() {
                    self.exec_command(action.take().unwrap());

                    return;
                }
            }
            UIEvent::Callback(callback_fn) => {
                (callback_fn.0)(&mut self.context);
                return;
            }
            UIEvent::GlobalUIDialog { value, parent } => {
                self.context.realized.insert(value.id(), parent);
                self.overlay.insert(value.id(), value);
                self.process_realizations();
                return;
            }
            _ => {}
        }

        self.process_realizations();

        let Self {
            ref mut components,
            ref mut context,
            ref mut overlay,
            ..
        } = self;

        /* inform each component */
        for c in overlay.values_mut().chain(components.values_mut()) {
            if c.process_event(&mut event, context) {
                break;
            }
        }

        if !self.context.replies.is_empty() {
            let replies: smallvec::SmallVec<[UIEvent; 8]> =
                self.context.replies.drain(0..).collect();
            // Pass replies to self and call count on the map iterator to force evaluation
            replies.into_iter().map(|r| self.rcv_event(r)).count();
        }
    }

    fn process_realizations(&mut self) {
        while let Some((id, parent)) = self.context.realized.pop() {
            match parent {
                None => {
                    self.component_tree.insert(id, ComponentPath::new(id));
                }
                Some(parent) if self.component_tree.contains_key(&parent) => {
                    let mut v = self.component_tree[&parent].clone();
                    v.push_front(id);
                    if let Some(p) = v.root() {
                        assert_eq!(
                            v.resolve(&self.components[p] as &dyn Component)
                                .unwrap()
                                .id(),
                            id
                        );
                    }
                    self.component_tree.insert(id, v);
                }
                Some(parent) if !self.context.realized.contains_key(&parent) => {
                    log::debug!(
                        "BUG: component_realize new_id = {:?} parent = {:?} but component_tree \
                         does not include parent, skipping.",
                        id,
                        parent
                    );
                    self.component_tree.insert(id, ComponentPath::new(id));
                }
                Some(_) => {
                    let from_index = self.context.realized.len();
                    self.context.realized.insert(id, parent);
                    self.context.realized.move_index(from_index, 0);
                }
            }
        }

        while let Some(id) = self.context.unrealized.pop() {
            let mut to_delete = BTreeSet::new();
            for (desc, _) in self.component_tree.iter().filter(|(_, path)| {
                path.parent()
                    .map(|p| self.context.unrealized.contains(p) || *p == id)
                    .unwrap_or(false)
            }) {
                to_delete.insert(*desc);
            }
            self.context.unrealized.extend(to_delete.into_iter());
            self.component_tree.shift_remove(&id);
            self.components.shift_remove(&id);
            self.overlay.shift_remove(&id);
        }
    }

    pub fn try_wait_on_child(&mut self) -> Option<bool> {
        let should_return_flag = match self.child {
            Some(ForkType::Generic {
                ref id,
                ref command,
                ref mut child,
            }) => {
                let w = child.try_wait();
                match w {
                    Ok(Some(_)) => true,
                    Ok(None) => false,
                    Err(err) => {
                        log::error!(
                            "Failed to wait on child process {} {} ({}): {}",
                            id,
                            if let Some(v) = command.as_ref() {
                                v.as_ref()
                            } else {
                                ""
                            },
                            child.id(),
                            err
                        );
                        return None;
                    }
                }
            }
            Some(ForkType::Finished) => {
                /* Fork has already finished */
                self.child = None;
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

    /// Switch back to the terminal's main screen (The command line the user
    /// sees before opening the application)
    pub fn switch_to_main_screen(&mut self) {
        self.screen.switch_to_main_screen();
    }

    pub fn switch_to_alternate_screen(&mut self) {
        self.screen.switch_to_alternate_screen(&self.context);
    }

    fn flush(&mut self) {
        self.screen.flush();
    }

    pub fn check_accounts(&mut self) {
        let mut ctr = 0;
        for i in 0..self.context.accounts.len() {
            if self.context.is_online_idx(i).is_ok() {
                ctr += 1;
            }
        }
        if ctr != self.context.accounts.len() {
            self.timer.thread().unpark();
        }
        self.context.input_thread.check();
    }

    pub fn pulse(&mut self) {
        self.check_accounts();
        self.redraw();
    }
}
