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

use std::{collections::BTreeSet, env, os::unix::io::RawFd, sync::Arc, thread};

use crossbeam::channel::{unbounded, Receiver, Sender};
use indexmap::{IndexMap, IndexSet};
use melib::{
    backends::{AccountHash, BackendEvent, BackendEventConsumer, Backends, RefreshEvent},
    utils::datetime,
    UnixTimestamp,
};
use smallvec::SmallVec;

use super::*;
use crate::{
    jobs::JobExecutor,
    terminal::{get_events, screen::Screen},
};

struct InputHandler {
    pipe: (RawFd, RawFd),
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
        let pipe = self.pipe.0;
        let tx = self.state_tx.clone();
        thread::Builder::new()
            .name("input-thread".to_string())
            .spawn(move || {
                get_events(
                    |i| {
                        tx.send(ThreadEvent::Input(i)).unwrap();
                    },
                    &rx,
                    pipe,
                    working,
                )
            })
            .unwrap();
        self.control = control;
    }

    fn kill(&self) {
        let _ = nix::unistd::write(self.pipe.1, &[1]);
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

#[derive(Debug, Clone)]
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
    receiver: Receiver<ThreadEvent>,
    input_thread: InputHandler,
    pub children: Vec<std::process::Child>,

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
        let Context {
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
            accounts[account_pos].watch();

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
        let (sender, receiver) =
            crossbeam::channel::bounded(32 * ::std::mem::size_of::<ThreadEvent>());
        let job_executor = Arc::new(JobExecutor::new(sender.clone()));
        let input_thread = unbounded();
        let input_thread_pipe = nix::unistd::pipe()
            .map_err(|err| Box::new(err) as Box<dyn std::error::Error + Send + Sync + 'static>)
            .unwrap();
        let backends = Backends::new();
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
        Context {
            accounts,
            settings,
            dirty_areas: VecDeque::with_capacity(0),
            replies: VecDeque::with_capacity(0),
            realized: IndexMap::default(),
            unrealized: IndexSet::default(),
            temp_files: Vec::new(),
            children: vec![],

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
}

/// A State object to manage and own components and components of the UI.
/// `State` is responsible for managing the terminal and interfacing with
/// `melib`
pub struct State {
    screen: Box<Screen>,
    draw_rate_limit: RateLimit,
    child: Option<ForkType>,
    pub mode: UIMode,
    overlay: IndexMap<ComponentId, Box<dyn Component>>,
    components: IndexMap<ComponentId, Box<dyn Component>>,
    component_tree: IndexMap<ComponentId, ComponentPath>,
    pub context: Box<Context>,
    timer: thread::JoinHandle<()>,

    display_messages: SmallVec<[DisplayMessage; 8]>,
    display_messages_expiration_start: Option<UnixTimestamp>,
    display_messages_active: bool,
    display_messages_dirty: bool,
    display_messages_initialised: bool,
    display_messages_pos: usize,
    display_messages_area: Area,
}

#[derive(Debug)]
struct DisplayMessage {
    timestamp: UnixTimestamp,
    msg: String,
}

impl Drop for State {
    fn drop(&mut self) {
        // When done, restore the defaults to avoid messing with the terminal.
        self.screen.switch_to_main_screen();
        use nix::sys::wait::{waitpid, WaitPidFlag};
        for child in self.context.children.iter_mut() {
            if let Err(err) = waitpid(
                nix::unistd::Pid::from_raw(child.id() as i32),
                Some(WaitPidFlag::WNOHANG),
            ) {
                log::warn!("Failed to wait on subprocess {}: {}", child.id(), err);
            }
        }
        if let Some(ForkType::Embed(child_pid)) = self.child.take() {
            /* Try wait, we don't want to block */
            if let Err(e) = waitpid(child_pid, Some(WaitPidFlag::WNOHANG)) {
                log::warn!("Failed to wait on subprocess {}: {}", child_pid, e);
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
        /*
         * Create async channel to block the input-thread if we need to fork and stop
         * it from reading stdin, see get_events() for details
         */
        let input_thread = unbounded();
        let input_thread_pipe = nix::unistd::pipe()
            .map_err(|err| Box::new(err) as Box<dyn std::error::Error + Send + Sync + 'static>)?;
        let backends = Backends::new();
        let settings = Box::new(if let Some(settings) = settings {
            settings
        } else {
            Settings::new()?
        });
        /*
        let mut plugin_manager = PluginManager::new();
        for (_, p) in settings.plugins.clone() {
            if crate::plugins::PluginKind::Backend == p.kind() {
                debug!("registering {:?}", &p);
                crate::plugins::backend::PluginBackend::register(
                    plugin_manager.listener(),
                    p.clone(),
                    &mut backends,
                );
            }
            plugin_manager.register(p)?;
        }
        */

        let termsize = termion::terminal_size()?;
        let cols = termsize.0 as usize;
        let rows = termsize.1 as usize;

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
        let mut s = State {
            screen: Box::new(Screen {
                cols,
                rows,
                grid: CellBuffer::new(cols, rows, Cell::with_char(' ')),
                overlay_grid: CellBuffer::new(cols, rows, Cell::with_char(' ')),
                mouse: settings.terminal.use_mouse.is_true(),
                stdout: None,
                draw_horizontal_segment_fn: if settings.terminal.use_color() {
                    Screen::draw_horizontal_segment
                } else {
                    Screen::draw_horizontal_segment_no_color
                },
            }),
            child: None,
            mode: UIMode::Normal,
            components: IndexMap::default(),
            overlay: IndexMap::default(),
            component_tree: IndexMap::default(),
            timer,
            draw_rate_limit: RateLimit::new(1, 3, job_executor.clone()),
            display_messages: SmallVec::new(),
            display_messages_expiration_start: None,
            display_messages_pos: 0,
            display_messages_active: false,
            display_messages_dirty: false,
            display_messages_initialised: false,
            display_messages_area: ((0, 0), (0, 0)),
            context: Box::new(Context {
                accounts,
                settings,
                dirty_areas: VecDeque::with_capacity(5),
                replies: VecDeque::with_capacity(5),
                realized: IndexMap::default(),
                unrealized: IndexSet::default(),
                temp_files: Vec::new(),
                children: vec![],

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
            s.screen.grid.set_ascii_drawing(true);
            s.screen.overlay_grid.set_ascii_drawing(true);
        }

        s.screen.switch_to_alternate_screen(&s.context);
        for i in 0..s.context.accounts.len() {
            if !s.context.accounts[i].backend_capabilities.is_remote {
                s.context.accounts[i].watch();
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
    pub fn refresh_event(&mut self, event: RefreshEvent) {
        let account_hash = event.account_hash;
        let mailbox_hash = event.mailbox_hash;
        if self.context.accounts[&account_hash]
            .mailbox_entries
            .contains_key(&mailbox_hash)
        {
            if self.context.accounts[&account_hash]
                .load(mailbox_hash)
                .is_err()
            {
                self.context.replies.push_back(UIEvent::from(event));
                return;
            }
            let Context {
                ref mut accounts, ..
            } = &mut *self.context;

            if let Some(notification) = accounts[&account_hash].reload(event, mailbox_hash) {
                if let UIEvent::Notification(_, _, _) = notification {
                    self.rcv_event(UIEvent::MailboxUpdate((account_hash, mailbox_hash)));
                }
                self.rcv_event(notification);
            }
        } else if let melib::backends::RefreshEventKind::Failure(err) = event.kind {
            log::warn!("Backend could not refresh: {}", err);
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

    /// On `SIGWNICH` the `State` redraws itself according to the new terminal
    /// size.
    pub fn update_size(&mut self) {
        self.screen.update_size();
        self.rcv_event(UIEvent::Resize);
        self.display_messages_dirty = true;
        self.display_messages_initialised = false;
        self.display_messages_area = ((0, 0), (0, 0));

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
        if self.display_messages_active {
            let now = datetime::now();
            if self
                .display_messages_expiration_start
                .map(|t| t + 5 < now)
                .unwrap_or(false)
            {
                self.display_messages_active = false;
                self.display_messages_dirty = true;
                self.display_messages_initialised = false;
                self.display_messages_expiration_start = None;
                areas.push((
                    (0, 0),
                    (
                        self.screen.cols.saturating_sub(1),
                        self.screen.rows.saturating_sub(1),
                    ),
                ));
            }
        }

        /* Sort by x_start, ie upper_left corner's x coordinate */
        areas.sort_by(|a, b| (a.0).0.partial_cmp(&(b.0).0).unwrap());

        if self.display_messages_active {
            /* Check if any dirty area intersects with the area occupied by floating
             * notification box */
            let (displ_top, displ_bot) = self.display_messages_area;
            for &((top_x, top_y), (bottom_x, bottom_y)) in &areas {
                self.display_messages_dirty |= !(bottom_y < displ_top.1
                    || displ_bot.1 < top_y
                    || bottom_x < displ_top.0
                    || displ_bot.0 < top_x);
            }
        }
        /* draw each dirty area */
        let rows = self.screen.rows;
        for y in 0..rows {
            let mut segment = None;
            for ((x_start, y_start), (x_end, y_end)) in &areas {
                if y < *y_start || y > *y_end {
                    continue;
                }
                if let Some((x_start, x_end)) = segment.take() {
                    (self.screen.draw_horizontal_segment_fn)(
                        &mut self.screen.grid,
                        self.screen.stdout.as_mut().unwrap(),
                        x_start,
                        x_end,
                        y,
                    );
                }
                match segment {
                    ref mut s @ None => {
                        *s = Some((*x_start, *x_end));
                    }
                    ref mut s @ Some(_) if s.unwrap().1 < *x_start => {
                        (self.screen.draw_horizontal_segment_fn)(
                            &mut self.screen.grid,
                            self.screen.stdout.as_mut().unwrap(),
                            s.unwrap().0,
                            s.unwrap().1,
                            y,
                        );
                        *s = Some((*x_start, *x_end));
                    }
                    ref mut s @ Some(_) if s.unwrap().1 < *x_end => {
                        (self.screen.draw_horizontal_segment_fn)(
                            &mut self.screen.grid,
                            self.screen.stdout.as_mut().unwrap(),
                            s.unwrap().0,
                            s.unwrap().1,
                            y,
                        );
                        *s = Some((s.unwrap().1, *x_end));
                    }
                    Some((_, ref mut x)) => {
                        *x = *x_end;
                    }
                }
            }
            if let Some((x_start, x_end)) = segment {
                (self.screen.draw_horizontal_segment_fn)(
                    &mut self.screen.grid,
                    self.screen.stdout.as_mut().unwrap(),
                    x_start,
                    x_end,
                    y,
                );
            }
        }

        if self.display_messages_dirty && self.display_messages_active {
            if let Some(DisplayMessage {
                ref timestamp,
                ref msg,
                ..
            }) = self.display_messages.get(self.display_messages_pos)
            {
                if !self.display_messages_initialised {
                    {
                        /* Clear area previously occupied by floating notification box */
                        let displ_area = self.display_messages_area;
                        for y in get_y(upper_left!(displ_area))..=get_y(bottom_right!(displ_area)) {
                            (self.screen.draw_horizontal_segment_fn)(
                                &mut self.screen.grid,
                                self.screen.stdout.as_mut().unwrap(),
                                get_x(upper_left!(displ_area)),
                                get_x(bottom_right!(displ_area)),
                                y,
                            );
                        }
                    }
                    let noto_colors = crate::conf::value(&self.context, "status.notification");
                    use crate::melib::text_processing::{Reflow, TextProcessing};

                    let msg_lines = msg.split_lines_reflow(Reflow::All, Some(self.screen.cols / 3));
                    let width = msg_lines
                        .iter()
                        .map(|line| line.grapheme_len() + 4)
                        .max()
                        .unwrap_or(0);

                    let displ_area = place_in_area(
                        (
                            (0, 0),
                            (
                                self.screen.cols.saturating_sub(1),
                                self.screen.rows.saturating_sub(1),
                            ),
                        ),
                        (width, std::cmp::min(self.screen.rows, msg_lines.len() + 4)),
                        false,
                        false,
                    );
                    let box_displ_area = create_box(&mut self.screen.overlay_grid, displ_area);
                    for row in self.screen.overlay_grid.bounds_iter(box_displ_area) {
                        for c in row {
                            self.screen.overlay_grid[c]
                                .set_ch(' ')
                                .set_fg(noto_colors.fg)
                                .set_bg(noto_colors.bg)
                                .set_attrs(noto_colors.attrs);
                        }
                    }
                    let ((x, mut y), box_displ_area_bottom_right) = box_displ_area;
                    for line in msg_lines
                        .into_iter()
                        .chain(Some(String::new()))
                        .chain(Some(datetime::timestamp_to_string(*timestamp, None, false)))
                    {
                        write_string_to_grid(
                            &line,
                            &mut self.screen.overlay_grid,
                            noto_colors.fg,
                            noto_colors.bg,
                            noto_colors.attrs,
                            ((x, y), box_displ_area_bottom_right),
                            Some(x),
                        );
                        y += 1;
                    }

                    if self.display_messages.len() > 1 {
                        write_string_to_grid(
                            &if self.display_messages_pos == 0 {
                                format!(
                                    "Next: {}",
                                    self.context.settings.shortcuts.general.info_message_next
                                )
                            } else if self.display_messages_pos + 1 == self.display_messages.len() {
                                format!(
                                    "Prev: {}",
                                    self.context
                                        .settings
                                        .shortcuts
                                        .general
                                        .info_message_previous
                                )
                            } else {
                                format!(
                                    "Prev: {} Next: {}",
                                    self.context
                                        .settings
                                        .shortcuts
                                        .general
                                        .info_message_previous,
                                    self.context.settings.shortcuts.general.info_message_next
                                )
                            },
                            &mut self.screen.overlay_grid,
                            noto_colors.fg,
                            noto_colors.bg,
                            noto_colors.attrs,
                            ((x, y), box_displ_area_bottom_right),
                            Some(x),
                        );
                    }
                    self.display_messages_area = displ_area;
                }
                for y in get_y(upper_left!(self.display_messages_area))
                    ..=get_y(bottom_right!(self.display_messages_area))
                {
                    (self.screen.draw_horizontal_segment_fn)(
                        &mut self.screen.overlay_grid,
                        self.screen.stdout.as_mut().unwrap(),
                        get_x(upper_left!(self.display_messages_area)),
                        get_x(bottom_right!(self.display_messages_area)),
                        y,
                    );
                }
            }
            self.display_messages_dirty = false;
        } else if self.display_messages_dirty {
            /* Clear area previously occupied by floating notification box */
            let displ_area = self.display_messages_area;
            for y in get_y(upper_left!(displ_area))..=get_y(bottom_right!(displ_area)) {
                (self.screen.draw_horizontal_segment_fn)(
                    &mut self.screen.grid,
                    self.screen.stdout.as_mut().unwrap(),
                    get_x(upper_left!(displ_area)),
                    get_x(bottom_right!(displ_area)),
                    y,
                );
            }
            self.display_messages_dirty = false;
        }
        if !self.overlay.is_empty() {
            let area = center_area(
                (
                    (0, 0),
                    (
                        self.screen.cols.saturating_sub(1),
                        self.screen.rows.saturating_sub(1),
                    ),
                ),
                (
                    if self.screen.cols / 3 > 30 {
                        self.screen.cols / 3
                    } else {
                        self.screen.cols
                    },
                    if self.screen.rows / 5 > 10 {
                        self.screen.rows / 5
                    } else {
                        self.screen.rows
                    },
                ),
            );
            copy_area(&mut self.screen.overlay_grid, &self.screen.grid, area, area);
            self.overlay.get_index_mut(0).unwrap().1.draw(
                &mut self.screen.overlay_grid,
                area,
                &mut self.context,
            );
            for y in get_y(upper_left!(area))..=get_y(bottom_right!(area)) {
                (self.screen.draw_horizontal_segment_fn)(
                    &mut self.screen.overlay_grid,
                    self.screen.stdout.as_mut().unwrap(),
                    get_x(upper_left!(area)),
                    get_x(bottom_right!(area)),
                    y,
                );
            }
        }
        self.flush();
    }

    /// Draw the entire screen from scratch.
    pub fn render(&mut self) {
        self.screen.update_size();
        let cols = self.screen.cols;
        let rows = self.screen.rows;
        self.context
            .dirty_areas
            .push_back(((0, 0), (cols - 1, rows - 1)));

        self.redraw();
    }

    pub fn draw_component(&mut self, idx: usize) {
        let component = &mut self.components[idx];
        let upper_left = (0, 0);
        let bottom_right = (self.screen.cols - 1, self.screen.rows - 1);

        if component.is_dirty() {
            component.draw(
                &mut self.screen.grid,
                (upper_left, bottom_right),
                &mut self.context,
            );
        }
    }

    pub fn can_quit_cleanly(&mut self) -> bool {
        let State {
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
                env::set_var(key.as_str(), val.as_str());
            }
            PrintEnv(key) => {
                self.context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                        env::var(key.as_str()).unwrap_or_else(|e| e.to_string()),
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
                    self.context.replies.push_back(UIEvent::Notification(
                        None,
                        format!("Account {} was not found.", account_name),
                        Some(NotificationType::Error(ErrorKind::None)),
                    ));
                    return;
                };
                if *self.context.accounts[account_index]
                    .settings
                    .conf
                    .search_backend()
                    != crate::conf::SearchBackend::Sqlite3
                {
                    self.context.replies.push_back(UIEvent::Notification(
                        None,
                        format!(
                            "Account {} doesn't have an sqlite3 search backend.",
                            account_name
                        ),
                        Some(NotificationType::Error(ErrorKind::None)),
                    ));
                    return;
                }
                match crate::sqlite3::index(&mut self.context, account_index) {
                    Ok(job) => {
                        let handle = self
                            .context
                            .main_loop_handler
                            .job_executor
                            .spawn_blocking(job);
                        self.context.accounts[account_index].active_jobs.insert(
                            handle.job_id,
                            crate::conf::accounts::JobRequest::Generic {
                                name: "Message index rebuild".into(),
                                handle,
                                on_finish: None,
                                log_level: LogLevel::INFO,
                            },
                        );
                        self.context.replies.push_back(UIEvent::Notification(
                            None,
                            "Message index rebuild started.".to_string(),
                            Some(NotificationType::Info),
                        ));
                    }
                    Err(err) => {
                        self.context.replies.push_back(UIEvent::Notification(
                            Some("Message index rebuild failed".to_string()),
                            err.to_string(),
                            Some(NotificationType::Error(err.kind)),
                        ));
                    }
                }
            }
            #[cfg(not(feature = "sqlite3"))]
            AccountAction(ref account_name, ReIndex) => {
                self.context.replies.push_back(UIEvent::Notification(
                    None,
                    "Message index rebuild failed: meli is not built with sqlite3 support."
                        .to_string(),
                    Some(NotificationType::Error(ErrorKind::None)),
                ));
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
                    self.context.replies.push_back(UIEvent::Notification(
                        None,
                        format!("Account {} was not found.", account_name),
                        Some(NotificationType::Error(ErrorKind::None)),
                    ));
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
                self.screen.mouse = !self.screen.mouse;
                self.screen.set_mouse(self.screen.mouse);
                self.rcv_event(UIEvent::StatusEvent(StatusEvent::SetMouse(
                    self.screen.mouse,
                )));
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
            v => {
                self.rcv_event(UIEvent::Action(v));
            }
        }
    }

    /// The application's main loop sends `UIEvents` to state via this method.
    pub fn rcv_event(&mut self, mut event: UIEvent) {
        if let UIEvent::Input(_) = event {
            if self.display_messages_expiration_start.is_none() {
                self.display_messages_expiration_start = Some(datetime::now());
            }
        }

        match event {
            // Command type is handled only by State.
            UIEvent::Command(cmd) => {
                if let Ok(action) = parse_command(cmd.as_bytes()) {
                    if action.needs_confirmation() {
                        let new = Box::new(UIConfirmationDialog::new(
                            "You sure?",
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
                    } else if let Action::ReloadConfiguration = action {
                        match Settings::new().and_then(|new_settings| {
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
                                    != toml::Value::try_from(&self.context.settings.accounts[key])
                                {
                                    return Err("cannot reload account configuration changes; \
                                                restart meli instead."
                                        .into());
                                }
                            }
                            if toml::Value::try_from(&new_settings)
                                == toml::Value::try_from(&self.context.settings)
                            {
                                return Err("No changes detected.".into());
                            }
                            Ok(Box::new(new_settings))
                        }) {
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
                } else {
                    self.context.replies.push_back(UIEvent::StatusEvent(
                        StatusEvent::DisplayMessage("invalid command".to_string()),
                    ));
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
            UIEvent::Fork(ForkType::Generic(child)) => {
                self.context.children.push(child);
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
            UIEvent::BackendEvent(_, BackendEvent::Refresh(refresh_event)) => {
                self.refresh_event(refresh_event);
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
                self.display_messages_expiration_start = Some(datetime::now());
                self.display_messages_active = true;
                self.display_messages_initialised = false;
                self.display_messages_dirty = true;
                self.display_messages_pos = self.display_messages_pos.saturating_sub(1);
                return;
            }
            UIEvent::Input(ref key)
                if *key == self.context.settings.shortcuts.general.info_message_next =>
            {
                self.display_messages_expiration_start = Some(datetime::now());
                self.display_messages_active = true;
                self.display_messages_initialised = false;
                self.display_messages_dirty = true;
                self.display_messages_pos = std::cmp::min(
                    self.display_messages.len().saturating_sub(1),
                    self.display_messages_pos + 1,
                );
                return;
            }
            UIEvent::StatusEvent(StatusEvent::DisplayMessage(ref msg)) => {
                self.display_messages.push(DisplayMessage {
                    timestamp: datetime::now(),
                    msg: msg.clone(),
                });
                self.display_messages_active = true;
                self.display_messages_initialised = false;
                self.display_messages_dirty = true;
                self.display_messages_expiration_start = None;
                self.display_messages_pos = self.display_messages.len() - 1;
                self.redraw();
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
            UIEvent::GlobalUIDialog(dialog) => {
                self.overlay.insert(dialog.id(), dialog);
                return;
            }
            _ => {}
        }
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
            self.component_tree.remove(&id);
            self.components.remove(&id);
        }

        if !self.context.replies.is_empty() {
            let replies: smallvec::SmallVec<[UIEvent; 8]> =
                self.context.replies.drain(0..).collect();
            // Pass replies to self and call count on the map iterator to force evaluation
            replies.into_iter().map(|r| self.rcv_event(r)).count();
        }
    }

    pub fn try_wait_on_child(&mut self) -> Option<bool> {
        let should_return_flag = match self.child {
            Some(ForkType::NewDraft(_, ref mut c)) => {
                let w = c.try_wait();
                match w {
                    Ok(Some(_)) => true,
                    Ok(None) => false,
                    Err(err) => {
                        log::error!("Failed to wait on editor process: {err}");
                        return None;
                    }
                }
            }
            Some(ForkType::Generic(ref mut c)) => {
                let w = c.try_wait();
                match w {
                    Ok(Some(_)) => true,
                    Ok(None) => false,
                    Err(err) => {
                        log::error!("Failed to wait on child process: {err}");
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
}
