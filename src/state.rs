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

/*! The application's state.

The UI crate has an Box<dyn Component>-Component-System design. The System part, is also the application's state, so they're both merged in the `State` struct.

`State` owns all the Components of the UI. In the application's main event loop, input is handed to the state in the form of `UIEvent` objects which traverse the component graph. Components decide to handle each input or not.

Input is received in the main loop from threads which listen on the stdin for user input, observe folders for file changes etc. The relevant struct is `ThreadEvent`.
*/

use super::*;
use crate::plugins::PluginManager;
use melib::backends::{AccountHash, MailboxHash, NotifyFn};

use crossbeam::channel::{unbounded, Receiver, Sender};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::env;
use std::io::Write;
use std::os::unix::io::RawFd;
use std::thread;
use termion::raw::IntoRawMode;
use termion::screen::AlternateScreen;
use termion::{clear, cursor};

pub type StateStdout = termion::screen::AlternateScreen<termion::raw::RawTerminal<std::io::Stdout>>;

struct InputHandler {
    pipe: (RawFd, RawFd),
    rx: Receiver<InputCommand>,
    tx: Sender<InputCommand>,
}

impl InputHandler {
    fn restore(&self, tx: Sender<ThreadEvent>) {
        /* Clear channel without blocking. switch_to_main_screen() issues a kill when
         * returning from a fork and there's no input thread, so the newly created thread will
         * receive it and die. */
        //let _ = self.rx.try_iter().count();
        let rx = self.rx.clone();
        let pipe = self.pipe.0;
        thread::Builder::new()
            .name("input-thread".to_string())
            .spawn(move || {
                get_events(
                    |i| {
                        tx.send(ThreadEvent::Input(i)).unwrap();
                    },
                    &rx,
                    pipe,
                )
            })
            .unwrap();
    }

    fn kill(&self) {
        let _ = nix::unistd::write(self.pipe.1, &[1]);
        self.tx.send(InputCommand::Kill).unwrap();
    }
}

/// A context container for loaded settings, accounts, UI changes, etc.
pub struct Context {
    pub accounts: Vec<Account>,
    pub account_hashes: HashMap<AccountHash, usize>,
    pub settings: Settings,

    pub runtime_settings: Settings,
    /// Areas of the screen that must be redrawn in the next render
    pub dirty_areas: VecDeque<Area>,

    /// Events queue that components send back to the state
    pub replies: VecDeque<UIEvent>,
    sender: Sender<ThreadEvent>,
    receiver: Receiver<ThreadEvent>,
    input: InputHandler,
    work_controller: WorkController,
    pub children: Vec<std::process::Child>,
    pub plugin_manager: PluginManager,

    pub temp_files: Vec<File>,
}

impl Context {
    pub fn replies(&mut self) -> smallvec::SmallVec<[UIEvent; 8]> {
        self.replies.drain(0..).collect()
    }

    pub fn input_kill(&self) {
        self.input.kill();
    }

    pub fn restore_input(&self) {
        self.input.restore(self.sender.clone());
    }

    pub fn is_online(&mut self, account_pos: usize) -> Result<()> {
        let Context {
            ref mut accounts,
            ref mut replies,
            ..
        } = self;
        let was_online = accounts[account_pos].is_online;
        let ret = accounts[account_pos].is_online();
        if ret.is_ok() {
            if !was_online {
                for mailbox_node in accounts[account_pos].list_mailboxes() {
                    debug!(
                        "hash & mailbox: {:?} {}",
                        mailbox_node.hash,
                        accounts[account_pos][&mailbox_node.hash].name()
                    );
                }
                /* Account::watch() needs
                 * - work_controller to pass `work_context` to the watcher threads and then add them
                 *   to the controller's static thread list,
                 * - sender to pass a RefreshEventConsumer closure to watcher threads for them to
                 *   inform the main binary that refresh events arrived
                 * - replies to report any failures to the user
                 */
                accounts[account_pos].watch();

                replies.push_back(UIEvent::AccountStatusChange(account_pos));
            }
        }
        if ret.is_ok() != was_online {
            replies.push_back(UIEvent::AccountStatusChange(account_pos));
        }
        ret
    }

    pub fn work_controller(&self) -> &WorkController {
        &self.work_controller
    }
}

/// A State object to manage and own components and components of the UI. `State` is responsible for
/// managing the terminal and interfacing with `melib`
pub struct State {
    cols: usize,
    rows: usize,

    grid: CellBuffer,
    overlay_grid: CellBuffer,
    draw_rate_limit: RateLimit,
    stdout: Option<StateStdout>,
    child: Option<ForkType>,
    draw_horizontal_segment_fn: fn(&mut CellBuffer, &mut StateStdout, usize, usize, usize) -> (),
    pub mode: UIMode,
    overlay: Vec<Box<dyn Component>>,
    components: Vec<Box<dyn Component>>,
    pub context: Context,
    timer: thread::JoinHandle<()>,

    display_messages: SmallVec<[DisplayMessage; 8]>,
    display_messages_expiration_start: Option<UnixTimestamp>,
    display_messages_active: bool,
    display_messages_pos: usize,
}

#[derive(Debug)]
struct DisplayMessage {
    timestamp: UnixTimestamp,
    msg: String,
}

impl Drop for State {
    fn drop(&mut self) {
        // When done, restore the defaults to avoid messing with the terminal.
        self.switch_to_main_screen();
        use nix::sys::wait::{waitpid, WaitPidFlag};
        for child in self.context.children.iter_mut() {
            if let Err(err) = waitpid(
                nix::unistd::Pid::from_raw(child.id() as i32),
                Some(WaitPidFlag::WNOHANG),
            ) {
                debug!("Failed to wait on subprocess {}: {}", child.id(), err);
            }
        }
        if let Some(ForkType::Embed(child_pid)) = self.child.take() {
            /* Try wait, we don't want to block */
            if let Err(e) = waitpid(child_pid, Some(WaitPidFlag::WNOHANG)) {
                debug!("Failed to wait on subprocess {}: {}", child_pid, e);
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
         * Create async channel to block the input-thread if we need to fork and stop it from reading
         * stdin, see get_events() for details
         * */
        let input_thread = unbounded();
        let input_thread_pipe = nix::unistd::pipe()
            .map_err(|err| Box::new(err) as Box<dyn std::error::Error + Send + Sync + 'static>)?;
        let mut backends = Backends::new();
        let settings = if let Some(settings) = settings {
            settings
        } else {
            Settings::new()?
        };
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

        let termsize = termion::terminal_size()?;
        let cols = termsize.0 as usize;
        let rows = termsize.1 as usize;

        let mut account_hashes = HashMap::with_capacity_and_hasher(1, Default::default());
        let work_controller = WorkController::new(sender.clone());
        let accounts: Vec<Account> = {
            let mut file_accs = settings
                .accounts
                .iter()
                .collect::<Vec<(&String, &AccountConf)>>();
            file_accs.sort_by(|a, b| a.0.cmp(&b.0));

            file_accs
                .into_iter()
                .enumerate()
                .map(|(index, (n, a_s))| {
                    let sender = sender.clone();
                    let account_hash = {
                        use std::collections::hash_map::DefaultHasher;
                        use std::hash::Hasher;
                        let mut hasher = DefaultHasher::new();
                        hasher.write(n.as_bytes());
                        hasher.finish()
                    };
                    account_hashes.insert(account_hash, index);
                    Account::new(
                        index,
                        account_hash,
                        n.to_string(),
                        a_s.clone(),
                        &backends,
                        work_controller.get_context(),
                        sender.clone(),
                        NotifyFn::new(Box::new(move |f: MailboxHash| {
                            sender
                                .send(ThreadEvent::UIEvent(UIEvent::WorkerProgress(
                                    account_hash,
                                    f,
                                )))
                                .unwrap();
                        })),
                    )
                })
                .collect::<Result<Vec<Account>>>()?
        };

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

        let mut s = State {
            cols,
            rows,
            grid: CellBuffer::new(cols, rows, Cell::with_char(' ')),
            overlay_grid: CellBuffer::new(cols, rows, Cell::with_char(' ')),
            stdout: None,
            child: None,
            mode: UIMode::Normal,
            components: Vec::with_capacity(8),
            overlay: Vec::new(),
            timer,
            draw_rate_limit: RateLimit::new(1, 3),
            draw_horizontal_segment_fn: if settings.terminal.use_color() {
                State::draw_horizontal_segment
            } else {
                State::draw_horizontal_segment_no_color
            },
            display_messages: SmallVec::new(),
            display_messages_expiration_start: None,
            display_messages_pos: 0,
            display_messages_active: false,

            context: Context {
                accounts,
                account_hashes,
                settings: settings.clone(),
                runtime_settings: settings,
                dirty_areas: VecDeque::with_capacity(5),
                replies: VecDeque::with_capacity(5),
                temp_files: Vec::new(),
                work_controller,
                children: vec![],
                plugin_manager,

                sender,
                receiver,
                input: InputHandler {
                    pipe: input_thread_pipe,
                    rx: input_thread.1,
                    tx: input_thread.0,
                },
            },
        };
        s.draw_rate_limit
            .timer
            .set_value(std::time::Duration::from_millis(3));
        if s.context.settings.terminal.ascii_drawing {
            s.grid.set_ascii_drawing(true);
            s.overlay_grid.set_ascii_drawing(true);
        }

        s.switch_to_alternate_screen();
        debug!("inserting mailbox hashes:");
        for i in 0..s.context.accounts.len() {
            if s.context.is_online(i).is_ok() && s.context.accounts[i].is_empty() {
                return Err(MeliError::new(format!(
                    "Account {} has no mailboxes configured.",
                    s.context.accounts[i].name()
                )));
            }
        }
        s.context.restore_input();
        Ok(s)
    }

    /*
     * When we receive a mailbox hash from a watcher thread,
     * we match the hash to the index of the mailbox, request a reload
     * and startup a thread to remind us to poll it every now and then till it's finished.
     */
    pub fn refresh_event(&mut self, event: RefreshEvent) {
        let account_hash = event.account_hash();
        let mailbox_hash = event.mailbox_hash();
        if let Some(&idxa) = self.context.account_hashes.get(&account_hash) {
            if self.context.accounts[idxa]
                .mailbox_entries
                .contains_key(&mailbox_hash)
            {
                if self.context.accounts[idxa].load(mailbox_hash).is_err() {
                    self.context.replies.push_back(UIEvent::from(event));
                    return;
                }
                let Context {
                    ref mut accounts, ..
                } = &mut self.context;

                if let Some(notification) = accounts[idxa].reload(event, mailbox_hash) {
                    if let UIEvent::Notification(_, _, _) = notification {
                        self.rcv_event(UIEvent::MailboxUpdate((idxa, mailbox_hash)));
                    }
                    self.rcv_event(notification);
                }
            } else {
                if let melib::backends::RefreshEventKind::Failure(err) = event.kind() {
                    debug!(err);
                }
            }
        }
    }

    pub fn new_thread(&mut self, id: thread::ThreadId, name: String) {
        self.context
            .work_controller
            .static_threads
            .lock()
            .unwrap()
            .insert(id, name.into());
    }

    /// Switch back to the terminal's main screen (The command line the user sees before opening
    /// the application)
    pub fn switch_to_main_screen(&mut self) {
        write!(
            self.stdout(),
            "{}{}{}{}",
            termion::screen::ToMainScreen,
            cursor::Show,
            RestoreWindowTitleIconFromStack,
            BracketModeEnd,
        )
        .unwrap();
        self.flush();
        self.stdout = None;
    }

    pub fn switch_to_alternate_screen(&mut self) {
        let s = std::io::stdout();

        let mut stdout = AlternateScreen::from(s.into_raw_mode().unwrap());

        write!(
            &mut stdout,
            "{save_title_to_stack}{}{}{}{window_title}{}{}",
            termion::screen::ToAlternateScreen,
            cursor::Hide,
            clear::All,
            cursor::Goto(1, 1),
            BracketModeStart,
            save_title_to_stack = SaveWindowTitleIconToStack,
            window_title = if let Some(ref title) = self.context.settings.terminal.window_title {
                format!("\x1b]2;{}\x07", title)
            } else {
                String::new()
            },
        )
        .unwrap();

        self.stdout = Some(stdout);
        self.flush();
    }

    pub fn receiver(&self) -> Receiver<ThreadEvent> {
        self.context.receiver.clone()
    }

    pub fn sender(&self) -> Sender<ThreadEvent> {
        self.context.sender.clone()
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
            debug!(
                "Size updated, from ({}, {}) -> ({:?}, {:?})",
                self.cols, self.rows, termcols, termrows
            );
        }
        self.cols = termcols.unwrap_or(72) as usize;
        self.rows = termrows.unwrap_or(120) as usize;
        self.grid.resize(self.cols, self.rows, Cell::with_char(' '));
        self.overlay_grid
            .resize(self.cols, self.rows, Cell::with_char(' '));

        self.rcv_event(UIEvent::Resize);

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
            let now = melib::datetime::now();
            if self
                .display_messages_expiration_start
                .map(|t| t + 5 < now)
                .unwrap_or(false)
            {
                self.display_messages_active = false;
                self.display_messages_expiration_start = None;
                areas.push((
                    (0, 0),
                    (self.cols.saturating_sub(1), self.rows.saturating_sub(1)),
                ));
            }
        }

        /* Sort by x_start, ie upper_left corner's x coordinate */
        areas.sort_by(|a, b| (a.0).0.partial_cmp(&(b.0).0).unwrap());
        /* draw each dirty area */
        let rows = self.rows;
        for y in 0..rows {
            let mut segment = None;
            for ((x_start, y_start), (x_end, y_end)) in &areas {
                if y < *y_start || y > *y_end {
                    continue;
                }
                if let Some((x_start, x_end)) = segment.take() {
                    (self.draw_horizontal_segment_fn)(
                        &mut self.grid,
                        self.stdout.as_mut().unwrap(),
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
                        (self.draw_horizontal_segment_fn)(
                            &mut self.grid,
                            self.stdout.as_mut().unwrap(),
                            s.unwrap().0,
                            s.unwrap().1,
                            y,
                        );
                        *s = Some((*x_start, *x_end));
                    }
                    ref mut s @ Some(_) if s.unwrap().1 < *x_end => {
                        (self.draw_horizontal_segment_fn)(
                            &mut self.grid,
                            self.stdout.as_mut().unwrap(),
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
                (self.draw_horizontal_segment_fn)(
                    &mut self.grid,
                    self.stdout.as_mut().unwrap(),
                    x_start,
                    x_end,
                    y,
                );
            }
        }

        if self.display_messages_active {
            if let Some(DisplayMessage {
                ref timestamp,
                ref msg,
                ..
            }) = self.display_messages.get(self.display_messages_pos)
            {
                let noto_colors = crate::conf::value(&self.context, "status.notification");
                use crate::melib::text_processing::{Reflow, TextProcessing};

                let msg_lines = msg.split_lines_reflow(Reflow::All, Some(self.cols / 3));
                let width = msg_lines
                    .iter()
                    .map(|line| line.grapheme_len() + 4)
                    .max()
                    .unwrap_or(0);

                let displ_area = place_in_area(
                    (
                        (0, 0),
                        (self.cols.saturating_sub(1), self.rows.saturating_sub(1)),
                    ),
                    (width, std::cmp::min(self.rows, msg_lines.len() + 4)),
                    false,
                    false,
                );
                for row in self.overlay_grid.bounds_iter(displ_area) {
                    for c in row {
                        self.overlay_grid[c]
                            .set_ch(' ')
                            .set_fg(noto_colors.fg)
                            .set_bg(noto_colors.bg)
                            .set_attrs(noto_colors.attrs);
                    }
                }
                let ((x, mut y), box_displ_area_bottom_right) =
                    create_box(&mut self.overlay_grid, displ_area);
                for line in msg_lines
                    .into_iter()
                    .chain(Some(String::new()))
                    .chain(Some(melib::datetime::timestamp_to_string(*timestamp, None)))
                {
                    write_string_to_grid(
                        &line,
                        &mut self.overlay_grid,
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
                        if self.display_messages_pos == 0 {
                            "Next: >"
                        } else if self.display_messages_pos + 1 == self.display_messages.len() {
                            "Prev: <"
                        } else {
                            "Prev: <, Next: >"
                        },
                        &mut self.overlay_grid,
                        noto_colors.fg,
                        noto_colors.bg,
                        noto_colors.attrs,
                        ((x, y), box_displ_area_bottom_right),
                        Some(x),
                    );
                }
                for y in get_y(upper_left!(displ_area))..=get_y(bottom_right!(displ_area)) {
                    (self.draw_horizontal_segment_fn)(
                        &mut self.overlay_grid,
                        self.stdout.as_mut().unwrap(),
                        get_x(upper_left!(displ_area)),
                        get_x(bottom_right!(displ_area)),
                        y,
                    );
                }
            }
        }
        if !self.overlay.is_empty() {
            let area = center_area(
                (
                    (0, 0),
                    (self.cols.saturating_sub(1), self.rows.saturating_sub(1)),
                ),
                (
                    if self.cols / 3 > 30 {
                        self.cols / 3
                    } else {
                        self.cols
                    },
                    if self.rows / 5 > 10 {
                        self.rows / 5
                    } else {
                        self.rows
                    },
                ),
            );
            copy_area(&mut self.overlay_grid, &mut self.grid, area, area);
            self.overlay
                .get_mut(0)
                .unwrap()
                .draw(&mut self.overlay_grid, area, &mut self.context);
            for y in get_y(upper_left!(area))..=get_y(bottom_right!(area)) {
                (self.draw_horizontal_segment_fn)(
                    &mut self.overlay_grid,
                    self.stdout.as_mut().unwrap(),
                    get_x(upper_left!(area)),
                    get_x(bottom_right!(area)),
                    y,
                );
            }
        }
        self.flush();
    }

    /// Draw only a specific `area` on the screen.
    fn draw_horizontal_segment(
        grid: &mut CellBuffer,
        stdout: &mut StateStdout,
        x_start: usize,
        x_end: usize,
        y: usize,
    ) {
        write!(
            stdout,
            "{}",
            cursor::Goto(x_start as u16 + 1, (y + 1) as u16)
        )
        .unwrap();
        let mut current_fg = Color::Default;
        let mut current_bg = Color::Default;
        let mut current_attrs = Attr::DEFAULT;
        write!(stdout, "\x1B[m").unwrap();
        for x in x_start..=x_end {
            let c = &grid[(x, y)];
            if c.attrs() != current_attrs {
                c.attrs().write(current_attrs, stdout).unwrap();
                current_attrs = c.attrs();
            }
            if c.bg() != current_bg {
                c.bg().write_bg(stdout).unwrap();
                current_bg = c.bg();
            }
            if c.fg() != current_fg {
                c.fg().write_fg(stdout).unwrap();
                current_fg = c.fg();
            }
            if !c.empty() {
                write!(stdout, "{}", c.ch()).unwrap();
            }
        }
    }

    fn draw_horizontal_segment_no_color(
        grid: &mut CellBuffer,
        stdout: &mut StateStdout,
        x_start: usize,
        x_end: usize,
        y: usize,
    ) {
        write!(
            stdout,
            "{}",
            cursor::Goto(x_start as u16 + 1, (y + 1) as u16)
        )
        .unwrap();
        let mut current_attrs = Attr::DEFAULT;
        write!(stdout, "\x1B[m").unwrap();
        for x in x_start..=x_end {
            let c = &grid[(x, y)];
            if c.attrs() != current_attrs {
                c.attrs().write(current_attrs, stdout).unwrap();
                current_attrs = c.attrs();
            }
            if !c.empty() {
                write!(stdout, "{}", c.ch()).unwrap();
            }
        }
    }

    /// Draw the entire screen from scratch.
    pub fn render(&mut self) {
        self.update_size();
        let cols = self.cols;
        let rows = self.rows;
        self.context
            .dirty_areas
            .push_back(((0, 0), (cols - 1, rows - 1)));

        self.redraw();
    }

    pub fn draw_component(&mut self, idx: usize) {
        let component = &mut self.components[idx];
        let upper_left = (0, 0);
        let bottom_right = (self.cols - 1, self.rows - 1);

        if component.is_dirty() {
            component.draw(
                &mut self.grid,
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
        components.iter_mut().all(|c| c.can_quit_cleanly(context))
    }

    pub fn register_component(&mut self, component: Box<dyn Component>) {
        self.components.push(component);
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
                    .iter_mut()
                    .find(|a| a.name() == account_name)
                {
                    match account.mailbox_operation(op) {
                        Err(err) => {
                            self.context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(err.to_string()),
                            ));
                        }
                        Ok(msg) => {
                            self.context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(format!("`{}`: {}", account_name, msg)),
                            ));
                        }
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
            AccountAction(ref account_name, ReIndex) => {
                #[cfg(feature = "sqlite3")]
                match crate::sqlite3::index(&mut self.context, account_name) {
                    Ok(()) => {
                        self.context.replies.push_back(UIEvent::Notification(
                            None,
                            "Message index rebuild started.".to_string(),
                            Some(NotificationType::INFO),
                        ));
                    }
                    Err(e) => {
                        self.context.replies.push_back(UIEvent::Notification(
                            None,
                            format!("Message index rebuild failed: {}.", e),
                            Some(NotificationType::ERROR),
                        ));
                    }
                }
                #[cfg(not(feature = "sqlite3"))]
                {
                    self.context.replies.push_back(UIEvent::Notification(
                        None,
                        "Message index rebuild failed: meli is not built with sqlite3 support."
                            .to_string(),
                        Some(NotificationType::ERROR),
                    ));
                }
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
                self.display_messages_expiration_start = Some(melib::datetime::now());
            }
        }

        match event {
            // Command type is handled only by State.
            UIEvent::Command(cmd) => {
                if let Ok(action) = parse_command(&cmd.as_bytes()) {
                    if action.needs_confirmation() {
                        self.overlay.push(Box::new(UIConfirmationDialog::new(
                            "You sure?",
                            vec![(true, "yes".to_string()), (false, "no".to_string())],
                            true,
                            Some(Box::new(move |id: ComponentId, result: bool| {
                                Some(UIEvent::FinishedUIDialog(
                                    id,
                                    Box::new(if result { Some(action) } else { None }),
                                ))
                            })),
                            &mut self.context,
                        )));
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
                write!(self.stdout(), "{}", cursor::Hide,).unwrap();
                self.flush();
                 */
                self.switch_to_main_screen();
                self.switch_to_alternate_screen();
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
            UIEvent::WorkerProgress(account_hash, mailbox_hash) => {
                if let Some(&account_idx) = self.context.account_hashes.get(&account_hash) {
                    let _ = self.context.accounts[account_idx].load(mailbox_hash);
                }
                return;
            }
            UIEvent::ChangeMode(m) => {
                self.context
                    .sender
                    .send(ThreadEvent::UIEvent(UIEvent::ChangeMode(m)))
                    .unwrap();
            }
            UIEvent::Timer(id) if id == self.draw_rate_limit.id() => {
                self.draw_rate_limit.reset();
                self.redraw();
                return;
            }
            UIEvent::Input(Key::Alt('<')) => {
                self.display_messages_expiration_start = Some(melib::datetime::now());
                self.display_messages_active = true;
                self.display_messages_pos = self.display_messages_pos.saturating_sub(1);
                return;
            }
            UIEvent::Input(Key::Alt('>')) => {
                self.display_messages_expiration_start = Some(melib::datetime::now());
                self.display_messages_active = true;
                self.display_messages_pos = std::cmp::min(
                    self.display_messages.len().saturating_sub(1),
                    self.display_messages_pos + 1,
                );
                return;
            }
            UIEvent::StatusEvent(StatusEvent::DisplayMessage(ref msg)) => {
                self.display_messages.push(DisplayMessage {
                    timestamp: melib::datetime::now(),
                    msg: msg.clone(),
                });
                self.display_messages_active = true;
                self.display_messages_expiration_start = None;
                self.display_messages_pos = self.display_messages.len() - 1;
                self.redraw();
            }
            UIEvent::FinishedUIDialog(ref id, ref mut results)
                if self.overlay.iter().any(|c| c.id() == *id) =>
            {
                if let Some(Some(ref mut action)) = results.downcast_mut::<Option<Action>>() {
                    self.exec_command(std::mem::replace(action, Action::ToggleThreadSnooze));

                    let pos = self.overlay.iter().position(|c| c.id() == *id).unwrap();
                    self.overlay.remove(pos);
                    return;
                }
            }
            UIEvent::GlobalUIDialog(dialog) => {
                self.overlay.push(dialog);
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
        for c in overlay.iter_mut().chain(components.iter_mut()) {
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

    pub fn try_wait_on_child(&mut self) -> Option<bool> {
        let should_return_flag = match self.child {
            Some(ForkType::NewDraft(_, ref mut c)) => {
                let w = c.try_wait();
                match w {
                    Ok(Some(_)) => true,
                    Ok(None) => false,
                    Err(e) => {
                        log(
                            format!("Failed to wait on editor process: {}", e.to_string()),
                            ERROR,
                        );
                        return None;
                    }
                }
            }
            Some(ForkType::Generic(ref mut c)) => {
                let w = c.try_wait();
                match w {
                    Ok(Some(_)) => true,
                    Ok(None) => false,
                    Err(e) => {
                        log(
                            format!("Failed to wait on child process: {}", e.to_string()),
                            ERROR,
                        );
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

    pub fn check_accounts(&mut self) {
        let mut ctr = 0;
        for i in 0..self.context.accounts.len() {
            if self.context.is_online(i).is_ok() {
                ctr += 1;
            }
        }
        if ctr != self.context.accounts.len() {
            self.timer.thread().unpark();
        }
    }
}
