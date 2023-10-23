use std::{
    fs::File,
    io::prelude::*,
    os::raw::c_int,
    sync::{Arc, Mutex},
};

use meli::{
    terminal::{embed::*, *},
    *,
};
use nix::sys::wait::WaitStatus;

fn notify(
    signals: &[c_int],
    sender: crossbeam::channel::Sender<ThreadEvent>,
) -> std::result::Result<crossbeam::channel::Receiver<c_int>, std::io::Error> {
    use std::time::Duration;
    let (alarm_pipe_r, alarm_pipe_w) =
        nix::unistd::pipe().map_err(|err| std::io::Error::from_raw_os_error(err as i32))?;
    let alarm_handler = move |info: &nix::libc::siginfo_t| {
        let value = unsafe { info.si_value().sival_ptr as u8 };
        let _ = nix::unistd::write(alarm_pipe_w, &[value]);
    };
    unsafe {
        signal_hook_registry::register_sigaction(signal_hook::consts::SIGALRM, alarm_handler)?;
    }
    let (s, r) = crossbeam::channel::bounded(100);
    let mut signals = signal_hook::iterator::Signals::new(signals)?;
    let _ = nix::fcntl::fcntl(
        alarm_pipe_r,
        nix::fcntl::FcntlArg::F_SETFL(nix::fcntl::OFlag::O_NONBLOCK),
    );
    std::thread::spawn(move || {
        let mut ctr = 0;
        loop {
            ctr %= 3;
            if ctr == 0 {
                let _ = sender
                    .send_timeout(ThreadEvent::Pulse, Duration::from_millis(500))
                    .ok();
            }

            for signal in signals.pending() {
                let _ = s.send_timeout(signal, Duration::from_millis(500)).ok();
            }

            std::thread::sleep(std::time::Duration::from_millis(100));
            ctr += 1;
        }
    });
    Ok(r)
}

#[derive(Debug)]
enum EmbedStatus {
    Stopped(Arc<Mutex<EmbedTerminal>>),
    Running(Arc<Mutex<EmbedTerminal>>),
}

impl EmbedStatus {
    #[inline(always)]
    fn is_stopped(&self) -> bool {
        matches!(self, Self::Stopped(_))
    }
}

impl std::ops::Deref for EmbedStatus {
    type Target = Arc<Mutex<EmbedTerminal>>;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Stopped(ref e) | Self::Running(ref e) => e,
        }
    }
}

impl std::ops::DerefMut for EmbedStatus {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Stopped(ref mut e) | Self::Running(ref mut e) => e,
        }
    }
}

#[derive(Debug)]
struct EmbedContainer {
    command: String,
    embed_area: Area,
    embed: Option<EmbedStatus>,
    id: ComponentId,
    dirty: bool,
    log_file: File,
}

impl EmbedContainer {
    fn new(command: String) -> Box<Self> {
        Box::new(Self {
            command,
            embed: None,
            embed_area: ((0, 0), (80, 20)),
            dirty: true,
            log_file: File::open(".embed.out").unwrap(),
            id: ComponentId::default(),
        })
    }
}

impl std::fmt::Display for EmbedContainer {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "embed")
    }
}

impl Component for EmbedContainer {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if let Some(ref mut embed_pty) = self.embed {
            let embed_area = area;
            let theme_default = crate::conf::value(context, "theme_default");
            match embed_pty {
                EmbedStatus::Running(_) => {
                    let mut guard = embed_pty.lock().unwrap();
                    grid.clear_area(embed_area, theme_default);

                    grid.copy_area(
                        guard.grid.buffer(),
                        embed_area,
                        ((0, 0), pos_dec(guard.grid.terminal_size, (1, 1))),
                    );
                    guard.set_terminal_size((embed_area.width(), embed_area.height()));
                    context.dirty_areas.push_back(area);
                    self.dirty = false;
                    return;
                }
                EmbedStatus::Stopped(_) => {
                    let guard = embed_pty.lock().unwrap();

                    grid.copy_area(
                        guard.grid.buffer(),
                        embed_area,
                        ((0, 0), pos_dec(guard.grid.terminal_size, (1, 1))),
                    );

                    grid.change_theme(
                        embed_area,
                        ThemeAttribute {
                            fg: Color::Byte(8),
                            ..theme_default
                        },
                    );
                    let stopped_message: String =
                        format!("Process with PID {} has stopped.", guard.child_pid);
                    let stopped_message_2: String = "-press 'e' to re-activate.".to_string();
                    const STOPPED_MESSAGE_3: &str =
                        "-press Ctrl-C to forcefully kill it and return to editor.";
                    let max_len = std::cmp::max(
                        stopped_message.len(),
                        std::cmp::max(stopped_message_2.len(), STOPPED_MESSAGE_3.len()),
                    );
                    let inner_area = create_box(
                        grid,
                        (
                            pos_inc(area.upper_left(), (1, 0)),
                            pos_inc(
                                area.upper_left(),
                                (
                                    std::cmp::min(max_len + 5, area.width()),
                                    std::cmp::min(5, area.height()),
                                ),
                            ),
                        ),
                    );
                    grid.clear_area(inner_area, theme_default);
                    for (i, l) in [
                        stopped_message.as_str(),
                        stopped_message_2.as_str(),
                        STOPPED_MESSAGE_3,
                    ]
                    .iter()
                    .enumerate()
                    {
                        grid.write_string(
                            l,
                            theme_default.fg,
                            theme_default.bg,
                            theme_default.attrs,
                            (
                                pos_inc((0, i), inner_area.upper_left()),
                                inner_area.bottom_right(),
                            ),
                            Some(get_x(inner_area.upper_left())),
                        );
                    }
                }
            }
        } else {
            let theme_default = crate::conf::value(context, "theme_default");
            grid.clear_area(area, theme_default);
            self.embed_area = (area.upper_left(), area.bottom_right());
            match create_pty(
                self.embed_area.width(),
                self.embed_area.height(),
                self.command.clone(),
            ) {
                Ok(embed) => {
                    //embed.lock().unwrap().set_log_file(self.log_file.take());
                    self.embed = Some(EmbedStatus::Running(embed));
                    self.set_dirty(true);
                    context
                        .replies
                        .push_back(UIEvent::ChangeMode(UIMode::Embed));
                    context.replies.push_back(UIEvent::Fork(ForkType::Embed(
                        self.embed.as_ref().unwrap().lock().unwrap().child_pid,
                    )));
                }
                Err(err) => {
                    context.replies.push_back(UIEvent::Notification(
                        Some(format!("Failed to create pseudoterminal: {}", err)),
                        err.to_string(),
                        Some(NotificationType::Error(melib::error::ErrorKind::External)),
                    ));
                }
            }
        }
        context.dirty_areas.push_back(area);
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match event {
            UIEvent::EmbedInput((Key::Ctrl('z'), _)) => {
                self.embed.as_ref().unwrap().lock().unwrap().stop();
                match self.embed.take() {
                    Some(EmbedStatus::Running(e)) | Some(EmbedStatus::Stopped(e)) => {
                        self.embed = Some(EmbedStatus::Stopped(e));
                    }
                    _ => {}
                }
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                self.set_dirty(true);
            }
            UIEvent::EmbedInput((ref k, ref b)) => {
                let _ = self
                    .log_file
                    .write_all(format!("{} bytes {:?}", k, b).as_bytes());
                let _ = self.log_file.flush();
                if let Some(ref mut embed) = self.embed {
                    let mut embed_guard = embed.lock().unwrap();
                    if embed_guard.write_all(b).is_err() {
                        match embed_guard.is_active() {
                            Ok(WaitStatus::Exited(_, exit_code)) => {
                                drop(embed_guard);
                                _ = self.embed.take();
                                if exit_code != 0 {
                                    context.replies.push_back(UIEvent::Notification(
                                        None,
                                        format!(
                                            "Subprocess has exited with exit code {}",
                                            exit_code
                                        ),
                                        Some(NotificationType::Error(
                                            melib::error::ErrorKind::External,
                                        )),
                                    ));
                                }
                                self.set_dirty(true);
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                            }
                            #[cfg(any(target_os = "linux", target_os = "android"))]
                            Ok(WaitStatus::PtraceEvent(_, _, _))
                            | Ok(WaitStatus::PtraceSyscall(_)) => {
                                drop(embed_guard);
                                match self.embed.take() {
                                    Some(EmbedStatus::Running(e))
                                    | Some(EmbedStatus::Stopped(e)) => {
                                        self.embed = Some(EmbedStatus::Stopped(e));
                                    }
                                    _ => {}
                                }
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                                self.set_dirty(true);
                                return true;
                            }
                            Ok(WaitStatus::Stopped(_, _)) => {
                                drop(embed_guard);
                                match self.embed.take() {
                                    Some(EmbedStatus::Running(e))
                                    | Some(EmbedStatus::Stopped(e)) => {
                                        self.embed = Some(EmbedStatus::Stopped(e));
                                    }
                                    _ => {}
                                }
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                                self.set_dirty(true);
                                return true;
                            }
                            Ok(WaitStatus::Continued(_)) | Ok(WaitStatus::StillAlive) => {
                                context
                                    .replies
                                    .push_back(UIEvent::EmbedInput((k.clone(), b.to_vec())));
                                return true;
                            }
                            Ok(WaitStatus::Signaled(_, signal, _)) => {
                                drop(embed_guard);
                                context.replies.push_back(UIEvent::Notification(
                                    None,
                                    format!("Subprocess was killed by {} signal", signal),
                                    Some(NotificationType::Error(
                                        melib::error::ErrorKind::External,
                                    )),
                                ));
                                self.embed = None;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                            }
                            Err(err) => {
                                context.replies.push_back(UIEvent::Notification(
                                    Some("Embed editor crashed.".to_string()),
                                    format!("Subprocess has exited with reason {}", &err),
                                    Some(NotificationType::Error(
                                        melib::error::ErrorKind::External,
                                    )),
                                ));
                                drop(embed_guard);
                                self.embed = None;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                            }
                        }
                    }
                }
                self.set_dirty(true);
                return true;
            }
            UIEvent::Resize => {
                self.set_dirty(true);
            }
            UIEvent::Input(Key::Char('e')) if self.embed.is_some() => {
                self.embed.as_ref().unwrap().lock().unwrap().wake_up();
                match self.embed.take() {
                    Some(EmbedStatus::Running(e)) | Some(EmbedStatus::Stopped(e)) => {
                        self.embed = Some(EmbedStatus::Running(e));
                    }
                    _ => {}
                }
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Embed));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(Key::Ctrl('c'))
                if self.embed.is_some() && self.embed.as_ref().unwrap().is_stopped() =>
            {
                match self.embed.take() {
                    Some(EmbedStatus::Running(embed)) | Some(EmbedStatus::Stopped(embed)) => {
                        let guard = embed.lock().unwrap();
                        guard.wake_up();
                        guard.terminate();
                    }
                    _ => {}
                }
                context.replies.push_back(UIEvent::Notification(
                    None,
                    "Subprocess was killed by SIGTERM signal".to_string(),
                    Some(NotificationType::Error(melib::error::ErrorKind::External)),
                ));
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                self.set_dirty(true);
                return true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        true
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}

fn main() -> std::io::Result<()> {
    let command = std::env::args()
        .nth(1)
        .expect("expected command as first argument");
    /* Create a channel to communicate with other threads. The main process is
     * the sole receiver.
     */
    let (sender, receiver) = crossbeam::channel::bounded(32 * ::std::mem::size_of::<ThreadEvent>());
    /* Catch SIGWINCH to handle terminal resizing */
    let signals = &[
        /* Catch SIGWINCH to handle terminal resizing */
        signal_hook::consts::SIGWINCH,
        /* Catch SIGCHLD to handle embed applications status change */
        signal_hook::consts::SIGCHLD,
    ];
    let quit_key: Key = Key::Char('q');

    let window = EmbedContainer::new(command);
    let signal_recvr = notify(signals, sender.clone())?;
    let mut state = meli::State::new(Some(Default::default()), sender, receiver.clone()).unwrap();
    let status_bar = Box::new(StatusBar::new(&state.context, window));
    state.register_component(status_bar);
    /* Keep track of the input mode. See UIMode for details */
    'main: loop {
        state.render();

        'inner: loop {
            /* Check if any components have sent reply events to State. */
            let events: smallvec::SmallVec<[UIEvent; 8]> = state.context.replies();
            for e in events {
                state.rcv_event(e);
            }
            state.redraw();

            /* Poll on all channels. Currently we have the input channel for stdin,
             * watching events and the signal watcher. */
            crossbeam::select! {
                recv(receiver) -> r => {
                    match r {
                         Ok(ThreadEvent::Pulse) | Ok(ThreadEvent::UIEvent(UIEvent::Timer(_))) => {},
                        _ => {
                            log::debug!("{:?}", &r);
                        }
                    }
                    match r.unwrap() {
                        ThreadEvent::Input((Key::Ctrl('z'), _)) if state.mode != UIMode::Embed => {
                            state.switch_to_main_screen();
                            //_thread_handler.join().expect("Couldn't join on the associated thread");
                            let self_pid = nix::unistd::Pid::this();
                            nix::sys::signal::kill(self_pid, nix::sys::signal::Signal::SIGSTOP).unwrap();
                            state.switch_to_alternate_screen();
                            // BUG: thread sends input event after one received key
                            state.update_size();
                            state.render();
                            state.redraw();
                        },
                        ThreadEvent::Input(raw_input @ (Key::Ctrl('l'), _)) => {
                            /* Manual screen redraw */
                            state.update_size();
                            state.render();
                            state.redraw();
                            if state.mode == UIMode::Embed {
                                state.rcv_event(UIEvent::EmbedInput(raw_input));
                                state.redraw();
                            }
                        },
                        ThreadEvent::Input((k, r)) => {
                            match state.mode {
                                UIMode::Normal => {
                                    match k {
                                        _ if k == quit_key => {
                                            if state.can_quit_cleanly() {
                                                drop(state);
                                                break 'main;
                                            } else {
                                                state.redraw();
                                            }
                                        },
                                        key  => {
                                            state.rcv_event(UIEvent::Input(key));
                                            state.redraw();
                                        },
                                    }
                                },
                                UIMode::Insert => {
                                    match k {
                                        Key::Esc => {
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Normal));
                                            state.redraw();
                                        },
                                        k => {
                                            state.rcv_event(UIEvent::InsertInput(k));
                                            state.redraw();
                                        },
                                    }
                                }
                                UIMode::Command => {
                                    match k {
                                        Key::Char('\n') => {
                                            state.mode = UIMode::Normal;
                                            state.rcv_event(UIEvent::ChangeMode(UIMode::Normal));
                                            state.redraw();
                                        },
                                        k => {
                                            state.rcv_event(UIEvent::CmdInput(k));
                                            state.redraw();
                                        },
                                    }
                                },
                                UIMode::Embed => {
                                    state.rcv_event(UIEvent::EmbedInput((k,r)));
                                    state.redraw();
                                },
                                UIMode::Fork => {
                                    break 'inner; // `goto` 'reap loop, and wait on child.
                                },
                            }
                        },
                        ThreadEvent::RefreshMailbox(event) => {
                            state.refresh_event(*event);
                            state.redraw();
                        },
                        ThreadEvent::UIEvent(UIEvent::ChangeMode(f)) => {
                            state.mode = f;
                            if f == UIMode::Fork {
                                break 'inner; // `goto` 'reap loop, and wait on child.
                            }
                        }
                        ThreadEvent::UIEvent(e) => {
                            state.rcv_event(e);
                            state.redraw();
                        },
                        ThreadEvent::Pulse => {
                            state.check_accounts();
                            state.redraw();
                        },
                        ThreadEvent::JobFinished(id) => {
                            log::debug!("Job finished {}", id);
                            for account in state.context.accounts.values_mut() {
                                if account.process_event(&id) {
                                    break;
                                }
                            }
                            //state.new_thread(id, name);
                        },
                    }
                },
                recv(signal_recvr) -> sig => {
                    match sig.unwrap() {
                        signal_hook::consts::SIGWINCH => {
                            if state.mode != UIMode::Fork  {
                                state.update_size();
                                state.render();
                                state.redraw();
                            }
                        },
                        signal_hook::consts::SIGCHLD => {
                            state.rcv_event(UIEvent::EmbedInput((Key::Null, vec![0])));
                            state.redraw();

                        }
                        other => {
                            log::debug!("got other signal: {:?}", other);
                        }
                    }
                },
            }
        } // end of 'inner

        'reap: loop {
            match state.try_wait_on_child() {
                Some(true) => {
                    state.restore_input();
                    state.switch_to_alternate_screen();
                }
                Some(false) => {
                    use std::{thread, time};
                    let ten_millis = time::Duration::from_millis(1500);
                    thread::sleep(ten_millis);

                    continue 'reap;
                }
                None => {
                    state.mode = UIMode::Normal;
                    state.render();
                    break 'reap;
                }
            }
        }
    }
    Ok(())
}
