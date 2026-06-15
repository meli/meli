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

//! Notification handling components
use std::process::{Command, Stdio};

use melib::{utils::datetime, UnixTimestamp};
pub use system::*;

use super::*;

mod system {
    use std::borrow::Cow;

    use super::*;
    use crate::types::RateLimit;

    /// Passes notifications to the OS using `notify-rust` crate.
    #[derive(Debug)]
    pub struct SystemNotifications {
        rate_limit: RateLimit,
        id: ComponentId,
    }

    impl std::fmt::Display for SystemNotifications {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "")
        }
    }

    impl SystemNotifications {
        pub fn new(context: &Context) -> Self {
            Self {
                rate_limit: RateLimit::new(
                    1000,
                    1000,
                    context.main_loop_handler.job_executor.clone(),
                ),
                id: ComponentId::default(),
            }
        }
    }

    impl Component for SystemNotifications {
        fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}

        fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
            if !context.settings.notifications.enable.system_enabled()
                || context.settings.notifications.script.is_some()
            {
                return false;
            }

            #[cfg(any(
                all(target_os = "linux", feature = "dbus-notifications"),
                target_os = "macos"
            ))]
            if let UIEvent::Notification {
                ref title,
                source: _,
                ref body,
                ref kind,
            } = event
            {
                if !self.rate_limit.tick() {
                    return false;
                }

                let mut notification = notify_rust::Notification::new();
                notification
                    .appname("meli")
                    .summary(title.as_ref().map(<_>::as_ref).unwrap_or("meli"))
                    .body(&escape_str(body));
                match *kind {
                    Some(NotificationType::NewMail) => {
                        #[cfg(all(unix, not(target_os = "macos")))]
                        notification.hint(notify_rust::Hint::Category("email".to_owned()));
                        notification.icon("mail-message-new");
                        notification.sound_name("message-new-email");
                    }
                    Some(NotificationType::SentMail) => {
                        #[cfg(all(unix, not(target_os = "macos")))]
                        notification.hint(notify_rust::Hint::Category("email".to_owned()));
                        notification.icon("mail-send");
                        notification.sound_name("message-sent-email");
                    }
                    Some(NotificationType::Saved) => {
                        notification.icon("document-save");
                    }
                    Some(NotificationType::Info) => {
                        notification.icon("dialog-information");
                    }
                    Some(NotificationType::Error(melib::ErrorKind::Authentication)) => {
                        notification.icon("dialog-password");
                    }
                    Some(NotificationType::Error(melib::ErrorKind::Bug)) => {
                        notification.icon("face-embarrassed");
                    }
                    Some(NotificationType::Error(melib::ErrorKind::None))
                    | Some(NotificationType::Error(melib::ErrorKind::External)) => {
                        notification.icon("dialog-error");
                    }
                    Some(NotificationType::Error(melib::ErrorKind::Network(_))) => {
                        notification.icon("network-error");
                    }
                    Some(NotificationType::Error(melib::ErrorKind::TimedOut)) => {
                        notification.icon("network-offline");
                    }
                    _ => {}
                }
                #[cfg(all(unix, not(target_os = "macos")))]
                if context.settings.notifications.play_sound.is_true() {
                    if let Some(ref sound_path) = context.settings.notifications.sound_file {
                        notification.hint(notify_rust::Hint::SoundFile(sound_path.to_owned()));
                    }
                } else {
                    notification.hint(notify_rust::Hint::SuppressSound(true));
                }

                if let Err(err) = notification.show() {
                    log::error!("Could not show system notification: {err}");
                }
            }
            false
        }

        fn set_dirty(&mut self, _value: bool) {}

        fn is_dirty(&self) -> bool {
            false
        }

        fn id(&self) -> ComponentId {
            self.id
        }
    }

    #[cfg(all(target_os = "linux", feature = "dbus-notifications"))]
    #[inline]
    fn escape_str(s: &'_ str) -> Cow<'_, str> {
        if !s.chars().any(|c| {
            let i = c as u32;
            ['&', '<', '>', '\'', '"'].contains(&c)
                || (0x1..=0x8).contains(&i)
                || (0xb..=0xc).contains(&i)
                || (0xe..=0x1f).contains(&i)
                || (0x7f..=0x84).contains(&i)
                || (0x86..=0x9f).contains(&i)
        }) {
            return Cow::Borrowed(s);
        }
        let mut ret: String = String::with_capacity(s.len());
        for c in s.chars() {
            match c {
                '&' => ret.push_str("&amp;"),
                '<' => ret.push_str("&lt;"),
                '>' => ret.push_str("&gt;"),
                '\'' => ret.push_str("&apos;"),
                '"' => ret.push_str("&quot;"),
                _ => {
                    let i = c as u32;
                    if (0x1..=0x8).contains(&i)
                        || (0xb..=0xc).contains(&i)
                        || (0xe..=0x1f).contains(&i)
                        || (0x7f..=0x84).contains(&i)
                        || (0x86..=0x9f).contains(&i)
                    {
                        use std::fmt::Write;
                        let _ = write!(ret, "&#{i:x}%{i:x};");
                    } else {
                        ret.push(c);
                    }
                }
            }
        }
        Cow::Owned(ret)
    }

    #[cfg(target_os = "macos")]
    #[inline]
    fn escape_str(s: &'_ str) -> Cow<'_, str> {
        Cow::Borrowed(s)
    }
}

/// Passes notifications to a user defined shell command
#[derive(Debug)]
pub struct NotificationRouter {
    system: SystemNotifications,
    /// Identifier of component.
    id: ComponentId,
}

impl NotificationRouter {
    /// Create a new [`NotificationRouter`] component.
    pub fn new(context: &Context) -> Self {
        Self {
            system: SystemNotifications::new(context),
            id: ComponentId::default(),
        }
    }

    /// Update an `xbiff` file at `path`.
    ///
    /// The ASCII byte `z` is appended to the file, unless the file size is
    /// above 128 bytes in which case the file is truncated to 0 bytes.
    pub fn update_xbiff(path: &str) -> Result<()> {
        let mut file = std::fs::OpenOptions::new()
            .append(true) /* writes will append to a file instead of overwriting previous contents */
            .create(true) /* a new file will be created if the file does not yet already exist. */
            .open(path)?;
        if file.metadata()?.len() > 128 {
            file.set_len(0)?;
        } else {
            std::io::Write::write_all(&mut file, b"z")?;
        }
        Ok(())
    }

    fn show_notification(
        &self,
        context: &mut Context,
        title: Option<&str>,
        body: &str,
        kind: Option<&NotificationType>,
    ) -> Result<()> {
        if matches!(kind, Some(NotificationType::NewMail)) {
            if let Some(ref path) = context.settings.notifications.xbiff_file_path {
                Self::update_xbiff(path).chain_err_details(|| "Could not update xbiff file")?;
            }
        }

        let script = if matches!(kind, Some(NotificationType::NewMail))
            && context.settings.notifications.new_mail_script.is_some()
        {
            context.settings.notifications.new_mail_script.as_ref()
        } else {
            context.settings.notifications.script.as_ref()
        };

        if let Some(ref bin) = script {
            let child = Command::new(bin)
                .arg(kind.map(|k| k.to_string()).unwrap_or_default())
                .arg(title.as_ref().map(<_>::as_ref).unwrap_or("meli"))
                .arg(body)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .chain_err_details(|| "Could not run notification script")?;
            context
                .children
                .entry(melib::identify!(NotificationRouter).into())
                .or_default()
                .push(ForkedProcess::Generic {
                    id: melib::identify!(NotificationRouter).into(),
                    command: Some(
                        format!(
                            "{bin} {kind} {title} {body}",
                            kind = kind.map(|k| k.to_string()).unwrap_or_default(),
                            title = title.as_ref().map(<_>::as_ref).unwrap_or("meli"),
                        )
                        .into(),
                    ),
                    child,
                });
        }
        Ok(())
    }
}

impl std::fmt::Display for NotificationRouter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "")
    }
}

impl Component for NotificationRouter {
    fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        _ = self.system.process_event(event, context);

        if let UIEvent::Notification {
            ref title,
            source: _,
            ref body,
            ref kind,
        } = event
        {
            if context.settings.notifications.enable.system_enabled() {
                if let Err(err) =
                    self.show_notification(context, title.as_deref(), body, kind.as_ref())
                {
                    log::error!("{err}");
                }
            }
        }

        false
    }

    fn is_dirty(&self) -> bool {
        false
    }

    fn set_dirty(&mut self, _value: bool) {}

    fn id(&self) -> ComponentId {
        self.id
    }
}

#[derive(Debug)]
/// On-screen-display messages.
struct DisplayMessage {
    timestamp: UnixTimestamp,
    msg: String,
    repeats: u8,
}

#[derive(Debug)]
/// Show notifications on [`Screen`].
pub struct DisplayMessageBox {
    /// Stored messages.
    messages: Vec<DisplayMessage>,
    /// Timestamp of the latest time the widget was set to be displayed, used to
    /// calculate expiration (i.e. when to stop displaying it).
    pub expiration_start: Option<UnixTimestamp>,
    /// Whether the widget is to be displayed.
    pub active: bool,
    /// Whether the widget needs to be redrawn.
    dirty: bool,
    /// Whether the widget has been drawn at least once.
    pub initialised: bool,
    /// Position of message from `messages` field to show.
    pub pos: usize,
    /// The last [`Area`] this widget used to be drawn.
    cached_area: Area,
    /// Identifier of component.
    id: ComponentId,
}

impl DisplayMessageBox {
    /// Create a new [`DisplayMessageBox`] for this specific [`Screen`].
    pub fn new(sc: &Screen<Tty>) -> Self {
        Self {
            messages: Vec::new(),
            expiration_start: None,
            pos: 0,
            active: false,
            dirty: false,
            initialised: false,
            cached_area: sc.area().into_empty(),
            id: ComponentId::default(),
        }
    }

    /// Returns the last [`Area`] this widget used to be drawn.
    #[inline]
    pub fn cached_area(&self) -> Area {
        self.cached_area
    }

    /// Returns `true` if there are no messages.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.messages.is_empty()
    }

    /// Returns the amount of messages.
    #[inline]
    pub fn len(&self) -> usize {
        self.messages.len()
    }

    /// Prevent box from being shown unless armed again.
    #[inline]
    pub fn deactivate(&mut self) {
        self.active = false;
        self.set_dirty(true);
        self.initialised = false;
        self.expiration_start = None;
    }

    /// Append a message if its contents are not equal to the last existing
    /// message.
    #[inline]
    pub fn try_push(&mut self, msg: String, timestamp: UnixTimestamp) -> bool {
        if let Some(ref mut last) = self.messages.last_mut().filter(|el| el.msg == msg) {
            last.repeats = last.repeats.saturating_add(1);
            return false;
        }
        self.messages.push(DisplayMessage {
            msg,
            timestamp,
            repeats: 1,
        });
        true
    }

    /// Trigger display of last message, if any.
    #[inline]
    pub fn arm(&mut self, context: &mut Context) {
        if self.messages.is_empty() {
            return;
        }
        self.pos = self.messages.len().saturating_sub(1);
        if context.settings.notifications.enable.ui_enabled() {
            self.active = true;
            self.initialised = false;
            self.set_dirty(true);
            self.expiration_start = None;
        }
    }

    /// Activate and set position to previous message, if any.
    #[inline]
    pub fn show_previous(&mut self) {
        self.expiration_start = Some(datetime::now());
        self.active = true;
        self.initialised = false;
        self.set_dirty(true);
        self.pos = self.pos.saturating_sub(1);
    }

    /// Activate and set position to next message, if any.
    #[inline]
    pub fn show_next(&mut self) {
        self.expiration_start = Some(datetime::now());
        self.active = true;
        self.initialised = false;
        self.set_dirty(true);
        self.pos = self.len().saturating_sub(1).min(self.pos + 1);
    }
}

impl std::fmt::Display for DisplayMessageBox {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "")
    }
}

impl Component for DisplayMessageBox {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if let Some(DisplayMessage {
            ref timestamp,
            ref msg,
            ..
        }) = self.messages.get(self.pos)
        {
            let noto_colors = crate::conf::value(context, "status.notification");
            use crate::melib::text::{Reflow, TextProcessing};

            let box_width = area.width() / 3;
            if box_width < 10 {
                self.set_dirty(false);
                return;
            }

            let msg_lines = msg.split_lines_reflow(Reflow::All, Some(box_width));
            let width = msg_lines
                .iter()
                .map(|line| line.grapheme_len() + 4)
                .max()
                .unwrap_or(0);

            if width == 0 {
                self.set_dirty(false);
                return;
            }

            self.cached_area = area.place_inside(
                (width, area.height().min(msg_lines.len() + 4)),
                false,
                false,
            );
            let box_displ_area = create_box(grid, self.cached_area);
            for row in grid.bounds_iter(box_displ_area) {
                for c in row {
                    grid[c]
                        .set_ch(' ')
                        .set_fg(noto_colors.fg)
                        .set_bg(noto_colors.bg)
                        .set_attrs(noto_colors.attrs);
                }
            }
            let mut lines_no = 0;
            for (idx, line) in msg_lines
                .into_iter()
                .chain(Some(String::new()))
                .chain(Some(datetime::timestamp_to_string(*timestamp, None, false)))
                .enumerate()
            {
                let (_, y) = grid.write_string(
                    &line,
                    noto_colors.fg,
                    noto_colors.bg,
                    noto_colors.attrs,
                    box_displ_area.skip_rows(idx),
                    None,
                    Some(0),
                );
                lines_no += 1 + y;
            }

            if self.messages.len() > 1 {
                grid.write_string(
                    &if self.pos == 0 {
                        format!(
                            "Next: {}",
                            context.settings.shortcuts.general.info_message_next
                        )
                    } else if self.pos + 1 == self.len() {
                        format!(
                            "Prev: {}",
                            context.settings.shortcuts.general.info_message_previous
                        )
                    } else {
                        format!(
                            "Prev: {} Next: {}",
                            context.settings.shortcuts.general.info_message_previous,
                            context.settings.shortcuts.general.info_message_next
                        )
                    },
                    noto_colors.fg,
                    noto_colors.bg,
                    noto_colors.attrs,
                    box_displ_area.skip_rows(lines_no),
                    None,
                    Some(0),
                );
            }
        } else {
            self.cached_area = area.into_empty();
        }
        self.set_dirty(false);
    }

    fn process_event(&mut self, _event: &mut UIEvent, _context: &mut Context) -> bool {
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}
