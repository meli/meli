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

#[cfg(all(target_os = "linux", feature = "dbus-notifications"))]
pub use dbus::*;
use melib::{utils::datetime, UnixTimestamp};
use smallvec::SmallVec;

use super::*;

#[cfg(all(target_os = "linux", feature = "dbus-notifications"))]
mod dbus {
    use super::*;
    use crate::types::RateLimit;

    /// Passes notifications to the OS using Dbus
    #[derive(Debug)]
    pub struct DbusNotifications {
        rate_limit: RateLimit,
        id: ComponentId,
    }

    impl std::fmt::Display for DbusNotifications {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "")
        }
    }

    impl DbusNotifications {
        pub fn new(context: &Context) -> Self {
            DbusNotifications {
                rate_limit: RateLimit::new(
                    1000,
                    1000,
                    context.main_loop_handler.job_executor.clone(),
                ),
                id: ComponentId::default(),
            }
        }
    }

    impl Component for DbusNotifications {
        fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}

        fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
            if !context.settings.notifications.enable {
                return false;
            }

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

                let settings = &context.settings.notifications;
                let mut notification = notify_rust::Notification::new();
                notification
                    .appname("meli")
                    .summary(title.as_ref().map(<_>::as_ref).unwrap_or("meli"))
                    .body(&escape_str(body));
                match *kind {
                    Some(NotificationType::NewMail) => {
                        notification.hint(notify_rust::Hint::Category("email".to_owned()));
                        notification.icon("mail-message-new");
                        notification.sound_name("message-new-email");
                    }
                    Some(NotificationType::SentMail) => {
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
                if settings.play_sound.is_true() {
                    if let Some(ref sound_path) = settings.sound_file {
                        notification.hint(notify_rust::Hint::SoundFile(sound_path.to_owned()));
                    }
                } else {
                    notification.hint(notify_rust::Hint::SuppressSound(true));
                }

                if let Err(err) = notification.show() {
                    log::error!("Could not show dbus notification: {err}");
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

    fn escape_str(s: &str) -> String {
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
                        let _ = write!(ret, "&#{:x}%{:x};", i, i);
                    } else {
                        ret.push(c);
                    }
                }
            }
        }
        ret
    }
}

/// Passes notifications to a user defined shell command
#[derive(Debug, Default)]
pub struct NotificationCommand {
    id: ComponentId,
}

impl NotificationCommand {
    pub fn new() -> Self {
        NotificationCommand::default()
    }
}

impl std::fmt::Display for NotificationCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "")
    }
}

impl Component for NotificationCommand {
    fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::Notification {
            ref title,
            source: _,
            ref body,
            ref kind,
        } = event
        {
            if context.settings.notifications.enable {
                if *kind == Some(NotificationType::NewMail) {
                    if let Some(ref path) = context.settings.notifications.xbiff_file_path {
                        if let Err(err) = update_xbiff(path) {
                            log::error!("Could not update xbiff file: {err}.");
                        }
                    }
                }

                let mut script = context.settings.notifications.script.as_ref();
                if *kind == Some(NotificationType::NewMail)
                    && context.settings.notifications.new_mail_script.is_some()
                {
                    script = context.settings.notifications.new_mail_script.as_ref();
                }

                if let Some(ref bin) = script {
                    match Command::new(bin)
                        .arg(&kind.map(|k| k.to_string()).unwrap_or_default())
                        .arg(title.as_ref().map(<_>::as_ref).unwrap_or("meli"))
                        .arg(body.as_ref())
                        .stdin(Stdio::piped())
                        .stdout(Stdio::piped())
                        .spawn()
                    {
                        Ok(child) => {
                            context.children.push(child);
                        }
                        Err(err) => {
                            log::error!("Could not run notification script: {err}.");
                        }
                    }
                } else {
                    #[cfg(target_os = "macos")]
                    {
                        let applescript = format!(
                            "display notification \"{message}\" with title \"{title}\" subtitle \
                             \"{subtitle}\"",
                            message = body.replace('"', "'"),
                            title = title
                                .as_ref()
                                .map(<_>::as_ref)
                                .unwrap_or("meli")
                                .replace('"', "'"),
                            subtitle = kind
                                .map(|k| k.to_string())
                                .unwrap_or_default()
                                .replace('"', "'")
                        );
                        match Command::new("osascript")
                            .arg("-e")
                            .arg(applescript)
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn()
                        {
                            Ok(child) => {
                                context.children.push(child);
                                return false;
                            }
                            Err(err) => {
                                log::error!("Could not run notification script: {err}.");
                            }
                        }
                    }

                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(format!(
                            "{title}{}{body}",
                            if title.is_some() { " " } else { "" },
                            title = title.as_deref().unwrap_or_default(),
                            body = body,
                        ))));
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

fn update_xbiff(path: &str) -> Result<()> {
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

#[derive(Debug)]
/// On-screen-display messages.
pub struct DisplayMessage {
    pub timestamp: UnixTimestamp,
    pub msg: String,
}

#[derive(Debug)]
/// Show notifications on [`Screen`].
pub struct DisplayMessageBox {
    messages: SmallVec<[DisplayMessage; 8]>,
    pub expiration_start: Option<UnixTimestamp>,
    pub active: bool,
    dirty: bool,
    pub initialised: bool,
    pub pos: usize,
    cached_area: Area,
    id: ComponentId,
}

impl DisplayMessageBox {
    pub fn new(sc: &Screen<Tty>) -> Box<Self> {
        Box::new(Self {
            messages: SmallVec::new(),
            expiration_start: None,
            pos: 0,
            active: false,
            dirty: false,
            initialised: false,
            cached_area: sc.area().into_empty(),
            id: ComponentId::default(),
        })
    }

    #[inline]
    pub fn cached_area(&self) -> Area {
        self.cached_area
    }
}

impl std::ops::Deref for DisplayMessageBox {
    type Target = SmallVec<[DisplayMessage; 8]>;

    fn deref(&self) -> &Self::Target {
        &self.messages
    }
}

impl std::ops::DerefMut for DisplayMessageBox {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.messages
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
            use crate::melib::text_processing::{Reflow, TextProcessing};

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
