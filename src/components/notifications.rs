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

/*!
Notification handling components.
*/
use std::process::{Command, Stdio};

use super::*;

#[cfg(feature = "dbus-notifications")]
pub use dbus::*;

#[cfg(feature = "dbus-notifications")]
mod dbus {
    use super::*;
    use crate::types::RateLimit;

    /// Passes notifications to the OS using Dbus
    #[derive(Debug)]
    pub struct DbusNotifications {
        rate_limit: RateLimit,
    }

    impl fmt::Display for DbusNotifications {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "")
        }
    }

    impl DbusNotifications {
        pub fn new() -> Self {
            DbusNotifications {
                rate_limit: RateLimit::new(1000, 1000),
            }
        }
    }

    impl Component for DbusNotifications {
        fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}

        fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
            if !context.settings.notifications.enable {
                return false;
            }

            if let UIEvent::Notification(ref title, ref body, ref kind) = event {
                if !self.rate_limit.tick() {
                    return false;
                }

                let settings = &context.settings.notifications;
                let mut notification = notify_rust::Notification::new();
                notification
                    .appname("meli")
                    .icon("mail-message-new")
                    .summary(title.as_ref().map(String::as_str).unwrap_or("meli"))
                    .body(&escape_str(body));
                if *kind == Some(NotificationType::NEWMAIL) {
                    notification.hint(notify_rust::Hint::Category("email".to_owned()));
                }
                if settings.play_sound.is_true() {
                    if let Some(ref sound_path) = settings.sound_file {
                        notification.hint(notify_rust::Hint::SoundFile(sound_path.to_owned()));
                    } else {
                        notification.sound_name("message-new-email");
                    }
                } else {
                    notification.hint(notify_rust::Hint::SuppressSound(true));
                }

                if let Err(err) = notification.show() {
                    debug!("Could not show dbus notification: {:?}", &err);
                    melib::log(
                        format!("Could not show dbus notification: {}", err),
                        melib::ERROR,
                    );
                }
            }
            false
        }

        fn set_dirty(&mut self, _value: bool) {}

        fn is_dirty(&self) -> bool {
            false
        }

        fn id(&self) -> ComponentId {
            ComponentId::nil()
        }

        fn set_id(&mut self, _id: ComponentId) {}
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
                    if (0x1 <= i && i <= 0x8)
                        || (0xb <= i && i <= 0xc)
                        || (0xe <= i && i <= 0x1f)
                        || (0x7f <= i && i <= 0x84)
                        || (0x86 <= i && i <= 0x9f)
                    {
                        ret.push_str(&format!("&#{:x}%{:x};", i, i));
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
#[derive(Debug)]
pub struct NotificationCommand {}

impl NotificationCommand {
    pub fn new() -> Self {
        NotificationCommand {}
    }
}

impl fmt::Display for NotificationCommand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}

impl Component for NotificationCommand {
    fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if !context.settings.notifications.enable {
            return false;
        }

        if let UIEvent::Notification(ref title, ref body, ref kind) = event {
            if let Some(ref bin) = context.settings.notifications.script {
                match Command::new(bin)
                    .arg(&kind.map(|k| k.to_string()).unwrap_or_default())
                    .arg(title.as_ref().map(String::as_str).unwrap_or("meli"))
                    .arg(body)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    Ok(child) => {
                        context.children.push(child);
                    }
                    Err(err) => {
                        log(
                            format!("Could not run notification script: {}.", err.to_string()),
                            ERROR,
                        );
                        debug!("Could not run notification script: {:?}", err);
                    }
                }
            }

            if *kind == Some(NotificationType::NEWMAIL) {
                if let Some(ref path) = context.settings.notifications.xbiff_file_path {
                    if let Err(err) = update_xbiff(path) {
                        debug!("Could not update xbiff file: {:?}", &err);
                        melib::log(format!("Could not update xbiff file: {}.", err), ERROR);
                    }
                }
            }
        }
        false
    }
    fn id(&self) -> ComponentId {
        ComponentId::nil()
    }
    fn is_dirty(&self) -> bool {
        false
    }
    fn set_dirty(&mut self, _value: bool) {}
    fn set_id(&mut self, _id: ComponentId) {}
}

fn update_xbiff(path: &str) -> Result<()> {
    let mut file = std::fs::OpenOptions::new()
        .append(true) /* writes will append to a file instead of overwriting previous contents */
        .create(true) /* a new file will be created if the file does not yet already exist.*/
        .open(path)?;
    if file.metadata()?.len() > 128 {
        file.set_len(0)?;
    } else {
        std::io::Write::write_all(&mut file, b"z")?;
    }
    Ok(())
}
