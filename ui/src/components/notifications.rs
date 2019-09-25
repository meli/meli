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

/*!
Notification handling components.
*/
use notify_rust;
use std::process::{Command, Stdio};

use super::*;

/// Passes notifications to the OS using the XDG specifications.
#[derive(Debug)]
pub struct XDGNotifications {}

impl fmt::Display for XDGNotifications {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        write!(f, "")
    }
}

impl Component for XDGNotifications {
    fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::Notification(ref title, ref body, ref kind) = event {
            let settings = &context.runtime_settings.notifications;
            let mut notification = notify_rust::Notification::new();
            notification
                .appname("meli")
                .icon("mail-message-new")
                .summary(title.as_ref().map(String::as_str).unwrap_or("meli"))
                .body(&escape_str(body))
                .icon("dialog-information");
            match kind {
                Some(NotificationType::NewMail) => {
                    notification.hint(notify_rust::hints::NotificationHint::Category(
                        "email".to_owned(),
                    ));
                }
                _ => {}
            }
            if settings.play_sound.is_true() {
                if let Some(ref sound_path) = settings.sound_file {
                    notification.hint(notify_rust::hints::NotificationHint::SoundFile(
                        sound_path.to_owned(),
                    ));
                } else {
                    notification.sound_name("message-new-email");
                }
            }

            notification.show().unwrap();
        }
        false
    }
    fn set_dirty(&mut self) {}
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_escape_str() {
        let title: &str = "& > Title τίτλος";
        let body: &str = "& > Body σώμα";
        notify_Notification::new()
            .appname("meli")
            .icon("mail-message-new")
            .summary(title)
            .body(&escape_str(body))
            .icon("dialog-information")
            .show()
            .unwrap();
    }
}

/// Passes notifications to the OS using the XDG specifications.
#[derive(Debug)]
pub struct NotificationFilter {}

impl fmt::Display for NotificationFilter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        write!(f, "")
    }
}

impl Component for NotificationFilter {
    fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::Notification(ref title, ref body, ref kind) = event {
            if let Some(ref bin) = context.runtime_settings.notifications.script {
                if let Err(v) = Command::new(bin)
                    .arg(title.as_ref().map(String::as_str).unwrap_or("Event"))
                    .arg(body)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    debug!("{:?}", v);
                }
            }

            match kind {
                Some(NotificationType::NewMail) => {
                    if let Some(ref path) = context.runtime_settings.notifications.xbiff_file_path {
                        let mut file = std::fs::OpenOptions::new().append(true) /* writes will append to a file instead of overwriting previous contents */
                         .create(true) /* a new file will be created if the file does not yet already exist.*/
                         .open(path).unwrap();
                        if file.metadata().unwrap().len() > 128 {
                            file.set_len(0).unwrap();
                        } else {
                            std::io::Write::write_all(&mut file, b"z").unwrap();
                        }
                    }
                }
                _ => {}
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
    fn set_dirty(&mut self) {}
    fn set_id(&mut self, _id: ComponentId) {}
}
