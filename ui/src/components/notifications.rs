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
use notify_rust::Notification as notify_Notification;
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
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        if let UIEvent::Notification(ref title, ref body) = event {
            notify_Notification::new()
                .appname("meli")
                .icon("mail-message-new")
                .summary(title.as_ref().map(|v| v.as_str()).unwrap_or("Event"))
                .body(&escape_str(body))
                .icon("dialog-information")
                .show()
                .unwrap();
        }
        false
    }
    fn set_dirty(&mut self) {}

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
        if let UIEvent::Notification(ref title, ref body) = event {
            if let Some(ref bin) = context.runtime_settings.notifications.script {
                if let Err(v) = Command::new(bin)
                    .arg(title.as_ref().map(|v| v.as_str()).unwrap_or("Event"))
                    .arg(body)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    if cfg!(feature = "debug_log") {
                        eprintln!("{:?}", v);
                    }
                }
            }
        }
        false
    }
    fn id(&self) -> ComponentId {
        ComponentId::nil()
    }
    fn set_dirty(&mut self) {}
    fn set_id(&mut self, _id: ComponentId) {}
}
