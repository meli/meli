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

use std::{
    io::Write,
    process::{Command, Stdio},
};

use super::*;

#[derive(Debug)]
pub struct HtmlView {
    pager: Pager,
    bytes: Vec<u8>,
    coordinates: Option<(AccountHash, MailboxHash, EnvelopeHash)>,
    id: ComponentId,
}

impl HtmlView {
    pub fn new(body: &Attachment, context: &mut Context) -> Self {
        let id = ComponentId::new_v4();
        let bytes: Vec<u8> = body.decode_rec(Default::default());

        let settings = &context.settings;
        let mut display_text = if let Some(filter_invocation) = settings.pager.html_filter.as_ref()
        {
            let command_obj = Command::new("sh")
                .args(["-c", filter_invocation])
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn();
            match command_obj {
                Err(err) => {
                    context.replies.push_back(UIEvent::Notification(
                        Some(format!(
                            "Failed to start html filter process: {}",
                            filter_invocation,
                        )),
                        err.to_string(),
                        Some(NotificationType::Error(melib::ErrorKind::External)),
                    ));
                    String::from_utf8_lossy(&bytes).to_string()
                }
                Ok(mut html_filter) => {
                    html_filter
                        .stdin
                        .as_mut()
                        .unwrap()
                        .write_all(&bytes)
                        .expect("Failed to write to html filter stdin");
                    let mut display_text = format!(
                        "Text piped through `{}`. Press `v` to open in web browser. \n\n",
                        filter_invocation
                    );
                    display_text.push_str(&String::from_utf8_lossy(
                        &html_filter.wait_with_output().unwrap().stdout,
                    ));
                    display_text
                }
            }
        } else if let Ok(mut html_filter) = Command::new("w3m")
            .args(["-I", "utf-8", "-T", "text/html"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
        {
            html_filter
                .stdin
                .as_mut()
                .unwrap()
                .write_all(&bytes)
                .expect("Failed to write to html filter stdin");
            let mut display_text =
                String::from("Text piped through `w3m`. Press `v` to open in web browser. \n\n");
            display_text.push_str(&String::from_utf8_lossy(
                &html_filter.wait_with_output().unwrap().stdout,
            ));

            display_text
        } else {
            context.replies.push_back(UIEvent::Notification(
                Some("Failed to find any application to use as html filter".to_string()),
                String::new(),
                Some(NotificationType::Error(melib::error::ErrorKind::None)),
            ));
            String::from_utf8_lossy(&bytes).to_string()
        };
        if body.count_attachments() > 1 {
            display_text =
                body.attachments()
                    .iter()
                    .enumerate()
                    .fold(display_text, |mut s, (idx, a)| {
                        let _ = writeln!(s, "[{}] {}\n\n", idx, a);
                        s
                    });
        }
        let colors = crate::conf::value(context, "mail.view.body");
        let pager = Pager::from_string(display_text, None, None, None, colors);
        HtmlView {
            pager,
            bytes,
            id,
            coordinates: None,
        }
    }

    pub fn set_coordinates(&mut self, new_value: Option<(AccountHash, MailboxHash, EnvelopeHash)>) {
        self.coordinates = new_value;
    }
}

impl fmt::Display for HtmlView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "view")
    }
}

impl Component for HtmlView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        self.pager.draw(grid, area, context);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.pager.process_event(event, context) {
            return true;
        }

        if let UIEvent::Input(Key::Char('v')) = event {
            let command = if let Some(coordinates) = self.coordinates {
                mailbox_settings!(context[coordinates.0][&coordinates.1].pager.html_open)
                    .as_ref()
                    .map(|s| s.to_string())
                    .or_else(|| query_default_app("text/html").ok())
            } else {
                query_default_app("text/html").ok()
            };
            let command = if cfg!(target_os = "macos") {
                command.or_else(|| Some("open".into()))
            } else if cfg!(target_os = "linux") {
                command.or_else(|| Some("xdg-open".into()))
            } else {
                command
            };
            if let Some(command) = command {
                let p = create_temp_file(&self.bytes, None, None, Some("html"), true);
                let exec_cmd =
                    super::desktop_exec_to_command(&command, p.path.display().to_string(), false);
                match Command::new("sh")
                    .args(["-c", &exec_cmd])
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    Ok(child) => {
                        context.temp_files.push(p);
                        context.children.push(child);
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Failed to start `{}`: {}",
                                &exec_cmd, err
                            )),
                        ));
                    }
                }
            } else {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                        "Couldn't find a default application for html files.".to_string(),
                    )));
            }
            return true;
        }
        false
    }
    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        self.pager.get_shortcuts(context)
    }
    fn is_dirty(&self) -> bool {
        self.pager.is_dirty()
    }
    fn set_dirty(&mut self, value: bool) {
        self.pager.set_dirty(value);
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
