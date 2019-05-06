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

use super::*;
use std::io::Write;
use std::process::{Command, Stdio};

#[derive(Debug)]
pub struct HtmlView {
    pager: Pager,
    bytes: Vec<u8>,
    id: ComponentId,
}

impl HtmlView {
    pub fn new(body: &Attachment, context: &mut Context, account_pos: usize) -> Self {
        let id = ComponentId::new_v4();
        let bytes: Vec<u8> = decode_rec(body, None);

        let settings = context.accounts[account_pos].runtime_settings.conf();
        let mut display_text = if let Some(filter_invocation) = settings.html_filter() {
            let parts = split_command!(filter_invocation);
            let (cmd, args) = (parts[0], &parts[1..]);
            let command_obj = Command::new(cmd)
                .args(args)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn();
            if command_obj.is_err() {
                context.replies.push_back(UIEvent::Notification(
                    Some(format!(
                        "Failed to start html filter process: {}",
                        filter_invocation
                    )),
                    String::new(),
                ));
                String::from_utf8_lossy(&bytes).to_string()
            } else {
                let mut html_filter = command_obj.unwrap();
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
        } else {
            if let Ok(mut html_filter) = Command::new("w3m")
                .args(&["-I", "utf-8", "-T", "text/html"])
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
                let mut display_text = String::from(
                    "Text piped through `w3m`. Press `v` to open in web browser. \n\n",
                );
                display_text.push_str(&String::from_utf8_lossy(
                    &html_filter.wait_with_output().unwrap().stdout,
                ));

                display_text
            } else {
                context.replies.push_back(UIEvent::Notification(
                    Some(format!(
                        "Failed to find any application to use as html filter"
                    )),
                    String::new(),
                ));
                String::from_utf8_lossy(&bytes).to_string()
            }
        };
        if body.count_attachments() > 1 {
            display_text =
                body.attachments()
                    .iter()
                    .enumerate()
                    .fold(display_text, |mut s, (idx, a)| {
                        s.push_str(&format!("[{}] {}\n\n\n", idx, a));
                        s
                    });
        }
        let pager = Pager::from_string(display_text, None, None, None);
        HtmlView { pager, bytes, id }
    }
}

impl fmt::Display for HtmlView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
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
            // TODO: Optional filter that removes outgoing resource requests (images and
            // scripts)
            let binary = query_default_app("text/html");
            if let Ok(binary) = binary {
                let mut p = create_temp_file(&self.bytes, None);
                Command::new(&binary)
                    .arg(p.path())
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                    .unwrap_or_else(|_| panic!("Failed to start {}", binary.display()));
                context.temp_files.push(p);
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
    fn is_dirty(&self) -> bool {
        self.pager.is_dirty()
    }
    fn set_dirty(&mut self) {
        self.pager.set_dirty();
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
