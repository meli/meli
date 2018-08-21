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

pub struct HtmlView {
    pager: Pager,
    bytes: Vec<u8>,
}

impl HtmlView {
    pub fn new(bytes: Vec<u8>) -> Self {
        let mut html_filter = Command::new("w3m")
            .args(&["-I", "utf-8", "-T", "text/html"])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to start html filter process");
        html_filter
            .stdin
            .as_mut()
            .unwrap()
            .write_all(&bytes)
            .expect("Failed to write to w3m stdin");
        let mut display_text =
            String::from("Text piped through `w3m`. Press `v` to open in web browser. \n\n");
        display_text.push_str(&String::from_utf8_lossy(
            &html_filter.wait_with_output().unwrap().stdout,
        ));

        let buf = MailView::plain_text_to_buf(&display_text, true);
        let pager = Pager::from_buf(buf.split_newlines(), None);
        HtmlView { pager, bytes }
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
    fn process_event(&mut self, event: &UIEvent, context: &mut Context) {
        match event.event_type {
            UIEventType::Input(Key::Char('v')) => {
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
                    context.replies.push_back(UIEvent {
                        id: 0,
                        event_type: UIEventType::StatusNotification(format!(
                            "Couldn't find a default application for html files."
                        )),
                    });
                }
                return;
            }
            _ => {}
        }
        self.pager.process_event(event, context);
    }
    fn is_dirty(&self) -> bool {
        self.pager.is_dirty()
    }
    fn set_dirty(&mut self) {}
}
