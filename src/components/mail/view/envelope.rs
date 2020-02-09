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

use super::*;
use linkify::{Link, LinkFinder};
use std::process::{Command, Stdio};

use xdg_utils::query_default_app;

#[derive(PartialEq, Debug)]
enum ViewMode {
    Normal,
    Url,
    Attachment(usize),
    Raw,
    Subview,
}

impl ViewMode {
    fn is_attachment(&self) -> bool {
        match self {
            ViewMode::Attachment(_) => true,
            _ => false,
        }
    }
}

/// Contains an Envelope view, with sticky headers, a pager for the body, and subviews for more
/// menus
#[derive(Debug)]
pub struct EnvelopeView {
    pager: Option<Pager>,
    subview: Option<Box<dyn Component>>,
    dirty: bool,
    mode: ViewMode,
    wrapper: EnvelopeWrapper,

    account_pos: usize,
    cmd_buf: String,
    id: ComponentId,
}

impl fmt::Display for EnvelopeView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        write!(f, "view mail")
    }
}

impl EnvelopeView {
    pub fn new(
        wrapper: EnvelopeWrapper,
        pager: Option<Pager>,
        subview: Option<Box<dyn Component>>,
        account_pos: usize,
    ) -> Self {
        EnvelopeView {
            pager,
            subview,
            dirty: true,
            mode: ViewMode::Normal,
            wrapper,
            account_pos,
            cmd_buf: String::with_capacity(4),
            id: ComponentId::new_v4(),
        }
    }

    /// Returns the string to be displayed in the Viewer
    fn attachment_to_text<'closure, 's: 'closure, 'context: 's>(
        &'s self,
        body: &'context Attachment,
        context: &'context mut Context,
    ) -> String {
        let finder = LinkFinder::new();
        let body_text = String::from_utf8_lossy(&decode_rec(
            &body,
            Some(Box::new(|a: &Attachment, v: &mut Vec<u8>| {
                if a.content_type().is_text_html() {
                    use std::io::Write;
                    let settings = &context.settings;
                    if let Some(filter_invocation) = settings.pager.html_filter.as_ref() {
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
                                    filter_invocation,
                                )),
                                String::new(),
                                Some(NotificationType::ERROR),
                            ));
                            return;
                        }

                        let mut html_filter = command_obj.unwrap();
                        html_filter
                            .stdin
                            .as_mut()
                            .unwrap()
                            .write_all(&v)
                            .expect("Failed to write to stdin");
                        *v = format!(
                            "Text piped through `{}`. Press `v` to open in web browser. \n\n",
                            filter_invocation
                        )
                        .into_bytes();
                        v.extend(html_filter.wait_with_output().unwrap().stdout);
                    }
                }
            })),
        ))
        .into_owned();
        match self.mode {
            ViewMode::Normal | ViewMode::Subview => {
                let mut t = body_text.to_string();
                if body.count_attachments() > 1 {
                    t = body
                        .attachments()
                        .iter()
                        .enumerate()
                        .fold(t, |mut s, (idx, a)| {
                            s.push_str(&format!("[{}] {}\n\n", idx, a));
                            s
                        });
                }
                t
            }
            ViewMode::Raw => String::from_utf8_lossy(body.body()).into_owned(),
            ViewMode::Url => {
                let mut t = body_text.to_string();
                for (lidx, l) in finder.links(&body.text()).enumerate() {
                    let offset = if lidx < 10 {
                        lidx * 3
                    } else if lidx < 100 {
                        26 + (lidx - 9) * 4
                    } else if lidx < 1000 {
                        385 + (lidx - 99) * 5
                    } else {
                        panic!("BUG: Message body with more than 100 urls, fix this");
                    };
                    t.insert_str(l.start() + offset, &format!("[{}]", lidx));
                }
                if body.count_attachments() > 1 {
                    t = body
                        .attachments()
                        .iter()
                        .enumerate()
                        .fold(t, |mut s, (idx, a)| {
                            s.push_str(&format!("[{}] {}\n\n", idx, a));
                            s
                        });
                }
                t
            }
            ViewMode::Attachment(aidx) => {
                let attachments = body.attachments();
                let mut ret = "Viewing attachment. Press `r` to return \n".to_string();
                ret.push_str(&attachments[aidx].text());
                ret
            }
        }
    }
    /*
     * TODO: add recolor changes so that this function returns a vector of required highlights
     * to pass to write_string...
    pub fn plain_text_to_buf(s: &str, highlight_urls: bool) -> CellBuffer {
        let mut buf = CellBuffer::from(s);

        if highlight_urls {
            let lines: Vec<&str> = s.split('\n').map(|l| l.trim_end()).collect();
            let mut shift = 0;
            let mut lidx_total = 0;
            let finder = LinkFinder::new();
            for r in &lines {
                for l in finder.links(&r) {
                    let offset = if lidx_total < 10 {
                        3
                    } else if lidx_total < 100 {
                        4
                    } else if lidx_total < 1000 {
                        5
                    } else {
                        panic!("BUG: Message body with more than 100 urls");
                    };
                    for i in 1..=offset {
                        buf[(l.start() + shift - i, 0)].set_fg(Color::Byte(226));
                        //buf[(l.start() + shift - 2, 0)].set_fg(Color::Byte(226));
                        //buf[(l.start() + shift - 3, 0)].set_fg(Color::Byte(226));
                    }
                    lidx_total += 1;
                }
                // Each Cell represents one char so next line will be:
                shift += r.chars().count() + 1;
            }
        }
        buf
    }
    */
}

impl Component for EnvelopeView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let y: usize = {
            let envelope: &Envelope = &self.wrapper;

            if self.mode == ViewMode::Raw {
                clear_area(grid, area, crate::conf::value(context, "theme_default"));
                context.dirty_areas.push_back(area);
                get_y(upper_left) - 1
            } else {
                let (x, y) = write_string_to_grid(
                    &format!("Date: {}", envelope.date_as_str()),
                    grid,
                    Color::Byte(33),
                    Color::Default,
                    Attr::Default,
                    area,
                    Some(get_x(upper_left)),
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)].set_ch(' ');
                    grid[(x, y)].set_bg(Color::Default);
                    grid[(x, y)].set_fg(Color::Default);
                }
                let (x, y) = write_string_to_grid(
                    &format!("From: {}", envelope.field_from_to_string()),
                    grid,
                    Color::Byte(33),
                    Color::Default,
                    Attr::Default,
                    (set_y(upper_left, y + 1), bottom_right),
                    Some(get_x(upper_left)),
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)].set_ch(' ');
                    grid[(x, y)].set_bg(Color::Default);
                    grid[(x, y)].set_fg(Color::Default);
                }
                let (x, y) = write_string_to_grid(
                    &format!("To: {}", envelope.field_to_to_string()),
                    grid,
                    Color::Byte(33),
                    Color::Default,
                    Attr::Default,
                    (set_y(upper_left, y + 1), bottom_right),
                    Some(get_x(upper_left)),
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)].set_ch(' ');
                    grid[(x, y)].set_bg(Color::Default);
                    grid[(x, y)].set_fg(Color::Default);
                }
                let (x, y) = write_string_to_grid(
                    &format!("Subject: {}", envelope.subject()),
                    grid,
                    Color::Byte(33),
                    Color::Default,
                    Attr::Default,
                    (set_y(upper_left, y + 1), bottom_right),
                    Some(get_x(upper_left)),
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)].set_ch(' ');
                    grid[(x, y)].set_bg(Color::Default);
                    grid[(x, y)].set_fg(Color::Default);
                }
                let (x, y) = write_string_to_grid(
                    &format!("Message-ID: <{}>", envelope.message_id_raw()),
                    grid,
                    Color::Byte(33),
                    Color::Default,
                    Attr::Default,
                    (set_y(upper_left, y + 1), bottom_right),
                    Some(get_x(upper_left)),
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)].set_ch(' ');
                    grid[(x, y)].set_bg(Color::Default);
                    grid[(x, y)].set_fg(Color::Default);
                }
                clear_area(
                    grid,
                    (set_y(upper_left, y + 1), set_y(bottom_right, y + 2)),
                    crate::conf::value(context, "theme_default"),
                );
                context
                    .dirty_areas
                    .push_back((upper_left, set_y(bottom_right, y + 1)));
                y + 1
            }
        };

        if self.dirty {
            let body = self.wrapper.body_bytes(self.wrapper.buffer());
            match self.mode {
                ViewMode::Attachment(aidx) if body.attachments()[aidx].is_html() => {
                    let attachment = &body.attachments()[aidx];
                    self.subview = Some(Box::new(HtmlView::new(&attachment, context)));
                }
                ViewMode::Normal if body.is_html() => {
                    self.subview = Some(Box::new(HtmlView::new(&body, context)));
                    self.mode = ViewMode::Subview;
                }
                _ => {
                    let text = {
                        self.attachment_to_text(&body, context)
                        /*
                        let text = self.attachment_to_text(&body);
                        // URL indexes must be colored (ugh..)
                        EnvelopeView::plain_text_to_buf(&text, self.mode == ViewMode::Url)
                        */
                    };
                    let cursor_pos = if self.mode.is_attachment() {
                        Some(0)
                    } else {
                        self.pager.as_ref().map(Pager::cursor_pos)
                    };
                    let colors = crate::conf::value(context, "mail.view.body");
                    self.pager = Some(Pager::from_string(
                        text,
                        Some(context),
                        cursor_pos,
                        None,
                        colors,
                    ));
                }
            };
            self.dirty = false;
        }
        if let Some(s) = self.subview.as_mut() {
            s.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);
        } else if let Some(p) = self.pager.as_mut() {
            p.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut sub) = self.subview {
            if sub.process_event(event, context) {
                return true;
            }
        } else if let Some(ref mut p) = self.pager {
            if p.process_event(event, context) {
                return true;
            }
        }
        match *event {
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt('')) if !self.cmd_buf.is_empty() => {
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                return true;
            }
            UIEvent::Input(Key::Char(c)) if c >= '0' && c <= '9' => {
                self.cmd_buf.push(c);
                return true;
            }
            UIEvent::Input(Key::Char('r'))
                if self.mode == ViewMode::Normal || self.mode == ViewMode::Raw =>
            {
                self.mode = if self.mode == ViewMode::Raw {
                    ViewMode::Normal
                } else {
                    ViewMode::Raw
                };
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Char('r'))
                if self.mode.is_attachment() || self.mode == ViewMode::Subview =>
            {
                self.mode = ViewMode::Normal;
                self.subview.take();
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Char('a'))
                if !self.cmd_buf.is_empty() && self.mode == ViewMode::Normal =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));

                {
                    let envelope: &Envelope = self.wrapper.envelope();
                    if let Some(u) = envelope
                        .body_bytes(self.wrapper.buffer())
                        .attachments()
                        .get(lidx)
                    {
                        match u.content_type() {
                            ContentType::MessageRfc822 => {
                                self.mode = ViewMode::Subview;
                                let colors = crate::conf::value(context, "mail.view.body");
                                self.subview = Some(Box::new(Pager::from_string(
                                    String::from_utf8_lossy(&decode_rec(u, None)).to_string(),
                                    Some(context),
                                    None,
                                    None,
                                    colors,
                                )));
                            }

                            ContentType::Text { .. } => {
                                self.mode = ViewMode::Attachment(lidx);
                                self.dirty = true;
                            }
                            ContentType::Multipart { .. } => {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(
                                        "Multipart attachments are not supported yet.".to_string(),
                                    ),
                                ));
                                return true;
                            }
                            ContentType::Other { ref name, .. } => {
                                let attachment_type = u.mime_type();
                                let binary = query_default_app(&attachment_type);
                                if let Ok(binary) = binary {
                                    let p = create_temp_file(
                                        &decode(u, None),
                                        name.as_ref().map(String::as_str),
                                        None,
                                        true,
                                    );
                                    Command::new(&binary)
                                        .arg(p.path())
                                        .stdin(Stdio::piped())
                                        .stdout(Stdio::piped())
                                        .spawn()
                                        .unwrap_or_else(|_| {
                                            panic!("Failed to start {}", binary.display())
                                        });
                                    context.temp_files.push(p);
                                } else {
                                    context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(format!(
                                            "Couldn't find a default application for type {}",
                                            attachment_type
                                        )),
                                    ));
                                    return true;
                                }
                            }
                            ContentType::OctetStream { .. } => {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(
                                        "application/octet-stream isn't supported yet".to_string(),
                                    ),
                                ));
                                return true;
                            }
                            ContentType::PGPSignature => {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(
                                        "Signatures aren't supported yet".to_string(),
                                    ),
                                ));
                                return true;
                            }
                        }
                    } else {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Attachment `{}` not found.",
                                lidx
                            )),
                        ));
                        return true;
                    }
                };
                return true;
            }
            UIEvent::Input(Key::Char('g'))
                if !self.cmd_buf.is_empty() && self.mode == ViewMode::Url =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                let url = {
                    let envelope: &Envelope = self.wrapper.envelope();
                    let finder = LinkFinder::new();
                    let t = envelope
                        .body_bytes(self.wrapper.buffer())
                        .text()
                        .to_string();
                    let links: Vec<Link> = finder.links(&t).collect();
                    if let Some(u) = links.get(lidx) {
                        u.as_str().to_string()
                    } else {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!("Link `{}` not found.", lidx)),
                        ));
                        return true;
                    }
                };

                Command::new("xdg-open")
                    .arg(url)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                    .expect("Failed to start xdg_open");
                return true;
            }
            UIEvent::Input(Key::Char('u')) => {
                match self.mode {
                    ViewMode::Normal => self.mode = ViewMode::Url,
                    ViewMode::Url => self.mode = ViewMode::Normal,
                    _ => {}
                }
                self.dirty = true;
                return true;
            }
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty
            || self.pager.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
            || self.subview.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
    }
    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn kill(&mut self, id: ComponentId, context: &mut Context) {
        debug_assert!(self.id == id);
        context
            .replies
            .push_back(UIEvent::Action(Tab(Kill(self.id))));
    }

    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
