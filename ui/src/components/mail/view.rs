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
use linkify::{Link, LinkFinder};
use std::process::{Command, Stdio};

mod html;
pub use self::html::*;
mod thread;
pub use self::thread::*;

mod envelope;
pub use self::envelope::*;

use mime_apps::query_default_app;

#[derive(PartialEq, Debug)]
enum ViewMode {
    Normal,
    Url,
    Attachment(usize),
    Raw,
    Subview,
    ContactSelector(Selector),
}

impl Default for ViewMode {
    fn default() -> Self {
        ViewMode::Normal
    }
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
#[derive(Debug, Default)]
pub struct MailView {
    coordinates: (usize, usize, EnvelopeHash),
    pager: Option<Pager>,
    subview: Option<Box<Component>>,
    dirty: bool,
    mode: ViewMode,

    cmd_buf: String,
    id: ComponentId,
}

impl fmt::Display for MailView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        write!(f, "{}", MailView::DESCRIPTION)
    }
}

impl MailView {
    const DESCRIPTION: &'static str = "mail";
    pub fn new(
        coordinates: (usize, usize, EnvelopeHash),
        pager: Option<Pager>,
        subview: Option<Box<Component>>,
        context: &mut Context,
    ) -> Self {
        let account = &mut context.accounts[coordinates.0];
        let (hash, is_seen) = {
            let mailbox = &mut account[coordinates.1].as_mut().unwrap();
            let envelope: &mut Envelope = &mut mailbox.collection.entry(coordinates.2).or_default();
            (envelope.hash(), envelope.is_seen())
        };
        if !is_seen {
            let folder_hash = {
                let mailbox = &mut account[coordinates.1].as_mut().unwrap();
                mailbox.folder.hash()
            };
            let op = {
                let backend = &account.backend;
                backend.operation(hash, folder_hash)
            };
            let mailbox = &mut account[coordinates.1].as_mut().unwrap();
            let envelope: &mut Envelope = &mut mailbox.collection.entry(coordinates.2).or_default();
            envelope.set_seen(op).unwrap();
        }
        MailView {
            coordinates,
            pager,
            subview,
            dirty: true,
            mode: ViewMode::Normal,

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
            body,
            Some(Box::new(move |a: &'closure Attachment, v: &mut Vec<u8>| {
                if a.content_type().is_text_html() {
                    use std::io::Write;
                    use std::process::{Command, Stdio};
                    let settings = context.accounts[self.coordinates.0].runtime_settings.conf();
                    /* FIXME: duplication with view/html.rs */
                    if let Some(filter_invocation) = settings.html_filter() {
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
                                .write_all(&v)
                                .expect("Failed to write to html filter stdin");
                            *v = String::from(
                                "Text piped through `w3m`. Press `v` to open in web browser. \n\n",
                            )
                            .into_bytes();
                            v.extend(html_filter.wait_with_output().unwrap().stdout);
                        } else {
                            context.replies.push_back(UIEvent::Notification(
                                Some(format!(
                                    "Failed to find any application to use as html filter"
                                )),
                                String::new(),
                            ));
                            return;
                        }
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
            ViewMode::Raw => String::from_utf8_lossy(body.bytes()).into_owned(),
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
                        panic!("FIXME: Message body with more than 100 urls, fix this");
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
            ViewMode::ContactSelector(_) => unimplemented!(),
        }
    }
    pub fn plain_text_to_buf(s: &str, highlight_urls: bool) -> CellBuffer {
        let mut buf = CellBuffer::from(s);

        if highlight_urls {
            let lines: Vec<&str> = s.split('\n').map(|l| l.trim_right()).collect();
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
}

impl Component for MailView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let y: usize = {
            let accounts = &mut context.accounts;
            let mailbox = &mut accounts[self.coordinates.0][self.coordinates.1]
                .as_ref()
                .unwrap();
            if !mailbox.collection.contains_key(&self.coordinates.2) {
                /* The envelope has been renamed or removed, so wait for the appropriate event to
                 * arrive */
                return;
            }
            let envelope: &Envelope = &mailbox.collection[&self.coordinates.2];

            if self.mode == ViewMode::Raw {
                clear_area(grid, area);
                context.dirty_areas.push_back(area);
                get_y(upper_left) - 1
            } else {
                let (x, y) = write_string_to_grid(
                    &format!("Date: {}", envelope.date_as_str()),
                    grid,
                    Color::Byte(33),
                    Color::Default,
                    area,
                    true,
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
                    (set_y(upper_left, y + 1), bottom_right),
                    true,
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
                    (set_y(upper_left, y + 1), bottom_right),
                    true,
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
                    (set_y(upper_left, y + 1), bottom_right),
                    true,
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
                    (set_y(upper_left, y + 1), bottom_right),
                    true,
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)].set_ch(' ');
                    grid[(x, y)].set_bg(Color::Default);
                    grid[(x, y)].set_fg(Color::Default);
                }
                clear_area(grid, (set_y(upper_left, y + 1), set_y(bottom_right, y + 1)));
                context
                    .dirty_areas
                    .push_back((upper_left, set_y(bottom_right, y + 1)));
                y + 1
            }
        };

        if self.dirty {
            let body = {
                let mailbox_idx = self.coordinates; // coordinates are mailbox idxs
                let mailbox = &context.accounts[mailbox_idx.0][mailbox_idx.1]
                    .as_ref()
                    .unwrap();
                let envelope: &Envelope = &mailbox.collection[&mailbox_idx.2];
                let op = context.accounts[mailbox_idx.0]
                    .backend
                    .operation(envelope.hash(), mailbox.folder.hash());
                envelope.body(op)
            };
            match self.mode {
                ViewMode::Attachment(aidx) if body.attachments()[aidx].is_html() => {
                    self.pager = None;
                    let attachment = &body.attachments()[aidx];
                    self.subview = Some(Box::new(HtmlView::new(
                        &attachment,
                        context,
                        self.coordinates.0,
                    )));
                    self.mode = ViewMode::Subview;
                }
                ViewMode::Normal if body.is_html() => {
                    self.subview =
                        Some(Box::new(HtmlView::new(&body, context, self.coordinates.0)));
                    self.pager = None;
                    self.mode = ViewMode::Subview;
                }
                ViewMode::Subview | ViewMode::ContactSelector(_) => {}
                ViewMode::Raw => {
                    let text = {
                        let mailbox_idx = self.coordinates; // coordinates are mailbox idxs
                        let mailbox = &context.accounts[mailbox_idx.0][mailbox_idx.1]
                            .as_ref()
                            .unwrap();
                        let envelope: &Envelope = &mailbox.collection[&mailbox_idx.2];
                        let mut op = context.accounts[mailbox_idx.0]
                            .backend
                            .operation(envelope.hash(), mailbox.folder.hash());
                        op.as_bytes()
                            .map(|v| String::from_utf8_lossy(v).into_owned())
                            .unwrap_or_else(|e| e.to_string())
                    };
                    self.pager = Some(Pager::from_string(
                        text,
                        Some(context),
                        None,
                        Some(width!(area)),
                    ));
                    self.subview = None;
                }
                _ => {
                    let text = {
                        self.attachment_to_text(&body, context)
                        /*
                        // URL indexes must be colored (ugh..)
                        MailView::plain_text_to_buf(&text, self.mode == ViewMode::Url)
                        */
                    };
                    let cursor_pos = if self.mode.is_attachment() {
                        Some(0)
                    } else {
                        self.pager.as_mut().map(|p| p.cursor_pos())
                    };
                    self.pager = Some(Pager::from_string(
                        text,
                        Some(context),
                        cursor_pos,
                        Some(width!(area)),
                    ));
                    self.subview = None;
                }
            };
            self.dirty = false;
        }
        match self.mode {
            ViewMode::Subview if self.subview.is_some() => {
                if let Some(s) = self.subview.as_mut() {
                    s.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);
                }
            }
            ViewMode::ContactSelector(ref mut s) => {
                clear_area(grid, (set_y(upper_left, y + 1), bottom_right));
                s.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);
            }
            _ => {
                if let Some(p) = self.pager.as_mut() {
                    p.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);
                }
            }
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match self.mode {
            ViewMode::Subview => {
                if let Some(s) = self.subview.as_mut() {
                    if s.process_event(event, context) {
                        return true;
                    }
                }
            }
            ViewMode::ContactSelector(ref mut s) => {
                if s.process_event(event, context) {
                    return true;
                }
            }
            _ => {
                if let Some(p) = self.pager.as_mut() {
                    if p.process_event(event, context) {
                        return true;
                    }
                }
            }
        }

        match *event {
            UIEvent::Input(Key::Char('c')) => {
                if let ViewMode::ContactSelector(_) = self.mode {
                    if let ViewMode::ContactSelector(s) =
                        std::mem::replace(&mut self.mode, ViewMode::Normal)
                    {
                        let account = &mut context.accounts[self.coordinates.0];
                        let mut results = Vec::new();
                        {
                            let mailbox = &account[self.coordinates.1].as_ref().unwrap();
                            let envelope: &Envelope = &mailbox.collection[&self.coordinates.2];
                            for c in s.collect() {
                                let c = usize::from_ne_bytes({
                                    [c[0], c[1], c[2], c[3], c[4], c[5], c[6], c[7]]
                                });
                                for (idx, env) in envelope
                                    .from()
                                    .iter()
                                    .chain(envelope.to().iter())
                                    .enumerate()
                                {
                                    if idx != c {
                                        continue;
                                    }

                                    let mut new_card: Card = Card::new();
                                    new_card.set_email(env.get_email());
                                    new_card.set_lastname(env.get_display_name());
                                    results.push(new_card);
                                }
                            }
                        }
                        for c in results {
                            account.address_book.add_card(c);
                        }
                    }
                    return true;
                }
                let accounts = &context.accounts;
                let mailbox = &accounts[self.coordinates.0][self.coordinates.1]
                    .as_ref()
                    .unwrap();
                let envelope: &Envelope = &mailbox.collection[&self.coordinates.2];

                let mut entries = Vec::new();
                for (idx, env) in envelope
                    .from()
                    .iter()
                    .chain(envelope.to().iter())
                    .enumerate()
                {
                    entries.push((idx.to_ne_bytes().to_vec(), format!("{}", env)));
                }
                self.mode = ViewMode::ContactSelector(Selector::new(entries, true));
                self.dirty = true;
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt('')) => {
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
            }
            UIEvent::Input(Key::Char(c)) if c >= '0' && c <= '9' => {
                self.cmd_buf.push(c);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufSet(
                        self.cmd_buf.clone(),
                    )));
            }
            UIEvent::Input(Key::Alt('r'))
                if self.mode == ViewMode::Normal || self.mode == ViewMode::Subview =>
            {
                self.mode = ViewMode::Raw;
                self.set_dirty();
            }
            UIEvent::Input(Key::Char('r'))
                if self.mode.is_attachment()
                    || self.mode == ViewMode::Subview
                    || self.mode == ViewMode::Url
                    || self.mode == ViewMode::Raw =>
            {
                self.mode = ViewMode::Normal;
                self.set_dirty();
            }
            UIEvent::Input(Key::Char('a'))
                if !self.cmd_buf.is_empty()
                    && (self.mode == ViewMode::Normal || self.mode == ViewMode::Subview) =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));

                {
                    let accounts = &context.accounts;
                    let mailbox = &accounts[self.coordinates.0][self.coordinates.1]
                        .as_ref()
                        .unwrap();

                    let envelope: &Envelope = &mailbox.collection[&self.coordinates.2];
                    let op = context.accounts[self.coordinates.0]
                        .backend
                        .operation(envelope.hash(), mailbox.folder.hash());
                    if let Some(u) = envelope.body(op).attachments().get(lidx) {
                        match u.content_type() {
                            ContentType::MessageRfc822 => {
                                self.mode = ViewMode::Subview;
                                match EnvelopeWrapper::new(u.bytes().to_vec()) {
                                    Ok(wrapper) => {
                                        self.subview = Some(Box::new(EnvelopeView::new(
                                            wrapper,
                                            None,
                                            None,
                                            self.coordinates.0,
                                        )));
                                    }
                                    Err(e) => {
                                        context.replies.push_back(UIEvent::StatusEvent(
                                            StatusEvent::DisplayMessage(format!("{}", e)),
                                        ));
                                    }
                                }
                                return true;
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
                            ContentType::Unsupported { .. } => {
                                let attachment_type = u.mime_type();
                                let binary = query_default_app(&attachment_type);
                                if let Ok(binary) = binary {
                                    let mut p = create_temp_file(&decode(u, None), None);
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
                    let accounts = &context.accounts;
                    let mailbox = &accounts[self.coordinates.0][self.coordinates.1]
                        .as_ref()
                        .unwrap();

                    let envelope: &Envelope = &mailbox.collection[&self.coordinates.2];
                    let finder = LinkFinder::new();
                    let op = context.accounts[self.coordinates.0]
                        .backend
                        .operation(envelope.hash(), mailbox.folder.hash());
                    let mut t = envelope.body(op).text().to_string();
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
            }
            UIEvent::Input(Key::Char('u')) => {
                match self.mode {
                    ViewMode::Normal => self.mode = ViewMode::Url,
                    ViewMode::Url => self.mode = ViewMode::Normal,
                    _ => {}
                }
                self.dirty = true;
            }
            UIEvent::EnvelopeRename(_, old_hash, new_hash) if old_hash == self.coordinates.2 => {
                self.coordinates.2 = new_hash;
            }
            _ => {
                return false;
            }
        }
        true
    }
    fn is_dirty(&self) -> bool {
        self.dirty
            || self.pager.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
            || self.subview.as_ref().map(|p| p.is_dirty()).unwrap_or(false)
            || if let ViewMode::ContactSelector(ref s) = self.mode {
                s.is_dirty()
            } else {
                false
            }
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
        match self.mode {
            ViewMode::Normal => {
                if let Some(p) = self.pager.as_mut() {
                    p.set_dirty();
                }
            }
            ViewMode::Subview => {
                if let Some(s) = self.subview.as_mut() {
                    s.set_dirty();
                }
            }
            _ => {}
        }
    }
    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if let Some(ref sbv) = self.subview {
            sbv.get_shortcuts(context)
        } else if let Some(ref pgr) = self.pager {
            pgr.get_shortcuts(context)
        } else {
            Default::default()
        };

        let mut our_map = FnvHashMap::with_capacity_and_hasher(4, Default::default());
        our_map.insert("add_addresses_to_contacts", Key::Char('c'));
        our_map.insert("view_raw_source", Key::Alt('r'));
        if self.mode.is_attachment() || self.mode == ViewMode::Subview || self.mode == ViewMode::Raw
        {
            our_map.insert("return_to_normal_view", Key::Char('r'));
        }
        our_map.insert("open_attachment", Key::Char('a'));
        if self.mode == ViewMode::Url {
            our_map.insert("go_to_url", Key::Char('g'));
        }
        if self.mode == ViewMode::Normal || self.mode == ViewMode::Url {
            our_map.insert("toggle_url_mode", Key::Char('u'));
        }
        map.insert(MailView::DESCRIPTION.to_string(), our_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
