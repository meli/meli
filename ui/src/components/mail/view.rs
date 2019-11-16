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

use std::convert::TryFrom;
use std::process::{Command, Stdio};

pub mod list_management;

mod html;
pub use self::html::*;
mod thread;
pub use self::thread::*;

mod envelope;
pub use self::envelope::*;

use mime_apps::query_default_app;

#[derive(PartialEq, Debug, Clone)]
enum ViewMode {
    Normal,
    Url,
    Attachment(usize),
    Raw,
    Subview,
    ContactSelector(Selector<Card>),
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
    fn is_contact_selector(&self) -> bool {
        match self {
            ViewMode::ContactSelector(_) => true,
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
    subview: Option<Box<dyn Component>>,
    dirty: bool,
    mode: ViewMode,
    expand_headers: bool,

    cmd_buf: String,
    id: ComponentId,
}

impl Clone for MailView {
    fn clone(&self) -> Self {
        MailView {
            subview: None,
            cmd_buf: String::with_capacity(4),
            pager: self.pager.clone(),
            mode: self.mode.clone(),
            ..*self
        }
    }
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
        subview: Option<Box<dyn Component>>,
    ) -> Self {
        MailView {
            coordinates,
            pager,
            subview,
            dirty: true,
            mode: ViewMode::Normal,
            expand_headers: false,

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
                    let settings = &context.settings;
                    /* FIXME: duplication with view/html.rs */
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
                                Some(
                                    "Failed to find any application to use as html filter"
                                        .to_string(),
                                ),
                                String::new(),
                                Some(NotificationType::ERROR),
                            ));
                            return;
                        }
                    }
                } else if a.is_signed() {
                    v.clear();
                    if context.settings.pgp.auto_verify_signatures {
                        v.extend(crate::mail::pgp::verify_signature(a, context).into_iter());
                    }
                }
            })),
        ))
        .into_owned();
        match self.mode {
            ViewMode::Normal | ViewMode::Subview | ViewMode::ContactSelector(_) => {
                let mut t = body_text.to_string();
                t.push('\n');
                if body.count_attachments() > 1 {
                    t = body
                        .attachments()
                        .iter()
                        .enumerate()
                        .fold(t, |mut s, (idx, a)| {
                            s.push_str(&format!("\n[{}] {}\n", idx, a));
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
        }
    }
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

    pub fn update(&mut self, new_coordinates: (usize, usize, EnvelopeHash)) {
        self.coordinates = new_coordinates;
        self.mode = ViewMode::Normal;
        self.set_dirty();
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
            let account = &mut context.accounts[self.coordinates.0];
            if !account.contains_key(self.coordinates.2) {
                /* The envelope has been renamed or removed, so wait for the appropriate event to
                 * arrive */
                self.dirty = false;
                return;
            }
            let (hash, is_seen) = {
                let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                (envelope.hash(), envelope.is_seen())
            };
            if !is_seen {
                let op = account.operation(hash);
                let mut envelope: EnvelopeRefMut =
                    account.collection.get_env_mut(self.coordinates.2);
                if let Err(e) = envelope.set_seen(op) {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(format!(
                            "Could not set message as seen: {}",
                            e
                        ))));
                }
            }
            let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);

            let header_fg = if context.settings.terminal.theme == "light" {
                Color::Black
            } else {
                Color::Byte(33)
            };

            if self.mode == ViewMode::Raw {
                clear_area(grid, area);
                context.dirty_areas.push_back(area);
                get_y(upper_left) - 1
            } else {
                let (x, y) = write_string_to_grid(
                    &format!("Date: {}", envelope.date_as_str()),
                    grid,
                    header_fg,
                    Color::Default,
                    Attr::Default,
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
                    header_fg,
                    Color::Default,
                    Attr::Default,
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
                    header_fg,
                    Color::Default,
                    Attr::Default,
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
                    header_fg,
                    Color::Default,
                    Attr::Default,
                    (set_y(upper_left, y + 1), bottom_right),
                    true,
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)].set_ch(' ');
                    grid[(x, y)].set_bg(Color::Default);
                    grid[(x, y)].set_fg(Color::Default);
                }
                let (x, mut y) = write_string_to_grid(
                    &format!("Message-ID: <{}>", envelope.message_id_raw()),
                    grid,
                    header_fg,
                    Color::Default,
                    Attr::Default,
                    (set_y(upper_left, y + 1), bottom_right),
                    true,
                );
                for x in x..=get_x(bottom_right) {
                    grid[(x, y)].set_ch(' ');
                    grid[(x, y)].set_bg(Color::Default);
                    grid[(x, y)].set_fg(Color::Default);
                }
                if self.expand_headers && envelope.in_reply_to().is_some() {
                    let (x, _y) = write_string_to_grid(
                        &format!("In-Reply-To: {}", envelope.in_reply_to_display().unwrap()),
                        grid,
                        header_fg,
                        Color::Default,
                        Attr::Default,
                        (set_y(upper_left, y + 1), bottom_right),
                        true,
                    );
                    for x in x..=get_x(bottom_right) {
                        grid[(x, _y)].set_ch(' ');
                        grid[(x, _y)].set_bg(Color::Default);
                        grid[(x, _y)].set_fg(Color::Default);
                    }
                    let (x, _y) = write_string_to_grid(
                        &format!(
                            "References: {}",
                            envelope
                                .references()
                                .iter()
                                .map(std::string::ToString::to_string)
                                .collect::<Vec<String>>()
                                .join(", ")
                        ),
                        grid,
                        header_fg,
                        Color::Default,
                        Attr::Default,
                        (set_y(upper_left, _y + 1), bottom_right),
                        true,
                    );
                    for x in x..=get_x(bottom_right) {
                        grid[(x, _y)].set_ch(' ');
                        grid[(x, _y)].set_bg(Color::Default);
                        grid[(x, _y)].set_fg(Color::Default);
                    }
                    y = _y;
                }
                if let Some(list_management::ListActions {
                    ref id,
                    ref archive,
                    ref post,
                    ref unsubscribe,
                }) = list_management::detect(&envelope)
                {
                    let mut x = get_x(upper_left);
                    y += 1;
                    if let Some(id) = id {
                        let (_x, _) = write_string_to_grid(
                            "List-ID: ",
                            grid,
                            header_fg,
                            Color::Default,
                            Attr::Default,
                            (set_y(upper_left, y), bottom_right),
                            false,
                        );
                        let (_x, _y) = write_string_to_grid(
                            id,
                            grid,
                            Color::Default,
                            Color::Default,
                            Attr::Default,
                            ((_x, y), bottom_right),
                            false,
                        );
                        x = _x;
                        if _y != y {
                            x = get_x(upper_left);
                        }
                        y = _y;
                    }
                    if archive.is_some() || post.is_some() || unsubscribe.is_some() {
                        let (_x, _y) = write_string_to_grid(
                            " Available actions: [ ",
                            grid,
                            header_fg,
                            Color::Default,
                            Attr::Default,
                            ((x, y), bottom_right),
                            true,
                        );
                        x = _x;
                        if _y != y {
                            x = get_x(upper_left);
                        }

                        y = _y;
                    }
                    if archive.is_some() {
                        let (_x, _y) = write_string_to_grid(
                            "list-archive, ",
                            grid,
                            Color::Default,
                            Color::Default,
                            Attr::Default,
                            ((x, y), bottom_right),
                            true,
                        );
                        x = _x;
                        if _y != y {
                            x = get_x(upper_left);
                        }
                        y = _y;
                    }
                    if post.is_some() {
                        let (_x, _y) = write_string_to_grid(
                            "list-post, ",
                            grid,
                            Color::Default,
                            Color::Default,
                            Attr::Default,
                            ((x, y), bottom_right),
                            true,
                        );
                        x = _x;
                        if _y != y {
                            x = get_x(upper_left);
                        }
                        y = _y;
                    }
                    if unsubscribe.is_some() {
                        let (_x, _y) = write_string_to_grid(
                            "list-unsubscribe, ",
                            grid,
                            Color::Default,
                            Color::Default,
                            Attr::Default,
                            ((x, y), bottom_right),
                            true,
                        );
                        x = _x;
                        if _y != y {
                            x = get_x(upper_left);
                        }
                        y = _y;
                    }
                    if archive.is_some() || post.is_some() || unsubscribe.is_some() {
                        if x >= 2 {
                            grid[(x - 2, y)].set_ch(' ');
                        }
                        if x > 0 {
                            grid[(x - 1, y)].set_fg(header_fg);
                            grid[(x - 1, y)].set_bg(Color::Default);
                            grid[(x - 1, y)].set_ch(']');
                        }
                    }
                    for x in x..=get_x(bottom_right) {
                        grid[(x, y)].set_ch(' ');
                        grid[(x, y)].set_bg(Color::Default);
                        grid[(x, y)].set_fg(Color::Default);
                    }
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
                let account = &mut context.accounts[self.coordinates.0];
                let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                let op = account.operation(envelope.hash());
                match envelope.body(op) {
                    Ok(body) => body,
                    Err(e) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Failed to open e-mail".to_string()),
                            e.to_string(),
                            Some(NotificationType::ERROR),
                        ));
                        log(
                            format!(
                                "Failed to open envelope {}: {}",
                                envelope.message_id_display(),
                                e.to_string()
                            ),
                            ERROR,
                        );
                        return;
                    }
                }
            };
            match self.mode {
                ViewMode::Attachment(aidx) if body.attachments()[aidx].is_html() => {
                    self.pager = None;
                    let attachment = &body.attachments()[aidx];
                    self.subview = Some(Box::new(HtmlView::new(&attachment, context)));
                    self.mode = ViewMode::Subview;
                }
                ViewMode::Normal if body.is_html() => {
                    self.subview = Some(Box::new(HtmlView::new(&body, context)));
                    self.pager = None;
                    self.mode = ViewMode::Subview;
                }
                ViewMode::Subview | ViewMode::ContactSelector(_) => {}
                ViewMode::Raw => {
                    let text = {
                        let account = &mut context.accounts[self.coordinates.0];
                        let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                        let mut op = account.operation(envelope.hash());
                        op.as_bytes()
                            .map(|v| String::from_utf8_lossy(v).into_owned())
                            .unwrap_or_else(|e| e.to_string())
                    };
                    self.pager = Some(Pager::from_string(text, Some(context), None, None));
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
                    self.pager = Some(Pager::from_string(text, Some(context), cursor_pos, None));
                    self.subview = None;
                }
            };
        }
        match self.mode {
            ViewMode::Subview if self.subview.is_some() => {
                if let Some(s) = self.subview.as_mut() {
                    s.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);
                }
            }
            _ => {
                if let Some(p) = self.pager.as_mut() {
                    p.draw(grid, (set_y(upper_left, y + 1), bottom_right), context);
                }
            }
        }
        if let ViewMode::ContactSelector(ref mut s) = self.mode {
            s.draw(grid, center_area(area, s.content.size()), context);
        }
        self.dirty = false;
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
                    if s.is_done() {
                        if let ViewMode::ContactSelector(s) =
                            std::mem::replace(&mut self.mode, ViewMode::Normal)
                        {
                            let account = &mut context.accounts[self.coordinates.0];
                            {
                                for card in s.collect() {
                                    account.address_book.add_card(card);
                                }
                            }
                        }
                        self.set_dirty();
                    }
                    return true;
                }
                if let Some(p) = self.pager.as_mut() {
                    if p.process_event(event, context) {
                        return true;
                    }
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

        let shortcuts = &self.get_shortcuts(context)[MailView::DESCRIPTION];
        match *event {
            UIEvent::Input(ref key)
                if !self.mode.is_contact_selector()
                    && *key == shortcuts["add_addresses_to_contacts"] =>
            {
                let account = &mut context.accounts[self.coordinates.0];
                let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);

                let mut entries = Vec::new();
                for addr in envelope.from().iter().chain(envelope.to().iter()) {
                    let mut new_card: Card = Card::new();
                    new_card.set_email(addr.get_email());
                    new_card.set_name(addr.get_display_name());
                    entries.push((new_card, format!("{}", addr)));
                }
                drop(envelope);
                self.mode = ViewMode::ContactSelector(Selector::new(
                    "select contacts to add",
                    entries,
                    false,
                    context,
                ));
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt(''))
                if self.mode.is_contact_selector() =>
            {
                self.mode = ViewMode::Normal;
                self.set_dirty();
                return true;
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt('')) => {
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                return true;
            }
            UIEvent::Input(Key::Char(c)) if c >= '0' && c <= '9' => {
                self.cmd_buf.push(c);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufSet(
                        self.cmd_buf.clone(),
                    )));
                return true;
            }
            UIEvent::Input(ref key)
                if (self.mode == ViewMode::Normal || self.mode == ViewMode::Subview)
                    && *key == shortcuts["view_raw_source"] =>
            {
                self.mode = ViewMode::Raw;
                self.set_dirty();
                return true;
            }
            UIEvent::Input(ref key)
                if (self.mode.is_attachment()
                    || self.mode == ViewMode::Subview
                    || self.mode == ViewMode::Url
                    || self.mode == ViewMode::Raw)
                    && *key == shortcuts["return_to_normal_view"] =>
            {
                self.mode = ViewMode::Normal;
                self.set_dirty();
                return true;
            }
            UIEvent::Input(ref key)
                if (self.mode == ViewMode::Normal || self.mode == ViewMode::Subview)
                    && !self.cmd_buf.is_empty()
                    && *key == shortcuts["open_mailcap"] =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));

                {
                    let account = &mut context.accounts[self.coordinates.0];
                    let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                    let op = account.operation(envelope.hash());

                    let attachments = match envelope.body(op) {
                        Ok(body) => body.attachments(),
                        Err(e) => {
                            context.replies.push_back(UIEvent::Notification(
                                Some("Failed to open e-mail".to_string()),
                                e.to_string(),
                                Some(NotificationType::ERROR),
                            ));
                            log(
                                format!(
                                    "Failed to open envelope {}: {}",
                                    envelope.message_id_display(),
                                    e.to_string()
                                ),
                                ERROR,
                            );
                            return true;
                        }
                    };
                    drop(envelope);
                    drop(account);
                    if let Some(u) = attachments.get(lidx) {
                        if let Ok(()) = crate::mailcap::MailcapEntry::execute(u, context) {
                            self.set_dirty();
                        } else {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(format!(
                                    "no mailcap entry found for {}",
                                    u.content_type()
                                )),
                            ));
                        }
                    } else {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Attachment `{}` not found.",
                                lidx
                            )),
                        ));
                    }
                    return true;
                }
            }
            UIEvent::Input(ref key)
                if *key == shortcuts["open_attachment"]
                    && !self.cmd_buf.is_empty()
                    && (self.mode == ViewMode::Normal || self.mode == ViewMode::Subview) =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));

                {
                    let account = &mut context.accounts[self.coordinates.0];
                    let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                    let op = account.operation(envelope.hash());

                    let attachments = match envelope.body(op) {
                        Ok(body) => body.attachments(),
                        Err(e) => {
                            context.replies.push_back(UIEvent::Notification(
                                Some("Failed to open e-mail".to_string()),
                                e.to_string(),
                                Some(NotificationType::ERROR),
                            ));
                            log(
                                format!(
                                    "Failed to open envelope {}: {}",
                                    envelope.message_id_display(),
                                    e.to_string()
                                ),
                                ERROR,
                            );
                            return true;
                        }
                    };
                    if let Some(u) = attachments.get(lidx) {
                        match u.content_type() {
                            ContentType::MessageRfc822 => {
                                match EnvelopeWrapper::new(u.body().to_vec()) {
                                    Ok(wrapper) => {
                                        context.replies.push_back(UIEvent::Action(Tab(New(Some(
                                            Box::new(EnvelopeView::new(
                                                wrapper,
                                                None,
                                                None,
                                                self.coordinates.0,
                                            )),
                                        )))));
                                    }
                                    Err(e) => {
                                        context.replies.push_back(UIEvent::StatusEvent(
                                            StatusEvent::DisplayMessage(format!("{}", e)),
                                        ));
                                    }
                                }
                                return true;
                            }

                            ContentType::Text { .. } | ContentType::PGPSignature => {
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
                                let mut name_opt = name.as_ref().and_then(|n| {
                                    melib::email::parser::phrase(n.as_bytes())
                                        .to_full_result()
                                        .ok()
                                        .and_then(|n| String::from_utf8(n).ok())
                                });
                                if name_opt.is_none() {
                                    name_opt = name.as_ref().map(|n| n.clone());
                                }
                                if let Ok(binary) = binary {
                                    let p =
                                        create_temp_file(&decode(u, None), name_opt, None, true);
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
                                            StatusEvent::DisplayMessage(if name.is_some() {
                                                format!(
                                                        "Couldn't find a default application for file {} (type {})",
                                                        name.as_ref().unwrap(), attachment_type
                                            )
                                            } else {
                                                format!( "Couldn't find a default application for type {}", attachment_type)
                                            }

                                            ,
                                            )));
                                    return true;
                                }
                            }
                            ContentType::OctetStream { ref name } => {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(
                                        format!(
                                        "Failed to open {}. application/octet-stream isn't supported yet",
                                        name.as_ref().map(|n| n.as_str()).unwrap_or("file")
                                        )
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
            }
            UIEvent::Input(ref key) if *key == shortcuts["toggle_expand_headers"] => {
                self.expand_headers = !self.expand_headers;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key)
                if !self.cmd_buf.is_empty()
                    && self.mode == ViewMode::Url
                    && *key == shortcuts["go_to_url"] =>
            {
                let lidx = self.cmd_buf.parse::<usize>().unwrap();
                self.cmd_buf.clear();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                let url = {
                    let account = &mut context.accounts[self.coordinates.0];
                    let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                    let finder = LinkFinder::new();
                    let op = account.operation(envelope.hash());
                    let t = match envelope.body(op) {
                        Ok(body) => body.text().to_string(),
                        Err(e) => {
                            context.replies.push_back(UIEvent::Notification(
                                Some("Failed to open e-mail".to_string()),
                                e.to_string(),
                                Some(NotificationType::ERROR),
                            ));
                            log(
                                format!(
                                    "Failed to open envelope {}: {}",
                                    envelope.message_id_display(),
                                    e.to_string()
                                ),
                                ERROR,
                            );
                            return true;
                        }
                    };
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

                if let Err(e) = Command::new("xdg-open")
                    .arg(url)
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    context.replies.push_back(UIEvent::Notification(
                        Some("Failed to launch xdg-open".to_string()),
                        e.to_string(),
                        Some(NotificationType::ERROR),
                    ));
                }
                return true;
            }
            UIEvent::Input(ref key)
                if (self.mode == ViewMode::Normal || self.mode == ViewMode::Url)
                    && *key == shortcuts["toggle_url_mode"] =>
            {
                match self.mode {
                    ViewMode::Normal => self.mode = ViewMode::Url,
                    ViewMode::Url => self.mode = ViewMode::Normal,
                    _ => {}
                }
                self.dirty = true;
                return true;
            }
            UIEvent::EnvelopeRename(old_hash, new_hash) if self.coordinates.2 == old_hash => {
                self.coordinates.2 = new_hash;
            }
            UIEvent::Action(View(ViewAction::SaveAttachment(a_i, ref path))) => {
                use std::io::Write;
                let account = &mut context.accounts[self.coordinates.0];
                let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                let op = account.operation(envelope.hash());

                let attachments = match envelope.body(op) {
                    Ok(body) => body.attachments(),
                    Err(e) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Failed to open e-mail".to_string()),
                            e.to_string(),
                            Some(NotificationType::ERROR),
                        ));
                        log(
                            format!(
                                "Failed to open envelope {}: {}",
                                envelope.message_id_display(),
                                e.to_string()
                            ),
                            ERROR,
                        );
                        return true;
                    }
                };
                if let Some(u) = attachments.get(a_i) {
                    match u.content_type() {
                        ContentType::MessageRfc822
                        | ContentType::Text { .. }
                        | ContentType::PGPSignature => {
                            debug!(path);
                            let mut f = match std::fs::File::create(path) {
                                Err(e) => {
                                    context.replies.push_back(UIEvent::Notification(
                                        Some(format!("Failed to create file at {}", path)),
                                        e.to_string(),
                                        Some(NotificationType::ERROR),
                                    ));
                                    log(
                                        format!(
                                            "Failed to create file at {}: {}",
                                            path,
                                            e.to_string()
                                        ),
                                        ERROR,
                                    );
                                    return true;
                                }
                                Ok(f) => f,
                            };

                            f.write_all(&decode(u, None)).unwrap();
                            f.flush().unwrap();
                        }

                        ContentType::Multipart { .. } => {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(
                                    "Multipart attachments are not supported yet.".to_string(),
                                ),
                            ));
                            return true;
                        }
                        ContentType::OctetStream { name: ref _name }
                        | ContentType::Other {
                            name: ref _name, ..
                        } => {
                            let mut f = match std::fs::File::create(path.trim()) {
                                Err(e) => {
                                    context.replies.push_back(UIEvent::Notification(
                                        Some(format!("Failed to create file at {}", path)),
                                        e.to_string(),
                                        Some(NotificationType::ERROR),
                                    ));
                                    log(
                                        format!(
                                            "Failed to create file at {}: {}",
                                            path,
                                            e.to_string()
                                        ),
                                        ERROR,
                                    );
                                    return true;
                                }
                                Ok(f) => f,
                            };

                            f.write_all(&decode(u, None)).unwrap();
                            f.flush().unwrap();
                        }
                    }
                    context.replies.push_back(UIEvent::Notification(
                        None,
                        format!("Saved at {}", &path),
                        Some(NotificationType::INFO),
                    ));
                } else {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(format!(
                            "Attachment `{}` not found.",
                            a_i
                        ))));
                    return true;
                }
            }
            UIEvent::Action(MailingListAction(ref e)) => {
                let unsafe_context = context as *mut Context;
                let account = &context.accounts[self.coordinates.0];
                if !account.contains_key(self.coordinates.2) {
                    /* The envelope has been renamed or removed, so wait for the appropriate event to
                     * arrive */
                    return true;
                }
                let envelope: EnvelopeRef = account.collection.get_env(self.coordinates.2);
                if let Some(actions) = list_management::detect(&envelope) {
                    match e {
                        MailingListAction::ListPost if actions.post.is_some() => {
                            /* open composer */
                            let mut draft = Draft::default();
                            draft.set_header("To", actions.post.unwrap().to_string());
                            context.replies.push_back(UIEvent::Action(Tab(NewDraft(
                                self.coordinates.0,
                                Some(draft),
                            ))));
                            return true;
                        }
                        MailingListAction::ListUnsubscribe if actions.unsubscribe.is_some() => {
                            /* autosend or open unsubscribe option*/
                            let unsubscribe = actions.unsubscribe.unwrap();
                            for option in unsubscribe {
                                /* TODO: Ask for confirmation before proceding with an action */
                                match option {
                                    list_management::UnsubscribeOption::Email(email) => {
                                        if let Ok(mailto) = Mailto::try_from(email) {
                                            let mut draft: Draft = mailto.into();
                                            draft.headers_mut().insert(
                                                "From".into(),
                                                crate::components::mail::get_display_name(
                                                    context,
                                                    self.coordinates.0,
                                                ),
                                            );
                                            if super::compose::send_draft(
                                                ToggleFlag::False,
                                                /* FIXME: refactor to avoid unsafe.
                                                 *
                                                 * actions contains byte slices from the envelope's
                                                 * headers send_draft only needs a mut ref for
                                                 * context to push back replies and save the sent
                                                 * message */
                                                unsafe { &mut *(unsafe_context) },
                                                self.coordinates.0,
                                                draft,
                                            ) {
                                                context.replies.push_back(UIEvent::Notification(
                                                    Some("Sent unsubscribe email.".into()),
                                                    "Sent unsubscribe email".to_string(),
                                                    None,
                                                ));
                                                return true;
                                            }
                                        }
                                    }
                                    list_management::UnsubscribeOption::Url(url) => {
                                        if let Err(e) = Command::new("xdg-open")
                                            .arg(String::from_utf8_lossy(url).into_owned())
                                            .stdin(Stdio::piped())
                                            .stdout(Stdio::piped())
                                            .spawn()
                                        {
                                            context.replies.push_back(UIEvent::StatusEvent(
                                                StatusEvent::DisplayMessage(format!(
                                                    "Couldn't launch xdg-open: {}",
                                                    e
                                                )),
                                            ));
                                        }
                                        return true;
                                    }
                                }
                            }
                        }
                        MailingListAction::ListArchive if actions.archive.is_some() => {
                            /* open archive url with xdg-open */
                            if let Err(e) = Command::new("xdg-open")
                                .arg(actions.archive.unwrap())
                                .stdin(Stdio::piped())
                                .stdout(Stdio::piped())
                                .spawn()
                            {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(format!(
                                        "Couldn't launch xdg-open: {}",
                                        e
                                    )),
                                ));
                            }
                            return true;
                        }
                        _ => { /* error print message to user */ }
                    }
                }
            }
            UIEvent::Action(Listing(OpenInNewTab)) => {
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(New(Some(Box::new(self.clone()))))));
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
        if self.mode.is_attachment()
            || self.mode == ViewMode::Subview
            || self.mode == ViewMode::Raw
            || self.mode == ViewMode::Url
        {
            our_map.insert("return_to_normal_view", Key::Char('r'));
        }
        our_map.insert("open_attachment", Key::Char('a'));
        our_map.insert("open_mailcap", Key::Char('m'));
        if self.mode == ViewMode::Url {
            our_map.insert("go_to_url", Key::Char('g'));
        }
        if self.mode == ViewMode::Normal || self.mode == ViewMode::Url {
            our_map.insert("toggle_url_mode", Key::Char('u'));
        }
        our_map.insert("toggle_expand_headers", Key::Char('h'));
        map.insert(MailView::DESCRIPTION.to_string(), our_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }

    fn kill(&mut self, id: ComponentId, context: &mut Context) {
        debug_assert!(self.id == id);
        context
            .replies
            .push_back(UIEvent::Action(Tab(Kill(self.id))));
    }
}
