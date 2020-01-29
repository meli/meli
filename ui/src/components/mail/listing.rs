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
use crate::types::segment_tree::SegmentTree;
use smallvec::SmallVec;
use std::ops::{Deref, DerefMut};

mod conversations;
pub use self::conversations::*;

mod compact;
pub use self::compact::*;

mod thread;
pub use self::thread::*;

mod plain;
pub use self::plain::*;

#[derive(Debug, Default, Clone)]
pub struct DataColumns {
    pub columns: [CellBuffer; 12],
    pub widths: [usize; 12], // widths of columns calculated in first draw and after size changes
    pub segment_tree: [SegmentTree; 12],
}

#[derive(Debug, Default)]
/// Save theme colors to avoid looking them up again and again from settings
struct ColorCache {
    theme_default: ThemeAttribute,

    unseen: ThemeAttribute,
    highlighted: ThemeAttribute,
    even: ThemeAttribute,
    odd: ThemeAttribute,
    selected: ThemeAttribute,
    attachment_flag: ThemeAttribute,
    thread_snooze_flag: ThemeAttribute,

    /* Conversations */
    subject: ThemeAttribute,
    from: ThemeAttribute,
    date: ThemeAttribute,
    padding: ThemeAttribute,
    unseen_padding: ThemeAttribute,
}

#[derive(Debug)]
pub(super) struct EntryStrings {
    pub(super) date: DateString,
    pub(super) subject: SubjectString,
    pub(super) flag: FlagString,
    pub(super) from: FromString,
    pub(super) tags: TagString,
}

#[macro_export]
macro_rules! address_list {
    (($name:expr) as comma_sep_list) => {{
        let mut ret: String =
            $name
                .into_iter()
                .fold(String::new(), |mut s: String, n: &Address| {
                    s.extend(n.to_string().chars());
                    s.push_str(", ");
                    s
                });
        ret.pop();
        ret.pop();
        ret
    }};
}

macro_rules! column_str {
    (
        struct $name:ident($($t:ty),+)) => {
        #[derive(Debug)]
        pub(super) struct $name($(pub $t),+);

        impl Deref for $name {
            type Target = String;
            fn deref(&self) -> &String {
                &self.0
            }
        }
        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut String {
                &mut self.0
            }
        }
    };
}

column_str!(struct DateString(String));
column_str!(struct FromString(String));
column_str!(struct SubjectString(String));
column_str!(struct FlagString(String));
column_str!(struct TagString(String, SmallVec<[Color; 8]>));

#[derive(Debug)]
struct AccountMenuEntry {
    name: String,
    // Index in the config account vector.
    index: usize,
}

pub trait MailListingTrait: ListingTrait {
    fn perform_action(
        &mut self,
        context: &mut Context,
        thread_hash: ThreadHash,
        a: &ListingAction,
    ) {
        let account = &mut context.accounts[self.coordinates().0];
        let mut envs_to_set: SmallVec<[EnvelopeHash; 8]> = SmallVec::new();
        let folder_hash = account[self.coordinates().1].unwrap().folder.hash();
        for (_, h) in account.collection.threads[&folder_hash].thread_group_iter(thread_hash) {
            envs_to_set.push(
                account.collection.threads[&folder_hash].thread_nodes()[&h]
                    .message()
                    .unwrap(),
            );
        }
        for env_hash in envs_to_set {
            let op = account.operation(env_hash);
            let mut envelope: EnvelopeRefMut = account.collection.get_env_mut(env_hash);
            match a {
                ListingAction::SetSeen => {
                    if let Err(e) = envelope.set_seen(op) {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(e.to_string()),
                        ));
                    }
                }
                ListingAction::SetUnseen => {
                    if let Err(e) = envelope.set_unseen(op) {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(e.to_string()),
                        ));
                    }
                }
                ListingAction::Delete => {
                    /* do nothing */
                    continue;
                }
                ListingAction::Tag(Remove(ref tag_str)) => {
                    let backend_lck = account.backend.write().unwrap();
                    let mut op = backend_lck.operation(envelope.hash());
                    if let Err(err) = op.set_tag(&mut envelope, tag_str.to_string(), false) {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not set tag.".to_string()),
                            err.to_string(),
                            Some(NotificationType::ERROR),
                        ));
                        return;
                    }
                }
                ListingAction::Tag(Add(ref tag_str)) => {
                    let backend_lck = account.backend.write().unwrap();
                    let mut op = backend_lck.operation(envelope.hash());

                    if let Err(err) = op.set_tag(&mut envelope, tag_str.to_string(), true) {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not set tag.".to_string()),
                            err.to_string(),
                            Some(NotificationType::ERROR),
                        ));
                        return;
                    }
                }
                _ => unreachable!(),
            }
            self.row_updates().push(thread_hash);
            drop(envelope);
        }
    }

    fn row_updates(&mut self) -> &mut SmallVec<[ThreadHash; 8]>;
    fn get_focused_items(&self, _context: &Context) -> SmallVec<[ThreadHash; 8]>;
    fn redraw_list(&mut self, _context: &Context, _items: Box<dyn Iterator<Item = ThreadHash>>) {
        unimplemented!()
    }
}

pub trait ListingTrait: Component {
    fn coordinates(&self) -> (usize, usize);
    fn set_coordinates(&mut self, _: (usize, usize));
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context);
    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context);
    fn filter(&mut self, _filter_term: &str, _context: &Context) {}
    fn set_movement(&mut self, mvm: PageMovement);
}

#[derive(Debug)]
pub enum ListingComponent {
    Plain(PlainListing),
    Threaded(ThreadListing),
    Compact(CompactListing),
    Conversations(ConversationsListing),
}
use crate::ListingComponent::*;

impl core::ops::Deref for ListingComponent {
    type Target = dyn MailListingTrait;

    fn deref(&self) -> &Self::Target {
        match &self {
            Compact(ref l) => l,
            Plain(ref l) => l,
            Threaded(ref l) => l,
            Conversations(ref l) => l,
        }
    }
}

impl core::ops::DerefMut for ListingComponent {
    fn deref_mut(&mut self) -> &mut (dyn MailListingTrait + 'static) {
        match self {
            Compact(l) => l,
            Plain(l) => l,
            Threaded(l) => l,
            Conversations(l) => l,
        }
    }
}

impl ListingComponent {
    fn set_style(&mut self, new_style: IndexStyle) {
        match new_style {
            IndexStyle::Plain => {
                if let Plain(_) = self {
                    return;
                }
                let mut new_l = PlainListing::default();
                let coors = self.coordinates();
                new_l.set_coordinates((coors.0, coors.1));
                *self = Plain(new_l);
            }
            IndexStyle::Threaded => {
                if let Threaded(_) = self {
                    return;
                }
                let mut new_l = ThreadListing::default();
                let coors = self.coordinates();
                new_l.set_coordinates((coors.0, coors.1));
                *self = Threaded(new_l);
            }
            IndexStyle::Compact => {
                if let Compact(_) = self {
                    return;
                }
                let mut new_l = CompactListing::default();
                let coors = self.coordinates();
                new_l.set_coordinates((coors.0, coors.1));
                *self = Compact(new_l);
            }
            IndexStyle::Conversations => {
                if let Conversations(_) = self {
                    return;
                }
                let mut new_l = ConversationsListing::default();
                let coors = self.coordinates();
                new_l.set_coordinates((coors.0, coors.1));
                *self = Conversations(new_l);
            }
        }
    }
}

#[derive(Debug)]
pub struct Listing {
    component: ListingComponent,
    accounts: Vec<AccountMenuEntry>,
    dirty: bool,
    visible: bool,
    cursor_pos: (usize, usize),
    startup_checks_rate: RateLimit,
    id: ComponentId,
    theme_default: ThemeAttribute,

    show_divider: bool,
    menu_visibility: bool,
    cmd_buf: String,
    /// This is the width of the right container to the entire width.
    ratio: usize, // right/(container width) * 100
}

impl fmt::Display for Listing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.component {
            Compact(ref l) => write!(f, "{}", l),
            Plain(ref l) => write!(f, "{}", l),
            Threaded(ref l) => write!(f, "{}", l),
            Conversations(ref l) => write!(f, "{}", l),
        }
    }
}

impl Component for Listing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        for i in 0..context.accounts.len() {
            let _ = context.is_online(i);
        }

        if !self.is_dirty() {
            return;
        }
        if !is_valid_area!(area) {
            return;
        }
        self.theme_default = crate::conf::value(context, "theme_default");
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let total_cols = get_x(bottom_right) - get_x(upper_left);

        let right_component_width = if self.menu_visibility {
            (self.ratio * total_cols) / 100
        } else {
            total_cols
        };
        let mid = get_x(bottom_right) - right_component_width;
        if self.dirty && mid != get_x(upper_left) {
            if self.show_divider {
                for i in get_y(upper_left)..=get_y(bottom_right) {
                    grid[(mid, i)]
                        .set_ch(VERT_BOUNDARY)
                        .set_fg(self.theme_default.fg)
                        .set_bg(self.theme_default.bg)
                        .set_attrs(self.theme_default.attrs);
                }
            } else {
                for i in get_y(upper_left)..=get_y(bottom_right) {
                    grid[(mid, i)]
                        .set_fg(self.theme_default.fg)
                        .set_bg(self.theme_default.bg)
                        .set_attrs(self.theme_default.attrs);
                }
            }
            context
                .dirty_areas
                .push_back(((mid, get_y(upper_left)), (mid, get_y(bottom_right))));
        }

        if right_component_width == total_cols {
            if let Err(err) = context.is_online(self.cursor_pos.0) {
                clear_area(grid, area);
                let (x, _) = write_string_to_grid(
                    "offline: ",
                    grid,
                    Color::Byte(243),
                    Color::Default,
                    Attr::Default,
                    (set_x(upper_left, mid + 1), bottom_right),
                    None,
                );
                write_string_to_grid(
                    &err.to_string(),
                    grid,
                    Color::Red,
                    Color::Default,
                    Attr::Default,
                    (set_x(upper_left, x + 1), bottom_right),
                    None,
                );
                context.dirty_areas.push_back(area);
                return;
            } else {
                self.component.draw(grid, area, context);
            }
        } else if right_component_width == 0 {
            self.draw_menu(grid, area, context);
        } else {
            self.draw_menu(grid, (upper_left, (mid, get_y(bottom_right))), context);
            if let Err(err) = context.is_online(self.cursor_pos.0) {
                clear_area(grid, (set_x(upper_left, mid + 1), bottom_right));
                let (x, _) = write_string_to_grid(
                    "offline: ",
                    grid,
                    Color::Byte(243),
                    Color::Default,
                    Attr::Default,
                    (set_x(upper_left, mid + 1), bottom_right),
                    None,
                );
                write_string_to_grid(
                    &err.to_string(),
                    grid,
                    Color::Red,
                    Color::Default,
                    Attr::Default,
                    (set_x(upper_left, x + 1), bottom_right),
                    None,
                );
                self.component.set_dirty(false);
                context.dirty_areas.push_back(area);
            } else {
                self.component
                    .draw(grid, (set_x(upper_left, mid + 1), bottom_right), context);
            }
        }
        self.dirty = false;
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match event {
            UIEvent::StartupCheck(ref f) => {
                if context.accounts[self.component.coordinates().0]
                    .folders_order
                    .get(self.component.coordinates().1)
                    .map(|&folder_hash| *f == folder_hash)
                    .unwrap_or(false)
                {
                    if !self.startup_checks_rate.tick() {
                        return false;
                    }
                }
            }
            UIEvent::Timer(n) if *n == self.startup_checks_rate.id() => {
                if self.startup_checks_rate.active {
                    self.startup_checks_rate.reset();
                    if let Some(folder_hash) = context.accounts[self.component.coordinates().0]
                        .folders_order
                        .get(self.component.coordinates().1)
                    {
                        return self
                            .process_event(&mut UIEvent::StartupCheck(*folder_hash), context);
                    }
                }
            }
            _ => {}
        }

        if self.component.process_event(event, context) {
            return true;
        }

        let shortcuts = self.get_shortcuts(context);
        match *event {
            UIEvent::Input(ref k)
                if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_folder"])
                    || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_folder"]) =>
            {
                let amount = if self.cmd_buf.is_empty() {
                    1
                } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    amount
                } else {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    return true;
                };
                let folder_length = context.accounts[self.cursor_pos.0].len();
                match k {
                    k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_folder"])
                        && folder_length > 0 =>
                    {
                        if self.cursor_pos.1 + amount < folder_length {
                            self.cursor_pos.1 += amount;
                            self.component
                                .set_coordinates((self.cursor_pos.0, self.cursor_pos.1));
                            self.set_dirty(true);
                        } else {
                            return true;
                        }
                    }
                    k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_folder"]) => {
                        if self.cursor_pos.1 >= amount {
                            self.cursor_pos.1 -= amount;
                            self.component
                                .set_coordinates((self.cursor_pos.0, self.cursor_pos.1));
                            self.set_dirty(true);
                        } else {
                            return true;
                        }
                    }
                    _ => return false,
                }
                /* Account might have no folders yet if it's offline */
                if let Some(&folder_hash) = context.accounts[self.cursor_pos.0]
                    .folders_order
                    .get(self.cursor_pos.1)
                {
                    /* Check if per-folder configuration overrides general configuration */
                    if let Some(index_style) =
                        context.accounts.get(self.cursor_pos.0).and_then(|account| {
                            account.folder_confs(folder_hash).conf_override.index_style
                        })
                    {
                        self.component.set_style(index_style);
                    } else if let Some(index_style) = context
                        .accounts
                        .get(self.cursor_pos.0)
                        .and_then(|account| Some(account.settings.conf.index_style()))
                    {
                        self.component.set_style(index_style);
                    }
                }
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context).unwrap(),
                    )));
                return true;
            }
            UIEvent::Input(ref k)
                if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_account"])
                    || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_account"]) =>
            {
                let amount = if self.cmd_buf.is_empty() {
                    1
                } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    amount
                } else {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    return true;
                };
                match k {
                    k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_account"]) => {
                        if self.cursor_pos.0 + amount < self.accounts.len() {
                            self.cursor_pos = (self.cursor_pos.0 + amount, 0);
                            self.component.set_coordinates((self.cursor_pos.0, 0));
                            self.set_dirty(true);
                        } else {
                            return true;
                        }
                    }
                    k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_account"]) => {
                        if self.cursor_pos.0 >= amount {
                            self.cursor_pos = (self.cursor_pos.0 - amount, 0);
                            self.component.set_coordinates((self.cursor_pos.0, 0));
                            self.set_dirty(true);
                        } else {
                            return true;
                        }
                    }
                    _ => return false,
                }

                /* Account might have no folders yet if it's offline */
                if let Some(&folder_hash) = context.accounts[self.cursor_pos.0]
                    .folders_order
                    .get(self.cursor_pos.1)
                {
                    /* Check if per-folder configuration overrides general configuration */
                    if let Some(index_style) =
                        context.accounts.get(self.cursor_pos.0).and_then(|account| {
                            account.folder_confs(folder_hash).conf_override.index_style
                        })
                    {
                        self.component.set_style(index_style);
                    } else if let Some(index_style) = context
                        .accounts
                        .get(self.cursor_pos.0)
                        .and_then(|account| Some(account.settings.conf.index_style()))
                    {
                        self.component.set_style(index_style);
                    }
                }
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context).unwrap(),
                    )));
                return true;
            }
            UIEvent::Action(ref action) => match action {
                Action::Listing(ListingAction::SetPlain) => {
                    self.component.set_style(IndexStyle::Plain);
                    return true;
                }
                Action::Listing(ListingAction::SetThreaded) => {
                    self.component.set_style(IndexStyle::Threaded);
                    return true;
                }
                Action::Listing(ListingAction::SetCompact) => {
                    self.component.set_style(IndexStyle::Compact);
                    return true;
                }
                Action::Listing(ListingAction::SetConversations) => {
                    self.component.set_style(IndexStyle::Conversations);
                    return true;
                }
                Action::Listing(a @ ListingAction::SetSeen)
                | Action::Listing(a @ ListingAction::SetUnseen)
                | Action::Listing(a @ ListingAction::Delete)
                | Action::Listing(a @ ListingAction::Tag(_)) => {
                    let focused = self.component.get_focused_items(context);
                    for i in focused {
                        self.component.perform_action(context, i, a);
                    }
                    self.component.set_dirty(true);
                    return true;
                }
                _ => {}
            },
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Resize => {
                self.set_dirty(true);
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Listing::DESCRIPTION]["scroll_up"]) =>
            {
                let amount = if self.cmd_buf.is_empty() {
                    1
                } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    amount
                } else {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    return true;
                };
                self.component.set_movement(PageMovement::Up(amount));
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Listing::DESCRIPTION]["scroll_down"]) =>
            {
                let amount = if self.cmd_buf.is_empty() {
                    1
                } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    amount
                } else {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    return true;
                };
                self.component.set_movement(PageMovement::Down(amount));
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Listing::DESCRIPTION]["prev_page"]) =>
            {
                let mult = if self.cmd_buf.is_empty() {
                    1
                } else if let Ok(mult) = self.cmd_buf.parse::<usize>() {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    mult
                } else {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    return true;
                };
                self.component.set_movement(PageMovement::PageUp(mult));
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Listing::DESCRIPTION]["next_page"]) =>
            {
                let mult = if self.cmd_buf.is_empty() {
                    1
                } else if let Ok(mult) = self.cmd_buf.parse::<usize>() {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    mult
                } else {
                    self.cmd_buf.clear();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                    return true;
                };
                self.component.set_movement(PageMovement::PageDown(mult));
                return true;
            }
            UIEvent::Input(ref key) if *key == Key::Home => {
                self.component.set_movement(PageMovement::Home);
                return true;
            }
            UIEvent::Input(ref key) if *key == Key::End => {
                self.component.set_movement(PageMovement::End);
                return true;
            }
            UIEvent::Input(ref k)
                if shortcut!(k == shortcuts[Listing::DESCRIPTION]["toggle_menu_visibility"]) =>
            {
                self.menu_visibility = !self.menu_visibility;
                self.set_dirty(true);
            }
            UIEvent::Input(ref k)
                if shortcut!(k == shortcuts[Listing::DESCRIPTION]["new_mail"]) =>
            {
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(NewDraft(self.cursor_pos.0, None))));
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Listing::DESCRIPTION]["search"]) =>
            {
                context
                    .replies
                    .push_back(UIEvent::ExInput(Key::Paste("filter ".to_string())));
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Execute));
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Listing::DESCRIPTION]["set_seen"]) =>
            {
                let mut event = UIEvent::Action(Action::Listing(ListingAction::SetSeen));
                if self.process_event(&mut event, context) {
                    return true;
                }
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Listing::DESCRIPTION]["refresh"]) =>
            {
                let account = &mut context.accounts[self.cursor_pos.0];
                if let Some(&folder_hash) = account.folders_order.get(self.cursor_pos.1) {
                    if let Err(err) = account.refresh(folder_hash) {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not refresh.".to_string()),
                            err.to_string(),
                            Some(NotificationType::INFO),
                        ));
                    }
                }
                return true;
            }
            UIEvent::StartupCheck(_) => {
                self.dirty = true;
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context).unwrap(),
                    )));
            }
            UIEvent::MailboxUpdate(_) => {
                self.dirty = true;
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context).unwrap(),
                    )));
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
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.component.is_dirty()
    }
    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.component.set_dirty(value);
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = self.component.get_shortcuts(context);
        let config_map = context.settings.shortcuts.listing.key_values();
        map.insert(Listing::DESCRIPTION, config_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.component.id()
    }
    fn set_id(&mut self, id: ComponentId) {
        self.component.set_id(id);
    }

    fn get_status(&self, context: &Context) -> Option<String> {
        Some({
            let folder_hash = if let Some(h) = context.accounts[self.cursor_pos.0]
                .folders_order
                .get(self.cursor_pos.1)
            {
                *h
            } else {
                return Some(String::new());
            };
            if !context.accounts[self.cursor_pos.0].folders[&folder_hash].is_available() {
                return Some(String::new());
            }
            let account = &context.accounts[self.cursor_pos.0];
            let m = if account[self.cursor_pos.1].is_available() {
                account[self.cursor_pos.1].unwrap()
            } else {
                return Some(String::new());
            };
            format!(
                "Mailbox: {}, Messages: {}, New: {}",
                m.folder.name(),
                m.envelopes.len(),
                m.folder.count().ok().map(|(v, _)| v).unwrap_or(0),
            )
        })
    }
}

impl From<IndexStyle> for ListingComponent {
    fn from(index_style: IndexStyle) -> Self {
        match index_style {
            IndexStyle::Plain => Plain(Default::default()),
            IndexStyle::Threaded => Threaded(Default::default()),
            IndexStyle::Compact => Compact(Default::default()),
            IndexStyle::Conversations => Conversations(Default::default()),
        }
    }
}

impl Listing {
    const DESCRIPTION: &'static str = "listing";
    pub fn new(accounts: &[Account]) -> Self {
        let account_entries = accounts
            .iter()
            .enumerate()
            .map(|(i, a)| AccountMenuEntry {
                name: a.name().to_string(),
                index: i,
            })
            .collect();
        /* Check if per-folder configuration overrides general configuration */
        let component = if let Some(index_style) = accounts.get(0).and_then(|account| {
            account.folders_order.get(0).and_then(|folder_hash| {
                account.folder_confs(*folder_hash).conf_override.index_style
            })
        }) {
            ListingComponent::from(index_style)
        } else if let Some(index_style) = accounts
            .get(0)
            .and_then(|account| Some(account.settings.conf.index_style()))
        {
            ListingComponent::from(index_style)
        } else {
            Conversations(Default::default())
        };
        Listing {
            component,
            accounts: account_entries,
            visible: true,
            dirty: true,
            cursor_pos: (0, 0),
            startup_checks_rate: RateLimit::new(2, 1000),
            theme_default: ThemeAttribute::default(),
            id: ComponentId::new_v4(),
            show_divider: false,
            menu_visibility: true,
            ratio: 90,
            cmd_buf: String::with_capacity(4),
        }
    }

    fn draw_menu(&mut self, grid: &mut CellBuffer, mut area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        for row in grid.bounds_iter(area) {
            for c in row {
                grid[c]
                    .set_ch(' ')
                    .set_fg(self.theme_default.fg)
                    .set_bg(self.theme_default.bg)
                    .set_attrs(self.theme_default.attrs);
            }
        }
        /* visually divide menu and listing */
        area = (area.0, pos_dec(area.1, (1, 0)));
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        self.dirty = false;
        let mut y = get_y(upper_left);
        for a in &self.accounts {
            y += self.print_account(grid, (set_y(upper_left, y), bottom_right), &a, context);
            y += 3;
        }

        context.dirty_areas.push_back(area);
    }
    /*
     * Print a single account in the menu area.
     */
    fn print_account(
        &self,
        grid: &mut CellBuffer,
        area: Area,
        a: &AccountMenuEntry,
        context: &mut Context,
    ) -> usize {
        if !is_valid_area!(area) {
            debug!("BUG: invalid area in print_account");
        }
        // Each entry and its index in the account
        let entries: FnvHashMap<FolderHash, Folder> = context.accounts[a.index]
            .list_folders()
            .into_iter()
            .map(|f| (f.hash(), f))
            .collect();
        let folders_order: FnvHashMap<FolderHash, usize> = context.accounts[a.index]
            .folders_order()
            .iter()
            .enumerate()
            .map(|(i, &fh)| (fh, i))
            .collect();

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let must_highlight_account: bool = self.cursor_pos.0 == a.index;

        let mut inc = 0;
        let mut depth = 0;
        let mut lines: Vec<(usize, usize, FolderHash, Option<usize>)> = Vec::new();

        /* Gather the folder tree structure in `lines` recursively */
        fn print(
            folder_idx: FolderHash,
            depth: &mut usize,
            inc: &mut usize,
            entries: &FnvHashMap<FolderHash, Folder>,
            folders_order: &FnvHashMap<FolderHash, usize>,
            lines: &mut Vec<(usize, usize, FolderHash, Option<usize>)>,
            index: usize, //account index
            context: &mut Context,
        ) {
            match context.accounts[index].status(entries[&folder_idx].hash()) {
                Ok(_) => {
                    lines.push((
                        *depth,
                        *inc,
                        folder_idx,
                        entries[&folder_idx].count().ok().map(|(v, _)| v),
                    ));
                }
                Err(_) => {
                    lines.push((*depth, *inc, folder_idx, None));
                }
            }
            *inc += 1;
            let mut children: Vec<FolderHash> = entries[&folder_idx].children().to_vec();
            children
                .sort_unstable_by(|a, b| folders_order[a].partial_cmp(&folders_order[b]).unwrap());
            *depth += 1;
            for child in children {
                print(
                    child,
                    depth,
                    inc,
                    entries,
                    folders_order,
                    lines,
                    index,
                    context,
                );
            }
            *depth -= 1;
        }
        let mut keys = entries.keys().cloned().collect::<Vec<FolderHash>>();
        keys.sort_unstable_by(|a, b| folders_order[a].partial_cmp(&folders_order[b]).unwrap());

        /* Start with roots */
        for f in keys {
            if entries[&f].parent().is_none() {
                print(
                    f,
                    &mut depth,
                    &mut inc,
                    &entries,
                    &folders_order,
                    &mut lines,
                    a.index,
                    context,
                );
            }
        }

        /* Print account name first */
        write_string_to_grid(
            &a.name,
            grid,
            self.theme_default.fg,
            self.theme_default.bg,
            Attr::Bold,
            area,
            None,
        );

        if lines.is_empty() {
            write_string_to_grid(
                "offline",
                grid,
                Color::Byte(243),
                Color::Default,
                Attr::Default,
                (pos_inc(upper_left, (0, 1)), bottom_right),
                None,
            );
            return 0;
        }

        let lines_len = lines.len();
        let mut idx = 0;

        for y in get_y(upper_left) + 1..get_y(bottom_right) {
            if idx == lines_len {
                break;
            }
            let (att, index_att, unread_count_att) = if must_highlight_account {
                if self.cursor_pos.1 == idx {
                    let mut ret = (
                        crate::conf::value(context, "mail.sidebar_highlighted"),
                        crate::conf::value(context, "mail.sidebar_highlighted_index"),
                        crate::conf::value(context, "mail.sidebar_highlighted_unread_count"),
                    );

                    if std::env::var("NO_COLOR").is_ok()
                        && (context.settings.terminal.use_color.is_false()
                            || context.settings.terminal.use_color.is_internal())
                    {
                        ret.0.attrs |= Attr::Reverse;
                        ret.1.attrs |= Attr::Reverse;
                        ret.2.attrs |= Attr::Reverse;
                    }
                    ret
                } else {
                    (
                        crate::conf::value(context, "mail.sidebar_highlighted_account"),
                        crate::conf::value(context, "mail.sidebar_highlighted_account_index"),
                        crate::conf::value(
                            context,
                            "mail.sidebar_highlighted_account_unread_count",
                        ),
                    )
                }
            } else {
                (
                    crate::conf::value(context, "mail.sidebar"),
                    crate::conf::value(context, "mail.sidebar_index"),
                    crate::conf::value(context, "mail.sidebar_unread_count"),
                )
            };

            let (depth, inc, folder_idx, count) = lines[idx];
            /* Calculate how many columns the folder index tags should occupy with right alignment,
             * eg.
             *  1
             *  2
             * ...
             *  9
             * 10
             */
            let total_folder_no_digits = {
                let mut len = lines_len;
                let mut ctr = 1;
                while len > 9 {
                    ctr += 1;
                    len /= 10;
                }
                ctr
            };
            let (x, _) = write_string_to_grid(
                &format!("{:>width$}", inc, width = total_folder_no_digits),
                grid,
                index_att.fg,
                index_att.bg,
                index_att.attrs,
                (set_y(upper_left, y), bottom_right),
                None,
            );
            let (x, _) = write_string_to_grid(
                &" ".repeat(depth + 1),
                grid,
                att.fg,
                att.bg,
                att.attrs,
                ((x, y), bottom_right),
                None,
            );
            let (x, _) = write_string_to_grid(
                entries[&folder_idx].name(),
                grid,
                att.fg,
                att.bg,
                att.attrs,
                ((x, y), bottom_right),
                None,
            );

            /* Unread message count */
            let count_string = if let Some(c) = count {
                if c > 0 {
                    format!(" {}", c)
                } else {
                    String::new()
                }
            } else {
                " ...".to_string()
            };

            let (x, _) = write_string_to_grid(
                &count_string,
                grid,
                unread_count_att.fg,
                unread_count_att.bg,
                unread_count_att.attrs
                    | if count.unwrap_or(0) > 0 {
                        Attr::Bold
                    } else {
                        Attr::Default
                    },
                (
                    (
                        /* Hide part of folder name if need be to fit the message count */
                        std::cmp::min(x, get_x(bottom_right).saturating_sub(count_string.len())),
                        y,
                    ),
                    bottom_right,
                ),
                None,
            );
            for c in grid.row_iter(x..(get_x(bottom_right) + 1), y) {
                grid[c].set_fg(att.fg).set_bg(att.bg).set_attrs(att.attrs);
            }
            idx += 1;
        }
        if idx == 0 {
            0
        } else {
            idx - 1
        }
    }
}
