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

mod offline;
pub use self::offline::*;

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
/// Creates a comma separated list `String` out of an `Address` iterable.
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
    entries: SmallVec<[(usize, MailboxHash); 16]>,
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
        let mailbox_hash = self.coordinates().1;
        for (_, h) in account.collection.threads[&mailbox_hash].thread_group_iter(thread_hash) {
            envs_to_set.push(
                account.collection.threads[&mailbox_hash].thread_nodes()[&h]
                    .message()
                    .unwrap(),
            );
        }
        for env_hash in envs_to_set {
            let mut op = account.operation(env_hash);
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
                    drop(envelope);
                    if let Err(err) = account.delete(env_hash) {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not delete.".to_string()),
                            err.to_string(),
                            Some(NotificationType::ERROR),
                        ));
                        return;
                    }
                    continue;
                }
                ListingAction::CopyTo(ref mailbox_path) => {
                    drop(envelope);
                    if let Err(err) = op
                        .as_bytes()
                        .and_then(|bytes| account.save(bytes, mailbox_path, None))
                    {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not copy.".to_string()),
                            err.to_string(),
                            Some(NotificationType::ERROR),
                        ));
                        return;
                    }
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
            self.set_dirty(true);
            drop(envelope);
        }
    }

    fn row_updates(&mut self) -> &mut SmallVec<[ThreadHash; 8]>;
    fn get_focused_items(&self, _context: &Context) -> SmallVec<[ThreadHash; 8]>;
    fn redraw_list(&mut self, _context: &Context, _items: Box<dyn Iterator<Item = ThreadHash>>) {
        unimplemented!()
    }

    /// Use `force` when there have been changes in the mailbox or account lists in `context`
    fn refresh_mailbox(&mut self, context: &mut Context, force: bool);
}

pub trait ListingTrait: Component {
    fn coordinates(&self) -> (usize, MailboxHash);
    fn set_coordinates(&mut self, _: (usize, MailboxHash));
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
    Offline(OfflineListing),
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
            Offline(ref l) => l,
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
            Offline(l) => l,
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
                *self = Plain(PlainListing::new(self.coordinates()));
            }
            IndexStyle::Threaded => {
                if let Threaded(_) = self {
                    return;
                }
                *self = Threaded(ThreadListing::new(self.coordinates()));
            }
            IndexStyle::Compact => {
                if let Compact(_) = self {
                    return;
                }
                *self = Compact(CompactListing::new(self.coordinates()));
            }
            IndexStyle::Conversations => {
                if let Conversations(_) = self {
                    return;
                }
                *self = Conversations(ConversationsListing::new(self.coordinates()));
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
            Offline(ref l) => write!(f, "{}", l),
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
                clear_area(grid, area, self.theme_default);
                let (x, _) = write_string_to_grid(
                    "offline: ",
                    grid,
                    Color::Byte(243),
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    (set_x(upper_left, mid + 1), bottom_right),
                    None,
                );
                write_string_to_grid(
                    &err.to_string(),
                    grid,
                    Color::Red,
                    self.theme_default.bg,
                    self.theme_default.attrs,
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
                clear_area(
                    grid,
                    (set_x(upper_left, mid + 1), bottom_right),
                    self.theme_default,
                );
                let (x, _) = write_string_to_grid(
                    "offline: ",
                    grid,
                    Color::Byte(243),
                    self.theme_default.bg,
                    self.theme_default.attrs,
                    (set_x(upper_left, mid + 1), bottom_right),
                    None,
                );
                write_string_to_grid(
                    &err.to_string(),
                    grid,
                    Color::Red,
                    self.theme_default.bg,
                    self.theme_default.attrs,
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
                if self.component.coordinates().1 == *f {
                    if !self.startup_checks_rate.tick() {
                        return false;
                    }
                }
            }
            UIEvent::Timer(n) if *n == self.startup_checks_rate.id() => {
                if self.startup_checks_rate.active {
                    self.startup_checks_rate.reset();
                    return self.process_event(
                        &mut UIEvent::StartupCheck(self.component.coordinates().1),
                        context,
                    );
                }
            }
            UIEvent::AccountStatusChange(account_index) => {
                if self.cursor_pos.0 == *account_index {
                    self.change_account(context);
                } else {
                    self.accounts[*account_index].entries = context.accounts[*account_index]
                        .list_mailboxes()
                        .into_iter()
                        .filter(|mailbox_node| {
                            context.accounts[*account_index][&mailbox_node.hash]
                                .ref_mailbox
                                .is_subscribed()
                        })
                        .map(|f| (f.depth, f.hash))
                        .collect::<_>();
                    self.set_dirty(true);
                }
                return true;
            }
            UIEvent::MailboxDelete((account_index, _mailbox_hash))
            | UIEvent::MailboxCreate((account_index, _mailbox_hash)) => {
                self.accounts[*account_index].entries = context.accounts[*account_index]
                    .list_mailboxes()
                    .into_iter()
                    .filter(|mailbox_node| {
                        context.accounts[*account_index][&mailbox_node.hash]
                            .ref_mailbox
                            .is_subscribed()
                    })
                    .map(|f| (f.depth, f.hash))
                    .collect::<_>();
                if self.cursor_pos.0 == *account_index {
                    self.cursor_pos.1 = std::cmp::min(
                        self.accounts[self.cursor_pos.0].entries.len() - 1,
                        self.cursor_pos.1,
                    );
                    self.component.set_coordinates((
                        self.cursor_pos.0,
                        self.accounts[self.cursor_pos.0].entries[self.cursor_pos.1].1,
                    ));
                    self.component.refresh_mailbox(context, true);
                }
                self.set_dirty(true);
                return true;
            }
            _ => {}
        }

        if self.component.process_event(event, context) {
            return true;
        }

        let shortcuts = self.get_shortcuts(context);
        match *event {
            UIEvent::Input(ref k)
                if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_mailbox"])
                    || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_mailbox"]) =>
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
                    k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_mailbox"]) => {
                        if let Some((_, mailbox_hash)) = self.accounts[self.cursor_pos.0]
                            .entries
                            .get(self.cursor_pos.1 + amount)
                        {
                            self.cursor_pos.1 += amount;
                            self.component
                                .set_coordinates((self.cursor_pos.0, *mailbox_hash));
                            self.set_dirty(true);
                        } else {
                            return true;
                        }
                    }
                    k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_mailbox"]) => {
                        if self.cursor_pos.1 >= amount {
                            if let Some((_, mailbox_hash)) = self.accounts[self.cursor_pos.0]
                                .entries
                                .get(self.cursor_pos.1 - amount)
                            {
                                self.cursor_pos.1 -= amount;
                                self.component
                                    .set_coordinates((self.cursor_pos.0, *mailbox_hash));
                                self.set_dirty(true);
                            } else {
                                return true;
                            }
                        } else {
                            return true;
                        }
                    }
                    _ => {}
                }
                if let Some((_, mailbox_hash)) = self.accounts[self.cursor_pos.0]
                    .entries
                    .get(self.cursor_pos.1)
                {
                    /* Account might have no mailboxes yet if it's offline */
                    /* Check if per-mailbox configuration overrides general configuration */
                    let index_style =
                        mailbox_acc_settings!(context[self.cursor_pos.0][mailbox_hash].index_style);
                    self.component.set_style(*index_style);
                }
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context),
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
                        } else {
                            return true;
                        }
                    }
                    k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_account"]) => {
                        if self.cursor_pos.0 >= amount {
                            self.cursor_pos = (self.cursor_pos.0 - amount, 0);
                        } else {
                            return true;
                        }
                    }
                    _ => return false,
                }
                self.change_account(context);

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
                Action::ViewMailbox(idx) => {
                    if let Some((_, mailbox_hash)) =
                        self.accounts[self.cursor_pos.0].entries.get(*idx)
                    {
                        self.cursor_pos.1 = *idx;
                        self.component
                            .set_coordinates((self.cursor_pos.0, *mailbox_hash));
                        self.set_dirty(true);
                    } else {
                        return true;
                    }
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
                    .push_back(UIEvent::ExInput(Key::Paste("search ".to_string())));
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
                if let Some(&mailbox_hash) = account.mailboxes_order.get(self.cursor_pos.1) {
                    if let Err(err) = account.refresh(mailbox_hash) {
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
                        self.get_status(context),
                    )));
            }
            UIEvent::MailboxUpdate(_) => {
                self.dirty = true;
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context),
                    )));
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt('')) if !self.cmd_buf.is_empty() => {
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

    fn get_status(&self, context: &Context) -> String {
        let mailbox_hash = if let Some((_, mailbox_hash)) = self.accounts[self.cursor_pos.0]
            .entries
            .get(self.cursor_pos.1)
        {
            *mailbox_hash
        } else {
            return String::new();
        };

        let account = &context.accounts[self.cursor_pos.0];
        use crate::conf::accounts::MailboxStatus;
        match account[&mailbox_hash].status {
            MailboxStatus::Available | MailboxStatus::Parsing(_, _) => format!(
                "Mailbox: {}, Messages: {}, New: {}",
                account[&mailbox_hash].ref_mailbox.name(),
                account.collection[&mailbox_hash].len(),
                account[&mailbox_hash]
                    .ref_mailbox
                    .count()
                    .ok()
                    .map(|(v, _)| v)
                    .unwrap_or(0),
            ),
            MailboxStatus::Failed(_) | MailboxStatus::None => account[&mailbox_hash].status(),
        }
    }
}

impl From<(IndexStyle, (usize, MailboxHash))> for ListingComponent {
    fn from((index_style, coordinates): (IndexStyle, (usize, MailboxHash))) -> Self {
        match index_style {
            IndexStyle::Plain => Plain(PlainListing::new(coordinates)),
            IndexStyle::Threaded => Threaded(ThreadListing::new(coordinates)),
            IndexStyle::Compact => Compact(CompactListing::new(coordinates)),
            IndexStyle::Conversations => Conversations(ConversationsListing::new(coordinates)),
        }
    }
}

impl Listing {
    pub const DESCRIPTION: &'static str = "listing";
    pub fn new(context: &mut Context) -> Self {
        let account_entries: Vec<AccountMenuEntry> = context
            .accounts
            .iter()
            .enumerate()
            .map(|(i, a)| {
                let entries: SmallVec<[(usize, MailboxHash); 16]> = a
                    .list_mailboxes()
                    .into_iter()
                    .filter(|mailbox_node| a[&mailbox_node.hash].ref_mailbox.is_subscribed())
                    .map(|f| (f.depth, f.hash))
                    .collect::<_>();

                AccountMenuEntry {
                    name: a.name().to_string(),
                    index: i,
                    entries,
                }
            })
            .collect();
        let mut ret = Listing {
            component: Offline(OfflineListing::new((0, 0))),
            accounts: account_entries,
            visible: true,
            dirty: true,
            cursor_pos: (0, 0),
            startup_checks_rate: RateLimit::new(2, 1000),
            theme_default: conf::value(context, "theme_default"),
            id: ComponentId::new_v4(),
            show_divider: false,
            menu_visibility: true,
            ratio: 90,
            cmd_buf: String::with_capacity(4),
        };
        ret.change_account(context);
        ret
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
        let mailboxes: FnvHashMap<MailboxHash, Mailbox> = context.accounts[a.index]
            .mailbox_entries
            .iter()
            .map(|(&hash, entry)| (hash, entry.ref_mailbox.clone()))
            .collect();

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let must_highlight_account: bool = self.cursor_pos.0 == a.index;

        let mut lines: Vec<(usize, usize, MailboxHash, Option<usize>)> = Vec::new();

        for (i, &(depth, mailbox_hash)) in a.entries.iter().enumerate() {
            if mailboxes[&mailbox_hash].is_subscribed() {
                match context.accounts[a.index][&mailbox_hash].status {
                    crate::conf::accounts::MailboxStatus::Failed(_) => {
                        lines.push((depth, i, mailbox_hash, None));
                    }
                    _ => {
                        lines.push((
                            depth,
                            i,
                            mailbox_hash,
                            mailboxes[&mailbox_hash].count().ok().map(|(v, _)| v),
                        ));
                    }
                }
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
                self.theme_default.bg,
                self.theme_default.attrs,
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

                    if !context.settings.terminal.use_color() {
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

            let (depth, inc, mailbox_idx, count) = lines[idx];
            /* Calculate how many columns the mailbox index tags should occupy with right alignment,
             * eg.
             *  1
             *  2
             * ...
             *  9
             * 10
             */
            let total_mailbox_no_digits = {
                let mut len = lines_len;
                let mut ctr = 1;
                while len > 9 {
                    ctr += 1;
                    len /= 10;
                }
                ctr
            };
            let (x, _) = write_string_to_grid(
                &format!("{:>width$}", inc, width = total_mailbox_no_digits),
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
                mailboxes[&mailbox_idx].name(),
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
                        /* Hide part of mailbox name if need be to fit the message count */
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

    fn change_account(&mut self, context: &mut Context) {
        self.accounts[self.cursor_pos.0].entries = context.accounts[self.cursor_pos.0]
            .list_mailboxes()
            .into_iter()
            .filter(|mailbox_node| {
                context.accounts[self.cursor_pos.0][&mailbox_node.hash]
                    .ref_mailbox
                    .is_subscribed()
            })
            .map(|f| (f.depth, f.hash))
            .collect::<_>();
        /* Account might have no mailboxes yet if it's offline */
        if let Some((_, mailbox_hash)) = self.accounts[self.cursor_pos.0]
            .entries
            .get(self.cursor_pos.1)
        {
            self.component
                .set_coordinates((self.cursor_pos.0, *mailbox_hash));
            /* Check if per-mailbox configuration overrides general configuration */

            let index_style =
                mailbox_acc_settings!(context[self.cursor_pos.0][mailbox_hash].index_style);
            self.component.set_style(*index_style);
        } else {
            /* Set to dummy */
            self.component = Offline(OfflineListing::new((self.cursor_pos.0, 0)));
        }
        self.set_dirty(true);
        context
            .replies
            .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(debug!(
                self.get_status(context)
            ))));
    }
}
