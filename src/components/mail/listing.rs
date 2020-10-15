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
use crate::conf::accounts::JobRequest;
use crate::types::segment_tree::SegmentTree;
use melib::backends::EnvelopeHashBatch;
use smallvec::SmallVec;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
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
    selected: ThemeAttribute,
    even: ThemeAttribute,
    odd: ThemeAttribute,
    even_unseen: ThemeAttribute,
    even_highlighted: ThemeAttribute,
    even_selected: ThemeAttribute,
    odd_unseen: ThemeAttribute,
    odd_highlighted: ThemeAttribute,
    odd_selected: ThemeAttribute,
    attachment_flag: ThemeAttribute,
    thread_snooze_flag: ThemeAttribute,
    tag_default: ThemeAttribute,

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
column_str!(struct TagString(String, SmallVec<[Option<Color>; 8]>));

#[derive(Debug)]
struct AccountMenuEntry {
    name: String,
    hash: AccountHash,
    index: usize,
    entries: SmallVec<[(usize, u32, bool, MailboxHash); 16]>,
}

pub trait MailListingTrait: ListingTrait {
    fn perform_action(
        &mut self,
        context: &mut Context,
        thread_hashes: SmallVec<[ThreadHash; 8]>,
        a: &ListingAction,
    ) {
        let account_hash = self.coordinates().0;
        let account = &mut context.accounts[&account_hash];
        let mut envs_to_set: SmallVec<[EnvelopeHash; 8]> = SmallVec::new();
        let mailbox_hash = self.coordinates().1;
        {
            let threads_lck = account.collection.get_threads(mailbox_hash);
            for thread_hash in thread_hashes {
                for (_, h) in threads_lck.thread_group_iter(thread_hash) {
                    envs_to_set.push(threads_lck.thread_nodes()[&h].message().unwrap());
                }
                self.row_updates().push(thread_hash);
            }
        }
        if envs_to_set.is_empty() {
            return;
        }
        let env_hashes = EnvelopeHashBatch::try_from(envs_to_set.as_slice()).unwrap();
        match a {
            ListingAction::SetSeen => {
                let job = account.backend.write().unwrap().set_flags(
                    env_hashes.clone(),
                    mailbox_hash,
                    smallvec::smallvec![(Ok(Flag::SEEN), true)],
                );
                match job {
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                    Ok(fut) => {
                        let handle = account.job_executor.spawn_specialized(fut);
                        account
                            .insert_job(handle.job_id, JobRequest::SetFlags { env_hashes, handle });
                    }
                }
            }
            ListingAction::SetUnseen => {
                let job = account.backend.write().unwrap().set_flags(
                    env_hashes.clone(),
                    mailbox_hash,
                    smallvec::smallvec![(Ok(Flag::SEEN), false)],
                );
                match job {
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                    Ok(fut) => {
                        let handle = account.job_executor.spawn_specialized(fut);
                        account
                            .insert_job(handle.job_id, JobRequest::SetFlags { env_hashes, handle });
                    }
                }
            }
            ListingAction::Tag(Remove(ref tag_str)) => {
                let job = account.backend.write().unwrap().set_flags(
                    env_hashes.clone(),
                    mailbox_hash,
                    smallvec::smallvec![(Err(tag_str.to_string()), false)],
                );
                match job {
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                    Ok(fut) => {
                        let handle = account.job_executor.spawn_specialized(fut);
                        account
                            .insert_job(handle.job_id, JobRequest::SetFlags { env_hashes, handle });
                    }
                }
            }
            ListingAction::Tag(Add(ref tag_str)) => {
                let job = account.backend.write().unwrap().set_flags(
                    env_hashes.clone(),
                    mailbox_hash,
                    smallvec::smallvec![(Err(tag_str.to_string()), true)],
                );
                match job {
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                    Ok(fut) => {
                        let handle = account.job_executor.spawn_specialized(fut);
                        account
                            .insert_job(handle.job_id, JobRequest::SetFlags { env_hashes, handle });
                    }
                }
            }
            ListingAction::Delete => {
                let job = account
                    .backend
                    .write()
                    .unwrap()
                    .delete_messages(env_hashes.clone(), mailbox_hash);
                match job {
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                    Ok(fut) => {
                        let handle = account.job_executor.spawn_specialized(fut);
                        account.insert_job(
                            handle.job_id,
                            JobRequest::DeleteMessages { env_hashes, handle },
                        );
                    }
                }
            }
            ListingAction::CopyTo(ref mailbox_path) => {
                match account
                    .mailbox_by_path(mailbox_path)
                    .and_then(|destination_mailbox_hash| {
                        account.backend.write().unwrap().copy_messages(
                            env_hashes,
                            mailbox_hash,
                            destination_mailbox_hash,
                            /* move? */ false,
                        )
                    }) {
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                    Ok(fut) => {
                        let handle = account.job_executor.spawn_specialized(fut);
                        account.insert_job(
                            handle.job_id,
                            JobRequest::Generic {
                                name: "message copying".into(),
                                handle,
                                on_finish: None,
                                logging_level: melib::LoggingLevel::INFO,
                            },
                        );
                    }
                }
            }
            ListingAction::CopyToOtherAccount(ref _account_name, ref _mailbox_path) => {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                        "Unimplemented.".into(),
                    )));
            }
            ListingAction::MoveTo(ref mailbox_path) => {
                match account
                    .mailbox_by_path(mailbox_path)
                    .and_then(|destination_mailbox_hash| {
                        account.backend.write().unwrap().copy_messages(
                            env_hashes,
                            mailbox_hash,
                            destination_mailbox_hash,
                            /* move? */ true,
                        )
                    }) {
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                    Ok(fut) => {
                        let handle = account.job_executor.spawn_specialized(fut);
                        account.insert_job(
                            handle.job_id,
                            JobRequest::Generic {
                                name: "message moving".into(),
                                handle,
                                on_finish: None,
                                logging_level: melib::LoggingLevel::INFO,
                            },
                        );
                    }
                }
            }
            ListingAction::MoveToOtherAccount(ref _account_name, ref _mailbox_path) => {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                        "Unimplemented.".into(),
                    )));
            }
            _ => unreachable!(),
        }
        self.set_dirty(true);
    }

    fn row_updates(&mut self) -> &mut SmallVec<[ThreadHash; 8]>;
    fn selection(&mut self) -> &mut HashMap<ThreadHash, bool>;
    fn get_focused_items(&self, _context: &Context) -> SmallVec<[ThreadHash; 8]>;
    fn redraw_threads_list(
        &mut self,
        context: &Context,
        items: Box<dyn Iterator<Item = ThreadHash>>,
    );

    fn redraw_envelope_list(
        &mut self,
        _context: &Context,
        _items: Box<dyn Iterator<Item = EnvelopeHash>>,
    ) {
    }

    /// Use `force` when there have been changes in the mailbox or account lists in `context`
    fn refresh_mailbox(&mut self, context: &mut Context, force: bool);
}

pub trait ListingTrait: Component {
    fn coordinates(&self) -> (AccountHash, MailboxHash);
    fn set_coordinates(&mut self, _: (AccountHash, MailboxHash));
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context);
    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context);
    fn filter(
        &mut self,
        _filter_term: String,
        _results: Result<SmallVec<[EnvelopeHash; 512]>>,
        _context: &Context,
    ) {
    }
    fn set_command_modifier(&mut self, _is_active: bool) {}
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

#[derive(PartialEq, Debug)]
enum ListingFocus {
    Menu,
    Mailbox,
}

#[derive(Debug)]
pub struct Listing {
    component: ListingComponent,
    accounts: Vec<AccountMenuEntry>,
    dirty: bool,
    visible: bool,
    cursor_pos: (usize, usize),
    menu_cursor_pos: (usize, usize),
    startup_checks_rate: RateLimit,
    id: ComponentId,
    theme_default: ThemeAttribute,

    show_divider: bool,
    menu_visibility: bool,
    cmd_buf: String,
    /// This is the width of the right container to the entire width.
    ratio: usize, // right/(container width) * 100
    menu_width: WidgetWidth,
    focus: ListingFocus,
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
            if self.focus == ListingFocus::Menu {
                (self.ratio * total_cols) / 100
            } else {
                match self.menu_width {
                    WidgetWidth::Set(ref mut v) | WidgetWidth::Hold(ref mut v) => {
                        if *v == 0 {
                            *v = 1;
                        } else if *v >= total_cols {
                            *v = total_cols.saturating_sub(2);
                        }
                        total_cols.saturating_sub(*v)
                    }
                    WidgetWidth::Unset => {
                        self.menu_width =
                            WidgetWidth::Set(total_cols - ((self.ratio * total_cols) / 100));
                        (self.ratio * total_cols) / 100
                    }
                }
            }
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

        let account_hash = self.accounts[self.cursor_pos.0].hash;
        if right_component_width == total_cols {
            if context.is_online(account_hash).is_err() {
                match self.component {
                    ListingComponent::Offline(_) => {}
                    _ => {
                        self.component = Offline(OfflineListing::new((account_hash, 0)));
                    }
                }
            }

            self.component.draw(grid, area, context);
        } else if right_component_width == 0 {
            self.draw_menu(grid, area, context);
        } else {
            self.draw_menu(grid, (upper_left, (mid, get_y(bottom_right))), context);
            if context.is_online(account_hash).is_err() {
                match self.component {
                    ListingComponent::Offline(_) => {}
                    _ => {
                        self.component = Offline(OfflineListing::new((account_hash, 0)));
                    }
                }
            }
            self.component
                .draw(grid, (set_x(upper_left, mid + 1), bottom_right), context);
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
            UIEvent::AccountStatusChange(account_hash) => {
                let account_index: usize = context
                    .accounts
                    .get_index_of(account_hash)
                    .expect("Invalid account_hash in UIEventMailbox{Delete,Create}");
                if self.cursor_pos.0 == account_index {
                    self.change_account(context);
                } else {
                    self.accounts[account_index].entries = context.accounts[&*account_hash]
                        .list_mailboxes()
                        .into_iter()
                        .filter(|mailbox_node| {
                            context.accounts[&*account_hash][&mailbox_node.hash]
                                .ref_mailbox
                                .is_subscribed()
                        })
                        .map(|f| (f.depth, f.indentation, f.has_sibling, f.hash))
                        .collect::<_>();
                    self.set_dirty(true);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                            self.get_status(context),
                        )));
                }
                return true;
            }
            UIEvent::MailboxDelete((account_hash, _mailbox_hash))
            | UIEvent::MailboxCreate((account_hash, _mailbox_hash)) => {
                let account_index = context
                    .accounts
                    .get_index_of(account_hash)
                    .expect("Invalid account_hash in UIEventMailbox{Delete,Create}");
                self.accounts[account_index].entries = context.accounts[&*account_hash]
                    .list_mailboxes()
                    .into_iter()
                    .filter(|mailbox_node| {
                        context.accounts[&*account_hash][&mailbox_node.hash]
                            .ref_mailbox
                            .is_subscribed()
                    })
                    .map(|f| (f.depth, f.indentation, f.has_sibling, f.hash))
                    .collect::<_>();
                if self.cursor_pos.0 == account_index {
                    self.cursor_pos.1 = std::cmp::min(
                        self.accounts[self.cursor_pos.0].entries.len() - 1,
                        self.cursor_pos.1,
                    );
                    self.component.set_coordinates((
                        self.accounts[self.cursor_pos.0].hash,
                        self.accounts[self.cursor_pos.0].entries[self.cursor_pos.1].3,
                    ));
                    self.component.refresh_mailbox(context, true);
                }
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context),
                    )));
                self.set_dirty(true);
                return true;
            }
            _ => {}
        }

        if self.focus == ListingFocus::Mailbox && self.component.process_event(event, context) {
            return true;
        }

        let shortcuts = self.get_shortcuts(context);
        if self.focus == ListingFocus::Mailbox {
            match *event {
                UIEvent::Input(Key::Mouse(MouseEvent::Press(MouseButton::Left, x, _y)))
                    if self.menu_visibility =>
                {
                    match self.menu_width {
                        WidgetWidth::Hold(wx) | WidgetWidth::Set(wx)
                            if wx + 1 == usize::from(x) =>
                        {
                            self.menu_width = WidgetWidth::Hold(wx - 1);
                        }
                        WidgetWidth::Set(_) => return false,
                        WidgetWidth::Hold(x) => {
                            self.menu_width = WidgetWidth::Set(x);
                        }
                        WidgetWidth::Unset => return false,
                    }
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(Key::Mouse(MouseEvent::Hold(x, _y))) if self.menu_visibility => {
                    match self.menu_width {
                        WidgetWidth::Hold(ref mut hx) => {
                            *hx = usize::from(x).saturating_sub(1);
                        }
                        _ => return false,
                    }
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(Key::Mouse(MouseEvent::Release(x, _y))) if self.menu_visibility => {
                    match self.menu_width {
                        WidgetWidth::Hold(_) => {
                            self.menu_width = WidgetWidth::Set(usize::from(x).saturating_sub(1));
                        }
                        _ => return false,
                    }
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(Key::Left) if self.menu_visibility => {
                    self.focus = ListingFocus::Menu;
                    self.ratio = 50;
                    self.set_dirty(true);
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_mailbox"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_mailbox"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    match k {
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_mailbox"]) => {
                            if self.accounts[self.cursor_pos.0]
                                .entries
                                .get(self.cursor_pos.1 + amount)
                                .is_some()
                            {
                                self.cursor_pos.1 += amount;
                            } else {
                                return true;
                            }
                        }
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_mailbox"]) => {
                            if self.cursor_pos.1 >= amount {
                                if self.accounts[self.cursor_pos.0]
                                    .entries
                                    .get(self.cursor_pos.1 - amount)
                                    .is_some()
                                {
                                    self.cursor_pos.1 -= amount;
                                } else {
                                    return true;
                                }
                            } else {
                                return true;
                            }
                        }
                        _ => {}
                    }
                    self.change_account(context);
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
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
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
                    Action::Listing(ListingAction::Import(file_path, mailbox_path)) => {
                        let account = &mut context.accounts[self.cursor_pos.0];
                        if let Err(err) = account
                            .mailbox_by_path(&mailbox_path)
                            .and_then(|mailbox_hash| {
                                Ok((
                                    std::fs::read(&file_path).chain_err_summary(|| {
                                        format!("Could not read {}", file_path.display())
                                    })?,
                                    mailbox_hash,
                                ))
                            })
                            .and_then(|(bytes, mailbox_hash)| {
                                account.save(&bytes, mailbox_hash, None)
                            })
                        {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(err.to_string()),
                            ));
                        }
                        return true;
                    }
                    Action::Listing(a @ ListingAction::SetSeen)
                    | Action::Listing(a @ ListingAction::SetUnseen)
                    | Action::Listing(a @ ListingAction::Delete)
                    | Action::Listing(a @ ListingAction::CopyTo(_))
                    | Action::Listing(a @ ListingAction::MoveTo(_))
                    | Action::Listing(a @ ListingAction::CopyToOtherAccount(_, _))
                    | Action::Listing(a @ ListingAction::MoveToOtherAccount(_, _))
                    | Action::Listing(a @ ListingAction::Tag(_)) => {
                        let focused = self.component.get_focused_items(context);
                        self.component.perform_action(context, focused, a);
                        let mut row_updates: SmallVec<[ThreadHash; 8]> = SmallVec::new();
                        for (k, v) in self.component.selection().iter_mut() {
                            if *v {
                                *v = false;
                                row_updates.push(*k);
                            }
                        }
                        self.component.row_updates().extend(row_updates.drain(..));
                        self.component.set_dirty(true);
                        return true;
                    }
                    Action::ViewMailbox(idx) => {
                        if let Some((_, _, _, mailbox_hash)) =
                            self.accounts[self.cursor_pos.0].entries.get(*idx)
                        {
                            let account_hash = self.accounts[self.cursor_pos.0].hash;
                            self.cursor_pos.1 = *idx;
                            self.component
                                .set_coordinates((account_hash, *mailbox_hash));
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
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
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
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
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
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        mult
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
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
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        mult
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
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
                    if shortcut!(
                        k == shortcuts[Listing::DESCRIPTION]["toggle_menu_visibility"]
                    ) =>
                {
                    self.menu_visibility = !self.menu_visibility;
                    self.set_dirty(true);
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Listing::DESCRIPTION]["search"]) =>
                {
                    context
                        .replies
                        .push_back(UIEvent::CmdInput(Key::Paste("search ".to_string())));
                    context
                        .replies
                        .push_back(UIEvent::ChangeMode(UIMode::Command));
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
                                Some(NotificationType::Error(err.kind)),
                            ));
                        }
                    }
                    return true;
                }
                _ => {}
            }
        } else if self.focus == ListingFocus::Menu {
            match *event {
                UIEvent::Input(Key::Right) => {
                    self.focus = ListingFocus::Mailbox;
                    self.ratio = 90;
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["open_mailbox"]) =>
                {
                    self.cursor_pos = self.menu_cursor_pos;
                    self.change_account(context);
                    self.focus = ListingFocus::Mailbox;
                    self.ratio = 90;
                    self.set_dirty(true);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                            self.get_status(context),
                        )));
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["scroll_up"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["scroll_down"]) =>
                {
                    let mut amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["scroll_up"]) {
                        while amount > 0 {
                            if self.menu_cursor_pos.1 > 0 {
                                self.menu_cursor_pos.1 -= 1;
                            } else if self.menu_cursor_pos.0 > 0 {
                                self.menu_cursor_pos.0 -= 1;
                                self.menu_cursor_pos.1 = self.accounts[self.menu_cursor_pos.0]
                                    .entries
                                    .len()
                                    .saturating_sub(1);
                            } else {
                                return true;
                            }
                            amount -= 1;
                        }
                    } else if shortcut!(k == shortcuts[Listing::DESCRIPTION]["scroll_down"]) {
                        while amount > 0 {
                            if self.menu_cursor_pos.1 + 1
                                < self.accounts[self.menu_cursor_pos.0].entries.len()
                            {
                                self.menu_cursor_pos.1 += 1;
                            } else if self.menu_cursor_pos.0 + 1 < self.accounts.len() {
                                self.menu_cursor_pos.0 += 1;
                                self.menu_cursor_pos.1 = 0;
                            } else {
                                return true;
                            }
                            amount -= 1;
                        }
                    }
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_mailbox"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_mailbox"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    match k {
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_mailbox"]) => {
                            if self.accounts[self.menu_cursor_pos.0]
                                .entries
                                .get(self.menu_cursor_pos.1 + amount)
                                .is_some()
                            {
                                self.menu_cursor_pos.1 += amount;
                                self.set_dirty(true);
                            } else {
                                return true;
                            }
                        }
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_mailbox"]) => {
                            if self.cursor_pos.1 >= amount {
                                if self.accounts[self.menu_cursor_pos.0]
                                    .entries
                                    .get(self.menu_cursor_pos.1 - amount)
                                    .is_some()
                                {
                                    self.menu_cursor_pos.1 -= amount;
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
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_account"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_account"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_page"])
                        || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_page"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_command_modifier(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    match k {
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_account"])
                            || shortcut!(k == shortcuts[Listing::DESCRIPTION]["next_page"]) =>
                        {
                            if self.menu_cursor_pos.0 + amount < self.accounts.len() {
                                self.menu_cursor_pos = (self.menu_cursor_pos.0 + amount, 0);
                            } else {
                                return true;
                            }
                        }
                        k if shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_account"])
                            || shortcut!(k == shortcuts[Listing::DESCRIPTION]["prev_page"]) =>
                        {
                            if self.menu_cursor_pos.0 >= amount {
                                self.menu_cursor_pos = (self.menu_cursor_pos.0 - amount, 0);
                            } else {
                                return true;
                            }
                        }
                        _ => return false,
                    }
                    self.set_dirty(true);

                    return true;
                }
                _ => {}
            }
        }
        match *event {
            UIEvent::Input(ref k)
                if shortcut!(k == shortcuts[Listing::DESCRIPTION]["new_mail"]) =>
            {
                let account_hash = context.accounts[self.cursor_pos.0].hash();
                let composer = Composer::new(account_hash, context);
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(New(Some(Box::new(composer))))));
                return true;
            }
            UIEvent::StartupCheck(_)
            | UIEvent::MailboxUpdate(_)
            | UIEvent::EnvelopeUpdate(_)
            | UIEvent::EnvelopeRename(_, _)
            | UIEvent::EnvelopeRemove(_, _) => {
                self.dirty = true;
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.get_status(context),
                    )));
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt('')) if !self.cmd_buf.is_empty() => {
                self.cmd_buf.clear();
                self.component.set_command_modifier(false);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                return true;
            }
            UIEvent::Input(Key::Char(c)) if c >= '0' && c <= '9' => {
                self.cmd_buf.push(c);
                self.component.set_command_modifier(true);
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
        let mut config_map = context.settings.shortcuts.listing.key_values();
        if self.focus != ListingFocus::Menu {
            config_map.remove("open_mailbox");
        }
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
        let mailbox_hash = if let Some((_, _, _, mailbox_hash)) = self.accounts[self.cursor_pos.0]
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
            MailboxStatus::Available | MailboxStatus::Parsing(_, _) => {
                let (unseen, total) = account[&mailbox_hash]
                    .ref_mailbox
                    .count()
                    .ok()
                    .unwrap_or((0, 0));
                format!(
                    "Mailbox: {}, Messages: {}, New: {}",
                    account[&mailbox_hash].name(),
                    total,
                    unseen
                )
            }
            MailboxStatus::Failed(_) | MailboxStatus::None => account[&mailbox_hash].status(),
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
            .map(|(i, (h, a))| {
                let entries: SmallVec<[(usize, u32, bool, MailboxHash); 16]> = a
                    .list_mailboxes()
                    .into_iter()
                    .filter(|mailbox_node| a[&mailbox_node.hash].ref_mailbox.is_subscribed())
                    .map(|f| (f.depth, f.indentation, f.has_sibling, f.hash))
                    .collect::<_>();

                AccountMenuEntry {
                    name: a.name().to_string(),
                    hash: *h,
                    index: i,
                    entries,
                }
            })
            .collect();
        let mut ret = Listing {
            component: Offline(OfflineListing::new((account_entries[0].hash, 0))),
            accounts: account_entries,
            visible: true,
            dirty: true,
            cursor_pos: (0, 0),
            menu_cursor_pos: (0, 0),
            startup_checks_rate: RateLimit::new(2, 1000),
            theme_default: conf::value(context, "theme_default"),
            id: ComponentId::new_v4(),
            show_divider: false,
            menu_visibility: true,
            ratio: 90,
            menu_width: WidgetWidth::Unset,
            focus: ListingFocus::Mailbox,
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
        let mailboxes: HashMap<MailboxHash, Mailbox> = context.accounts[a.index]
            .mailbox_entries
            .iter()
            .map(|(&hash, entry)| (hash, entry.ref_mailbox.clone()))
            .collect();

        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let must_highlight_account: bool = (self.focus == ListingFocus::Mailbox
            && self.cursor_pos.0 == a.index)
            || (self.focus == ListingFocus::Menu && self.menu_cursor_pos.0 == a.index);

        let mut lines: Vec<(usize, usize, u32, bool, MailboxHash, Option<usize>)> = Vec::new();

        for (i, &(depth, indentation, has_sibling, mailbox_hash)) in a.entries.iter().enumerate() {
            if mailboxes[&mailbox_hash].is_subscribed() {
                match context.accounts[a.index][&mailbox_hash].status {
                    crate::conf::accounts::MailboxStatus::Failed(_) => {
                        lines.push((depth, i, indentation, has_sibling, mailbox_hash, None));
                    }
                    _ => {
                        lines.push((
                            depth,
                            i,
                            indentation,
                            has_sibling,
                            mailbox_hash,
                            mailboxes[&mailbox_hash].count().ok().map(|(v, _)| v),
                        ));
                    }
                }
            }
        }

        let account_attrs = if must_highlight_account {
            crate::conf::value(context, "mail.sidebar_highlighted_account_name")
        } else {
            crate::conf::value(context, "mail.sidebar_account_name")
        };
        /* Print account name first */
        write_string_to_grid(
            &a.name,
            grid,
            account_attrs.fg,
            account_attrs.bg,
            account_attrs.attrs,
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
        let mut branches = String::with_capacity(16);

        for y in get_y(upper_left) + 1..get_y(bottom_right) {
            if idx == lines_len {
                break;
            }
            let (att, index_att, unread_count_att) = if must_highlight_account {
                if (self.focus == ListingFocus::Mailbox && self.cursor_pos.1 == idx)
                    || (self.focus == ListingFocus::Menu && self.menu_cursor_pos.1 == idx)
                {
                    let mut ret = (
                        crate::conf::value(context, "mail.sidebar_highlighted"),
                        crate::conf::value(context, "mail.sidebar_highlighted_index"),
                        crate::conf::value(context, "mail.sidebar_highlighted_unread_count"),
                    );

                    if !context.settings.terminal.use_color() {
                        ret.0.attrs |= Attr::REVERSE;
                        ret.1.attrs |= Attr::REVERSE;
                        ret.2.attrs |= Attr::REVERSE;
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

            let (depth, inc, indentation, has_sibling, mailbox_idx, count) = lines[idx];
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

            let has_sibling_str: &str =
                account_settings!(context[a.hash].listing.sidebar_mailbox_tree_has_sibling)
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(" ");
            let no_sibling_str: &str =
                account_settings!(context[a.hash].listing.sidebar_mailbox_tree_no_sibling)
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(" ");

            let has_sibling_leaf_str: &str = account_settings!(
                context[a.hash]
                    .listing
                    .sidebar_mailbox_tree_has_sibling_leaf
            )
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or(" ");

            let no_sibling_leaf_str: &str =
                account_settings!(context[a.hash].listing.sidebar_mailbox_tree_no_sibling_leaf)
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(" ");

            let (x, _) = write_string_to_grid(
                &format!("{:>width$}", inc, width = total_mailbox_no_digits),
                grid,
                index_att.fg,
                index_att.bg,
                index_att.attrs,
                (set_y(upper_left, y), bottom_right),
                None,
            );
            {
                branches.clear();
                branches.push_str(no_sibling_str);
                let mut o = 1;
                let leading_zeros = indentation.leading_zeros();
                for _ in 0..(30_u32.saturating_sub(leading_zeros)) {
                    if indentation & o > 0 {
                        branches.push_str(has_sibling_str);
                    } else {
                        branches.push_str(no_sibling_str);
                    }
                    o <<= 1;
                }
                if depth > 0 {
                    if has_sibling {
                        branches.push_str(has_sibling_leaf_str);
                    } else {
                        branches.push_str(no_sibling_leaf_str);
                    }
                }
            }
            let (x, _) = write_string_to_grid(
                &branches,
                grid,
                att.fg,
                att.bg,
                att.attrs,
                ((x, y), bottom_right),
                None,
            );
            let (x, _) = write_string_to_grid(
                context.accounts[a.index].mailbox_entries[&mailbox_idx].name(),
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
                        Attr::BOLD
                    } else {
                        Attr::DEFAULT
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
        let account_hash = context.accounts[self.cursor_pos.0].hash();
        self.accounts[self.cursor_pos.0].entries = context.accounts[self.cursor_pos.0]
            .list_mailboxes()
            .into_iter()
            .filter(|mailbox_node| {
                context.accounts[self.cursor_pos.0][&mailbox_node.hash]
                    .ref_mailbox
                    .is_subscribed()
            })
            .map(|f| (f.depth, f.indentation, f.has_sibling, f.hash))
            .collect::<_>();
        /* Account might have no mailboxes yet if it's offline */
        if let Some((_, _, _, mailbox_hash)) = self.accounts[self.cursor_pos.0]
            .entries
            .get(self.cursor_pos.1)
        {
            self.component
                .set_coordinates((account_hash, *mailbox_hash));
            /* Check if per-mailbox configuration overrides general configuration */

            let index_style =
                mailbox_settings!(context[account_hash][mailbox_hash].listing.index_style);
            self.component.set_style(*index_style);
        } else {
            /* Set to dummy */
            self.component = Offline(OfflineListing::new((account_hash, 0)));
        }
        self.set_dirty(true);
        context
            .replies
            .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                self.get_status(context),
            )));
        self.menu_cursor_pos = self.cursor_pos;
    }
}
