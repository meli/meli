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

mod compact;
pub use self::compact::*;

mod thread;
pub use self::thread::*;

mod plain;
pub use self::plain::*;

#[derive(Debug, Default, Clone)]
pub(in crate::listing) struct DataColumns {
    columns: [CellBuffer; 12],
    widths: [usize; 12], // widths of columns calculated in first draw and after size changes
}

#[derive(Debug)]
struct AccountMenuEntry {
    name: String,
    // Index in the config account vector.
    index: usize,
}

trait ListingTrait {
    fn coordinates(&self) -> (usize, usize, Option<EnvelopeHash>);
    fn set_coordinates(&mut self, _: (usize, usize, Option<EnvelopeHash>));
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context);
    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context);
}

#[derive(Debug)]
pub enum ListingComponent {
    Plain(PlainListing),
    Threaded(ThreadListing),
    Compact(CompactListing),
}
use crate::ListingComponent::*;

impl ListingTrait for ListingComponent {
    fn coordinates(&self) -> (usize, usize, Option<EnvelopeHash>) {
        match &self {
            Compact(ref l) => l.coordinates(),
            Plain(ref l) => l.coordinates(),
            Threaded(ref l) => l.coordinates(),
        }
    }
    fn set_coordinates(&mut self, c: (usize, usize, Option<EnvelopeHash>)) {
        match self {
            Compact(ref mut l) => l.set_coordinates(c),
            Plain(ref mut l) => l.set_coordinates(c),
            Threaded(ref mut l) => l.set_coordinates(c),
        }
    }
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        match self {
            Compact(ref mut l) => l.draw_list(grid, area, context),
            Plain(ref mut l) => l.draw_list(grid, area, context),
            Threaded(ref mut l) => l.draw_list(grid, area, context),
        }
    }
    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context) {
        match self {
            Compact(ref mut l) => l.highlight_line(grid, area, idx, context),
            Plain(ref mut l) => l.highlight_line(grid, area, idx, context),
            Threaded(ref mut l) => l.highlight_line(grid, area, idx, context),
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
    id: ComponentId,

    show_divider: bool,
    menu_visibility: bool,
    /// This is the width of the right container to the entire width.
    ratio: usize, // right/(container width) * 100
}

impl fmt::Display for Listing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.component {
            Compact(ref l) => write!(f, "{}", l),
            Plain(ref l) => write!(f, "{}", l),
            Threaded(ref l) => write!(f, "{}", l),
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
            (self.ratio * total_cols) / 100
        } else {
            total_cols
        };
        let mid = get_x(bottom_right) - right_component_width;
        if self.dirty && mid != get_x(upper_left) {
            if self.show_divider {
                for i in get_y(upper_left)..=get_y(bottom_right) {
                    grid[(mid, i)].set_ch(VERT_BOUNDARY);
                    grid[(mid, i)].set_fg(Color::Default);
                    grid[(mid, i)].set_bg(Color::Default);
                }
            } else {
                for i in get_y(upper_left)..=get_y(bottom_right) {
                    grid[(mid, i)].set_fg(Color::Default);
                    grid[(mid, i)].set_bg(Color::Default);
                }
            }
            context
                .dirty_areas
                .push_back(((mid, get_y(upper_left)), (mid, get_y(bottom_right))));
        }
        self.dirty = false;
        if right_component_width == total_cols {
            match self.component {
                Compact(ref mut l) => l.draw(grid, area, context),
                Plain(ref mut l) => l.draw(grid, area, context),
                Threaded(ref mut l) => l.draw(grid, area, context),
            }
        } else if right_component_width == 0 {
            self.draw_menu(grid, area, context);
        } else {
            self.draw_menu(grid, (upper_left, (mid, get_y(bottom_right))), context);
            match self.component {
                Compact(ref mut l) => {
                    l.draw(grid, (set_x(upper_left, mid + 1), bottom_right), context)
                }
                Plain(ref mut l) => {
                    l.draw(grid, (set_x(upper_left, mid + 1), bottom_right), context)
                }
                Threaded(ref mut l) => {
                    l.draw(grid, (set_x(upper_left, mid + 1), bottom_right), context)
                }
            }
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if match self.component {
            Plain(ref mut l) => l.process_event(event, context),
            Compact(ref mut l) => l.process_event(event, context),
            Threaded(ref mut l) => l.process_event(event, context),
        } {
            return true;
        }

        let shortcuts = &self.get_shortcuts(context)[Listing::DESCRIPTION];
        match *event {
            UIEvent::Input(ref k)
                if k == shortcuts["next_folder"] || k == shortcuts["prev_folder"] =>
            {
                let folder_length = context.accounts[self.cursor_pos.0].len();
                match k {
                    k if k == shortcuts["next_folder"] && folder_length > 0 => {
                        if self.cursor_pos.1 < folder_length - 1 {
                            self.cursor_pos.1 += 1;
                            self.component.set_coordinates((
                                self.cursor_pos.0,
                                self.cursor_pos.1,
                                None,
                            ));
                            self.set_dirty();
                        } else {
                            return true;
                        }
                    }
                    k if k == shortcuts["prev_folder"] => {
                        if self.cursor_pos.1 > 0 {
                            self.cursor_pos.1 -= 1;
                            self.component.set_coordinates((
                                self.cursor_pos.0,
                                self.cursor_pos.1,
                                None,
                            ));
                            self.set_dirty();
                        } else {
                            return true;
                        }
                    }
                    _ => return false,
                }
                let folder_hash =
                    context.accounts[self.cursor_pos.0].folders_order[self.cursor_pos.1];
                // Inform State that we changed the current folder view.
                context
                    .replies
                    .push_back(UIEvent::RefreshMailbox((self.cursor_pos.0, folder_hash)));
                return true;
            }
            UIEvent::Input(ref k)
                if k == shortcuts["next_account"] || k == shortcuts["prev_account"] =>
            {
                match k {
                    k if k == shortcuts["next_account"] => {
                        if self.cursor_pos.0 < self.accounts.len() - 1 {
                            self.cursor_pos = (self.cursor_pos.0 + 1, 0);
                            self.component.set_coordinates((self.cursor_pos.0, 0, None));
                            self.set_dirty();
                        } else {
                            return true;
                        }
                    }
                    k if k == shortcuts["prev_account"] => {
                        if self.cursor_pos.0 > 0 {
                            self.cursor_pos = (self.cursor_pos.0 - 1, 0);
                            self.component.set_coordinates((self.cursor_pos.0, 0, None));
                            self.set_dirty();
                        } else {
                            return true;
                        }
                    }
                    _ => return false,
                }
                let folder_hash =
                    context.accounts[self.cursor_pos.0].folders_order[self.cursor_pos.1];
                // Inform State that we changed the current folder view.
                context
                    .replies
                    .push_back(UIEvent::RefreshMailbox((self.cursor_pos.0, folder_hash)));
                return true;
            }
            UIEvent::Action(ref action) => match action {
                Action::Listing(ListingAction::SetPlain) => {
                    if let Plain(_) = self.component {
                        return true;
                    }
                    let mut new_l = PlainListing::default();
                    new_l.set_coordinates((self.cursor_pos.0, self.cursor_pos.1, None));
                    self.component = Plain(new_l);
                    return true;
                }
                Action::Listing(ListingAction::SetThreaded) => {
                    if let Threaded(_) = self.component {
                        return true;
                    }
                    let mut new_l = ThreadListing::default();
                    new_l.set_coordinates((self.cursor_pos.0, self.cursor_pos.1, None));
                    self.component = Threaded(new_l);
                    return true;
                }
                Action::Listing(ListingAction::SetCompact) => {
                    if let Compact(_) = self.component {
                        return true;
                    }
                    let mut new_l = CompactListing::default();
                    new_l.set_coordinates((self.cursor_pos.0, self.cursor_pos.1, None));
                    self.component = Compact(new_l);
                    return true;
                }
                _ => {}
            },
            UIEvent::RefreshMailbox((idxa, folder_hash)) => {
                self.cursor_pos = (
                    idxa,
                    context.accounts[idxa]
                        .folders_order
                        .iter()
                        .position(|&h| h == folder_hash)
                        .unwrap_or(0),
                );
                self.dirty = true;
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Input(ref k) if k == shortcuts["toggle-menu-visibility"] => {
                self.menu_visibility = !self.menu_visibility;
                self.set_dirty();
            }
            UIEvent::Input(ref k) if k == shortcuts["new_mail"] => {
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(NewDraft(self.cursor_pos.0, None))));
                return true;
            }
            UIEvent::StartupCheck(_) => {
                self.dirty = true;
            }
            UIEvent::MailboxUpdate(_) => {
                self.dirty = true;
            }
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty
            || match self.component {
                Compact(ref l) => l.is_dirty(),
                Plain(ref l) => l.is_dirty(),
                Threaded(ref l) => l.is_dirty(),
            }
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
        match self.component {
            Compact(ref mut l) => l.set_dirty(),
            Plain(ref mut l) => l.set_dirty(),
            Threaded(ref mut l) => l.set_dirty(),
        }
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = match self.component {
            Compact(ref l) => l.get_shortcuts(context),
            Plain(ref l) => l.get_shortcuts(context),
            Threaded(ref l) => l.get_shortcuts(context),
        };
        let config_map = context.settings.shortcuts.listing.key_values();
        map.insert(
            Listing::DESCRIPTION.to_string(),
            [
                (
                    "new_mail",
                    if let Some(key) = config_map.get("new_mail") {
                        (*key).clone()
                    } else {
                        Key::Char('m')
                    },
                ),
                (
                    "prev_folder",
                    if let Some(key) = config_map.get("prev_folder") {
                        (*key).clone()
                    } else {
                        Key::Char('K')
                    },
                ),
                (
                    "next_folder",
                    if let Some(key) = config_map.get("next_folder") {
                        (*key).clone()
                    } else {
                        Key::Char('J')
                    },
                ),
                (
                    "prev_account",
                    if let Some(key) = config_map.get("prev_account") {
                        (*key).clone()
                    } else {
                        Key::Char('l')
                    },
                ),
                (
                    "next_account",
                    if let Some(key) = config_map.get("next_account") {
                        (*key).clone()
                    } else {
                        Key::Char('h')
                    },
                ),
                ("toggle-menu-visibility", Key::Char('`')),
            ]
            .iter()
            .cloned()
            .collect(),
        );

        map
    }

    fn id(&self) -> ComponentId {
        match self.component {
            Compact(ref l) => l.id(),
            Plain(ref l) => l.id(),
            Threaded(ref l) => l.id(),
        }
    }
    fn set_id(&mut self, id: ComponentId) {
        match self.component {
            Compact(ref mut l) => l.set_id(id),
            Plain(ref mut l) => l.set_id(id),
            Threaded(ref mut l) => l.set_id(id),
        }
    }
}

impl From<IndexStyle> for ListingComponent {
    fn from(index_style: IndexStyle) -> Self {
        match index_style {
            IndexStyle::Plain => Plain(Default::default()),
            IndexStyle::Threaded => Threaded(Default::default()),
            IndexStyle::Compact => Compact(Default::default()),
        }
    }
}

impl Listing {
    const DESCRIPTION: &'static str = "listing";
    pub fn new(accounts: &[Account]) -> Self {
        let accounts = accounts
            .iter()
            .enumerate()
            .map(|(i, a)| AccountMenuEntry {
                name: a.name().to_string(),
                index: i,
            })
            .collect();
        Listing {
            component: Compact(Default::default()),
            accounts,
            visible: true,
            dirty: true,
            cursor_pos: (0, 0),
            id: ComponentId::new_v4(),
            show_divider: false,
            menu_visibility: true,
            ratio: 90,
        }
    }

    fn draw_menu(&mut self, grid: &mut CellBuffer, mut area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        clear_area(grid, area);
        /* visually divide menu and listing */
        area = (area.0, pos_dec(area.1, (1, 0)));
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        self.dirty = false;
        let mut y = get_y(upper_left);
        for a in &self.accounts {
            y += 1;
            y += self.print_account(grid, (set_y(upper_left, y), bottom_right), &a, context);
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

        let highlight = self.cursor_pos.0 == a.index;

        let mut inc = 0;
        let mut depth = String::from("");
        let mut s = format!("{}\n", a.name);
        fn pop(depth: &mut String) {
            depth.pop();
        }

        fn push(depth: &mut String, c: char) {
            depth.push(c);
        }

        fn print(
            folder_idx: FolderHash,
            depth: &mut String,
            inc: &mut usize,
            entries: &FnvHashMap<FolderHash, Folder>,
            folders_order: &FnvHashMap<FolderHash, usize>,
            s: &mut String,
            index: usize, //account index
            context: &mut Context,
        ) {
            match context.accounts[index].status(entries[&folder_idx].hash()) {
                Ok(_) => {
                    let account = &context.accounts[index];
                    let count = account[entries[&folder_idx].hash()]
                        .as_ref()
                        .unwrap()
                        .envelopes
                        .iter()
                        .map(|h| &account.collection[&h])
                        .filter(|e| !e.is_seen())
                        .count();
                    let len = s.len();
                    s.insert_str(
                        len,
                        &format!("{} {}   {}\n  ", *inc, &entries[&folder_idx].name(), count),
                    );
                }
                Err(_) => {
                    let len = s.len();
                    s.insert_str(
                        len,
                        &format!("{} {}   ...\n  ", *inc, &entries[&folder_idx].name()),
                    );
                }
            }
            *inc += 1;
            let mut children: Vec<FolderHash> = entries[&folder_idx].children().to_vec();
            children
                .sort_unstable_by(|a, b| folders_order[a].partial_cmp(&folders_order[b]).unwrap());
            push(depth, ' ');
            for child in children {
                let len = s.len();
                s.insert_str(len, &format!("{} ", depth));
                print(child, depth, inc, entries, folders_order, s, index, context);
            }
            pop(depth);
        }
        for f in entries.keys() {
            if entries[f].parent().is_none() {
                print(
                    *f,
                    &mut depth,
                    &mut inc,
                    &entries,
                    &folders_order,
                    &mut s,
                    a.index,
                    context,
                );
            }
        }

        let lines: Vec<&str> = s.lines().collect();
        let lines_len = lines.len();
        if lines_len < 2 {
            return 0;
        }
        let mut idx = 0;
        for y in get_y(upper_left)..get_y(bottom_right) {
            if idx == lines_len {
                break;
            }
            let s = lines[idx].to_string();
            let (color_fg, color_bg) = if highlight {
                if self.cursor_pos.1 + 1 == idx {
                    (Color::Byte(233), Color::Byte(15))
                } else {
                    (Color::Byte(15), Color::Byte(233))
                }
            } else {
                (Color::Default, Color::Default)
            };

            write_string_to_grid(
                &s,
                grid,
                color_fg,
                color_bg,
                (set_y(upper_left, y), bottom_right),
                false,
            );
            {
                enum CellPos {
                    BeforeIndex,
                    Index,
                    //AfterIndex,
                }
                let mut pos = CellPos::BeforeIndex;
                let mut x = get_x(upper_left);
                while let Some(cell) = grid.get_mut(x, y) {
                    if x == get_x(bottom_right) {
                        break;
                    }
                    match (cell.ch(), &pos) {
                        (c, CellPos::Index) | (c, CellPos::BeforeIndex) if c.is_numeric() => {
                            pos = CellPos::Index;
                            cell.set_fg(Color::Byte(243));
                            x += 1;
                            continue;
                        }
                        (c, CellPos::BeforeIndex) if c.is_whitespace() => {
                            x += 1;
                            continue;
                        }
                        _ => {
                            break;
                        }
                    }
                }
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
