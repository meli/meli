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

/*! Entities that handle Mail specific functions.
 */
use super::*;
use melib::backends::Folder;

pub mod listing;
pub use listing::*;
pub mod view;
pub use view::*;
mod compose;
pub use self::compose::*;

#[derive(Debug)]
struct AccountMenuEntry {
    name: String,
    // Index in the config account vector.
    index: usize,
    // Each entry and its index in the account
    entries: Vec<(usize, Folder)>,
}

/// The account sidebar.
#[derive(Debug)]
pub struct AccountMenu {
    accounts: Vec<AccountMenuEntry>,
    dirty: bool,
    cursor: Option<(usize, usize)>,
}

impl fmt::Display for AccountMenu {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        write!(f, "menu")
    }
}

impl AccountMenu {
    pub fn new(accounts: &[Account]) -> Self {
        let accounts = accounts
            .iter()
            .enumerate()
            .map(|(i, a)| AccountMenuEntry {
                name: a.name().to_string(),
                index: i,
                entries: {
                    let mut entries = Vec::with_capacity(a.len());
                    let mut idx = 0;
                    for acc in a.list_folders() {
                        entries.push((idx, acc));
                        idx += 1;
                    }
                    entries
                },
            })
            .collect();
        AccountMenu {
            accounts,
            dirty: true,
            cursor: None,
        }
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
            eprintln!("BUG: invalid area in print_account");
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let highlight = self.cursor.map(|(x, _)| x == a.index).unwrap_or(false);

        let mut parents: Vec<Option<usize>> = vec![None; a.entries.len()];

        for (idx, e) in a.entries.iter().enumerate() {
            for c in e.1.children() {
                parents[*c] = Some(idx);
            }
        }
        let mut roots = Vec::new();
        for (idx, c) in parents.iter().enumerate() {
            if c.is_none() {
                roots.push(idx);
            }
        }

        let mut inc = 0;
        let mut depth = String::from("");
        let mut s = format!("{}\n", a.name);
        fn pop(depth: &mut String) {
            depth.pop();
            depth.pop();
        }

        fn push(depth: &mut String, c: char) {
            depth.push(c);
        }

        fn print(
            root: usize,
            parents: &[Option<usize>],
            depth: &mut String,
            entries: &[(usize, Folder)],
            s: &mut String,
            inc: &mut usize,
            index: usize, //account index
            context: &mut Context,
        ) -> () {
            let len = s.len();
            match context.accounts[index].status(root) {
                Ok(_) => {}
                Err(_) => {
                    return;
                    // TODO: Show progress visually
                }
            }
            let count = context.accounts[index][root]
                .as_ref()
                .unwrap()
                .collection
                .iter()
                .filter(|e| !e.is_seen())
                .count();
            s.insert_str(
                len,
                &format!("{} {}   {}\n  ", *inc, &entries[root].1.name(), count),
            );
            *inc += 1;
            let children_no = entries[root].1.children().len();
            for (idx, child) in entries[root].1.children().iter().enumerate() {
                let len = s.len();
                s.insert_str(len, &format!("{}├─", depth));
                push(depth, if idx == children_no - 1 { '│' } else { ' ' });
                print(*child, parents, depth, entries, s, inc, index, context);
                pop(depth);
            }
        }
        for r in roots {
            print(
                r, &parents, &mut depth, &a.entries, &mut s, &mut inc, a.index, context,
            );
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
            let s = if idx == lines_len - 2 {
                lines[idx].replace("├", "└")
            } else {
                lines[idx].to_string()
            };
            let (color_fg, color_bg) = if highlight {
                if self.cursor.unwrap().1 + 1 == idx {
                    (Color::Byte(233), Color::Byte(15))
                } else {
                    (Color::Byte(15), Color::Byte(233))
                }
            } else {
                (Color::Default, Color::Default)
            };

            let (x, _) = write_string_to_grid(
                &s,
                grid,
                color_fg,
                color_bg,
                (set_y(upper_left, y), bottom_right),
                false,
            );

            if highlight && idx > 1 && self.cursor.unwrap().1 == idx - 1 {
                change_colors(grid, ((x, y), (get_x(bottom_right), y)), color_fg, color_bg);
            } else {
                change_colors(grid, ((x, y), set_y(bottom_right, y)), color_fg, color_bg);
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

impl Component for AccountMenu {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        clear_area(grid, area);
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        self.dirty = false;
        let mut y = get_y(upper_left);
        for a in &self.accounts {
            y += self.print_account(grid, (set_y(upper_left, y), bottom_right), &a, context);
        }

        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &UIEvent, _context: &mut Context) -> bool {
        match event.event_type {
            UIEventType::RefreshMailbox(c) => {
                self.cursor = Some(c);
                self.dirty = true;
            }
            UIEventType::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEventType::Resize => {
                self.dirty = true;
            }
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
    }
}
