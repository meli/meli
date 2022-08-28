/*
 * meli
 *
 * Copyright 2020 Manos Pitsidianakis
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

use crate::conf::ThemeAttribute;
use crate::segment_tree::SegmentTree;
use crate::Component;
use crate::{
    clear_area, get_y, pos_inc, terminal::Attr, write_string_to_grid, Area, CellBuffer,
    ComponentId, Context, Key, ShortcutMaps, UIEvent,
};
use melib::text_processing::TextProcessing;
use std::convert::TryInto;

#[derive(PartialEq, Copy, Clone, Debug)]
enum ShowMenuScrollbar {
    Never,
    True,
    False,
}

#[derive(Debug)]
pub struct Table<const N: usize> {
    pub cursor_position: usize,
    pub new_cursor_position: usize,
    pub theme_attribute_even: ThemeAttribute,
    pub theme_attribute_odd: ThemeAttribute,
    show_menu_scrollbar: ShowMenuScrollbar,
    pub rows: Vec<[String; N]>,
    pub widths: Vec<usize>, // widths of columns calculated in first draw and after size changes
    pub length: usize,
    pub segment_tree: Vec<SegmentTree>,
    pub dirty: bool,
    pub id: ComponentId,
}

impl<const N: usize> Table<N> {
    const DESCRIPTION: &'static str = "general";
    pub fn new(
        theme_attribute_even: ThemeAttribute,
        theme_attribute_odd: ThemeAttribute,
    ) -> Box<Self> {
        Box::new(Table {
            cursor_position: 0,
            new_cursor_position: 0,
            show_menu_scrollbar: ShowMenuScrollbar::Never,
            rows: vec![],
            widths: vec![0; N],
            segment_tree: vec![SegmentTree::default(); N],
            length: 0,
            theme_attribute_even,
            theme_attribute_odd,
            dirty: true,
            id: ComponentId::new_v4(),
        })
    }

    pub fn clear(&mut self) {
        self.rows.clear();
        self.widths.clear();
        self.segment_tree.clear();
        self.set_dirty(true);
        self.length = 0;
    }

    pub fn insert_rows(&mut self, values: Vec<[String; N]>) {
        let mut row_widths: Vec<Vec<u8>> = vec![vec![]; N];
        for row in values {
            for i in 0..N {
                let width = row[i].grapheme_width();
                self.widths[i] = std::cmp::max(self.widths[i], width);
                row_widths[i].push(width.try_into().unwrap_or(255));
            }
            //self.widths.push(
            self.rows.push(row);
            self.length += 1;
        }

        for (i, widths) in row_widths.into_iter().enumerate() {
            self.segment_tree[i] = widths.into();
        }
    }
}

impl<const N: usize> std::fmt::Display for Table<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "table")
    }
}

impl<const N: usize> Component for Table<N> {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        if !is_valid_area!(area) {
            return;
        }

        clear_area(grid, area, self.theme_attribute_even);
        if self.length == 0 {
            return;
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let page_size = get_y(bottom_right) - get_y(upper_left);
        //let prev_page_no = (self.cursor_position).wrapping_div(page_size);
        let page_no = (self.new_cursor_position).wrapping_div(page_size);

        let top_idx = page_no * page_size;

        let mut rows = 0;
        for (i, row) in self.rows.iter().skip(top_idx).take(page_size).enumerate() {
            rows = i;
            let mut width_accumulator: usize = 0;
            let mut row_attr = if i % 2 == 0 {
                self.theme_attribute_even
            } else {
                self.theme_attribute_odd
            };
            if top_idx + i == self.new_cursor_position {
                row_attr.attrs |= Attr::REVERSE;
            }

            for (col, value) in row.iter().enumerate() {
                width_accumulator += self.widths[col];
                let (mut x, _) = write_string_to_grid(
                    &value,
                    grid,
                    row_attr.fg,
                    row_attr.bg,
                    row_attr.attrs,
                    (
                        pos_inc((0, i), upper_left),
                        pos_inc((width_accumulator, i), upper_left),
                    ),
                    None,
                );
                width_accumulator += 1;
                while x < width_accumulator {
                    grid[pos_inc((x, i), upper_left)]
                        .set_bg(row_attr.bg)
                        .set_ch(' ');
                    x += 1;
                }
            }
        }
        self.cursor_position = self.new_cursor_position;
        self.show_menu_scrollbar = ShowMenuScrollbar::True;
        crate::ScrollBar::default().set_show_arrows(true).draw(
            grid,
            (
                pos_inc(upper_left!(area), (width!(area), 0)),
                bottom_right!(area),
            ),
            context,
            /* position */
            top_idx,
            /* visible_rows */
            rows,
            /* length */
            self.rows.len(),
        );

        context.dirty_areas.push_back(area);

        //self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = self.get_shortcuts(context);
        match event {
            UIEvent::Input(ref key)
                if crate::shortcut!(key == shortcuts[Self::DESCRIPTION]["scroll_up"]) =>
            {
                self.set_dirty(true);
                if self.cursor_position == 0 {
                    return false;
                }

                debug_assert!(self.cursor_position < self.length);
                self.new_cursor_position -= 1;
                return true;
            }
            UIEvent::Input(ref key)
                if crate::shortcut!(key == shortcuts[Self::DESCRIPTION]["scroll_down"]) =>
            {
                if self.cursor_position + 1 >= self.length {
                    return false;
                }

                debug_assert!(self.cursor_position < self.length);
                self.new_cursor_position += 1;
                self.set_dirty(true);
                return true;
            }
            UIEvent::Resize => self.set_dirty(true),
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();
        let general_map = context.settings.shortcuts.general.key_values();
        map.insert(Self::DESCRIPTION, general_map);
        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }

    fn can_quit_cleanly(&mut self, _context: &Context) -> bool {
        true
    }
}
