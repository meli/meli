/*
 * meli - layouts
 *
 * Copyright 2017-2018, 2020 Manos Pitsidianakis
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
use crate::terminal::cells::boundaries::{
    HORZ_BOUNDARY, VERT_BOUNDARY, _LIGHT_DOWN_AND_HORIZONTAL, _LIGHT_UP_AND_HORIZONTAL,
};

/// A horizontally split in half container.
#[derive(Debug)]
pub struct HSplit {
    top: Box<dyn Component>,
    bottom: Box<dyn Component>,
    show_divider: bool,
    ratio: usize, // bottom/whole height * 100
    id: ComponentId,
}

impl std::fmt::Display for HSplit {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.top, f)
    }
}

impl HSplit {
    pub fn new(
        top: Box<dyn Component>,
        bottom: Box<dyn Component>,
        ratio: usize,
        show_divider: bool,
    ) -> Self {
        HSplit {
            top,
            bottom,
            show_divider,
            ratio,
            id: ComponentId::default(),
        }
    }
}

impl Component for HSplit {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !is_valid_area!(area) {
            return;
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let total_rows = get_y(bottom_right) - get_y(upper_left);
        let bottom_component_height = (self.ratio * total_rows) / 100;
        let mid = get_y(upper_left) + total_rows - bottom_component_height;

        if self.show_divider {
            for i in get_x(upper_left)..=get_x(bottom_right) {
                grid[(i, mid)].set_ch('─');
            }
            context
                .dirty_areas
                .push_back(((get_x(upper_left), mid), (get_x(bottom_right), mid)));
        }

        self.top.draw(
            grid,
            (
                upper_left,
                (get_x(bottom_right), get_y(upper_left) + mid - 1),
            ),
            context,
        );
        self.bottom.draw(
            grid,
            ((get_x(upper_left), get_y(upper_left) + mid), bottom_right),
            context,
        );
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        self.top.process_event(event, context) || self.bottom.process_event(event, context)
    }

    fn is_dirty(&self) -> bool {
        self.top.is_dirty() || self.bottom.is_dirty()
    }

    fn set_dirty(&mut self, value: bool) {
        self.top.set_dirty(value);
        self.bottom.set_dirty(value);
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut top_map = self.top.shortcuts(context);
        top_map.extend(self.bottom.shortcuts(context).into_iter());
        top_map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}

/// A vertically split in half container.
#[derive(Debug)]
pub struct VSplit {
    left: Box<dyn Component>,
    right: Box<dyn Component>,
    show_divider: bool,
    prev_visibility: (bool, bool),
    /// This is the width of the right container to the entire width.
    ratio: usize, // right/(container width) * 100
    id: ComponentId,
}

impl std::fmt::Display for VSplit {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // [ref:TODO]: display focused component
        std::fmt::Display::fmt(&self.right, f)
    }
}

impl VSplit {
    pub fn new(
        left: Box<dyn Component>,
        right: Box<dyn Component>,
        ratio: usize,
        show_divider: bool,
    ) -> Self {
        VSplit {
            left,
            right,
            show_divider,
            prev_visibility: (true, true),
            ratio,
            id: ComponentId::default(),
        }
    }
}

impl Component for VSplit {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !is_valid_area!(area) {
            return;
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);
        let total_cols = get_x(bottom_right) - get_x(upper_left);
        let visibility = (self.left.is_visible(), self.right.is_visible());
        if visibility != self.prev_visibility {
            self.set_dirty(true);
            self.prev_visibility = visibility;
        }
        let right_component_width = match visibility {
            (true, true) => (self.ratio * total_cols) / 100,
            (false, true) => total_cols,
            (true, false) => 0,
            (false, false) => {
                clear_area(grid, area, crate::conf::value(context, "theme_default"));
                return;
            }
        };

        let mid = get_x(bottom_right) - right_component_width;

        if get_y(upper_left) > 1 {
            let c = grid
                .get(mid, get_y(upper_left) - 1)
                .map(Cell::ch)
                .unwrap_or_else(|| ' ');
            if let HORZ_BOUNDARY = c {
                grid[(mid, get_y(upper_left) - 1)].set_ch(_LIGHT_DOWN_AND_HORIZONTAL);
            }
        }

        if self.show_divider && mid != get_x(upper_left) {
            for i in get_y(upper_left)..=get_y(bottom_right) {
                grid[(mid, i)].set_ch(VERT_BOUNDARY);
                grid[(mid, i)].set_fg(Color::Default);
                grid[(mid, i)].set_bg(Color::Default);
            }
            if get_y(bottom_right) > 1 {
                let c = grid
                    .get(mid, get_y(bottom_right) - 1)
                    .map(Cell::ch)
                    .unwrap_or_else(|| ' ');
                if let HORZ_BOUNDARY = c {
                    grid[(mid, get_y(bottom_right) + 1)].set_ch(_LIGHT_UP_AND_HORIZONTAL);
                }
            }
            context
                .dirty_areas
                .push_back(((mid, get_y(upper_left)), (mid, get_y(bottom_right))));
        }

        if right_component_width == total_cols {
            self.right.draw(grid, area, context);
        } else if right_component_width == 0 {
            self.left.draw(grid, area, context);
        } else {
            self.left.draw(
                grid,
                (
                    upper_left,
                    (
                        if self.show_divider { mid - 1 } else { mid },
                        get_y(bottom_right),
                    ),
                ),
                context,
            );
            self.right
                .draw(grid, (set_x(upper_left, mid + 1), bottom_right), context);
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        self.left.process_event(event, context) || self.right.process_event(event, context)
    }

    fn is_dirty(&self) -> bool {
        self.left.is_dirty() || self.right.is_dirty()
    }

    fn set_dirty(&mut self, value: bool) {
        self.left.set_dirty(value);
        self.right.set_dirty(value);
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut right_map = self.right.shortcuts(context);
        right_map.extend(self.left.shortcuts(context).into_iter());
        right_map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}
