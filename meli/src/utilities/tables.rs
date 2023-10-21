/*
 * meli
 *
 * Copyright 2017-2022 Manos Pitsidianakis
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

//! UI components to display tabular lists.
use std::mem::MaybeUninit;

use super::*;
use crate::segment_tree::SegmentTree;

#[derive(Debug, Default, Copy, Clone)]
pub enum ColumnElasticity {
    #[default]
    Rigid,
    Grow {
        min: usize,
        max: Option<usize>,
    },
}

impl ColumnElasticity {
    pub fn set_rigid(&mut self) {
        *self = Self::Rigid;
    }
    pub fn set_grow(&mut self, min: usize, max: Option<usize>) {
        *self = Self::Grow { min, max };
    }
}

/*#[derive(Debug, Default, Clone)]
pub enum TableRowFormat {
    #[default]
    None,
    Fill(FormatTag),
    Range {
        start: usize,
        end: usize,
        val: FormatTag,
    },
}

impl TableRowFormat {
    pub const SELECTED: u8 = 0;
    pub const UNREAD: u8 = 1;
}
*/

#[derive(Debug, Default, Clone)]
pub struct TableThemeConfig {
    pub theme: TableTheme,
    //pub row_formats: HashMap<usize, SmallVec<[(u8, TableRowFormat); 6]>>,
}

impl TableThemeConfig {
    pub fn set_single_theme(&mut self, value: ThemeAttribute) -> &mut Self {
        self.theme = TableTheme::Single(value);
        self
    }

    pub fn set_even_odd_theme(&mut self, even: ThemeAttribute, odd: ThemeAttribute) -> &mut Self {
        self.theme = TableTheme::EvenOdd { even, odd };
        self
    }
}

#[derive(Debug, Clone)]
pub enum TableTheme {
    Single(ThemeAttribute),
    EvenOdd {
        even: ThemeAttribute,
        odd: ThemeAttribute,
    },
}

impl Default for TableTheme {
    fn default() -> Self {
        Self::Single(ThemeAttribute::default())
    }
}

#[derive(Debug, Default, Clone)]
pub struct TableCursorConfig {
    pub handle: bool,
    pub theme: TableTheme,
}

impl TableCursorConfig {
    pub fn set_handle(&mut self, value: bool) -> &mut Self {
        self.handle = value;
        self
    }

    pub fn set_single_theme(&mut self, value: ThemeAttribute) -> &mut Self {
        self.theme = TableTheme::Single(value);
        self
    }

    pub fn set_even_odd_theme(&mut self, even: ThemeAttribute, odd: ThemeAttribute) -> &mut Self {
        self.theme = TableTheme::EvenOdd { even, odd };
        self
    }
}

#[derive(Debug, Clone)]
pub struct DataColumns<const N: usize> {
    pub cursor_config: TableCursorConfig,
    pub theme_config: TableThemeConfig,
    pub columns: Box<[CellBuffer; N]>,
    /// widths of columns calculated in first draw and after size changes
    pub widths: [usize; N],
    pub elasticities: [ColumnElasticity; N],
    pub x_offset: usize,
    pub width_accum: usize,
    pub segment_tree: Box<[SegmentTree; N]>,
}

// Workaround because Default derive doesn't work for const generic array
// lengths yet.
impl<const N: usize> Default for DataColumns<N> {
    fn default() -> Self {
        fn init_array<T, const N: usize>(cl: impl Fn() -> T) -> [T; N] {
            // https://doc.rust-lang.org/std/mem/union.MaybeUninit.html#initializing-an-array-element-by-element
            let mut data: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };
            for elem in &mut data[..] {
                elem.write(cl());
            }
            let ptr = &data as *const [MaybeUninit<T>; N];
            unsafe { (ptr as *const [T; N]).read() }
        }
        Self {
            cursor_config: TableCursorConfig::default(),
            theme_config: TableThemeConfig::default(),
            columns: Box::new(init_array(CellBuffer::default)),
            widths: [0_usize; N],
            elasticities: [ColumnElasticity::default(); N],
            x_offset: 0,
            width_accum: 0,
            segment_tree: Box::new(init_array(SegmentTree::default)),
        }
    }
}

impl<const N: usize> DataColumns<N> {
    pub const ARROW_UP: &str = "🠉";
    pub const ARROW_DOWN: &str = "🠋";
    pub const ARROW_UP_ASCII: &str = "^";
    pub const ARROW_DOWN_ASCII: &str = "v";
    // const ARROW_UP_1: &str = "↑";
    // const ARROW_DOWN_1: &str = "↓";
    // const ARROW_UP_3: &str = "▲";
    // const ARROW_DOWN_4: &str = "▼";

    pub fn recalc_widths(
        &mut self,
        (screen_width, screen_height): (usize, usize),
        top_idx: usize,
    ) -> usize {
        let mut width_accum = 0;
        let mut growees = 0;
        let mut growees_max = 0;
        for i in 0..N {
            if screen_height == 0 {
                self.widths[i] = 0;
                continue;
            }
            self.widths[i] =
                self.segment_tree[i].get_max(top_idx, top_idx + screen_height - 1) as usize;
            if self.widths[i] == 0 {
                self.widths[i] = self.columns[i].cols;
            }
            match self.elasticities[i] {
                ColumnElasticity::Rigid => {}
                ColumnElasticity::Grow {
                    min,
                    max: Some(max),
                } => {
                    self.widths[i] = std::cmp::max(min, std::cmp::min(max, self.widths[i]));
                    growees += 1;
                }
                ColumnElasticity::Grow { min, max: None } => {
                    self.widths[i] = std::cmp::max(min, self.widths[i]);
                    growees += 1;
                    growees_max += 1;
                }
            }
            width_accum += self.widths[i];
        }
        // add column gaps
        width_accum += 2 * N.saturating_sub(1);
        debug_assert!(growees >= growees_max);
        if width_accum >= screen_width || screen_height == 0 || screen_width == 0 || growees == 0 {
            self.width_accum = width_accum;
            return width_accum;
        }
        let distribute = screen_width - width_accum;
        let part = distribute / growees;

        for i in 0..N {
            match self.elasticities[i] {
                ColumnElasticity::Rigid => {}
                ColumnElasticity::Grow {
                    min: _,
                    max: Some(_),
                } => {}
                ColumnElasticity::Grow { min: _, max: None } => {
                    self.widths[i] += part;
                    width_accum += part;
                }
            }
        }
        self.width_accum = width_accum;
        width_accum
    }

    pub fn draw(
        &self,
        grid: &mut CellBuffer,
        top_idx: usize,
        cursor_pos: usize,
        mut bounds: BoundsIterator,
    ) {
        let mut _relative_x_offset = 0;
        let mut skip_cols = (0, 0);
        let mut start_col = 0;
        let total_area = bounds.area();
        let (width, height) = (width!(total_area), height!(total_area));
        while _relative_x_offset < self.x_offset && start_col < N {
            _relative_x_offset += self.widths[start_col] + 2;
            if self.x_offset <= _relative_x_offset {
                skip_cols.0 = start_col;
                skip_cols.1 = _relative_x_offset - self.x_offset;
                _relative_x_offset = self.x_offset;
                break;
            }
            start_col += 1;
        }

        for col in skip_cols.0..N {
            if bounds.is_empty() {
                break;
            }

            let mut column_width = self.widths[col];
            if column_width > bounds.width {
                column_width = bounds.width;
            } else if column_width == 0 {
                skip_cols.1 = 0;
                continue;
            }

            let mut column_area = bounds.add_x(column_width + 2);
            copy_area(
                grid,
                &self.columns[col],
                column_area.area(),
                (
                    (skip_cols.1, top_idx),
                    (
                        column_width.saturating_sub(1),
                        self.columns[col].rows.saturating_sub(1),
                    ),
                ),
            );
            let gap_area = column_area.add_x(column_width);
            match self.theme_config.theme {
                TableTheme::Single(row_attr) => {
                    change_theme(grid, gap_area.area(), row_attr);
                }
                TableTheme::EvenOdd { even, odd } => {
                    change_theme(grid, gap_area.area(), even);
                    let mut top_idx = top_idx;
                    for row in gap_area {
                        if top_idx % 2 != 0 {
                            change_theme(grid, row.area(), odd);
                        }
                        top_idx += 1;
                    }
                }
            };

            skip_cols.1 = 0;
        }
        if self.cursor_config.handle && (top_idx..(top_idx + height)).contains(&cursor_pos) {
            let offset = cursor_pos - top_idx;
            let row_attr = match self.cursor_config.theme {
                TableTheme::Single(attr) => attr,
                TableTheme::EvenOdd { even, odd: _ } if cursor_pos % 2 == 0 => even,
                TableTheme::EvenOdd { even: _, odd } => odd,
            };

            change_theme(
                grid,
                (
                    pos_inc(upper_left!(total_area), (0, offset)),
                    pos_inc(upper_left!(total_area), (width, offset)),
                ),
                row_attr,
            );
        }
    }
}
