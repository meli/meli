/*
 * meli - pager
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

use super::*;

/// A pager for text.
/// `Pager` holds its own content in its own `CellBuffer` and when `draw` is called, it draws the
/// current view of the text. It is responsible for scrolling etc.
#[derive(Default, Debug, Clone)]
pub struct Pager {
    text: String,
    cursor: (usize, usize),
    reflow: Reflow,
    height: usize,
    width: usize,
    minimum_width: usize,
    search: Option<SearchPattern>,
    dirty: bool,

    colors: ThemeAttribute,
    initialised: bool,
    show_scrollbar: bool,
    content: CellBuffer,
    text_lines: (usize, Vec<String>),
    movement: Option<PageMovement>,
    id: ComponentId,
}

impl fmt::Display for Pager {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Pager::DESCRIPTION)
    }
}

impl Pager {
    pub const DESCRIPTION: &'static str = "pager";
    pub fn new(context: &Context) -> Self {
        let mut ret = Pager::default();
        ret.minimum_width = context.settings.pager.minimum_width;
        ret.set_colors(crate::conf::value(context, "theme_default"))
            .set_reflow(if context.settings.pager.split_long_lines {
                Reflow::All
            } else {
                Reflow::No
            });
        ret
    }

    pub fn set_show_scrollbar(&mut self, new_val: bool) -> &mut Self {
        self.show_scrollbar = new_val;
        self
    }

    pub fn set_colors(&mut self, new_val: ThemeAttribute) -> &mut Self {
        self.colors = new_val;
        self
    }

    pub fn set_reflow(&mut self, new_val: Reflow) -> &mut Self {
        self.reflow = new_val;
        self
    }

    pub fn set_initialised(&mut self, new_val: bool) -> &mut Self {
        self.initialised = new_val;
        self
    }

    pub fn reflow(&self) -> Reflow {
        self.reflow
    }

    pub fn update_from_str(&mut self, text: &str, mut width: Option<usize>) {
        if let Some(ref mut width) = width.as_mut() {
            if **width < self.minimum_width {
                **width = self.minimum_width;
            }
        }

        let lines: Vec<String> = text.split_lines_reflow(self.reflow, width);
        let height = lines.len() + 2;
        let width = width.unwrap_or_else(|| lines.iter().map(|l| l.len()).max().unwrap_or(0));
        let mut empty_cell = Cell::with_char(' ');
        empty_cell.set_fg(self.colors.fg);
        empty_cell.set_bg(self.colors.bg);
        let mut content = CellBuffer::new(1, 1, empty_cell);
        content.set_ascii_drawing(self.content.ascii_drawing);
        self.text = text.to_string();
        self.text_lines = (0, vec![]);
        self.content = content;
        self.height = height;
        self.width = width;
        self.initialised = false;
        self.dirty = true;
        self.cursor = (0, 0);
    }
    pub fn from_string(
        mut text: String,
        context: Option<&Context>,
        cursor_pos: Option<usize>,
        mut width: Option<usize>,
        colors: ThemeAttribute,
    ) -> Self {
        let pager_filter: Option<&String> = if let Some(context) = context {
            context.settings.pager.filter.as_ref()
        } else {
            None
        };

        let pager_minimum_width: usize = if let Some(context) = context {
            context.settings.pager.minimum_width
        } else {
            0
        };

        let reflow: Reflow = if let Some(context) = context {
            if context.settings.pager.split_long_lines {
                Reflow::All
            } else {
                Reflow::No
            }
        } else {
            Reflow::All
        };

        if let Some(ref mut width) = width.as_mut() {
            if **width < pager_minimum_width {
                **width = pager_minimum_width;
            }
        }

        if let Some(content) = pager_filter.and_then(|bin| {
            use std::io::Write;
            use std::process::{Command, Stdio};
            let mut filter_child = Command::new("sh")
                .args(&["-c", bin])
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .expect("Failed to start pager filter process");
            {
                let stdin = filter_child.stdin.as_mut().expect("failed to open stdin");
                stdin
                    .write_all(text.as_bytes())
                    .expect("Failed to write to stdin");
            }

            text = String::from_utf8_lossy(
                &filter_child
                    .wait_with_output()
                    .expect("Failed to wait on filter")
                    .stdout,
            )
            .to_string();
            if text.is_empty() {
                None
            } else {
                crate::terminal::ansi::ansi_to_cellbuffer(&text)
            }
        }) {
            return Pager::from_buf(content, cursor_pos);
        }
        let content = {
            if let Some(context) = context {
                CellBuffer::new_with_context(1, 1, None, context)
            } else {
                let mut empty_cell = Cell::with_char(' ');
                empty_cell.set_fg(colors.fg);
                empty_cell.set_bg(colors.bg);
                CellBuffer::new(1, 1, empty_cell)
            }
        };
        Pager {
            text,
            text_lines: (0, vec![]),
            reflow,
            cursor: (0, cursor_pos.unwrap_or(0)),
            height: content.size().1,
            width: content.size().0,
            minimum_width: pager_minimum_width,
            initialised: false,
            dirty: true,
            content,
            id: ComponentId::new_v4(),
            colors,
            ..Default::default()
        }
    }
    pub fn from_str(
        text: &str,
        cursor_pos: Option<usize>,
        width: Option<usize>,
        colors: ThemeAttribute,
    ) -> Self {
        let mut empty_cell = Cell::with_char(' ');
        empty_cell.set_fg(colors.fg);
        empty_cell.set_bg(colors.bg);

        Pager {
            text: text.to_string(),
            text_lines: (0, vec![]),
            cursor: (0, cursor_pos.unwrap_or(0)),
            height: 1,
            width: width.unwrap_or(1),
            initialised: false,
            dirty: true,
            content: CellBuffer::new(1, 1, empty_cell),
            colors,
            id: ComponentId::new_v4(),
            ..Default::default()
        }
    }

    pub fn from_buf(content: CellBuffer, cursor_pos: Option<usize>) -> Self {
        let (width, height) = content.size();
        Pager {
            text: String::new(),
            cursor: (0, cursor_pos.unwrap_or(0)),
            height,
            width,
            dirty: true,
            content,
            initialised: true,
            id: ComponentId::new_v4(),
            ..Default::default()
        }
    }
    pub fn print_string(content: &mut CellBuffer, lines: &[String], colors: ThemeAttribute) {
        let width = content.size().0;
        debug!(colors);
        for (i, l) in lines.iter().enumerate() {
            write_string_to_grid(
                l,
                content,
                colors.fg,
                colors.bg,
                Attr::DEFAULT,
                ((0, i), (width.saturating_sub(1), i)),
                None,
            );
            if l.starts_with("⤷") {
                content[(0, i)].set_fg(Color::Byte(240));
                content[(0, i)].set_attrs(Attr::BOLD);
            }
        }
    }

    pub fn cursor_pos(&self) -> usize {
        self.cursor.1
    }

    pub fn size(&self) -> (usize, usize) {
        (self.width, self.height)
    }
}

impl Component for Pager {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !is_valid_area!(area) {
            return;
        }
        if !self.is_dirty() {
            return;
        }

        if !self.initialised && !self.text.is_empty() {
            let mut width = width!(area);
            if width < self.minimum_width {
                width = self.minimum_width;
            }

            let lines: &[String] = if self.text_lines.0 == width.saturating_sub(4) {
                &self.text_lines.1
            } else {
                let lines = self
                    .text
                    .split_lines_reflow(self.reflow, Some(width.saturating_sub(4)));
                self.text_lines = (width.saturating_sub(4), lines);
                &self.text_lines.1
            };
            let height = lines.len() + 2;
            let mut empty_cell = Cell::with_char(' ');
            empty_cell.set_fg(self.colors.fg);
            empty_cell.set_bg(self.colors.bg);
            let mut content = CellBuffer::new(width, height, empty_cell);
            content.set_ascii_drawing(self.content.ascii_drawing);
            if let Some(ref mut search) = self.search {
                use melib::text_processing::search::KMP;
                search.positions.clear();
                for (y, l) in lines.iter().enumerate() {
                    search.positions.extend(
                        l.kmp_search(&search.pattern)
                            .into_iter()
                            .map(|offset| (y, offset)),
                    );
                }
            }
            Pager::print_string(&mut content, &lines, self.colors);
            #[cfg(feature = "regexp")]
            {
                for text_formatter in
                    crate::conf::text_format_regexps(context, "pager.envelope.body")
                {
                    let t = content.insert_tag(text_formatter.tag);
                    for (i, l) in lines.iter().enumerate() {
                        for (start, end) in text_formatter.regexp.find_iter(l) {
                            content.set_tag(t, (start, i), (end, i));
                        }
                    }
                }
            }
            if let Some(ref mut search) = self.search {
                let results_attr = crate::conf::value(context, "pager.highlight_search");
                let results_current_attr =
                    crate::conf::value(context, "pager.highlight_search_current");
                search.cursor =
                    std::cmp::min(search.positions.len().saturating_sub(1), search.cursor);
                for (i, (y, x)) in search.positions.iter().enumerate() {
                    for c in content.row_iter(*x..*x + search.pattern.grapheme_len(), *y) {
                        if i == search.cursor {
                            content[c]
                                .set_fg(results_current_attr.fg)
                                .set_bg(results_current_attr.bg)
                                .set_attrs(results_current_attr.attrs);
                        } else {
                            content[c]
                                .set_fg(results_attr.fg)
                                .set_bg(results_attr.bg)
                                .set_attrs(results_attr.attrs);
                        }
                    }
                }
            }
            self.content = content;
            self.height = height;
            self.width = width;
            self.initialised = true;
        }

        self.dirty = false;

        let height = height!(area);
        if let Some(mvm) = self.movement.take() {
            match mvm {
                PageMovement::Up(amount) => {
                    self.cursor.1 = self.cursor.1.saturating_sub(amount);
                }
                PageMovement::PageUp(multiplier) => {
                    self.cursor.1 = self.cursor.1.saturating_sub(height * multiplier);
                }
                PageMovement::Down(amount) => {
                    if self.cursor.1 + amount + 1 < self.height {
                        self.cursor.1 += amount;
                    } else {
                        self.cursor.1 = self.height.saturating_sub(1);
                    }
                }
                PageMovement::PageDown(multiplier) => {
                    if self.cursor.1 + height * multiplier + 1 < self.height {
                        self.cursor.1 += height * multiplier;
                    } else if self.cursor.1 + height * multiplier > self.height {
                        self.cursor.1 = self.height - 1;
                    } else {
                        self.cursor.1 = (self.height / height) * height;
                    }
                }
                PageMovement::Right(amount) => {
                    if self.cursor.0 + amount + 1 < self.width {
                        self.cursor.0 += amount;
                    } else {
                        self.cursor.0 = self.width.saturating_sub(1);
                    }
                }
                PageMovement::Left(amount) => {
                    self.cursor.0 = self.cursor.0.saturating_sub(amount);
                }
                PageMovement::Home => {
                    self.cursor.1 = 0;
                }
                PageMovement::End => {
                    self.cursor.1 = self.height.saturating_sub(1);
                }
            }
        }

        if self.height == 0 || self.width == 0 {
            return;
        }
        if let Some(ref mut search) = self.search {
            if !search.positions.is_empty() {
                if let Some(mvm) = search.movement.take() {
                    match mvm {
                        PageMovement::Up(_) => {
                            if self.cursor.1 > search.positions[search.cursor].0 {
                                self.cursor.1 = search.positions[search.cursor].0;
                            }
                        }
                        PageMovement::Down(_) => {
                            if self.cursor.1 + height < search.positions[search.cursor].0 {
                                self.cursor.1 = search.positions[search.cursor].0;
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        clear_area(grid, area, crate::conf::value(context, "theme_default"));
        let (width, height) = self.content.size();
        let (mut cols, mut rows) = (width!(area), height!(area));
        if self.show_scrollbar && rows < height {
            cols -= 1;
        }
        if self.show_scrollbar && cols < width {
            rows -= 1;
        }
        self.cursor = (
            std::cmp::min(width.saturating_sub(cols), self.cursor.0),
            std::cmp::min(height.saturating_sub(rows), self.cursor.1),
        );
        copy_area(
            grid,
            &self.content,
            area,
            (
                (
                    std::cmp::min((width - 1).saturating_sub(cols), self.cursor.0),
                    std::cmp::min((height - 1).saturating_sub(rows), self.cursor.1),
                ),
                (
                    std::cmp::min(self.cursor.0 + cols, width - 1),
                    std::cmp::min(self.cursor.1 + rows, height - 1),
                ),
            ),
        );
        if self.show_scrollbar && rows + 1 < height {
            ScrollBar::default().set_show_arrows(true).draw(
                grid,
                (
                    set_x(upper_left!(area), get_x(bottom_right!(area))),
                    bottom_right!(area),
                ),
                context,
                self.cursor.1,
                rows,
                height,
            );
        }
        if self.show_scrollbar && cols + 1 < width {
            ScrollBar::default().set_show_arrows(true).draw_horizontal(
                grid,
                (
                    set_y(upper_left!(area), get_y(bottom_right!(area))),
                    bottom_right!(area),
                ),
                context,
                self.cursor.0,
                cols,
                width,
            );
        }
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = self.get_shortcuts(context);
        match event {
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::DESCRIPTION]["scroll_up"]) =>
            {
                self.cursor.1 = self.cursor.1.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::DESCRIPTION]["scroll_down"]) =>
            {
                self.cursor.1 = self.cursor.1 + 1;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Home) => {
                self.movement = Some(PageMovement::Home);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::End) => {
                self.movement = Some(PageMovement::End);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Left) => {
                self.movement = Some(PageMovement::Left(1));
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Right) => {
                self.movement = Some(PageMovement::Right(1));
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::DESCRIPTION]["page_up"]) =>
            {
                self.movement = Some(PageMovement::PageUp(1));
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::DESCRIPTION]["page_down"]) =>
            {
                self.movement = Some(PageMovement::PageDown(1));
                self.dirty = true;
                return true;
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Action(View(Pipe(ref bin, ref args))) => {
                use std::io::Write;
                use std::process::{Command, Stdio};
                let mut command_obj = match Command::new(bin)
                    .args(args.as_slice())
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()
                {
                    Ok(o) => o,
                    Err(e) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Could not pipe to {}: {}",
                                bin, e
                            )),
                        ));
                        return true;
                    }
                };
                let stdin = command_obj.stdin.as_mut().expect("failed to open stdin");
                stdin
                    .write_all(self.text.as_bytes())
                    .expect("Failed to write to stdin");

                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(format!(
                        "Pager text piped to '{}{}{}'",
                        &bin,
                        if args.is_empty() { "" } else { " " },
                        args.join(" ")
                    ))));
                return true;
            }
            UIEvent::Action(Action::Listing(ListingAction::Search(pattern))) => {
                self.search = Some(SearchPattern {
                    pattern: pattern.to_string(),
                    positions: vec![],
                    cursor: 0,
                    movement: Some(PageMovement::Home),
                });
                self.initialised = false;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Char('n')) if self.search.is_some() => {
                if let Some(ref mut search) = self.search {
                    search.movement = Some(PageMovement::Down(1));
                    search.cursor += 1;
                } else {
                    unsafe {
                        std::hint::unreachable_unchecked();
                    }
                }
                self.initialised = false;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Char('N')) if self.search.is_some() => {
                if let Some(ref mut search) = self.search {
                    search.movement = Some(PageMovement::Up(1));
                    search.cursor = search.cursor.saturating_sub(1);
                } else {
                    unsafe {
                        std::hint::unreachable_unchecked();
                    }
                }
                self.initialised = false;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Esc) if self.search.is_some() => {
                self.search = None;
                self.initialised = false;
                self.dirty = true;
                return true;
            }
            UIEvent::Resize => {
                self.initialised = false;
                self.dirty = true;
            }
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
        let config_map: IndexMap<&'static str, Key> = context.settings.shortcuts.pager.key_values();
        let mut ret: ShortcutMaps = Default::default();
        ret.insert(Pager::DESCRIPTION, config_map);
        ret
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}
