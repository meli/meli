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
use melib::text_processing::LineBreakText;

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
    filtered_content: Option<(String, Result<CellBuffer>)>,
    text_lines: Vec<String>,
    line_breaker: LineBreakText,
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
    const PAGES_AHEAD_TO_RENDER_NO: usize = 16;
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

        self.text = text.to_string();
        self.text_lines.clear();
        self.line_breaker = LineBreakText::new(self.text.clone(), self.reflow, width);
        self.height = 0;
        self.width = 0;
        self.search = None;
        self.set_dirty(true);
        self.initialised = false;
        self.cursor = (0, 0);
    }

    pub fn from_string(
        text: String,
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

        let mut ret = Pager {
            text,
            text_lines: vec![],
            reflow,
            cursor: (0, cursor_pos.unwrap_or(0)),
            height: 1,
            width: 1,
            minimum_width: pager_minimum_width,
            initialised: false,
            dirty: true,
            id: ComponentId::new_v4(),
            filtered_content: None,
            colors,
            ..Default::default()
        };

        if let Some(bin) = pager_filter {
            ret.filter(bin);
        }

        ret
    }

    pub fn filter(&mut self, cmd: &str) {
        let _f = |bin: &str, text: &str| -> Result<CellBuffer> {
            use std::io::Write;
            use std::process::{Command, Stdio};
            let mut filter_child = Command::new("sh")
                .args(&["-c", bin])
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .chain_err_summary(|| "Failed to start pager filter process")?;
            let stdin = filter_child
                .stdin
                .as_mut()
                .ok_or_else(|| "failed to open stdin")?;
            stdin
                .write_all(text.as_bytes())
                .chain_err_summary(|| "Failed to write to stdin")?;
            let out = filter_child
                .wait_with_output()
                .chain_err_summary(|| "Failed to wait on filter")?
                .stdout;
            let mut dev_null = std::fs::File::open("/dev/null")?;
            let mut embedded = crate::terminal::embed::EmbedGrid::new();
            embedded.set_terminal_size((80, 20));

            for b in out {
                embedded.process_byte(&mut dev_null, b);
            }
            Ok(std::mem::replace(embedded.buffer_mut(), Default::default()))
        };
        let buf = _f(cmd, &self.text);
        if let Some((width, height)) = buf.as_ref().ok().map(CellBuffer::size) {
            self.width = width;
            self.height = height;
        }
        self.filtered_content = Some((cmd.to_string(), buf));
    }

    pub fn cursor_pos(&self) -> usize {
        self.cursor.1
    }

    pub fn size(&self) -> (usize, usize) {
        (self.width, self.height)
    }

    pub fn initialise(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let mut width = width!(area);
        if width < self.minimum_width {
            width = self.minimum_width;
        }
        if self.filtered_content.is_none() {
            if self.line_breaker.width() != Some(width.saturating_sub(4)) {
                let line_breaker = LineBreakText::new(
                    self.text.clone(),
                    self.reflow,
                    Some(width.saturating_sub(4)),
                );

                self.line_breaker = line_breaker;
                self.text_lines.clear();
            };
            self.height = self.text_lines.len();
            self.width = width;
            if let Some(ref mut search) = self.search {
                use melib::text_processing::search::KMP;
                search.positions.clear();
                for (y, l) in self.text_lines.iter().enumerate() {
                    search.positions.extend(
                        l.kmp_search(&search.pattern)
                            .into_iter()
                            .map(|offset| (y, offset)),
                    );
                }
                if let Some(pos) = search.positions.get(search.cursor) {
                    if self.cursor.1 > pos.0 || self.cursor.1 + height!(area) < pos.0 {
                        self.cursor.1 = pos.0.saturating_sub(3);
                    }
                }
            }
            self.draw_lines_up_to(
                grid,
                area,
                context,
                self.cursor.1 + Self::PAGES_AHEAD_TO_RENDER_NO * height!(area),
            );
        }
        self.draw_page(grid, area, context);

        self.initialised = true;
    }

    pub fn draw_lines_up_to(
        &mut self,
        _grid: &mut CellBuffer,
        area: Area,
        _context: &mut Context,
        up_to: usize,
    ) {
        if self.line_breaker.is_finished() {
            return;
        }
        let old_lines_no = self.text_lines.len();
        if up_to == 0 {
            self.text_lines.extend(self.line_breaker.by_ref());
        } else {
            if old_lines_no >= up_to + height!(area) {
                return;
            }
            let new_lines_no = (up_to + height!(area)) - old_lines_no;
            self.text_lines
                .extend(self.line_breaker.by_ref().take(new_lines_no));
        };
        let new_lines_no = self.text_lines.len() - old_lines_no;
        if let Some(ref mut search) = self.search {
            use melib::text_processing::search::KMP;
            for (y, l) in self.text_lines.iter().enumerate().skip(old_lines_no) {
                search.positions.extend(
                    l.kmp_search(&search.pattern)
                        .into_iter()
                        .map(|offset| (y, offset)),
                );
            }
        }
        self.height += new_lines_no;
    }

    fn draw_page(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if let Some((ref cmd, ref filtered_content)) = self.filtered_content {
            match filtered_content {
                Ok(ref content) => {
                    copy_area(
                        grid,
                        &content,
                        area,
                        (
                            (
                                std::cmp::min(
                                    self.cursor.0,
                                    content.size().0.saturating_sub(width!(area)),
                                ),
                                std::cmp::min(
                                    self.cursor.1,
                                    content.size().1.saturating_sub(height!(area)),
                                ),
                            ),
                            pos_dec(content.size(), (1, 1)),
                        ),
                    );
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateSubStatus(
                            cmd.to_string(),
                        )));
                    return;
                }
                Err(ref err) => {
                    let mut cmd = cmd.as_str();
                    cmd.truncate_at_boundary(4);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateSubStatus(format!(
                            "{}: {}",
                            cmd, err
                        ))));
                }
            }
        }

        let (mut upper_left, bottom_right) = area;
        for l in self
            .text_lines
            .iter()
            .skip(self.cursor.1)
            .take(height!(area) + 1)
        {
            write_string_to_grid(
                l,
                grid,
                self.colors.fg,
                self.colors.bg,
                Attr::DEFAULT,
                (upper_left, bottom_right),
                None,
            );
            if l.starts_with("⤷") {
                grid[upper_left]
                    .set_fg(crate::conf::value(context, "highlight").fg)
                    .set_attrs(crate::conf::value(context, "highlight").attrs);
            }
            upper_left = pos_inc(upper_left, (0, 1));
        }

        if get_y(upper_left) <= get_y(bottom_right) {
            clear_area(
                grid,
                (upper_left, bottom_right),
                crate::conf::value(context, "theme_default"),
            );
        }

        let (upper_left, _bottom_right) = area;
        #[cfg(feature = "regexp")]
        {
            for text_formatter in crate::conf::text_format_regexps(context, "pager.envelope.body") {
                let t = grid.insert_tag(text_formatter.tag);
                for (i, l) in self
                    .text_lines
                    .iter()
                    .skip(self.cursor.1)
                    .enumerate()
                    .take(height!(area) + 1)
                {
                    let i = i + get_y(upper_left);
                    for (start, end) in text_formatter.regexp.find_iter(l) {
                        let start = start + get_x(upper_left);
                        let end = end + get_x(upper_left);
                        grid.set_tag(t, (start, i), (end, i));
                    }
                }
            }
        }
        let cursor_line = self.cursor.1;
        if let Some(ref mut search) = self.search {
            let results_attr = crate::conf::value(context, "pager.highlight_search");
            let results_current_attr =
                crate::conf::value(context, "pager.highlight_search_current");
            search.cursor = std::cmp::min(search.positions.len().saturating_sub(1), search.cursor);
            for (i, (y, x)) in search
                .positions
                .iter()
                .enumerate()
                .filter(|(_, (y, _))| *y >= cursor_line)
                .take(height!(area) + 1)
            {
                let x = *x + get_x(upper_left);
                let y = *y - cursor_line;
                for c in grid.row_iter(
                    x..x + search.pattern.grapheme_width(),
                    y + get_y(upper_left),
                ) {
                    if i == search.cursor {
                        grid[c]
                            .set_fg(results_current_attr.fg)
                            .set_bg(results_current_attr.bg)
                            .set_attrs(results_current_attr.attrs);
                    } else {
                        grid[c]
                            .set_fg(results_attr.fg)
                            .set_bg(results_attr.bg)
                            .set_attrs(results_attr.attrs);
                    }
                }
            }
        }
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

        if !self.initialised {
            self.initialise(grid, area, context);
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
                    self.draw_lines_up_to(
                        grid,
                        area,
                        context,
                        self.cursor.1 + Self::PAGES_AHEAD_TO_RENDER_NO * height,
                    );
                }
                PageMovement::PageDown(multiplier) => {
                    if self.cursor.1 + height * multiplier + 1 < self.height {
                        self.cursor.1 += height * multiplier;
                    } else if self.cursor.1 + height * multiplier > self.height {
                        self.cursor.1 = self.height.saturating_sub(1);
                    } else {
                        self.cursor.1 = (self.height / height) * height;
                    }
                    self.draw_lines_up_to(
                        grid,
                        area,
                        context,
                        self.cursor.1 + Self::PAGES_AHEAD_TO_RENDER_NO * height,
                    );
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
                    self.draw_lines_up_to(grid, area, context, 0);
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
        let (mut cols, mut rows) = (width!(area), height!(area));
        if cols < 2 || rows < 2 {
            return;
        }
        let (has_more_lines, (width, height)) = if self.filtered_content.is_some() {
            (false, (self.width, self.height))
        } else {
            (
                !self.line_breaker.is_finished(),
                (self.line_breaker.width().unwrap_or(cols), self.height),
            )
        };
        if self.show_scrollbar && rows < height {
            cols -= 1;
            rows -= 1;
        } else if self.search.is_some() {
            rows -= 1;
        }

        if self.show_scrollbar && cols < width {
            rows -= 1;
        }
        self.cursor = (
            std::cmp::min(width.saturating_sub(cols), self.cursor.0),
            std::cmp::min(height.saturating_sub(rows), self.cursor.1),
        );
        self.draw_page(
            grid,
            (upper_left!(area), pos_inc(upper_left!(area), (cols, rows))),
            context,
        );
        if self.show_scrollbar && rows < height {
            ScrollBar::default().set_show_arrows(true).draw(
                grid,
                (
                    set_x(upper_left!(area), get_x(bottom_right!(area))),
                    bottom_right!(area),
                ),
                context,
                /* position */
                self.cursor.1,
                /* visible_rows */
                rows,
                /* length */
                height,
            );
        }
        if self.show_scrollbar && cols < width {
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
        if (rows < height) || self.search.is_some() {
            const RESULTS_STR: &str = "Results for ";
            let shown_lines = self.cursor.1 + rows;
            let total_lines = height;
            if rows < height {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                        ScrollUpdate::Update {
                            id: self.id,
                            context: ScrollContext {
                                shown_lines,
                                total_lines,
                                has_more_lines,
                            },
                        },
                    )));
            } else {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                        ScrollUpdate::End(self.id),
                    )));
            };
            if let Some(ref search) = self.search {
                let status_message = format!(
                    "{results_str}{search_pattern}: {current_pos}/{total_results}{has_more_lines}",
                    results_str = RESULTS_STR,
                    search_pattern = &search.pattern,
                    current_pos = if search.positions.is_empty() {
                        0
                    } else {
                        search.cursor + 1
                    },
                    total_results = search.positions.len(),
                    has_more_lines = if !has_more_lines { "" } else { "(+)" }
                );
                let mut attribute = crate::conf::value(context, "status.bar");
                if !context.settings.terminal.use_color() {
                    attribute.attrs |= Attr::REVERSE;
                }
                let (_, y) = write_string_to_grid(
                    &status_message,
                    grid,
                    attribute.fg,
                    attribute.bg,
                    attribute.attrs,
                    (
                        set_y(upper_left!(area), get_y(bottom_right!(area))),
                        bottom_right!(area),
                    ),
                    None,
                );
                /* set search pattern to italics */
                let start_x = get_x(upper_left!(area)) + RESULTS_STR.len();
                for c in grid.row_iter(start_x..(start_x + search.pattern.grapheme_width()), y) {
                    grid[c].set_attrs(attribute.attrs | Attr::ITALICS);
                }
            };
        }
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = self.get_shortcuts(context);
        match event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.set_colors(crate::conf::value(context, "theme_default"));
                self.set_dirty(true);
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::DESCRIPTION]["scroll_up"]) =>
            {
                self.movement = Some(PageMovement::Up(1));
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Self::DESCRIPTION]["scroll_down"]) =>
            {
                self.movement = Some(PageMovement::Down(1));
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
            UIEvent::Action(View(Filter(ref cmd))) => {
                self.filter(cmd);
                self.initialised = false;
                self.dirty = true;
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
            UIEvent::Input(Key::Esc) if self.filtered_content.is_some() => {
                self.filtered_content = None;
                self.initialised = false;
                self.dirty = true;
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateSubStatus(
                        String::new(),
                    )));
                return true;
            }
            UIEvent::Resize => {
                self.initialised = false;
                self.dirty = true;
            }
            UIEvent::VisibilityChange(false) => {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                        ScrollUpdate::End(self.id),
                    )));
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateSubStatus(
                        String::new(),
                    )));
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
