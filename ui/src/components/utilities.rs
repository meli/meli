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

/*! Various useful components that can be used in a generic fashion.
 */
use super::*;

mod widgets;

pub use self::widgets::*;
use fnv::FnvHashSet;

/// A horizontally split in half container.
#[derive(Debug)]
pub struct HSplit {
    top: Box<dyn Component>,
    bottom: Box<dyn Component>,
    show_divider: bool,
    ratio: usize, // bottom/whole height * 100
    id: ComponentId,
}

impl fmt::Display for HSplit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display subject/info
        Display::fmt(&self.top, f)
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
            id: ComponentId::new_v4(),
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

    fn set_dirty(&mut self) {
        self.top.set_dirty();
        self.bottom.set_dirty();
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut top_map = self.top.get_shortcuts(context);
        top_map.extend(self.bottom.get_shortcuts(context).into_iter());
        top_map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
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

impl fmt::Display for VSplit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display focused component
        Display::fmt(&self.right, f)
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
            id: ComponentId::new_v4(),
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
            self.set_dirty();
            self.prev_visibility = visibility;
        }
        let right_component_width = match visibility {
            (true, true) => (self.ratio * total_cols) / 100,
            (false, true) => total_cols,
            (true, false) => 0,
            (false, false) => {
                clear_area(grid, area);
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
                grid[(mid, get_y(upper_left) - 1)].set_ch(LIGHT_DOWN_AND_HORIZONTAL);
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
                    grid[(mid, get_y(bottom_right) + 1)].set_ch(LIGHT_UP_AND_HORIZONTAL);
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
        (self.left.process_event(event, context) || self.right.process_event(event, context))
    }

    fn is_dirty(&self) -> bool {
        self.left.is_dirty() || self.right.is_dirty()
    }

    fn set_dirty(&mut self) {
        self.left.set_dirty();
        self.right.set_dirty();
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut right_map = self.right.get_shortcuts(context);
        right_map.extend(self.left.get_shortcuts(context).into_iter());
        right_map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

#[derive(Debug)]
pub enum PageMovement {
    Home,
    PageUp,
    PageDown,
    End,
}

/// A pager for text.
/// `Pager` holds its own content in its own `CellBuffer` and when `draw` is called, it draws the
/// current view of the text. It is responsible for scrolling etc.
#[derive(Default, Debug)]
pub struct Pager {
    text: String,
    cursor_pos: usize,
    max_cursor_pos: Option<usize>,
    height: usize,
    width: usize,
    dirty: bool,
    content: CellBuffer,
    movement: Option<PageMovement>,
    id: ComponentId,
}

impl fmt::Display for Pager {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display info
        write!(f, "{}", Pager::DESCRIPTION)
    }
}

impl Pager {
    const DESCRIPTION: &'static str = "pager";
    pub fn update_from_str(&mut self, text: &str, width: Option<usize>) {
        let lines: Vec<&str> = if let Some(width) = width {
            word_break_string(text, width)
        } else {
            text.trim().split('\n').collect()
        };

        let height = lines.len() + 1;
        let width = width.unwrap_or_else(|| lines.iter().map(|l| l.len()).max().unwrap_or(0));
        let mut content = CellBuffer::new(width, height, Cell::with_char(' '));
        Pager::print_string(&mut content, lines);
        self.text = text.to_string();
        self.content = content;
        self.height = height;
        self.width = width;
        self.dirty = true;
        self.cursor_pos = 0;
        self.max_cursor_pos = None;
    }
    pub fn from_string(
        mut text: String,
        context: Option<&Context>,
        cursor_pos: Option<usize>,
        width: Option<usize>,
    ) -> Self {
        let pager_filter: Option<&String> = if let Some(context) = context {
            context.settings.pager.filter.as_ref()
        //let format_flowed: bool = context.settings.pager.format_flowed;
        } else {
            None
        };

        if let Some(bin) = pager_filter {
            use std::io::Write;
            use std::process::{Command, Stdio};
            let mut filter_child = Command::new(bin)
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
        }

        let content = {
            let lines: Vec<&str> = if let Some(width) = width {
                word_break_string(text.as_str(), width)
            } else {
                text.trim().split('\n').collect()
            };

            let height = lines.len() + 1;
            let width = width.unwrap_or_else(|| lines.iter().map(|l| l.len()).max().unwrap_or(0));
            let mut content = CellBuffer::new(width, height, Cell::with_char(' '));
            //interpret_format_flowed(&text);
            Pager::print_string(&mut content, lines);
            content
        };
        Pager {
            text,
            cursor_pos: cursor_pos.unwrap_or(0),
            height: content.size().1,
            width: content.size().0,
            dirty: true,
            content,
            id: ComponentId::new_v4(),
            ..Default::default()
        }
    }
    pub fn from_str(text: &str, cursor_pos: Option<usize>, width: Option<usize>) -> Self {
        let lines: Vec<&str> = if let Some(width) = width {
            word_break_string(text, width)
        } else {
            text.trim().split('\n').collect()
        };

        let height = lines.len() + 1;
        let width = width.unwrap_or_else(|| lines.iter().map(|l| l.len()).max().unwrap_or(0));
        let mut content = CellBuffer::new(width, height, Cell::with_char(' '));

        Pager::print_string(&mut content, lines);
        Pager {
            text: text.to_string(),
            cursor_pos: cursor_pos.unwrap_or(0),
            height,
            width,
            dirty: true,
            content,
            id: ComponentId::new_v4(),
            ..Default::default()
        }
    }
    pub fn from_buf(content: CellBuffer, cursor_pos: Option<usize>) -> Self {
        let (width, height) = content.size();
        Pager {
            text: String::new(),
            cursor_pos: cursor_pos.unwrap_or(0),
            height,
            width,
            dirty: true,
            content,
            id: ComponentId::new_v4(),
            ..Default::default()
        }
    }
    pub fn print_string(content: &mut CellBuffer, lines: Vec<&str>) {
        let width = content.size().0;
        for (i, l) in lines.iter().enumerate() {
            write_string_to_grid(
                l,
                content,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, i), (width - 1, i)),
                true,
            );
        }
    }
    pub fn cursor_pos(&self) -> usize {
        self.cursor_pos
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

        self.dirty = false;

        let height = height!(area);
        if let Some(mvm) = self.movement.take() {
            match mvm {
                PageMovement::PageUp => {
                    self.cursor_pos = self.cursor_pos.saturating_sub(height);
                }
                PageMovement::PageDown => {
                    if self.cursor_pos + height < self.height {
                        self.cursor_pos += height;
                    }
                }
                PageMovement::Home => {
                    self.cursor_pos = 0;
                }
                PageMovement::End => {
                    self.cursor_pos = (self.height / height) * height;
                }
            }
        }

        if self.height == 0 || self.width == 0 {
            return;
        }

        clear_area(grid, area);
        //let pager_context: usize = context.settings.pager.pager_context;
        //let pager_stop: bool = context.settings.pager.pager_stop;
        //let rows = y(bottom_right) - y(upper_left);
        //let page_length = rows / self.height;
        let width = width!(area);
        if width != self.width {
            // Reflow text.
            let lines: Vec<&str> = word_break_string(self.text.as_str(), width);
            let height = lines.len() + 1;
            self.width = width;
            self.height = height;
            self.content = CellBuffer::new(width, height, Cell::with_char(' '));
            Pager::print_string(&mut self.content, lines);
        }
        if self.cursor_pos + height >= self.height {
            self.cursor_pos = self.height.saturating_sub(height);
        };
        copy_area_with_break(
            grid,
            &self.content,
            area,
            ((0, self.cursor_pos), (self.width - 1, self.height - 1)),
        );
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = &self.get_shortcuts(context)[Self::DESCRIPTION];
        match event {
            UIEvent::Input(ref key) if *key == shortcuts["scroll_up"] => {
                if self.cursor_pos > 0 {
                    self.cursor_pos -= 1;
                    self.dirty = true;
                }
                return true;
            }
            UIEvent::Input(ref key) if *key == shortcuts["scroll_down"] => {
                if self.height > 0 && self.cursor_pos + 1 < self.height {
                    self.cursor_pos += 1;
                    self.dirty = true;
                }
                return true;
            }
            UIEvent::Input(ref key) if *key == shortcuts["page_up"] => {
                self.movement = Some(PageMovement::PageUp);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key) if *key == shortcuts["page_down"] => {
                self.movement = Some(PageMovement::PageDown);
                self.dirty = true;
                return true;
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.dirty = true;
            }
            UIEvent::Action(Pager(Pipe(ref bin, ref args))) => {
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
                        args.join(", ")
                    ))));
                return true;
            }
            UIEvent::Resize => {
                self.dirty = true;
                self.max_cursor_pos = None;
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
    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let config_map: FnvHashMap<&'static str, &Key> =
            context.settings.shortcuts.pager.key_values();
        [(
            Pager::DESCRIPTION.to_string(),
            [
                ("scroll_up", (*config_map["scroll_up"]).clone()),
                ("scroll_down", (*config_map["scroll_down"]).clone()),
                ("page_up", (*config_map["page_up"]).clone()),
                ("page_down", (*config_map["page_down"]).clone()),
            ]
            .iter()
            .cloned()
            .collect::<ShortcutMap>(),
        )]
        .iter()
        .cloned()
        .collect()
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

/// Status bar.
#[derive(Debug)]
pub struct StatusBar {
    container: Box<dyn Component>,
    status: String,
    notifications: VecDeque<String>,
    ex_buffer: Field,
    display_buffer: String,
    mode: UIMode,
    height: usize,
    dirty: bool,
    id: ComponentId,

    auto_complete: AutoComplete,
    cmd_history: Vec<String>,
}

impl fmt::Display for StatusBar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display info
        write!(f, "status bar")
    }
}

impl StatusBar {
    pub fn new(container: Box<dyn Component>) -> Self {
        StatusBar {
            container,
            status: String::with_capacity(256),
            notifications: VecDeque::new(),
            ex_buffer: Field::Text(UText::new(String::with_capacity(256)), None),
            display_buffer: String::with_capacity(8),
            dirty: true,
            mode: UIMode::Normal,
            height: 1,
            id: ComponentId::new_v4(),
            auto_complete: AutoComplete::new(Vec::new()),

            cmd_history: crate::execute::history::old_cmd_history(),
        }
    }
    fn draw_status_bar(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        clear_area(grid, area);
        let (x, y) = write_string_to_grid(
            &self.status,
            grid,
            Color::Byte(123),
            Color::Byte(26),
            Attr::Default,
            area,
            false,
        );
        let offset = self.status.find('|').unwrap_or_else(|| self.status.len());
        for x in get_x(upper_left!(area))..get_x(upper_left!(area)) + offset {
            grid[(x, y)].set_attrs(Attr::Bold);
        }
        if let Some(n) = self.notifications.pop_front() {
            write_string_to_grid(
                &n,
                grid,
                Color::Byte(219),
                Color::Byte(88),
                Attr::Default,
                (
                    (std::cmp::max(x, width!(area).saturating_sub(n.len())), y),
                    bottom_right!(area),
                ),
                false,
            );
        }
        let (x, y) = bottom_right!(area);
        for (idx, c) in self.display_buffer.chars().rev().enumerate() {
            if let Some(cell) = grid.get_mut(x.saturating_sub(idx).saturating_sub(1), y) {
                cell.set_ch(c);
            } else {
                break;
            }
        }

        change_colors(grid, area, Color::Byte(123), Color::Byte(26));
        context.dirty_areas.push_back(area);
    }
    fn draw_execute_bar(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        clear_area(grid, area);
        let (x, y) = write_string_to_grid(
            self.ex_buffer.as_str(),
            grid,
            Color::Byte(219),
            Color::Byte(88),
            Attr::Default,
            area,
            false,
        );
        grid[(x, y)].set_attrs(Attr::Underline);
        change_colors(grid, area, Color::Byte(219), Color::Byte(88));
        context.dirty_areas.push_back(area);
    }
}

impl Component for StatusBar {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !is_valid_area!(area) {
            return;
        }
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let total_rows = get_y(bottom_right) - get_y(upper_left);
        if total_rows <= self.height {
            return;
        }
        let height = self.height;

        self.container.draw(
            grid,
            (
                upper_left,
                (get_x(bottom_right), get_y(bottom_right) - height),
            ),
            context,
        );

        if !self.is_dirty() {
            return;
        }
        self.dirty = false;
        self.draw_status_bar(
            grid,
            (set_y(upper_left, get_y(bottom_right)), bottom_right),
            context,
        );
        match self.mode {
            UIMode::Normal => {}
            UIMode::Execute => {
                self.draw_execute_bar(
                    grid,
                    (
                        set_y(upper_left, get_y(bottom_right) - height + 1),
                        set_y(bottom_right, get_y(bottom_right) - height + 1),
                    ),
                    context,
                );
                /* don't autocomplete for less than 3 characters */
                if self.ex_buffer.as_str().split_graphemes().len() <= 2 {
                    return;
                }

                let mut unique_suggestions: FnvHashSet<&str> = FnvHashSet::default();
                let mut suggestions: Vec<AutoCompleteEntry> = self
                    .cmd_history
                    .iter()
                    .filter_map(|h| {
                        let sug = self.ex_buffer.as_str();
                        if h.starts_with(sug) && !unique_suggestions.contains(sug) {
                            unique_suggestions.insert(sug);
                            Some(h.clone().into())
                        } else {
                            None
                        }
                    })
                    .collect();
                suggestions.extend(crate::execute::COMMAND_COMPLETION.iter().filter_map(|e| {
                    if e.0.starts_with(self.ex_buffer.as_str()) {
                        Some(e.into())
                    } else {
                        None
                    }
                }));
                if suggestions.is_empty() && !self.auto_complete.suggestions().is_empty() {
                    self.auto_complete.set_suggestions(suggestions);
                    /* redraw self.container because we have got ridden of an autocomplete
                     * box, and it must be drawn over */
                    self.container.set_dirty();
                    return;
                }
                /* redraw self.container because we have less suggestions than before */
                if suggestions.len() < self.auto_complete.suggestions().len() {
                    self.container.set_dirty();
                }

                if self.auto_complete.set_suggestions(suggestions) {
                    let len = self.auto_complete.suggestions().len() - 1;
                    self.auto_complete.set_cursor(len);
                }
                let hist_height = std::cmp::min(15, self.auto_complete.suggestions().len());
                let hist_area = if height < self.auto_complete.suggestions().len() {
                    let mut scrollbar = ScrollBar::default();
                    scrollbar.set_show_arrows(false);
                    scrollbar.set_block_character(Some('▌'));
                    scrollbar.draw(
                        grid,
                        (
                            (
                                get_x(upper_left),
                                get_y(bottom_right) - height - hist_height + 1,
                            ),
                            set_y(bottom_right, get_y(bottom_right) - height),
                        ),
                        self.auto_complete.cursor(),
                        hist_height,
                        self.auto_complete.suggestions().len(),
                    );
                    change_colors(
                        grid,
                        (
                            (
                                get_x(upper_left),
                                get_y(bottom_right) - height - hist_height + 1,
                            ),
                            set_y(bottom_right, get_y(bottom_right) - height),
                        ),
                        Color::Byte(197), // DeepPink2,
                        Color::Byte(174), //LightPink3
                    );
                    context.dirty_areas.push_back((
                        (
                            get_x(upper_left),
                            get_y(bottom_right) - height - hist_height + 1,
                        ),
                        set_y(bottom_right, get_y(bottom_right) - height),
                    ));
                    (
                        (
                            get_x(upper_left) + 1,
                            get_y(bottom_right) - height - hist_height + 1,
                        ),
                        set_y(bottom_right, get_y(bottom_right) - height),
                    )
                } else {
                    (
                        (
                            get_x(upper_left),
                            get_y(bottom_right) - height - hist_height + 1,
                        ),
                        set_y(bottom_right, get_y(bottom_right) - height),
                    )
                };
                let offset = if hist_height
                    > (self.auto_complete.suggestions().len() - self.auto_complete.cursor())
                {
                    self.auto_complete.suggestions().len() - hist_height
                } else {
                    self.auto_complete.cursor()
                };
                clear_area(grid, hist_area);
                if hist_height > 0 {
                    change_colors(
                        grid,
                        hist_area,
                        Color::Byte(88),  // DarkRed,
                        Color::Byte(174), //LightPink3
                    );
                }
                for (y_offset, s) in self
                    .auto_complete
                    .suggestions()
                    .iter()
                    .skip(offset)
                    .take(hist_height)
                    .enumerate()
                {
                    let (x, y) = write_string_to_grid(
                        s.as_str(),
                        grid,
                        Color::Byte(88),  // DarkRed,
                        Color::Byte(174), //LightPink3
                        Attr::Default,
                        (
                            set_y(
                                upper_left!(hist_area),
                                get_y(bottom_right!(hist_area)) - hist_height + y_offset + 1,
                            ),
                            bottom_right!(hist_area),
                        ),
                        true,
                    );
                    write_string_to_grid(
                        &s.description,
                        grid,
                        Color::White,
                        Color::Byte(174),
                        Attr::Default,
                        ((x + 2, y), bottom_right!(hist_area)),
                        false,
                    );
                    if y_offset + offset == self.auto_complete.cursor() {
                        change_colors(
                            grid,
                            (
                                set_y(
                                    upper_left!(hist_area),
                                    get_y(bottom_right!(hist_area)) - hist_height + y_offset + 1,
                                ),
                                set_y(
                                    bottom_right!(hist_area),
                                    get_y(bottom_right!(hist_area)) - hist_height + y_offset + 1,
                                ),
                            ),
                            Color::Byte(88),  // DarkRed,
                            Color::Byte(173), //LightSalmon3
                        );
                        write_string_to_grid(
                            &s.as_str()[self.ex_buffer.as_str().len()..],
                            grid,
                            Color::Byte(97), // MediumPurple3,
                            Color::Byte(88), //LightPink3
                            Attr::Default,
                            (
                                (
                                    get_y(upper_left)
                                        + self.ex_buffer.as_str().split_graphemes().len(),
                                    get_y(bottom_right) - height + 1,
                                ),
                                set_y(bottom_right, get_y(bottom_right) - height + 1),
                            ),
                            true,
                        );
                    }
                }
                context.dirty_areas.push_back(hist_area);
            }
            _ => {}
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if self.container.process_event(event, context) {
            return true;
        }

        match event {
            UIEvent::ChangeMode(m) => {
                let offset = self.status.find('|').unwrap_or_else(|| self.status.len());
                self.status.replace_range(..offset, &format!("{} ", m));
                self.dirty = true;
                self.mode = *m;
                match m {
                    UIMode::Normal => {
                        self.height = 1;
                        if !self.ex_buffer.is_empty() {
                            context
                                .replies
                                .push_back(UIEvent::Command(self.ex_buffer.as_str().to_string()));
                        }
                        if parse_command(&self.ex_buffer.as_str().as_bytes())
                            .to_full_result()
                            .is_ok()
                            && self.cmd_history.last().map(String::as_str)
                                != Some(self.ex_buffer.as_str())
                        {
                            crate::execute::history::log_cmd(self.ex_buffer.as_str().to_string());
                            self.cmd_history.push(self.ex_buffer.as_str().to_string());
                        }
                        self.ex_buffer.clear();
                    }
                    UIMode::Execute => {
                        self.height = 2;
                    }
                    _ => {}
                };
            }
            UIEvent::ExInput(Key::Char('\t')) => {
                if let Some(suggestion) = self.auto_complete.get_suggestion() {
                    let mut utext = UText::new(suggestion);
                    let len = utext.as_str().len();
                    utext.set_cursor(len);
                    self.container.set_dirty();
                    self.set_dirty();
                    self.ex_buffer = Field::Text(utext, None);
                }
            }
            UIEvent::ExInput(Key::Char(c)) => {
                self.dirty = true;
                self.ex_buffer
                    .process_event(&mut UIEvent::InsertInput(Key::Char(*c)), context);
                return true;
            }
            UIEvent::ExInput(Key::Paste(s)) => {
                self.dirty = true;
                self.ex_buffer
                    .process_event(&mut UIEvent::InsertInput(Key::Paste(s.clone())), context);
                return true;
            }
            UIEvent::ExInput(Key::Ctrl('u')) => {
                self.dirty = true;
                self.ex_buffer.clear();
                return true;
            }
            UIEvent::ExInput(k @ Key::Backspace) | UIEvent::ExInput(k @ Key::Ctrl('h')) => {
                self.dirty = true;
                self.ex_buffer
                    .process_event(&mut UIEvent::InsertInput(k.clone()), context);
                return true;
            }
            UIEvent::ExInput(Key::Up) => {
                self.auto_complete.dec_cursor();
                self.dirty = true;
            }
            UIEvent::ExInput(Key::Down) => {
                self.auto_complete.inc_cursor();
                self.dirty = true;
            }
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::StatusEvent(StatusEvent::DisplayMessage(s)) => {
                self.notifications.push_back(s.clone());
                self.dirty = true;
            }
            UIEvent::StatusEvent(StatusEvent::BufClear) => {
                self.display_buffer.clear();
                self.dirty = true;
            }
            UIEvent::StatusEvent(StatusEvent::BufSet(s)) => {
                self.display_buffer = s.clone();
                self.dirty = true;
            }
            UIEvent::StatusEvent(StatusEvent::UpdateStatus(ref mut s)) => {
                self.status = format!("{} | {}", self.mode, std::mem::replace(s, String::new()));
                self.dirty = true;
            }
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.container.is_dirty()
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        self.container.get_shortcuts(context)
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

#[derive(Debug)]
pub struct Progress {
    description: String,
    total_work: usize,
    finished: usize,
    id: ComponentId,
}

impl Progress {
    pub fn new(s: String, total_work: usize) -> Self {
        Progress {
            description: s,
            total_work,
            finished: 0,
            id: ComponentId::new_v4(),
        }
    }

    pub fn add_work(&mut self, n: usize) {
        if self.finished >= self.total_work {
            return;
        }
        self.finished += n;
    }

    pub fn percentage(&self) -> usize {
        if self.total_work > 0 {
            100 * self.finished / self.total_work
        } else {
            0
        }
    }

    pub fn description(&self) -> &str {
        &self.description
    }
}

impl fmt::Display for Progress {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display info
        write!(f, "progress bar")
    }
}

impl Component for Progress {
    fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {
        unimplemented!()
    }
    fn process_event(&mut self, _event: &mut UIEvent, _context: &mut Context) -> bool {
        false
    }
    fn set_dirty(&mut self) {}

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

#[derive(Debug)]
pub struct Tabbed {
    pinned: usize,
    children: Vec<Box<dyn Component>>,
    cursor_pos: usize,

    show_shortcuts: bool,

    dirty: bool,
    id: ComponentId,
}

impl Tabbed {
    pub fn new(children: Vec<Box<dyn Component>>) -> Self {
        let pinned = children.len();
        Tabbed {
            pinned,
            children,
            cursor_pos: 0,
            show_shortcuts: false,
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }
    fn draw_tabs(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);

        if self.children.is_empty() {
            clear_area(grid, area);
            return;
        }

        let mut x = get_x(upper_left);
        let y: usize = get_y(upper_left);
        for (idx, c) in self.children.iter().enumerate() {
            let (fg, bg) = if idx == self.cursor_pos {
                (Color::Default, Color::Default)
            } else {
                (Color::Byte(15), Color::Byte(8))
            };
            let (x_, _y_) = write_string_to_grid(
                &format!(" {} ", c),
                grid,
                fg,
                bg,
                Attr::Default,
                (set_x(upper_left, x), bottom_right!(area)),
                false,
            );
            x = x_ + 1;
            if idx == self.pinned.saturating_sub(1) {
                x += 2;
            }
            if y != _y_ {
                break;
            }
        }
        let (cols, _) = grid.size();
        let cslice: &mut [Cell] = grid;
        //TODO: bounds check
        let cslice_len = cslice.len();
        for c in cslice[(y * cols) + x.saturating_sub(1)
            ..std::cmp::min((y * cols) + x.saturating_sub(1), cslice_len)]
            .iter_mut()
        {
            c.set_bg(Color::Byte(7));
            c.set_ch(' ');
        }

        if self.cursor_pos == self.children.len() - 1 {
            cslice[(y * cols) + x].set_ch('▍');
            cslice[(y * cols) + x].set_fg(Color::Byte(8));
            cslice[(y * cols) + x].set_bg(Color::Default);
        }

        context.dirty_areas.push_back(area);
    }
    pub fn add_component(&mut self, new: Box<dyn Component>) {
        self.children.push(new);
    }
}

impl fmt::Display for Tabbed {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO display info
        write!(f, "tabs")
    }
}

impl Component for Tabbed {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.dirty {
            clear_area(
                grid,
                (
                    upper_left!(area),
                    set_x(upper_left!(area), get_x(bottom_right!(area))),
                ),
            );
            self.dirty = false;
        }

        if self.children.len() > 1 {
            self.draw_tabs(
                grid,
                (
                    upper_left!(area),
                    set_x(upper_left!(area), get_x(bottom_right!(area))),
                ),
                context,
            );
            let y = get_y(upper_left!(area));
            self.children[self.cursor_pos].draw(
                grid,
                (set_y(upper_left!(area), y + 1), bottom_right!(area)),
                context,
            );
        } else {
            self.children[self.cursor_pos].draw(grid, area, context);
        }

        if self.show_shortcuts {
            let area = (
                pos_inc(upper_left!(area), (2, 1)),
                set_x(
                    bottom_right!(area),
                    get_x(bottom_right!(area)).saturating_sub(2),
                ),
            );
            clear_area(grid, area);
            create_box(grid, area);

            let mut idx = 0;
            // TODO: print into a pager
            for (desc, shortcuts) in self.children[self.cursor_pos]
                .get_shortcuts(context)
                .into_iter()
            {
                write_string_to_grid(
                    &desc,
                    grid,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    (
                        pos_inc(upper_left!(area), (2, 1 + idx)),
                        set_x(
                            bottom_right!(area),
                            get_x(bottom_right!(area)).saturating_sub(2),
                        ),
                    ),
                    false,
                );
                idx += 2;
                let mut shortcuts = shortcuts.into_iter().collect::<Vec<_>>();
                shortcuts.sort_unstable_by_key(|(ref k, _)| *k);
                for (k, v) in shortcuts {
                    let (x, y) = write_string_to_grid(
                        &k,
                        grid,
                        Color::Byte(29),
                        Color::Default,
                        Attr::Default,
                        (
                            pos_inc(upper_left!(area), (2, 1 + idx)),
                            set_x(
                                bottom_right!(area),
                                get_x(bottom_right!(area)).saturating_sub(2),
                            ),
                        ),
                        false,
                    );
                    write_string_to_grid(
                        &format!("{}", v),
                        grid,
                        Color::Default,
                        Color::Default,
                        Attr::Default,
                        (
                            (x + 2, y),
                            set_x(
                                bottom_right!(area),
                                get_x(bottom_right!(area)).saturating_sub(2),
                            ),
                        ),
                        false,
                    );
                    idx += 1;
                }
                idx += 1;
            }
            context.dirty_areas.push_back(area);
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match *event {
            UIEvent::Input(Key::Char('T')) => {
                self.cursor_pos = (self.cursor_pos + 1) % self.children.len();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.children[self.cursor_pos]
                            .get_status(context)
                            .unwrap_or_default(),
                    )));
                self.set_dirty();
                return true;
            }
            UIEvent::Input(Key::Char('?')) => {
                self.show_shortcuts = !self.show_shortcuts;
                self.set_dirty();
                return true;
            }
            UIEvent::Action(Tab(NewDraft(account_idx, ref draft))) => {
                let mut composer = Composer::new(account_idx);
                if let Some(draft) = draft {
                    composer.set_draft(draft.clone());
                }
                self.add_component(Box::new(composer));
                self.cursor_pos = self.children.len() - 1;
                self.children[self.cursor_pos].set_dirty();
                return true;
            }
            UIEvent::Action(Tab(Reply(coordinates, msg))) => {
                self.add_component(Box::new(Composer::with_context(coordinates, msg, context)));
                self.cursor_pos = self.children.len() - 1;
                self.children[self.cursor_pos].set_dirty();
                return true;
            }
            UIEvent::Action(Tab(Edit(account_pos, msg))) => {
                self.add_component(Box::new(Composer::edit(account_pos, msg, context)));
                self.cursor_pos = self.children.len() - 1;
                self.children[self.cursor_pos].set_dirty();
                return true;
            }
            UIEvent::Action(Tab(New(ref mut e))) if e.is_some() => {
                self.add_component(e.take().unwrap());
                self.cursor_pos = self.children.len() - 1;
                self.children[self.cursor_pos].set_dirty();
                return true;
            }
            UIEvent::Action(Tab(Close)) => {
                if self.pinned > self.cursor_pos {
                    return true;
                }
                let id = self.children[self.cursor_pos].id();
                self.children[self.cursor_pos].kill(id, context);
                self.set_dirty();
                return true;
            }
            UIEvent::Action(Tab(Kill(id))) => {
                if self.pinned > self.cursor_pos {
                    return true;
                }
                if let Some(c_idx) = self.children.iter().position(|x| x.id() == id) {
                    self.children.remove(c_idx);
                    self.cursor_pos = 0;
                    self.set_dirty();
                    return true;
                } else {
                    debug!(
                        "DEBUG: Child component with id {:?} not found.\nList: {:?}",
                        id, self.children
                    );
                }
            }
            _ => {}
        }
        self.children[self.cursor_pos].process_event(event, context)
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.children[self.cursor_pos].is_dirty()
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
        self.children[self.cursor_pos].set_dirty();
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

type EntryIdentifier = Vec<u8>;
/// Shows selection to user
#[derive(Debug, PartialEq)]
pub struct Selector {
    single_only: bool,
    /// allow only one selection
    entries: Vec<(EntryIdentifier, bool)>,
    selected_entry_count: u32,
    content: CellBuffer,

    cursor: usize,

    dirty: bool,
    id: ComponentId,
}

impl fmt::Display for Selector {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("Selector", f)
    }
}

impl Component for Selector {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let (width, height) = self.content.size();
        copy_area_with_break(grid, &self.content, area, ((0, 0), (width, height)));
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        let (width, height) = self.content.size();
        match *event {
            UIEvent::Input(Key::Char('\t')) => {
                self.entries[self.cursor].1 = !self.entries[self.cursor].1;
                if self.entries[self.cursor].1 {
                    write_string_to_grid(
                        "x",
                        &mut self.content,
                        Color::Default,
                        Color::Default,
                        Attr::Default,
                        ((1, self.cursor), (width, self.cursor)),
                        false,
                    );
                } else {
                    write_string_to_grid(
                        " ",
                        &mut self.content,
                        Color::Default,
                        Color::Default,
                        Attr::Default,
                        ((1, self.cursor), (width, self.cursor)),
                        false,
                    );
                }
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Up) if self.cursor > 0 => {
                self.cursor -= 1;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Down) if self.cursor < height.saturating_sub(1) => {
                self.cursor += 1;
                self.dirty = true;
                return true;
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

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }
}

impl Selector {
    pub fn new(mut entries: Vec<(EntryIdentifier, String)>, single_only: bool) -> Selector {
        let width = entries
            .iter()
            .max_by_key(|e| e.1.len())
            .map(|v| v.1.len())
            .unwrap_or(0)
            + 4;
        let height = entries.len();
        let mut content = CellBuffer::new(width, height, Cell::with_char(' '));
        let identifiers = entries
            .iter_mut()
            .map(|(id, _)| (std::mem::replace(&mut *id, Vec::new()), false))
            .collect();
        for (i, e) in entries.into_iter().enumerate() {
            write_string_to_grid(
                &format!("[ ] {}", e.1),
                &mut content,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, i), (width - 1, i)),
                false,
            );
        }

        Selector {
            single_only,
            entries: identifiers,
            selected_entry_count: 0,
            content,
            cursor: 0,
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }

    pub fn collect(self) -> Vec<EntryIdentifier> {
        self.entries
            .into_iter()
            .filter(|v| v.1)
            .map(|(id, _)| id)
            .collect()
    }
}
