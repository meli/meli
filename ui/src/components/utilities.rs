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
use text_processing::Reflow;

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

#[derive(Debug, Clone, Copy)]
pub enum PageMovement {
    Up(usize),
    Right(usize),
    Left(usize),
    Down(usize),
    PageUp(usize),
    PageDown(usize),
    Home,
    End,
}

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
    pub fn set_reflow(&mut self, new_val: Reflow) {
        self.reflow = new_val;
    }

    pub fn reflow(&self) -> Reflow {
        self.reflow
    }

    pub fn update_from_str(&mut self, text: &str, width: Option<usize>) {
        let lines: Vec<String> = text.split_lines_reflow(self.reflow, width);
        let height = lines.len() + 2;
        let width = width.unwrap_or_else(|| lines.iter().map(|l| l.len()).max().unwrap_or(0));
        let ascii_drawing = self.content.ascii_drawing;
        let mut content = CellBuffer::new(width, height, Cell::with_char(' '));
        content.set_ascii_drawing(ascii_drawing);
        Pager::print_string(&mut content, lines);
        self.text = text.to_string();
        self.content = content;
        self.height = height;
        self.width = width;
        self.dirty = true;
        self.cursor = (0, 0);
    }
    pub fn from_string(
        mut text: String,
        context: Option<&Context>,
        cursor_pos: Option<usize>,
        width: Option<usize>,
    ) -> Self {
        let pager_filter: Option<&String> = if let Some(context) = context {
            context.settings.pager.filter.as_ref()
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
            let lines: Vec<String> = if let Some(width) = width {
                text.split_lines(width)
            } else {
                text.trim().split('\n').map(str::to_string).collect()
            };

            let height = lines.len() + 1;
            let width = width.unwrap_or_else(|| lines.iter().map(|l| l.len()).max().unwrap_or(0));
            let mut content = if let Some(context) = context {
                CellBuffer::new_with_context(width, height, Cell::with_char(' '), context)
            } else {
                CellBuffer::new(width, height, Cell::with_char(' '))
            };
            Pager::print_string(&mut content, lines);
            content
        };
        Pager {
            text,
            cursor: (0, cursor_pos.unwrap_or(0)),
            height: content.size().1,
            width: content.size().0,
            dirty: true,
            content,
            id: ComponentId::new_v4(),
            ..Default::default()
        }
    }
    pub fn from_str(text: &str, cursor_pos: Option<usize>, width: Option<usize>) -> Self {
        let lines: Vec<String> = if let Some(width) = width {
            text.split_lines(width)
        } else {
            text.trim().split('\n').map(str::to_string).collect()
        };

        let height = lines.len() + 1;
        let width = width.unwrap_or_else(|| lines.iter().map(|l| l.len()).max().unwrap_or(0));
        let mut content = CellBuffer::new(width, height, Cell::with_char(' '));

        Pager::print_string(&mut content, lines);
        Pager {
            text: text.to_string(),
            cursor: (0, cursor_pos.unwrap_or(0)),
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
            cursor: (0, cursor_pos.unwrap_or(0)),
            height,
            width,
            dirty: true,
            content,
            id: ComponentId::new_v4(),
            ..Default::default()
        }
    }
    pub fn print_string(content: &mut CellBuffer, lines: Vec<String>) {
        let width = content.size().0;
        for (i, l) in lines.iter().enumerate() {
            write_string_to_grid(
                l,
                content,
                Color::Default,
                Color::Default,
                Attr::Default,
                ((0, i), (width.saturating_sub(1), i)),
                None,
            );
        }
    }
    pub fn cursor_pos(&self) -> usize {
        self.cursor.1
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
                PageMovement::Up(amount) => {
                    self.cursor.1 = self.cursor.1.saturating_sub(amount);
                }
                PageMovement::PageUp(multiplier) => {
                    self.cursor.1 = self.cursor.1.saturating_sub(height * multiplier);
                }
                PageMovement::Down(amount) => {
                    if self.cursor.1 + amount < self.height {
                        self.cursor.1 += amount;
                    }
                }
                PageMovement::PageDown(multiplier) => {
                    if self.cursor.1 + height * multiplier < self.height {
                        self.cursor.1 += height * multiplier;
                    }
                }
                PageMovement::Right(multiplier) => {
                    let offset = width!(area) / 3;
                    if self.cursor.0 + offset * multiplier < self.content.size().0 {
                        self.cursor.0 += offset * multiplier;
                    }
                }
                PageMovement::Left(multiplier) => {
                    let offset = width!(area) / 3;
                    self.cursor.0 = self.cursor.0.saturating_sub(offset * multiplier);
                }
                PageMovement::Home => {
                    self.cursor.1 = 0;
                }
                PageMovement::End => {
                    self.cursor.1 = (self.height / height) * height;
                }
            }
        }

        if self.height == 0 || self.width == 0 {
            return;
        }

        clear_area(grid, area);
        let (width, height) = self.content.size();
        let (cols, rows) = (width!(area), height!(area));
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
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let shortcuts = &self.get_shortcuts(context)[Self::DESCRIPTION];
        match event {
            UIEvent::Input(ref key) if *key == shortcuts["scroll_up"] => {
                self.cursor.1 = self.cursor.1.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key) if *key == shortcuts["scroll_down"] => {
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
            UIEvent::Input(ref key) if *key == shortcuts["page_up"] => {
                self.movement = Some(PageMovement::PageUp(1));
                self.dirty = true;
                return true;
            }
            UIEvent::Input(ref key) if *key == shortcuts["page_down"] => {
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
            UIEvent::Resize => {
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
    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let config_map: FnvHashMap<&'static str, Key> =
            context.settings.shortcuts.pager.key_values();
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

/// Status bar.
#[derive(Debug)]
pub struct StatusBar {
    container: Box<dyn Component>,
    status: String,
    notifications: VecDeque<String>,
    cur_notification: Option<(std::time::Instant, String)>,
    ex_buffer: Field,
    ex_buffer_cmd_history_pos: Option<usize>,
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
            cur_notification: None,
            ex_buffer: Field::Text(UText::new(String::with_capacity(256)), None),
            ex_buffer_cmd_history_pos: None,
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
            None,
        );
        let offset = self.status.find('|').unwrap_or_else(|| self.status.len());
        if y < get_y(bottom_right!(area)) + 1 {
            for x in get_x(upper_left!(area))
                ..std::cmp::min(
                    get_x(upper_left!(area)) + offset,
                    get_x(bottom_right!(area)),
                )
            {
                grid[(x, y)].set_attrs(Attr::Bold);
            }
        }
        if self.cur_notification.is_some() {
            let (t, n) = self.cur_notification.as_ref().unwrap();
            if std::time::Instant::now().duration_since(*t) < std::time::Duration::new(5, 0) {
                write_string_to_grid(
                    n,
                    grid,
                    Color::Byte(219),
                    Color::Byte(88),
                    Attr::Default,
                    (
                        (std::cmp::max(x, width!(area).saturating_sub(n.len())), y),
                        bottom_right!(area),
                    ),
                    None,
                );
            } else {
                self.cur_notification = None;
            }
        }
        if self.cur_notification.is_none() {
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
                    None,
                );
                self.cur_notification = Some((std::time::Instant::now(), n));
            }
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
            None,
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

                    self.container.set_dirty();
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
                        Some(get_x(upper_left!(hist_area))),
                    );
                    write_string_to_grid(
                        &s.description,
                        grid,
                        Color::White,
                        Color::Byte(174),
                        Attr::Default,
                        ((x + 2, y), bottom_right!(hist_area)),
                        None,
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
                                    get_x(upper_left)
                                        + self.ex_buffer.as_str().split_graphemes().len(),
                                    get_y(bottom_right) - height + 1,
                                ),
                                set_y(bottom_right, get_y(bottom_right) - height + 1),
                            ),
                            None,
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
                self.set_dirty();
                self.container.set_dirty();
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
                        self.ex_buffer_cmd_history_pos.take();
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
                self.ex_buffer_cmd_history_pos.take();
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
            UIEvent::ExInput(Key::Ctrl('p')) => {
                let pos = self.ex_buffer_cmd_history_pos.map(|p| p + 1).unwrap_or(0);
                let pos = Some(std::cmp::min(pos, self.cmd_history.len().saturating_sub(1)));
                if pos != self.ex_buffer_cmd_history_pos {
                    let mut utext = UText::new(
                        self.cmd_history[self.cmd_history.len().saturating_sub(1) - pos.unwrap()]
                            .clone(),
                    );
                    let len = utext.as_str().len();
                    utext.set_cursor(len);
                    self.container.set_dirty();
                    self.set_dirty();
                    self.ex_buffer = Field::Text(utext, None);
                    self.ex_buffer_cmd_history_pos = pos;
                    self.dirty = true;
                }

                return true;
            }
            UIEvent::ExInput(Key::Ctrl('n')) => {
                if Some(0) == self.ex_buffer_cmd_history_pos {
                    self.ex_buffer_cmd_history_pos = None;
                    self.ex_buffer.clear();
                    self.dirty = true;
                } else if self.ex_buffer_cmd_history_pos.is_some() {
                    let pos = self.ex_buffer_cmd_history_pos.map(|p| p - 1);
                    let mut utext = UText::new(
                        self.cmd_history[self.cmd_history.len().saturating_sub(1) - pos.unwrap()]
                            .clone(),
                    );
                    let len = utext.as_str().len();
                    utext.set_cursor(len);
                    self.container.set_dirty();
                    self.set_dirty();
                    self.ex_buffer = Field::Text(utext, None);
                    self.ex_buffer_cmd_history_pos = pos;
                    self.dirty = true;
                }

                return true;
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

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        self.container.can_quit_cleanly(context)
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
    fn is_dirty(&self) -> bool {
        false
    }

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
    help_screen_cursor: (usize, usize),
    help_content: CellBuffer,
    help_curr_views: ShortcutMaps,

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
            help_content: CellBuffer::default(),
            help_screen_cursor: (0, 0),
            help_curr_views: ShortcutMaps::default(),
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
                None,
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
            context.dirty_areas.push_back((
                upper_left!(area),
                set_x(upper_left!(area), get_x(bottom_right!(area))),
            ));
        }

        /* If children are dirty but self isn't and the shortcuts panel is visible, it will get
         * overwritten. */
        let must_redraw_shortcuts: bool = self.show_shortcuts && !self.dirty && self.is_dirty();

        /* children should be drawn after the shortcuts/help panel lest they overwrite the panel on
         * the grid. the drawing order is determined by the dirty_areas queue which is LIFO */
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

        if (self.show_shortcuts && self.dirty) || must_redraw_shortcuts {
            let area = (
                pos_inc(upper_left!(area), (2, 1)),
                set_x(
                    bottom_right!(area),
                    get_x(bottom_right!(area)).saturating_sub(2),
                ),
            );
            context.dirty_areas.push_back(area);
            clear_area(grid, area);
            create_box(grid, area);
            let area = (
                pos_inc(upper_left!(area), (3, 2)),
                set_x(
                    bottom_right!(area),
                    get_x(bottom_right!(area)).saturating_sub(3),
                ),
            );
            let mut children_maps = self.children[self.cursor_pos].get_shortcuts(context);
            let our_map = self.get_shortcuts(context);
            children_maps.extend(our_map.into_iter());
            if children_maps.is_empty() {
                return;
            }
            if (children_maps == self.help_curr_views) && must_redraw_shortcuts {
                let (width, height) = self.help_content.size();
                let (cols, rows) = (width!(area), height!(area));
                copy_area(
                    grid,
                    &self.help_content,
                    area,
                    (
                        (
                            std::cmp::min(
                                (width - 1).saturating_sub(cols),
                                self.help_screen_cursor.0,
                            ),
                            std::cmp::min(
                                (height - 1).saturating_sub(rows),
                                self.help_screen_cursor.1,
                            ),
                        ),
                        (
                            std::cmp::min(self.help_screen_cursor.0 + cols, width - 1),
                            std::cmp::min(self.help_screen_cursor.1 + rows, height - 1),
                        ),
                    ),
                );
                self.dirty = false;
                return;
            }
            self.help_curr_views = children_maps;
            let mut max_length = 5;
            let mut max_width = "Use Up, Down, Left, Right to scroll.".len() + 3;
            for (desc, shortcuts) in self.help_curr_views.iter() {
                max_length += shortcuts.len() + 3;
                max_width = std::cmp::max(
                    max_width,
                    std::cmp::max(
                        desc.len(),
                        shortcuts.keys().map(|k| k.len() + 5).max().unwrap_or(0),
                    ),
                );
            }
            self.help_content =
                CellBuffer::new_with_context(max_width, max_length + 2, Cell::default(), context);
            let (width, height) = self.help_content.size();
            let (cols, rows) = (width!(area), height!(area));
            if cols == 0 || rows == 0 {
                return;
            }
            /* trim cursor if it's bigger than the help screen */
            self.help_screen_cursor = (
                std::cmp::min(width.saturating_sub(cols), self.help_screen_cursor.0),
                std::cmp::min(height.saturating_sub(rows), self.help_screen_cursor.1),
            );

            /* In this case we will be scrolling, so show the user how to do it */
            if height.wrapping_div(rows) > 0 || width.wrapping_div(cols) > 0 {
                write_string_to_grid(
                    "Use Up, Down, Left, Right to scroll.",
                    &mut self.help_content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((2, 0), (max_width.saturating_sub(2), max_length - 1)),
                    None,
                );
            }
            let mut idx = 0;
            for (desc, shortcuts) in self.help_curr_views.iter() {
                write_string_to_grid(
                    desc,
                    &mut self.help_content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((2, 2 + idx), (max_width.saturating_sub(2), max_length - 1)),
                    None,
                );
                idx += 2;
                let mut shortcuts = shortcuts.iter().collect::<Vec<_>>();
                shortcuts.sort_unstable_by_key(|(ref k, _)| *k);
                for (k, v) in shortcuts {
                    debug!(&(k, v));
                    let (x, y) = write_string_to_grid(
                        &k,
                        &mut self.help_content,
                        Color::Byte(29),
                        Color::Default,
                        Attr::Default,
                        ((2, 2 + idx), (max_width.saturating_sub(2), max_length - 1)),
                        None,
                    );
                    write_string_to_grid(
                        &format!("{}", v),
                        &mut self.help_content,
                        Color::Default,
                        Color::Default,
                        Attr::Default,
                        ((x + 2, y), (max_width.saturating_sub(2), max_length - 1)),
                        None,
                    );
                    idx += 1;
                }
                idx += 1;
            }
            copy_area(
                grid,
                &self.help_content,
                area,
                (
                    (
                        std::cmp::min((width - 1).saturating_sub(cols), self.help_screen_cursor.0),
                        std::cmp::min((height - 1).saturating_sub(rows), self.help_screen_cursor.1),
                    ),
                    (
                        std::cmp::min(self.help_screen_cursor.0 + cols, width - 1),
                        std::cmp::min(self.help_screen_cursor.1 + rows, height - 1),
                    ),
                ),
            );
        }
        self.dirty = false;
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        let our_shortcuts = self.get_shortcuts(context);
        let shortcuts: &ShortcutMap = &our_shortcuts["general"];
        match *event {
            UIEvent::Input(Key::Alt(no)) if no >= '1' && no <= '9' => {
                let no = no as usize - '1' as usize;
                if no < self.children.len() {
                    self.cursor_pos = no % self.children.len();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                            self.children[self.cursor_pos]
                                .get_status(context)
                                .unwrap_or_default(),
                        )));
                    self.set_dirty();
                }
                return true;
            }
            UIEvent::Input(ref key) if *key == shortcuts["next_tab"] => {
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
                if self.show_shortcuts {
                    /* children below the shortcut overlay must be redrawn */
                    self.set_dirty();
                }
                self.show_shortcuts = !self.show_shortcuts;
                self.dirty = true;
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
                let composer = match Composer::edit(account_pos, msg, context) {
                    Ok(c) => c,
                    Err(e) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Failed to open e-mail".to_string()),
                            e.to_string(),
                            Some(NotificationType::ERROR),
                        ));
                        log(
                            format!(
                                "Failed to open envelope {}: {}",
                                context.accounts[account_pos]
                                    .collection
                                    .get_env(msg)
                                    .message_id_display(),
                                e.to_string()
                            ),
                            ERROR,
                        );
                        return true;
                    }
                };
                self.add_component(Box::new(composer));
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
            UIEvent::Resize => {
                self.dirty = true;
            }
            UIEvent::Input(ref k) if self.show_shortcuts => {
                match k {
                    Key::Up => {
                        self.help_screen_cursor.1 = self.help_screen_cursor.1.saturating_sub(1);
                    }
                    Key::Down => {
                        self.help_screen_cursor.1 = self.help_screen_cursor.1 + 1;
                    }
                    Key::Left => {
                        self.help_screen_cursor.0 = self.help_screen_cursor.0.saturating_sub(1);
                    }
                    Key::Right => {
                        self.help_screen_cursor.0 = self.help_screen_cursor.0 + 1;
                    }
                    _ => {
                        /* ignore, don't pass to components below the shortcut panel */
                        return false;
                    }
                }
                self.dirty = true;
                return true;
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

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();
        map.insert("general", context.settings.shortcuts.general.key_values());
        map
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        for (i, c) in self.children.iter_mut().enumerate() {
            if !c.can_quit_cleanly(context) {
                self.cursor_pos = i;
                self.set_dirty();
                return false;
            }
        }
        true
    }
}

#[derive(Debug, Copy, PartialEq, Clone)]
enum SelectorCursor {
    /// Cursor is at an entry
    Entry(usize),
    /// Cursor is located on the Ok button
    Ok,
    /// Cursor is located on the Cancel button
    Cancel,
}

/// Shows a little window with options for user to select.
///
/// Instantiate with Selector::new(). Set single_only to true if user should only choose one of the
/// options. After passing input events to this component, check Selector::is_done to see if the
/// user has finalised their choices. Collect the choices by consuming the Selector with
/// Selector::collect()
#[derive(Debug, PartialEq, Clone)]
pub struct Selector<T: PartialEq + Debug + Clone + Sync + Send> {
    /// allow only one selection
    single_only: bool,
    entries: Vec<(T, bool)>,
    pub content: CellBuffer,

    cursor: SelectorCursor,

    /// If true, user has finished their selection
    done: bool,
    dirty: bool,
    id: ComponentId,
}

impl<T: PartialEq + Debug + Clone + Sync + Send> fmt::Display for Selector<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("Selector", f)
    }
}

impl<T: PartialEq + Debug + Clone + Sync + Send> Component for Selector<T> {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let (width, height) = self.content.size();
        copy_area_with_break(grid, &self.content, area, ((0, 0), (width, height)));
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        let (width, height) = self.content.size();
        match (event, self.cursor) {
            (UIEvent::Input(Key::Char('\n')), _) if self.single_only => {
                /* User can only select one entry, so Enter key finalises the selection */
                self.done = true;
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Entry(c)) if !self.single_only => {
                /* User can select multiple entries, so Enter key toggles the entry under the
                 * cursor */
                self.entries[c].1 = !self.entries[c].1;
                if self.entries[c].1 {
                    write_string_to_grid(
                        "x",
                        &mut self.content,
                        Color::Default,
                        Color::Default,
                        Attr::Default,
                        ((3, c + 2), (width - 2, c + 2)),
                        None,
                    );
                } else {
                    write_string_to_grid(
                        " ",
                        &mut self.content,
                        Color::Default,
                        Color::Default,
                        Attr::Default,
                        ((3, c + 2), (width - 2, c + 2)),
                        None,
                    );
                }
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Ok) if !self.single_only => {
                self.done = true;
                return true;
            }
            (UIEvent::Input(Key::Esc), _) => {
                for e in self.entries.iter_mut() {
                    e.1 = false;
                }
                self.done = true;
                return true;
            }
            (UIEvent::Input(Key::Char('\n')), SelectorCursor::Cancel) if !self.single_only => {
                for e in self.entries.iter_mut() {
                    e.1 = false;
                }
                self.done = true;
                return true;
            }
            (UIEvent::Input(Key::Up), SelectorCursor::Entry(c)) if c > 0 => {
                if self.single_only {
                    // Redraw selection
                    change_colors(
                        &mut self.content,
                        ((2, c + 2), (width - 2, c + 2)),
                        Color::Default,
                        Color::Default,
                    );
                    change_colors(
                        &mut self.content,
                        ((2, c + 1), (width - 2, c + 1)),
                        Color::Default,
                        Color::Byte(8),
                    );
                    self.entries[c].1 = false;
                    self.entries[c - 1].1 = true;
                } else {
                    // Redraw cursor
                    change_colors(
                        &mut self.content,
                        ((2, c + 2), (4, c + 2)),
                        Color::Default,
                        Color::Default,
                    );
                    change_colors(
                        &mut self.content,
                        ((2, c + 1), (4, c + 1)),
                        Color::Default,
                        Color::Byte(8),
                    );
                }
                self.cursor = SelectorCursor::Entry(c - 1);
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(Key::Up), SelectorCursor::Ok)
            | (UIEvent::Input(Key::Up), SelectorCursor::Cancel) => {
                change_colors(
                    &mut self.content,
                    (
                        ((width - "OK    Cancel".len()) / 2, height - 3),
                        (width - 1, height - 3),
                    ),
                    Color::Default,
                    Color::Default,
                );
                let c = self.entries.len().saturating_sub(1);
                self.cursor = SelectorCursor::Entry(c);
                change_colors(
                    &mut self.content,
                    ((2, c + 2), (4, c + 2)),
                    Color::Default,
                    Color::Byte(8),
                );
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(Key::Down), SelectorCursor::Entry(c))
                if c < self.entries.len().saturating_sub(1) =>
            {
                if self.single_only {
                    // Redraw selection
                    change_colors(
                        &mut self.content,
                        ((2, c + 2), (width - 2, c + 2)),
                        Color::Default,
                        Color::Default,
                    );
                    change_colors(
                        &mut self.content,
                        ((2, c + 3), (width - 2, c + 3)),
                        Color::Default,
                        Color::Byte(8),
                    );
                    self.entries[c].1 = false;
                    self.entries[c + 1].1 = true;
                } else {
                    // Redraw cursor
                    change_colors(
                        &mut self.content,
                        ((2, c + 2), (4, c + 2)),
                        Color::Default,
                        Color::Default,
                    );
                    change_colors(
                        &mut self.content,
                        ((2, c + 3), (4, c + 3)),
                        Color::Default,
                        Color::Byte(8),
                    );
                }
                self.cursor = SelectorCursor::Entry(c + 1);
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(Key::Down), SelectorCursor::Entry(c)) if !self.single_only => {
                self.cursor = SelectorCursor::Ok;
                change_colors(
                    &mut self.content,
                    ((2, c + 2), (4, c + 2)),
                    Color::Default,
                    Color::Default,
                );
                change_colors(
                    &mut self.content,
                    (
                        ((width - "OK    Cancel".len()) / 2, height - 3),
                        ((width - "OK    Cancel".len()) / 2 + 1, height - 3),
                    ),
                    Color::Default,
                    Color::Byte(8),
                );
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(Key::Down), _) | (UIEvent::Input(Key::Up), _) => return true,
            (UIEvent::Input(Key::Right), SelectorCursor::Ok) => {
                self.cursor = SelectorCursor::Cancel;
                change_colors(
                    &mut self.content,
                    (
                        ((width - "OK    Cancel".len()) / 2, height - 3),
                        ((width - "OK    Cancel".len()) / 2 + 1, height - 3),
                    ),
                    Color::Default,
                    Color::Default,
                );
                change_colors(
                    &mut self.content,
                    (
                        ((width - "OK    Cancel".len()) / 2 + 6, height - 3),
                        ((width - "OK    Cancel".len()) / 2 + 11, height - 3),
                    ),
                    Color::Default,
                    Color::Byte(8),
                );
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(Key::Left), SelectorCursor::Cancel) => {
                self.cursor = SelectorCursor::Ok;
                change_colors(
                    &mut self.content,
                    (
                        ((width - "OK    Cancel".len()) / 2, height - 3),
                        ((width - "OK    Cancel".len()) / 2 + 1, height - 3),
                    ),
                    Color::Default,
                    Color::Byte(8),
                );
                change_colors(
                    &mut self.content,
                    (
                        ((width - "OK    Cancel".len()) / 2 + 6, height - 3),
                        ((width - "OK    Cancel".len()) / 2 + 11, height - 3),
                    ),
                    Color::Default,
                    Color::Default,
                );
                self.dirty = true;
                return true;
            }
            (UIEvent::Input(Key::Left), _) | (UIEvent::Input(Key::Right), _) => return true,
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

impl<T: PartialEq + Debug + Clone + Sync + Send> Selector<T> {
    pub fn new(
        title: &str,
        entries: Vec<(T, String)>,
        single_only: bool,
        context: &Context,
    ) -> Selector<T> {
        let width = std::cmp::max(
            "OK    Cancel".len(),
            std::cmp::max(
                entries
                    .iter()
                    .max_by_key(|e| e.1.len())
                    .map(|v| v.1.len())
                    .unwrap_or(0),
                title.len(),
            ),
        ) + 7;
        let height = entries.len()
            + 4
            + if single_only {
                0
            } else {
                /* Extra room for buttons Okay/Cancel */
                3
            };
        let mut content =
            CellBuffer::new_with_context(width, height, Cell::with_char(' '), context);
        let ascii_drawing = context.settings.terminal.ascii_drawing;
        write_string_to_grid(
            if ascii_drawing { "+-" } else { "┏━" },
            &mut content,
            Color::Byte(8),
            Color::Default,
            Attr::Default,
            ((0, 0), (width - 1, 0)),
            None,
        );
        let (x, _) = write_string_to_grid(
            title,
            &mut content,
            Color::Default,
            Color::Default,
            Attr::Default,
            ((2, 0), (width - 1, 0)),
            None,
        );
        for i in 1..(width - title.len() - 1) {
            write_string_to_grid(
                if ascii_drawing { "-" } else { "━" },
                &mut content,
                Color::Byte(8),
                Color::Default,
                Attr::Default,
                ((x + i, 0), (width - 1, 0)),
                None,
            );
        }
        write_string_to_grid(
            if ascii_drawing { "+" } else { "┓" },
            &mut content,
            Color::Byte(8),
            Color::Default,
            Attr::Default,
            ((width - 1, 0), (width - 1, 0)),
            None,
        );
        write_string_to_grid(
            if ascii_drawing { "+" } else { "┗" },
            &mut content,
            Color::Byte(8),
            Color::Default,
            Attr::Default,
            ((0, height - 1), (width - 1, height - 1)),
            None,
        );
        write_string_to_grid(
            &if ascii_drawing {
                "-".repeat(width - 2)
            } else {
                "━".repeat(width - 2)
            },
            &mut content,
            Color::Byte(8),
            Color::Default,
            Attr::Default,
            ((1, height - 1), (width - 2, height - 1)),
            None,
        );
        write_string_to_grid(
            if ascii_drawing { "+" } else { "┛" },
            &mut content,
            Color::Byte(8),
            Color::Default,
            Attr::Default,
            ((width - 1, height - 1), (width - 1, height - 1)),
            None,
        );
        for i in 1..height - 1 {
            write_string_to_grid(
                if ascii_drawing { "|" } else { "┃" },
                &mut content,
                Color::Byte(8),
                Color::Default,
                Attr::Default,
                ((0, i), (width - 1, i)),
                None,
            );
            write_string_to_grid(
                if ascii_drawing { "|" } else { "┃" },
                &mut content,
                Color::Byte(8),
                Color::Default,
                Attr::Default,
                ((width - 1, i), (width - 1, i)),
                None,
            );
        }
        if single_only {
            for (i, e) in entries.iter().enumerate() {
                write_string_to_grid(
                    &e.1,
                    &mut content,
                    Color::Default,
                    if i == 0 {
                        Color::Byte(8)
                    } else {
                        Color::Default
                    },
                    Attr::Default,
                    ((2, i + 2), (width - 1, i + 2)),
                    None,
                );
            }
        } else {
            for (i, e) in entries.iter().enumerate() {
                write_string_to_grid(
                    &format!("[ ] {}", e.1),
                    &mut content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((2, i + 2), (width - 1, i + 2)),
                    None,
                );
                if i == 0 {
                    content[(2, i + 2)].set_bg(Color::Byte(8));
                    content[(3, i + 2)].set_bg(Color::Byte(8));
                    content[(4, i + 2)].set_bg(Color::Byte(8));
                }
            }
            write_string_to_grid(
                "OK    Cancel",
                &mut content,
                Color::Default,
                Color::Default,
                Attr::Bold,
                (
                    ((width - "OK    Cancel".len()) / 2, height - 3),
                    (width - 1, height - 3),
                ),
                None,
            );
        }
        let mut identifiers: Vec<(T, bool)> =
            entries.into_iter().map(|(id, _)| (id, false)).collect();
        if single_only {
            /* set default option */
            identifiers[0].1 = true;
        }

        Selector {
            single_only,
            entries: identifiers,
            content,
            cursor: SelectorCursor::Entry(0),
            done: false,
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }

    pub fn is_done(&self) -> bool {
        self.done
    }

    pub fn collect(self) -> Vec<T> {
        self.entries
            .into_iter()
            .filter(|v| v.1)
            .map(|(id, _)| id)
            .collect()
    }
}

#[derive(Debug)]
pub struct RawBuffer {
    pub buf: CellBuffer,
    cursor: (usize, usize),
    dirty: bool,
}

impl fmt::Display for RawBuffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt("Raw buffer", f)
    }
}

impl Component for RawBuffer {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if self.dirty {
            clear_area(grid, area);
            let (width, height) = self.buf.size();
            let (cols, rows) = (width!(area), height!(area));
            self.cursor = (
                std::cmp::min(width.saturating_sub(cols), self.cursor.0),
                std::cmp::min(height.saturating_sub(rows), self.cursor.1),
            );
            clear_area(grid, area);
            copy_area(
                grid,
                &self.buf,
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
            context.dirty_areas.push_back(area);
            self.dirty = false;
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        match *event {
            UIEvent::Input(Key::Left) => {
                self.cursor.0 = self.cursor.0.saturating_sub(1);
                self.dirty = true;
                true
            }
            UIEvent::Input(Key::Right) => {
                self.cursor.0 = self.cursor.0 + 1;
                self.dirty = true;
                true
            }
            UIEvent::Input(Key::Up) => {
                self.cursor.1 = self.cursor.1.saturating_sub(1);
                self.dirty = true;
                true
            }
            UIEvent::Input(Key::Down) => {
                self.cursor.1 = self.cursor.1 + 1;
                self.dirty = true;
                true
            }
            _ => false,
        }
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self) {
        self.dirty = true;
    }

    fn id(&self) -> ComponentId {
        ComponentId::nil()
    }
}

impl RawBuffer {
    pub fn new(buf: CellBuffer) -> Self {
        RawBuffer {
            buf,
            dirty: true,
            cursor: (0, 0),
        }
    }
}
