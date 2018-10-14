use super::*;
use components::utilities::PageMovement;

pub trait IndexContent: Component {
    /* Handles the drawing of one entry */
    fn make_entry(&mut self, idx: usize) -> ();

    /* Handles what happens when the user selects an entry in the index listing */
    fn enter_entry(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) -> ();

    /* Refreshes content */
    fn refresh(&mut self, context: &mut Context) -> ();

    fn search(&self, term: &str) -> Option<usize>;
}

#[derive(Debug, PartialEq)]
enum IndexState {
    Uninitialized,
    Listing,
    Unfocused,
    Search,
}

#[derive(Debug)]
pub struct Index {
    cursor_pos: usize,
    new_cursor_pos: usize,
    length: usize,

    /// Cache current view.
    canvas: CellBuffer,
    /// If we must redraw on next redraw event
    dirty: bool,
    state: IndexState,

    content: Box<IndexContent>,
}

impl Index {
    fn highlight_line(&self, grid: &mut CellBuffer, area: Area, idx: usize) {
        let fg_color = Color::Default;
        let bg_color = if self.cursor_pos == idx {
            Color::Byte(246)
        /* } else if idx % 2 == 0 {
                   Color::Byte(236)*/
        } else {
            Color::Default
        };
        change_colors(grid, area, fg_color, bg_color);
    }
}

impl Component for Index {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.dirty {
            return;
        }

        match self.state {
            IndexState::Uninitialized => {
                self.content.refresh(context);

                /* copy area */
                self.state = IndexState::Listing;
                self.draw(grid, area, context);
                return;
            }
            IndexState::Listing => {
                /* rehighlight entries, redraw pages */
                let upper_left = upper_left!(area);
                let bottom_right = bottom_right!(area);
                let rows = get_y(bottom_right) - get_y(upper_left) + 1;
                let prev_page_no = (self.cursor_pos).wrapping_div(rows);
                let page_no = (self.new_cursor_pos).wrapping_div(rows);

                let top_idx = page_no * rows;
                if self.new_cursor_pos >= self.length {
                    self.new_cursor_pos = self.length - 1;
                }
                /* If cursor position has changed, remove the highlight from the previous position and
                 * apply it in the new one. */
                if self.cursor_pos != self.new_cursor_pos && prev_page_no == page_no {
                    let old_cursor_pos = self.cursor_pos;
                    self.cursor_pos = self.new_cursor_pos;
                    for idx in &[old_cursor_pos, self.new_cursor_pos] {
                        if *idx >= self.length {
                            continue; //bounds check
                        }
                        let new_area = (
                            set_y(upper_left, get_y(upper_left) + (*idx % rows)),
                            set_y(bottom_right, get_y(upper_left) + (*idx % rows)),
                        );
                        self.highlight_line(grid, new_area, *idx);
                        context.dirty_areas.push_back(new_area);
                    }
                    return;
                } else if self.cursor_pos != self.new_cursor_pos {
                    self.cursor_pos = self.new_cursor_pos;
                }

                /* Page_no has changed, so draw new page */
                copy_area(
                    grid,
                    &self.canvas,
                    area,
                    ((0, top_idx), (500 - 1, self.length)),
                );
                self.highlight_line(
                    grid,
                    (
                        (
                            get_x(upper_left),
                            get_y(upper_left) + (self.cursor_pos % rows),
                        ),
                        (
                            get_x(bottom_right),
                            get_y(upper_left) + (self.cursor_pos % rows),
                        ),
                    ),
                    self.cursor_pos,
                );
                context.dirty_areas.push_back(area);
            }
            IndexState::Unfocused => {
                self.content.draw(grid, area, context);
            }
            IndexState::Search => unreachable!(),
        }

        self.dirty = false;
        return;
    }

    fn process_event(&mut self, event: &UIEvent, context: &mut Context) -> bool {
        if self.content.process_event(event, context) {
            return true;
        }
        match event.event_type {
            UIEventType::Input(Key::Up) => {
                if self.cursor_pos > 0 {
                    self.new_cursor_pos = self.new_cursor_pos.saturating_sub(1);
                    self.set_dirty();
                }
                return true;
            }
            UIEventType::Input(Key::Down) => {
                if self.length > 0 && self.new_cursor_pos < self.length - 1 {
                    self.new_cursor_pos += 1;
                    self.set_dirty();
                }
                return true;
            }
            UIEventType::Input(Key::Char('\n')) if self.state == IndexState::Listing => {
                self.state = IndexState::Unfocused;
                self.set_dirty();
                return true;
            }
            UIEventType::Input(Key::Char('i')) if self.state == IndexState::Unfocused => {
                self.state = IndexState::Listing;
                self.set_dirty();
                return true;
            }
            UIEventType::ChangeMode(UIMode::Normal) => {
                self.set_dirty();
            }
            UIEventType::Resize => {
                self.set_dirty();
            }
            _ => {}
        }

        false
    }
    fn is_dirty(&self) -> bool {
        self.dirty || self.content.is_dirty()
    }
    fn set_dirty(&mut self) {
        self.dirty = true;
    }
}

impl fmt::Display for Index {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.content, f)
    }
}
