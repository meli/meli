/*
 * meli - status tab module.
 *
 * Copyright 2019 Manos Pitsidianakis
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
use std::fmt;

#[derive(Debug)]
pub struct StatusPanel {
    cursor: (usize, usize),
    content: CellBuffer,
    dirty: bool,
    id: ComponentId,
}

impl fmt::Display for StatusPanel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "status")
    }
}

impl Component for StatusPanel {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.dirty {
            return;
        }
        let (width, height) = self.content.size();
        {
            let (_, y) = write_string_to_grid(
                "Worker threads",
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Bold,
                ((1, 1), (width - 1, height - 1)),
                true,
            );
            let mut y = y + 1;
            let work_controller = context.work_controller().threads.lock().unwrap();
            let mut workers: Vec<&Worker> = work_controller.values().collect::<Vec<&Worker>>();
            let mut max_name = 0;
            workers.sort_by_key(|w| {
                max_name = std::cmp::max(max_name, w.name.len());
                w.name.as_str()
            });
            for worker in workers {
                let (x, y_off) = write_string_to_grid(
                    &format!(
                        "- {:<max_name$} {}",
                        worker.name.as_str(),
                        worker.status.as_str(),
                        max_name = max_name
                    ),
                    &mut self.content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((1, y), (width - 1, height - 1)),
                    true,
                );
                for x in x..(width - 1) {
                    self.content[(x, y)].set_ch(' ');
                }

                y = y_off + 1;
            }
            write_string_to_grid(
                "Static threads",
                &mut self.content,
                Color::Default,
                Color::Default,
                Attr::Bold,
                ((1, y + 1), (width - 1, height - 1)),
                true,
            );
            y += 2;

            let work_controller = context.work_controller().static_threads.lock().unwrap();
            let mut workers: Vec<&Worker> = work_controller.values().collect::<Vec<&Worker>>();
            max_name = 0;
            workers.retain(|w| w.name != "WorkController-thread");
            workers.sort_by_key(|w| {
                max_name = std::cmp::max(max_name, w.name.len());
                w.name.as_str()
            });
            for worker in workers {
                let (x, y_off) = write_string_to_grid(
                    &format!(
                        "- {:<max_name$} {}",
                        worker.name.as_str(),
                        worker.status.as_str(),
                        max_name = max_name
                    ),
                    &mut self.content,
                    Color::Default,
                    Color::Default,
                    Attr::Default,
                    ((1, y), (width - 1, height - 1)),
                    true,
                );
                for x in x..(width - 1) {
                    self.content[(x, y)].set_ch(' ');
                }

                y = y_off + 1;
            }
        }
        let (cols, rows) = (width!(area), height!(area));
        self.cursor = (
            std::cmp::min(width.saturating_sub(cols), self.cursor.0),
            std::cmp::min(height.saturating_sub(rows), self.cursor.1),
        );
        clear_area(grid, area);
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
        self.dirty = false;
        context.dirty_areas.push_back(area);
    }
    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        match *event {
            UIEvent::Input(Key::Left) => {
                self.cursor.0 = self.cursor.0.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Right) => {
                self.cursor.0 = self.cursor.0 + 1;
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Up) => {
                self.cursor.1 = self.cursor.1.saturating_sub(1);
                self.dirty = true;
                return true;
            }
            UIEvent::Input(Key::Down) => {
                self.cursor.1 = self.cursor.1 + 1;
                self.dirty = true;
                return true;
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

impl StatusPanel {
    pub fn new() -> StatusPanel {
        let content = CellBuffer::new(120, 40, Cell::default());

        StatusPanel {
            cursor: (0, 0),
            content,
            dirty: true,
            id: ComponentId::new_v4(),
        }
    }
}
