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

use std::borrow::Cow;

use super::*;
use crate::components::PageMovement;

#[derive(Debug)]
pub struct OfflineListing {
    cursor_pos: (AccountHash, MailboxHash),
    _row_updates: SmallVec<[EnvelopeHash; 8]>,
    _selection: HashMap<EnvelopeHash, bool>,
    messages: Vec<Cow<'static, str>>,
    dirty: bool,
    id: ComponentId,
}

impl MailListingTrait for OfflineListing {
    fn row_updates(&mut self) -> &mut SmallVec<[EnvelopeHash; 8]> {
        &mut self._row_updates
    }

    fn selection(&mut self) -> &mut HashMap<EnvelopeHash, bool> {
        &mut self._selection
    }

    fn get_focused_items(&self, _context: &Context) -> SmallVec<[EnvelopeHash; 8]> {
        SmallVec::new()
    }

    fn refresh_mailbox(&mut self, _context: &mut Context, _force: bool) {}
    fn redraw_threads_list(
        &mut self,
        _context: &Context,
        _items: Box<dyn Iterator<Item = ThreadHash>>,
    ) {
    }
    fn redraw_envelope_list(
        &mut self,
        _context: &Context,
        _items: Box<dyn Iterator<Item = EnvelopeHash>>,
    ) {
    }
}

impl ListingTrait for OfflineListing {
    fn coordinates(&self) -> (AccountHash, MailboxHash) {
        self.cursor_pos
    }

    fn set_coordinates(&mut self, coordinates: (AccountHash, MailboxHash)) {
        self.cursor_pos = coordinates;
    }

    fn next_entry(&mut self, _: &mut Context) {}

    fn prev_entry(&mut self, _: &mut Context) {}

    fn highlight_line(
        &mut self,
        _grid: &mut CellBuffer,
        _area: Area,
        _idx: usize,
        _context: &Context,
    ) {
    }

    fn draw_list(&mut self, _: &mut CellBuffer, _: Area, _: &mut Context) {}

    fn view_area(&self) -> Option<Area> {
        None
    }

    fn unfocused(&self) -> bool {
        false
    }

    fn set_modifier_active(&mut self, _: bool) {}

    fn set_modifier_command(&mut self, _: Option<Modifier>) {}

    fn modifier_command(&self) -> Option<Modifier> {
        None
    }

    fn set_movement(&mut self, _: PageMovement) {}

    fn focus(&self) -> Focus {
        Focus::None
    }

    fn set_focus(&mut self, _new_value: Focus, _context: &mut Context) {}
}

impl std::fmt::Display for OfflineListing {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "mail")
    }
}

impl OfflineListing {
    pub fn new(cursor_pos: (AccountHash, MailboxHash)) -> Box<Self> {
        Box::new(Self {
            cursor_pos,
            _row_updates: SmallVec::new(),
            _selection: HashMap::default(),
            messages: vec![],
            dirty: true,
            id: ComponentId::default(),
        })
    }
}

impl Component for OfflineListing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.dirty {
            return;
        }
        self.dirty = false;
        let theme_default = conf::value(context, "theme_default");
        let text_unfocused = conf::value(context, "text.unfocused");
        let error_message = conf::value(context, "error_message");
        grid.clear_area(area, theme_default);
        if let Err(err) = context.is_online(self.cursor_pos.0) {
            let (x, _) = grid.write_string(
                "offline: ",
                error_message.fg,
                error_message.bg,
                error_message.attrs,
                area,
                None,
                None,
            );

            let (_, mut y_offset) = grid.write_string(
                &err.to_string(),
                error_message.fg,
                error_message.bg,
                error_message.attrs,
                area,
                Some(x + 1),
                Some(0),
            );
            y_offset += 1;
            if let Some(msg) = self.messages.last() {
                grid.write_string(
                    msg,
                    text_unfocused.fg,
                    text_unfocused.bg,
                    Attr::BOLD,
                    area.skip_rows(y_offset),
                    None,
                    None,
                );
            }
            y_offset += 1;
            for (i, msg) in self.messages.iter().rev().skip(1).enumerate() {
                grid.write_string(
                    msg,
                    text_unfocused.fg,
                    text_unfocused.bg,
                    text_unfocused.attrs,
                    area.skip_rows(y_offset + i),
                    None,
                    None,
                );
            }
        } else {
            grid.write_string(
                "loading...",
                conf::value(context, "highlight").fg,
                conf::value(context, "highlight").bg,
                conf::value(context, "highlight").attrs,
                area,
                None,
                None,
            );
            let mut jobs: SmallVec<[_; 64]> = context.accounts[&self.cursor_pos.0]
                .active_jobs
                .iter()
                .collect();
            jobs.sort_by_key(|(j, _)| *j);
            for (i, (job_id, j)) in jobs.into_iter().enumerate() {
                grid.write_string(
                    &format!("{}: {:?}", job_id, j),
                    text_unfocused.fg,
                    text_unfocused.bg,
                    text_unfocused.attrs,
                    area.skip_rows(i + 1),
                    None,
                    None,
                );
            }

            context
                .replies
                .push_back(UIEvent::AccountStatusChange(self.cursor_pos.0, None));
        }
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, _context: &mut Context) -> bool {
        match event {
            UIEvent::AccountStatusChange(account_hash, msg)
                if *account_hash == self.cursor_pos.0 =>
            {
                if let Some(msg) = msg.clone() {
                    self.messages.push(msg);
                }
                self.set_dirty(true);
            }
            UIEvent::ChangeMode(UIMode::Normal)
            | UIEvent::Resize
            | UIEvent::ConfigReload { old_settings: _ }
            | UIEvent::VisibilityChange(_) => {
                self.set_dirty(true);
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
    }

    fn set_dirty(&mut self, _value: bool) {
        self.dirty = true;
    }

    fn id(&self) -> ComponentId {
        self.id
    }
}
