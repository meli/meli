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

use super::*;
use std::borrow::Cow;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Cursor {
    Header,
    List(usize),
}

#[derive(Debug)]
enum State {
    Offline {
        ui_messages: Vec<Cow<'static, str>>,
    },
    Loading {
        ui_messages: Vec<Cow<'static, str>>,
    },
    Complete {
        messages: Vec<Cow<'static, str>>,
        rows: RowsState<(ThreadHash, EnvelopeHash)>,
    },
}

#[derive(Debug)]
pub struct SearchAll {
    account_hash: AccountHash,
    account_pos: usize,
    state: State,

    focus: Focus,
    view: Box<MailView>,
    color_cache: ColorCache,

    cursor: Cursor,
    query: String,
    data_columns: DataColumns<4>,

    sort: (SortField, SortOrder),
    subsort: (SortField, SortOrder),

    dirty: bool,
    initialized: bool,
    id: ComponentId,
}

impl fmt::Display for SearchAll {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "search `{:?}`", &self.query)
    }
}

impl SearchAll {
    pub fn with_account(
        account_hash: AccountHash,
        account_pos: usize,
        query: String,
        context: &Context,
    ) -> Self {
        let mut ret = SearchAll {
            account_hash,
            account_pos,
            state: State::Loading {
                ui_messages: vec![],
            },
            cursor: Cursor::List(0),
            sort: (Default::default(), Default::default()),
            subsort: (SortField::Date, SortOrder::Desc),
            query,
            data_columns: DataColumns::default(),
            color_cache: ColorCache::new(context, IndexStyle::Plain),
            focus: Focus::None,
            view: Box::new(MailView::default()),
            dirty: true,
            initialized: false,
            id: ComponentId::new_v4(),
        };
        ret
    }
}

impl Component for SearchAll {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let upper_left = set_y(upper_left, get_y(upper_left) + 1);

        if height!(area) < 4 {
            return;
        }

        let width = width!(area);

        if !self.initialized {}
        //let header_height = self.form.len();
        let header_height = 1;
        let theme_default = crate::conf::value(context, "theme_default");

        let header_area = (
            upper_left,
            (get_x(bottom_right), get_y(upper_left) + header_height),
        );

        let body_area = (
            (
                get_x(upper_left!(header_area)),
                get_y(bottom_right!(header_area)) + 1,
            ),
            bottom_right,
        );

        clear_area(grid, area, theme_default);

        /* Regardless of view mode, do the following */
        //self.form.draw(grid, header_area, context);
        match self.cursor {
            Cursor::Header => {}
            Cursor::List(_) => {}
        }

        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, mut event: &mut UIEvent, context: &mut Context) -> bool {
        if let State::Offline {
            ref mut ui_messages,
        } = self.state
        {
            match event {
                UIEvent::AccountStatusChange(account_hash, msg)
                    if *account_hash == self.account_hash =>
                {
                    if let Some(msg) = msg.clone() {
                        ui_messages.push(msg);
                    }
                    self.dirty = true;
                    return false;
                }
                _ => {}
            }
        }

        let shortcuts = self.get_shortcuts(context);
        if self.cursor == Cursor::Header
        //&& self.form.process_event(event, context)
        {
            if let UIEvent::InsertInput(_) = event {
                //self.has_changes = true;
            }
            return true;
        }

        match *event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.color_cache = ColorCache::new(context, IndexStyle::Plain);
                self.set_dirty(true);
            }
            UIEvent::Resize => {
                self.set_dirty(true);
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
        //self.form.set_dirty(value);
    }

    fn kill(&mut self, uuid: Uuid, context: &mut Context) {
        if self.id != uuid {
            return;
        }

        context.replies.push_back(UIEvent::Action(Tab(Kill(uuid))));
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = Default::default();

        map.insert(
            Shortcuts::GENERAL,
            account_settings!(context[self.account_hash].shortcuts.general).key_values(),
        );

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        true
    }
}
