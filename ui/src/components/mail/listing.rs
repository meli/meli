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

use super::*;

mod compact;
pub use self::compact::*;

mod thread;
pub use self::thread::*;

mod plain;
pub use self::plain::*;

trait ListingTrait {
    fn coordinates(&self) -> (usize, usize, Option<EnvelopeHash>);
    fn set_coordinates(&mut self, (usize, usize, Option<EnvelopeHash>));
}

#[derive(Debug)]
pub enum Listing {
    Plain(PlainListing),
    Threaded(ThreadListing),
    Compact(CompactListing),
}

impl fmt::Display for Listing {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Listing::Compact(l) => write!(f, "{}", l),
            Listing::Plain(l) => write!(f, "{}", l),
            Listing::Threaded(l) => write!(f, "{}", l),
        }
    }
}

impl Default for Listing {
    fn default() -> Self {
        Listing::Threaded(Default::default())
    }
}

impl Component for Listing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        match self {
            Listing::Compact(l) => l.draw(grid, area, context),
            Listing::Plain(l) => l.draw(grid, area, context),
            Listing::Threaded(l) => l.draw(grid, area, context),
        }
    }
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if match self {
            Listing::Plain(l) => l.process_event(event, context),
            Listing::Compact(l) => l.process_event(event, context),
            Listing::Threaded(l) => l.process_event(event, context),
        } {
            return true;
        }

        match event.event_type {
            UIEventType::Resize => self.set_dirty(),
            UIEventType::Action(ref action) => match action {
                Action::Listing(ListingAction::SetPlain) => {
                    let new_l = match self {
                        Listing::Plain(_) => {
                            return true;
                        }
                        Listing::Threaded(l) => {
                            let mut new_l = PlainListing::default();
                            new_l.set_coordinates(l.coordinates());
                            new_l
                        }
                        Listing::Compact(l) => {
                            let mut new_l = PlainListing::default();
                            new_l.set_coordinates(l.coordinates());
                            new_l
                        }
                    };
                    *self = Listing::Plain(new_l);
                    return true;
                }
                Action::Listing(ListingAction::SetThreaded) => {
                    let new_l = match self {
                        Listing::Threaded(_) => {
                            return true;
                        }
                        Listing::Plain(l) => {
                            let mut new_l = ThreadListing::default();
                            new_l.set_coordinates(l.coordinates());
                            new_l
                        }
                        Listing::Compact(l) => {
                            let mut new_l = ThreadListing::default();
                            new_l.set_coordinates(l.coordinates());
                            new_l
                        }
                    };
                    *self = Listing::Threaded(new_l);
                    return true;
                }
                Action::Listing(ListingAction::SetCompact) => {
                    let new_l = match self {
                        Listing::Compact(_) => {
                            return true;
                        }
                        Listing::Threaded(l) => {
                            let mut new_l = CompactListing::default();
                            new_l.set_coordinates(l.coordinates());
                            new_l
                        }
                        Listing::Plain(l) => {
                            let mut new_l = CompactListing::default();
                            new_l.set_coordinates(l.coordinates());
                            new_l
                        }
                    };
                    *self = Listing::Compact(new_l);
                    return true;
                }
                _ => {}
            },
            _ => {}
        }
        false
    }
    fn is_dirty(&self) -> bool {
        match self {
            Listing::Compact(l) => l.is_dirty(),
            Listing::Plain(l) => l.is_dirty(),
            Listing::Threaded(l) => l.is_dirty(),
        }
    }
    fn set_dirty(&mut self) {
        match self {
            Listing::Compact(l) => l.set_dirty(),
            Listing::Plain(l) => l.set_dirty(),
            Listing::Threaded(l) => l.set_dirty(),
        }
    }

    fn get_shortcuts(&self, context: &Context) -> ShortcutMap {
        match self {
            Listing::Compact(l) => l.get_shortcuts(context),
            Listing::Plain(l) => l.get_shortcuts(context),
            Listing::Threaded(l) => l.get_shortcuts(context),
        }
    }
}

impl From<IndexStyle> for Listing {
    fn from(index_style: IndexStyle) -> Self {
        match index_style {
            IndexStyle::Plain => Listing::Plain(Default::default()),
            IndexStyle::Threaded => Listing::Threaded(Default::default()),
            IndexStyle::Compact => Listing::Compact(Default::default()),
        }
    }
}
