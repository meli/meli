/*
 * meli
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

/*! Components visual and logical separations of application interfaces.
 *
 * They can draw on the terminal and receive events, but also do other stuff as well. (For example, see the `notifications` module.)
 * See the `Component` Trait for more details.
 */

use super::*;
use crate::melib::text_processing::{TextProcessing, Truncate};
use crate::terminal::boundaries::*;

pub mod mail;
pub use crate::mail::*;

pub mod notifications;

pub mod utilities;
pub use self::utilities::*;

pub mod contacts;
pub use crate::contacts::*;

use std::fmt;
use std::fmt::{Debug, Display};

use fnv::FnvHashMap;
use uuid::Uuid;

use super::{Key, StatusEvent, UIEvent};

type ComponentId = Uuid;

pub type ShortcutMap = FnvHashMap<&'static str, Key>;
pub type ShortcutMaps = FnvHashMap<&'static str, ShortcutMap>;

/// Types implementing this Trait can draw on the terminal and receive events.
/// If a type wants to skip drawing if it has not changed anything, it can hold some flag in its
/// fields (eg self.dirty = false) and act upon that in their `draw` implementation.
pub trait Component: Display + Debug + Send {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context);
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool;
    fn is_dirty(&self) -> bool;
    fn is_visible(&self) -> bool {
        true
    }
    fn can_quit_cleanly(&mut self, _context: &Context) -> bool {
        true
    }
    fn set_dirty(&mut self, value: bool);
    fn kill(&mut self, _id: ComponentId, _context: &mut Context) {}
    fn set_id(&mut self, _id: ComponentId) {}
    fn id(&self) -> ComponentId;

    fn get_shortcuts(&self, _context: &Context) -> ShortcutMaps {
        Default::default()
    }

    fn get_status(&self, _context: &Context) -> Option<String> {
        None
    }
}
