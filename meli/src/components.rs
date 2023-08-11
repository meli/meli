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

//! Components visual and logical separations of application interfaces.
///
/// They can draw on the terminal and receive events, but also do other stuff
/// as well.
/// For an example, see the [`notifications`] module.
/// See also the [`Component`] trait for more details.
use smallvec::SmallVec;

use super::*;

pub mod notifications;

pub mod mailbox_management;
pub use mailbox_management::*;

pub mod jobs_view;
pub use jobs_view::*;

#[cfg(feature = "svgscreenshot")]
pub mod svg;

use std::{
    fmt,
    fmt::{Debug, Display},
};

use indexmap::IndexMap;
use uuid::Uuid;

#[derive(Clone, Copy, Eq, Deserialize, Hash, Ord, PartialOrd, PartialEq, Serialize)]
#[repr(transparent)]
pub struct ComponentId(Uuid);

impl AsRef<Uuid> for ComponentId {
    fn as_ref(&self) -> &Uuid {
        &self.0
    }
}

impl std::fmt::Display for ComponentId {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.as_simple(), fmt)
    }
}

impl std::fmt::Debug for ComponentId {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.0.as_simple(), fmt)
    }
}

impl Default for ComponentId {
    fn default() -> Self {
        Self(Uuid::new_v4())
    }
}

impl std::fmt::LowerHex for ComponentId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::LowerHex::fmt(self.0.as_hyphenated(), f)
    }
}

impl std::fmt::UpperHex for ComponentId {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::UpperHex::fmt(self.0.as_hyphenated(), f)
    }
}

pub type ShortcutMap = IndexMap<&'static str, Key>;
pub type ShortcutMaps = IndexMap<&'static str, ShortcutMap>;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScrollContext {
    pub shown_lines: usize,
    pub total_lines: usize,
    pub has_more_lines: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum ScrollUpdate {
    End(ComponentId),
    Update {
        id: ComponentId,
        context: ScrollContext,
    },
}

/// Types implementing this Trait can draw on the terminal and receive events.
/// If a type wants to skip drawing if it has not changed anything, it can hold
/// some flag in its fields (eg `self.dirty = false`) and act upon that in their
/// [`draw`](Component::draw) implementation.
pub trait Component: Display + Debug + Send + Sync {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context);
    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool;
    fn is_dirty(&self) -> bool;
    /// If the component is meant to be currently visible to the user.
    fn is_visible(&self) -> bool {
        true
    }

    /// If the component can quit right away without any unsaved or ongoing
    /// operations.
    fn can_quit_cleanly(&mut self, _context: &Context) -> bool {
        true
    }

    fn set_dirty(&mut self, value: bool);

    fn kill(&mut self, _id: ComponentId, _context: &mut Context) {}

    fn id(&self) -> ComponentId;

    fn shortcuts(&self, _context: &Context) -> ShortcutMaps {
        Default::default()
    }

    /// Get status message for the status line.
    fn status(&self, _context: &Context) -> String {
        String::new()
    }

    fn attributes(&self) -> &'static ComponentAttr {
        &ComponentAttr::DEFAULT
    }

    fn children(&self) -> IndexMap<ComponentId, &dyn Component> {
        IndexMap::default()
    }

    fn children_mut(&mut self) -> IndexMap<ComponentId, &mut dyn Component> {
        IndexMap::default()
    }

    fn realize(&self, parent: Option<ComponentId>, context: &mut Context) {
        // log::trace!("Realizing id {} w/ parent {:?}", self.id(), &parent);
        context.realized.insert(self.id(), parent);
    }

    fn unrealize(&self, context: &mut Context) {
        // log::trace!("Unrealizing id {}", self.id());
        context.unrealized.insert(self.id());
        context
            .replies
            .push_back(UIEvent::ComponentUnrealize(self.id()));
    }
}

impl Component for Box<dyn Component> {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        (**self).draw(grid, area, context)
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        (**self).process_event(event, context)
    }

    fn is_dirty(&self) -> bool {
        (**self).is_dirty()
    }

    fn is_visible(&self) -> bool {
        (**self).is_visible()
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        (**self).can_quit_cleanly(context)
    }

    fn set_dirty(&mut self, value: bool) {
        (**self).set_dirty(value)
    }

    fn kill(&mut self, id: ComponentId, context: &mut Context) {
        (**self).kill(id, context)
    }

    fn id(&self) -> ComponentId {
        (**self).id()
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        (**self).shortcuts(context)
    }

    fn status(&self, context: &Context) -> String {
        (**self).status(context)
    }

    fn attributes(&self) -> &'static ComponentAttr {
        (**self).attributes()
    }

    fn children(&self) -> IndexMap<ComponentId, &dyn Component> {
        (**self).children()
    }

    fn children_mut(&mut self) -> IndexMap<ComponentId, &mut dyn Component> {
        (**self).children_mut()
    }

    fn realize(&self, parent: Option<ComponentId>, context: &mut Context) {
        (**self).realize(parent, context)
    }

    fn unrealize(&self, context: &mut Context) {
        (**self).unrealize(context)
    }
}

bitflags::bitflags! {
    /// Attributes of a [`Component`] widget.
    ///
    /// `ComponentAttr::DEFAULT` represents no attribute.
    pub struct ComponentAttr: u8 {
        /// Nothing special going on.
        const DEFAULT        = 0;
        const HAS_ANIMATIONS = 1;
        const CONTAINER      = 1 << 1;
    }
}

impl Default for ComponentAttr {
    fn default() -> Self {
        Self::DEFAULT
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct ComponentPath {
    pub id: ComponentId,
    pub tail: SmallVec<[ComponentId; 8]>,
}

impl ComponentPath {
    pub fn new(id: ComponentId) -> Self {
        Self {
            id,
            tail: SmallVec::default(),
        }
    }

    pub fn push_front(&mut self, id: ComponentId) {
        self.tail.insert(0, self.id);
        self.id = id;
    }

    pub fn push_back(&mut self, id: ComponentId) {
        self.tail.push(id);
    }

    pub fn resolve<'c>(&self, root: &'c dyn Component) -> Option<&'c dyn Component> {
        let mut cursor = root;
        for id in self.tail.iter().rev().chain(std::iter::once(&self.id)) {
            // log::trace!("resolve cursor = {} next id is {}", cursor.id(), &id);
            if *id == cursor.id() {
                // log::trace!("continue;");
                continue;
            }
            cursor = cursor.children().remove(id)?;
        }
        Some(cursor)
    }

    #[inline]
    pub fn parent(&self) -> Option<&ComponentId> {
        self.tail.first()
    }

    #[inline]
    pub fn root(&self) -> Option<&ComponentId> {
        self.tail.last()
    }
}
