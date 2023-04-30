/*
 * meli - melib
 *
 * Copyright  Manos Pitsidianakis
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

use std::collections::HashMap;

use smallvec::SmallVec;

use super::{ThreadNode, ThreadNodeHash};

/* `ThreadsIterator` returns messages according to the sorted order. For
 * example, for the following threads:
 *
 *  ```
 *  A_
 *   |_ B
 *   |_C
 *  D
 *  E_
 *   |_F
 *   ```
 *
 *   the iterator returns them as `A, B, C, D, E, F`
 */

pub struct ThreadsGroupIterator<'a> {
    pub(super) root_tree: SmallVec<[ThreadNodeHash; 1024]>,
    pub(super) pos: usize,
    pub(super) stack: SmallVec<[usize; 16]>,
    pub(super) thread_nodes: &'a HashMap<ThreadNodeHash, ThreadNode>,
}
impl<'a> Iterator for ThreadsGroupIterator<'a> {
    type Item = (usize, ThreadNodeHash, bool);
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut tree = &(*self.root_tree);
            for i in self.stack.iter() {
                tree = &self.thread_nodes[&tree[*i]].children;
            }
            if self.pos == tree.len() {
                if let Some(p) = self.stack.pop() {
                    self.pos = p + 1;
                } else {
                    return None;
                }
            } else {
                debug_assert!(self.pos < tree.len());
                let ret = (
                    self.stack.len(),
                    tree[self.pos],
                    !self.stack.is_empty() && (self.pos < (tree.len() - 1)),
                );
                if !self.thread_nodes[&tree[self.pos]].children.is_empty() {
                    self.stack.push(self.pos);
                    self.pos = 0;
                    if self.thread_nodes[&ret.1].message.is_some() {
                        return Some(ret);
                    } else {
                        continue;
                    }
                }
                self.pos += 1;
                if self.thread_nodes[&ret.1].message.is_some() {
                    return Some(ret);
                }
            }
        }
    }
}
/* `ThreadIterator` returns messages of a specific thread according to the
 * sorted order. For example, for the following thread:
 *
 *  ```
 *  A_
 *   |_ B
 *   |_C
 *   |_D
 *   ```
 *
 *   the iterator returns them as `A, B, C, D`
 */

pub struct ThreadGroupIterator<'a> {
    pub(super) group: ThreadNodeHash,
    pub(super) pos: usize,
    pub(super) stack: SmallVec<[usize; 16]>,
    pub(super) thread_nodes: &'a HashMap<ThreadNodeHash, ThreadNode>,
}

impl<'a> Iterator for ThreadGroupIterator<'a> {
    type Item = (usize, ThreadNodeHash);
    fn next(&mut self) -> Option<(usize, ThreadNodeHash)> {
        loop {
            let mut tree = &[self.group][..];
            for i in self.stack.iter() {
                tree = self.thread_nodes[&tree[*i]].children.as_slice();
            }
            if self.pos == tree.len() {
                if self.stack.is_empty() {
                    return None;
                }
                self.pos = self.stack.pop().unwrap() + 1;
            } else {
                debug_assert!(self.pos < tree.len());
                let ret = (self.stack.len(), tree[self.pos]);
                if !self.thread_nodes[&tree[self.pos]].children.is_empty() {
                    self.stack.push(self.pos);
                    self.pos = 0;
                    if self.thread_nodes[&ret.1].message.is_some() {
                        return Some(ret);
                    } else {
                        continue;
                    }
                }
                self.pos += 1;
                if self.thread_nodes[&ret.1].message.is_some() {
                    return Some(ret);
                }
            }
        }
    }
}
