/*
 * meli - mailbox threading module.
 *
 * Copyright 2017 Manos Pitsidianakis
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

/*!
 * This module implements Jamie Zawinski's (threading algorithm)
 * [https://www.jwz.org/doc/threading.html]. It is a bit of a beast, so prepare for a lot of
 * bloated code that's necessary for the crap modern e-mail is. Quoted comments (/* " .. " */) are
 * taken almost verbatim from the algorithm.
 *
 * The entry point of this module is the `Threads` struct and its `new` method. It contains
 * `ThreadNodes` which are the nodes in the thread trees that might have messages associated with
 * them. The root nodes (first messages in each thread) are stored in `root_set` and `tree`
 * vectors. `Threads` has inner mutability since we need to sort without the user having mutable
 * ownership.
 */

use crate::email::parser::BytesExt;
use crate::email::*;
use crate::structs::StackVec;

#[cfg(feature = "unicode_algorithms")]
use text_processing::grapheme_clusters::*;
use uuid::Uuid;

use fnv::{FnvHashMap, FnvHashSet};
use std::cell::{Ref, RefCell};
use std::cmp::Ordering;
use std::fmt;
use std::iter::FromIterator;
use std::mem;
use std::ops::Index;
use std::result::Result as StdResult;
use std::str::FromStr;
use std::string::ToString;
use std::sync::{Arc, RwLock};

type Envelopes = Arc<RwLock<FnvHashMap<EnvelopeHash, Envelope>>>;

#[derive(PartialEq, Hash, Eq, Copy, Clone, Serialize, Deserialize, Default)]
pub struct ThreadHash(Uuid);

impl fmt::Debug for ThreadHash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0.to_string())
    }
}

impl fmt::Display for ThreadHash {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0.to_string())
    }
}

impl ThreadHash {
    fn new() -> Self {
        ThreadHash(Uuid::new_v4())
    }
    pub fn null() -> Self {
        ThreadHash(Uuid::nil())
    }
}

/* Helper macros to avoid repeating ourselves */

fn rec_change_root_parent(
    b: &mut FnvHashMap<ThreadHash, ThreadNode>,
    idx: ThreadHash,
    new_root: ThreadHash,
) {
    let parent = {
        let entry = b.entry(idx).or_default();
        entry.thread_group = new_root;
        entry.parent
    };
    if let Some(p) = parent {
        rec_change_children(b, p, new_root);
        rec_change_root_parent(b, p, new_root);
    }
}
fn rec_change_children(
    b: &mut FnvHashMap<ThreadHash, ThreadNode>,
    idx: ThreadHash,
    new_root: ThreadHash,
) {
    b.entry(idx).and_modify(|e| {
        e.thread_group = new_root;
    });

    let mut ctr = 0;
    while ctr < b[&idx].children.len() {
        let c = b[&idx].children[ctr];
        rec_change_children(b, c, new_root);
        ctr += 1;
    }
}

macro_rules! remove_from_parent {
    ($buf:expr, $idx:expr) => {{
        let mut parent: Option<ThreadHash> = None;
        let entry_parent = $buf.entry($idx).or_default().parent;
        if let Some(p) = entry_parent {
            parent = Some(p);
            if let Some(pos) = $buf[&p].children.iter().position(|c| *c == $idx) {
                $buf.entry(p).and_modify(|e| {
                    e.children.remove(pos);
                });
            }
            rec_change_root_parent($buf, p, p);
        }
        $buf.entry($idx).and_modify(|e| e.parent = None);
        rec_change_children($buf, $idx, $idx);
        $buf.entry($idx).and_modify(|e| e.thread_group = $idx);
        parent
    }};
}

macro_rules! make {
    (($p:expr)parent of($c:expr), $buf:expr) => {{
        let prev_parent = remove_from_parent!($buf, $c);
        if !($buf[&$p]).children.contains(&$c) {
            /* Pruned nodes keep their children in case they show up in a later merge, so do not panic
             * if children exists */
            $buf.entry($p).and_modify(|e| e.children.push($c));
        }
        let child_date = $buf[&$c].date;
        let child_len = $buf[&$c].len;
        let has_unseen = $buf[&$c].has_unseen;
        $buf.entry($c).and_modify(|e| {
            e.parent = Some($p);
        });
        $buf.entry($p).and_modify(|e| {
            e.len += child_len + 1;
            e.date = std::cmp::max(e.date, child_date);
            e.has_unseen |= has_unseen;
        });
        union($buf, $c, $p);
        prev_parent
        }};
}

/* Strip common prefixes from subjects */
trait SubjectPrefix {
    fn is_a_reply(&self) -> bool;
    fn strip_prefixes(&mut self) -> &mut Self;
}

impl SubjectPrefix for &[u8] {
    fn is_a_reply(&self) -> bool {
        self.starts_with(b"RE: ")
            || self.starts_with(b"Re: ")
            || self.starts_with(b"FW: ")
            || self.starts_with(b"Fw: ")
    }

    fn strip_prefixes(&mut self) -> &mut Self {
        let result = {
            let mut slice = self.trim();
            loop {
                if slice.starts_with(b"RE: ")
                    || slice.starts_with(b"Re: ")
                    || slice.starts_with(b"FW: ")
                    || slice.starts_with(b"Fw: ")
                {
                    slice = &slice[3..];
                    continue;
                }
                if slice.starts_with(b"FWD: ")
                    || slice.starts_with(b"Fwd: ")
                    || slice.starts_with(b"fwd: ")
                {
                    slice = &slice[4..];
                    continue;
                }
                if slice.starts_with(b" ") || slice.starts_with(b"\t") || slice.starts_with(b"\r") {
                    //FIXME just trim whitespace
                    slice = &slice[1..];
                    continue;
                }
                if slice.starts_with(b"[")
                    && !(slice.starts_with(b"[PATCH") || slice.starts_with(b"[RFC"))
                {
                    if let Some(pos) = slice.find(b"]") {
                        slice = &slice[pos..];
                        continue;
                    }
                    slice = &slice[1..];
                    continue;
                }
                break;
            }
            slice
        };
        *self = result;
        self
    }
}

/* Sorting states. */

#[derive(Debug, Clone, PartialEq, Copy, Deserialize, Serialize)]
pub enum SortOrder {
    Asc,
    Desc,
}

#[derive(Debug, Clone, PartialEq, Copy, Deserialize, Serialize)]
pub enum SortField {
    Subject,
    Date,
}

impl Default for SortField {
    fn default() -> Self {
        SortField::Date
    }
}

impl Default for SortOrder {
    fn default() -> Self {
        SortOrder::Desc
    }
}

impl FromStr for SortField {
    type Err = ();
    fn from_str(s: &str) -> StdResult<Self, Self::Err> {
        match s.trim() {
            "subject" | "s" | "sub" | "sbj" | "subj" => Ok(SortField::Subject),
            "date" | "d" => Ok(SortField::Date),
            _ => Err(()),
        }
    }
}

impl FromStr for SortOrder {
    type Err = ();
    fn from_str(s: &str) -> StdResult<Self, Self::Err> {
        match s.trim() {
            "asc" => Ok(SortOrder::Asc),
            "desc" => Ok(SortOrder::Desc),
            _ => Err(()),
        }
    }
}

/* `ThreadsIterator` returns messages according to the sorted order. For example, for the following
 * threads:
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

pub struct ThreadsIterator<'a> {
    pos: usize,
    stack: StackVec<usize>,
    root_tree: Ref<'a, Vec<ThreadHash>>,
    thread_nodes: &'a FnvHashMap<ThreadHash, ThreadNode>,
}
impl<'a> Iterator for ThreadsIterator<'a> {
    type Item = (usize, ThreadHash, bool);
    fn next(&mut self) -> Option<(usize, ThreadHash, bool)> {
        {
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
                    return Some(ret);
                }
                self.pos += 1;
                return Some(ret);
            }
        }
        self.next()
    }
}
/* `ThreadIterator` returns messages of a specific thread according to the sorted order. For example, for the following
 * thread:
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

pub struct ThreadIterator<'a> {
    init_pos: usize,
    pos: usize,
    stack: StackVec<usize>,
    root_tree: Ref<'a, Vec<ThreadHash>>,
    thread_nodes: &'a FnvHashMap<ThreadHash, ThreadNode>,
}
impl<'a> Iterator for ThreadIterator<'a> {
    type Item = (usize, ThreadHash);
    fn next(&mut self) -> Option<(usize, ThreadHash)> {
        {
            let mut tree = &(*self.root_tree);
            for i in self.stack.iter() {
                tree = &self.thread_nodes[&tree[*i]].children;
            }
            if self.pos == tree.len() || (self.stack.is_empty() && self.pos > self.init_pos) {
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
                    return Some(ret);
                }
                self.pos += 1;
                return Some(ret);
            }
        }
        self.next()
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ThreadNode {
    message: Option<EnvelopeHash>,
    parent: Option<ThreadHash>,
    children: Vec<ThreadHash>,
    date: UnixTimestamp,
    indentation: usize,
    show_subject: bool,
    pruned: bool,

    len: usize,
    has_unseen: bool,

    snoozed: bool,

    /* Union/Find set fields */
    thread_group: ThreadHash,
    rank: i32,
}

impl Default for ThreadNode {
    fn default() -> ThreadNode {
        ThreadNode {
            message: None,
            parent: None,
            children: Vec::new(),
            date: UnixTimestamp::default(),
            indentation: 0,
            show_subject: true,
            pruned: false,

            len: 0,
            has_unseen: false,
            snoozed: false,

            thread_group: ThreadHash::default(),
            rank: 0,
        }
    }
}

impl ThreadNode {
    fn new(thread_group: ThreadHash) -> Self {
        ThreadNode {
            thread_group,
            ..Default::default()
        }
    }
    pub fn show_subject(&self) -> bool {
        self.show_subject
    }

    pub fn has_unseen(&self) -> bool {
        self.has_unseen
    }

    pub fn set_has_unseen(&mut self, new_val: bool) {
        self.has_unseen = new_val;
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn date(&self) -> UnixTimestamp {
        self.date
    }

    pub fn datetime(&self) -> chrono::DateTime<chrono::Utc> {
        use chrono::{TimeZone, Utc};
        use std::convert::TryInto;

        Utc.timestamp(self.date.try_into().unwrap_or(0), 0)
    }

    pub fn is_empty(&self) -> bool {
        self.parent.is_none() && self.message.is_none() && self.children.is_empty()
    }

    pub fn message(&self) -> Option<EnvelopeHash> {
        self.message
    }

    pub fn has_message(&self) -> bool {
        self.message.is_some()
    }

    pub fn parent(&self) -> Option<ThreadHash> {
        self.parent
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn children(&self) -> &[ThreadHash] {
        &self.children
    }

    pub fn indentation(&self) -> usize {
        self.indentation
    }

    pub fn snoozed(&self) -> bool {
        self.snoozed
    }

    pub fn thread_group(&self) -> ThreadHash {
        self.thread_group
    }

    pub fn set_snoozed(&mut self, set: bool) {
        self.snoozed = set;
    }

    fn insert_child_pos(
        vec: &[ThreadHash],
        child: ThreadHash,
        sort: (SortField, SortOrder),
        buf: &FnvHashMap<ThreadHash, ThreadNode>,
        envelopes: &Envelopes,
    ) -> usize {
        let envelopes = envelopes.read().unwrap();
        match sort {
            (SortField::Date, SortOrder::Asc) => {
                match vec.binary_search_by(|probe| buf[&probe].date.cmp(&buf[&child].date)) {
                    Ok(p) => p,
                    Err(p) => p,
                }
            }
            (SortField::Date, SortOrder::Desc) => {
                match vec
                    .binary_search_by(|probe| buf[&probe].date.cmp(&buf[&child].date).reverse())
                {
                    Ok(p) => p,
                    Err(p) => p,
                }
            }
            (SortField::Subject, SortOrder::Asc) => {
                match vec.binary_search_by(|probe| {
                    match (
                        buf.get(&probe).map(|n| n.message.as_ref()).unwrap_or(None),
                        buf.get(&child).map(|n| n.message.as_ref()).unwrap_or(None),
                    ) {
                        (Some(p), Some(c)) => {
                            #[cfg(feature = "unicode_algorithms")]
                            {
                                envelopes[p]
                                    .subject()
                                    .split_graphemes()
                                    .cmp(&envelopes[c].subject().split_graphemes())
                            }
                            #[cfg(not(feature = "unicode_algorithms"))]
                            {
                                envelopes[p].subject().cmp(&envelopes[c].subject())
                            }
                        }
                        (Some(_), None) => Ordering::Greater,
                        (None, Some(_)) => Ordering::Less,
                        (None, None) => Ordering::Equal,
                    }
                }) {
                    Ok(p) => p,
                    Err(p) => p,
                }
            }
            (SortField::Subject, SortOrder::Desc) => {
                match vec.binary_search_by(|probe| {
                    match (
                        buf.get(&probe).map(|n| n.message.as_ref()).unwrap_or(None),
                        buf.get(&child).map(|n| n.message.as_ref()).unwrap_or(None),
                    ) {
                        (Some(p), Some(c)) => {
                            #[cfg(feature = "unicode_algorithms")]
                            {
                                envelopes[c]
                                    .subject()
                                    .split_graphemes()
                                    .cmp(&envelopes[p].subject().split_graphemes())
                            }
                            #[cfg(not(feature = "unicode_algorithms"))]
                            {
                                envelopes[c].subject().cmp(&envelopes[p].subject())
                            }
                        }
                        (Some(_), None) => Ordering::Less,
                        (None, Some(_)) => Ordering::Greater,
                        (None, None) => Ordering::Equal,
                    }
                }) {
                    Ok(p) => p,
                    Err(p) => p,
                }
            }
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Threads {
    pub thread_nodes: FnvHashMap<ThreadHash, ThreadNode>,
    root_set: RefCell<Vec<ThreadHash>>,
    tree_index: RefCell<Vec<ThreadHash>>,

    message_ids: FnvHashMap<Vec<u8>, ThreadHash>,
    pub message_ids_set: FnvHashSet<Vec<u8>>,
    pub missing_message_ids: FnvHashSet<Vec<u8>>,
    pub hash_set: FnvHashSet<EnvelopeHash>,
    sort: RefCell<(SortField, SortOrder)>,
    subsort: RefCell<(SortField, SortOrder)>,
}

impl PartialEq for ThreadNode {
    fn eq(&self, other: &ThreadNode) -> bool {
        match (self.message, other.message) {
            (Some(s), Some(o)) => s == o,
            _ => false,
        }
    }
}

pub struct RootIterator<'a> {
    pos: usize,
    root_tree: Ref<'a, Vec<ThreadHash>>,
    thread_nodes: &'a FnvHashMap<ThreadHash, ThreadNode>,
}

impl<'a> Iterator for RootIterator<'a> {
    type Item = ThreadHash;
    fn next(&mut self) -> Option<ThreadHash> {
        {
            if self.pos == self.root_tree.len() {
                return None;
            }
            let mut ret = self.root_tree[self.pos];
            self.pos += 1;
            let thread_node = &self.thread_nodes[&ret];
            if thread_node.message().is_none() {
                ret = thread_node.children()[0];
                while self.thread_nodes[&ret].message().is_none() {
                    ret = self.thread_nodes[&ret].children()[0];
                }
            }
            Some(ret)
        }
    }
}

pub fn find_root_hash(buf: &FnvHashMap<ThreadHash, ThreadNode>, h: ThreadHash) -> ThreadHash {
    if buf[&h].parent.is_none() {
        return h;
    }
    let p = buf[&h].parent.unwrap();
    if buf[&p].message.is_none() {
        return h;
    }
    find_root_hash(buf, p)
}

pub fn find_thread_group(buf: &FnvHashMap<ThreadHash, ThreadNode>, h: ThreadHash) -> ThreadHash {
    if buf[&h].thread_group == h {
        return h;
    }
    let p = buf[&h].thread_group;
    find_thread_group(buf, p)
}
fn find(buf: &mut FnvHashMap<ThreadHash, ThreadNode>, h: ThreadHash) -> ThreadHash {
    if buf[&h].thread_group == h {
        return h;
    }
    let p = buf[&h].thread_group;
    let new_group = find(buf, p);
    buf.entry(h).and_modify(|e| e.thread_group = new_group);
    new_group
}
fn union(buf: &mut FnvHashMap<ThreadHash, ThreadNode>, x: ThreadHash, y: ThreadHash) -> ThreadHash {
    let mut x_root = find(buf, x);
    let mut y_root = find(buf, y);

    // x and y are already in the same set
    if x_root == y_root {
        return x_root;
    }

    if buf[&x_root].rank < buf[&y_root].rank {
        mem::swap(&mut x_root, &mut y_root);
    }

    // x and y are not in same set, so we merge them
    //
    buf.entry(y_root).and_modify(|e| e.thread_group = x_root);
    if buf[&x_root].rank == buf[&y_root].rank {
        buf.entry(x_root).and_modify(|e| {
            e.rank += 1;
        });
    }
    x_root
}

impl Threads {
    pub fn is_snoozed(&self, h: ThreadHash) -> bool {
        let root = find_root_hash(&self.thread_nodes, h);
        self.thread_nodes[&root].snoozed()
    }
    pub fn find(&mut self, i: ThreadHash) -> ThreadHash {
        find(&mut self.thread_nodes, i)
    }
    fn union(&mut self, x: ThreadHash, y: ThreadHash) -> ThreadHash {
        let mut x_root = self.find(x);
        let mut y_root = self.find(y);

        // x and y are already in the same set
        if x_root == y_root {
            return x_root;
        }

        if self.thread_nodes[&x_root].rank < self.thread_nodes[&y_root].rank {
            mem::swap(&mut x_root, &mut y_root);
        }

        // x and y are not in same set, so we merge them
        //
        self.thread_nodes
            .entry(y_root)
            .and_modify(|e| e.thread_group = x_root);
        if self.thread_nodes[&x_root].rank == self.thread_nodes[&y_root].rank {
            self.thread_nodes.entry(x_root).and_modify(|e| e.rank += 1);
        }
        x_root
    }

    pub fn new(length: usize) -> Threads {
        /* To reconstruct thread information from the mails we need: */

        /* a vector to hold thread members */
        let thread_nodes: FnvHashMap<ThreadHash, ThreadNode> = FnvHashMap::with_capacity_and_hasher(
            (length as f64 * 1.2) as usize,
            Default::default(),
        );
        /* A hash table of Message IDs */
        let message_ids: FnvHashMap<Vec<u8>, ThreadHash> =
            FnvHashMap::with_capacity_and_hasher(length, Default::default());
        /* A hash set of Message IDs we haven't encountered yet as an Envelope */
        let missing_message_ids: FnvHashSet<Vec<u8>> =
            FnvHashSet::with_capacity_and_hasher(length, Default::default());
        /* A hash set of Message IDs we have encountered as a MessageID */
        let message_ids_set: FnvHashSet<Vec<u8>> =
            FnvHashSet::with_capacity_and_hasher(length, Default::default());
        let hash_set: FnvHashSet<EnvelopeHash> =
            FnvHashSet::with_capacity_and_hasher(length, Default::default());

        Threads {
            thread_nodes,
            message_ids,
            message_ids_set,
            missing_message_ids,
            hash_set,
            subsort: RefCell::new((SortField::Subject, SortOrder::Desc)),

            ..Default::default()
        }
    }

    pub fn threads_iter(&self) -> ThreadsIterator {
        ThreadsIterator {
            pos: 0,
            stack: StackVec::new(),
            root_tree: self.tree_index.borrow(),
            thread_nodes: &self.thread_nodes,
        }
    }

    pub fn thread_iter(&self, index: usize) -> ThreadIterator {
        ThreadIterator {
            init_pos: index,
            pos: index,
            stack: StackVec::new(),
            root_tree: self.tree_index.borrow(),
            thread_nodes: &self.thread_nodes,
        }
    }

    pub fn update_envelope(
        &mut self,
        envelopes: &Envelopes,
        old_hash: EnvelopeHash,
        new_hash: EnvelopeHash,
    ) -> Result<(), ()> {
        /* must update:
         * - hash_set
         * - message fields in thread_nodes
         */
        let thread_hash = if let Some((key, _)) = self
            .thread_nodes
            .iter()
            .find(|(_, n)| n.message.map(|n| n == old_hash).unwrap_or(false))
        {
            *key
        } else {
            return Err(());
        };

        self.thread_nodes.get_mut(&thread_hash).unwrap().message = Some(new_hash);
        self.thread_nodes.get_mut(&thread_hash).unwrap().has_unseen =
            !envelopes.read().unwrap()[&new_hash].is_seen()
                || self.thread_nodes[&thread_hash]
                    .children
                    .iter()
                    .fold(false, |acc, x| acc || self.thread_nodes[x].has_unseen);

        let mut thread_hash_iter = thread_hash;
        while self.thread_nodes[&thread_hash_iter].parent.is_some() {
            let parent_hash = self.thread_nodes[&thread_hash_iter].parent.unwrap();

            self.thread_nodes.get_mut(&parent_hash).unwrap().has_unseen = self.thread_nodes
                [&parent_hash]
                .children
                .iter()
                .fold(false, |acc, x| acc || self.thread_nodes[x].has_unseen);
            thread_hash_iter = parent_hash;
        }

        self.hash_set.remove(&old_hash);
        self.hash_set.insert(new_hash);
        Ok(())
    }

    #[inline]
    pub fn remove(&mut self, envelope_hash: EnvelopeHash) {
        self.hash_set.remove(&envelope_hash);

        let t_id: ThreadHash = if let Some((pos, n)) = self
            .thread_nodes
            .iter_mut()
            .find(|(_, n)| n.message.map(|n| n == envelope_hash).unwrap_or(false))
        {
            n.message = None;
            *pos
        } else {
            return;
        };

        if self.thread_nodes[&t_id].parent.is_none() {
            let mut tree_index = self.tree_index.borrow_mut();
            if let Some(i) = tree_index.iter().position(|t| *t == t_id) {
                tree_index.remove(i);
            }
        }
    }

    pub fn amend(&mut self, envelopes: &mut Envelopes) {
        let envelopes_lck = envelopes.read().unwrap();
        let new_hash_set = FnvHashSet::from_iter(envelopes_lck.keys().cloned());

        let difference: Vec<EnvelopeHash> =
            self.hash_set.difference(&new_hash_set).cloned().collect();
        for h in difference {
            self.remove(h);
        }
        drop(envelopes_lck);

        let difference: Vec<EnvelopeHash> =
            new_hash_set.difference(&self.hash_set).cloned().collect();
        for h in difference {
            //debug!("inserting {}", envelopes_lck[&h].subject());
            self.insert(envelopes, h);
        }
    }

    /// Update show_subject details of ThreadNode
    pub fn update_show_subject(
        &mut self,
        id: ThreadHash,
        env_hash: EnvelopeHash,
        envelopes: &Envelopes,
    ) {
        let envelopes = envelopes.read().unwrap();
        let mut subject = envelopes[&env_hash].subject();
        let mut subject = subject.to_mut().as_bytes();
        let stripped_subject = subject.strip_prefixes();
        if let Some(parent_id) = self.thread_nodes[&id].parent {
            if let Some(parent_hash) = self.thread_nodes[&parent_id].message {
                debug_assert!(envelopes.contains_key(&parent_hash));
                /* decide if the subject should be shown in the UI.
                 * If parent subject is Foobar and reply is `Re: Foobar`
                 * then showing the reply's subject is reduntant
                 */
                let mut parent_subject = envelopes[&parent_hash].subject();
                let mut parent_subject = parent_subject.to_mut().as_bytes();
                parent_subject.strip_prefixes();
                if stripped_subject == &parent_subject {
                    self.thread_nodes.entry(id).and_modify(|e| {
                        e.show_subject = false;
                    });
                }
            }
        }
        for i in 0..self.thread_nodes[&id].children.len() {
            let child_hash = self.thread_nodes[&id].children[i];
            if let Some(child_env_hash) = self.thread_nodes[&child_hash].message() {
                let mut child_subject = envelopes[&child_env_hash].subject();
                let mut child_subject = child_subject.to_mut().as_bytes();
                child_subject.strip_prefixes();
                if stripped_subject == &child_subject {
                    self.thread_nodes.entry(child_hash).and_modify(|e| {
                        e.show_subject = false;
                    });
                }
            }
        }
    }

    pub fn insert(&mut self, envelopes: &mut Envelopes, env_hash: EnvelopeHash) {
        let envelopes_lck = envelopes.read().unwrap();
        if self
            .message_ids
            .contains_key(envelopes_lck[&env_hash].message_id().raw())
            && !self
                .missing_message_ids
                .contains(envelopes_lck[&env_hash].message_id().raw())
        {
            return;
        }

        let reply_to_id: Option<ThreadHash> = envelopes_lck[&env_hash]
            .in_reply_to()
            .map(crate::email::StrBuild::raw)
            .and_then(|r| self.message_ids.get(r).cloned());
        let new_id = ThreadHash::new();
        self.thread_nodes.insert(
            new_id,
            ThreadNode {
                message: Some(env_hash),
                parent: reply_to_id,
                date: envelopes_lck[&env_hash].date(),
                has_unseen: !envelopes_lck[&env_hash].is_seen(),
                ..ThreadNode::new(new_id)
            },
        );
        self.message_ids
            .insert(envelopes_lck[&env_hash].message_id().raw().to_vec(), new_id);
        self.message_ids_set.insert(
            envelopes_lck[&env_hash]
                .message_id()
                .raw()
                .to_vec()
                .to_vec(),
        );
        self.missing_message_ids
            .remove(envelopes_lck[&env_hash].message_id().raw());
        self.hash_set.insert(env_hash);
        if let Some(reply_to_id) = reply_to_id {
            self.union(reply_to_id, new_id);
            make!((reply_to_id) parent of (new_id), &mut self.thread_nodes);
        } else {
            if let Some(r) = envelopes_lck[&env_hash]
                .in_reply_to()
                .map(crate::email::StrBuild::raw)
            {
                let reply_to_id = ThreadHash::new();
                self.thread_nodes.insert(
                    reply_to_id,
                    ThreadNode {
                        date: envelopes_lck[&env_hash].date(),
                        thread_group: reply_to_id,
                        ..ThreadNode::new(reply_to_id)
                    },
                );
                self.union(reply_to_id, new_id);
                make!((reply_to_id) parent of (new_id), &mut self.thread_nodes);
                self.missing_message_ids.insert(r.to_vec());
                self.message_ids.insert(r.to_vec(), reply_to_id);
                self.message_ids_set.insert(r.to_vec().to_vec());
            }
            self.tree_insert_root(new_id, envelopes);
        }
        drop(envelopes_lck);
        self.update_show_subject(new_id, env_hash, envelopes);
        envelopes
            .write()
            .unwrap()
            .get_mut(&env_hash)
            .unwrap()
            .set_thread(new_id);
    }

    /* Insert or update */
    pub fn insert_reply(&mut self, envelopes: &mut Envelopes, env_hash: EnvelopeHash) -> bool {
        let mut envelopes_lck = envelopes.write().unwrap();
        let reply_to_id: Option<ThreadHash> = envelopes_lck[&env_hash]
            .in_reply_to()
            .map(crate::email::StrBuild::raw)
            .and_then(|r| self.message_ids.get(r).cloned());
        if let Some(id) = self
            .message_ids
            .get(envelopes_lck[&env_hash].message_id().raw())
            .cloned()
        {
            if self.thread_nodes[&id].message.is_some() {
                return false;
            }
            self.thread_nodes.entry(id).and_modify(|n| {
                n.message = Some(env_hash);
                n.date = envelopes_lck[&env_hash].date();
                n.pruned = false;
                if n.parent.is_none() {
                    if let Some(reply_to_id) = reply_to_id {
                        n.parent = Some(reply_to_id);
                    }
                }
            });
            if let Some(reply_to_id) = reply_to_id {
                if !self.thread_nodes[&reply_to_id].children.contains(&id) {
                    make!((reply_to_id) parent of (id), &mut self.thread_nodes);
                    self.union(id, reply_to_id);
                }
            }

            self.message_ids
                .insert(envelopes_lck[&env_hash].message_id().raw().to_vec(), id);
            self.message_ids_set.insert(
                envelopes_lck[&env_hash]
                    .message_id()
                    .raw()
                    .to_vec()
                    .to_vec(),
            );
            self.missing_message_ids
                .remove(envelopes_lck[&env_hash].message_id().raw());
            envelopes_lck.get_mut(&env_hash).unwrap().set_thread(id);
            self.hash_set.insert(env_hash);
            drop(envelopes_lck);
            if self.thread_nodes[&id].parent.is_none() {
                self.tree_insert_root(id, envelopes);
            }
            {
                let mut tree_index = self.tree_index.borrow_mut();
                for c in &self.thread_nodes[&id].children {
                    if let Some(i) = tree_index.iter().position(|t| *t == *c) {
                        tree_index.remove(i);
                    }
                }
            }
            self.update_show_subject(id, env_hash, envelopes);
            true
        } else if let Some(reply_to_id) = reply_to_id {
            let new_id = ThreadHash::new();
            self.thread_nodes.insert(
                new_id,
                ThreadNode {
                    message: Some(env_hash),
                    parent: Some(reply_to_id),
                    date: envelopes_lck[&env_hash].date(),
                    has_unseen: !envelopes_lck[&env_hash].is_seen(),
                    ..ThreadNode::new(new_id)
                },
            );
            self.message_ids
                .insert(envelopes_lck[&env_hash].message_id().raw().to_vec(), new_id);
            self.message_ids_set.insert(
                envelopes_lck[&env_hash]
                    .message_id()
                    .raw()
                    .to_vec()
                    .to_vec(),
            );
            self.missing_message_ids
                .remove(envelopes_lck[&env_hash].message_id().raw());
            envelopes_lck.get_mut(&env_hash).unwrap().set_thread(new_id);
            self.hash_set.insert(env_hash);
            self.union(reply_to_id, new_id);
            make!((reply_to_id) parent of (new_id), &mut self.thread_nodes);
            drop(envelopes_lck);
            self.update_show_subject(new_id, env_hash, envelopes);
            true
        } else {
            false
        }
    }

    fn inner_subsort_by(&self, _subsort: (SortField, SortOrder), _envelopes: &Envelopes) {
        //FIXME: self\.thread_nodes needs interior mutability */
        return;
        /*
        let Threads {
            ref tree_index,
            ref thread_nodes,
            ..
        } = self;
        let tree = &mut tree_index.borrow_mut();
        for t in tree.iter_mut() {
            thread_nodes[t].children.sort_by(|a, b| match subsort {
                (SortField::Date, SortOrder::Desc) => {
                    let a = &thread_nodes[&a];
                    let b = &thread_nodes[&b];
                    b.date.cmp(&a.date)
                }
                (SortField::Date, SortOrder::Asc) => {
                    let a = &thread_nodes[&a];
                    let b = &thread_nodes[&b];
                    a.date.cmp(&b.date)
                }
                (SortField::Subject, SortOrder::Desc) => {
                    let a = &thread_nodes[&a].message();
                    let b = &thread_nodes[&b].message();

                    match (a, b) {
                        (Some(_), Some(_)) => {}
                        (Some(_), None) => {
                            return Ordering::Greater;
                        }
                        (None, Some(_)) => {
                            return Ordering::Less;
                        }
                        (None, None) => {
                            return Ordering::Equal;
                        }
                    }
                    let ma = &envelopes[&a.unwrap()];
                    let mb = &envelopes[&b.unwrap()];
                    ma.subject().cmp(&mb.subject())
                }
                (SortField::Subject, SortOrder::Asc) => {
                    let a = &thread_nodes[&a].message();
                    let b = &thread_nodes[&b].message();

                    match (a, b) {
                        (Some(_), Some(_)) => {}
                        (Some(_), None) => {
                            return Ordering::Less;
                        }
                        (None, Some(_)) => {
                            return Ordering::Greater;
                        }
                        (None, None) => {
                            return Ordering::Equal;
                        }
                    }
                    let ma = &envelopes[&a.unwrap()];
                    let mb = &envelopes[&b.unwrap()];
                    mb.subject().cmp(&ma.subject())
                }
            });
        }
        */
    }

    pub fn vec_inner_sort_by(
        &self,
        vec: &mut Vec<ThreadHash>,
        sort: (SortField, SortOrder),
        envelopes: &Envelopes,
    ) {
        let envelopes = envelopes.read().unwrap();
        vec.sort_by(|b, a| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let a = &self.thread_nodes[&a];
                let b = &self.thread_nodes[&b];
                b.date.cmp(&a.date)
            }
            (SortField::Date, SortOrder::Asc) => {
                let a = &self.thread_nodes[&a];
                let b = &self.thread_nodes[&b];
                a.date.cmp(&b.date)
            }
            (SortField::Subject, SortOrder::Desc) => {
                let a = &self.thread_nodes[&a].message();
                let b = &self.thread_nodes[&b].message();

                match (a, b) {
                    (Some(_), Some(_)) => {}
                    (Some(_), None) => {
                        return Ordering::Greater;
                    }
                    (None, Some(_)) => {
                        return Ordering::Less;
                    }
                    (None, None) => {
                        return Ordering::Equal;
                    }
                }
                let ma = &envelopes[&a.unwrap()];
                let mb = &envelopes[&b.unwrap()];
                #[cfg(feature = "unicode_algorithms")]
                {
                    ma.subject()
                        .split_graphemes()
                        .cmp(&mb.subject().split_graphemes())
                }
                #[cfg(not(feature = "unicode_algorithms"))]
                {
                    ma.subject().cmp(&mb.subject())
                }
            }
            (SortField::Subject, SortOrder::Asc) => {
                let a = &self.thread_nodes[&a].message();
                let b = &self.thread_nodes[&b].message();

                match (a, b) {
                    (Some(_), Some(_)) => {}
                    (Some(_), None) => {
                        return Ordering::Less;
                    }
                    (None, Some(_)) => {
                        return Ordering::Greater;
                    }
                    (None, None) => {
                        return Ordering::Equal;
                    }
                }
                let ma = &envelopes[&a.unwrap()];
                let mb = &envelopes[&b.unwrap()];
                #[cfg(feature = "unicode_algorithms")]
                {
                    mb.subject()
                        .as_ref()
                        .split_graphemes()
                        .cmp(&ma.subject().split_graphemes())
                }

                #[cfg(not(feature = "unicode_algorithms"))]
                {
                    mb.subject().as_ref().cmp(&ma.subject())
                }
            }
        });
    }
    fn inner_sort_by(&self, sort: (SortField, SortOrder), envelopes: &Envelopes) {
        let tree = &mut self.tree_index.borrow_mut();
        let envelopes = envelopes.read().unwrap();
        tree.sort_by(|b, a| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let a = &self.thread_nodes[&a];
                let b = &self.thread_nodes[&b];
                b.date.cmp(&a.date)
            }
            (SortField::Date, SortOrder::Asc) => {
                let a = &self.thread_nodes[&a];
                let b = &self.thread_nodes[&b];
                a.date.cmp(&b.date)
            }
            (SortField::Subject, SortOrder::Desc) => {
                let a = &self.thread_nodes[&a].message();
                let b = &self.thread_nodes[&b].message();

                match (a, b) {
                    (Some(_), Some(_)) => {}
                    (Some(_), None) => {
                        return Ordering::Greater;
                    }
                    (None, Some(_)) => {
                        return Ordering::Less;
                    }
                    (None, None) => {
                        return Ordering::Equal;
                    }
                }
                let ma = &envelopes[&a.unwrap()];
                let mb = &envelopes[&b.unwrap()];
                #[cfg(feature = "unicode_algorithms")]
                {
                    ma.subject()
                        .split_graphemes()
                        .cmp(&mb.subject().split_graphemes())
                }
                #[cfg(not(feature = "unicode_algorithms"))]
                {
                    ma.subject().cmp(&mb.subject())
                }
            }
            (SortField::Subject, SortOrder::Asc) => {
                let a = &self.thread_nodes[&a].message();
                let b = &self.thread_nodes[&b].message();

                match (a, b) {
                    (Some(_), Some(_)) => {}
                    (Some(_), None) => {
                        return Ordering::Less;
                    }
                    (None, Some(_)) => {
                        return Ordering::Greater;
                    }
                    (None, None) => {
                        return Ordering::Equal;
                    }
                }
                let ma = &envelopes[&a.unwrap()];
                let mb = &envelopes[&b.unwrap()];
                #[cfg(feature = "unicode_algorithms")]
                {
                    mb.subject()
                        .as_ref()
                        .split_graphemes()
                        .cmp(&ma.subject().split_graphemes())
                }

                #[cfg(not(feature = "unicode_algorithms"))]
                {
                    mb.subject().as_ref().cmp(&ma.subject())
                }
            }
        });
    }

    pub fn sort_by(
        &self,
        sort: (SortField, SortOrder),
        subsort: (SortField, SortOrder),
        envelopes: &Envelopes,
    ) {
        if *self.sort.borrow() != sort {
            self.inner_sort_by(sort, envelopes);
            *self.sort.borrow_mut() = sort;
        }
        if *self.subsort.borrow() != subsort {
            self.inner_subsort_by(subsort, envelopes);
            *self.subsort.borrow_mut() = subsort;
        }
    }

    pub fn thread_to_mail(&self, i: ThreadHash) -> EnvelopeHash {
        let thread = &self.thread_nodes[&i];
        thread.message().unwrap()
    }

    pub fn thread_nodes(&self) -> &FnvHashMap<ThreadHash, ThreadNode> {
        &self.thread_nodes
    }

    pub fn len(&self) -> usize {
        self.hash_set.len()
    }

    pub fn root_len(&self) -> usize {
        self.tree_index.borrow().len()
    }

    pub fn root_set(&self, idx: usize) -> ThreadHash {
        self.tree_index.borrow()[idx]
    }

    pub fn root_iter(&self) -> RootIterator {
        RootIterator {
            pos: 0,
            root_tree: self.tree_index.borrow(),
            thread_nodes: &self.thread_nodes,
        }
    }

    /*
    fn link_envelope(&mut self, envelope: &mut Envelope) {
        let t_idx: ThreadHash = {
            let m_id = envelope.message_id().raw();

            /* t_idx: The index of this message's ThreadNode in thread_nodes
             *
             * If id_table contains an empty Container for this ID:
             *  Store this message in the Container's message slot.
             * Else:
             *  Create a new Container object holding this message;
             *  Index the Container by Message-ID in id_table.
             */
            if self.message_ids.get(m_id).is_some() {
                let node_idx = self.message_ids[m_id];
                /* the already existing ThreadNote should be empty, since we're
                 * seeing this message for the first time. otherwise it's a
                 * duplicate. */
                if !self.missing_message_ids.contains(m_id) {
                    return;
                }
                self.missing_message_ids.remove(m_id);
                node_idx
            } else {
                /* Create a new ThreadNode object holding this message */
                /* The new thread node's set is just itself */
                let new_id = ThreadHash::new();
                let node = ThreadNode {
                    message: Some(envelope.hash()),
                    date: envelope.date(),
                    thread_group: new_id,
                    ..Default::default()
                };
                self.thread_nodes.insert(new_id, node);

                self.message_ids.insert(m_id.to_vec(), new_id);
                self.message_ids_set.insert(m_id.to_vec());
                new_id
            }
        };
        self.thread_nodes.entry(t_idx).and_modify(|e| {
            e.date = envelope.date();
            e.message = Some(envelope.hash());
            e.has_unseen |= !envelope.is_seen();
        });
        envelope.set_thread(t_idx);
        self.hash_set.insert(envelope.hash());

        /* For each element in the message's References field:
         *
         * Find a ThreadNode object for the given Message-ID:
         * If there's one in message_ids use that;
         * Otherwise, make (and index) one with a null Message
         *
         * Link the References field's ThreadNode together in the order implied
         * by the References header.
         */

        /* The index of the reference we are currently examining, start from current message */
        let mut ref_ptr = t_idx;

        for &refn in envelope.references().iter().rev() {
            let r_id = refn.raw();
            let parent_id = if self.message_ids.contains_key(r_id) {
                self.message_ids[r_id]
            } else {
                /* Create a new ThreadNode object holding this reference */
                let new_id = ThreadHash::new();
                self.thread_nodes.insert(
                    new_id,
                    ThreadNode {
                        date: envelope.date(),
                        thread_group: new_id,
                        ..Default::default()
                    },
                );
                self.message_ids.insert(r_id.to_vec(), new_id);
                self.missing_message_ids.insert(r_id.to_vec());
                self.message_ids_set.insert(r_id.to_vec());
                new_id
            };

            /* If they are already linked, don't change the existing links.
            if self.thread_nodes[&ref_ptr].has_parent()
                && self.thread_nodes[&ref_ptr].parent.unwrap() != parent_id
            {
                ref_ptr = parent_id;
                continue;
            } */
            if self.thread_nodes[&ref_ptr].parent.is_some() {
                if self.thread_nodes[&parent_id].parent == Some(ref_ptr) {
                    eprintln!("ALARM");
                    remove_from_parent!(&mut self.thread_nodes, parent_id);
                }
                ref_ptr = parent_id;
                continue;
            }

            /* Do not add a link if adding that link would introduce a loop: that is, before
             * asserting A->B, search down the children of B to see if A is reachable, and also
             * search down the children of A to see if B is reachable. If either is already
             * reachable as a child of the other, don't add the link.
             */
            if self.find(ref_ptr) != self.find(parent_id) {
                self.union(ref_ptr, parent_id);
                make!((parent_id) parent of (ref_ptr), &mut self.thread_nodes);
            }
            ref_ptr = parent_id;
        }
    }*/

    fn tree_insert_root(&mut self, new_id: ThreadHash, envelopes: &Envelopes) {
        debug_assert!(
            self.thread_nodes[&new_id].parent.is_none()
                || self.thread_nodes[self.thread_nodes[&new_id].parent.as_ref().unwrap()]
                    .message
                    .is_none()
        );
        /* Index of reply_to_id in self.trees */
        let Threads {
            ref mut tree_index,
            ref thread_nodes,
            ..
        } = self;
        let mut tree_index = tree_index.borrow_mut();
        for c in &thread_nodes[&new_id].children {
            if let Some(i) = tree_index.iter().position(|t| *t == *c) {
                tree_index.remove(i);
            }
        }
        let pos = ThreadNode::insert_child_pos(
            &tree_index,
            new_id,
            *self.sort.borrow(),
            &self.thread_nodes,
            envelopes,
        );
        tree_index.insert(pos, new_id);
    }
}

impl Index<&ThreadHash> for Threads {
    type Output = ThreadNode;

    fn index(&self, index: &ThreadHash) -> &ThreadNode {
        self.thread_nodes
            .get(index)
            .expect("thread index out of bounds")
    }
}

/*
fn print_threadnodes(
    node_hash: ThreadHash,
    nodes: &FnvHashMap<ThreadHash, ThreadNode>,
    envelopes: &Envelopes,
) {
    fn help(
        level: usize,
        node_hash: ThreadHash,
        nodes: &FnvHashMap<ThreadHash, ThreadNode>,
        envelopes: &Envelopes,
    ) {
        eprint!("{}ThreadNode {}\n{}\tmessage: {}\n{}\tparent: {}\n{}\tthread_group: {}\n{}\tchildren (len: {}):\n",
                  "\t".repeat(level),
                  node_hash,
                  "\t".repeat(level),
                  nodes[&node_hash].message().as_ref().map(|m| format!("{} - {}\n{}\t\t{}", envelopes[m].message_id_display(), envelopes[m].subject(), "\t".repeat(level), envelopes[m].references().iter().map(ToString::to_string).collect::<Vec<String>>().join(", "))).unwrap_or_else(|| "None".to_string()),
                  "\t".repeat(level),
                  nodes[&node_hash].parent().as_ref().map(ToString::to_string).unwrap_or_else(|| "None".to_string()),
                  "\t".repeat(level),
                  nodes[&node_hash].thread_group,
                  "\t".repeat(level),
                  nodes[&node_hash].children.len(),
                  );
        for c in &nodes[&node_hash].children {
            help(level + 2, *c, nodes, envelopes);
        }
    }
    help(0, node_hash, nodes, envelopes);
}
*/
