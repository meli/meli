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
 * The entry point of this module is the `Threads` struct and its `new` method. It contains `ThreadNodes` which are the
 * nodes in the thread trees that might have messages associated with them. The root nodes (first
 * messages in each thread) are stored in `root_set` and `tree` vectors. The `ThreadTree` struct
 * contains the sorted threads. `Threads` has inner mutability since we need to sort without the
 * user having mutable ownership.
 */

use crate::email::parser::BytesExt;
use crate::email::*;
use crate::grapheme_clusters::*;
use crate::structs::StackVec;
use uuid::Uuid;

use fnv::{FnvHashMap, FnvHashSet};
use std::cell::{Ref, RefCell};
use std::cmp;
use std::cmp::Ordering;
use std::fmt;
use std::iter::FromIterator;
use std::mem;
use std::ops::Index;
use std::result::Result as StdResult;
use std::str::FromStr;
use std::string::ToString;

type Envelopes = FnvHashMap<EnvelopeHash, Envelope>;

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
        $buf.entry($c).and_modify(|e| e.parent = Some($p));
        union($buf, $c, $p);
        prev_parent
        }};
}

/* Strip common prefixes from subjects */
trait SubjectPrefix {
    fn is_a_reply(&self) -> bool;
    fn strip_prefixes(&mut self);
}

impl SubjectPrefix for &[u8] {
    fn is_a_reply(&self) -> bool {
        self.starts_with(b"RE: ")
            || self.starts_with(b"Re: ")
            || self.starts_with(b"FW: ")
            || self.starts_with(b"Fw: ")
    }

    fn strip_prefixes(&mut self) {
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

/*
 * The thread tree holds the sorted state of the thread nodes */

#[derive(Clone, Debug, Deserialize, Serialize)]
struct ThreadTree {
    id: ThreadHash,
    children: Vec<ThreadTree>,
}

impl ThreadTree {
    fn new(id: ThreadHash) -> Self {
        ThreadTree {
            id,
            children: Vec::new(),
        }
    }
    fn insert_child(
        vec: &mut Vec<ThreadTree>,
        child: ThreadTree,
        sort: (SortField, SortOrder),
        buf: &FnvHashMap<ThreadHash, ThreadNode>,
        envelopes: &Envelopes,
    ) -> usize {
        let pos = match sort {
            (SortField::Date, SortOrder::Asc) => {
                match vec.binary_search_by(|probe| buf[&probe.id].date.cmp(&buf[&child.id].date)) {
                    Ok(p) => p,
                    Err(p) => p,
                }
            }
            (SortField::Date, SortOrder::Desc) => {
                match vec.binary_search_by(|probe| {
                    buf[&probe.id].date.cmp(&buf[&child.id].date).reverse()
                }) {
                    Ok(p) => p,
                    Err(p) => p,
                }
            }
            (SortField::Subject, SortOrder::Asc) => {
                match vec.binary_search_by(|probe| {
                    match (
                        buf.get(&probe.id)
                            .map(|n| n.message.as_ref())
                            .unwrap_or(None),
                        buf.get(&child.id)
                            .map(|n| n.message.as_ref())
                            .unwrap_or(None),
                    ) {
                        (Some(p), Some(c)) => envelopes[p]
                            .subject()
                            .split_graphemes()
                            .cmp(&envelopes[c].subject().split_graphemes()),
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
                        buf.get(&probe.id)
                            .map(|n| n.message.as_ref())
                            .unwrap_or(None),
                        buf.get(&child.id)
                            .map(|n| n.message.as_ref())
                            .unwrap_or(None),
                    ) {
                        (Some(p), Some(c)) => envelopes[c]
                            .subject()
                            .split_graphemes()
                            .cmp(&envelopes[p].subject().split_graphemes()),
                        (Some(_), None) => Ordering::Less,
                        (None, Some(_)) => Ordering::Greater,
                        (None, None) => Ordering::Equal,
                    }
                }) {
                    Ok(p) => p,
                    Err(p) => p,
                }
            }
        };
        vec.insert(pos, child);
        pos
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
    tree: Ref<'a, Vec<ThreadTree>>,
}
impl<'a> Iterator for ThreadsIterator<'a> {
    type Item = (usize, ThreadHash, bool);
    fn next(&mut self) -> Option<(usize, ThreadHash, bool)> {
        {
            let mut tree = &(*self.tree);
            for i in self.stack.iter() {
                tree = &tree[*i].children;
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
                    tree[self.pos].id,
                    !self.stack.is_empty() && (self.pos < (tree.len() - 1)),
                );
                if !tree[self.pos].children.is_empty() {
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
    tree: Ref<'a, Vec<ThreadTree>>,
}
impl<'a> Iterator for ThreadIterator<'a> {
    type Item = (usize, ThreadHash);
    fn next(&mut self) -> Option<(usize, ThreadHash)> {
        {
            let mut tree = &(*self.tree);
            for i in self.stack.iter() {
                tree = &tree[*i].children;
            }
            if self.pos == tree.len() || (self.stack.is_empty() && self.pos > self.init_pos) {
                if self.stack.is_empty() {
                    return None;
                }
                self.pos = self.stack.pop().unwrap() + 1;
            } else {
                debug_assert!(self.pos < tree.len());
                let ret = (self.stack.len(), tree[self.pos].id);
                if !tree[self.pos].children.is_empty() {
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

    pub fn len(&self) -> usize {
        self.len
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
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Threads {
    pub thread_nodes: FnvHashMap<ThreadHash, ThreadNode>,
    root_set: RefCell<Vec<ThreadHash>>,
    tree: RefCell<Vec<ThreadTree>>,

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
    root_tree: Ref<'a, Vec<ThreadTree>>,
}

impl<'a> Iterator for RootIterator<'a> {
    type Item = ThreadHash;
    fn next(&mut self) -> Option<ThreadHash> {
        {
            if self.pos == self.root_tree.len() {
                return None;
            }
            self.pos += 1;
            Some(self.root_tree[self.pos - 1].id)
        }
    }
}
fn find_ref(buf: &FnvHashMap<ThreadHash, ThreadNode>, h: ThreadHash) -> ThreadHash {
    if buf[&h].thread_group == h {
        return h;
    }
    let p = buf[&h].thread_group;
    find_ref(buf, p)
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
        let root = find_ref(&self.thread_nodes, h);
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
    fn prune_empty_nodes(&mut self, root_set: &mut Vec<ThreadHash>) {
        fn prune(
            thread_nodes: &mut FnvHashMap<ThreadHash, ThreadNode>,
            idx: ThreadHash,
            root_set: &mut Vec<ThreadHash>,
        ) -> bool {
            /* "If it is an empty container with no children, nuke it." */
            if !thread_nodes[&idx].has_message() && thread_nodes[&idx].children.is_empty() {
                remove_from_parent!(thread_nodes, idx);
                thread_nodes.entry(idx).and_modify(|n| n.pruned = true);
                return true;
            }

            /*
                    if !thread_nodes[&idx].has_message() && !thread_nodes[&idx].has_parent() {
                        if thread_nodes[&idx].children.len() == 1 {
                            /* "Do not promote the children if doing so would promote them to the root set
                             * -- unless there is only one child, in which case, do." */
                            let child = thread_nodes[&idx].children[0];
                            root_set.push(child);
                            remove_from_parent!(thread_nodes, child);
                            thread_nodes.entry(idx).and_modify(|n| {
                                n.pruned = true;
                                n.children.push(child);
                            });
                            return true; // Pruned
                        }
                    } else if let Some(p) = thread_nodes[&idx].parent {
                        if !thread_nodes[&idx].has_message() {
                            let orphans = thread_nodes[&idx].children.clone();
                            for c in &orphans {
                                make!((p) parent of (*c), thread_nodes);
                            }
                            remove_from_parent!(thread_nodes, idx);
                            /* Keep children in case we happen upon them later and mark it as pruned */
                            thread_nodes.entry(idx).and_modify(|n| {
                                n.pruned = true;
                                n.children = orphans;
                            });
                            return true; // Pruned
                        }
                    }
            */

            /*  Recurse to children, but keep in mind more children can be added in each iteration
             */
            let mut c_idx = 0;
            loop {
                if c_idx >= thread_nodes[&idx].children.len() {
                    break;
                }
                let c = thread_nodes[&idx].children[c_idx];
                if !prune(thread_nodes, c, root_set) {
                    c_idx += 1;
                }
            }
            thread_nodes[&idx].pruned
        }

        let mut idx = 0;
        loop {
            if idx >= root_set.len() {
                break;
            }
            if prune(&mut self.thread_nodes, root_set[idx], root_set) {
                root_set.remove(idx);
            } else {
                idx += 1;
            }
        }
    }

    pub fn prune_tree(&self) {
        self.tree
            .borrow_mut()
            .retain(|c| !self.thread_nodes[&c.id].is_empty());
    }

    pub fn new(envelopes: &mut Envelopes) -> Threads {
        /* To reconstruct thread information from the mails we need: */

        /* a vector to hold thread members */
        let thread_nodes: FnvHashMap<ThreadHash, ThreadNode> = FnvHashMap::with_capacity_and_hasher(
            (envelopes.len() as f64 * 1.2) as usize,
            Default::default(),
        );
        /* A hash table of Message IDs */
        let message_ids: FnvHashMap<Vec<u8>, ThreadHash> =
            FnvHashMap::with_capacity_and_hasher(envelopes.len(), Default::default());
        /* A hash set of Message IDs we haven't encountered yet as an Envelope */
        let missing_message_ids: FnvHashSet<Vec<u8>> =
            FnvHashSet::with_capacity_and_hasher(envelopes.len(), Default::default());
        /* A hash set of Message IDs we have encountered as a MessageID */
        let message_ids_set: FnvHashSet<Vec<u8>> =
            FnvHashSet::with_capacity_and_hasher(envelopes.len(), Default::default());
        let hash_set: FnvHashSet<EnvelopeHash> =
            FnvHashSet::with_capacity_and_hasher(envelopes.len(), Default::default());

        let mut t = Threads {
            thread_nodes,
            message_ids,
            message_ids_set,
            missing_message_ids,
            hash_set,
            subsort: RefCell::new((SortField::Subject, SortOrder::Desc)),

            ..Default::default()
        };
        /* Add each message to message_ids and threads, and link them together according to the
         * References / In-Reply-To headers */
        t.link_threads(envelopes);

        t.create_root_set(envelopes);
        t.build_envelopes(envelopes);
        //for (i, _t) in t.thread_nodes.iter().enumerate() {
        //    if !_t.has_parent() && _t.children.is_empty() && !_t.has_message() {
        //        continue;
        //    }
        //    debug!("--------------------------");
        //    if let Some(m) = _t.message {
        //        debug!(
        //            "\tmessage: {}\t{}",
        //            envelopes[&m].subject(),
        //            envelopes[&m].message_id()
        //        );
        //    } else {
        //        debug!("\tNo message");
        //    }
        //    debug!(
        //        "Thread #{}, children {}:\n\t{:#?}",
        //        i,
        //        _t.children.len(),
        //        _t
        //    );
        //    if !_t.children.is_empty() {
        //        debug!("{:?}", _t.children);
        //    }
        //}
        //for (i, _t) in t.tree.borrow().iter().enumerate() {
        //    debug!("Tree #{} id {}, children {}", i, _t.id, _t.children.len());
        //    if let Some(m) = t.thread_nodes[_t.id].message {
        //        debug!("\tmessage: {}", envelopes[&m].subject());
        //    } else {
        //        debug!("\tNo message");
        //    }
        //}
        t
    }

    fn create_root_set(&mut self, envelopes: &Envelopes) {
        /* Walk over the elements of message_ids, and gather a list of the ThreadNode objects that
         * have no parents. These are the root messages of each thread */
        let mut root_set: Vec<ThreadHash> = Vec::with_capacity(envelopes.len());

        /* Find the root set */
        for v in self.message_ids.values() {
            if self.thread_nodes[v].parent.is_none() {
                root_set.push(*v);
            }
        }

        /* Prune empty thread nodes */
        self.prune_empty_nodes(&mut root_set);

        self.root_set = RefCell::new(root_set);
    }

    pub fn print_tree(&self, envelopes: &Envelopes) {
        let len = self.tree.borrow().len();
        for (i, t) in self.tree.borrow().iter().enumerate() {
            debug!("tree #{}/{}", i + 1, len);
            print_threadnodes(t.id, &self.thread_nodes, envelopes);
        }
    }
    pub fn threads_iter(&self) -> ThreadsIterator {
        ThreadsIterator {
            pos: 0,
            stack: StackVec::new(),
            tree: self.tree.borrow(),
        }
    }

    pub fn thread_iter(&self, index: usize) -> ThreadIterator {
        ThreadIterator {
            init_pos: index,
            pos: index,
            stack: StackVec::new(),
            tree: self.tree.borrow(),
        }
    }

    pub fn update_envelope(
        &mut self,
        old_hash: EnvelopeHash,
        new_hash: EnvelopeHash,
        envelopes: &Envelopes,
    ) -> Result<(), ()> {
        /* must update:
         * - hash_set
         * - message fields in thread_nodes
         */
        let thread_hash = if let Some((key, node)) = self
            .thread_nodes
            .iter_mut()
            .find(|(_, n)| n.message.map(|n| n == old_hash).unwrap_or(false))
        {
            node.message = Some(new_hash);
            *key
        } else {
            return Err(());
        };
        self.hash_set.remove(&old_hash);
        self.hash_set.insert(new_hash);
        self.rebuild_thread(thread_hash, envelopes);
        Ok(())
    }

    #[inline]
    pub fn remove(&mut self, envelope_hash: EnvelopeHash, envelopes: &mut Envelopes) {
        self.hash_set.remove(&envelope_hash);
        //{
        //    let pos = self
        //        .thread_nodes
        //        .iter()
        //        .position(|n| n.message.map(|n| n == envelope_hash).unwrap_or(false))
        //        .unwrap();
        // debug!("DEBUG: {} in threads is idx= {}", envelope_hash, pos);
        //}

        let t_id: ThreadHash;
        {
            if let Some((pos, n)) = self
                .thread_nodes
                .iter_mut()
                .find(|(_, n)| n.message.map(|n| n == envelope_hash).unwrap_or(false))
            {
                t_id = *pos;
                n.message = None;
            } else {
                /* else it was deleted during a thread_rebuild or others */
                return;
            }
        }

        let mut node_idx = t_id;

        /* Trace path back to root ThreadNode */
        while let Some(p) = &self.thread_nodes[&node_idx].parent {
            node_idx = *p;
        }
        {
            let tree = self.tree.get_mut();
            if let Some(pos) = tree.iter().position(|t| t.id == node_idx) {
                tree[pos].children.clear();
                if node_idx == t_id {
                    tree.remove(pos);
                } else {
                    node_build(
                        &mut tree[pos],
                        node_idx,
                        *(self.sort.borrow()),
                        &mut self.thread_nodes,
                        1,
                        envelopes,
                    );
                }
            }
        }

        let mut root_set: Vec<ThreadHash> = self.tree.borrow().iter().map(|t| t.id).collect();
        self.prune_empty_nodes(&mut root_set);
        self.tree.borrow_mut().retain(|t| root_set.contains(&t.id));
    }

    pub fn amend(&mut self, envelopes: &mut Envelopes) {
        let new_hash_set = FnvHashSet::from_iter(envelopes.keys().cloned());

        let difference: Vec<EnvelopeHash> =
            self.hash_set.difference(&new_hash_set).cloned().collect();
        for h in difference {
            self.remove(h, envelopes);
        }

        let difference: Vec<EnvelopeHash> =
            new_hash_set.difference(&self.hash_set).cloned().collect();
        for h in difference {
            debug!("inserting {}", envelopes[&h].subject());
            let env = envelopes.entry(h).or_default() as *mut Envelope;
            unsafe {
                // `envelopes` is borrowed immutably and `insert` only changes the envelope's
                // `thread` field.
                self.insert(&mut (*env), envelopes);
            }
        }
        self.create_root_set(envelopes);

        let mut root_set: Vec<ThreadHash> = self.tree.borrow().iter().map(|t| t.id).collect();
        self.prune_empty_nodes(&mut root_set);
        let tree = self.tree.get_mut();
        tree.retain(|t| root_set.contains(&t.id));
    }

    pub fn insert(&mut self, envelope: &mut Envelope, envelopes: &Envelopes) {
        self.link_envelope(envelope);
        {
            let id = self.message_ids[envelope.message_id().raw()];
            self.rebuild_thread(id, envelopes);
        }
    }

    /* Insert or update */
    pub fn insert_reply(&mut self, envelopes: &mut Envelopes, env_hash: EnvelopeHash) -> bool {
        let reply_to_id: Option<ThreadHash> = self
            .message_ids
            .get(
                envelopes[&env_hash]
                    .in_reply_to()
                    .map(crate::email::StrBuild::raw)
                    .unwrap_or(&[]),
            )
            .cloned();
        if let Some(id) = self
            .message_ids
            .get(envelopes[&env_hash].message_id().raw())
            .cloned()
        {
            self.thread_nodes.entry(id).and_modify(|n| {
                n.message = Some(env_hash);
                n.date = envelopes[&env_hash].date();
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

            self.rebuild_thread(reply_to_id.unwrap_or(id), envelopes);
            self.message_ids
                .insert(envelopes[&env_hash].message_id().raw().to_vec(), id);
            self.message_ids_set
                .insert(envelopes[&env_hash].message_id().raw().to_vec().to_vec());
            self.missing_message_ids
                .remove(envelopes[&env_hash].message_id().raw());
            envelopes.get_mut(&env_hash).unwrap().set_thread(id);
            self.hash_set.insert(env_hash);
            true
        } else if let Some(reply_to_id) = reply_to_id {
            let new_id = ThreadHash::new();
            self.thread_nodes.insert(
                new_id,
                ThreadNode {
                    message: Some(env_hash),
                    parent: Some(reply_to_id),
                    date: envelopes[&env_hash].date(),
                    ..ThreadNode::new(new_id)
                },
            );
            self.message_ids
                .insert(envelopes[&env_hash].message_id().raw().to_vec(), new_id);
            self.message_ids_set
                .insert(envelopes[&env_hash].message_id().raw().to_vec().to_vec());
            self.missing_message_ids
                .remove(envelopes[&env_hash].message_id().raw());
            envelopes.get_mut(&env_hash).unwrap().set_thread(new_id);
            self.hash_set.insert(env_hash);
            self.union(reply_to_id, new_id);
            make!((reply_to_id) parent of (new_id), &mut self.thread_nodes);
            self.rebuild_thread(reply_to_id, envelopes);
            true
        } else {
            let new_id = ThreadHash::new();
            self.thread_nodes.insert(
                new_id,
                ThreadNode {
                    message: Some(env_hash),
                    parent: None,
                    date: envelopes[&env_hash].date(),
                    ..ThreadNode::new(new_id)
                },
            );
            self.message_ids
                .insert(envelopes[&env_hash].message_id().raw().to_vec(), new_id);
            self.message_ids_set
                .insert(envelopes[&env_hash].message_id().raw().to_vec().to_vec());
            self.missing_message_ids
                .remove(envelopes[&env_hash].message_id().raw());
            envelopes.get_mut(&env_hash).unwrap().set_thread(new_id);
            self.hash_set.insert(env_hash);
            self.rebuild_thread(new_id, envelopes);
            false
        }
        /*
        {
            if let Some(in_reply_to) = envelope.in_reply_to() {
                if !self.message_ids.contains_key(in_reply_to.raw()) {
                    return false;
                }
            } else {
                return false;
            }
        }
        let hash: EnvelopeHash = envelope.hash();
        envelopes.insert(hash, envelope.clone());
        {
            let envelope = envelopes.entry(hash).or_default() as *mut Envelope;
            unsafe {
                /* Safe because insert only changes envelope's fields and nothing more */
                self.insert(&mut (*envelope), &envelopes);
            }
        }
        let envelope: &Envelope = &envelopes[&hash];
        {
            let in_reply_to = envelope.in_reply_to().unwrap().raw();
            let parent_id = self.message_ids[in_reply_to];
            self.rebuild_thread(parent_id, envelopes);
        }
        true
            */
    }

    /* Update thread tree information on envelope insertion */
    fn rebuild_thread(&mut self, id: ThreadHash, envelopes: &Envelopes) {
        let mut node_idx = id;
        let mut stack = StackVec::new();

        {
            let tree = self.tree.get_mut();
            for &c in &self.thread_nodes[&id].children {
                if let Some(pos) = tree.iter().position(|t| t.id == c) {
                    tree.remove(pos);
                }
            }
        }
        let no_parent: bool = if let Some(node) = self.thread_nodes.get(&node_idx) {
            if let (None, None, 0) = (node.parent, node.message, node.children.len()) {
                return;
            }
            node.parent.is_none()
        } else {
            panic!(format!(
                "node_idx = {:?} not found in self.thread_nodes",
                node_idx
            ));
        };

        if no_parent {
            let tree = self.tree.get_mut();
            if let Some(pos) = tree.iter().position(|t| t.id == id) {
                tree[pos] = ThreadTree::new(id);
                node_build(
                    &mut tree[pos],
                    id,
                    *(self.sort.borrow()),
                    &mut self.thread_nodes,
                    1,
                    envelopes,
                );
                return;
            } else {
                for &c in &self.thread_nodes[&id].children {
                    if let Some(pos) = tree.iter().position(|t| t.id == c) {
                        tree.remove(pos);
                    }
                }
            }
            let mut new_tree = ThreadTree::new(id);
            node_build(
                &mut new_tree,
                id,
                *(self.sort.borrow()),
                &mut self.thread_nodes,
                1,
                envelopes,
            );
            ThreadTree::insert_child(
                tree,
                new_tree,
                *(self.sort.borrow()),
                &self.thread_nodes,
                envelopes,
            );
            return;
        }

        /* Trace path back to root ThreadNode */
        while let Some(p) = &self.thread_nodes[&node_idx].parent {
            node_idx = *p;
            stack.push(node_idx);
        }

        {
            /* Trace path from root ThreadTree to the envelope's parent */
            let mut tree = self.tree.get_mut();
            for &s in stack.iter().rev() {
                /* Borrow checker is being a tad silly here, so the following
                 * is basically this:
                 *
                 *  let tree = &mut tree[s].children;
                 */
                let temp_tree = tree;
                if let Some(pos) = temp_tree.iter().position(|v| v.id == s) {
                    tree = &mut temp_tree[pos].children;
                } else {
                    let mut tree_node = ThreadTree::new(s);
                    node_build(
                        &mut tree_node,
                        s,
                        *(self.sort.borrow()),
                        &mut self.thread_nodes,
                        1,
                        envelopes,
                    );
                    let new_id = ThreadTree::insert_child(
                        temp_tree,
                        tree_node,
                        *(self.sort.borrow()),
                        &self.thread_nodes,
                        envelopes,
                    );
                    tree = &mut temp_tree[new_id].children;
                }
            }
            let pos = if let Some(pos) = tree.iter().position(|v| v.id == id) {
                pos
            } else {
                /* Add new child */
                let tree_node = ThreadTree::new(id);
                ThreadTree::insert_child(
                    tree,
                    tree_node,
                    *(self.sort.borrow()),
                    &self.thread_nodes,
                    envelopes,
                )
            };
            node_build(
                &mut tree[pos],
                id,
                *(self.sort.borrow()),
                &mut self.thread_nodes,
                1,
                envelopes,
            );
        }
    }

    /*
     * Finalize instance by building the thread tree, set show subject and thread lengths etc. */
    fn build_envelopes(&mut self, envelopes: &Envelopes) {
        {
            let tree = self.tree.get_mut();
            tree.clear();
            for i in self.root_set.borrow().iter() {
                let mut tree_node = ThreadTree::new(*i);
                node_build(
                    &mut tree_node,
                    *i,
                    *(self.sort.borrow()),
                    &mut self.thread_nodes,
                    0, /* indentation */
                    envelopes,
                );
                ThreadTree::insert_child(
                    tree,
                    tree_node,
                    *(self.sort.borrow()),
                    &self.thread_nodes,
                    envelopes,
                );
            }
        }
        self.inner_sort_by(*self.sort.borrow(), envelopes);
        self.inner_subsort_by(*self.subsort.borrow(), envelopes);
    }

    fn inner_subsort_by(&self, subsort: (SortField, SortOrder), envelopes: &Envelopes) {
        let tree = &mut self.tree.borrow_mut();
        for t in tree.iter_mut() {
            t.children.sort_by(|a, b| match subsort {
                (SortField::Date, SortOrder::Desc) => {
                    let a = &self.thread_nodes[&a.id];
                    let b = &self.thread_nodes[&b.id];
                    b.date.cmp(&a.date)
                }
                (SortField::Date, SortOrder::Asc) => {
                    let a = &self.thread_nodes[&a.id];
                    let b = &self.thread_nodes[&b.id];
                    a.date.cmp(&b.date)
                }
                (SortField::Subject, SortOrder::Desc) => {
                    let a = &self.thread_nodes[&a.id].message();
                    let b = &self.thread_nodes[&b.id].message();

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
                    let a = &self.thread_nodes[&a.id].message();
                    let b = &self.thread_nodes[&b.id].message();

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
    }

    fn inner_sort_by(&self, sort: (SortField, SortOrder), envelopes: &Envelopes) {
        let tree = &mut self.tree.borrow_mut();
        tree.sort_by(|a, b| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let a = &self.thread_nodes[&a.id];
                let b = &self.thread_nodes[&b.id];
                b.date.cmp(&a.date)
            }
            (SortField::Date, SortOrder::Asc) => {
                let a = &self.thread_nodes[&a.id];
                let b = &self.thread_nodes[&b.id];
                a.date.cmp(&b.date)
            }
            (SortField::Subject, SortOrder::Desc) => {
                let a = &self.thread_nodes[&a.id].message();
                let b = &self.thread_nodes[&b.id].message();

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
                ma.subject()
                    .split_graphemes()
                    .cmp(&mb.subject().split_graphemes())
            }
            (SortField::Subject, SortOrder::Asc) => {
                let a = &self.thread_nodes[&a.id].message();
                let b = &self.thread_nodes[&b.id].message();

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
                mb.subject()
                    .as_ref()
                    .split_graphemes()
                    .cmp(&ma.subject().split_graphemes())
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
        self.tree.borrow().len()
    }

    pub fn root_set(&self, idx: usize) -> ThreadHash {
        self.tree.borrow()[idx].id
    }

    pub fn root_iter(&self) -> RootIterator {
        self.prune_tree();
        RootIterator {
            pos: 0,
            root_tree: self.tree.borrow(),
        }
    }

    pub fn has_sibling(&self, h: ThreadHash) -> bool {
        if let Some(parent) = self[&h].parent {
            let children = &self[&parent].children;
            if children.is_empty() {
                return false;
            }
            let pos = children
                .iter()
                .position(|&x| x == h)
                .expect("Did not find node in parent!");
            pos != children.len() - 1
        } else {
            false
        }
    }

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
        let mut tree = self.tree.borrow_mut();
        let mut i = 0;
        while i < tree.len() {
            // Evaluate if useless
            let node = &self.thread_nodes[&tree[i].id];
            if let (None, None, 0) = (node.parent, node.message, node.children.len()) {
                tree.remove(i);
                continue;
            }
            i += 1;
        }
    }

    fn link_threads(&mut self, envelopes: &mut Envelopes) {
        for e in envelopes.values_mut() {
            self.link_envelope(e);
        }
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

fn node_build(
    tree: &mut ThreadTree,
    idx: ThreadHash,
    sort: (SortField, SortOrder),
    thread_nodes: &mut FnvHashMap<ThreadHash, ThreadNode>,
    indentation: usize,
    envelopes: &Envelopes,
) {
    if let Some(hash) = thread_nodes[&idx].message {
        if !envelopes.contains_key(&hash) {
            /* invalidate node */
            //        thread_nodes[&idx].message = None;
        } else if let Some(parent_id) = thread_nodes[&idx].parent {
            if let Some(parent_hash) = thread_nodes[&parent_id].message {
                if !envelopes.contains_key(&parent_hash) {
                    /* invalidate node */
                    //               thread_nodes[&parent_id].message = None;
                } else {
                    /* decide if the subject should be shown in the UI.
                     * If parent subject is Foobar and reply is `Re: Foobar`
                     * then showing the reply's subject can be reduntant
                     */
                    let mut subject = envelopes[&hash].subject();
                    let mut subject = subject.to_mut().as_bytes();
                    subject.strip_prefixes();
                    let mut parent_subject = envelopes[&parent_hash].subject();
                    let mut parent_subject = parent_subject.to_mut().as_bytes();
                    parent_subject.strip_prefixes();
                    if subject == parent_subject {
                        thread_nodes.entry(idx).and_modify(|e| {
                            e.show_subject = false;
                        });
                    }
                }
            }
        }
    } else if let Some(node) = thread_nodes.get(&idx) {
        if let (None, None, 0) = (node.parent, node.message, node.children.len()) {
            return;
        }
    }

    let indentation = if thread_nodes[&idx].has_message() {
        thread_nodes
            .entry(idx)
            .and_modify(|e| e.indentation = indentation);
        indentation + 1
    } else if indentation > 0 {
        indentation
    } else {
        indentation + 1
    };

    let mut has_unseen = if let Some(msg) = thread_nodes[&idx].message {
        !envelopes[&msg].is_seen()
    } else {
        false
    };

    thread_nodes.entry(idx).and_modify(|e| {
        e.len = e.children.len();
        e.indentation = indentation
    });

    let mut child_vec: Vec<ThreadTree> = Vec::new();
    /* No child/parent relationship is mutated at any point and no nodes are added or removed. Only
     * each node's fields change, so the following is safe.
     */
    let children = &thread_nodes[&idx].children as *const Vec<ThreadHash>;
    for &c in unsafe { &(*children) } {
        let mut new_tree = ThreadTree::new(c);
        node_build(
            &mut new_tree,
            c,
            sort,
            thread_nodes,
            indentation + 1,
            envelopes,
        );
        let _c = (thread_nodes[&c].len, thread_nodes[&c].date);
        thread_nodes.entry(idx).and_modify(|e| {
            e.len += _c.0;
            e.date = cmp::max(e.date, _c.1);
        });

        has_unseen |= thread_nodes[&c].has_unseen;
        ThreadTree::insert_child(&mut child_vec, new_tree, sort, thread_nodes, envelopes);
    }
    tree.children = child_vec;
    thread_nodes.entry(idx).and_modify(|e| {
        e.has_unseen = has_unseen;
    });
}

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
