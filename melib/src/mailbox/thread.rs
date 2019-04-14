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

use crate::mailbox::email::parser::BytesExt;
use crate::mailbox::email::*;

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

type Envelopes = FnvHashMap<EnvelopeHash, Envelope>;

/* Helper macros to avoid repeating ourselves */

fn rec_change_root_parent(b: &mut Vec<ThreadNode>, idx: usize, new_root: usize) {
    b[idx].thread_group = new_root;
    if let Some(p) = b[idx].parent {
        rec_change_children(b, p, new_root);
        rec_change_root_parent(b, p, new_root);
    }
}
fn rec_change_children(b: &mut Vec<ThreadNode>, idx: usize, new_root: usize) {
    b[idx].thread_group = new_root;

    for c in b[idx].children.clone() {
        rec_change_children(b, c, new_root);
    }
}

macro_rules! remove_from_parent {
    ($buf:expr, $idx:expr) => {
        if let Some(p) = $buf[$idx].parent {
            if let Some(pos) = $buf[p].children.iter().position(|c| *c == $idx) {
                $buf[p].children.remove(pos);
            }
            rec_change_root_parent($buf, p, p);
        }
        $buf[$idx].parent = None;
        rec_change_children($buf, $idx, $idx);
        $buf[$idx].thread_group = $idx;
    };
}

macro_rules! make {
    (($p:expr)parent of($c:expr), $buf:expr) => {
        remove_from_parent!($buf, $c);
        if !($buf[$p]).children.contains(&$c) {
            $buf[$p].children.push($c);
        } else {
            panic!();
        }
        $buf[$c].parent = Some($p);
        union($buf, $c, $p);
    };
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

#[derive(Clone, Deserialize, Serialize)]
struct ThreadTree {
    id: usize,
    children: Vec<ThreadTree>,
}

impl fmt::Debug for ThreadTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "")
    }
}

impl ThreadTree {
    fn new(id: usize) -> Self {
        ThreadTree {
            id,
            children: Vec::new(),
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
    stack: Vec<usize>,
    tree: Ref<'a, Vec<ThreadTree>>,
}
impl<'a> Iterator for ThreadsIterator<'a> {
    type Item = (usize, usize);
    fn next(&mut self) -> Option<(usize, usize)> {
        {
            let mut tree = &(*self.tree);
            for i in &self.stack {
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
    stack: Vec<usize>,
    tree: Ref<'a, Vec<ThreadTree>>,
}
impl<'a> Iterator for ThreadIterator<'a> {
    type Item = (usize, usize);
    fn next(&mut self) -> Option<(usize, usize)> {
        {
            let mut tree = &(*self.tree);
            for i in &self.stack {
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
    parent: Option<usize>,
    children: Vec<usize>,
    date: UnixTimestamp,
    indentation: usize,
    show_subject: bool,

    len: usize,
    has_unseen: bool,

    /* Union/Find set fields */
    thread_group: usize,
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

            len: 0,
            has_unseen: false,
            thread_group: 0,
            rank: 0,
        }
    }
}

impl ThreadNode {
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
        self.len == 0
    }

    pub fn message(&self) -> Option<EnvelopeHash> {
        self.message
    }

    pub fn has_message(&self) -> bool {
        self.message.is_some()
    }

    pub fn parent(&self) -> Option<usize> {
        self.parent
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn children(&self) -> &[usize] {
        &self.children
    }

    pub fn indentation(&self) -> usize {
        self.indentation
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Threads {
    thread_nodes: Vec<ThreadNode>,
    root_set: RefCell<Vec<usize>>,
    tree: RefCell<Vec<ThreadTree>>,

    message_ids: FnvHashMap<Vec<u8>, usize>,
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
    type Item = usize;
    fn next(&mut self) -> Option<usize> {
        {
            if self.pos == self.root_tree.len() {
                return None;
            }
            self.pos += 1;
            Some(self.root_tree[self.pos - 1].id)
        }
    }
}
fn find(buf: &mut Vec<ThreadNode>, i: usize) -> usize {
    if buf[i].thread_group == i {
        return i;
    }
    let p = buf[i].thread_group;
    buf[i].thread_group = find(buf, p);
    buf[i].thread_group
}
fn union(buf: &mut Vec<ThreadNode>, x: usize, y: usize) -> usize {
    let mut x_root = find(buf, x);
    let mut y_root = find(buf, y);

    // x and y are already in the same set
    if x_root == y_root {
        return x_root;
    }

    if buf[x_root].rank < buf[y_root].rank {
        mem::swap(&mut x_root, &mut y_root);
    }

    // x and y are not in same set, so we merge them
    //
    buf[y_root].thread_group = x_root;
    if buf[x_root].rank == buf[y_root].rank {
        buf[x_root].rank += 1;
    }
    x_root
}

impl Threads {
    fn find(&mut self, i: usize) -> usize {
        find(&mut self.thread_nodes, i)
    }
    fn union(&mut self, x: usize, y: usize) -> usize {
        let mut x_root = self.find(x);
        let mut y_root = self.find(y);

        // x and y are already in the same set
        if x_root == y_root {
            return x_root;
        }

        if self.thread_nodes[x_root].rank < self.thread_nodes[y_root].rank {
            mem::swap(&mut x_root, &mut y_root);
        }

        // x and y are not in same set, so we merge them
        //
        self.thread_nodes[y_root].thread_group = x_root;
        if self.thread_nodes[x_root].rank == self.thread_nodes[y_root].rank {
            self.thread_nodes[x_root].rank += 1;
        }
        x_root
    }
    fn prune_empty_nodes(&mut self, root_set: &mut Vec<usize>) {
        fn prune(
            thread_nodes: &mut Vec<ThreadNode>,
            idx: usize,
            root_set: &mut Vec<usize>,
        ) -> bool {
            /* "If it is an empty container with no children, nuke it." */
            if !thread_nodes[idx].has_message() && thread_nodes[idx].children.is_empty() {
                remove_from_parent!(thread_nodes, idx);
                return true;
            }

            if !thread_nodes[idx].has_message() {
                if thread_nodes[idx].children.len() == 1 {
                    /* "Do not promote the children if doing so would promote them to the root set
                     * -- unless there is only one child, in which case, do." */
                    let child = thread_nodes[idx].children[0];
                    root_set.push(child);
                    thread_nodes[idx].children.clear();
                    remove_from_parent!(thread_nodes, idx);
                    remove_from_parent!(thread_nodes, child);
                    return true; // Pruned
                } else if let Some(p) = thread_nodes[idx].parent {
                    let orphans = thread_nodes[idx].children.clone();
                    for c in orphans {
                        make!((p) parent of (c), thread_nodes);
                    }
                    remove_from_parent!(thread_nodes, idx);
                    thread_nodes[idx].children.clear();
                    return true; // Pruned
                }
            }

            /*  Recurse to children, but keep in mind more children can be added in each iteration
             */
            let mut c_idx = 0;
            loop {
                if c_idx == thread_nodes[idx].children.len() {
                    break;
                }
                let c = thread_nodes[idx].children[c_idx];
                if !prune(thread_nodes, c, root_set) {
                    c_idx += 1;
                }
            }
            !thread_nodes[idx].has_message() && thread_nodes[idx].children.is_empty()
        }

        let mut idx = 0;
        loop {
            if idx == root_set.len() {
                break;
            }
            if prune(&mut self.thread_nodes, root_set[idx], root_set) {
                root_set.remove(idx);
            } else {
                idx += 1;
            }
        }
    }

    pub fn new(collection: &mut Envelopes) -> Threads {
        /* To reconstruct thread information from the mails we need: */

        /* a vector to hold thread members */
        let thread_nodes: Vec<ThreadNode> =
            Vec::with_capacity((collection.len() as f64 * 1.2) as usize);
        /* A hash table of Message IDs */
        let message_ids: FnvHashMap<Vec<u8>, usize> =
            FnvHashMap::with_capacity_and_hasher(collection.len(), Default::default());
        let hash_set: FnvHashSet<EnvelopeHash> =
            FnvHashSet::with_capacity_and_hasher(collection.len(), Default::default());

        let mut t = Threads {
            thread_nodes,
            message_ids,
            hash_set,
            subsort: RefCell::new((SortField::Subject, SortOrder::Desc)),

            ..Default::default()
        };
        /* Add each message to message_ids and threads, and link them together according to the
         * References / In-Reply-To headers */
        t.link_threads(collection);

        t.create_root_set(collection);
        t.build_collection(collection);
        //if cfg!(debug_assertions) {
        //    for (i, _t) in t.thread_nodes.iter().enumerate() {
        //        eprint!("{}:{}_{}:	", file!(), line!(), column!());
        // eprintln!("Thread #{}, children {}", i, _t.children.len());
        //        if !_t.children.is_empty() {
        //            eprint!("{}:{}_{}:	", file!(), line!(), column!());
        //   eprintln!("{:?}", _t.children);
        //        }
        //        if let Some(m) = _t.message {
        //            eprint!("{}:{}_{}:	", file!(), line!(), column!());
        //  eprintln!("\tmessage: {}", collection[&m].subject());
        //        } else {
        //            eprint!("{}:{}_{}:	", file!(), line!(), column!());
        //  eprintln!("\tNo message");
        //        }
        //    }
        //    eprint!("{}:{}_{}:	", file!(), line!(), column!());
        //eprintln!("\n");
        //    for (i, _t) in t.tree.borrow().iter().enumerate() {
        //        eprint!("{}:{}_{}:	", file!(), line!(), column!());
        //eprintln!("Tree #{} id {}, children {}", i, _t.id, _t.children.len());
        //        if let Some(m) = t.thread_nodes[_t.id].message {
        //            eprint!("{}:{}_{}:	", file!(), line!(), column!());
        //eprintln!("\tmessage: {}", collection[&m].subject());
        //        } else {
        //            eprint!("{}:{}_{}:	", file!(), line!(), column!());
        //eprintln!("\tNo message");
        //        }
        //    }
        //}
        t
    }

    fn create_root_set(&mut self, collection: &Envelopes) {
        /* Walk over the elements of message_ids, and gather a list of the ThreadNode objects that
         * have no parents. These are the root messages of each thread */
        let mut root_set: Vec<usize> = Vec::with_capacity(collection.len());

        /* Find the root set */
        for v in self.message_ids.values() {
            if self.thread_nodes[*v].parent.is_none() {
                root_set.push(*v);
            }
        }

        let mut roots_to_remove: Vec<usize> = Vec::with_capacity(root_set.len());
        /* Prune empty thread nodes */
        self.prune_empty_nodes(&mut root_set);

        /* "Group root set by subject."
         *
         * "If any two members of the root set have the same subject, merge them. This is so that
         * messages which don't have References headers at all still get threaded (to the extent
         * possible, at least.)"
         */
        let mut subject_table: FnvHashMap<Vec<u8>, (bool, usize)> =
            FnvHashMap::with_capacity_and_hasher(collection.len(), Default::default());

        for &r in root_set.iter() {
            /* "Find the subject of that sub-tree": */
            let (mut subject, mut is_re): (_, bool) = if self.thread_nodes[r].message.is_some() {
                /* "If there is a message in the Container, the subject is the subject of that
                 * message. " */
                let msg_idx = self.thread_nodes[r].message.unwrap();
                let envelope = &collection[&msg_idx];
                (envelope.subject(), !envelope.references().is_empty())
            } else {
                /* "If there is no message in the Container, then the Container will have at least
                 * one child Container, and that Container will have a message. Use the subject of
                 * that message instead." */
                let msg_idx = self.thread_nodes[self.thread_nodes[r].children[0]]
                    .message
                    .unwrap();
                let envelope = &collection[&msg_idx];
                (envelope.subject(), !envelope.references().is_empty())
            };

            /* "Strip ``Re:'', ``RE:'', ``RE[5]:'', ``Re: Re[4]: Re:'' and so on." */
            /* References of this envelope can be empty but if the subject contains a ``Re:``
             * prefix, it's a reply */
            let mut stripped_subj = subject.to_mut().as_bytes();
            is_re |= stripped_subj.is_a_reply();
            stripped_subj.strip_prefixes();

            if stripped_subj.is_empty() {
                continue;
            }

            /* "Add this Container to the subject_table if:" */
            if subject_table.contains_key(stripped_subj) {
                let (other_is_re, id) = subject_table[stripped_subj];
                /* "This one is an empty container and the old one is not: the empty one is more
                 * interesting as a root, so put it in the table instead."
                 * or
                 * "The container in the table has a ``Re:'' version of this subject, and this
                 * container has a non-``Re:'' version of this subject. The non-re version is the
                 * more interesting of the two." */
                if (!self.thread_nodes[id].has_message() && self.thread_nodes[r].has_message())
                    || (other_is_re && !is_re)
                {
                    mem::replace(
                        subject_table.entry(stripped_subj.to_vec()).or_default(),
                        (is_re, r),
                    );
                }
            } else {
                /* "There is no container in the table with this subject" */
                subject_table.insert(stripped_subj.to_vec(), (is_re, r));
            }
        }

        /* "Now the subject_table is populated with one entry for each subject which occurs in the
         * root set. Now iterate over the root set, and gather together the difference." */
        for i in 0..root_set.len() {
            let r = root_set[i];

            /* "Find the subject of this Container (as above.)" */
            let (mut subject, mut is_re): (_, bool) = if self.thread_nodes[r].message.is_some() {
                let msg_idx = self.thread_nodes[r].message.unwrap();
                let envelope = &collection[&msg_idx];
                (envelope.subject(), !envelope.references().is_empty())
            } else {
                let msg_idx = self.thread_nodes[self.thread_nodes[r].children[0]]
                    .message
                    .unwrap();
                let envelope = &collection[&msg_idx];
                (envelope.subject(), !envelope.references().is_empty())
            };

            let mut subject = subject.to_mut().as_bytes();
            is_re |= subject.is_a_reply();
            subject.strip_prefixes();
            if subject.is_empty() {
                continue;
            }

            let (other_is_re, other_idx) = subject_table[subject];
            /* "If it is null, or if it is this container, continue." */
            if !self.thread_nodes[other_idx].has_message() || other_idx == r {
                continue;
            }

            /* "Otherwise, we want to group together this Container and the one in the table. There
             * are a few possibilities:" */

            /*
             * "If both are dummies, append one's children to the other, and remove the now-empty
             * container."
             */
            if !self.thread_nodes[r].has_message() && !self.thread_nodes[other_idx].has_message() {
                let children = self.thread_nodes[r].children.clone();
                for c in children {
                    make!((other_idx) parent of (c), &mut self.thread_nodes);
                }
                roots_to_remove.push(i);

            /* "If one container is a empty and the other is not, make the non-empty one be a child
             * of the empty, and a sibling of the other ``real'' messages with the same subject
             * (the empty's children.)"
             */
            } else if self.thread_nodes[r].has_message()
                && !self.thread_nodes[other_idx].has_message()
            {
                make!((other_idx) parent of (r), &mut self.thread_nodes);
                if !root_set.contains(&other_idx) {
                    root_set.push(other_idx);
                }
                roots_to_remove.push(i);
            } else if !self.thread_nodes[r].has_message()
                && self.thread_nodes[other_idx].has_message()
            {
                make!((r) parent of (other_idx), &mut self.thread_nodes);
                if let Some(pos) = root_set.iter().position(|&i| i == other_idx) {
                    roots_to_remove.push(pos);
                }
            /*
             * "If that container is a non-empty, and that message's subject does not begin with ``Re:'', but this
             * message's subject does, then make this be a child of the other."
             */
            } else if self.thread_nodes[other_idx].has_message() && !other_is_re && is_re {
                make!((other_idx) parent of (r), &mut self.thread_nodes);
                roots_to_remove.push(i);

            /* "If that container is a non-empty, and that message's subject begins with ``Re:'', but this
             * message's subject does not, then make that be a child of this one -- they were misordered. (This
             * happens somewhat implicitly, since if there are two messages, one with Re: and one without, the one
             * without will be in the hash table, regardless of the order in which they were
             * seen.)"
             */
            } else if self.thread_nodes[other_idx].has_message() && other_is_re && !is_re {
                make!((r) parent of (other_idx), &mut self.thread_nodes);
                if let Some(pos) = root_set.iter().position(|r| *r == other_idx) {
                    roots_to_remove.push(pos);
                }

            /* "Otherwise, make a new empty container and make both msgs be a child of it. This catches the
             * both-are-replies and neither-are-replies cases, and makes them be siblings instead of asserting a
             * hierarchical relationship which might not be true."
             */
            } else {
                self.thread_nodes.push(Default::default());
                let new_id = self.thread_nodes.len() - 1;
                self.thread_nodes[new_id].thread_group = new_id;
                make!((new_id) parent of (r), &mut self.thread_nodes);
                make!((new_id) parent of (other_idx), &mut self.thread_nodes);
                root_set[i] = new_id;
                if let Some(pos) = root_set.iter().position(|r| *r == other_idx) {
                    roots_to_remove.push(pos);
                }
            }
        }
        roots_to_remove.sort_unstable();
        roots_to_remove.dedup();
        for r in roots_to_remove.into_iter().rev() {
            root_set.remove(r);
        }

        self.root_set = RefCell::new(root_set);
    }

    pub fn threads_iter(&self) -> ThreadsIterator {
        ThreadsIterator {
            pos: 0,
            stack: Vec::with_capacity(4),
            tree: self.tree.borrow(),
        }
    }

    pub fn thread_iter(&self, index: usize) -> ThreadIterator {
        ThreadIterator {
            init_pos: index,
            pos: index,
            stack: Vec::with_capacity(4),
            tree: self.tree.borrow(),
        }
    }

    pub fn update_envelope(
        &mut self,
        old_hash: EnvelopeHash,
        new_hash: EnvelopeHash,
        collection: &Envelopes,
    ) -> Result<(), ()> {
        /* must update:
         * - hash_set
         * - message fields in thread_nodes
         */
        let idx = if let Some((idx, node)) = self
            .thread_nodes
            .iter_mut()
            .enumerate()
            .find(|(_, n)| n.message.map(|n| n == old_hash).unwrap_or(false))
        {
            node.message = Some(new_hash);
            idx
        } else {
            return Err(());
        };
        self.hash_set.remove(&old_hash);
        self.hash_set.insert(new_hash);
        self.rebuild_thread(idx, collection);
        Ok(())
    }

    #[inline]
    pub fn remove(&mut self, envelope_hash: EnvelopeHash, collection: &mut Envelopes) {
        self.hash_set.remove(&envelope_hash);
        //{
        //    let pos = self
        //        .thread_nodes
        //        .iter()
        //        .position(|n| n.message.map(|n| n == envelope_hash).unwrap_or(false))
        //        .unwrap();
        //    if cfg!(debug_assertions) {
        // eprint!("{}:{}_{}:	", file!(), line!(), column!());
        // eprintln!("DEBUG: {} in threads is idx= {}", envelope_hash, pos);
        // }
        //}

        let t_id: usize;
        {
            if let Some(pos) = self
                .thread_nodes
                .iter()
                .position(|n| n.message.map(|n| n == envelope_hash).unwrap_or(false))
            {
                t_id = pos;
                self.thread_nodes[pos].message = None;
            } else {
                /* else it was deleted during a thread_rebuild or others */
                return;
            }
        }

        let mut node_idx = t_id;

        /* Trace path back to root ThreadNode */
        while let Some(p) = &self.thread_nodes[node_idx].parent {
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
                        &mut self.thread_nodes,
                        1,
                        collection,
                    );
                }
            }
        }

        let mut root_set: Vec<usize> = self.tree.borrow().iter().map(|t| t.id).collect();
        self.prune_empty_nodes(&mut root_set);
        self.tree.borrow_mut().retain(|t| root_set.contains(&t.id));
    }

    pub fn amend(&mut self, collection: &mut Envelopes) {
        let new_hash_set = FnvHashSet::from_iter(collection.keys().cloned());

        let difference: Vec<EnvelopeHash> =
            self.hash_set.difference(&new_hash_set).cloned().collect();
        for h in difference {
            self.remove(h, collection);
        }

        let difference: Vec<EnvelopeHash> =
            new_hash_set.difference(&self.hash_set).cloned().collect();
        for h in difference {
            if cfg!(debug_assertions) {
                eprint!("{}:{}_{}:	", file!(), line!(), column!());
                eprintln!("inserting {}", collection[&h].subject());
            }
            let env = collection.entry(h).or_default() as *mut Envelope;
            unsafe {
                // `collection` is borrowed immutably and `insert` only changes the envelope's
                // `thread` field.
                self.insert(&mut (*env), collection);
            }
        }
        self.create_root_set(collection);

        let mut root_set: Vec<usize> = self.tree.borrow().iter().map(|t| t.id).collect();
        self.prune_empty_nodes(&mut root_set);
        let tree = self.tree.get_mut();
        tree.retain(|t| root_set.contains(&t.id));
    }

    pub fn insert(&mut self, envelope: &mut Envelope, collection: &Envelopes) {
        self.link_envelope(envelope);
        {
            let id = self.message_ids[envelope.message_id().raw()];
            self.rebuild_thread(id, collection);
        }
    }

    pub fn insert_reply(&mut self, envelope: &Envelope, collection: &mut Envelopes) -> bool {
        //return false;
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
        collection.insert(hash, envelope.clone());
        {
            let envelope = collection.entry(hash).or_default() as *mut Envelope;
            unsafe {
                /* Safe because insert only changes envelope's fields and nothing more */
                self.insert(&mut (*envelope), &collection);
            }
        }
        let envelope: &Envelope = &collection[&hash];
        {
            let in_reply_to = envelope.in_reply_to().unwrap().raw();
            let parent_id = self.message_ids[in_reply_to];
            self.rebuild_thread(parent_id, collection);
        }
        true
    }

    /* Update thread tree information on envelope insertion */
    fn rebuild_thread(&mut self, id: usize, collection: &Envelopes) {
        let mut node_idx = id;
        let mut stack = Vec::with_capacity(32);

        let no_parent: bool = if let Some(node) = self.thread_nodes.get(node_idx) {
            node.parent.is_none()
        } else {
            false
        };

        if no_parent {
            let tree = self.tree.get_mut();
            if let Some(tree) = tree.iter_mut().find(|t| t.id == id) {
                *tree = ThreadTree::new(id);
                node_build(tree, id, &mut self.thread_nodes, 1, collection);
                return;
            }
            tree.push(ThreadTree::new(id));
        }

        /* Trace path back to root ThreadNode */
        while let Some(p) = &self.thread_nodes[node_idx].parent {
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
                    let tree_node = ThreadTree::new(s);
                    temp_tree.push(tree_node);
                    let new_id = temp_tree.len() - 1;
                    tree = &mut temp_tree[new_id].children;
                }
            }
            let pos = if let Some(pos) = tree.iter().position(|v| v.id == id) {
                pos
            } else {
                /* Add new child */
                let tree_node = ThreadTree::new(id);
                tree.push(tree_node);
                tree.len() - 1
            };
            node_build(&mut tree[pos], id, &mut self.thread_nodes, 1, collection);
        }
        // FIXME: use insertion according to self.sort etc instead of sorting everytime
        self.inner_sort_by(*self.sort.borrow(), collection);
        self.inner_subsort_by(*self.subsort.borrow(), collection);
    }

    /*
     * Finalize instance by building the thread tree, set show subject and thread lengths etc. */
    fn build_collection(&mut self, collection: &Envelopes) {
        {
            let tree = self.tree.get_mut();
            tree.clear();
            for i in self.root_set.borrow().iter() {
                let mut tree_node = ThreadTree::new(*i);
                node_build(
                    &mut tree_node,
                    *i,
                    &mut self.thread_nodes,
                    0, /* indentation */
                    collection,
                );
                tree.push(tree_node);
            }
        }
        self.inner_sort_by(*self.sort.borrow(), collection);
        self.inner_subsort_by(*self.subsort.borrow(), collection);
    }

    fn inner_subsort_by(&self, subsort: (SortField, SortOrder), collection: &Envelopes) {
        let tree = &mut self.tree.borrow_mut();
        for t in tree.iter_mut() {
            t.children.sort_by(|a, b| match subsort {
                (SortField::Date, SortOrder::Desc) => {
                    let a = &self.thread_nodes[a.id];
                    let b = &self.thread_nodes[b.id];
                    b.date.cmp(&a.date)
                }
                (SortField::Date, SortOrder::Asc) => {
                    let a = &self.thread_nodes[a.id];
                    let b = &self.thread_nodes[b.id];
                    a.date.cmp(&b.date)
                }
                (SortField::Subject, SortOrder::Desc) => {
                    let a = &self.thread_nodes[a.id].message();
                    let b = &self.thread_nodes[b.id].message();

                    if a.is_none() || b.is_none() {
                        return Ordering::Equal;
                    }
                    let ma = &collection[&a.unwrap()];
                    let mb = &collection[&b.unwrap()];
                    ma.subject().cmp(&mb.subject())
                }
                (SortField::Subject, SortOrder::Asc) => {
                    let a = &self.thread_nodes[a.id].message();
                    let b = &self.thread_nodes[b.id].message();

                    if a.is_none() || b.is_none() {
                        return Ordering::Equal;
                    }
                    let ma = &collection[&a.unwrap()];
                    let mb = &collection[&b.unwrap()];
                    mb.subject().cmp(&ma.subject())
                }
            });
        }
    }

    fn inner_sort_by(&self, sort: (SortField, SortOrder), collection: &Envelopes) {
        let tree = &mut self.tree.borrow_mut();
        tree.sort_by(|a, b| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let a = &self.thread_nodes[a.id];
                let b = &self.thread_nodes[b.id];
                b.date.cmp(&a.date)
            }
            (SortField::Date, SortOrder::Asc) => {
                let a = &self.thread_nodes[a.id];
                let b = &self.thread_nodes[b.id];
                a.date.cmp(&b.date)
            }
            (SortField::Subject, SortOrder::Desc) => {
                let a = &self.thread_nodes[a.id].message();
                let b = &self.thread_nodes[b.id].message();

                if a.is_none() || b.is_none() {
                    return Ordering::Equal;
                }
                let ma = &collection[&a.unwrap()];
                let mb = &collection[&b.unwrap()];
                ma.subject().cmp(&mb.subject())
            }
            (SortField::Subject, SortOrder::Asc) => {
                let a = &self.thread_nodes[a.id].message();
                let b = &self.thread_nodes[b.id].message();

                if a.is_none() || b.is_none() {
                    return Ordering::Equal;
                }
                let ma = &collection[&a.unwrap()];
                let mb = &collection[&b.unwrap()];
                mb.subject().cmp(&ma.subject())
            }
        });
    }

    pub fn sort_by(
        &self,
        sort: (SortField, SortOrder),
        subsort: (SortField, SortOrder),
        collection: &Envelopes,
    ) {
        if *self.sort.borrow() != sort {
            self.inner_sort_by(sort, collection);
            *self.sort.borrow_mut() = sort;
        }
        if *self.subsort.borrow() != subsort {
            self.inner_subsort_by(subsort, collection);
            *self.subsort.borrow_mut() = subsort;
        }
    }

    pub fn thread_to_mail(&self, i: usize) -> EnvelopeHash {
        let thread = &self.thread_nodes[i];
        thread.message().unwrap()
    }

    pub fn thread_nodes(&self) -> &Vec<ThreadNode> {
        &self.thread_nodes
    }

    pub fn len(&self) -> usize {
        self.hash_set.len()
    }

    pub fn root_len(&self) -> usize {
        self.tree.borrow().len()
    }

    pub fn root_set(&self, idx: usize) -> usize {
        self.tree.borrow()[idx].id
    }

    pub fn root_iter(&self) -> RootIterator {
        RootIterator {
            pos: 0,
            root_tree: self.tree.borrow(),
        }
    }

    pub fn has_sibling(&self, i: usize) -> bool {
        if let Some(parent) = self[i].parent {
            self[parent].children.len() > 1
        } else {
            false
        }
    }

    fn link_envelope(&mut self, envelope: &mut Envelope) {
        let t_idx: usize = {
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
                if self.thread_nodes[node_idx].message.is_some() {
                    return;
                }
                node_idx
            } else {
                /* Create a new ThreadNode object holding this message */
                self.thread_nodes.push(ThreadNode {
                    message: Some(envelope.hash()),
                    date: envelope.date(),
                    ..Default::default()
                });
                /* The new thread node's set is just itself */
                let new_id = self.thread_nodes.len() - 1;
                self.thread_nodes[new_id].thread_group = new_id;

                self.message_ids.insert(m_id.to_vec(), new_id);
                new_id
            }
        };
        self.thread_nodes[t_idx].date = envelope.date();
        self.thread_nodes[t_idx].message = Some(envelope.hash());
        self.thread_nodes[t_idx].has_unseen |= !envelope.is_seen();
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
        if self.thread_nodes[t_idx].has_parent() {
            remove_from_parent!(&mut self.thread_nodes, t_idx);
        }

        for &refn in envelope.references().iter().rev() {
            let r_id = refn.raw();
            let parent_id = if self.message_ids.contains_key(r_id) {
                self.message_ids[r_id]
            } else {
                /* Create a new ThreadNode object holding this reference */
                self.thread_nodes.push(ThreadNode {
                    ..Default::default()
                });
                let new_id = self.thread_nodes.len() - 1;
                self.thread_nodes[new_id].thread_group = new_id;
                self.message_ids.insert(r_id.to_vec(), new_id);
                new_id
            };
            /* If they are already linked, don't change the existing links. */
            if self.thread_nodes[ref_ptr].has_parent() {
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
        }
    }

    fn link_threads(&mut self, collection: &mut Envelopes) {
        for v in collection.values_mut() {
            self.link_envelope(v);
        }
    }
}

impl Index<usize> for Threads {
    type Output = ThreadNode;

    fn index(&self, index: usize) -> &ThreadNode {
        self.thread_nodes
            .get(index)
            .expect("thread index out of bounds")
    }
}

fn node_build(
    tree: &mut ThreadTree,
    idx: usize,
    thread_nodes: &mut Vec<ThreadNode>,
    indentation: usize,
    collection: &Envelopes,
) {
    if let Some(hash) = thread_nodes[idx].message {
        if !collection.contains_key(&hash) {
            /* invalidate node */
            //        thread_nodes[idx].message = None;
        } else if let Some(parent_id) = thread_nodes[idx].parent {
            if let Some(parent_hash) = thread_nodes[parent_id].message {
                if !collection.contains_key(&parent_hash) {
                    /* invalidate node */
                    //               thread_nodes[parent_id].message = None;
                } else {
                    /* decide if the subject should be shown in the UI.
                     * If parent subject is Foobar and reply is `Re: Foobar`
                     * then showing the reply's subject can be reduntant
                     */
                    let mut subject = collection[&hash].subject();
                    let mut subject = subject.to_mut().as_bytes();
                    subject.strip_prefixes();
                    let mut parent_subject = collection[&parent_hash].subject();
                    let mut parent_subject = parent_subject.to_mut().as_bytes();
                    parent_subject.strip_prefixes();
                    if subject == parent_subject {
                        thread_nodes[idx].show_subject = false;
                    }
                }
            }
        }
    }

    let indentation = if thread_nodes[idx].has_message() {
        thread_nodes[idx].indentation = indentation;
        indentation + 1
    } else if indentation > 0 {
        indentation
    } else {
        indentation + 1
    };

    let mut has_unseen = if let Some(msg) = thread_nodes[idx].message {
        !collection[&msg].is_seen()
    } else {
        false
    };
    let mut child_vec: Vec<ThreadTree> = Vec::new();

    thread_nodes[idx].len = thread_nodes[idx].children.len();

    /* No child/parent relationship is mutated at any point and no nodes are added or removed. Only
     * each node's fields change, so the following is safe.
     */
    let children = &thread_nodes[idx].children as *const Vec<usize>;
    for &c in unsafe { &(*children) } {
        let mut new_tree = ThreadTree::new(c);
        node_build(&mut new_tree, c, thread_nodes, indentation, collection);
        thread_nodes[idx].len += thread_nodes[c].len;
        thread_nodes[idx].date = cmp::max(thread_nodes[idx].date, thread_nodes[c].date);

        has_unseen |= thread_nodes[c].has_unseen;
        child_vec.push(new_tree);
    }
    tree.children = child_vec;
    thread_nodes[idx].has_unseen = has_unseen;
}
