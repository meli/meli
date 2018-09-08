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
 * Threading algorithm
 */

use mailbox::email::*;

extern crate fnv;
use self::fnv::{FnvHashMap, FnvHashSet};
use std::cell::{Ref, RefCell};
use std::cmp::Ordering;
use std::iter::FromIterator;
use std::ops::Index;
use std::result::Result as StdResult;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Copy, Deserialize)]
pub enum SortOrder {
    Asc,
    Desc,
}

#[derive(Debug, Clone, PartialEq, Copy, Deserialize)]
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

#[derive(Clone, Debug, Deserialize)]
struct ThreadTree {
    id: usize,
    children: Vec<ThreadTree>,
}

impl ThreadTree {
    fn new(id: usize) -> Self {
        ThreadTree {
            id,
            children: Vec::new(),
        }
    }
}

pub struct ThreadIterator<'a> {
    pos: usize,
    stack: Vec<usize>,
    tree: Ref<'a, Vec<ThreadTree>>,
}
impl<'a> Iterator for ThreadIterator<'a> {
    type Item = usize;
    fn next(&mut self) -> Option<usize> {
        {
            let mut tree = &(*self.tree);
            for i in &self.stack {
                tree = &tree[*i].children;
            }
            if self.pos == tree.len() {
                if self.stack.is_empty() {
                    return None;
                }
                self.pos = self.stack.pop().unwrap() + 1;
            } else {
                debug_assert!(self.pos < tree.len());
                let ret = tree[self.pos].id;
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

#[derive(Clone, Debug, Deserialize)]
pub struct ThreadNode {
    message: Option<EnvelopeHash>,
    parent: Option<usize>,
    children: Vec<usize>,
    date: UnixTimestamp,
    indentation: usize,
    show_subject: bool,

    len: usize,
    has_unseen: bool,
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

    fn is_descendant(&self, thread_nodes: &[ThreadNode], other: &ThreadNode) -> bool {
        if self == other {
            return true;
        }

        for v in &self.children {
            if thread_nodes[*v].is_descendant(thread_nodes, other) {
                return true;
            }
        }
        return false;
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

#[derive(Clone, Debug, Default, Deserialize)]
pub struct Threads {
    thread_nodes: Vec<ThreadNode>,
    root_set: RefCell<Vec<usize>>,
    tree: RefCell<Vec<ThreadTree>>,

    message_ids: FnvHashMap<String, usize>,
    hash_set: FnvHashSet<EnvelopeHash>,
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
    root_set: Ref<'a, Vec<usize>>,
}

impl<'a> Iterator for RootIterator<'a> {
    type Item = usize;
    fn next(&mut self) -> Option<usize> {
        {
            if self.pos == self.root_set.len() {
                return None;
            }
            self.pos += 1;
            return Some(self.root_set[self.pos - 1]);
        }
    }
}

impl Threads {
    pub fn new(collection: &mut FnvHashMap<EnvelopeHash, Envelope>) -> Threads {
        /* To reconstruct thread information from the mails we need: */

        /* a vector to hold thread members */
        let mut thread_nodes: Vec<ThreadNode> =
            Vec::with_capacity((collection.len() as f64 * 1.2) as usize);
        /* A hash table of Message IDs */
        let mut message_ids: FnvHashMap<String, usize> =
            FnvHashMap::with_capacity_and_hasher(collection.len(), Default::default());
        let mut hash_set: FnvHashSet<EnvelopeHash> =
            FnvHashSet::with_capacity_and_hasher(collection.len(), Default::default());
        /* Add each message to message_ids and threads, and link them together according to the
         * References / In-Reply-To headers */
        link_threads(
            &mut thread_nodes,
            &mut message_ids,
            &mut hash_set,
            collection,
        );

        /* Walk over the elements of message_ids, and gather a list of the ThreadNode objects that have
         * no parents. These are the root messages of each thread */
        let mut root_set: Vec<usize> = Vec::with_capacity(collection.len());

        'root_set: for v in message_ids.values() {
            /* update length */
            fn set_length(id: usize, thread_nodes: &mut Vec<ThreadNode>) -> usize {
                let mut length = thread_nodes[id].children.len();
                let children: Vec<usize> = thread_nodes[id].children.iter().cloned().collect();
                for c in children {
                    length += set_length(c, thread_nodes);
                }
                thread_nodes[id].len = length;
                return length;
            }
            set_length(*v, &mut thread_nodes);

            if thread_nodes[*v].parent.is_none() {
                if !thread_nodes[*v].has_message() && thread_nodes[*v].children.len() == 1 {
                    /* Do not promote the children if doing so would promote them to the root set
                     * -- unless there is only one child, in which case, do. */
                    root_set.push(thread_nodes[*v].children[0]);

                    continue 'root_set;
                }
                root_set.push(*v);
            }
        }
        let mut t = Threads {
            thread_nodes,
            root_set: RefCell::new(root_set),
            message_ids,
            hash_set,
            ..Default::default()
        };
        t.build_collection(&collection);
        t
    }

    pub fn thread_iter(&self, index: usize) -> ThreadIterator {
        ThreadIterator {
            pos: index,
            stack: Vec::new(),
            tree: self.tree.borrow(),
        }
    }

    pub fn update(&mut self, collection: &mut FnvHashMap<EnvelopeHash, Envelope>) {
        let new_hash_set = FnvHashSet::from_iter(collection.keys().cloned());

        let difference: Vec<EnvelopeHash> =
            new_hash_set.difference(&self.hash_set).cloned().collect();
        for h in difference {
            self.insert(collection.entry(h).or_default());
        }
    }

    pub fn insert(&mut self, envelope: &mut Envelope) {
        link_envelope(
            &mut self.thread_nodes,
            &mut self.message_ids,
            &mut self.hash_set,
            envelope,
        );
    }

    fn build_collection(&mut self, collection: &FnvHashMap<EnvelopeHash, Envelope>) {
        {
            let tree = self.tree.get_mut();
            for i in self.root_set.borrow().iter() {
                let mut tree_node = ThreadTree::new(*i);
                node_build(
                    &mut tree_node,
                    *i,
                    &mut self.thread_nodes,
                    0,  /* indentation */
                    *i, /* root_subject_idx */
                    collection,
                );
                tree.push(tree_node);
            }
        }
        self.inner_sort_by(*self.sort.borrow(), collection);
        self.inner_subsort_by(*self.subsort.borrow(), collection);
    }

    fn inner_subsort_by(
        &self,
        subsort: (SortField, SortOrder),
        collection: &FnvHashMap<EnvelopeHash, Envelope>,
    ) {
        let tree = &mut self.tree.borrow_mut();
        for mut t in tree.iter_mut() {
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

    fn inner_sort_by(
        &self,
        sort: (SortField, SortOrder),
        collection: &FnvHashMap<EnvelopeHash, Envelope>,
    ) {
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
        collection: &FnvHashMap<EnvelopeHash, Envelope>,
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
        let thread = &self.thread_nodes[self.root_set.borrow()[i]];
        thread.message().unwrap()
    }

    pub fn thread_nodes(&self) -> &Vec<ThreadNode> {
        &self.thread_nodes
    }

    pub fn root_len(&self) -> usize {
        self.root_set.borrow().len()
    }

    pub fn root_set(&self, idx: usize) -> usize {
        self.root_set.borrow()[idx]
    }

    pub fn root_iter<'a>(&'a self) -> RootIterator<'a> {
        RootIterator {
            pos: 0,
            root_set: self.root_set.borrow(),
        }
    }

    pub fn has_sibling(&self, i: usize) -> bool {
        if let Some(parent) = self[i].parent {
            self[parent].children.len() > 1
        } else {
            false
        }
    }
}

fn link_envelope(
    thread_nodes: &mut Vec<ThreadNode>,
    message_ids: &mut FnvHashMap<String, usize>,
    hash_set: &mut FnvHashSet<EnvelopeHash>,
    envelope: &mut Envelope,
) {
    let m_id = envelope.message_id_raw().to_string();

    /* The index of this message's ThreadNode in thread_nodes */

    let t_idx: usize = if message_ids.get(&m_id).is_some() {
        let node_idx = message_ids[&m_id];
        /* the already existing ThreadNote should be empty, since we're
         * seeing this message for the first time. otherwise it's a
         * duplicate. */
        if thread_nodes[node_idx].message.is_some() {
            return;
        }
        thread_nodes[node_idx].date = envelope.date();
        thread_nodes[node_idx].message = Some(envelope.hash());
        envelope.set_thread(node_idx);

        node_idx
    } else {
        /* Create a new ThreadNode object holding this message */
        thread_nodes.push(ThreadNode {
            message: Some(envelope.hash()),
            date: envelope.date(),
            ..Default::default()
        });
        envelope.set_thread(thread_nodes.len() - 1);
        message_ids.insert(m_id, thread_nodes.len() - 1);
        hash_set.insert(envelope.hash());
        thread_nodes.len() - 1
    };

    /* For each element in the message's References field:
     *
     * Find a ThreadNode object for the given Message-ID:
     * If there's one in message_ids use that;
     * Otherwise, make (and index) one with a null Message
     *
     * Link the References field's ThreadNode together in the order implied
     * by the References header.
     * If they are already linked, don't change the existing links.
     * Do not add a link if adding that link would introduce a loop: that
     * is, before asserting A->B, search down the children of B to see if A
     * is reachable, and also search down the children of A to see if B is
     * reachable. If either is already reachable as a child of the other,
     * don't add the link.
     */

    /* The index of the reference we are currently examining, start from current message */
    let mut ref_ptr = t_idx;

    for &refn in envelope.references().iter().rev() {
        let r_id = String::from_utf8_lossy(refn.raw()).into();
        let parent_id = if message_ids.contains_key(&r_id) {
            let parent_id = message_ids[&r_id];
            if !(thread_nodes[parent_id].is_descendant(thread_nodes, &thread_nodes[ref_ptr])
                || thread_nodes[ref_ptr].is_descendant(thread_nodes, &thread_nodes[parent_id]))
            {
                thread_nodes[ref_ptr].parent = Some(parent_id);
                if !thread_nodes[parent_id].children.contains(&ref_ptr) {
                    thread_nodes[parent_id].children.push(ref_ptr);
                }

                let (left, right) = thread_nodes.split_at_mut(parent_id);
                let (parent, right) = right.split_first_mut().unwrap();
                for &c in &parent.children {
                    if c > parent_id {
                        right[c - parent_id - 1].parent = Some(parent_id);
                    } else {
                        left[c].parent = Some(parent_id);
                    }
                }
            }
            parent_id
        } else {
            /* Create a new ThreadNode object holding this reference */
            thread_nodes.push(ThreadNode {
                message: None,
                children: vec![ref_ptr; 1],
                date: envelope.date(),
                ..Default::default()
            });
            if thread_nodes[ref_ptr].parent.is_none() {
                thread_nodes[ref_ptr].parent = Some(thread_nodes.len() - 1);
            }
            message_ids.insert(r_id, thread_nodes.len() - 1);
            thread_nodes.len() - 1
        };

        /* Update thread's date */

        let mut parent_iter = parent_id;
        'date: loop {
            let p: &mut ThreadNode = &mut thread_nodes[parent_iter];
            if p.date < envelope.date() {
                p.date = envelope.date();
            }
            if let Some(p) = p.parent {
                parent_iter = p;
            } else {
                break 'date;
            }
        }
        ref_ptr = parent_id;
    }
}

fn link_threads(
    thread_nodes: &mut Vec<ThreadNode>,
    message_ids: &mut FnvHashMap<String, usize>,
    hash_set: &mut FnvHashSet<EnvelopeHash>,
    collection: &mut FnvHashMap<EnvelopeHash, Envelope>,
) {
    for v in collection.values_mut() {
        link_envelope(thread_nodes, message_ids, hash_set, v);
    }
}

impl Index<usize> for Threads {
    type Output = ThreadNode;

    fn index(&self, index: usize) -> &ThreadNode {
        self.thread_nodes
            .get(self.tree.borrow()[index].id)
            .expect("thread index out of bounds")
    }
}

fn node_build(
    tree: &mut ThreadTree,
    idx: usize,
    thread_nodes: &mut Vec<ThreadNode>,
    indentation: usize,
    root_subject_idx: usize,
    collection: &FnvHashMap<EnvelopeHash, Envelope>,
) {
    if let Some(msg_idx) = thread_nodes[root_subject_idx].message().as_ref() {
        let root_subject = collection[msg_idx].subject();
        /* If the ThreadNode has no Message, but does have children, remove this container but
         * promote its children to this level (that is, splice them in to the current child
         * list.) */
        if indentation > 0 && thread_nodes[idx].has_message() {
            let subject = collection[thread_nodes[idx].message().as_ref().unwrap()].subject();
            thread_nodes[idx].has_unseen =
                !collection[thread_nodes[idx].message().as_ref().unwrap()].is_seen();
            if subject == root_subject
                || subject.starts_with("Re: ") && subject.as_ref().ends_with(root_subject.as_ref())
            {
                thread_nodes[idx].show_subject = false;
            }
        }
    }
    if thread_nodes[idx].has_parent()
        && !thread_nodes[thread_nodes[idx].parent().unwrap()].has_message()
    {
        thread_nodes[idx].parent = None;
    }
    let indentation = if thread_nodes[idx].has_message() {
        thread_nodes[idx].indentation = indentation;
        indentation + 1
    } else if indentation > 0 {
        indentation
    } else {
        indentation + 1
    };

    let mut has_unseen = thread_nodes[idx].has_unseen;
    let mut child_vec: Vec<ThreadTree> = Vec::new();
    for c in thread_nodes[idx].children.clone().iter() {
        let mut new_tree = ThreadTree::new(*c);
        node_build(
            &mut new_tree,
            *c,
            thread_nodes,
            indentation,
            idx,
            collection,
        );
        has_unseen |= thread_nodes[*c].has_unseen;
        child_vec.push(new_tree);
    }
    tree.children = child_vec;
    thread_nodes[idx].has_unseen = has_unseen;
}
