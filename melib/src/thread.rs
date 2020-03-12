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

use crate::datetime::UnixTimestamp;
use crate::email::parser::BytesExt;
use crate::email::*;

mod iterators;
pub use iterators::*;

#[cfg(feature = "unicode_algorithms")]
use crate::text_processing::grapheme_clusters::*;
use uuid::Uuid;

use fnv::{FnvHashMap, FnvHashSet};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::fmt;
use std::iter::FromIterator;
use std::ops::Index;
use std::result::Result as StdResult;
use std::str::FromStr;
use std::string::ToString;
use std::sync::{Arc, RwLock};

use smallvec::SmallVec;

type Envelopes = Arc<RwLock<FnvHashMap<EnvelopeHash, Envelope>>>;

macro_rules! uuid_hash_type {
    ($n:ident) => {
        #[derive(PartialEq, Hash, Eq, Copy, Clone, Serialize, Deserialize, Default)]
        pub struct $n(Uuid);

        impl fmt::Debug for $n {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.0.to_string())
            }
        }

        impl fmt::Display for $n {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{}", self.0.to_string())
            }
        }

        impl $n {
            fn new() -> Self {
                $n(Uuid::new_v4())
            }
            pub fn null() -> Self {
                $n(Uuid::nil())
            }
        }
    };
}

uuid_hash_type!(ThreadNodeHash);
uuid_hash_type!(ThreadHash);

/* Helper macros to avoid repeating ourselves */

macro_rules! remove_from_parent {
    ($buf:expr, $idx:expr) => {{
        let mut parent: Option<ThreadNodeHash> = None;
        let entry_parent = $buf.entry($idx).or_default().parent;
        if let Some(p) = entry_parent {
            parent = Some(p);
            if let Some(pos) = $buf[&p].children.iter().position(|c| *c == $idx) {
                $buf.entry(p).and_modify(|e| {
                    e.children.remove(pos);
                });
            }
        }
        $buf.entry($idx).and_modify(|e| e.parent = None);
        parent
    }};
}

macro_rules! make {
    (($p:expr)parent of($c:expr), $threads:expr) => {{
        let old_group_hash = $threads.find_group($threads.thread_nodes[&$c].group);
        let parent_group_hash = $threads.find_group($threads.thread_nodes[&$p].group);
        if old_group_hash != parent_group_hash {
            let prev_parent = remove_from_parent!(&mut $threads.thread_nodes, $c);
            if !($threads.thread_nodes[&$p]).children.contains(&$c) {
                /* Pruned nodes keep their children in case they show up in a later merge, so do not panic
                 * if children exists */
                $threads.thread_nodes.entry($p).and_modify(|e| e.children.push($c));
            }
            $threads.thread_nodes.entry($c).and_modify(|e| {
                e.parent = Some($p);
            });
            let old_group = std::mem::replace($threads.groups.entry(old_group_hash).or_default(), ThreadGroup::Node {
                parent: RefCell::new(parent_group_hash),
            });
            $threads.thread_nodes.entry($c).and_modify(|e| {
                e.group = parent_group_hash;
            });
            $threads.thread_nodes.entry($p).and_modify(|e| {
                e.group = parent_group_hash;
            });
            {
                let parent_group = $threads.thread_ref_mut(parent_group_hash);
                match (parent_group, old_group) {
                    (Thread {
                        ref mut date,
                        ref mut len,
                        ref mut unseen,
                        ref mut snoozed,
                        ref mut attachments,
                        ..
                    }, ThreadGroup::Root(Thread {
                        date: old_date,
                        len: old_len,
                        unseen: old_unseen,
                        snoozed: old_snoozed,
                        attachments: old_attachments,
                        ..
                    })) => {
                        *date = std::cmp::max(old_date, *date);
                        *len += old_len;
                        *unseen += old_unseen;
                        *attachments += old_attachments;
                        *snoozed |= old_snoozed;
                    }
                    _ => unreachable!(),
                 }
            }
            prev_parent
        } else {
            None
        }
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

#[derive(Default, Clone, Debug, Deserialize, Serialize)]
pub struct Thread {
    pub root: ThreadNodeHash,
    pub date: UnixTimestamp,
    pub len: usize,
    pub unseen: usize,
    pub attachments: usize,

    pub snoozed: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum ThreadGroup {
    Root(Thread),
    Node { parent: RefCell<ThreadHash> },
}

impl Default for ThreadGroup {
    fn default() -> Self {
        ThreadGroup::Root(Thread::default())
    }
}

impl ThreadGroup {
    pub fn root(&self) -> Option<&Thread> {
        if let ThreadGroup::Root(ref root) = self {
            Some(root)
        } else {
            None
        }
    }
}

macro_rules! property {
    ($name:ident: $t:ty) => {
        pub fn $name(&self) -> $t {
            (self.$name).into()
        }
    }
}

impl Thread {
    property!(root: ThreadNodeHash);
    property!(len: usize);
    property!(unseen: usize);
    property!(snoozed: bool);
    property!(date: UnixTimestamp);

    pub fn has_attachments(&self) -> bool {
        self.attachments > 0
    }

    pub fn set_snoozed(&mut self, val: bool) {
        self.snoozed = val;
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ThreadNode {
    message: Option<EnvelopeHash>,
    parent: Option<ThreadNodeHash>,
    children: Vec<ThreadNodeHash>,
    date: UnixTimestamp,
    show_subject: bool,
    pruned: bool,
    is_root: bool,
    pub group: ThreadHash,

    unseen: bool,
}

impl Default for ThreadNode {
    fn default() -> ThreadNode {
        ThreadNode {
            message: None,
            parent: None,
            children: Vec::new(),
            date: UnixTimestamp::default(),
            show_subject: true,
            pruned: false,
            is_root: false,
            group: ThreadHash::new(),

            unseen: false,
        }
    }
}

impl ThreadNode {
    fn new() -> Self {
        ThreadNode {
            ..Default::default()
        }
    }
    pub fn show_subject(&self) -> bool {
        self.show_subject
    }

    pub fn unseen(&self) -> bool {
        self.unseen
    }

    pub fn set_unseen(&mut self, new_val: bool) {
        self.unseen = new_val;
    }

    pub fn date(&self) -> UnixTimestamp {
        self.date
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

    pub fn parent(&self) -> Option<ThreadNodeHash> {
        self.parent
    }

    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }

    pub fn children(&self) -> &[ThreadNodeHash] {
        &self.children
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Threads {
    pub thread_nodes: FnvHashMap<ThreadNodeHash, ThreadNode>,
    root_set: RefCell<Vec<ThreadNodeHash>>,
    tree_index: RefCell<Vec<ThreadNodeHash>>,
    pub groups: FnvHashMap<ThreadHash, ThreadGroup>,

    message_ids: FnvHashMap<Vec<u8>, ThreadNodeHash>,
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

impl Threads {
    pub fn is_snoozed(&self, h: ThreadNodeHash) -> bool {
        self.thread_ref(self.thread_nodes[&h].group).snoozed()
    }

    pub fn thread_ref(&self, h: ThreadHash) -> &Thread {
        match self.groups[&self.find_group(h)] {
            ThreadGroup::Root(ref root) => root,
            ThreadGroup::Node { .. } => unreachable!(),
        }
    }

    pub fn thread_ref_mut(&mut self, h: ThreadHash) -> &mut Thread {
        match self.groups.get_mut(&self.find_group(h)) {
            Some(ThreadGroup::Root(ref mut root)) => root,
            Some(ThreadGroup::Node { .. }) | None => unreachable!(),
        }
    }

    pub fn find_group(&self, h: ThreadHash) -> ThreadHash {
        let p = match self.groups[&h] {
            ThreadGroup::Root(_) => return h,
            ThreadGroup::Node { ref parent } => *parent.borrow(),
        };

        let parent_group = self.find_group(p);
        match self.groups[&h] {
            ThreadGroup::Node { ref parent } => {
                *parent.borrow_mut() = parent_group;
            }
            _ => unreachable!(),
        }
        parent_group
    }

    pub fn new(length: usize) -> Threads {
        /* To reconstruct thread information from the mails we need: */

        /* a vector to hold thread members */
        let thread_nodes: FnvHashMap<ThreadNodeHash, ThreadNode> =
            FnvHashMap::with_capacity_and_hasher(
                (length as f64 * 1.2) as usize,
                Default::default(),
            );
        /* A hash table of Message IDs */
        let message_ids: FnvHashMap<Vec<u8>, ThreadNodeHash> =
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
            sort: RefCell::new((SortField::Date, SortOrder::Desc)),
            subsort: RefCell::new((SortField::Subject, SortOrder::Desc)),

            ..Default::default()
        }
    }

    pub fn threads_group_iter(
        &self,
        root_tree: SmallVec<[ThreadNodeHash; 1024]>,
    ) -> ThreadsGroupIterator {
        ThreadsGroupIterator {
            root_tree,
            pos: 0,
            stack: SmallVec::new(),
            thread_nodes: &self.thread_nodes,
        }
    }

    pub fn thread_group_iter(&self, index: ThreadHash) -> ThreadGroupIterator {
        ThreadGroupIterator {
            group: self.thread_ref(index).root(),
            pos: 0,
            stack: SmallVec::new(),
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
        let was_unseen = self.thread_nodes[&thread_hash].unseen;
        let is_unseen = !envelopes.read().unwrap()[&new_hash].is_seen();
        if was_unseen != is_unseen {
            let Thread { ref mut unseen, .. } =
                self.thread_ref_mut(self.thread_nodes[&thread_hash].group);
            if was_unseen {
                *unseen -= 1;
            } else {
                *unseen += 1;
            }
        }
        self.thread_nodes.get_mut(&thread_hash).unwrap().unseen = is_unseen;
        self.hash_set.remove(&old_hash);
        self.hash_set.insert(new_hash);
        Ok(())
    }

    #[inline]
    pub fn remove(&mut self, envelope_hash: EnvelopeHash) {
        self.hash_set.remove(&envelope_hash);

        let t_id: ThreadNodeHash = if let Some((pos, n)) = self
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
        id: ThreadNodeHash,
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
        self.insert_internal(envelopes, env_hash, false);
    }

    fn insert_internal(
        &mut self,
        envelopes: &mut Envelopes,
        env_hash: EnvelopeHash,
        other_mailbox: bool,
    ) -> bool {
        let envelopes_lck = envelopes.read().unwrap();
        let reply_to_id: Option<ThreadNodeHash> = envelopes_lck[&env_hash]
            .in_reply_to()
            .map(crate::email::StrBuild::raw)
            .and_then(|r| self.message_ids.get(r).cloned());
        let message_id = envelopes_lck[&env_hash].message_id().raw();
        if self.message_ids_set.contains(message_id)
            && !self.missing_message_ids.contains(message_id)
        {
            return false;
        }

        if other_mailbox
            && reply_to_id.is_none()
            && !self.message_ids.contains_key(message_id)
            && !envelopes_lck[&env_hash]
                .references()
                .iter()
                .any(|r| self.message_ids.contains_key(r.raw()))
        {
            return false;
        }

        let new_id = self
            .message_ids
            .get(message_id)
            .cloned()
            .or(
                if envelopes_lck[&env_hash].thread() != ThreadNodeHash::null() {
                    Some(envelopes_lck[&env_hash].thread())
                } else {
                    None
                },
            )
            .unwrap_or_else(|| ThreadNodeHash::new());
        {
            let mut node = self.thread_nodes.entry(new_id).or_default();
            node.message = Some(env_hash);
            if node.parent.is_none() {
                node.parent = reply_to_id;
            }
            node.date = envelopes_lck[&env_hash].date();
            node.unseen = !envelopes_lck[&env_hash].is_seen();
        }

        if !self.groups.contains_key(&self.thread_nodes[&new_id].group) {
            self.groups.insert(
                self.thread_nodes[&new_id].group,
                ThreadGroup::Root(Thread {
                    root: new_id,
                    date: envelopes_lck[&env_hash].date(),
                    len: 1,
                    unseen: if !envelopes_lck[&env_hash].is_seen() {
                        1
                    } else {
                        0
                    },
                    attachments: if envelopes_lck[&env_hash].has_attachments() {
                        1
                    } else {
                        0
                    },
                    snoozed: false,
                }),
            );
        } else {
            let parent_group = self.thread_ref_mut(self.thread_nodes[&new_id].group);
            parent_group.date = std::cmp::max(parent_group.date, envelopes_lck[&env_hash].date());
            parent_group.len += 1;
            parent_group.unseen += if !envelopes_lck[&env_hash].is_seen() {
                1
            } else {
                0
            };
            parent_group.attachments += if envelopes_lck[&env_hash].has_attachments() {
                1
            } else {
                0
            };
        }

        self.message_ids.insert(message_id.to_vec(), new_id);
        self.message_ids_set.insert(message_id.to_vec());
        self.missing_message_ids.remove(message_id);
        self.hash_set.insert(env_hash);
        if let Some(reply_to_id) = reply_to_id {
            make!((reply_to_id) parent of (new_id), self);
        } else if let Some(r) = envelopes_lck[&env_hash]
            .in_reply_to()
            .map(crate::email::StrBuild::raw)
        {
            let reply_to_id = ThreadNodeHash::new();
            self.thread_nodes.insert(
                reply_to_id,
                ThreadNode {
                    date: envelopes_lck[&env_hash].date(),
                    ..ThreadNode::new()
                },
            );

            self.groups.insert(
                self.thread_nodes[&reply_to_id].group,
                ThreadGroup::Root(Thread {
                    root: reply_to_id,
                    date: envelopes_lck[&env_hash].date(),
                    len: 0,
                    unseen: 0,
                    attachments: 0,
                    snoozed: false,
                }),
            );
            make!((reply_to_id) parent of (new_id), self);
            self.message_ids.insert(r.to_vec(), reply_to_id);
            self.message_ids_set.insert(r.to_vec());
            self.missing_message_ids.insert(r.to_vec());
        }

        if envelopes_lck[&env_hash].references.is_some() {
            let mut current_descendant_id = new_id;
            let mut references = envelopes_lck[&env_hash].references();
            if references.first() == envelopes_lck[&env_hash].in_reply_to().as_ref() {
                references.reverse();
            }

            for reference in references.into_iter().rev() {
                if let Some(&id) = self.message_ids.get(reference.raw()) {
                    if self.thread_nodes[&id].date > self.thread_nodes[&current_descendant_id].date
                        || self.thread_nodes[&current_descendant_id].parent.is_some()
                    {
                        current_descendant_id = id;
                        continue;
                    }
                    make!((id) parent of (current_descendant_id), self);
                    current_descendant_id = id;
                } else {
                    let id = ThreadNodeHash::new();
                    self.thread_nodes.insert(
                        id,
                        ThreadNode {
                            date: envelopes_lck[&env_hash].date(),
                            ..ThreadNode::new()
                        },
                    );
                    self.groups.insert(
                        self.thread_nodes[&id].group,
                        ThreadGroup::Root(Thread {
                            root: id,
                            date: envelopes_lck[&env_hash].date(),
                            len: 0,
                            unseen: 0,
                            attachments: 0,
                            snoozed: false,
                        }),
                    );
                    make!((id) parent of (current_descendant_id), self);
                    self.missing_message_ids.insert(reference.raw().to_vec());
                    self.message_ids.insert(reference.raw().to_vec(), id);
                    self.message_ids_set.insert(reference.raw().to_vec());
                    current_descendant_id = id;
                }
            }
        }
        drop(envelopes_lck);
        self.update_show_subject(new_id, env_hash, envelopes);
        envelopes
            .write()
            .unwrap()
            .get_mut(&env_hash)
            .unwrap()
            .set_thread(new_id);

        /*
        save_graph(
            &self.tree_index.borrow(),
            &self.thread_nodes,
            &self
                .message_ids
                .iter()
                .map(|(a, &b)| (b, a.to_vec()))
                .collect::<FnvHashMap<ThreadNodeHash, Vec<u8>>>(),
            &envelopes,
        );
        */
        true
    }

    /* Insert or update */
    pub fn insert_reply(&mut self, envelopes: &mut Envelopes, env_hash: EnvelopeHash) -> bool {
        self.insert_internal(envelopes, env_hash, true)
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

    pub fn group_inner_sort_by(
        &self,
        vec: &mut [ThreadHash],
        sort: (SortField, SortOrder),
        envelopes: &Envelopes,
    ) {
        let envelopes = envelopes.read().unwrap();
        vec.sort_by(|a, b| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let a = self.thread_ref(*a).date();
                let b = self.thread_ref(*b).date();
                b.cmp(&a)
            }
            (SortField::Date, SortOrder::Asc) => {
                let a = self.thread_ref(*a).date();
                let b = self.thread_ref(*b).date();
                a.cmp(&b)
            }
            (SortField::Subject, SortOrder::Desc) => {
                let a = &self.thread_nodes[&self.thread_ref(*a).root()].message();
                let b = &self.thread_nodes[&self.thread_ref(*b).root()].message();

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
                let a = &self.thread_nodes[&self.thread_ref(*a).root()].message();
                let b = &self.thread_nodes[&self.thread_ref(*b).root()].message();

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
    pub fn node_inner_sort_by(
        &self,
        vec: &mut [ThreadNodeHash],
        sort: (SortField, SortOrder),
        envelopes: &Envelopes,
    ) {
        let envelopes = envelopes.read().unwrap();
        vec.sort_by(|a, b| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let a = self.thread_ref(self.thread_nodes[&a].group).date();
                let b = self.thread_ref(self.thread_nodes[&b].group).date();
                b.cmp(&a)
            }
            (SortField::Date, SortOrder::Asc) => {
                let a = self.thread_ref(self.thread_nodes[&a].group).date();
                let b = self.thread_ref(self.thread_nodes[&b].group).date();
                a.cmp(&b)
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
        tree.sort_by(|a, b| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let a = self.thread_ref(self.thread_nodes[&a].group).date();
                let b = self.thread_ref(self.thread_nodes[&b].group).date();
                b.cmp(&a)
            }
            (SortField::Date, SortOrder::Asc) => {
                let a = self.thread_ref(self.thread_nodes[&a].group).date();
                let b = self.thread_ref(self.thread_nodes[&b].group).date();
                a.cmp(&b)
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

    pub fn thread_to_mail(&self, i: ThreadNodeHash) -> EnvelopeHash {
        let thread = &self.thread_nodes[&i];
        thread.message().unwrap()
    }

    pub fn thread_nodes(&self) -> &FnvHashMap<ThreadNodeHash, ThreadNode> {
        &self.thread_nodes
    }

    pub fn len(&self) -> usize {
        self.hash_set.len()
    }

    pub fn root_len(&self) -> usize {
        self.tree_index.borrow().len()
    }

    pub fn root_set(&self, idx: usize) -> ThreadNodeHash {
        self.tree_index.borrow()[idx]
    }

    pub fn roots(&self) -> SmallVec<[ThreadHash; 1024]> {
        //FIXME: refactor filter
        self.groups
            .iter()
            .filter_map(|(h, g)| g.root().map(|_| *h))
            .collect::<SmallVec<[ThreadHash; 1024]>>()
    }
}

impl Index<&ThreadNodeHash> for Threads {
    type Output = ThreadNode;

    fn index(&self, index: &ThreadNodeHash) -> &ThreadNode {
        self.thread_nodes
            .get(index)
            .expect("thread index out of bounds")
    }
}

/*
fn print_threadnodes(
    node_hash: ThreadNodeHash,
    nodes: &FnvHashMap<ThreadNodeHash, ThreadNode>,
    envelopes: &Envelopes,
) {
    fn help(
        level: usize,
        node_hash: ThreadNodeHash,
        nodes: &FnvHashMap<ThreadNodeHash, ThreadNode>,
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

#[derive(Serialize)]
struct Node {
    id: String,
    subject: String,
    from: String,
    to: String,
    date: UnixTimestamp,
    references: String,
    in_reply_to: String,
}

#[derive(Serialize)]
struct Link {
    source: String,
    target: String,
}

#[derive(Serialize)]
struct Graph {
    nodes: Vec<Node>,
    links: Vec<Link>,
}

/*
fn save_graph(
    node_arr: &[ThreadNodeHash],
    nodes: &FnvHashMap<ThreadNodeHash, ThreadNode>,
    ids: &FnvHashMap<ThreadNodeHash, Vec<u8>>,
    envelopes: &Envelopes,
) {
    let envelopes = envelopes.read().unwrap();
    let mut graph = Graph {
        nodes: vec![],
        links: vec![],
    };
    let mut stack: SmallVec<[(ThreadNodeHash, String); 16]> = SmallVec::new();
    for n in node_arr {
        stack.extend(
            nodes[n].children.iter().cloned().zip(
                std::iter::repeat(unsafe { std::str::from_utf8_unchecked(&ids[n]) }.to_string())
                    .take(nodes[n].children.len()),
            ),
        );
        graph.nodes.push(Node {
            id: unsafe { std::str::from_utf8_unchecked(&ids[n]).to_string() },
            subject: nodes[n]
                .message
                .as_ref()
                .map(|h| envelopes[h].subject().to_string())
                .unwrap_or("missing".into()),
            from: nodes[n]
                .message
                .as_ref()
                .map(|h| envelopes[h].field_from_to_string())
                .unwrap_or("missing".into()),
            to: nodes[n]
                .message
                .as_ref()
                .map(|h| envelopes[h].field_to_to_string())
                .unwrap_or("missing".into()),
            date: nodes[n]
                .message
                .as_ref()
                .map(|h| envelopes[h].date())
                .unwrap_or(0),
            references: nodes[n]
                .message
                .as_ref()
                .map(|h| envelopes[h].field_references_to_string())
                .unwrap_or("missing".into()),
            in_reply_to: nodes[n]
                .message
                .as_ref()
                .and_then(|h| envelopes[h].in_reply_to())
                .map(|h| h.to_string())
                .unwrap_or("missing".into()),
        });
        while let Some((target, parent)) = stack.pop() {
            graph.nodes.push(Node {
                id: unsafe { std::str::from_utf8_unchecked(&ids[&target]).to_string() },
                subject: nodes[&target]
                    .message
                    .as_ref()
                    .map(|h| envelopes[h].subject().to_string())
                    .unwrap_or("missing".into()),
                from: nodes[&target]
                    .message
                    .as_ref()
                    .map(|h| envelopes[h].field_from_to_string())
                    .unwrap_or("missing".into()),
                to: nodes[&target]
                    .message
                    .as_ref()
                    .map(|h| envelopes[h].field_to_to_string())
                    .unwrap_or("missing".into()),
                date: nodes[&target]
                    .message
                    .as_ref()
                    .map(|h| envelopes[h].date())
                    .unwrap_or(0),
                references: nodes[&target]
                    .message
                    .as_ref()
                    .map(|h| envelopes[h].field_references_to_string())
                    .unwrap_or("missing".into()),
                in_reply_to: nodes[&target]
                    .message
                    .as_ref()
                    .and_then(|h| envelopes[h].in_reply_to())
                    .map(|h| h.to_string())
                    .unwrap_or("missing".into()),
            });
            graph.links.push(Link {
                source: parent,
                target: unsafe { std::str::from_utf8_unchecked(&ids[&target]).to_string() },
            });

            stack.extend(
                nodes[&target].children.iter().cloned().zip(
                    std::iter::repeat(
                        unsafe { std::str::from_utf8_unchecked(&ids[&target]) }.to_string(),
                    )
                    .take(nodes[&target].children.len()),
                ),
            );
        }
        let s = serde_json::to_string(&graph).unwrap();
        use std::fs::File;
        use std::io::prelude::*;

        let mut file = File::create(format!(
            "/tmp/meli/threads/threads_{}.json",
            crate::datetime::now()
        ))
        .unwrap();
        file.write_all(s.as_bytes()).unwrap();
    }
}
*/
