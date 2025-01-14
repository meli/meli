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

//! e-mail threading (conversations)
//!
//! This module implements Jamie Zawinski's [threading
//! algorithm](https://www.jwz.org/doc/threading.html). Quoted comments (/* " .. " */) are taken
//! almost verbatim from the algorithm.
//!
//! The entry point of this module is the [`Threads`] struct and its
//! [`new`](Threads::new) method. It contains [`ThreadNode`s](ThreadNode) which
//! are the nodes in the thread trees that might have messages associated with
//! them. The root nodes (first messages in each thread) are stored in
//! [`root_set`](Threads::root_set) and [`thread_nodes`](Threads::thread_nodes)
//! vectors. [`Threads`] has inner mutability since we need to sort without the
//! user having mutable ownership.

use crate::{
    email::{address::StrBuild, parser::BytesExt, *},
    SortField, SortOrder, UnixTimestamp,
};

mod iterators;
use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet, VecDeque},
    iter::FromIterator,
    ops::Index,
    string::ToString,
    sync::{Arc, RwLock},
};

pub use iterators::*;
use smallvec::SmallVec;
use uuid::Uuid;

use crate::text::grapheme_clusters::*;

type Envelopes = Arc<RwLock<HashMap<EnvelopeHash, Envelope>>>;

macro_rules! uuid_hash_type {
    ($n:ident) => {
        #[derive(PartialEq, Hash, Eq, Copy, Clone, Serialize, Deserialize, Default)]
        pub struct $n(Uuid);

        impl std::fmt::Debug for $n {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.0.to_string())
            }
        }

        impl std::fmt::Display for $n {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", self.0.to_string())
            }
        }

        impl<B: AsRef<[u8]>> From<B> for $n {
            fn from(val: B) -> Self {
                let val = val.as_ref();
                $n(Uuid::new_v5(&Uuid::NAMESPACE_URL, val))
            }
        }

        impl $n {
            pub fn new() -> Self {
                $n(Uuid::new_v4())
            }

            pub const fn null() -> Self {
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
            if let Some(old_env_hashes) = $threads.thread_to_envelope.get(&old_group_hash).cloned()
            {
                for &env_hash in &old_env_hashes {
                    *$threads.envelope_to_thread.entry(env_hash).or_default() = parent_group_hash;
                }
                $threads
                    .thread_to_envelope
                    .entry(parent_group_hash)
                    .or_default()
                    .extend(old_env_hashes.into_iter());
            }
            let prev_parent = remove_from_parent!(&mut $threads.thread_nodes, $c);
            if !($threads.thread_nodes[&$p]).children.contains(&$c) {
                /* Pruned nodes keep their children in case they show up in a later merge, so
                 * do not panic if children exists */
                $threads
                    .thread_nodes
                    .entry($p)
                    .and_modify(|e| e.children.push($c));
            }
            $threads.thread_nodes.entry($c).and_modify(|e| {
                e.parent = Some($p);
            });
            let old_group = std::mem::replace(
                $threads.groups.entry(old_group_hash).or_default(),
                ThreadGroup::Node {
                    parent: Arc::new(RwLock::new(parent_group_hash)),
                },
            );
            $threads.thread_nodes.entry($c).and_modify(|e| {
                e.group = parent_group_hash;
            });
            $threads.thread_nodes.entry($p).and_modify(|e| {
                e.group = parent_group_hash;
            });
            {
                let parent_group = $threads.thread_ref_mut(parent_group_hash);
                match (parent_group, old_group) {
                    (
                        Thread {
                            ref mut date,
                            ref mut len,
                            ref mut unseen,
                            ref mut snoozed,
                            ref mut attachments,
                            ..
                        },
                        ThreadGroup::Root(Thread {
                            date: old_date,
                            len: old_len,
                            unseen: old_unseen,
                            snoozed: old_snoozed,
                            attachments: old_attachments,
                            ..
                        }),
                    ) => {
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

/// Strip common prefixes from subjects
///
///
/// ```rust
/// use melib::thread::SubjectPrefix;
///
/// let mut subject = "Re: RE: Res: Re: Res: Subject";
/// assert_eq!(
///     subject.strip_prefixes_from_list(<&str>::USUAL_PREFIXES, None),
///     &"Subject"
/// );
/// let mut subject = "Re: RE: Res: Re: Res: Subject";
/// assert_eq!(
///     subject.strip_prefixes_from_list(<&str>::USUAL_PREFIXES, Some(1)),
///     &"RE: Res: Re: Res: Subject"
/// );
/// ```
pub trait SubjectPrefix {
    const USUAL_PREFIXES: &'static [&'static str] = &[
        "Re:",
        // Canada (Réponse)
        // Spanish (Respuesta)
        "RE:",
        "Fwd:",
        "Fw:",
        /* taken from
         * https://en.wikipedia.org/wiki/List_of_email_subject_abbreviations#Abbreviations_in_other_languages
         */
        "回复:",
        "回覆:",
        // Dutch (Antwoord)
        "Antw:",
        // Dutch (Doorsturen)
        "Doorst:",
        // Finnish (Välitetty)
        "VL:",
        // French (Référence)
        "REF:",
        // French (Transfert)
        // Canada (Transfert)
        "TR:",
        // German (Antwort)
        "AW:",
        // German (Weitergeleitet)
        "WG:",
        // Greek (Απάντηση)
        "ΑΠ:",
        "Απ:",
        "απ:",
        // Greek (Προωθημένο)
        "ΠΡΘ:",
        "Πρθ:",
        "πρθ:",
        // Greek (Σχετικό)
        "ΣΧΕΤ:",
        "Σχετ:",
        "σχετ:",
        // Greek (Προωθημένο)
        "ΠΡΘ:",
        "Πρθ:",
        "πρθ:",
        // Hungarian (Válasz)
        "Vá:",
        // Hungarian
        "Továbbítás:",
        // Italian (Riferimento)
        "R:",
        // Italian (Inoltro)
        "I:",
        // Italian (Riferimento)
        "RIF:",
        // Icelandic (Svara)
        // Swedish (Svar)
        // Norwegian (Svar)
        // Danish (Svar)
        "SV:",
        "Sv:",
        // Icelandic (Framsenda)
        "FS:",
        "Fs:",
        // Indonesian (Balas)
        "BLS:",
        // Indonesian (Terusan)
        "TRS:",
        // Norwegian (Videresendt)
        // Danish (Videresendt)
        // Finnish (Vastaus)
        "VS:",
        "Vs:",
        // Swedish (Vidarebefordrat)
        "VB:",
        "Vb:",
        // Spanish (Reenviado)
        "RV:",
        "Rv:",
        // Portuguese (Resposta)
        "RES:",
        "Res:",
        // Portuguese (Encaminhado)
        "ENC:",
        // Polish (Odpowiedź)
        "Odp:",
        // Polish (Podaj dalej)
        "PD:",
        // Turkish (Yanıt)
        "YNT:",
        // Turkish (İlet)
        "İLT:",
        // Welsh (Ateb)
        "ATB:",
        // Welsh (Ymlaen)
        "YML:",
    ];
    fn is_a_reply(&self) -> bool;
    fn strip_prefixes(&mut self) -> &mut Self;
    fn strip_prefixes_from_list(&mut self, list: &[&str], times: Option<u8>) -> &mut Self;
}

impl SubjectPrefix for &[u8] {
    fn is_a_reply(&self) -> bool {
        let self_ = self.trim();
        self_.starts_with(b"RE: ")
            || self_.starts_with(b"Re: ")
            || self_.starts_with(b"RES: ")
            || self_.starts_with(b"Res: ")
            || self_.starts_with(b"FW: ")
            || self_.starts_with(b"Fw: ")
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
                    slice = &slice[b"RE: ".len()..];
                    continue;
                }
                if slice.starts_with(b"FWD: ")
                    || slice.starts_with(b"Fwd: ")
                    || slice.starts_with(b"fwd: ")
                {
                    slice = &slice[b"FWD: ".len()..];
                    continue;
                }
                if slice.starts_with(b" ") || slice.starts_with(b"\t") || slice.starts_with(b"\r") {
                    // [ref:FIXME]: just trim whitespace
                    slice = &slice[b" ".len()..];
                    continue;
                }
                if slice.starts_with(b"[")
                    && !(slice.starts_with(b"[PATCH") || slice.starts_with(b"[RFC"))
                {
                    if let Some(pos) = slice.find(b"]") {
                        slice = &slice[pos + 1..];
                        continue;
                    }
                }
                break;
            }
            slice
        };
        *self = result;
        self
    }

    fn strip_prefixes_from_list(&mut self, list: &[&str], mut times: Option<u8>) -> &mut Self {
        let result = {
            let mut slice = self.trim();
            'outer: loop {
                let len = slice.len();
                for prefix in list.iter() {
                    if slice
                        .get(0..prefix.len())
                        .map(|p| p.eq_ignore_ascii_case(prefix.as_bytes()))
                        .unwrap_or(false)
                    {
                        slice = &slice[prefix.len()..];
                        slice = slice.trim();
                        times = times.map(|u| u.saturating_sub(1));
                        if times == Some(0) {
                            break 'outer;
                        }
                    }
                }
                if slice.len() == len || times == Some(0) {
                    break;
                }
            }
            slice
        };
        *self = result;
        self
    }
}

impl SubjectPrefix for &str {
    fn is_a_reply(&self) -> bool {
        self.as_bytes().is_a_reply()
    }

    fn strip_prefixes(&mut self) -> &mut Self {
        let result = {
            let mut slice = self.trim();
            loop {
                if slice.starts_with("RE: ")
                    || slice.starts_with("Re: ")
                    || slice.starts_with("FW: ")
                    || slice.starts_with("Fw: ")
                {
                    slice = &slice["RE: ".len()..];
                    continue;
                }
                if slice.starts_with("FWD: ")
                    || slice.starts_with("Fwd: ")
                    || slice.starts_with("fwd: ")
                {
                    slice = &slice["FWD: ".len()..];
                    continue;
                }
                if slice.starts_with(' ') || slice.starts_with('\t') || slice.starts_with('\r') {
                    // [ref:FIXME]: just trim whitespace
                    slice = &slice[1..];
                    continue;
                }
                if slice.starts_with('[')
                    && !(slice.starts_with("[PATCH") || slice.starts_with("[RFC"))
                {
                    if let Some(pos) = slice.find(']') {
                        slice = &slice[pos + 1..];
                        continue;
                    }
                }
                break;
            }
            slice
        };
        *self = result;
        self
    }

    fn strip_prefixes_from_list(&mut self, list: &[&str], mut times: Option<u8>) -> &mut Self {
        let result = {
            let mut slice = self.trim();
            'outer: loop {
                let len = slice.len();
                for prefix in list.iter() {
                    if slice
                        .get(0..prefix.len())
                        .map(|p| p.eq_ignore_ascii_case(prefix))
                        .unwrap_or(false)
                    {
                        slice = &slice[prefix.len()..];
                        slice = slice.trim();
                        times = times.map(|u| u.saturating_sub(1));
                        if times == Some(0) {
                            break 'outer;
                        }
                    }
                }
                if slice.len() == len || times == Some(0) {
                    break;
                }
            }
            slice
        };
        *self = result;
        self
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
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
    Node { parent: Arc<RwLock<ThreadHash>> },
}

impl Default for ThreadGroup {
    fn default() -> Self {
        Self::Root(Thread::default())
    }
}

impl ThreadGroup {
    pub fn root(&self) -> Option<&Thread> {
        if let Self::Root(ref root) = self {
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
    };
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

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn set_snoozed(&mut self, val: bool) {
        self.snoozed = val;
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ThreadNode {
    pub message: Option<EnvelopeHash>,
    pub other_mailbox: bool,
    pub parent: Option<ThreadNodeHash>,
    pub children: Vec<ThreadNodeHash>,
    pub date: UnixTimestamp,
    pub show_subject: bool,
    pub group: ThreadHash,
    pub unseen: bool,
}

impl Default for ThreadNode {
    fn default() -> Self {
        Self {
            message: None,
            parent: None,
            other_mailbox: false,
            children: Vec::new(),
            date: UnixTimestamp::default(),
            show_subject: true,
            group: ThreadHash::new(),
            unseen: false,
        }
    }
}

impl ThreadNode {
    fn new() -> Self {
        Self::default()
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
    pub thread_nodes: HashMap<ThreadNodeHash, ThreadNode>,
    root_set: Arc<RwLock<Vec<ThreadNodeHash>>>,
    tree_index: Arc<RwLock<Vec<ThreadNodeHash>>>,
    pub groups: HashMap<ThreadHash, ThreadGroup>,

    message_ids: HashMap<MessageID, ThreadNodeHash>,
    pub message_ids_set: HashSet<MessageID>,
    pub missing_message_ids: HashSet<MessageID>,
    pub hash_set: HashSet<EnvelopeHash>,
    pub thread_to_envelope: HashMap<ThreadHash, Vec<EnvelopeHash>>,
    pub envelope_to_thread: HashMap<EnvelopeHash, ThreadHash>,
    sort: Arc<RwLock<(SortField, SortOrder)>>,
    subsort: Arc<RwLock<(SortField, SortOrder)>>,
}

impl PartialEq for ThreadNode {
    fn eq(&self, other: &Self) -> bool {
        match (self.message, other.message) {
            (Some(s), Some(o)) => s == o,
            _ => false,
        }
    }
}

impl Eq for ThreadNode {}

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
            ThreadGroup::Node { ref parent } => *parent.read().unwrap(),
        };

        let parent_group = self.find_group(p);
        match self.groups[&h] {
            ThreadGroup::Node { ref parent } => {
                *parent.write().unwrap() = parent_group;
            }
            _ => unreachable!(),
        }
        parent_group
    }

    pub fn new(length: usize) -> Self {
        /* To reconstruct thread information from the mails we need: */

        /* a vector to hold thread members */
        let thread_nodes: HashMap<ThreadNodeHash, ThreadNode> =
            HashMap::with_capacity_and_hasher((length as f64 * 1.2) as usize, Default::default());
        /* A hash table of Message IDs */
        let message_ids: HashMap<MessageID, ThreadNodeHash> =
            HashMap::with_capacity_and_hasher(length, Default::default());
        /* A hash set of Message IDs we haven't encountered yet as an Envelope */
        let missing_message_ids: HashSet<MessageID> =
            HashSet::with_capacity_and_hasher(length, Default::default());
        /* A hash set of Message IDs we have encountered as a MessageID */
        let message_ids_set: HashSet<MessageID> =
            HashSet::with_capacity_and_hasher(length, Default::default());
        let hash_set: HashSet<EnvelopeHash> =
            HashSet::with_capacity_and_hasher(length, Default::default());
        let thread_to_envelope: HashMap<ThreadHash, Vec<EnvelopeHash>> =
            HashMap::with_capacity_and_hasher(length, Default::default());
        let envelope_to_thread: HashMap<EnvelopeHash, ThreadHash> =
            HashMap::with_capacity_and_hasher(length, Default::default());

        Self {
            thread_nodes,
            message_ids,
            message_ids_set,
            missing_message_ids,
            hash_set,
            thread_to_envelope,
            envelope_to_thread,
            sort: Arc::new(RwLock::new((SortField::Date, SortOrder::Desc))),
            subsort: Arc::new(RwLock::new((SortField::Subject, SortOrder::Desc))),

            ..Default::default()
        }
    }

    pub fn threads_iter(&self, root_tree: SmallVec<[ThreadNodeHash; 1024]>) -> ThreadsIterator {
        ThreadsIterator {
            root_tree,
            pos: 0,
            stack: SmallVec::new(),
            thread_nodes: &self.thread_nodes,
        }
    }

    pub fn thread_iter(&self, index: ThreadHash) -> ThreadIterator {
        ThreadIterator {
            group: self.thread_ref(index).root(),
            pos: 0,
            stack: SmallVec::new(),
            thread_nodes: &self.thread_nodes,
        }
    }

    #[allow(clippy::result_unit_err)]
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
        let thread_node_hash = if let Some((key, _)) = self
            .thread_nodes
            .iter()
            .find(|(_, n)| n.message.map(|n| n == old_hash).unwrap_or(false))
        {
            *key
        } else {
            return Err(());
        };

        self.thread_nodes
            .get_mut(&thread_node_hash)
            .unwrap()
            .message = Some(new_hash);
        let was_unseen = self.thread_nodes[&thread_node_hash].unseen;
        let is_unseen = !envelopes.read().unwrap()[&new_hash].is_seen();
        if was_unseen != is_unseen {
            let Thread { ref mut unseen, .. } =
                self.thread_ref_mut(self.thread_nodes[&thread_node_hash].group);
            if was_unseen {
                *unseen -= 1;
            } else {
                *unseen += 1;
            }
        }
        self.thread_nodes.get_mut(&thread_node_hash).unwrap().unseen = is_unseen;
        self.hash_set.remove(&old_hash);
        self.hash_set.insert(new_hash);
        let thread_hash = self.envelope_to_thread.remove(&old_hash).unwrap();
        self.thread_to_envelope
            .entry(thread_hash)
            .or_default()
            .retain(|h| *h != old_hash);
        self.thread_to_envelope
            .entry(thread_hash)
            .or_default()
            .push(new_hash);
        *self.envelope_to_thread.entry(new_hash).or_default() = thread_hash;
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
            let mut tree_index = self.tree_index.write().unwrap();
            if let Some(i) = tree_index.iter().position(|t| *t == t_id) {
                tree_index.remove(i);
            }
        }
        if let Some((message_id, _)) = self.message_ids.iter().find(|(_, h)| **h == t_id) {
            self.missing_message_ids.insert(message_id.clone());
        }
    }

    pub fn amend(&mut self, envelopes: &Envelopes) {
        let envelopes_lck = envelopes.read().unwrap();
        let new_hash_set = HashSet::from_iter(envelopes_lck.keys().cloned());

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

    /// Update `show_subject` details of [`ThreadNode`].
    pub fn update_show_subject(
        &mut self,
        id: ThreadNodeHash,
        env_hash: EnvelopeHash,
        envelopes: &Envelopes,
    ) {
        let mut stack = VecDeque::new();
        stack.push_back((id, Some(env_hash)));
        let envelopes = envelopes.read().unwrap();
        while let Some((id, env_hash)) = stack.pop_front() {
            if let Some(env_hash) = env_hash {
                let mut subject = envelopes[&env_hash].subject();
                let mut subject = subject.to_mut().as_bytes();
                let stripped_subject = subject.strip_prefixes();
                {
                    // check immediate parent envelope, skipping thread nodes that have no
                    // corresponding envelope
                    let mut parent_cursor = self.thread_nodes[&id].parent;
                    while let Some(parent_id) = parent_cursor {
                        if let Some(parent_hash) = self.thread_nodes[&parent_id].message {
                            debug_assert!(envelopes.contains_key(&parent_hash));
                            /* decide if the subject should be shown in the UI.
                             * If parent subject is Foobar and reply is `Re: Foobar`
                             * then showing the reply's subject is redundant
                             */
                            let mut parent_subject = envelopes[&parent_hash].subject();
                            let mut parent_subject = parent_subject.to_mut().as_bytes();
                            parent_subject.strip_prefixes();
                            if stripped_subject == &parent_subject
                                || stripped_subject.ends_with(parent_subject)
                            {
                                self.thread_nodes.entry(id).and_modify(|e| {
                                    e.show_subject = false;
                                });
                            }
                            break;
                        } else {
                            parent_cursor = self.thread_nodes[&parent_id].parent;
                        }
                    }
                }

                for i in 0..self.thread_nodes[&id].children.len() {
                    let child_hash = self.thread_nodes[&id].children[i];
                    stack.push_back((child_hash, self.thread_nodes[&child_hash].message()));
                    if let Some(child_env_hash) = self.thread_nodes[&child_hash].message() {
                        let mut child_subject = envelopes[&child_env_hash].subject();
                        let mut child_subject = child_subject.to_mut().as_bytes();
                        child_subject.strip_prefixes();
                        if stripped_subject == &child_subject
                            || child_subject.ends_with(stripped_subject)
                        {
                            self.thread_nodes.entry(child_hash).and_modify(|e| {
                                e.show_subject = false;
                            });
                        }
                    }
                }
            } else {
                for i in 0..self.thread_nodes[&id].children.len() {
                    let child_hash = self.thread_nodes[&id].children[i];
                    stack.push_back((child_hash, self.thread_nodes[&child_hash].message()));
                }
            }
        }
    }

    pub fn insert(&mut self, envelopes: &Envelopes, env_hash: EnvelopeHash) {
        self.insert_internal(envelopes, env_hash, false);
    }

    fn insert_internal(
        &mut self,
        envelopes: &Envelopes,
        env_hash: EnvelopeHash,
        other_mailbox: bool,
    ) -> bool {
        if self.hash_set.contains(&env_hash) {
            return true;
        }

        {
            let envelopes_lck = envelopes.read().unwrap();
            if !envelopes_lck.contains_key(&env_hash) {
                return false;
            }
            let message_id = envelopes_lck[&env_hash].message_id();
            if self.message_ids.contains_key(message_id)
                && !self.missing_message_ids.contains(message_id)
            {
                let thread_hash = self.message_ids[message_id];
                let node = self.thread_nodes.entry(thread_hash).or_default();
                drop(envelopes_lck);
                envelopes
                    .write()
                    .unwrap()
                    .get_mut(&env_hash)
                    .unwrap()
                    .set_thread(thread_hash);

                /* If thread node currently has a message from a foreign mailbox and env_hash
                 * is from current mailbox we want to update it, otherwise
                 * return */
                if node.other_mailbox || other_mailbox {
                    return false;
                }
            }
        }
        let envelopes_lck = envelopes.read().unwrap();
        let message_id = envelopes_lck[&env_hash].message_id();
        let reply_to_id: Option<ThreadNodeHash> =
            envelopes_lck[&env_hash].in_reply_to().and_then(|r| {
                r.refs()
                    .iter()
                    .rev()
                    .filter(|irt| irt != &message_id)
                    .find_map(|r| self.message_ids.get(r).cloned())
            });

        if other_mailbox
            && reply_to_id.is_none()
            && !self.message_ids.contains_key(message_id)
            && !envelopes_lck[&env_hash]
                .references()
                .iter()
                .any(|r| self.message_ids.contains_key(r))
        {
            return false;
        }

        let new_id = self
            .message_ids
            .get(message_id)
            .cloned()
            .or_else(|| {
                if envelopes_lck[&env_hash].thread() != ThreadNodeHash::null() {
                    Some(envelopes_lck[&env_hash].thread())
                } else {
                    None
                }
            })
            .unwrap_or_else(|| ThreadNodeHash::from(message_id.raw()));
        {
            let node = self.thread_nodes.entry(new_id).or_default();
            node.message = Some(env_hash);
            if node.parent.is_none() {
                node.parent = reply_to_id;
            }
            node.other_mailbox = other_mailbox;
            node.date = envelopes_lck[&env_hash].date();
            node.unseen = !envelopes_lck[&env_hash].is_seen();
        }

        let thread_hash = self.thread_nodes[&new_id].group;
        if let std::collections::hash_map::Entry::Vacant(e) = self.groups.entry(thread_hash) {
            e.insert(ThreadGroup::Root(Thread {
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
            }));
        } else {
            let parent_group = self.thread_ref_mut(thread_hash);
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

        self.message_ids.insert(message_id.clone(), new_id);
        self.message_ids_set.insert(message_id.clone());
        self.missing_message_ids.remove(message_id);
        self.hash_set.insert(env_hash);
        self.thread_to_envelope
            .entry(thread_hash)
            .or_default()
            .push(env_hash);
        *self.envelope_to_thread.entry(env_hash).or_default() = thread_hash;
        if let Some(reply_to_id) = reply_to_id {
            make!((reply_to_id) parent of (new_id), self);
        } else if let Some(r) = envelopes_lck[&env_hash]
            .in_reply_to()
            .clone()
            .into_iter()
            .find_map(|r| {
                r.refs()
                    .iter()
                    .rev()
                    .filter(|irt| *irt != message_id)
                    .find(|r| {
                        self.message_ids.contains_key(r) && !self.missing_message_ids.contains(r)
                    })
                    .cloned()
            })
        {
            let reply_to_id = ThreadNodeHash::from(&r.raw());
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
            self.message_ids.insert(r.clone(), reply_to_id);
            self.message_ids_set.insert(r.clone());
            self.missing_message_ids.insert(r);
        }

        if envelopes_lck[&env_hash].references.is_some() {
            let mut current_descendant_id = new_id;
            let mut references = envelopes_lck[&env_hash].references().to_vec();
            if envelopes_lck[&env_hash]
                .in_reply_to()
                .map(|r| {
                    r.refs().last().as_ref()
                        == references.first().filter(|irt| *irt != message_id).as_ref()
                })
                .unwrap_or(false)
            {
                references.reverse();
            }

            for reference in references.into_iter().rev() {
                if &reference == message_id {
                    continue;
                }
                if let Some(&id) = self.message_ids.get(&reference) {
                    if self.thread_nodes[&id].date > self.thread_nodes[&current_descendant_id].date
                        || self.thread_nodes[&current_descendant_id].parent.is_some()
                    {
                        current_descendant_id = id;
                        continue;
                    }
                    make!((id) parent of (current_descendant_id), self);
                    current_descendant_id = id;
                } else {
                    let id = ThreadNodeHash::from(reference.raw());
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
                    self.missing_message_ids.insert(reference.clone());
                    self.message_ids.insert(reference.clone(), id);
                    self.message_ids_set.insert(reference.clone());
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
            &self.tree_index.read().unwrap(),
            &self.thread_nodes,
            &self
                .message_ids
                .iter()
                .map(|(a, &b)| (b, a.to_vec()))
                .collect::<HashMap<ThreadNodeHash, MessageID>>(),
            &envelopes,
        );
        */
        true
    }

    /* Insert or update */
    pub fn insert_reply(&mut self, envelopes: &Envelopes, env_hash: EnvelopeHash) -> bool {
        self.insert_internal(envelopes, env_hash, true)
    }

    fn inner_subsort_by(&self, _subsort: (SortField, SortOrder), _envelopes: &Envelopes) {
        // [ref:FIXME]: self\.thread_nodes needs interior mutability */
        /*
        let Threads {
            ref tree_index,
            ref thread_nodes,
            ..
        } = self;
        let tree = &mut tree_index.write().unwrap();
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
                {
                    ma.subject()
                        .split_graphemes()
                        .cmp(&mb.subject().split_graphemes())
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
                {
                    mb.subject()
                        .as_ref()
                        .split_graphemes()
                        .cmp(&ma.subject().split_graphemes())
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
                let a = self.thread_ref(self.thread_nodes[a].group).date();
                let b = self.thread_ref(self.thread_nodes[b].group).date();
                b.cmp(&a)
            }
            (SortField::Date, SortOrder::Asc) => {
                let a = self.thread_ref(self.thread_nodes[a].group).date();
                let b = self.thread_ref(self.thread_nodes[b].group).date();
                a.cmp(&b)
            }
            (SortField::Subject, SortOrder::Desc) => {
                let a = &self.thread_nodes[a].message();
                let b = &self.thread_nodes[b].message();

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
                {
                    ma.subject()
                        .split_graphemes()
                        .cmp(&mb.subject().split_graphemes())
                }
            }
            (SortField::Subject, SortOrder::Asc) => {
                let a = &self.thread_nodes[a].message();
                let b = &self.thread_nodes[b].message();

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
                {
                    mb.subject()
                        .as_ref()
                        .split_graphemes()
                        .cmp(&ma.subject().split_graphemes())
                }
            }
        });
    }
    fn inner_sort_by(&self, sort: (SortField, SortOrder), envelopes: &Envelopes) {
        let tree = &mut self.tree_index.write().unwrap();
        let envelopes = envelopes.read().unwrap();
        tree.sort_by(|a, b| match sort {
            (SortField::Date, SortOrder::Desc) => {
                let a = self.thread_ref(self.thread_nodes[a].group).date();
                let b = self.thread_ref(self.thread_nodes[b].group).date();
                b.cmp(&a)
            }
            (SortField::Date, SortOrder::Asc) => {
                let a = self.thread_ref(self.thread_nodes[a].group).date();
                let b = self.thread_ref(self.thread_nodes[b].group).date();
                a.cmp(&b)
            }
            (SortField::Subject, SortOrder::Desc) => {
                let a = &self.thread_nodes[a].message();
                let b = &self.thread_nodes[b].message();

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
                {
                    ma.subject()
                        .split_graphemes()
                        .cmp(&mb.subject().split_graphemes())
                }
            }
            (SortField::Subject, SortOrder::Asc) => {
                let a = &self.thread_nodes[a].message();
                let b = &self.thread_nodes[b].message();

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
                {
                    mb.subject()
                        .as_ref()
                        .split_graphemes()
                        .cmp(&ma.subject().split_graphemes())
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
        if *self.sort.read().unwrap() != sort {
            self.inner_sort_by(sort, envelopes);
            *self.sort.write().unwrap() = sort;
        }
        if *self.subsort.read().unwrap() != subsort {
            self.inner_subsort_by(subsort, envelopes);
            *self.subsort.write().unwrap() = subsort;
        }
    }

    pub fn thread_to_mail(&self, i: ThreadNodeHash) -> EnvelopeHash {
        let thread = &self.thread_nodes[&i];
        thread.message().unwrap()
    }

    pub fn thread_nodes(&self) -> &HashMap<ThreadNodeHash, ThreadNode> {
        &self.thread_nodes
    }

    pub fn len(&self) -> usize {
        self.hash_set.len()
    }

    pub fn is_empty(&self) -> bool {
        self.hash_set.is_empty()
    }

    pub fn root_len(&self) -> usize {
        self.tree_index.read().unwrap().len()
    }

    pub fn root_set(&self, idx: usize) -> ThreadNodeHash {
        self.tree_index.read().unwrap()[idx]
    }

    pub fn roots(&self) -> SmallVec<[ThreadHash; 1024]> {
        // [ref:FIXME]: refactor filter
        self.groups
            .iter()
            .filter_map(|(h, g)| g.root().map(|_| *h))
            .collect::<SmallVec<[ThreadHash; 1024]>>()
    }
}

impl Index<&ThreadNodeHash> for Threads {
    type Output = ThreadNode;

    fn index(&self, index: &ThreadNodeHash) -> &ThreadNode {
        self.thread_nodes.get(index).expect("thread node not found")
    }
}

/*
fn print_threadnodes(
    node_hash: ThreadNodeHash,
    nodes: &HashMap<ThreadNodeHash, ThreadNode>,
    envelopes: &Envelopes,
) {
    fn help(
        level: usize,
        node_hash: ThreadNodeHash,
        nodes: &HashMap<ThreadNodeHash, ThreadNode>,
        envelopes: &Envelopes,
    ) {
        eprint!("{}ThreadNode {}\n{}\tmessage: {}\n{}\tparent: {}\n{}\tthread_group: {}\n{}\tchildren (len: {}):\n",
                  "\t".repeat(level),
                  node_hash,
                  "\t".repeat(level),
                  nodes[&node_hash].message().as_ref().map(|m| format!("{} - {}\n{}\t\t{}", envelopes[m].message_id(), envelopes[m].subject(), "\t".repeat(level), envelopes[m].references().iter().map(ToString::to_string).collect::<Vec<String>>().join(", "))).unwrap_or_else(|| "None".to_string()),
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

// #[derive(Serialize)]
// struct Node {
//     id: String,
//     subject: String,
//     from: String,
//     to: String,
//     date: UnixTimestamp,
//     references: String,
//     in_reply_to: String,
// }

// #[derive(Serialize)]
// struct Link {
//     source: String,
//     target: String,
// }

// #[derive(Serialize)]
// struct Graph {
//     nodes: Vec<Node>,
//     links: Vec<Link>,
// }

/*
fn save_graph(
    node_arr: &[ThreadNodeHash],
    nodes: &HashMap<ThreadNodeHash, ThreadNode>,
    ids: &HashMap<ThreadNodeHash, MessageID>,
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
            crate::utils::datetime::now()
        ))
        .unwrap();
        file.write_all(s.as_bytes()).unwrap();
    }
}
*/
