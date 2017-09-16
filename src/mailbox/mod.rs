/*
 * meli - mailbox module.
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

pub mod email;
pub use self::email::*;
/* Mail backends. Currently only maildir is supported */
pub mod backends;
use mailbox::backends::MailBackend;
use mailbox::backends::maildir;
use error::Result;
pub mod accounts;
pub use mailbox::accounts::Account;

extern crate fnv;

use self::fnv::FnvHashMap;
use std::option::Option;
use std;

type UnixTimestamp = i64;

/*a Mailbox represents a folder of mail. Currently only Maildir is supported.*/
#[derive(Debug,Clone)]
pub struct Mailbox{
    pub path: String,
    pub collection: Vec<Mail>,
    pub threaded_collection: Vec<usize>,
    threads: Vec<Thread>,
    length: usize,
}

/* a Thread struct is needed to describe the Thread tree forest during creation
 * of threads. Because of Rust's memory model, we store indexes of other node
 * instead of references and every reference is passed through the Thread owner
 * (a Vec<Thread>).
 *
 * message refers to a Mail entry in a Vec. If it's empty, the Thread is
 * nonexistent in our Mailbox but we know it exists (for example we have a copy
 * of a reply to a mail but we don't have its copy.
 */
#[derive(Clone, Copy, Debug)]
pub struct Thread {
    id: usize,
    message: Option<usize>,
    parent: Option<usize>,
    first_child: Option<usize>,
    next_sibling: Option<usize>,
    date: UnixTimestamp,
    indentation: usize,
    show_subject: bool,
}

impl Thread {
    pub fn get_message(&self) -> Option<usize> {
        self.message
    }
    pub fn get_parent(&self) -> Option<usize> {
        self.parent
    }
    pub fn has_parent(&self) -> bool {
        self.parent.is_some()
    }
    pub fn get_first_child(&self) -> Option<usize> {
        self.first_child
    }
    pub fn get_next_sibling(&self) -> Option<usize> {
        self.next_sibling
    }
    pub fn has_children(&self) -> bool {
        self.first_child.is_some()
    }
    pub fn has_sibling(&self) -> bool {
        self.next_sibling.is_some()
    }
    pub fn has_message(&self) -> bool {
        self.message.is_some()
    }
    fn set_indentation(&mut self, i: usize) {
        self.indentation = i;
    }
    pub fn get_indentation(&self) -> usize {
        self.indentation
    }
    fn is_descendant(&self, threads: &[Thread], other: &Thread) -> bool {
        if self == other {
            return true;
        }

        if let Some(v) = self.first_child {
            if threads[v].is_descendant(threads, other) {
                return true;
            }
        };
        if let Some(v) = self.next_sibling {
            if threads[v].is_descendant(threads, other) {
                return true;
            }
        };
        false
    }
    fn set_show_subject(&mut self, set: bool) -> () {
        self.show_subject = set;
    }
    pub fn get_show_subject(&self) -> bool  {
        self.show_subject
    }
}

impl PartialEq for Thread {
    fn eq(&self, other: &Thread) -> bool {
        match (self.message, other.message) {
            (Some(s), Some(o)) => {
                s == o
            },
            _ => {
                self.id == other.id
            }
        }
    }
}

fn build_collection(threads: &mut Vec<Thread>, id_table: &mut FnvHashMap<std::string::String, usize>, collection: &mut [Mail]) -> () 
{
    for (i, x) in collection.iter_mut().enumerate() {
        let x_index; /* x's index in threads */
        let m_id = x.get_message_id_raw().to_string();
        if id_table.contains_key(&m_id) {
            let t = id_table[&m_id];
            /* the already existing Thread should be empty, since we're
             * seeing this message for the first time */
            if threads[t].message.is_some() {
                /* skip duplicate message-id, but this should be handled instead */
                continue;
            }
            x_index = t;
            /* Store this message in the Thread's message slot.  */
            threads[t].date = x.get_date();
            x.set_thread(t);
            threads[t].message = Some(i);
        } else {
            /* Create a new Thread object holding this message */
            x_index = threads.len();
            threads.push(
                Thread {
                    message: Some(i),
                    id: x_index,
                    parent: None,
                    first_child: None,
                    next_sibling: None,
                    date: x.get_date(),
                    indentation: 0,
                    show_subject: true,
                });
            x.set_thread(x_index);
            id_table.insert(m_id, x_index);
        }
        /* For each element in the message's References field:
         *
         * Find a Thread object for the given Message-ID:
         * If there's one in id_table use that;
         * Otherwise, make (and index) one with a null Message
         *
         * Link the References field's Threads together in the order implied by the References header.
         * If they are already linked, don't change the existing links.
         * Do not add a link if adding that link would introduce a loop: that is, before asserting A->B, search down the children of B to see if A is reachable, and also search down the children of A to see if B is reachable. If either is already reachable as a child of the other, don't add the link.
         */
        let mut curr_ref = x_index;
        let mut iasf = 0;
        for &r in x.get_references().iter().rev() {
            if iasf == 1 {
                continue;
            }
            iasf += 1;
            let parent_id =
                if id_table.contains_key(r.get_raw()) {
                    let p = id_table[r.get_raw()];
                    if !(threads[p].is_descendant(threads, &threads[curr_ref]) ||
                         threads[curr_ref].is_descendant(threads, &threads[p])) {
                        threads[curr_ref].parent = Some(p);
                        if threads[p].first_child.is_none() {
                            threads[p].first_child = Some(curr_ref);
                        } else {
                            let mut child_iter = threads[p].first_child.unwrap();
                            while threads[child_iter].next_sibling.is_some() {
                                threads[child_iter].parent = Some(p);
                                child_iter = threads[child_iter].next_sibling.unwrap();
                            }
                            threads[child_iter].next_sibling = Some(curr_ref);
                            threads[child_iter].parent = Some(p);
                        }
                    }
                    p
                } else {
                    let idx = threads.len();
                    threads.push(
                        Thread {
                            message: None,
                            id: idx,
                            parent: None,
                            first_child: Some(curr_ref),
                            next_sibling: None,
                            date: x.get_date(),
                            indentation: 0,
                            show_subject: true,
                        });
                    if threads[curr_ref].parent.is_none() {
                        threads[curr_ref].parent = Some(idx);
                    }
                    id_table.insert(r.get_raw().to_string(), idx);
                    idx
                };
            /* update thread date */
            let mut parent_iter = parent_id;
            'date: loop {
                let p = &mut threads[parent_iter];
                if p.date < x.get_date() {
                    p.date = x.get_date();
                }
                match p.parent {
                    Some(p) => { parent_iter = p; },
                    None => { break 'date; },
                }
            }
            curr_ref = parent_id;
        }
    }
}

impl Mailbox
{
    pub fn new(path: &str, sent_folder: &Option<Result<Mailbox>>) -> Result<Mailbox> {
        let mut collection: Vec<Mail> = maildir::MaildirType::new(path).get()?;
        /* To reconstruct thread information from the mails we need: */

        /* a vector to hold thread members */
        let mut threads: Vec<Thread> = Vec::with_capacity((collection.len() as f64 * 1.2) as usize);
        /* A hash table of Message IDs */
        let mut id_table: FnvHashMap<std::string::String, usize> = FnvHashMap::with_capacity_and_hasher(collection.len(), Default::default());

        collection.sort_by(|a, b| a.get_date().cmp(&b.get_date()));
        /* Add each message to id_table and threads, and link them together according to the
         * References / In-Reply-To headers */
        build_collection(&mut threads, &mut id_table, &mut collection);
        let mut idx = collection.len();
        let mut tidx = threads.len();
        /* Link messages from Sent folder if they are relevant to this folder.
         * This means that
         *  - if a message from Sent is a reply to a message in this folder, we will
         *    add it to the threading (but not the collection; non-threading users shouldn't care
         *    about this)
         *  - if a message in this folder is a reply to a message we sent, we will add it to the
         *    threading
         */

        if let &Some(ref sent_box) = sent_folder {
            if sent_box.is_ok() {
                let sent_mailbox = sent_box.as_ref();
                let sent_mailbox = sent_mailbox.unwrap();;

                for ref x in &sent_mailbox.collection {
                    if id_table.contains_key(x.get_message_id_raw()) ||
                        (!x.get_in_reply_to_raw().is_empty() && id_table.contains_key(x.get_in_reply_to_raw())) {
                            let mut x: Mail = (*x).clone();
                            if id_table.contains_key(x.get_message_id_raw()) {
                                let c = id_table[x.get_message_id_raw()];
                                if threads[c].message.is_some() {
                                    /* skip duplicate message-id, but this should be handled instead */
                                    continue;
                                }
                                threads[c].message = Some(idx);
                                assert!(threads[c].has_children());
                                threads[c].date = x.get_date();
                                x.set_thread(c);
                            } else if !x.get_in_reply_to_raw().is_empty() && id_table.contains_key(x.get_in_reply_to_raw()) {
                                let p = id_table[x.get_in_reply_to_raw()];
                                let c = if id_table.contains_key(x.get_message_id_raw()) {
                                    id_table[x.get_message_id_raw()]
                                } else {
                                    threads.push(
                                        Thread {
                                            message: Some(idx),
                                            id: tidx,
                                            parent: Some(p),
                                            first_child: None,
                                            next_sibling: None,
                                            date: x.get_date(),
                                            indentation: 0,
                                            show_subject: true,
                                        });
                                    id_table.insert(x.get_message_id_raw().to_string(), tidx);
                                    x.set_thread(tidx);
                                    tidx += 1;
                                    tidx - 1
                                };
                                threads[c].parent = Some(p);
                                if threads[p].is_descendant(&threads, &threads[c]) ||
                                    threads[c].is_descendant(&threads, &threads[p]) {
                                        continue;
                                    }
                                if threads[p].first_child.is_none() {
                                    threads[p].first_child = Some(c);
                                } else {
                                    let mut fc = threads[p].first_child.unwrap();
                                    while threads[fc].next_sibling.is_some() {
                                        threads[fc].parent = Some(p);
                                        fc = threads[fc].next_sibling.unwrap();
                                    }
                                    threads[fc].next_sibling = Some(c);
                                    threads[fc].parent = Some(p);
                                }
                                /* update thread date */
                                let mut parent_iter = p;
                                'date: loop {
                                    let p = &mut threads[parent_iter];
                                    if p.date < x.get_date() {
                                        p.date = x.get_date();
                                    }
                                    match p.parent {
                                        Some(p) => { parent_iter = p; },
                                        None => { break 'date; },
                                    }
                                }
                            }
                            collection.push(x);
                            idx += 1;
                        }
                }
            }
        }
            /* Walk over the elements of id_table, and gather a list of the Thread objects that have
             * no parents. These are the root messages of each thread */
        let mut root_set = Vec::with_capacity(collection.len());
        'root_set: for v in id_table.values() {
            if threads[*v].parent.is_none() {
                if !threads[*v].has_message() && threads[*v].has_children() && !threads[threads[*v].first_child.unwrap()].has_sibling() {
                    /* Do not promote the children if doing so would promote them to the root set
                     * -- unless there is only one child, in which case, do. */
                    root_set.push(threads[*v].first_child.unwrap());
                    continue 'root_set;
                }
                root_set.push(*v);
            }
        }
        root_set.sort_by(|a, b| threads[*b].date.cmp(&threads[*a].date));

        /* Group messages together by thread in a collection so we can print them together */
        let mut threaded_collection: Vec<usize> = Vec::with_capacity(collection.len());
        fn build_threaded(threads: &mut Vec<Thread>, indentation: usize, threaded: &mut Vec<usize>, i: usize, root_subject_idx: usize, collection: &[Mail])
            {
            let thread = threads[i];
            if threads[root_subject_idx].has_message() {
                let root_subject = collection[threads[root_subject_idx].get_message().unwrap()].get_subject();
                /* If the Container has no Message, but does have children, remove this container but
                 * promote its children to this level (that is, splice them in to the current child
                 * list.) */
                if indentation > 0 && thread.has_message() {
                    let subject = collection[thread.get_message().unwrap()].get_subject();
                    if subject == root_subject || subject.starts_with("Re: ") && subject.ends_with(root_subject) {
                        threads[i].set_show_subject(false);
                    }
                }
            }
            if thread.has_parent() && !threads[thread.get_parent().unwrap()].has_message() {
                threads[i].parent = None;
            }
            let indentation =
                if thread.has_message() {
                    threads[i].set_indentation(indentation);
                    if !threaded.contains(&i) {
                        threaded.push(i);
                    }
                    indentation + 1
                } else if indentation > 0 {
                    indentation
                } else {
                    indentation + 1
                };
            if thread.has_children() {
                let mut fc = thread.get_first_child().unwrap();
                loop {
                    build_threaded(threads, indentation, threaded, fc, i, collection);
                    let thread_ = threads[fc];
                    if !thread_.has_sibling() {
                        break;
                    }
                    fc = thread_.get_next_sibling().unwrap();
                }
            }
        }
        for i in &root_set {
            build_threaded(&mut threads, 0, &mut threaded_collection, *i, *i, &collection);
        }

        let length = collection.len();

        Ok(Mailbox {
            path: path.to_string(),
            collection: collection,
            threads: threads,
            length: length,
            threaded_collection: threaded_collection,
        })
    }
    pub fn get_length(&self) -> usize {
        self.length
    }
    pub fn get_threaded_mail(&self, i: usize) -> usize {
        let thread = self.threads[self.threaded_collection[i]];
        thread.get_message().unwrap()
    }
    pub fn get_mail_and_thread(&mut self, i: usize) -> (&mut Mail, Thread) {
            let x = &mut self.collection.as_mut_slice()[i];
            let thread = self.threads[x.get_thread()];
            (x, thread)
    }
    pub fn get_thread(&self, i: usize) -> &Thread {
        &self.threads[i]
    }
}

