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

extern crate maildir;
extern crate mailparse;
use self::mailparse::*;
use std::cmp::Ordering;
use std::option::Option;
use std::collections::HashMap;
use std;

type UnixTimestamp = i64;

pub struct Mail {
    entry: maildir::MailEntry,
    subject: std::string::String,
    pub message_id: std::string::String,
    pub references: Vec<std::string::String>,
    date: UnixTimestamp,
    thread: usize,
}

/*a Mailbox represents a folder of mail. Currently only Maildir is supported.*/
pub struct Mailbox{
    pub collection: Box<Vec<Mail>>,
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
#[derive(Clone,Copy,Debug)]
pub struct Thread {
    id: usize,
    message: Option<usize>, 
    parent: Option<usize>,
    first_child: Option<usize>,
    next_sibling: Option<usize>,
    date: UnixTimestamp,
    indentation: usize,
}

impl Thread {
    pub fn get_message(&self) -> Option<usize> {
        self.message
    }
    pub fn get_parent(&self) -> Option<usize> {
        self.parent
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
    pub fn set_indentation(&mut self, i: usize) {
        self.indentation = i;
    }
    pub fn get_indentation(&self) -> usize {
        self.indentation
    }
    pub fn is_descendant(&self, threads: &Vec<Thread>, other: Thread) -> bool {
        match self.first_child {
            Some(v) => { 
                if threads[v] == other {
                    return true;
                }
                if threads[v].clone().is_descendant(&threads, other) {
                    return true;
                }
            },
            None => {}
        }
        match self.next_sibling {
            Some(v) => { 
                if threads[v] == other {
                    return true;
                }
                if threads[v].clone().is_descendant(threads, other) {
                    return true;
                }
            },
            None => {}
        }

        return false;


    }
}

impl PartialEq for Thread {
    fn eq(&self, other: &Thread) -> bool {
        self.id == other.id
    }
}

impl Mailbox {
    pub fn new(path: &str) -> Mailbox {
        let maildir = maildir::Maildir::from(path);
        let iter = maildir.list_cur();
        let mut collection: Box<Vec<Mail>> = Box::new(Vec::new());
        let mut threads: Vec<Thread> = Vec::new();
        let mut id_table: HashMap<std::string::String, usize> = HashMap::new();
        let mut idx = 0;
        for x in iter {
            let mut e = x.unwrap();
            let d = e.headers().unwrap().get_first_value("Date").unwrap();

            let m_id = match e.headers().unwrap().get_first_value("Message-Id") {
                Ok(v) => {
                    match v {
                        Some(v) => {
                            v
                        },
                        None => idx.to_string()
                    }
                }
                Err(_) => {
                    idx.to_string()
                }
            };
            let mut references: Vec<std::string::String> = Vec::new();
            match e.headers().unwrap().get_first_value("References") {
                Ok(v) => {
                    match v {
                        Some(v) => {
                            references.append(&mut v.split_whitespace().map(|x| x.to_string()).collect());
                        }
                        None => {}
                    }
                }
                Err(_) => {
                }
            };
            match e.headers().unwrap().get_first_value("In-Reply-To:") {
                Ok(v) => {
                    match v {
                        Some(v) => {
                            references.push(v);
                        }
                        None => {}
                    }
                }
                Err(_) => {
                }
            };
            let subject = match e.headers().unwrap().get_first_value("Subject") {
                Ok(v) => {
                    match v
                    {
                        Some(v) => v.clone(),
                        None => std::string::String::from("")
                    }
                },
                Err(x) => panic!(x)
            };
            collection.push(
                Mail {
                    entry: e,
                    subject: subject,
                    references: references,
                    message_id: m_id,
                    date: dateparse(&d.unwrap()).unwrap(),
                    thread: 0,
                });
            idx += 1;
        }
        idx = 0;

        collection.sort_by(|a, b| b.date.cmp(&a.date));
        for (i, x) in collection.iter_mut().enumerate() {
            let x_index;
            let m_id = x.message_id.clone();
            if id_table.contains_key(&m_id) {
                let c = id_table.get(&m_id).unwrap();
                /* the already existing Thread should be empty, since we're
                 * seeing this message for the first time
                 * Store this message in the Thread's message slot.  */
                threads[*c].message = Some(i);
                threads[*c].date = x.date;
                x.thread = *c;
                x_index = *c;
            } else {
                /* Create a new Thread object holding this message */
                threads.push(
                    Thread {
                        message: Some(i),
                        id: idx,
                        parent: None,
                        first_child: None,
                        next_sibling: None,
                        date: x.date,
                        indentation: 0,
                    });
                x_index = idx;
                x.thread = idx;
                id_table.insert(m_id, x_index);
                idx += 1;
            }
            /* For each element in the message's References field:
             *
             * Find a Thread object for the given Message-ID:
             * If there's one in id_table use that;
             * Otherwise, make (and index) one with a null Message. 
             *
             * Link the References field's Threads together in the order implied by the References header.
             * If they are already linked, don't change the existing links.
             * Do not add a link if adding that link would introduce a loop: that is, before asserting A->B, search down the children of B to see if A is reachable, and also search down the children of A to see if B is reachable. If either is already reachable as a child of the other, don't add the link. 
             */
            if x.references.len() == 0 {
                continue;
            }
            let r_to = x.references[x.references.len() - 1].clone();
            let parent_id = 
                if id_table.contains_key(&r_to) {

                    let p = id_table.get(&r_to).unwrap();
                    if threads[*p].is_descendant(&threads, threads[x_index]) ||
                       threads[x_index].is_descendant(&threads, threads[*p]) {
                            continue;
                       }
                    if threads[*p].first_child.is_none() {
                        threads[*p].first_child = Some(x_index);
                    } else {
                        let mut fc = threads[*p].first_child.unwrap();
                        while threads[fc].next_sibling.is_some() {
                            fc = threads[fc].next_sibling.unwrap();
                        }
                        threads[fc].next_sibling = Some(x_index);
                        threads[fc].parent = Some(*p);
                    }
                    *p
                } else {
                    threads.push(
                        Thread {
                            message: None,
                            id: idx,
                            parent: None,
                            first_child: Some(x_index),
                            next_sibling: None,
                            date: x.date,
                            indentation: 0,
                        });
                    id_table.insert(r_to.clone(), idx);
                    idx += 1;
                    idx-1
                };
            /* update thread date */
            let mut parent_iter = parent_id;
            loop {
                let mut p = &mut threads[parent_iter];
                p.date = x.date;
                if p.parent.is_none() {
                    break;
                } else {
                    parent_iter = p.get_parent().unwrap();
                }
            }
            threads[x_index].parent = Some(parent_id);
        }
        /* Walk over the elements of id_table, and gather a list of the Thread objects that have
         * no parents. */
        let mut root_set = Vec::new();
        for (_,v) in id_table.iter() {
            if threads[*v].parent.is_none() {
                root_set.push(*v);
            } 
        }
        root_set.sort_by(|a, b| threads[*b].date.cmp(&threads[*a].date));

        let mut threaded_collection: Vec<usize> = Vec::new();
        fn build_threaded(threads: &mut Vec<Thread>, indentation: usize, threaded: &mut Vec<usize>, index: usize) {

            let thread = threads[index];

            if thread.has_message() {
                threads[index].set_indentation(indentation);
                if !threaded.contains(&index) {
                    threaded.push(index);
                }
            }
            if thread.has_children() {
                let mut fc = thread.get_first_child().unwrap();
                loop {
                    build_threaded(threads, indentation + 1, threaded, fc);
                    let thread_ = threads[fc];
                    if !thread_.has_sibling() {
                        break;
                    }
                    fc = thread_.get_next_sibling().unwrap();
                }
            }
        }
        for i in &root_set {
            build_threaded(&mut threads, 0, &mut threaded_collection, *i);
        }

        let length = collection.len();
        Mailbox {
            collection: collection,
            threads: threads,
            length: length,
            threaded_collection: threaded_collection,
        }
    }
    pub fn get_length(&self) -> usize {
        self.length
    }
    pub fn get_threaded_mail(&self, i: usize) -> usize {
        let thread = self.threads[self.threaded_collection[i]];
        thread.get_message().unwrap()
    }
    pub fn get_mail_and_thread(&mut self, i: usize) -> (&mut Mail, Thread) {
        
            let ref mut x = self.collection.as_mut_slice()[i];
            let thread = self.threads[x.get_thread()].clone();
            (x, thread)
    }
    pub fn get_thread(&self, i: usize) -> Thread {
        self.threads[i].clone()
    }
}

impl Mail {
    pub fn get_entry(&mut self) -> &mut maildir::MailEntry {
        &mut self.entry
    }
    pub fn get_date(&self) -> i64 {
        self.date
    }
    pub fn get_subject(&self) -> &str {
        &self.subject
    }
    pub fn get_thread(&self) -> usize {
        self.thread
    }
}

impl Eq for Mail {}
impl Ord for  Mail {
    fn cmp(&self, other: &Mail) -> Ordering {
        self.date.cmp(&other.date)
    }
}
impl PartialOrd for Mail {
    fn partial_cmp(&self, other: &Mail) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Mail {
    fn eq(&self, other: &Mail) -> bool {
        self.date == other.date
    }
}
