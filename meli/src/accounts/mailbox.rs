//
// meli - accounts module.
//
// Copyright 2017 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use indexmap::IndexMap;
use melib::{
    backends::{Mailbox, MailboxHash},
    error::Error,
    log,
};
use smallvec::SmallVec;

use crate::{conf::FileMailboxConf, is_variant};

#[derive(Clone, Debug, Default)]
pub enum MailboxStatus {
    Available,
    Failed(Error),
    /// first argument is done work, and second is total work
    Parsing(usize, usize),
    #[default]
    None,
}

impl MailboxStatus {
    is_variant! { is_available, Available }
    is_variant! { is_parsing, Parsing(_, _) }
}

#[derive(Clone, Debug)]
pub struct MailboxEntry {
    pub status: MailboxStatus,
    pub name: String,
    pub path: String,
    pub ref_mailbox: Mailbox,
    pub conf: FileMailboxConf,
}

impl MailboxEntry {
    pub fn new(
        status: MailboxStatus,
        name: String,
        ref_mailbox: Mailbox,
        conf: FileMailboxConf,
    ) -> Self {
        let mut ret = Self {
            status,
            name,
            path: ref_mailbox.path().into(),
            ref_mailbox,
            conf,
        };
        match ret.conf.mailbox_conf.extra.get("encoding") {
            None => {}
            Some(v) if ["utf-8", "utf8"].iter().any(|e| v.eq_ignore_ascii_case(e)) => {}
            Some(v) if ["utf-7", "utf7"].iter().any(|e| v.eq_ignore_ascii_case(e)) => {
                ret.name = melib::backends::utf7::decode_utf7_imap(&ret.name);
                ret.path = melib::backends::utf7::decode_utf7_imap(&ret.path);
            }
            Some(other) => {
                log::warn!(
                    "mailbox `{}`: unrecognized mailbox name charset: {}",
                    &ret.name,
                    other
                );
            }
        }
        ret
    }

    pub fn status(&self) -> String {
        match self.status {
            MailboxStatus::Available => format!(
                "{} [{} messages]",
                self.name(),
                self.ref_mailbox.count().ok().unwrap_or((0, 0)).1
            ),
            MailboxStatus::Failed(ref e) => e.to_string(),
            MailboxStatus::None => "Retrieving mailbox.".to_string(),
            MailboxStatus::Parsing(done, total) => {
                format!("Parsing messages. [{}/{}]", done, total)
            }
        }
    }

    pub fn name(&self) -> &str {
        if let Some(name) = self.conf.mailbox_conf.alias.as_ref() {
            name
        } else {
            self.ref_mailbox.name()
        }
    }
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct MailboxNode {
    pub hash: MailboxHash,
    pub depth: usize,
    pub indentation: u32,
    pub has_sibling: bool,
    pub children: Vec<MailboxNode>,
}

pub fn build_mailboxes_order(
    tree: &mut Vec<MailboxNode>,
    mailbox_entries: &IndexMap<MailboxHash, MailboxEntry>,
    mailboxes_order: &mut Vec<MailboxHash>,
) {
    tree.clear();
    mailboxes_order.clear();
    for (h, f) in mailbox_entries.iter() {
        if f.ref_mailbox.parent().is_none() {
            fn rec(
                h: MailboxHash,
                mailbox_entries: &IndexMap<MailboxHash, MailboxEntry>,
                depth: usize,
            ) -> MailboxNode {
                let mut node = MailboxNode {
                    hash: h,
                    children: Vec::new(),
                    depth,
                    indentation: 0,
                    has_sibling: false,
                };
                for &c in mailbox_entries[&h].ref_mailbox.children() {
                    if mailbox_entries.contains_key(&c) {
                        node.children.push(rec(c, mailbox_entries, depth + 1));
                    }
                }
                node
            }

            tree.push(rec(*h, mailbox_entries, 0));
        }
    }

    macro_rules! mailbox_eq_key {
        ($mailbox:expr) => {{
            if let Some(sort_order) = $mailbox.conf.mailbox_conf.sort_order {
                (0, sort_order, $mailbox.ref_mailbox.path())
            } else {
                (1, 0, $mailbox.ref_mailbox.path())
            }
        }};
    }
    tree.sort_unstable_by(|a, b| {
        if mailbox_entries[&b.hash]
            .conf
            .mailbox_conf
            .sort_order
            .is_none()
            && mailbox_entries[&b.hash]
                .ref_mailbox
                .path()
                .eq_ignore_ascii_case("INBOX")
        {
            std::cmp::Ordering::Greater
        } else if mailbox_entries[&a.hash]
            .conf
            .mailbox_conf
            .sort_order
            .is_none()
            && mailbox_entries[&a.hash]
                .ref_mailbox
                .path()
                .eq_ignore_ascii_case("INBOX")
        {
            std::cmp::Ordering::Less
        } else {
            mailbox_eq_key!(mailbox_entries[&a.hash])
                .cmp(&mailbox_eq_key!(mailbox_entries[&b.hash]))
        }
    });

    let mut stack: SmallVec<[Option<&MailboxNode>; 16]> = SmallVec::new();
    for n in tree.iter_mut() {
        mailboxes_order.push(n.hash);
        n.children.sort_unstable_by(|a, b| {
            if mailbox_entries[&b.hash]
                .conf
                .mailbox_conf
                .sort_order
                .is_none()
                && mailbox_entries[&b.hash]
                    .ref_mailbox
                    .path()
                    .eq_ignore_ascii_case("INBOX")
            {
                std::cmp::Ordering::Greater
            } else if mailbox_entries[&a.hash]
                .conf
                .mailbox_conf
                .sort_order
                .is_none()
                && mailbox_entries[&a.hash]
                    .ref_mailbox
                    .path()
                    .eq_ignore_ascii_case("INBOX")
            {
                std::cmp::Ordering::Less
            } else {
                mailbox_eq_key!(mailbox_entries[&a.hash])
                    .cmp(&mailbox_eq_key!(mailbox_entries[&b.hash]))
            }
        });
        stack.extend(n.children.iter().rev().map(Some));
        while let Some(Some(next)) = stack.pop() {
            mailboxes_order.push(next.hash);
            stack.extend(next.children.iter().rev().map(Some));
        }
    }
    drop(stack);
    for node in tree.iter_mut() {
        fn rec(
            node: &mut MailboxNode,
            mailbox_entries: &IndexMap<MailboxHash, MailboxEntry>,
            mut indentation: u32,
            has_sibling: bool,
        ) {
            node.indentation = indentation;
            node.has_sibling = has_sibling;
            let mut iter = (0..node.children.len())
                .filter(|i| {
                    mailbox_entries[&node.children[*i].hash]
                        .ref_mailbox
                        .is_subscribed()
                })
                .collect::<SmallVec<[_; 8]>>()
                .into_iter()
                .peekable();
            indentation <<= 1;
            if has_sibling {
                indentation |= 1;
            }
            while let Some(i) = iter.next() {
                let c = &mut node.children[i];
                rec(c, mailbox_entries, indentation, iter.peek().is_some());
            }
        }

        rec(node, mailbox_entries, 0, false);
    }
}
