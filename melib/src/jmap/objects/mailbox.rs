/*
 * meli - jmap module.
 *
 * Copyright 2019 Manos Pitsidianakis
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

use super::*;

impl Id<MailboxObject> {
    pub fn into_hash(&self) -> MailboxHash {
        MailboxHash::from_bytes(self.inner.as_bytes())
    }
}

#[derive(Deserialize, Serialize, Debug, Default)]
#[serde(rename_all = "camelCase")]
pub struct MailboxObject {
    pub id: Id<MailboxObject>,
    pub is_subscribed: bool,
    pub my_rights: JmapRights,
    pub name: String,
    pub parent_id: Option<Id<MailboxObject>>,
    pub role: Option<String>,
    pub sort_order: u64,
    pub total_emails: u64,
    pub total_threads: u64,
    pub unread_emails: u64,
    pub unread_threads: u64,
}

impl Object for MailboxObject {
    const NAME: &'static str = "Mailbox";
}

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(rename_all = "camelCase")]
pub struct JmapRights {
    pub may_add_items: bool,
    pub may_create_child: bool,
    pub may_delete: bool,
    pub may_read_items: bool,
    pub may_remove_items: bool,
    pub may_rename: bool,
    pub may_set_keywords: bool,
    pub may_set_seen: bool,
    pub may_submit: bool,
}

impl Default for JmapRights {
    fn default() -> Self {
        Self {
            may_add_items: true,
            may_create_child: true,
            may_delete: true,
            may_read_items: true,
            may_remove_items: true,
            may_rename: true,
            may_set_keywords: true,
            may_set_seen: true,
            may_submit: true,
        }
    }
}

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MailboxGet {
    #[serde(flatten)]
    pub get_call: Get<MailboxObject>,
}
impl MailboxGet {
    pub fn new(get_call: Get<MailboxObject>) -> Self {
        Self { get_call }
    }
}

impl Method<MailboxObject> for MailboxGet {
    const NAME: &'static str = "Mailbox/get";
}

/// 2.5.  Mailbox/set
///
/// This is a standard `/set` method as described in `[RFC8620]`,
/// Section 5.3 but with the following additional request argument:
///
///
/// The following extra SetError types are defined:
///
/// For `destroy`:
///
/// - `mailboxHasChild`: The Mailbox still has at least one child Mailbox.  The
///   client MUST remove these before it can delete the parent Mailbox.
///
/// - `mailboxHasEmail`: The Mailbox has at least one Email assigned to it, and
///   the `onDestroyRemoveEmails` argument was false.
#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MailboxSet {
    #[serde(flatten)]
    pub set_call: Set<MailboxObject>,
    /// onDestroyRemoveEmails: `Boolean` (default: false)
    ///
    /// If false, any attempt to destroy a Mailbox that still has Emails
    /// in it will be rejected with a `mailboxHasEmail` SetError.  If
    /// true, any Emails that were in the Mailbox will be removed from it,
    /// and if in no other Mailboxes, they will be destroyed when the
    /// Mailbox is destroyed.
    #[serde(default)]
    pub on_destroy_remove_emails: bool,
}

impl MailboxSet {
    pub fn new(set_call: Set<MailboxObject>) -> Self {
        Self {
            set_call,
            on_destroy_remove_emails: false,
        }
    }
}

impl Method<MailboxObject> for MailboxSet {
    const NAME: &'static str = "Mailbox/set";
}
