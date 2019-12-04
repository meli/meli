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

#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MailboxObject {
    pub id: String,
    pub is_subscribed: bool,
    pub my_rights: JmapRights,
    pub name: String,
    pub parent_id: Option<String>,
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
#[derive(Deserialize, Serialize, Debug)]
#[serde(rename_all = "camelCase")]
pub struct MailboxGet {
    #[serde(flatten)]
    pub get_call: Get<MailboxObject>,
}

impl Method<MailboxObject> for MailboxGet {
    const NAME: &'static str = "Mailbox/query";
}
