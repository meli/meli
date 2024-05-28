/*
 * meli - jmap module.
 *
 * Copyright 2019-2022 Manos Pitsidianakis
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

use std::marker::PhantomData;

use crate::jmap::{
    email::{EmailGet, EmailObject},
    protocol::Method,
    rfc8620::{Changes, Get, Id, Object, ResultField},
};

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ThreadObject {
    #[serde(default)]
    pub id: Id<ThreadObject>,
    #[serde(default)]
    pub email_ids: Vec<Id<EmailObject>>,
}

impl Object for ThreadObject {
    const NAME: &'static str = "Thread";
}

impl ThreadObject {
    _impl!(get email_ids, email_ids: Vec<Id<EmailObject>>);
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ThreadGet {
    #[serde(flatten)]
    pub get_call: Get<ThreadObject>,
}

impl Method<ThreadObject> for ThreadGet {
    const NAME: &'static str = "Thread/get";
}

impl ThreadGet {
    pub const RESULT_FIELD_THREAD_IDS: ResultField<EmailGet, EmailObject> =
        ResultField::<EmailGet, EmailObject> {
            field: "/list/*/threadId",
            _ph: PhantomData,
        };

    pub fn new(get_call: Get<ThreadObject>) -> Self {
        Self { get_call }
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ThreadChanges {
    #[serde(flatten)]
    pub changes_call: Changes<ThreadObject>,
}

impl Method<ThreadObject> for ThreadChanges {
    const NAME: &'static str = "Thread/changes";
}

impl ThreadChanges {
    pub fn new(changes_call: Changes<ThreadObject>) -> Self {
        Self { changes_call }
    }
}
