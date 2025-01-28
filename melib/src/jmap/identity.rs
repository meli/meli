/*
 * meli - jmap module.
 *
 * Copyright 2023 Manos Pitsidianakis
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

use crate::jmap::{
    email::EmailAddress,
    methods::{Changes, Get, Set},
    objects::{Id, Object},
    protocol::Method,
};

/// # Identity
///
/// An *Identity* object stores information about an email address or domain the
/// user may send from.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct Identity {
    ///  id: `Id` (immutable; server-set)
    /// The id of the `Identity`.
    #[serde(default)]
    pub id: Id<Identity>,

    ///  name: `String` (default: "")
    /// The "From" name the client SHOULD use when creating a new
    /// [`Email`](crate::jmap::email::EmailObject) from this Identity.
    #[serde(default)]
    pub name: String,

    ///  email: `String` (immutable)
    /// The "From" email address the client MUST use when creating a new
    /// [`Email`](crate::jmap::email::EmailObject) from this Identity. If the
    /// "mailbox" part of the address (the section before the "@") is the
    /// single character "*" (e.g., "*@example.com"), the client may use any
    /// valid address ending in that domain (e.g., "[email protected]").
    pub email: String,

    ///  replyTo: `EmailAddress[]|null` (default: null)
    /// The Reply-To value the client SHOULD set when creating a new
    /// [`Email`](crate::jmap::email::EmailObject) from this Identity.
    #[serde(default)]
    pub reply_to: Option<Vec<EmailAddress>>,

    ///  bcc: `EmailAddress[]|null` (default: null)
    /// The Bcc value the client SHOULD set when creating a new
    /// [`Email`](crate::jmap::email::EmailObject) from this Identity.
    #[serde(default)]
    pub bcc: Option<Vec<EmailAddress>>,

    ///  textSignature: `String` (default: "")
    /// A signature the client SHOULD insert into new plaintext messages
    /// that will be sent from this Identity. Clients MAY ignore this
    /// and/or combine this with a client-specific signature preference.
    #[serde(default)]
    pub text_signature: String,

    ///  htmlSignature: `String` (default: "")
    /// A signature the client SHOULD insert into new HTML messages that
    /// will be sent from this Identity. This text MUST be an HTML
    /// snippet to be inserted into the `<body></body>` section of the
    /// HTML. Clients MAY ignore this and/or combine this with a client-
    /// specific signature preference.
    #[serde(default)]
    pub html_signature: String,

    ///  mayDelete: `Boolean` (server-set)
    /// Is the user allowed to delete this Identity? Servers may wish to
    /// set this to `false` for the user's username or other default
    /// address. Attempts to destroy an Identity with "mayDelete: false"
    /// will be rejected with a standard "forbidden"
    /// [`crate::jmap::methods::SetError`].
    #[serde(skip_serializing)]
    pub may_delete: bool,
}

impl Object for Identity {
    const NAME: &'static str = "Identity";
}

pub type IdentityGet = Get<Identity>;

impl Method<Identity> for IdentityGet {
    const NAME: &'static str = "Identity/get";
}
pub type IdentityChanges = Changes<Identity>;
impl Method<Identity> for IdentityChanges {
    const NAME: &'static str = "Identity/changes";
}

// [ref:TODO]: implement `forbiddenFrom` error for Identity/set.
/// `IdentitySet` method.
///
/// ```text
/// This is a standard "/set" method as described in [RFC8620],
/// Section 5.3. The following extra [`crate::jmap::methods::SetError`] types are defined:
/// For "create":
///  o  "forbiddenFrom": The user is not allowed to send from the address
///     given as the "email" property of the Identity.
/// ```
#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "camelCase", transparent)]
pub struct IdentitySet(pub Set<Identity>);

impl Method<Identity> for IdentitySet {
    const NAME: &'static str = "Identity/set";
}
