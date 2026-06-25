/*
 * meli - email module
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

//! Email addresses. Parsing functions are in
//! [`crate::email::parser::address`].
use std::{
    collections::HashSet,
    convert::TryFrom,
    hash::{Hash, Hasher},
};

use super::*;
#[cfg(test)]
mod tests;

/// A named group of mailboxes.
///
/// See Section 3.4 of `RFC5322`.
#[derive(Clone, Debug)]
pub struct GroupAddress {
    pub display_name: Box<str>,
    pub mailbox_list: Vec<Address>,
}

#[derive(Clone, Debug)]
/// A mailbox address.
///
/// See Section 3.4 of `RFC5322`.
// ```text
// > "Name Name2" <address@domain.tld>
// >  └─────┬──┘   └──────────┬─────┘
// > display_name             │
// >                          │
// >                    address_spec
// ```
pub struct MailboxAddress {
    pub display_name: Option<Box<str>>,
    pub address_spec: Box<str>,
}

impl Eq for MailboxAddress {}

impl PartialEq for MailboxAddress {
    fn eq(&self, other: &Self) -> bool {
        self.address_spec == other.address_spec
    }
}

/// An email address.
///
/// Conforms to [RFC5322 - Internet Message Format](https://tools.ietf.org/html/rfc5322).
///
/// # Creating an `Address`
///
/// You can directly create an address with [`Address::new`] /
/// [`Address::new_group`]:
///
/// ```rust
/// # use melib::email::Address;
/// let addr = Address::new(Some("Jörg Doe"), "joerg@example.com");
/// assert_eq!(addr.to_string().as_str(), "Jörg Doe <joerg@example.com>");
/// ```
///
/// or parse it from a raw value:
///
/// ```rust
/// let (rest_bytes, addr) = melib::email::parser::address::address(
///     "=?utf-8?q?J=C3=B6rg_Doe?= <joerg@example.com>".as_bytes(),
/// )
/// .unwrap();
/// assert!(rest_bytes.is_empty());
/// assert_eq!(addr.get_display_name(), Some("Jörg Doe"));
/// assert_eq!(addr.get_email(), "joerg@example.com");
/// ```
#[derive(Clone, Debug)]
pub enum Address {
    Mailbox(MailboxAddress),
    Group(GroupAddress),
}

impl Address {
    #[inline(always)]
    fn new_inner(display_name: Option<Box<str>>, address_spec: Box<str>) -> Self {
        Self::Mailbox(MailboxAddress {
            display_name,
            address_spec,
        })
    }

    /// Create a new mailbox address.
    ///
    /// See [`MailboxAddress]` type.
    #[inline(always)]
    pub fn new<A: Into<String>, B: Into<String>>(display_name: Option<A>, address_spec: B) -> Self {
        let display_name = display_name.map(Into::into).filter(|s| !s.is_empty());
        let address_spec = address_spec.into();
        Self::new_inner(display_name.map(Into::into), address_spec.into())
    }

    /// Create a new group address.
    ///
    /// See [`GroupAddress]` type.
    #[inline(always)]
    pub fn new_group<T: Into<String>>(display_name: T, mailbox_list: Vec<Self>) -> Self {
        let display_name = display_name.into();
        Self::Group(GroupAddress {
            display_name: display_name.into(),
            mailbox_list,
        })
    }

    /// Get the display name of this address.
    ///
    /// If it's a group, it's the name of the group. Otherwise it's the
    /// `display_name` part of the mailbox:
    ///
    ///
    /// ```text
    ///           raw                         raw
    /// ┌──────────┴────────────┐   ┌──────────┴────────────────────┐
    /// Name <address@domain.tld>   "Name Name2" <address@domain.tld>
    /// └─┬┘  └──────────┬─────┘     └─────┬──┘   └──────────┬─────┘
    /// display_name     │          display_name             │
    ///                  │                                   │
    ///            address_spec                        address_spec
    /// ```
    pub fn get_display_name(&self) -> Option<&str> {
        match self {
            Self::Mailbox(m) => m.display_name.as_deref().filter(|s| !s.is_empty()),
            Self::Group(g) => Some(&g.display_name),
        }
    }

    /// Get the address spec part of this address. A group returns an empty
    /// slice.
    pub fn get_email(&self) -> &str {
        match self {
            Self::Mailbox(m) => &m.address_spec,
            Self::Group(_) => "",
        }
    }

    pub fn get_fqdn(&self) -> Option<&str> {
        match self {
            Self::Mailbox(m) => {
                let fqdn_pos = m.address_spec.as_bytes().iter().position(|&b| b == b'@')? + 1;
                Some(&m.address_spec[fqdn_pos..])
            }
            Self::Group(_) => None,
        }
    }

    pub fn get_tags(&self, separator: char) -> Vec<String> {
        let email = self.get_email();
        let at_pos = email
            .as_bytes()
            .iter()
            .position(|&b| b == b'@')
            .unwrap_or(0);
        let email: &str = email[..at_pos].into();
        email
            .split(separator)
            .skip(1)
            .map(str::to_string)
            .collect::<_>()
    }

    pub fn list_try_from<T: AsRef<[u8]>>(val: T) -> Result<Vec<Self>> {
        Ok(parser::address::rfc2822address_list(val.as_ref())?
            .1
            .to_vec())
    }

    pub fn contains_address(&self, other: &Self) -> bool {
        match self {
            Self::Mailbox(_) => self == other,
            Self::Group(g) => g
                .mailbox_list
                .iter()
                .any(|addr| addr.contains_address(other)),
        }
    }

    /// Get subaddress out of an address (e.g. `ken+subaddress@example.org`).
    ///
    /// Subaddresses are commonly text following a "+" character in an email
    /// address's local part . They are defined in [RFC5233 `Sieve Email Filtering: Subaddress Extension`](https://tools.ietf.org/html/rfc5233.html)
    ///
    /// # Examples
    ///
    /// ```
    /// # use melib::email::Address;
    /// let addr = "ken+sieve@example.org";
    /// let (rest, val) = melib::email::parser::address::address(addr.as_bytes()).unwrap();
    /// assert!(rest.is_empty());
    /// assert_eq!(
    ///     val.subaddress("+"),
    ///     Some((
    ///         Address::new(None::<&str>, "ken@example.org"),
    ///         "sieve".to_string()
    ///     ))
    /// );
    /// ```
    pub fn subaddress(&self, separator: &str) -> Option<(Self, String)> {
        match self {
            Self::Mailbox(_) => {
                let email = self.get_email();
                let (local_part, domain) =
                    match super::parser::address::addr_spec_raw(email.as_bytes())
                        .map_err(Into::<Error>::into)
                        .and_then(|(_, (l, d))| {
                            Ok((String::from_utf8(l.into())?, String::from_utf8(d.into())?))
                        }) {
                        Ok(v) => v,
                        Err(_) => return None,
                    };
                let s = local_part.split(separator).collect::<Vec<_>>();
                if s.len() < 2 {
                    return None;
                }
                let subaddress = &local_part[s[0].len() + separator.len()..];

                let display_name = self.get_display_name();
                Some((
                    Self::new(display_name, format!("{}@{}", s[0], domain)),
                    subaddress.to_string(),
                ))
            }
            Self::Group(_) => None,
        }
    }

    /// Returns a type that prints the names of addresses (or the e-mail part,
    /// if the name is missing) suitably for UI display, e.g. without
    /// quotes.
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use melib::email::Address;
    /// let addr = Address::new(Some("Jörg T. Doe"), "joerg@example.com");
    /// assert_eq!(
    ///     addr.to_string().as_str(),
    ///     r#""Jörg T. Doe" <joerg@example.com>"#
    /// );
    /// assert_eq!(addr.display_name().to_string().as_str(), "Jörg T. Doe");
    /// ```
    pub fn display_name(&self) -> UINameAddress<'_> {
        UINameAddress(self)
    }

    /// Formats a slice of `Address`es with their `<Address as
    /// std::fmt::Display>::display` method, separated by comma or
    /// `separator` if passed.
    pub fn display_slice(slice: &[Self], separator: Option<&str>) -> String {
        let separator = separator.unwrap_or(", ");
        match slice.first() {
            None => String::new(),
            Some(f) if slice.len() == 1 => f.to_string(),
            Some(_) => slice
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<String>>()
                .join(separator),
        }
    }

    /// Formats a slice of `Address`es with their `Address::display_name`
    /// method, separated by comma or `separator` if passed.
    pub fn display_name_slice(slice: &[Self], separator: Option<&str>) -> String {
        let separator = separator.unwrap_or(", ");
        match slice.first() {
            None => String::new(),
            Some(f) if slice.len() == 1 => f.display_name().to_string(),
            Some(_) => slice
                .iter()
                .map(|a| a.display_name().to_string())
                .collect::<Vec<String>>()
                .join(separator),
        }
    }
}

impl Eq for Address {}

impl PartialEq for Address {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Mailbox(_), Self::Group(_)) | (Self::Group(_), Self::Mailbox(_)) => false,
            (Self::Mailbox(s), Self::Mailbox(o)) => s == o,
            (Self::Group(s), Self::Group(o)) => {
                s.display_name == o.display_name
                    && s.mailbox_list.iter().collect::<HashSet<_>>()
                        == o.mailbox_list.iter().collect::<HashSet<_>>()
            }
        }
    }
}

impl Hash for Address {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Mailbox(s) => {
                s.address_spec.hash(state);
            }
            Self::Group(s) => {
                s.display_name.hash(state);
                for sub in &s.mailbox_list {
                    sub.hash(state);
                }
            }
        }
    }
}

pub fn fmt_mailbox(
    display_name: Option<&str>,
    address_spec: &str,
    f: &mut std::fmt::Formatter,
) -> std::fmt::Result {
    // [ref:FIXME]: do proper string escaping; we need a string escaping trait
    if let Some(display_name) = display_name {
        let display_name = display_name
            .strip_prefix('"')
            .and_then(|d| d.strip_suffix('"'))
            .unwrap_or(display_name);
        let must_be_quoted = b"()<>[]:;@\\,.\""
            .iter()
            .any(|b| display_name.as_bytes().contains(b));
        let must_be_escaped = display_name.as_bytes().contains(&b'"');
        if must_be_escaped {
            let display_name = display_name.replace("\"", "\\\"");
            write!(f, "\"{display_name}\" <{address_spec}>")
        } else if must_be_quoted {
            write!(f, "\"{display_name}\" <{address_spec}>")
        } else {
            write!(f, "{display_name} <{address_spec}>")
        }
    } else if address_spec.is_empty() {
        write!(f, "<>")
    } else {
        write!(f, "{}", address_spec)
    }
}

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Mailbox(m) => fmt_mailbox(m.display_name.as_deref(), &m.address_spec, f),
            Self::Group(g) => {
                let attachment_strings: Vec<String> =
                    g.mailbox_list.iter().map(|a| format!("{a}")).collect();
                write!(f, "{}:{};", g.display_name, attachment_strings.join(", "))
            }
        }
    }
}

impl TryFrom<&str> for Address {
    type Error = Error;

    fn try_from(val: &str) -> Result<Self> {
        Ok(parser::address::address(val.as_bytes())?.1)
    }
}

impl serde::Serialize for Address {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.to_string().serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for Address {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = std::borrow::Cow::<'de, str>::deserialize(deserializer)?;
        Self::try_from(s.as_ref()).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct UINameAddress<'a>(&'a Address);

impl std::fmt::Display for UINameAddress<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Address::Mailbox(m) => {
                if let Some(ref display_name) = m.display_name {
                    write!(f, "{}", display_name)
                } else {
                    write!(f, "{}", m.address_spec)
                }
            }
            Address::Group(g) => {
                write!(f, "{}", g.display_name)
            }
        }
    }
}

/// Helper struct to return slices from a struct field on demand.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub struct StrBuilder {
    pub offset: usize,
    pub length: usize,
}

/// Structs implementing this trait must contain a `StrBuilder` field.
pub trait StrBuild {
    /// Create a new `Self` out of a string and a slice
    fn new(string: &[u8], slice: &[u8]) -> Self;
    /// Get the slice part of the string
    fn raw(&self) -> &[u8];
    /// Get the entire string as a slice
    fn val(&self) -> &[u8];
}

impl StrBuilder {
    pub fn display(&self, s: &[u8]) -> String {
        let offset = self.offset;
        let length = self.length;
        String::from_utf8_lossy(&s[offset..offset + length]).to_string()
    }

    pub fn display_bytes<'a>(&self, b: &'a [u8]) -> &'a [u8] {
        &b[self.offset..(self.offset + self.length)]
    }
}

/// A unique message identifier, used in `Message-ID`, `References` etc headers.
///
/// See section "3.6.4. Identification Fields" of `RFC5322`.
#[derive(Clone, Default)]
pub struct MessageID(pub Box<str>);

impl MessageID {
    pub fn new<T: Into<String>>(val: T) -> Self {
        let val: String = val.into();
        if val.trim().starts_with('<') && val.trim().ends_with('>') {
            let val = val.trim().trim_matches(['<', '>']);
            Self(val.into())
        } else {
            Self(val.into())
        }
    }

    #[inline(always)]
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn display_brackets(&self) -> impl std::fmt::Display + '_ {
        MessageIDBracket(self)
    }

    /// Formats a slice of [`MessageID`]es with their
    /// [`MessageID::display_brackets`] method, separated by comma or
    /// `separator` if passed.
    pub fn display_slice(slice: &[Self], separator: Option<&str>) -> String {
        let separator = separator.unwrap_or(", ");
        match slice.first() {
            None => String::new(),
            Some(f) if slice.len() == 1 => f.display_brackets().to_string(),
            Some(_) => slice
                .iter()
                .map(|a| a.display_brackets().to_string())
                .collect::<Vec<String>>()
                .join(separator),
        }
    }
}

struct MessageIDBracket<'a>(&'a MessageID);

impl std::fmt::Display for MessageIDBracket<'_> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "<")?;
        write!(fmt, "{}", self.0)?;
        write!(fmt, ">")
    }
}

impl std::fmt::Display for MessageID {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{}", self.0)
    }
}

impl PartialEq for MessageID {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for MessageID {}

impl PartialEq<str> for MessageID {
    fn eq(&self, other: &str) -> bool {
        self.0.as_ref() == other.trim().trim_matches(['<', '>'])
    }
}

impl PartialEq<&str> for MessageID {
    fn eq(&self, other: &&str) -> bool {
        self == *other
    }
}

impl std::fmt::Debug for MessageID {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Hash for MessageID {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct References {
    refs: Vec<MessageID>,
}

impl std::fmt::Debug for References {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut dbg_t = fmt.debug_tuple(crate::identify! {References});
        for r in &self.refs {
            dbg_t.field(r);
        }
        dbg_t.finish()
    }
}

impl References {
    pub fn new(refs: Vec<MessageID>) -> Option<Self> {
        if refs.is_empty() {
            return None;
        }
        Some(Self { refs })
    }

    pub fn push(&mut self, new: MessageID) {
        self.refs.push(new);
    }

    /// A parent reference should only be removed in order to break cycles (when
    /// an envelope refers to its own `Message-ID` as a parent).
    pub fn remove(&mut self, msgid: &MessageID) {
        self.refs.retain(|r| r != msgid);
    }

    pub fn refs(&self) -> &[MessageID] {
        &self.refs
    }
}

impl Extend<MessageID> for References {
    /// Insert new [`MessageID`] values, de-duplicated.
    fn extend<T: IntoIterator<Item = MessageID>>(&mut self, iter: T) {
        for elem in iter {
            if !self.refs.contains(&elem) {
                self.refs.push(elem);
            }
        }
    }
}

impl<'a> Extend<&'a MessageID> for References {
    /// Insert new [`MessageID`] values, de-duplicated.
    fn extend<T: IntoIterator<Item = &'a MessageID>>(&mut self, iter: T) {
        for elem in iter {
            if !self.refs.contains(elem) {
                self.refs.push(elem.clone());
            }
        }
    }
}

impl serde::Serialize for References {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.refs.serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for References {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let refs: Vec<MessageID> = Vec::<MessageID>::deserialize(deserializer)?;
        Ok(Self { refs })
    }
}

impl serde::Serialize for MessageID {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.as_str().serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for MessageID {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: String = String::deserialize(deserializer)?;
        Ok(Self::new(s))
    }
}
