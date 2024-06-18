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

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GroupAddress {
    pub raw: Vec<u8>,
    pub display_name: StrBuilder,
    pub mailbox_list: Vec<Address>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
/**
 * Container for an address.
 *
 * ```text
 * >           raw: Vec<u8>
 * > ┌──────────┴────────────┐
 * > Name <address@domain.tld>
 * > └─┬┘  └──────────┬─────┘
 * > display_name     │
 * >                  │
 * >            address_spec
 *
 *
 * >           raw: Vec<u8>
 * > ┌──────────┴────────────────────┐
 * > "Name Name2" <address@domain.tld>
 * >  └─────┬──┘   └──────────┬─────┘
 * > display_name             │
 * >                          │
 * >                    address_spec
 * ```
 */
pub struct MailboxAddress {
    pub raw: Vec<u8>,
    pub display_name: StrBuilder,
    pub address_spec: StrBuilder,
}

impl Eq for MailboxAddress {}

impl PartialEq for MailboxAddress {
    fn eq(&self, other: &Self) -> bool {
        self.address_spec.display_bytes(&self.raw) == other.address_spec.display_bytes(&other.raw)
    }
}

/// An email address.
///
/// Conforms to [RFC5322 - Internet Message Format](https://tools.ietf.org/html/rfc5322).
///
/// # Creating an `Address`
/// You can directly create an address with `Address::new`,
///
/// ```rust
/// # use melib::email::Address;
/// let addr = Address::new(
///     Some("Jörg Doe".to_string()),
///     "joerg@example.com".to_string(),
/// );
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
/// assert_eq!(addr.get_display_name(), Some("Jörg Doe".to_string()));
/// assert_eq!(addr.get_email(), "joerg@example.com".to_string());
/// ```
#[derive(Clone, Deserialize, Serialize)]
pub enum Address {
    Mailbox(MailboxAddress),
    Group(GroupAddress),
}

impl Address {
    pub fn new(display_name: Option<String>, address: String) -> Self {
        Self::Mailbox(if let Some(d) = display_name {
            MailboxAddress {
                raw: format!("{} <{}>", d, address).into_bytes(),
                display_name: StrBuilder {
                    offset: 0,
                    length: d.len(),
                },
                address_spec: StrBuilder {
                    offset: d.len() + 2,
                    length: address.len(),
                },
            }
        } else {
            MailboxAddress {
                raw: address.to_string().into_bytes(),
                display_name: StrBuilder {
                    offset: 0,
                    length: 0,
                },
                address_spec: StrBuilder {
                    offset: 0,
                    length: address.len(),
                },
            }
        })
    }

    pub fn new_group(display_name: String, mailbox_list: Vec<Self>) -> Self {
        Self::Group(GroupAddress {
            raw: format!(
                "{}:{};",
                display_name,
                mailbox_list
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(",")
            )
            .into_bytes(),
            display_name: StrBuilder {
                offset: 0,
                length: display_name.len(),
            },
            mailbox_list,
        })
    }

    pub fn raw(&self) -> &[u8] {
        match self {
            Self::Mailbox(m) => m.raw.as_slice(),
            Self::Group(g) => g.raw.as_slice(),
        }
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
    pub fn get_display_name(&self) -> Option<String> {
        let ret = match self {
            Self::Mailbox(m) => m.display_name.display(&m.raw),
            Self::Group(g) => g.display_name.display(&g.raw),
        };
        if ret.is_empty() {
            None
        } else {
            Some(ret)
        }
    }

    /// Get the address spec part of this address. A group returns an empty
    /// `String`.
    pub fn get_email(&self) -> String {
        match self {
            Self::Mailbox(m) => m.address_spec.display(&m.raw),
            Self::Group(_) => String::new(),
        }
    }

    pub fn address_spec_raw(&self) -> &[u8] {
        match self {
            Self::Mailbox(m) => m.address_spec.display_bytes(&m.raw),
            Self::Group(g) => &g.raw,
        }
    }

    pub fn get_fqdn(&self) -> Option<String> {
        match self {
            Self::Mailbox(m) => {
                let raw_address = m.address_spec.display_bytes(&m.raw);
                let fqdn_pos = raw_address.iter().position(|&b| b == b'@')? + 1;
                Some(String::from_utf8_lossy(&raw_address[fqdn_pos..]).into())
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
    ///         Address::new(None, "ken@example.org".to_string()),
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

    /// Get the display name of this address in bytes.
    ///
    /// For a string, see the [`display_name`](fn@Self::get_display_name)
    /// method.
    pub fn display_name_bytes(&self) -> &[u8] {
        match self {
            Self::Mailbox(m) => m.display_name.display_bytes(&m.raw),
            Self::Group(g) => g.display_name.display_bytes(&g.raw),
        }
    }

    /// Returns a type that prints addresses suitably for UI display, e.g.
    /// without quotes.
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use melib::email::Address;
    /// let addr = Address::new(
    ///     Some("Jörg T. Doe".to_string()),
    ///     "joerg@example.com".to_string(),
    /// );
    /// assert_eq!(
    ///     addr.to_string().as_str(),
    ///     r#""Jörg T. Doe" <joerg@example.com>"#
    /// );
    /// assert_eq!(
    ///     addr.display().to_string().as_str(),
    ///     "Jörg T. Doe <joerg@example.com>"
    /// );
    /// ```
    pub fn display(&self) -> UIAddress {
        UIAddress(self)
    }

    /// Returns a type that prints the names of addresses (or the e-mail part,
    /// if the name is missing) suitably for UI display, e.g. without
    /// quotes.
    ///
    /// ## Example
    ///
    /// ```rust
    /// # use melib::email::Address;
    /// let addr = Address::new(
    ///     Some("Jörg T. Doe".to_string()),
    ///     "joerg@example.com".to_string(),
    /// );
    /// assert_eq!(
    ///     addr.to_string().as_str(),
    ///     r#""Jörg T. Doe" <joerg@example.com>"#
    /// );
    /// assert_eq!(addr.display_name().to_string().as_str(), "Jörg T. Doe");
    /// ```
    pub fn display_name(&self) -> UINameAddress {
        UINameAddress(self)
    }

    /// Formats a slice of `Address`es with their `Address::display` method,
    /// separated by comma or `separator` if passed.
    pub fn display_slice(slice: &[Self]) -> String {
        match slice.first() {
            None => String::new(),
            Some(f) if slice.len() == 1 => f.display().to_string(),
            Some(_) => slice
                .iter()
                .map(|a| a.display().to_string())
                .collect::<Vec<String>>()
                .join(", "),
        }
    }

    /// Formats a slice of `Address`es with their `Address::display_name`
    /// method, separated by comma or `separator` if passed.
    pub fn display_name_slice(slice: &[Self]) -> String {
        match slice.first() {
            None => String::new(),
            Some(f) if slice.len() == 1 => f.display_name().to_string(),
            Some(_) => slice
                .iter()
                .map(|a| a.display_name().to_string())
                .collect::<Vec<String>>()
                .join(", "),
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
                self.display_name_bytes() == other.display_name_bytes()
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
                s.address_spec.display_bytes(&s.raw).hash(state);
            }
            Self::Group(s) => {
                s.display_name.display_bytes(&s.raw).hash(state);
                for sub in &s.mailbox_list {
                    sub.hash(state);
                }
            }
        }
    }
}

impl std::fmt::Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Mailbox(m) if m.display_name.length > 0 => match m.display_name.display(&m.raw) {
                d if d.contains('.') || d.contains(',') => {
                    write!(f, "\"{}\" <{}>", d, m.address_spec.display(&m.raw))
                }
                d => write!(f, "{} <{}>", d, m.address_spec.display(&m.raw)),
            },
            Self::Mailbox(m) => write!(f, "{}", m.address_spec.display(&m.raw)),
            Self::Group(g) => {
                let attachment_strings: Vec<String> =
                    g.mailbox_list.iter().map(|a| format!("{}", a)).collect();
                write!(
                    f,
                    "{}: {}",
                    g.display_name.display(&g.raw),
                    attachment_strings.join(", ")
                )
            }
        }
    }
}

impl std::fmt::Debug for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Mailbox(m) => f
                .debug_struct(stringify!(Address::Mailbox))
                .field("display_name", &m.display_name.display(&m.raw))
                .field("address_spec", &m.address_spec.display(&m.raw))
                .finish(),
            Self::Group(g) => {
                let attachment_strings: Vec<String> =
                    g.mailbox_list.iter().map(|a| format!("{}", a)).collect();

                f.debug_struct(stringify!(Address::Group))
                    .field("display_name", &g.display_name.display(&g.raw))
                    .field("addresses", &attachment_strings.join(", "))
                    .finish()
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

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct UIAddress<'a>(&'a Address);

impl std::fmt::Display for UIAddress<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Address::Mailbox(m) if m.display_name.length > 0 => write!(
                f,
                "{} <{}>",
                m.display_name.display(&m.raw),
                m.address_spec.display(&m.raw)
            ),
            Address::Mailbox(m) => write!(f, "{}", m.address_spec.display(&m.raw)),
            Address::Group(g) => {
                let attachment_strings: Vec<String> =
                    g.mailbox_list.iter().map(|a| Self(a).to_string()).collect();
                write!(
                    f,
                    "{}: {}",
                    g.display_name.display(&g.raw),
                    attachment_strings.join(", ")
                )
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct UINameAddress<'a>(&'a Address);

impl std::fmt::Display for UINameAddress<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            Address::Mailbox(m) if m.display_name.length > 0 => {
                write!(f, "{}", m.display_name.display(&m.raw),)
            }
            Address::Mailbox(m) => write!(f, "{}", m.address_spec.display(&m.raw)),
            Address::Group(g) => {
                let attachment_strings: Vec<String> =
                    g.mailbox_list.iter().map(|a| Self(a).to_string()).collect();
                write!(
                    f,
                    "{}: {}",
                    g.display_name.display(&g.raw),
                    attachment_strings.join(", ")
                )
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

/// `MessageID` is accessed through the `StrBuild` trait.
#[derive(Clone, Default, Deserialize, Serialize)]
pub struct MessageID(pub Vec<u8>, pub StrBuilder);

impl StrBuild for MessageID {
    fn new(string: &[u8], slice: &[u8]) -> Self {
        let offset = string.find(slice).unwrap_or(0);
        Self(
            string.to_owned(),
            StrBuilder {
                offset,
                length: slice.len() + 1,
            },
        )
    }

    fn raw(&self) -> &[u8] {
        let offset = self.1.offset;
        let length = self.1.length;
        &self.0[offset..offset + length.saturating_sub(1)]
    }

    fn val(&self) -> &[u8] {
        &self.0
    }
}

impl std::fmt::Display for MessageID {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.val().is_ascii() {
            write!(f, "{}", unsafe {
                std::str::from_utf8_unchecked(self.val())
            })
        } else {
            write!(f, "{}", String::from_utf8_lossy(self.val()))
        }
    }
}

impl PartialEq for MessageID {
    fn eq(&self, other: &Self) -> bool {
        self.raw() == other.raw()
    }
}

impl PartialEq<str> for MessageID {
    fn eq(&self, other: &str) -> bool {
        self.raw()
            == other
                .trim()
                .trim_start_matches('<')
                .trim_end_matches('>')
                .as_bytes()
    }
}

impl PartialEq<&str> for MessageID {
    fn eq(&self, other: &&str) -> bool {
        self == *other
    }
}

impl std::fmt::Debug for MessageID {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8(self.raw().to_vec()).unwrap())
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct References {
    pub raw: Vec<u8>,
    pub refs: Vec<MessageID>,
}

impl std::fmt::Debug for References {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:#?}", self.refs)
    }
}

#[macro_export]
macro_rules! make_address {
    ($d:expr, $a:expr) => {
        Address::Mailbox(if $d.is_empty() {
            MailboxAddress {
                raw: format!("{}", $a).into_bytes(),
                display_name: StrBuilder {
                    offset: 0,
                    length: 0,
                },
                address_spec: StrBuilder {
                    offset: 0,
                    length: $a.len(),
                },
            }
        } else {
            MailboxAddress {
                raw: format!("{} <{}>", $d, $a).into_bytes(),
                display_name: StrBuilder {
                    offset: 0,
                    length: $d.len(),
                },
                address_spec: StrBuilder {
                    offset: $d.len() + 2,
                    length: $a.len(),
                },
            }
        })
    };
}
