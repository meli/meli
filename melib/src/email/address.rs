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

use super::*;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GroupAddress {
    pub raw: Vec<u8>,
    pub display_name: StrBuilder,
    pub mailbox_list: Vec<Address>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
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
 *```
 */
pub struct MailboxAddress {
    pub raw: Vec<u8>,
    pub display_name: StrBuilder,
    pub address_spec: StrBuilder,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum Address {
    Mailbox(MailboxAddress),
    Group(GroupAddress),
}

impl Address {
    pub fn raw(&self) -> &[u8] {
        match self {
            Address::Mailbox(m) => m.raw.as_slice(),
            Address::Group(g) => g.raw.as_slice(),
        }
    }
    pub fn get_display_name(&self) -> String {
        match self {
            Address::Mailbox(m) => m.display_name.display(&m.raw),
            Address::Group(g) => g.display_name.display(&g.raw),
        }
    }

    pub fn get_email(&self) -> String {
        match self {
            Address::Mailbox(m) => m.address_spec.display(&m.raw),
            Address::Group(_) => String::new(),
        }
    }
    pub fn get_fqdn(&self) -> Option<String> {
        match self {
            Address::Mailbox(m) => {
                let raw_address = m.address_spec.display_bytes(&m.raw);
                let fqdn_pos = raw_address.iter().position(|&b| b == b'@')? + 1;
                Some(String::from_utf8_lossy(&raw_address[fqdn_pos..]).into())
            }
            Address::Group(_) => None,
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
}

impl Eq for Address {}
impl PartialEq for Address {
    fn eq(&self, other: &Address) -> bool {
        match (self, other) {
            (Address::Mailbox(_), Address::Group(_)) | (Address::Group(_), Address::Mailbox(_)) => {
                false
            }
            (Address::Mailbox(s), Address::Mailbox(o)) => {
                s.address_spec.display_bytes(&s.raw) == o.address_spec.display_bytes(&o.raw)
            }
            (Address::Group(s), Address::Group(o)) => {
                s.display_name.display_bytes(&s.raw) == o.display_name.display_bytes(&o.raw)
                    && s.mailbox_list
                        .iter()
                        .zip(o.mailbox_list.iter())
                        .fold(true, |b, (s, o)| b && (s == o))
            }
        }
    }
}

impl fmt::Display for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Address::Mailbox(m) if m.display_name.length > 0 => write!(
                f,
                "{} <{}>",
                m.display_name.display(&m.raw),
                m.address_spec.display(&m.raw)
            ),
            Address::Group(g) => {
                let attachment_strings: Vec<String> =
                    g.mailbox_list.iter().map(|a| format!("{}", a)).collect();
                write!(
                    f,
                    "{}: {}",
                    g.display_name.display(&g.raw),
                    attachment_strings.join(", ")
                )
            }
            Address::Mailbox(m) => write!(f, "{}", m.address_spec.display(&m.raw)),
        }
    }
}

impl fmt::Debug for Address {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// Helper struct to return slices from a struct field on demand.
#[derive(Clone, Debug, Serialize, Deserialize, Default, PartialEq, Eq, Copy)]
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
    pub fn display<'a>(&self, s: &'a [u8]) -> String {
        let offset = self.offset;
        let length = self.length;
        String::from_utf8_lossy(&s[offset..offset + length]).to_string()
    }

    pub fn display_bytes<'a>(&self, b: &'a [u8]) -> &'a [u8] {
        &b[self.offset..(self.offset + self.length)]
    }
}

/// `MessageID` is accessed through the `StrBuild` trait.
#[derive(Clone, Serialize, Deserialize, Default)]
pub struct MessageID(pub Vec<u8>, pub StrBuilder);

impl StrBuild for MessageID {
    fn new(string: &[u8], slice: &[u8]) -> Self {
        let offset = string.find(slice).unwrap_or(0);
        MessageID(
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

#[test]
fn test_strbuilder() {
    let m_id = b"<20170825132332.6734-1@el13635@mail.ntua.gr>";
    let (_, slice) = parser::message_id(m_id).unwrap();
    assert_eq!(
        MessageID::new(m_id, slice),
        MessageID(
            m_id.to_vec(),
            StrBuilder {
                offset: 1,
                length: 43,
            }
        )
    );
}

impl fmt::Display for MessageID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.val().is_ascii() {
            write!(f, "{}", unsafe { str::from_utf8_unchecked(self.val()) })
        } else {
            write!(f, "{}", String::from_utf8_lossy(self.val()))
        }
    }
}

impl PartialEq for MessageID {
    fn eq(&self, other: &MessageID) -> bool {
        self.raw() == other.raw()
    }
}
impl fmt::Debug for MessageID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", String::from_utf8(self.raw().to_vec()).unwrap())
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct References {
    pub raw: Vec<u8>,
    pub refs: Vec<MessageID>,
}

impl fmt::Debug for References {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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
