// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: Copyright 2021, 2022, 2023 chayleaf
// <chayleaf-cratesio@pavluk.org>

use std::{
    borrow::{Borrow, BorrowMut},
    fmt,
    ops::{Deref, DerefMut},
    str::FromStr,
};

use super::{Error, Result, TriCow, UrnSlice};

/// An owned RFC2141/8141 URN (Uniform Resource Name).
///
/// **Note:** the equivalence checks are done
/// [according to the specification](https://www.rfc-editor.org/rfc/rfc8141.html#section-3),
/// only taking the NID and NSS into account! If you need exact equivalence
/// checks, consider comparing using `Urn::as_str()` as the key. Some namespaces
/// may define additional lexical equivalence rules, these aren't accounted for
/// in this implementation (Meaning there might be false negatives for some
/// namespaces). There will, however, be no false positives.
///
/// `FromStr` requires a single allocation, but `TryFrom<String>` doesn't, so
/// prefer `TryFrom` when possible.
#[repr(transparent)]
#[derive(Eq, Hash, PartialEq)]
pub struct Urn(pub UrnSlice<'static>);

impl<'a> Borrow<UrnSlice<'a>> for Urn {
    fn borrow(&self) -> &UrnSlice<'a> {
        &self.0
    }
}

impl BorrowMut<UrnSlice<'static>> for Urn {
    fn borrow_mut(&mut self) -> &mut UrnSlice<'static> {
        &mut self.0
    }
}

impl<'a> From<UrnSlice<'a>> for Urn {
    fn from(value: UrnSlice<'a>) -> Self {
        Self(UrnSlice {
            urn: match value.urn {
                TriCow::Owned(s) => TriCow::Owned(s),
                TriCow::Borrowed(s) => TriCow::Owned(s.to_owned()),
                TriCow::MutBorrowed(s) => TriCow::Owned(s.to_owned()),
            },
            nid_len: value.nid_len,
            nss_len: value.nss_len,
            q_component_len: value.q_component_len,
            r_component_len: value.r_component_len,
        })
    }
}

impl<'a> From<&UrnSlice<'a>> for Urn {
    fn from(value: &UrnSlice<'a>) -> Self {
        Self(UrnSlice {
            urn: match &value.urn {
                TriCow::Owned(s) => TriCow::Owned(s.clone()),
                TriCow::Borrowed(s) => TriCow::Owned((*s).to_owned()),
                TriCow::MutBorrowed(s) => TriCow::Owned((*s).to_owned()),
            },
            nid_len: value.nid_len,
            nss_len: value.nss_len,
            q_component_len: value.q_component_len,
            r_component_len: value.r_component_len,
        })
    }
}

impl<'a> From<&mut UrnSlice<'a>> for Urn {
    fn from(value: &mut UrnSlice<'a>) -> Self {
        Self(UrnSlice {
            urn: match &value.urn {
                TriCow::Owned(s) => TriCow::Owned(s.clone()),
                TriCow::Borrowed(s) => TriCow::Owned((*s).to_owned()),
                TriCow::MutBorrowed(s) => TriCow::Owned((*s).to_owned()),
            },
            nid_len: value.nid_len,
            nss_len: value.nss_len,
            q_component_len: value.q_component_len,
            r_component_len: value.r_component_len,
        })
    }
}

impl Clone for Urn {
    fn clone(&self) -> Self {
        self.0.to_owned()
    }
}

impl fmt::Debug for Urn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Urn({})", self.as_str())
    }
}

impl Deref for Urn {
    type Target = UrnSlice<'static>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Urn {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a> AsRef<UrnSlice<'a>> for Urn {
    fn as_ref(&self) -> &UrnSlice<'a> {
        &self.0
    }
}

impl AsMut<UrnSlice<'static>> for Urn {
    fn as_mut(&mut self) -> &mut UrnSlice<'static> {
        &mut self.0
    }
}

impl<'a> PartialEq<UrnSlice<'a>> for Urn {
    fn eq(&self, other: &UrnSlice<'a>) -> bool {
        &self.0 == other
    }
}

impl AsRef<[u8]> for Urn {
    fn as_ref(&self) -> &[u8] {
        self.0.as_ref()
    }
}

impl AsRef<str> for Urn {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl fmt::Display for Urn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        self.0.fmt(f)
    }
}

impl FromStr for Urn {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        Ok(Self(UrnSlice::from_str(s)?))
    }
}

impl<'a> TryFrom<&'a str> for Urn {
    type Error = Error;
    fn try_from(value: &'a str) -> Result<Self> {
        Ok(Self(UrnSlice::try_from(value.to_owned())?))
    }
}

impl<'a> TryFrom<&'a mut str> for Urn {
    type Error = Error;
    fn try_from(value: &'a mut str) -> Result<Self> {
        Ok(Self(UrnSlice::try_from(value.to_owned())?))
    }
}

impl TryFrom<String> for Urn {
    type Error = Error;
    fn try_from(value: String) -> Result<Self> {
        Ok(Self(UrnSlice::try_from(value)?))
    }
}
