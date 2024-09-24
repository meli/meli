// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: Copyright 2021, 2022, 2023 chayleaf
// <chayleaf-cratesio@pavluk.org>

use core::{ops::Deref, slice::SliceIndex};

use super::Result;

#[allow(clippy::module_name_repetitions)]
pub(super) enum TriCow<'a> {
    Owned(String),
    Borrowed(&'a str),
    MutBorrowed(&'a mut str),
}

impl Deref for TriCow<'_> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Owned(s) => s,
            Self::Borrowed(s) => s,
            Self::MutBorrowed(s) => s,
        }
    }
}

impl TriCow<'_> {
    #[allow(clippy::unnecessary_wraps)]
    pub(super) fn replace_range(
        &mut self,
        range: core::ops::Range<usize>,
        with: &str,
    ) -> Result<()> {
        match self {
            TriCow::Owned(s) => {
                s.replace_range(range, with);
                Ok(())
            }
            TriCow::Borrowed(s) => {
                let mut s = s.to_owned();
                s.replace_range(range, with);
                *self = TriCow::Owned(s);
                Ok(())
            }
            TriCow::MutBorrowed(s) => {
                if range.len() == with.len() {
                    if let Some(slice) = s.get_mut(range.clone()) {
                        // SAFETY: both slice and with are valid utf-8 strings of same length
                        unsafe { slice.as_bytes_mut() }.copy_from_slice(with.as_bytes());
                        return Ok(());
                    }
                }
                {
                    let mut s = s.to_owned();
                    s.replace_range(range, with);
                    *self = TriCow::Owned(s);
                    Ok(())
                }
            }
        }
    }
    fn to_mut(&mut self) -> Result<&mut str> {
        match self {
            TriCow::Owned(s) => Ok(s.as_mut_str()),
            TriCow::Borrowed(s) => {
                *self = TriCow::Owned(s.to_owned());
                if let TriCow::Owned(s) = self {
                    Ok(s.as_mut_str())
                } else {
                    unreachable!("cow isn't owned after making it owned, what happened?")
                }
            }
            TriCow::MutBorrowed(s) => Ok(s),
        }
    }
    /// # Panics
    /// Panics if range isn't at valid character boundaries
    pub(super) fn make_uppercase<R>(&mut self, range: R) -> Result<()>
    where
        R: Clone + SliceIndex<[u8], Output = [u8]> + SliceIndex<str, Output = str>,
    {
        if self.as_bytes()[range.clone()]
            .iter()
            .any(u8::is_ascii_lowercase)
        {
            self.to_mut()?[range].make_ascii_uppercase();
        }
        Ok(())
    }
    /// # Panics
    /// Panics if range isn't at valid character boundaries
    pub(super) fn make_lowercase<R>(&mut self, range: R) -> Result<()>
    where
        R: Clone + SliceIndex<[u8], Output = [u8]> + SliceIndex<str, Output = str>,
    {
        if self.as_bytes()[range.clone()]
            .iter()
            .any(u8::is_ascii_uppercase)
        {
            // if this isn't ascii, it will fail later
            self.to_mut()?[range].make_ascii_lowercase();
        }
        Ok(())
    }
}
