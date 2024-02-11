/*
 * meli - lib.rs
 *
 * Copyright 2017 Manos Pitsidianakis
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

//! Utility modules for general use.

pub mod connections;
pub mod datetime;
pub mod futures;
pub mod random;
#[macro_use]
pub mod logging;
pub mod parsec;
pub mod percent_encoding;
pub mod shellexpand;
#[cfg(feature = "sqlite3")]
pub mod sqlite3;
pub mod xdg;

pub mod html_escape {
    //! HTML Coded Character Set

    /// Numeric and Special Graphic Entity Set
    ///
    /// ```text
    /// GLYPH   NAME      SYNTAX       DESCRIPTION
    /// <       lt      &lt;    Less than sign
    /// >       gt      &gt;    Greater than sign
    /// &       amp     &amp;   Ampersand
    /// "       quot    &quot;  Double quote sign
    /// ```
    ///
    /// Source: <https://www.w3.org/MarkUp/html-spec/html-spec_9.html#SEC9.7.1>
    #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
    pub enum HtmlEntity {
        /// Less than sign
        Lt,
        /// Greater than sign
        Gt,
        /// Ampersand
        Amp,
        /// Double quote sign
        Quot,
    }

    impl HtmlEntity {
        pub const ALL: [&'static str; 4] = ["&lt;", "&gt;", "&amp;", "&quot;"];
        pub const GLYPHS: [&'static str; 4] = ["<", ">", "&", "\""];

        pub const fn glyph(self) -> char {
            match self {
                Self::Lt => '<',
                Self::Gt => '>',
                Self::Amp => '&',
                Self::Quot => '"',
            }
        }

        pub const fn name(self) -> &'static str {
            match self {
                Self::Lt => "lt",
                Self::Gt => "gt",
                Self::Amp => "amp",
                Self::Quot => "quot",
            }
        }

        pub const fn syntax(self) -> &'static str {
            match self {
                Self::Lt => "&lt;",
                Self::Gt => "&gt;",
                Self::Amp => "&amp;",
                Self::Quot => "&quot;",
            }
        }
    }
}

use std::str::FromStr;

#[macro_export]
macro_rules! declare_u64_hash {
    ($type_name:ident) => {
        #[derive(
            Hash,
            Eq,
            PartialEq,
            Debug,
            Ord,
            PartialOrd,
            Default,
            Serialize,
            Deserialize,
            Copy,
            Clone,
        )]
        #[repr(transparent)]
        pub struct $type_name(pub u64);

        impl $type_name {
            #[inline(always)]
            pub fn from_bytes(bytes: &[u8]) -> Self {
                use std::{collections::hash_map::DefaultHasher, hash::Hasher};
                let mut h = DefaultHasher::new();
                h.write(bytes);
                Self(h.finish())
            }

            #[inline(always)]
            pub const fn to_be_bytes(self) -> [u8; 8] {
                self.0.to_be_bytes()
            }

            #[inline(always)]
            pub const fn is_null(self) -> bool {
                self.0 == 0
            }
        }

        impl std::fmt::Display for $type_name {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(fmt, "{}", self.0)
            }
        }
        #[cfg(feature = "sqlite3")]
        impl rusqlite::types::ToSql for $type_name {
            fn to_sql(&self) -> rusqlite::Result<rusqlite::types::ToSqlOutput> {
                Ok(rusqlite::types::ToSqlOutput::from(self.0 as i64))
            }
        }

        #[cfg(feature = "sqlite3")]
        impl rusqlite::types::FromSql for $type_name {
            fn column_result(
                value: rusqlite::types::ValueRef,
            ) -> rusqlite::types::FromSqlResult<Self> {
                let b: i64 = rusqlite::types::FromSql::column_result(value)?;

                Ok($type_name(b as u64))
            }
        }
    };
}

/* Sorting states. */

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub enum SortOrder {
    Asc,
    #[default]
    Desc,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub enum SortField {
    Subject,
    #[default]
    Date,
}

impl FromStr for SortField {
    type Err = ();
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.trim().to_ascii_lowercase().as_str() {
            "subject" | "s" | "sub" | "sbj" | "subj" => Ok(Self::Subject),
            "date" | "d" => Ok(Self::Date),
            _ => Err(()),
        }
    }
}

impl FromStr for SortOrder {
    type Err = ();
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.trim().to_ascii_lowercase().as_str() {
            "asc" => Ok(Self::Asc),
            "desc" => Ok(Self::Desc),
            _ => Err(()),
        }
    }
}

impl std::ops::Not for SortOrder {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Asc => Self::Desc,
            Self::Desc => Self::Asc,
        }
    }
}

pub mod hostname {
    //! Get local hostname.
    use std::io::Read;

    use crate::{
        error::{Error, ErrorKind, Result},
        src_err_arc_wrap,
    };

    /// Get local hostname with the `gethostname()` libc function.
    pub fn hostname() -> Result<std::ffi::OsString> {
        let retval = nix::unistd::gethostname().map_err(|err| {
            Error::new("Could not discover local hostname")
                .set_source(Some(src_err_arc_wrap! {err}))
                .set_kind(ErrorKind::OSError)
        });
        if retval.is_err() {
            let mut hostn_buf = String::with_capacity(256);
            if matches!(
                std::fs::File::open("/etc/hostname")
                    .ok()
                    .and_then(|mut f| f.read_to_string(&mut hostn_buf).ok()),
                Some(n) if n > 0
            ) {
                return Ok(hostn_buf.into());
            }
        }
        retval
    }
}
