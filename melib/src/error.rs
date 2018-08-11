/*
 * meli - error module
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

/*!
 * An error object for `melib`
 */

use std::borrow::Cow;
use std::error::Error;
use std::fmt;
use std::io;
use std::result;

use nom;

pub type Result<T> = result::Result<T, MeliError>;

#[derive(Debug, Clone)]
pub struct MeliError {
    details: String,
}

impl MeliError {
    pub fn new<M>(msg: M) -> MeliError
    where
        M: Into<String>,
    {
        MeliError {
            details: msg.into(),
        }
    }
}

impl fmt::Display for MeliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl Error for MeliError {
    fn description(&self) -> &str {
        &self.details
    }
}

impl From<io::Error> for MeliError {
    #[inline]
    fn from(kind: io::Error) -> MeliError {
        MeliError::new(kind.description())
    }
}

impl From<nom::IError> for MeliError {
    #[inline]
    fn from(kind: nom::IError) -> MeliError {
        MeliError::new(format!("{:?}", kind))
    }
}

impl<'a> From<Cow<'a, str>> for MeliError {
    #[inline]
    fn from(kind: Cow<'_, str>) -> MeliError {
        MeliError::new(format!("{:?}", kind))
    }
}

//use std::option;
//impl From<option::NoneError> for MeliError {
//    #[inline]
//    fn from(kind: option::NoneError) -> MeliError {
//        MeliError::new(format!("{:?}", kind))
//    }
//}
