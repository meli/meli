/*
 * meli - imap module.
 *
 * Copyright 2023 Damian Poddebniak <poddebniak@mailbox.org>
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

use std::sync::Arc;

use imap_codec::imap_types::{
    command::error::{AppendError, CopyError, ListError},
    error::ValidationError,
    extensions::r#move::error::MoveError,
};

use crate::error::{Error, ErrorKind};

impl From<ValidationError> for Error {
    #[inline]
    fn from(error: ValidationError) -> Self {
        Self {
            summary: error.to_string().into(),
            details: None,
            source: Some(Arc::new(error)),
            kind: ErrorKind::Bug,
        }
    }
}

impl<S, L> From<AppendError<S, L>> for Error
where
    AppendError<S, L>: std::fmt::Debug + std::fmt::Display + Sync + Send + 'static,
{
    #[inline]
    fn from(error: AppendError<S, L>) -> Self {
        Self {
            summary: error.to_string().into(),
            details: None,
            source: Some(Arc::new(error)),
            kind: ErrorKind::Bug,
        }
    }
}

impl<S, L> From<CopyError<S, L>> for Error
where
    CopyError<S, L>: std::fmt::Debug + std::fmt::Display + Sync + Send + 'static,
{
    #[inline]
    fn from(error: CopyError<S, L>) -> Self {
        Self {
            summary: error.to_string().into(),
            details: None,
            source: Some(Arc::new(error)),
            kind: ErrorKind::Bug,
        }
    }
}

impl<S, M> From<MoveError<S, M>> for Error
where
    MoveError<S, M>: std::fmt::Debug + std::fmt::Display + Sync + Send + 'static,
{
    #[inline]
    fn from(error: MoveError<S, M>) -> Self {
        Self {
            summary: error.to_string().into(),
            details: None,
            source: Some(Arc::new(error)),
            kind: ErrorKind::Bug,
        }
    }
}

impl<L1, L2> From<ListError<L1, L2>> for Error
where
    ListError<L1, L2>: std::fmt::Debug + std::fmt::Display + Sync + Send + 'static,
{
    #[inline]
    fn from(error: ListError<L1, L2>) -> Self {
        Self {
            summary: error.to_string().into(),
            details: None,
            source: Some(Arc::new(error)),
            kind: ErrorKind::Bug,
        }
    }
}
