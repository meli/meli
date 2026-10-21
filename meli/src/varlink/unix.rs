//
// meli
//
// Copyright 2026 Manos Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use std::{net::Shutdown, os::fd::FromRawFd as _, pin::Pin};

use futures::future::BoxFuture;
use melib::error::{Error, Result, ResultIntoError as _};
use smol::net::{unix::UnixStream, TcpStream};

use crate::varlink::server::{Listener, Stream};

impl Stream for TcpStream {
    #[inline]
    fn shutdown(&mut self) -> Result<()> {
        Self::shutdown(self, Shutdown::Both)?;
        Ok(())
    }

    #[inline]
    fn clone(&self) -> Pin<Box<dyn Stream>> {
        Box::pin(std::clone::Clone::clone(self))
    }
}

impl Stream for UnixStream {
    #[inline]
    fn shutdown(&mut self) -> Result<()> {
        Self::shutdown(self, Shutdown::Both)?;
        Ok(())
    }

    #[inline]
    fn clone(&self) -> Pin<Box<dyn Stream>> {
        Box::pin(std::clone::Clone::clone(self))
    }
}

#[derive(Debug)]
pub struct TcpListener {
    inner: smol::net::TcpListener,
}

impl From<smol::net::TcpListener> for TcpListener {
    fn from(inner: smol::net::TcpListener) -> Self {
        Self { inner }
    }
}

impl TcpListener {
    pub fn new(addr: &str, listen_socket: Option<usize>) -> Result<Self> {
        if let Some(listen_socket) = listen_socket {
            // SAFETY: we have to trust the user that the socket fd is correct.
            unsafe {
                return Ok(Self {
                    inner: std::net::TcpListener::from_raw_fd(listen_socket as std::os::fd::RawFd)
                        .try_into()?,
                });
            }
        }
        Ok(Self {
            inner: std::net::TcpListener::bind(addr)?.try_into()?,
        })
    }
}

impl Listener for TcpListener {
    fn accept(&self) -> BoxFuture<'_, Result<Pin<Box<dyn Stream>>>> {
        Box::pin(async move {
            let (s, _addr) = self.inner.accept().await?;
            Ok(Box::pin(s) as Pin<Box<dyn Stream>>)
        })
    }
}

#[derive(Debug)]
pub struct UnixListener {
    inner: smol::net::unix::UnixListener,
}

impl From<smol::net::unix::UnixListener> for UnixListener {
    fn from(inner: smol::net::unix::UnixListener) -> Self {
        Self { inner }
    }
}

impl TryFrom<std::os::unix::net::UnixListener> for UnixListener {
    type Error = Error;

    fn try_from(inner: std::os::unix::net::UnixListener) -> Result<Self> {
        Ok(Self {
            inner: inner.try_into()?,
        })
    }
}

impl UnixListener {
    #[cfg(not(any(target_os = "linux", target_os = "android")))]
    pub fn new_abstract(_: &str, _: Option<usize>, _: bool) -> Result<Self> {
        Err(
            melib::error::Error::new("Abstract UNIX sockets are only supported on Linux")
                .set_kind(melib::error::ErrorKind::NotSupported),
        )
    }

    #[cfg(any(target_os = "linux", target_os = "android"))]
    pub fn new_abstract(addr: &str, listen_socket: Option<usize>, _: bool) -> Result<Self> {
        if let Some(listen_socket) = listen_socket {
            // SAFETY: we have to trust the user that the socket fd is correct.
            let inner = unsafe {
                std::os::unix::net::UnixListener::from_raw_fd(listen_socket as std::os::fd::RawFd)
            };

            return Self::try_from(inner);
        }

        use std::os::{linux::net::SocketAddrExt as _, unix::net::SocketAddr};

        let addr = SocketAddr::from_abstract_name(addr)
            .chain_err_details(|| format!("Could not parse abstract name address {addr}"))?;
        let inner = std::os::unix::net::UnixListener::bind_addr(&addr)?;
        Self::try_from(inner)
    }

    pub fn new(addr: &str, listen_socket: Option<usize>, replace: bool) -> Result<Self> {
        if let Some(listen_socket) = listen_socket {
            // SAFETY: we have to trust the user that the socket fd is correct.
            let inner = unsafe {
                std::os::unix::net::UnixListener::from_raw_fd(listen_socket as std::os::fd::RawFd)
            };
            return Self::try_from(inner);
        }
        let path = std::path::Path::new(addr);
        if replace && matches!(path.try_exists(), Ok(true)) {
            std::fs::remove_file(path).chain_err_related_path(path)?;
        }
        Ok(Self {
            inner: smol::net::unix::UnixListener::bind(path)?,
        })
    }
}

impl Drop for UnixListener {
    fn drop(&mut self) {
        let Ok(local_addr) = self.inner.local_addr() else {
            return;
        };
        let Some(path) = local_addr.as_pathname() else {
            return;
        };
        _ = std::fs::remove_file(path);
    }
}

impl Listener for UnixListener {
    fn accept(&self) -> BoxFuture<'_, Result<Pin<Box<dyn Stream>>>> {
        Box::pin(async move {
            let (s, _addr) = self.inner.accept().await?;
            Ok(Box::pin(s) as Pin<Box<dyn Stream>>)
        })
    }
}
