/*
 * meli - melib library
 *
 * Copyright 2020  Manos Pitsidianakis
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

//! Connections layers (TCP/fd/TLS/Deflate) to use with remote backends.
use std::{os::unix::io::AsRawFd, time::Duration};

use flate2::{read::DeflateDecoder, write::DeflateEncoder, Compression};
#[cfg(any(target_os = "openbsd", target_os = "netbsd", target_os = "haiku"))]
use libc::SO_KEEPALIVE as KEEPALIVE_OPTION;
#[cfg(any(target_os = "macos", target_os = "ios"))]
use libc::TCP_KEEPALIVE as KEEPALIVE_OPTION;
#[cfg(not(any(
    target_os = "openbsd",
    target_os = "netbsd",
    target_os = "haiku",
    target_os = "macos",
    target_os = "ios"
)))]
use libc::TCP_KEEPIDLE as KEEPALIVE_OPTION;
use libc::{self, c_int, c_void};

// pub mod smol;
pub mod std_net;

pub const CONNECTION_ATTEMPT_DELAY: std::time::Duration = std::time::Duration::from_millis(250);

pub enum Connection {
    Tcp {
        inner: std::net::TcpStream,
        id: Option<&'static str>,
        trace: bool,
    },
    Fd {
        inner: std::os::unix::io::RawFd,
        id: Option<&'static str>,
        trace: bool,
    },
    #[cfg(feature = "tls")]
    Tls {
        inner: native_tls::TlsStream<Self>,
        id: Option<&'static str>,
        trace: bool,
    },
    Deflate {
        inner: DeflateEncoder<DeflateDecoder<Box<Self>>>,
        id: Option<&'static str>,
        trace: bool,
    },
}

impl std::fmt::Debug for Connection {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Tcp {
                ref trace,
                ref inner,
                ref id,
            } => fmt
                .debug_struct(stringify!(Connection))
                .field("variant", &stringify!(Tcp))
                .field(stringify!(trace), trace)
                .field(stringify!(id), id)
                .field(stringify!(inner), inner)
                .finish(),
            #[cfg(feature = "tls")]
            Tls {
                ref trace,
                ref inner,
                ref id,
            } => fmt
                .debug_struct(stringify!(Connection))
                .field("variant", &stringify!(Tls))
                .field(stringify!(trace), trace)
                .field(stringify!(id), id)
                .field(stringify!(inner), inner.get_ref())
                .finish(),
            Fd {
                ref trace,
                ref inner,
                ref id,
            } => fmt
                .debug_struct(stringify!(Connection))
                .field("variant", &stringify!(Fd))
                .field(stringify!(trace), trace)
                .field(stringify!(id), id)
                .field(stringify!(inner), inner)
                .finish(),
            Deflate {
                ref trace,
                ref inner,
                ref id,
            } => fmt
                .debug_struct(stringify!(Connection))
                .field("variant", &stringify!(Deflate))
                .field(stringify!(trace), trace)
                .field(stringify!(id), id)
                .field(stringify!(inner), inner)
                .finish(),
        }
    }
}

use Connection::*;

macro_rules! syscall {
    ($fn: ident ( $($arg: expr),* $(,)* ) ) => {{
        #[allow(unused_unsafe)]
        let res = unsafe { libc::$fn($($arg, )*) };
        if res == -1 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(res)
        }
    }};
}

impl Connection {
    pub const IO_BUF_SIZE: usize = 64 * 1024;

    pub fn deflate(mut self) -> Self {
        let trace = self.is_trace_enabled();
        let id = self.id();
        self.set_trace(false);
        Self::Deflate {
            inner: DeflateEncoder::new(
                DeflateDecoder::new_with_buf(Box::new(self), vec![0; Self::IO_BUF_SIZE]),
                Compression::default(),
            ),
            id,
            trace,
        }
    }

    #[cfg(feature = "tls")]
    pub fn new_tls(mut inner: native_tls::TlsStream<Self>) -> Self {
        let trace = inner.get_ref().is_trace_enabled();
        let id = inner.get_ref().id();
        if trace {
            inner.get_mut().set_trace(false);
        }
        Self::Tls { inner, id, trace }
    }

    pub fn new_tcp(inner: std::net::TcpStream) -> Self {
        Self::Tcp {
            inner,
            id: None,
            trace: false,
        }
    }

    pub fn trace(mut self, val: bool) -> Self {
        match self {
            Tcp { ref mut trace, .. } => *trace = val,
            #[cfg(feature = "tls")]
            Tls { ref mut trace, .. } => *trace = val,
            Fd { ref mut trace, .. } => {
                *trace = val;
            }
            Deflate { ref mut trace, .. } => *trace = val,
        }
        self
    }

    pub fn with_id(mut self, val: &'static str) -> Self {
        match self {
            Tcp { ref mut id, .. } => *id = Some(val),
            #[cfg(feature = "tls")]
            Tls { ref mut id, .. } => *id = Some(val),
            Fd { ref mut id, .. } => {
                *id = Some(val);
            }
            Deflate { ref mut id, .. } => *id = Some(val),
        }
        self
    }

    pub fn set_trace(&mut self, val: bool) {
        match self {
            Tcp { ref mut trace, .. } => *trace = val,
            #[cfg(feature = "tls")]
            Tls { ref mut trace, .. } => *trace = val,
            Fd { ref mut trace, .. } => {
                *trace = val;
            }
            Deflate { ref mut trace, .. } => *trace = val,
        }
    }

    pub fn set_nonblocking(&self, nonblocking: bool) -> std::io::Result<()> {
        if self.is_trace_enabled() {
            let id = self.id();
            log::trace!(
                "{}{}{}{:?} set_nonblocking({:?})",
                if id.is_some() { "[" } else { "" },
                if let Some(id) = id.as_ref() { id } else { "" },
                if id.is_some() { "]: " } else { "" },
                self,
                nonblocking
            );
        }
        match self {
            Tcp { ref inner, .. } => inner.set_nonblocking(nonblocking),
            #[cfg(feature = "tls")]
            Tls { ref inner, .. } => inner.get_ref().set_nonblocking(nonblocking),
            Fd { inner, .. } => {
                // [ref:VERIFY]
                nix::fcntl::fcntl(
                    *inner,
                    nix::fcntl::FcntlArg::F_SETFL(if nonblocking {
                        nix::fcntl::OFlag::O_NONBLOCK
                    } else {
                        !nix::fcntl::OFlag::O_NONBLOCK
                    }),
                )
                .map_err(|err| std::io::Error::from_raw_os_error(err as i32))?;
                Ok(())
            }
            Deflate { ref inner, .. } => inner.get_ref().get_ref().set_nonblocking(nonblocking),
        }
    }

    pub fn set_read_timeout(&self, dur: Option<Duration>) -> std::io::Result<()> {
        if self.is_trace_enabled() {
            let id = self.id();
            log::trace!(
                "{}{}{}{:?} set_read_timeout({:?})",
                if id.is_some() { "[" } else { "" },
                if let Some(id) = id.as_ref() { id } else { "" },
                if id.is_some() { "]: " } else { "" },
                self,
                dur
            );
        }
        match self {
            Tcp { ref inner, .. } => inner.set_read_timeout(dur),
            #[cfg(feature = "tls")]
            Tls { ref inner, .. } => inner.get_ref().set_read_timeout(dur),
            Fd { .. } => Ok(()),
            Deflate { ref inner, .. } => inner.get_ref().get_ref().set_read_timeout(dur),
        }
    }

    pub fn set_write_timeout(&self, dur: Option<Duration>) -> std::io::Result<()> {
        if self.is_trace_enabled() {
            let id = self.id();
            log::trace!(
                "{}{}{}{:?} set_write_timeout({:?})",
                if id.is_some() { "[" } else { "" },
                if let Some(id) = id.as_ref() { id } else { "" },
                if id.is_some() { "]: " } else { "" },
                self,
                dur
            );
        }
        match self {
            Tcp { ref inner, .. } => inner.set_write_timeout(dur),
            #[cfg(feature = "tls")]
            Tls { ref inner, .. } => inner.get_ref().set_write_timeout(dur),
            Fd { .. } => Ok(()),
            Deflate { ref inner, .. } => inner.get_ref().get_ref().set_write_timeout(dur),
        }
    }

    pub fn keepalive(&self) -> std::io::Result<Option<Duration>> {
        if self.is_trace_enabled() {
            log::trace!("{:?} keepalive()", self);
        }
        if matches!(self, Fd { .. }) {
            return Ok(None);
        }
        unsafe {
            let raw: c_int = self.getsockopt(libc::SOL_SOCKET, libc::SO_KEEPALIVE)?;
            if raw == 0 {
                return Ok(None);
            }
            let secs: c_int = self.getsockopt(libc::IPPROTO_TCP, KEEPALIVE_OPTION)?;
            Ok(Some(Duration::new(secs as u64, 0)))
        }
    }

    pub fn set_keepalive(&self, keepalive: Option<Duration>) -> std::io::Result<()> {
        if self.is_trace_enabled() {
            let id = self.id();
            log::trace!(
                "{}{}{}{:?} set_keepalive({:?})",
                if id.is_some() { "[" } else { "" },
                if let Some(id) = id.as_ref() { id } else { "" },
                if id.is_some() { "]: " } else { "" },
                self,
                keepalive
            );
        }
        if matches!(self, Fd { .. }) {
            return Ok(());
        }
        unsafe {
            self.setsockopt(
                libc::SOL_SOCKET,
                libc::SO_KEEPALIVE,
                keepalive.is_some() as c_int,
            )?;
            if let Some(dur) = keepalive {
                // [ref:TODO]: checked cast here
                self.setsockopt(libc::IPPROTO_TCP, KEEPALIVE_OPTION, dur.as_secs() as c_int)?;
            }
            Ok(())
        }
    }

    unsafe fn setsockopt<T>(&self, opt: c_int, val: c_int, payload: T) -> std::io::Result<()>
    where
        T: Copy,
    {
        let payload = std::ptr::addr_of!(payload) as *const c_void;
        syscall!(setsockopt(
            self.as_raw_fd(),
            opt,
            val,
            payload,
            std::mem::size_of::<T>() as libc::socklen_t,
        ))?;
        Ok(())
    }

    unsafe fn getsockopt<T: Copy>(&self, opt: c_int, val: c_int) -> std::io::Result<T> {
        let mut slot: T = std::mem::zeroed();
        let mut len = std::mem::size_of::<T>() as libc::socklen_t;
        syscall!(getsockopt(
            self.as_raw_fd(),
            opt,
            val,
            std::ptr::addr_of_mut!(slot) as *mut _,
            &mut len,
        ))?;
        assert_eq!(len as usize, std::mem::size_of::<T>());
        Ok(slot)
    }

    fn is_trace_enabled(&self) -> bool {
        match self {
            Fd { trace, .. } | Tcp { trace, .. } => *trace,
            #[cfg(feature = "tls")]
            Tls { trace, .. } => *trace,
            Deflate { trace, .. } => *trace,
        }
    }

    fn id(&self) -> Option<&'static str> {
        match self {
            Fd { id, .. } | Tcp { id, .. } => *id,
            #[cfg(feature = "tls")]
            Tls { id, .. } => *id,
            Deflate { id, .. } => *id,
        }
    }
}

impl Drop for Connection {
    fn drop(&mut self) {
        if let Fd { ref inner, .. } = self {
            let _ = nix::unistd::close(*inner);
        }
    }
}

impl std::io::Read for Connection {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let res = match self {
            Tcp { ref mut inner, .. } => inner.read(buf),
            #[cfg(feature = "tls")]
            Tls { ref mut inner, .. } => inner.read(buf),
            Fd { ref inner, .. } => {
                use std::os::unix::io::{FromRawFd, IntoRawFd};
                let mut f = unsafe { std::fs::File::from_raw_fd(*inner) };
                let ret = f.read(buf);
                let _ = f.into_raw_fd();
                ret
            }
            Deflate { ref mut inner, .. } => inner.read(buf),
        };
        if self.is_trace_enabled() {
            let id = self.id();
            if let Ok(len) = &res {
                log::trace!(
                    "{}{}{}{:?} read {:?} bytes:{:?}",
                    if id.is_some() { "[" } else { "" },
                    if let Some(id) = id.as_ref() { id } else { "" },
                    if id.is_some() { "]: " } else { "" },
                    self,
                    len,
                    String::from_utf8_lossy(&buf[..*len])
                );
            } else {
                log::trace!(
                    "{}{}{}{:?} could not read {:?}",
                    if id.is_some() { "[" } else { "" },
                    if let Some(id) = id.as_ref() { id } else { "" },
                    if id.is_some() { "]: " } else { "" },
                    self,
                    &res
                );
            }
        }
        res
    }
}

impl std::io::Write for Connection {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if self.is_trace_enabled() {
            let id = self.id();
            log::trace!(
                "{}{}{}{:?} writing {} bytes:{:?}",
                if id.is_some() { "[" } else { "" },
                if let Some(id) = id.as_ref() { id } else { "" },
                if id.is_some() { "]: " } else { "" },
                self,
                buf.len(),
                String::from_utf8_lossy(buf)
            );
        }
        match self {
            Tcp { ref mut inner, .. } => inner.write(buf),
            #[cfg(feature = "tls")]
            Tls { ref mut inner, .. } => inner.write(buf),
            Fd { ref inner, .. } => {
                use std::os::unix::io::{FromRawFd, IntoRawFd};
                let mut f = unsafe { std::fs::File::from_raw_fd(*inner) };
                let ret = f.write(buf);
                let _ = f.into_raw_fd();
                ret
            }
            Deflate { ref mut inner, .. } => inner.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Tcp { ref mut inner, .. } => inner.flush(),
            #[cfg(feature = "tls")]
            Tls { ref mut inner, .. } => inner.flush(),
            Fd { ref inner, .. } => {
                use std::os::unix::io::{FromRawFd, IntoRawFd};
                let mut f = unsafe { std::fs::File::from_raw_fd(*inner) };
                let ret = f.flush();
                let _ = f.into_raw_fd();
                ret
            }
            Deflate { ref mut inner, .. } => inner.flush(),
        }
    }
}

impl std::os::unix::io::AsRawFd for Connection {
    fn as_raw_fd(&self) -> std::os::unix::io::RawFd {
        match self {
            Tcp { ref inner, .. } => inner.as_raw_fd(),
            #[cfg(feature = "tls")]
            Tls { ref inner, .. } => inner.get_ref().as_raw_fd(),
            Fd { ref inner, .. } => *inner,
            Deflate { ref inner, .. } => inner.get_ref().get_ref().as_raw_fd(),
        }
    }
}

#[deprecated = "While it supports IPv6, it does not implement the happy eyeballs algorithm. Use \
                {std_net,smol}::tcp_stream_connect instead."]
pub fn lookup_ip(host: &str, port: u16) -> crate::Result<std::net::SocketAddr> {
    use std::net::ToSocketAddrs;

    use crate::error::{Error, ErrorKind, NetworkErrorKind};

    let addrs = (host, port).to_socket_addrs()?;
    for addr in addrs {
        if matches!(
            addr,
            std::net::SocketAddr::V4(_) | std::net::SocketAddr::V6(_)
        ) {
            return Ok(addr);
        }
    }

    Err(
        Error::new(format!("Could not lookup address {}:{}", host, port))
            .set_kind(ErrorKind::Network(NetworkErrorKind::HostLookupFailed)),
    )
}
