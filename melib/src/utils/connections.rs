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
use std::{
    borrow::Cow,
    os::{
        fd::{AsFd, BorrowedFd, OwnedFd},
        unix::io::AsRawFd,
    },
    time::Duration,
};

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
        inner: OwnedFd,
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
                .debug_struct(crate::identify!(Connection))
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
                .debug_struct(crate::identify!(Connection))
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
                .debug_struct(crate::identify!(Connection))
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
                .debug_struct(crate::identify!(Connection))
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

/// Hardcoded `setsockopt` arguments for type safety when calling
/// [`Connection::setsockopt`] in an `unsafe` block.
///
/// Add new variants when you need to call `setsockopt` with new arguments.
pub enum SockOpts {
    /// Set TCP Keep Alive.
    ///
    /// Following text is sourced from <https://tldp.org/HOWTO/html_single/TCP-Keepalive-HOWTO/>.
    ///
    /// ```text
    /// 4.2. The setsockopt function call
    ///
    /// All you need to enable keepalive for a specific socket is to set the specific socket option
    /// on the socket itself. The prototype of the function is as follows:
    ///
    ///
    ///   int setsockopt(int s, int level, int optname,
    ///                  const void *optval, socklen_t optlen)
    ///
    ///
    /// The first parameter is the socket, previously created with the socket(2); the second one
    /// must be SOL_SOCKET, and the third must be SO_KEEPALIVE . The fourth parameter must be a
    /// boolean integer value, indicating that we want to enable the option, while the last is the
    /// size of the value passed before.
    ///
    /// According to the manpage, 0 is returned upon success, and -1 is returned on error (and
    /// errno is properly set).
    ///
    /// There are also three other socket options you can set for keepalive when you write your
    /// application. They all use the SOL_TCP level instead of SOL_SOCKET, and they override
    /// system-wide variables only for the current socket. If you read without writing first, the
    /// current system-wide parameters will be returned.
    ///
    ///     TCP_KEEPCNT: overrides tcp_keepalive_probes
    ///
    ///     TCP_KEEPIDLE: overrides tcp_keepalive_time
    ///
    ///     TCP_KEEPINTVL: overrides tcp_keepalive_intvl
    /// ```
    ///
    /// Field `duration` overrides `tcp_keepalive_time`:
    ///
    /// ```text
    /// tcp_keepalive_time
    ///
    ///    the interval between the last data packet sent (simple ACKs are not considered data) and the
    ///    first keepalive probe; after the connection is marked to need keepalive, this counter is not
    ///    used any further
    /// ```
    ///
    /// The default value in the Linux kernel is 7200 seconds (2 hours).
    KeepAlive {
        enable: bool,
        duration: Option<Duration>,
    },
    TcpNoDelay {
        enable: bool,
    },
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
        let ret = Self::Tcp {
            inner,
            id: None,
            trace: false,
        };
        _ = ret.setsockopt(SockOpts::TcpNoDelay { enable: true });

        ret
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
                    inner.as_fd(),
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
            let raw: c_int = self.__getsockopt(libc::SOL_SOCKET, libc::SO_KEEPALIVE)?;
            if raw == 0 {
                return Ok(None);
            }
            let secs: c_int = self.__getsockopt(libc::IPPROTO_TCP, KEEPALIVE_OPTION)?;
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
        self.setsockopt(SockOpts::KeepAlive {
            enable: keepalive.is_some(),
            duration: keepalive,
        })
    }

    unsafe fn inner_setsockopt<T>(&self, opt: c_int, val: c_int, payload: T) -> std::io::Result<()>
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

    pub fn setsockopt(&self, option: SockOpts) -> std::io::Result<()> {
        match option {
            SockOpts::KeepAlive {
                enable: true,
                duration,
            } => {
                unsafe {
                    self.inner_setsockopt(libc::SOL_SOCKET, libc::SO_KEEPALIVE, <c_int>::from(true))
                }?;
                if let Some(dur) = duration {
                    unsafe {
                        self.inner_setsockopt(
                            libc::IPPROTO_TCP,
                            KEEPALIVE_OPTION,
                            dur.as_secs() as c_int,
                        )
                    }?;
                }
                Ok(())
            }
            SockOpts::KeepAlive {
                enable: false,
                duration: _,
            } => unsafe {
                self.inner_setsockopt(libc::SOL_SOCKET, libc::SO_KEEPALIVE, <c_int>::from(false))
            },
            SockOpts::TcpNoDelay { enable } => unsafe {
                #[cfg(any(
                    target_os = "openbsd",
                    target_os = "netbsd",
                    target_os = "haiku",
                    target_os = "macos",
                    target_os = "ios"
                ))]
                {
                    self.inner_setsockopt(
                        libc::IPPROTO_TCP,
                        libc::TCP_NODELAY,
                        if enable { c_int::from(1_u8) } else { 0 },
                    )
                }
                #[cfg(not(any(
                    target_os = "openbsd",
                    target_os = "netbsd",
                    target_os = "haiku",
                    target_os = "macos",
                    target_os = "ios"
                )))]
                {
                    self.inner_setsockopt(
                        libc::SOL_TCP,
                        libc::TCP_NODELAY,
                        if enable { c_int::from(1_u8) } else { 0 },
                    )
                }
            },
        }
    }

    #[inline]
    unsafe fn __getsockopt<T: Copy>(&self, opt: c_int, val: c_int) -> std::io::Result<T> {
        let mut slot: T = unsafe { std::mem::zeroed() };
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

impl std::io::Read for Connection {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let res = match self {
            Tcp { ref mut inner, .. } => inner.read(buf),
            #[cfg(feature = "tls")]
            Tls { ref mut inner, .. } => inner.read(buf),
            Fd { ref inner, .. } => {
                use std::os::unix::io::{FromRawFd, IntoRawFd};
                let mut f = unsafe { std::fs::File::from_raw_fd(inner.as_raw_fd()) };
                let ret = f.read(buf);
                let _ = f.into_raw_fd();
                ret
            }
            Deflate { ref mut inner, .. } => inner.read(buf),
        };
        if self.is_trace_enabled() {
            let id = self.id();
            match &res {
                Ok(len) => {
                    let slice = &buf[..*len];
                    log::trace!(
                        "{}{}{}{:?} read {:?} bytes:{}",
                        if id.is_some() { "[" } else { "" },
                        if let Some(id) = id.as_ref() { id } else { "" },
                        if id.is_some() { "]: " } else { "" },
                        self,
                        len,
                        std::str::from_utf8(slice)
                            .map(Cow::Borrowed)
                            .or_else(|_| crate::text::hex::bytes_to_hex(slice).map(Cow::Owned))
                            .unwrap_or(Cow::Borrowed("Could not convert to hex."))
                    );
                }
                Err(err) if matches!(err.kind(), std::io::ErrorKind::WouldBlock) => {}
                Err(err) => {
                    log::trace!(
                        "{}{}{}{:?} could not read {:?}",
                        if id.is_some() { "[" } else { "" },
                        if let Some(id) = id.as_ref() { id } else { "" },
                        if id.is_some() { "]: " } else { "" },
                        self,
                        err,
                    );
                }
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
                "{}{}{}{:?} writing {} bytes:{}",
                if id.is_some() { "[" } else { "" },
                if let Some(id) = id.as_ref() { id } else { "" },
                if id.is_some() { "]: " } else { "" },
                self,
                buf.len(),
                std::str::from_utf8(buf)
                    .map(Cow::Borrowed)
                    .or_else(|_| crate::text::hex::bytes_to_hex(buf).map(Cow::Owned))
                    .unwrap_or(Cow::Borrowed("Could not convert to hex."))
            );
        }
        match self {
            Tcp { ref mut inner, .. } => inner.write(buf),
            #[cfg(feature = "tls")]
            Tls { ref mut inner, .. } => inner.write(buf),
            Fd { ref inner, .. } => {
                use std::os::unix::io::{FromRawFd, IntoRawFd};
                let mut f = unsafe { std::fs::File::from_raw_fd(inner.as_raw_fd()) };
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
                let mut f = unsafe { std::fs::File::from_raw_fd(inner.as_raw_fd()) };
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
            Fd { ref inner, .. } => inner.as_raw_fd(),
            Deflate { ref inner, .. } => inner.get_ref().get_ref().as_raw_fd(),
        }
    }
}

impl AsFd for Connection {
    fn as_fd(&'_ self) -> BorrowedFd<'_> {
        match self {
            Tcp { ref inner, .. } => inner.as_fd(),
            #[cfg(feature = "tls")]
            Tls { ref inner, .. } => inner.get_ref().as_fd(),
            Fd { ref inner, .. } => inner.as_fd(),
            Deflate { ref inner, .. } => inner.get_ref().get_ref().as_fd(),
        }
    }
}

unsafe impl async_io::IoSafe for Connection {}

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
        Error::new(format!("Could not lookup address {host}:{port}"))
            .set_kind(ErrorKind::Network(NetworkErrorKind::HostLookupFailed)),
    )
}
