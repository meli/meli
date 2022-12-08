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
#[cfg(feature = "deflate_compression")]
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
use std::os::unix::io::AsRawFd;
use std::time::Duration;

#[derive(Debug)]
pub enum Connection {
    Tcp(std::net::TcpStream),
    Fd(std::os::unix::io::RawFd),
    #[cfg(feature = "tls")]
    Tls(native_tls::TlsStream<Self>),
    #[cfg(feature = "deflate_compression")]
    Deflate {
        inner: DeflateEncoder<DeflateDecoder<Box<Self>>>,
    },
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
    #[cfg(feature = "deflate_compression")]
    pub fn deflate(self) -> Self {
        Connection::Deflate {
            inner: DeflateEncoder::new(
                DeflateDecoder::new_with_buf(Box::new(self), vec![0; Self::IO_BUF_SIZE]),
                Compression::default(),
            ),
        }
    }

    pub fn set_nonblocking(&self, nonblocking: bool) -> std::io::Result<()> {
        match self {
            Tcp(ref t) => t.set_nonblocking(nonblocking),
            #[cfg(feature = "tls")]
            Tls(ref t) => t.get_ref().set_nonblocking(nonblocking),
            Fd(fd) => {
                //FIXME TODO Review
                nix::fcntl::fcntl(
                    *fd,
                    nix::fcntl::FcntlArg::F_SETFL(if nonblocking {
                        nix::fcntl::OFlag::O_NONBLOCK
                    } else {
                        !nix::fcntl::OFlag::O_NONBLOCK
                    }),
                )
                .map_err(|err| std::io::Error::from_raw_os_error(err as i32))?;
                Ok(())
            }
            #[cfg(feature = "deflate_compression")]
            Deflate { ref inner, .. } => inner.get_ref().get_ref().set_nonblocking(nonblocking),
        }
    }

    pub fn set_read_timeout(&self, dur: Option<Duration>) -> std::io::Result<()> {
        match self {
            Tcp(ref t) => t.set_read_timeout(dur),
            #[cfg(feature = "tls")]
            Tls(ref t) => t.get_ref().set_read_timeout(dur),
            Fd(_) => Ok(()),
            #[cfg(feature = "deflate_compression")]
            Deflate { ref inner, .. } => inner.get_ref().get_ref().set_read_timeout(dur),
        }
    }

    pub fn set_write_timeout(&self, dur: Option<Duration>) -> std::io::Result<()> {
        match self {
            Tcp(ref t) => t.set_write_timeout(dur),
            #[cfg(feature = "tls")]
            Tls(ref t) => t.get_ref().set_write_timeout(dur),
            Fd(_) => Ok(()),
            #[cfg(feature = "deflate_compression")]
            Deflate { ref inner, .. } => inner.get_ref().get_ref().set_write_timeout(dur),
        }
    }

    pub fn keepalive(&self) -> std::io::Result<Option<Duration>> {
        if let Fd(_) = self {
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
        if let Fd(_) = self {
            return Ok(());
        }
        unsafe {
            self.setsockopt(
                libc::SOL_SOCKET,
                libc::SO_KEEPALIVE,
                keepalive.is_some() as c_int,
            )?;
            if let Some(dur) = keepalive {
                // TODO: checked cast here
                self.setsockopt(libc::IPPROTO_TCP, KEEPALIVE_OPTION, dur.as_secs() as c_int)?;
            }
            Ok(())
        }
    }

    unsafe fn setsockopt<T>(&self, opt: c_int, val: c_int, payload: T) -> std::io::Result<()>
    where
        T: Copy,
    {
        let payload = &payload as *const T as *const c_void;
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
            &mut slot as *mut _ as *mut _,
            &mut len,
        ))?;
        assert_eq!(len as usize, std::mem::size_of::<T>());
        Ok(slot)
    }
}

impl Drop for Connection {
    fn drop(&mut self) {
        if let Fd(fd) = self {
            let _ = nix::unistd::close(*fd);
        }
    }
}

impl std::io::Read for Connection {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            Tcp(ref mut t) => t.read(buf),
            #[cfg(feature = "tls")]
            Tls(ref mut t) => t.read(buf),
            Fd(f) => {
                use std::os::unix::io::{FromRawFd, IntoRawFd};
                let mut f = unsafe { std::fs::File::from_raw_fd(*f) };
                let ret = f.read(buf);
                let _ = f.into_raw_fd();
                ret
            }
            #[cfg(feature = "deflate_compression")]
            Deflate { ref mut inner, .. } => inner.read(buf),
        }
    }
}

impl std::io::Write for Connection {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Tcp(ref mut t) => t.write(buf),
            #[cfg(feature = "tls")]
            Tls(ref mut t) => t.write(buf),
            Fd(f) => {
                use std::os::unix::io::{FromRawFd, IntoRawFd};
                let mut f = unsafe { std::fs::File::from_raw_fd(*f) };
                let ret = f.write(buf);
                let _ = f.into_raw_fd();
                ret
            }
            #[cfg(feature = "deflate_compression")]
            Deflate { ref mut inner, .. } => inner.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Tcp(ref mut t) => t.flush(),
            #[cfg(feature = "tls")]
            Tls(ref mut t) => t.flush(),
            Fd(f) => {
                use std::os::unix::io::{FromRawFd, IntoRawFd};
                let mut f = unsafe { std::fs::File::from_raw_fd(*f) };
                let ret = f.flush();
                let _ = f.into_raw_fd();
                ret
            }
            #[cfg(feature = "deflate_compression")]
            Deflate { ref mut inner, .. } => inner.flush(),
        }
    }
}

impl std::os::unix::io::AsRawFd for Connection {
    fn as_raw_fd(&self) -> std::os::unix::io::RawFd {
        match self {
            Tcp(ref t) => t.as_raw_fd(),
            #[cfg(feature = "tls")]
            Tls(ref t) => t.get_ref().as_raw_fd(),
            Fd(f) => *f,
            #[cfg(feature = "deflate_compression")]
            Deflate { ref inner, .. } => inner.get_ref().get_ref().as_raw_fd(),
        }
    }
}

pub fn lookup_ipv4(host: &str, port: u16) -> crate::Result<std::net::SocketAddr> {
    use std::net::ToSocketAddrs;

    let addrs = (host, port).to_socket_addrs()?;
    for addr in addrs {
        if let std::net::SocketAddr::V4(_) = addr {
            return Ok(addr);
        }
    }

    Err(
        crate::error::Error::new(format!("Could not lookup address {}:{}", host, port))
            .set_kind(crate::error::ErrorKind::Network(
                crate::error::NetworkErrorKind::HostLookupFailed,
            )),
    )
}

use futures::future::{self, Either, Future};

pub async fn timeout<O>(dur: Option<Duration>, f: impl Future<Output = O>) -> crate::Result<O> {
    futures::pin_mut!(f);
    if let Some(dur) = dur {
        match future::select(f, smol::Timer::after(dur)).await {
            Either::Left((out, _)) => Ok(out),
            Either::Right(_) => Err(crate::error::Error::new("Timed out.")
                .set_kind(crate::error::ErrorKind::Timeout)),
        }
    } else {
        Ok(f.await)
    }
}

pub async fn sleep(dur: Duration) {
    smol::Timer::after(dur).await;
}
