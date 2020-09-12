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
                .map_err(|err| {
                    std::io::Error::from_raw_os_error(err.as_errno().map(|n| n as i32).unwrap_or(0))
                })?;
                Ok(())
            }
            #[cfg(feature = "deflate_compression")]
            Deflate { ref inner, .. } => inner.get_ref().get_ref().set_nonblocking(nonblocking),
        }
    }

    pub fn set_read_timeout(&self, dur: Option<std::time::Duration>) -> std::io::Result<()> {
        match self {
            Tcp(ref t) => t.set_read_timeout(dur),
            #[cfg(feature = "tls")]
            Tls(ref t) => t.get_ref().set_read_timeout(dur),
            Fd(_) => Ok(()),
            #[cfg(feature = "deflate_compression")]
            Deflate { ref inner, .. } => inner.get_ref().get_ref().set_read_timeout(dur),
        }
    }

    pub fn set_write_timeout(&self, dur: Option<std::time::Duration>) -> std::io::Result<()> {
        match self {
            Tcp(ref t) => t.set_write_timeout(dur),
            #[cfg(feature = "tls")]
            Tls(ref t) => t.get_ref().set_write_timeout(dur),
            Fd(_) => Ok(()),
            #[cfg(feature = "deflate_compression")]
            Deflate { ref inner, .. } => inner.get_ref().get_ref().set_write_timeout(dur),
        }
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
        crate::error::MeliError::new(format!("Could not lookup address {}:{}", host, port))
            .set_kind(crate::error::ErrorKind::Network),
    )
}

use futures::future::{self, Either, Future};

pub async fn timeout<O>(
    dur: Option<std::time::Duration>,
    f: impl Future<Output = O>,
) -> crate::Result<O> {
    futures::pin_mut!(f);
    if let Some(dur) = dur {
        match future::select(f, smol::Timer::after(dur)).await {
            Either::Left((out, _)) => Ok(out),
            Either::Right(_) => Err(crate::error::MeliError::new("Timed out.")
                .set_kind(crate::error::ErrorKind::Timeout)),
        }
    } else {
        Ok(f.await)
    }
}
