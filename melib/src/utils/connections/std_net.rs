/*
 * Copyright (C) 2023 by Kim Minh Kaplan <kaplan+git@kim-minh.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
 * IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

use std::{
    io::{Error, ErrorKind, Result},
    net::{SocketAddr, TcpStream, ToSocketAddrs},
    time::{Duration, Instant},
};

use polling::{Event, Poller};
use socket2::{Domain, SockAddr, Socket};

/// Opens a TCP connection to a remote host.
///
/// If `addr` yields multiple addresses, `connect` uses the algorithm
/// described in [RFC 8305 Happy Eyeballs Version 2: Better
/// Connectivity Using
/// Concurrency](https://datatracker.ietf.org/doc/html/rfc8305) to
/// connect.
pub fn connect<A: ToSocketAddrs>(addr: A, timeout: Option<Duration>) -> Result<TcpStream> {
    let mut happy = HappyEyeballs::new()?;
    let start = Instant::now();
    let timeout_left = || -> Result<Option<Duration>> {
        let Some(v) = timeout else {
            return Ok(None);
        };
        Ok(Some(v.checked_sub(Instant::now() - start).ok_or_else(
            || Error::new(ErrorKind::TimedOut, "Connection timed out."),
        )?))
    };

    for a in prepare_addresses(addr)? {
        match happy.add(a.into(), Domain::for_address(a)) {
            AddOutcome::Connected(tcp) => return Ok(tcp),
            AddOutcome::Error => continue,
            AddOutcome::InProgress => (),
        }
        if let Some(sock) = happy.poll_once(timeout_left()?)? {
            return Ok(sock);
        }
    }

    while !happy.is_empty() {
        if let Some(sock) = happy.poll_once(timeout_left()?)? {
            return Ok(sock);
        }
    }
    Err(happy
        .error
        .unwrap_or_else(|| Error::new(ErrorKind::InvalidInput, "could not resolve to any address")))
}

/// Resolve addresses and order them to alternate between IPv6 and IPv4.
fn prepare_addresses<A>(addr: A) -> Result<Vec<SocketAddr>>
where
    A: ToSocketAddrs,
{
    let (addrs_v4, addrs_v6): (Vec<_>, Vec<_>) = addr.to_socket_addrs()?.partition(|a| match a {
        SocketAddr::V4(_) => true,
        SocketAddr::V6(_) => false,
    });
    let mut addrs = Vec::with_capacity(addrs_v4.len() + addrs_v6.len());
    let (mut left, mut right) = (addrs_v6.into_iter(), addrs_v4.into_iter());
    while let Some(a) = left.next() {
        addrs.push(a);
        std::mem::swap(&mut left, &mut right);
    }
    addrs.extend(right);
    Ok(addrs)
}

struct HappyEyeballs {
    poller: Poller,
    error: Option<Error>,
    attempts: Vec<Option<(SockAddr, Socket)>>,
    attempts_in_progress: usize,
}

impl HappyEyeballs {
    fn new() -> Result<Self> {
        Ok(Self {
            poller: Poller::new()?,
            error: None,
            attempts: Vec::new(),
            attempts_in_progress: 0,
        })
    }

    fn is_empty(&self) -> bool {
        self.attempts_in_progress == 0
    }

    fn set_error(&mut self, error: Error) {
        self.error.get_or_insert(error);
    }

    /// Initiate a fresh non-blocking TCP connection to `saddr`.
    ///
    /// Returns `AddOutcome::InProgress`.
    ///
    /// If there is an error concerning this particular connection
    /// attempt, return `AddOutcome::Error` and the error code is
    /// remembered in `self` if it is the first to occur; it will be
    /// the one returned if no connection can be established at all.
    ///
    /// If the connection succeeds immediately, returns
    /// `AddOutcome::Connected(stream)`. Because of non-blocking and
    /// the way TCP works, this should *not* happen.
    fn add(&mut self, saddr: SockAddr, domain: Domain) -> AddOutcome {
        let set_nonblocking =
            Socket::new(domain, socket2::Type::STREAM, Some(socket2::Protocol::TCP)).and_then(
                |sock| {
                    sock.set_nonblocking(true)?;
                    Ok(sock)
                },
            );
        let sock = match set_nonblocking {
            Ok(sock) => sock,
            Err(e) => {
                self.set_error(e);
                return AddOutcome::Error;
            }
        };
        match sock.connect(&saddr) {
            Ok(()) => match sock.set_nonblocking(false) {
                Ok(()) => return AddOutcome::Connected(sock.into()),
                Err(e) => self.set_error(e),
            },
            Err(e) if e.raw_os_error() == Some(libc::EINPROGRESS) => {
                let interest = Event::writable(self.attempts.len());
                match self.poller.add(&sock, interest) {
                    Ok(()) => {
                        self.attempts.push(Some((saddr, sock)));
                        self.attempts_in_progress += 1;
                        return AddOutcome::InProgress;
                    }
                    Err(e) => self.set_error(e),
                }
            }
            Err(e) => self.set_error(e),
        }
        AddOutcome::Error
    }

    fn poll_once(&mut self, timeout: Option<Duration>) -> Result<Option<TcpStream>> {
        let mut events = Vec::new();
        self.poller.wait(&mut events, timeout)?;
        for evt in &events {
            assert!(evt.writable);
            let (sock_addr, sock) = self.attempts[evt.key].take().expect("attempt exists");
            self.attempts_in_progress -= 1;
            self.poller.delete(&sock).expect("socket is in poll set");
            match nix::sys::socket::getsockopt(&sock, nix::sys::socket::sockopt::SocketError) {
                Err(err) => self.set_error(err.into()),
                Ok(0) => {
                    if let Some(tcp) = self.socket_into_blocking_tcp_stream(sock) {
                        return Ok(Some(tcp));
                    }
                    return Ok(None);
                }
                Ok(_) => {}
            }
            match sock.connect(&sock_addr) {
                Err(err) => self.set_error(err),
                Ok(()) => {
                    if let Some(tcp) = self.socket_into_blocking_tcp_stream(sock) {
                        return Ok(Some(tcp));
                    }
                }
            }
        }
        Ok(None)
    }

    fn socket_into_blocking_tcp_stream(&mut self, sock: Socket) -> Option<TcpStream> {
        match sock.set_nonblocking(false) {
            Ok(()) => Some(sock.into()),
            Err(err) => {
                self.set_error(err);
                None
            }
        }
    }
}

enum AddOutcome {
    Connected(TcpStream),
    InProgress,
    Error,
}

/*
#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{serve_4, serve_6, tar_pit};
    use rand::{thread_rng, Rng};
    use std::io::Read;
    use std::net::{Ipv4Addr, Ipv6Addr};

    #[test]
    fn test_no_ipv4() {
        let port = thread_rng().gen_range(49152..=65535);
        assert!(connect((Ipv4Addr::LOCALHOST, port)).is_err());
    }

    #[test]
    fn test_connect_ipv4() {
        let (_serve, addr, port) = serve_4();
        let mut data = String::new();
        connect((addr, port))
            .unwrap()
            .read_to_string(&mut data)
            .unwrap();
        assert_eq!(data, format!("{addr}"));
        assert!(connect((Ipv6Addr::LOCALHOST, port)).is_err());
    }

    #[test]
    fn test_no_ipv6() {
        let port = thread_rng().gen_range(49152..=65535);
        assert!(connect((Ipv6Addr::LOCALHOST, port)).is_err());
    }

    #[test]
    fn test_connect_ipv6() {
        let (_serve, addr, port) = serve_6();
        let mut data = String::new();
        connect((addr, port))
            .unwrap()
            .read_to_string(&mut data)
            .unwrap();
        assert_eq!(data, format!("{addr}"));
        assert!(connect((Ipv4Addr::LOCALHOST, port)).is_err());
    }

    #[test]
    fn test_connect_no_6_but_4() {
        let (_serve, addr, port) = serve_4();
        let expect = format!("{addr}");
        let saddr6: SocketAddr = (Ipv6Addr::LOCALHOST, port).into();
        let saddr4: SocketAddr = (Ipv4Addr::LOCALHOST, port).into();
        let mut data = String::new();
        {
            let saddrs = &[saddr4, saddr6][..];
            data.clear();
            connect(saddrs).unwrap().read_to_string(&mut data).unwrap();
            assert_eq!(data, expect);
        }
        {
            let saddrs = &[saddr6, saddr4][..];
            data.clear();
            connect(saddrs).unwrap().read_to_string(&mut data).unwrap();
            assert_eq!(data, expect);
        }
    }

    #[test]
    fn test_connect_no_4_but_6() {
        let (_serve, addr, port) = serve_6();
        let expect = format!("{addr}");
        let saddr6: SocketAddr = (Ipv6Addr::LOCALHOST, port).into();
        let saddr4: SocketAddr = (Ipv4Addr::LOCALHOST, port).into();
        let mut data = String::new();
        {
            let saddrs = &[saddr4, saddr6][..];
            data.clear();
            connect(saddrs).unwrap().read_to_string(&mut data).unwrap();
            assert_eq!(data, expect);
        }
        {
            let saddrs = &[saddr6, saddr4][..];
            data.clear();
            connect(saddrs).unwrap().read_to_string(&mut data).unwrap();
            assert_eq!(data, expect);
        }
    }

    #[test]
    fn test_connect() {
        let (_serve4, addr4, port4) = serve_4();
        let (_serve6, addr6, port6) = serve_6();
        let saddr4: SocketAddr = (addr4, port4).into();
        let saddr6: SocketAddr = (addr6, port6).into();
        let mut data = String::new();

        data.clear();
        connect(&[saddr6, saddr4][..])
            .unwrap()
            .read_to_string(&mut data)
            .unwrap();
        assert_eq!(data, format!("{addr6}"));

        data.clear();
        connect(&[saddr4, saddr6][..])
            .unwrap()
            .read_to_string(&mut data)
            .unwrap();
        // IPv6 is preferred
        assert_eq!(data, format!("{addr6}"));
    }

    #[test]
    fn test_connect_tar_pit4() {
        let (_serve4, addr4, port4) = tar_pit(Ipv4Addr::LOCALHOST);
        let (_serve6, addr6, port6) = serve_6();
        let saddr4: SocketAddr = (addr4, port4).into();
        let saddr6: SocketAddr = (addr6, port6).into();
        let mut data = String::new();

        data.clear();
        connect(&[saddr4, saddr6][..])
            .unwrap()
            .read_to_string(&mut data)
            .unwrap();
        assert_eq!(data, format!("{addr6}"));

        data.clear();
        connect(&[saddr6, saddr4][..])
            .unwrap()
            .read_to_string(&mut data)
            .unwrap();
        assert_eq!(data, format!("{addr6}"));
    }

    #[test]
    fn test_connect_tar_pit6() {
        let (_serve4, addr4, port4) = serve_4();
        let (_serve6, addr6, port6) = tar_pit(Ipv6Addr::LOCALHOST);
        let saddr4: SocketAddr = (addr4, port4).into();
        let saddr6: SocketAddr = (addr6, port6).into();
        let mut data = String::new();

        data.clear();
        let mut cnx = connect(&[saddr4, saddr6][..]).unwrap();
        cnx.read_to_string(&mut data).unwrap();
        assert_eq!(data, format!("{addr4}"));

        data.clear();
        connect(&[saddr6, saddr4][..])
            .unwrap()
            .read_to_string(&mut data)
            .unwrap();
        assert_eq!(data, format!("{addr4}"));
    }

    #[test]
    fn test_connect_tar_pit_all() {
        let (_serve4, addr4, port4) = tar_pit(Ipv4Addr::LOCALHOST);
        let (_serve6, addr6, port6) = tar_pit(Ipv6Addr::LOCALHOST);
        let saddr4: SocketAddr = (addr4, port4).into();
        let saddr6: SocketAddr = (addr6, port6).into();
        assert_eq!(
            connect(&[saddr4, saddr6][..]).unwrap_err().kind(),
            std::io::ErrorKind::TimedOut
        );
    }

    #[test]
    fn test_connect_empty() {
        let empty = &[][..];
        assert_eq!(
            connect(empty).unwrap_err().kind(),
            std::io::ErrorKind::InvalidInput
        );
    }
}
*/
