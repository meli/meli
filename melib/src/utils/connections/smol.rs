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

use std::{io::Result, sync::mpsc};

use smol::net::{AsyncToSocketAddrs, SocketAddr, TcpStream};

use super::CONNECTION_ATTEMPT_DELAY;

pub async fn connect<A: AsyncToSocketAddrs + Send>(addr: A) -> Result<TcpStream> {
    // modify the ordered list to interleave address families.
    let (addrs_v4, addrs_v6): (Vec<_>, Vec<_>) = {
        let fut = addr.to_socket_addrs();
        fut.await?.partition(|a| match a {
            SocketAddr::V4(_) => true,
            SocketAddr::V6(_) => false,
        })
    };
    let mut addrs = Vec::with_capacity(addrs_v4.len() + addrs_v6.len());
    let (mut left, mut right) = (addrs_v6.into_iter(), addrs_v4.into_iter());
    while let Some(a) = left.next() {
        addrs.push(a);
        std::mem::swap(&mut left, &mut right);
    }
    addrs.extend(right);

    let (tx, rx) = mpsc::channel();
    let mut last_error = None;
    let mut attempts = Vec::new();
    let mut attempts_count = 0;
    for a in addrs {
        attempts.push(smol::spawn({
            let tx = tx.clone();
            Box::pin(async move {
                let res = TcpStream::connect(a).await;
                tx.send(res).expect("channel is available");
            })
        }));
        attempts_count += 1;
        let recv = rx.recv_timeout(CONNECTION_ATTEMPT_DELAY);
        match recv {
            Ok(Ok(tcp)) => {
                for t in attempts {
                    t.cancel().await;
                }
                return Ok(tcp);
            }
            Ok(Err(error)) => {
                last_error = Some(error);
                attempts_count -= 1;
            }
            Err(mpsc::RecvTimeoutError::Timeout) => (),
            Err(error) => unreachable!("{}", error),
        }
    }
    drop(tx);

    while attempts_count > 0 {
        let res = rx.recv();
        match res {
            Ok(Ok(tcp)) => {
                for t in attempts {
                    t.cancel().await;
                }
                return Ok(tcp);
            }
            Ok(Err(error)) => {
                last_error = Some(error);
                attempts_count -= 1;
            }
            Err(error) => unreachable!("{}", error),
        }
    }
    Err(last_error.unwrap_or_else(|| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            "could not resolve to any address",
        )
    }))
}

/*
#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::{serve_4, serve_6, tar_pit};
    use rand::{thread_rng, Rng};
    use smol::io::ReadExt;
    use std::net::{Ipv4Addr, Ipv6Addr};

    #[async_std::test]
    async fn test_no_ipv4() {
        let port = thread_rng().gen_range(49152..=65535);
        assert!(connect((Ipv4Addr::LOCALHOST, port)).await.is_err());
    }

    #[async_std::test]
    async fn test_connect_ipv4() {
        let (_serve, addr, port) = serve_4();
        let mut data = String::new();
        connect((addr, port))
            .await
            .unwrap()
            .read_to_string(&mut data)
            .await
            .unwrap();
        assert_eq!(data, format!("{addr}"));
        assert!(connect((Ipv6Addr::LOCALHOST, port)).await.is_err());
    }

    #[async_std::test]
    async fn test_no_ipv6() {
        let port = thread_rng().gen_range(49152..=65535);
        assert!(connect((Ipv6Addr::LOCALHOST, port)).await.is_err());
    }

    #[async_std::test]
    async fn test_connect_ipv6() {
        let (_serve, addr, port) = serve_6();
        let mut data = String::new();
        connect((addr, port))
            .await
            .unwrap()
            .read_to_string(&mut data)
            .await
            .unwrap();
        assert_eq!(data, format!("{addr}"));
        assert!(connect((Ipv4Addr::LOCALHOST, port)).await.is_err());
    }

    #[async_std::test]
    async fn test_connect_no_6_but_4() {
        let (_serve, addr, port) = serve_4();
        let expect = format!("{addr}");
        let saddr6: SocketAddr = (Ipv6Addr::LOCALHOST, port).into();
        let saddr4: SocketAddr = (Ipv4Addr::LOCALHOST, port).into();
        let mut data = String::new();
        {
            let saddrs = &[saddr4, saddr6][..];
            data.clear();
            connect(saddrs)
                .await
                .unwrap()
                .read_to_string(&mut data)
                .await
                .unwrap();
            assert_eq!(data, expect);
        }
        {
            let saddrs = &[saddr6, saddr4][..];
            data.clear();
            connect(saddrs)
                .await
                .unwrap()
                .read_to_string(&mut data)
                .await
                .unwrap();
            assert_eq!(data, expect);
        }
    }

    #[async_std::test]
    async fn test_connect_no_4_but_6() {
        let (_serve, addr, port) = serve_6();
        let expect = format!("{addr}");
        let saddr6: SocketAddr = (Ipv6Addr::LOCALHOST, port).into();
        let saddr4: SocketAddr = (Ipv4Addr::LOCALHOST, port).into();
        let mut data = String::new();
        {
            let saddrs = &[saddr4, saddr6][..];
            data.clear();
            connect(saddrs)
                .await
                .unwrap()
                .read_to_string(&mut data)
                .await
                .unwrap();
            assert_eq!(data, expect);
        }
        {
            let saddrs = &[saddr6, saddr4][..];
            data.clear();
            connect(saddrs)
                .await
                .unwrap()
                .read_to_string(&mut data)
                .await
                .unwrap();
            assert_eq!(data, expect);
        }
    }

    #[async_std::test]
    async fn test_connect() {
        let (_serve4, addr4, port4) = serve_4();
        let (_serve6, addr6, port6) = serve_6();
        let saddr4: SocketAddr = (addr4, port4).into();
        let saddr6: SocketAddr = (addr6, port6).into();
        let mut data = String::new();

        data.clear();
        connect(&[saddr6, saddr4][..])
            .await
            .unwrap()
            .read_to_string(&mut data)
            .await
            .unwrap();
        assert_eq!(data, format!("{addr6}"));

        data.clear();
        connect(&[saddr4, saddr6][..])
            .await
            .unwrap()
            .read_to_string(&mut data)
            .await
            .unwrap();
        // IPv6 is preferred
        assert_eq!(data, format!("{addr6}"));
    }

    #[async_std::test]
    async fn test_connect_tar_pit4() {
        let (_serve4, addr4, port4) = tar_pit(Ipv4Addr::LOCALHOST);
        let (_serve6, addr6, port6) = serve_6();
        let saddr4: SocketAddr = (addr4, port4).into();
        let saddr6: SocketAddr = (addr6, port6).into();
        let mut data = String::new();

        data.clear();
        connect(&[saddr4, saddr6][..])
            .await
            .unwrap()
            .read_to_string(&mut data)
            .await
            .unwrap();
        assert_eq!(data, format!("{addr6}"));

        data.clear();
        connect(&[saddr6, saddr4][..])
            .await
            .unwrap()
            .read_to_string(&mut data)
            .await
            .unwrap();
        assert_eq!(data, format!("{addr6}"));
    }

    #[async_std::test]
    async fn test_connect_tar_pit6() {
        let (_serve4, addr4, port4) = serve_4();
        let (_serve6, addr6, port6) = tar_pit(Ipv6Addr::LOCALHOST);
        let saddr4: SocketAddr = (addr4, port4).into();
        let saddr6: SocketAddr = (addr6, port6).into();
        let mut data = String::new();

        data.clear();
        let mut cnx = connect(&[saddr4, saddr6][..]).await.unwrap();
        cnx.read_to_string(&mut data).await.unwrap();
        assert_eq!(data, format!("{addr4}"));

        data.clear();
        connect(&[saddr6, saddr4][..])
            .await
            .unwrap()
            .read_to_string(&mut data)
            .await
            .unwrap();
        assert_eq!(data, format!("{addr4}"));
    }

    #[async_std::test]
    async fn test_connect_tar_pit_all() {
        let (_serve4, addr4, port4) = tar_pit(Ipv4Addr::LOCALHOST);
        let (_serve6, addr6, port6) = tar_pit(Ipv6Addr::LOCALHOST);
        let saddr4: SocketAddr = (addr4, port4).into();
        let saddr6: SocketAddr = (addr6, port6).into();
        assert_eq!(
            connect(&[saddr4, saddr6][..]).await.unwrap_err().kind(),
            std::io::ErrorKind::TimedOut
        );
    }

    #[async_std::test]
    async fn test_connect_empty() {
        let empty = &[][..];
        assert_eq!(
            connect(empty).await.unwrap_err().kind(),
            std::io::ErrorKind::InvalidInput
        );
    }
}
*/
