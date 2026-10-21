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

use std::{pin::Pin, time::Duration};

use futures::{
    future::{self, BoxFuture, Either},
    io::{AsyncRead, AsyncWrite},
};
use melib::error::{Error, ErrorKind, Result};

use crate::varlink::handler;

pub trait Stream: AsyncRead + AsyncWrite + Send + Sync {
    fn shutdown(&mut self) -> Result<()>;
    fn clone(&self) -> Pin<Box<dyn Stream>>;
}

pub trait Listener: Send + 'static {
    fn accept(&self) -> BoxFuture<'_, Result<Pin<Box<dyn Stream>>>>;
}

impl Listener for Box<dyn Listener> {
    fn accept(&self) -> BoxFuture<'_, Result<Pin<Box<dyn Stream>>>> {
        (**self).accept()
    }
}

#[cfg(unix)]
/// See <https://varlink.org/#activation>
pub fn detect_listen_socket() -> Option<usize> {
    let listen_fds = std::env::var("LISTEN_FDS").ok()?;

    let n_fds = listen_fds.parse::<usize>().ok()?;
    if n_fds == 0 {
        return None;
    }
    if !matches!(std::env::var("LISTEN_PID"), Ok(pid) if pid.parse::<usize>() == Ok(std::process::id() as usize))
    {
        return None;
    }

    if n_fds == 1 {
        return Some(3);
    }

    std::env::var("LISTEN_FDNAMES")
        .ok()?
        .split(':')
        .enumerate()
        .find_map(|(i, name)| if name == "varlink" { Some(i + 3) } else { None })
}

#[cfg(not(unix))]
pub fn new_listener(_: &str, _: bool) -> Result<Box<dyn Listener>> {
    Err(Error::new("Sockets not implemented in this platform").set_kind(ErrorKind::NotImplemented))
}

#[cfg(unix)]
pub fn new_listener(addr: &str, replace: bool) -> Result<Box<dyn Listener>> {
    use crate::varlink::unix;

    let listen_socket = detect_listen_socket();
    if let Some(addr) = addr.strip_prefix("unix:@") {
        Ok(Box::new(unix::UnixListener::new_abstract(
            addr,
            listen_socket,
            replace,
        )?))
    } else if let Some(addr) = addr.strip_prefix("unix:") {
        Ok(Box::new(unix::UnixListener::new(
            addr,
            listen_socket,
            replace,
        )?))
    } else if let Some(addr) = addr.strip_prefix("tcp:") {
        Ok(Box::new(unix::TcpListener::new(addr, listen_socket)?))
    } else {
        Err(Error::new("Unrecognized address format")
            .set_details(format!(
                "Address {addr} is not recognized. Example values: `tcp:127.0.0.1:12345`, \
                 `unix:/run/org.meli-email.meli`, `unix:@org.meli-email.meli`"
            ))
            .set_kind(ErrorKind::ValueError))
    }
}

pub async fn serve<L: Listener>(
    listener: L,
    timeout: Option<Duration>,
    main_loop_handler: crate::state::MainLoopHandler,
) -> Result<()> {
    loop {
        let conn = if let Some(timeout) = timeout {
            match future::select(listener.accept(), smol::Timer::after(timeout)).await {
                Either::Left((out, _)) => out?,
                Either::Right(_) => {
                    return Err(Error::new("Timed out").set_kind(ErrorKind::TimedOut))
                }
            }
        } else {
            listener.accept().await?
        };
        let _main_loop_handler = main_loop_handler.clone();
        let job = main_loop_handler.job_executor.spawn(
            "varlink-connection".into(),
            async move {
                let handler = handler::Handler::new(_main_loop_handler, conn);
                handler.handle().await
            },
            true.into(),
        );
        main_loop_handler
            .sender
            .send(crate::types::ThreadEvent::Ipc(job))
            .unwrap();
    }
}
