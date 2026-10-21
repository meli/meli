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

#![cfg(unix)]

use futures::prelude::*;
use melib::utils::logging::{LogLevel, Logger};
use rusty_fork::rusty_fork_test;
use smol::net::{TcpListener, TcpStream};
use tempfile::TempDir;

use crate::types::{ThreadEvent, UIEvent};

rusty_fork_test! {
#[test]
fn varlink() {
    let _logger = Logger::new_with(LogLevel::TRACE, true);
    let temp_dir = TempDir::new().unwrap();
    let ctx = crate::Context::new_mock(&temp_dir);

    let listener = smol::block_on(TcpListener::bind(("127.0.0.1", 0))).unwrap();
    let local_addr = listener.local_addr().unwrap();
    let addr = format!(
        "tcp:{ip}:{port}",
        ip = local_addr.ip(),
        port = local_addr.port()
    );

    let listener = crate::varlink::unix::TcpListener::from(listener);
    log::trace!("varlink listening on {addr}");
    let server = ctx.main_loop_handler.job_executor.spawn(
        "varlink-server".into(),
        crate::varlink::server::serve(listener, None, ctx.main_loop_handler.clone()),
        crate::jobs::IsAsync::Async,
    );
    let mut client = smol::block_on(TcpStream::connect(local_addr)).unwrap();
    log::trace!("connected");
    smol::block_on(client.write_all(b"{\"method\": \"org.varlink.service.GetInfo\"}\0")).unwrap();

    fn read_reply(client: &mut TcpStream) -> String {
        let mut output_buf = vec![];
        let mut output = [0u8; 5];
        loop {
            let b = smol::block_on(client.read(&mut output)).unwrap();
            if b == 0 {
                break;
            }
            output_buf.extend(output[0..b].iter().cloned());
            if output_buf.contains(&b'\0') {
                break;
            }
        }
        String::from_utf8_lossy(&output_buf).into_owned()
    }

    assert_eq!(
        &read_reply(&mut client),
        "{\"parameters\":{\"interfaces\":[\"org.meli-email.meli\",\"org.varlink.service\"],\"product\":\"meli\",\"url\":\"https://meli-email.org\",\"vendor\":\"meli\",\"version\":\"\"}}\0");
    smol::block_on(client.write_all(b"{\"method\": \"org.meli-email.meli.Command\"}\0")).unwrap();
    assert_eq!(
        &read_reply(&mut client),
        "{\"error\":\"invalid type: null, expected struct CommandRequest\"}\0"
    );
    // Send invalid json
    smol::block_on(client.write_all(b"}\0")).unwrap();
    assert_eq!(
        &read_reply(&mut client),
        "{\"error\":\"expected value at line 1 column 1\"}\0"
    );
    smol::block_on(client.write_all(
        b"{\"method\": \"org.meli-email.meli.Command\",\"parameters\":{\"command\":\"quit\"}}\0",
    ))
    .unwrap();
    assert_eq!(&read_reply(&mut client), "{\"parameters\":{}}\0");

    let mut events = vec![];
    while let Ok(ev) = ctx.receiver.try_recv() {
        events.push(ev);
    }

    assert!(
        events
            .iter()
            .filter(|t| matches!(t, ThreadEvent::UIEvent(UIEvent::Command(ref c)) if c == "quit"))
            .count()
            == 1
    );

    drop(server);

    _ = temp_dir.close();
}
}
