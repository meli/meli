//
// meli
//
// Copyright 2017-2018 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

//! Signal handler setup.

use std::os::{fd::AsFd, raw::c_int};

use crate::*;

/// Set up signal handlers and notifying for received signals via `sender`.
pub fn notify(
    signals: &[c_int],
    sender: crossbeam::channel::Sender<ThreadEvent>,
) -> std::result::Result<crossbeam::channel::Receiver<c_int>, std::io::Error> {
    use std::time::Duration;
    let (alarm_pipe_r, alarm_pipe_w) =
        nix::unistd::pipe().map_err(|err| std::io::Error::from_raw_os_error(err as i32))?;
    let alarm_handler = move |info: &nix::libc::siginfo_t| {
        let value = unsafe { info.si_value().sival_ptr as u8 };
        let _ = nix::unistd::write(alarm_pipe_w.as_fd(), &[value]);
    };
    unsafe {
        signal_hook_registry::register_sigaction(signal_hook::consts::SIGALRM, alarm_handler)?;
    }
    let (s, r) = crossbeam::channel::bounded(100);
    let mut signals = signal_hook::iterator::Signals::new(signals)?;
    let _ = nix::fcntl::fcntl(
        alarm_pipe_r.as_fd(),
        nix::fcntl::FcntlArg::F_SETFL(nix::fcntl::OFlag::O_NONBLOCK),
    );
    std::thread::spawn(move || {
        let mut ctr = 0;
        loop {
            ctr %= 3;
            if ctr == 0 {
                let _ = sender
                    .send_timeout(ThreadEvent::Pulse, Duration::from_millis(500))
                    .ok();
            }

            for signal in signals.pending() {
                let _ = s.send_timeout(signal, Duration::from_millis(500)).ok();
            }

            std::thread::sleep(std::time::Duration::from_millis(100));
            ctr += 1;
        }
    });
    Ok(r)
}
