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

use std::time::Duration;

use futures::future::{self, Either, Future};

pub async fn timeout<O>(
    dur: Option<Duration>,
    f: impl Future<Output = O> + Send,
) -> crate::Result<O> {
    futures::pin_mut!(f);
    if let Some(dur) = dur {
        match future::select(f, smol::Timer::after(dur)).await {
            Either::Left((out, _)) => Ok(out),
            Either::Right(_) => {
                Err(crate::error::Error::new("Timed out.")
                    .set_kind(crate::error::ErrorKind::Timeout))
            }
        }
    } else {
        Ok(f.await)
    }
}

pub async fn sleep(dur: Duration) {
    smol::Timer::after(dur).await;
}
