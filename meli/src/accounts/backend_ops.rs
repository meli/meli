/*
 * meli - accounts module.
 *
 * Copyright 2023 Manos Pitsidianakis
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

//! Account mail backend operations.

use super::*;

impl Account {
    pub fn set_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: SmallVec<[FlagOp; 8]>,
    ) -> Result<JobId> {
        let fut = self.backend.write().unwrap().set_flags(
            env_hashes.clone(),
            mailbox_hash,
            flags.clone(),
        )?;
        let handle = self
            .main_loop_handler
            .job_executor
            .spawn_specialized("set_flags".into(), fut);
        let job_id = handle.job_id;
        self.insert_job(
            job_id,
            JobRequest::SetFlags {
                env_hashes,
                mailbox_hash,
                flags,
                handle,
            },
        );

        Ok(job_id)
    }

    //     #[cfg(not(feature = "sqlite3"))]
    //     pub(super) fn update_cached_env(&mut self, _: Envelope, _:
    // Option<EnvelopeHash>) {}

    #[cfg(feature = "sqlite3")]
    pub(super) fn update_cached_env(&mut self, env: Envelope, old_hash: Option<EnvelopeHash>) {
        if self.settings.conf.search_backend == crate::conf::SearchBackend::Sqlite3 {
            let msg_id = env.message_id_display().to_string();
            let name = self.name.clone();
            let backend = self.backend.clone();
            let fut = async move {
                crate::sqlite3::AccountCache::remove(
                    name.clone(),
                    old_hash.unwrap_or_else(|| env.hash()),
                )
                .await?;

                crate::sqlite3::AccountCache::insert(env, backend, name).await?;
                Ok(())
            };
            let handle = self
                .main_loop_handler
                .job_executor
                .spawn_specialized("sqlite3::remove".into(), fut);
            self.insert_job(
                handle.job_id,
                JobRequest::Generic {
                    name: format!("Update envelope {} in sqlite3 cache", msg_id).into(),
                    handle,
                    log_level: LogLevel::TRACE,
                    on_finish: None,
                },
            );
        }
    }
}
