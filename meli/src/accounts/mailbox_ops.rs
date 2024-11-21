//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use melib::conf::ToggleFlag;

use super::*;
use crate::command::actions::MailboxOperation;

impl Account {
    pub fn mailbox_operation(&mut self, op: MailboxOperation) -> Result<()> {
        if self.settings.account.read_only {
            return Err(Error::new("Account is read-only."));
        }
        match op {
            MailboxOperation::Create(path) => {
                let job = self
                    .backend
                    .write()
                    .unwrap()
                    .create_mailbox(path.to_string())?;
                let handle = self.main_loop_handler.job_executor.spawn(
                    "create_mailbox".into(),
                    job,
                    self.is_async(),
                );
                self.insert_job(
                    handle.job_id,
                    JobRequest::Mailbox(MailboxJobRequest::CreateMailbox { path, handle }),
                );
                Ok(())
            }
            MailboxOperation::Delete(path) => {
                if self.mailbox_entries.len() == 1 {
                    return Err(Error::new("Cannot delete only mailbox."));
                }

                let mailbox_hash = self.mailbox_by_path(&path)?;
                let job = self.backend.write().unwrap().delete_mailbox(mailbox_hash)?;
                let handle = self.main_loop_handler.job_executor.spawn(
                    "delete-mailbox".into(),
                    job,
                    self.is_async(),
                );
                self.insert_job(
                    handle.job_id,
                    JobRequest::Mailbox(MailboxJobRequest::DeleteMailbox {
                        mailbox_hash,
                        handle,
                    }),
                );
                Ok(())
            }
            MailboxOperation::Subscribe(path) => {
                let mailbox_hash = self.mailbox_by_path(&path)?;
                let job = self
                    .backend
                    .write()
                    .unwrap()
                    .set_mailbox_subscription(mailbox_hash, true)?;
                let handle = self.main_loop_handler.job_executor.spawn(
                    "subscribe-mailbox".into(),
                    job,
                    self.is_async(),
                );
                self.insert_job(
                    handle.job_id,
                    JobRequest::Mailbox(MailboxJobRequest::SetMailboxSubscription {
                        mailbox_hash,
                        new_value: true,
                        handle,
                    }),
                );
                Ok(())
            }
            MailboxOperation::Unsubscribe(path) => {
                let mailbox_hash = self.mailbox_by_path(&path)?;
                let job = self
                    .backend
                    .write()
                    .unwrap()
                    .set_mailbox_subscription(mailbox_hash, false)?;
                let handle = self.main_loop_handler.job_executor.spawn(
                    "unsubscribe-mailbox".into(),
                    job,
                    self.is_async(),
                );
                self.insert_job(
                    handle.job_id,
                    JobRequest::Mailbox(MailboxJobRequest::SetMailboxSubscription {
                        mailbox_hash,
                        new_value: false,
                        handle,
                    }),
                );
                Ok(())
            }
            MailboxOperation::Rename(path, new_path) => {
                let mailbox_hash = self.mailbox_by_path(&path)?;
                let job = self
                    .backend
                    .write()
                    .unwrap()
                    .rename_mailbox(mailbox_hash, new_path.clone())?;
                let handle = self.main_loop_handler.job_executor.spawn(
                    format!("rename-mailbox {path} to {new_path}").into(),
                    job,
                    self.is_async(),
                );
                self.insert_job(
                    handle.job_id,
                    JobRequest::Mailbox(MailboxJobRequest::RenameMailbox {
                        handle,
                        mailbox_hash,
                        new_path,
                    }),
                );
                Ok(())
            }
            MailboxOperation::SetPermissions(_) => Err(Error::new("Not implemented.")),
        }
    }

    pub fn process_mailbox_event(&mut self, job_id: JobId, mut job: MailboxJobRequest) {
        macro_rules! try_handle {
            ($handle:ident, $binding:pat => $then:block) => {{
                try_handle! { $handle, Err(err) => {
                    self.main_loop_handler
                        .job_executor
                        .set_job_success(job_id, false);
                    self.main_loop_handler
                        .send(ThreadEvent::UIEvent(UIEvent::Notification {
                            title: None,
                            body: format!("{}: {} failed", &self.name, job).into(),
                            kind: Some(NotificationType::Error(err.kind)),
                            source: Some(err),
                        }));
                    return;
                },
                $binding => $then
                }
            }};
            ($handle:ident, Err($err:pat) => $then_err: block, $binding:pat => $then:block) => {{
                match $handle.chan.try_recv() {
                    _err @ Ok(None) | _err @ Err(_) => {
                        /* canceled */
                        #[cfg(debug_assertions)]
                        log::trace!(
                            "handle.chan.try_recv() for job {} returned {:?}",
                            job_id,
                            _err
                        );
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                    }
                    Ok(Some(Err($err))) => $then_err,
                    Ok(Some(Ok($binding))) => $then,
                }
            }};
        }

        match job {
            MailboxJobRequest::Mailboxes { ref mut handle } => {
                if let Ok(Some(mailboxes)) = handle.chan.try_recv() {
                    if let Err(err) = mailboxes.and_then(|mailboxes| self.init(mailboxes)) {
                        if !err.is_recoverable() {
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(self.name.to_string().into()),
                                    source: Some(err.clone()),
                                    body: err.to_string().into(),
                                    kind: Some(NotificationType::Error(err.kind)),
                                },
                            ));
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::AccountStatusChange(
                                    self.hash,
                                    Some(err.to_string().into()),
                                ),
                            ));
                            self.is_online.set_err(err);
                            self.main_loop_handler
                                .job_executor
                                .set_job_success(job_id, false);
                            return;
                        }
                        let mailboxes_job = self.backend.read().unwrap().mailboxes();
                        if let Ok(mailboxes_job) = mailboxes_job {
                            let handle = self.main_loop_handler.job_executor.spawn(
                                "list-mailboxes".into(),
                                mailboxes_job,
                                self.is_async(),
                            );
                            self.insert_job(
                                handle.job_id,
                                JobRequest::Mailbox(MailboxJobRequest::Mailboxes { handle }),
                            );
                        };
                    } else {
                        self.main_loop_handler.send(ThreadEvent::UIEvent(
                            UIEvent::AccountStatusChange(
                                self.hash,
                                Some("Loaded mailboxes.".into()),
                            ),
                        ));
                    }
                }
            }
            MailboxJobRequest::CreateMailbox { ref mut handle, .. } => {
                try_handle! { handle, (mailbox_hash, mut mailboxes) => {
                    self.main_loop_handler.send(ThreadEvent::UIEvent(
                            UIEvent::MailboxCreate((self.hash, mailbox_hash)),
                    ));
                    let mut new = FileMailboxConf::default();
                    new.mailbox_conf.subscribe = ToggleFlag::InternalVal(true);
                    new.mailbox_conf.usage = if mailboxes[&mailbox_hash].special_usage()
                        != SpecialUsageMailbox::Normal
                    {
                        Some(mailboxes[&mailbox_hash].special_usage())
                    } else {
                        let tmp = SpecialUsageMailbox::detect_usage(
                            mailboxes[&mailbox_hash].name(),
                        );
                        if let Some(tmp) = tmp.filter(|&v| v != SpecialUsageMailbox::Normal)
                        {
                            mailboxes.entry(mailbox_hash).and_modify(|entry| {
                                let _ = entry.set_special_usage(tmp);
                            });
                        }
                        tmp
                    };
                    // if new mailbox has parent, we need to update its children field
                    if let Some(parent_hash) = mailboxes[&mailbox_hash].parent() {
                        self.mailbox_entries
                            .entry(parent_hash)
                            .and_modify(|parent| {
                                parent.ref_mailbox =
                                    mailboxes.remove(&parent_hash).unwrap();
                            });
                    }
                    let status = MailboxStatus::default();

                    self.mailbox_entries.insert(
                        mailbox_hash,
                        MailboxEntry::new(
                            status,
                            mailboxes[&mailbox_hash].path().to_string(),
                            mailboxes.remove(&mailbox_hash).unwrap(),
                            new,
                        ),
                    );
                    self.collection
                        .threads
                        .write()
                        .unwrap()
                        .insert(mailbox_hash, Threads::default());
                    self.collection
                        .mailboxes
                        .write()
                        .unwrap()
                        .insert(mailbox_hash, Default::default());
                    build_mailboxes_order(
                        &mut self.tree,
                        &self.mailbox_entries,
                        &mut self.mailboxes_order,
                    );
                }}
            }
            MailboxJobRequest::DeleteMailbox {
                mailbox_hash,
                ref mut handle,
                ..
            } => {
                try_handle! { handle, mut mailboxes => {
                    self.main_loop_handler
                        .send(ThreadEvent::UIEvent(UIEvent::MailboxDelete((
                                        self.hash,
                                        mailbox_hash,
                        ))));
                    if let Some(pos) =
                        self.mailboxes_order.iter().position(|&h| h == mailbox_hash)
                    {
                        self.mailboxes_order.remove(pos);
                    }
                    if let Some(pos) = self.tree.iter().position(|n| n.hash == mailbox_hash) {
                        self.tree.remove(pos);
                    }
                    if self.settings.sent_mailbox == Some(mailbox_hash) {
                        self.settings.sent_mailbox = None;
                    }
                    self.collection
                        .threads
                        .write()
                        .unwrap()
                        .remove(&mailbox_hash);
                    let deleted_mailbox =
                        self.mailbox_entries.shift_remove(&mailbox_hash).unwrap();
                    // if deleted mailbox had parent, we need to update its children field
                    if let Some(parent_hash) = deleted_mailbox.ref_mailbox.parent() {
                        self.mailbox_entries
                            .entry(parent_hash)
                            .and_modify(|parent| {
                                parent.ref_mailbox = mailboxes.remove(&parent_hash).unwrap();
                            });
                    }
                    self.collection
                        .mailboxes
                        .write()
                        .unwrap()
                        .remove(&mailbox_hash);
                    build_mailboxes_order(
                        &mut self.tree,
                        &self.mailbox_entries,
                        &mut self.mailboxes_order,
                    );
                    // [ref:FIXME] remove from settings as well

                    self.main_loop_handler
                        .send(ThreadEvent::UIEvent(UIEvent::Notification {
                            title: Some(
                                       format!("{}: mailbox deleted successfully", &self.name).into(),
                                   ),
                                   source: None,
                                   body: "".into(),
                                   kind: Some(NotificationType::Info),
                        }));
                }}
            }
            MailboxJobRequest::RenameMailbox {
                ref mut handle,
                mailbox_hash,
                ref mut new_path,
            } => {
                use indexmap::map::MutableKeys;
                try_handle! { handle, mailbox => {
                    let new_hash = mailbox.hash();
                    if let Some((_, key, entry)) = self.mailbox_entries.get_full_mut2(&mailbox_hash) {
                        *key = new_hash;
                        *entry = MailboxEntry::new(entry.status.clone(), std::mem::take(new_path), mailbox, entry.conf.clone());
                    }
                    if let Some(key) = self.mailboxes_order.iter_mut().find(|k| **k == mailbox_hash) {
                        *key = new_hash;
                    }
                    if let Some((_, key, _)) = self.event_queue.get_full_mut2(&mailbox_hash) {
                        *key = new_hash;
                    }
                    {
                        let mut threads = self.collection.threads.write().unwrap();
                        if let Some(entry) = threads.remove(&mailbox_hash) {
                            threads.insert(new_hash, entry);
                        }
                    }
                    {
                        let mut mailboxes = self.collection.mailboxes.write().unwrap();
                        if let Some(entry) = mailboxes.remove(&mailbox_hash) {
                            mailboxes.insert(new_hash, entry);
                        }
                    }
                    build_mailboxes_order(
                        &mut self.tree,
                        &self.mailbox_entries,
                        &mut self.mailboxes_order,
                    );
                }}
            }
            MailboxJobRequest::SetMailboxPermissions { ref mut handle, .. } => {
                try_handle! { handle, _ => {
                    self.main_loop_handler
                        .send(ThreadEvent::UIEvent(UIEvent::Notification {
                            title: Some(
                                       format!("{}: mailbox permissions set successfully", &self.name)
                                       .into(),
                                   ),
                                   source: None,
                                   body: "".into(),
                                   kind: Some(NotificationType::Info),
                        }));
                }}
            }
            MailboxJobRequest::SetMailboxSubscription {
                ref mut handle,
                ref mailbox_hash,
                ref new_value,
            } => {
                try_handle! { handle, () => {
                    if self.mailbox_entries.contains_key(mailbox_hash) {
                        self.mailbox_entries.entry(*mailbox_hash).and_modify(|m| {
                            m.conf.mailbox_conf.subscribe = if *new_value {
                                ToggleFlag::True
                            } else {
                                ToggleFlag::False
                            };
                            let _ = m.ref_mailbox.set_is_subscribed(*new_value);
                        });
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(
                                    format!(
                                        "{}: `{}` has been {}subscribed.",
                                        &self.name,
                                        self.mailbox_entries[mailbox_hash].name(),
                                        if *new_value { "" } else { "un" }
                                    )
                                    .into(),
                                ),
                                source: None,
                                body: "".into(),
                                kind: Some(NotificationType::Info),
                            }));
                    }
                }}
            }
        }
    }
}
