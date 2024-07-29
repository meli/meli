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
                let handle = if self.backend_capabilities.is_async {
                    self.main_loop_handler
                        .job_executor
                        .spawn_specialized("create_mailbox".into(), job)
                } else {
                    self.main_loop_handler
                        .job_executor
                        .spawn_blocking("create_mailbox".into(), job)
                };
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
                let handle = if self.backend_capabilities.is_async {
                    self.main_loop_handler
                        .job_executor
                        .spawn_specialized("delete_mailbox".into(), job)
                } else {
                    self.main_loop_handler
                        .job_executor
                        .spawn_blocking("delete_mailbox".into(), job)
                };
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
                let handle = if self.backend_capabilities.is_async {
                    self.main_loop_handler
                        .job_executor
                        .spawn_specialized("subscribe_mailbox".into(), job)
                } else {
                    self.main_loop_handler
                        .job_executor
                        .spawn_blocking("subscribe_mailbox".into(), job)
                };
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
                let handle = if self.backend_capabilities.is_async {
                    self.main_loop_handler
                        .job_executor
                        .spawn_specialized("unsubscribe_mailbox".into(), job)
                } else {
                    self.main_loop_handler
                        .job_executor
                        .spawn_blocking("unsubscribe_mailbox".into(), job)
                };
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
            MailboxOperation::Rename(_, _) => Err(Error::new("Not implemented.")),
            MailboxOperation::SetPermissions(_) => Err(Error::new("Not implemented.")),
        }
    }

    pub fn process_mailbox_event(&mut self, job_id: JobId, job: &mut MailboxJobRequest) {
        match job {
            MailboxJobRequest::Mailboxes { ref mut handle } => {
                if let Ok(Some(mailboxes)) = handle.chan.try_recv() {
                    if let Err(err) = mailboxes.and_then(|mailboxes| self.init(mailboxes)) {
                        if !err.is_recoverable() {
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(self.name.clone().into()),
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
                            let handle = if self.backend_capabilities.is_async {
                                self.main_loop_handler
                                    .job_executor
                                    .spawn_specialized("mailboxes_list".into(), mailboxes_job)
                            } else {
                                self.main_loop_handler
                                    .job_executor
                                    .spawn_blocking("mailboxes_list".into(), mailboxes_job)
                            };
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
            MailboxJobRequest::CreateMailbox {
                ref path,
                ref mut handle,
                ..
            } => {
                if let Ok(Some(r)) = handle.chan.try_recv() {
                    match r {
                        Err(err) => {
                            self.main_loop_handler
                                .job_executor
                                .set_job_success(job_id, false);
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::Notification {
                                    title: Some(
                                        format!(
                                            "{}: could not create mailbox {}",
                                            &self.name, path
                                        )
                                        .into(),
                                    ),
                                    source: None,
                                    body: err.to_string().into(),
                                    kind: Some(NotificationType::Error(err.kind)),
                                },
                            ));
                        }
                        Ok((mailbox_hash, mut mailboxes)) => {
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
                        }
                    }
                }
            }
            MailboxJobRequest::DeleteMailbox {
                mailbox_hash,
                ref mut handle,
                ..
            } => {
                match handle.chan.try_recv() {
                    Err(_) => { /* canceled */ }
                    Ok(None) => {}
                    Ok(Some(Err(err))) => {
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(
                                    format!("{}: could not delete mailbox", &self.name).into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(mut mailboxes))) => {
                        let mailbox_hash = *mailbox_hash;
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
                        let deleted_mailbox = self.mailbox_entries.remove(&mailbox_hash).unwrap();
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
                    }
                }
            }
            MailboxJobRequest::SetMailboxPermissions { ref mut handle, .. } => {
                match handle.chan.try_recv() {
                    Err(_) => { /* canceled */ }
                    Ok(None) => {}
                    Ok(Some(Err(err))) => {
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(
                                    format!("{}: could not set mailbox permissions", &self.name)
                                        .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(_))) => {
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
                    }
                }
            }
            MailboxJobRequest::SetMailboxSubscription {
                ref mut handle,
                ref mailbox_hash,
                ref new_value,
            } => {
                match handle.chan.try_recv() {
                    Err(_) => { /* canceled */ }
                    Ok(None) => {}
                    Ok(Some(Err(err))) => {
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(
                                    format!("{}: could not set mailbox subscription", &self.name)
                                        .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(()))) if self.mailbox_entries.contains_key(mailbox_hash) => {
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
                    Ok(Some(Ok(()))) => {}
                }
            }
        }
    }
}
