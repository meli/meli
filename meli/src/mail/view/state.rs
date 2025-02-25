/*
 * meli
 *
 * Copyright 2017 Manos Pitsidianakis
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

use melib::{text::Truncate, Envelope, Error, Mail, Result};

use super::{EnvelopeView, MailView, ViewSettings};
use crate::{
    jobs::JoinHandle, mailbox_settings, Component, Context, ShortcutMaps, ThreadEvent, UIEvent,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PendingReplyAction {
    Reply,
    ReplyToAuthor,
    ReplyToAll,
    ForwardAttachment,
    ForwardInline,
}

#[derive(Debug)]
pub enum MailViewState {
    Init {
        pending_action: Option<PendingReplyAction>,
    },
    LoadingBody {
        main_loop_handler: crate::MainLoopHandler,
        handle: JoinHandle<Result<Vec<u8>>>,
        pending_action: Option<PendingReplyAction>,
    },
    Error {
        err: Error,
    },
    Loaded {
        bytes: Vec<u8>,
        env: Box<Envelope>,
        env_view: Box<EnvelopeView>,
        stack: Vec<Box<dyn Component>>,
    },
}

impl Drop for MailViewState {
    fn drop(&mut self) {
        let Self::LoadingBody {
            handle,
            main_loop_handler,
            ..
        } = self
        else {
            return;
        };
        let Some(ev) = handle.cancel() else {
            return;
        };
        main_loop_handler.send(ThreadEvent::UIEvent(UIEvent::StatusEvent(ev)));
    }
}

impl std::fmt::Display for MailViewState {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Init { .. } | Self::LoadingBody { .. } => {
                write!(fmt, "loading")
            }
            Self::Error { err } => {
                let err = err.to_string();
                write!(fmt, "{}", err.trim_at_boundary(8))
            }
            Self::Loaded { env, .. } => {
                write!(fmt, "{}", env.subject().as_ref().trim_at_boundary(8))
            }
        }
    }
}

impl MailViewState {
    pub fn load_bytes(self_: &mut MailView, bytes: Vec<u8>, context: &mut Context) {
        let Some(coordinates) = self_.coordinates else {
            return;
        };
        let account = &mut context.accounts[&coordinates.0];
        // Ensure all envelope headers are populated, because the email backend might
        // have not populated them all.
        _ = account
            .collection
            .get_env_mut(coordinates.2)
            .populate_headers(&bytes);
        let env = Box::new(account.collection.get_env(coordinates.2).clone());
        let env_view = Box::new(EnvelopeView::new(
            Mail {
                envelope: *env.clone(),
                bytes: bytes.clone(),
            },
            None,
            None,
            Some(ViewSettings {
                theme_default: crate::conf::value(context, "theme_default"),
                body_theme: crate::conf::value(context, "mail.view.body"),
                env_view_shortcuts: mailbox_settings!(
                    context[coordinates.0][&coordinates.1]
                        .shortcuts
                        .envelope_view
                )
                .key_values(),
                pager_filter: mailbox_settings!(
                    context[coordinates.0][&coordinates.1].pager.filter
                )
                .clone(),
                html_filter: mailbox_settings!(
                    context[coordinates.0][&coordinates.1].pager.html_filter
                )
                .clone(),
                url_launcher: mailbox_settings!(
                    context[coordinates.0][&coordinates.1].pager.url_launcher
                )
                .clone(),
                auto_choose_multipart_alternative: mailbox_settings!(
                    context[coordinates.0][&coordinates.1]
                        .pager
                        .auto_choose_multipart_alternative
                )
                .is_true(),
                expand_headers: false,
                sticky_headers: *mailbox_settings!(
                    context[coordinates.0][&coordinates.1].pager.sticky_headers
                ),
                show_date_in_my_timezone: mailbox_settings!(
                    context[coordinates.0][&coordinates.1]
                        .pager
                        .show_date_in_my_timezone
                )
                .is_true(),
                show_extra_headers: mailbox_settings!(
                    context[coordinates.0][&coordinates.1]
                        .pager
                        .show_extra_headers
                )
                .clone(),
                auto_verify_signatures: *mailbox_settings!(
                    context[coordinates.0][&coordinates.1]
                        .pgp
                        .auto_verify_signatures
                ),
                auto_decrypt: *mailbox_settings!(
                    context[coordinates.0][&coordinates.1].pgp.auto_decrypt
                ),
            }),
            context.main_loop_handler.clone(),
        ));
        self_.state = Self::Loaded {
            env,
            bytes,
            env_view,
            stack: vec![],
        };
    }

    pub fn is_dirty(&self) -> bool {
        matches!(self, Self::Loaded { ref env_view, .. } if env_view.is_dirty())
    }

    pub fn set_dirty(&mut self, dirty: bool) {
        if let Self::Loaded {
            ref mut env_view, ..
        } = self
        {
            env_view.set_dirty(dirty);
        }
    }

    pub fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        if let Self::Loaded { ref env_view, .. } = self {
            env_view.shortcuts(context)
        } else {
            ShortcutMaps::default()
        }
    }

    pub fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let Self::Loaded {
            ref mut env_view, ..
        } = self
        {
            env_view.process_event(event, context)
        } else {
            false
        }
    }
}

impl Default for MailViewState {
    fn default() -> Self {
        Self::Init {
            pending_action: None,
        }
    }
}
