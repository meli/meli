/*
 * meli
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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

use std::{
    collections::HashSet,
    convert::TryFrom,
    fmt::Write as _,
    io::Write,
    process::{Command, Stdio},
};

use melib::{
    email::attachment_types::ContentType, list_management, mailto::Mailto, parser::BytesExt,
    utils::datetime, Card, Draft, FlagOp, HeaderName, SpecialUsageMailbox,
};
use smallvec::SmallVec;

use super::*;
use crate::{accounts::JobRequest, jobs::JobId};

mod utils;
pub use utils::*;

mod thread;
pub use thread::*;
mod types;
pub use types::*;
pub mod state;
use state::*;

pub mod envelope;
pub use envelope::EnvelopeView;

pub mod filters;
pub use filters::*;

#[cfg(test)]
mod tests;

/// Contains an Envelope view, with sticky headers, a pager for the body, and
/// subviews for more menus
#[derive(Debug)]
pub struct MailView {
    coordinates: Option<(AccountHash, MailboxHash, EnvelopeHash)>,
    dirty: bool,
    contact_selector: Option<Box<UIDialog<Card>>>,
    forward_dialog: Option<Box<UIDialog<Option<PendingReplyAction>>>>,
    theme_default: ThemeAttribute,
    active_jobs: HashSet<JobId>,
    initialized: bool,
    state: MailViewState,
    main_loop_handler: MainLoopHandler,
    id: ComponentId,
}

impl std::fmt::Display for MailView {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.state.fmt(f)
    }
}

impl Drop for MailView {
    fn drop(&mut self) {
        if let MailViewState::LoadingBody { ref mut handle, .. } = self.state {
            if let Some(canceled) = handle.cancel() {
                self.main_loop_handler
                    .send(UIEvent::StatusEvent(canceled).into());
            }
        }
    }
}

impl MailView {
    pub fn new(
        coordinates: Option<(AccountHash, MailboxHash, EnvelopeHash)>,
        initialize_now: bool,
        context: &mut Context,
    ) -> Self {
        let mut ret = Self {
            coordinates,
            dirty: true,
            contact_selector: None,
            forward_dialog: None,
            theme_default: crate::conf::value(context, "mail.view.body"),
            active_jobs: Default::default(),
            initialized: false,
            state: MailViewState::default(),
            main_loop_handler: context.main_loop_handler.clone(),
            id: ComponentId::default(),
        };

        if initialize_now {
            ret.init_futures(context);
        }
        ret
    }

    fn init_futures(&mut self, context: &mut Context) {
        log::trace!("MailView::init_futures");
        self.theme_default = crate::conf::value(context, "mail.view.body");
        let mut pending_action = None;
        let Some(coordinates) = self.coordinates else {
            return;
        };
        let account = &mut context.accounts[&coordinates.0];
        if account.contains_key(coordinates.2) {
            {
                match account
                    .operation(coordinates.2)
                    .and_then(|mut op| op.as_bytes())
                {
                    Ok(fut) => {
                        let mut handle = account
                            .main_loop_handler
                            .job_executor
                            .spawn_specialized("fetch_envelopes".into(), fut);
                        let job_id = handle.job_id;
                        pending_action = if let MailViewState::Init {
                            ref mut pending_action,
                        } = self.state
                        {
                            pending_action.take()
                        } else {
                            None
                        };
                        if let Ok(Some(bytes_result)) = try_recv_timeout!(&mut handle.chan) {
                            match bytes_result {
                                Ok(bytes) => {
                                    MailViewState::load_bytes(self, bytes, context);
                                }
                                Err(err) => {
                                    self.state = MailViewState::Error { err };
                                }
                            }
                        } else {
                            self.state = MailViewState::LoadingBody {
                                handle,
                                pending_action: pending_action.take(),
                            };
                            self.active_jobs.insert(job_id);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::NewJob(job_id)));
                        }
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!("Could not get message: {}", err)),
                        ));
                    }
                }
            }
        }
        if let Some(p) = pending_action {
            self.perform_action(p, context);
        }
        self.initialized = true;
    }

    fn perform_action(&mut self, action: PendingReplyAction, context: &mut Context) {
        let Some(coordinates) = self.coordinates else {
            return;
        };
        let (bytes, reply_body, env) = match self.state {
            MailViewState::Init {
                ref mut pending_action,
                ..
            }
            | MailViewState::LoadingBody {
                ref mut pending_action,
                ..
            } => {
                *pending_action = Some(action);
                return;
            }
            MailViewState::Loaded {
                ref bytes,
                ref env,
                ref env_view,
                ..
            } => (
                bytes,
                EnvelopeView::attachment_displays_to_text(&env_view.display, false),
                env,
            ),
            MailViewState::Error { .. } => {
                return;
            }
        };
        let composer = match action {
            PendingReplyAction::Reply => {
                Box::new(Composer::reply_to_select(coordinates, reply_body, context))
            }
            PendingReplyAction::ReplyToAuthor => {
                Box::new(Composer::reply_to_author(coordinates, reply_body, context))
            }
            PendingReplyAction::ReplyToAll => {
                Box::new(Composer::reply_to_all(coordinates, reply_body, context))
            }
            PendingReplyAction::ForwardAttachment => {
                Box::new(Composer::forward(coordinates, bytes, env, true, context))
            }
            PendingReplyAction::ForwardInline => {
                Box::new(Composer::forward(coordinates, bytes, env, false, context))
            }
        };

        context
            .replies
            .push_back(UIEvent::Action(Tab(New(Some(composer)))));
    }

    pub fn update(
        &mut self,
        new_coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        context: &mut Context,
    ) {
        if let MailViewState::LoadingBody { ref mut handle, .. } = self.state {
            if let Some(canceled) = handle.cancel() {
                context.replies.push_back(UIEvent::StatusEvent(canceled));
            }
        }
        if self.coordinates != Some(new_coordinates) {
            self.coordinates = Some(new_coordinates);
            self.init_futures(context);
            self.set_dirty(true);
        }
    }

    fn start_contact_selector(&mut self, context: &mut Context) {
        let Some(coordinates) = self.coordinates else {
            return;
        };
        let account = &context.accounts[&coordinates.0];
        if !account.contains_key(coordinates.2) {
            context
                .replies
                .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                    "Email not found".into(),
                )));
            return;
        }
        let envelope: EnvelopeRef = account.collection.get_env(coordinates.2);

        let mut entries = Vec::new();
        for addr in envelope.from().iter().chain(envelope.to().iter()) {
            let mut new_card: Card = Card::new();
            new_card
                .set_email(addr.get_email())
                .set_id(addr.get_email().into());
            if let Some(display_name) = addr.get_display_name() {
                new_card.set_name(display_name);
            }
            entries.push((new_card, format!("{}", addr)));
        }
        drop(envelope);
        self.contact_selector = Some(Box::new(Selector::new(
            "select contacts to add",
            entries,
            false,
            Some(Box::new(move |id: ComponentId, results: &[Card]| {
                Some(UIEvent::FinishedUIDialog(id, Box::new(results.to_vec())))
            })),
            context,
        )));
        self.dirty = true;
    }
}

impl Component for MailView {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        let Some(coordinates) = self.coordinates else {
            return;
        };

        if !self.initialized {
            self.init_futures(context);
            return;
        }
        {
            let account = &context.accounts[&coordinates.0];
            if !account.contains_key(coordinates.2) {
                /* The envelope has been renamed or removed, so wait for the appropriate
                 * event to arrive */
                return;
            }
        }

        if let MailViewState::Loaded {
            ref mut env_view, ..
        } = self.state
        {
            {
                let account = &mut context.accounts[&coordinates.0];
                if !account.collection.get_env(coordinates.2).is_seen() {
                    if let Err(err) = account.set_flags(
                        coordinates.2.into(),
                        coordinates.1,
                        smallvec::smallvec![FlagOp::Set(Flag::SEEN)],
                    ) {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(format!(
                                "Could not set message as seen: {err}",
                            )),
                        ));
                    }
                }
            }
            env_view.draw(grid, area, context);
        } else if let MailViewState::Error { ref err } = self.state {
            grid.clear_area(area, self.theme_default);
            context.dirty_areas.push_back(area);
            context.replies.push_back(UIEvent::Notification {
                title: Some("Failed to open e-mail".into()),
                source: None,
                body: err.to_string().into(),
                kind: Some(NotificationType::Error(err.kind)),
            });
            log::error!("Failed to open envelope: {err}");
            self.init_futures(context);
            return;
        } else {
            grid.clear_area(area, self.theme_default);
            context.dirty_areas.push_back(area);
            return;
        };
        if let Some(ref mut s) = self.contact_selector.as_mut() {
            s.draw(grid, area, context);
        } else if let Some(ref mut s) = self.forward_dialog.as_mut() {
            s.draw(grid, area, context);
        }

        self.dirty = false;
    }

    fn process_event(&mut self, mut event: &mut UIEvent, context: &mut Context) -> bool {
        if let Some(ref mut s) = self.contact_selector {
            if s.process_event(event, context) {
                return true;
            }
        }

        if let Some(ref mut s) = self.forward_dialog {
            if s.process_event(event, context) {
                return true;
            }
        }

        let Some(coordinates) = self.coordinates else {
            return false;
        };
        if coordinates.0.is_null() || coordinates.1.is_null() {
            return false;
        }

        /* If envelope data is loaded, pass it to envelope views */
        if self.state.process_event(event, context) {
            return true;
        }

        match (
            &mut self.contact_selector,
            &mut self.forward_dialog,
            &mut event,
        ) {
            (Some(ref s), _, UIEvent::FinishedUIDialog(id, results)) if *id == s.id() => {
                if let Some(results) = results.downcast_ref::<Vec<Card>>() {
                    let account = &mut context.accounts[&coordinates.0];
                    {
                        for card in results.iter() {
                            account.address_book.add_card(card.clone());
                        }
                    }
                    self.contact_selector = None;
                }
                self.set_dirty(true);
                return true;
            }
            (_, Some(ref s), UIEvent::FinishedUIDialog(id, result)) if *id == s.id() => {
                if let Some(result) = result.downcast_ref::<Option<PendingReplyAction>>() {
                    self.forward_dialog = None;
                    if let Some(result) = *result {
                        self.perform_action(result, context);
                    }
                }
                self.set_dirty(true);
                return true;
            }
            _ => {}
        }
        match &event {
            UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id))
                if self.active_jobs.contains(job_id) =>
            {
                match self.state {
                    MailViewState::LoadingBody {
                        ref mut handle,
                        pending_action: _,
                    } if handle.job_id == *job_id => {
                        match handle.chan.try_recv() {
                            Err(_) => { /* Job was canceled */ }
                            Ok(None) => { /* something happened, perhaps a worker
                                  * thread panicked */
                            }
                            Ok(Some(Ok(bytes))) => {
                                MailViewState::load_bytes(self, bytes, context);
                            }
                            Ok(Some(Err(err))) => {
                                self.state = MailViewState::Error { err };
                            }
                        }
                    }
                    MailViewState::Init { .. } => {
                        self.init_futures(context);
                    }
                    MailViewState::Loaded { .. } => {
                        log::debug!(
                            "MailView.active_jobs contains job id {:?} but MailViewState is \
                             already loaded; what job was this and why was it in active_jobs?",
                            job_id
                        );
                    }
                    _ => {}
                }
                self.active_jobs.remove(job_id);
                self.set_dirty(true);
            }
            _ => {}
        }

        let shortcuts = &self.shortcuts(context);
        match *event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.theme_default = crate::conf::value(context, "theme_default");
                self.set_dirty(true);
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["reply"]) =>
            {
                self.perform_action(PendingReplyAction::Reply, context);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["reply_to_all"]) =>
            {
                self.perform_action(PendingReplyAction::ReplyToAll, context);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["reply_to_author"]) =>
            {
                self.perform_action(PendingReplyAction::ReplyToAuthor, context);
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["forward"]) =>
            {
                match mailbox_settings!(
                    context[coordinates.0][&coordinates.1]
                        .composing
                        .forward_as_attachment
                ) {
                    f if f.is_ask() => {
                        self.forward_dialog = Some(Box::new(UIDialog::new(
                            "How do you want the email to be forwarded?",
                            vec![
                                (
                                    Some(PendingReplyAction::ForwardInline),
                                    "inline".to_string(),
                                ),
                                (
                                    Some(PendingReplyAction::ForwardAttachment),
                                    "as attachment".to_string(),
                                ),
                            ],
                            true,
                            Some(Box::new(
                                move |id: ComponentId, result: &[Option<PendingReplyAction>]| {
                                    Some(UIEvent::FinishedUIDialog(
                                        id,
                                        Box::new(result.first().cloned().flatten()),
                                    ))
                                },
                            )),
                            context,
                        )));
                    }
                    f if f.is_true() => {
                        self.perform_action(PendingReplyAction::ForwardAttachment, context);
                    }
                    _ => {
                        self.perform_action(PendingReplyAction::ForwardInline, context);
                    }
                }
                return true;
            }
            UIEvent::FinishedUIDialog(id, ref result) if id == self.id() => {
                if let Some(result) = result.downcast_ref::<PendingReplyAction>() {
                    self.perform_action(*result, context);
                }
                return true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::ENVELOPE_VIEW]["edit"]) =>
            {
                let account_hash = coordinates.0;
                let env_hash = coordinates.2;
                let (sender, mut receiver) = crate::jobs::oneshot::channel();
                let operation = context.accounts[&account_hash].operation(env_hash);
                let bytes_job = async move {
                    let _ = sender.send(operation?.as_bytes()?.await);
                    Ok(())
                };
                let handle = if context.accounts[&account_hash]
                    .backend_capabilities
                    .is_async
                {
                    context
                        .main_loop_handler
                        .job_executor
                        .spawn_specialized("fetch_envelope".into(), bytes_job)
                } else {
                    context
                        .main_loop_handler
                        .job_executor
                        .spawn_blocking("fetch_envelope".into(), bytes_job)
                };
                context.accounts[&account_hash].insert_job(
                    handle.job_id,
                    JobRequest::Generic {
                        name: "fetch envelope".into(),
                        handle,
                        on_finish: Some(CallbackFn(Box::new(move |context: &mut Context| {
                            match receiver.try_recv() {
                                Err(_) => { /* Job was canceled */ }
                                Ok(None) => { /* something happened, perhaps a worker
                                      * thread panicked */
                                }
                                Ok(Some(result)) => {
                                    match result.and_then(|bytes| {
                                        Composer::edit(account_hash, env_hash, &bytes, context)
                                    }) {
                                        Ok(composer) => {
                                            context.replies.push_back(UIEvent::Action(Tab(New(
                                                Some(Box::new(composer)),
                                            ))));
                                        }
                                        Err(err) => {
                                            let err_string = format!(
                                                "Failed to open envelope {}: {}",
                                                context.accounts[&account_hash]
                                                    .collection
                                                    .envelopes
                                                    .read()
                                                    .unwrap()
                                                    .get(&env_hash)
                                                    .map(|env| env.message_id_display())
                                                    .unwrap_or_else(|| "Not found".into()),
                                                err
                                            );
                                            log::error!("{err_string}");
                                            context.replies.push_back(UIEvent::Notification {
                                                title: Some("Failed to open e-mail".into()),
                                                source: None,
                                                body: err_string.into(),
                                                kind: Some(NotificationType::Error(err.kind)),
                                            });
                                        }
                                    }
                                }
                            }
                        }))),
                        log_level: LogLevel::DEBUG,
                    },
                );
                return true;
            }
            UIEvent::Action(View(ViewAction::AddAddressesToContacts)) => {
                self.start_contact_selector(context);
                return true;
            }
            UIEvent::Input(ref key)
                if self.contact_selector.is_none()
                    && shortcut!(
                        key == shortcuts[Shortcuts::ENVELOPE_VIEW]["add_addresses_to_contacts"]
                    ) =>
            {
                self.start_contact_selector(context);
                return true;
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt(''))
                if self.contact_selector.is_some() || self.forward_dialog.is_some() =>
            {
                if let Some(s) = self.contact_selector.take() {
                    s.unrealize(context);
                }
                if let Some(s) = self.forward_dialog.take() {
                    s.unrealize(context);
                }
                self.set_dirty(true);
                return true;
            }
            UIEvent::EnvelopeRename(old_hash, new_hash) if coordinates.2 == old_hash => {
                self.coordinates.as_mut().unwrap().2 = new_hash;
            }
            UIEvent::Action(MailingListAction(ref e)) => {
                let account = &context.accounts[&coordinates.0];
                if !account.contains_key(coordinates.2) {
                    /* The envelope has been renamed or removed, so wait for the appropriate
                     * event to arrive */
                    return true;
                }
                let envelope: EnvelopeRef = account.collection.get_env(coordinates.2);
                let detect = list_management::ListActions::detect(&envelope);
                if let Some(ref actions) = detect {
                    match e {
                        MailingListAction::ListPost if actions.post.is_some() => {
                            /* open composer */
                            let mut failure = true;
                            if let list_management::ListAction::Email(list_post_addr) =
                                actions.post.as_ref().unwrap()[0]
                            {
                                if let Ok(mailto) = Mailto::try_from(list_post_addr) {
                                    let draft: Draft = mailto.into();
                                    let mut composer =
                                        Composer::with_account(coordinates.0, context);
                                    composer.set_draft(draft, context);
                                    context.replies.push_back(UIEvent::Action(Tab(New(Some(
                                        Box::new(composer),
                                    )))));
                                    failure = false;
                                }
                            }
                            if failure {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(String::from(
                                        "Couldn't parse List-Post header value",
                                    )),
                                ));
                            }
                            return true;
                        }
                        MailingListAction::ListUnsubscribe if actions.unsubscribe.is_some() => {
                            /* autosend or open unsubscribe option */
                            let unsubscribe = actions.unsubscribe.as_ref().unwrap();
                            for option in unsubscribe.iter() {
                                /* [ref:TODO]: Ask for confirmation before proceeding with an action */
                                match option {
                                    list_management::ListAction::Email(email) => {
                                        if let Ok(mailto) = Mailto::try_from(*email) {
                                            let mut draft: Draft = mailto.into();
                                            draft.set_header(
                                                HeaderName::FROM,
                                                context.accounts[&coordinates.0]
                                                    .settings
                                                    .account()
                                                    .make_display_name()
                                                    .to_string(),
                                            );
                                            /* Manually drop stuff because borrowck doesn't do it
                                             * on its own */
                                            drop(detect);
                                            drop(envelope);
                                            if let Err(err) = super::compose::send_draft(
                                                ToggleFlag::False,
                                                context,
                                                coordinates.0,
                                                draft,
                                                SpecialUsageMailbox::Sent,
                                                Flag::SEEN,
                                                true,
                                            ) {
                                                context.replies.push_back(UIEvent::StatusEvent(
                                                    StatusEvent::DisplayMessage(format!(
                                                        "Couldn't send unsubscribe e-mail: {}",
                                                        err
                                                    )),
                                                ));
                                            }
                                            return true;
                                        }
                                    }
                                    list_management::ListAction::Url(url) => {
                                        let url_launcher = mailbox_settings!(
                                            context[coordinates.0][&coordinates.1]
                                                .pager
                                                .url_launcher
                                        )
                                        .as_ref()
                                        .map(|s| s.as_str())
                                        .unwrap_or(
                                            #[cfg(target_os = "macos")]
                                            {
                                                "open"
                                            },
                                            #[cfg(not(target_os = "macos"))]
                                            {
                                                "xdg-open"
                                            },
                                        );
                                        match Command::new(url_launcher)
                                            .arg(String::from_utf8_lossy(url).into_owned())
                                            .stdin(Stdio::piped())
                                            .stdout(Stdio::piped())
                                            .spawn()
                                        {
                                            Ok(child) => {
                                                context
                                                    .children
                                                    .entry(url_launcher.to_string().into())
                                                    .or_default()
                                                    .push(child);
                                            }
                                            Err(err) => {
                                                context.replies.push_back(UIEvent::StatusEvent(
                                                    StatusEvent::DisplayMessage(format!(
                                                        "Couldn't launch {}: {}",
                                                        url_launcher, err
                                                    )),
                                                ));
                                            }
                                        }
                                        return true;
                                    }
                                    list_management::ListAction::No => {}
                                }
                            }
                        }
                        MailingListAction::ListArchive if actions.archive.is_some() => {
                            /* open archive url with url_launcher */
                            let url_launcher = mailbox_settings!(
                                context[coordinates.0][&coordinates.1].pager.url_launcher
                            )
                            .as_ref()
                            .map(|s| s.as_str())
                            .unwrap_or(
                                #[cfg(target_os = "macos")]
                                {
                                    "open"
                                },
                                #[cfg(not(target_os = "macos"))]
                                {
                                    "xdg-open"
                                },
                            );
                            match Command::new(url_launcher)
                                .arg(actions.archive.unwrap())
                                .stdin(Stdio::piped())
                                .stdout(Stdio::piped())
                                .spawn()
                            {
                                Ok(child) => context
                                    .children
                                    .entry(url_launcher.to_string().into())
                                    .or_default()
                                    .push(child),
                                Err(err) => {
                                    context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(format!(
                                            "Couldn't launch {}: {}",
                                            url_launcher, err
                                        )),
                                    ));
                                }
                            }
                            return true;
                        }
                        _ => { /* error print message to user */ }
                    }
                };
            }
            UIEvent::Action(Listing(OpenInNewTab)) => {
                let mut new_tab = Self::new(self.coordinates, true, context);
                new_tab.set_dirty(true);
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(New(Some(Box::new(new_tab))))));
                return true;
            }
            UIEvent::Input(ref key)
                if context
                    .settings
                    .shortcuts
                    .envelope_view
                    .commands
                    .iter()
                    .any(|cmd| {
                        if cmd.shortcut == *key {
                            for cmd in &cmd.command {
                                context.replies.push_back(UIEvent::Command(cmd.to_string()));
                            }
                            return true;
                        }
                        false
                    }) =>
            {
                return true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || self.state.is_dirty()
            || self
                .contact_selector
                .as_ref()
                .map(|s| s.is_dirty())
                .unwrap_or(false)
            || self
                .forward_dialog
                .as_ref()
                .map(|s| s.is_dirty())
                .unwrap_or(false)
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        if let Some(ref mut s) = self.contact_selector {
            s.set_dirty(value);
        } else if let Some(ref mut s) = self.forward_dialog {
            s.set_dirty(value);
        }
        self.state.set_dirty(value);
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        self.state.shortcuts(context)
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn kill(&mut self, id: ComponentId, context: &mut Context) {
        if self.id == id {
            context
                .replies
                .push_back(UIEvent::Action(Tab(Kill(self.id))));
        }
    }
}
