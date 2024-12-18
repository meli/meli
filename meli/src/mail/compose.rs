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
    borrow::Cow,
    convert::TryInto,
    fmt::Write as _,
    future::Future,
    io::Write,
    pin::Pin,
    process::{Command, Stdio},
    sync::{Arc, Mutex},
};

use indexmap::IndexSet;
use melib::{
    email::attachment_types::{ContentType, MultipartType},
    list_management,
    parser::BytesExt,
    Address, Contacts, Draft, HeaderName, SpecialUsageMailbox, SubjectPrefix, UnixTimestamp,
};
use nix::sys::wait::WaitStatus;

use super::*;
use crate::{
    accounts::JobRequest,
    jobs::{IsAsync, JoinHandle},
    terminal::embedded::Terminal,
    types::{sanitize_filename, File},
};

#[cfg(feature = "gpgme")]
pub mod gpg;

pub mod edit_attachments;
use edit_attachments::*;

pub mod hooks;

#[derive(Debug, Eq, PartialEq)]
enum Cursor {
    Headers,
    Body,
    Sign,
    Encrypt,
    Attachments,
}

#[derive(Debug)]
struct EmbeddedPty {
    running: bool,
    terminal: Arc<Mutex<Terminal>>,
    file: File,
}

impl EmbeddedPty {
    #[inline]
    fn is_stopped(&self) -> bool {
        !self.running
    }

    #[inline]
    fn is_dirty(&self) -> bool {
        std::ops::Deref::deref(self)
            .try_lock()
            .ok()
            .map_or(true, |e| e.grid.is_dirty())
    }
}

impl std::ops::Deref for EmbeddedPty {
    type Target = Arc<Mutex<Terminal>>;

    fn deref(&self) -> &Self::Target {
        &self.terminal
    }
}

impl std::ops::DerefMut for EmbeddedPty {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.terminal
    }
}

#[derive(Debug)]
pub struct Composer {
    reply_context: Option<(MailboxHash, EnvelopeHash)>,
    account_hash: AccountHash,

    cursor: Cursor,

    pager: Pager,
    draft: Draft,
    form: FormWidget<bool>,

    mode: ViewMode,

    embedded_pty: Option<EmbeddedPty>,
    embedded_dimensions: (usize, usize),
    #[cfg(feature = "gpgme")]
    gpg_state: gpg::GpgComposeState,
    dirty: bool,
    has_changes: bool,
    initialized: bool,
    hooks: Vec<hooks::Hook>,
    id: ComponentId,
}

#[derive(Debug)]
enum ViewMode {
    Discard(ComponentId, UIDialog<char>),
    EditAttachments {
        widget: EditAttachments,
    },
    Edit,
    EmbeddedPty,
    SelectRecipients(UIDialog<Address>),
    #[cfg(feature = "gpgme")]
    SelectKey(bool, gpg::KeySelection),
    Send(UIConfirmationDialog),
    WaitingForSendResult(UIDialog<char>, JoinHandle<Result<()>>),
}

impl ViewMode {
    #[inline]
    fn is_edit(&self) -> bool {
        matches!(self, Self::Edit)
    }

    #[inline]
    fn is_edit_attachments(&self) -> bool {
        matches!(self, Self::EditAttachments { .. })
    }
}

impl std::fmt::Display for Composer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let subject = self.draft.headers().get(HeaderName::SUBJECT);
        if let Some(ref val) = subject.filter(|s| !s.is_empty()) {
            val.trim_at_boundary(4);
            write!(f, "{}", val)
        } else if let Some(ref val) = self
            .draft
            .headers()
            .get(HeaderName::TO)
            .filter(|s| !s.is_empty())
        {
            val.trim_at_boundary(4);
            write!(f, "to {}", val)
        } else {
            write!(f, "draft")
        }
    }
}

impl Composer {
    pub fn new(context: &Context) -> Self {
        let mut pager = Pager::new(context);
        pager.set_show_scrollbar(true);
        let mut form = FormWidget::default();
        form.set_cursor(2);
        Self {
            reply_context: None,
            account_hash: AccountHash::default(),
            cursor: Cursor::Headers,
            pager,
            draft: Draft::default(),
            hooks: vec![
                hooks::HEADERWARN,
                hooks::PASTDATEWARN,
                hooks::MISSINGATTACHMENTWARN,
                hooks::EMPTYDRAFTWARN,
            ],
            form,
            mode: ViewMode::Edit,
            #[cfg(feature = "gpgme")]
            gpg_state: gpg::GpgComposeState::default(),
            dirty: true,
            has_changes: false,
            embedded_pty: None,
            embedded_dimensions: (80, 20),
            initialized: false,
            id: ComponentId::default(),
        }
    }

    pub fn with_account(account_hash: AccountHash, context: &Context) -> Self {
        let mut ret = Self {
            account_hash,
            ..Self::new(context)
        };

        // Add user's custom hooks.
        for hook in account_settings!(context[account_hash].composing.custom_compose_hooks)
            .iter()
            .cloned()
            .map(Into::into)
        {
            ret.hooks.push(hook);
        }

        ret.hooks.retain(|h| {
            !account_settings!(context[account_hash].composing.disabled_compose_hooks)
                .iter()
                .any(|hn| hn.as_str() == h.name())
        });

        for h in context.accounts[&account_hash]
            .backend_capabilities
            .extra_submission_headers
        {
            ret.draft.set_header(h.clone(), String::new());
        }

        for (h, v) in
            account_settings!(context[account_hash].composing.default_header_values).iter()
        {
            if v.is_empty() {
                continue;
            }
            ret.draft.set_header(h.into(), v.into());
        }
        if *account_settings!(context[account_hash].composing.insert_user_agent) {
            ret.draft.set_header(
                HeaderName::USER_AGENT,
                format!("meli {}", option_env!("CARGO_PKG_VERSION").unwrap_or("0.0")),
            );
        }
        let format_flowed = *account_settings!(context[account_hash].composing.format_flowed);
        if *account_settings!(context[account_hash].composing.use_signature) {
            let override_value = account_settings!(context[account_hash].composing.signature_file)
                .as_deref()
                .map(Cow::Borrowed)
                .filter(|p| p.as_ref().is_file());
            let account_value = || {
                context.accounts[&account_hash]
                    .signature_file()
                    .map(Cow::Owned)
            };
            if let Some(path) = override_value.or_else(account_value) {
                match std::fs::read_to_string(path.as_ref()).chain_err_related_path(path.as_ref()) {
                    Ok(sig) => {
                        let mut delimiter =
                            account_settings!(context[account_hash].composing.signature_delimiter)
                                .as_deref()
                                .map(Cow::Borrowed)
                                .unwrap_or_else(|| Cow::Borrowed("\n\n-- \n"));
                        if format_flowed {
                            delimiter = Cow::Owned(delimiter.replace(" \n", " \n\n"));
                        }
                        _ = write!(&mut ret.draft.body, "{}{}", delimiter.as_ref(), sig);
                    }
                    Err(err) => {
                        log::error!(
                            "Could not open signature file for account `{}`: {}.",
                            context.accounts[&account_hash].name(),
                            err
                        );
                    }
                }
            }
        }
        if format_flowed {
            ret.pager.set_reflow(melib::text::Reflow::FormatFlowed);
        }
        ret
    }

    pub fn edit(
        account_hash: AccountHash,
        env_hash: EnvelopeHash,
        bytes: &[u8],
        context: &Context,
    ) -> Result<Self> {
        let mut ret = Self::with_account(account_hash, context);
        // Add user's custom hooks.
        for hook in account_settings!(context[account_hash].composing.custom_compose_hooks)
            .iter()
            .cloned()
            .map(Into::into)
        {
            ret.hooks.push(hook);
        }
        ret.hooks.retain(|h| {
            !account_settings!(context[account_hash].composing.disabled_compose_hooks)
                .iter()
                .any(|hn| hn.as_str() == h.name())
        });
        let envelope: EnvelopeRef = context.accounts[&account_hash].collection.get_env(env_hash);

        ret.draft = Draft::edit(&envelope, bytes, Text::Plain)?;

        ret.account_hash = account_hash;
        Ok(ret)
    }

    pub fn reply_to(
        coordinates @ (account_hash, _, _): (AccountHash, MailboxHash, EnvelopeHash),
        reply_body: String,
        context: &Context,
        reply_to_all: bool,
    ) -> Self {
        let mut ret = Self::with_account(account_hash, context);
        // Add user's custom hooks.
        for hook in account_settings!(context[account_hash].composing.custom_compose_hooks)
            .iter()
            .cloned()
            .map(Into::into)
        {
            ret.hooks.push(hook);
        }
        ret.hooks.retain(|h| {
            !account_settings!(context[account_hash].composing.disabled_compose_hooks)
                .iter()
                .any(|hn| hn.as_str() == h.name())
        });
        let account = &context.accounts[&account_hash];
        let envelope = account.collection.get_env(coordinates.2);
        let subject = {
            let subject = envelope.subject();
            let prefix_list = account_settings!(
                context[ret.account_hash]
                    .composing
                    .reply_prefix_list_to_strip
            )
            .as_ref()
            .map(|v| v.iter().map(String::as_str).collect::<Vec<&str>>())
            .unwrap_or_default();
            let subject_stripped = subject.as_ref().strip_prefixes_from_list(
                if prefix_list.is_empty() {
                    <&str>::USUAL_PREFIXES
                } else {
                    &prefix_list
                },
                Some(1),
            ) == &subject.as_ref();

            let prefix =
                account_settings!(context[ret.account_hash].composing.reply_prefix).as_str();
            if subject_stripped {
                format!("{prefix} {subject}", prefix = prefix, subject = subject)
            } else {
                subject.to_string()
            }
        };
        ret.draft.set_header(HeaderName::SUBJECT, subject);
        ret.draft.set_header(
            HeaderName::REFERENCES,
            format!(
                "{} {}",
                envelope
                    .references()
                    .iter()
                    .fold(String::new(), |mut acc, x| {
                        if !acc.is_empty() {
                            acc.push(' ');
                        }
                        acc.push_str(&x.display_brackets().to_string());
                        acc
                    }),
                envelope.message_id().display_brackets()
            ),
        );
        ret.draft.set_header(
            HeaderName::IN_REPLY_TO,
            envelope.message_id().display_brackets().to_string(),
        );

        if let Some(reply_to) = envelope.other_headers().get(HeaderName::TO) {
            let to: &str = reply_to;
            let extra_identities = &account.settings.account.extra_identities;
            if let Some(extra) = extra_identities
                .iter()
                .find(|extra| to.contains(extra.as_str()))
            {
                ret.draft.set_header(HeaderName::FROM, extra.into());
            }
        }

        // "Mail-Followup-To/(To+Cc+(Mail-Reply-To/Reply-To/From)) for follow-up,
        // Mail-Reply-To/Reply-To/From for reply-to-author."
        // source: https://cr.yp.to/proto/replyto.html
        if reply_to_all {
            let mut to = IndexSet::new();

            if let Some(actions) = list_management::ListActions::detect(&envelope) {
                if let Some(post) = actions.post {
                    if let list_management::ListAction::Email(list_post_addr) = post[0] {
                        if let Ok(list_address) =
                            melib::email::parser::generic::mailto(list_post_addr)
                                .map(|(_, m)| m.address)
                        {
                            to.extend(list_address);
                        }
                    }
                }
            }
            if let Some(reply_to) = envelope
                .other_headers()
                .get(HeaderName::MAIL_FOLLOWUP_TO)
                .and_then(|v| v.try_into().ok())
            {
                to.insert(reply_to);
            } else if let Some(reply_to) = envelope
                .other_headers()
                .get(HeaderName::REPLY_TO)
                .and_then(|v| v.try_into().ok())
            {
                to.insert(reply_to);
            } else {
                to.extend(envelope.from().iter().cloned());
            }
            to.extend(envelope.to().iter().cloned());
            let ours = context.accounts[&coordinates.0]
                .settings
                .account()
                .main_identity_address();
            to.shift_remove(&ours);
            ret.draft.set_header(HeaderName::TO, {
                let mut ret: String =
                    to.into_iter()
                        .fold(String::new(), |mut s: String, n: Address| {
                            s.push_str(&n.to_string());
                            s.push_str(", ");
                            s
                        });
                ret.pop();
                ret.pop();
                ret
            });
            ret.draft
                .set_header(HeaderName::CC, envelope.field_cc_to_string());
        } else if let Some(reply_to) = envelope.other_headers().get(HeaderName::MAIL_REPLY_TO) {
            ret.draft.set_header(HeaderName::TO, reply_to.to_string());
        } else if let Some(reply_to) = envelope.other_headers().get(HeaderName::REPLY_TO) {
            ret.draft.set_header(HeaderName::TO, reply_to.to_string());
        } else {
            ret.draft
                .set_header(HeaderName::TO, envelope.field_from_to_string());
        }
        ret.draft.body = {
            let mut quoted = attribution_string(
                account_settings!(
                    context[ret.account_hash]
                        .composing
                        .attribution_format_string
                )
                .as_ref()
                .map(|s| s.as_str()),
                envelope.from().first(),
                envelope.date(),
                *account_settings!(
                    context[ret.account_hash]
                        .composing
                        .attribution_use_posix_locale
                ),
            );
            for l in reply_body.lines() {
                quoted.push('>');
                quoted.push_str(l);
                quoted.push('\n');
            }
            _ = write!(&mut quoted, "{}", ret.draft.body);
            quoted
        };

        ret.account_hash = coordinates.0;
        ret.reply_context = Some((coordinates.1, coordinates.2));
        ret
    }

    pub fn reply_to_select(
        coordinates @ (account_hash, _, _): (AccountHash, MailboxHash, EnvelopeHash),
        reply_body: String,
        context: &Context,
    ) -> Self {
        let mut ret = Self::reply_to(coordinates, reply_body, context, false);
        let account = &context.accounts[&account_hash];
        let parent_message = account.collection.get_env(coordinates.2);
        /* If message is from a mailing list and we detect a List-Post header, ask
         * user if they want to reply to the mailing list or the submitter of
         * the message */
        if let Some(actions) = list_management::ListActions::detect(&parent_message) {
            if let Some(post) = actions.post {
                if let list_management::ListAction::Email(list_post_addr) = post[0] {
                    if let Ok((_, mailto)) = melib::email::parser::generic::mailto(list_post_addr) {
                        let mut addresses = vec![(
                            parent_message.from()[0].clone(),
                            parent_message.field_from_to_string(),
                        )];
                        for add in mailto.address {
                            let add_s = add.to_string();
                            addresses.push((add, add_s));
                        }
                        ret.mode = ViewMode::SelectRecipients(UIDialog::new(
                            "select recipients",
                            addresses,
                            false,
                            Some(Box::new(move |id: ComponentId, results: &[Address]| {
                                Some(UIEvent::FinishedUIDialog(
                                    id,
                                    Box::new(
                                        results
                                            .iter()
                                            .map(|a| a.to_string())
                                            .collect::<Vec<String>>()
                                            .join(", "),
                                    ),
                                ))
                            })),
                            context,
                        ));
                    }
                }
            }
        }
        ret
    }

    pub fn reply_to_author(
        coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        reply_body: String,
        context: &Context,
    ) -> Self {
        Self::reply_to(coordinates, reply_body, context, false)
    }

    pub fn reply_to_all(
        coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        reply_body: String,
        context: &Context,
    ) -> Self {
        Self::reply_to(coordinates, reply_body, context, true)
    }

    pub fn forward(
        coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        bytes: &[u8],
        env: &Envelope,
        as_attachment: bool,
        context: &Context,
    ) -> Self {
        let mut composer = Self::with_account(coordinates.0, context);
        let mut draft: Draft = Draft::default();
        draft.set_header(HeaderName::SUBJECT, format!("Fwd: {}", env.subject()));
        let preamble = format!(
            r#"
---------- Forwarded message ---------
From: {}
Date: {}
Subject: {}
To: {}

"#,
            env.field_from_to_string(),
            env.date_as_str(),
            env.subject(),
            env.field_to_to_string()
        );
        if as_attachment {
            let mut attachment = AttachmentBuilder::new(b"");
            let mut disposition: ContentDisposition = ContentDispositionKind::Attachment.into();
            {
                disposition.filename = Some(format!("{}.eml", env.message_id()));
            }
            attachment
                .set_raw(bytes.to_vec())
                .set_body_to_raw()
                .set_content_type(ContentType::MessageRfc822)
                .set_content_transfer_encoding(ContentTransferEncoding::_8Bit)
                .set_content_disposition(disposition);
            draft.attachments.push(attachment);
            draft.body = preamble;
        } else {
            let content_type = ContentType::default();
            let preamble: AttachmentBuilder =
                Attachment::new(content_type, Default::default(), preamble.into_bytes()).into();
            draft.attachments.push(preamble);
            draft.attachments.push(env.body_bytes(bytes).into());
        }
        composer.set_draft(draft, context);
        composer
    }

    pub fn set_draft(&mut self, draft: Draft, context: &Context) {
        self.draft = draft;
        self.update_form(context);
    }

    fn update_draft(&mut self) {
        let header_values = self.form.values_mut();
        let draft_header_map = self.draft.headers_mut();
        for (k, v) in draft_header_map.iter_mut() {
            if let Some(vn) = header_values.get(k.as_str()) {
                *v = vn.as_str().to_string();
            }
        }
    }

    fn update_form(&mut self, context: &Context) {
        let old_cursor = self.form.cursor();
        let shortcuts = self.shortcuts(context);
        self.form = FormWidget::new(
            ("Save".into(), true),
            /* cursor_up_shortcut */
            shortcuts
                .get(Shortcuts::COMPOSING)
                .and_then(|c| c.get("scroll_up").cloned())
                .unwrap_or_else(|| context.settings.shortcuts.composing.scroll_up.clone()),
            /* cursor_down_shortcut */
            shortcuts
                .get(Shortcuts::COMPOSING)
                .and_then(|c| c.get("scroll_down").cloned())
                .unwrap_or_else(|| context.settings.shortcuts.composing.scroll_down.clone()),
        );
        self.form.hide_buttons();
        self.form.set_cursor(old_cursor);
        let headers = self.draft.headers();
        let account_hash = self.account_hash;
        for k in context.accounts[&account_hash]
            .backend_capabilities
            .extra_submission_headers
        {
            if matches!(*k, HeaderName::NEWSGROUPS) {
                self.form.push_cl((
                    k.into(),
                    headers[k].to_string(),
                    Box::new(move |c, term| {
                        c.accounts[&account_hash]
                            .mailbox_entries
                            .values()
                            .filter_map(|v| {
                                if v.path.starts_with(term) {
                                    Some(v.path.to_string())
                                } else {
                                    None
                                }
                            })
                            .map(AutoCompleteEntry::from)
                            .collect::<Vec<AutoCompleteEntry>>()
                    }),
                ));
            } else {
                self.form.push((k.into(), headers[k].to_string()));
            }
        }
        for k in &[
            HeaderName::DATE,
            HeaderName::FROM,
            HeaderName::TO,
            HeaderName::CC,
            HeaderName::BCC,
            HeaderName::SUBJECT,
        ] {
            if matches!(*k, HeaderName::TO | HeaderName::CC | HeaderName::BCC) {
                self.form.push_cl((
                    k.into(),
                    headers[k].to_string(),
                    Box::new(move |c, term| {
                        let book: &Contacts = &c.accounts[&account_hash].contacts;
                        let results: Vec<String> = book.search(term);
                        results
                            .into_iter()
                            .map(AutoCompleteEntry::from)
                            .collect::<Vec<AutoCompleteEntry>>()
                    }),
                ));
            } else if k == HeaderName::FROM {
                self.form.push_cl((
                    k.into(),
                    headers[k].to_string(),
                    Box::new(move |c, _term| {
                        c.accounts
                            .values()
                            .map(|acc| {
                                let addr = acc.settings.account.main_identity_address();
                                let desc = match account_settings!(c[acc.hash()].send_mail) {
                                    crate::conf::composing::SendMail::ShellCommand(ref cmd) => {
                                        let mut cmd = cmd.as_str();
                                        cmd.truncate_at_boundary(10);
                                        format!("{} [exec: {}]", acc.name(), cmd)
                                    }
                                    #[cfg(feature = "smtp")]
                                    crate::conf::composing::SendMail::Smtp(ref inner) => {
                                        let mut hostname = inner.hostname.as_str();
                                        hostname.truncate_at_boundary(10);
                                        format!("{} [smtp: {}]", acc.name(), hostname)
                                    }
                                    crate::conf::composing::SendMail::ServerSubmission => {
                                        format!("{} [server submission]", acc.name())
                                    }
                                };

                                (addr.to_string(), desc)
                            })
                            .map(AutoCompleteEntry::from)
                            .collect::<Vec<AutoCompleteEntry>>()
                    }),
                ));
            } else {
                self.form.push((k.into(), headers[k].to_string()));
            }
        }
    }

    fn draw_attachments(&self, grid: &mut CellBuffer, area: Area, context: &Context) {
        let attachments_no = self.draft.attachments().len();
        let theme_default = crate::conf::value(context, "theme_default");
        grid.clear_area(area, theme_default);
        let our_map: ShortcutMap =
            account_settings!(context[self.account_hash].shortcuts.composing).key_values();
        let mut shortcuts: ShortcutMaps = Default::default();
        shortcuts.insert(Shortcuts::COMPOSING, our_map);
        let toggle_shortcut = Key::Char('\n');
        let edit_shortcut = &shortcuts[Shortcuts::COMPOSING]["edit"];
        #[cfg(feature = "gpgme")]
        if self
            .gpg_state
            .sign_mail
            .unwrap_or(ActionFlag::False)
            .is_true()
        {
            let key_list = self
                .gpg_state
                .sign_keys
                .iter()
                .map(|k| k.to_string())
                .collect::<Vec<_>>()
                .join(", ");

            grid.write_string(
                &format!(
                    "☑ sign with [toggle: {}, edit: {}] {}",
                    toggle_shortcut,
                    edit_shortcut,
                    if self.gpg_state.sign_keys.is_empty() {
                        "default key"
                    } else {
                        key_list.as_str()
                    },
                ),
                theme_default.fg,
                if self.cursor == Cursor::Sign {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                area.skip_rows(1),
                None,
                None,
            );
        } else {
            grid.write_string(
                &format!(
                    "☐ don't sign [toggle: {}, edit: {}]",
                    toggle_shortcut, edit_shortcut,
                ),
                theme_default.fg,
                if self.cursor == Cursor::Sign {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                area.skip_rows(1),
                None,
                None,
            );
        }
        #[cfg(feature = "gpgme")]
        if self
            .gpg_state
            .encrypt_mail
            .unwrap_or(ActionFlag::False)
            .is_true()
        {
            let key_list = self
                .gpg_state
                .encrypt_keys
                .iter()
                .map(|k| k.to_string())
                .collect::<Vec<_>>()
                .join(", ");

            grid.write_string(
                &format!(
                    "{}{}{}",
                    if self.gpg_state.encrypt_keys.is_empty() {
                        "☐ no keys selected to encrypt with"
                    } else {
                        "☑ encrypt with"
                    },
                    &format!(
                        " [toggle: {}, edit: {}]{}",
                        toggle_shortcut,
                        edit_shortcut,
                        if self.gpg_state.encrypt_keys.is_empty() {
                            ""
                        } else {
                            " "
                        }
                    ),
                    if self.gpg_state.encrypt_keys.is_empty() {
                        ""
                    } else {
                        key_list.as_str()
                    }
                ),
                theme_default.fg,
                if self.cursor == Cursor::Encrypt {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                area.skip_rows(2),
                None,
                None,
            );
        } else {
            grid.write_string(
                &format!(
                    "☐ don't encrypt [toggle: {}, edit: {}]",
                    toggle_shortcut, edit_shortcut,
                ),
                theme_default.fg,
                if self.cursor == Cursor::Encrypt {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                area.skip_rows(2),
                None,
                None,
            );
        }
        if attachments_no == 0 {
            grid.write_string(
                &format!("no attachments [edit: {}]", edit_shortcut),
                theme_default.fg,
                if self.cursor == Cursor::Attachments {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                area.skip_rows(3),
                None,
                None,
            );
        } else {
            grid.write_string(
                &format!(
                    "{} attachment{} [edit: {}]",
                    attachments_no,
                    if attachments_no != 1 { "s" } else { "" },
                    edit_shortcut
                ),
                theme_default.fg,
                if self.cursor == Cursor::Attachments {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                area.skip_rows(3),
                None,
                None,
            );
            for (i, a) in self.draft.attachments().iter().enumerate() {
                grid.write_string(
                    &if let Some(name) = a.content_type().name() {
                        format!(
                            "[{}] \"{}\", {} {}",
                            i,
                            name,
                            a.content_type(),
                            melib::BytesDisplay(a.raw.len())
                        )
                    } else {
                        format!(
                            "[{}] {} {}",
                            i,
                            a.content_type(),
                            melib::BytesDisplay(a.raw.len())
                        )
                    },
                    theme_default.fg,
                    theme_default.bg,
                    theme_default.attrs,
                    area.skip_rows(4 + i),
                    None,
                    None,
                );
            }
        }
    }

    fn update_from_file(&mut self, file: File, context: &mut Context) -> bool {
        match file.read_to_string().and_then(|res| {
            self.draft.update(res.as_str()).map_err(|err| {
                self.draft.set_body(res);
                err
            })
        }) {
            Ok(has_changes) => {
                self.has_changes = has_changes;
                true
            }
            Err(err) => {
                context.replies.push_back(UIEvent::Notification {
                    title: Some("Could not parse draft headers correctly.".into()),
                    source: None,
                    body:
                        format!("{err}\nThe invalid text has been set as the body of your draft",)
                            .into(),
                    kind: Some(NotificationType::Error(melib::error::ErrorKind::None)),
                });
                self.has_changes = true;
                false
            }
        }
    }

    #[cfg(feature = "gpgme")]
    fn create_key_selection_widget(
        &self,
        secret: bool,
        header: &HeaderName,
        context: &Context,
    ) -> Result<gpg::KeySelectionLoading> {
        let (_, mut list) = melib::email::parser::address::rfc2822address_list(
            self.form.values()[header.as_str()].as_str().as_bytes(),
        )
        .map_err(|_err| -> Error { format!("No valid address in `{header}:`").into() })?;
        if list.is_empty() {
            return Err(format!("No valid address in `{header}:`").into());
        }
        let first = list.remove(0);
        let patterns = (
            first.get_email(),
            list.into_iter()
                .map(|addr| addr.get_email())
                .collect::<Vec<String>>(),
        );
        gpg::KeySelectionLoading::new(
            secret,
            account_settings!(context[self.account_hash].pgp.allow_remote_lookup).is_true(),
            patterns,
            *account_settings!(context[self.account_hash].pgp.allow_remote_lookup),
            context,
        )
    }
}

impl Component for Composer {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if area.height() < 4 {
            return;
        }

        if !self.initialized {
            #[cfg(feature = "gpgme")]
            if self.gpg_state.sign_mail.is_none() {
                self.gpg_state.sign_mail =
                    Some(*account_settings!(context[self.account_hash].pgp.auto_sign));
            }
            #[cfg(feature = "gpgme")]
            {
                self.gpg_state.encrypt_for_self =
                    *account_settings!(context[self.account_hash].pgp.encrypt_for_self);
            }
            if !self.draft.headers().contains_key(HeaderName::FROM)
                || self.draft.headers()[HeaderName::FROM].is_empty()
            {
                self.draft.set_header(
                    HeaderName::FROM,
                    context.accounts[&self.account_hash]
                        .settings
                        .account()
                        .main_identity_address()
                        .to_string(),
                );
            }
            self.pager.update_from_str(self.draft.body(), Some(77));
            self.update_form(context);
            self.initialized = true;
        }

        let theme_default = crate::conf::value(context, "theme_default");
        if self.dirty {
            grid.clear_area(area, theme_default);
        }

        let header_height = self.form.len();

        let header_area = area
            .skip_rows(1)
            .take_rows(header_height)
            .skip_cols(1)
            .skip_cols_from_end(1);
        let attachments_no = self.draft.attachments().len();
        let attachment_area = area
            .skip_rows(header_height + 1)
            .skip_rows(
                area.height()
                    .saturating_sub(header_area.height() + 4 + attachments_no),
            )
            .skip_cols(1)
            .skip_cols_from_end(1);

        let body_area = area
            .skip_rows(header_height + 2)
            .skip_rows_from_end(attachment_area.height())
            .skip_cols(1)
            .skip_cols_from_end(1);

        if self.dirty {
            grid.clear_area(area.nth_row(0), crate::conf::value(context, "highlight"));
            let our_map: ShortcutMap =
                account_settings!(context[self.account_hash].shortcuts.composing).key_values();
            let mut shortcuts: ShortcutMaps = Default::default();
            shortcuts.insert(Shortcuts::COMPOSING, our_map);
            let scroll_down_shortcut = &shortcuts[Shortcuts::COMPOSING]["scroll_down"];
            let scroll_up_shortcut = &shortcuts[Shortcuts::COMPOSING]["scroll_up"];
            let field_shortcut = Key::Char('\n');
            let edit_shortcut = &shortcuts[Shortcuts::COMPOSING]["edit"];
            grid.write_string(
                &format!(
                    "COMPOSING {} [scroll down: {}, scroll up: {}, edit fields: {}, edit body: {}]",
                    if self.reply_context.is_some() {
                        "REPLY"
                    } else {
                        "MESSAGE"
                    },
                    scroll_down_shortcut,
                    scroll_up_shortcut,
                    field_shortcut,
                    edit_shortcut
                ),
                crate::conf::value(context, "highlight").fg,
                crate::conf::value(context, "highlight").bg,
                crate::conf::value(context, "highlight").attrs,
                area.nth_row(0),
                None,
                None,
            );
        }

        /* Regardless of view mode, do the following */

        if self.dirty {
            match self.cursor {
                Cursor::Headers => {
                    grid.change_theme(header_area, theme_default);
                }
                Cursor::Body => {
                    grid.change_theme(
                        body_area,
                        ThemeAttribute {
                            fg: theme_default.fg,
                            bg: crate::conf::value(context, "highlight").bg,
                            attrs: if grid.use_color {
                                crate::conf::value(context, "highlight").attrs
                            } else {
                                crate::conf::value(context, "highlight").attrs | Attr::REVERSE
                            },
                        },
                    );
                }
                Cursor::Sign | Cursor::Encrypt | Cursor::Attachments => {}
            }
        }

        self.form.draw(grid, header_area, context);

        if let Some(ref mut embedded_pty) = self.embedded_pty {
            let embedded_area = area;
            if embedded_pty.is_dirty() {
                if embedded_pty.running {
                    let mut guard = embedded_pty.lock().unwrap();
                    grid.clear_area(embedded_area, theme_default);

                    grid.copy_area(guard.grid.buffer(), embedded_area, guard.grid.area());
                    guard.set_terminal_size((embedded_area.width(), embedded_area.height()));
                    guard.grid.set_dirty(false);
                    context.dirty_areas.push_back(embedded_area);
                    self.dirty = false;
                } else {
                    let mut guard = embedded_pty.lock().unwrap();

                    grid.copy_area(
                        guard.grid.buffer(),
                        embedded_area,
                        guard.grid.buffer().area(),
                    );
                    grid.change_colors(embedded_area, Color::Byte(8), theme_default.bg);
                    let our_map: ShortcutMap =
                        account_settings!(context[self.account_hash].shortcuts.composing)
                            .key_values();
                    let mut shortcuts: ShortcutMaps = Default::default();
                    shortcuts.insert(Shortcuts::COMPOSING, our_map);
                    let stopped_message: String =
                        format!("Process with PID {} has stopped.", guard.child_pid);
                    let stopped_message_2: String = format!(
                        "- re-activate '{}' (edit shortcut)",
                        shortcuts[Shortcuts::COMPOSING]["edit"]
                    );
                    const STOPPED_MESSAGE_3: &str =
                        "- press Ctrl-C to forcefully kill it and return to editor";
                    let max_len = std::cmp::max(
                        stopped_message.len(),
                        std::cmp::max(stopped_message_2.len(), STOPPED_MESSAGE_3.len()),
                    );
                    let inner_area = create_box(grid, area.center_inside((max_len + 5, 5)));
                    grid.clear_area(inner_area, theme_default);
                    for (i, l) in [
                        stopped_message.as_str(),
                        stopped_message_2.as_str(),
                        STOPPED_MESSAGE_3,
                    ]
                    .iter()
                    .enumerate()
                    {
                        grid.write_string(
                            l,
                            theme_default.fg,
                            theme_default.bg,
                            theme_default.attrs,
                            inner_area.skip_rows(i),
                            None,
                            None,
                        );
                    }
                    context.dirty_areas.push_back(area);
                    guard.grid.set_dirty(false);
                    self.dirty = false;
                }
            }
            return;
        } else {
            self.embedded_dimensions = (area.width(), area.height());
        }

        if self.pager.size().0 > body_area.width() {
            self.pager.set_initialised(false);
        }
        if self.dirty {
            // Force clean pager area, because if body height is less than body_area it will
            // might leave draw artifacts in the remaining area.
            grid.clear_area(body_area, theme_default);
            self.pager.set_dirty(true);
        }
        self.pager.draw(grid, body_area, context);

        if !self.mode.is_edit_attachments() {
            self.draw_attachments(grid, attachment_area, context);
        }
        match self.mode {
            ViewMode::Edit | ViewMode::EmbeddedPty => {}
            ViewMode::EditAttachments { ref mut widget } => {
                let inner_area = area.center_inside((
                    area.width().saturating_sub(2),
                    area.height().saturating_sub(2),
                ));
                (EditAttachmentsRefMut {
                    inner: widget,
                    draft: &mut self.draft,
                })
                .draw(grid, inner_area, context);
            }
            ViewMode::Send(ref mut s) => {
                let inner_area = area.center_inside((
                    area.width().saturating_sub(2),
                    area.height().saturating_sub(2),
                ));
                s.draw(grid, inner_area, context);
            }
            #[cfg(feature = "gpgme")]
            ViewMode::SelectKey(
                _,
                gpg::KeySelection::Loaded {
                    ref mut widget,
                    keys: _,
                },
            ) => {
                let inner_area = area.center_inside((
                    area.width().saturating_sub(2),
                    area.height().saturating_sub(2),
                ));
                widget.draw(grid, inner_area, context);
            }
            #[cfg(feature = "gpgme")]
            ViewMode::SelectKey(_, _) => {}
            ViewMode::SelectRecipients(ref mut s) => {
                let inner_area = area.center_inside((
                    area.width().saturating_sub(2),
                    area.height().saturating_sub(2),
                ));
                s.draw(grid, inner_area, context);
            }
            ViewMode::Discard(_, ref mut s) => {
                let inner_area = area.center_inside((
                    area.width().saturating_sub(2),
                    area.height().saturating_sub(2),
                ));
                /* Let user choose whether to quit with/without saving or cancel */
                s.draw(grid, inner_area, context);
            }
            ViewMode::WaitingForSendResult(ref mut s, _) => {
                let inner_area = area.center_inside((
                    area.width().saturating_sub(2),
                    area.height().saturating_sub(2),
                ));
                /* Let user choose whether to wait for success or cancel */
                s.draw(grid, inner_area, context);
            }
        }

        if self.dirty {
            self.dirty = false;
            context.dirty_areas.push_back(area);
        }
    }

    fn process_event(&mut self, mut event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::VisibilityChange(_) = event {
            self.pager.process_event(event, context);
        }
        let shortcuts = self.shortcuts(context);
        // Process scrolling first, since in my infinite wisdom I made this so
        // unnecessarily complex
        match &event {
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["scroll_up"]) =>
            {
                self.set_dirty(true);
                self.cursor = match self.cursor {
                    // match order is evaluation order, so it matters here because of the if guard
                    // process_event side effects
                    Cursor::Attachments => Cursor::Encrypt,
                    Cursor::Encrypt => Cursor::Sign,
                    Cursor::Sign => Cursor::Body,
                    Cursor::Body if !self.pager.process_event(event, context) => {
                        self.form.process_event(event, context);
                        Cursor::Headers
                    }
                    Cursor::Body => Cursor::Body,
                    Cursor::Headers if self.form.process_event(event, context) => Cursor::Headers,
                    Cursor::Headers => Cursor::Headers,
                };
                return true;
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["scroll_down"]) =>
            {
                self.set_dirty(true);
                self.cursor = match self.cursor {
                    Cursor::Headers if self.form.process_event(event, context) => Cursor::Headers,
                    Cursor::Headers => Cursor::Body,
                    Cursor::Body if self.pager.process_event(event, context) => Cursor::Body,
                    Cursor::Body => Cursor::Sign,
                    Cursor::Sign => Cursor::Encrypt,
                    Cursor::Encrypt => Cursor::Attachments,
                    Cursor::Attachments => Cursor::Attachments,
                };
                return true;
            }
            _ => {}
        }
        if self.cursor == Cursor::Headers
            && self.mode.is_edit()
            && self.form.process_event(event, context)
        {
            if let UIEvent::InsertInput(_) = event {
                self.update_draft();
                self.has_changes = true;
            }
            self.set_dirty(true);
            return true;
        }
        match (&mut self.mode, &mut event) {
            (ViewMode::Edit, _) => {
                if self.pager.process_event(event, context) {
                    return true;
                }
            }
            (ViewMode::EditAttachments { ref mut widget }, _) => {
                if (EditAttachmentsRefMut {
                    inner: widget,
                    draft: &mut self.draft,
                })
                .process_event(event, context)
                {
                    if matches!(
                        widget.buttons.result(),
                        Some(FormButtonAction::Cancel | FormButtonAction::Accept)
                    ) {
                        self.mode = ViewMode::Edit;
                    }
                    self.set_dirty(true);
                    return true;
                }
            }
            (ViewMode::Send(ref selector), UIEvent::FinishedUIDialog(id, result))
                if selector.id() == *id =>
            {
                if matches!(result.downcast_ref::<bool>(), Some(true)) {
                    self.update_draft();
                    match send_draft_async(
                        #[cfg(feature = "gpgme")]
                        self.gpg_state.clone(),
                        context,
                        self.account_hash,
                        self.draft.clone(),
                        SpecialUsageMailbox::Sent,
                        Flag::SEEN,
                    ) {
                        Ok(job) => {
                            let handle = context.main_loop_handler.job_executor.spawn(
                                "compose::submit".into(),
                                job,
                                IsAsync::Blocking,
                            );
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::NewJob(
                                    handle.job_id,
                                )));
                            self.mode = ViewMode::WaitingForSendResult(
                                UIDialog::new(
                                    "Waiting for confirmation.. The tab will close automatically \
                                     on successful submission.",
                                    vec![
                                        ('c', "force close tab".to_string()),
                                        (
                                            'n',
                                            "close this message and return to edit mode"
                                                .to_string(),
                                        ),
                                    ],
                                    true,
                                    Some(Box::new(move |id: ComponentId, results: &[char]| {
                                        Some(UIEvent::FinishedUIDialog(
                                            id,
                                            Box::new(results.first().cloned().unwrap_or('c')),
                                        ))
                                    })),
                                    context,
                                ),
                                handle,
                            );
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: None,
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            });
                            save_draft(
                                self.draft.clone().finalise().unwrap().as_bytes(),
                                context,
                                SpecialUsageMailbox::Drafts,
                                Flag::SEEN | Flag::DRAFT,
                                self.account_hash,
                            );
                            self.mode = ViewMode::Edit;
                        }
                    }
                }
                self.set_dirty(true);
                return true;
            }
            (ViewMode::Send(ref dialog), UIEvent::ComponentUnrealize(ref id))
                if *id == dialog.id() =>
            {
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
            }
            (ViewMode::SelectRecipients(ref dialog), UIEvent::ComponentUnrealize(ref id))
                if *id == dialog.id() =>
            {
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
            }
            (ViewMode::Discard(_, ref dialog), UIEvent::ComponentUnrealize(ref id))
                if *id == dialog.id() =>
            {
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
            }
            #[cfg(feature = "gpgme")]
            (ViewMode::SelectKey(_, ref mut selector), UIEvent::ComponentUnrealize(ref id))
                if *id == selector.id() =>
            {
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
                return true;
            }
            (ViewMode::Send(ref mut selector), _) => {
                if selector.process_event(event, context) {
                    self.set_dirty(true);
                    return true;
                }
            }
            (
                ViewMode::SelectRecipients(ref selector),
                UIEvent::FinishedUIDialog(id, ref mut result),
            ) if selector.id() == *id => {
                if let Some(to_val) = result.downcast_mut::<String>() {
                    self.draft
                        .set_header(HeaderName::TO, std::mem::take(to_val));
                    self.update_form(context);
                }
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
                return true;
            }
            (ViewMode::SelectRecipients(ref mut selector), _) => {
                if selector.process_event(event, context) {
                    self.set_dirty(true);
                    return true;
                }
            }
            (ViewMode::Discard(u, ref selector), UIEvent::FinishedUIDialog(id, ref mut result))
                if selector.id() == *id =>
            {
                if let Some(key) = result.downcast_mut::<char>() {
                    match key {
                        'x' => {
                            context.replies.push_back(UIEvent::Action(Tab(Kill(*u))));
                            return true;
                        }
                        'n' => {}
                        'y' => {
                            save_draft(
                                self.draft.clone().finalise().unwrap().as_bytes(),
                                context,
                                SpecialUsageMailbox::Drafts,
                                Flag::SEEN | Flag::DRAFT,
                                self.account_hash,
                            );
                            context.replies.push_back(UIEvent::Action(Tab(Kill(*u))));
                            return true;
                        }
                        _ => {}
                    }
                }
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
                return true;
            }
            (ViewMode::Discard(_, ref mut selector), _) => {
                if selector.process_event(event, context) {
                    self.set_dirty(true);
                    return true;
                }
            }
            (
                ViewMode::WaitingForSendResult(ref selector, _),
                UIEvent::FinishedUIDialog(id, result),
            ) if selector.id() == *id => {
                if let Some(key) = result.downcast_mut::<char>() {
                    match key {
                        'c' => {
                            context
                                .replies
                                .push_back(UIEvent::Action(Tab(Kill(self.id))));
                            self.set_dirty(true);
                            return true;
                        }
                        'n' => {
                            self.set_dirty(true);
                            if let ViewMode::WaitingForSendResult(_, handle) =
                                std::mem::replace(&mut self.mode, ViewMode::Edit)
                            {
                                context.accounts[&self.account_hash].active_jobs.insert(
                                    handle.job_id,
                                    JobRequest::SendMessageBackground { handle },
                                );
                            }
                        }
                        _ => {}
                    }
                }
                return true;
            }
            (
                ViewMode::WaitingForSendResult(_, ref mut handle),
                UIEvent::StatusEvent(StatusEvent::JobFinished(ref job_id)),
            ) if handle.job_id == *job_id => {
                match handle
                    .chan
                    .try_recv()
                    .map_err(|_: futures::channel::oneshot::Canceled| {
                        Error::new("Job was canceled")
                    }) {
                    Err(err) | Ok(Some(Err(err))) => {
                        self.mode = ViewMode::Edit;
                        context.replies.push_back(UIEvent::Notification {
                            title: None,
                            source: None,
                            body: err.to_string().into(),
                            kind: Some(NotificationType::Error(err.kind)),
                        });
                        self.set_dirty(true);
                    }
                    Ok(None) | Ok(Some(Ok(()))) => {
                        context
                            .replies
                            .push_back(UIEvent::Action(Tab(Kill(self.id))));
                    }
                }
                return false;
            }
            (ViewMode::WaitingForSendResult(ref mut selector, _), _) => {
                if selector.process_event(event, context) {
                    self.set_dirty(true);
                    return true;
                }
            }
            #[cfg(feature = "gpgme")]
            (
                ViewMode::SelectKey(is_encrypt, ref mut selector),
                UIEvent::FinishedUIDialog(id, result),
            ) if *id == selector.id() => {
                if let Some(Some(keys)) = result.downcast_mut::<Option<Vec<melib::gpgme::Key>>>() {
                    if *is_encrypt {
                        self.gpg_state.encrypt_keys.clear();
                        self.gpg_state.encrypt_keys = std::mem::take(keys);
                    } else {
                        self.gpg_state.sign_keys.clear();
                        self.gpg_state.sign_keys = std::mem::take(keys);
                    }
                }
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
                return true;
            }
            #[cfg(feature = "gpgme")]
            (ViewMode::SelectKey(_, ref mut selector), _) => {
                if selector.process_event(event, context) {
                    self.set_dirty(true);
                    return true;
                }
            }
            _ => {}
        }

        match *event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.set_dirty(true);
            }
            UIEvent::Resize => {
                self.set_dirty(true);
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["scroll_up"]) =>
            {
                self.set_dirty(true);
                self.cursor = match self.cursor {
                    Cursor::Headers => return true,
                    Cursor::Body => {
                        self.form.process_event(event, context);
                        Cursor::Headers
                    }
                    Cursor::Sign => Cursor::Body,
                    Cursor::Encrypt => Cursor::Sign,
                    Cursor::Attachments => Cursor::Encrypt,
                };
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["scroll_down"]) =>
            {
                self.set_dirty(true);
                self.cursor = match self.cursor {
                    Cursor::Headers => Cursor::Body,
                    Cursor::Body => Cursor::Sign,
                    Cursor::Sign => Cursor::Encrypt,
                    Cursor::Encrypt => Cursor::Attachments,
                    Cursor::Attachments => return true,
                };
            }
            UIEvent::Input(Key::Char('\n'))
                if self.mode.is_edit()
                    && (self.cursor == Cursor::Sign || self.cursor == Cursor::Encrypt) =>
            {
                #[cfg(feature = "gpgme")]
                match self.cursor {
                    Cursor::Sign => {
                        let is_true = self
                            .gpg_state
                            .sign_mail
                            .unwrap_or(ActionFlag::False)
                            .is_true();
                        self.gpg_state.sign_mail = Some(ActionFlag::from(!is_true));
                    }
                    Cursor::Encrypt => {
                        let is_true = self
                            .gpg_state
                            .encrypt_mail
                            .unwrap_or(ActionFlag::False)
                            .is_true();
                        self.gpg_state.encrypt_mail = Some(ActionFlag::from(!is_true));
                    }
                    _ => {}
                };
                self.set_dirty(true);
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::COMPOSING]["send_mail"])
                    && self.mode.is_edit() =>
            {
                self.update_draft();

                {
                    let Self {
                        ref mut hooks,
                        ref mut draft,
                        ..
                    } = self;

                    // Collect errors in a vector because filter_map borrows context
                    let errors = hooks
                        .iter_mut()
                        .filter_map(|h| {
                            if let Err(err) = h(context, draft) {
                                Some(err)
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();
                    for err in errors {
                        context.replies.push_back(UIEvent::Notification {
                            title: None,
                            source: None,
                            body: err.to_string().into(),
                            kind: None,
                        });
                    }
                }
                self.mode = ViewMode::Send(UIConfirmationDialog::new(
                    "send mail?",
                    vec![(true, "yes".to_string()), (false, "no".to_string())],
                    /* only one choice */
                    true,
                    Some(Box::new(move |id: ComponentId, result: bool| {
                        Some(UIEvent::FinishedUIDialog(id, Box::new(result)))
                    })),
                    context,
                ));
                return true;
            }
            UIEvent::EmbeddedInput((Key::Ctrl('z'), _)) => {
                self.embedded_pty.as_ref().unwrap().lock().unwrap().stop();
                if let Some(EmbeddedPty {
                    running: _,
                    terminal,
                    file,
                }) = self.embedded_pty.take()
                {
                    self.embedded_pty = Some(EmbeddedPty {
                        running: false,
                        terminal,
                        file,
                    });
                }
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                self.set_dirty(true);
            }
            UIEvent::EmbeddedInput((ref k, ref b)) => {
                if let Some(ref mut embedded) = self.embedded_pty {
                    let mut embedded_guard = embedded.lock().unwrap();
                    if embedded_guard.write_all(b).is_err() {
                        match embedded_guard.is_active() {
                            Ok(WaitStatus::Exited(_, exit_code)) => {
                                drop(embedded_guard);
                                let embedded_pty = self.embedded_pty.take();
                                if exit_code != 0 {
                                    context.replies.push_back(UIEvent::Notification {
                                        title: None,
                                        source: None,
                                        body: format!(
                                            "Subprocess has exited with exit code {exit_code}",
                                        )
                                        .into(),
                                        kind: Some(NotificationType::Error(
                                            melib::error::ErrorKind::External,
                                        )),
                                    });
                                } else if let Some(EmbeddedPty {
                                    running: true,
                                    file,
                                    ..
                                }) = embedded_pty
                                {
                                    self.update_from_file(file, context);
                                }
                                self.initialized = false;
                                self.mode = ViewMode::Edit;
                                self.set_dirty(true);
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                            }
                            #[cfg(any(target_os = "linux", target_os = "android"))]
                            Ok(WaitStatus::PtraceEvent(_, _, _))
                            | Ok(WaitStatus::PtraceSyscall(_)) => {
                                drop(embedded_guard);
                                if let Some(EmbeddedPty {
                                    running: _,
                                    terminal,
                                    file,
                                }) = self.embedded_pty.take()
                                {
                                    self.embedded_pty = Some(EmbeddedPty {
                                        running: false,
                                        terminal,
                                        file,
                                    });
                                }
                                self.mode = ViewMode::Edit;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                                self.set_dirty(true);
                                return true;
                            }
                            Ok(WaitStatus::Stopped(_, _)) => {
                                drop(embedded_guard);
                                if let Some(EmbeddedPty {
                                    running: _,
                                    terminal,
                                    file,
                                }) = self.embedded_pty.take()
                                {
                                    self.embedded_pty = Some(EmbeddedPty {
                                        running: false,
                                        terminal,
                                        file,
                                    });
                                }
                                self.mode = ViewMode::Edit;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                                self.set_dirty(true);
                                return true;
                            }
                            Ok(WaitStatus::Continued(_)) | Ok(WaitStatus::StillAlive) => {
                                context
                                    .replies
                                    .push_back(UIEvent::EmbeddedInput((k.clone(), b.to_vec())));
                                drop(embedded_guard);
                                self.set_dirty(true);
                                return true;
                            }
                            Ok(WaitStatus::Signaled(_, signal, _)) => {
                                drop(embedded_guard);
                                context.replies.push_back(UIEvent::Notification {
                                    title: None,
                                    source: None,
                                    body: format!("Subprocess was killed by {signal} signal")
                                        .into(),
                                    kind: Some(NotificationType::Error(
                                        melib::error::ErrorKind::External,
                                    )),
                                });
                                self.initialized = false;
                                self.embedded_pty = None;
                                self.mode = ViewMode::Edit;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                            }
                            Err(err) => {
                                context.replies.push_back(UIEvent::Notification {
                                    title: Some("Embedded editor crashed.".into()),
                                    source: None,
                                    body: format!("Subprocess has exited with reason {err}").into(),
                                    kind: Some(NotificationType::Error(
                                        melib::error::ErrorKind::External,
                                    )),
                                });
                                drop(embedded_guard);
                                self.initialized = false;
                                self.embedded_pty = None;
                                self.mode = ViewMode::Edit;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                            }
                        }
                    }
                }
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && self.cursor == Cursor::Sign
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["edit"]) =>
            {
                #[cfg(feature = "gpgme")]
                {
                    match self
                        .create_key_selection_widget(false, &HeaderName::FROM, context)
                        .map(Into::into)
                    {
                        Ok(widget) => {
                            self.gpg_state.sign_mail = Some(ActionFlag::from(true));
                            self.mode = ViewMode::SelectKey(false, widget);
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some("Could not list keys.".into()),
                                source: None,
                                body: format!("libgpgme error: {err}").into(),
                                kind: Some(NotificationType::Error(
                                    melib::error::ErrorKind::External,
                                )),
                            });
                        }
                    }
                    self.set_dirty(true);
                }
                return true;
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && self.cursor == Cursor::Encrypt
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["edit"]) =>
            {
                #[cfg(feature = "gpgme")]
                {
                    let mut result =
                        self.create_key_selection_widget(false, &HeaderName::TO, context);
                    if !self.form.values()[HeaderName::CC.as_str()]
                        .as_str()
                        .is_empty()
                    {
                        result = result.and_then(|mut to_result| {
                            let cc_result =
                                self.create_key_selection_widget(false, &HeaderName::CC, context)?;
                            to_result.merge(cc_result);
                            Ok(to_result)
                        });
                    }
                    if !self.form.values()[HeaderName::BCC.as_str()]
                        .as_str()
                        .is_empty()
                    {
                        result = result.and_then(|mut to_result| {
                            let bcc_result =
                                self.create_key_selection_widget(false, &HeaderName::BCC, context)?;
                            to_result.merge(bcc_result);
                            Ok(to_result)
                        });
                    }
                    if !self.form.values()[HeaderName::FROM.as_str()]
                        .as_str()
                        .is_empty()
                    {
                        result = result.and_then(|mut to_result| {
                            let from_result = self.create_key_selection_widget(
                                false,
                                &HeaderName::FROM,
                                context,
                            )?;
                            to_result.merge(from_result);
                            Ok(to_result)
                        });
                    }
                    match result.map(Into::into) {
                        Ok(widget) => {
                            self.gpg_state.encrypt_mail = Some(ActionFlag::from(true));
                            self.mode = ViewMode::SelectKey(true, widget);
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some("Could not list keys.".into()),
                                source: None,
                                body: format!("libgpgme error: {err}").into(),
                                kind: Some(NotificationType::Error(
                                    melib::error::ErrorKind::External,
                                )),
                            });
                        }
                    }
                    self.set_dirty(true);
                }
                return true;
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && self.cursor == Cursor::Attachments
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["edit"]) =>
            {
                self.mode = ViewMode::EditAttachments {
                    widget: EditAttachments::new(Some(self.account_hash)),
                };
                self.set_dirty(true);

                return true;
            }
            UIEvent::Input(ref key)
                if self.embedded_pty.is_some()
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["edit"]) =>
            {
                if let Some(EmbeddedPty {
                    running: _,
                    terminal,
                    file,
                }) = self.embedded_pty.take()
                {
                    terminal.lock().unwrap().wake_up();
                    self.embedded_pty = Some(EmbeddedPty {
                        running: true,
                        terminal,
                        file,
                    });
                }
                self.mode = ViewMode::EmbeddedPty;
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Embedded));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(Key::Ctrl('c'))
                if self.embedded_pty.is_some()
                    && self.embedded_pty.as_ref().unwrap().is_stopped() =>
            {
                if let Some(EmbeddedPty {
                    running: _,
                    terminal,
                    file,
                }) = self.embedded_pty.take()
                {
                    let guard = terminal.lock().unwrap();
                    guard.wake_up();
                    guard.terminate();
                    self.update_from_file(file, context);
                }
                context.replies.push_back(UIEvent::Notification {
                    title: None,
                    source: None,
                    body: "Subprocess was killed by SIGTERM signal".into(),
                    kind: Some(NotificationType::Error(melib::error::ErrorKind::External)),
                });
                self.initialized = false;
                self.mode = ViewMode::Edit;
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["edit"]) =>
            {
                /* Edit draft in $EDITOR */
                let editor = if let Some(editor_command) =
                    account_settings!(context[self.account_hash].composing.editor_command).as_ref()
                {
                    editor_command.to_string()
                } else {
                    match std::env::var("EDITOR") {
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some(err.to_string().into()),
                                source: None,
                                body: "$EDITOR is not set. You can change an envvar's value with \
                                       setenv or set composing.editor_command setting in your \
                                       configuration."
                                    .into(),
                                kind: Some(NotificationType::Error(melib::error::ErrorKind::None)),
                            });
                            self.set_dirty(true);
                            return true;
                        }
                        Ok(v) => v,
                    }
                };
                /* update Draft's headers based on form values */
                self.update_draft();
                self.draft.set_wrap_header_preamble(
                    account_settings!(context[self.account_hash].composing.wrap_header_preamble)
                        .clone(),
                );
                let filename = format!(
                    "{date}_{subject}_{to}_{in_reply_to}",
                    date = self.draft.headers.get(HeaderName::DATE).unwrap_or_default(),
                    subject = self
                        .draft
                        .headers
                        .get(HeaderName::SUBJECT)
                        .unwrap_or_default(),
                    to = self.draft.headers.get(HeaderName::TO).unwrap_or_default(),
                    in_reply_to = self
                        .draft
                        .headers
                        .get(HeaderName::IN_REPLY_TO)
                        .unwrap_or_default()
                );

                let f = match File::create_temp_file(
                    self.draft.to_edit_string().as_bytes(),
                    sanitize_filename(filename).as_deref(),
                    None,
                    Some("eml"),
                    true,
                ) {
                    Ok(f) => f,
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification {
                            title: None,
                            source: None,
                            body: err.to_string().into(),
                            kind: Some(NotificationType::Error(err.kind)),
                        });
                        self.set_dirty(true);
                        return true;
                    }
                };

                if *account_settings!(context[self.account_hash].composing.embedded_pty) {
                    let command = [editor, f.path().display().to_string()].join(" ");
                    match crate::terminal::embedded::create_pty(
                        self.embedded_dimensions.0,
                        self.embedded_dimensions.1,
                        &command,
                    ) {
                        Ok(terminal) => {
                            self.embedded_pty = Some(EmbeddedPty {
                                running: true,
                                terminal,
                                file: f,
                            });
                            self.set_dirty(true);
                            context
                                .replies
                                .push_back(UIEvent::ChangeMode(UIMode::Embedded));
                            context.replies.push_back(UIEvent::Fork(ForkType::Embedded {
                                id: "editor".into(),
                                command: Some(command.into()),
                                pid: self
                                    .embedded_pty
                                    .as_ref()
                                    .unwrap()
                                    .lock()
                                    .unwrap()
                                    .child_pid,
                            }));
                            self.mode = ViewMode::EmbeddedPty;
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some(
                                    format!("Failed to create pseudoterminal: {}", err).into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(
                                    melib::error::ErrorKind::External,
                                )),
                            });
                        }
                    }
                    self.set_dirty(true);
                    return true;
                }
                /* Kill input thread so that spawned command can be sole receiver of stdin */
                {
                    context.input_kill();
                }

                let editor_command = format!("{} {}", editor, f.path().display());
                log::trace!(
                    "Executing: sh -c \"{}\"",
                    editor_command.replace('"', "\\\"")
                );
                match Command::new("sh")
                    .args(["-c", &editor_command])
                    .stdin(Stdio::inherit())
                    .stdout(Stdio::inherit())
                    .spawn()
                {
                    Ok(mut child) => {
                        let _ = child.wait();
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification {
                            title: Some(format!("Failed to execute {}: {}", editor, err).into()),
                            source: None,
                            body: err.to_string().into(),
                            kind: Some(NotificationType::Error(melib::error::ErrorKind::External)),
                        });
                        context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                        context.restore_input();
                        self.set_dirty(true);
                        return true;
                    }
                }
                context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                match f.read_to_string().and_then(|res| {
                    self.draft.update(res.as_str()).map_err(|err| {
                        self.draft.set_body(res);
                        err
                    })
                }) {
                    Ok(has_changes) => {
                        self.has_changes = has_changes;
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification {
                            title: Some("Could not parse draft headers correctly.".into()),
                            source: None,
                            body: format!(
                                "{err}\nThe invalid text has been set as the body of your draft",
                            )
                            .into(),
                            kind: Some(NotificationType::Error(melib::error::ErrorKind::None)),
                        });
                        self.has_changes = true;
                    }
                }
                self.initialized = false;
                self.set_dirty(true);
                return true;
            }
            UIEvent::Action(Action::Tab(ComposerAction(ref a))) => match a {
                ComposerTabAction::AddAttachmentPipe(ref command) => {
                    if command.is_empty() {
                        context.replies.push_back(UIEvent::Notification {
                            title: None,
                            source: None,
                            body: format!("pipe command value is invalid: {command}").into(),
                            kind: Some(NotificationType::Error(melib::error::ErrorKind::None)),
                        });
                        return false;
                    }
                    let res = File::create_temp_file(&[], None, None, None, true)
                        .and_then(|f| {
                            let std_file = f.as_std_file()?;
                            Ok((
                                f,
                                Command::new("sh")
                                    .args(["-c", command])
                                    .stdin(Stdio::null())
                                    .stdout(Stdio::from(std_file))
                                    .spawn()?,
                            ))
                        })
                        .and_then(|(f, child)| Ok((f, child.wait_with_output()?.stderr)));
                    match res {
                        Ok((f, stderr)) => {
                            if !stderr.is_empty() {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(format!(
                                        "Command stderr output: `{}`.",
                                        String::from_utf8_lossy(&stderr)
                                    )),
                                ));
                            }
                            let attachment =
                                match melib::email::compose::attachment_from_file(&f.path()) {
                                    Ok(a) => a,
                                    Err(err) => {
                                        context.replies.push_back(UIEvent::Notification {
                                            title: Some("could not add attachment".into()),
                                            source: None,
                                            body: err.to_string().into(),
                                            kind: Some(NotificationType::Error(
                                                melib::error::ErrorKind::None,
                                            )),
                                        });
                                        self.set_dirty(true);
                                        return true;
                                    }
                                };
                            self.draft.attachments_mut().push(attachment);
                            self.has_changes = true;
                            self.set_dirty(true);
                            return true;
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: None,
                                source: None,
                                body: format!("could not execute pipe command {command}: {err}")
                                    .into(),
                                kind: Some(NotificationType::Error(
                                    melib::error::ErrorKind::External,
                                )),
                            });
                            self.set_dirty(true);
                            return true;
                        }
                    }
                }
                ComposerTabAction::AddAttachment(ref path) => {
                    let attachment = match melib::email::compose::attachment_from_file(path) {
                        Ok(a) => a,
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some("could not add attachment".into()),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(melib::error::ErrorKind::None)),
                            });
                            self.set_dirty(true);
                            return true;
                        }
                    };
                    self.draft.attachments_mut().push(attachment);
                    self.has_changes = true;
                    self.set_dirty(true);
                    return true;
                }
                ComposerTabAction::AddAttachmentFilePicker(ref command) => {
                    let command = if let Some(cmd) =
                        command
                            .as_ref()
                            .or(context.settings.terminal.file_picker_command.as_ref())
                    {
                        cmd.as_str()
                    } else {
                        context.replies.push_back(UIEvent::Notification {
                            title: None,
                            source: None,
                            body: "You haven't defined any command to launch in \
                                   [terminal.file_picker_command]."
                                .into(),
                            kind: Some(NotificationType::Error(melib::error::ErrorKind::None)),
                        });
                        self.set_dirty(true);
                        return true;
                    };
                    /* Kill input thread so that spawned command can be sole receiver of stdin */
                    {
                        context.input_kill();
                    }

                    log::trace!("Executing: sh -c \"{}\"", command.replace('"', "\\\""));
                    match Command::new("sh")
                        .args(["-c", command])
                        .stdin(Stdio::inherit())
                        .stdout(Stdio::piped())
                        .stderr(Stdio::piped())
                        .spawn()
                        .and_then(|child| Ok(child.wait_with_output()?.stdout))
                    {
                        Ok(stdout) => {
                            for path in stdout
                                .split(|c| [b'\0', b'\t', b'\n'].contains(c))
                                .filter(|p| !p.trim().is_empty())
                            {
                                match melib::email::compose::attachment_from_file(
                                    &String::from_utf8_lossy(path).as_ref(),
                                ) {
                                    Ok(a) => {
                                        self.draft.attachments_mut().push(a);
                                        self.has_changes = true;
                                    }
                                    Err(err) => {
                                        context.replies.push_back(UIEvent::Notification {
                                            title: Some(
                                                format!(
                                                    "could not add attachment: {}",
                                                    String::from_utf8_lossy(path)
                                                )
                                                .into(),
                                            ),
                                            source: None,
                                            body: err.to_string().into(),
                                            kind: Some(NotificationType::Error(
                                                melib::error::ErrorKind::None,
                                            )),
                                        });
                                    }
                                };
                            }
                        }
                        Err(err) => {
                            let command = command.to_string();
                            context.replies.push_back(UIEvent::Notification {
                                title: Some(
                                    format!("Failed to execute {}: {}", command, err).into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(
                                    melib::error::ErrorKind::External,
                                )),
                            });
                            context.restore_input();
                            self.set_dirty(true);
                            return true;
                        }
                    }
                    context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                    self.set_dirty(true);
                    return true;
                }
                ComposerTabAction::RemoveAttachment(idx) => {
                    if *idx + 1 > self.draft.attachments().len() {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(
                                "attachment with given index does not exist".to_string(),
                            ),
                        ));
                        self.set_dirty(true);
                        return true;
                    }
                    self.draft.attachments_mut().remove(*idx);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                            "attachment removed".to_string(),
                        )));
                    self.set_dirty(true);
                    return true;
                }
                ComposerTabAction::SaveDraft => {
                    save_draft(
                        self.draft.clone().finalise().unwrap().as_bytes(),
                        context,
                        SpecialUsageMailbox::Drafts,
                        Flag::SEEN | Flag::DRAFT,
                        self.account_hash,
                    );
                    self.set_dirty(true);
                    return true;
                }
                ComposerTabAction::DiscardDraft => {
                    context
                        .replies
                        .push_back(UIEvent::Action(Tab(Kill(self.id))));
                    self.set_dirty(true);
                    return true;
                }
                #[cfg(feature = "gpgme")]
                ComposerTabAction::ToggleSign => {
                    let is_true = self
                        .gpg_state
                        .sign_mail
                        .unwrap_or(ActionFlag::False)
                        .is_true();
                    self.gpg_state.sign_mail = Some(ActionFlag::from(!is_true));
                    self.set_dirty(true);
                    return true;
                }
                #[cfg(feature = "gpgme")]
                ComposerTabAction::ToggleEncrypt => {
                    let is_true = self
                        .gpg_state
                        .encrypt_mail
                        .unwrap_or(ActionFlag::False)
                        .is_true();
                    self.gpg_state.encrypt_mail = Some(ActionFlag::from(!is_true));
                    self.set_dirty(true);
                    return true;
                }
            },
            UIEvent::Input(ref key)
                if context
                    .settings
                    .shortcuts
                    .composing
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
        match self.mode {
            ViewMode::EmbeddedPty => {
                self.dirty
                    || self
                        .embedded_pty
                        .as_ref()
                        .map(EmbeddedPty::is_dirty)
                        .unwrap_or(false)
            }
            ViewMode::EditAttachments { ref widget } => {
                widget.dirty
                    || widget.buttons.is_dirty()
                    || self.dirty
                    || self.pager.is_dirty()
                    || self.form.is_dirty()
            }
            ViewMode::Edit => self.dirty || self.pager.is_dirty() || self.form.is_dirty(),
            ViewMode::Discard(_, ref widget) => {
                widget.is_dirty() || self.pager.is_dirty() || self.form.is_dirty()
            }
            ViewMode::SelectRecipients(ref widget) => {
                widget.is_dirty() || self.pager.is_dirty() || self.form.is_dirty()
            }
            #[cfg(feature = "gpgme")]
            ViewMode::SelectKey(_, ref widget) => {
                widget.is_dirty() || self.pager.is_dirty() || self.form.is_dirty()
            }
            ViewMode::Send(ref widget) => {
                widget.is_dirty() || self.pager.is_dirty() || self.form.is_dirty()
            }
            ViewMode::WaitingForSendResult(ref widget, _) => {
                widget.is_dirty() || self.pager.is_dirty() || self.form.is_dirty()
            }
        }
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.pager.set_dirty(value);
        self.form.set_dirty(value);
        match self.mode {
            ViewMode::Discard(_, ref mut widget) => {
                widget.set_dirty(value);
            }
            ViewMode::SelectRecipients(ref mut widget) => {
                widget.set_dirty(value);
            }
            #[cfg(feature = "gpgme")]
            ViewMode::SelectKey(_, ref mut widget) => {
                widget.set_dirty(value);
            }
            ViewMode::Send(ref mut widget) => {
                widget.set_dirty(value);
            }
            ViewMode::WaitingForSendResult(ref mut widget, _) => {
                widget.set_dirty(value);
            }
            ViewMode::Edit => {}
            ViewMode::EmbeddedPty => {
                if let Some(pty) = self.embedded_pty.as_ref() {
                    if let Ok(mut guard) = pty.try_lock() {
                        guard.grid.set_dirty(value);
                    }
                }
            }
            ViewMode::EditAttachments { ref mut widget } => {
                (EditAttachmentsRefMut {
                    inner: widget,
                    draft: &mut self.draft,
                })
                .set_dirty(value);
            }
        }
    }

    fn kill(&mut self, uuid: ComponentId, context: &mut Context) {
        if self.id != uuid {
            return;
        }

        if !self.has_changes {
            context.replies.push_back(UIEvent::Action(Tab(Kill(uuid))));
            return;
        }

        self.mode = ViewMode::Discard(
            uuid,
            UIDialog::new(
                "this draft has unsaved changes",
                vec![
                    ('x', "quit without saving".to_string()),
                    ('y', "save draft and quit".to_string()),
                    ('n', "cancel".to_string()),
                ],
                true,
                Some(Box::new(move |id: ComponentId, results: &[char]| {
                    Some(UIEvent::FinishedUIDialog(
                        id,
                        Box::new(results.first().copied().unwrap_or('n')),
                    ))
                })),
                context,
            ),
        );
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if self.mode.is_edit() {
            self.pager.shortcuts(context)
        } else {
            Default::default()
        };

        let our_map: ShortcutMap =
            account_settings!(context[self.account_hash].shortcuts.composing).key_values();
        map.insert(Shortcuts::COMPOSING, our_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        if !self.has_changes {
            return true;
        }

        let id = self.id;
        /* Play it safe and ask user for confirmation */
        self.mode = ViewMode::Discard(
            id,
            UIDialog::new(
                "this draft has unsaved changes",
                vec![
                    ('x', "quit without saving".to_string()),
                    ('y', "save draft and quit".to_string()),
                    ('n', "cancel".to_string()),
                ],
                true,
                Some(Box::new(move |id: ComponentId, results: &[char]| {
                    Some(UIEvent::FinishedUIDialog(
                        id,
                        Box::new(results.first().copied().unwrap_or('n')),
                    ))
                })),
                context,
            ),
        );
        self.set_dirty(true);
        false
    }
}

pub fn send_draft(
    _sign_mail: ToggleFlag,
    context: &mut Context,
    account_hash: AccountHash,
    mut draft: Draft,
    mailbox_type: SpecialUsageMailbox,
    flags: Flag,
    complete_in_background: bool,
) -> Result<Option<JoinHandle<Result<()>>>> {
    let format_flowed = *account_settings!(context[account_hash].composing.format_flowed);
    /*    if sign_mail.is_true() {
        let mut content_type = ContentType::default();
        if format_flowed {
            if let ContentType::Text {
                ref mut parameters, ..
            } = content_type
            {
                parameters.push((b"format".to_vec(), b"flowed".to_vec()));
            }
        }

        let mut body: AttachmentBuilder = Attachment::new(
            content_type,
            Default::default(),
            std::mem::replace(&mut draft.body, String::new()).into_bytes(),
        )
        .into();
        if !draft.attachments.is_empty() {
            let mut parts = std::mem::replace(&mut draft.attachments, Vec::new());
            parts.insert(0, body);
            let boundary = ContentType::make_boundary(&parts);
            body = Attachment::new(
                ContentType::Multipart {
                    boundary: boundary.into_bytes(),
                    kind: MultipartType::Mixed,
                    parts: parts.into_iter().map(|a| a.into()).collect::<Vec<_>>(),
                },
                Default::default(),
                Vec::new(),
            )
            .into();
        }
    let output = todo!();
    crate::mail::pgp::sign(
        body.into(),
        account_settings!(context[account_hash].pgp.gpg_binary)
            .as_ref()
            .map(|s| s.as_str()),
        account_settings!(context[account_hash].pgp.sign_key)
            .as_ref()
            .map(|s| s.as_str()),
    );
    match output {
        Err(err) => {
            log::error!(
                    "Could not sign draft in account `{}`: {err}.",
                    context.accounts[&account_hash].name(),
            );
            context.replies.push_back(UIEvent::Notification(
                Some(format!(
                    "Could not sign draft in account `{}`.",
                    context.accounts[&account_hash].name()
                )),
                err.to_string(),
                Some(NotificationType::Error(err.kind)),
            ));
            return Err(err);
        }
        Ok(output) => {
            draft.attachments.push(output);
        }
    }
    } else {
    */
    {
        let mut content_type = ContentType::default();
        if format_flowed {
            if let ContentType::Text {
                ref mut parameters, ..
            } = content_type
            {
                parameters.push((b"format".to_vec(), b"flowed".to_vec()));
            }

            let body: AttachmentBuilder = Attachment::new(
                content_type,
                Default::default(),
                std::mem::take(&mut draft.body).into_bytes(),
            )
            .into();
            draft.attachments.insert(0, body);
        }
    }
    let bytes = draft.finalise().unwrap();
    let send_mail = account_settings!(context[account_hash].send_mail).clone();
    let ret =
        context.accounts[&account_hash].send(bytes.clone(), send_mail, complete_in_background);
    save_draft(bytes.as_bytes(), context, mailbox_type, flags, account_hash);
    ret
}

pub fn save_draft(
    bytes: &[u8],
    context: &mut Context,
    mailbox_type: SpecialUsageMailbox,
    flags: Flag,
    account_hash: AccountHash,
) {
    match context.accounts[&account_hash].save_special(bytes, mailbox_type, flags) {
        Err(Error {
            summary,
            details,
            kind,
            ..
        }) => {
            context.replies.push_back(UIEvent::Notification {
                title: details,
                source: None,
                body: summary,
                kind: Some(NotificationType::Error(kind)),
            });
        }
        Ok(mailbox_hash) => {
            context.replies.push_back(UIEvent::Notification {
                title: Some("Message saved".into()),
                source: None,
                body: format!(
                    "Message saved in `{}`",
                    &context.accounts[&account_hash].mailbox_entries[&mailbox_hash].name
                )
                .into(),
                kind: Some(NotificationType::Info),
            });
        }
    }
}

pub fn send_draft_async(
    #[cfg(feature = "gpgme")] gpg_state: gpg::GpgComposeState,
    context: &Context,
    account_hash: AccountHash,
    mut draft: Draft,
    mailbox_type: SpecialUsageMailbox,
    flags: Flag,
) -> Result<Pin<Box<dyn Future<Output = Result<()>> + Send>>> {
    let store_sent_mail = *account_settings!(context[account_hash].composing.store_sent_mail);
    let format_flowed = *account_settings!(context[account_hash].composing.format_flowed);
    let event_sender = context.main_loop_handler.sender.clone();
    #[cfg(feature = "gpgme")]
    let mut filters_stack: Vec<AttachmentFilterBox> = vec![];
    #[cfg(feature = "gpgme")]
    if gpg_state.sign_mail.unwrap_or(ActionFlag::False).is_true()
        && !gpg_state
            .encrypt_mail
            .unwrap_or(ActionFlag::False)
            .is_true()
    {
        filters_stack.push(Box::new(crate::mail::pgp::sign_filter(
            (account_settings!(context[account_hash].pgp.auto_sign).is_true()
                && gpg_state.sign_keys.is_empty())
            .then(|| account_settings!(context[account_hash].pgp.sign_key).clone())
            .flatten(),
            gpg_state.sign_keys,
        )?));
    } else if gpg_state
        .encrypt_mail
        .unwrap_or(ActionFlag::False)
        .is_true()
    {
        filters_stack.push(Box::new(crate::mail::pgp::encrypt_filter(
            gpg_state.encrypt_for_self.then_some(()).map_or_else(
                || Ok(None),
                |()| {
                    draft.headers().get(HeaderName::FROM).map_or_else(
                        || Ok(None),
                        |s| Some(melib::Address::try_from(s)).transpose(),
                    )
                },
            )?,
            (gpg_state.sign_mail.unwrap_or(ActionFlag::False).is_true()
                && gpg_state.sign_keys.is_empty())
            .then(|| account_settings!(context[account_hash].pgp.sign_key).clone())
            .flatten(),
            gpg_state
                .sign_mail
                .unwrap_or(ActionFlag::False)
                .is_true()
                .then(|| gpg_state.sign_keys.clone()),
            gpg_state
                .encrypt_keys
                .is_empty()
                .then(|| account_settings!(context[account_hash].pgp.encrypt_key).clone())
                .flatten(),
            gpg_state.encrypt_keys,
        )?));
    }
    let send_mail = account_settings!(context[account_hash].send_mail).clone();
    let send_cb = context.accounts[&account_hash].send_async(send_mail);
    let mut content_type = ContentType::default();
    if let (
        true,
        ContentType::Text {
            ref mut parameters, ..
        },
    ) = (format_flowed, &mut content_type)
    {
        parameters.push((b"format".to_vec(), b"flowed".to_vec()));
    }
    let mut body: AttachmentBuilder = Attachment::new(
        content_type,
        Default::default(),
        std::mem::take(&mut draft.body).into_bytes(),
    )
    .into();
    if !draft.attachments.is_empty() {
        let mut parts = std::mem::take(&mut draft.attachments);
        parts.insert(0, body);
        let boundary = ContentType::make_boundary(&parts);
        body = Attachment::new(
            ContentType::Multipart {
                boundary: boundary.into_bytes(),
                kind: MultipartType::Mixed,
                parts: parts.into_iter().map(|a| a.into()).collect::<Vec<_>>(),
                parameters: vec![],
            },
            Default::default(),
            vec![],
        )
        .into();
    }
    Ok(Box::pin(async move {
        #[cfg(feature = "gpgme")]
        for f in filters_stack {
            body = f(body).await?;
        }

        draft.attachments.insert(0, body);
        let message = Arc::new(draft.finalise()?);
        let ret = send_cb(message.clone()).await;
        let is_ok = ret.is_ok();
        if !is_ok || store_sent_mail {
            event_sender
                .send(ThreadEvent::UIEvent(UIEvent::Callback(CallbackFn(
                    Box::new(move |context| {
                        save_draft(
                            message.as_bytes(),
                            context,
                            if is_ok {
                                mailbox_type
                            } else {
                                SpecialUsageMailbox::Drafts
                            },
                            if is_ok {
                                flags
                            } else {
                                Flag::SEEN | Flag::DRAFT
                            },
                            account_hash,
                        );
                    }),
                ))))
                .unwrap();
        } else if !store_sent_mail && is_ok {
            let f = File::create_temp_file(message.as_bytes(), None, None, Some("eml"), false)?;
            log::info!(
                "store_sent_mail is false; stored sent mail to {}",
                f.path().display()
            );
        }
        ret
    }))
}

/* Sender details
 * %+f — the sender's name and email address.
 * %+n — the sender's name (or email address, if no name is included).
 * %+a — the sender's email address.
 */
fn attribution_string(
    fmt: Option<&str>,
    sender: Option<&Address>,
    date: UnixTimestamp,
    posix: bool,
) -> String {
    let fmt = fmt.unwrap_or("On %a, %0e %b %Y %H:%M, %+f wrote:%n");
    let fmt = fmt.replace(
        "%+f",
        &sender
            .map(|addr| addr.to_string())
            .unwrap_or_else(|| "\"\"".to_string()),
    );
    let fmt = fmt.replace(
        "%+n",
        &sender
            .map(|addr| addr.get_display_name().unwrap_or_else(|| addr.get_email()))
            .unwrap_or_else(|| "\"\"".to_string()),
    );
    let fmt = fmt.replace(
        "%+a",
        &sender
            .map(|addr| addr.get_email())
            .unwrap_or_else(|| "\"\"".to_string()),
    );
    melib::utils::datetime::timestamp_to_string(date, Some(fmt.as_str()), posix)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compose_reply_subject_prefix() {
        let raw_mail = r#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Subject: RE: your e-mail
Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
Content-Type: text/plain

hello world.
"#;

        let envelope =
            Envelope::from_bytes(raw_mail.as_bytes(), None).expect("Could not parse mail");
        let tempdir = tempfile::tempdir().unwrap();
        let context = Context::new_mock(&tempdir);
        let account_hash = context.accounts[0].hash();
        let mailbox_hash = MailboxHash::default();
        let envelope_hash = envelope.hash();
        context.accounts[0]
            .collection
            .insert(envelope, mailbox_hash);
        let composer = Composer::reply_to(
            (account_hash, mailbox_hash, envelope_hash),
            String::new(),
            &context,
            false,
        );
        assert_eq!(
            &composer.draft.headers()[HeaderName::SUBJECT],
            "RE: your e-mail"
        );
        assert_eq!(
            &composer.draft.headers()[HeaderName::TO],
            r#"some name <some@example.com>"#
        );
        let raw_mail = r#"From: "some name" <some@example.com>
To: "me" <myself@example.com>
Cc:
Subject: your e-mail
Message-ID: <h2g7f.z0gy2pgaen5m@example.com>
Content-Type: text/plain

hello world.
"#;
        let envelope =
            Envelope::from_bytes(raw_mail.as_bytes(), None).expect("Could not parse mail");
        let envelope_hash = envelope.hash();
        context.accounts[0]
            .collection
            .insert(envelope, mailbox_hash);
        let composer = Composer::reply_to(
            (account_hash, mailbox_hash, envelope_hash),
            String::new(),
            &context,
            false,
        );
        assert_eq!(
            &composer.draft.headers()[HeaderName::SUBJECT],
            "Re: your e-mail"
        );
        assert_eq!(
            &composer.draft.headers()[HeaderName::TO],
            r#"some name <some@example.com>"#
        );
    }
}
