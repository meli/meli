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

use super::*;
use melib::email::attachment_types::{ContentType, MultipartType};
use melib::list_management;
use melib::Draft;

use crate::conf::accounts::JobRequest;
use crate::jobs::JoinHandle;
use crate::terminal::embed::EmbedTerminal;
use indexmap::IndexSet;
use nix::sys::wait::WaitStatus;
use std::convert::TryInto;
use std::future::Future;
use std::pin::Pin;
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex};

#[cfg(feature = "gpgme")]
mod gpg;

mod edit_attachments;
use edit_attachments::*;

#[derive(Debug, PartialEq, Eq)]
enum Cursor {
    Headers,
    Body,
    Sign,
    Encrypt,
    Attachments,
}

#[derive(Debug)]
enum EmbedStatus {
    Stopped(Arc<Mutex<EmbedTerminal>>, File),
    Running(Arc<Mutex<EmbedTerminal>>, File),
}

impl EmbedStatus {
    #[inline(always)]
    fn is_stopped(&self) -> bool {
        matches!(self, Self::Stopped(_, _))
    }
}

impl std::ops::Deref for EmbedStatus {
    type Target = Arc<Mutex<EmbedTerminal>>;
    fn deref(&self) -> &Arc<Mutex<EmbedTerminal>> {
        use EmbedStatus::*;
        match self {
            Stopped(ref e, _) | Running(ref e, _) => e,
        }
    }
}

impl std::ops::DerefMut for EmbedStatus {
    fn deref_mut(&mut self) -> &mut Arc<Mutex<EmbedTerminal>> {
        use EmbedStatus::*;
        match self {
            Stopped(ref mut e, _) | Running(ref mut e, _) => e,
        }
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

    embed_area: Area,
    embed: Option<EmbedStatus>,
    #[cfg(feature = "gpgme")]
    gpg_state: gpg::GpgComposeState,
    dirty: bool,
    has_changes: bool,
    initialized: bool,
    id: ComponentId,
}

#[derive(Debug)]
enum ViewMode {
    Discard(Uuid, UIDialog<char>),
    EditAttachments {
        widget: EditAttachments,
    },
    Edit,
    Embed,
    SelectRecipients(UIDialog<Address>),
    #[cfg(feature = "gpgme")]
    SelectEncryptKey(bool, gpg::KeySelection),
    Send(UIConfirmationDialog),
    WaitingForSendResult(UIDialog<char>, JoinHandle<Result<()>>),
}

impl ViewMode {
    fn is_edit(&self) -> bool {
        matches!(self, ViewMode::Edit)
    }

    fn is_edit_attachments(&self) -> bool {
        matches!(self, ViewMode::EditAttachments { .. })
    }
}

impl fmt::Display for Composer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.reply_context.is_some() {
            write!(
                f,
                "reply: {}",
                (&self.draft.headers()["Subject"]).trim_at_boundary(8)
            )
        } else {
            write!(f, "composing")
        }
    }
}

impl Composer {
    pub fn new(context: &Context) -> Self {
        let mut pager = Pager::new(context);
        pager.set_show_scrollbar(true);
        Composer {
            reply_context: None,
            account_hash: 0,
            cursor: Cursor::Headers,
            pager,
            draft: Draft::default(),
            form: FormWidget::default(),
            mode: ViewMode::Edit,
            #[cfg(feature = "gpgme")]
            gpg_state: gpg::GpgComposeState::default(),
            dirty: true,
            has_changes: false,
            embed_area: ((0, 0), (0, 0)),
            embed: None,
            initialized: false,
            id: ComponentId::new_v4(),
        }
    }

    pub fn with_account(account_hash: AccountHash, context: &Context) -> Self {
        let mut ret = Composer {
            account_hash,
            ..Composer::new(context)
        };
        for (h, v) in
            account_settings!(context[account_hash].composing.default_header_values).iter()
        {
            if v.is_empty() {
                continue;
            }
            ret.draft.set_header(h, v.into());
        }
        if *account_settings!(context[account_hash].composing.insert_user_agent) {
            ret.draft.set_header(
                "User-Agent",
                format!("meli {}", option_env!("CARGO_PKG_VERSION").unwrap_or("0.0")),
            );
        }
        if *account_settings!(context[account_hash].composing.format_flowed) {
            ret.pager
                .set_reflow(melib::text_processing::Reflow::FormatFlowed);
        }
        ret
    }

    pub fn edit(
        account_hash: AccountHash,
        env_hash: EnvelopeHash,
        bytes: &[u8],
        context: &Context,
    ) -> Result<Self> {
        let mut ret = Composer::with_account(account_hash, context);
        let envelope: EnvelopeRef = context.accounts[&account_hash].collection.get_env(env_hash);

        ret.draft = Draft::edit(&envelope, bytes)?;

        ret.account_hash = account_hash;
        Ok(ret)
    }

    pub fn reply_to(
        coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        reply_body: String,
        context: &mut Context,
        reply_to_all: bool,
    ) -> Self {
        let mut ret = Composer::with_account(coordinates.0, context);
        let account = &context.accounts[&coordinates.0];
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
        ret.draft.set_header("Subject", subject);
        ret.draft.set_header(
            "References",
            format!(
                "{} {}",
                envelope
                    .references()
                    .iter()
                    .fold(String::new(), |mut acc, x| {
                        if !acc.is_empty() {
                            acc.push(' ');
                        }
                        acc.push_str(&x.to_string());
                        acc
                    }),
                envelope.message_id_display()
            ),
        );
        ret.draft
            .set_header("In-Reply-To", envelope.message_id_display().into());

        if let Some(reply_to) = envelope.other_headers().get("To").map(|v| v.as_str()) {
            let to: &str = reply_to;
            let extra_identities = &account.settings.account.extra_identities;
            if let Some(extra) = extra_identities
                .iter()
                .find(|extra| to.contains(extra.as_str()))
            {
                ret.draft.set_header("From", extra.into());
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
                            to.insert(list_address);
                        }
                    }
                }
            }
            if let Some(reply_to) = envelope
                .other_headers()
                .get("Mail-Followup-To")
                .and_then(|v| v.as_str().try_into().ok())
            {
                to.insert(reply_to);
            } else if let Some(reply_to) = envelope
                .other_headers()
                .get("Reply-To")
                .and_then(|v| v.as_str().try_into().ok())
            {
                to.insert(reply_to);
            } else {
                to.extend(envelope.from().iter().cloned());
            }
            to.extend(envelope.to().iter().cloned());
            if let Ok(ours) = TryInto::<Address>::try_into(
                crate::components::mail::get_display_name(context, coordinates.0).as_str(),
            ) {
                to.remove(&ours);
            }
            ret.draft.set_header("To", {
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
            ret.draft.set_header("Cc", envelope.field_cc_to_string());
        } else if let Some(reply_to) = envelope.other_headers().get("Mail-Reply-To") {
            ret.draft.set_header("To", reply_to.to_string());
        } else if let Some(reply_to) = envelope.other_headers().get("Reply-To") {
            ret.draft.set_header("To", reply_to.to_string());
        } else {
            ret.draft.set_header("To", envelope.field_from_to_string());
        }
        ret.draft.body = {
            let mut ret = attribution_string(
                account_settings!(
                    context[ret.account_hash]
                        .composing
                        .attribution_format_string
                )
                .as_ref()
                .map(|s| s.as_str()),
                envelope.from().get(0),
                envelope.date(),
                *account_settings!(
                    context[ret.account_hash]
                        .composing
                        .attribution_use_posix_locale
                ),
            );
            for l in reply_body.lines() {
                ret.push('>');
                ret.push_str(l);
                ret.push('\n');
            }
            ret
        };

        ret.account_hash = coordinates.0;
        ret.reply_context = Some((coordinates.1, coordinates.2));
        ret
    }

    pub fn reply_to_select(
        coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        reply_body: String,
        context: &mut Context,
    ) -> Self {
        let mut ret = Composer::reply_to(coordinates, reply_body, context, false);
        let account = &context.accounts[&coordinates.0];
        let parent_message = account.collection.get_env(coordinates.2);
        /* If message is from a mailing list and we detect a List-Post header, ask user if they
         * want to reply to the mailing list or the submitter of the message */
        if let Some(actions) = list_management::ListActions::detect(&parent_message) {
            if let Some(post) = actions.post {
                if let list_management::ListAction::Email(list_post_addr) = post[0] {
                    if let Ok(list_address) = melib::email::parser::generic::mailto(list_post_addr)
                        .map(|(_, m)| m.address)
                    {
                        let list_address_string = list_address.to_string();
                        ret.mode = ViewMode::SelectRecipients(UIDialog::new(
                            "select recipients",
                            vec![
                                (
                                    parent_message.from()[0].clone(),
                                    parent_message.field_from_to_string(),
                                ),
                                (list_address, list_address_string),
                            ],
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
        context: &mut Context,
    ) -> Self {
        Composer::reply_to(coordinates, reply_body, context, false)
    }

    pub fn reply_to_all(
        coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        reply_body: String,
        context: &mut Context,
    ) -> Self {
        Composer::reply_to(coordinates, reply_body, context, true)
    }

    pub fn forward(
        coordinates: (AccountHash, MailboxHash, EnvelopeHash),
        bytes: &[u8],
        env: &Envelope,
        as_attachment: bool,
        context: &mut Context,
    ) -> Self {
        let mut composer = Composer::with_account(coordinates.0, context);
        let mut draft: Draft = Draft::default();
        draft.set_header("Subject", format!("Fwd: {}", env.subject()));
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
                disposition.filename = Some(format!("{}.eml", env.message_id_raw()));
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
        composer.set_draft(draft);
        composer
    }

    pub fn set_draft(&mut self, draft: Draft) {
        self.draft = draft;
        self.update_form();
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

    fn update_form(&mut self) {
        let old_cursor = self.form.cursor();
        self.form = FormWidget::new(("Save".into(), true));
        self.form.hide_buttons();
        self.form.set_cursor(old_cursor);
        let headers = self.draft.headers();
        let account_hash = self.account_hash;
        for &k in &["Date", "From", "To", "Cc", "Bcc", "Subject"] {
            if k == "To" || k == "Cc" || k == "Bcc" {
                self.form.push_cl((
                    k.into(),
                    headers[k].to_string(),
                    Box::new(move |c, term| {
                        let book: &AddressBook = &c.accounts[&account_hash].address_book;
                        let results: Vec<String> = book.search(term);
                        results
                            .into_iter()
                            .map(AutoCompleteEntry::from)
                            .collect::<Vec<AutoCompleteEntry>>()
                    }),
                ));
            } else if k == "From" {
                self.form.push_cl((
                    k.into(),
                    headers[k].to_string(),
                    Box::new(move |c, _term| {
                        c.accounts
                            .values()
                            .map(|acc| {
                                let addr = if let Some(display_name) =
                                    acc.settings.account.display_name()
                                {
                                    format!(
                                        "{} <{}>",
                                        display_name,
                                        acc.settings.account.identity()
                                    )
                                } else {
                                    acc.settings.account.identity().to_string()
                                };
                                let desc =
                                    match account_settings!(c[acc.hash()].composing.send_mail) {
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

                                (addr, desc)
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
        clear_area(grid, area, theme_default);
        #[cfg(feature = "gpgme")]
        if self.gpg_state.sign_mail.is_true() {
            let key_list = self
                .gpg_state
                .sign_keys
                .iter()
                .map(|k| k.fingerprint())
                .collect::<Vec<_>>()
                .join(", ");
            write_string_to_grid(
                &format!(
                    "☑ sign with {}",
                    if self.gpg_state.sign_keys.is_empty() {
                        "default key"
                    } else {
                        key_list.as_str()
                    }
                ),
                grid,
                theme_default.fg,
                if self.cursor == Cursor::Sign {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                (pos_inc(upper_left!(area), (0, 1)), bottom_right!(area)),
                None,
            );
        } else {
            write_string_to_grid(
                "☐ don't sign",
                grid,
                theme_default.fg,
                if self.cursor == Cursor::Sign {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                (pos_inc(upper_left!(area), (0, 1)), bottom_right!(area)),
                None,
            );
        }
        #[cfg(feature = "gpgme")]
        if self.gpg_state.encrypt_mail.is_true() {
            let key_list = self
                .gpg_state
                .encrypt_keys
                .iter()
                .map(|k| k.fingerprint())
                .collect::<Vec<_>>()
                .join(", ");

            write_string_to_grid(
                &format!(
                    "{}{}",
                    if self.gpg_state.encrypt_keys.is_empty() {
                        "☐ no keys to encrypt with!"
                    } else {
                        "☑ encrypt with "
                    },
                    if self.gpg_state.encrypt_keys.is_empty() {
                        ""
                    } else {
                        key_list.as_str()
                    }
                ),
                grid,
                theme_default.fg,
                if self.cursor == Cursor::Encrypt {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                (pos_inc(upper_left!(area), (0, 2)), bottom_right!(area)),
                None,
            );
        } else {
            write_string_to_grid(
                "☐ don't encrypt",
                grid,
                theme_default.fg,
                if self.cursor == Cursor::Encrypt {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                (pos_inc(upper_left!(area), (0, 2)), bottom_right!(area)),
                None,
            );
        }
        if attachments_no == 0 {
            write_string_to_grid(
                "no attachments",
                grid,
                theme_default.fg,
                if self.cursor == Cursor::Attachments {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                (pos_inc(upper_left!(area), (0, 3)), bottom_right!(area)),
                None,
            );
        } else {
            write_string_to_grid(
                &format!("{} attachments ", attachments_no),
                grid,
                theme_default.fg,
                if self.cursor == Cursor::Attachments {
                    crate::conf::value(context, "highlight").bg
                } else {
                    theme_default.bg
                },
                theme_default.attrs,
                (pos_inc(upper_left!(area), (0, 3)), bottom_right!(area)),
                None,
            );
            for (i, a) in self.draft.attachments().iter().enumerate() {
                if let Some(name) = a.content_type().name() {
                    write_string_to_grid(
                        &format!(
                            "[{}] \"{}\", {} {}",
                            i,
                            name,
                            a.content_type(),
                            melib::Bytes(a.raw.len())
                        ),
                        grid,
                        theme_default.fg,
                        theme_default.bg,
                        theme_default.attrs,
                        (pos_inc(upper_left!(area), (0, 4 + i)), bottom_right!(area)),
                        None,
                    );
                } else {
                    write_string_to_grid(
                        &format!("[{}] {} {}", i, a.content_type(), melib::Bytes(a.raw.len())),
                        grid,
                        theme_default.fg,
                        theme_default.bg,
                        theme_default.attrs,
                        (pos_inc(upper_left!(area), (0, 4 + i)), bottom_right!(area)),
                        None,
                    );
                }
            }
        }
    }

    fn update_from_file(&mut self, file: File, context: &mut Context) -> bool {
        let result = file.read_to_string();
        match self.draft.update(result.as_str()) {
            Ok(has_changes) => {
                self.has_changes = has_changes;
                true
            }
            Err(err) => {
                context.replies.push_back(UIEvent::Notification(
                    Some("Could not parse draft headers correctly.".to_string()),
                    format!(
                        "{}\nThe invalid text has been set as the body of your draft",
                        &err
                    ),
                    Some(NotificationType::Error(melib::error::ErrorKind::None)),
                ));
                self.draft.set_body(result);
                self.has_changes = true;
                false
            }
        }
    }
}

impl Component for Composer {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        let upper_left = upper_left!(area);
        let bottom_right = bottom_right!(area);

        let upper_left = set_y(upper_left, get_y(upper_left) + 1);

        if height!(area) < 4 {
            return;
        }

        let width = width!(area);

        if !self.initialized {
            #[cfg(feature = "gpgme")]
            if self.gpg_state.sign_mail.is_unset() {
                self.gpg_state.sign_mail = ToggleFlag::InternalVal(*account_settings!(
                    context[self.account_hash].pgp.auto_sign
                ));
            }
            if !self.draft.headers().contains_key("From") || self.draft.headers()["From"].is_empty()
            {
                self.draft.set_header(
                    "From",
                    crate::components::mail::get_display_name(context, self.account_hash),
                );
            }
            self.pager.update_from_str(self.draft.body(), Some(77));
            self.update_form();
            self.initialized = true;
        }
        let header_height = self.form.len();
        let theme_default = crate::conf::value(context, "theme_default");

        let mid = if width > 80 {
            let width = width - 80;
            let mid = width / 2;

            if self.dirty {
                for i in get_y(upper_left)..=get_y(bottom_right) {
                    //set_and_join_box(grid, (mid, i), VERT_BOUNDARY);
                    grid[(mid, i)]
                        .set_fg(theme_default.fg)
                        .set_bg(theme_default.bg);
                    //set_and_join_box(grid, (mid + 80, i), VERT_BOUNDARY);
                    grid[(mid + 80, i)]
                        .set_fg(theme_default.fg)
                        .set_bg(theme_default.bg);
                }
            }
            mid
        } else {
            0
        };

        let header_area = (
            set_x(upper_left, mid + 1),
            (
                get_x(bottom_right).saturating_sub(mid),
                get_y(upper_left) + header_height,
            ),
        );
        let attachments_no = self.draft.attachments().len();
        let attachment_area = (
            (
                mid + 1,
                get_y(bottom_right).saturating_sub(4 + attachments_no),
            ),
            pos_dec(bottom_right, (mid, 0)),
        );

        let body_area = (
            (
                get_x(upper_left!(header_area)),
                get_y(bottom_right!(header_area)) + 1,
            ),
            (
                get_x(bottom_right!(header_area)),
                get_y(upper_left!(attachment_area)) - 1,
            ),
        );

        let (x, y) = write_string_to_grid(
            if self.reply_context.is_some() {
                "COMPOSING REPLY"
            } else {
                "COMPOSING MESSAGE"
            },
            grid,
            crate::conf::value(context, "highlight").fg,
            crate::conf::value(context, "highlight").bg,
            crate::conf::value(context, "highlight").attrs,
            (
                pos_dec(upper_left!(header_area), (0, 1)),
                bottom_right!(header_area),
            ),
            None,
        );
        clear_area(grid, ((x, y), (set_y(bottom_right, y))), theme_default);
        change_colors(
            grid,
            (
                set_x(pos_dec(upper_left!(header_area), (0, 1)), x),
                set_y(bottom_right!(header_area), y),
            ),
            crate::conf::value(context, "highlight").fg,
            crate::conf::value(context, "highlight").bg,
        );
        clear_area(
            grid,
            (
                pos_dec(upper_left, (0, 1)),
                set_x(bottom_right, get_x(upper_left) + mid),
            ),
            theme_default,
        );
        clear_area(
            grid,
            (
                (
                    get_x(bottom_right).saturating_sub(mid),
                    get_y(upper_left) - 1,
                ),
                bottom_right,
            ),
            theme_default,
        );

        /* Regardless of view mode, do the following */
        self.form.draw(grid, header_area, context);
        if let Some(ref mut embed_pty) = self.embed {
            let embed_area = (upper_left!(header_area), bottom_right!(body_area));
            match embed_pty {
                EmbedStatus::Running(_, _) => {
                    let mut guard = embed_pty.lock().unwrap();
                    clear_area(grid, embed_area, theme_default);
                    copy_area(
                        grid,
                        guard.grid.buffer(),
                        embed_area,
                        ((0, 0), pos_dec(guard.grid.terminal_size, (1, 1))),
                    );
                    guard.set_terminal_size((width!(embed_area), height!(embed_area)));
                    context.dirty_areas.push_back(area);
                    self.dirty = false;
                    return;
                }
                EmbedStatus::Stopped(_, _) => {
                    let guard = embed_pty.lock().unwrap();
                    copy_area(
                        grid,
                        guard.grid.buffer(),
                        embed_area,
                        ((0, 0), pos_dec(guard.grid.terminal_size, (1, 1))),
                    );
                    change_colors(grid, embed_area, Color::Byte(8), theme_default.bg);
                    let our_map: ShortcutMap =
                        account_settings!(context[self.account_hash].shortcuts.composing)
                            .key_values();
                    let mut shortcuts: ShortcutMaps = Default::default();
                    shortcuts.insert(Shortcuts::COMPOSING, our_map);
                    let stopped_message: String =
                        format!("Process with PID {} has stopped.", guard.child_pid);
                    let stopped_message_2: String = format!(
                        "-press '{}' (edit_mail shortcut) to re-activate.",
                        shortcuts[Shortcuts::COMPOSING]["edit_mail"]
                    );
                    const STOPPED_MESSAGE_3: &str =
                        "-press Ctrl-C to forcefully kill it and return to editor.";
                    let max_len = std::cmp::max(
                        stopped_message.len(),
                        std::cmp::max(stopped_message_2.len(), STOPPED_MESSAGE_3.len()),
                    );
                    let inner_area = create_box(
                        grid,
                        (
                            pos_inc(upper_left!(body_area), (1, 0)),
                            pos_inc(
                                upper_left!(body_area),
                                (
                                    std::cmp::min(max_len + 5, width!(body_area)),
                                    std::cmp::min(5, height!(body_area)),
                                ),
                            ),
                        ),
                    );
                    clear_area(grid, inner_area, theme_default);
                    for (i, l) in [
                        stopped_message.as_str(),
                        stopped_message_2.as_str(),
                        STOPPED_MESSAGE_3,
                    ]
                    .iter()
                    .enumerate()
                    {
                        write_string_to_grid(
                            l,
                            grid,
                            theme_default.fg,
                            theme_default.bg,
                            theme_default.attrs,
                            (
                                pos_inc((0, i), upper_left!(inner_area)),
                                bottom_right!(inner_area),
                            ),
                            Some(get_x(upper_left!(inner_area))),
                        );
                    }
                    context.dirty_areas.push_back(area);
                    self.dirty = false;
                    return;
                }
            }
        } else {
            self.embed_area = (upper_left!(header_area), bottom_right!(body_area));
        }

        if !self.mode.is_edit_attachments() {
            self.pager.set_dirty(true);
            if self.pager.size().0 > width!(body_area) {
                self.pager.set_initialised(false);
            }
            self.pager.draw(grid, body_area, context);
        }

        match self.cursor {
            Cursor::Headers => {
                change_colors(
                    grid,
                    (
                        pos_dec(upper_left!(body_area), (1, 0)),
                        pos_dec(
                            set_y(upper_left!(body_area), get_y(bottom_right!(body_area))),
                            (1, 0),
                        ),
                    ),
                    theme_default.fg,
                    theme_default.bg,
                );
            }
            Cursor::Body => {
                change_colors(
                    grid,
                    (
                        pos_dec(upper_left!(body_area), (1, 0)),
                        pos_dec(
                            set_y(upper_left!(body_area), get_y(bottom_right!(body_area))),
                            (1, 0),
                        ),
                    ),
                    theme_default.fg,
                    crate::conf::value(context, "highlight").bg,
                );
            }
            Cursor::Sign | Cursor::Encrypt | Cursor::Attachments => {}
        }

        match self.mode {
            ViewMode::Edit | ViewMode::Embed => {}
            ViewMode::EditAttachments { ref mut widget } => {
                let inner_area = create_box(
                    grid,
                    (upper_left!(body_area), bottom_right!(attachment_area)),
                );
                (EditAttachmentsRefMut {
                    inner: widget,
                    draft: &mut self.draft,
                })
                .draw(
                    grid,
                    (
                        pos_inc(upper_left!(inner_area), (1, 1)),
                        bottom_right!(inner_area),
                    ),
                    context,
                );
            }
            ViewMode::Send(ref mut s) => {
                s.draw(grid, area, context);
            }
            #[cfg(feature = "gpgme")]
            ViewMode::SelectEncryptKey(
                _,
                gpg::KeySelection::Loaded {
                    ref mut widget,
                    keys: _,
                },
            ) => {
                widget.draw(grid, area, context);
            }
            #[cfg(feature = "gpgme")]
            ViewMode::SelectEncryptKey(_, _) => {}
            ViewMode::SelectRecipients(ref mut s) => {
                s.draw(grid, area, context);
            }
            ViewMode::Discard(_, ref mut s) => {
                /* Let user choose whether to quit with/without saving or cancel */
                s.draw(grid, area, context);
            }
            ViewMode::WaitingForSendResult(ref mut s, _) => {
                /* Let user choose whether to wait for success or cancel */
                s.draw(grid, area, context);
            }
        }
        if !self.mode.is_edit_attachments() {
            self.draw_attachments(grid, attachment_area, context);
        }
        self.dirty = false;
        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, mut event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::VisibilityChange(_) = event {
            self.pager.process_event(event, context);
        }
        let shortcuts = self.get_shortcuts(context);
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
                    if widget.buttons.result() == Some(FormButtonActions::Cancel) {
                        self.mode = ViewMode::Edit;
                        self.set_dirty(true);
                    }
                    return true;
                }
            }
            (ViewMode::Send(ref selector), UIEvent::FinishedUIDialog(id, result))
                if selector.id() == *id =>
            {
                if let Some(true) = result.downcast_ref::<bool>() {
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
                            let handle = context.job_executor.spawn_blocking(job);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::NewJob(
                                    handle.job_id,
                                )));
                            self.mode = ViewMode::WaitingForSendResult(
                                UIDialog::new(
                                    "Waiting for confirmation.. The tab will close automatically on successful submission.",
                                    vec![
                                    ('c', "force close tab".to_string()),
                                    ('n', "close this message and return to edit mode".to_string()),
                                    ],
                                    true,
                                    Some(Box::new(move |id: ComponentId, results: &[char]| {
                                        Some(UIEvent::FinishedUIDialog(
                                                id,
                                                Box::new(results.first().cloned().unwrap_or('c')),
                                        ))
                                    })),
                                    context,
                                ), handle);
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                None,
                                err.to_string(),
                                Some(NotificationType::Error(err.kind)),
                            ));
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
            (ViewMode::Send(ref dialog), UIEvent::ComponentKill(ref id)) if *id == dialog.id() => {
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
            }
            (ViewMode::SelectRecipients(ref dialog), UIEvent::ComponentKill(ref id))
                if *id == dialog.id() =>
            {
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
            }
            (ViewMode::Discard(_, ref dialog), UIEvent::ComponentKill(ref id))
                if *id == dialog.id() =>
            {
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
            }
            #[cfg(feature = "gpgme")]
            (ViewMode::SelectEncryptKey(_, ref mut selector), UIEvent::ComponentKill(ref id))
                if *id == selector.id() =>
            {
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
                return true;
            }
            (ViewMode::Send(ref mut selector), _) => {
                if selector.process_event(event, context) {
                    return true;
                }
            }
            (
                ViewMode::SelectRecipients(ref selector),
                UIEvent::FinishedUIDialog(id, ref mut result),
            ) if selector.id() == *id => {
                if let Some(to_val) = result.downcast_mut::<String>() {
                    self.draft.set_header("To", std::mem::take(to_val));
                    self.update_form();
                }
                self.mode = ViewMode::Edit;
                return true;
            }
            (ViewMode::SelectRecipients(ref mut selector), _) => {
                if selector.process_event(event, context) {
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
                self.set_dirty(true);
                self.mode = ViewMode::Edit;
                return true;
            }
            (ViewMode::Discard(_, ref mut selector), _) => {
                if selector.process_event(event, context) {
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
                        MeliError::new("Job was canceled")
                    }) {
                    Err(err) | Ok(Some(Err(err))) => {
                        self.mode = ViewMode::Edit;
                        context.replies.push_back(UIEvent::Notification(
                            None,
                            err.to_string(),
                            Some(NotificationType::Error(err.kind)),
                        ));
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
                    return true;
                }
            }
            #[cfg(feature = "gpgme")]
            (
                ViewMode::SelectEncryptKey(is_encrypt, ref mut selector),
                UIEvent::FinishedUIDialog(id, result),
            ) if *id == selector.id() => {
                debug!(&result);
                if let Some(key) = result.downcast_mut::<Option<melib::gpgme::Key>>() {
                    debug!("got key {:?}", key);
                    if let Some(key) = key {
                        if *is_encrypt {
                            self.gpg_state.encrypt_keys.clear();
                            self.gpg_state.encrypt_keys.push(key.clone());
                        } else {
                            self.gpg_state.sign_keys.clear();
                            self.gpg_state.sign_keys.push(key.clone());
                        }
                    }
                }
                self.mode = ViewMode::Edit;
                self.set_dirty(true);
                return true;
            }
            #[cfg(feature = "gpgme")]
            (ViewMode::SelectEncryptKey(_, ref mut selector), _) => {
                if selector.process_event(event, context) {
                    return true;
                }
            }
            _ => {}
        }
        if self.cursor == Cursor::Headers
            && self.mode.is_edit()
            && self.form.process_event(event, context)
        {
            if let UIEvent::InsertInput(_) = event {
                self.has_changes = true;
            }
            return true;
        }

        match *event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.set_dirty(true);
            }
            UIEvent::Resize => {
                self.set_dirty(true);
            }
            /*
            /* Switch e-mail From: field to the `left` configured account. */
            UIEvent::Input(Key::Left) if self.cursor == Cursor::From => {
            self.draft.headers_mut().insert(
            "From".into(),
            get_display_name(context, self.account_hash),
            );
            self.dirty = true;
            return true;
            }
            /* Switch e-mail From: field to the `right` configured account. */
            UIEvent::Input(Key::Right) if self.cursor == Cursor::From => {
            if self.account_cursor + 1 < context.accounts.len() {
            self.account_cursor += 1;
            self.draft.headers_mut().insert(
            "From".into(),
            get_display_name(context, self.account_cursor),
            );
            self.dirty = true;
            }
            return true;
            }*/
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["scroll_up"]) =>
            {
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
                self.dirty = true;
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["scroll_down"]) =>
            {
                self.cursor = match self.cursor {
                    Cursor::Headers => Cursor::Body,
                    Cursor::Body => Cursor::Sign,
                    Cursor::Sign => Cursor::Encrypt,
                    Cursor::Encrypt => Cursor::Attachments,
                    Cursor::Attachments => return true,
                };
                self.dirty = true;
            }
            UIEvent::Input(Key::Char('\n'))
                if self.mode.is_edit()
                    && (self.cursor == Cursor::Sign || self.cursor == Cursor::Encrypt) =>
            {
                #[cfg(feature = "gpgme")]
                match self.cursor {
                    Cursor::Sign => {
                        let is_true = self.gpg_state.sign_mail.is_true();
                        self.gpg_state.sign_mail = ToggleFlag::from(!is_true);
                    }
                    Cursor::Encrypt => {
                        let is_true = self.gpg_state.encrypt_mail.is_true();
                        self.gpg_state.encrypt_mail = ToggleFlag::from(!is_true);
                    }
                    _ => {}
                };
                self.dirty = true;
            }
            UIEvent::Input(ref key)
                if shortcut!(key == shortcuts[Shortcuts::COMPOSING]["send_mail"])
                    && self.mode.is_edit() =>
            {
                self.update_draft();
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
            UIEvent::EmbedInput((Key::Ctrl('z'), _)) => {
                self.embed.as_ref().unwrap().lock().unwrap().stop();
                match self.embed.take() {
                    Some(EmbedStatus::Running(e, f)) | Some(EmbedStatus::Stopped(e, f)) => {
                        self.embed = Some(EmbedStatus::Stopped(e, f));
                    }
                    _ => {}
                }
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                self.set_dirty(true);
            }
            UIEvent::EmbedInput((ref k, ref b)) => {
                use std::io::Write;
                if let Some(ref mut embed) = self.embed {
                    let mut embed_guard = embed.lock().unwrap();
                    if embed_guard.write_all(b).is_err() {
                        match embed_guard.is_active() {
                            Ok(WaitStatus::Exited(_, exit_code)) => {
                                drop(embed_guard);
                                let embed = self.embed.take();
                                if exit_code != 0 {
                                    context.replies.push_back(UIEvent::Notification(
                                        None,
                                        format!(
                                            "Subprocess has exited with exit code {}",
                                            exit_code
                                        ),
                                        Some(NotificationType::Error(
                                            melib::error::ErrorKind::External,
                                        )),
                                    ));
                                } else if let Some(EmbedStatus::Running(_, file)) = embed {
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
                                drop(embed_guard);
                                match self.embed.take() {
                                    Some(EmbedStatus::Running(e, f))
                                    | Some(EmbedStatus::Stopped(e, f)) => {
                                        self.embed = Some(EmbedStatus::Stopped(e, f));
                                    }
                                    _ => {}
                                }
                                self.mode = ViewMode::Edit;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                                self.set_dirty(true);
                                return true;
                            }
                            Ok(WaitStatus::Stopped(_, _)) => {
                                drop(embed_guard);
                                match self.embed.take() {
                                    Some(EmbedStatus::Running(e, f))
                                    | Some(EmbedStatus::Stopped(e, f)) => {
                                        self.embed = Some(EmbedStatus::Stopped(e, f));
                                    }
                                    _ => {}
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
                                    .push_back(UIEvent::EmbedInput((k.clone(), b.to_vec())));
                                return true;
                            }
                            Ok(WaitStatus::Signaled(_, signal, _)) => {
                                drop(embed_guard);
                                context.replies.push_back(UIEvent::Notification(
                                    None,
                                    format!("Subprocess was killed by {} signal", signal),
                                    Some(NotificationType::Error(
                                        melib::error::ErrorKind::External,
                                    )),
                                ));
                                self.initialized = false;
                                self.embed = None;
                                self.mode = ViewMode::Edit;
                                context
                                    .replies
                                    .push_back(UIEvent::ChangeMode(UIMode::Normal));
                            }
                            Err(err) => {
                                context.replies.push_back(UIEvent::Notification(
                                    Some("Embed editor crashed.".to_string()),
                                    format!("Subprocess has exited with reason {}", &err),
                                    Some(NotificationType::Error(
                                        melib::error::ErrorKind::External,
                                    )),
                                ));
                                drop(embed_guard);
                                self.initialized = false;
                                self.embed = None;
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
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["edit_mail"]) =>
            {
                #[cfg(feature = "gpgme")]
                match melib::email::parser::address::rfc2822address_list(
                    self.form.values()["From"].as_str().as_bytes(),
                )
                .map_err(|_err| -> MeliError { "No valid sender address in `From:`".into() })
                .and_then(|(_, list)| {
                    list.get(0)
                        .cloned()
                        .ok_or_else(|| "No valid sender address in `From:`".into())
                })
                .and_then(|addr| {
                    gpg::KeySelection::new(
                        false,
                        account_settings!(context[self.account_hash].pgp.allow_remote_lookup)
                            .is_true(),
                        addr.get_email(),
                        *account_settings!(context[self.account_hash].pgp.allow_remote_lookup),
                        context,
                    )
                }) {
                    Ok(widget) => {
                        self.gpg_state.sign_mail = ToggleFlag::from(true);
                        self.mode = ViewMode::SelectEncryptKey(false, widget);
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not list keys.".to_string()),
                            format!("libgpgme error: {}", &err),
                            Some(NotificationType::Error(melib::error::ErrorKind::External)),
                        ));
                    }
                }
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && self.cursor == Cursor::Encrypt
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["edit_mail"]) =>
            {
                #[cfg(feature = "gpgme")]
                match melib::email::parser::address::rfc2822address_list(
                    self.form.values()["To"].as_str().as_bytes(),
                )
                .map_err(|_err| -> MeliError { "No valid recipient addresses in `To:`".into() })
                .and_then(|(_, list)| {
                    list.get(0)
                        .cloned()
                        .ok_or_else(|| "No valid recipient addresses in `To:`".into())
                })
                .and_then(|addr| {
                    gpg::KeySelection::new(
                        false,
                        account_settings!(context[self.account_hash].pgp.allow_remote_lookup)
                            .is_true(),
                        addr.get_email(),
                        *account_settings!(context[self.account_hash].pgp.allow_remote_lookup),
                        context,
                    )
                }) {
                    Ok(widget) => {
                        self.gpg_state.encrypt_mail = ToggleFlag::from(true);
                        self.mode = ViewMode::SelectEncryptKey(true, widget);
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not list keys.".to_string()),
                            format!("libgpgme error: {}", &err),
                            Some(NotificationType::Error(melib::error::ErrorKind::External)),
                        ));
                    }
                }
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(ref key)
                if self.mode.is_edit()
                    && self.cursor == Cursor::Attachments
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["edit_mail"]) =>
            {
                self.mode = ViewMode::EditAttachments {
                    widget: EditAttachments::new(),
                };
                self.set_dirty(true);

                return true;
            }
            UIEvent::Input(ref key)
                if self.embed.is_some()
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["edit_mail"]) =>
            {
                self.embed.as_ref().unwrap().lock().unwrap().wake_up();
                match self.embed.take() {
                    Some(EmbedStatus::Running(e, f)) | Some(EmbedStatus::Stopped(e, f)) => {
                        self.embed = Some(EmbedStatus::Running(e, f));
                    }
                    _ => {}
                }
                self.mode = ViewMode::Embed;
                context
                    .replies
                    .push_back(UIEvent::ChangeMode(UIMode::Embed));
                self.set_dirty(true);
                return true;
            }
            UIEvent::Input(Key::Ctrl('c'))
                if self.embed.is_some() && self.embed.as_ref().unwrap().is_stopped() =>
            {
                match self.embed.take() {
                    Some(EmbedStatus::Running(embed, file))
                    | Some(EmbedStatus::Stopped(embed, file)) => {
                        let guard = embed.lock().unwrap();
                        guard.wake_up();
                        guard.terminate();
                        self.update_from_file(file, context);
                    }
                    _ => {}
                }
                context.replies.push_back(UIEvent::Notification(
                    None,
                    "Subprocess was killed by SIGTERM signal".to_string(),
                    Some(NotificationType::Error(melib::error::ErrorKind::External)),
                ));
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
                    && shortcut!(key == shortcuts[Shortcuts::COMPOSING]["edit_mail"]) =>
            {
                /* Edit draft in $EDITOR */
                let editor = if let Some(editor_command) =
                    account_settings!(context[self.account_hash].composing.editor_command).as_ref()
                {
                    editor_command.to_string()
                } else {
                    match std::env::var("EDITOR") {
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                            Some(err.to_string()),
                            "$EDITOR is not set. You can change an envvar's value with setenv or set composing.editor_command setting in your configuration.".to_string(),
                            Some(NotificationType::Error(melib::error::ErrorKind::None)),
                        ));
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

                let f = create_temp_file(
                    self.draft.to_edit_string().as_str().as_bytes(),
                    None,
                    None,
                    true,
                );

                if *account_settings!(context[self.account_hash].composing.embed) {
                    match crate::terminal::embed::create_pty(
                        width!(self.embed_area),
                        height!(self.embed_area),
                        [editor, f.path().display().to_string()].join(" "),
                    ) {
                        Ok(embed) => {
                            self.embed = Some(EmbedStatus::Running(embed, f));
                            self.set_dirty(true);
                            context
                                .replies
                                .push_back(UIEvent::ChangeMode(UIMode::Embed));
                            context.replies.push_back(UIEvent::Fork(ForkType::Embed(
                                self.embed.as_ref().unwrap().lock().unwrap().child_pid,
                            )));
                            self.mode = ViewMode::Embed;
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                Some(format!("Failed to create pseudoterminal: {}", err)),
                                err.to_string(),
                                Some(NotificationType::Error(melib::error::ErrorKind::External)),
                            ));
                        }
                    }
                    return true;
                }
                /* Kill input thread so that spawned command can be sole receiver of stdin */
                {
                    context.input_kill();
                }

                let editor_command = format!("{} {}", editor, f.path().display());
                log(
                    format!(
                        "Executing: sh -c \"{}\"",
                        editor_command.replace('"', "\\\"")
                    ),
                    DEBUG,
                );
                match Command::new("sh")
                    .args(&["-c", &editor_command])
                    .stdin(Stdio::inherit())
                    .stdout(Stdio::inherit())
                    .spawn()
                {
                    Ok(mut child) => {
                        let _ = child.wait();
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some(format!("Failed to execute {}: {}", editor, err)),
                            err.to_string(),
                            Some(NotificationType::Error(melib::error::ErrorKind::External)),
                        ));
                        context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                        context.restore_input();
                        return true;
                    }
                }
                context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                let result = f.read_to_string();
                match self.draft.update(result.as_str()) {
                    Ok(has_changes) => {
                        self.has_changes = has_changes;
                    }
                    Err(err) => {
                        context.replies.push_back(UIEvent::Notification(
                            Some("Could not parse draft headers correctly.".to_string()),
                            format!(
                                "{}\nThe invalid text has been set as the body of your draft",
                                &err
                            ),
                            Some(NotificationType::Error(melib::error::ErrorKind::None)),
                        ));
                        self.draft.set_body(result);
                        self.has_changes = true;
                    }
                }
                self.initialized = false;
                self.set_dirty(true);
                return true;
            }
            UIEvent::Action(ref a) => match a {
                Action::Compose(ComposeAction::AddAttachmentPipe(ref command)) => {
                    if command.is_empty() {
                        context.replies.push_back(UIEvent::Notification(
                            None,
                            format!("pipe command value is invalid: {}", command),
                            Some(NotificationType::Error(melib::error::ErrorKind::None)),
                        ));
                        return false;
                    }
                    let f = create_temp_file(&[], None, None, true);
                    match Command::new("sh")
                        .args(&["-c", command])
                        .stdin(Stdio::null())
                        .stdout(Stdio::from(f.file()))
                        .spawn()
                        .and_then(|child| Ok(child.wait_with_output()?.stderr))
                    {
                        Ok(stderr) => {
                            if !stderr.is_empty() {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(format!(
                                        "Command stderr output: `{}`.",
                                        String::from_utf8_lossy(&stderr)
                                    )),
                                ));
                            }
                            let attachment =
                                match melib::email::compose::attachment_from_file(f.path()) {
                                    Ok(a) => a,
                                    Err(err) => {
                                        context.replies.push_back(UIEvent::Notification(
                                            Some("could not add attachment".to_string()),
                                            err.to_string(),
                                            Some(NotificationType::Error(
                                                melib::error::ErrorKind::None,
                                            )),
                                        ));
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
                            context.replies.push_back(UIEvent::Notification(
                                None,
                                format!("could not execute pipe command {}: {}", command, &err),
                                Some(NotificationType::Error(melib::error::ErrorKind::External)),
                            ));
                            return true;
                        }
                    }
                }
                Action::Compose(ComposeAction::AddAttachment(ref path)) => {
                    let attachment = match melib::email::compose::attachment_from_file(path) {
                        Ok(a) => a,
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification(
                                Some("could not add attachment".to_string()),
                                err.to_string(),
                                Some(NotificationType::Error(melib::error::ErrorKind::None)),
                            ));
                            self.set_dirty(true);
                            return true;
                        }
                    };
                    self.draft.attachments_mut().push(attachment);
                    self.has_changes = true;
                    self.set_dirty(true);
                    return true;
                }
                Action::Compose(ComposeAction::AddAttachmentFilePicker(ref command)) => {
                    let command = if let Some(cmd) =
                        command
                            .as_ref()
                            .or(context.settings.terminal.file_picker_command.as_ref())
                    {
                        cmd.as_str()
                    } else {
                        context.replies.push_back(UIEvent::Notification(
                            None,
                            "You haven't defined any command to launch.".into(),
                            Some(NotificationType::Error(melib::error::ErrorKind::None)),
                        ));
                        return true;
                    };
                    /* Kill input thread so that spawned command can be sole receiver of stdin */
                    {
                        context.input_kill();
                    }

                    log(
                        format!("Executing: sh -c \"{}\"", command.replace('"', "\\\"")),
                        DEBUG,
                    );
                    match Command::new("sh")
                        .args(&["-c", command])
                        .stdin(Stdio::inherit())
                        .stdout(Stdio::inherit())
                        .stderr(Stdio::piped())
                        .spawn()
                        .and_then(|child| Ok(child.wait_with_output()?.stderr))
                    {
                        Ok(stderr) => {
                            debug!(&String::from_utf8_lossy(&stderr));
                            for path in stderr.split(|c| [b'\0', b'\t', b'\n'].contains(c)) {
                                match melib::email::compose::attachment_from_file(
                                    &String::from_utf8_lossy(path).as_ref(),
                                ) {
                                    Ok(a) => {
                                        self.draft.attachments_mut().push(a);
                                        self.has_changes = true;
                                    }
                                    Err(err) => {
                                        context.replies.push_back(UIEvent::Notification(
                                            Some(format!(
                                                "could not add attachment: {}",
                                                String::from_utf8_lossy(path)
                                            )),
                                            err.to_string(),
                                            Some(NotificationType::Error(
                                                melib::error::ErrorKind::None,
                                            )),
                                        ));
                                    }
                                };
                            }
                        }
                        Err(err) => {
                            let command = command.to_string();
                            context.replies.push_back(UIEvent::Notification(
                                Some(format!("Failed to execute {}: {}", command, err)),
                                err.to_string(),
                                Some(NotificationType::Error(melib::error::ErrorKind::External)),
                            ));
                            context.restore_input();
                            return true;
                        }
                    }
                    context.replies.push_back(UIEvent::Fork(ForkType::Finished));
                    self.set_dirty(true);
                    return true;
                }
                Action::Compose(ComposeAction::RemoveAttachment(idx)) => {
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
                Action::Compose(ComposeAction::SaveDraft) => {
                    save_draft(
                        self.draft.clone().finalise().unwrap().as_bytes(),
                        context,
                        SpecialUsageMailbox::Drafts,
                        Flag::SEEN | Flag::DRAFT,
                        self.account_hash,
                    );
                    return true;
                }
                #[cfg(feature = "gpgme")]
                Action::Compose(ComposeAction::ToggleSign) => {
                    let is_true = self.gpg_state.sign_mail.is_true();
                    self.gpg_state.sign_mail = ToggleFlag::from(!is_true);
                    self.set_dirty(true);
                    return true;
                }
                #[cfg(feature = "gpgme")]
                Action::Compose(ComposeAction::ToggleEncrypt) => {
                    let is_true = self.gpg_state.encrypt_mail.is_true();
                    self.gpg_state.encrypt_mail = ToggleFlag::from(!is_true);
                    self.set_dirty(true);
                    return true;
                }
                _ => {}
            },
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        match self.mode {
            ViewMode::Embed => true,
            ViewMode::EditAttachments { ref widget } => widget.dirty || widget.buttons.is_dirty(),
            ViewMode::Edit => self.dirty || self.pager.is_dirty() || self.form.is_dirty(),
            ViewMode::Discard(_, ref widget) => {
                widget.is_dirty() || self.pager.is_dirty() || self.form.is_dirty()
            }
            ViewMode::SelectRecipients(ref widget) => {
                widget.is_dirty() || self.pager.is_dirty() || self.form.is_dirty()
            }
            #[cfg(feature = "gpgme")]
            ViewMode::SelectEncryptKey(_, ref widget) => {
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
        if let ViewMode::EditAttachments { ref mut widget } = self.mode {
            (EditAttachmentsRefMut {
                inner: widget,
                draft: &mut self.draft,
            })
            .set_dirty(value);
        }
    }

    fn kill(&mut self, uuid: Uuid, context: &mut Context) {
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

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = if self.mode.is_edit() {
            self.pager.get_shortcuts(context)
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
    fn set_id(&mut self, id: ComponentId) {
        self.id = id;
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
    crate::components::mail::pgp::sign(
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
            debug!("{:?} could not sign draft msg", err);
            log(
                format!(
                    "Could not sign draft in account `{}`: {}.",
                    context.accounts[&account_hash].name(),
                    err.to_string()
                ),
                ERROR,
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
    let send_mail = account_settings!(context[account_hash].composing.send_mail).clone();
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
        Err(MeliError {
            summary,
            details,
            kind,
            ..
        }) => {
            context.replies.push_back(UIEvent::Notification(
                details.map(|s| s.into()),
                summary.to_string(),
                Some(NotificationType::Error(kind)),
            ));
        }
        Ok(mailbox_hash) => {
            context.replies.push_back(UIEvent::Notification(
                Some("Message saved".into()),
                format!(
                    "Message saved in `{}`",
                    &context.accounts[&account_hash].mailbox_entries[&mailbox_hash].name
                ),
                Some(NotificationType::Info),
            ));
        }
    }
}

pub fn send_draft_async(
    #[cfg(feature = "gpgme")] gpg_state: gpg::GpgComposeState,
    context: &mut Context,
    account_hash: AccountHash,
    mut draft: Draft,
    mailbox_type: SpecialUsageMailbox,
    flags: Flag,
) -> Result<Pin<Box<dyn Future<Output = Result<()>> + Send>>> {
    let store_sent_mail = *account_settings!(context[account_hash].composing.store_sent_mail);
    let format_flowed = *account_settings!(context[account_hash].composing.format_flowed);
    let event_sender = context.sender.clone();
    #[cfg(feature = "gpgme")]
    #[allow(clippy::type_complexity)]
    let mut filters_stack: Vec<
        Box<
            dyn FnOnce(
                    AttachmentBuilder,
                )
                    -> Pin<Box<dyn Future<Output = Result<AttachmentBuilder>> + Send>>
                + Send,
        >,
    > = vec![];
    #[cfg(feature = "gpgme")]
    if gpg_state.sign_mail.is_true() && !gpg_state.encrypt_mail.is_true() {
        filters_stack.push(Box::new(crate::components::mail::pgp::sign_filter(
            gpg_state.sign_keys,
        )?));
    } else if gpg_state.encrypt_mail.is_true() {
        filters_stack.push(Box::new(crate::components::mail::pgp::encrypt_filter(
            if gpg_state.sign_mail.is_true() {
                Some(gpg_state.sign_keys.clone())
            } else {
                None
            },
            gpg_state.encrypt_keys,
        )?));
    }
    let send_mail = account_settings!(context[account_hash].composing.send_mail).clone();
    let send_cb = context.accounts[&account_hash].send_async(send_mail);
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
            },
            Default::default(),
            Vec::new(),
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
            let f = create_temp_file(message.as_bytes(), None, None, false);
            log(
                format!(
                    "store_sent_mail is false; stored sent mail to {}",
                    f.path().display()
                ),
                INFO,
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
    melib::datetime::timestamp_to_string(date, Some(fmt.as_str()), posix)
}

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

    let envelope = Envelope::from_bytes(raw_mail.as_bytes(), None).expect("Could not parse mail");
    let mut context = Context::new_mock();
    let account_hash = context.accounts[0].hash();
    let mailbox_hash = 0;
    let envelope_hash = envelope.hash();
    context.accounts[0]
        .collection
        .insert(envelope, mailbox_hash);
    let composer = Composer::reply_to(
        (account_hash, mailbox_hash, envelope_hash),
        String::new(),
        &mut context,
        false,
    );
    assert_eq!(&composer.draft.headers()["Subject"], "RE: your e-mail");
    assert_eq!(
        &composer.draft.headers()["To"],
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
    let envelope = Envelope::from_bytes(raw_mail.as_bytes(), None).expect("Could not parse mail");
    let envelope_hash = envelope.hash();
    context.accounts[0]
        .collection
        .insert(envelope, mailbox_hash);
    let composer = Composer::reply_to(
        (account_hash, mailbox_hash, envelope_hash),
        String::new(),
        &mut context,
        false,
    );
    assert_eq!(&composer.draft.headers()["Subject"], "Re: your e-mail");
    assert_eq!(
        &composer.draft.headers()["To"],
        r#"some name <some@example.com>"#
    );
}
