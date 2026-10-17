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
    fs::File,
    io::Write as _,
    os::unix::fs::PermissionsExt as _,
    path::Path,
    process::{Command, Stdio},
};

use melib::{
    email::{
        list_management::{ListAction, ListActions},
        mailto::Mailto,
        Draft, Flag,
    },
    error::{Error, Result},
    utils::shellexpand::ShellExpandTrait as _,
    HeaderName, LogLevel, SpecialUsageMailbox,
};

use crate::{
    accounts::JobRequest,
    components::prelude::*,
    mail::compose::{send_draft_async, Composer},
    mailbox_settings, ForkedProcess, NotificationType,
};

pub fn save_attachment(path: &Path, bytes: &[u8]) -> Result<()> {
    let mut f = File::options()
        .read(true)
        .write(true)
        .create_new(true)
        .open(path.expand())?;
    let mut permissions = f.metadata()?.permissions();
    permissions.set_mode(0o600); // Read/write for owner only.
    f.set_permissions(permissions)?;
    f.write_all(bytes)?;
    f.flush()?;
    Ok(())
}

pub fn desktop_exec_to_command(command: &str, path: String, is_url: bool) -> String {
    /* Purge unused field codes */
    let command = command
        .replace("%i", "")
        .replace("%c", "")
        .replace("%k", "");
    if command.contains("%f") {
        command.replacen("%f", &path.replace(' ', "\\ "), 1)
    } else if command.contains("%F") {
        command.replacen("%F", &path.replace(' ', "\\ "), 1)
    } else if command.contains("%u") || command.contains("%U") {
        let from_pattern = if command.contains("%u") { "%u" } else { "%U" };
        if is_url {
            command.replacen(from_pattern, &path, 1)
        } else {
            command.replacen(
                from_pattern,
                &format!("file://{path}").replace(' ', "\\ "),
                1,
            )
        }
    } else if is_url {
        format!("{command} {path}")
    } else {
        format!("{} {}", command, path.replace(' ', "\\ "))
    }
}

pub fn handle_mailing_list_action(
    user_action: &MailingListAction,
    available_actions: ListActions,
    coordinates: (AccountHash, MailboxHash, EnvelopeHash),
    context: &mut Context,
) {
    enum ValidOption {
        Email { draft: Draft, auto: bool },
        Url(String),
    }

    let mut valid_options = vec![];
    let mut invalid_options: Vec<Error> = vec![];

    macro_rules! process {
        ($actions:expr) => {{
            for option in $actions {
                match option {
                    ListAction::Email(email) => match Mailto::try_from(email.as_slice()) {
                        Ok(mailto) => {
                            let mut draft: Draft = mailto.into();
                            draft.set_header(
                                HeaderName::FROM,
                                context.accounts[&coordinates.0]
                                    .settings
                                    .account()
                                    .main_identity_address()
                                    .to_string(),
                            );
                            if !matches!(
                                user_action,
                                MailingListAction::ListPost | MailingListAction::ListOwner
                            ) {
                                valid_options.push(ValidOption::Email {
                                    draft: draft.clone(),
                                    auto: true,
                                });
                            }
                            valid_options.push(ValidOption::Email { draft, auto: false });
                        }
                        Err(err) => {
                            invalid_options.push(err);
                        }
                    },
                    ListAction::Url(url) => {
                        valid_options
                            .push(ValidOption::Url(String::from_utf8_lossy(&url).into_owned()));
                    }
                    ListAction::No => {
                        if matches!(user_action, MailingListAction::ListPost) {
                            context.replies.push_back(UIEvent::Notification {
                                title: None,
                                source: None,
                                body: "List prohibits posting (It has a List-Post: NO header)"
                                    .into(),
                                kind: None,
                            });
                            return;
                        }
                        // Ignore invalid "NO" value, it's only for List-Post
                    }
                }
            }
        }};
    }

    let actions = match user_action {
        MailingListAction::ListPost => available_actions.post,
        MailingListAction::ListUnsubscribe => available_actions.unsubscribe,
        MailingListAction::ListArchive => available_actions.archive,
        MailingListAction::ListHelp => available_actions.help,
        MailingListAction::ListSubscribe => available_actions.subscribe,
        MailingListAction::ListOwner => available_actions.owner,
    };
    if let Some(actions) = actions {
        process!(actions);
    }

    if valid_options.is_empty() {
        let title = Some(user_action.as_str().into());
        let body = if invalid_options.is_empty() {
            format!("No {} header found", user_action.as_str()).into()
        } else {
            format!("Could not parse header values: {:?}", invalid_options).into()
        };
        context.replies.push_back(UIEvent::Notification {
            title,
            source: None,
            body,
            kind: None,
        });
        return;
    }

    let question = format!(
        "{list_id}: {hdr}",
        list_id = available_actions
            .id
            .as_deref()
            .unwrap_or("<list without ID>"),
        hdr = user_action.as_str()
    );
    let options: Vec<(usize, String)> = valid_options
        .iter()
        .enumerate()
        .map(|(i, opt)| {
            (
                i,
                match opt {
                    ValidOption::Email { draft, auto: false } => format!(
                        "Send e-mail to {to} (opens composer tab)",
                        to = &draft.headers[HeaderName::TO]
                    ),
                    ValidOption::Email { draft, auto: true } => format!(
                        "Send e-mail to {to} automatically",
                        to = &draft.headers[HeaderName::TO]
                    ),
                    ValidOption::Url(url) => format!("Open URL {url} with default URL launcher"),
                },
            )
        })
        .collect();
    let user_action_as_str = user_action.as_str();
    let on_confirm_cb = Box::new(
        move |_: ComponentId, results: &[usize], context: &mut Context| {
            let Some(result) = results.first().cloned() else {
                return;
            };
            let opt = valid_options.remove(result);
            match opt {
                ValidOption::Email { draft, auto: false } => {
                    let mut composer = Composer::with_account(coordinates.0, context);
                    composer.set_draft(draft, context);
                    context
                        .replies
                        .push_back(UIEvent::Action(Tab(New(Some(Box::new(composer))))));
                }
                ValidOption::Email { draft, auto: true } => {
                    match send_draft_async(
                        Default::default(),
                        context,
                        coordinates.0,
                        draft,
                        SpecialUsageMailbox::Sent,
                        Flag::SEEN,
                    ) {
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some(
                                    format!("Couldn't send {user_action_as_str} e-mail").into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            });
                        }
                        Ok(fut) => {
                            let account = &mut context.accounts[&coordinates.0];
                            let handle = account.main_loop_handler.job_executor.spawn(
                                user_action_as_str.into(),
                                fut,
                                account.is_async(),
                            );
                            let job_id = handle.job_id;
                            context.accounts[&coordinates.0].insert_job(
                                job_id,
                                JobRequest::Generic {
                                    name: user_action_as_str.into(),
                                    handle,
                                    on_finish: None,
                                    log_level: LogLevel::INFO,
                                },
                            );
                        }
                    }
                }
                ValidOption::Url(url) => {
                    let url_launcher = mailbox_settings!(
                        context[&coordinates.0][&coordinates.1].pager.url_launcher
                    )
                    .as_ref()
                    .map(|s| s.as_str())
                    .unwrap_or(if cfg!(target_os = "macos") {
                        "open"
                    } else {
                        "xdg-open"
                    });
                    match Command::new(url_launcher)
                        .arg(&url)
                        .stdin(Stdio::piped())
                        .stdout(Stdio::piped())
                        .spawn()
                    {
                        Ok(child) => {
                            context
                                .children
                                .entry(url_launcher.to_string().into())
                                .or_default()
                                .push(ForkedProcess::Generic {
                                    id: url_launcher.to_string().into(),
                                    command: Some(format!("{url_launcher} {url}").into()),
                                    child,
                                });
                        }
                        Err(err) => {
                            context.replies.push_back(UIEvent::Notification {
                                title: Some(format!("Couldn't launch {url_launcher}").into()),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind().into())),
                            });
                        }
                    }
                }
            }
        },
    );
    context.replies.push_back(UIEvent::GlobalUIDialog {
        value: Box::new(UIDialog::new(
            question,
            options,
            true,
            Some(on_confirm_cb),
            context,
        )),
        parent: None,
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_desktop_exec() {
        assert_eq!(
            "ristretto /tmp/file".to_string(),
            desktop_exec_to_command("ristretto %F", "/tmp/file".to_string(), false)
        );
        assert_eq!(
            "/usr/lib/firefox-esr/firefox-esr file:///tmp/file".to_string(),
            desktop_exec_to_command(
                "/usr/lib/firefox-esr/firefox-esr %u",
                "/tmp/file".to_string(),
                false
            )
        );
        assert_eq!(
            "/usr/lib/firefox-esr/firefox-esr www.example.com".to_string(),
            desktop_exec_to_command(
                "/usr/lib/firefox-esr/firefox-esr %u",
                "www.example.com".to_string(),
                true
            )
        );
        assert_eq!(
            "/usr/bin/vlc --started-from-file www.example.com".to_string(),
            desktop_exec_to_command(
                "/usr/bin/vlc --started-from-file %U",
                "www.example.com".to_string(),
                true
            )
        );
        assert_eq!(
            "zathura --fork file:///tmp/file".to_string(),
            desktop_exec_to_command("zathura --fork %U", "file:///tmp/file".to_string(), true)
        );
    }
}
