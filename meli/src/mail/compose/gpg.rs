/*
 * meli
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

use super::*;

#[derive(Debug)]
pub enum KeySelection {
    LoadingKeys {
        handle: JoinHandle<Result<Vec<melib::gpgme::Key>>>,
        progress_spinner: ProgressSpinner,
        secret: bool,
        local: bool,
        pattern: String,
        allow_remote_lookup: ActionFlag,
    },
    Error {
        id: ComponentId,
        err: Error,
    },
    Loaded {
        widget: Box<UIDialog<melib::gpgme::Key>>,
        keys: Vec<melib::gpgme::Key>,
    },
}

impl std::fmt::Display for KeySelection {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "select pgp keys")
    }
}

impl KeySelection {
    pub fn new(
        secret: bool,
        local: bool,
        pattern: String,
        allow_remote_lookup: ActionFlag,
        context: &Context,
    ) -> Result<Self> {
        use melib::gpgme::*;
        let mut ctx = Context::new()?;
        if local {
            ctx.set_auto_key_locate(LocateKey::LOCAL)?;
        } else {
            ctx.set_auto_key_locate(LocateKey::WKD | LocateKey::LOCAL)?;
        }
        let job = ctx.keylist(secret, Some(pattern.clone()))?;
        let handle = context.main_loop_handler.job_executor.spawn(
            "gpg::keylist".into(),
            job,
            IsAsync::Blocking,
        );
        let mut progress_spinner = ProgressSpinner::new(8, context);
        progress_spinner.start();
        Ok(Self::LoadingKeys {
            handle,
            secret,
            local,
            pattern,
            allow_remote_lookup,
            progress_spinner,
        })
    }
}

impl Component for KeySelection {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        match self {
            Self::LoadingKeys {
                ref mut progress_spinner,
                ..
            } => progress_spinner.draw(grid, area.center_inside((2, 2)), context),
            Self::Error { ref err, .. } => {
                let theme_default = crate::conf::value(context, "theme_default");
                grid.write_string(
                    &err.to_string(),
                    theme_default.fg,
                    theme_default.bg,
                    theme_default.attrs,
                    area.center_inside((15, 2)),
                    None,
                    Some(0),
                );
            }
            Self::Loaded { ref mut widget, .. } => widget.draw(grid, area, context),
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match self {
            Self::LoadingKeys {
                ref mut progress_spinner,
                ref mut handle,
                secret,
                local,
                ref mut pattern,
                allow_remote_lookup,
                ..
            } => match event {
                UIEvent::StatusEvent(StatusEvent::JobFinished(ref id)) if *id == handle.job_id => {
                    match handle.chan.try_recv() {
                        Err(_) => { /* Job was canceled */ }
                        Ok(None) => { /* something happened, perhaps a worker thread panicked */ }
                        Ok(Some(Ok(keys))) => {
                            if keys.is_empty() {
                                let id = progress_spinner.id();
                                if allow_remote_lookup.is_true() {
                                    match Self::new(
                                        *secret,
                                        *local,
                                        std::mem::take(pattern),
                                        *allow_remote_lookup,
                                        context,
                                    ) {
                                        Ok(w) => {
                                            *self = w;
                                        }
                                        Err(err) => *self = Self::Error { err, id },
                                    }
                                } else if !*local && allow_remote_lookup.is_ask() {
                                    *self = Self::Error {
                                        err: Error::new(format!(
                                            "No keys found for {}, perform remote lookup?",
                                            pattern
                                        )),
                                        id,
                                    }
                                } else {
                                    *self = Self::Error {
                                        err: Error::new(format!("No keys found for {}.", pattern)),
                                        id,
                                    }
                                }
                                if let Self::Error { ref err, .. } = self {
                                    context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(err.to_string()),
                                    ));
                                    let res: Option<melib::gpgme::Key> = None;
                                    context
                                        .replies
                                        .push_back(UIEvent::FinishedUIDialog(id, Box::new(res)));
                                }
                                return false;
                            }
                            let mut widget = Box::new(UIDialog::new(
                                "select key",
                                keys.iter()
                                    .map(|k| {
                                        (
                                            k.clone(),
                                            if let Some(primary_uid) = k.primary_uid() {
                                                format!("{} {}", k.fingerprint(), primary_uid)
                                            } else {
                                                k.fingerprint().to_string()
                                            },
                                        )
                                    })
                                    .collect::<Vec<(melib::gpgme::Key, String)>>(),
                                true,
                                Some(Box::new(
                                    move |id: ComponentId, results: &[melib::gpgme::Key]| {
                                        Some(UIEvent::FinishedUIDialog(
                                            id,
                                            Box::new(results.first().cloned()),
                                        ))
                                    },
                                )),
                                context,
                            ));
                            widget.set_dirty(true);
                            *self = Self::Loaded { widget, keys };
                        }
                        Ok(Some(Err(err))) => {
                            *self = Self::Error {
                                err,
                                id: ComponentId::default(),
                            };
                        }
                    }
                    false
                }
                _ => progress_spinner.process_event(event, context),
            },
            Self::Error { .. } => false,
            Self::Loaded { ref mut widget, .. } => widget.process_event(event, context),
        }
    }

    fn is_dirty(&self) -> bool {
        match self {
            Self::LoadingKeys {
                ref progress_spinner,
                ..
            } => progress_spinner.is_dirty(),
            Self::Error { .. } => true,
            Self::Loaded { ref widget, .. } => widget.is_dirty(),
        }
    }

    fn set_dirty(&mut self, value: bool) {
        match self {
            Self::LoadingKeys {
                ref mut progress_spinner,
                ..
            } => progress_spinner.set_dirty(value),
            Self::Error { .. } => {}
            Self::Loaded { ref mut widget, .. } => widget.set_dirty(value),
        }
    }

    fn kill(&mut self, _uuid: ComponentId, _context: &mut Context) {}

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        match self {
            Self::LoadingKeys { .. } | Self::Error { .. } => ShortcutMaps::default(),
            Self::Loaded { ref widget, .. } => widget.shortcuts(context),
        }
    }

    fn id(&self) -> ComponentId {
        match self {
            Self::LoadingKeys {
                ref progress_spinner,
                ..
            } => progress_spinner.id(),
            Self::Error { ref id, .. } => *id,
            Self::Loaded { ref widget, .. } => widget.id(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct GpgComposeState {
    pub sign_mail: Option<ActionFlag>,
    pub encrypt_mail: Option<ActionFlag>,
    pub encrypt_keys: Vec<melib::gpgme::Key>,
    pub encrypt_for_self: bool,
    pub sign_keys: Vec<melib::gpgme::Key>,
}

impl Default for GpgComposeState {
    fn default() -> Self {
        Self {
            sign_mail: None,
            encrypt_mail: None,
            encrypt_keys: vec![],
            encrypt_for_self: true,
            sign_keys: vec![],
        }
    }
}
