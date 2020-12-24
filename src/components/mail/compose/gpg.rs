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
        allow_remote_lookup: ToggleFlag,
    },
    Error {
        id: ComponentId,
        err: MeliError,
    },
    Loaded {
        widget: UIDialog<melib::gpgme::Key>,
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
        allow_remote_lookup: ToggleFlag,
        context: &mut Context,
    ) -> Result<Self> {
        use melib::gpgme::*;
        debug!("KeySelection::new");
        debug!(&secret);
        debug!(&local);
        debug!(&pattern);
        debug!(&allow_remote_lookup);
        let mut ctx = Context::new()?;
        if local {
            ctx.set_auto_key_locate(LocateKey::LOCAL)?;
        } else {
            ctx.set_auto_key_locate(LocateKey::WKD | LocateKey::LOCAL)?;
        }
        let job = ctx.keylist(secret, Some(pattern.clone()))?;
        let handle = context.job_executor.spawn_specialized(job);
        let mut progress_spinner = ProgressSpinner::new(8, context);
        progress_spinner.start();
        Ok(KeySelection::LoadingKeys {
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
            KeySelection::LoadingKeys {
                ref mut progress_spinner,
                ..
            } => progress_spinner.draw(grid, center_area(area, (2, 2)), context),
            KeySelection::Error { ref err, .. } => {
                let theme_default = crate::conf::value(context, "theme_default");
                write_string_to_grid(
                    &err.to_string(),
                    grid,
                    theme_default.fg,
                    theme_default.bg,
                    theme_default.attrs,
                    center_area(area, (15, 2)),
                    Some(0),
                );
            }
            KeySelection::Loaded { ref mut widget, .. } => widget.draw(grid, area, context),
        }
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        debug!(&self);
        debug!(&event);
        match self {
            KeySelection::LoadingKeys {
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
                                        std::mem::replace(pattern, String::new()),
                                        *allow_remote_lookup,
                                        context,
                                    ) {
                                        Ok(w) => {
                                            *self = w;
                                        }
                                        Err(err) => *self = KeySelection::Error { err, id },
                                    }
                                } else if !*local && allow_remote_lookup.is_ask() {
                                    *self = KeySelection::Error {
                                        err: MeliError::new(format!(
                                            "No keys found for {}, perform remote lookup?",
                                            pattern
                                        )),
                                        id,
                                    }
                                } else {
                                    *self = KeySelection::Error {
                                        err: MeliError::new(format!(
                                            "No keys found for {}.",
                                            pattern
                                        )),
                                        id,
                                    }
                                }
                                if let KeySelection::Error { ref err, .. } = self {
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
                            let mut widget = UIDialog::new(
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
                                            Box::new(results.get(0).map(|k| k.clone())),
                                        ))
                                    },
                                )),
                                context,
                            );
                            widget.set_dirty(true);
                            *self = KeySelection::Loaded { widget, keys };
                        }
                        Ok(Some(Err(err))) => {
                            *self = KeySelection::Error {
                                err,
                                id: ComponentId::new_v4(),
                            };
                        }
                    }
                    false
                }
                _ => progress_spinner.process_event(event, context),
            },
            KeySelection::Error { .. } => false,
            KeySelection::Loaded { ref mut widget, .. } => widget.process_event(event, context),
        }
    }

    fn is_dirty(&self) -> bool {
        match self {
            KeySelection::LoadingKeys {
                ref progress_spinner,
                ..
            } => progress_spinner.is_dirty(),
            KeySelection::Error { .. } => true,
            KeySelection::Loaded { ref widget, .. } => widget.is_dirty(),
        }
    }

    fn set_dirty(&mut self, value: bool) {
        match self {
            KeySelection::LoadingKeys {
                ref mut progress_spinner,
                ..
            } => progress_spinner.set_dirty(value),
            KeySelection::Error { .. } => {}
            KeySelection::Loaded { ref mut widget, .. } => widget.set_dirty(value),
        }
    }

    fn kill(&mut self, _uuid: Uuid, _context: &mut Context) {}

    fn get_shortcuts(&self, context: &Context) -> ShortcutMaps {
        match self {
            KeySelection::LoadingKeys { .. } | KeySelection::Error { .. } => {
                ShortcutMaps::default()
            }
            KeySelection::Loaded { ref widget, .. } => widget.get_shortcuts(context),
        }
    }

    fn id(&self) -> ComponentId {
        match self {
            KeySelection::LoadingKeys {
                ref progress_spinner,
                ..
            } => progress_spinner.id(),
            KeySelection::Error { ref id, .. } => *id,
            KeySelection::Loaded { ref widget, .. } => widget.id(),
        }
    }

    fn set_id(&mut self, new_id: ComponentId) {
        match self {
            KeySelection::LoadingKeys {
                ref mut progress_spinner,
                ..
            } => progress_spinner.set_id(new_id),
            KeySelection::Error { ref mut id, .. } => *id = new_id,
            KeySelection::Loaded { ref mut widget, .. } => widget.set_id(new_id),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GpgComposeState {
    pub sign_mail: ToggleFlag,
    pub encrypt_mail: ToggleFlag,
    pub encrypt_keys: Vec<melib::gpgme::Key>,
    pub encrypt_for_self: bool,
    pub sign_keys: Vec<melib::gpgme::Key>,
}

impl GpgComposeState {
    pub fn new() -> Self {
        GpgComposeState {
            sign_mail: ToggleFlag::Unset,
            encrypt_mail: ToggleFlag::Unset,
            encrypt_keys: vec![],
            encrypt_for_self: true,
            sign_keys: vec![],
        }
    }
}
