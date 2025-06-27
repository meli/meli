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

type KeylistJoinHandle = JoinHandle<Result<Vec<melib::gpgme::Key>>>;

#[derive(Debug)]
pub struct KeySelectionLoading {
    handles: (KeylistJoinHandle, Vec<KeylistJoinHandle>),
    progress_spinner: ProgressSpinner,
    secret: bool,
    local: bool,
    patterns: (String, Vec<String>),
    allow_remote_lookup: ActionFlag,
}

impl KeySelectionLoading {
    pub fn new(
        secret: bool,
        local: bool,
        patterns: (String, Vec<String>),
        allow_remote_lookup: ActionFlag,
        context: &Context,
    ) -> Result<Self> {
        use melib::gpgme::{self, *};
        let mut ctx = gpgme::Context::new()?;
        if local {
            ctx.set_auto_key_locate(LocateKey::LOCAL)?;
        } else {
            ctx.set_auto_key_locate(LocateKey::WKD | LocateKey::LOCAL)?;
        }
        let (pattern, other_patterns) = patterns;
        let main_job = ctx.keylist(secret, Some(pattern.clone()))?;
        let main_handle = context.main_loop_handler.job_executor.spawn(
            "gpg::keylist".into(),
            main_job,
            IsAsync::Blocking,
        );
        let other_handles = other_patterns
            .iter()
            .map(|pattern| {
                let job = ctx.keylist(secret, Some(pattern.clone()))?;
                Ok(context.main_loop_handler.job_executor.spawn(
                    "gpg::keylist".into(),
                    job,
                    IsAsync::Blocking,
                ))
            })
            .collect::<Result<Vec<KeylistJoinHandle>>>()?;
        let mut progress_spinner = ProgressSpinner::new(8, context);
        progress_spinner.start();
        Ok(Self {
            handles: (main_handle, other_handles),
            secret,
            local,
            patterns: (pattern, other_patterns),
            allow_remote_lookup,
            progress_spinner,
        })
    }

    pub fn merge(&mut self, rhs: Self) {
        let Self {
            handles: (_, ref mut other_handles),
            secret: _,
            local: _,
            patterns: (_, ref mut other_patterns),
            allow_remote_lookup: _,
            progress_spinner: _,
        } = self;
        let Self {
            handles: (rhs_handle, rhs_other_handles),
            patterns: (rhs_pattern, rhs_other_patterns),
            secret: _,
            local: _,
            allow_remote_lookup: _,
            progress_spinner: _,
        } = rhs;
        other_handles.push(rhs_handle);
        other_handles.extend(rhs_other_handles);
        other_patterns.push(rhs_pattern);
        other_patterns.extend(rhs_other_patterns);
    }
}

#[derive(Debug)]
pub enum KeySelection {
    Loading {
        inner: KeySelectionLoading,
        /// Accumulate results from intermediate results (i.e. not the main
        /// pattern)
        keys_accumulator: Vec<melib::gpgme::Key>,
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

impl From<KeySelectionLoading> for KeySelection {
    fn from(inner: KeySelectionLoading) -> Self {
        Self::Loading {
            inner,
            keys_accumulator: vec![],
        }
    }
}

impl std::fmt::Display for KeySelection {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "select pgp keys")
    }
}

impl Component for KeySelection {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        match self {
            Self::Loading {
                inner:
                    KeySelectionLoading {
                        ref mut progress_spinner,
                        ..
                    },
                keys_accumulator: _,
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
            Self::Loading {
                inner:
                    KeySelectionLoading {
                        ref mut progress_spinner,
                        handles: (ref mut main_handle, ref mut other_handles),
                        secret,
                        local,
                        patterns: (ref mut pattern, ref mut other_patterns),
                        allow_remote_lookup,
                        ..
                    },
                ref mut keys_accumulator,
            } => match event {
                UIEvent::StatusEvent(StatusEvent::JobFinished(ref id))
                    if *id == main_handle.job_id
                        || other_handles.iter().any(|h| h.job_id == *id) =>
                {
                    let mut main_handle_ref = &mut (*main_handle);
                    let is_main = *id == main_handle_ref.job_id;
                    let other_handle_ref_opt = other_handles.iter_mut().find(|h| h.job_id == *id);
                    let handle = if is_main {
                        &mut main_handle_ref
                    } else {
                        &mut (*other_handle_ref_opt.unwrap())
                    };
                    match handle.chan.try_recv() {
                        Err(_) => { /* Job was canceled */ }
                        Ok(None) => { /* something happened, perhaps a worker thread panicked */ }
                        Ok(Some(Ok(keys))) => {
                            if keys.is_empty() {
                                let id = progress_spinner.id();
                                if allow_remote_lookup.is_true() {
                                    match KeySelectionLoading::new(
                                        *secret,
                                        *local,
                                        (std::mem::take(pattern), std::mem::take(other_patterns)),
                                        *allow_remote_lookup,
                                        context,
                                    ) {
                                        Ok(inner) => {
                                            let keys_accumulator = std::mem::take(keys_accumulator);
                                            *self = Self::Loading {
                                                inner,
                                                keys_accumulator,
                                            };
                                        }
                                        Err(err) => *self = Self::Error { err, id },
                                    }
                                } else if !*local && allow_remote_lookup.is_ask() {
                                    *self = Self::Error {
                                        err: Error::new(format!(
                                            "No keys found for {pattern}, perform remote lookup?"
                                        )),
                                        id,
                                    }
                                } else {
                                    *self = Self::Error {
                                        err: if pattern.is_empty() {
                                            Error::new("No keys found.")
                                        } else {
                                            Error::new(format!("No keys found for {pattern}."))
                                        },
                                        id,
                                    }
                                }
                                if let Self::Error { ref err, .. } = self {
                                    context.replies.push_back(UIEvent::StatusEvent(
                                        StatusEvent::DisplayMessage(err.to_string()),
                                    ));
                                    // Even in case of error, we should send a FinishedUIDialog
                                    // event so that the component parent knows we're done.
                                    let res: Option<Vec<melib::gpgme::Key>> = None;
                                    context
                                        .replies
                                        .push_back(UIEvent::FinishedUIDialog(id, Box::new(res)));
                                }
                                return false;
                            }
                            keys_accumulator.extend(keys);
                            if !is_main {
                                other_handles.retain(|h| h.job_id != *id);
                                return false;
                            }
                            if other_handles.is_empty() {
                                // We are done with all Futures, so finally transition into the
                                // "show the user the list of keys to select" state.
                                let mut widget = Box::new(UIDialog::new(
                                    "select key",
                                    keys_accumulator
                                        .iter()
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
                                    false,
                                    Some(Box::new(
                                        move |id: ComponentId, results: &[melib::gpgme::Key]| {
                                            Some(UIEvent::FinishedUIDialog(
                                                id,
                                                Box::new(if results.is_empty() {
                                                    None
                                                } else {
                                                    Some(results.to_vec())
                                                }),
                                            ))
                                        },
                                    )),
                                    context,
                                ));
                                widget.set_dirty(true);
                                *self = Self::Loaded {
                                    widget,
                                    keys: std::mem::take(keys_accumulator),
                                };
                            } else {
                                // Main handle has finished, replace it with some other one from
                                // other_handles.
                                *main_handle = other_handles.remove(0);
                            }
                        }
                        Ok(Some(Err(err))) => {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(err.to_string()),
                            ));
                            // Even in case of error, we should send a FinishedUIDialog
                            // event so that the component parent knows we're done.
                            let res: Option<Vec<melib::gpgme::Key>> = None;
                            context
                                .replies
                                .push_back(UIEvent::FinishedUIDialog(self.id(), Box::new(res)));
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
            Self::Loading {
                inner:
                    KeySelectionLoading {
                        ref progress_spinner,
                        ..
                    },
                keys_accumulator: _,
            } => progress_spinner.is_dirty(),
            Self::Error { .. } => true,
            Self::Loaded { ref widget, .. } => widget.is_dirty(),
        }
    }

    fn set_dirty(&mut self, value: bool) {
        match self {
            Self::Loading {
                inner:
                    KeySelectionLoading {
                        ref mut progress_spinner,
                        ..
                    },
                keys_accumulator: _,
            } => progress_spinner.set_dirty(value),
            Self::Error { .. } => {}
            Self::Loaded { ref mut widget, .. } => widget.set_dirty(value),
        }
    }

    fn kill(&mut self, _uuid: ComponentId, _context: &mut Context) {}

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        match self {
            Self::Loading { .. } | Self::Error { .. } => ShortcutMaps::default(),
            Self::Loaded { ref widget, .. } => widget.shortcuts(context),
        }
    }

    fn id(&self) -> ComponentId {
        match self {
            Self::Loading {
                inner:
                    KeySelectionLoading {
                        ref progress_spinner,
                        ..
                    },
                keys_accumulator: _,
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

#[cfg(test)]
mod tests {
    use std::{borrow::Cow, ffi::CString, thread::sleep, time::Duration};

    use melib::gpgme::{EngineInfo, LocateKey, Protocol};
    use rusty_fork::rusty_fork_test;

    use super::*;

    impl KeySelection {
        fn new_mock(
            secret: bool,
            local: bool,
            pattern: String,
            allow_remote_lookup: ActionFlag,
            context: &Context,
            ctx: &mut melib::gpgme::Context,
        ) -> Result<Self> {
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
            Ok(Self::Loading {
                inner: KeySelectionLoading {
                    handles: (handle, vec![]),
                    secret,
                    local,
                    patterns: (pattern, vec![]),
                    allow_remote_lookup,
                    progress_spinner,
                },
                keys_accumulator: vec![],
            })
        }
    }

    const PUBKEY: &[u8]=b"-----BEGIN PGP PUBLIC KEY BLOCK-----\r\nVersion: GnuPG v2.1.0-gitb3c71eb (GNU/Linux)\r\n\r\nmQGiBDo41NoRBADSfQazKGYf8nokq6zUKH/6INtV6MypSzSGmX2XErnARkIIPPYj\r\ncQRQ8zCbGV7ZU2ezVbzhFLUSJveE8PZUzzCrLp1O2NSyBTRcR5HVSXW95nJfY8eV\r\npOvZRAKul0BVLh81kYTsrfzaaCjh9VWNP26LoeN2r+PjZyktXe7gM3C4SwCgoTxK\r\nWUVi9HoT2HCLY7p7oig5hEcEALdCJal0UYomX3nJapIVLVZg3vkidr1RICYMb2vz\r\n58i17h8sxEtobD1vdIKNejulntaRAXs4n0tDYD9z7pRlwG1CLz1R9WxYzeOOqUDr\r\nfnVXdmU8L/oVWABat8v1V7QQhjMMf+41fuzVwDMMGqjVPLhu4X6wp3A8uyM3YDnQ\r\nVMN1A/4n2G5gHoOvjqxn8Ch5tBAdMGfO8gH4RjQOwzm2R1wPQss/yzUN1+tlMZGX\r\nK2dQ2FCWC/hDUSNaEQRlI15wxxBNZ2RQwlzE2A8v113DpvyzOtv0QO95gJ1teCXC\r\n7j/BN9asgHaBBc39JLO/TcpuI7Hf8PQ5VcP2F0UE3lczGhXbLLRESm9lIFJhbmRv\r\nbSBIYWNrZXIgKHRlc3Qga2V5IHdpdGggcGFzc3BocmFzZSAiYWJjIikgPGpvZUBl\r\neGFtcGxlLmNvbT6IYgQTEQIAIgUCTbdXqQIbIwYLCQgHAwIGFQgCCQoLBBYCAwEC\r\nHgECF4AACgkQr4IkT5zZ/VUcCACfQvSPi//9/gBv8SVrK6O4DiyD+jAAn3LEnfF1\r\n4j6MjwlqXTqol2VgQn1yuQENBDo41N0QBACedJb7Qhm50JSPe1V+rSZKLHT5nc3l\r\n2k1n7//wNsJkgDW2J7snIRjGtSzeNxMPh+hVzFidzAf3sbOlARQoBrMPPKpnJWtm\r\n6LEDf2lSwO36l0/bo6qDRmiFRJoHWytTJEjxVwRclVt4bXqHfNw9FKhZZbcKeAN2\r\noHgmBVSU6edHdwADBQP+OGAkEG4PcfSb8x191R+wkV/q2hA5Ay9z289Dx2rO28CO\r\n4M2fhhcjSmgr6x0DsrkfESCiG47UGJ169eu+QqJwk3HiF4crGN9rE5+VelBVFtrd\r\nMWkX2rPLGQWyw8iCZKbeH8g/ujmkaLovSmalzDcLe4v1xSLaP7Fnfzit0iIGZAGI\r\nRgQYEQIABgUCOjjU3QAKCRCvgiRPnNn9VVSaAJ9+rj1lIQnRl20i8Rom2Hwbe3re\r\n9QCfSYFnkZUw0yKF2DfCfqrDzdGAsbaIRgQYEQIABgUCOjjU3gAKCRCvgiRPnNn9\r\nVe4iAJ9FrGMlFR7s+GWf1scTeeyrthKrPQCfSpc/Yps72aFI7hPfyIa9MuerVZ4=\r\n=QRit\r\n-----END PGP PUBLIC KEY BLOCK-----\r\n";

    rusty_fork_test! {
    #[test]
    fn test_gpg_verify_sig() {
        let tempdir = tempfile::tempdir().unwrap();
        {
            #[allow(unused_unsafe)]
            unsafe {
                std::env::set_var("GNUPGHOME", tempdir.path());
            }

            #[allow(unused_unsafe)]
            unsafe {
                std::env::set_var("GPG_AGENT_INFO", "");
            }
        }

        let mut ctx = Context::new_mock(&tempdir);
        let mut gpgme_ctx = match melib::gpgme::Context::new() {
            Ok(v) => v,
            Err(err) if err.kind.is_not_found() => {
                eprintln!("INFO: libgpgme could not be loaded, skipping this test.");
                return;
            }
            err => err.unwrap(),
        };
        let current_engine_info = gpgme_ctx.engine_info().unwrap();
        let prev_len = current_engine_info.len();
        let Some(EngineInfo {
            file_name: Some(engine_file_name),
            ..
        }) = current_engine_info
            .into_iter()
            .find(|eng| eng.protocol == Protocol::OpenPGP)
        else {
            eprintln!("WARN: No openpg protocol engine returned from gpgme.");
            return;
        };
        gpgme_ctx
            .set_engine_info(
                Protocol::OpenPGP,
                Some(Cow::Owned(CString::new(engine_file_name).unwrap())),
                Some(Cow::Owned(
                    CString::new(tempdir.path().display().to_string()).unwrap(),
                )),
            )
            .unwrap();
        let new_engine_info = gpgme_ctx.engine_info().unwrap();
        assert_eq!(
            new_engine_info.len(),
            prev_len,
            "new_engine_info was expected to have {} entry/ies but has {}: {:#?}",
            prev_len,
            new_engine_info.len(),
            new_engine_info
        );
        assert_eq!(
            new_engine_info[0].home_dir,
            Some(tempdir.path().display().to_string()),
            "new_engine_info was expected to have temp dir as home_dir but has: {:#?}",
            new_engine_info[0].home_dir
        );
        let mut pubkey_data = Some(gpgme_ctx.new_data_mem(PUBKEY).unwrap());
        for _ in 0..2 {
            let mut key_sel = KeySelection::new_mock(
                false,
                true,
                "".to_string(),
                false.into(),
                &ctx,
                &mut gpgme_ctx,
            )
            .unwrap();
            let component_id = key_sel.id();

            for _ in 0..2 {
                sleep(Duration::from_secs(2));
            }
            while let Ok(ev) = ctx.receiver.try_recv() {
                // if !matches!(ev, ThreadEvent::UIEvent(UIEvent::Timer(_))) {
                //     dbg!(&ev);
                // }
                if let ThreadEvent::UIEvent(mut ev) = ev {
                    key_sel.process_event(&mut ev, &mut ctx);
                } else if let ThreadEvent::JobFinished(job_id) = ev {
                    let mut ev = UIEvent::StatusEvent(StatusEvent::JobFinished(job_id));
                    key_sel.process_event(&mut ev, &mut ctx);
                }
            }
            if let Some(pubkey_data) = pubkey_data.take() {
                assert!(
                    matches!(
                        key_sel,
                        KeySelection::Error {
                            ref id,
                            ref err
                        } if *id == component_id && err.to_string() == melib::Error::new("No keys found.").to_string(),
                    ),
                    "key_sel should have been an error but is: {key_sel:?}"
                );
                gpgme_ctx.import_key(pubkey_data).unwrap();
            } else {
                let assert_key = |key: &melib::gpgme::Key| {
                    key.fingerprint() == "ADAB7FCC1F4DE2616ECFA402AF82244F9CD9FD55"
                        && key.primary_uid()
                            == Some(melib::Address::new(
                                Some("Joe Random Hacker".into()),
                                "joe@example.com".into(),
                            ))
                        && key.can_encrypt()
                        && key.can_sign()
                        && !key.secret()
                        && !key.revoked()
                        && !key.expired()
                        && !key.invalid()
                };
                assert!(
                    matches!(
                        key_sel,
                        KeySelection::Loaded {
                            ref keys,
                            widget: _,
                        } if keys.len() == 1 && assert_key(&keys[0]),
                    ),
                    "key_sel should have been an error but is: {key_sel:?}"
                );
            }
        }
    }
    }
}
