//
// meli
//
// Copyright 2026 Manos Pitsidianakis <manos@pitsidianak.is>
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

use std::pin::Pin;

use futures::io::{AsyncReadExt as _, AsyncWriteExt as _};
use melib::error::Result;

use crate::{
    state::MainLoopHandler,
    terminal::Key,
    varlink::{
        interface::{
            CommandRequest, CommandType, GetInfoResponse, GetInfoType,
            GetInterfaceDescriptionRequest, GetInterfaceDescriptionType, InputCharacterRequest,
            InputCharacterType, InputKeyRequest, InputKeyType, InputPasteRequest, InputPasteType,
            MethodRequest, Modifier, ServerVersionType, VarlinkError, VersionIdentifier,
        },
        server::Stream,
    },
    ThreadEvent, UIEvent,
};

pub struct Handler {
    main_loop_handler: MainLoopHandler,
    stream: Pin<Box<dyn Stream>>,
}

impl Handler {
    pub fn new(main_loop_handler: MainLoopHandler, stream: Pin<Box<dyn Stream>>) -> Self {
        Self {
            main_loop_handler,
            stream,
        }
    }

    pub async fn handle(self) -> Result<()> {
        let Self {
            main_loop_handler,
            mut stream,
        } = self;

        let mut writer = stream.clone();
        let mut buf = vec![0; 1024 * 8];
        let mut i = 0;
        loop {
            let bytes = stream.read(&mut buf[i..]).await?;
            if bytes == 0 {
                break;
            }
            i += bytes;
            if let Some(pos) = buf[..i].iter().position(|b| *b == b'\0') {
                let mut oneway = false;
                let reply: Result<serde_json::Value> = match serde_json::from_reader::<
                    _,
                    MethodRequest,
                >(&buf[..pos])
                {
                    Ok(request) => {
                        oneway = request.oneway;
                        match request.method.rfind('.') {
                            None => Ok(serde_json::json! {
                                VarlinkError {
                                    error: "org.varlink.service.InterfaceNotFound".to_string(),
                                    parameters: serde_json::json!{{"interface": request.method }}
                                }
                            }),
                            Some(pos) => {
                                let varlink_interface = &request.method[..pos];
                                match varlink_interface {
                                    "org.varlink.service" => match request.method {
                                        m if m == GetInfoType::NAME => {
                                            Ok(serde_json::json! {GetInfoResponse {
                                                vendor: "meli".to_string(),
                                                product: "meli".to_string(),
                                                version: "".to_string(),
                                                url: "https://meli-email.org".to_string(),
                                                interfaces: vec!["org.meli-email.meli".to_string(), "org.varlink.service".to_string()],
                                            }})
                                        }
                                        m if m == GetInterfaceDescriptionType::NAME => {
                                            match serde_json::from_value::<
                                                GetInterfaceDescriptionRequest,
                                            >(
                                                request.parameters
                                            ) {
                                                Ok(req) => match req.interface.as_str() {
                                                    "org.varlink.service" => Ok(
                                                        serde_json::json! {{ "description": crate::varlink::interface::VARLINK_SERVICE }},
                                                    ),
                                                    "org.meli-email.meli" => Ok(
                                                        serde_json::json! {{ "description": crate::varlink::interface::MELI }},
                                                    ),
                                                    other => Ok(serde_json::json! {
                                                        VarlinkError {
                                                            error: "org.varlink.service.InterfaceNotFound".to_string(),
                                                            parameters: serde_json::json!{{"interface": other}}
                                                        }
                                                    }),
                                                },
                                                Err(err) => Err(err.into()),
                                            }
                                        }

                                        other => Ok(serde_json::json! {
                                            VarlinkError {
                                                error: "org.varlink.service.MethodNotFound".to_string(),
                                                parameters: serde_json::json!{{"method": other}}
                                            }
                                        }),
                                    },
                                    "org.meli-email.meli" => match request.method {
                                        m if m == CommandType::NAME => {
                                            match serde_json::from_value::<CommandRequest>(
                                                request.parameters,
                                            ) {
                                                Ok(req) => {
                                                    let success =
                                                        match crate::command::parse_command(
                                                            &req.command,
                                                        ) {
                                                            Ok(_) => {
                                                                main_loop_handler
                                                                    .sender
                                                                    .send(ThreadEvent::UIEvent(
                                                                        UIEvent::Command(
                                                                            req.command,
                                                                        ),
                                                                    ))
                                                                    .unwrap();
                                                                Ok(())
                                                            }
                                                            Err(err) => Err(err),
                                                        };

                                                    if let Err(err) = success {
                                                        Err(err.into())
                                                    } else {
                                                        Ok(serde_json::json! {{}})
                                                    }
                                                }
                                                Err(err) => Err(err.into()),
                                            }
                                        }
                                        m if m == InputKeyType::NAME => {
                                            match serde_json::from_value::<InputKeyRequest>(
                                                request.parameters,
                                            ) {
                                                Ok(req) => {
                                                    main_loop_handler
                                                        .sender
                                                        .send(ThreadEvent::Input((
                                                            req.key.into(),
                                                            vec![],
                                                        )))
                                                        .unwrap();
                                                    Ok(serde_json::json! {{}})
                                                }
                                                Err(err) => Err(err.into()),
                                            }
                                        }
                                        m if m == InputCharacterType::NAME => {
                                            match serde_json::from_value::<InputCharacterRequest>(
                                                request.parameters,
                                            ) {
                                                Ok(req) if req.character.chars().count() != 1 => {
                                                    Ok(serde_json::json! {
                                                        VarlinkError {
                                                            error: "org.varlink.service.InvalidParameter".to_string(),
                                                            parameters: serde_json::json!{{"parameter": "character"}}
                                                        }
                                                    })
                                                }
                                                Ok(req) => {
                                                    let ch = req.character.chars().next().unwrap();
                                                    main_loop_handler
                                                        .sender
                                                        .send(ThreadEvent::Input((
                                                            match req.modifier {
                                                                None => Key::Char(ch),
                                                                Some(Modifier::Alt) => Key::Alt(ch),
                                                                Some(Modifier::Ctrl) => {
                                                                    Key::Ctrl(ch)
                                                                }
                                                            },
                                                            vec![],
                                                        )))
                                                        .unwrap();
                                                    Ok(serde_json::json! {{}})
                                                }
                                                Err(err) => Err(err.into()),
                                            }
                                        }
                                        m if m == InputPasteType::NAME => {
                                            match serde_json::from_value::<InputPasteRequest>(
                                                request.parameters,
                                            ) {
                                                Ok(req) => {
                                                    main_loop_handler
                                                        .sender
                                                        .send(ThreadEvent::Input((
                                                            Key::Paste(req.paste),
                                                            vec![],
                                                        )))
                                                        .unwrap();
                                                    Ok(serde_json::json! {{}})
                                                }
                                                Err(err) => Err(err.into()),
                                            }
                                        }
                                        m if m == ServerVersionType::NAME => {
                                            Ok(serde_json::json! {VersionIdentifier {
                                                major: crate::version_migrations::LATEST.major(),
                                                minor: crate::version_migrations::LATEST.minor(),
                                                patch: crate::version_migrations::LATEST.patch(),
                                                pre: if crate::version_migrations::LATEST.pre().is_empty() {
                                                    None
                                                } else {
                                                    Some(crate::version_migrations::LATEST.pre().to_string())
                                                },
                                            }})
                                        }
                                        other => Ok(serde_json::json! {
                                            VarlinkError {
                                                error: "org.varlink.service.MethodNotFound".to_string(),
                                                parameters: serde_json::json!{{"method": other}}
                                            }
                                        }),
                                    },
                                    other => Ok(serde_json::json! {
                                        VarlinkError {
                                            error: "org.varlink.service.InterfaceNotFound".to_string(),
                                            parameters: serde_json::json!{{"interface": other }}
                                        }
                                    }),
                                }
                            }
                        }
                    }
                    Err(err) => Err(err.into()),
                };
                let json_val = match reply {
                    Ok(parameters) => {
                        serde_json::json! {{"parameters": parameters}}
                    }
                    Err(err) => {
                        serde_json::json! {{"error": err.to_string()}}
                    }
                };
                if oneway {
                    log::trace!("not replying {json_val:?}, oneway requested");
                } else {
                    log::trace!("sending reply {json_val:?}");
                    let reply = format!("{}\0", serde_json::to_string(&json_val).unwrap());
                    writer.write_all(reply.as_bytes()).await.unwrap();
                }
                buf.splice(..=pos, std::iter::repeat_n(0, pos));
                i = 0;
            }
            if i >= buf.len() {
                break;
            }
        }
        Ok(())
    }
}
