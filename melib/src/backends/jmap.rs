/*
 * meli - jmap module.
 *
 * Copyright 2019 Manos Pitsidianakis
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

use crate::async_workers::{Async, AsyncBuilder, AsyncStatus, WorkContext};
use crate::backends::BackendOp;
use crate::backends::FolderHash;
use crate::backends::RefreshEvent;
use crate::backends::RefreshEventKind::{self, *};
use crate::backends::{BackendFolder, Folder, FolderOperation, MailBackend, RefreshEventConsumer};
use crate::conf::AccountSettings;
use crate::email::*;
use crate::error::{MeliError, Result};
use fnv::{FnvHashMap, FnvHashSet};
use reqwest::blocking::Client;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::str::FromStr;
use std::sync::{Arc, Mutex, RwLock};

#[macro_export]
macro_rules! _impl {
        ($field:ident : $t:ty) => {
            pub fn $field(mut self, new_val: $t) -> Self {
                self.$field = new_val;
                self
            }
        }
    }

#[macro_export]
macro_rules! _impl_get_mut {
        ($method:ident, $field:ident : $t:ty) => {
            pub fn $method(&mut self) -> &mut $t {
                &mut self.$field
            }
        }
    }

pub mod connection;
use connection::*;

pub mod protocol;
use protocol::*;

pub mod rfc8620;
use rfc8620::*;

pub mod objects;
use objects::*;

pub mod folder;
use folder::*;

#[derive(Debug, Default)]
pub struct EnvelopeCache {
    bytes: Option<String>,
    headers: Option<String>,
    body: Option<String>,
    flags: Option<Flag>,
}

#[derive(Debug, Clone)]
pub struct JmapServerConf {
    pub server_hostname: String,
    pub server_username: String,
    pub server_password: String,
    pub server_port: u16,
    pub danger_accept_invalid_certs: bool,
}

macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            MeliError::new(format!(
                "Configuration error ({}): JMAP connection requires the field `{}` set",
                $s.name.as_str(),
                $var
            ))
        })
    };
    ($s:ident[$var:literal], $default:expr) => {
        $s.extra
            .get($var)
            .map(|v| {
                <_>::from_str(v).map_err(|e| {
                    MeliError::new(format!(
                        "Configuration error ({}): Invalid value for field `{}`: {}\n{}",
                        $s.name.as_str(),
                        $var,
                        v,
                        e
                    ))
                })
            })
            .unwrap_or_else(|| Ok($default))
    };
}

impl JmapServerConf {
    pub fn new(s: &AccountSettings) -> Result<Self> {
        Ok(JmapServerConf {
            server_hostname: get_conf_val!(s["server_hostname"])?.to_string(),
            server_username: get_conf_val!(s["server_username"])?.to_string(),
            server_password: get_conf_val!(s["server_password"])?.to_string(),
            server_port: get_conf_val!(s["server_port"], 443)?,
            danger_accept_invalid_certs: get_conf_val!(s["danger_accept_invalid_certs"], false)?,
        })
    }
}

struct IsSubscribedFn(Box<dyn Fn(&str) -> bool + Send + Sync>);

impl std::fmt::Debug for IsSubscribedFn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "IsSubscribedFn Box")
    }
}

impl std::ops::Deref for IsSubscribedFn {
    type Target = Box<dyn Fn(&str) -> bool + Send + Sync>;
    fn deref(&self) -> &Box<dyn Fn(&str) -> bool + Send + Sync> {
        &self.0
    }
}
macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).ok_or_else(|| {
            MeliError::new(format!(
                "Configuration error ({}): JMAP connection requires the field `{}` set",
                $s.name.as_str(),
                $var
            ))
        })
    };
    ($s:ident[$var:literal], $default:expr) => {
        $s.extra
            .get($var)
            .map(|v| {
                <_>::from_str(v).map_err(|e| {
                    MeliError::new(format!(
                        "Configuration error ({}): Invalid value for field `{}`: {}\n{}",
                        $s.name.as_str(),
                        $var,
                        v,
                        e
                    ))
                })
            })
            .unwrap_or_else(|| Ok($default))
    };
}

#[derive(Debug)]
pub struct JmapType {
    account_name: String,
    online: Arc<Mutex<bool>>,
    is_subscribed: Arc<IsSubscribedFn>,
    server_conf: JmapServerConf,
    connection: Arc<JmapConnection>,
    folders: Arc<RwLock<FnvHashMap<FolderHash, JmapFolder>>>,
}

impl MailBackend for JmapType {
    fn is_online(&self) -> bool {
        *self.online.lock().unwrap()
    }
    fn get(&mut self, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        let mut w = AsyncBuilder::new();
        let folders = self.folders.clone();
        let connection = self.connection.clone();
        let folder_hash = folder.hash();
        let handle = {
            let tx = w.tx();
            let closure = move |_work_context| {
                tx.send(AsyncStatus::Payload(
                    protocol::get_message_list(&connection, &folders.read().unwrap()[&folder_hash])
                        .and_then(|ids| {
                            protocol::get_message(&connection, std::dbg!(&ids).as_slice())
                        }),
                ))
                .unwrap();
                tx.send(AsyncStatus::Finished).unwrap();
            };
            Box::new(closure)
        };
        w.build(handle)
    }

    fn watch(
        &self,
        sender: RefreshEventConsumer,
        work_context: WorkContext,
    ) -> Result<std::thread::ThreadId> {
        Err(MeliError::from("sadfsa"))
    }

    fn folders(&self) -> Result<FnvHashMap<FolderHash, Folder>> {
        if self.folders.read().unwrap().is_empty() {
            let folders = std::dbg!(protocol::get_mailboxes(&self.connection))?;
            let ret = Ok(folders
                .iter()
                .map(|(&h, f)| (h, BackendFolder::clone(f) as Folder))
                .collect());
            *self.folders.write().unwrap() = folders;
            ret
        } else {
            Ok(self
                .folders
                .read()
                .unwrap()
                .iter()
                .map(|(&h, f)| (h, BackendFolder::clone(f) as Folder))
                .collect())
        }
    }

    fn operation(&self, hash: EnvelopeHash) -> Box<dyn BackendOp> {
        unimplemented!()
    }

    fn save(&self, bytes: &[u8], folder: &str, flags: Option<Flag>) -> Result<()> {
        Ok(())
    }

    fn folder_operation(&mut self, path: &str, op: FolderOperation) -> Result<()> {
        Ok(())
    }

    fn as_any(&self) -> &dyn::std::any::Any {
        self
    }
}

impl JmapType {
    pub fn new(
        s: &AccountSettings,
        is_subscribed: Box<dyn Fn(&str) -> bool + Send + Sync>,
    ) -> Result<Box<dyn MailBackend>> {
        let online = Arc::new(Mutex::new(false));
        let server_conf = JmapServerConf::new(s)?;

        Ok(Box::new(JmapType {
            connection: Arc::new(JmapConnection::new(&server_conf, online.clone())?),
            folders: Arc::new(RwLock::new(FnvHashMap::default())),
            account_name: s.name.clone(),
            online,
            is_subscribed: Arc::new(IsSubscribedFn(is_subscribed)),
            server_conf,
        }))
    }

    pub fn validate_config(s: &AccountSettings) -> Result<()> {
        get_conf_val!(s["server_hostname"])?;
        get_conf_val!(s["server_username"])?;
        get_conf_val!(s["server_password"])?;
        get_conf_val!(s["server_port"], 443)?;
        get_conf_val!(s["danger_accept_invalid_certs"], false)?;
        Ok(())
    }
}
