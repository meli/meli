/*
 * meli - plugins
 *
 * Copyright 2019  Manos Pitsidianakis
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
use fnv::FnvHashMap;
use melib::async_workers::{Async, AsyncBuilder, AsyncStatus, WorkContext};
use melib::backends::FolderHash;
use melib::backends::{
    Backend, BackendOp, Backends, Folder, MailBackend, RefreshEvent, RefreshEventConsumer,
};
use melib::conf::AccountSettings;
use melib::email::{Envelope, EnvelopeHash, Flag};
use melib::error::{MeliError, Result};
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex, RwLock};

#[derive(Debug)]
pub struct PluginBackend {
    plugin: Plugin,
    child: std::process::Child,
    channel: Arc<Mutex<RpcChannel>>,
    is_online: Arc<Mutex<(std::time::Instant, Result<()>)>>,
}

impl MailBackend for PluginBackend {
    fn is_online(&self) -> Result<()> {
        if let Ok(mut is_online) = self.is_online.try_lock() {
            let now = std::time::Instant::now();
            if now.duration_since(is_online.0) >= std::time::Duration::new(2, 0) {
                let mut channel = self.channel.lock().unwrap();
                channel.write_ref(&rmpv::ValueRef::Ext(BACKEND_FN, b"is_online"))?;
                debug!(channel.expect_ack())?;
                let ret: PluginResult<()> = debug!(channel.from_read())?;
                is_online.0 = now;
                is_online.1 = ret.into();
            }
            is_online.1.clone()
        } else {
            Err(MeliError::new("busy"))
        }
    }

    fn connect(&mut self) {}

    fn get(&mut self, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        let mut w = AsyncBuilder::new();
        let folder_hash = folder.hash();
        let channel = self.channel.clone();
        let handle = {
            let tx = w.tx();
            let closure = move |_work_context| {
                let mut channel = channel.lock().unwrap();
                channel
                    .write_ref(&rmpv::ValueRef::Ext(BACKEND_FN, b"get"))
                    .unwrap();
                channel.expect_ack().unwrap();
                loop {
                    let read_val: Result<PluginResult<Option<Vec<String>>>> =
                        debug!(channel.from_read());
                    match read_val.map(Into::into).and_then(std::convert::identity) {
                        Ok(Some(a)) => {
                            tx.send(AsyncStatus::Payload(Ok(a
                                .into_iter()
                                .filter_map(|s| Envelope::from_bytes(s.as_bytes(), None).ok())
                                .collect::<Vec<Envelope>>())))
                                .unwrap();
                        }
                        Ok(None) => {
                            tx.send(AsyncStatus::Finished).unwrap();
                            return;
                        }
                        Err(err) => {
                            tx.send(AsyncStatus::Payload(Err(err))).unwrap();
                            tx.send(AsyncStatus::Finished).unwrap();
                            return;
                        }
                    };
                }
            };
            Box::new(closure)
        };
        w.build(handle)
    }

    fn refresh(
        &mut self,
        _folder_hash: FolderHash,
        _sender: RefreshEventConsumer,
    ) -> Result<Async<Result<Vec<RefreshEvent>>>> {
        Err(MeliError::new("Unimplemented."))
    }
    fn watch(
        &self,
        sender: RefreshEventConsumer,
        work_context: WorkContext,
    ) -> Result<std::thread::ThreadId> {
        Err(MeliError::new("Unimplemented."))
    }
    fn folders(&self) -> Result<FnvHashMap<FolderHash, Folder>> {
        let mut ret: FnvHashMap<FolderHash, Folder> = Default::default();
        ret.insert(0, Folder::default());
        Ok(ret)
    }
    fn operation(&self, hash: EnvelopeHash) -> Box<dyn BackendOp> {
        unimplemented!()
    }

    fn save(&self, bytes: &[u8], folder: &str, flags: Option<Flag>) -> Result<()> {
        Err(MeliError::new("Unimplemented."))
    }
    fn create_folder(&mut self, name: String) -> Result<Folder> {
        Err(MeliError::new("Unimplemented."))
    }
    fn tags(&self) -> Option<Arc<RwLock<BTreeMap<u64, String>>>> {
        None
    }
    fn as_any(&self) -> &dyn::std::any::Any {
        self
    }
}

impl PluginBackend {
    pub fn new(
        listener: UnixListener,
        plugin: Plugin,
        _s: &AccountSettings,
        _is_subscribed: Box<dyn Fn(&str) -> bool>,
    ) -> Result<Box<dyn MailBackend>> {
        if plugin.kind != PluginKind::Backend {
            return Err(MeliError::new(format!(
                "Error: Plugin `{}` is not a mail backend plugin, it's `{:?}`",
                &plugin.name, &plugin.kind
            )));
        }
        let parts = split_command!(&plugin.executable);
        let child = std::process::Command::new(&parts[0])
            .args(&parts[1..])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;
        let (mut stream, _) = listener.accept()?;
        /* send init message to plugin to register hooks */
        let session = Uuid::new_v4();
        let channel = RpcChannel::new(stream, &session)?;
        let now = std::time::Instant::now() - std::time::Duration::from_secs(5);

        Ok(Box::new(PluginBackend {
            child,
            plugin,
            channel: Arc::new(Mutex::new(channel)),
            is_online: Arc::new(Mutex::new((now, Err(MeliError::new("Unitialized"))))),
        }))
    }

    pub fn register(listener: UnixListener, plugin: Plugin, backends: &mut Backends) {
        backends.register(
            plugin.name.clone(),
            Backend {
                create_fn: Box::new(move || {
                    let plugin = plugin.clone();
                    let listener = listener.try_clone().unwrap();
                    Box::new(move |f, i| {
                        let plugin = plugin.clone();
                        let listener = listener.try_clone().unwrap();
                        PluginBackend::new(listener, plugin, f, i)
                    })
                }),
                validate_conf_fn: Box::new(|_| Ok(())),
            },
        );
    }
}
