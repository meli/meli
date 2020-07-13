/*
 * meli - ui plugins
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

/*! Plugins are executed by meli and communication is done by `messagepack` IPC.
 */
use melib::error::{MeliError, Result};
use std::collections::HashMap;
use std::io::Write;
use std::os::unix::net::{UnixListener, UnixStream};
use std::process::Stdio;
use uuid::Uuid;

pub mod backend;
pub mod rpc;
pub use rpc::*;

pub const BACKEND_FN: i8 = 0;
pub const BACKEND_OP_FN: i8 = 1;

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum PluginKind {
    LongLived,
    Filter,
    Backend,
}

impl Default for PluginKind {
    fn default() -> Self {
        Self::LongLived
    }
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct Plugin {
    kind: PluginKind,
    executable: String,
    name: String,
    #[serde(default)]
    hooks: Vec<String>,
}

impl Plugin {
    pub fn kind(&self) -> PluginKind {
        self.kind
    }
}

#[derive(Debug)]
pub struct PluginManager {
    plugins: HashMap<String, Plugin>,
    sessions: HashMap<Uuid, String>,
    instances: HashMap<Uuid, std::process::Child>,
    streams: HashMap<Uuid, RpcChannel>,
    hooks: HashMap<String, UIHook>,
    listener: UnixListener,
}

impl Drop for PluginManager {
    fn drop(&mut self) {
        let _ = std::fs::remove_file("./soworkfile");
        for (k, c) in self.instances.iter_mut() {
            if let Err(err) = debug!(c.kill()) {
                eprintln!(
                    "Error: could not kill process {} spawned by plugin {} ({})",
                    c.id(),
                    &self.plugins[&self.sessions[k]].name,
                    err
                );
            }
        }
    }
}

impl PluginManager {
    pub fn new() -> Self {
        let _ = std::fs::remove_file("./soworkfile");
        let listener = UnixListener::bind("./soworkfile").unwrap();
        /*
            debug!("bound");
            // accept connections and process them, spawning a new thread for each one
            thread::spawn(move || {
                debug!("spawn");
                let stream = listener.accept();
                debug!("socket stream {:?}", &stream);
                match stream {
                    Ok((mut stream, _)) => {
                        debug!("socket stream {:?}", &stream);
                        /* connection succeeded */
                        thread::spawn(move || {
                            debug!("socket listen {:?}", &stream);
                            debug!(initialize(stream));
                            //let mut response = Vec::new();
                            //debug!(stream.read_to_end(&mut response));
                            //loop {
                            //    debug!("pre-flush 1");
                            //    stream.flush();
                            //    debug!("post-flush 1");
                            //    if debug!(rmpv::decode::value::read_value(&mut stream)).is_err() {
                            //        return;
                            //    }
                            //    debug!("post-read_value");
                            //    //debug!("socket response {}", unsafe {
                            //    //    String::from_utf8_lossy(&response)
                            //    //});
                            //    stream.flush();
                            //    debug!("post-flush 2");
                            //    if debug!(rmpv::encode::write_value(
                            //        &mut stream,
                            //        &rmpv::Value::String("hello 2 u 2".into())
                            //    ))
                            //    .is_err()
                            //    {
                            //        return;
                            //    }
                            //    debug!("post-write_value");
                            //}
                        });
                    }
                    Err(err) => {
                        /* connection failed */
                        debug!(err);
                    }
                }
            });
        */
        let mut hooks: HashMap<String, UIHook> = Default::default();

        hooks.insert(
            "attachment-view".to_string(),
            UIHook {
                name: "attachment-view".to_string(),
                wait_response: true,
                listeners: Vec::new(),
            },
        );

        hooks.insert(
            "refresh-account".to_string(),
            UIHook {
                name: "refresh-account".to_string(),
                wait_response: false,
                listeners: Vec::new(),
            },
        );

        PluginManager {
            plugins: Default::default(),
            sessions: Default::default(),
            instances: Default::default(),
            streams: Default::default(),
            hooks,
            listener,
        }
    }

    pub fn register(&mut self, plugin: Plugin) -> Result<()> {
        debug!(&plugin);
        match plugin.kind {
            PluginKind::LongLived => {
                /* spawn thread */
                let inv = &plugin.executable;
                let child = std::process::Command::new("sh")
                    .args(&["-c", inv])
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()?;
                let (stream, _) = self.listener.accept()?;
                /* send init message to plugin to register hooks */
                let session = Uuid::new_v4();
                let channel = RpcChannel::new(stream, &session)?;

                for h in &plugin.hooks {
                    self.add_listener(h, session);
                }

                self.instances.insert(session, child);
                self.sessions.insert(session, plugin.name.clone());
                self.streams.insert(session, channel);
                self.plugins.insert(plugin.name.clone(), plugin);
                Ok(())
            }
            PluginKind::Filter => {
                let session = Uuid::new_v4();
                for h in &plugin.hooks {
                    self.add_listener(h, session);
                }

                self.sessions.insert(session, plugin.name.clone());
                self.plugins.insert(plugin.name.clone(), plugin);
                /* send init message to plugin to register hooks */
                Ok(())
            }
            PluginKind::Backend => {
                self.plugins.insert(plugin.name.clone(), plugin);
                /* send init message to plugin to register hooks */
                Ok(())
            }
        }
    }

    pub fn register_hook(&mut self, hook: UIHook) {
        self.hooks.insert(hook.name.clone(), hook);
    }

    pub fn add_listener(&mut self, hook: &str, session: Uuid) {
        self.hooks
            .entry(hook.to_string())
            .and_modify(|entry| entry.listeners.push(session));
    }

    pub fn activate_hook(&mut self, hook: &str, bytes: Vec<u8>) -> Result<FilterResult> {
        debug!("activate_hook {}", hook);
        debug!("bytes {:?}", &bytes);
        for l in &self.hooks[hook].listeners {
            let plugin = &self.plugins[&self.sessions[l]];
            debug!(&plugin);
            match &plugin.kind {
                PluginKind::LongLived => {
                    debug!("listener: {}", l);
                    let channel = self.streams.get_mut(l).unwrap();
                    channel.write_ref(&rmpv::ValueRef::Binary(bytes.as_slice()))?;
                    let reply: Result<FilterResult> = channel.from_read();
                    return reply;
                }
                PluginKind::Filter => {
                    let inv = &plugin.executable;
                    let mut child = std::process::Command::new("sh")
                        .args(&["-c", inv])
                        .stdin(Stdio::piped())
                        .stdout(Stdio::piped())
                        .spawn()?;
                    let (stream, _) = self.listener.accept()?;
                    let mut channel = RpcChannel::new(stream, l)?;
                    channel.write_ref(&rmpv::ValueRef::Binary(bytes.as_slice()))?;
                    let reply: Result<FilterResult> = channel.from_read();
                    child.kill()?;
                    return reply;
                }
                k => {
                    debug!("got plugin kind {:?} in hook  {}", k, hook);
                }
            }
        }
        Err(MeliError::new("no listeners for this hook"))
    }

    pub fn listener(&self) -> UnixListener {
        self.listener.try_clone().unwrap()
    }
}

#[derive(Debug)]
pub struct UIHook {
    name: String,
    wait_response: bool,
    listeners: Vec<Uuid>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "t", content = "c")]
pub enum FilterResult {
    UiMessage(String),
    Text(String),
    Ansi(String),
    Binary(Vec<u8>),
    Error(String),
}
