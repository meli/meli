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

use crate::workers::WorkController;
use melib::error::{MeliError, Result};
use rmpv::{Value, ValueRef};
use std::any::TypeId;
use std::collections::HashMap;
use std::io::{self, BufRead, BufReader};
use std::io::{Read, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::thread::ThreadId;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PluginKind {
    LongLived,
    Ephemeral,
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
}

#[derive(Debug)]
pub struct PluginManager {
    plugins: HashMap<String, Plugin>,
    instances: HashMap<String, std::process::Child>,
    hooks: HashMap<String, UIHook>,
}

impl Drop for PluginManager {
    fn drop(&mut self) {
        let _ = std::fs::remove_file("./soworkfile");
    }
}

impl PluginManager {
    pub fn new() -> Self {
        let _ = std::fs::remove_file("./soworkfile");
        let listener = UnixListener::bind("./soworkfile").unwrap();
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

        PluginManager {
            plugins: Default::default(),
            instances: Default::default(),
            hooks: Default::default(),
        }
    }

    pub fn register(&mut self, plugin: Plugin) -> Result<()> {
        debug!(&plugin);
        match plugin.kind {
            PluginKind::LongLived => {
                /* spawn thread */
                let parts = split_command!(&plugin.executable);
                let mut child = std::process::Command::new(&parts[0])
                    .args(&parts[1..])
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()?;

                /* add thread to workcontroller */
                self.instances.insert(plugin.name.clone(), child);
                self.plugins.insert(plugin.name.clone(), plugin);
                /* send init message to plugin to register hooks */
                Ok(())
            }
            PluginKind::Ephemeral => {
                self.plugins.insert(plugin.name.clone(), plugin);
                /* send init message to plugin to register hooks */
                Ok(())
            }
        }
    }

    pub fn register_hook(&mut self, hook: UIHook) {
        self.hooks.insert(hook.name.clone(), hook);
    }
}

#[derive(Debug)]
pub struct UIHook {
    name: String,
    listeners: Vec<String>,
    kind: TypeId,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PluginGreeting {
    version: String,
}

pub fn initialize(mut stream: UnixStream) -> Result<()> {
    let greeting: std::result::Result<PluginGreeting, _> =
        rmp_serde::decode::from_read(&mut stream);
    match debug!(greeting) {
        Ok(greeting) => {
            if greeting.version != "dev" {
                return Err("Plugin is not compatible with our API (dev)".into());
            }
        }
        Err(err) => {
            return Err(MeliError::new(err.to_string()));
        }
    }

    loop {
        debug!("pre-flush 1");
        stream.flush();
        debug!("post-flush 1");
        if debug!(rmpv::decode::value::read_value(&mut stream)).is_err() {
            break;
        }
        debug!("post-read_value");
        //debug!("socket response {}", unsafe {
        //    String::from_utf8_lossy(&response)
        //});
        stream.flush();
        debug!("post-flush 2");
        if debug!(rmpv::encode::write_value(
            &mut stream,
            &rmpv::Value::String("hello 2 u 2".into())
        ))
        .is_err()
        {
            break;
        }
        debug!("post-write_value");
    }

    return Ok(());
}
