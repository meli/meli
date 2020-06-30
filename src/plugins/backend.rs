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
use melib::async_workers::{Async, AsyncBuilder, AsyncStatus, WorkContext};
use melib::backends::*;
use melib::conf::AccountSettings;
use melib::email::{Envelope, EnvelopeHash, Flag};
use melib::error::{MeliError, Result};
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::future::Future;
use std::sync::{Arc, Mutex, RwLock};

// TODO replace with melib::Envelope after simplifying melib::Envelope's
// fields/interface/deserializing
#[derive(Debug, Clone, Serialize, Deserialize)]
struct SimpleEnvelope {
    hash: EnvelopeHash,
    subject: String,
    from: String,
    to: String,
    date: String,
    message_id: String,
    references: String,
}

#[derive(Debug)]
pub struct PluginBackend {
    plugin: Plugin,
    child: std::process::Child,
    channel: Arc<Mutex<RpcChannel>>,
    tag_index: Option<Arc<RwLock<BTreeMap<u64, String>>>>,
    is_online: Arc<Mutex<(std::time::Instant, Result<()>)>>,
}

impl Drop for PluginBackend {
    fn drop(&mut self) {
        if let Err(err) = debug!(self.child.kill()) {
            eprintln!(
                "Error: could not kill process {} spawned by plugin {} ({})",
                self.child.id(),
                &self.plugin.name,
                err
            );
        }
    }
}

impl MailBackend for PluginBackend {
    fn is_online(&self) -> Result<()> {
        if let Ok(mut is_online) = self.is_online.try_lock() {
            let now = std::time::Instant::now();
            if now.duration_since(is_online.0) >= std::time::Duration::new(2, 0) {
                if let Ok(mut channel) = self.channel.try_lock() {
                    channel.write_ref(&rmpv::ValueRef::Ext(BACKEND_FN, b"is_online"))?;
                    debug!(channel.expect_ack())?;
                    let ret: PluginResult<()> = debug!(channel.from_read())?;
                    is_online.0 = now;
                    is_online.1 = ret.into();
                }
            }
            is_online.1.clone()
        } else {
            Err(MeliError::new("busy"))
        }
    }

    fn connect(&mut self) {}

    fn get(&mut self, mailbox: &Mailbox) -> Async<Result<Vec<Envelope>>> {
        let mut w = AsyncBuilder::new();
        let _mailbox_hash = mailbox.hash();
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
                    let read_val: Result<PluginResult<Option<Vec<SimpleEnvelope>>>> =
                        channel.from_read();
                    match read_val.map(Into::into).and_then(std::convert::identity) {
                        Ok(Some(a)) => {
                            tx.send(AsyncStatus::Payload(Ok(a
                                .into_iter()
                                .filter_map(
                                    |SimpleEnvelope {
                                         hash,
                                         date,
                                         from,
                                         to,
                                         subject,
                                         message_id,
                                         references,
                                     }| {
                                        let mut env = melib::Envelope::new(hash);
                                        env.set_date(date.as_bytes());
                                        if let Ok(d) =
                                            melib::email::parser::generic::date(date.as_bytes())
                                        {
                                            env.set_datetime(d);
                                        }
                                        env.set_message_id(message_id.as_bytes());
                                        let parse_result =
                                            melib::email::parser::address::rfc2822address_list(
                                                from.as_bytes(),
                                            );
                                        if parse_result.is_ok() {
                                            let value = parse_result.unwrap().1;
                                            env.set_from(value);
                                        }
                                        let parse_result =
                                            melib::email::parser::address::rfc2822address_list(
                                                to.as_bytes(),
                                            );
                                        if parse_result.is_ok() {
                                            let value = parse_result.unwrap().1;
                                            env.set_to(value);
                                        }
                                        let parse_result = melib::email::parser::encodings::phrase(
                                            subject.as_bytes(),
                                            false,
                                        );
                                        if parse_result.is_ok() {
                                            let value = parse_result.unwrap().1;
                                            env.set_subject(value);
                                        }
                                        if !references.is_empty() {
                                            let parse_result =
                                                melib::email::parser::address::references(
                                                    references.as_bytes(),
                                                );
                                            if parse_result.is_ok() {
                                                for v in parse_result.unwrap().1 {
                                                    env.push_references(v);
                                                }
                                            }
                                            env.set_references(references.as_bytes());
                                        }

                                        Some(env)
                                    },
                                )
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
        _mailbox_hash: MailboxHash,
        _sender: RefreshEventConsumer,
    ) -> Result<Async<()>> {
        Err(MeliError::new("Unimplemented."))
    }
    fn watch(
        &self,
        _sender: RefreshEventConsumer,
        _work_context: WorkContext,
    ) -> Result<std::thread::ThreadId> {
        Err(MeliError::new("Unimplemented."))
    }

    fn mailboxes(&self) -> Result<HashMap<MailboxHash, Mailbox>> {
        let mut ret: HashMap<MailboxHash, Mailbox> = Default::default();
        ret.insert(0, Mailbox::default());
        Ok(ret)
    }

    fn operation(&self, hash: EnvelopeHash) -> Result<Box<dyn BackendOp>> {
        Ok(Box::new(PluginOp {
            hash,
            channel: self.channel.clone(),
            tag_index: self.tag_index.clone(),
            bytes: None,
        }))
    }

    fn save(
        &self,
        _bytes: Vec<u8>,
        _mailbox_hash: MailboxHash,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }
    fn create_mailbox(
        &mut self,
        _name: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        Err(MeliError::new("Unimplemented."))
    }
    fn tags(&self) -> Option<Arc<RwLock<BTreeMap<u64, String>>>> {
        self.tag_index.clone()
    }
    fn as_any(&self) -> &dyn ::std::any::Any {
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
        let inv = &plugin.executable;
        let child = std::process::Command::new("sh")
            .args(&["-c", inv])
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()?;
        let (stream, _) = listener.accept()?;
        /* send init message to plugin to register hooks */
        let session = Uuid::new_v4();
        let channel = RpcChannel::new(stream, &session)?;
        let now = std::time::Instant::now() - std::time::Duration::from_secs(5);

        Ok(Box::new(PluginBackend {
            child,
            plugin,
            channel: Arc::new(Mutex::new(channel)),
            tag_index: None,
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

#[derive(Debug)]
struct PluginOp {
    hash: EnvelopeHash,
    channel: Arc<Mutex<RpcChannel>>,
    tag_index: Option<Arc<RwLock<BTreeMap<u64, String>>>>,
    bytes: Option<String>,
}

impl BackendOp for PluginOp {
    fn as_bytes(&mut self) -> Result<&[u8]> {
        if let Some(ref bytes) = self.bytes {
            return Ok(bytes.as_bytes());
        }

        if let Ok(mut channel) = self.channel.try_lock() {
            channel.write_ref(&rmpv::ValueRef::Ext(BACKEND_OP_FN, b"as_bytes"))?;
            debug!(channel.expect_ack())?;
            channel.write_ref(&rmpv::ValueRef::Integer(self.hash.into()))?;
            debug!(channel.expect_ack())?;
            let bytes: Result<PluginResult<String>> = channel.from_read();
            self.bytes = Some(bytes.map(Into::into).and_then(std::convert::identity)?);
            Ok(self.bytes.as_ref().map(String::as_bytes).unwrap())
        } else {
            Err(MeliError::new("busy"))
        }
    }

    fn fetch_flags(&self) -> Result<Flag> {
        Err(MeliError::new("Unimplemented."))
    }

    fn set_flag(
        &mut self,
        _f: Flag,
        _value: bool,
    ) -> Result<Pin<Box<dyn Future<Output = Result<()>> + Send>>> {
        Err(MeliError::new("Unimplemented."))
    }

    fn set_tag(
        &mut self,
        _tag: String,
        _value: bool,
    ) -> Result<Pin<Box<dyn Future<Output = Result<()>> + Send>>> {
        Err(MeliError::new("Unimplemented."))
    }
}
