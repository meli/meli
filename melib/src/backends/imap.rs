/*
 * meli - imap module.
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

#[macro_use]
mod protocol_parser;
pub use protocol_parser::{UntaggedResponse::*, *};
mod folder;
pub use folder::*;
mod operations;
pub use operations::*;
mod connection;
pub use connection::*;
mod watch;
pub use watch::*;

extern crate native_tls;

use crate::async_workers::{Async, AsyncBuilder, AsyncStatus};
use crate::backends::BackendOp;
use crate::backends::FolderHash;
use crate::backends::RefreshEvent;
use crate::backends::RefreshEventKind::{self, *};
use crate::backends::{BackendFolder, Folder, FolderOperation, MailBackend, RefreshEventConsumer};
use crate::conf::AccountSettings;
use crate::email::parser::BytesExt;
use crate::email::*;
use crate::error::{MeliError, Result};
use fnv::{FnvHashMap, FnvHashSet};
use native_tls::TlsConnector;
use std::iter::FromIterator;
use std::net::SocketAddr;
use std::str::FromStr;
use std::sync::{Arc, Mutex};
pub type UID = usize;

#[derive(Debug)]
pub struct ImapType {
    account_name: String,
    server_hostname: String,
    server_username: String,
    server_password: String,
    connection: Arc<Mutex<ImapConnection>>,
    danger_accept_invalid_certs: bool,

    capabilities: FnvHashSet<Vec<u8>>,
    folders: FnvHashMap<FolderHash, ImapFolder>,
    folder_connections: FnvHashMap<FolderHash, Arc<Mutex<ImapConnection>>>,
    hash_index: Arc<Mutex<FnvHashMap<EnvelopeHash, (UID, FolderHash)>>>,
    uid_index: Arc<Mutex<FnvHashMap<usize, EnvelopeHash>>>,
}

impl MailBackend for ImapType {
    fn get(&mut self, folder: &Folder) -> Async<Result<Vec<Envelope>>> {
        macro_rules! exit_on_error {
            ($tx:expr,$($result:expr)+) => {
                $(if let Err(e) = $result {
                $tx.send(AsyncStatus::Payload(Err(e.into())));
                    std::process::exit(1);
                })+
            };
        };

        let mut w = AsyncBuilder::new();
        let handle = {
            let tx = w.tx();
            let hash_index = self.hash_index.clone();
            let uid_index = self.uid_index.clone();
            let folder_path = folder.path().to_string();
            let folder_hash = folder.hash();
            let folder_exists = self.folders[&folder_hash].exists.clone();
            let connection = self.connection.clone();
            let closure = move || {
                let connection = connection.clone();
                let tx = tx.clone();
                let mut response = String::with_capacity(8 * 1024);
                let conn = connection.lock();
                exit_on_error!(&tx, conn);
                let mut conn = conn.unwrap();
                debug!("locked for get {}", folder_path);

                exit_on_error!(&tx,
                               conn.send_command(format!("EXAMINE {}", folder_path).as_bytes())
                               conn.read_response(&mut response)
                );
                let examine_response = protocol_parser::select_response(&response)
                    .to_full_result()
                    .map_err(MeliError::from);
                exit_on_error!(&tx, examine_response);
                let mut exists: usize = match examine_response.unwrap() {
                    SelectResponse::Ok(ok) => ok.exists,
                    SelectResponse::Bad(b) => b.exists,
                };
                {
                    let mut folder_exists = folder_exists.lock().unwrap();
                    *folder_exists = exists;
                }

                while exists > 1 {
                    let mut envelopes = vec![];
                    exit_on_error!(&tx,
                                   conn.send_command(format!("UID FETCH {}:{} (FLAGS RFC822.HEADER)", std::cmp::max(exists.saturating_sub(20000), 1), exists).as_bytes())
                                   conn.read_response(&mut response)
                    );
                    debug!(
                        "fetch response is {} bytes and {} lines",
                        response.len(),
                        response.lines().collect::<Vec<&str>>().len()
                    );
                    match protocol_parser::uid_fetch_response(response.as_bytes())
                        .to_full_result()
                        .map_err(MeliError::from)
                    {
                        Ok(v) => {
                            debug!("responses len is {}", v.len());
                            for (uid, flags, b) in v {
                                if let Ok(e) = Envelope::from_bytes(&b, flags) {
                                    hash_index
                                        .lock()
                                        .unwrap()
                                        .insert(e.hash(), (uid, folder_hash));
                                    uid_index.lock().unwrap().insert(uid, e.hash());
                                    envelopes.push(e);
                                }
                            }
                        }
                        Err(e) => {
                            debug!(&e);
                            tx.send(AsyncStatus::Payload(Err(e)));
                        }
                    }
                    exists = std::cmp::max(exists.saturating_sub(20000), 1);
                    debug!("sending payload");
                    tx.send(AsyncStatus::Payload(Ok(envelopes)));
                }
                drop(conn);
                tx.send(AsyncStatus::Finished);
            };
            Box::new(closure)
        };
        w.build(handle)
    }

    fn watch(&self, sender: RefreshEventConsumer) -> Result<()> {
        let has_idle: bool = self.capabilities.contains(&b"IDLE"[0..]);
        let folders = self.imap_folders();
        let conn = self.new_connection()?;
        let main_conn = self.connection.clone();
        let hash_index = self.hash_index.clone();
        let uid_index = self.uid_index.clone();
        std::thread::Builder::new()
            .name(format!("{} imap connection", self.account_name.as_str(),))
            .spawn(move || {
                let kit = ImapWatchKit {
                    conn,
                    main_conn,
                    hash_index,
                    uid_index,
                    folders,
                    sender,
                };
                if has_idle {
                    idle(kit);
                } else {
                    poll_with_examine(kit);
                }
            })?;
        Ok(())
    }

    fn folders(&self) -> FnvHashMap<FolderHash, Folder> {
        if !self.folders.is_empty() {
            return self
                .folders
                .iter()
                .map(|(h, f)| (*h, f.clone() as Folder))
                .collect();
        }

        let mut folders: FnvHashMap<FolderHash, ImapFolder> = Default::default();
        let mut res = String::with_capacity(8 * 1024);
        let mut conn = self.connection.lock().unwrap();
        conn.send_command(b"LIST \"\" \"*\"").unwrap();
        conn.read_response(&mut res).unwrap();
        debug!("out: {}", &res);
        for l in res.lines().map(|l| l.trim()) {
            if let Ok(mut folder) =
                protocol_parser::list_folder_result(l.as_bytes()).to_full_result()
            {
                if let Some(parent) = folder.parent {
                    if folders.contains_key(&parent) {
                        folders
                            .entry(parent)
                            .and_modify(|e| e.children.push(folder.hash));
                    } else {
                        /* Insert dummy parent entry, populating only the children field. Later
                         * when we encounter the parent entry we will swap its children with
                         * dummy's */
                        folders.insert(
                            parent,
                            ImapFolder {
                                children: vec![folder.hash],
                                ..ImapFolder::default()
                            },
                        );
                    }
                }

                if folders.contains_key(&folder.hash) {
                    let entry = folders.entry(folder.hash).or_default();
                    std::mem::swap(&mut entry.children, &mut folder.children);
                    std::mem::swap(entry, &mut folder);
                } else {
                    folders.insert(folder.hash, folder);
                }
            } else {
                debug!("parse error for {:?}", l);
            }
        }
        debug!(&folders);
        folders
            .iter()
            .map(|(h, f)| (*h, f.clone() as Folder))
            .collect()
    }

    fn operation(&self, hash: EnvelopeHash, _folder_hash: FolderHash) -> Box<BackendOp> {
        let (uid, folder_hash) = self.hash_index.lock().unwrap()[&hash];
        Box::new(ImapOp::new(
            uid,
            self.folders[&folder_hash].path().to_string(),
            self.connection.clone(),
        ))
    }

    fn save(&self, bytes: &[u8], folder: &str, flags: Option<Flag>) -> Result<()> {
        let path = self
            .folders
            .values()
            .find(|v| v.name == folder)
            .ok_or(MeliError::new(""))?;
        let mut response = String::with_capacity(8 * 1024);
        let mut conn = self.connection.lock().unwrap();
        let flags = flags.unwrap_or(Flag::empty());
        conn.send_command(
            format!(
                "APPEND \"{}\" ({}) {{{}}}",
                path.path(),
                flags_to_imap_list!(flags),
                bytes.len()
            )
            .as_bytes(),
        )?;
        // wait for "+ Ready for literal data" reply
        conn.wait_for_continuation_request()?;
        conn.send_literal(bytes)?;
        conn.read_response(&mut response)?;
        Ok(())
    }

    fn folder_operation(&mut self, path: &str, op: FolderOperation) -> Result<()> {
        use FolderOperation::*;

        match (&op, self.folders.values().any(|f| f.path == path)) {
            (Create, true) => {
                return Err(MeliError::new(format!(
                    "Folder named `{}` in account `{}` already exists.",
                    path, self.account_name,
                )));
            }
            (op, false) if *op != Create => {
                return Err(MeliError::new(format!(
                    "No folder named `{}` in account `{}`",
                    path, self.account_name,
                )));
            }
            _ => {}
        }

        let mut response = String::with_capacity(8 * 1024);
        match op {
            Create => {
                let mut conn = self.connection.lock()?;
                conn.send_command(format!("CREATE \"{}\"", path,).as_bytes())?;
                conn.read_response(&mut response)?;
                conn.send_command(format!("SUBSCRIBE \"{}\"", path,).as_bytes())?;
                conn.read_response(&mut response)?;
            }
            Rename(dest) => {
                let mut conn = self.connection.lock()?;
                conn.send_command(format!("RENAME \"{}\" \"{}\"", path, dest).as_bytes())?;
                conn.read_response(&mut response)?;
            }
            Delete => {
                let mut conn = self.connection.lock()?;
                conn.send_command(format!("DELETE \"{}\"", path,).as_bytes())?;
                conn.read_response(&mut response)?;
            }
            Subscribe => {
                let mut conn = self.connection.lock()?;
                conn.send_command(format!("SUBSCRIBE \"{}\"", path,).as_bytes())?;
                conn.read_response(&mut response)?;
            }
            Unsubscribe => {
                let mut conn = self.connection.lock()?;
                conn.send_command(format!("UNSUBSCRIBE \"{}\"", path,).as_bytes())?;
                conn.read_response(&mut response)?;
            }
        }
        Ok(())
    }
}

fn lookup_ipv4(host: &str, port: u16) -> Result<SocketAddr> {
    use std::net::ToSocketAddrs;

    let addrs = (host, port).to_socket_addrs()?;
    for addr in addrs {
        if let SocketAddr::V4(_) = addr {
            return Ok(addr);
        }
    }

    Err(MeliError::new("Cannot lookup address"))
}

macro_rules! get_conf_val {
    ($s:ident[$var:literal]) => {
        $s.extra.get($var).unwrap_or_else(|| {
            eprintln!(
                "Configuration error ({}): IMAP connection requires the field `{}` set",
                $s.name.as_str(),
                $var
            );
            std::process::exit(1);
        })
    };
    ($s:ident[$var:literal], $default:expr) => {
        $s.extra
            .get($var)
            .map(|v| {
                <_>::from_str(v).unwrap_or_else(|_| {
                    eprintln!(
                        "Configuration error ({}): Invalid value for field `{}`: {}",
                        $s.name.as_str(),
                        $var,
                        v,
                    );
                    std::process::exit(1);
                })
            })
            .unwrap_or_else(|| $default)
    };
}

macro_rules! exit_on_error {
    ($s:ident, $($result:expr)+) => {
        $(if let Err(e) = $result {
            eprintln!(
                "IMAP error ({}): {}",
                $s.name.as_str(),
                e.to_string(),
            );
            std::process::exit(1);
        })+
    };
    ($s:ident returning $result:expr) => {
        match $result {
            Err(e) => {
                eprintln!(
                    "IMAP error ({}): {}",
                    $s.name.as_str(),
                    e.to_string(),
                );
                std::process::exit(1);
            },
            Ok(v) => v,
        }
    }
}
impl ImapType {
    pub fn new(s: &AccountSettings, is_subscribed: Box<Fn(&str) -> bool>) -> Self {
        use std::io::prelude::*;
        use std::net::TcpStream;
        debug!(s);
        let path = get_conf_val!(s["server_hostname"]);
        let danger_accept_invalid_certs: bool =
            get_conf_val!(s["danger_accept_invalid_certs"], false);

        let mut connector = TlsConnector::builder();
        if danger_accept_invalid_certs {
            connector.danger_accept_invalid_certs(true);
        }
        let connector = connector.build().unwrap();

        let addr = if let Ok(a) = lookup_ipv4(path, 143) {
            a
        } else {
            eprintln!("Could not lookup address {}", &path);
            std::process::exit(1);
        };

        let mut socket = TcpStream::connect(&addr);
        let cmd_id = 0;
        exit_on_error!(
            s,
            socket
            socket.as_mut().unwrap().write_all(format!("M{} STARTTLS\r\n", cmd_id).as_bytes())
        );
        let mut socket = exit_on_error!(s returning socket);
        // FIXME handle response properly
        let mut buf = vec![0; 1024];
        let mut response = String::with_capacity(1024);
        let mut cap_flag = false;
        loop {
            let len = socket.read(&mut buf).unwrap();
            response.push_str(unsafe { std::str::from_utf8_unchecked(&buf[0..len]) });
            if !cap_flag {
                if response.starts_with("* OK [CAPABILITY") && response.find("\r\n").is_some() {
                    if let Some(pos) = response.as_bytes().find(b"\r\n") {
                        response.drain(0..pos + 2);
                        cap_flag = true;
                    }
                } else if response.starts_with("* OK ") && response.find("\r\n").is_some() {
                    if let Some(pos) = response.as_bytes().find(b"\r\n") {
                        response.drain(0..pos + 2);
                    }
                }
            }
            if cap_flag && response == "M0 OK Begin TLS negotiation now.\r\n" {
                break;
            }
        }

        socket
            .set_nonblocking(true)
            .expect("set_nonblocking call failed");
        socket
            .set_read_timeout(Some(std::time::Duration::new(120, 0)))
            .unwrap();
        let stream = {
            let mut conn_result = connector.connect(path, socket);
            if let Err(native_tls::HandshakeError::WouldBlock(midhandshake_stream)) = conn_result {
                let mut midhandshake_stream = Some(midhandshake_stream);
                loop {
                    match midhandshake_stream.take().unwrap().handshake() {
                        Ok(r) => {
                            conn_result = Ok(r);
                            break;
                        }
                        Err(native_tls::HandshakeError::WouldBlock(stream)) => {
                            midhandshake_stream = Some(stream);
                        }
                        p => {
                            p.unwrap();
                        }
                    }
                }
            }
            conn_result.unwrap()
        };

        let mut m = ImapType {
            account_name: s.name().to_string(),
            server_hostname: get_conf_val!(s["server_hostname"]).to_string(),
            server_username: get_conf_val!(s["server_username"]).to_string(),
            server_password: get_conf_val!(s["server_password"]).to_string(),
            folders: Default::default(),
            connection: Arc::new(Mutex::new(ImapConnection { cmd_id, stream })),
            danger_accept_invalid_certs,
            folder_connections: Default::default(),
            hash_index: Default::default(),
            uid_index: Default::default(),
            capabilities: Default::default(),
        };

        let mut res = String::with_capacity(8 * 1024);
        let mut conn = exit_on_error!(s returning m.connection.lock());
        exit_on_error!(s,
                       conn.send_command( format!( "LOGIN \"{}\" \"{}\"", get_conf_val!(s["server_username"]), get_conf_val!(s["server_password"])).as_bytes())
                       conn.read_lines(&mut res, String::new())
                       std::io::stderr().write(res.as_bytes())
        );
        m.capabilities = match protocol_parser::capabilities(res.as_bytes())
            .to_full_result()
            .map_err(MeliError::from)
        {
            Ok(c) => {
                eprintln!("cap len {}", c.len());

                FnvHashSet::from_iter(c.into_iter().map(|s| s.to_vec()))
            }
            Err(e) => {
                eprintln!(
                    "Could not login in account `{}`: {}",
                    m.account_name.as_str(),
                    e
                );
                std::process::exit(1);
            }
        };
        debug!(m
            .capabilities
            .iter()
            .map(|s| String::from_utf8(s.to_vec()).unwrap())
            .collect::<Vec<String>>());
        drop(conn);

        m.folders = m.imap_folders();
        m.folders.retain(|_, f| is_subscribed(f.path()));
        let keys = m
            .folders
            .keys()
            .cloned()
            .collect::<FnvHashSet<FolderHash>>();
        for f in m.folders.values_mut() {
            f.children.retain(|c| keys.contains(c));
        }
        /*
        for f in m.folders.keys() {
            m.folder_connections.insert(
                *f,
                Arc::new(Mutex::new(exit_on_error!(s returning m.new_connection()))),
            );
        }*/
        m
    }

    pub fn shell(&mut self) {
        let mut conn = self.connection.lock().unwrap();
        let mut res = String::with_capacity(8 * 1024);

        let mut input = String::new();
        loop {
            use std::io;
            input.clear();

            match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    conn.send_command(input.as_bytes()).unwrap();
                    conn.read_response(&mut res).unwrap();
                    debug!("out: {}", &res);
                    if input.trim().eq_ignore_ascii_case("logout") {
                        break;
                    }
                }
                Err(error) => debug!("error: {}", error),
            }
        }
    }

    fn new_connection(&self) -> Result<ImapConnection> {
        use std::io::prelude::*;
        use std::net::TcpStream;
        let path = &self.server_hostname;

        let mut connector = TlsConnector::builder();
        if self.danger_accept_invalid_certs {
            connector.danger_accept_invalid_certs(true);
        }
        let connector = connector.build()?;

        let addr = if let Ok(a) = lookup_ipv4(path, 143) {
            a
        } else {
            return Err(MeliError::new(format!(
                "Could not lookup address {}",
                &path
            )));
        };

        let mut socket = TcpStream::connect(&addr)?;
        let cmd_id = 0;
        socket.write_all(format!("M{} STARTTLS\r\n", cmd_id).as_bytes())?;

        let mut buf = vec![0; 1024];
        let mut response = String::with_capacity(1024);
        let mut cap_flag = false;
        loop {
            let len = socket.read(&mut buf)?;
            response.push_str(unsafe { std::str::from_utf8_unchecked(&buf[0..len]) });
            if !cap_flag {
                if response.starts_with("* OK [CAPABILITY") && response.find("\r\n").is_some() {
                    if let Some(pos) = response.as_bytes().find(b"\r\n") {
                        response.drain(0..pos + 2);
                        cap_flag = true;
                    }
                } else if response.starts_with("* OK ") && response.find("\r\n").is_some() {
                    if let Some(pos) = response.as_bytes().find(b"\r\n") {
                        response.drain(0..pos + 2);
                    }
                }
            }
            if cap_flag && response == "M0 OK Begin TLS negotiation now.\r\n" {
                break;
            }
        }

        socket
            .set_nonblocking(true)
            .expect("set_nonblocking call failed");
        socket.set_read_timeout(Some(std::time::Duration::new(120, 0)))?;
        let stream = {
            let mut conn_result = connector.connect(path, socket);
            if let Err(native_tls::HandshakeError::WouldBlock(midhandshake_stream)) = conn_result {
                let mut midhandshake_stream = Some(midhandshake_stream);
                loop {
                    match midhandshake_stream.take().unwrap().handshake() {
                        Ok(r) => {
                            conn_result = Ok(r);
                            break;
                        }
                        Err(native_tls::HandshakeError::WouldBlock(stream)) => {
                            midhandshake_stream = Some(stream);
                        }
                        p => {
                            p?;
                        }
                    }
                }
            }
            conn_result?
        };
        let mut ret = ImapConnection { cmd_id, stream };
        ret.send_command(
            format!(
                "LOGIN \"{}\" \"{}\"",
                &self.server_username, &self.server_password
            )
            .as_bytes(),
        )?;
        Ok(ret)
    }

    pub fn imap_folders(&self) -> FnvHashMap<FolderHash, ImapFolder> {
        let mut folders: FnvHashMap<FolderHash, ImapFolder> = Default::default();
        let mut res = String::with_capacity(8 * 1024);
        let mut conn = self.connection.lock().unwrap();
        conn.send_command(b"LIST \"\" \"*\"").unwrap();
        conn.read_response(&mut res).unwrap();
        debug!("out: {}", &res);
        for l in res.lines().map(|l| l.trim()) {
            if let Ok(mut folder) =
                protocol_parser::list_folder_result(l.as_bytes()).to_full_result()
            {
                if let Some(parent) = folder.parent {
                    if folders.contains_key(&parent) {
                        folders
                            .entry(parent)
                            .and_modify(|e| e.children.push(folder.hash));
                    } else {
                        /* Insert dummy parent entry, populating only the children field. Later
                         * when we encounter the parent entry we will swap its children with
                         * dummy's */
                        folders.insert(
                            parent,
                            ImapFolder {
                                children: vec![folder.hash],
                                ..ImapFolder::default()
                            },
                        );
                    }
                }
                if folders.contains_key(&folder.hash) {
                    let entry = folders.entry(folder.hash).or_default();
                    std::mem::swap(&mut entry.children, &mut folder.children);
                    *entry = folder;
                } else {
                    folders.insert(folder.hash, folder);
                }
            } else {
                debug!("parse error for {:?}", l);
            }
        }
        debug!(folders)
    }
}
