/*
 * meli - mailbox module.
 *
 * Copyright 2017 Manos Pitsidianakis
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

//! # Maildir Backend
//!
//! This module implements a maildir backend according to the maildir
//! specification. <https://cr.yp.to/proto/maildir.html>

use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs,
    io::{self, Read, Write},
    os::unix::fs::PermissionsExt,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    time::Duration,
};

use futures::{channel::mpsc, SinkExt};
use notify::Watcher;
use regex::Regex;

pub mod utilities;
pub mod watch;

#[cfg(test)]
mod tests;

use utilities::{
    HashIndex, HashIndexes, MaildirFilePathExt, MaildirMailbox, MaildirMailboxPathExt, MaildirOp,
    PathMod,
};

use crate::{
    backends::{prelude::*, RefreshEventKind::*},
    error::{Error, ErrorKind, IntoError, Result, ResultIntoError},
    utils::shellexpand::ShellExpandTrait,
};

#[derive(Debug, Default)]
pub struct Configuration {
    pub rename_regex: Option<Regex>,
    pub path: PathBuf,
    pub root_mailbox_name: String,
    /// Is `root_mailbox` a valid maildir folder or just a folder containing
    /// valid maildir folders?
    pub is_root_a_mailbox: bool,
    pub settings: AccountSettings,
}

impl Configuration {
    pub fn new(settings: &AccountSettings) -> Result<Self> {
        let rename_regex = if let Some(v) = settings.extra.get("rename_regex").map(|v| {
            Regex::new(v).map_err(|e| {
                Error::new(format!(
                    "Configuration error ({}): Invalid value for field `{}`: {}",
                    settings.name.as_str(),
                    "rename_regex",
                    v,
                ))
                .set_source(Some(crate::src_err_arc_wrap!(e)))
                .set_kind(ErrorKind::ValueError)
            })
        }) {
            Some(v?)
        } else {
            None
        };

        Ok(Self {
            rename_regex,
            settings: settings.clone(),
            ..Self::default()
        })
    }
}

/// The maildir backend instance type.
#[derive(Debug)]
pub struct MaildirType {
    pub account_name: String,
    pub account_hash: AccountHash,
    pub mailboxes: HashMap<MailboxHash, MaildirMailbox>,
    pub mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
    pub hash_indexes: HashIndexes,
    pub event_consumer: BackendEventConsumer,
    pub is_subscribed: IsSubscribedFn,
    pub collection: Collection,
    pub config: Arc<Configuration>,
}

impl MailBackend for MaildirType {
    fn capabilities(&self) -> MailBackendCapabilities {
        const CAPABILITIES: MailBackendCapabilities = MailBackendCapabilities {
            is_async: false,
            is_remote: false,
            supports_search: false,
            extensions: None,
            supports_tags: false,
            supports_submission: false,
            extra_submission_headers: &[],
            metadata: None,
        };
        CAPABILITIES
    }

    fn is_online(&self) -> ResultFuture<()> {
        Ok(Box::pin(async { Ok(()) }))
    }

    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        let res = Ok(self
            .mailboxes
            .iter()
            .map(|(h, f)| (*h, BackendMailbox::clone(f)))
            .collect());
        Ok(Box::pin(async { res }))
    }

    fn fetch(&mut self, mailbox_hash: MailboxHash) -> ResultStream<Vec<Envelope>> {
        let mailbox: &MaildirMailbox = &self.mailboxes[&mailbox_hash];
        let unseen = mailbox.unseen.clone();
        let total = mailbox.total.clone();
        let mut path: PathBuf = mailbox.fs_path().into();
        let map = self.hash_indexes.clone();
        let mailbox_index = self.mailbox_index.clone();
        let chunk_size = 2048;
        path.push("new");
        for p in path.read_dir()?.flatten() {
            utilities::move_to_cur(&self.config, &p.path()).ok().take();
        }
        path.pop();
        path.push("cur");
        let files: Vec<PathBuf> = path
            .read_dir()?
            .flatten()
            .map(|e| e.path())
            .collect::<Vec<_>>();
        async fn fetch(
            chunk: Vec<std::path::PathBuf>,
            mailbox_hash: MailboxHash,
            unseen: Arc<Mutex<usize>>,
            total: Arc<Mutex<usize>>,
            map: HashIndexes,
            mailbox_index: Arc<Mutex<HashMap<EnvelopeHash, MailboxHash>>>,
        ) -> Result<Option<Vec<Envelope>>> {
            let mut local_r: Vec<Envelope> = Vec::with_capacity(chunk.len());
            let mut unseen_total: usize = 0;
            let mut buf = Vec::with_capacity(4096);
            for file in chunk {
                let env_hash = file.to_envelope_hash();
                {
                    map.lock()
                        .unwrap()
                        .entry(mailbox_hash)
                        .or_default()
                        .insert(env_hash, PathBuf::from(&file).into());
                }
                let mut reader = io::BufReader::new(fs::File::open(&file)?);
                buf.clear();
                reader.read_to_end(&mut buf)?;
                match Envelope::from_bytes(buf.as_slice(), Some(file.flags())) {
                    Ok(mut env) => {
                        env.set_hash(env_hash);
                        mailbox_index.lock().unwrap().insert(env_hash, mailbox_hash);
                        if !env.is_seen() {
                            unseen_total += 1;
                        }
                        local_r.push(env);
                    }
                    Err(err) => {
                        debug!(
                            "DEBUG: hash {}, path: {} couldn't be parsed, {}",
                            env_hash,
                            file.as_path().display(),
                            err,
                        );
                        continue;
                    }
                }
            }
            *total.lock().unwrap() += local_r.len();
            *unseen.lock().unwrap() += unseen_total;
            if local_r.is_empty() {
                Ok(None)
            } else {
                Ok(Some(local_r))
            }
        }
        Ok(Box::pin(try_fn_stream(|emitter| async move {
            for chunk in files.chunks(chunk_size) {
                if let Some(res) = fetch(
                    chunk.to_vec(),
                    mailbox_hash,
                    unseen.clone(),
                    total.clone(),
                    map.clone(),
                    mailbox_index.clone(),
                )
                .await
                .map_err(|err| {
                    log::debug!("fetch err {:?}", &err);
                    err
                })? {
                    emitter.emit(res).await;
                }
            }
            Ok(())
        })))
    }

    fn refresh(&mut self, mailbox_hash: MailboxHash) -> ResultFuture<()> {
        let account_hash = self.account_hash;
        let sender = self.event_consumer.clone();

        let mailbox: &MaildirMailbox = &self.mailboxes[&mailbox_hash];
        let path: PathBuf = mailbox.fs_path().into();
        let map = self.hash_indexes.clone();
        let mailbox_index = self.mailbox_index.clone();
        let config = self.config.clone();

        Ok(Box::pin(async move {
            let thunk = move |sender: &BackendEventConsumer| {
                log::trace!("refreshing {:?}", mailbox_hash);
                let mut buf = Vec::with_capacity(4096);
                let files = Self::list_mail_in_maildir_fs(&config, path.clone(), false)?;
                let mut current_hashes = {
                    let mut map = map.lock().unwrap();
                    let map = map.entry(mailbox_hash).or_default();
                    map.keys().cloned().collect::<HashSet<EnvelopeHash>>()
                };
                for file in files {
                    let hash = file.to_envelope_hash();
                    {
                        let mut map = map.lock().unwrap();
                        let map = map.entry(mailbox_hash).or_default();
                        if map.contains_key(&hash) {
                            map.remove(&hash);
                            current_hashes.remove(&hash);
                            continue;
                        }
                        (*map).insert(hash, PathBuf::from(&file).into());
                    }
                    let mut reader = io::BufReader::new(fs::File::open(&file)?);
                    buf.clear();
                    reader.read_to_end(&mut buf)?;
                    if let Ok(mut env) = Envelope::from_bytes(buf.as_slice(), Some(file.flags())) {
                        env.set_hash(hash);
                        mailbox_index
                            .lock()
                            .unwrap()
                            .insert(env.hash(), mailbox_hash);
                        (sender)(
                            account_hash,
                            BackendEvent::Refresh(RefreshEvent {
                                account_hash,
                                mailbox_hash,
                                kind: Create(Box::new(env)),
                            }),
                        );
                    } else {
                        log::trace!(
                            "DEBUG: hash {}, path: {} couldn't be parsed",
                            hash,
                            file.as_path().display()
                        );
                        continue;
                    }
                }
                for ev in current_hashes.into_iter().map(|h| {
                    BackendEvent::Refresh(RefreshEvent {
                        account_hash,
                        mailbox_hash,
                        kind: Remove(h),
                    })
                }) {
                    (sender)(account_hash, ev);
                }
                Ok(())
            };
            if let Err(err) = thunk(&sender) {
                (sender)(
                    account_hash,
                    BackendEvent::Refresh(RefreshEvent {
                        account_hash,
                        mailbox_hash,
                        kind: Failure(err),
                    }),
                );
            }
            Ok(())
        }))
    }

    fn watch(&self) -> ResultStream<BackendEvent> {
        let root_mailbox = self.config.path.to_path_buf();
        let (mut tx, rx) = mpsc::channel(16);
        let watcher = {
            let watcher = notify::RecommendedWatcher::new(
                move |res| {
                    futures::executor::block_on(async {
                        _ = tx.send(res).await;
                    })
                },
                notify::Config::default().with_poll_interval(Duration::from_secs(2)),
            );

            watcher
                .and_then(|mut watcher| {
                    watcher.watch(&root_mailbox, notify::RecursiveMode::Recursive)?;
                    Ok(Box::new(watcher))
                })
                .map_err(|err| err.set_err_details("Failed to create file change monitor."))?
        };
        let mailbox_counts = self
            .mailboxes
            .iter()
            .map(|(&k, v)| (k, (v.unseen.clone(), v.total.clone())))
            .collect::<HashMap<MailboxHash, (Arc<Mutex<usize>>, Arc<Mutex<usize>>)>>();
        let watch_state = watch::MaildirWatch {
            watcher,
            account_hash: self.account_hash,
            rx,
            hash_indexes: self.hash_indexes.clone(),
            mailbox_index: self.mailbox_index.clone(),
            mailbox_counts,
            config: self.config.clone(),
        };
        let stream = watch_state.watch();
        Ok(Box::pin(stream))
    }

    fn envelope_bytes_by_hash(&self, hash: EnvelopeHash) -> ResultFuture<Vec<u8>> {
        let op = MaildirOp::new(
            hash,
            self.hash_indexes.clone(),
            self.mailbox_index.lock().unwrap()[&hash],
        );

        Ok(Box::pin(async move { op.as_bytes().await }))
    }

    fn save(
        &self,
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        flags: Option<Flag>,
    ) -> ResultFuture<()> {
        let path = self.mailboxes[&mailbox_hash].fs_path.clone();
        Ok(Box::pin(async move {
            _ = Self::save_to_mailbox(path, bytes, flags)?;
            Ok(())
        }))
    }

    fn set_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flag_ops: SmallVec<[FlagOp; 8]>,
    ) -> ResultFuture<()> {
        if flag_ops.iter().any(|op| op.is_tag()) {
            return Err(Error::new("Maildir doesn't support tags."));
        }
        let hash_index = self.hash_indexes.clone();
        let config = self.config.clone();

        Ok(Box::pin(async move {
            let mut hash_indexes_lck = hash_index.lock().unwrap();
            let hash_index = hash_indexes_lck.entry(mailbox_hash).or_default();

            for env_hash in env_hashes.iter() {
                let path = {
                    if !hash_index.contains_key(&env_hash) {
                        continue;
                    }
                    if let Some(modif) = &hash_index[&env_hash].modified {
                        match modif {
                            PathMod::Path(ref path) => path.clone(),
                            PathMod::Hash(hash) => hash_index[hash].to_path_buf(),
                        }
                    } else {
                        hash_index[&env_hash].to_path_buf()
                    }
                };
                let mut new_flags = path.flags();
                for op in flag_ops.iter() {
                    if let FlagOp::Set(f) | FlagOp::UnSet(f) = op {
                        new_flags.set(*f, op.as_bool());
                    }
                }

                let new_name: PathBuf = path.set_flags(new_flags, &config)?;
                hash_index.entry(env_hash).or_default().modified =
                    Some(PathMod::Path(new_name.clone()));

                log::debug!("renaming {:?} to {:?}", path, new_name);
                fs::rename(path, &new_name)?;
                log::debug!("success in rename");
            }
            Ok(())
        }))
    }

    fn delete_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        let hash_index = self.hash_indexes.clone();
        Ok(Box::pin(async move {
            let mut hash_indexes_lck = hash_index.lock().unwrap();
            let hash_index = hash_indexes_lck.entry(mailbox_hash).or_default();

            for env_hash in env_hashes.iter() {
                let path = {
                    if !hash_index.contains_key(&env_hash) {
                        continue;
                    }
                    if let Some(modif) = &hash_index[&env_hash].modified {
                        match modif {
                            PathMod::Path(ref path) => path.clone(),
                            PathMod::Hash(hash) => hash_index[hash].to_path_buf(),
                        }
                    } else {
                        hash_index[&env_hash].to_path_buf()
                    }
                };

                fs::remove_file(&path)?;
            }
            Ok(())
        }))
    }

    fn copy_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        source_mailbox_hash: MailboxHash,
        destination_mailbox_hash: MailboxHash,
        move_: bool,
    ) -> ResultFuture<()> {
        if !self.mailboxes.contains_key(&source_mailbox_hash) {
            return Err(Error::new("Invalid source mailbox hash").set_kind(ErrorKind::Bug));
        } else if !self.mailboxes.contains_key(&destination_mailbox_hash) {
            return Err(Error::new("Invalid destination mailbox hash").set_kind(ErrorKind::Bug));
        }
        let hash_index = self.hash_indexes.clone();
        let config = self.config.clone();
        let mut dest_dir: PathBuf = self.mailboxes[&destination_mailbox_hash].fs_path().into();
        dest_dir.push("cur");
        Ok(Box::pin(async move {
            let mut hash_indexes_lck = hash_index.lock().unwrap();
            let hash_index = hash_indexes_lck.entry(source_mailbox_hash).or_default();

            for env_hash in env_hashes.iter() {
                let path_src = {
                    if !hash_index.contains_key(&env_hash) {
                        continue;
                    }
                    if let Some(modif) = &hash_index[&env_hash].modified {
                        match modif {
                            PathMod::Path(ref path) => path.clone(),
                            PathMod::Hash(hash) => hash_index[hash].to_path_buf(),
                        }
                    } else {
                        hash_index[&env_hash].to_path_buf()
                    }
                };
                let dest_path = path_src.place_in_dir(&dest_dir, &config)?;
                hash_index.entry(env_hash).or_default().modified =
                    Some(PathMod::Path(dest_path.clone()));
                if move_ {
                    log::trace!("renaming {:?} to {:?}", path_src, dest_path);
                    fs::rename(&path_src, &dest_path)
                        .chain_err_summary(|| {
                            format!(
                                "Could not rename {} to {}",
                                path_src.display(),
                                dest_path.display()
                            )
                        })
                        .chain_err_related_path(&path_src)?;
                    log::trace!("success in rename");
                } else {
                    log::trace!("copying {:?} to {:?}", path_src, dest_path);
                    fs::copy(&path_src, &dest_path)
                        .chain_err_summary(|| {
                            format!(
                                "Could not copy {} to {}",
                                path_src.display(),
                                dest_path.display()
                            )
                        })
                        .chain_err_related_path(&path_src)?;
                    log::trace!("success in copy");
                }
            }
            Ok(())
        }))
    }

    fn collection(&self) -> Collection {
        self.collection.clone()
    }

    fn create_mailbox(
        &mut self,
        name: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        let (mut fs_path, suffix) =
            match Self::mailbox_path_to_fs_path_and_suffix(&self.config, &name) {
                Ok(v) => v,
                Err(err) => {
                    return Ok(Box::pin(async move { Err(err) }));
                }
            };
        if let Err(err) = std::fs::create_dir(&fs_path)
            .chain_err_summary(|| "Could not create new mailbox")
            .chain_err_related_path(&fs_path)
        {
            return Ok(Box::pin(async move { Err(err) }));
        };
        let mut undo_ops = VecDeque::new();
        let mut push_undo_op = |fs_path: &Path| {
            undo_ops.push_back(Box::new({
                let fs_path = fs_path.to_path_buf();
                move || -> Result<()> {
                    std::fs::remove_dir(&fs_path)
                        .chain_err_summary(|| "Could not cleanup filesystem folder")
                        .chain_err_related_path(&fs_path)?;
                    Ok(())
                }
            }));
        };
        push_undo_op(&fs_path);
        for d in &["cur", "new", "tmp"] {
            fs_path.push(d);
            if let Err(err) = std::fs::create_dir(&fs_path)
                .chain_err_summary(|| "Could not create new mailbox")
                .chain_err_related_path(&fs_path)
            {
                while let Some(op) = undo_ops.pop_back() {
                    if let Err(err) = op() {
                        log::error!("{}", err);
                    }
                }
                return Ok(Box::pin(async move { Err(err) }));
            };
            push_undo_op(&fs_path);
            fs_path.pop();
        }
        let mailbox_hash = match self.mailbox_from_path(fs_path, suffix) {
            Ok(v) => v,
            Err(err) => {
                while let Some(op) = undo_ops.pop_back() {
                    if let Err(err) = op() {
                        log::error!("{}", err);
                    }
                }
                return Ok(Box::pin(async move { Err(err) }));
            }
        };
        let mailboxes_fut = self.mailboxes()?;
        Ok(Box::pin(
            async move { Ok((mailbox_hash, mailboxes_fut.await?)) },
        ))
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(
            Error::new("Deleting mailboxes is currently unimplemented for maildir backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: bool,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Mailbox subscriptions are not possible for the maildir backend.",
        ))
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(
            Error::new("Renaming mailboxes is currently unimplemented for maildir backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: crate::backends::MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(Error::new(
            "Setting mailbox permissions is not possible for the maildir backend.",
        ))
    }

    fn search(
        &self,
        _query: crate::search::Query,
        _mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<Vec<EnvelopeHash>> {
        Err(
            Error::new("Search is unimplemented for the maildir backend.")
                .set_kind(ErrorKind::NotImplemented),
        )
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

impl MaildirType {
    pub fn new(
        settings: &AccountSettings,
        is_subscribed: IsSubscribedFn,
        event_consumer: BackendEventConsumer,
    ) -> Result<Box<Self>> {
        let mut mailboxes: HashMap<MailboxHash, MaildirMailbox> = Default::default();
        fn recurse_mailboxes<P: AsRef<Path>>(
            mailboxes: &mut HashMap<MailboxHash, MaildirMailbox>,
            config: &Configuration,
            p: P,
        ) -> Result<Vec<MailboxHash>> {
            if !p.as_ref().try_exists().unwrap_or(false) || !p.as_ref().is_dir() {
                return Err(Error::new(format!(
                    "Configuration error: Path \"{}\" {}",
                    p.as_ref().display(),
                    if !p.as_ref().try_exists().unwrap_or(false) {
                        "does not exist."
                    } else {
                        "is not a directory."
                    }
                )));
            }
            let mut children = Vec::new();
            for mut f in fs::read_dir(&p).unwrap() {
                'entries: for f in f.iter_mut() {
                    {
                        let path = f.path();
                        if path.ends_with("cur") || path.ends_with("new") || path.ends_with("tmp") {
                            continue 'entries;
                        }
                        if path.is_dir() {
                            if let Ok(mut f) = MaildirMailbox::new(
                                path.to_str().unwrap().to_string(),
                                path.file_name().unwrap().to_str().unwrap().to_string(),
                                None,
                                Vec::new(),
                                false,
                                config,
                            ) {
                                f.children = recurse_mailboxes(mailboxes, config, &path)?;
                                for c in &f.children {
                                    if let Some(f) = mailboxes.get_mut(c) {
                                        f.parent = Some(f.hash);
                                    }
                                }
                                children.push(f.hash);
                                mailboxes.insert(f.hash, f);
                            } else {
                                /* If directory is invalid (i.e. has no {cur,new,tmp}
                                 * subfolders), accept it ONLY if
                                 * it contains subdirs of any depth that are
                                 * valid maildir paths
                                 */
                                let subdirs = recurse_mailboxes(mailboxes, config, &path)?;
                                if !subdirs.is_empty() {
                                    if let Ok(f) = MaildirMailbox::new(
                                        path.to_str().unwrap().to_string(),
                                        path.file_name().unwrap().to_str().unwrap().to_string(),
                                        None,
                                        subdirs,
                                        true,
                                        config,
                                    ) {
                                        for c in &f.children {
                                            if let Some(f) = mailboxes.get_mut(c) {
                                                f.parent = Some(f.hash);
                                            }
                                        }
                                        children.push(f.hash);
                                        mailboxes.insert(f.hash, f);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            Ok(children)
        }
        let root_mailbox = PathBuf::from(&settings.root_mailbox).expand();
        if !root_mailbox.try_exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "Configuration error ({}): root_mailbox `{}` is not a valid directory.",
                settings.name,
                settings.root_mailbox.as_str()
            )));
        } else if !root_mailbox.is_dir() {
            return Err(Error::new(format!(
                "Configuration error ({}): root_mailbox `{}` is not a directory.",
                settings.name,
                settings.root_mailbox.as_str()
            )));
        }

        let (is_root_a_mailbox, root_mailbox_name) = if let Ok(f) = MaildirMailbox::new_root_mailbox(
            root_mailbox.to_str().unwrap().to_string(),
            root_mailbox
                .file_name()
                .unwrap_or_default()
                .to_str()
                .unwrap_or_default()
                .to_string(),
            None,
            Vec::with_capacity(0),
            false,
            settings,
        ) {
            let name = f.name.clone();
            mailboxes.insert(f.hash, f);
            (true, name)
        } else {
            (
                false,
                root_mailbox
                    .file_name()
                    .unwrap_or_default()
                    .to_str()
                    .unwrap_or_default()
                    .to_string(),
            )
        };

        let config = Arc::new(Configuration {
            path: root_mailbox.clone(),
            root_mailbox_name,
            is_root_a_mailbox,
            ..Configuration::new(settings)?
        });

        if mailboxes.is_empty() {
            let children = recurse_mailboxes(&mut mailboxes, &config, &root_mailbox)?;
            for c in &children {
                if let Some(f) = mailboxes.get_mut(c) {
                    f.parent = None;
                }
            }
        } else {
            let root_hash = *mailboxes.keys().next().unwrap();
            let children = recurse_mailboxes(&mut mailboxes, &config, &root_mailbox)?;
            for c in &children {
                if let Some(f) = mailboxes.get_mut(c) {
                    f.parent = Some(root_hash);
                }
            }
            if let Some(f) = mailboxes.get_mut(&root_hash) {
                f.children = children;
            }
        }
        for f in mailboxes.values_mut() {
            if is_subscribed(f.path()) {
                f.is_subscribed = true;
            }
        }

        let mut hash_indexes =
            HashMap::with_capacity_and_hasher(mailboxes.len(), Default::default());
        for &fh in mailboxes.keys() {
            hash_indexes.insert(
                fh,
                HashIndex {
                    index: HashMap::with_capacity_and_hasher(0, Default::default()),
                    _hash: fh,
                },
            );
        }

        Ok(Box::new(Self {
            account_name: settings.name.to_string(),
            account_hash: AccountHash::from_bytes(settings.name.as_bytes()),
            mailboxes,
            is_subscribed,
            hash_indexes: Arc::new(Mutex::new(hash_indexes)),
            mailbox_index: Default::default(),
            event_consumer,
            collection: Default::default(),
            config,
        }))
    }

    pub fn save_to_mailbox(
        mut path: PathBuf,
        bytes: Vec<u8>,
        flags: Option<Flag>,
    ) -> Result<PathBuf> {
        path.validate_fs_subdirs()?;
        path.push("cur");
        {
            type BeBytes128 = [u8; 16];
            debug_assert_eq!(std::mem::size_of::<u64>(), 8);
            debug_assert_eq!(
                std::mem::size_of::<BeBytes128>(),
                2 * std::mem::size_of::<[u8; 8]>()
            );
            let mut rand_num: BeBytes128 = [0u8; 16];
            rand_num[0..8].copy_from_slice(&crate::utils::random::random_u64().to_be_bytes());
            rand_num[8..].copy_from_slice(&crate::utils::random::random_u64().to_be_bytes());
            let hostname = crate::utils::hostname::hostname()
                .ok()
                .and_then(|osstr| osstr.into_string().ok().map(Cow::Owned))
                .unwrap_or(Cow::Borrowed("localhost"));
            let mut filename = format!(
                "{}.{:x}_{}.{}:2,",
                crate::utils::random::clock_millis(),
                u128::from_be_bytes(rand_num),
                std::process::id(),
                hostname.trim()
            );
            if let Some(flags) = flags {
                if !(flags & Flag::DRAFT).is_empty() {
                    filename.push('D');
                }
                if !(flags & Flag::FLAGGED).is_empty() {
                    filename.push('F');
                }
                if !(flags & Flag::PASSED).is_empty() {
                    filename.push('P');
                }
                if !(flags & Flag::REPLIED).is_empty() {
                    filename.push('R');
                }
                if !(flags & Flag::SEEN).is_empty() {
                    filename.push('S');
                }
                if !(flags & Flag::TRASHED).is_empty() {
                    filename.push('T');
                }
            }
            path.push(filename);
        }
        log::trace!("saving at {}", path.display());
        let file = fs::File::create(&path)
            .chain_err_summary(|| format!("Could not create {}", path.display()))
            .chain_err_related_path(&path)?;
        let metadata = file
            .metadata()
            .chain_err_summary(|| format!("Could not retrieve file metadata of {}", path.display()))
            .chain_err_related_path(&path)?;
        let mut permissions = metadata.permissions();

        permissions.set_mode(0o600); // Read/write for owner only.
        file.set_permissions(permissions)
            .chain_err_summary(|| {
                format!("Could not set new file permissions to {}", path.display())
            })
            .chain_err_related_path(&path)?;

        let mut writer = io::BufWriter::new(file);
        writer
            .write_all(&bytes)
            .chain_err_summary(|| format!("Could not write bytes to new file {}", path.display()))
            .chain_err_related_path(&path)?;
        Ok(path)
    }

    pub fn validate_config(s: &mut AccountSettings) -> Result<()> {
        let root_mailbox = PathBuf::from(&s.root_mailbox).expand();
        if !root_mailbox.try_exists().unwrap_or(false) {
            return Err(Error::new(format!(
                "Configuration error ({}): root_mailbox `{}` is not a valid directory.",
                s.name,
                s.root_mailbox.as_str()
            )));
        } else if !root_mailbox.is_dir() {
            return Err(Error::new(format!(
                "Configuration error ({}): root_mailbox `{}` is not a directory.",
                s.name,
                s.root_mailbox.as_str()
            )));
        }
        _ = Configuration::new(s)?;
        _ = s.extra.swap_remove("rename_regex");

        Ok(())
    }

    pub fn list_mail_in_maildir_fs(
        config: &Configuration,
        mut path: PathBuf,
        read_only: bool,
    ) -> Result<Vec<PathBuf>> {
        path.validate_fs_subdirs()?;
        let mut files: Vec<PathBuf> = vec![];
        path.push("new");
        for p in path.read_dir().chain_err_related_path(&path)?.flatten() {
            if !read_only {
                utilities::move_to_cur(config, &p.path()).ok().take();
            } else {
                files.push(p.path());
            }
        }
        path.pop();
        path.push("cur");
        for e in path.read_dir().chain_err_related_path(&path)?.flatten() {
            files.push(e.path());
        }
        Ok(files)
    }

    fn mailbox_path_to_fs_path_and_suffix(
        config: &Configuration,
        name: &str,
    ) -> Result<(PathBuf, String)> {
        let mut fs_path = config.path.clone();
        let mut suffix = name.to_string();
        let root_mailbox_path_str = PathBuf::from(&config.settings.root_mailbox)
            .expand()
            .display()
            .to_string();
        if suffix.starts_with(&root_mailbox_path_str)
            && suffix.get(root_mailbox_path_str.len()..).is_some()
            && suffix[root_mailbox_path_str.len()..].starts_with("/")
        {
            suffix.replace_range(0..=root_mailbox_path_str.len(), "");
        }
        if suffix.starts_with(&config.root_mailbox_name)
            && suffix.get(config.root_mailbox_name.len()..).is_some()
            && suffix[config.root_mailbox_name.len()..].starts_with("/")
        {
            suffix.replace_range(0..=config.root_mailbox_name.len(), "");
        }
        fs_path.push(&suffix);
        let not_in_root = config
            .path
            .parent()
            .map(|p| fs_path.starts_with(p) && !fs_path.starts_with(&config.path))
            .unwrap_or(false);
        if not_in_root {
            return Err(Error::new(format!(
                "Path given, `{}`, is not included in the root mailbox path `{}`. A maildir \
                 backend cannot contain mailboxes outside of its root path.",
                name,
                config.path.display()
            )));
        }
        if !fs_path.starts_with(&config.path) {
            return Err(Error::new(format!(
                "Path given (`{}`) is absolute. Please provide a path relative to the account's \
                 root mailbox.",
                name
            )));
        }
        Ok((fs_path, suffix))
    }

    /// Processes a path and inserts it into mailboxes if it's a valid maildir
    /// mailbox.
    pub fn mailbox_from_path(&mut self, fs_path: PathBuf, suffix: String) -> Result<MailboxHash> {
        fs_path.validate_fs_subdirs()?;
        let parent = fs_path.parent().and_then(|p| {
            self.mailboxes
                .iter()
                .find(|(_, f)| f.fs_path == p)
                .map(|item| *item.0)
        });

        let mailbox_hash: MailboxHash = fs_path.to_mailbox_hash();
        if let Some(parent) = parent {
            self.mailboxes
                .entry(parent)
                .and_modify(|entry| entry.children.push(mailbox_hash));
        }
        let path = if self.config.is_root_a_mailbox {
            let mut path = PathBuf::from(&self.config.root_mailbox_name);
            path.push(suffix);
            path
        } else {
            PathBuf::from(&suffix)
        };
        let name = fs_path.file_name().unwrap().to_str().unwrap().to_string();
        let new_mailbox = MaildirMailbox {
            hash: mailbox_hash,
            path,
            name,
            fs_path,
            parent,
            children: vec![],
            usage: Default::default(),
            is_subscribed: true,
            permissions: Default::default(),
            unseen: Default::default(),
            total: Default::default(),
        };

        self.mailboxes.insert(mailbox_hash, new_mailbox);
        Ok(mailbox_hash)
    }
}
