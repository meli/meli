/*
 * meli - backends module
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

pub mod utf7;

#[cfg(test)]
mod tests;

pub mod prelude {
    pub use async_fn_stream::try_fn_stream;
    pub use futures::{
        future::BoxFuture,
        lock::Mutex as FutureMutex,
        stream::{BoxStream, Stream},
    };
    pub type ResultFuture<T> = crate::Result<BoxFuture<'static, crate::Result<T>>>;
    pub type ResultStream<T> = crate::Result<BoxStream<'static, crate::Result<T>>>;
    pub use std::{
        any::Any,
        borrow::Cow,
        cell::RefCell,
        collections::{BTreeSet, HashMap},
        ops::Deref,
        pin::Pin,
        sync::Arc,
    };

    pub use indexmap::{self, IndexMap, IndexSet};
    pub use smallvec::{self, SmallVec};

    pub use super::{
        AccountHash, BackendEvent, BackendEventConsumer, BackendMailbox, EnvelopeHashBatch, FlagOp,
        IsSubscribedFn, LazyCountSet, MailBackend, MailBackendCapabilities,
        MailBackendExtensionStatus, Mailbox, MailboxHash, MailboxPermissions, RefreshEvent,
        RefreshEventKind, TagHash,
    };
    pub use crate::{
        conf::AccountSettings,
        email::{Envelope, EnvelopeHash, Flag},
        error::{Error, ErrorKind, Result},
        search::Query,
        Collection, HeaderName, LogLevel, SpecialUsageMailbox,
    };
}
use prelude::*;

pub type BackendCreator = Box<
    dyn Fn(&AccountSettings, IsSubscribedFn, BackendEventConsumer) -> Result<Box<dyn MailBackend>>,
>;

pub type BackendValidateConfigFn = Box<dyn Fn(&mut AccountSettings) -> Result<()>>;

/// A hashmap containing all available mail backends.
/// An abstraction over any available backends.
pub struct Backends {
    map: HashMap<std::string::String, Backend>,
}

pub struct Backend {
    pub create_fn: Box<dyn Fn() -> BackendCreator>,
    pub validate_conf_fn: BackendValidateConfigFn,
}

impl Default for Backends {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(feature = "notmuch")]
pub const NOTMUCH_ERROR_MSG: &str = "libnotmuch5 was not found in your system. Make sure it is \
                                     installed and in the library paths. For a custom file path, \
                                     use `library_file_path` setting in your notmuch account.\n";
#[cfg(not(feature = "notmuch"))]
pub const NOTMUCH_ERROR_MSG: &str = "this version of meli is not compiled with notmuch support. \
                                     Use an appropriate version and make sure libnotmuch5 is \
                                     installed and in the library paths.\n";

#[cfg(not(feature = "notmuch"))]
pub const NOTMUCH_ERROR_DETAILS: &str = "";

#[cfg(all(
    feature = "notmuch",
    all(target_family = "unix", not(target_os = "macos"))
))]
pub const NOTMUCH_ERROR_DETAILS: &str = r#"If you have installed the library manually, try setting the `LD_LIBRARY_PATH` environment variable to its `lib` directory. Otherwise, set it to the location of libnotmuch.5.so. Example:

LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/path/to/notmuch/lib" meli

or, put this in your shell init script (.bashenv, .zshenv, .bashrc, .zshrc, .profile):

export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/path/to/notmuch/lib"

You can also set any location by specifying the library file path with the configuration flag `library_file_path`."#;

#[cfg(all(feature = "notmuch", target_os = "macos"))]
pub const NOTMUCH_ERROR_DETAILS: &str = r#"If you have installed the library via homebrew, try setting the `DYLD_LIBRARY_PATH` environment variable to its `lib` directory. Otherwise, set it to the location of libnotmuch.5.dylib. Example:

DYLD_LIBRARY_PATH="$(brew --prefix)/lib" meli

or, put this in your shell init script (.bashenv, .zshenv, .bashrc, .zshrc, .profile):

export DYLD_LIBRARY_PATH="$(brew --prefix)/lib"

Make sure to append to DYLD_LIBRARY_PATH if it's not empty, by prepending a colon to the libnotmuch5.dylib location:

export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$(brew --prefix)/lib"

You can also set any location by specifying the library file path with the configuration flag `library_file_path`."#;

#[cfg(all(
    feature = "notmuch",
    not(any(target_family = "unix", target_os = "macos"))
))]
pub const NOTMUCH_ERROR_DETAILS: &str = r#"If notmuch is installed but the library isn't found, consult your system's documentation on how to make dynamic libraries discoverable."#;

impl Backends {
    pub fn new() -> Self {
        const fn as_dyn_b<T: MailBackend + 'static>(self_: Box<T>) -> Box<dyn MailBackend> {
            self_ as Box<dyn MailBackend>
        }

        let mut b = Self {
            map: HashMap::with_capacity_and_hasher(1, Default::default()),
        };
        #[cfg(feature = "maildir")]
        {
            use crate::maildir::MaildirType;

            b.register(
                "maildir".to_string(),
                Backend {
                    create_fn: Box::new(|| {
                        Box::new(|f, i, ev| MaildirType::new(f, i, ev).map(as_dyn_b))
                    }),
                    validate_conf_fn: Box::new(MaildirType::validate_config),
                },
            );
        }
        {
            use crate::mbox::MboxType;

            b.register(
                "mbox".to_string(),
                Backend {
                    create_fn: Box::new(|| {
                        Box::new(|f, i, ev| MboxType::new(f, i, ev).map(as_dyn_b))
                    }),
                    validate_conf_fn: Box::new(MboxType::validate_config),
                },
            );
        }
        #[cfg(feature = "imap")]
        {
            use crate::imap::ImapType;

            b.register(
                "imap".to_string(),
                Backend {
                    create_fn: Box::new(|| {
                        Box::new(|f, i, ev| ImapType::new(f, i, ev).map(as_dyn_b))
                    }),
                    validate_conf_fn: Box::new(ImapType::validate_config),
                },
            );
        }
        #[cfg(feature = "nntp")]
        {
            use crate::nntp::NntpType;

            b.register(
                "nntp".to_string(),
                Backend {
                    create_fn: Box::new(|| {
                        Box::new(|f, i, ev| NntpType::new(f, i, ev).map(as_dyn_b))
                    }),
                    validate_conf_fn: Box::new(NntpType::validate_config),
                },
            );
        }
        #[cfg(feature = "notmuch")]
        {
            use crate::notmuch::NotmuchDb;

            b.register(
                "notmuch".to_string(),
                Backend {
                    create_fn: Box::new(|| {
                        Box::new(|f, i, ev| NotmuchDb::new(f, i, ev).map(as_dyn_b))
                    }),
                    validate_conf_fn: Box::new(NotmuchDb::validate_config),
                },
            );
        }
        #[cfg(feature = "jmap")]
        {
            use crate::jmap::JmapType;

            b.register(
                "jmap".to_string(),
                Backend {
                    create_fn: Box::new(|| {
                        Box::new(|f, i, ev| JmapType::new(f, i, ev).map(as_dyn_b))
                    }),
                    validate_conf_fn: Box::new(JmapType::validate_config),
                },
            );
        }
        b
    }

    pub fn get(&self, key: &str) -> BackendCreator {
        if !self.map.contains_key(key) {
            if key == "notmuch" {
                eprint!("{}", NOTMUCH_ERROR_MSG);
                #[cfg(feature = "notmuch")]
                {
                    eprint!("{}", NOTMUCH_ERROR_DETAILS);
                }
            }
            panic!("{} is not a valid mail backend", key);
        }
        (self.map[key].create_fn)()
    }

    pub fn register(&mut self, key: String, backend: Backend) {
        if self.map.contains_key(&key) {
            panic!("{} is an already registered backend", key);
        }
        self.map.insert(key, backend);
    }

    pub fn validate_config(&self, key: &str, s: &mut AccountSettings) -> Result<()> {
        (self
            .map
            .get(key)
            .ok_or_else(|| {
                Error::new(format!(
                    "{}{} is not a valid mail backend. {}",
                    if key == "notmuch" {
                        NOTMUCH_ERROR_MSG
                    } else {
                        ""
                    },
                    key,
                    if cfg!(feature = "notmuch") && key == "notmuch" {
                        NOTMUCH_ERROR_DETAILS
                    } else {
                        ""
                    },
                ))
                .set_kind(ErrorKind::Configuration)
            })?
            .validate_conf_fn)(s)
    }
}

#[derive(Clone, Debug)]
pub enum BackendEvent {
    Notice {
        description: String,
        content: Option<String>,
        level: LogLevel,
    },
    Refresh(RefreshEvent),
    RefreshBatch(Vec<RefreshEvent>),
    AccountStateChange {
        message: Cow<'static, str>,
    },
}

impl From<Error> for BackendEvent {
    fn from(val: Error) -> Self {
        Self::Notice {
            description: val.summary.to_string(),
            content: Some(val.to_string()),
            level: LogLevel::ERROR,
        }
    }
}

#[derive(Clone, Debug)]
pub enum RefreshEventKind {
    Update(EnvelopeHash, Box<Envelope>),
    /// `Rename(old_hash, new_hash)`
    Rename(EnvelopeHash, EnvelopeHash),
    Create(Box<Envelope>),
    Remove(EnvelopeHash),
    NewFlags(EnvelopeHash, (Flag, Vec<String>)),
    Rescan,
    Failure(Error),
    MailboxCreate(Mailbox),
    MailboxDelete(MailboxHash),
    MailboxRename {
        old_mailbox_hash: MailboxHash,
        new_mailbox: Mailbox,
    },
    MailboxSubscribe(MailboxHash),
    MailboxUnsubscribe(MailboxHash),
}

#[derive(Clone, Debug)]
pub struct RefreshEvent {
    pub account_hash: AccountHash,
    pub mailbox_hash: MailboxHash,
    pub kind: RefreshEventKind,
}

#[derive(Clone)]
pub struct BackendEventConsumer(Arc<dyn Fn(AccountHash, BackendEvent) + Send + Sync>);

impl BackendEventConsumer {
    pub fn new(b: Arc<dyn Fn(AccountHash, BackendEvent) + Send + Sync>) -> Self {
        Self(b)
    }
}

impl std::fmt::Debug for BackendEventConsumer {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(crate::identify!(BackendEventConsumer))
            .finish()
    }
}

impl Deref for BackendEventConsumer {
    type Target = dyn Fn(AccountHash, BackendEvent) + Send + Sync;

    fn deref(&self) -> &Self::Target {
        &(*self.0)
    }
}

impl From<Arc<dyn Fn(AccountHash, BackendEvent) + Send + Sync>> for BackendEventConsumer {
    fn from(val: Arc<dyn Fn(AccountHash, BackendEvent) + Send + Sync>) -> Self {
        Self(val)
    }
}

impl From<Box<dyn Fn(AccountHash, BackendEvent) + Send + Sync>> for BackendEventConsumer {
    fn from(val: Box<dyn Fn(AccountHash, BackendEvent) + Send + Sync>) -> Self {
        Self(val.into())
    }
}

impl Default for BackendEventConsumer {
    fn default() -> Self {
        Self(Arc::new(|_, _| {}))
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum FlagOp {
    Set(Flag),
    SetTag(String),
    UnSet(Flag),
    UnSetTag(String),
}

impl From<&FlagOp> for bool {
    fn from(val: &FlagOp) -> Self {
        matches!(val, FlagOp::Set(_) | FlagOp::SetTag(_))
    }
}

impl FlagOp {
    #[inline]
    pub fn is_flag(&self) -> bool {
        matches!(self, Self::Set(_) | Self::UnSet(_))
    }

    #[inline]
    pub fn is_tag(&self) -> bool {
        matches!(self, Self::SetTag(_) | Self::UnSetTag(_))
    }

    #[inline]
    pub fn as_bool(&self) -> bool {
        self.into()
    }
}

#[derive(Clone, Debug)]
pub struct MailBackendCapabilities {
    pub is_async: bool,
    pub is_remote: bool,
    pub extensions: Option<Vec<(String, MailBackendExtensionStatus)>>,
    pub supports_search: bool,
    pub supports_tags: bool,
    pub supports_submission: bool,
    pub extra_submission_headers: &'static [HeaderName],
    pub metadata: Option<serde_json::Value>,
}

#[derive(Clone, Copy, Debug)]
pub enum MailBackendExtensionStatus {
    Unsupported { comment: Option<&'static str> },
    Supported { comment: Option<&'static str> },
    Enabled { comment: Option<&'static str> },
}

pub trait MailBackend: ::std::fmt::Debug + Send + Sync {
    fn capabilities(&self) -> MailBackendCapabilities;
    fn is_online(&self) -> ResultFuture<()> {
        Ok(Box::pin(async { Ok(()) }))
    }
    fn fetch(&mut self, mailbox_hash: MailboxHash) -> ResultStream<Vec<Envelope>>;
    fn refresh(&mut self, mailbox_hash: MailboxHash) -> ResultFuture<()>;
    fn watch(&self) -> ResultFuture<()>;
    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>>;
    fn envelope_bytes_by_hash(&self, hash: EnvelopeHash) -> ResultFuture<Vec<u8>>;

    fn save(
        &self,
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        flags: Option<Flag>,
    ) -> ResultFuture<()>;
    fn save_batch(
        &self,
        batch: Vec<(Vec<u8>, MailboxHash, Option<Flag>)>,
    ) -> ResultStream<Result<()>> {
        let mut futures = batch
            .into_iter()
            .map(|(bytes, mailbox_hash, flags)| self.save(bytes, mailbox_hash, flags))
            .collect::<Result<Vec<Pin<Box<_>>>>>()?;
        Ok(Box::pin(try_fn_stream(|emitter| async move {
            while !futures.is_empty() {
                let max_len = futures.len().min(100);
                futures::future::try_join_all(futures.drain(0..max_len)).await?;
                emitter.emit(Ok(())).await;
            }
            Ok(())
        })))
    }

    fn copy_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        source_mailbox_hash: MailboxHash,
        destination_mailbox_hash: MailboxHash,
        move_: bool,
    ) -> ResultFuture<()>;

    fn set_flags(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
        flags: SmallVec<[FlagOp; 8]>,
    ) -> ResultFuture<()>;

    fn delete_messages(
        &mut self,
        env_hashes: EnvelopeHashBatch,
        mailbox_hash: MailboxHash,
    ) -> ResultFuture<()>;

    fn collection(&self) -> crate::Collection;
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn create_mailbox(
        &mut self,
        path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)>;

    fn delete_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>>;

    fn set_mailbox_subscription(
        &mut self,
        mailbox_hash: MailboxHash,
        val: bool,
    ) -> ResultFuture<()>;

    fn rename_mailbox(
        &mut self,
        mailbox_hash: MailboxHash,
        new_path: String,
    ) -> ResultFuture<Mailbox>;

    fn set_mailbox_permissions(
        &mut self,
        mailbox_hash: MailboxHash,
        val: MailboxPermissions,
    ) -> ResultFuture<()>;

    fn search(
        &self,
        query: crate::search::Query,
        mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<Vec<EnvelopeHash>>;

    fn submit(
        &self,
        _bytes: Vec<u8>,
        _mailbox_hash: Option<MailboxHash>,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        Err(Error::new("Submission not supported in this backend.")
            .set_kind(ErrorKind::NotSupported))
    }
    fn submit_batch(
        &self,
        batch: Vec<(Vec<u8>, Option<MailboxHash>, Option<Flag>)>,
    ) -> ResultStream<Result<()>> {
        let mut futures = batch
            .into_iter()
            .map(|(bytes, mailbox_hash, flags)| self.submit(bytes, mailbox_hash, flags))
            .collect::<Result<Vec<Pin<Box<_>>>>>()?;
        Ok(Box::pin(try_fn_stream(|emitter| async move {
            while !futures.is_empty() {
                let max_len = futures.len().min(100);
                futures::future::try_join_all(futures.drain(0..max_len)).await?;
                emitter.emit(Ok(())).await;
            }
            Ok(())
        })))
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, Serialize)]
pub enum SpecialUsageMailbox {
    #[default]
    Normal,
    Inbox,
    Archive,
    Drafts,
    Flagged,
    Junk,
    Sent,
    Trash,
}

impl std::fmt::Display for SpecialUsageMailbox {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use SpecialUsageMailbox::*;
        write!(
            f,
            "{}",
            match self {
                Normal => "Normal",
                Inbox => "Inbox",
                Archive => "Archive",
                Drafts => "Drafts",
                Flagged => "Flagged",
                Junk => "Junk",
                Sent => "Sent",
                Trash => "Trash",
            }
        )
    }
}

impl SpecialUsageMailbox {
    pub fn detect_usage(name: &str) -> Option<Self> {
        if name.eq_ignore_ascii_case("inbox") {
            Some(Self::Inbox)
        } else if name.eq_ignore_ascii_case("archive") {
            Some(Self::Archive)
        } else if name.eq_ignore_ascii_case("drafts") {
            Some(Self::Drafts)
        } else if name.eq_ignore_ascii_case("junk") || name.eq_ignore_ascii_case("spam") {
            Some(Self::Junk)
        } else if name.eq_ignore_ascii_case("sent") {
            Some(Self::Sent)
        } else if name.eq_ignore_ascii_case("trash") {
            Some(Self::Trash)
        } else {
            Some(Self::Normal)
        }
    }
}

pub trait BackendMailbox: std::fmt::Debug + std::any::Any {
    fn hash(&self) -> MailboxHash;
    /// Final component of `path`.
    fn name(&self) -> &str;
    /// Path of mailbox within the mailbox hierarchy, with `/` as separator.
    fn path(&self) -> &str;
    fn clone(&self) -> Mailbox;
    fn children(&self) -> &[MailboxHash];
    fn parent(&self) -> Option<MailboxHash>;
    fn is_subscribed(&self) -> bool;
    fn set_is_subscribed(&mut self, new_val: bool) -> Result<()>;
    fn set_special_usage(&mut self, new_val: SpecialUsageMailbox) -> Result<()>;
    fn special_usage(&self) -> SpecialUsageMailbox;
    fn permissions(&self) -> MailboxPermissions;
    fn count(&self) -> Result<(usize, usize)>;
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

crate::declare_u64_hash!(AccountHash);
crate::declare_u64_hash!(MailboxHash);
crate::declare_u64_hash!(TagHash);

pub type Mailbox = Box<dyn BackendMailbox + Send + Sync>;

impl Clone for Mailbox {
    fn clone(&self) -> Self {
        BackendMailbox::clone(self.deref())
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct MailboxPermissions {
    pub create_messages: bool,
    pub remove_messages: bool,
    pub set_flags: bool,
    pub create_child: bool,
    pub rename_messages: bool,
    pub delete_messages: bool,
    pub delete_mailbox: bool,
    pub change_permissions: bool,
}

impl Default for MailboxPermissions {
    fn default() -> Self {
        Self {
            create_messages: false,
            remove_messages: false,
            set_flags: false,
            create_child: false,
            rename_messages: false,
            delete_messages: false,
            delete_mailbox: true,
            change_permissions: false,
        }
    }
}

impl std::fmt::Display for MailboxPermissions {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(fmt, "{:#?}", self)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnvelopeHashBatch {
    pub first: EnvelopeHash,
    pub rest: SmallVec<[EnvelopeHash; 64]>,
}

impl From<EnvelopeHash> for EnvelopeHashBatch {
    fn from(value: EnvelopeHash) -> Self {
        Self {
            first: value,
            rest: SmallVec::new(),
        }
    }
}

impl std::convert::TryFrom<&[EnvelopeHash]> for EnvelopeHashBatch {
    type Error = ();

    fn try_from(value: &[EnvelopeHash]) -> std::result::Result<Self, Self::Error> {
        if value.is_empty() {
            return Err(());
        }
        Ok(Self {
            first: value[0],
            rest: value[1..].iter().cloned().collect(),
        })
    }
}

impl From<&EnvelopeHashBatch> for BTreeSet<EnvelopeHash> {
    fn from(val: &EnvelopeHashBatch) -> Self {
        val.iter().collect()
    }
}

impl EnvelopeHashBatch {
    pub fn iter(&self) -> impl std::iter::Iterator<Item = EnvelopeHash> + '_ {
        std::iter::once(self.first).chain(self.rest.iter().cloned())
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        1 + self.rest.len()
    }

    pub fn to_set(&self) -> BTreeSet<EnvelopeHash> {
        self.into()
    }
}

#[derive(Clone, Default)]
pub struct LazyCountSet {
    pub not_yet_seen: usize,
    pub set: BTreeSet<EnvelopeHash>,
}

impl std::fmt::Debug for LazyCountSet {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct(crate::identify!(LazyCountSet))
            .field("not_yet_seen", &self.not_yet_seen)
            .field("set", &self.set.len())
            .field("total_len", &self.len())
            .finish()
    }
}

impl LazyCountSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_not_yet_seen(&mut self, new_val: usize) {
        self.not_yet_seen = new_val;
    }

    pub fn insert_existing(&mut self, new_val: EnvelopeHash) -> bool {
        self.not_yet_seen = 0;
        self.set.insert(new_val)
    }

    pub fn insert_existing_set(&mut self, set: BTreeSet<EnvelopeHash>) {
        self.set.extend(set);
        self.not_yet_seen = 0;
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.set.len() + self.not_yet_seen
    }

    #[inline(always)]
    pub fn clear(&mut self) {
        self.set.clear();
        self.not_yet_seen = 0;
    }

    pub fn insert_new(&mut self, new_val: EnvelopeHash) {
        self.not_yet_seen = 0;
        self.set.insert(new_val);
    }

    pub fn insert_set(&mut self, set: BTreeSet<EnvelopeHash>) {
        self.not_yet_seen = 0;
        self.set.extend(set);
    }

    pub fn remove(&mut self, env_hash: EnvelopeHash) -> bool {
        self.set.remove(&env_hash)
    }

    #[inline(always)]
    pub fn contains(&self, value: &EnvelopeHash) -> bool {
        self.set.contains(value)
    }
}

#[derive(Clone)]
pub struct IsSubscribedFn(Arc<dyn Fn(&str) -> bool + Send + Sync>);

impl From<Arc<dyn Fn(&str) -> bool + Send + Sync>> for IsSubscribedFn {
    fn from(val: Arc<dyn Fn(&str) -> bool + Send + Sync>) -> Self {
        Self(val)
    }
}

impl From<Box<dyn Fn(&str) -> bool + Send + Sync>> for IsSubscribedFn {
    fn from(val: Box<dyn Fn(&str) -> bool + Send + Sync>) -> Self {
        Self(val.into())
    }
}

impl Default for IsSubscribedFn {
    fn default() -> Self {
        Self(Arc::new(|_| true))
    }
}

impl std::fmt::Debug for IsSubscribedFn {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_struct(crate::identify!(IsSubscribedFn)).finish()
    }
}

impl std::ops::Deref for IsSubscribedFn {
    type Target = dyn Fn(&str) -> bool + Send + Sync;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}
