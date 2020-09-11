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

use smallvec::SmallVec;
#[macro_export]
macro_rules! tag_hash {
    ($tag:ident) => {{
        use std::collections::hash_map::DefaultHasher;
        use std::hash::Hasher;
        let mut hasher = DefaultHasher::new();
        hasher.write($tag.as_bytes());
        hasher.finish()
    }};
}

#[cfg(feature = "imap_backend")]
pub mod imap;
#[cfg(feature = "imap_backend")]
pub mod nntp;
#[cfg(feature = "notmuch_backend")]
pub mod notmuch;
#[cfg(feature = "notmuch_backend")]
pub use self::notmuch::NotmuchDb;
#[cfg(feature = "jmap_backend")]
pub mod jmap;
#[cfg(feature = "maildir_backend")]
pub mod maildir;
#[cfg(feature = "mbox_backend")]
pub mod mbox;
#[cfg(feature = "imap_backend")]
pub use self::imap::ImapType;
#[cfg(feature = "imap_backend")]
pub use self::nntp::NntpType;
use crate::conf::AccountSettings;
use crate::error::{MeliError, Result};

#[cfg(feature = "maildir_backend")]
use self::maildir::MaildirType;
#[cfg(feature = "mbox_backend")]
use self::mbox::MboxType;
use super::email::{Envelope, EnvelopeHash, Flag};
use std::any::Any;
use std::collections::BTreeMap;
use std::fmt;
use std::fmt::Debug;
use std::ops::Deref;
use std::sync::{Arc, RwLock};

pub use futures::stream::Stream;
use std::future::Future;
pub use std::pin::Pin;

use std::collections::HashMap;

#[macro_export]
macro_rules! get_path_hash {
    ($path:expr) => {{
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        $path.hash(&mut hasher);
        hasher.finish()
    }};
}

pub type BackendCreator = Box<
    dyn Fn(
        &AccountSettings,
        Box<dyn Fn(&str) -> bool + Send + Sync>,
        BackendEventConsumer,
    ) -> Result<Box<dyn MailBackend>>,
>;

/// A hashmap containing all available mail backends.
/// An abstraction over any available backends.
pub struct Backends {
    map: HashMap<std::string::String, Backend>,
}

pub struct Backend {
    pub create_fn: Box<dyn Fn() -> BackendCreator>,
    pub validate_conf_fn: Box<dyn Fn(&AccountSettings) -> Result<()>>,
}

impl Default for Backends {
    fn default() -> Self {
        Backends::new()
    }
}

#[cfg(feature = "notmuch_backend")]
pub const NOTMUCH_ERROR_MSG: &str =
    "libnotmuch5 was not found in your system. Make sure it is installed and in the library paths.\n";
#[cfg(not(feature = "notmuch_backend"))]
pub const NOTMUCH_ERROR_MSG: &str = "this version of meli is not compiled with notmuch support. Use an appropriate version and make sure libnotmuch5 is installed and in the library paths.\n";

impl Backends {
    pub fn new() -> Self {
        let mut b = Backends {
            map: HashMap::with_capacity_and_hasher(1, Default::default()),
        };
        #[cfg(feature = "maildir_backend")]
        {
            b.register(
                "maildir".to_string(),
                Backend {
                    create_fn: Box::new(|| Box::new(|f, i, ev| MaildirType::new(f, i, ev))),
                    validate_conf_fn: Box::new(MaildirType::validate_config),
                },
            );
        }
        #[cfg(feature = "mbox_backend")]
        {
            b.register(
                "mbox".to_string(),
                Backend {
                    create_fn: Box::new(|| Box::new(|f, i, ev| MboxType::new(f, i, ev))),
                    validate_conf_fn: Box::new(MboxType::validate_config),
                },
            );
        }
        #[cfg(feature = "imap_backend")]
        {
            b.register(
                "imap".to_string(),
                Backend {
                    create_fn: Box::new(|| Box::new(|f, i, ev| imap::ImapType::new(f, i, ev))),
                    validate_conf_fn: Box::new(imap::ImapType::validate_config),
                },
            );
            b.register(
                "nntp".to_string(),
                Backend {
                    create_fn: Box::new(|| Box::new(|f, i, ev| nntp::NntpType::new(f, i, ev))),
                    validate_conf_fn: Box::new(nntp::NntpType::validate_config),
                },
            );
        }
        #[cfg(feature = "notmuch_backend")]
        {
            if libloading::Library::new("libnotmuch.so.5").is_ok() {
                b.register(
                    "notmuch".to_string(),
                    Backend {
                        create_fn: Box::new(|| Box::new(|f, i, ev| NotmuchDb::new(f, i, ev))),
                        validate_conf_fn: Box::new(NotmuchDb::validate_config),
                    },
                );
            }
        }
        #[cfg(feature = "jmap_backend")]
        {
            b.register(
                "jmap".to_string(),
                Backend {
                    create_fn: Box::new(|| Box::new(|f, i, ev| jmap::JmapType::new(f, i, ev))),
                    validate_conf_fn: Box::new(jmap::JmapType::validate_config),
                },
            );
        }
        b
    }

    pub fn get(&self, key: &str) -> BackendCreator {
        if !self.map.contains_key(key) {
            if key == "notmuch" {
                eprint!("{}", NOTMUCH_ERROR_MSG);
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

    pub fn validate_config(&self, key: &str, s: &AccountSettings) -> Result<()> {
        (self
            .map
            .get(key)
            .ok_or_else(|| {
                MeliError::new(format!(
                    "{}{} is not a valid mail backend",
                    if key == "notmuch" {
                        NOTMUCH_ERROR_MSG
                    } else {
                        ""
                    },
                    key
                ))
            })?
            .validate_conf_fn)(s)
    }
}

#[derive(Debug, Clone)]
pub enum BackendEvent {
    Notice {
        description: Option<String>,
        content: String,
        level: crate::LoggingLevel,
    },
    Refresh(RefreshEvent),
    //Job(Box<Future<Output = Result<()>> + Send + 'static>)
}

impl From<MeliError> for BackendEvent {
    fn from(val: MeliError) -> BackendEvent {
        BackendEvent::Notice {
            description: val.summary.as_ref().map(|s| s.to_string()),
            content: val.to_string(),
            level: crate::LoggingLevel::ERROR,
        }
    }
}

#[derive(Debug, Clone)]
pub enum RefreshEventKind {
    Update(EnvelopeHash, Box<Envelope>),
    /// Rename(old_hash, new_hash)
    Rename(EnvelopeHash, EnvelopeHash),
    Create(Box<Envelope>),
    Remove(EnvelopeHash),
    NewFlags(EnvelopeHash, (Flag, Vec<String>)),
    Rescan,
    Failure(MeliError),
}

#[derive(Debug, Clone)]
pub struct RefreshEvent {
    pub mailbox_hash: MailboxHash,
    pub account_hash: AccountHash,
    pub kind: RefreshEventKind,
}

#[derive(Clone)]
pub struct BackendEventConsumer(Arc<dyn Fn(AccountHash, BackendEvent) + Send + Sync>);
impl BackendEventConsumer {
    pub fn new(b: Arc<dyn Fn(AccountHash, BackendEvent) + Send + Sync>) -> Self {
        BackendEventConsumer(b)
    }
}

impl fmt::Debug for BackendEventConsumer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BackendEventConsumer")
    }
}

impl Deref for BackendEventConsumer {
    type Target = dyn Fn(AccountHash, BackendEvent) + Send + Sync;

    fn deref(&self) -> &Self::Target {
        &(*self.0)
    }
}

#[derive(Debug, Clone)]
pub struct MailBackendCapabilities {
    pub is_async: bool,
    pub is_remote: bool,
    pub extensions: Option<Vec<(String, MailBackendExtensionStatus)>>,
    pub supports_search: bool,
    pub supports_tags: bool,
    pub supports_submission: bool,
}

#[derive(Debug, Copy, Clone)]
pub enum MailBackendExtensionStatus {
    Unsupported { comment: Option<&'static str> },
    Supported { comment: Option<&'static str> },
    Enabled { comment: Option<&'static str> },
}

pub type ResultFuture<T> = Result<Pin<Box<dyn Future<Output = Result<T>> + Send + 'static>>>;

pub trait MailBackend: ::std::fmt::Debug + Send + Sync {
    fn capabilities(&self) -> MailBackendCapabilities;
    fn is_online(&self) -> ResultFuture<()> {
        Ok(Box::pin(async { Ok(()) }))
    }

    fn fetch(
        &mut self,
        mailbox_hash: MailboxHash,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>>;

    fn refresh(&mut self, mailbox_hash: MailboxHash) -> ResultFuture<()>;
    fn watch(&self) -> ResultFuture<()>;
    fn mailboxes(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>>;
    fn operation(&self, hash: EnvelopeHash) -> Result<Box<dyn BackendOp>>;

    fn save(
        &self,
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        flags: Option<Flag>,
    ) -> ResultFuture<()>;

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
        flags: SmallVec<[(std::result::Result<Flag, String>, bool); 8]>,
    ) -> ResultFuture<()>;

    fn delete_messages(
        &self,
        _env_hashes: EnvelopeHashBatch,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }
    fn delete(&self, _env_hash: EnvelopeHash, _mailbox_hash: MailboxHash) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }
    fn tags(&self) -> Option<Arc<RwLock<BTreeMap<u64, String>>>> {
        None
    }
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;

    fn create_mailbox(
        &mut self,
        _path: String,
    ) -> ResultFuture<(MailboxHash, HashMap<MailboxHash, Mailbox>)> {
        Err(MeliError::new("Unimplemented."))
    }

    fn delete_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
    ) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(MeliError::new("Unimplemented."))
    }

    fn set_mailbox_subscription(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: bool,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn rename_mailbox(
        &mut self,
        _mailbox_hash: MailboxHash,
        _new_path: String,
    ) -> ResultFuture<Mailbox> {
        Err(MeliError::new("Unimplemented."))
    }

    fn set_mailbox_permissions(
        &mut self,
        _mailbox_hash: MailboxHash,
        _val: MailboxPermissions,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }

    fn search(
        &self,
        _query: crate::search::Query,
        _mailbox_hash: Option<MailboxHash>,
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>> {
        Err(MeliError::new("Unimplemented."))
    }
}

/// A `BackendOp` manages common operations for the various mail backends. They only live for the
/// duration of the operation. They are generated by the `operation` method of `Mailbackend` trait.
///
/// # Motivation
///
/// We need a way to do various operations on individual mails regardless of what backend they come
/// from (eg local or imap).
///
/// # Creation
/// ```ignore
/// /* Create operation from Backend */
///
/// let op = backend.operation(message.hash(), mailbox.hash());
/// ```
///
/// # Example
/// ```ignore
/// use melib::backends::{BackendOp};
/// use melib::Result;
/// use melib::{Envelope, Flag};
///
/// #[derive(Debug)]
/// struct FooOp {}
///
/// impl BackendOp for FooOp {
///     fn as_bytes(&mut self) -> Result<&[u8]> {
///         unimplemented!()
///     }
///     fn fetch_flags(&self) -> Result<Flag> {
///         unimplemented!()
///     }
/// }
///
/// let operation = Box::new(FooOp {});
/// ```
pub trait BackendOp: ::std::fmt::Debug + ::std::marker::Send {
    fn as_bytes(&mut self) -> ResultFuture<Vec<u8>>;
    fn fetch_flags(&self) -> ResultFuture<Flag>;
}

/// Wrapper for BackendOps that are to be set read-only.
///
/// Warning: Backend implementations may still cause side-effects (for example IMAP can set the
/// Seen flag when fetching an envelope)
#[derive(Debug)]
pub struct ReadOnlyOp {
    op: Box<dyn BackendOp>,
}

impl ReadOnlyOp {
    pub fn new(op: Box<dyn BackendOp>) -> Box<dyn BackendOp> {
        Box::new(ReadOnlyOp { op })
    }
}

impl BackendOp for ReadOnlyOp {
    fn as_bytes(&mut self) -> ResultFuture<Vec<u8>> {
        self.op.as_bytes()
    }
    fn fetch_flags(&self) -> ResultFuture<Flag> {
        self.op.fetch_flags()
    }
}

#[derive(Debug, Copy, Hash, Eq, Clone, Serialize, Deserialize, PartialEq)]
pub enum SpecialUsageMailbox {
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

impl Default for SpecialUsageMailbox {
    fn default() -> Self {
        SpecialUsageMailbox::Normal
    }
}

impl SpecialUsageMailbox {
    pub fn detect_usage(name: &str) -> Option<SpecialUsageMailbox> {
        if name.eq_ignore_ascii_case("inbox") {
            Some(SpecialUsageMailbox::Inbox)
        } else if name.eq_ignore_ascii_case("archive") {
            Some(SpecialUsageMailbox::Archive)
        } else if name.eq_ignore_ascii_case("drafts") {
            Some(SpecialUsageMailbox::Drafts)
        } else if name.eq_ignore_ascii_case("junk") || name.eq_ignore_ascii_case("spam") {
            Some(SpecialUsageMailbox::Junk)
        } else if name.eq_ignore_ascii_case("sent") {
            Some(SpecialUsageMailbox::Sent)
        } else if name.eq_ignore_ascii_case("trash") {
            Some(SpecialUsageMailbox::Trash)
        } else {
            Some(SpecialUsageMailbox::Normal)
        }
    }
}

pub trait BackendMailbox: Debug {
    fn hash(&self) -> MailboxHash;
    fn name(&self) -> &str;
    /// Path of mailbox within the mailbox hierarchy, with `/` as separator.
    fn path(&self) -> &str;
    fn change_name(&mut self, new_name: &str);
    fn clone(&self) -> Mailbox;
    fn children(&self) -> &[MailboxHash];
    fn parent(&self) -> Option<MailboxHash>;
    fn is_subscribed(&self) -> bool;
    fn set_is_subscribed(&mut self, new_val: bool) -> Result<()>;
    fn set_special_usage(&mut self, new_val: SpecialUsageMailbox) -> Result<()>;
    fn special_usage(&self) -> SpecialUsageMailbox;
    fn permissions(&self) -> MailboxPermissions;
    fn count(&self) -> Result<(usize, usize)>;
}

pub type AccountHash = u64;
pub type MailboxHash = u64;
pub type Mailbox = Box<dyn BackendMailbox + Send + Sync>;

impl Clone for Mailbox {
    fn clone(&self) -> Self {
        BackendMailbox::clone(self.deref())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
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
        MailboxPermissions {
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

#[derive(Debug, Clone, PartialEq)]
pub struct EnvelopeHashBatch {
    pub first: EnvelopeHash,
    pub rest: SmallVec<[EnvelopeHash; 64]>,
}

impl From<EnvelopeHash> for EnvelopeHashBatch {
    fn from(value: EnvelopeHash) -> Self {
        EnvelopeHashBatch {
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
        Ok(EnvelopeHashBatch {
            first: value[0],
            rest: value[1..].iter().cloned().collect(),
        })
    }
}

impl EnvelopeHashBatch {
    fn iter(&self) -> impl std::iter::Iterator<Item = EnvelopeHash> + '_ {
        std::iter::once(self.first).chain(self.rest.iter().cloned())
    }
}
