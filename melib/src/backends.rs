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
use smallvec::SmallVec;

#[cfg(feature = "imap")]
pub mod imap;
#[cfg(feature = "imap")]
pub mod nntp;
#[cfg(feature = "notmuch")]
pub mod notmuch;
#[cfg(feature = "notmuch")]
pub use self::notmuch::NotmuchDb;
#[cfg(feature = "jmap")]
pub mod jmap;
#[cfg(feature = "maildir")]
pub mod maildir;
#[cfg(feature = "mbox")]
pub mod mbox;
use std::{
    any::Any,
    borrow::Cow,
    collections::{BTreeSet, HashMap},
    fmt,
    fmt::Debug,
    future::Future,
    ops::Deref,
    pin::Pin,
    sync::{Arc, RwLock},
};

use futures::stream::Stream;

#[cfg(feature = "imap")]
pub use self::imap::ImapType;
#[cfg(feature = "maildir")]
use self::maildir::MaildirType;
#[cfg(feature = "mbox")]
use self::mbox::MboxType;
#[cfg(feature = "imap")]
pub use self::nntp::NntpType;
use super::email::{Envelope, EnvelopeHash, Flag};
use crate::{
    conf::AccountSettings,
    error::{Error, ErrorKind, Result},
    LogLevel,
};

#[macro_export]
macro_rules! get_path_hash {
    ($path:expr) => {{
        use std::{
            collections::hash_map::DefaultHasher,
            hash::{Hash, Hasher},
        };
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

#[cfg(all(feature = "notmuch", target_os = "unix"))]
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

#[cfg(all(feature = "notmuch", not(any(target_os = "unix", target_os = "macos"))))]
pub const NOTMUCH_ERROR_DETAILS: &str = r#"If notmuch is installed but the library isn't found, consult your system's documentation on how to make dynamic libraries discoverable."#;

impl Backends {
    pub fn new() -> Self {
        let mut b = Self {
            map: HashMap::with_capacity_and_hasher(1, Default::default()),
        };
        #[cfg(feature = "maildir")]
        {
            b.register(
                "maildir".to_string(),
                Backend {
                    create_fn: Box::new(|| Box::new(|f, i, ev| MaildirType::new(f, i, ev))),
                    validate_conf_fn: Box::new(MaildirType::validate_config),
                },
            );
        }
        #[cfg(feature = "mbox")]
        {
            b.register(
                "mbox".to_string(),
                Backend {
                    create_fn: Box::new(|| Box::new(|f, i, ev| MboxType::new(f, i, ev))),
                    validate_conf_fn: Box::new(MboxType::validate_config),
                },
            );
        }
        #[cfg(feature = "imap")]
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
        #[cfg(feature = "notmuch")]
        {
            b.register(
                "notmuch".to_string(),
                Backend {
                    create_fn: Box::new(|| Box::new(|f, i, ev| NotmuchDb::new(f, i, ev))),
                    validate_conf_fn: Box::new(NotmuchDb::validate_config),
                },
            );
        }
        #[cfg(feature = "jmap")]
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
            })?
            .validate_conf_fn)(s)
    }
}

#[derive(Debug, Clone)]
pub enum BackendEvent {
    Notice {
        description: String,
        content: Option<String>,
        level: LogLevel,
    },
    Refresh(RefreshEvent),
    AccountStateChange {
        message: Cow<'static, str>,
    },
    //Job(Box<Future<Output = Result<()>> + Send + 'static>)
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

#[derive(Debug, Clone)]
pub enum RefreshEventKind {
    Update(EnvelopeHash, Box<Envelope>),
    /// Rename(old_hash, new_hash)
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
        Self(b)
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

    #[allow(clippy::type_complexity)]
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
    ) -> ResultFuture<SmallVec<[EnvelopeHash; 512]>>;

    fn submit(
        &self,
        _bytes: Vec<u8>,
        _mailbox_hash: Option<MailboxHash>,
        _flags: Option<Flag>,
    ) -> ResultFuture<()> {
        Err(Error::new("Submission not supported in this backend.")
            .set_kind(ErrorKind::NotSupported))
    }
}

/// A `BackendOp` manages common operations for the various mail backends. They
/// only live for the duration of the operation. They are generated by the
/// `operation` method of `Mailbackend` trait.
///
/// # Motivation
///
/// We need a way to do various operations on individual mails regardless of
/// what backend they come from (eg local or imap).
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
/// }
///
/// let operation = Box::new(FooOp {});
/// ```
pub trait BackendOp: ::std::fmt::Debug + ::std::marker::Send {
    fn as_bytes(&mut self) -> ResultFuture<Vec<u8>>;
}

/// Wrapper for BackendOps that are to be set read-only.
///
/// Warning: Backend implementations may still cause side-effects (for example
/// IMAP can set the Seen flag when fetching an envelope)
#[derive(Debug)]
pub struct ReadOnlyOp {
    op: Box<dyn BackendOp>,
}

impl ReadOnlyOp {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(op: Box<dyn BackendOp>) -> Box<dyn BackendOp> {
        Box::new(Self { op })
    }
}

impl BackendOp for ReadOnlyOp {
    fn as_bytes(&mut self) -> ResultFuture<Vec<u8>> {
        self.op.as_bytes()
    }
}

#[derive(Default, Debug, Copy, Hash, Eq, Clone, Serialize, Deserialize, PartialEq)]
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

pub trait BackendMailbox: Debug {
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Default, Clone)]
pub struct LazyCountSet {
    not_yet_seen: usize,
    set: BTreeSet<EnvelopeHash>,
}

impl fmt::Debug for LazyCountSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("LazyCountSet")
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
        if self.not_yet_seen == 0 {
            false
        } else {
            if !self.set.contains(&new_val) {
                self.not_yet_seen -= 1;
            }
            self.set.insert(new_val);
            true
        }
    }

    pub fn insert_existing_set(&mut self, set: BTreeSet<EnvelopeHash>) {
        let old_len = self.set.len();
        self.set.extend(set.into_iter());
        self.not_yet_seen = self.not_yet_seen.saturating_sub(self.set.len() - old_len);
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
        self.set.insert(new_val);
    }

    pub fn insert_set(&mut self, set: BTreeSet<EnvelopeHash>) {
        self.set.extend(set.into_iter());
    }

    pub fn remove(&mut self, env_hash: EnvelopeHash) -> bool {
        self.set.remove(&env_hash)
    }
}

#[test]
fn test_lazy_count_set() {
    let mut new = LazyCountSet::default();
    assert_eq!(new.len(), 0);
    new.set_not_yet_seen(10);
    assert_eq!(new.len(), 10);
    for i in 0..10 {
        assert!(new.insert_existing(EnvelopeHash(i)));
    }
    assert_eq!(new.len(), 10);
    assert!(!new.insert_existing(EnvelopeHash(10)));
    assert_eq!(new.len(), 10);
}

pub struct IsSubscribedFn(Box<dyn Fn(&str) -> bool + Send + Sync>);

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
