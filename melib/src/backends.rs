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
#[cfg(feature = "jmap_backend")]
pub use self::jmap::JmapType;
use crate::async_workers::*;
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
                    create_fn: Box::new(|| Box::new(|f, i| MaildirType::new(f, i))),
                    validate_conf_fn: Box::new(MaildirType::validate_config),
                },
            );
        }
        #[cfg(feature = "mbox_backend")]
        {
            b.register(
                "mbox".to_string(),
                Backend {
                    create_fn: Box::new(|| Box::new(|f, i| MboxType::new(f, i))),
                    validate_conf_fn: Box::new(MboxType::validate_config),
                },
            );
        }
        #[cfg(feature = "imap_backend")]
        {
            b.register(
                "imap".to_string(),
                Backend {
                    create_fn: Box::new(|| Box::new(|f, i| imap::ImapType::new(f, i))),
                    validate_conf_fn: Box::new(imap::ImapType::validate_config),
                },
            );
        }
        #[cfg(feature = "notmuch_backend")]
        {
            if libloading::Library::new("libnotmuch.so.5").is_ok() {
                b.register(
                    "notmuch".to_string(),
                    Backend {
                        create_fn: Box::new(|| Box::new(|f, i| NotmuchDb::new(f, i))),
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
                    create_fn: Box::new(|| Box::new(|f, i| JmapType::new(f, i))),
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
    mailbox_hash: MailboxHash,
    account_hash: AccountHash,
    kind: RefreshEventKind,
}

impl RefreshEvent {
    pub fn mailbox_hash(&self) -> MailboxHash {
        self.mailbox_hash
    }

    pub fn account_hash(&self) -> AccountHash {
        self.account_hash
    }

    pub fn kind(self) -> RefreshEventKind {
        /* consumes self! */
        self.kind
    }
}

/// A `RefreshEventConsumer` is a boxed closure that must be used to consume a `RefreshEvent` and
/// send it to a UI provided channel. We need this level of abstraction to provide an interface for
/// all users of mailbox refresh events.
pub struct RefreshEventConsumer(Box<dyn Fn(RefreshEvent) -> () + Send + Sync>);
impl RefreshEventConsumer {
    pub fn new(b: Box<dyn Fn(RefreshEvent) -> () + Send + Sync>) -> Self {
        RefreshEventConsumer(b)
    }
    pub fn send(&self, r: RefreshEvent) {
        self.0(r);
    }
}

impl fmt::Debug for RefreshEventConsumer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RefreshEventConsumer")
    }
}

pub struct NotifyFn(Box<dyn Fn(MailboxHash) -> () + Send + Sync>);

impl fmt::Debug for NotifyFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NotifyFn Box")
    }
}

impl From<Box<dyn Fn(MailboxHash) -> () + Send + Sync>> for NotifyFn {
    fn from(kind: Box<dyn Fn(MailboxHash) -> () + Send + Sync>) -> Self {
        NotifyFn(kind)
    }
}

impl NotifyFn {
    pub fn new(b: Box<dyn Fn(MailboxHash) -> () + Send + Sync>) -> Self {
        NotifyFn(b)
    }
    pub fn notify(&self, f: MailboxHash) {
        self.0(f);
    }
}

pub type ResultFuture<T> = Result<Pin<Box<dyn Future<Output = Result<T>> + Send + 'static>>>;

pub trait MailBackend: ::std::fmt::Debug + Send + Sync {
    fn is_async(&self) -> bool;
    fn is_remote(&self) -> bool;
    fn supports_search(&self) -> bool;
    fn is_online(&self) -> Result<()> {
        Err(MeliError::new("Unimplemented."))
    }
    fn is_online_async(&self) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }
    fn get(&mut self, mailbox: &Mailbox) -> Result<Async<Result<Vec<Envelope>>>>;
    fn get_async(
        &mut self,
        _mailbox: &Mailbox,
    ) -> Result<Pin<Box<dyn Stream<Item = Result<Vec<Envelope>>> + Send + 'static>>> {
        Err(MeliError::new("Unimplemented."))
    }
    fn refresh(
        &mut self,
        _mailbox_hash: MailboxHash,
        _sender: RefreshEventConsumer,
    ) -> Result<Async<()>> {
        Err(MeliError::new("Unimplemented."))
    }
    fn refresh_async(
        &mut self,
        _mailbox_hash: MailboxHash,
        _sender: RefreshEventConsumer,
    ) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }
    fn watch(
        &self,
        sender: RefreshEventConsumer,
        work_context: WorkContext,
    ) -> Result<std::thread::ThreadId>;
    fn watch_async(&self, _sender: RefreshEventConsumer) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }
    fn mailboxes(&self) -> Result<HashMap<MailboxHash, Mailbox>>;
    fn mailboxes_async(&self) -> ResultFuture<HashMap<MailboxHash, Mailbox>> {
        Err(MeliError::new("Unimplemented."))
    }
    fn operation(&self, hash: EnvelopeHash) -> Result<Box<dyn BackendOp>>;

    fn save(
        &self,
        bytes: Vec<u8>,
        mailbox_hash: MailboxHash,
        flags: Option<Flag>,
    ) -> ResultFuture<()>;
    fn delete(&self, _env_hash: EnvelopeHash, _mailbox_hash: MailboxHash) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }
    fn tags(&self) -> Option<Arc<RwLock<BTreeMap<u64, String>>>> {
        None
    }
    fn as_any(&self) -> &dyn Any;

    fn as_any_mut(&mut self) -> &mut dyn Any {
        unimplemented!()
    }

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
    fn copy_to(&self, _mailbox_hash: MailboxHash) -> ResultFuture<()> {
        Err(MeliError::new("Unimplemented."))
    }
    fn fetch_flags(&self) -> ResultFuture<Flag>;
    fn set_flag(&mut self, flag: Flag, value: bool) -> ResultFuture<()>;
    fn set_tag(&mut self, tag: String, value: bool) -> ResultFuture<()>;
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
    fn set_flag(&mut self, _flag: Flag, _value: bool) -> ResultFuture<()> {
        Err(MeliError::new("read-only set."))
    }
    fn set_tag(&mut self, _tag: String, _value: bool) -> ResultFuture<()> {
        Err(MeliError::new("read-only set."))
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

#[derive(Debug)]
struct DummyMailbox {
    v: Vec<MailboxHash>,
}

impl BackendMailbox for DummyMailbox {
    fn hash(&self) -> MailboxHash {
        0
    }

    fn name(&self) -> &str {
        ""
    }

    fn path(&self) -> &str {
        ""
    }

    fn change_name(&mut self, _s: &str) {}

    fn clone(&self) -> Mailbox {
        mailbox_default()
    }

    fn special_usage(&self) -> SpecialUsageMailbox {
        SpecialUsageMailbox::Normal
    }

    fn children(&self) -> &[MailboxHash] {
        &self.v
    }

    fn parent(&self) -> Option<MailboxHash> {
        None
    }

    fn permissions(&self) -> MailboxPermissions {
        MailboxPermissions::default()
    }
    fn is_subscribed(&self) -> bool {
        true
    }
    fn set_is_subscribed(&mut self, _new_val: bool) -> Result<()> {
        Ok(())
    }
    fn set_special_usage(&mut self, _new_val: SpecialUsageMailbox) -> Result<()> {
        Ok(())
    }
    fn count(&self) -> Result<(usize, usize)> {
        Ok((0, 0))
    }
}

pub fn mailbox_default() -> Mailbox {
    Box::new(DummyMailbox {
        v: Vec::with_capacity(0),
    })
}

pub type AccountHash = u64;
pub type MailboxHash = u64;
pub type Mailbox = Box<dyn BackendMailbox + Send + Sync>;

impl Clone for Mailbox {
    fn clone(&self) -> Self {
        BackendMailbox::clone(self.deref())
    }
}

impl Default for Mailbox {
    fn default() -> Self {
        mailbox_default()
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
