/*
 * meli
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    convert::TryFrom,
    fs::File,
    future::Future,
    io::{BufWriter, Write},
    ops::{Deref, DerefMut},
    pin::Pin,
};

use futures::future::try_join_all;
use melib::{
    backends::EnvelopeHashBatch, mbox::MboxMetadata, utils::datetime, FlagOp, UnixTimestamp,
};
use smallvec::SmallVec;

use super::*;
use crate::{
    accounts::{JobRequest, MailboxStatus},
    components::ExtendShortcutsMaps,
};

// [ref:TODO]: emoji_text_presentation_selector should be printed along with the chars
// before it but not as a separate Cell
//macro_rules! emoji_text_presentation_selector {
//    () => {
//        "\u{FE0E}"
//    };
//}
//
//pub const DEFAULT_ATTACHMENT_FLAG: &str = concat!("📎",
// emoji_text_presentation_selector!()); pub const DEFAULT_SELECTED_FLAG: &str =
// concat!("☑️", emoji_text_presentation_selector!());
// pub const DEFAULT_UNSEEN_FLAG: &str = concat!("●",
// emoji_text_presentation_selector!()); pub const DEFAULT_SNOOZED_FLAG: &str =
// concat!("💤", emoji_text_presentation_selector!());

pub const DEFAULT_ATTACHMENT_FLAG: &str = "📎";
pub const DEFAULT_SELECTED_FLAG: &str = "☑️";
pub const DEFAULT_UNSEEN_FLAG: &str = "●";
pub const DEFAULT_SNOOZED_FLAG: &str = "💤";

#[derive(Debug, Default)]
pub struct RowsState<T> {
    pub selection: HashMap<EnvelopeHash, bool>,
    pub row_updates: SmallVec<[EnvelopeHash; 8]>,
    // [ref:FIXME]: env vec should have at least one element guaranteed
    pub thread_to_env: HashMap<ThreadHash, SmallVec<[EnvelopeHash; 8]>>,
    pub env_to_thread: HashMap<EnvelopeHash, ThreadHash>,
    pub thread_order: HashMap<ThreadHash, usize>,
    pub env_order: HashMap<EnvelopeHash, usize>,
    #[allow(clippy::type_complexity)]
    pub entries: Vec<(T, EntryStrings)>,
    pub all_threads: HashSet<ThreadHash>,
    pub all_envelopes: HashSet<EnvelopeHash>,
    pub row_attr_cache: HashMap<usize, ThemeAttribute>,
}

impl<T> RowsState<T> {
    #[inline(always)]
    pub fn clear(&mut self) {
        self.selection.clear();
        self.row_updates.clear();
        self.thread_to_env.clear();
        self.env_to_thread.clear();
        self.thread_order.clear();
        self.env_order.clear();
        self.entries.clear();
        self.all_threads.clear();
        self.all_envelopes.clear();
        self.row_attr_cache.clear();
    }

    #[inline(always)]
    pub fn is_thread_selected(&self, thread: ThreadHash) -> bool {
        debug_assert!(self.all_threads.contains(&thread));
        debug_assert!(self.thread_order.contains_key(&thread));
        debug_assert!(self.thread_to_env.contains_key(&thread));
        self.thread_to_env
            .get(&thread)
            .iter()
            .flat_map(|v| v.iter())
            .any(|env_hash| self.selection[env_hash])
    }

    #[inline(always)]
    pub fn insert_thread(
        &mut self,
        thread: ThreadHash,
        metadata: T,
        mut env_hashes: SmallVec<[EnvelopeHash; 8]>,
        entry_strings: EntryStrings,
    ) {
        env_hashes.dedup();
        env_hashes.retain(|h| !self.all_envelopes.contains(h));
        if env_hashes.is_empty() {
            return;
        }
        let index = self.entries.len();
        for &env_hash in &env_hashes {
            self.selection.insert(env_hash, false);
            self.env_to_thread.insert(env_hash, thread);
            self.env_order.insert(env_hash, index);
            self.all_envelopes.insert(env_hash);
        }
        if !self.all_threads.contains(&thread) {
            self.thread_order.insert(thread, index);
            self.all_threads.insert(thread);
            self.thread_to_env.insert(thread, env_hashes);
        } else {
            self.thread_to_env
                .entry(thread)
                .or_default()
                .extend_from_slice(&env_hashes);
        }
        self.entries.push((metadata, entry_strings));
    }

    #[inline(always)]
    pub fn row_update_add_thread(&mut self, thread: ThreadHash) {
        let env_hashes = self.thread_to_env.entry(thread).or_default().clone();
        for env_hash in env_hashes {
            self.row_updates.push(env_hash);
        }
    }

    #[inline(always)]
    pub fn row_update_add_envelope(&mut self, env_hash: EnvelopeHash) {
        self.row_updates.push(env_hash);
    }

    #[inline(always)]
    pub fn contains_thread(&self, thread: ThreadHash) -> bool {
        debug_assert_eq!(
            self.all_threads.contains(&thread),
            self.thread_order.contains_key(&thread)
        );
        debug_assert_eq!(
            self.thread_order.contains_key(&thread),
            self.thread_to_env.contains_key(&thread)
        );
        self.thread_order.contains_key(&thread)
    }

    #[inline(always)]
    pub fn contains_env(&self, env_hash: EnvelopeHash) -> bool {
        self.all_envelopes.contains(&env_hash)
    }

    #[inline(always)]
    pub fn update_selection_with_thread(
        &mut self,
        thread: ThreadHash,
        mut cl: impl FnMut(&mut bool),
    ) {
        let env_hashes = self.thread_to_env.entry(thread).or_default().clone();
        for env_hash in env_hashes {
            self.selection.entry(env_hash).and_modify(&mut cl);
            self.row_updates.push(env_hash);
        }
    }

    #[inline(always)]
    pub fn update_selection_with_env(
        &mut self,
        env_hash: EnvelopeHash,
        mut cl: impl FnMut(&mut bool),
    ) {
        self.selection.entry(env_hash).and_modify(&mut cl);
        self.row_updates.push(env_hash);
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[inline(always)]
    pub fn clear_selection(&mut self) {
        for (k, v) in self.selection.iter_mut() {
            if *v {
                *v = false;
                self.row_updates.push(*k);
            }
        }
    }

    pub fn rename_env(&mut self, old_hash: EnvelopeHash, new_hash: EnvelopeHash) {
        self.row_updates.push(new_hash);
        if let Some(row) = self.env_order.remove(&old_hash) {
            self.env_order.insert(new_hash, row);
        }
        if let Some(thread) = self.env_to_thread.remove(&old_hash) {
            self.thread_to_env
                .entry(thread)
                .or_default()
                .retain(|h| *h != old_hash);
            self.thread_to_env.entry(thread).or_default().push(new_hash);
        }
        let selection_status = self.selection.remove(&old_hash).unwrap_or(false);
        self.selection.insert(new_hash, selection_status);
        self.all_envelopes.remove(&old_hash);
        self.all_envelopes.insert(old_hash);
    }
}

mod conversations;
pub use self::conversations::*;

mod compact;
pub use self::compact::*;

mod thread;
pub use self::thread::*;

mod plain;
pub use self::plain::*;

mod offline;
pub use self::offline::*;

#[derive(Clone, Copy, Debug)]
pub enum Focus {
    None,
    Entry,
    EntryFullscreen,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum Modifier {
    #[default]
    SymmetricDifference,
    Union,
    Difference,
    Intersection,
}

#[derive(Debug, Default)]
/// Save theme colors to avoid looking them up again and again from settings
pub struct ColorCache {
    pub theme_default: ThemeAttribute,

    pub unseen: ThemeAttribute,
    pub highlighted: ThemeAttribute,
    pub selected: ThemeAttribute,
    pub highlighted_selected: ThemeAttribute,
    pub even: ThemeAttribute,
    pub odd: ThemeAttribute,
    pub even_unseen: ThemeAttribute,
    pub even_highlighted: ThemeAttribute,
    pub even_selected: ThemeAttribute,
    pub odd_unseen: ThemeAttribute,
    pub odd_highlighted: ThemeAttribute,
    pub odd_selected: ThemeAttribute,
    pub even_highlighted_selected: ThemeAttribute,
    pub odd_highlighted_selected: ThemeAttribute,
    pub tag_default: ThemeAttribute,

    // Conversations
    pub subject: ThemeAttribute,
    pub from: ThemeAttribute,
    pub date: ThemeAttribute,
}

impl ColorCache {
    pub fn new(context: &Context, style: IndexStyle) -> Self {
        let mut ret = match style {
            IndexStyle::Plain => Self {
                even: crate::conf::value(context, "mail.listing.plain.even"),
                odd: crate::conf::value(context, "mail.listing.plain.odd"),
                even_unseen: crate::conf::value(context, "mail.listing.plain.even_unseen"),
                odd_unseen: crate::conf::value(context, "mail.listing.plain.odd_unseen"),
                even_highlighted: crate::conf::value(
                    context,
                    "mail.listing.plain.even_highlighted",
                ),
                odd_highlighted: crate::conf::value(context, "mail.listing.plain.odd_highlighted"),
                odd_highlighted_selected: crate::conf::value(
                    context,
                    "mail.listing.plain.odd_highlighted_selected",
                ),
                even_selected: crate::conf::value(context, "mail.listing.plain.even_selected"),
                even_highlighted_selected: crate::conf::value(
                    context,
                    "mail.listing.plain.even_highlighted_selected",
                ),
                odd_selected: crate::conf::value(context, "mail.listing.plain.odd_selected"),
                tag_default: crate::conf::value(context, "mail.listing.tag_default"),
                theme_default: crate::conf::value(context, "theme_default"),
                ..Self::default()
            },
            IndexStyle::Threaded => Self {
                even_unseen: crate::conf::value(context, "mail.listing.plain.even_unseen"),
                even_selected: crate::conf::value(context, "mail.listing.plain.even_selected"),
                even_highlighted: crate::conf::value(
                    context,
                    "mail.listing.plain.even_highlighted",
                ),
                even_highlighted_selected: crate::conf::value(
                    context,
                    "mail.listing.plain.even_highlighted_selected",
                ),
                odd_unseen: crate::conf::value(context, "mail.listing.plain.odd_unseen"),
                odd_selected: crate::conf::value(context, "mail.listing.plain.odd_selected"),
                odd_highlighted: crate::conf::value(context, "mail.listing.plain.odd_highlighted"),
                odd_highlighted_selected: crate::conf::value(
                    context,
                    "mail.listing.plain.odd_highlighted_selected",
                ),
                even: crate::conf::value(context, "mail.listing.plain.even"),
                odd: crate::conf::value(context, "mail.listing.plain.odd"),
                tag_default: crate::conf::value(context, "mail.listing.tag_default"),
                theme_default: crate::conf::value(context, "theme_default"),
                ..Self::default()
            },
            IndexStyle::Compact => Self {
                even_unseen: crate::conf::value(context, "mail.listing.compact.even_unseen"),
                even_selected: crate::conf::value(context, "mail.listing.compact.even_selected"),
                even_highlighted: crate::conf::value(
                    context,
                    "mail.listing.compact.even_highlighted",
                ),
                even_highlighted_selected: crate::conf::value(
                    context,
                    "mail.listing.compact.even_highlighted_selected",
                ),
                odd_unseen: crate::conf::value(context, "mail.listing.compact.odd_unseen"),
                odd_selected: crate::conf::value(context, "mail.listing.compact.odd_selected"),
                odd_highlighted: crate::conf::value(
                    context,
                    "mail.listing.compact.odd_highlighted",
                ),
                odd_highlighted_selected: crate::conf::value(
                    context,
                    "mail.listing.compact.odd_highlighted_selected",
                ),
                even: crate::conf::value(context, "mail.listing.compact.even"),
                odd: crate::conf::value(context, "mail.listing.compact.odd"),
                tag_default: crate::conf::value(context, "mail.listing.tag_default"),
                theme_default: crate::conf::value(context, "theme_default"),
                ..Self::default()
            },
            IndexStyle::Conversations => Self {
                theme_default: crate::conf::value(context, "mail.listing.conversations"),
                subject: crate::conf::value(context, "mail.listing.conversations.subject"),
                from: crate::conf::value(context, "mail.listing.conversations.from"),
                date: crate::conf::value(context, "mail.listing.conversations.date"),
                selected: crate::conf::value(context, "mail.listing.conversations.selected"),
                unseen: crate::conf::value(context, "mail.listing.conversations.unseen"),
                highlighted: crate::conf::value(context, "mail.listing.conversations.highlighted"),
                highlighted_selected: crate::conf::value(
                    context,
                    "mail.listing.conversations.highlighted_selected",
                ),
                tag_default: crate::conf::value(context, "mail.listing.tag_default"),
                ..Self::default()
            },
        };
        if !context.settings.terminal.use_color() {
            ret.highlighted.attrs |= Attr::REVERSE;
            ret.tag_default.attrs |= Attr::REVERSE;
            ret.even_highlighted.attrs |= Attr::REVERSE;
            ret.odd_highlighted.attrs |= Attr::REVERSE;
            ret.even_highlighted_selected.attrs |= Attr::REVERSE | Attr::DIM;
            ret.odd_highlighted_selected.attrs |= Attr::REVERSE | Attr::DIM;
        }
        ret
    }
}

#[derive(Debug)]
pub struct EntryStrings {
    pub date: DateString,
    pub subject: SubjectString,
    pub flag: FlagString,
    pub from: FromString,
    pub tags: TagString,
}

#[macro_export]
macro_rules! digits_of_num {
    ($num:expr) => {{
        const GUESS: [usize; 65] = [
            1, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6, 7, 7, 7, 8, 8,
            8, 9, 9, 9, 9, 10, 10, 10, 11, 11, 11, 12, 12, 12, 12, 13, 13, 13, 14, 14, 14, 15, 15,
            15, 15, 16, 16, 16, 17, 17, 17, 18, 18, 18, 18, 19,
        ];
        const TENS: [usize; 20] = [
            1,
            10,
            100,
            1000,
            10000,
            100000,
            1000000,
            10000000,
            100000000,
            1000000000,
            10000000000,
            100000000000,
            1000000000000,
            10000000000000,
            100000000000000,
            1000000000000000,
            10000000000000000,
            100000000000000000,
            1000000000000000000,
            10000000000000000000,
        ];
        const SIZE_IN_BITS: usize = std::mem::size_of::<usize>() * 8;

        let leading_zeros = $num.leading_zeros() as usize;
        let base_two_digits: usize = SIZE_IN_BITS - leading_zeros;
        let x = GUESS[base_two_digits];
        x + if $num >= TENS[x] { 1 } else { 0 }
    }};
}

macro_rules! column_str {
    (
        struct $name:ident($($t:ty),+)) => {
        #[derive(Debug)]
        pub struct $name($(pub $t),+);

        impl Deref for $name {
            type Target = String;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
}

column_str!(struct DateString(String));
column_str!(struct FromString(String));
column_str!(struct SubjectString(String));
column_str!(struct FlagString(String));
column_str!(struct TagString(String, SmallVec<[Option<Color>; 8]>));

#[derive(Clone, Copy, Debug)]
struct MailboxMenuEntry {
    depth: usize,
    indentation: u32,
    has_sibling: bool,
    visible: bool,
    collapsed: bool,
    mailbox_hash: MailboxHash,
    index_style: Option<IndexStyle>,
}

#[derive(Debug)]
struct AccountMenuEntry {
    name: String,
    hash: AccountHash,
    index: usize,
    entries: SmallVec<[MailboxMenuEntry; 16]>,
}

pub trait MailListingTrait: ListingTrait {
    fn as_component(&self) -> &dyn Component
    where
        Self: Sized,
    {
        self
    }

    fn as_component_mut(&mut self) -> &mut dyn Component
    where
        Self: Sized,
    {
        self
    }

    fn perform_action(
        &mut self,
        context: &mut Context,
        envs_to_set: SmallVec<[EnvelopeHash; 8]>,
        a: &ListingAction,
    ) {
        fn inner(
            context: &mut Context,
            envs_to_set: SmallVec<[EnvelopeHash; 8]>,
            account_hash: AccountHash,
            mailbox_hash: MailboxHash,
            a: &ListingAction,
        ) {
            let env_hashes = if let Ok(batch) = EnvelopeHashBatch::try_from(envs_to_set.as_slice())
            {
                batch
            } else {
                return;
            };
            let account = &mut context.accounts[&account_hash];
            match a {
                ListingAction::Flag(FlagAction::Set(Flag::SEEN)) | ListingAction::SetSeen => {
                    if let Err(err) = account.set_flags(
                        env_hashes,
                        mailbox_hash,
                        smallvec::smallvec![FlagOp::Set(Flag::SEEN)],
                    ) {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                }
                ListingAction::Flag(FlagAction::Unset(Flag::SEEN)) | ListingAction::SetUnseen => {
                    if let Err(err) = account.set_flags(
                        env_hashes,
                        mailbox_hash,
                        smallvec::smallvec![FlagOp::UnSet(Flag::SEEN)],
                    ) {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                }
                ListingAction::Flag(FlagAction::Set(flag)) => {
                    if let Err(err) = account.set_flags(
                        env_hashes,
                        mailbox_hash,
                        smallvec::smallvec![FlagOp::Set(*flag)],
                    ) {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                }
                ListingAction::Flag(FlagAction::Unset(flag)) => {
                    if let Err(err) = account.set_flags(
                        env_hashes,
                        mailbox_hash,
                        smallvec::smallvec![FlagOp::UnSet(*flag)],
                    ) {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                }
                ListingAction::Tag(TagAction::Add(ref tag_str)) => {
                    if let Err(err) = account.set_flags(
                        env_hashes,
                        mailbox_hash,
                        smallvec::smallvec![FlagOp::SetTag(tag_str.into())],
                    ) {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                }
                ListingAction::Tag(TagAction::Remove(ref tag_str)) => {
                    if let Err(err) = account.set_flags(
                        env_hashes,
                        mailbox_hash,
                        smallvec::smallvec![FlagOp::UnSetTag(tag_str.into())],
                    ) {
                        context.replies.push_back(UIEvent::StatusEvent(
                            StatusEvent::DisplayMessage(err.to_string()),
                        ));
                    }
                }
                ListingAction::Delete => {
                    let job = account
                        .backend
                        .write()
                        .unwrap()
                        .delete_messages(env_hashes.clone(), mailbox_hash);
                    match job {
                        Err(err) => {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(err.to_string()),
                            ));
                        }
                        Ok(fut) => {
                            let handle = account
                                .main_loop_handler
                                .job_executor
                                .spawn_specialized("delete".into(), fut);
                            account.insert_job(
                                handle.job_id,
                                JobRequest::DeleteMessages { env_hashes, handle },
                            );
                        }
                    }
                }
                ListingAction::CopyTo(ref mailbox_path) => {
                    match account.mailbox_by_path(mailbox_path).and_then(
                        |destination_mailbox_hash| {
                            account.backend.write().unwrap().copy_messages(
                                env_hashes,
                                mailbox_hash,
                                destination_mailbox_hash,
                                /* move? */ false,
                            )
                        },
                    ) {
                        Err(err) => {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(err.to_string()),
                            ));
                        }
                        Ok(fut) => {
                            let handle = account
                                .main_loop_handler
                                .job_executor
                                .spawn_specialized("copy_to_mailbox".into(), fut);
                            account.insert_job(
                                handle.job_id,
                                JobRequest::Generic {
                                    name: "message copying".into(),
                                    handle,
                                    on_finish: None,
                                    log_level: LogLevel::INFO,
                                },
                            );
                        }
                    }
                }
                ListingAction::CopyToOtherAccount(ref _account_name, ref _mailbox_path) => {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                            "Copying to another account is currently unimplemented".into(),
                        )));
                }
                ListingAction::MoveTo(ref mailbox_path) => {
                    match account.mailbox_by_path(mailbox_path).and_then(
                        |destination_mailbox_hash| {
                            account.backend.write().unwrap().copy_messages(
                                env_hashes,
                                mailbox_hash,
                                destination_mailbox_hash,
                                /* move? */ true,
                            )
                        },
                    ) {
                        Err(err) => {
                            context.replies.push_back(UIEvent::StatusEvent(
                                StatusEvent::DisplayMessage(err.to_string()),
                            ));
                        }
                        Ok(fut) => {
                            let handle = account
                                .main_loop_handler
                                .job_executor
                                .spawn_specialized("move_to_mailbox".into(), fut);
                            account.insert_job(
                                handle.job_id,
                                JobRequest::Generic {
                                    name: "message moving".into(),
                                    handle,
                                    on_finish: None,
                                    log_level: LogLevel::INFO,
                                },
                            );
                        }
                    }
                }
                ListingAction::ExportMbox(format, ref path) => {
                    let futures: Result<Vec<_>> = envs_to_set
                        .iter()
                        .map(|&env_hash| {
                            account.operation(env_hash).and_then(|mut op| op.as_bytes())
                        })
                        .collect::<Result<Vec<_>>>();
                    let mut path = path.to_path_buf();
                    if path.is_relative() {
                        path = context.current_dir().join(&path);
                    }
                    let account = &mut context.accounts[&account_hash];
                    let format = (*format).unwrap_or_default();
                    let collection = account.collection.clone();
                    let (sender, mut receiver) = crate::jobs::oneshot::channel();
                    let fut: Pin<Box<dyn Future<Output = Result<()>> + Send + 'static>> =
                        Box::pin(async move {
                            let cl = async move {
                                // fully capture variables.
                                let _ = (&envs_to_set, &collection);
                                let bytes: Vec<Vec<u8>> = try_join_all(futures?).await?;
                                let envs: Vec<_> = envs_to_set
                                    .iter()
                                    .map(|&env_hash| collection.get_env(env_hash))
                                    .collect();
                                if path.is_dir() {
                                    if envs.len() == 1 {
                                        path.push(format!("{}.mbox", envs[0].message_id_raw()));
                                    } else {
                                        let now = datetime::timestamp_to_string(
                                            datetime::now(),
                                            Some(datetime::formats::RFC3339_DATETIME),
                                            false,
                                        );
                                        path.push(format!(
                                            "{}-{}-{}_envelopes.mbox",
                                            now,
                                            envs[0].message_id_raw(),
                                            envs.len(),
                                        ));
                                    }
                                }
                                let mut file = BufWriter::new(File::create(&path)?);
                                let mut iter = envs.iter().zip(bytes);
                                let tags_lck = collection.tag_index.read().unwrap();
                                if let Some((env, ref bytes)) = iter.next() {
                                    let tags: Vec<&str> = env
                                        .tags()
                                        .iter()
                                        .filter_map(|h| tags_lck.get(h).map(|s| s.as_str()))
                                        .collect();
                                    format.append(
                                        &mut file,
                                        bytes.as_slice(),
                                        env.from().first(),
                                        Some(env.date()),
                                        (env.flags(), tags),
                                        MboxMetadata::CClient,
                                        true,
                                        false,
                                    )?;
                                }
                                for (env, bytes) in iter {
                                    let tags: Vec<&str> = env
                                        .tags()
                                        .iter()
                                        .filter_map(|h| tags_lck.get(h).map(|s| s.as_str()))
                                        .collect();
                                    format.append(
                                        &mut file,
                                        bytes.as_slice(),
                                        env.from().first(),
                                        Some(env.date()),
                                        (env.flags(), tags),
                                        MboxMetadata::CClient,
                                        false,
                                        false,
                                    )?;
                                }
                                file.flush()?;
                                Ok(path)
                            };
                            let r: Result<PathBuf> = cl.await;
                            let _ = sender.send(r);
                            Ok(())
                        });
                    let handle = account
                        .main_loop_handler
                        .job_executor
                        .spawn_blocking("exporting mbox".into(), fut);
                    account.insert_job(
                        handle.job_id,
                        JobRequest::Generic {
                            name: "exporting mbox".into(),
                            handle,
                            on_finish: Some(CallbackFn(Box::new(move |context: &mut Context| {
                                context.replies.push_back(match receiver.try_recv() {
                                    Err(_) | Ok(None) => UIEvent::Notification {
                                        title: Some("Could not export mbox".into()),
                                        source: None,
                                        body: "Job was canceled.".into(),
                                        kind: Some(NotificationType::Info),
                                    },
                                    Ok(Some(Err(err))) => UIEvent::Notification {
                                        title: Some("Could not export mbox".into()),
                                        source: None,
                                        body: err.to_string().into(),
                                        kind: Some(NotificationType::Error(err.kind)),
                                    },
                                    Ok(Some(Ok(path))) => UIEvent::Notification {
                                        title: Some("Successfully exported mbox".into()),
                                        source: None,
                                        body: format!("Wrote to file {}", path.display()).into(),
                                        kind: Some(NotificationType::Info),
                                    },
                                });
                            }))),
                            log_level: LogLevel::INFO,
                        },
                    );
                }
                ListingAction::MoveToOtherAccount(ref _account_name, ref _mailbox_path) => {
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::DisplayMessage(
                            "Moving to another account is currently unimplemented".into(),
                        )));
                }
                _ => unreachable!(),
            }
        }
        let account_hash = self.coordinates().0;
        let mailbox_hash = self.coordinates().1;
        /*{
            let threads_lck = account.collection.get_threads(mailbox_hash);
            for thread_hash in thread_hashes {
                for (_, h) in threads_lck.thread_iter(thread_hash) {
                    envs_to_set.push(threads_lck.thread_nodes()[&h].message().unwrap());
                }
                self.row_updates().push(thread_hash);
            }
        }
        */
        inner(context, envs_to_set, account_hash, mailbox_hash, a);
        self.set_dirty(true);
    }

    fn row_updates(&mut self) -> &mut SmallVec<[EnvelopeHash; 8]>;
    fn selection(&mut self) -> &mut HashMap<EnvelopeHash, bool>;
    fn get_focused_items(&self, _context: &Context) -> SmallVec<[EnvelopeHash; 8]>;
    fn redraw_threads_list(
        &mut self,
        context: &Context,
        items: Box<dyn Iterator<Item = ThreadHash>>,
    );

    fn redraw_envelope_list(
        &mut self,
        _context: &Context,
        _items: Box<dyn Iterator<Item = EnvelopeHash>>,
    ) {
    }

    /// Use `force` when there have been changes in the mailbox or account lists
    /// in `context`
    fn refresh_mailbox(&mut self, context: &mut Context, force: bool);
}

pub trait ListingTrait: Component {
    fn coordinates(&self) -> (AccountHash, MailboxHash);
    fn set_coordinates(&mut self, _: (AccountHash, MailboxHash));
    fn next_entry(&mut self, context: &mut Context);
    fn prev_entry(&mut self, context: &mut Context);
    fn draw_list(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context);
    fn highlight_line(&mut self, grid: &mut CellBuffer, area: Area, idx: usize, context: &Context);
    fn filter(
        &mut self,
        _filter_term: String,
        _results: SmallVec<[EnvelopeHash; 512]>,
        _context: &Context,
    ) {
    }
    fn unfocused(&self) -> bool;
    fn view_area(&self) -> Option<Area>;
    fn set_modifier_active(&mut self, _new_val: bool);
    fn set_modifier_command(&mut self, _new_val: Option<Modifier>);
    fn modifier_command(&self) -> Option<Modifier>;
    fn set_movement(&mut self, mvm: PageMovement);
    fn focus(&self) -> Focus;
    fn set_focus(&mut self, new_value: Focus, context: &mut Context);

    fn kick_parent(&self, parent: ComponentId, msg: ListingMessage, context: &mut Context) {
        log::trace!(
            "kick_parent self is {} parent is {} msg is {:?}",
            self.id(),
            parent,
            &msg
        );
        context.replies.push_back(UIEvent::IntraComm {
            from: self.id(),
            to: parent,
            content: Box::new(msg),
        });
    }

    fn format_date(&self, context: &Context, epoch: UnixTimestamp) -> String {
        let d = std::time::UNIX_EPOCH + std::time::Duration::from_secs(epoch);
        let now: std::time::Duration = std::time::SystemTime::now()
            .duration_since(d)
            .unwrap_or_else(|_| std::time::Duration::new(std::u64::MAX, 0));
        match now.as_secs() {
            n if context.settings.listing.recent_dates && n < 60 * 60 => format!(
                "{} minute{} ago",
                n / (60),
                if n / 60 == 1 { "" } else { "s" }
            ),
            n if context.settings.listing.recent_dates && n < 24 * 60 * 60 => format!(
                "{} hour{} ago",
                n / (60 * 60),
                if n / (60 * 60) == 1 { "" } else { "s" }
            ),
            n if context.settings.listing.recent_dates && n < 7 * 24 * 60 * 60 => format!(
                "{} day{} ago",
                n / (24 * 60 * 60),
                if n / (24 * 60 * 60) == 1 { "" } else { "s" }
            ),
            _ => melib::utils::datetime::timestamp_to_string(
                epoch,
                context
                    .settings
                    .listing
                    .datetime_fmt
                    .as_deref()
                    .or(Some("%Y-%m-%d %T")),
                false,
            ),
        }
    }
}

#[derive(Debug)]
pub enum ListingComponent {
    Compact(Box<CompactListing>),
    Conversations(Box<ConversationsListing>),
    Offline(Box<OfflineListing>),
    Plain(Box<PlainListing>),
    Threaded(Box<ThreadListing>),
}
use crate::ListingComponent::*;

impl std::ops::Deref for ListingComponent {
    type Target = dyn MailListingTrait;

    fn deref(&self) -> &Self::Target {
        match &self {
            Compact(ref l) => l.as_ref(),
            Conversations(ref l) => l.as_ref(),
            Offline(ref l) => l.as_ref(),
            Plain(ref l) => l.as_ref(),
            Threaded(ref l) => l.as_ref(),
        }
    }
}

impl std::ops::DerefMut for ListingComponent {
    fn deref_mut(&mut self) -> &mut (dyn MailListingTrait + 'static) {
        match self {
            Compact(l) => l.as_mut(),
            Conversations(l) => l.as_mut(),
            Offline(l) => l.as_mut(),
            Plain(l) => l.as_mut(),
            Threaded(l) => l.as_mut(),
        }
    }
}

impl ListingComponent {
    fn id(&self) -> ComponentId {
        match self {
            Compact(l) => l.as_component().id(),
            Conversations(l) => l.as_component().id(),
            Offline(l) => l.as_component().id(),
            Plain(l) => l.as_component().id(),
            Threaded(l) => l.as_component().id(),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
enum ListingFocus {
    Menu,
    Mailbox,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct CursorPos {
    account: usize,
    menu: MenuEntryCursor,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum MenuEntryCursor {
    Status,
    Mailbox(usize),
}

impl std::ops::Sub<MenuEntryCursor> for isize {
    type Output = Self;

    fn sub(self, other: MenuEntryCursor) -> Self {
        if let MenuEntryCursor::Mailbox(v) = other {
            v as Self - self
        } else {
            self - 1
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ShowMenuScrollbar {
    Never,
    True,
    False,
}

#[derive(Debug)]
pub struct Listing {
    component: ListingComponent,
    accounts: Vec<AccountMenuEntry>,
    status: Option<AccountStatus>,
    dirty: bool,
    cursor_pos: CursorPos,
    menu_cursor_pos: CursorPos,
    menu: Screen<Virtual>,
    menu_scrollbar_show_timer: crate::jobs::Timer,
    show_menu_scrollbar: ShowMenuScrollbar,
    startup_checks_rate: RateLimit,
    id: ComponentId,
    theme_default: ThemeAttribute,

    sidebar_divider: char,
    sidebar_divider_theme: ThemeAttribute,

    menu_visibility: bool,
    cmd_buf: String,
    /// This is the width of the right container to the entire width.
    ratio: usize, // right/(container width) * 100
    prev_ratio: usize,
    menu_width: WidgetWidth,
    focus: ListingFocus,
    view: Box<ThreadView>,
}

impl std::fmt::Display for Listing {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.component {
            Compact(ref l) => write!(f, "{}", l),
            Conversations(ref l) => write!(f, "{}", l),
            Offline(ref l) => write!(f, "{}", l),
            Plain(ref l) => write!(f, "{}", l),
            Threaded(ref l) => write!(f, "{}", l),
        }
    }
}

impl Component for Listing {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.is_dirty() {
            return;
        }
        let total_cols = area.width();

        let right_component_width = if self.is_menu_visible() {
            if self.focus == ListingFocus::Menu {
                (self.ratio * total_cols) / 100
            } else {
                match self.menu_width {
                    WidgetWidth::Set(ref mut v) | WidgetWidth::Hold(ref mut v) => {
                        if *v == 0 {
                            *v = 1;
                        } else if *v >= total_cols {
                            *v = total_cols.saturating_sub(2);
                        }
                        total_cols.saturating_sub(*v)
                    }
                    WidgetWidth::Unset => {
                        self.menu_width =
                            WidgetWidth::Set(total_cols - ((self.ratio * total_cols) / 100));
                        (self.ratio * total_cols) / 100
                    }
                }
            }
        } else {
            total_cols
        };
        let mid = area.width().saturating_sub(right_component_width);
        if self.dirty && mid != 0 {
            let divider_area = area.nth_col(mid);
            for row in grid.bounds_iter(divider_area) {
                for c in row {
                    grid[c]
                        .set_ch(self.sidebar_divider)
                        .set_fg(self.sidebar_divider_theme.fg)
                        .set_bg(self.sidebar_divider_theme.bg)
                        .set_attrs(self.sidebar_divider_theme.attrs);
                }
            }
            context.dirty_areas.push_back(divider_area);
        }

        let account_hash = self.accounts[self.cursor_pos.account].hash;
        if right_component_width == total_cols {
            if context.is_online(account_hash).is_err()
                && !matches!(self.component, ListingComponent::Offline(_))
            {
                self.component.unrealize(context);
                self.component =
                    Offline(OfflineListing::new((account_hash, MailboxHash::default())));
                self.component
                    .process_event(&mut UIEvent::VisibilityChange(true), context);
                self.component.realize(self.id().into(), context);
            }

            if let Some(s) = self.status.as_mut() {
                s.draw(grid, area, context);
            } else {
                self.component.draw(grid, area, context);
                if self.component.unfocused() {
                    self.view
                        .draw(grid, self.component.view_area().unwrap_or(area), context);
                }
            }
        } else if right_component_width == 0 {
            self.draw_menu(grid, area, context);
        } else {
            self.draw_menu(grid, area.take_cols(mid), context);
            if context.is_online(account_hash).is_err()
                && !matches!(self.component, ListingComponent::Offline(_))
            {
                self.component.unrealize(context);
                self.component =
                    Offline(OfflineListing::new((account_hash, MailboxHash::default())));
                self.component
                    .process_event(&mut UIEvent::VisibilityChange(true), context);
                self.component.realize(self.id().into(), context);
            }
            if let Some(s) = self.status.as_mut() {
                s.draw(grid, area.skip_cols(mid + 1), context);
            } else {
                let area = area.skip_cols(mid + 1);
                self.component.draw(grid, area, context);
                if self.component.unfocused() {
                    self.view
                        .draw(grid, self.component.view_area().unwrap_or(area), context);
                }
            }
        }
        self.dirty = false;
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        match event {
            UIEvent::ConfigReload { old_settings: _ } => {
                self.theme_default = crate::conf::value(context, "theme_default");
                let account_hash = context.accounts[self.cursor_pos.account].hash();
                self.sidebar_divider =
                    *account_settings!(context[account_hash].listing.sidebar_divider);
                self.sidebar_divider_theme = conf::value(context, "mail.sidebar_divider");
                self.menu.grid_mut().empty();
                self.set_dirty(true);
            }
            UIEvent::Timer(n) if *n == self.menu_scrollbar_show_timer.id() => {
                if self.show_menu_scrollbar == ShowMenuScrollbar::True {
                    self.show_menu_scrollbar = ShowMenuScrollbar::False;
                    self.set_dirty(true);
                    self.menu.grid_mut().empty();
                }
                return true;
            }
            UIEvent::StartupCheck(ref f)
                if self.component.coordinates().1 == *f && !self.startup_checks_rate.tick() =>
            {
                return false;
            }
            UIEvent::Timer(n) if *n == self.startup_checks_rate.id() => {
                if self.startup_checks_rate.active {
                    self.startup_checks_rate.reset();
                    return self.process_event(
                        &mut UIEvent::StartupCheck(self.component.coordinates().1),
                        context,
                    );
                }
            }
            UIEvent::AccountStatusChange(account_hash, msg) => {
                let account_index: usize = context
                    .accounts
                    .get_index_of(account_hash)
                    .expect("Invalid account_hash in UIEventMailbox{Delete,Create}");
                if self.cursor_pos.account == account_index {
                    self.change_account(context);
                } else {
                    let previous_collapsed_mailboxes: BTreeSet<MailboxHash> = self.accounts
                        [account_index]
                        .entries
                        .iter()
                        .filter_map(|e| {
                            if e.collapsed {
                                Some(e.mailbox_hash)
                            } else {
                                None
                            }
                        })
                        .collect::<_>();
                    let previous_index_styles: BTreeMap<MailboxHash, IndexStyle> = self.accounts
                        [account_index]
                        .entries
                        .iter()
                        .filter_map(|e| Some((e.mailbox_hash, e.index_style?)))
                        .collect::<_>();
                    self.accounts[account_index].entries = context.accounts[&*account_hash]
                        .list_mailboxes()
                        .into_iter()
                        .filter(|mailbox_node| {
                            context.accounts[&*account_hash][&mailbox_node.hash]
                                .ref_mailbox
                                .is_subscribed()
                        })
                        .map(|f| MailboxMenuEntry {
                            depth: f.depth,
                            indentation: f.indentation,
                            has_sibling: f.has_sibling,
                            mailbox_hash: f.hash,
                            visible: true,
                            collapsed: if previous_collapsed_mailboxes.is_empty() {
                                context.accounts[&*account_hash][&f.hash].conf.collapsed
                            } else {
                                previous_collapsed_mailboxes.contains(&f.hash)
                            },
                            index_style: previous_index_styles.get(&f.hash).copied(),
                        })
                        .collect::<_>();
                    self.menu.grid_mut().empty();
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(match msg {
                            Some(msg) => format!("{} {}", self.status(context), msg),
                            None => self.status(context),
                        })));
                }
            }
            UIEvent::MailboxDelete((account_hash, mailbox_hash))
            | UIEvent::MailboxCreate((account_hash, mailbox_hash)) => {
                let account_index = context
                    .accounts
                    .get_index_of(account_hash)
                    .expect("Invalid account_hash in UIEventMailbox{Delete,Create}");
                self.menu.grid_mut().empty();
                let previous_collapsed_mailboxes: BTreeSet<MailboxHash> = self.accounts
                    [account_index]
                    .entries
                    .iter()
                    .filter_map(|e| {
                        if e.collapsed {
                            Some(e.mailbox_hash)
                        } else {
                            None
                        }
                    })
                    .collect::<_>();
                let previous_index_styles: BTreeMap<MailboxHash, IndexStyle> = self.accounts
                    [account_index]
                    .entries
                    .iter()
                    .filter_map(|e| Some((e.mailbox_hash, e.index_style?)))
                    .collect::<_>();
                self.accounts[account_index].entries = context.accounts[&*account_hash]
                    .list_mailboxes()
                    .into_iter()
                    .filter(|mailbox_node| {
                        context.accounts[&*account_hash][&mailbox_node.hash]
                            .ref_mailbox
                            .is_subscribed()
                    })
                    .map(|f| MailboxMenuEntry {
                        depth: f.depth,
                        indentation: f.indentation,
                        has_sibling: f.has_sibling,
                        mailbox_hash: f.hash,
                        visible: true,
                        collapsed: previous_collapsed_mailboxes.contains(&f.hash),
                        index_style: previous_index_styles.get(&f.hash).copied(),
                    })
                    .collect::<_>();
                let mut fallback = 0;
                if let MenuEntryCursor::Mailbox(ref mut cur) = self.cursor_pos.menu {
                    *cur = std::cmp::min(
                        self.accounts[self.cursor_pos.account]
                            .entries
                            .len()
                            .saturating_sub(1),
                        *cur,
                    );
                    fallback = *cur;
                }
                if self.component.coordinates() == (*account_hash, *mailbox_hash) {
                    self.component
                        .process_event(&mut UIEvent::VisibilityChange(false), context);
                    self.component.set_coordinates((
                        self.accounts[self.cursor_pos.account].hash,
                        self.accounts[self.cursor_pos.account].entries[fallback].mailbox_hash,
                    ));
                    self.component.refresh_mailbox(context, true);
                    self.component
                        .process_event(&mut UIEvent::VisibilityChange(true), context);
                }
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.status(context),
                    )));
                self.set_dirty(true);
                return true;
            }
            UIEvent::ChangeMode(UIMode::Normal) => {
                self.set_dirty(true);
            }
            UIEvent::Resize => {
                self.set_dirty(true);
            }
            UIEvent::Action(Action::ViewMailbox(ref idx)) => {
                if let Some(MailboxMenuEntry { mailbox_hash, .. }) =
                    self.accounts[self.cursor_pos.account].entries.get(*idx)
                {
                    let account_hash = self.accounts[self.cursor_pos.account].hash;
                    self.cursor_pos.menu = MenuEntryCursor::Mailbox(*idx);
                    self.status = None;
                    self.component
                        .process_event(&mut UIEvent::VisibilityChange(false), context);
                    self.component
                        .set_coordinates((account_hash, *mailbox_hash));
                    self.component
                        .process_event(&mut UIEvent::VisibilityChange(true), context);
                    self.menu.grid_mut().empty();
                    self.set_dirty(true);
                }
                return true;
            }
            UIEvent::IntraComm {
                from,
                to,
                ref content,
            } if (*from, *to) == (self.component.id(), self.id()) => {
                match content.downcast_ref::<ListingMessage>().copied() {
                    None => {}
                    Some(ListingMessage::FocusUpdate { new_value }) => {
                        self.view.process_event(
                            &mut UIEvent::VisibilityChange(!matches!(new_value, Focus::None)),
                            context,
                        );
                        if matches!(new_value, Focus::Entry) {
                            // Need to clear gap between sidebar and listing component, if any.
                            self.dirty = true;
                        }
                    }
                    Some(ListingMessage::UpdateView) => {
                        self.view.set_dirty(true);
                    }
                    Some(ListingMessage::OpenEntryUnderCursor {
                        env_hash,
                        thread_hash,
                        show_thread,
                        go_to_first_unread,
                    }) => {
                        let (a, m) = self.component.coordinates();
                        self.view.unrealize(context);
                        self.view = Box::new(ThreadView::new(
                            (a, m, env_hash),
                            thread_hash,
                            Some(env_hash),
                            go_to_first_unread,
                            if show_thread {
                                None
                            } else {
                                Some(ThreadViewFocus::MailView)
                            },
                            context,
                        ));
                    }
                }
                return true;
            }
            #[cfg(feature = "debug-tracing")]
            UIEvent::IntraComm {
                from,
                to,
                ref content,
            } => {
                if *from == self.component.id() || *to == self.id() {
                    log::debug!(
                        "BUG intracomm event: {:?} downcast content {:?}",
                        event,
                        content.downcast_ref::<ListingMessage>().copied()
                    );
                    log::debug!(
                        "BUG component is {} and self id is {}",
                        self.component.id(),
                        self.id()
                    );
                }
            }
            _ => {}
        }

        if self.component.unfocused() && self.view.process_event(event, context) {
            return true;
        }

        if self.focus == ListingFocus::Mailbox && self.status.is_some() {
            if let Some(s) = self.status.as_mut() {
                if s.process_event(event, context) {
                    return true;
                }
            }
        }
        if (self.focus == ListingFocus::Mailbox && self.status.is_none())
            && ((self.component.unfocused() && self.view.process_event(event, context))
                || self.component.process_event(event, context))
        {
            return true;
        }

        let shortcuts = {
            let mut m = self.shortcuts(context);
            m.insert(
                Shortcuts::GENERAL,
                context.settings.shortcuts.general.key_values(),
            );
            m
        };
        if self.focus == ListingFocus::Mailbox {
            match *event {
                UIEvent::Input(Key::Mouse(MouseEvent::Press(MouseButton::Left, x, _y)))
                    if self.is_menu_visible() =>
                {
                    match self.menu_width {
                        WidgetWidth::Hold(wx) | WidgetWidth::Set(wx)
                            if wx + 1 == usize::from(x) =>
                        {
                            self.menu_width = WidgetWidth::Hold(wx - 1);
                        }
                        WidgetWidth::Set(_) => return false,
                        WidgetWidth::Hold(x) => {
                            self.menu_width = WidgetWidth::Set(x);
                        }
                        WidgetWidth::Unset => return false,
                    }
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(Key::Mouse(MouseEvent::Hold(x, _y))) if self.is_menu_visible() => {
                    match self.menu_width {
                        WidgetWidth::Hold(ref mut hx) => {
                            *hx = usize::from(x).saturating_sub(1);
                        }
                        _ => return false,
                    }
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(Key::Mouse(MouseEvent::Release(x, _y)))
                    if self.is_menu_visible() =>
                {
                    match self.menu_width {
                        WidgetWidth::Hold(_) => {
                            self.menu_width = WidgetWidth::Set(usize::from(x).saturating_sub(1));
                        }
                        _ => return false,
                    }
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref k)
                    if self.is_menu_visible()
                        && shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_left"]) =>
                {
                    self.focus = ListingFocus::Menu;
                    if self.show_menu_scrollbar != ShowMenuScrollbar::Never {
                        self.menu_scrollbar_show_timer.rearm();
                        self.show_menu_scrollbar = ShowMenuScrollbar::True;
                    }
                    self.prev_ratio = self.ratio;
                    self.ratio = 50;
                    self.set_dirty(true);
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["next_mailbox"])
                        || shortcut!(k == shortcuts[Shortcuts::LISTING]["prev_mailbox"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    let target = match k {
                        k if shortcut!(k == shortcuts[Shortcuts::LISTING]["next_mailbox"]) => {
                            match self.cursor_pos.menu {
                                MenuEntryCursor::Status => amount.saturating_sub(1),
                                MenuEntryCursor::Mailbox(idx) => idx + amount,
                            }
                        }
                        k if shortcut!(k == shortcuts[Shortcuts::LISTING]["prev_mailbox"]) => {
                            match self.cursor_pos.menu {
                                MenuEntryCursor::Status => {
                                    return true;
                                }
                                MenuEntryCursor::Mailbox(idx) => {
                                    if idx >= amount {
                                        idx - amount
                                    } else {
                                        return true;
                                    }
                                }
                            }
                        }
                        _ => return true,
                    };
                    if self.accounts[self.cursor_pos.account]
                        .entries
                        .get(target)
                        .is_some()
                    {
                        self.cursor_pos.menu = MenuEntryCursor::Mailbox(target)
                    } else {
                        return true;
                    }
                    self.change_account(context);
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["next_account"])
                        || shortcut!(k == shortcuts[Shortcuts::LISTING]["prev_account"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    match k {
                        k if shortcut!(k == shortcuts[Shortcuts::LISTING]["next_account"]) => {
                            if self.cursor_pos.account + amount < self.accounts.len() {
                                self.cursor_pos.account += amount;
                                self.cursor_pos.menu = MenuEntryCursor::Mailbox(0);
                            } else {
                                return true;
                            }
                        }
                        k if shortcut!(k == shortcuts[Shortcuts::LISTING]["prev_account"]) => {
                            if self.cursor_pos.account >= amount {
                                self.cursor_pos.account -= amount;
                                self.cursor_pos.menu = MenuEntryCursor::Mailbox(0);
                            } else {
                                return true;
                            }
                        }
                        _ => return false,
                    }
                    self.change_account(context);

                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["toggle_menu_visibility"]) =>
                {
                    self.menu_visibility = !self.menu_visibility;
                    self.set_dirty(true);
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["increase_sidebar"]) =>
                {
                    self.ratio = self.ratio.saturating_sub(2);
                    self.prev_ratio = self.prev_ratio.saturating_sub(2);
                    self.menu_width = WidgetWidth::Unset;
                    self.set_dirty(true);
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["decrease_sidebar"]) =>
                {
                    self.ratio += 2;
                    self.ratio = std::cmp::min(100, self.ratio);
                    self.prev_ratio += 2;
                    self.prev_ratio = std::cmp::min(100, self.prev_ratio);
                    self.menu_width = WidgetWidth::Unset;
                    self.set_dirty(true);
                }
                _ => {}
            }

            if self.status.is_none() {
                match event {
                    UIEvent::Action(ref action) => match action {
                        Action::Listing(ListingAction::SetPlain) => {
                            self.set_index_style(IndexStyle::Plain, context);
                            return true;
                        }
                        Action::Listing(ListingAction::SetThreaded) => {
                            self.set_index_style(IndexStyle::Threaded, context);
                            return true;
                        }
                        Action::Listing(ListingAction::SetCompact) => {
                            self.set_index_style(IndexStyle::Compact, context);
                            return true;
                        }
                        Action::Listing(ListingAction::SetConversations) => {
                            self.set_index_style(IndexStyle::Conversations, context);
                            return true;
                        }
                        Action::Listing(ListingAction::Import(file_path, mailbox_path)) => {
                            let account = &mut context.accounts[self.cursor_pos.account];
                            if let Err(err) = account
                                .mailbox_by_path(mailbox_path)
                                .and_then(|mailbox_hash| {
                                    Ok((
                                        std::fs::read(file_path).chain_err_summary(|| {
                                            format!("Could not read {}", file_path.display())
                                        })?,
                                        mailbox_hash,
                                    ))
                                })
                                .and_then(|(bytes, mailbox_hash)| {
                                    account.save(&bytes, mailbox_hash, None)
                                })
                            {
                                context.replies.push_back(UIEvent::StatusEvent(
                                    StatusEvent::DisplayMessage(err.to_string()),
                                ));
                            }
                            return true;
                        }
                        Action::Listing(a @ ListingAction::SetSeen)
                        | Action::Listing(a @ ListingAction::SetUnseen)
                        | Action::Listing(a @ ListingAction::Delete)
                        | Action::Listing(a @ ListingAction::CopyTo(_))
                        | Action::Listing(a @ ListingAction::MoveTo(_))
                        | Action::Listing(a @ ListingAction::CopyToOtherAccount(_, _))
                        | Action::Listing(a @ ListingAction::MoveToOtherAccount(_, _))
                        | Action::Listing(a @ ListingAction::ExportMbox(_, _))
                        | Action::Listing(a @ ListingAction::Flag(_))
                        | Action::Listing(a @ ListingAction::Tag(_)) => {
                            let focused = self.component.get_focused_items(context);
                            self.component.perform_action(context, focused, a);
                            let should_be_unselected: bool = matches!(
                                a,
                                ListingAction::Delete
                                    | ListingAction::MoveTo(_)
                                    | ListingAction::MoveToOtherAccount(_, _)
                            );
                            let mut row_updates: SmallVec<[EnvelopeHash; 8]> = SmallVec::new();
                            for (k, v) in self.component.selection().iter_mut() {
                                if *v {
                                    *v = !should_be_unselected;
                                    row_updates.push(*k);
                                }
                            }
                            self.component.row_updates().extend(row_updates);
                            return true;
                        }
                        Action::Listing(ListingAction::ClearSelection) => {
                            // Clear selection.
                            let row_updates: SmallVec<[EnvelopeHash; 8]> =
                                self.component.get_focused_items(context);
                            for h in &row_updates {
                                if let Some(val) = self.component.selection().get_mut(h) {
                                    *val = false;
                                }
                            }
                            self.component.row_updates().extend(row_updates);
                            self.component.set_dirty(true);
                            return true;
                        }
                        _ => {}
                    },
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::LISTING]["scroll_up"]) =>
                    {
                        let amount = if self.cmd_buf.is_empty() {
                            1
                        } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            amount
                        } else {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            return true;
                        };
                        self.component.set_movement(PageMovement::Up(amount));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::LISTING]["scroll_down"]) =>
                    {
                        let amount = if self.cmd_buf.is_empty() {
                            1
                        } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            amount
                        } else {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            return true;
                        };
                        self.component.set_movement(PageMovement::Down(amount));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_right"]) =>
                    {
                        let amount = if self.cmd_buf.is_empty() {
                            1
                        } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            amount
                        } else {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            return true;
                        };
                        self.component.set_movement(PageMovement::Right(amount));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::GENERAL]["scroll_left"]) =>
                    {
                        let amount = if self.cmd_buf.is_empty() {
                            1
                        } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            amount
                        } else {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            return true;
                        };
                        self.component.set_movement(PageMovement::Left(amount));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::LISTING]["prev_page"]) =>
                    {
                        let mult = if self.cmd_buf.is_empty() {
                            1
                        } else if let Ok(mult) = self.cmd_buf.parse::<usize>() {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            mult
                        } else {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            return true;
                        };
                        self.component.set_movement(PageMovement::PageUp(mult));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::LISTING]["next_page"]) =>
                    {
                        let mult = if self.cmd_buf.is_empty() {
                            1
                        } else if let Ok(mult) = self.cmd_buf.parse::<usize>() {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            mult
                        } else {
                            self.cmd_buf.clear();
                            self.component.set_modifier_active(false);
                            context
                                .replies
                                .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                            return true;
                        };
                        self.component.set_movement(PageMovement::PageDown(mult));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::GENERAL]["home_page"]) =>
                    {
                        self.component.set_movement(PageMovement::Home);
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::GENERAL]["end_page"]) =>
                    {
                        self.component.set_movement(PageMovement::End);
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::LISTING]["search"]) =>
                    {
                        context
                            .replies
                            .push_back(UIEvent::CmdInput(Key::Paste("search ".to_string())));
                        context
                            .replies
                            .push_back(UIEvent::ChangeMode(UIMode::Command));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::LISTING]["set_seen"]) =>
                    {
                        let mut event = UIEvent::Action(Action::Listing(ListingAction::SetSeen));
                        if self.process_event(&mut event, context) {
                            return true;
                        }
                    }
                    UIEvent::Input(ref key)
                        if shortcut!(key == shortcuts[Shortcuts::LISTING]["refresh"]) =>
                    {
                        let account = &mut context.accounts[self.cursor_pos.account];
                        if let MenuEntryCursor::Mailbox(idx) = self.cursor_pos.menu {
                            if let Some(&mailbox_hash) = account.mailboxes_order.get(idx) {
                                if let Err(err) = account.refresh(mailbox_hash) {
                                    context.replies.push_back(UIEvent::Notification {
                                        title: Some("Could not refresh.".into()),
                                        source: None,
                                        body: err.to_string().into(),
                                        kind: Some(NotificationType::Error(err.kind)),
                                    });
                                }
                            }
                        }
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if !self.component.unfocused()
                            && shortcut!(
                                key == shortcuts[Shortcuts::LISTING]["union_modifier"]
                            )
                            && self.component.modifier_command().is_some() =>
                    {
                        self.component.set_modifier_command(Some(Modifier::Union));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if !self.component.unfocused()
                            && shortcut!(key == shortcuts[Shortcuts::LISTING]["diff_modifier"])
                            && self.component.modifier_command().is_some() =>
                    {
                        self.component
                            .set_modifier_command(Some(Modifier::Difference));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if !self.component.unfocused()
                            && shortcut!(
                                key == shortcuts[Shortcuts::LISTING]["intersection_modifier"]
                            )
                            && self.component.modifier_command().is_some() =>
                    {
                        self.component
                            .set_modifier_command(Some(Modifier::Intersection));
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if self.component.unfocused()
                            && shortcut!(key == shortcuts[Shortcuts::LISTING]["next_entry"]) =>
                    {
                        self.component.next_entry(context);
                        return true;
                    }
                    UIEvent::Input(ref key)
                        if self.component.unfocused()
                            && shortcut!(
                                key == shortcuts[Shortcuts::LISTING]["previous_entry"]
                            ) =>
                    {
                        self.component.prev_entry(context);
                        return true;
                    }
                    UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt(''))
                        if !self.component.unfocused() =>
                    {
                        // Clear selection.
                        let row_updates: SmallVec<[EnvelopeHash; 8]> =
                            self.component.get_focused_items(context);
                        for h in &row_updates {
                            if let Some(val) = self.component.selection().get_mut(h) {
                                *val = false;
                            }
                        }
                        self.component.row_updates().extend(row_updates);
                        self.component.set_dirty(true);
                        return true;
                    }
                    UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt(''))
                        if !self.cmd_buf.is_empty() =>
                    {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    }
                    UIEvent::Input(Key::Char(c)) if c.is_ascii_digit() => {
                        self.cmd_buf.push(*c);
                        self.component.set_modifier_active(true);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufSet(
                                self.cmd_buf.clone(),
                            )));
                        return true;
                    }
                    _ => {}
                }
            }
        } else if self.focus == ListingFocus::Menu {
            match *event {
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["focus_right"]) =>
                {
                    self.focus = ListingFocus::Mailbox;
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                            ScrollUpdate::End(self.id),
                        )));
                    self.ratio = self.prev_ratio;
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["open_mailbox"])
                        && self.menu_cursor_pos.menu == MenuEntryCursor::Status =>
                {
                    self.cursor_pos = self.menu_cursor_pos;
                    self.open_status(self.menu_cursor_pos.account, context);
                    self.set_dirty(true);
                    self.focus = ListingFocus::Mailbox;
                    self.ratio = self.prev_ratio;
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                            ScrollUpdate::End(self.id),
                        )));
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["toggle_mailbox_collapse"])
                        && matches!(self.menu_cursor_pos.menu, MenuEntryCursor::Mailbox(_)) =>
                {
                    let target_mailbox_idx =
                        if let MenuEntryCursor::Mailbox(idx) = self.menu_cursor_pos.menu {
                            idx
                        } else {
                            return false;
                        };
                    if let Some(target) = self.accounts[self.menu_cursor_pos.account]
                        .entries
                        .get_mut(target_mailbox_idx)
                    {
                        target.collapsed = !(target.collapsed);
                        self.dirty = true;
                        self.menu.grid_mut().empty();
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                                ScrollUpdate::End(self.id),
                            )));
                        return true;
                    }
                    return false;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["open_mailbox"]) =>
                {
                    self.cursor_pos = self.menu_cursor_pos;
                    self.change_account(context);
                    self.focus = ListingFocus::Mailbox;
                    self.ratio = self.prev_ratio;
                    self.set_dirty(true);
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                            ScrollUpdate::End(self.id),
                        )));
                    context
                        .replies
                        .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                            self.status(context),
                        )));
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["scroll_up"])
                        || shortcut!(k == shortcuts[Shortcuts::LISTING]["scroll_down"]) =>
                {
                    let mut amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["scroll_up"]) {
                        while amount > 0 {
                            match self.menu_cursor_pos {
                                CursorPos {
                                    ref mut account,
                                    menu: MenuEntryCursor::Status,
                                } => {
                                    if *account > 0 {
                                        *account -= 1;
                                        self.menu_cursor_pos.menu = MenuEntryCursor::Mailbox(
                                            self.accounts[*account].entries.len().saturating_sub(1),
                                        );
                                    } else {
                                        return true;
                                    }
                                }
                                CursorPos {
                                    ref account,
                                    menu: MenuEntryCursor::Mailbox(ref mut mailbox_idx),
                                } => loop {
                                    if *mailbox_idx > 0 {
                                        *mailbox_idx -= 1;
                                        if self.accounts[*account].entries[*mailbox_idx].visible {
                                            break;
                                        }
                                    } else {
                                        self.menu_cursor_pos.menu = MenuEntryCursor::Status;
                                        break;
                                    }
                                },
                            }

                            amount -= 1;
                        }
                    } else if shortcut!(k == shortcuts[Shortcuts::LISTING]["scroll_down"]) {
                        while amount > 0 {
                            match self.menu_cursor_pos {
                                // If current account has mailboxes, go to first mailbox
                                CursorPos {
                                    ref account,
                                    ref mut menu,
                                } if !self.accounts[*account].entries.is_empty()
                                    && *menu == MenuEntryCursor::Status =>
                                {
                                    *menu = MenuEntryCursor::Mailbox(0);
                                }
                                // If current account has no mailboxes, go to next account
                                CursorPos {
                                    ref mut account,
                                    ref mut menu,
                                } if *account + 1 < self.accounts.len()
                                    && *menu == MenuEntryCursor::Status =>
                                {
                                    *account += 1;
                                    *menu = MenuEntryCursor::Status;
                                }
                                // If current account has no mailboxes and there is no next account,
                                // return true
                                CursorPos {
                                    menu: MenuEntryCursor::Status,
                                    ..
                                } => {
                                    return true;
                                }
                                CursorPos {
                                    ref mut account,
                                    menu: MenuEntryCursor::Mailbox(ref mut mailbox_idx),
                                } => loop {
                                    if (*mailbox_idx + 1) < self.accounts[*account].entries.len() {
                                        *mailbox_idx += 1;
                                        if self.accounts[*account].entries[*mailbox_idx].visible {
                                            break;
                                        }
                                    } else if *account + 1 < self.accounts.len() {
                                        *account += 1;
                                        self.menu_cursor_pos.menu = MenuEntryCursor::Status;
                                        break;
                                    } else {
                                        return true;
                                    }
                                },
                            }

                            amount -= 1;
                        }
                    }
                    if self.show_menu_scrollbar != ShowMenuScrollbar::Never {
                        self.menu_scrollbar_show_timer.rearm();
                        self.show_menu_scrollbar = ShowMenuScrollbar::True;
                    }
                    self.menu.grid_mut().empty();
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["next_mailbox"])
                        || shortcut!(k == shortcuts[Shortcuts::LISTING]["prev_mailbox"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    let target = match k {
                        k if shortcut!(k == shortcuts[Shortcuts::LISTING]["next_mailbox"]) => {
                            match self.menu_cursor_pos.menu {
                                MenuEntryCursor::Status => amount.saturating_sub(1),
                                MenuEntryCursor::Mailbox(idx) => idx + amount,
                            }
                        }
                        k if shortcut!(k == shortcuts[Shortcuts::LISTING]["prev_mailbox"]) => {
                            match self.menu_cursor_pos.menu {
                                MenuEntryCursor::Status => {
                                    return true;
                                }
                                MenuEntryCursor::Mailbox(idx) => {
                                    if idx >= amount {
                                        idx - amount
                                    } else {
                                        return true;
                                    }
                                }
                            }
                        }
                        _ => return true,
                    };
                    if self.accounts[self.menu_cursor_pos.account]
                        .entries
                        .get(target)
                        .is_some()
                    {
                        self.menu_cursor_pos.menu = MenuEntryCursor::Mailbox(target)
                    } else {
                        return true;
                    }
                    if self.show_menu_scrollbar != ShowMenuScrollbar::Never {
                        self.menu_scrollbar_show_timer.rearm();
                        self.show_menu_scrollbar = ShowMenuScrollbar::True;
                    }
                    self.menu.grid_mut().empty();
                    return true;
                }
                UIEvent::Input(ref k)
                    if shortcut!(k == shortcuts[Shortcuts::LISTING]["next_account"])
                        || shortcut!(k == shortcuts[Shortcuts::LISTING]["prev_account"])
                        || shortcut!(k == shortcuts[Shortcuts::LISTING]["next_page"])
                        || shortcut!(k == shortcuts[Shortcuts::LISTING]["prev_page"]) =>
                {
                    let amount = if self.cmd_buf.is_empty() {
                        1
                    } else if let Ok(amount) = self.cmd_buf.parse::<usize>() {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        amount
                    } else {
                        self.cmd_buf.clear();
                        self.component.set_modifier_active(false);
                        context
                            .replies
                            .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                        return true;
                    };
                    match k {
                        k if shortcut!(k == shortcuts[Shortcuts::LISTING]["next_account"])
                            || shortcut!(k == shortcuts[Shortcuts::LISTING]["next_page"]) =>
                        {
                            if self.menu_cursor_pos.account + amount >= self.accounts.len() {
                                // Go to last mailbox.
                                self.menu_cursor_pos.menu = MenuEntryCursor::Mailbox(
                                    self.accounts[self.menu_cursor_pos.account]
                                        .entries
                                        .len()
                                        .saturating_sub(1),
                                );
                            } else if self.menu_cursor_pos.account + amount < self.accounts.len() {
                                self.menu_cursor_pos.account += amount;
                                self.menu_cursor_pos.menu = MenuEntryCursor::Mailbox(0);
                            } else {
                                return true;
                            }
                        }
                        k if shortcut!(k == shortcuts[Shortcuts::LISTING]["prev_account"])
                            || shortcut!(k == shortcuts[Shortcuts::LISTING]["prev_page"]) =>
                        {
                            if self.menu_cursor_pos.account >= amount {
                                self.menu_cursor_pos.account -= amount;
                                self.menu_cursor_pos.menu = MenuEntryCursor::Mailbox(0);
                            } else {
                                return true;
                            }
                        }
                        _ => return false,
                    }
                    if self.show_menu_scrollbar != ShowMenuScrollbar::Never {
                        self.menu_scrollbar_show_timer.rearm();
                        self.show_menu_scrollbar = ShowMenuScrollbar::True;
                    }
                    self.menu.grid_mut().empty();
                    self.set_dirty(true);

                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::GENERAL]["home_page"]) =>
                {
                    if matches!(
                        self.menu_cursor_pos,
                        CursorPos {
                            account: 0,
                            menu: MenuEntryCursor::Mailbox(0)
                        }
                    ) {
                        return true;
                    }
                    if self.menu_cursor_pos.menu == MenuEntryCursor::Mailbox(0) {
                        self.menu_cursor_pos.account = 0;
                    } else {
                        self.menu_cursor_pos.menu = MenuEntryCursor::Mailbox(0);
                    }
                    if self.show_menu_scrollbar != ShowMenuScrollbar::Never {
                        self.menu_scrollbar_show_timer.rearm();
                        self.show_menu_scrollbar = ShowMenuScrollbar::True;
                    }
                    self.menu.grid_mut().empty();
                    self.set_dirty(true);
                    return true;
                }
                UIEvent::Input(ref key)
                    if shortcut!(key == shortcuts[Shortcuts::GENERAL]["end_page"]) =>
                {
                    let CursorPos {
                        ref mut account,
                        ref mut menu,
                    } = self.menu_cursor_pos;
                    if matches!(
                        (*account, *menu),
                        (a, MenuEntryCursor::Mailbox(
                            i
                        )) if a == self.accounts.len().saturating_sub(1) && i ==
                            self.accounts[*account].entries.len().saturating_sub(1)
                    ) {
                        // Do nothing, this is the End.
                        // "Father?"
                        // "Yes, son?"
                        // "I want to kill you"
                        // "Come on, baby"
                        return true;
                    } else if matches!(
                        *menu,
                        MenuEntryCursor::Mailbox(
                            i
                        ) if i ==
                            self.accounts[*account].entries.len().saturating_sub(1)
                    ) {
                        *account = self.accounts.len().saturating_sub(1);
                        *menu = MenuEntryCursor::Mailbox(0);
                    } else {
                        *menu = MenuEntryCursor::Mailbox(
                            self.accounts[*account].entries.len().saturating_sub(1),
                        );
                    }
                    if self.show_menu_scrollbar != ShowMenuScrollbar::Never {
                        self.menu_scrollbar_show_timer.rearm();
                        self.show_menu_scrollbar = ShowMenuScrollbar::True;
                    }
                    self.menu.grid_mut().empty();
                    self.set_dirty(true);
                    return true;
                }
                _ => {}
            }
        }
        match *event {
            UIEvent::Input(ref k) if shortcut!(k == shortcuts[Shortcuts::LISTING]["new_mail"]) => {
                let account_hash = context.accounts[self.cursor_pos.account].hash();
                let composer = Composer::with_account(account_hash, context);
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(New(Some(Box::new(composer))))));
                return true;
            }
            UIEvent::Action(Action::Tab(ManageMailboxes)) => {
                let account_pos = self.cursor_pos.account;
                let mgr = MailboxManager::new(context, account_pos);
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(New(Some(Box::new(mgr))))));
                return true;
            }
            UIEvent::Action(Action::Tab(ManageJobs)) => {
                let mgr = JobManager::new(context);
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(New(Some(Box::new(mgr))))));
                return true;
            }
            UIEvent::Action(Action::Compose(ComposeAction::Mailto(ref mailto))) => {
                let account_hash = context.accounts[self.cursor_pos.account].hash();
                let mut composer = Composer::with_account(account_hash, context);
                composer.set_draft(mailto.into(), context);
                context
                    .replies
                    .push_back(UIEvent::Action(Tab(New(Some(Box::new(composer))))));
                return true;
            }
            UIEvent::StartupCheck(_)
            | UIEvent::MailboxUpdate(_)
            | UIEvent::EnvelopeUpdate(_)
            | UIEvent::EnvelopeRename(_, _)
            | UIEvent::EnvelopeRemove(_, _) => {
                self.dirty = true;
                // clear menu to force redraw
                self.menu.grid_mut().empty();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.status(context),
                    )));
            }
            UIEvent::Input(Key::Backspace) if !self.cmd_buf.is_empty() => {
                self.cmd_buf.pop();
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufSet(
                        self.cmd_buf.clone(),
                    )));
                return true;
            }
            UIEvent::Input(Key::Esc) | UIEvent::Input(Key::Alt('')) if !self.cmd_buf.is_empty() => {
                self.cmd_buf.clear();
                self.component.set_modifier_active(false);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufClear));
                return true;
            }
            UIEvent::Input(Key::Char(c)) if c.is_ascii_digit() => {
                self.cmd_buf.push(c);
                self.component.set_modifier_active(true);
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::BufSet(
                        self.cmd_buf.clone(),
                    )));
                return true;
            }
            UIEvent::Input(ref key)
                if context
                    .settings
                    .shortcuts
                    .listing
                    .commands
                    .iter()
                    .any(|cmd| {
                        if cmd.shortcut == *key {
                            for cmd in &cmd.command {
                                context.replies.push_back(UIEvent::Command(cmd.to_string()));
                            }
                            return true;
                        }
                        false
                    }) =>
            {
                return true;
            }
            _ => {}
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || self
                .status
                .as_ref()
                .map(Component::is_dirty)
                .unwrap_or_else(|| self.component.is_dirty())
            || if self.component.unfocused() {
                self.view.is_dirty()
            } else {
                self.component.is_dirty()
            }
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        if let Some(s) = self.status.as_mut() {
            s.set_dirty(value);
        } else {
            self.component.set_dirty(value);
            if self.component.unfocused() {
                self.view.set_dirty(value);
            }
        }
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        let mut map = ShortcutMaps::default();
        if self.focus != ListingFocus::Menu && self.component.unfocused() {
            map.extend_shortcuts(self.view.shortcuts(context));
        }
        map.extend_shortcuts(if let Some(s) = self.status.as_ref() {
            s.shortcuts(context)
        } else {
            self.component.shortcuts(context)
        });
        let mut config_map = context.settings.shortcuts.listing.key_values();
        if self.focus != ListingFocus::Menu {
            config_map.remove("open_mailbox");
        }
        map.insert(Shortcuts::LISTING, config_map);

        map
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn status(&self, context: &Context) -> String {
        let mailbox_hash = match self.cursor_pos.menu {
            MenuEntryCursor::Mailbox(idx) => {
                if let Some(MailboxMenuEntry { mailbox_hash, .. }) =
                    self.accounts[self.cursor_pos.account].entries.get(idx)
                {
                    *mailbox_hash
                } else {
                    return String::new();
                }
            }
            MenuEntryCursor::Status => {
                return format!("{} status", &self.accounts[self.cursor_pos.account].name)
            }
        };

        let account = &context.accounts[self.cursor_pos.account];
        match account[&mailbox_hash].status {
            MailboxStatus::Available | MailboxStatus::Parsing(_, _) => {
                let (unseen, total) = account[&mailbox_hash]
                    .ref_mailbox
                    .count()
                    .ok()
                    .unwrap_or((0, 0));
                format!(
                    "Mailbox: {}, Messages: {}, New: {}{}",
                    account[&mailbox_hash].name(),
                    total,
                    unseen,
                    if account[&mailbox_hash].status.is_parsing() {
                        "(Loading...)"
                    } else {
                        ""
                    }
                )
            }
            MailboxStatus::Failed(_) | MailboxStatus::None => account[&mailbox_hash].status(),
        }
    }

    fn children(&self) -> IndexMap<ComponentId, &dyn Component> {
        let mut ret = IndexMap::default();
        ret.insert(
            self.component.id(),
            match &self.component {
                Compact(l) => l.as_component(),
                Conversations(l) => l.as_component(),
                Offline(l) => l.as_component(),
                Plain(l) => l.as_component(),
                Threaded(l) => l.as_component(),
            },
        );

        ret
    }

    fn children_mut(&mut self) -> IndexMap<ComponentId, &mut dyn Component> {
        let mut ret = IndexMap::default();
        ret.insert(
            self.component.id(),
            match &mut self.component {
                Compact(l) => l.as_component_mut(),
                Conversations(l) => l.as_component_mut(),
                Offline(l) => l.as_component_mut(),
                Plain(l) => l.as_component_mut(),
                Threaded(l) => l.as_component_mut(),
            },
        );

        ret
    }
}

impl Listing {
    pub fn new(context: &mut Context) -> Self {
        let account_entries: Vec<AccountMenuEntry> = context
            .accounts
            .iter()
            .enumerate()
            .map(|(i, (h, a))| {
                let entries: SmallVec<[MailboxMenuEntry; 16]> = a
                    .list_mailboxes()
                    .into_iter()
                    .filter(|mailbox_node| a[&mailbox_node.hash].ref_mailbox.is_subscribed())
                    .map(|f| MailboxMenuEntry {
                        depth: f.depth,
                        indentation: f.indentation,
                        has_sibling: f.has_sibling,
                        mailbox_hash: f.hash,
                        visible: true,
                        collapsed: a[&f.hash].conf.collapsed,
                        index_style: a[&f.hash].conf.conf_override().listing.index_style,
                    })
                    .collect::<_>();

                AccountMenuEntry {
                    name: a.name().to_string(),
                    hash: *h,
                    index: i,
                    entries,
                }
            })
            .collect();
        let first_account_hash = account_entries[0].hash;
        let mut ret = Self {
            component: Offline(OfflineListing::new((
                first_account_hash,
                MailboxHash::default(),
            ))),
            view: Box::<ThreadView>::default(),
            accounts: account_entries,
            status: None,
            dirty: true,
            cursor_pos: CursorPos {
                account: 0,
                menu: MenuEntryCursor::Mailbox(0),
            },
            menu_cursor_pos: CursorPos {
                account: 0,
                menu: MenuEntryCursor::Mailbox(0),
            },
            menu: Screen::<Virtual>::new(),
            menu_scrollbar_show_timer: context.main_loop_handler.job_executor.clone().create_timer(
                std::time::Duration::from_secs(0),
                std::time::Duration::from_millis(1200),
            ),
            show_menu_scrollbar: ShowMenuScrollbar::Never,
            startup_checks_rate: RateLimit::new(
                2,
                1000,
                context.main_loop_handler.job_executor.clone(),
            ),
            theme_default: conf::value(context, "theme_default"),
            id: ComponentId::default(),
            sidebar_divider: *account_settings!(
                context[first_account_hash].listing.sidebar_divider
            ),
            sidebar_divider_theme: conf::value(context, "mail.sidebar_divider"),
            menu_visibility: !*account_settings!(
                context[first_account_hash].listing.hide_sidebar_on_launch
            ),
            ratio: *account_settings!(context[first_account_hash].listing.sidebar_ratio),
            prev_ratio: *account_settings!(context[first_account_hash].listing.sidebar_ratio),
            menu_width: WidgetWidth::Unset,
            focus: ListingFocus::Mailbox,
            cmd_buf: String::with_capacity(4),
        };
        ret.component.realize(ret.id().into(), context);
        ret.change_account(context);
        ret
    }

    fn draw_menu(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        grid.clear_area(area, self.theme_default);
        let total_height: usize = 3 * (self.accounts.len())
            + self
                .accounts
                .iter()
                .map(|entry| entry.entries.len() + 1)
                .sum::<usize>();
        let min_width: usize = 2 * area.width();
        let (width, height) = self.menu.grid().size();
        let cursor = match self.focus {
            ListingFocus::Mailbox => self.cursor_pos,
            ListingFocus::Menu => self.menu_cursor_pos,
        };
        if min_width > width || height < total_height || self.dirty {
            let _ = self.menu.resize(min_width * 2, total_height);
            let mut y = 0;
            for a in 0..self.accounts.len() {
                let menu_area = self.menu.area().skip_rows(y);
                y += self.print_account(menu_area, a, context);
                y += 3;
            }
        }

        let rows = area.height();
        const SCROLLING_CONTEXT: usize = 3;
        let y_offset = (cursor.account)
            + self
                .accounts
                .iter()
                .take(cursor.account)
                .map(|entry| entry.entries.len() + 1)
                .sum::<usize>()
            + match cursor.menu {
                MenuEntryCursor::Status => 0,
                MenuEntryCursor::Mailbox(idx) => idx + 1,
            }
            + SCROLLING_CONTEXT;
        let skip_offset = if y_offset <= rows {
            0
        } else {
            rows * y_offset.wrapping_div(rows).saturating_sub(1) + y_offset.wrapping_rem(rows)
        };

        grid.copy_area(
            self.menu.grid(),
            area,
            self.menu
                .area()
                .skip_rows(skip_offset.min((self.menu.area().height() - 1).saturating_sub(rows)))
                .take_rows((skip_offset + rows).min(self.menu.area().height() - 1)),
        );
        if self.show_menu_scrollbar == ShowMenuScrollbar::True && total_height > rows {
            if self.focus == ListingFocus::Menu {
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                        ScrollUpdate::Update {
                            id: self.id,
                            context: ScrollContext {
                                shown_lines: skip_offset + rows,
                                total_lines: total_height,
                                has_more_lines: false,
                            },
                        },
                    )));
            }
            ScrollBar::default().set_show_arrows(true).draw(
                grid,
                area.nth_col(area.width().saturating_sub(1)),
                context,
                // position
                skip_offset,
                // visible_rows
                rows,
                // length
                total_height,
            );
        } else if total_height < rows {
            context
                .replies
                .push_back(UIEvent::StatusEvent(StatusEvent::ScrollUpdate(
                    ScrollUpdate::End(self.id),
                )));
        }

        context.dirty_areas.push_back(area);
    }

    /// Print a single account in the menu area.
    fn print_account(&mut self, mut area: Area, aidx: usize, context: &Context) -> usize {
        let account_y = self.menu.area().height() - area.height();
        #[derive(Clone, Copy, Debug)]
        struct Line {
            collapsed: bool,
            depth: usize,
            inc: isize,
            indentation: u32,
            has_sibling: bool,
            mailbox_idx: MailboxHash,
            count: Option<usize>,
            collapsed_count: Option<usize>,
        }
        // Each entry and its index in the account
        let mailboxes: HashMap<MailboxHash, Mailbox> = context.accounts[self.accounts[aidx].index]
            .mailbox_entries
            .iter()
            .map(|(&hash, entry)| (hash, entry.ref_mailbox.clone()))
            .collect();

        let cursor = match self.focus {
            ListingFocus::Mailbox => self.cursor_pos,
            ListingFocus::Menu => self.menu_cursor_pos,
        };

        let must_highlight_account: bool = cursor.account == self.accounts[aidx].index;

        let mut lines: Vec<Line> = Vec::new();

        for (
            i,
            &MailboxMenuEntry {
                depth,
                indentation,
                has_sibling,
                mailbox_hash,
                visible: _,
                collapsed,
                index_style: _,
            },
        ) in self.accounts[aidx].entries.iter().enumerate()
        {
            if mailboxes[&mailbox_hash].is_subscribed() {
                match context.accounts[self.accounts[aidx].index][&mailbox_hash].status {
                    MailboxStatus::Failed(_) => {
                        lines.push(Line {
                            collapsed,
                            depth,
                            inc: i as isize,
                            indentation,
                            has_sibling,
                            mailbox_idx: mailbox_hash,
                            count: None,
                            collapsed_count: None,
                        });
                    }
                    _ => {
                        lines.push(Line {
                            collapsed,
                            depth,
                            inc: i as isize,
                            indentation,
                            has_sibling,
                            mailbox_idx: mailbox_hash,
                            count: mailboxes[&mailbox_hash].count().ok().map(|(v, _)| v),
                            collapsed_count: None,
                        });
                    }
                }
            }
        }

        let account_attrs = if must_highlight_account {
            if cursor.menu == MenuEntryCursor::Status {
                let mut v = crate::conf::value(context, "mail.sidebar_highlighted");
                if !context.settings.terminal.use_color() {
                    v.attrs |= Attr::REVERSE;
                }
                v
            } else {
                crate::conf::value(context, "mail.sidebar_highlighted_account_name")
            }
        } else {
            crate::conf::value(context, "mail.sidebar_account_name")
        };

        // Print account name first
        self.menu.grid_mut().write_string(
            &self.accounts[aidx].name,
            account_attrs.fg,
            account_attrs.bg,
            account_attrs.attrs,
            area,
            None,
        );
        area = self.menu.area().skip_rows(account_y);

        if lines.is_empty() {
            self.menu.grid_mut().write_string(
                "offline",
                crate::conf::value(context, "error_message").fg,
                account_attrs.bg,
                account_attrs.attrs,
                area.skip_rows(1),
                None,
            );
            return 0;
        }
        area = self.menu.area().skip_rows(account_y);

        let lines_len = lines.len();
        let mut idx = 0;
        let mut branches = String::with_capacity(16);

        // What depth to skip if a mailbox is toggled to collapse
        // The value should be the collapsed mailbox's indentation, so that its children
        // are not visible.
        let mut skip: Option<usize> = None;
        let mut skipped_counter: usize = 0;
        'grid_loop: for y in 0..area.height() {
            if idx == lines_len {
                break;
            }
            let mut l = lines[idx];
            while let Some(p) = skip {
                if l.depth > p {
                    self.accounts[aidx].entries[idx].visible = false;
                    idx += 1;
                    skipped_counter += 1;
                    if idx >= lines.len() {
                        break 'grid_loop;
                    }
                    l = lines[idx];
                } else {
                    skip = None;
                }
            }
            self.accounts[aidx].entries[idx].visible = true;
            if l.collapsed {
                skip = Some(l.depth);
                // Calculate total unseen from hidden children mailboxes
                let mut idx = idx + 1;
                let mut counter = 0;
                while idx < lines.len() {
                    if lines[idx].depth <= l.depth {
                        break;
                    }
                    counter += lines[idx].count.unwrap_or(0);
                    idx += 1;
                }
                l.collapsed_count = Some(counter);
            }
            let (att, index_att, unread_count_att) = if must_highlight_account {
                if match cursor.menu {
                    MenuEntryCursor::Mailbox(c) => c == idx,
                    _ => false,
                } {
                    let mut ret = (
                        crate::conf::value(context, "mail.sidebar_highlighted"),
                        crate::conf::value(context, "mail.sidebar_highlighted_index"),
                        crate::conf::value(context, "mail.sidebar_highlighted_unread_count"),
                    );

                    if !context.settings.terminal.use_color() {
                        ret.0.attrs |= Attr::REVERSE;
                        ret.1.attrs |= Attr::REVERSE;
                        ret.2.attrs |= Attr::REVERSE;
                    }
                    ret
                } else {
                    (
                        crate::conf::value(context, "mail.sidebar_highlighted_account"),
                        crate::conf::value(context, "mail.sidebar_highlighted_account_index"),
                        crate::conf::value(
                            context,
                            "mail.sidebar_highlighted_account_unread_count",
                        ),
                    )
                }
            } else {
                (
                    crate::conf::value(context, "mail.sidebar"),
                    crate::conf::value(context, "mail.sidebar_index"),
                    crate::conf::value(context, "mail.sidebar_unread_count"),
                )
            };

            // Calculate how many columns the mailbox index tags should occupy with right
            // alignment, eg.
            //  1
            //  2
            // ...
            //  9
            // 10
            let total_mailbox_no_digits = {
                let mut len = lines_len;
                let mut ctr = 1;
                while len > 9 {
                    ctr += 1;
                    len /= 10;
                }
                ctr
            };

            let has_sibling_str: &str = account_settings!(
                context[self.accounts[aidx].hash]
                    .listing
                    .sidebar_mailbox_tree_has_sibling
            )
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or(" ");
            let no_sibling_str: &str = account_settings!(
                context[self.accounts[aidx].hash]
                    .listing
                    .sidebar_mailbox_tree_no_sibling
            )
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or(" ");

            let has_sibling_leaf_str: &str = account_settings!(
                context[self.accounts[aidx].hash]
                    .listing
                    .sidebar_mailbox_tree_has_sibling_leaf
            )
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or(" ");

            let no_sibling_leaf_str: &str = account_settings!(
                context[self.accounts[aidx].hash]
                    .listing
                    .sidebar_mailbox_tree_no_sibling_leaf
            )
            .as_ref()
            .map(|s| s.as_str())
            .unwrap_or(" ");

            let (x, _) = self.menu.grid_mut().write_string(
                &if *account_settings!(
                    context[self.accounts[aidx].hash]
                        .listing
                        .relative_menu_indices
                ) && must_highlight_account
                {
                    format!(
                        "{:>width$}",
                        (l.inc - cursor.menu).abs(),
                        width = total_mailbox_no_digits
                    )
                } else {
                    format!("{:>width$}", l.inc, width = total_mailbox_no_digits)
                },
                index_att.fg,
                index_att.bg,
                index_att.attrs,
                area.nth_row(y + 1),
                None,
            );
            area = self.menu.area().skip_rows(account_y);
            {
                branches.clear();
                branches.push_str(no_sibling_str);
                let leading_zeros = l.indentation.leading_zeros();
                let mut o = 1_u32.wrapping_shl(31_u32.saturating_sub(leading_zeros));
                for _ in 0..(32_u32.saturating_sub(leading_zeros)) {
                    if l.indentation & o > 0 {
                        branches.push_str(has_sibling_str);
                    } else {
                        branches.push_str(no_sibling_str);
                    }
                    o >>= 1;
                }
                if l.depth > 0 {
                    if l.has_sibling {
                        branches.push_str(has_sibling_leaf_str);
                    } else {
                        branches.push_str(no_sibling_leaf_str);
                    }
                }
            }
            let x = self
                .menu
                .grid_mut()
                .write_string(
                    &branches,
                    att.fg,
                    att.bg,
                    att.attrs,
                    area.nth_row(y + 1).skip_cols(x),
                    None,
                )
                .0
                + x;
            area = self.menu.area().skip_rows(account_y);
            let x = self
                .menu
                .grid_mut()
                .write_string(
                    context.accounts[self.accounts[aidx].index].mailbox_entries[&l.mailbox_idx]
                        .name(),
                    att.fg,
                    att.bg,
                    att.attrs,
                    area.nth_row(y + 1).skip_cols(x),
                    None,
                )
                .0
                + x;
            area = self.menu.area().skip_rows(account_y);

            // Unread message count
            let count_string = match (l.count, l.collapsed_count) {
                (None, None) => " ...".to_string(),
                (Some(0), None) => String::new(),
                (Some(0), Some(0)) | (None, Some(0)) => " v".to_string(),
                (Some(0), Some(coll)) => format!(" ({}) v", coll),
                (Some(c), Some(0)) => format!(" {} v", c),
                (Some(c), Some(coll)) => format!(" {} ({}) v", c, coll),
                (Some(c), None) => format!(" {}", c),
                (None, Some(coll)) => format!(" ({}) v", coll),
            };

            let x = self
                .menu
                .grid_mut()
                .write_string(
                    &count_string,
                    unread_count_att.fg,
                    unread_count_att.bg,
                    unread_count_att.attrs
                        | if l.count.unwrap_or(0) > 0 {
                            Attr::BOLD
                        } else {
                            Attr::DEFAULT
                        },
                    area.nth_row(y + 1)
                        .skip_cols(x.min(area.width().saturating_sub(count_string.len()))),
                    None,
                )
                .0
                + x.min(area.width().saturating_sub(count_string.len()));
            area = self.menu.area().skip_rows(account_y);
            for c in self.menu.grid_mut().row_iter(area, x..area.width(), y + 1) {
                self.menu.grid_mut()[c]
                    .set_fg(att.fg)
                    .set_bg(att.bg)
                    .set_attrs(att.attrs);
            }
            idx += 1;
        }
        if idx == 0 {
            0
        } else {
            idx - 1 - skipped_counter
        }
    }

    fn change_account(&mut self, context: &mut Context) {
        let account_hash = context.accounts[self.cursor_pos.account].hash();
        let previous_collapsed_mailboxes: BTreeSet<MailboxHash> = self.accounts
            [self.cursor_pos.account]
            .entries
            .iter()
            .filter_map(|e| {
                if e.collapsed {
                    Some(e.mailbox_hash)
                } else {
                    None
                }
            })
            .collect::<_>();
        let previous_index_styles: BTreeMap<MailboxHash, IndexStyle> = self.accounts
            [self.cursor_pos.account]
            .entries
            .iter()
            .filter_map(|e| Some((e.mailbox_hash, e.index_style?)))
            .collect::<_>();
        self.accounts[self.cursor_pos.account].entries = context.accounts[self.cursor_pos.account]
            .list_mailboxes()
            .into_iter()
            .filter(|mailbox_node| {
                context.accounts[self.cursor_pos.account][&mailbox_node.hash]
                    .ref_mailbox
                    .is_subscribed()
            })
            .map(|f| MailboxMenuEntry {
                depth: f.depth,
                indentation: f.indentation,
                has_sibling: f.has_sibling,
                mailbox_hash: f.hash,
                visible: true,
                collapsed: if previous_collapsed_mailboxes.is_empty() {
                    context.accounts[self.cursor_pos.account][&f.hash]
                        .conf
                        .collapsed
                } else {
                    previous_collapsed_mailboxes.contains(&f.hash)
                },
                index_style: previous_index_styles.get(&f.hash).copied(),
            })
            .collect::<_>();
        match self.cursor_pos.menu {
            MenuEntryCursor::Mailbox(idx) => {
                // Account might have no mailboxes yet if it's offline
                if let Some(MailboxMenuEntry {
                    mailbox_hash,
                    index_style,
                    ..
                }) = self.accounts[self.cursor_pos.account].entries.get(idx)
                {
                    self.component
                        .process_event(&mut UIEvent::VisibilityChange(false), context);
                    self.component
                        .set_coordinates((account_hash, *mailbox_hash));
                    self.component.refresh_mailbox(context, true);

                    // Check if per-mailbox configuration overrides general configuration
                    let index_style_override =
                        *mailbox_settings!(context[account_hash][mailbox_hash].listing.index_style);
                    self.set_index_style(index_style.unwrap_or(index_style_override), context);
                } else if !matches!(self.component, ListingComponent::Offline(_)) {
                    self.component.unrealize(context);
                    self.component =
                        Offline(OfflineListing::new((account_hash, MailboxHash::default())));
                    self.component.realize(self.id().into(), context);
                }
                self.component
                    .process_event(&mut UIEvent::VisibilityChange(true), context);
                self.status = None;
                context
                    .replies
                    .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                        self.status(context),
                    )));
            }
            MenuEntryCursor::Status => {
                self.open_status(self.cursor_pos.account, context);
            }
        }
        self.sidebar_divider = *account_settings!(context[account_hash].listing.sidebar_divider);
        self.set_dirty(true);
        self.menu_cursor_pos = self.cursor_pos;
        // clear menu to force redraw
        self.menu.grid_mut().empty();
        if *account_settings!(context[account_hash].listing.show_menu_scrollbar) {
            self.show_menu_scrollbar = ShowMenuScrollbar::True;
            self.menu_scrollbar_show_timer.rearm();
        } else {
            self.show_menu_scrollbar = ShowMenuScrollbar::Never;
        }
    }

    fn open_status(&mut self, account_idx: usize, context: &mut Context) {
        self.status = Some(AccountStatus::new(account_idx, self.theme_default));
        self.menu.grid_mut().empty();
        context
            .replies
            .push_back(UIEvent::StatusEvent(StatusEvent::UpdateStatus(
                self.status(context),
            )));
    }

    fn is_menu_visible(&self) -> bool {
        !matches!(self.component.focus(), Focus::EntryFullscreen) && self.menu_visibility
    }

    fn set_index_style(&mut self, new_style: IndexStyle, context: &mut Context) {
        let old = match new_style {
            IndexStyle::Plain => {
                if matches!(self.component, Plain(_)) {
                    return;
                }
                let coordinates = self.component.coordinates();
                std::mem::replace(
                    &mut self.component,
                    Plain(PlainListing::new(self.id, coordinates)),
                )
            }
            IndexStyle::Threaded => {
                if matches!(self.component, Threaded(_)) {
                    return;
                }
                let coordinates = self.component.coordinates();
                std::mem::replace(
                    &mut self.component,
                    Threaded(ThreadListing::new(self.id, coordinates, context)),
                )
            }
            IndexStyle::Compact => {
                if matches!(self.component, Compact(_)) {
                    return;
                }
                let coordinates = self.component.coordinates();
                std::mem::replace(
                    &mut self.component,
                    Compact(CompactListing::new(self.id, coordinates)),
                )
            }
            IndexStyle::Conversations => {
                if matches!(self.component, Conversations(_)) {
                    return;
                }
                let coordinates = self.component.coordinates();
                std::mem::replace(
                    &mut self.component,
                    Conversations(ConversationsListing::new(self.id, coordinates)),
                )
            }
        };
        if let MenuEntryCursor::Mailbox(idx) = self.cursor_pos.menu {
            if let Some(mbox_entry) = self.accounts[self.cursor_pos.account].entries.get_mut(idx) {
                mbox_entry.index_style = Some(new_style);
            }
        }
        self.component
            .process_event(&mut UIEvent::VisibilityChange(true), context);
        old.unrealize(context);
        self.component.realize(self.id.into(), context);
    }
}

#[derive(Clone, Copy, Debug)]
pub enum ListingMessage {
    FocusUpdate {
        new_value: Focus,
    },
    OpenEntryUnderCursor {
        env_hash: EnvelopeHash,
        thread_hash: ThreadHash,
        show_thread: bool,
        go_to_first_unread: bool,
    },
    UpdateView,
}
