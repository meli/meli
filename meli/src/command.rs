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

//! A parser module for user commands passed through
//! [`Command`](crate::types::UIMode::Command) mode.

#[cfg(test)]
mod tests;

pub mod actions;
#[macro_use]
pub mod error;
#[macro_use]
pub mod argcheck;
pub mod completions;
pub mod history;
pub mod parser;
use actions::MailboxOperation;
use error::CommandError;
pub use parser::parse_command;

pub use crate::actions::{
    AccountAction::{self, *},
    Action::{self, *},
    ComposeAction::{self, *},
    ComposerTabAction, FlagAction,
    ListingAction::{self, *},
    MailingListAction::{self, *},
    TabAction::{self, *},
    TagAction,
    ViewAction::{self, *},
};

pub type CommandCompletionEntry = (
    &'static str,
    TokenStream,
    fn(&str) -> melib::nom::IResult<&str, Result<Action, CommandError>>,
);

/// Macro to create a const table with every command part that can be
/// auto-completed and its description
macro_rules! define_commands {
    ( [$({ desc: $desc:literal, tokens: $tokens:expr, parser: $parser:path}),*]) => {
        pub const COMMAND_COMPLETION: &[CommandCompletionEntry] = &[$(($desc, TokenStream { tokens: $tokens }, $parser)),* ];
    };
}

#[derive(Clone, Copy, Debug)]
pub struct TokenStream {
    tokens: &'static [Token],
}

use Token::*;

/// A token encountered in the UI's command execution bar
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
    Literal(&'static str),
    Filepath,
    NewFilepath,
    Alternatives(&'static [&'static str]),
    AccountName,
    MailboxPath,
    NewMailboxPath,
    QuotedStringValue,
    RestOfStringValue,
    AttachmentIndexValue,
    MailboxIndexValue,
    IndexValue,
}

define_commands!([
    {
        desc: "set [plain/threaded/compact/conversations] changes the mail listing view",
        tokens: &[
            Literal("set"),
            Alternatives(&["plain", "threaded", "compact", "conversations"])
        ],
        parser: parser::set
    },
    {
        desc: "set [seen/unseen], toggles message's Seen flag",
        tokens: &[
            Literal("set"),
            Alternatives(&["seen", "unseen"])
        ],
        parser: parser::set
    },
    {
        desc: "delete message",
        tokens: &[Literal("delete")],
        parser: parser::delete_message
    },
    {
        desc: "copy/move message",
        tokens: &[Alternatives(&["copyto", "moveto"]), MailboxPath],
        parser: parser::copymove
    },
    {
        desc: "copy/move message to other account",
        tokens: &[Alternatives(&["copyto", "moveto"]), AccountName, MailboxPath],
        parser: parser::copymove
    },
    {
        desc: "import FILESYSTEM_PATH MAILBOX_PATH",
        tokens: &[Literal("import"), Filepath, MailboxPath],
        parser: parser::import
    },
    {
        desc: "public-inbox [import/import-thread] ACCOUNT_NAME MAILBOX_PATH MESSAGE_ID",
        tokens: &[Literal("public-inbox"), Alternatives(&["import", "import-thread"]), AccountName, MailboxPath, QuotedStringValue],
        parser: parser::public_inbox_import
    },
    {
        desc: "close non-sticky tabs",
        tokens: &[Literal("close")],
        parser: parser::close
    },
    {
        desc: "go <n>, switch to nth mailbox in this account",
        tokens: &[Literal("goto"), MailboxIndexValue],
        parser: parser::goto
    },
    {
        desc: "subsort [date/subject] [asc/desc], sorts first level replies in threads.",
        tokens: &[Literal("subsort"), Alternatives(&["date", "subject"]), Alternatives(&["asc", "desc"]) ],
        parser: parser::subsort
    },
    {
        desc: "sort [date/subject] [asc/desc], sorts threads.",
        tokens: &[Literal("sort"), Alternatives(&["date", "subject"]), Alternatives(&["asc", "desc"]) ],
        parser: parser::sort
    },
    {
        desc: "sort <column index> [asc/desc], sorts table columns.",
        tokens: &[Literal("sort"), IndexValue, Alternatives(&["asc", "desc"]) ],
        parser: parser::sort_column
    },
    {
        desc: "turn off new notifications for this thread",
        tokens: &[Literal("toggle"), Literal("thread_snooze")],
        parser: parser::toggle
    },
    {
        desc: "search <TERM>, searches list with given term",
        tokens: &[Literal("search"), RestOfStringValue],
        parser: parser::search
    },
    {
        desc: "clear-selection",
        tokens: &[Literal("clear-selection")],
        parser: parser::select
    },
    {
        desc: "select <TERM>, selects envelopes matching with given term",
        tokens: &[Literal("select"), RestOfStringValue],
        parser: parser::select
    },
    {
        desc: "export-mbox PATH, save mail as mbox to PATH",
        tokens: &[Literal("export-mbox"), Filepath],
        parser: parser::export_mbox
    },
    {
        desc: "list-[unsubscribe/post/archive]",
        tokens: &[Alternatives(&["list-archive", "list-post", "list-unsubscribe"])],
        parser: parser::mailinglist
    },
    {
        desc: "setenv VAR=VALUE",
        tokens: &[Literal("setenv"), RestOfStringValue],
        parser: parser::setenv
    },
    {
        desc: "printenv VAR",
        tokens: &[Literal("printenv"), QuotedStringValue],
        parser: parser::printenv
    },
    {
        desc: "mailto MAILTO_ADDRESS",
        tokens: &[Literal("mailto"), QuotedStringValue],
        parser: parser::mailto
    },
    {
        desc: "pipe EXECUTABLE ARGS, Pipe pager contents to binary",
        tokens: &[Literal("pipe"), Filepath, RestOfStringValue],
        parser: parser::pipe
    },
    {
        desc: "filter EXECUTABLE ARGS, Filter pager contents through binary",
        tokens: &[Literal("filter"), Filepath, RestOfStringValue],
        parser: parser::filter
    },
    {
        desc: "add-attachment PATH, add PATH as an attachment",
        tokens: &[Literal("add-attachment"), Filepath],
        parser: parser::add_attachment
    },
    {
        desc: "launch file picker to select an attachment",
        tokens: &[Literal("add-attachment-file-picker")],
        parser: parser::add_attachment
    },
    {
        desc: "remove-attachment INDEX",
        tokens: &[Literal("remove-attachment"), IndexValue],
        parser: parser::remove_attachment
    },
    {
        desc: "save draft",
        tokens: &[Literal("save-draft")],
        parser: parser::save_draft
    },
    {
        desc: "discard draft",
        tokens: &[Literal("discard-draft")],
        parser: parser::discard_draft
    },
    {
        desc: "switch between sign/unsign for this draft",
        tokens: &[Literal("toggle"), Literal("sign")],
        parser: parser::toggle
    },
    {
        desc: "toggle encryption for this draft",
        tokens: &[Literal("toggle"), Literal("encrypt")],
        parser: parser::toggle
    },
    {
        desc: "create-mailbox ACCOUNT MAILBOX_PATH",
        tokens: &[Literal("create-mailbox"), AccountName, NewMailboxPath],
        parser: parser::create_mailbox
    },
    {
        desc: "subscribe-mailbox ACCOUNT MAILBOX_PATH",
        tokens: &[Literal("subscribe-mailbox"), AccountName, MailboxPath],
        parser: parser::sub_mailbox
    },
    {
        desc: "unsubscribe-mailbox ACCOUNT MAILBOX_PATH",
        tokens: &[Literal("unsubscribe-mailbox"), AccountName, MailboxPath],
        parser: parser::unsub_mailbox
    },
    {
        desc: "rename-mailbox ACCOUNT MAILBOX_PATH_SRC MAILBOX_PATH_DEST",
        tokens: &[Literal("rename-mailbox"), AccountName, MailboxPath, NewMailboxPath],
        parser: parser::rename_mailbox
    },
    {
        desc: "delete-mailbox ACCOUNT MAILBOX_PATH",
        tokens: &[Literal("delete-mailbox"), AccountName, MailboxPath],
        parser: parser::delete_mailbox
    },
    {
        desc: "reindex ACCOUNT, rebuild account cache in the background",
        tokens: &[Literal("reindex"), AccountName],
        parser: parser::reindex
    },
    {
        desc: "opens envelope view in new tab",
        tokens: &[Literal("open-in-tab")],
        parser: parser::open_in_new_tab
    },
    {
        desc: "save-attachment INDEX PATH",
        tokens: &[Literal("save-attachment"), AttachmentIndexValue, NewFilepath],
        parser: parser::save_attachment
    },
    {
        desc: "save-attachment-picker ",
        tokens: &[Literal("save-attachment-picker")],
        parser: parser::save_attachment
    },
    {
        desc: "pipe-attachment INDEX EXECUTABLE ARGS",
        tokens: &[Literal("pipe-attachment"), AttachmentIndexValue, RestOfStringValue],
        parser: parser::pipe_attachment
    },
    {
        desc: "export-mail PATH",
        tokens: &[Literal("export-mail"), NewFilepath],
        parser: parser::export_mail
    },
    {
        desc: "export-thread PATH",
        tokens: &[Literal("export-thread"), NewFilepath],
        parser: parser::export_thread
    },
    {
        desc: "export-thread-mbox PATH",
        tokens: &[Literal("export-thread-mbox"), NewFilepath],
        parser: parser::export_thread_mbox
    },
    {
        desc: "add-addresses-to-contacts",
        tokens: &[Literal("add-addresses-to-contacts")],
        parser: parser::add_addresses_to_contacts
    },
    {
        desc: "tag [add/remove], edits message's tags.",
        tokens: &[Literal("tag"), Alternatives(&["add", "remove"])],
        parser: parser::_tag
    },
    {
        desc: "print ACCOUNT SETTING",
        tokens: &[Literal("print"), AccountName, QuotedStringValue],
        parser: parser::print_account_setting
    },
    {
        desc: "print SETTING",
        tokens: &[Literal("print"), QuotedStringValue],
        parser: parser::print_setting
    },
    {
        desc: "toggle mouse support",
        tokens: &[Literal("toggle"), Literal("mouse")],
        parser: parser::toggle
    },
    {
        desc: "view and manage mailbox preferences",
        tokens: &[Literal("manage-mailboxes")],
        parser: parser::manage_mailboxes
    },
    {
        desc: "read documentation",
        tokens: {
            #[cfg(feature = "cli-docs")]
            {
                &[Literal("man"), Alternatives(crate::manpages::POSSIBLE_VALUES)]
            }
            #[cfg(not(feature = "cli-docs"))]
            { &[] }
        },
        parser: parser::view_manpage
    },
    {
        desc: "view and manage jobs",
        tokens: &[Literal("manage-jobs")],
        parser: parser::manage_jobs
    },
    {
        desc: "quit meli",
        tokens: &[Literal("quit")],
        parser: parser::quit
    },
    {
        desc: "reload configuration file",
        tokens: &[Literal("reload-config")],
        parser: parser::reload_config
    }
]);
