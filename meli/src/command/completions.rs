//
// meli
//
// Copyright 2026 - Manos Pitsidianakis
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

//! Generate intelligent command completions based on user's input
//!
//! This module exposes a [`CompletionsGenerator`] struct that:
//!
//! - Allows insertion/update of known accounts and their mailboxes via [`CompletionsGenerator::add_account`] method.
//! - Generates [`AutoCompleteEntry`] suggestions based on given input via [`CompletionsGenerator::generate`] method.
//!
//! To generate suggestions, it uses a [`Lexer`] to split user input in lexical tokens, based on
//! quoting and space escaping.
//!
//! It then matches each lexical token against the known command tokens registered in
//! [`COMMAND_COMPLETION`] array which includes all known commands and what syntax they expect.
//!
//! For each command, there are three essential match cases:
//!
//! - No/invalid match
//! - Valid match, that may still be completed further
//! - Incomplete match that needs to be completed to become valid

use std::{borrow::Cow, path::Path};

use indexmap::{IndexMap, IndexSet};

use crate::{
    command::{
        parser::{LexToken, LexTokenError, Lexer},
        Token, TokenStream, COMMAND_COMPLETION,
    },
    melib::ShellExpandTrait,
    utilities::AutoCompleteEntry,
};

/// Generate command completions.
///
/// See module documentation for details.
#[derive(Debug, Clone)]
pub struct CompletionsGenerator {
    /// Known accounts and their mailboxes
    pub mailboxes: IndexMap<String, IndexSet<String>>,
    pub maximum_filesystem_matches: usize,
}

impl Default for CompletionsGenerator {
    fn default() -> Self {
        Self {
            mailboxes: IndexMap::default(),
            maximum_filesystem_matches: 100,
        }
    }
}

fn quote_if_necessary(s: &'_ str) -> Cow<'_, str> {
    if s.contains([' ', '"']) {
        if s.contains('"') {
            let escaped = format!("\"{}\"", s.replace('"', "\\\""));
            Cow::Owned(escaped)
        } else {
            let escaped = format!("\"{s}\"");
            Cow::Owned(escaped)
        }
    } else {
        Cow::Borrowed(s)
    }
}

fn quote_incomplete_if_necessary(s: &'_ str) -> Cow<'_, str> {
    if s.contains([' ', '"']) {
        if s.contains('"') {
            let escaped = format!("\"{}", s.replace('"', "\\\""));
            Cow::Owned(escaped)
        } else {
            let escaped = format!("\"{s}");
            Cow::Owned(escaped)
        }
    } else {
        Cow::Borrowed(s)
    }
}

fn quote(s: &str) -> String {
    if s.contains('"') {
        format!("\"{}\"", s.replace('"', "\\\""))
    } else {
        format!("\"{s}\"")
    }
}

impl CompletionsGenerator {
    /// Add or update account mailboxes
    pub fn add_account(&mut self, account: String, mailboxes: IndexSet<String>) {
        self.mailboxes.insert(account, mailboxes);
    }

    /// Get command suggestions for input
    pub fn generate(&self, input: &str) -> Vec<AutoCompleteEntry> {
        let mut suggestions: IndexSet<AutoCompleteEntry> = Default::default();
        for (desc, token_stream, _) in COMMAND_COMPLETION.iter() {
            let mut lexer = Lexer::new(input);
            let matcher = Matcher {
                token_stream,
                mailboxes: &self.mailboxes,
                acc_match: None,
                pos: 0,
                previous_match: None,
                lex_iter: &mut lexer,
            };
            // Reduce matches to get either first error or last matching token
            let result = matcher
                .into_iter()
                .reduce(|acc, elem| {
                    acc?;
                    elem
                })
                .unwrap_or_else(|| {
                    Err(MatchError::Next {
                        next_token: token_stream.tokens.first().expect("non-empty token stream"),
                        data: MatcherMetadata {
                            acc_match: None,
                            previous_match: None,
                        },
                    })
                });
            match result {
                // Last lexeme is a match, see if we can generate any more suggestions out of
                // it.
                Ok((lex_token, token, data)) => {
                    self.complete_lex_token(&data, lex_token, token, &mut suggestions, desc, input);
                }
                // Ignore invalid matches
                Err(MatchError::Invalid) => {}
                // If we append a space to input we might be able to generate suggestions
                Err(MatchError::WhitespaceAndNext {
                    next_token,
                    mut data,
                }) => {
                    // Check previous token match for more suggestions before generating
                    // suggestions for after adding space
                    if let Some((lex_token, token)) = data.previous_match.take() {
                        let skip_next_token = matches!(token, Token::Filepath)
                            && !Path::new(lex_token.value())
                                .expand_tilde()
                                .try_exists()
                                .unwrap_or(false);
                        self.complete_lex_token(
                            &data,
                            lex_token,
                            token,
                            &mut suggestions,
                            desc,
                            input,
                        );
                        if skip_next_token {
                            continue;
                        }
                    }
                    match next_token {
                        Token::Literal(lit) => {
                            suggestions.insert((format!("{input} {lit}"), *desc).into());
                        }
                        Token::Alternatives(lits) => {
                            for lit in *lits {
                                suggestions.insert((format!("{input} {lit}"), *desc).into());
                            }
                        }
                        Token::AccountName => {
                            for acc in self.mailboxes.keys() {
                                suggestions.insert(
                                    (
                                        format!("{input} {acc}", acc = quote_if_necessary(acc)),
                                        *desc,
                                    )
                                        .into(),
                                );
                            }
                        }
                        Token::MailboxPath => {
                            self.complete_mailbox_path(
                                &data,
                                None,
                                &mut suggestions,
                                |mbox| {
                                    (
                                        format!("{input} {mbox}", mbox = quote_if_necessary(mbox)),
                                        *desc,
                                    )
                                        .into()
                                },
                                None::<fn(_) -> AutoCompleteEntry>,
                                false,
                            );
                        }
                        Token::NewMailboxPath => {
                            self.complete_mailbox_path(
                                &data,
                                None,
                                &mut suggestions,
                                |mbox| {
                                    (
                                        format!(
                                            "{input} {mbox}",
                                            mbox =
                                                quote_incomplete_if_necessary(&format!("{mbox}/"))
                                        ),
                                        *desc,
                                    )
                                        .into()
                                },
                                None::<fn(_) -> AutoCompleteEntry>,
                                true,
                            );
                        }
                        _ => {}
                    }
                }
                // Input ends with whitespace and we can match `next_token`
                Err(MatchError::Next {
                    next_token,
                    mut data,
                }) => {
                    match next_token {
                        Token::Literal(lit) => {
                            suggestions.insert((format!("{input}{lit}"), *desc).into());
                        }
                        Token::Alternatives(lits) => {
                            for lit in *lits {
                                suggestions.insert((format!("{input}{lit}"), *desc).into());
                            }
                        }
                        Token::AccountName => {
                            for acc in self.mailboxes.keys() {
                                suggestions.insert(
                                    (
                                        format!("{input}{acc}", acc = quote_if_necessary(acc)),
                                        *desc,
                                    )
                                        .into(),
                                );
                            }
                        }
                        Token::MailboxPath => {
                            self.complete_mailbox_path(
                                &data,
                                None,
                                &mut suggestions,
                                |mbox| {
                                    (
                                        format!("{input}{mbox}", mbox = quote_if_necessary(mbox)),
                                        *desc,
                                    )
                                        .into()
                                },
                                None::<fn(_) -> AutoCompleteEntry>,
                                false,
                            );
                        }
                        Token::NewMailboxPath => {
                            self.complete_mailbox_path(
                                &data,
                                None,
                                &mut suggestions,
                                |mbox| {
                                    (
                                        format!(
                                            "{input}{mbox}",
                                            mbox =
                                                quote_incomplete_if_necessary(&format!("{mbox}/"))
                                        ),
                                        *desc,
                                    )
                                        .into()
                                },
                                None::<fn(_) -> AutoCompleteEntry>,
                                true,
                            );
                        }
                        _ => {}
                    }
                    if let Some((lex_token, token)) = data.previous_match.take() {
                        self.complete_lex_token(
                            &data,
                            lex_token,
                            token,
                            &mut suggestions,
                            desc,
                            // MatchError::Next has a trailing whitespace so trim it.
                            input.trim_end(),
                        );
                    }
                }
                // Input can only be valid if extra stuff is added
                Err(MatchError::Incomplete {
                    lexeme: Ok(lex_token),
                    token,
                    data,
                }) => match token {
                    Token::Literal(lit) => {
                        suggestions.insert(
                            (
                                format!("{}{}", input.strip_suffix(lex_token.raw()).unwrap(), lit),
                                *desc,
                            )
                                .into(),
                        );
                    }
                    Token::Alternatives(lits) => {
                        for lit in *lits {
                            if lit.starts_with(lex_token.value()) && *lit != lex_token.value() {
                                suggestions.insert(
                                    (
                                        format!(
                                            "{}{}",
                                            input.strip_suffix(lex_token.raw()).unwrap(),
                                            lit
                                        ),
                                        *desc,
                                    )
                                        .into(),
                                );
                            }
                        }
                    }
                    Token::AccountName => {
                        for acc in self.mailboxes.keys() {
                            if acc.starts_with(lex_token.value()) && acc != lex_token.value() {
                                suggestions.insert(
                                    (
                                        format!(
                                            "{input}{acc}",
                                            input = input.strip_suffix(lex_token.raw()).unwrap(),
                                            acc = quote_if_necessary(acc)
                                        ),
                                        *desc,
                                    )
                                        .into(),
                                );
                            }
                        }
                    }
                    Token::MailboxPath => {
                        self.complete_mailbox_path(
                            &data,
                            Some(lex_token.value()),
                            &mut suggestions,
                            |mbox| {
                                (
                                    format!(
                                        "{input}{mbox}",
                                        input = input.strip_suffix(lex_token.raw()).unwrap(),
                                        mbox = quote_if_necessary(mbox)
                                    ),
                                    *desc,
                                )
                                    .into()
                            },
                            Some(|mbox| {
                                (
                                    format!(
                                        "{input}{mbox}",
                                        input = input.strip_suffix(lex_token.raw()).unwrap(),
                                        mbox = lex_token.incomplete(mbox)
                                    ),
                                    *desc,
                                )
                                    .into()
                            }),
                            false,
                        );
                    }
                    _ => {}
                },
                // Same as before, except that lexeme is an unclosed quoted string
                Err(MatchError::Incomplete {
                    lexeme: Err(lex_token_err),
                    token,
                    data,
                }) => {
                    if let (Some(raw), Some(value)) = (lex_token_err.raw(), lex_token_err.value()) {
                        match token {
                            Token::Literal(lit) => {
                                suggestions.insert(
                                    (
                                        format!(
                                            "{input}{lit}",
                                            input = input.strip_suffix(raw).unwrap(),
                                        ),
                                        *desc,
                                    )
                                        .into(),
                                );
                            }
                            Token::Alternatives(lits) => {
                                for lit in *lits {
                                    if lit.starts_with(value) && *lit != value {
                                        suggestions.insert(
                                            (
                                                format!(
                                                    "{input}{lit}",
                                                    input = input.strip_suffix(raw).unwrap(),
                                                ),
                                                *desc,
                                            )
                                                .into(),
                                        );
                                    }
                                }
                            }
                            Token::NewFilepath | Token::Filepath => {
                                suggestions.extend(
                                    Path::new(value)
                                        .complete(true, value.ends_with('/'))
                                        .into_iter()
                                        .take(self.maximum_filesystem_matches)
                                        .map(|m| {
                                            (
                                                format!(
                                                    "{input}\"{value}{m}\"",
                                                    input = input.strip_suffix(raw).unwrap(),
                                                    value = value,
                                                    m = m.replace('"', "\\\""),
                                                ),
                                                *desc,
                                            )
                                                .into()
                                        }),
                                );
                            }
                            Token::AccountName => {
                                for acc in self.mailboxes.keys() {
                                    if acc.starts_with(value) && acc != value {
                                        suggestions.insert(
                                            (
                                                format!(
                                                    "{input}{acc}",
                                                    input = input.strip_suffix(raw).unwrap(),
                                                    acc = quote(acc)
                                                ),
                                                *desc,
                                            )
                                                .into(),
                                        );
                                    }
                                }
                            }
                            Token::MailboxPath => {
                                self.complete_mailbox_path(
                                    &data,
                                    Some(value),
                                    &mut suggestions,
                                    |mbox| {
                                        (
                                            format!(
                                                "{input}{mbox}",
                                                input = input.strip_suffix(raw).unwrap(),
                                                mbox = quote(mbox)
                                            ),
                                            *desc,
                                        )
                                            .into()
                                    },
                                    Some(|mbox| {
                                        (
                                            format!(
                                                "{input}\"{mbox}",
                                                input = input.strip_suffix(raw).unwrap(),
                                            ),
                                            *desc,
                                        )
                                            .into()
                                    }),
                                    false,
                                );
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
        suggestions.into_iter().collect::<Vec<AutoCompleteEntry>>()
    }

    fn complete_lex_token(
        &self,
        data: &MatcherMetadata<'_, '_, '_>,
        lex_token: LexToken,
        token: &Token,
        suggestions: &mut IndexSet<AutoCompleteEntry>,
        desc: &'static str,
        input: &str,
    ) {
        if !lex_token.is_whitespace() {
            match token {
                Token::NewFilepath | Token::Filepath => {
                    suggestions.extend(
                        Path::new(lex_token.value())
                            .complete(true, lex_token.value().ends_with('/'))
                            .into_iter()
                            .take(self.maximum_filesystem_matches)
                            .filter(|m| !m.is_empty())
                            .map(|m| {
                                (
                                    format!(
                                        "{input}{path}",
                                        input = input.strip_suffix(lex_token.raw()).unwrap(),
                                        path = lex_token.append(&m)
                                    ),
                                    desc,
                                )
                                    .into()
                            }),
                    );
                }
                Token::AccountName => {
                    for acc in self.mailboxes.keys() {
                        if acc.starts_with(lex_token.value()) && acc != lex_token.value() {
                            suggestions.insert(
                                (
                                    format!(
                                        "{input}{acc}",
                                        input = input.strip_suffix(lex_token.raw()).unwrap(),
                                        acc = lex_token.complete(acc),
                                    ),
                                    desc,
                                )
                                    .into(),
                            );
                        }
                    }
                }
                Token::MailboxPath => {
                    self.complete_mailbox_path(
                        data,
                        Some(lex_token.value()),
                        suggestions,
                        |mbox| {
                            (
                                format!(
                                    "{input}{mbox}",
                                    input = input.strip_suffix(lex_token.raw()).unwrap(),
                                    mbox = lex_token.complete(mbox),
                                ),
                                desc,
                            )
                                .into()
                        },
                        Some(|mbox| {
                            (
                                format!(
                                    "{input}{mbox}",
                                    input = input.strip_suffix(lex_token.raw()).unwrap(),
                                    mbox = lex_token.incomplete(mbox),
                                ),
                                desc,
                            )
                                .into()
                        }),
                        false,
                    );
                }
                Token::NewMailboxPath => {
                    self.complete_mailbox_path(
                        data,
                        Some(lex_token.value()),
                        suggestions,
                        |mbox| {
                            (
                                format!(
                                    "{input}{mbox}",
                                    input = input.strip_suffix(lex_token.raw()).unwrap(),
                                    mbox = lex_token.incomplete(&format!("{mbox}/")),
                                ),
                                desc,
                            )
                                .into()
                        },
                        None::<fn(_) -> AutoCompleteEntry>,
                        true,
                    );
                }
                _ => {}
            }
        }
    }

    fn complete_mailbox_path<'a>(
        &'a self,
        data: &MatcherMetadata<'_, '_, '_>,
        value: Option<&str>,
        suggestions: &mut IndexSet<AutoCompleteEntry>,
        fmt_closure: impl Fn(&'a str) -> AutoCompleteEntry,
        incomplete_fmt_closure: Option<impl Fn(&'a str) -> AutoCompleteEntry>,
        new: bool,
    ) {
        // Helper macro to create a mailbox iterator based on whether an accountname has been matched
        macro_rules! for_mbox {
            (for $mbox:ident in $self:expr, $data:expr, $block:block) => {{
                let mut acc_iter = $data.acc_match.map(|acc| $self.mailboxes[acc].iter());
                let mut all_iter = $self.mailboxes.values().flatten();
                for $mbox in acc_iter
                    .as_mut()
                    .map(|i| i as &mut dyn Iterator<Item = &String>)
                    .unwrap_or_else(|| &mut all_iter as &mut dyn Iterator<Item = &String>)
                {
                    $block
                }
            }};
        }

        if new {
            if let Some(value) = value {
                for_mbox!( for mbox in self, data, {
                    if mbox.starts_with(value) && mbox != value {
                        let value_levels = value.matches('/').count();
                        let (idx, _) = mbox.match_indices('/').nth(value_levels).unwrap_or((mbox.len(), ""));
                        if let Some(ref cl) = incomplete_fmt_closure {
                            suggestions.insert(cl(&mbox[..idx]));
                        } else {
                            suggestions.insert(fmt_closure(&mbox[..idx]));
                        }
                    }
                });
            } else {
                for_mbox!( for mbox in self, data, {
                    if let Some((idx, _)) = mbox.match_indices('/').nth(0) {
                        if let Some(ref cl) = incomplete_fmt_closure {
                            suggestions.insert(cl(&mbox[..idx]));
                        } else {
                            suggestions.insert(fmt_closure(&mbox[..idx]));
                        }
                    }
                });
            }
        } else {
            if let Some(value) = value {
                for_mbox!( for mbox in self, data, {
                    if mbox.starts_with(value) && mbox != value {
                        let value_levels = value.matches('/').count();
                        let levels = mbox.matches('/').count();
                        if value_levels == levels {
                            suggestions.insert(fmt_closure(mbox));
                        } else if let Some((idx, _)) = mbox.match_indices('/').nth(value_levels) {
                            if let Some(ref cl) = incomplete_fmt_closure {
                                suggestions.insert(cl(&mbox[..=idx]));
                            } else {
                                suggestions.insert(fmt_closure(&mbox[..=idx]));
                            }
                        }
                    }
                });
            } else {
                for_mbox!( for mbox in self, data, {
                    let levels = mbox.matches('/').count();
                    if levels == 0 {
                        suggestions.insert(fmt_closure(mbox));
                    } else if let Some((idx, _)) = mbox.match_indices('/').nth(0) {
                        if let Some(ref cl) = incomplete_fmt_closure {
                            suggestions.insert(cl(&mbox[..=idx]));
                        } else {
                            suggestions.insert(fmt_closure(&mbox[..=idx]));
                        }
                    }
                });
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum MatchError<'a, 'b, 'c> {
    Incomplete {
        lexeme: std::result::Result<LexToken<'c>, LexTokenError<'c>>,
        token: &'a Token,
        data: Box<MatcherMetadata<'a, 'b, 'c>>,
    },
    /// Input ends with whitespace and we can match `next_token`
    Next {
        next_token: &'a Token,
        data: MatcherMetadata<'a, 'b, 'c>,
    },
    /// If we add a space to input we can match `next_token`
    WhitespaceAndNext {
        next_token: &'a Token,
        data: MatcherMetadata<'a, 'b, 'c>,
    },
    Invalid,
}

#[derive(Debug, PartialEq, Eq)]
struct MatcherMetadata<'a, 'b, 'c> {
    acc_match: Option<&'b str>,
    previous_match: Option<(LexToken<'c>, &'a Token)>,
}

#[derive(Debug)]
struct Matcher<'a, 'b, 'c> {
    token_stream: &'a TokenStream,
    pos: usize,
    mailboxes: &'b IndexMap<String, IndexSet<String>>,
    acc_match: Option<&'b str>,

    previous_match: Option<(LexToken<'c>, &'a Token)>,
    lex_iter: &'b mut Lexer<'c>,
}

impl<'a, 'b, 'c> Iterator for Matcher<'a, 'b, 'c> {
    type Item = std::result::Result<
        (LexToken<'c>, &'a Token, MatcherMetadata<'a, 'b, 'c>),
        MatchError<'a, 'b, 'c>,
    >;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(token) = self.token_stream.tokens.get(self.pos) else {
            if self.lex_iter.next().is_some() {
                return Some(Err(MatchError::Invalid));
            }
            return None;
        };

        let mut next_err = None;
        let lexeme = loop {
            let Some(lexeme) = self.lex_iter.next() else {
                if let Some(next_err) = next_err {
                    self.pos = self.token_stream.tokens.len();
                    return Some(Err(MatchError::Next {
                        next_token: next_err,
                        data: MatcherMetadata {
                            previous_match: self.previous_match.take(),
                            acc_match: self.acc_match.take(),
                        },
                    }));
                }
                if self.pos > 0 {
                    self.pos = self.token_stream.tokens.len();
                    return Some(Err(MatchError::WhitespaceAndNext {
                        next_token: token,
                        data: MatcherMetadata {
                            previous_match: self.previous_match.take(),
                            acc_match: self.acc_match.take(),
                        },
                    }));
                }
                self.pos = self.token_stream.tokens.len();
                return None;
            };
            if matches!(lexeme, Ok(LexToken::Whitespace { .. })) {
                next_err = Some(token);
                continue;
            }
            break lexeme;
        };
        let lexeme_value = match lexeme {
            Ok(ref v) => v.value(),
            Err(ref lex_err) => {
                if let Some(val) = lex_err.value() {
                    val
                } else if matches!(token, Token::RestOfStringValue) {
                    return None;
                } else {
                    self.pos = self.token_stream.tokens.len();
                    return Some(Err(MatchError::Invalid));
                }
            }
        };
        match token {
            Token::Literal(lit) => {
                if *lit != lexeme_value {
                    self.pos = self.token_stream.tokens.len();
                    let no_more_lexemes = self.lex_iter.next().is_none();
                    if lit.starts_with(lexeme_value) && no_more_lexemes {
                        return Some(Err(MatchError::Incomplete {
                            lexeme,
                            token,
                            data: Box::new(MatcherMetadata {
                                previous_match: self.previous_match.take(),
                                acc_match: self.acc_match.take(),
                            }),
                        }));
                    } else {
                        return Some(Err(MatchError::Invalid));
                    }
                }
            }
            Token::NewFilepath | Token::Filepath => {}
            Token::Alternatives(lits) => {
                if lits.iter().all(|lit| *lit != lexeme_value) {
                    self.pos = self.token_stream.tokens.len();
                    let no_more_lexemes = self.lex_iter.next().is_none();
                    if lits.iter().any(|lit| lit.starts_with(lexeme_value)) && no_more_lexemes {
                        return Some(Err(MatchError::Incomplete {
                            lexeme,
                            token,
                            data: Box::new(MatcherMetadata {
                                previous_match: self.previous_match.take(),
                                acc_match: self.acc_match.take(),
                            }),
                        }));
                    } else {
                        return Some(Err(MatchError::Invalid));
                    }
                }
            }
            Token::AccountName => {
                let Some(acc_match) = self.mailboxes.keys().find(|acc| *acc == lexeme_value) else {
                    self.pos = self.token_stream.tokens.len();
                    let no_more_lexemes = self.lex_iter.next().is_none();
                    if self
                        .mailboxes
                        .keys()
                        .any(|acc| acc.starts_with(lexeme_value))
                        && no_more_lexemes
                    {
                        return Some(Err(MatchError::Incomplete {
                            lexeme,
                            token,
                            data: Box::new(MatcherMetadata {
                                previous_match: self.previous_match.take(),
                                acc_match: self.acc_match.take(),
                            }),
                        }));
                    } else {
                        return Some(Err(MatchError::Invalid));
                    }
                };
                self.acc_match = Some(acc_match);
            }
            Token::MailboxPath => {
                if let Some(acc) = self.acc_match {
                    if self.mailboxes[acc].iter().all(|mbox| *mbox != lexeme_value) {
                        self.pos = self.token_stream.tokens.len();
                        let no_more_lexemes = self.lex_iter.next().is_none();
                        if self.mailboxes[acc]
                            .iter()
                            .any(|mbox| mbox.starts_with(lexeme_value))
                            && no_more_lexemes
                        {
                            return Some(Err(MatchError::Incomplete {
                                lexeme,
                                token,
                                data: Box::new(MatcherMetadata {
                                    previous_match: self.previous_match.take(),
                                    acc_match: self.acc_match.take(),
                                }),
                            }));
                        } else {
                            return Some(Err(MatchError::Invalid));
                        }
                    }
                }
            }
            Token::NewMailboxPath => {}
            Token::QuotedStringValue => {}
            Token::RestOfStringValue => {}
            Token::AttachmentIndexValue => {}
            Token::MailboxIndexValue => {}
            Token::IndexValue => {}
        }
        self.pos += 1;
        match lexeme {
            Err(lex_err) => {
                self.pos = self.token_stream.tokens.len();
                if self.lex_iter.next().is_none() {
                    Some(Err(MatchError::Incomplete {
                        lexeme: Err(lex_err),
                        token,
                        data: Box::new(MatcherMetadata {
                            previous_match: self.previous_match.take(),
                            acc_match: self.acc_match.take(),
                        }),
                    }))
                } else {
                    Some(Err(MatchError::Invalid))
                }
            }
            Ok(lexeme) => {
                self.previous_match = Some((lexeme.clone(), token));
                Some(Ok((
                    lexeme,
                    token,
                    MatcherMetadata {
                        previous_match: None,
                        acc_match: self.acc_match,
                    },
                )))
            }
        }
    }
}
