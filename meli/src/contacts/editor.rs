//
// meli
//
// Copyright 2019, 2026 Manos Pitsidianakis <manos@pitsidianak.is>
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

use std::{borrow::Cow, sync::Arc};

use futures::channel::oneshot;
use melib::{
    contacts::{jscontact::json_types::UTCDateTime, AddressBook, AddressBookName, Card},
    error::Result,
    AccountHash, ContactBackendID, Error,
};

use crate::{
    account_settings,
    components::prelude::*,
    contacts::export::ContactExportDialog,
    jobs::JobId,
    terminal::{text_editing::UText, Screen, Virtual},
    types::NotificationType,
    utilities::form::{Field, FormButtonAction, TextField},
    CellBuffer, Component, ComponentId, Context, Key, StatusEvent, UIDialog, UIEvent,
};

#[derive(Debug, Clone, Ord, Hash, Eq, PartialEq, PartialOrd)]
pub enum ContactFieldLabel {
    Created,
    CryptoKey,
    Emails,
    Keywords,
    Kind,
    Name,
    Nickname,
    Note,
    GrammaticalGender,
    Pronouns,
    Extra,
    ProdId,
    Uid,
    Updated,
    Url,
    Other(String),
}

impl AsRef<str> for ContactFieldLabel {
    fn as_ref(&self) -> &str {
        match self {
            Self::Created => "Created",
            Self::CryptoKey => "Cryptographic key",
            Self::Emails => "E-mail",
            Self::Keywords => "Keywords",
            Self::Kind => "Kind",
            Self::Name => "Name",
            Self::Extra => "Extra",
            Self::Nickname => "Nickname/Aliases",
            Self::Note => "Note",
            Self::GrammaticalGender => "Grammatical gender",
            Self::Pronouns => "Pronouns",
            Self::ProdId => "Product ID",
            Self::Uid => "Uid",
            Self::Updated => "Updated",
            Self::Url => "URL",
            Self::Other(other) => other.as_str(),
        }
    }
}

impl crate::form::FormWidgetLabel for ContactFieldLabel {
    fn display(&'_ self) -> Cow<'_, str> {
        Cow::Borrowed(self.as_ref())
    }
}

impl std::fmt::Display for ContactFieldLabel {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(self.as_ref(), fmt)
    }
}

impl ContactFieldLabel {
    fn field_as_value(&self, v: Field) -> serde_json::Value {
        match self {
            Self::CryptoKey => {
                let dict_field = v.as_dict_field().expect("dict key field");
                let values = dict_field
                    .values
                    .values()
                    .iter()
                    .map(|(key, f)| {
                        (
                            key.as_ref().to_owned(),
                            serde_json::json! {{ "uri":
                                f.as_text_field().expect("str key field").as_str()
                            }},
                        )
                    })
                    .collect::<serde_json::Map<String, serde_json::Value>>();
                serde_json::Value::Object(values)
            }
            Self::Name => {
                serde_json::json! {{
                    "full" : v.as_text_field().expect("str name field").as_str()
                }}
            }
            Self::Nickname => {
                let dict_field = v.as_dict_field().expect("dict nickname field");
                let values = dict_field
                    .values
                    .values()
                    .iter()
                    .map(|(key, f)| {
                        (
                            key.as_ref().to_owned(),
                            serde_json::json! {{ "name":
                                f.as_text_field().expect("str name field").as_str()
                            }},
                        )
                    })
                    .collect::<serde_json::Map<String, serde_json::Value>>();
                serde_json::Value::Object(values)
            }
            Self::Note => {
                let dict_field = v.as_dict_field().expect("dict note field");
                let values = dict_field
                    .values
                    .values()
                    .iter()
                    .map(|(key, f)| {
                        (
                            key.as_ref().to_owned(),
                            serde_json::json! {{ "note":
                                f.as_text_field().expect("str note field").as_str()
                            }},
                        )
                    })
                    .collect::<serde_json::Map<String, serde_json::Value>>();
                serde_json::Value::Object(values)
            }
            Self::Emails => {
                let dict_field = v.as_dict_field().expect("dict email field");
                let values = dict_field
                    .values
                    .values()
                    .iter()
                    .map(|(key, f)| {
                        (
                            key.as_ref().to_owned(),
                            serde_json::json! {{ "address":
                                f.as_text_field().expect("str address field").as_str()
                            }},
                        )
                    })
                    .collect::<serde_json::Map<String, serde_json::Value>>();
                serde_json::Value::Object(values)
            }
            Self::Keywords => {
                let dict_field = v.as_dict_field().expect("dict keywords field");
                let values = dict_field
                    .values
                    .values()
                    .iter()
                    .map(|(_, f)| {
                        (
                            f.as_text_field()
                                .expect("str keyword field")
                                .as_str()
                                .to_owned(),
                            serde_json::Value::Bool(true),
                        )
                    })
                    .collect::<serde_json::Map<String, serde_json::Value>>();
                serde_json::Value::Object(values)
            }
            Self::Url => {
                let dict_field = v.as_dict_field().expect("dict url field");
                let values = dict_field
                    .values
                    .values()
                    .iter()
                    .map(|(key, f)| {
                        (
                            key.as_ref().to_owned(),
                            serde_json::json! {{ "uri":
                                f.as_text_field().expect("str url field").as_str()
                            }},
                        )
                    })
                    .collect::<serde_json::Map<String, serde_json::Value>>();
                serde_json::Value::Object(values)
            }
            Self::Pronouns => {
                let dict_field = v.as_dict_field().expect("dict pronouns field");
                let values = dict_field
                    .values
                    .values()
                    .iter()
                    .map(|(key, f)| {
                        (
                            key.as_ref().to_owned(),
                            serde_json::json! {{ "pronouns":
                                f.as_text_field().expect("str pronouns field").as_str()
                            }},
                        )
                    })
                    .collect::<serde_json::Map<String, serde_json::Value>>();
                serde_json::Value::Object(values)
            }
            Self::Created
            | Self::Updated
            | Self::Uid
            | Self::ProdId
            | Self::Kind
            | Self::GrammaticalGender => v.as_json_value(),
            Self::Extra => {
                let dict_field = v.as_dict_field().expect("dict extra field");
                let values = dict_field
                    .values
                    .values()
                    .iter()
                    .map(|(key, f)| {
                        (
                            key.as_ref().to_owned(),
                            serde_json::from_str(
                                f.as_text_field().expect("str extra field").as_str(),
                            )
                            .unwrap_or_default(),
                        )
                    })
                    .collect::<serde_json::Map<String, serde_json::Value>>();
                serde_json::Value::Object(values)
            }
            Self::Other(_) => {
                serde_json::from_str(v.as_str().unwrap_or_default()).expect("deserialize")
            }
        }
    }
}

type FormWidget = crate::utilities::form::FormWidget<FormButtonAction, ContactFieldLabel, Field>;

#[derive(Debug)]
enum ViewMode {
    ReadOnly,
    Discard {
        dialog: Box<UIDialog<char>>,
        create: bool,
    },
    WaitingForSaveResult {
        dialog: Box<UIDialog<char>>,
        receiver: oneshot::Receiver<Result<()>>,
        job_id: JobId,
        create: bool,
    },
    Edit {
        create: bool,
    },
}

#[derive(Debug)]
pub struct ContactEditor {
    id: ComponentId,
    card: Card,
    mode: ViewMode,
    form: FormWidget,
    account_hash: AccountHash,
    book_id: (Arc<ContactBackendID>, AddressBookName),
    content: Screen<Virtual>,
    theme_default: ThemeAttribute,
    dirty: bool,
    has_changes: bool,
    initialized: bool,
}

impl std::fmt::Display for ContactEditor {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.card)
    }
}

impl ContactEditor {
    pub fn create_card(
        account_hash: AccountHash,
        book_id: (Arc<ContactBackendID>, AddressBookName),
        context: &Context,
    ) -> Self {
        let book: &AddressBook = &context.accounts[&account_hash].contacts.books[&book_id];
        let mode = if book.read_only {
            ViewMode::ReadOnly
        } else {
            ViewMode::Edit { create: true }
        };
        let theme_default: ThemeAttribute = crate::conf::theme_value(context, "theme_default");
        let mut card = Card::new();
        card.created = UTCDateTime::now();
        card.updated = card.created;
        if *account_settings!(context[&account_hash].composing.insert_user_agent) {
            card.prod_id = format!("meli/{}", crate::version_migrations::LATEST.as_str());
        }
        Self {
            id: ComponentId::default(),
            card,
            mode,
            form: FormWidget::default(),
            account_hash,
            book_id,
            content: Screen::<Virtual>::new(theme_default),
            theme_default,
            dirty: true,
            has_changes: false,
            initialized: false,
        }
    }

    pub fn edit_card(
        account_hash: AccountHash,
        book_id: (Arc<ContactBackendID>, AddressBookName),
        context: &Context,
        card: Card,
    ) -> Self {
        let book: &AddressBook = &context.accounts[&account_hash].contacts.books[&book_id];
        let mode = if book.read_only {
            ViewMode::ReadOnly
        } else {
            ViewMode::Edit { create: false }
        };
        let theme_default: ThemeAttribute = crate::conf::theme_value(context, "theme_default");
        Self {
            id: ComponentId::default(),
            card,
            mode,
            form: FormWidget::default(),
            account_hash,
            book_id,
            content: Screen::<Virtual>::new(theme_default),
            theme_default,
            dirty: true,
            has_changes: false,
            initialized: false,
        }
    }

    fn initialize(&mut self, context: &Context) {
        if !self.content.resize_with_context(100, 1, context) {
            return;
        }
        let area = self.content.area();

        let is_create = matches!(
            self.mode,
            ViewMode::Discard { create: true, .. }
                | ViewMode::WaitingForSaveResult { create: true, .. }
                | ViewMode::Edit { create: true }
        );
        if matches!(self.mode, ViewMode::ReadOnly) {
            self.content.grid_mut().write_string(
                "This contact is in a read-only address book.",
                &self.theme_default,
                area,
                None,
                None,
            );
        } else if is_create {
            self.content.grid_mut().write_string(
                "New contact",
                &self.theme_default,
                area,
                None,
                None,
            );
        } else {
            let (x, _) = self.content.grid_mut().write_string(
                "Last edited: ",
                &self.theme_default,
                area,
                None,
                None,
            );
            self.content.grid_mut().write_string(
                &self.card.updated.to_string(),
                &self.theme_default,
                area.skip_cols(x),
                None,
                None,
            );
        }

        self.form = FormWidget::new(
            if matches!(self.mode, ViewMode::ReadOnly) {
                ("Cancel(Esc)".into(), FormButtonAction::Cancel)
            } else {
                (
                    if is_create {
                        "Create".into()
                    } else {
                        "Save".into()
                    },
                    FormButtonAction::Accept,
                )
            },
            // cursor_up_shortcut
            context.settings.shortcuts.general.scroll_up.clone(),
            // cursor_down_shortcut
            context.settings.shortcuts.general.scroll_down.clone(),
            // cursor_right_shortcut
            context.settings.shortcuts.general.scroll_right.clone(),
            // cursor_left_shortcut
            context.settings.shortcuts.general.scroll_left.clone(),
        );
        if !matches!(self.mode, ViewMode::ReadOnly) {
            if !is_create {
                self.form
                    .add_button(("Export".into(), FormButtonAction::Other("Export")));
            }
            self.form
                .add_button(("Cancel(Esc)".into(), FormButtonAction::Cancel));
        }
        let others = serde_json::json!(self.card)
            .as_object_mut()
            .expect("object")
            .iter_mut()
            .filter_map(|(s, v)| match s.as_str() {
                "@type" | "version" | "uid" | "created" | "updated" | "kind" | "name"
                | "nicknames" | "speakToAs" | "prodId" | "emails" | "onlineServices"
                | "cryptoKeys" | "keywords" | "notes" => None,
                other => Some((
                    ContactFieldLabel::Other(other.to_owned()),
                    std::mem::take(v),
                )),
            })
            .collect::<IndexMap<ContactFieldLabel, serde_json::Value>>();
        self.form.push((
            ContactFieldLabel::Name,
            self.card.name().unwrap_or_default().to_string(),
        ));
        self.form.push_dict(
            (
                ContactFieldLabel::Nickname,
                self.card
                    .nicknames
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.to_owned(),
                            Field::Text(TextField::new(UText::new(v.name.clone()), None)),
                        )
                    })
                    .collect(),
            ),
            (Box::new(|| Field::Text(TextField::new(UText::new(String::new()), None)))
                as Box<dyn Fn() -> Field + Send + Sync>)
                .into(),
        );
        self.form
            .push((ContactFieldLabel::Uid, self.card.uid.to_string()))
            .set_read_only(true);
        self.form
            .push((ContactFieldLabel::Created, self.card.created.to_string()))
            .set_read_only(true);
        self.form
            .push((ContactFieldLabel::Updated, self.card.updated.to_string()))
            .set_read_only(true);
        self.form
            .push((ContactFieldLabel::ProdId, self.card.prod_id.to_string()))
            .set_read_only(true);
        let kind = self.form.push_choices((
            ContactFieldLabel::Kind,
            vec![
                "individual".into(),
                "group".into(),
                "org".into(),
                "location".into(),
                "device".into(),
                "application".into(),
            ],
        ));
        kind.cursor = kind
            .values
            .iter()
            .position(|i| i == self.card.kind.as_str())
            .unwrap_or_default();
        let grammatical_gender = self.form.push_choices((
            ContactFieldLabel::GrammaticalGender,
            vec![
                "none".into(),
                "animate".into(),
                "common".into(),
                "feminine".into(),
                "inanimate".into(),
                "masculine".into(),
                "neuter".into(),
            ],
        ));
        grammatical_gender.cursor = grammatical_gender
            .values
            .iter()
            .position(|i| i == self.card.speak_to_as.grammatical_gender.as_str())
            .unwrap_or_default();
        self.form.push_dict(
            (
                ContactFieldLabel::Pronouns,
                self.card
                    .speak_to_as
                    .pronouns
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.to_owned(),
                            Field::Text(TextField::new(UText::new(v.pronouns.clone()), None)),
                        )
                    })
                    .collect(),
            ),
            (Box::new(|| Field::Text(TextField::new(UText::new(String::new()), None)))
                as Box<dyn Fn() -> Field + Send + Sync>)
                .into(),
        );
        self.form.push_dict(
            (
                ContactFieldLabel::Url,
                self.card
                    .online_services
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.to_owned(),
                            Field::Text(TextField::new(UText::new(v.uri.clone()), None)),
                        )
                    })
                    .collect(),
            ),
            (Box::new(|| Field::Text(TextField::new(UText::new(String::new()), None)))
                as Box<dyn Fn() -> Field + Send + Sync>)
                .into(),
        );
        self.form.push_dict(
            (
                ContactFieldLabel::Emails,
                self.card
                    .emails
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.to_owned(),
                            Field::Text(TextField::new(UText::new(v.address.clone()), None)),
                        )
                    })
                    .collect(),
            ),
            (Box::new(|| Field::Text(TextField::new(UText::new(String::new()), None)))
                as Box<dyn Fn() -> Field + Send + Sync>)
                .into(),
        );
        self.form
            .push_dict(
                (
                    ContactFieldLabel::Keywords,
                    self.card
                        .keywords
                        .iter()
                        .map(|(k, _)| {
                            (
                                k.to_owned(),
                                Field::Text(TextField::new(UText::new(k.to_owned()), None)),
                            )
                        })
                        .collect(),
                ),
                (Box::new(|| Field::Text(TextField::new(UText::new(String::new()), None)))
                    as Box<dyn Fn() -> Field + Send + Sync>)
                    .into(),
            )
            .is_a_set = true;
        self.form.push_dict(
            (
                ContactFieldLabel::CryptoKey,
                self.card
                    .crypto_keys
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.to_owned(),
                            Field::Text(TextField::new(UText::new(v.resource.uri.clone()), None)),
                        )
                    })
                    .collect(),
            ),
            (Box::new(|| Field::Text(TextField::new(UText::new(String::new()), None)))
                as Box<dyn Fn() -> Field + Send + Sync>)
                .into(),
        );
        self.form.push_dict(
            (
                ContactFieldLabel::Note,
                self.card
                    .notes
                    .iter()
                    .map(|(k, v)| {
                        (
                            k.to_owned(),
                            Field::Text(TextField::new(UText::new(v.note.clone()), None)),
                        )
                    })
                    .collect(),
            ),
            (Box::new(|| Field::Text(TextField::new(UText::new(String::new()), None)))
                as Box<dyn Fn() -> Field + Send + Sync>)
                .into(),
        );
        if !self.card.extra.is_empty() {
            self.form.push_dict(
                (
                    ContactFieldLabel::Extra,
                    self.card
                        .extra
                        .iter()
                        .map(|(k, v)| {
                            (
                                k.to_owned(),
                                Field::Text(
                                    TextField::new(UText::new(v.to_string()), None)
                                        .with_read_only(true),
                                ),
                            )
                        })
                        .collect(),
                ),
                (Box::new(|| {
                    Field::Text(
                        TextField::new(UText::new(String::new()), None).with_read_only(true),
                    )
                }) as Box<dyn Fn() -> Field + Send + Sync>)
                    .into(),
            );
        }
        for (label, val) in others {
            self.form.push((label, val.to_string())).set_read_only(true);
        }
    }

    fn as_json(&self) -> serde_json::Value {
        let fields: IndexMap<ContactFieldLabel, serde_json::Value> = self
            .form
            .values()
            .clone()
            .into_iter()
            .map(|(label, v)| {
                let value = label.field_as_value(v);
                (label, value)
            })
            .collect();
        let mut retval: serde_json::Map<String, serde_json::Value> = std::iter::once((
            "@type".to_owned(),
            serde_json::Value::String("Card".to_owned()),
        ))
        .chain(std::iter::once((
            "version".to_owned(),
            serde_json::Value::String("1.0".to_owned()),
        )))
        .collect();
        for (label, value) in fields {
            match label {
                ContactFieldLabel::CryptoKey => {
                    retval.insert("cryptoKeys".to_string(), value);
                }
                ContactFieldLabel::Name => {
                    retval.insert("name".to_string(), value);
                }
                ContactFieldLabel::Nickname => {
                    retval.insert("nicknames".to_string(), value);
                }
                ContactFieldLabel::Note => {
                    retval.insert("notes".to_string(), value);
                }
                ContactFieldLabel::Emails => {
                    retval.insert("emails".to_string(), value);
                }
                ContactFieldLabel::Keywords => {
                    retval.insert("keywords".to_string(), value);
                }
                ContactFieldLabel::Created => {
                    retval.insert("created".to_string(), value);
                }
                ContactFieldLabel::Updated => {
                    retval.insert("updated".to_string(), value);
                }
                ContactFieldLabel::Uid => {
                    retval.insert("uid".to_string(), value);
                }
                ContactFieldLabel::ProdId => {
                    retval.insert("prodId".to_string(), value);
                }
                ContactFieldLabel::Kind => {
                    retval.insert("kind".to_string(), value);
                }
                ContactFieldLabel::Url => {
                    retval.insert("onlineServices".to_string(), value);
                }
                ContactFieldLabel::Other(other) => {
                    retval.insert(other, value);
                }
                ContactFieldLabel::GrammaticalGender => {
                    retval.entry("speakToAs").or_insert_with(|| {
                        serde_json::json!(melib::jscontact::json_types::SpeakToAs::default())
                    })["grammaticalGender"] = value;
                }
                ContactFieldLabel::Pronouns => {
                    retval.entry("speakToAs").or_insert_with(|| {
                        serde_json::json!(melib::jscontact::json_types::SpeakToAs::default())
                    })["pronouns"] = value;
                }
                ContactFieldLabel::Extra => {
                    let serde_json::Value::Object(values) = value else {
                        unreachable!();
                    };
                    retval.extend(values);
                }
            };
        }

        serde_json::Value::Object(retval)
    }
}

impl Component for ContactEditor {
    fn draw(&mut self, grid: &mut CellBuffer, area: Area, context: &mut Context) {
        if !self.initialized {
            self.initialize(context);
            self.initialized = true;
        }

        if self.is_dirty() {
            grid.clear_area(area, self.theme_default);
            grid.copy_area(self.content.grid(), area, self.content.area());
            self.dirty = false;
        }

        self.form
            .draw(grid, area.skip_rows(self.content.area().height()), context);
        if let ViewMode::Discard { ref mut dialog, .. } = self.mode {
            // Let user choose whether to quit with/without saving or cancel
            dialog.draw(grid, area, context);
        }

        context.dirty_areas.push_back(area);
    }

    fn process_event(&mut self, event: &mut UIEvent, context: &mut Context) -> bool {
        if let UIEvent::ConfigReload { old_settings: _ } = event {
            self.theme_default = crate::conf::theme_value(context, "theme_default");
            self.content.grid_mut().empty();
            self.initialized = false;
            self.set_dirty(true);
        }
        match self.mode {
            ViewMode::Discard {
                ref mut dialog,
                ref create,
            } => {
                if matches!(event, UIEvent::ComponentUnrealize(ref id) if *id == dialog.id()) {
                    let create = *create;
                    dialog.unrealize(context);
                    self.mode = ViewMode::Edit { create };
                    self.set_dirty(true);
                    return true;
                }
                if dialog.process_event(event, context) {
                    self.set_dirty(true);
                    return true;
                }
            }
            ViewMode::Edit { create } => {
                if matches!(event, UIEvent::Input(Key::Esc)) {
                    if self.can_quit_cleanly(context) {
                        self.unrealize(context);
                    }
                    return true;
                }
                if self.form.process_event(event, context) {
                    match self.form.buttons_result() {
                        None => {}
                        Some(FormButtonAction::Accept) => {
                            let json = self.as_json();
                            let mut card: Card = serde_json::from_value(json).expect("deserialize");
                            card.uid = self.card.uid.clone();
                            let res = if create {
                                context.accounts[&self.account_hash]
                                    .add_card(self.book_id.clone(), card)
                            } else {
                                context.accounts[&self.account_hash]
                                    .edit_card(self.book_id.clone(), card)
                            };
                            match res {
                                Ok((job_id, receiver)) => {
                                    let dialog = UIDialog::new(
                                        "Waiting for confirmation. The tab will close \
                                         automatically on successful creation.",
                                        vec![
                                            ('c', "force close tab".to_string()),
                                            (
                                                'n',
                                                "close this message and return to edit mode"
                                                    .to_string(),
                                            ),
                                        ],
                                        true,
                                        Some(Box::new(
                                            move |id: ComponentId, results: &[char], context| {
                                                context.replies.push_back(
                                                    UIEvent::FinishedUIDialog(
                                                        id,
                                                        Box::new(
                                                            results.first().cloned().unwrap_or('c'),
                                                        ),
                                                    ),
                                                );
                                            },
                                        )),
                                        context,
                                    )
                                    .into();
                                    self.mode = ViewMode::WaitingForSaveResult {
                                        dialog,
                                        job_id,
                                        receiver,
                                        create,
                                    };
                                }
                                Err(err) => {
                                    self.mode = ViewMode::Edit { create };
                                    context.replies.push_back(UIEvent::Notification {
                                        title: None,
                                        source: None,
                                        body: err.to_string().into(),
                                        kind: Some(NotificationType::Error(err.kind)),
                                    });
                                    self.initialized = false;
                                    self.set_dirty(true);
                                }
                            }
                            self.set_dirty(true);
                        }
                        Some(FormButtonAction::Cancel) => {
                            self.unrealize(context);
                        }
                        Some(FormButtonAction::Other("Export")) => {
                            let card = if self.has_changes {
                                let json = self.as_json();
                                let mut card: Card =
                                    serde_json::from_value(json).expect("deserialize");
                                card.uid = self.card.uid.clone();
                                card
                            } else {
                                self.card.clone()
                            };
                            context.replies.push_back(UIEvent::GlobalUIDialog {
                                value: Box::new(ContactExportDialog::new(card, context)),
                                parent: None,
                            });
                            return true;
                        }
                        Some(FormButtonAction::Reset | FormButtonAction::Other(_)) => {}
                    }
                    self.set_dirty(true);
                    if matches!(event, UIEvent::InsertInput(_)) {
                        self.has_changes = true;
                    }
                    return true;
                }
            }
            ViewMode::ReadOnly => {
                if matches!(event, UIEvent::Input(Key::Esc)) {
                    if self.can_quit_cleanly(context) {
                        self.unrealize(context);
                    }
                    return true;
                }
            }
            ViewMode::WaitingForSaveResult {
                ref mut dialog,
                ref mut receiver,
                job_id,
                create,
            } => {
                match event {
                    UIEvent::FinishedUIDialog(id, result) if *id == dialog.id() => {
                        if let Some(key) = result.downcast_mut::<char>() {
                            match key {
                                'c' => {
                                    self.unrealize(context);
                                    return true;
                                }
                                'n' => {
                                    self.set_dirty(true);
                                    self.mode = ViewMode::Edit { create };
                                }
                                _ => {}
                            }
                        }
                    }
                    UIEvent::StatusEvent(StatusEvent::JobFinished(ref other_job_id))
                        if *other_job_id == job_id =>
                    {
                        match receiver.try_recv().map_err(
                            |_: futures::channel::oneshot::Canceled| Error::new("Job was canceled"),
                        ) {
                            Err(err) | Ok(Some(Err(err))) => {
                                self.mode = ViewMode::Edit { create };
                                context.replies.push_back(UIEvent::Notification {
                                    title: None,
                                    source: None,
                                    body: err.to_string().into(),
                                    kind: Some(NotificationType::Error(err.kind)),
                                });
                                self.set_dirty(true);
                            }
                            Ok(None) | Ok(Some(Ok(()))) => {
                                self.mode = ViewMode::Edit { create };
                                context.replies.push_back(UIEvent::Notification {
                                    title: Some("Contact saved.".into()),
                                    source: None,
                                    body: format!(
                                        "Card was {}.",
                                        if create { "created" } else { "saved" }
                                    )
                                    .into(),
                                    kind: Some(NotificationType::Info),
                                });
                                self.unrealize(context);
                            }
                        }
                    }
                    _ => {
                        if dialog.process_event(event, context) {
                            self.set_dirty(true);
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    fn is_dirty(&self) -> bool {
        self.dirty
            || self.form.is_dirty()
            || matches!(self.mode,  ViewMode::Discard {ref dialog, ..} if dialog.is_dirty())
    }

    fn set_dirty(&mut self, value: bool) {
        self.dirty = value;
        self.form.set_dirty(value);
        if let ViewMode::Discard { ref mut dialog, .. } = self.mode {
            dialog.set_dirty(value);
        }
    }

    fn shortcuts(&self, context: &Context) -> ShortcutMaps {
        match self.mode {
            ViewMode::Discard { .. }
            | ViewMode::ReadOnly
            | ViewMode::WaitingForSaveResult { .. } => ShortcutMaps::default(),
            ViewMode::Edit { create: _ } => self.form.shortcuts(context),
        }
    }

    fn id(&self) -> ComponentId {
        self.id
    }

    fn can_quit_cleanly(&mut self, context: &Context) -> bool {
        if !self.has_changes {
            return true;
        }

        match self.mode {
            ViewMode::Discard { .. } | ViewMode::ReadOnly => true,
            ViewMode::WaitingForSaveResult { .. } => false,
            ViewMode::Edit { create } => {
                let parent_id = self.id;
                // Ask user for confirmation
                self.mode = ViewMode::Discard {
                    dialog: Box::new(UIDialog::new(
                        "this contact has unsaved changes",
                        vec![
                            ('y', "quit without saving".to_string()),
                            ('n', "cancel".to_string()),
                        ],
                        true,
                        Some(Box::new(move |id, results: &[char], context| {
                            context
                                .replies
                                .push_back(if matches!(results.first(), Some(&'y')) {
                                    UIEvent::ComponentUnrealize(parent_id)
                                } else {
                                    UIEvent::ComponentUnrealize(id)
                                });
                        })),
                        context,
                    )),
                    create,
                };
                self.set_dirty(true);
                false
            }
        }
    }
}
