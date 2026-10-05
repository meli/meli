//
// meli
//
// Copyright 2026  Manos Pitsidianakis
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

use std::{
    future::Future,
    io::Write,
    os::unix::fs::PermissionsExt,
    path::Path,
    sync::{Arc, Mutex},
};

use indexmap::IndexMap;
use melib::{
    contacts::{
        backend::ContactBackend, mutt, notmuchcontact, vcard, AddressBook, AddressBookName, Card,
        CardId, ContactBackendID,
    },
    error::{Result, ResultIntoError},
    ErrorKind,
};

use crate::{
    accounts::{Account, JobRequest},
    jobs::{IsAsync, JobId, JoinHandle},
    NotificationType, StatusEvent, ThreadEvent, UIEvent,
};

impl Account {
    pub fn init_contact_backend<
        F: Future<Output = Result<Box<dyn ContactBackend>>> + Send + 'static,
    >(
        &mut self,
        name: String,
        format: String,
        fut: F,
        is_async: IsAsync,
    ) {
        let account_name = &self.name;
        let handle = self.main_loop_handler.job_executor.spawn(
            format!("initialize {account_name} {name} {format} contacts").into(),
            fut,
            is_async,
        );
        let job_id = handle.job_id;
        self.active_jobs.insert(
            job_id,
            JobRequest::Contacts(ContactJobRequest::Initialize {
                name,
                format,
                handle,
            }),
        );
        self.active_job_instants
            .insert(std::time::Instant::now(), job_id);
        self.main_loop_handler
            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                StatusEvent::NewJob(job_id),
            )));
    }

    pub fn init_contacts(&mut self) {
        {
            // Create a default local address book for the account.
            let default_address_book = match Self::default_address_book(self.name.as_ref()) {
                Ok(v) => v,
                Err(err) => {
                    self.main_loop_handler
                        .send(ThreadEvent::UIEvent(UIEvent::Notification {
                            title: Some(format!("{}: Could not load contacts", self.name).into()),
                            body: err.to_string().into(),
                            kind: Some(NotificationType::Error(err.kind)),
                            source: Some(err),
                        }));
                    AddressBook::new("default".into(), false)
                }
            };

            self.contacts.add_book("meli", "Card", default_address_book);
        }

        {
            let mut backend = self.backend.lock().unwrap();
            let format = self.settings.account().format.clone();
            match backend.contact_backend() {
                Ok(contacts_job) => {
                    drop(backend);
                    self.init_contact_backend(
                        "meli".to_string(),
                        format,
                        contacts_job,
                        self.is_async(),
                    );
                }
                Err(err) => {
                    if !matches!(err.kind, ErrorKind::NotImplemented) {
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(
                                    format!(
                                        "{}: Could not request mail backend's contact backend",
                                        self.name
                                    )
                                    .into(),
                                ),
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                                source: Some(err),
                            }));
                    }
                }
            }
        }
        let conf = self.settings.account().contacts.clone();
        for (name, contact_backend) in conf {
            use melib::conf::ContactBackendConf;
            match contact_backend {
                ContactBackendConf::NotmuchAddress(query) => {
                    self.init_contact_backend(
                        name,
                        "notmuch_address_book_query".to_string(),
                        async move {
                            Ok(Box::new(notmuchcontact::NotmuchContacts { query })
                                as Box<dyn ContactBackend>)
                        },
                        IsAsync::Async,
                    );
                }
                ContactBackendConf::MuttAlias(mutt_alias_file) => {
                    let path = Path::new(&mutt_alias_file).into();
                    self.init_contact_backend(
                        name,
                        "mutt_alias_file".to_string(),
                        async move {
                            Ok(Box::new(mutt::MuttContacts { path }) as Box<dyn ContactBackend>)
                        },
                        IsAsync::Async,
                    );
                }
                ContactBackendConf::VCard(vcard_path) => {
                    let path = Path::new(&vcard_path).into();
                    self.init_contact_backend(
                        name,
                        "vcard".to_string(),
                        async move {
                            Ok(Box::new(vcard::VCardContacts { path }) as Box<dyn ContactBackend>)
                        },
                        IsAsync::Async,
                    );
                }
                ContactBackendConf::CardDAV(server_conf) => {
                    self.init_contact_backend(
                        name,
                        "carddav".to_string(),
                        async move {
                            use melib::{
                                contacts::carddav::{CardDAVConnection, CardDAVContacts},
                                utils::webdav::WebDAVConnection,
                            };
                            let mut connection = WebDAVConnection::new(&server_conf).await?;
                            connection.connect().await?;
                            let connection = CardDAVConnection::new(connection).await?.into();

                            Ok(Box::new(CardDAVContacts { connection }) as Box<dyn ContactBackend>)
                        },
                        IsAsync::Async,
                    );
                }
            }
        }
    }

    fn default_address_book(name: &str) -> Result<AddressBook> {
        let data_dir = xdg::BaseDirectories::with_profile("meli", name)?;
        let mut default_address_book = AddressBook::new("default".into(), false);

        if let Ok(data) = data_dir.place_data_file("contacts") {
            if data.exists() {
                fn read_contacts(book: &mut AddressBook, file: &Path) -> Result<()> {
                    let reader = std::io::BufReader::new(
                        std::fs::File::open(file).chain_err_related_path(file)?,
                    );
                    let data: IndexMap<CardId, Card> =
                        serde_json::from_reader(reader).chain_err_related_path(file)?;
                    for (id, c) in data {
                        if !book.card_exists(id) && !c.external_resource() {
                            book.add_card(c);
                        }
                    }
                    Ok(())
                }
                read_contacts(&mut default_address_book, &data)?;
            }
        }
        Ok(default_address_book)
    }

    pub fn write_default_address_book_to_disk(&self) -> Result<()> {
        let Some(book) = self.contacts.get_book("meli", "default", "Card") else {
            return Ok(());
        };
        let data_dir = xdg::BaseDirectories::with_profile("meli", self.name.as_ref())?;
        let (data, data_new) = (
            data_dir.place_data_file("contacts")?,
            data_dir.place_data_file("contacts_new")?,
        );
        let f = std::fs::File::create(&data_new).chain_err_related_path(&data_new)?;
        if let Ok(metadata) = f.metadata() {
            let mut permissions = metadata.permissions();

            permissions.set_mode(0o600); // Read/write for owner only.
            f.set_permissions(permissions)
                .chain_err_related_path(&data_new)?;
        }
        let mut writer = std::io::BufWriter::new(f);
        serde_json::to_writer(&mut writer, &book.cards).chain_err_related_path(&data_new)?;
        writer.flush().chain_err_related_path(&data_new)?;
        drop(writer);
        std::fs::rename(&data_new, &data).chain_err_related_path(&data)?;
        Ok(())
    }

    pub fn add_contact_backend(
        &mut self,
        name: String,
        format: String,
        mut backend: Box<dyn ContactBackend>,
    ) {
        match backend.address_books() {
            Ok(v) => {
                let handle = self.main_loop_handler.job_executor.spawn(
                    format!("fetch {name} {format} address books").into(),
                    v,
                    backend.capabilities().is_async.into(),
                );
                let job_id = handle.job_id;
                self.main_loop_handler
                    .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                        StatusEvent::NewJob(job_id),
                    )));
                self.contact_backends.insert(
                    ContactBackendID {
                        name: name.clone().into(),
                        format: format.clone().into(),
                    },
                    Arc::new(Mutex::new(backend)),
                );
                self.active_jobs.insert(
                    job_id,
                    JobRequest::Contacts(ContactJobRequest::AddressBooks {
                        name,
                        format,
                        handle,
                    }),
                );
                self.active_job_instants
                    .insert(std::time::Instant::now(), job_id);
            }
            Err(err) => {
                self.main_loop_handler
                    .send(ThreadEvent::UIEvent(UIEvent::Notification {
                        title: Some(
                            format!(
                                "{}: Could not load {name} {format} backend contacts",
                                self.name
                            )
                            .into(),
                        ),
                        body: err.to_string().into(),
                        kind: Some(NotificationType::Error(err.kind)),
                        source: Some(err),
                    }));
            }
        }
    }

    pub fn fetch_contact_backend_address_books(
        &mut self,
        name: String,
        format: String,
        address_books: Vec<AddressBookName>,
    ) {
        let backend_id = ContactBackendID {
            name: name.clone().into(),
            format: format.clone().into(),
        };
        let backend = self.contact_backends[&backend_id].as_ref();
        let mut backend = backend.lock().unwrap();
        for book in address_books {
            match backend.fetch_book(&book) {
                Ok(v) => {
                    let handle = self.main_loop_handler.job_executor.spawn(
                        format!("fetch {name} {format} address book").into(),
                        v,
                        self.is_async(),
                    );
                    let job_id = handle.job_id;
                    self.active_jobs.insert(
                        job_id,
                        JobRequest::Contacts(ContactJobRequest::Cards {
                            name: format.clone(),
                            format: format.clone(),
                            book,
                            handle,
                        }),
                    );
                    self.active_job_instants
                        .insert(std::time::Instant::now(), job_id);
                    self.main_loop_handler
                        .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                            StatusEvent::NewJob(job_id),
                        )));
                }
                Err(err) => {
                    self.main_loop_handler
                        .send(ThreadEvent::UIEvent(UIEvent::Notification {
                            title: Some(
                                format!(
                                    "{}: Could not load {name} {format} backend contacts",
                                    self.name
                                )
                                .into(),
                            ),
                            body: err.to_string().into(),
                            kind: Some(NotificationType::Error(err.kind)),
                            source: Some(err),
                        }));
                }
            }
        }
    }

    pub fn process_contact_event(&mut self, job_id: JobId, job: ContactJobRequest) -> bool {
        macro_rules! is_canceled {
            ($handle:expr) => {{
                if $handle.is_canceled() {
                    self.main_loop_handler
                        .job_executor
                        .set_job_success(job_id, false);
                    /* canceled */
                    return true;
                }
            }};
        }
        match job {
            ContactJobRequest::Initialize {
                name,
                format,
                mut handle,
            } => {
                is_canceled! { handle };
                match handle.chan.try_recv() {
                    Err(_) => { /* canceled */ }
                    Ok(None) => {}
                    Ok(Some(Err(err))) => {
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(
                                    format!(
                                        "{}: {name} {format} contact initialization failed",
                                        self.name
                                    )
                                    .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(backend))) => {
                        self.add_contact_backend(name, format, backend);
                    }
                }
            }
            ContactJobRequest::AddressBooks {
                name,
                format,
                mut handle,
            } => {
                is_canceled! { handle };
                match handle.chan.try_recv() {
                    Err(_) => { /* canceled */ }
                    Ok(None) => {}
                    Ok(Some(Err(err))) => {
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(
                                    format!(
                                        "{}: {name} {format} contact initialization failed",
                                        self.name
                                    )
                                    .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(address_books))) => {
                        self.fetch_contact_backend_address_books(name, format, address_books);
                    }
                }
            }
            ContactJobRequest::Cards {
                name,
                format,
                book,
                mut handle,
            } => {
                is_canceled! { handle };
                match handle.chan.try_recv() {
                    Err(_) => { /* canceled */ }
                    Ok(None) => {}
                    Ok(Some(Err(err))) => {
                        self.main_loop_handler
                            .job_executor
                            .set_job_success(job_id, false);
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(
                                    format!(
                                        "{}: {name} {format} contact initialization failed",
                                        self.name
                                    )
                                    .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(cards))) => {
                        let mut b = AddressBook::new(book, true);
                        for c in cards {
                            b.add_card(c);
                        }
                        self.contacts.add_book(&name, &format, b);
                        self.main_loop_handler.send(ThreadEvent::UIEvent(
                            UIEvent::AccountStatusChange(self.hash, None),
                        ));
                    }
                }
            }
        }
        true
    }
}

pub enum ContactJobRequest {
    Initialize {
        name: String,
        format: String,
        handle: JoinHandle<Result<Box<dyn melib::contacts::backend::ContactBackend>>>,
    },
    AddressBooks {
        name: String,
        format: String,
        handle: JoinHandle<Result<Vec<AddressBookName>>>,
    },
    Cards {
        name: String,
        format: String,
        book: AddressBookName,
        handle: JoinHandle<Result<Vec<Card>>>,
    },
}

impl std::fmt::Debug for ContactJobRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Initialize { .. } => write!(f, "JobRequest::Initialize"),
            Self::AddressBooks { .. } => write!(f, "JobRequest::AddressBooks"),
            Self::Cards { .. } => write!(f, "JobRequest::Cards"),
        }
    }
}

impl std::fmt::Display for ContactJobRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Initialize { .. } => write!(f, "Initialize"),
            Self::AddressBooks { .. } => write!(f, "Get address books"),
            Self::Cards { .. } => write!(f, "Get cards"),
        }
    }
}

impl ContactJobRequest {
    pub fn cancel(&self) -> Option<StatusEvent> {
        match self {
            Self::Initialize { handle, .. } => handle.cancel(),
            Self::AddressBooks { handle, .. } => handle.cancel(),
            Self::Cards { handle, .. } => handle.cancel(),
        }
    }
}
