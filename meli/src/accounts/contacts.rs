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
    io::Write as _,
    os::unix::fs::PermissionsExt as _,
    path::Path,
    sync::{Arc, Mutex},
};

use futures::channel::oneshot;
use indexmap::IndexMap;
use melib::{
    backends::prelude::ResultFuture,
    contacts::{
        backend::{ContactBackend, ContactBackendCapabilities},
        mutt, notmuchcontact, vcard, AddressBook, AddressBookName, Card, CardId, ContactBackendID,
    },
    error::{Error, Result, ResultIntoError as _},
    ErrorKind,
};

use crate::{
    accounts::{Account, JobRequest},
    jobs::{IsAsync, JobId, JoinHandle},
    types::ContactBackendStatusChangeKind,
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
            format!("{account_name}: initialize {name} {format} contacts").into(),
            fut,
            is_async,
        );
        let job_id = handle.job_id;
        self.active_jobs.insert(
            job_id,
            JobRequest::Contacts(ContactJobRequest::Initialize {
                backend_id: Arc::new(ContactBackendID {
                    name: name.into(),
                    format: format.into(),
                }),
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
            let default_address_backend = self.default_address_backend();

            let address_book = default_address_backend.fetch_default_book();

            let backend_id = Arc::new(ContactBackendID {
                name: DefaultContacts::NAME.to_string().into(),
                format: DefaultContacts::FORMAT.to_string().into(),
            });
            self.contacts.add_book(backend_id.clone(), address_book);
            self.contact_backends.insert(
                backend_id,
                Arc::new(Mutex::new(Box::new(default_address_backend))),
            );
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

    fn default_address_backend(&self) -> DefaultContacts {
        let mut cards = Default::default();
        if let Ok(data) = xdg::BaseDirectories::with_profile("meli", self.name.as_ref())
            .map_err(Error::from)
            .and_then(|data_dir| data_dir.place_data_file("contacts").map_err(Into::into))
        {
            if data.exists() {
                fn read_contacts(file: &Path) -> Result<IndexMap<CardId, Card>> {
                    let reader = std::io::BufReader::new(
                        std::fs::File::open(file).chain_err_related_path(file)?,
                    );
                    let data: IndexMap<CardId, Card> =
                        serde_json::from_reader(reader).chain_err_related_path(file)?;
                    Ok(data)
                }
                match read_contacts(&data) {
                    Err(err) => {
                        self.main_loop_handler
                            .send(ThreadEvent::UIEvent(UIEvent::Notification {
                                title: Some(
                                    format!("{}: Could not load contacts", self.name).into(),
                                ),
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                                source: Some(err),
                            }))
                    }
                    Ok(v) => {
                        cards = v;
                    }
                }
            }
        }
        DefaultContacts { cards }
    }

    pub fn write_default_address_book_to_disk(&self) -> Result<()> {
        let Some(book) = self.contacts.get_book(
            &ContactBackendID {
                name: DefaultContacts::NAME.to_string().into(),
                format: DefaultContacts::FORMAT.to_string().into(),
            },
            DefaultContacts::DEFAULT_ADDRESS_BOOK,
        ) else {
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
        backend_id: Arc<ContactBackendID>,
        mut backend: Box<dyn ContactBackend>,
    ) {
        match backend.address_books() {
            Ok(v) => {
                let handle = self.main_loop_handler.job_executor.spawn(
                    format!(
                        "{account_name}:fetch {name} {format} address books",
                        account_name = self.name,
                        name = backend_id.name,
                        format = backend_id.format
                    )
                    .into(),
                    v,
                    backend.capabilities().is_async.into(),
                );
                let job_id = handle.job_id;
                self.main_loop_handler
                    .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                        StatusEvent::NewJob(job_id),
                    )));
                self.contact_backends
                    .insert(backend_id.clone(), Arc::new(Mutex::new(backend)));
                self.active_jobs.insert(
                    job_id,
                    JobRequest::Contacts(ContactJobRequest::AddressBooks { backend_id, handle }),
                );
                self.active_job_instants
                    .insert(std::time::Instant::now(), job_id);
            }
            Err(err) => {
                self.main_loop_handler
                    .send(ThreadEvent::UIEvent(UIEvent::Notification {
                        title: Some(
                            format!(
                                "{account}: Could not load {name} {format} backend contacts",
                                account = self.name,
                                name = backend_id.name,
                                format = backend_id.format,
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

    pub fn fetch_contact_backend_address_book(
        &mut self,
        backend_id: Arc<ContactBackendID>,
        book: AddressBookName,
    ) {
        let backend = self.contact_backends[&backend_id].as_ref();
        let mut backend = backend.lock().unwrap();
        match backend.fetch_book(&book) {
            Ok(v) => {
                let handle = self.main_loop_handler.job_executor.spawn(
                    format!(
                        "{account_name}:fetch {name} {format} address book",
                        account_name = self.name,
                        name = backend_id.name,
                        format = backend_id.format
                    )
                    .into(),
                    v,
                    backend.capabilities().is_async.into(),
                );
                let job_id = handle.job_id;
                self.active_jobs.insert(
                    job_id,
                    JobRequest::Contacts(ContactJobRequest::Cards {
                        backend_id,
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
                                "{account}: Could not load {name} {format} backend contacts",
                                account = self.name,
                                name = backend_id.name,
                                format = backend_id.format
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
                backend_id,
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
                                        "{account}: {name} {format} contact initialization failed",
                                        account = self.name,
                                        name = backend_id.name,
                                        format = backend_id.format,
                                    )
                                    .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(backend))) => {
                        self.add_contact_backend(backend_id, backend);
                    }
                }
            }
            ContactJobRequest::AddressBooks {
                backend_id,
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
                                        "{account}: {name} {format} contact initialization failed",
                                        account = self.name,
                                        name = backend_id.name,
                                        format = backend_id.format,
                                    )
                                    .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(address_books))) => {
                        for book in address_books {
                            self.fetch_contact_backend_address_book(backend_id.clone(), book);
                        }
                    }
                }
            }
            ContactJobRequest::Cards {
                backend_id,
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
                                        "{account}: {name} {format} address book {book} fetch \
                                         failed",
                                        account = self.name,
                                        name = backend_id.name,
                                        format = backend_id.format,
                                    )
                                    .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(b))) => {
                        self.main_loop_handler.send(ThreadEvent::UIEvent(
                            UIEvent::ContactBackendStatusChange(
                                self.hash,
                                backend_id.clone(),
                                book,
                                ContactBackendStatusChangeKind::NewBook,
                            ),
                        ));
                        self.contacts.add_book(backend_id, b);
                    }
                }
            }
            ContactJobRequest::Edit {
                backend_id,
                book,
                mut handle,
                create: true,
                card_id: _,
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
                                        "{account}: {name} {format} contact creation failed",
                                        account = self.name,
                                        name = backend_id.name,
                                        format = backend_id.format,
                                    )
                                    .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(new_card))) => {
                        self.main_loop_handler.send(ThreadEvent::UIEvent(
                            UIEvent::ContactBackendStatusChange(
                                self.hash,
                                backend_id.clone(),
                                book.clone(),
                                ContactBackendStatusChangeKind::Creation(new_card.id()),
                            ),
                        ));
                        self.contacts.books[&(backend_id, book)].add_card(new_card);
                    }
                }
            }
            ContactJobRequest::Edit {
                backend_id,
                book,
                mut handle,
                create: false,
                card_id,
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
                                        "{account}: {name} {format} contact edit failed",
                                        account = self.name,
                                        name = backend_id.name,
                                        format = backend_id.format,
                                    )
                                    .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(new_card))) => {
                        if self.contacts.books[&(Arc::clone(&backend_id), book.clone())]
                            .card_exists(card_id)
                        {
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::ContactBackendStatusChange(
                                    self.hash,
                                    backend_id.clone(),
                                    book.clone(),
                                    ContactBackendStatusChangeKind::Modification(card_id),
                                ),
                            ));
                            self.contacts.books[&(backend_id, book)]
                                .cards
                                .insert(card_id, new_card);
                        }
                    }
                }
            }
            ContactJobRequest::DeleteCard {
                backend_id,
                book,
                mut handle,
                card_id,
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
                                        "{account}: {name} {format} contact deletion failed",
                                        account = self.name,
                                        name = backend_id.name,
                                        format = backend_id.format,
                                    )
                                    .into(),
                                ),
                                source: None,
                                body: err.to_string().into(),
                                kind: Some(NotificationType::Error(err.kind)),
                            }));
                    }
                    Ok(Some(Ok(()))) => {
                        if self.contacts.books[&(Arc::clone(&backend_id), book.clone())]
                            .card_exists(card_id)
                        {
                            self.main_loop_handler.send(ThreadEvent::UIEvent(
                                UIEvent::ContactBackendStatusChange(
                                    self.hash,
                                    backend_id.clone(),
                                    book.clone(),
                                    ContactBackendStatusChangeKind::Deletion(card_id),
                                ),
                            ));
                            self.contacts.books[&(backend_id, book)]
                                .cards
                                .shift_remove(&card_id);
                        }
                    }
                }
            }
        }
        true
    }

    pub fn add_card(
        &mut self,
        (backend_id, book): (Arc<ContactBackendID>, AddressBookName),
        card: Card,
    ) -> Result<(JobId, oneshot::Receiver<Result<()>>)> {
        let (sender, receiver) = oneshot::channel();
        let backend = self.contact_backends[&*backend_id].as_ref();
        let mut backend = backend.lock().unwrap();
        let card_id = card.id();
        let job_name = format!("{name}:contact::create {card}", name = self.name).into();
        let fut = backend.add(&book, card)?;
        let handle = self.main_loop_handler.job_executor.spawn(
            job_name,
            async move {
                match fut.await {
                    ok @ Ok(_) => {
                        _ = sender.send(Ok(()));
                        ok
                    }
                    Err(err) => {
                        _ = sender.send(Err(err.clone()));
                        Err(err)
                    }
                }
            },
            backend.capabilities().is_async.into(),
        );
        let job_id = handle.job_id;
        self.active_jobs.insert(
            job_id,
            JobRequest::Contacts(ContactJobRequest::Edit {
                backend_id,
                book,
                handle,
                card_id,
                create: true,
            }),
        );
        self.active_job_instants
            .insert(std::time::Instant::now(), job_id);
        self.main_loop_handler
            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                StatusEvent::NewJob(job_id),
            )));
        Ok((job_id, receiver))
    }

    pub fn edit_card(
        &mut self,
        (backend_id, book): (Arc<ContactBackendID>, AddressBookName),
        card: Card,
    ) -> Result<(JobId, oneshot::Receiver<Result<()>>)> {
        let (sender, receiver) = oneshot::channel();
        let backend = self.contact_backends[&*backend_id].as_ref();
        let mut backend = backend.lock().unwrap();
        let card_id = card.id();
        let fut = backend.edit(&book, card)?;
        let handle = self.main_loop_handler.job_executor.spawn(
            format!("{name}:contact::edit", name = self.name).into(),
            async move {
                match fut.await {
                    ok @ Ok(_) => {
                        _ = sender.send(Ok(()));
                        ok
                    }
                    Err(err) => {
                        _ = sender.send(Err(err.clone()));
                        Err(err)
                    }
                }
            },
            backend.capabilities().is_async.into(),
        );
        let job_id = handle.job_id;
        self.active_jobs.insert(
            job_id,
            JobRequest::Contacts(ContactJobRequest::Edit {
                backend_id,
                book,
                handle,
                create: false,
                card_id,
            }),
        );
        self.active_job_instants
            .insert(std::time::Instant::now(), job_id);
        self.main_loop_handler
            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                StatusEvent::NewJob(job_id),
            )));
        Ok((job_id, receiver))
    }

    pub fn delete_card(
        &mut self,
        (backend_id, book): (Arc<ContactBackendID>, AddressBookName),
        card: Card,
    ) -> Result<(JobId, oneshot::Receiver<Result<()>>)> {
        let (sender, receiver) = oneshot::channel();
        let backend = self.contact_backends[&*backend_id].as_ref();
        let mut backend = backend.lock().unwrap();
        let card_id = card.id();
        let job_name = format!("{name}:contact::delete {card}", name = self.name).into();
        let fut = backend.delete(&book, card)?;
        let handle = self.main_loop_handler.job_executor.spawn(
            job_name,
            async move {
                match fut.await {
                    ok @ Ok(_) => {
                        _ = sender.send(Ok(()));
                        ok
                    }
                    Err(err) => {
                        _ = sender.send(Err(err.clone()));
                        Err(err)
                    }
                }
            },
            backend.capabilities().is_async.into(),
        );
        let job_id = handle.job_id;
        self.active_jobs.insert(
            job_id,
            JobRequest::Contacts(ContactJobRequest::DeleteCard {
                backend_id,
                book,
                handle,
                card_id,
            }),
        );
        self.active_job_instants
            .insert(std::time::Instant::now(), job_id);
        self.main_loop_handler
            .send(ThreadEvent::UIEvent(UIEvent::StatusEvent(
                StatusEvent::NewJob(job_id),
            )));
        Ok((job_id, receiver))
    }
}

pub enum ContactJobRequest {
    Initialize {
        backend_id: Arc<ContactBackendID>,
        handle: JoinHandle<Result<Box<dyn melib::contacts::backend::ContactBackend>>>,
    },
    AddressBooks {
        backend_id: Arc<ContactBackendID>,
        handle: JoinHandle<Result<Vec<AddressBookName>>>,
    },
    Cards {
        backend_id: Arc<ContactBackendID>,
        book: AddressBookName,
        handle: JoinHandle<Result<AddressBook>>,
    },
    Edit {
        backend_id: Arc<ContactBackendID>,
        book: AddressBookName,
        card_id: CardId,
        create: bool,
        handle: JoinHandle<Result<Card>>,
    },
    DeleteCard {
        backend_id: Arc<ContactBackendID>,
        book: AddressBookName,
        card_id: CardId,
        handle: JoinHandle<Result<()>>,
    },
}

impl std::fmt::Debug for ContactJobRequest {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Initialize { backend_id, .. } => fmt
                .debug_struct("ContactJobRequest::Initialize")
                .field("backend_id", backend_id)
                .finish_non_exhaustive(),
            Self::AddressBooks { backend_id, .. } => fmt
                .debug_struct("ContactJobRequest::AddressBooks")
                .field("backend_id", backend_id)
                .finish_non_exhaustive(),
            Self::Cards { backend_id, .. } => fmt
                .debug_struct("ContactJobRequest::Cards")
                .field("backend_id", backend_id)
                .finish_non_exhaustive(),
            Self::Edit {
                backend_id,
                book,
                create: true,
                ..
            } => fmt
                .debug_struct("ContactJobRequest::Edit(create)")
                .field("backend_id", backend_id)
                .field("book", book)
                .finish_non_exhaustive(),
            Self::Edit {
                backend_id,
                book,
                create: false,
                card_id,
                ..
            } => fmt
                .debug_struct("ContactJobRequest::Edit")
                .field("backend_id", backend_id)
                .field("book", book)
                .field("card_id", card_id)
                .finish_non_exhaustive(),
            Self::DeleteCard {
                backend_id,
                book,
                card_id,
                ..
            } => fmt
                .debug_struct("ContactJobRequest::DeleteCard")
                .field("backend_id", backend_id)
                .field("book", book)
                .field("card_id", card_id)
                .finish_non_exhaustive(),
        }
    }
}

impl std::fmt::Display for ContactJobRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Initialize { .. } => write!(f, "Initialize"),
            Self::AddressBooks { .. } => write!(f, "Get address books"),
            Self::Cards { .. } => write!(f, "Get cards"),
            Self::Edit { create: true, .. } => write!(f, "Add card"),
            Self::Edit { .. } => write!(f, "Edit card"),
            Self::DeleteCard { .. } => write!(f, "Delete card"),
        }
    }
}

impl ContactJobRequest {
    pub fn cancel(&self) -> Option<StatusEvent> {
        match self {
            Self::Initialize { handle, .. } => handle.cancel(),
            Self::AddressBooks { handle, .. } => handle.cancel(),
            Self::Cards { handle, .. } => handle.cancel(),
            Self::Edit { handle, .. } => handle.cancel(),
            Self::DeleteCard { handle, .. } => handle.cancel(),
        }
    }
}

#[derive(Debug)]
struct DefaultContacts {
    cards: IndexMap<CardId, Card>,
}

impl DefaultContacts {
    const DEFAULT_ADDRESS_BOOK: &str = "default";
    const NAME: &str = "meli";
    const FORMAT: &str = "JSContactV1";

    fn address_books(&self) -> Vec<AddressBookName> {
        vec![AddressBookName(
            Self::DEFAULT_ADDRESS_BOOK.to_string().into(),
        )]
    }

    fn fetch_default_book(&self) -> AddressBook {
        AddressBook {
            name: Self::DEFAULT_ADDRESS_BOOK.into(),
            read_only: false,
            cards: self.cards.clone(),
        }
    }
}

impl ContactBackend for DefaultContacts {
    fn capabilities(&mut self) -> ContactBackendCapabilities {
        ContactBackendCapabilities {
            is_async: true,
            is_remote: false,
            ..ContactBackendCapabilities::default()
        }
    }

    fn address_books(&mut self) -> ResultFuture<Vec<AddressBookName>> {
        let address_books = Self::address_books(self);
        Ok(Box::pin(async { Ok(address_books) }))
    }

    fn fetch_book(&mut self, address_book: &AddressBookName) -> ResultFuture<AddressBook> {
        if address_book.0.as_ref() != Self::DEFAULT_ADDRESS_BOOK {
            return Err(Error::new("").set_kind(ErrorKind::ValueError));
        }
        let book = Self::fetch_default_book(self);

        Ok(Box::pin(async move { Ok(book) }))
    }

    fn search(
        &self,
        _term: &str,
        _address_book: Option<&AddressBookName>,
    ) -> ResultFuture<Vec<Card>> {
        Err(Error::new("").set_kind(ErrorKind::NotSupported))
    }

    fn add(&mut self, address_book: &AddressBookName, new_card: Card) -> ResultFuture<Card> {
        if address_book.0.as_ref() != Self::DEFAULT_ADDRESS_BOOK {
            return Err(Error::new("").set_kind(ErrorKind::ValueError));
        }
        self.cards.insert(new_card.id(), new_card.clone());
        Ok(Box::pin(async move { Ok(new_card) }))
    }

    fn edit(&mut self, address_book: &AddressBookName, card: Card) -> ResultFuture<Card> {
        self.add(address_book, card)
    }

    fn delete(&mut self, address_book: &AddressBookName, card: Card) -> ResultFuture<()> {
        if address_book.0.as_ref() != Self::DEFAULT_ADDRESS_BOOK {
            return Err(Error::new("").set_kind(ErrorKind::ValueError));
        }
        let ret = if self.cards.shift_remove(&(CardId::from(card.uid))).is_some() {
            Ok(())
        } else {
            Err(Error::new("").set_kind(ErrorKind::NotSupported))
        };

        Ok(Box::pin(async move { ret }))
    }
}
