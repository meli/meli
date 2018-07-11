/*
 * meli - bin.rs
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

mod ui;
use ui::*;

extern crate melib;
extern crate termion;
use melib::*;

use std::sync::mpsc::{sync_channel, SyncSender, Receiver};
use std::thread;
use std::io::{stdout, stdin, };

fn main() {
    /* Lock all stdios */
    let _stdout = stdout();
    let mut _stdout = _stdout.lock();
    let stdin = stdin();
    let stdin = stdin;
    /*
    let _stderr = stderr();
    let mut _stderr = _stderr.lock();
    */


    let set = Settings::new();
    let backends = Backends::new();

    let (sender, receiver): (SyncSender<ThreadEvent>, Receiver<ThreadEvent>) = sync_channel(::std::mem::size_of::<ThreadEvent>());
    {
        let sender = sender.clone();
        thread::Builder::new().name("input-thread".to_string()).spawn(move || {
            get_events(stdin, |k| { sender.send(ThreadEvent::Input(k)).unwrap(); })
                                }).unwrap();
    }

    //let mailbox = Mailbox::new("/home/epilys/Downloads/rust/nutt/Inbox4");
    let mut j = 0;
    let folder_length = set.accounts["norn"].folders.len();
    let mut account = Account::new("norn".to_string(), set.accounts["norn"].clone(), backends);
    {
        let sender = sender.clone();
        account.watch(RefreshEventConsumer::new(Box::new(move |r| {
            sender.send(ThreadEvent::from(r)).unwrap();
        })));
    }




    let mut state = State::new(_stdout);

    let a = Entity {component: Box::new(TextBox::new("a text box".to_string())) };
    let listing = MailListing::new(Mailbox::new_dummy());
    let b = Entity { component: Box::new(listing) };
    let window  = Entity { component: Box::new(VSplit::new(a,b,90)) };
    state.register_entity(window);
    state.render();
    'main: loop {
        let mailbox = &mut account[j];
        //let mut index: Box<Window> = match *mailbox.as_ref().unwrap() {
        //    Ok(ref v) => Box::new(Index::new(v)),
        //    Err(ref v) => Box::new(ErrorWindow::new((*v).clone())),
        //};
        ////eprintln!("{:?}", set);
        match *mailbox.as_ref().unwrap() {
            Ok(ref v) => {
                state.rcv_event(UIEvent { id: 0, event_type: UIEventType::RefreshMailbox(v.clone()) });
            },
            Err(_) => {},
        };

        //index.draw();
        //
        state.render();

        'inner: loop {
            match receiver.recv().unwrap() {
                ThreadEvent::Input(k) => {
                    match k {
                        key @ Key::Char('j') | key @ Key::Char('k') => {
                           state.rcv_event(UIEvent { id: 0, event_type: UIEventType::Input(key)});
                           state.render();
                        }, 
                        key @ Key::Up | key @ Key::Down => {
                           state.rcv_event(UIEvent { id: 0, event_type: UIEventType::Input(key)});
                           state.render();
                            }
                        Key::Char('\n') => {
         //                   index.handle_input(k);
                           state.rcv_event(UIEvent { id: 0, event_type: UIEventType::Input(Key::Char('\n'))});
                           state.render();
                        }
                        Key::Char('i') | Key::Esc => {
                           state.rcv_event(UIEvent { id: 0, event_type: UIEventType::Input(Key::Esc)});
                           state.render();
                        }
                        Key::F(_) => {
          //                  if !index.handle_input(k) {
           //                     break 'main;
            //                }
                        },
                        Key::Char('q') | Key::Char('Q') => {
                            break 'main;
                        },
                        Key::Char('J') => if j < folder_length - 1 {
                            j += 1;
                            break 'inner;
                        },
                        Key::Char('K') => if j > 0 {
                            j -= 1;
                            break 'inner;
                        },
                        _ => {}
                    }
                },
                ThreadEvent::RefreshMailbox { name : n } => {
                    eprintln!("Refresh mailbox {}", n);
                },
            }
        }
    }
}
