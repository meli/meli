extern crate melt_ui;
extern crate termion;

use melt_ui::{State, Entity, BoxPanel, HSplit, VSplit, TextBox, MailListing};
use termion::raw::IntoRawMode;

use std::io::{stdout, stdin, stderr};
use std::sync::mpsc::{sync_channel, SyncSender, Receiver};
use std::thread;

fn main() {
    /* Lock all stdios */
    let _stdout = stdout();
    let mut _stdout = _stdout.lock();
    let stdin = stdin();
    let stdin = stdin.lock();
    let _stderr = stderr();
    let mut _stderr = _stderr.lock();
    let mut s = State::new(_stdout.into_raw_mode().unwrap(), stdin);
    //s.hello_w();
    
  //  let ent = Entity { width: 30, height: 30, margin_top: 0, margin_left: 0, component: Box::new(BoxPanel{}) };
  //  s.register_entity(ent);

    let a = Entity {component: Box::new(TextBox::new("a text box".to_string())) };
    //let b = Entity { component: Box::new(TextBox::new("b text box".to_string())) };
    let l = Entity { component: Box::new(TextBox::new("left text box".to_string())) };
    let r = Entity { component: Box::new(TextBox::new("right text box".to_string())) };
    let b = Entity { component: Box::new(VSplit::new(l,r,50)) };
    let top = Entity { component: Box::new(HSplit::new(a, b, 70)) };
    //let bottom = Entity { component: Box::new(TextBox::new("hello world.".to_string())) };
    let bottom = Entity { component: Box::new(MailListing::new(200)) };
    let ent = Entity { component: Box::new(HSplit::new(top, bottom, 50)) };
  //
    s.register_entity(ent);
    s.render();



}
