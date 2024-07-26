//
// meli
//
// Copyright 2017- Emmanouil Pitsidianakis <manos@pitsidianak.is>
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

use super::*;

#[test]
fn test_command_parser() {
    let mut input = "sort".to_string();
    macro_rules! match_input {
        ($input:expr) => {{
            let mut sugg: HashSet<String> = Default::default();
            //print!("{}", $input);
            for (_tags, _desc, tokens, _) in COMMAND_COMPLETION.iter() {
                //    //println!("{:?}, {:?}, {:?}", _tags, _desc, tokens);
                let _ = tokens.matches(&mut $input.as_str(), &mut sugg);
                //    if !m.is_empty() {
                //        //print!("{:?} ", desc);
                //        //println!(" result = {:#?}\n\n", m);
                //    }
            }
            //println!("suggestions = {:#?}", sugg);
            sugg.into_iter()
                .map(|s| format!("{}{}", $input.as_str(), s.as_str()))
                .collect::<HashSet<String>>()
        }};
    }
    assert_eq!(
        &match_input!(input),
        &IntoIterator::into_iter(["sort date".to_string(), "sort subject".to_string()]).collect(),
    );
    input = "so".to_string();
    assert_eq!(
        &match_input!(input),
        &IntoIterator::into_iter(["sort".to_string()]).collect(),
    );
    input = "so ".to_string();
    assert_eq!(&match_input!(input), &HashSet::default(),);
    input = "to".to_string();
    assert_eq!(
        &match_input!(input),
        &IntoIterator::into_iter(["toggle".to_string()]).collect(),
    );
    input = "toggle ".to_string();
    assert_eq!(
        &match_input!(input),
        &IntoIterator::into_iter([
            "toggle mouse".to_string(),
            "toggle sign".to_string(),
            "toggle encrypt".to_string(),
            "toggle thread_snooze".to_string()
        ])
        .collect(),
    );
}

#[test]
#[ignore]
fn test_command_parser_interactive() {
    use std::io;
    let mut input = String::new();
    loop {
        input.clear();
        print!("> ");
        match io::stdin().read_line(&mut input) {
            Ok(_n) => {
                println!("Input is {:?}", input.as_str().trim());
                let mut sugg: HashSet<String> = Default::default();
                let mut vec = vec![];
                //print!("{}", input);
                for (_tags, _desc, tokens, _) in COMMAND_COMPLETION.iter() {
                    //println!("{:?}, {:?}, {:?}", _tags, _desc, tokens);
                    let m = tokens.matches(&mut input.as_str().trim(), &mut sugg);
                    if !m.is_empty() {
                        vec.push(tokens);
                        //print!("{:?} ", desc);
                        //println!(" result = {:#?}\n\n", m);
                    }
                }
                println!(
                    "suggestions = {:#?}",
                    sugg.into_iter()
                        .zip(vec.into_iter())
                        .map(|(s, v)| format!(
                            "{}{} {:?}",
                            input.as_str().trim(),
                            if input.trim().is_empty() {
                                s.trim()
                            } else {
                                s.as_str()
                            },
                            v
                        ))
                        .collect::<Vec<String>>()
                );
                if input.trim() == "quit" {
                    break;
                }
            }
            Err(error) => println!("error: {}", error),
        }
    }
    println!("alright");
}

#[test]
fn test_command_parser_all() {
    use CommandError::*;

    for cmd in [
        "set unseen",
        "set seen",
        "delete",
        "copyto somewhere",
        "moveto somewhere",
        "import fpath mpath",
        "close  ",
        "go 5",
    ] {
        parse_command(cmd.as_bytes()).unwrap_or_else(|err| panic!("{} failed {}", cmd, err));
    }

    assert_eq!(
        parse_command(b"setfafsfoo").unwrap_err().to_string(),
        Parsing {
            inner: "setfafsfoo".into(),
            kind: "".into(),
        }
        .to_string(),
    );
    assert_eq!(
        parse_command(b"set foo").unwrap_err().to_string(),
        BadValue {
            inner: "foo".into(),
            suggestions: Some(&[
                "seen",
                "unseen",
                "plain",
                "threaded",
                "compact",
                "conversations"
            ])
        }
        .to_string(),
    );
    assert_eq!(
        parse_command(b"moveto ").unwrap_err().to_string(),
        WrongNumberOfArguments {
            too_many: false,
            takes: (1, Some(1)),
            given: 0,
            __func__: "moveto",
            inner: "".into(),
        }
        .to_string(),
    );
    assert_eq!(
        parse_command(b"reindex 1 2 3").unwrap_err().to_string(),
        WrongNumberOfArguments {
            too_many: true,
            takes: (1, Some(1)),
            given: 2,
            __func__: "reindex",
            inner: "".into(),
        }
        .to_string(),
    );
}

#[test]
fn test_command_error_display() {
    assert_eq!(
        &CommandError::BadValue {
            inner: "foo".into(),
            suggestions: Some(&[
                "seen",
                "unseen",
                "plain",
                "threaded",
                "compact",
                "conversations"
            ])
        }
        .to_string(),
        "Bad value/argument: foo. Possible values are: seen, unseen, plain, threaded, compact, \
         conversations"
    );
}
