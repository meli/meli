// Copyright (c) 2014-2018 Markus Unterwaditzer & contributors
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use std::{borrow::ToOwned, sync::mpsc::channel, time::Duration};

use chrono::{NaiveDate, NaiveDateTime};

use super::{
    component::{fold_line, write_component},
    icalendar::{ICalendar, *},
    parse_component,
    parser::{ParseErrorReason, Parser},
    util::*,
    vcard::Vcard,
};

macro_rules! s(
    ($i:expr) => ($i.to_owned());
);

const VCARD_BASIC: &str = concat!(
    "BEGIN:VCARD\r\n",
    "VERSION:2.1\r\n",
    "N:Mustermann;Erika\r\n",
    "FN:Erika Mustermann\r\n",
    "ORG:Wikipedia\r\n",
    "TITLE:Oberleutnant\r\n",
    "PHOTO;JPEG:http://commons.wikimedia.org/wiki/File:Erika_Mustermann_2010.jpg\r\n",
    "TEL;WORK;VOICE:(0221) 9999123\r\n",
    "TEL;HOME;VOICE:(0221) 1234567\r\n",
    "ADR;HOME:;;Heidestrasse 17;Koeln;;51147;Deutschland\r\n",
    "EMAIL;PREF;INTERNET:erika@mustermann.de\r\n",
    "REV:20140301T221110Z\r\n",
    "END:VCARD\r\n",
);

const VCALENDAR_TEST_ENTRY: &str = concat!(
    "BEGIN:VCALENDAR\r\n",
    "VERSION:2.0\r\n",
    "PRODID:http://www.example.com/calendarapplication/\r\n",
    "METHOD:PUBLISH\r\n",
    "BEGIN:VEVENT\r\n",
    "UID:461092315540@example.com\r\n",
    "ORGANIZER;CN=\"Alice Balder, Example Inc.\":MAILTO:alice@example.com\r\n",
    "LOCATION:Somewhere\r\n",
    "SUMMARY:Eine Kurzinfo\r\n",
    "DESCRIPTION:Beschreibung des Termines\r\n",
    "CLASS:PUBLIC\r\n",
    "DTSTART:20060910T220000Z\r\n",
    "DTEND:20060919T215900Z\r\n",
    "DTSTAMP:20060812T125900Z\r\n",
    "END:VEVENT\r\n",
    "END:VCALENDAR\r\n"
);

const VCALENDAR_TEST_ENTRY_OC: &str = concat!(
    "BEGIN:VCALENDAR\r\n",
    "VERSION:2.0\r\n",
    "PRODID:ownCloud Calendar\r\n",
    "CALSCALE:GREGORIAN\r\n",
    "BEGIN:VEVENT\r\n",
    "UID:ff411055a5\r\n",
    "DTSTAMP:20160128T223013Z\r\n",
    "CREATED:20160128T223013Z\r\n",
    "LAST-MODIFIED:20160128T223013Z\r\n",
    "SUMMARY:Amon Amarth - Jomsviking\r\n",
    "DTSTART;VALUE=DATE:20160325\r\n",
    "DTEND;VALUE=DATE:20160326\r\n",
    "LOCATION:\r\n",
    "DESCRIPTION:\r\n",
    "CATEGORIES:\r\n",
    "END:VEVENT\r\n",
    "END:VCALENDAR\r\n"
);

#[test]
fn test_vobject_vcard_basic() {
    let item = parse_component(VCARD_BASIC).unwrap();

    assert_eq!(
        item.get_only("FN").unwrap().raw_value,
        s!("Erika Mustermann")
    );
    assert_eq!(
        item.get_only("N").unwrap().raw_value,
        s!("Mustermann;Erika")
    );

    let mut tel_values = item.get_all("TEL").iter().map(|x| &x.raw_value[..]);
    assert_eq!(tel_values.next().unwrap(), s!("(0221) 9999123"));
    assert_eq!(tel_values.next().unwrap(), s!("(0221) 1234567"));
    assert!(tel_values.next().is_none());
}

#[test]
fn test_vobject_line_cont() {
    let item = parse_component(
        "BEGIN:VCARD\nVERSION:2.1\nN;ENCODING=QUOTED-PRINTABLE:Nikdo;Nikdo=\n\tvic\nFN;\
         ENCODING=QUOTED-PRINT\n ABLE:Alice;Alice=vic\nNOTE:This ends with equal \
         sign=\nTEL;WORK:5555\n 4444\nEND:VCARD",
    )
    .unwrap();

    assert_eq!(item.name, s!("VCARD"));
    assert_eq!(item.get_only("TEL").unwrap().raw_value, s!("55554444"));
    assert_eq!(item.get_only("N").unwrap().raw_value, s!("Nikdo;Nikdo=vic"));
    assert_eq!(
        item.get_only("FN").unwrap().raw_value,
        s!("Alice;Alice=vic")
    );
}

#[test]
fn test_vobject_icalendar_basic() {
    let item = parse_component(VCALENDAR_TEST_ENTRY).unwrap();

    assert_eq!(item.name, s!("VCALENDAR"));
    assert!(item.get_only("LOCATION").is_none());
    assert!(item.get_only("ORGANIZER").is_none());

    let event = &item.subcomponents[0];
    assert_eq!(event.name, s!("VEVENT"));
    assert!(event.get_only("ORGANIZER").is_some());
    assert_eq!(
        event.get_only("LOCATION").unwrap().raw_value,
        s!("Somewhere")
    );
}

#[test]
fn test_vobject_icalendar_multline() {
    // Adapted from a very popular provider's export
    // this used to give ParseError { desc: "Expected :, found \n" }
    let event = parse_component(
        "BEGIN:VEVENT\nATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Jo\n \
         hn Doe;X-NUM-GUESTS=0:mailto:jd@cal.test\nSUMMARY:Important meeting\nEND:VEVENT\n",
    )
    .unwrap();

    assert_eq!(event.name, s!("VEVENT"));
    assert_eq!(
        event.get_only("SUMMARY").unwrap().raw_value,
        s!("Important meeting")
    );
}

#[test]
fn test_vobject_icalendar_multline2() {
    // Adapted from a very popular provider's export
    // this used to give ParseError { desc: "No property name found." }
    let event = parse_component(
        "BEGIN:VCALENDAR\nBEGIN:VEVENT\nATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;\
         PARTSTAT=ACCEPTED;CN=Jo\n hn Doe;X-NUM-GUESTS=0:mailto:jd@cal.test\nSUMMARY:Important \
         meeting\nEND:VEVENT\nEND:VCALENDAR\n",
    )
    .unwrap();

    assert_eq!(event.name, s!("VCALENDAR"));
}

#[test]
fn test_vobject_escaping() {
    let item = parse_component(
        "BEGIN:VCALENDAR\nORGANIZER;CN=\"Cott:n Eye Joe\":mailto:joe@joe.com\nEND:VCALENDAR\n",
    )
    .unwrap();
    assert_eq!(item.name, s!("VCALENDAR"));
    assert_eq!(
        item.get_only("ORGANIZER").unwrap().raw_value,
        s!("mailto:joe@joe.com")
    );
}

#[test]
fn test_vobject_property_groups() {
    let item = parse_component(
        "BEGIN:VCARD\nfoo.EMAIL;TYPE=INTERNET:foo@example.com\nfoo.X-ACTUAL-TYPE:CUSTOM\nEND:\
         VCARD\n",
    )
    .unwrap();
    assert_eq!(
        item.get_only("EMAIL").unwrap().prop_group,
        Some("foo".to_owned())
    );
}

#[test]
fn test_vobject_unfold1() {
    let mut p = Parser {
        input: "ab\r\n c",
        pos: 2,
    };
    assert_eq!(p.consume_char(), Some('c'));
    assert_eq!(p.pos, 6);
}

#[test]
fn test_vobject_unfold2() {
    let mut p = Parser {
        input: "ab\n\tc\nx",
        pos: 2,
    };
    assert_eq!(p.consume_char(), Some('c'));
    assert_eq!(p.consume_char(), Some('\n'));
    assert_eq!(p.consume_char(), Some('x'));
}

#[test]
fn test_vobject_consume_while() {
    let mut p = Parser {
        input: "af\n oo:bar",
        pos: 1,
    };
    assert_eq!(p.consume_while(|x| x != ':'), "foo");
    assert_eq!(p.consume_char(), Some(':'));
    assert_eq!(p.consume_while(|x| x != '\n'), "bar");
}

#[test]
fn test_vobject_consume_while2() {
    let mut p = Parser {
        input: "af\n oo\n\t:bar",
        pos: 1,
    };
    assert_eq!(p.consume_while(|x| x != ':'), "foo");
    assert_eq!(p.consume_char(), Some(':'));
    assert_eq!(p.consume_while(|x| x != '\n'), "bar");
}

#[test]
fn test_vobject_consume_while3() {
    let mut p = Parser {
        input: "af\n oo:\n bar",
        pos: 1,
    };
    assert_eq!(p.consume_while(|x| x != ':'), "foo");
    assert_eq!(p.consume_char(), Some(':'));
    assert_eq!(p.consume_while(|x| x != '\n'), "bar");
}

#[test]
fn test_vobject_consume_only_char() {
    let mut p = Parser {
        input: "\n \"bar",
        pos: 0,
    };
    assert!(p.consume_only_char('"'));
    assert_eq!(p.pos, 3);
    assert!(!p.consume_only_char('"'));
    assert_eq!(p.pos, 3);
    assert!(p.consume_only_char('b'));
    assert_eq!(p.pos, 4);
}

#[test]
fn test_vobject_mismatched_begin_end_tags_returns_error() {
    // Test for infinite loops as well
    let mut p = Parser {
        input: "BEGIN:a\nBEGIN:b\nEND:a",
        pos: 0,
    };

    let (tx, rx) = channel();
    ::std::thread::spawn(move || tx.send(p.consume_component()));

    let result = rx.recv_timeout(Duration::from_millis(50)).unwrap();
    if let Err(ParseErrorReason::MismatchedTag(begin, end)) = &result {
        assert_eq!((begin.as_str(), end.as_str()), ("b", "a"));
    } else {
        panic!("Got unexpected result {result:?}");
    }
}

#[test]
fn test_fold() {
    let line = "This should be multiple lines and fold on char boundaries. \
                毎害止加食下組多地将写館来局必第。東証細再記得玲祉込吉宣会法授";
    let expected = "This should be multiple lines and fold on char boundaries. 毎害止加食\r\n \
                    下組多地将写館来局必第。東証細再記得玲祉込吉宣会法\r\n 授";
    assert_eq!(expected, fold_line(line));
    assert_eq!("ab", fold_line("ab"));
}

#[test]
fn test_vcard_basic() {
    let item = Vcard::build(VCARD_BASIC).unwrap();

    assert_eq!(
        item.adr()[0].raw(),
        ";;Heidestrasse 17;Koeln;;51147;Deutschland"
    );
    assert_eq!(item.fullname()[0].raw(), "Erika Mustermann");
    assert_eq!(item.name().unwrap().plain(), "Mustermann;Erika");
    assert_eq!(item.name().unwrap().surname().unwrap(), "Mustermann");
    assert_eq!(item.name().unwrap().given_name().unwrap(), "Erika");
    assert_eq!(item.org()[0].raw(), "Wikipedia");
    assert_eq!(item.title()[0].raw(), "Oberleutnant");
}

#[test]
fn test_vcard_builder() {
    let build = Vcard::builder()
        .with_name(
            indexmap::indexmap! {},
            None,
            Some("Mustermann".into()),
            None,
            Some("Erika".into()),
            None,
        )
        .with_fullname("Erika Mustermann".into())
        .with_org(vec!["Wikipedia".into()])
        .with_title("Oberleutnant".into())
        .with_tel(
            indexmap::indexmap!("TYPE".to_string() => "WORK".to_string()),
            "(0221) 9999123".into(),
        )
        .with_tel(
            indexmap::indexmap!("TYPE".to_string() => "HOME".to_string()),
            "(0221) 1234567".into(),
        )
        .with_adr(
            indexmap::indexmap!("TYPE".to_string() => "HOME".to_string()),
            None,
            None,
            Some("Heidestrasse 17".into()),
            Some("Koeln".into()),
            None,
            Some("51147".into()),
            Some("Deutschland".into()),
        )
        .with_email("erika@mustermann.de".into())
        .with_rev("20140301T221110Z".into())
        .build()
        .unwrap();

    let build_string = write_component(&build);

    assert_eq!(
        concat!(
            "BEGIN:VCARD\r\n",
            "VERSION:4.0\r\n",
            "N:;Mustermann;;Erika;\r\n",
            "FN:Erika Mustermann\r\n",
            "ORG:Wikipedia\r\n",
            "TITLE:Oberleutnant\r\n",
            "TEL;TYPE=WORK:(0221) 9999123\r\n",
            "TEL;TYPE=HOME:(0221) 1234567\r\n",
            "ADR;TYPE=HOME:;;Heidestrasse 17;Koeln;;51147;Deutschland\r\n",
            "EMAIL:erika@mustermann.de\r\n",
            "REV:20140301T221110Z\r\n",
            "END:VCARD\r\n",
        ),
        build_string,
    );
}

#[test]
fn test_ical_parse() {
    let cal = ICalendar::build(VCALENDAR_TEST_ENTRY);
    assert!(
        cal.is_ok(),
        "Not okay: {cal:?}\n in '{VCALENDAR_TEST_ENTRY}'"
    );
}

#[test]
fn test_ical_iter() {
    let ical = ICalendar::build(VCALENDAR_TEST_ENTRY).unwrap();
    assert_eq!(ical.events().count(), 1);
}

#[test]
fn test_ical_icalendar_attributes() {
    let ical = ICalendar::build(VCALENDAR_TEST_ENTRY).unwrap();
    assert_eq!(ical.version().unwrap().raw(), "2.0");
    assert_eq!(
        ical.prodid().unwrap().raw(),
        "http://www.example.com/calendarapplication/"
    );
}

#[test]
fn test_ical_event_attributes() {
    let ical = ICalendar::build(VCALENDAR_TEST_ENTRY).unwrap();
    let ev = ical.events().next().unwrap().unwrap();
    assert_eq!(
        ev.dtend().map(|e| e.raw().clone()),
        Some("20060919T215900Z".to_owned())
    );
    assert_eq!(
        ev.dtstart().map(|e| e.raw().clone()),
        Some("20060910T220000Z".to_owned())
    );
    assert_eq!(
        ev.dtstamp().map(|e| e.raw().clone()),
        Some("20060812T125900Z".to_owned())
    );
    assert_eq!(
        ev.uid().map(|e| e.raw().clone()),
        Some("461092315540@example.com".to_owned())
    );
    assert_eq!(
        ev.description().map(|e| e.raw().clone()),
        Some("Beschreibung des Termines".to_owned())
    );
    assert_eq!(
        ev.summary().map(|e| e.raw().clone()),
        Some("Eine Kurzinfo".to_owned())
    );
    assert_eq!(ev.url(), None);
    assert_eq!(
        ev.location().map(|e| e.raw().clone()),
        Some("Somewhere".to_owned())
    );
    assert_eq!(
        ev.class().map(|e| e.raw().clone()),
        Some("PUBLIC".to_owned())
    );
    assert_eq!(ev.categories(), None);
    assert_eq!(ev.transp(), None);
    assert_eq!(ev.rrule(), None);
}

#[test]
fn test_ical_event_attributes_oc() {
    let ical = ICalendar::build(VCALENDAR_TEST_ENTRY_OC).unwrap();
    assert_eq!(ical.version().unwrap().raw(), "2.0");
    assert_eq!(ical.prodid().unwrap().raw(), "ownCloud Calendar");
    let ev = ical.events().next().unwrap().unwrap();
    assert_eq!(
        ev.dtend().map(|e| e.raw().clone()),
        Some("20160326".to_owned())
    );
    assert_eq!(
        ev.dtstart().map(|e| e.raw().clone()),
        Some("20160325".to_owned())
    );
    assert_eq!(
        ev.dtstamp().map(|e| e.raw().clone()),
        Some("20160128T223013Z".to_owned())
    );
    assert_eq!(
        ev.uid().map(|e| e.raw().clone()),
        Some("ff411055a5".to_owned())
    );
    assert_eq!(
        ev.description().map(|e| e.raw().clone()),
        Some("".to_owned())
    );
    assert_eq!(
        ev.summary().map(|e| e.raw().clone()),
        Some("Amon Amarth - Jomsviking".to_owned())
    );
    assert_eq!(ev.url(), None);
    assert_eq!(ev.location().map(|e| e.raw().clone()), Some("".to_owned()));
    assert_eq!(ev.class().map(|e| e.raw().clone()), None);
    assert_eq!(
        ev.categories().map(|e| e.raw().clone()),
        Some("".to_owned())
    );
    assert_eq!(ev.transp(), None);
    assert_eq!(ev.rrule(), None);
}

#[test]
fn test_ical_event_attributes_with_conversions() {
    let ical = ICalendar::build(VCALENDAR_TEST_ENTRY).unwrap();
    let ev = ical.events().next().unwrap().unwrap();
    assert_eq!(
        ev.dtend().map(|e| e.as_datetime().unwrap()).unwrap(),
        Time::DateTime(NaiveDateTime::parse_from_str("20060919T215900Z", DATE_TIME_FMT).unwrap())
    );
    assert_eq!(
        ev.dtstart().map(|e| e.as_datetime().unwrap()).unwrap(),
        Time::DateTime(NaiveDateTime::parse_from_str("20060910T220000Z", DATE_TIME_FMT).unwrap())
    );
    assert_eq!(
        ev.dtstamp().map(|e| e.as_datetime().unwrap()).unwrap(),
        Time::DateTime(NaiveDateTime::parse_from_str("20060812T125900Z", DATE_TIME_FMT).unwrap())
    );
}

#[test]
fn test_ical_event_attributes_oc_with_conversions() {
    let ical = ICalendar::build(VCALENDAR_TEST_ENTRY_OC).unwrap();
    assert_eq!(ical.version().unwrap().raw(), "2.0");
    assert_eq!(ical.prodid().unwrap().raw(), "ownCloud Calendar");
    let ev = ical.events().next().unwrap().unwrap();
    assert_eq!(
        ev.dtend().map(|e| e.as_datetime().unwrap()).unwrap(),
        Time::Date(NaiveDate::parse_from_str("20160326", DATE_FMT).unwrap())
    );
    assert_eq!(
        ev.dtstart().map(|e| e.as_datetime().unwrap()).unwrap(),
        Time::Date(NaiveDate::parse_from_str("20160325", DATE_FMT).unwrap())
    );
    assert_eq!(
        ev.dtstamp().map(|e| e.as_datetime().unwrap()).unwrap(),
        Time::DateTime(NaiveDateTime::parse_from_str("20160128T223013Z", DATE_TIME_FMT).unwrap())
    );
}

#[test]
fn test_ical_build_event() {
    let mut ical = ICalendar::empty();
    let mut builder = Event::build();

    let desc = Description::new(String::from("test"), ::indexmap::IndexMap::new());
    builder.set_description(desc, None);

    let uid = Uid::new(String::from("testuid"), ::indexmap::IndexMap::new());
    builder.set_uid(uid, None);

    let summary = Summary::new(String::from("summary"), ::indexmap::IndexMap::new());
    builder.set_summary(summary, None);

    ical.add_event(builder);

    let ev = ical.events().next().unwrap().unwrap();
    assert_eq!(
        ev.uid().map(|e| e.raw().clone()),
        Some("testuid".to_owned())
    );
    assert_eq!(
        ev.description().map(|e| e.raw().clone()),
        Some("test".to_owned())
    );
    assert_eq!(
        ev.summary().map(|e| e.raw().clone()),
        Some("summary".to_owned())
    );
}
