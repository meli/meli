/*
 * meli - addressbook module
 *
 * Copyright 2019 Manos Pitsidianakis
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

/// Convert VCard strings to meli Cards (contacts).
use super::*;
use crate::chrono::TimeZone;
use crate::error::{MeliError, Result};
use fnv::FnvHashMap;

/* Supported vcard versions */
pub trait VCardVersion: core::fmt::Debug {}

/// https://tools.ietf.org/html/rfc6350
#[derive(Debug)]
pub struct VCardVersion4;
impl VCardVersion for VCardVersion4 {}

/// https://tools.ietf.org/html/rfc2426
#[derive(Debug)]
pub struct VCardVersion3;
impl VCardVersion for VCardVersion3 {}

pub struct CardDeserializer;

static HEADER: &'static str = "BEGIN:VCARD\r\nVERSION:4.0\r\n";
static FOOTER: &'static str = "END:VCARD\r\n";

#[derive(Debug)]
pub struct VCard<T: VCardVersion>(
    fnv::FnvHashMap<String, ContentLine>,
    std::marker::PhantomData<*const T>,
);

impl<V: VCardVersion> VCard<V> {
    pub fn new_v4() -> VCard<impl VCardVersion> {
        VCard(
            FnvHashMap::default(),
            std::marker::PhantomData::<*const VCardVersion4>,
        )
    }
}

#[derive(Debug, Default, Clone)]
pub struct ContentLine {
    group: Option<String>,
    params: Vec<String>,
    value: String,
}

impl CardDeserializer {
    pub fn from_str(mut input: &str) -> Result<VCard<impl VCardVersion>> {
        input = if !input.starts_with(HEADER) || !input.ends_with(FOOTER) {
            return Err(MeliError::new(format!("Error while parsing vcard: input does not start or end with correct header and footer. input is:\n{:?}", input)));
        } else {
            &input[HEADER.len()..input.len() - FOOTER.len()]
        };

        let mut ret = FnvHashMap::default();

        enum Stage {
            Group,
            Name,
            Param,
            Value,
        }
        let mut stage: Stage;

        for l in input.lines() {
            let mut el = ContentLine::default();
            let mut value_start = 0;
            let mut has_colon = false;
            stage = Stage::Group;
            let mut name = String::new();
            for i in 0..l.len() {
                let byte = l.as_bytes()[i];
                match (byte, &stage) {
                    (b'.', Stage::Group) if l.as_bytes()[i] != b'\\' => {
                        el.group = Some(l[value_start..i].to_string());
                        value_start = i + 1;
                        stage = Stage::Name;
                    }
                    (b';', Stage::Group) => {
                        name = l[value_start..i].to_string();
                        value_start = i + 1;
                        stage = Stage::Param;
                    }
                    (b';', Stage::Param) => {
                        el.params.push(l[value_start..i].to_string());
                        value_start = i + 1;
                    }
                    (b':', Stage::Group) | (b':', Stage::Name) => {
                        name = l[value_start..i].to_string();
                        has_colon = true;
                        value_start = i + 1;
                        stage = Stage::Value;
                    }
                    (b':', Stage::Param) if l.as_bytes()[i] != b'\\' => {
                        el.params.push(l[value_start..i].to_string());
                        has_colon = true;
                        value_start = i + 1;
                        stage = Stage::Value;
                    }
                    _ => {}
                }
            }
            el.value = l[value_start..].to_string();
            if !has_colon {
                return Err(MeliError::new(format!(
                    "Error while parsing vcard: error at line {}, no colon. {:?}",
                    l, el
                )));
            }
            if name.is_empty() {
                return Err(MeliError::new(format!(
                    "Error while parsing vcard: error at line {}, no name for content line. {:?}",
                    l, el
                )));
            }
            ret.insert(name, el);
        }
        Ok(VCard(ret, std::marker::PhantomData::<*const VCardVersion4>))
    }
}

impl<V: VCardVersion> std::convert::TryInto<Card> for VCard<V> {
    type Error = crate::error::MeliError;

    fn try_into(mut self) -> crate::error::Result<Card> {
        let mut card = Card::new();
        card.set_id(CardId::Hash({
            use std::hash::Hasher;
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            if let Some(val) = self.0.get("FN") {
                hasher.write(val.value.as_bytes());
            }
            if let Some(val) = self.0.get("N") {
                hasher.write(val.value.as_bytes());
            }
            if let Some(val) = self.0.get("EMAIL") {
                hasher.write(val.value.as_bytes());
            }
            hasher.finish()
        }));
        if let Some(val) = self.0.remove("FN") {
            card.set_name(val.value);
        } else {
            return Err(MeliError::new("FN entry missing in VCard."));
        }
        if let Some(val) = self.0.remove("NICKNAME") {
            card.set_additionalname(val.value);
        }
        if let Some(val) = self.0.remove("BDAY") {
            /* 4.3.4.  DATE-AND-OR-TIME

            Either a DATE-TIME, a DATE, or a TIME value.  To allow unambiguous
            interpretation, a stand-alone TIME value is always preceded by a "T".

            Examples for "date-and-or-time":

                      19961022T140000
                      --1022T1400
                      ---22T14
                      19850412
                      1985-04
                      1985
                      --0412
                      ---12
                      T102200
                      T1022
                      T10
                      T-2200
                      T--00
                      T102200Z
                      T102200-0800
                      */
            card.birthday = chrono::Local.datetime_from_str(&val.value, "%Y%m%d").ok();
        }
        if let Some(val) = self.0.remove("EMAIL") {
            card.set_email(val.value);
        }
        if let Some(val) = self.0.remove("URL") {
            card.set_url(val.value);
        }
        if let Some(val) = self.0.remove("KEY") {
            card.set_key(val.value);
        }
        for (k, v) in self.0.into_iter() {
            card.set_extra_property(&k, v.value);
        }

        Ok(card)
    }
}

#[test]
fn test_card() {
    let j = "BEGIN:VCARD\r\nVERSION:4.0\r\nN:Gump;Forrest;;Mr.;\r\nFN:Forrest Gump\r\nORG:Bubba Gump Shrimp Co.\r\nTITLE:Shrimp Man\r\nPHOTO;MEDIATYPE=image/gif:http://www.example.com/dir_photos/my_photo.gif\r\nTEL;TYPE=work,voice;VALUE=uri:tel:+1-111-555-1212\r\nTEL;TYPE=home,voice;VALUE=uri:tel:+1-404-555-1212\r\nADR;TYPE=WORK;PREF=1;LABEL=\"100 Waters Edge\\nBaytown\\, LA 30314\\nUnited States of America\":;;100 Waters Edge;Baytown;LA;30314;United States of America\r\nADR;TYPE=HOME;LABEL=\"42 Plantation St.\\nBaytown\\, LA 30314\\nUnited States of America\":;;42 Plantation St.;Baytown;LA;30314;United States of America\r\nEMAIL:forrestgump@example.com\r\nREV:20080424T195243Z\r\nx-qq:21588891\r\nEND:VCARD\r\n";
    println!("results = {:#?}", CardDeserializer::from_str(j).unwrap());
}
