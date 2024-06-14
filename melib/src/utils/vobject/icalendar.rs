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

use chrono::{NaiveDate, NaiveDateTime};

use super::{
    component::{parse_component, Component},
    error::*,
    property::Property,
    util::{DATE_FMT, DATE_TIME_FMT},
};

/// An `ICalendar` representing type
#[derive(Debug)]
pub struct ICalendar(Component);

impl ICalendar {
    /// Parse a string to a [`ICalendar`] object
    ///
    /// Returns an error if the parsed text is not a [`ICalendar`] (that means
    /// that an error is returned also if this is a valid Vcard!)
    pub fn build(s: &str) -> Result<Self, VObjectError> {
        let c = parse_component(s)?;
        Self::from_component(c).map_err(|_| VObjectError::NotAnICalendar(s.to_owned()))
    }

    pub fn empty() -> Self {
        let c = Component::new("VCALENDAR");
        Self(c)
    }

    /// Add an event to the calendar
    pub fn add_event(&mut self, builder: EventBuilder) {
        self.0.subcomponents.push(builder.into_component())
    }

    /// Chainable variant of [`ICalendar::add_event`].
    pub fn with_event(mut self, builder: EventBuilder) -> Self {
        self.0.subcomponents.push(builder.into_component());
        self
    }

    /// Wrap a Component into an [`ICalendar`] object, or don't do it if the
    /// Component is not an [`ICalendar`].
    pub fn from_component(c: Component) -> Result<Self, Component> {
        if c.name == "VCALENDAR" {
            Ok(Self(c))
        } else {
            Err(c)
        }
    }

    /// Get an iterator over the events in this calendar
    ///
    /// The iterator creates `Ok(&Event)` instances on the fly, or
    /// `Err(&Component)` instances if the item cannot be parsed as an
    /// `Event`, not forgetting any data.
    ///
    /// # Getting actual objects
    ///
    /// For getting a `Event`-instance iterator from this, one can use this as
    /// follows:
    ///
    /// ```ignore
    /// # use vobject::component::Component;
    /// # use vobject::icalendar::Event;
    /// # use vobject::icalendar::ICalendar;
    /// # let icalendar = ICalendar::from_component(Component {
    /// #     name:          "VCALENDAR".to_owned(),
    /// #     props:         Default::default(),
    /// #     subcomponents: vec![]
    /// # }).unwrap();
    /// icalendar
    ///     .events()
    ///     .filter_map(Result::ok)
    ///     .map(|ev| ev.clone())
    ///     .collect::<Vec<Event>>();
    /// ```
    pub fn events(&self) -> EventIterator<'_> {
        EventIterator::new(self.0.subcomponents.iter())
    }

    make_getter_function_for_optional!(version, "VERSION", Version);
    make_getter_function_for_optional!(prodid, "PRODID", Prodid);
}

create_data_type!(Version);
create_data_type!(Prodid);

pub struct EventIterator<'a>(::std::slice::Iter<'a, Component>);

impl<'a> EventIterator<'a> {
    fn new(i: ::std::slice::Iter<'a, Component>) -> Self {
        Self(i)
    }
}

impl<'a> Iterator for EventIterator<'a> {
    type Item = Result<Event<'a>, &'a Component>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Event::from_component)
    }
}

#[derive(Clone, Debug)]
pub struct Event<'a>(&'a Component);

impl<'a> Event<'a> {
    fn from_component(c: &'a Component) -> Result<Self, &'a Component> {
        if c.name == "VEVENT" {
            Ok(Self(c))
        } else {
            Err(c)
        }
    }

    make_getter_function_for_optional!(dtend, "DTEND", Dtend);
    make_getter_function_for_optional!(dtstart, "DTSTART", Dtstart);
    make_getter_function_for_optional!(dtstamp, "DTSTAMP", Dtstamp);
    make_getter_function_for_optional!(uid, "UID", Uid);
    make_getter_function_for_optional!(description, "DESCRIPTION", Description);
    make_getter_function_for_optional!(summary, "SUMMARY", Summary);
    make_getter_function_for_optional!(url, "URL", Url);
    make_getter_function_for_optional!(location, "LOCATION", Location);
    make_getter_function_for_optional!(class, "CLASS", Class);
    make_getter_function_for_optional!(categories, "CATEGORIES", Categories);
    make_getter_function_for_optional!(transp, "TRANSP", Transp);
    make_getter_function_for_optional!(rrule, "RRULE", Rrule);

    pub fn build() -> EventBuilder {
        EventBuilder(Component::new(String::from("VEVENT")))
    }
}

create_data_type!(Dtend);
create_data_type!(Dtstart);
create_data_type!(Dtstamp);
create_data_type!(Uid);
create_data_type!(Description);
create_data_type!(Summary);
create_data_type!(Url);
create_data_type!(Location);
create_data_type!(Class);
create_data_type!(Categories);
create_data_type!(Transp);
create_data_type!(Rrule);

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Time {
    Date(NaiveDate),
    DateTime(NaiveDateTime),
}

pub trait AsDateTime {
    fn as_datetime(&self) -> Result<Time, VObjectError>;
}

impl AsDateTime for Dtend {
    fn as_datetime(&self) -> Result<Time, VObjectError> {
        Ok(
            match NaiveDateTime::parse_from_str(&self.0, DATE_TIME_FMT) {
                Ok(dt) => Time::DateTime(dt),
                Err(_) => NaiveDate::parse_from_str(&self.0, DATE_FMT).map(Time::Date)?,
            },
        )
    }
}

impl AsDateTime for Dtstart {
    fn as_datetime(&self) -> Result<Time, VObjectError> {
        Ok(
            match NaiveDateTime::parse_from_str(&self.0, DATE_TIME_FMT) {
                Ok(dt) => Time::DateTime(dt),
                Err(_) => NaiveDate::parse_from_str(&self.0, DATE_FMT).map(Time::Date)?,
            },
        )
    }
}

impl AsDateTime for Dtstamp {
    fn as_datetime(&self) -> Result<Time, VObjectError> {
        Ok(
            match NaiveDateTime::parse_from_str(&self.0, DATE_TIME_FMT) {
                Ok(dt) => Time::DateTime(dt),
                Err(_) => NaiveDate::parse_from_str(&self.0, DATE_FMT).map(Time::Date)?,
            },
        )
    }
}

#[derive(Clone, Debug)]
pub struct EventBuilder(Component);

macro_rules! setter_fn {
    ($($(#[$attr:meta])* pub fn $fnname:ident(&mut self, $name:literal: $type:ty, $tostring:expr);)*) => {
        $(
        $(#[$attr])*
        pub fn $fnname(
            &mut self,
            value: $type,
            params: Option<::indexmap::IndexMap<String, String>>,
        ) {
            let property = Property {
                name: String::from($name),
                params: params.unwrap_or_default(),
                raw_value: $tostring(value),
                prop_group: None,
            };

            self.0.set(property);
        }
        )*
    };
}

macro_rules! chain_setter_fn {
    ($($(#[$attr:meta])* pub fn $fnname:ident(mut self, $name:literal: $type:ty, $tostring:expr);)*) => {
        $(
        $(#[$attr])*
        pub fn $fnname(
            mut self,
            value: $type,
            params: Option<::indexmap::IndexMap<String, String>>,
        ) -> Self {
            let property = Property {
                name: String::from($name),
                params: params.unwrap_or_default(),
                raw_value: $tostring(value),
                prop_group: None,
            };

            self.0.push(property);
            self
        }
        )*
    };
}

impl EventBuilder {
    /// Private function for adding event to calendar
    fn into_component(self) -> Component {
        self.0
    }

    setter_fn! {
        /// Setter for "DTEND" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn set_dtend(&mut self, "DTEND": Dtend, Dtend::into_raw);

        /// Setter for "DTSTART" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn set_dtstart(&mut self, "DTSTART": Dtstart, Dtstart::into_raw);

        /// Setter for "DTSTAMP" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn set_dtstamp(&mut self, "DTSTAMP": Dtstamp, Dtstamp::into_raw);

        /// Setter for "UID" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn set_uid(&mut self, "UID": Uid, Uid::into_raw);

        /// Setter for "DESCRIPTION" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn  set_description(&mut self, "DESCRIPTION": Description, Description::into_raw);

        /// Setter for "SUMMARY" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn set_summary(&mut self, "SUMMARY": Summary, Summary::into_raw);

        /// Setter for "URL" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn set_url(&mut self, "URL": Url, Url::into_raw);

        /// Setter for "LOCATION" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn set_location(&mut self, "LOCATION": Location, Location::into_raw);

        /// Setter for "CLASS" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn set_class(&mut self, "CLASS": Class, Class::into_raw);

        /// Setter for "CATEGORIES" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn  set_categories(&mut self, "CATEGORIES": Categories, Categories::into_raw
        );

        /// Setter for "TRANSP" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn set_transp(&mut self, "TRANSP": Transp, Transp::into_raw);

        /// Setter for "RRULE" property
        ///
        /// # Notice
        ///
        /// Internally, the property is overridden. Old values are dropped silently:
        pub fn set_rrule(&mut self, "RRULE": Rrule, Rrule::into_raw);
    }

    //
    // chainable builders
    //

    chain_setter_fn! {
        /// Chainable setter for "DTEND" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_dtend(mut self, "DTEND": Dtend, Dtend::into_raw);

        /// Chainable setter for "DTSTART" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_dtstart(mut self, "DTSTART": Dtstart, Dtstart::into_raw);

        /// Chainable setter for "DTSTAMP" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_dtstamp(mut self, "DTSTAMP": Dtstamp, Dtstamp::into_raw);

        /// Chainable setter for "UID" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_uid(mut self, "UID": Uid, Uid::into_raw);

        /// Chainable setter for "DESCRIPTION" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_description(mut self, "DESCRIPTION": Description, Description::into_raw);

        /// Chainable setter for "SUMMARY" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_summary(mut self, "SUMMARY": Summary, Summary::into_raw);

        /// Chainable setter for "URL" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_url(mut self, "URL": Url, Url::into_raw);

        /// Chainable setter for "LOCATION" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_location(mut self, "LOCATION": Location, Location::into_raw);

        /// Chainable setter for "CLASS" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_class(mut self, "CLASS": Class, Class::into_raw);

        /// Chainable setter for "CATEGORIES" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_categories(mut self, "CATEGORIES": Categories, Categories::into_raw);

        /// Chainable setter for "TRANSP" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_transp(mut self, "TRANSP": Transp, Transp::into_raw);

        /// Chainable setter for "RRULE" property.
        ///
        /// # Notice
        ///
        /// Internally, the property is added, not overridden.
        pub fn with_rrule(mut self, "RRULE": Rrule, Rrule::into_raw);
    }
}
