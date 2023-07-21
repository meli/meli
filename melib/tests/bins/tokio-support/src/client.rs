use std::io::{Error as IoError, Write};

use bounded_static::IntoBoundedStatic;
use bytes::{Buf, BufMut, BytesMut};
use imap_codec::{
    codec::{Decode, DecodeError, Encode},
    imap_types::{
        command::Command,
        response::{Greeting, Response},
        state::State as ImapState,
    },
};
use thiserror::Error;
use tokio_util::codec::{Decoder, Encoder};

use super::{find_crlf_inclusive, FramingError, FramingState};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImapClientCodec {
    state: FramingState,
    imap_state: ImapState<'static>,
    max_literal_length: u32,
}

impl ImapClientCodec {
    pub fn new(max_literal_length: u32) -> Self {
        Self {
            state: FramingState::ReadLine { to_consume_acc: 0 },
            imap_state: ImapState::Greeting,
            max_literal_length,
        }
    }
}

#[derive(Debug, Error)]
pub enum ImapClientCodecError {
    #[error(transparent)]
    Io(#[from] IoError),
    #[error(transparent)]
    Framing(#[from] FramingError),
    #[error("Parsing failed")]
    ParsingFailed(BytesMut),
}

impl PartialEq for ImapClientCodecError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Io(error1), Self::Io(error2)) => error1.kind() == error2.kind(),
            (Self::Framing(kind2), Self::Framing(kind1)) => kind1 == kind2,
            (Self::ParsingFailed(x), Self::ParsingFailed(y)) => x == y,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Event {
    Greeting(Greeting<'static>),
    Response(Response<'static>),
}

impl Decoder for ImapClientCodec {
    type Item = Event;
    type Error = ImapClientCodecError;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        loop {
            match self.state {
                FramingState::ReadLine {
                    ref mut to_consume_acc,
                } => {
                    match find_crlf_inclusive(*to_consume_acc, src) {
                        Some(line) => match line {
                            // After skipping `to_consume_acc` bytes, we need `to_consume` more
                            // bytes to form a full line (including the `\r\n`).
                            Ok(to_consume) => {
                                *to_consume_acc += to_consume;
                                let line = &src[..*to_consume_acc];

                                // TODO: Choose the required parser.
                                let parser = match self.imap_state {
                                    ImapState::Greeting => |input| {
                                        Greeting::decode(input).map(|(rem, grt)| {
                                            (rem, Event::Greeting(grt.into_static()))
                                        })
                                    },
                                    _ => |input| {
                                        Response::decode(input).map(|(rem, rsp)| {
                                            (rem, Event::Response(rsp.into_static()))
                                        })
                                    },
                                };

                                match parser(line) {
                                    // We got a complete message.
                                    Ok((rem, outcome)) => {
                                        assert!(rem.is_empty());

                                        src.advance(*to_consume_acc);
                                        self.state = FramingState::ReadLine { to_consume_acc: 0 };

                                        if self.imap_state == ImapState::Greeting {
                                            self.imap_state = ImapState::NotAuthenticated;
                                        }

                                        return Ok(Some(outcome));
                                    }
                                    Err(error) => match error {
                                        // We supposedly need more data ...
                                        //
                                        // This should not happen because a line that doesn't end
                                        // with a literal is always "complete" in IMAP.
                                        DecodeError::Incomplete => {
                                            unreachable!();
                                        }
                                        // We found a literal.
                                        DecodeError::LiteralFound { length, .. } => {
                                            if length <= self.max_literal_length {
                                                src.reserve(length as usize);

                                                self.state = FramingState::ReadLiteral {
                                                    to_consume_acc: *to_consume_acc,
                                                    length,
                                                };

                                                return Ok(None);
                                            } else {
                                                src.advance(*to_consume_acc);

                                                self.state =
                                                    FramingState::ReadLine { to_consume_acc: 0 };

                                                return Err(ImapClientCodecError::Framing(
                                                    FramingError::LiteralTooLarge {
                                                        max_literal_length: self.max_literal_length,
                                                        length,
                                                    },
                                                ));
                                            }
                                        }
                                        DecodeError::Failed => {
                                            let consumed = src.split_to(*to_consume_acc);
                                            self.state =
                                                FramingState::ReadLine { to_consume_acc: 0 };

                                            return Err(ImapClientCodecError::ParsingFailed(
                                                consumed,
                                            ));
                                        }
                                    },
                                }
                            }
                            // After skipping `to_consume_acc` bytes, we need `to_consume` more
                            // bytes to form a full line (including the `\n`).
                            //
                            // Note: This line is missing the `\r\n` and should be discarded.
                            Err(to_discard) => {
                                *to_consume_acc += to_discard;
                                src.advance(*to_consume_acc);

                                self.state = FramingState::ReadLine { to_consume_acc: 0 };
                                return Err(ImapClientCodecError::Framing(FramingError::NotCrLf));
                            }
                        },
                        // More data needed.
                        None => {
                            return Ok(None);
                        }
                    }
                }
                FramingState::ReadLiteral {
                    to_consume_acc,
                    length,
                } => {
                    if to_consume_acc + length as usize <= src.len() {
                        self.state = FramingState::ReadLine {
                            to_consume_acc: to_consume_acc + length as usize,
                        }
                    } else {
                        return Ok(None);
                    }
                }
            }
        }
    }
}

impl<'a> Encoder<&Command<'a>> for ImapClientCodec {
    type Error = IoError;

    fn encode(&mut self, item: &Command, dst: &mut BytesMut) -> Result<(), Self::Error> {
        //dst.reserve(item.len());
        let mut writer = dst.writer();
        // TODO(225): Don't use `dump` here.
        let data = item.encode().dump();
        writer.write_all(&data)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "quirk_crlf_relaxed")]
    use std::num::NonZeroU32;

    use bytes::BytesMut;
    use imap_codec::imap_types::{
        core::{Literal, NString},
        fetch::{MessageDataItem, Section},
        response::{Data, GreetingKind},
    };
    use tokio_util::codec::Decoder;

    use super::*;

    #[test]
    fn test_decoder_line() {
        let tests = [
            (b"".as_ref(), Ok(None)),
            (b"* ", Ok(None)),
            (b"OK ...\r", Ok(None)),
            (
                b"\n",
                Ok(Some(Event::Greeting(
                    Greeting::new(GreetingKind::Ok, None, "...").unwrap(),
                ))),
            ),
            (b"", Ok(None)),
            (b"xxxx", Ok(None)),
            (
                b"\r\n",
                Err(ImapClientCodecError::ParsingFailed(BytesMut::from(
                    b"xxxx\r\n".as_ref(),
                ))),
            ),
        ];

        let mut src = BytesMut::new();
        let mut codec = ImapClientCodec::new(1024);

        for (test, expected) in tests {
            src.extend_from_slice(test);
            let got = codec.decode(&mut src);

            assert_eq!(expected, got);

            dbg!((std::str::from_utf8(test).unwrap(), &expected, &got));
        }
    }

    #[test]
    fn test_decoder_literal() {
        let tests = [
            (
                b"* OK ...\r\n".as_ref(),
                Ok(Some(Event::Greeting(
                    Greeting::new(GreetingKind::Ok, None, "...").unwrap(),
                ))),
            ),
            (b"* 12 FETCH (BODY[HEADER] {3}", Ok(None)),
            (b"\r", Ok(None)),
            (b"\n", Ok(None)),
            (b"a", Ok(None)),
            (b"bc)", Ok(None)),
            (b"\r", Ok(None)),
            (
                b"\n",
                Ok(Some(Event::Response(Response::Data(
                    Data::fetch(
                        12,
                        vec![MessageDataItem::BodyExt {
                            section: Some(Section::Header(None)),
                            origin: None,
                            data: NString(Some(Literal::try_from("abc").unwrap().into())),
                        }],
                    )
                    .unwrap(),
                )))),
            ),
        ];

        let mut src = BytesMut::new();
        let mut codec = ImapClientCodec::new(1024);

        for (test, expected) in tests {
            src.extend_from_slice(test);
            let got = codec.decode(&mut src);

            dbg!((std::str::from_utf8(test).unwrap(), &expected, &got));

            assert_eq!(expected, got);
        }
    }

    #[test]
    fn test_decoder_error() {
        let tests = [
            // We need to process the greeting first.
            (
                b"* OK ...\r\n".as_ref(),
                Ok(Some(Event::Greeting(
                    Greeting::new(GreetingKind::Ok, None, "...").unwrap(),
                ))),
            ),
            (
                b"xxx\r\n".as_ref(),
                Err(ImapClientCodecError::ParsingFailed(BytesMut::from(
                    b"xxx\r\n".as_ref(),
                ))),
            ),
            (
                b"* search 1\n",
                #[cfg(not(feature = "quirk_crlf_relaxed"))]
                Err(ImapClientCodecError::Framing(FramingError::NotCrLf)),
                #[cfg(feature = "quirk_crlf_relaxed")]
                Ok(Some(Event::Response(Response::Data(Data::Search(vec![
                    NonZeroU32::try_from(1).unwrap(),
                ]))))),
            ),
            (
                b"* 1 fetch (BODY[] {17}\r\naaaaaaaaaaaaaaaa)\r\n",
                Err(ImapClientCodecError::Framing(
                    FramingError::LiteralTooLarge {
                        max_literal_length: 16,
                        length: 17,
                    },
                )),
            ),
        ];

        let mut src = BytesMut::new();
        let mut codec = ImapClientCodec::new(16);

        for (test, expected) in tests {
            src.extend_from_slice(test);
            let got = codec.decode(&mut src);

            dbg!((std::str::from_utf8(test).unwrap(), &expected, &got));

            assert_eq!(expected, got);
        }
    }
}
