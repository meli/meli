use std::io::{Error as IoError, Write};

use bounded_static::IntoBoundedStatic;
use bytes::{Buf, BufMut, BytesMut};
use imap_codec::{
    codec::{Decode, DecodeError, Encode},
    imap_types::{
        command::Command,
        response::{Greeting, Response},
    },
};
use thiserror::Error;
use tokio_util::codec::{Decoder, Encoder};

use super::{find_crlf_inclusive, FramingError, FramingState};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImapServerCodec {
    state: FramingState,
    max_literal_size: usize,
}

impl ImapServerCodec {
    pub fn new(max_literal_size: usize) -> Self {
        Self {
            state: FramingState::ReadLine { to_consume_acc: 0 },
            max_literal_size,
        }
    }
}

#[derive(Debug, Error)]
pub enum ImapServerCodecError {
    #[error(transparent)]
    Io(#[from] IoError),
    #[error(transparent)]
    Framing(#[from] FramingError),
    #[error("Parsing failed")]
    ParsingFailed(BytesMut),
}

impl PartialEq for ImapServerCodecError {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Io(error1), Self::Io(error2)) => error1.kind() == error2.kind(),
            (Self::Framing(kind1), Self::Framing(kind2)) => kind1 == kind2,
            (Self::ParsingFailed(x), Self::ParsingFailed(y)) => x == y,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Event {
    Command(Command<'static>),
    ActionRequired(Action),
    // More might be require.
}

#[derive(Debug, PartialEq, Eq)]
pub enum Action {
    SendLiteralAck(u32),
    SendLiteralReject(u32),
}

impl Decoder for ImapServerCodec {
    type Item = Event;
    type Error = ImapServerCodecError;

    fn decode(&mut self, src: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        loop {
            match self.state {
                FramingState::ReadLine {
                    ref mut to_consume_acc,
                } => match find_crlf_inclusive(*to_consume_acc, src) {
                    Some(line) => match line {
                        // After skipping `to_consume_acc` bytes, we need `to_consume` more
                        // bytes to form a full line (including the `\r\n`).
                        Ok(to_consume) => {
                            *to_consume_acc += to_consume;
                            let line = &src[..*to_consume_acc];

                            // TODO: Choose the required parser.
                            match Command::decode(line) {
                                // We got a complete message.
                                Ok((rem, cmd)) => {
                                    assert!(rem.is_empty());
                                    let cmd = cmd.into_static();

                                    src.advance(*to_consume_acc);
                                    self.state = FramingState::ReadLine { to_consume_acc: 0 };

                                    return Ok(Some(Event::Command(cmd)));
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
                                        if length as usize <= self.max_literal_size {
                                            src.reserve(length as usize);

                                            self.state = FramingState::ReadLiteral {
                                                to_consume_acc: *to_consume_acc,
                                                length,
                                            };

                                            return Ok(Some(Event::ActionRequired(
                                                Action::SendLiteralAck(length),
                                            )));
                                        } else {
                                            src.advance(*to_consume_acc);

                                            self.state =
                                                FramingState::ReadLine { to_consume_acc: 0 };

                                            return Ok(Some(Event::ActionRequired(
                                                Action::SendLiteralReject(length),
                                            )));
                                        }
                                    }
                                    DecodeError::Failed => {
                                        let consumed = src.split_to(*to_consume_acc);
                                        self.state = FramingState::ReadLine { to_consume_acc: 0 };

                                        return Err(ImapServerCodecError::ParsingFailed(consumed));
                                    }
                                },
                            }
                        }
                        // After skipping `to_consume_acc` bytes, we need `to_consume` more
                        // bytes to form a full line (including the `\n`).
                        //
                        // Note: This line is missing the `\r\n` and should be discarded.
                        Err(to_discard) => {
                            src.advance(*to_consume_acc + to_discard);
                            self.state = FramingState::ReadLine { to_consume_acc: 0 };

                            return Err(ImapServerCodecError::Framing(FramingError::NotCrLf));
                        }
                    },
                    // More data needed.
                    None => {
                        return Ok(None);
                    }
                },
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

impl Encoder<&Greeting<'_>> for ImapServerCodec {
    type Error = IoError;

    fn encode(&mut self, item: &Greeting, dst: &mut BytesMut) -> Result<(), Self::Error> {
        //dst.reserve(item.len());
        let mut writer = dst.writer();
        // TODO(225): Don't use `dump` here.
        let data = item.encode().dump();
        writer.write_all(&data)?;
        Ok(())
    }
}

impl Encoder<&Response<'_>> for ImapServerCodec {
    type Error = IoError;

    fn encode(&mut self, item: &Response, dst: &mut BytesMut) -> Result<(), Self::Error> {
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
    use bytes::BytesMut;
    use imap_codec::imap_types::{
        command::{Command, CommandBody},
        core::{AString, AtomExt, IString, Literal},
        secret::Secret,
    };
    #[cfg(feature = "quirk_crlf_relaxed")]
    use imap_types::core::Tag;
    use tokio_util::codec::Decoder;

    use super::*;

    #[test]
    fn test_decoder_line() {
        let tests = [
            (b"".as_ref(), Ok(None)),
            (b"a noop", Ok(None)),
            (b"\r", Ok(None)),
            (
                b"\n",
                Ok(Some(Event::Command(
                    Command::new("a", CommandBody::Noop).unwrap(),
                ))),
            ),
            (b"", Ok(None)),
            (b"xxxx", Ok(None)),
            (
                b"\r\n",
                Err(ImapServerCodecError::ParsingFailed(BytesMut::from(
                    b"xxxx\r\n".as_ref(),
                ))),
            ),
        ];

        let mut src = BytesMut::new();
        let mut codec = ImapServerCodec::new(1024);

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
            (b"".as_ref(), Ok(None)),
            (b"a login", Ok(None)),
            (b" {", Ok(None)),
            (b"5", Ok(None)),
            (b"}", Ok(None)),
            (
                b"\r\n",
                Ok(Some(Event::ActionRequired(Action::SendLiteralAck(5)))),
            ),
            (b"a", Ok(None)),
            (b"l", Ok(None)),
            (b"i", Ok(None)),
            (b"ce", Ok(None)),
            (b" ", Ok(None)),
            (
                b"password\r\n",
                Ok(Some(Event::Command(
                    Command::new(
                        "a",
                        CommandBody::Login {
                            username: AString::String(IString::Literal(
                                Literal::try_from(b"alice".as_ref()).unwrap(),
                            )),
                            password: Secret::new(AString::Atom(
                                AtomExt::try_from("password").unwrap(),
                            )),
                        },
                    )
                    .unwrap(),
                ))),
            ),
        ];

        let mut src = BytesMut::new();
        let mut codec = ImapServerCodec::new(1024);

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
            (
                b"xxx\r\n".as_ref(),
                Err(ImapServerCodecError::ParsingFailed(BytesMut::from(
                    b"xxx\r\n".as_ref(),
                ))),
            ),
            (
                b"a noop\n",
                #[cfg(not(feature = "quirk_crlf_relaxed"))]
                Err(ImapServerCodecError::Framing(FramingError::NotCrLf)),
                #[cfg(feature = "quirk_crlf_relaxed")]
                Ok(Some(Event::Command(Command {
                    tag: Tag::unvalidated("a"),
                    body: CommandBody::Noop,
                }))),
            ),
            (
                b"a login alice {16}\r\n",
                Ok(Some(Event::ActionRequired(Action::SendLiteralAck(16)))),
            ),
            (
                b"aaaaaaaaaaaaaaaa\r\n",
                Ok(Some(Event::Command(
                    Command::new(
                        "a",
                        CommandBody::login("alice", Literal::try_from("aaaaaaaaaaaaaaaa").unwrap())
                            .unwrap(),
                    )
                    .unwrap(),
                ))),
            ),
            (
                b"a login alice {17}\r\n",
                Ok(Some(Event::ActionRequired(Action::SendLiteralReject(17)))),
            ),
            (
                b"a login alice {1-}\r\n",
                Err(ImapServerCodecError::ParsingFailed(BytesMut::from(
                    b"a login alice {1-}\r\n".as_ref(),
                ))),
            ),
            (
                // Ohhhhhh, IMAP :-/
                b"a login alice }\r\n",
                Ok(Some(Event::Command(
                    Command::new("a", CommandBody::login("alice", "}").unwrap()).unwrap(),
                ))),
            ),
        ];

        let mut src = BytesMut::new();
        let mut codec = ImapServerCodec::new(16);

        for (test, expected) in tests {
            src.extend_from_slice(test);
            dbg!(&src, &codec);
            let got = codec.decode(&mut src);
            dbg!((std::str::from_utf8(test).unwrap(), &expected, &got));

            assert_eq!(expected, got);
        }
    }
}
