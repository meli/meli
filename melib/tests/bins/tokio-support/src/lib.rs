//! Support for tokio and (tokio_util::codec).

use thiserror::Error;

pub mod client;
pub mod server;

/// All interactions transmitted by client and server are in the form of
/// lines, that is, strings that end with a CRLF.
///
/// The protocol receiver of an IMAP4rev1 client or server is either ...
#[derive(Debug, Clone, PartialEq, Eq)]
enum FramingState {
    /// ... reading a line, or ...
    ReadLine { to_consume_acc: usize },
    /// ... is reading a sequence of octets
    /// with a known count followed by a line.
    ReadLiteral { to_consume_acc: usize, length: u32 },
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum FramingError {
    #[error("Expected `\\r\\n`, got `\\n`")]
    NotCrLf,
    #[error("Could not find a line searching a maximum of {max_line_length} bytes")]
    LineTooLarge { max_line_length: u32 },
    #[error("Could not find a message while searching a maximum of {max_message_length} bytes")]
    MessageTooLarge { max_message_length: u32 },
    #[error("Expected a maximum literal length of {max_literal_length} bytes, got {length} bytes")]
    LiteralTooLarge {
        max_literal_length: u32,
        length: u32,
    },
}

/// Skip the first `skip` bytes of `buf` and count how many more bytes are needed to cover the next `\r\n`.
///
/// This function returns `Ok(None)` when no line was found, `Ok(Some(length))` with
/// `buf[..skip + length]` being the first line (including `\r\n`), or `Err(length)` with
/// `buf[..skip + length]` being the first line (including `\n`) with a missing `\r`.
fn find_crlf_inclusive(skip: usize, buf: &[u8]) -> Option<Result<usize, usize>> {
    #[allow(clippy::manual_map)]
    match buf.iter().skip(skip).position(|item| *item == b'\n') {
        Some(position) => {
            #[cfg(not(feature = "quirk_crlf_relaxed"))]
            if buf[skip + position.saturating_sub(1)] == b'\r' {
                Some(Ok(position + 1))
            } else {
                Some(Err(position + 1))
            }
            #[cfg(feature = "quirk_crlf_relaxed")]
            Some(Ok(position + 1))
        }
        None => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_crlf_inclusive() {
        let tests = [
            (b"A\r".as_ref(), 0, None),
            (b"A\r\n", 0, Some(Ok(3))),
            #[cfg(not(feature = "quirk_crlf_relaxed"))]
            (b"A\n", 0, Some(Err(2))),
            #[cfg(feature = "quirk_crlf_relaxed")]
            (b"A\n", 0, Some(Ok(2))),
            #[cfg(not(feature = "quirk_crlf_relaxed"))]
            (b"\n", 0, Some(Err(1))),
            #[cfg(feature = "quirk_crlf_relaxed")]
            (b"\n", 0, Some(Ok(1))),
            (b"aaa\r\nA\r".as_ref(), 5, None),
            (b"aaa\r\nA\r\n", 5, Some(Ok(3))),
            #[cfg(not(feature = "quirk_crlf_relaxed"))]
            (b"aaa\r\nA\n", 5, Some(Err(2))),
            #[cfg(feature = "quirk_crlf_relaxed")]
            (b"aaa\r\nA\n", 5, Some(Ok(2))),
            #[cfg(not(feature = "quirk_crlf_relaxed"))]
            (b"aaa\r\n\n", 5, Some(Err(1))),
            #[cfg(feature = "quirk_crlf_relaxed")]
            (b"aaa\r\n\n", 5, Some(Ok(1))),
        ];

        for (test, skip, expected) in tests {
            let got = find_crlf_inclusive(skip, test);

            dbg!((std::str::from_utf8(test).unwrap(), skip, &expected, &got));

            assert_eq!(expected, got);
        }
    }
}
