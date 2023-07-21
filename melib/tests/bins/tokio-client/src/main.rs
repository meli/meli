use anyhow::{Context, Error};
use futures::{SinkExt, StreamExt};
use imap_codec::imap_types::{
    command::{Command, CommandBody},
    core::Tag,
    response::{Response, Status},
};
use tokio::{self, net::TcpStream};
use tokio_support::client::{Event, ImapClientCodec};
use tokio_util::codec::Decoder;

// Poor human's terminal color support.
const BLUE: &str = "\x1b[34m";
const RED: &str = "\x1b[31m";
const RESET: &str = "\x1b[0m";

#[tokio::main]
async fn main() -> Result<(), Error> {
    let addr = std::env::args()
        .nth(1)
        .context("USAGE: tokio-client <host>:<port>")?;

    let mut framed = {
        let stream = TcpStream::connect(&addr)
            .await
            .context(format!("Could not connect to `{addr}`"))?;
        // This is for demonstration purposes only, and we probably want a bigger number.
        let max_literal_size = 1024;

        ImapClientCodec::new(max_literal_size).framed(stream)
    };

    // First, we read the server greeting.
    match framed
        .next()
        .await
        // We get an `Option<Result<...>>` here that denotes ...
        // 1) if we got something from the server, and
        // 2) if it was valid.
        .context("Connection closed unexpectedly")?
        .context("Failed to obtain next message")?
    {
        Event::Greeting(greeting) => {
            println!("S: {BLUE}{greeting:#?}{RESET}");
        }
        Event::Response(response) => {
            return Err(Error::msg(format!("Expected greeting, got `{response:?}`")));
        }
    };

    // Then, we send a login command to the server ...
    let tag_login = Tag::unvalidated("A1");
    let cmd = Command {
        tag: tag_login.clone(),
        body: CommandBody::login("alice", "password").context("Could not create command")?,
    };
    framed.send(&cmd).await.context("Could not send command")?;
    println!("C: {RED}{cmd:#?}{RESET}");

    // ... and process the response(s). We must read zero or many data responses before we can
    // finally examine the status response that tells us whether the login succeeded.
    loop {
        let frame = framed
            .next()
            .await
            .context("Connection closed unexpectedly")?
            .context("Failed to obtain next message")?;
        println!("S: {BLUE}{frame:#?}{RESET}");

        match frame {
            Event::Greeting(greeting) => {
                return Err(Error::msg(format!("Expected response, got `{greeting:?}`")));
            }
            Event::Response(response) => match response {
                Response::Status(ref status) if status.tag() == Some(&tag_login) => {
                    if matches!(status, Status::Ok { .. }) {
                        println!("[!] got login done (successful)");
                    } else {
                        println!("[!] got login done (failed)");
                    }

                    break;
                }
                _ => {
                    println!("[!] unexpected response");
                }
            },
        }
    }

    let tag_logout = Tag::unvalidated("A2");
    let cmd = Command {
        tag: tag_logout.clone(),
        body: CommandBody::Logout,
    };
    framed.send(&cmd).await.context("Could not send command")?;
    println!("C: {RED}{cmd:#?}{RESET}");

    loop {
        let frame = framed
            .next()
            .await
            .context("Connection closed unexpectedly")?
            .context("Failed to obtain next message")?;
        println!("S: {BLUE}{frame:#?}{RESET}");

        match frame {
            Event::Greeting(greeting) => {
                return Err(Error::msg(format!("Expected response, got `{greeting:?}`")));
            }
            Event::Response(response) => match response {
                Response::Status(Status::Bye { .. }) => {
                    println!("[!] got bye");
                }
                Response::Status(Status::Ok {
                    tag: Some(ref tag), ..
                }) if *tag == tag_logout => {
                    println!("[!] got logout done");
                    break;
                }
                _ => {
                    println!("[!] unexpected response");
                }
            },
        }
    }

    Ok(())
}
