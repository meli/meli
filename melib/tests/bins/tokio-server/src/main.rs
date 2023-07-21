use anyhow::{Context, Error};
use argon2::Argon2;
use futures::{SinkExt, StreamExt};
use imap_codec::imap_types::{
    command::CommandBody,
    core::{NonEmptyVec, Text},
    response::{Capability, Continue, Data, Greeting, Response, Status},
};
use tokio::{self, net::TcpListener};
use tokio_support::server::{Action, Event, ImapServerCodec};
use tokio_util::codec::Decoder;

// Poor human's terminal color support.
const BLUE: &str = "\x1b[34m";
const RED: &str = "\x1b[31m";
const RESET: &str = "\x1b[0m";

#[tokio::main]
async fn main() -> Result<(), Error> {
    let addr = std::env::args()
        .nth(1)
        .context("USAGE: tokio-server <host>:<port>")?;

    let mut framed = {
        let stream = {
            // Bind listener ...
            let listener = TcpListener::bind(&addr)
                .await
                .context(format!("Could not bind to `{addr}`"))?;

            // ... and accept a single connection.
            let (stream, _) = listener
                .accept()
                .await
                .context("Could not accept connection")?;

            stream
        };

        // Accept 2 MiB literals.
        let mib2 = 2 * 1024 * 1024;
        ImapServerCodec::new(mib2).framed(stream)
    };

    // Send a positive greeting ...
    let greeting = Greeting::ok(None, "Hello, World!").context("Could not create greeting")?;
    framed
        .send(&greeting)
        .await
        .context("Could not send greeting")?;
    println!("S: {BLUE}{greeting:#?}{RESET}");

    // ... and process the following commands in a loop.
    loop {
        match framed
            .next()
            .await
            .context("Connection closed unexpectedly")?
            .context("Failed to obtain next message")?
        {
            Event::Command(cmd) => {
                println!("C: {RED}{cmd:#?}{RESET}");

                match (cmd.tag, cmd.body) {
                    (tag, CommandBody::Capability) => {
                        let rsp = Response::Data(Data::Capability(NonEmptyVec::from(
                            Capability::Imap4Rev1,
                        )));
                        framed.send(&rsp).await.context("Could not send response")?;
                        println!("S: {BLUE}{rsp:#?}{RESET}");

                        let rsp = Response::Status(
                            Status::ok(Some(tag), None, "CAPABILITY done")
                                .context("Could not create `Status`")?,
                        );
                        framed.send(&rsp).await.context("Could not send response")?;
                        println!("S: {BLUE}{rsp:#?}{RESET}");
                    }
                    (tag, CommandBody::Login { username, password }) => {
                        let login_okay = {
                            let username_okay = username.as_ref() == b"alice";
                            let password_okay = {
                                // Salt should be unique per password.
                                let salt = b"hf63l9nx43gf95ks";
                                let password = password.declassify().as_ref();

                                let mut output = [0u8; 32];
                                Argon2::default()
                                    .hash_password_into(password, salt, &mut output)
                                    .map_err(|error| Error::msg(error.to_string()))
                                    .context("Failed to hash password.")?;

                                output
                                    == [
                                        227, 130, 151, 49, 100, 203, 239, 68, 119, 207, 247, 237,
                                        214, 42, 85, 208, 198, 107, 116, 35, 64, 122, 143, 68, 236,
                                        228, 130, 250, 31, 221, 217, 77,
                                    ]
                            };

                            username_okay && password_okay
                        };

                        let rsp = if login_okay {
                            Response::Status(Status::Ok {
                                tag: Some(tag),
                                code: None,
                                text: Text::unvalidated("LOGIN succeeded"),
                            })
                        } else {
                            Response::Status(Status::Ok {
                                tag: Some(tag),
                                code: None,
                                text: Text::unvalidated("LOGIN failed"),
                            })
                        };
                        framed.send(&rsp).await.context("Could not send response")?;
                        println!("S: {BLUE}{rsp:#?}{RESET}");
                    }
                    (tag, CommandBody::Logout) => {
                        let rsp = Response::Status(
                            Status::bye(None, "...").expect("Could not create `Status`"),
                        );
                        framed.send(&rsp).await.context("Could not send response")?;
                        println!("S: {BLUE}{rsp:#?}{RESET}");

                        let rsp = Response::Status(
                            Status::ok(Some(tag), None, "LOGOUT done")
                                .expect("Could not create `Status`"),
                        );
                        framed.send(&rsp).await.context("Could not send response")?;
                        println!("S: {BLUE}{rsp:#?}{RESET}");

                        return Ok(());
                    }
                    (tag, body) => {
                        let text = format!("{} not supported", body.name());
                        let rsp = Response::Status(
                            Status::no(Some(tag), None, text)
                                .context("Could not create `Status`")?,
                        );
                        framed.send(&rsp).await.context("Could not send response")?;
                        println!("S: {BLUE}{rsp:#?}{RESET}");
                    }
                }
            }
            Event::ActionRequired(Action::SendLiteralAck(_)) => {
                println!("[!] Send continuation request.");
                let rsp = Response::Continue(
                    Continue::basic(None, "...").context("Could not create `Continue`")?,
                );
                framed.send(&rsp).await.context("Could not send response")?;
                println!("S: {BLUE}{rsp:#?}{RESET}");
            }
            Event::ActionRequired(Action::SendLiteralReject(_)) => {
                println!("[!] Send literal reject.");
                let rsp = Response::Status(
                    Status::bad(None, None, "literal too large.")
                        .context("Could not create `Status`")?,
                );
                framed.send(&rsp).await.context("Could not send response")?;
                println!("S: {BLUE}{rsp:#?}{RESET}");
            }
        }
    }
}
