extern crate melib;

use melib::futures;
use melib::smol;
use melib::smtp::*;
use melib::Result;

/// Opens an interactive shell on an IMAP server. Suggested use is with rlwrap(1)
///
/// # Example invocation:
/// ```sh
/// ./imap_conn server_hostname server_username server_password server_port");
/// ```
///
/// `danger_accept_invalid_certs` is turned on by default, so no certificate validation is performed.

fn main() -> Result<()> {
    let conf = SmtpServerConf {
        hostname: "smtp1.ntua.gr".into(),
        port: 587,
        security: SmtpSecurity::StartTLS {
            danger_accept_invalid_certs: false,
        },
        extensions: SmtpExtensionSupport::default(),
        auth: SmtpAuth::Auto {
            username: "el13635".into(),
            password: Password::CommandEval(
                "gpg2 --no-tty -q -d ~/.passwords/msmtp/ntua.gpg".into(),
            ),
            require_auth: true,
        },
        envelope_from: String::new(),
    };
    for _ in 0..1 {
        std::thread::spawn(|| smol::run(futures::future::pending::<()>()));
    }

    let mut conn = futures::executor::block_on(SmtpConnection::new_connection(conf)).unwrap();
    futures::executor::block_on(conn.mail_transaction(
         r##"To: pr.birch@gmail.com
Auto-Submitted: auto-generated
Subject: Fwd: *** SMTP TEST #2 information ***
From: Manos <el13635@mail.ntua.gr>
Message-Id: <E1hSjnr-0003fN-RL2@postretch>
Date: Mon, 13 Jul 2020 15:02:15 +0300

postretch : May 20 18:02:00 : epilys : user NOT in sudoers ; TTY=pts/13 ; PWD=/tmp/db-project ; USER=postgres ; COMMAND=/usr/bin/dropdb Prescriptions-R-X"##,
    )).unwrap();
    Ok(())
}
