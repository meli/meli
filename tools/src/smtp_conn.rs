extern crate melib;

use melib::futures;
use melib::smol;
use melib::smtp::*;
use melib::Result;

fn main() -> Result<()> {
    let conf = SmtpServerConf {
        hostname: "smtp1.example.com".into(),
        port: 587,
        security: SmtpSecurity::StartTLS {
            danger_accept_invalid_certs: false,
        },
        extensions: SmtpExtensionSupport::default(),
        auth: SmtpAuth::Auto {
            username: "username".into(),
            password: Password::CommandEval("gpg2 --no-tty -q -d ~/.passwords/password.gpg".into()),
            require_auth: true,
            auth_type: Default::default(),
        },
        envelope_from: String::new(),
    };
    std::thread::spawn(move || {
        let ex = smol::Executor::new();
        futures::executor::block_on(ex.run(futures::future::pending::<()>()));
    });

    let mut conn = futures::executor::block_on(SmtpConnection::new_connection(conf)).unwrap();
    futures::executor::block_on(conn.mail_transaction(
         r##"To: username@example.com
Auto-Submitted: auto-generated
Subject: Fwd: *** SMTP TEST #2 information ***
From: Xxxxx <username@example.com>
Message-Id: <E1hSjnr-0003fN-RL2@example>
Date: Mon, 13 Jul 2020 15:02:15 +0300

machine : May 20 18:02:00 : user : user NOT in sudoers ; TTY=pts/13 ; PWD=/tmp/db-project ; USER=postgres ; COMMAND=/usr/bin/dropdb Prescriptions-R-X"##,
None
    )).unwrap();
    Ok(())
}
