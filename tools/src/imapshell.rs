extern crate melib;

use melib::{backends::ImapType, futures, smol, AccountSettings, BackendEventConsumer, Result};

/// Opens an interactive shell on an IMAP server. Suggested use is with
/// rlwrap(1)
///
/// # Example invocation:
/// ```sh
/// ./imapshell server_hostname server_username server_password server_port");
/// ```
///
/// `danger_accept_invalid_certs` is turned on by default, so no certificate
/// validation is performed.

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1).collect::<Vec<String>>();
    if args.len() != 4 {
        eprintln!("Usage: imapshell server_hostname server_username server_password server_port");
        std::process::exit(1);
    }

    let (a, b, c, d) = (
        std::mem::take(&mut args[0]),
        std::mem::take(&mut args[1]),
        std::mem::take(&mut args[2]),
        std::mem::take(&mut args[3]),
    );
    let set = AccountSettings {
        extra: [
            ("server_hostname".to_string(), a),
            ("server_username".to_string(), b),
            ("server_password".to_string(), c),
            ("server_port".to_string(), d),
            (
                "danger_accept_invalid_certs".to_string(),
                "true".to_string(),
            ),
        ]
        .iter()
        .cloned()
        .collect(),
        ..Default::default()
    };
    let mut imap = ImapType::new(
        &set,
        Box::new(|_| true),
        BackendEventConsumer::new(std::sync::Arc::new(|_, _| ())),
    )?;

    std::thread::spawn(move || {
        let ex = smol::Executor::new();
        futures::executor::block_on(ex.run(futures::future::pending::<()>()));
    });
    (imap.as_any_mut())
        .downcast_mut::<ImapType>()
        .unwrap()
        .shell();
    Ok(())
}
