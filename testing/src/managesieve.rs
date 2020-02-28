extern crate melib;

use melib::backends::imap::managesieve::new_managesieve_connection;
use melib::AccountSettings;
use melib::Result;

/// Opens an interactive shell on a managesieve server. Suggested use is with rlwrap(1)
///
/// # Example invocation:
/// ```sh
/// ./manage_sieve server_hostname server_username server_password server_port");
/// ```
///
/// `danger_accept_invalid_certs` is turned on by default, so no certificate validation is performed.

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1).collect::<Vec<String>>();
    if args.len() != 4 {
        eprintln!(
            "Usage: manage_sieve server_hostname server_username server_password server_port"
        );
        std::process::exit(1);
    }

    let (a, b, c, d) = (
        std::mem::replace(&mut args[0], String::new()),
        std::mem::replace(&mut args[1], String::new()),
        std::mem::replace(&mut args[2], String::new()),
        std::mem::replace(&mut args[3], String::new()),
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
    let mut conn = new_managesieve_connection(&set)?;
    conn.connect()?;
    let mut res = String::with_capacity(8 * 1024);

    let mut input = String::new();
    loop {
        use std::io;
        input.clear();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                if input.trim().eq_ignore_ascii_case("logout") {
                    break;
                }
                conn.send_command(input.as_bytes()).unwrap();
                conn.read_lines(&mut res, String::new()).unwrap();
                println!("out: {}", &res);
            }
            Err(error) => println!("error: {}", error),
        }
    }

    Ok(())
}
