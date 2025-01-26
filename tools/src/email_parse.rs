extern crate melib;
use melib::{Result, *};

/// Parses e-mail from files and prints the debug information of the parsed
/// `Envelope`
///
/// # Example invocation
/// ```sh
/// ./emailparse /path/to/email [/path/to/email2 /path/to/email3 ..]"
/// ```
fn main() -> Result<()> {
    if std::env::args().len() == 1 {
        eprintln!("Usage: ./emailparse /path/to/email [/path/to/email2 /path/to/email3 ..]");
        std::process::exit(1);
    }

    for i in std::env::args().skip(1) {
        println!("Path is {}", i);
        let filename = std::path::PathBuf::from(&i);

        if filename.exists() && filename.is_file() {
            let buffer = std::fs::read_to_string(&filename)
                .unwrap_or_else(|_| panic!("Something went wrong reading the file {}", i));
            let env = Envelope::from_bytes(buffer.as_bytes(), None).expect("Couldn't parse email");
            println!("Env is {:#?}", env);
            println!("{:?}", env.body_bytes(buffer.as_bytes()));
        } else {
            println!("{} is not a valid file.", i);
        }
    }
    Ok(())
}
