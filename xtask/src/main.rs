#![allow(clippy::print_stderr)]
#![allow(clippy::print_stdout)]
use std::error::Error;

mod config_macros;

fn main() {
    if let Err(err) = run_app() {
        eprintln!("{err}");
        std::process::exit(-1);
    }
}

fn confoverrides() -> Result<(), Box<dyn Error>> {
    use std::path::Path;
    std::env::set_current_dir(Path::new("./meli"))?;
    let mut confoverride = vec![];
    config_macros::override_derive(config_macros::LIST, &mut confoverride);
    let confoverride = std::str::from_utf8(&confoverride)?;
    print!("{confoverride}");
    Ok(())
}

fn run_app() -> Result<(), Box<dyn Error>> {
    let task = std::env::args().nth(1);
    match task.as_deref() {
        Some("confoverrides") => confoverrides()?,
        _ => print_help(),
    }
    Ok(())
}

fn print_help() {
    eprintln!(
        "Tasks:

confoverrides            rebuild meli/src/conf/overrides.rs",
    )
}
