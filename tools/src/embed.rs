extern crate ui;
use ui::terminal::embed::create_pty;

fn main() -> std::io::Result<()> {
    create_pty().unwrap();
    Ok(())
}
