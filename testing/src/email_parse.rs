extern crate melib;
use melib::*;

use melib::Result;

fn main() -> Result<()> {
    for i in std::env::args().skip(1) {
        println!("i is {}", i);
        let filename = std::path::PathBuf::from(i);

        if filename.is_file() {
            let buffer = std::fs::read_to_string(&filename).expect(&format!(
                "Something went wrong reading the file {}",
                filename.display()
            ));
            let env = Envelope::from_bytes(&buffer.as_bytes(), None).expect("Couldn't parse email");
            eprintln!("Env is {:#?}", env);
            eprintln!("{:?}", env.body_bytes(buffer.as_bytes()));
        } else {
            println!("it's not a file");
        }
    }
    Ok(())
}
