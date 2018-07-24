use std;
use std::path::PathBuf;
use std::io::Write;

use uuid::Uuid;

#[derive(Debug)]
pub struct File {
    path: PathBuf,
}


impl File {
    pub fn file(&mut self) -> std::fs::File {
        std::fs::File::create(&self.path).unwrap()
    }
    pub fn path(&mut self) -> &mut PathBuf {
        &mut self.path
    }
}


pub fn create_temp_file(bytes: &[u8], filename: Option<&PathBuf>) -> File {
    let mut dir = std::env::temp_dir();

    let path = if let Some(p) = filename {
        p
    } else {
        dir.push("meli");
        std::fs::DirBuilder::new().recursive(true).create(&dir).unwrap();
        let u = Uuid::new_v4();
        dir.push(u.hyphenated().to_string());
        &dir
    };

    let mut f = std::fs::File::create(path).unwrap();

    f.write(bytes).unwrap();
    f.flush().unwrap();
    File {
        path: path.clone(),
    }
}

