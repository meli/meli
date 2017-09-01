use std::error::Error;
use std::fmt;
use std::result;
use std::io;

pub type Result<T> = result::Result<T, MeliError>;

#[derive(Debug)]
pub struct MeliError {
    details: String
}

impl MeliError {
    pub fn new<M>(msg: M) -> MeliError where M: Into<String> {
        MeliError{details: msg.into()}
    }
}

impl fmt::Display for MeliError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,"{}",self.details)
    }
}

impl Error for MeliError {
    fn description(&self) -> &str {
        &self.details
    }
}

impl From<io::Error> for MeliError {
    #[inline]
    fn from(kind: io::Error) -> MeliError {
        MeliError::new(kind.description())
    }
}

