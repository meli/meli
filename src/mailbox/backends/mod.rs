pub mod maildir;

use mailbox::email::Mail;
use error::Result;

pub trait MailBackend {
    fn get(&self) -> Result<Vec<Mail>>;
}
