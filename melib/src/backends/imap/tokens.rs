use std::fmt;
use std::num::NonZeroUsize;

trait Join {
    fn join(&self, sep: char) -> String;
}

impl<T> Join for [T]
where
    T: fmt::Display,
{
    fn join(&self, sep: char) -> String {
        if self.is_empty() {
            String::from("")
        } else if self.len() == 1 {
            format!("{}", self[0])
        } else {
            format!("{}{}{}", self[0], sep, self[1..].join(sep))
        }
    }
}

struct Search {
    charset: Option<String>,
    search_keys: Vec<SearchKey>,
}

impl fmt::Display for Search {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "SEARCH{} {}",
            if let Some(ch) = self.charset.as_ref() {
                format!(" CHARSET {}", ch)
            } else {
                format!("")
            },
            self.search_keys.join(' ')
        )
    }
}

enum SearchKey {
    All,
    Answered,
    Bcc(String),
    Before(String),
    Body(String),
    Cc(String),
    Deleted,
    Flagged,
    From(String),
    Keyword(FlagKeyword),
    New,
    Old,
    On(String),
    Recent,
    Seen,
    Since(String),
    Subject(String),
    Text(String),
    To(String),
    Unanswered,
    Undeleted,
    Unflagged,
    Unkeyword(FlagKeyword),
    Unseen,
    Draft,
    Header(String, String), //HeaderFldName
    Larger(u64),
    Not(Box<SearchKey>),
    Or(Box<SearchKey>, Box<SearchKey>),
    SentBefore(String), //Date
    SentOn(String),     //Date
    SentSince(String),  //Date
    Smaller(u64),
    Uid(SequenceSet),
    Undraft,
    SequenceSet(SequenceSet),
    And(Vec<SearchKey>),
}
impl fmt::Display for SearchKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                SearchKey::All => format!("ALL"),
                SearchKey::Answered => format!("ANSWERED"),
                SearchKey::Bcc(ref s) => format!("BCC {}", s),
                SearchKey::Before(ref s) => format!("BEFORE {}", s),
                SearchKey::Body(ref s) => format!("BODY {}", s),
                SearchKey::Cc(ref s) => format!("CC {}", s),
                SearchKey::Deleted => format!("DELETED"),
                SearchKey::Flagged => format!("FLAGGED"),
                SearchKey::From(ref s) => format!("FROM {}", s),
                SearchKey::Keyword(ref s) => format!("KEYWORD {}", s),
                SearchKey::New => format!("NEW"),
                SearchKey::Old => format!("OLD"),
                SearchKey::On(ref s) => format!("ON {}", s),
                SearchKey::Recent => format!("RECENT"),
                SearchKey::Seen => format!("SEEN"),
                SearchKey::Since(ref s) => format!("SINCE {}", s),
                SearchKey::Subject(ref s) => format!("SUBJECT {}", s),
                SearchKey::Text(ref s) => format!("TEXT {}", s),
                SearchKey::To(ref s) => format!("TO {}", s),
                SearchKey::Unanswered => format!("UNANSWERED"),
                SearchKey::Undeleted => format!("UNDELETED"),
                SearchKey::Unflagged => format!("UNFLAGGED"),
                SearchKey::Unkeyword(ref s) => format!("UNKEYWORD {}", s),
                SearchKey::Unseen => format!("UNSEEN"),
                SearchKey::Draft => format!("DRAFT"),
                SearchKey::Header(ref name, ref value) => format!("HEADER {} {}", name, value),
                SearchKey::Larger(ref s) => format!("LARGER {}", s),
                SearchKey::Not(ref s) => format!("NOT {}", s),
                SearchKey::Or(ref a, ref b) => format!("OR {} {}", a, b),
                SearchKey::SentBefore(ref s) => format!("SENTBEFORE {}", s),
                SearchKey::SentOn(ref s) => format!("SENTON {}", s),
                SearchKey::SentSince(ref s) => format!("SENTSINCE {}", s),
                SearchKey::Smaller(ref s) => format!("SMALLER {}", s),
                SearchKey::Uid(ref s) => format!("UID {}", s),
                SearchKey::Undraft => format!("UNDRAFT"),
                SearchKey::SequenceSet(ref s) => format!("SEQUENCESET {}", s),
                SearchKey::And(ref s) => format!("({})", s.join(' ')),
            }
        )
    }
}

struct Delete {
    mailbox: Mailbox,
}

impl fmt::Display for Delete {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DELETE {}", self.mailbox)
    }
}

struct Examine {
    mailbox: Mailbox,
}

impl fmt::Display for Examine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "EXAMINE {}", self.mailbox)
    }
}

struct Select {
    mailbox: Mailbox,
}

impl fmt::Display for Select {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SELECT {}", self.mailbox)
    }
}

struct List {
    mailbox: Mailbox,
    list: String,
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "LIST {} \"{}\"",
            if self.mailbox.is_empty() {
                format!("\"\"")
            } else {
                format!("{}", self.mailbox)
            },
            self.list.as_str()
        )
    }
}

struct Lsub {
    mailbox: Mailbox,
    list: String,
}

impl fmt::Display for Lsub {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LSUB {} \"{}\"", self.mailbox, self.list)
    }
}

enum StatusAttribute {
    Messages,
    Recent,
    UidNext,
    UidValidity,
    Unseen,
}

impl fmt::Display for StatusAttribute {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                StatusAttribute::Messages => "MESSAGES",
                StatusAttribute::Recent => "RECENT",
                StatusAttribute::UidNext => "UIDNEXT",
                StatusAttribute::UidValidity => "UIDVALIDITY",
                StatusAttribute::Unseen => "UNSEEN",
            }
        )
    }
}

struct Status {
    mailbox: Mailbox,
    status_attributes: Vec<StatusAttribute>,
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "STATUS {} ({})",
            self.mailbox,
            self.status_attributes.join(' ')
        )
    }
}

struct Store {
    sequence_set: SequenceSet,
    //store_att_flags:
}

impl fmt::Display for Store {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        unimplemented!()
        //write!(f, "STORE {}", self.sequence_set)
    }
}

struct Unsubscribe {
    mailbox: Mailbox,
}

impl fmt::Display for Unsubscribe {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "UNSUBSCRIBE {}", self.mailbox)
    }
}

struct Subscribe {
    mailbox: Mailbox,
}

impl fmt::Display for Subscribe {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SUBSCRIBE {}", self.mailbox)
    }
}

struct Copy {
    sequence_set: SequenceSet,
    mailbox: Mailbox,
}

impl fmt::Display for Copy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "COPY {} {}", self.sequence_set, self.mailbox)
    }
}

struct Create {
    mailbox: Mailbox,
}

impl fmt::Display for Create {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "CREATE {}", self.mailbox)
    }
}

struct Rename {
    from: Mailbox,
    to: Mailbox,
}

impl fmt::Display for Rename {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RENAME {} {}", self.from, self.to)
    }
}

struct Append {
    mailbox: Mailbox,
    flag_list: Vec<Flag>,
    date_time: Option<String>,
    literal: String,
}

impl fmt::Display for Append {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "APPEND {}{}{} {}",
            self.mailbox,
            if self.flag_list.is_empty() {
                String::from("")
            } else {
                format!(" {}", self.flag_list.join(' '))
            },
            if let Some(date_time) = self.date_time.as_ref() {
                format!(" {}", date_time)
            } else {
                String::from("")
            },
            self.literal.as_str()
        )
    }
}

struct Fetch {
    sequence_set: SequenceSet,
}

impl fmt::Display for Fetch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FETCH {}", self.sequence_set)
    }
}

enum Flag {
    Answered,
    Flagged,
    Deleted,
    Seen,
    Draft,
    /*atom */
    X(String),
}

impl fmt::Display for Flag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "\\{}",
            match self {
                Flag::Answered => "Answered",
                Flag::Flagged => "Flagged",
                Flag::Deleted => "Deleted",
                Flag::Seen => "Seen",
                Flag::Draft => "Draft",
                Flag::X(ref c) => c.as_str(),
            }
        )
    }
}

enum Uid {
    Copy(Copy),
    Fetch(Fetch),
    Search(Search),
    Store(Store),
}

impl fmt::Display for Uid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "UID {}",
            match self {
                Uid::Copy(ref c) => format!("{}", c),
                Uid::Fetch(ref c) => format!("{}", c),
                Uid::Search(ref c) => format!("{}", c),
                Uid::Store(ref c) => format!("{}", c),
            }
        )
    }
}

enum CommandSelect {
    Check,
    Close,
    Expunge,
    Copy(Copy),
    Fetch(Fetch),
    Store(Store),
    Uid(Uid),
    Search(Search),
}

impl fmt::Display for CommandSelect {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CommandSelect::Check => format!("CHECK"),
                CommandSelect::Close => format!("CLOSE"),
                CommandSelect::Expunge => format!("EXPUNGE"),
                CommandSelect::Copy(ref c) => format!("{}", c),
                CommandSelect::Fetch(ref c) => format!("{}", c),
                CommandSelect::Store(ref c) => format!("{}", c),
                CommandSelect::Uid(ref c) => format!("{}", c),
                CommandSelect::Search(ref c) => format!("{}", c),
            }
        )
    }
}

/// Valid in all states
enum CommandAny {
    Capability,
    Logout,
    Noop,
    XCommand(String),
}

impl fmt::Display for CommandAny {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CommandAny::Capability => format!("CAPABILITY"),
                CommandAny::Logout => format!("LOGOUT"),
                CommandAny::Noop => format!("NOOP"),
                CommandAny::XCommand(ref x) => format!("{}", x),
            }
        )
    }
}

enum CommandAuth {
    Append(Append),
    Create(Create),
    Delete(Delete),
    Examine(Examine),
    List(List),
    Lsub(Lsub),
    Rename(Rename),
    Select(Select),
    Status(Status),
    Subscribe(Subscribe),
    Unsubscribe(Unsubscribe),
}

impl fmt::Display for CommandAuth {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CommandAuth::Append(ref c) => c.to_string(),
                CommandAuth::Create(ref c) => c.to_string(),
                CommandAuth::Delete(ref c) => c.to_string(),
                CommandAuth::Examine(ref c) => c.to_string(),
                CommandAuth::List(ref c) => c.to_string(),
                CommandAuth::Lsub(ref c) => c.to_string(),
                CommandAuth::Rename(ref c) => c.to_string(),
                CommandAuth::Select(ref c) => c.to_string(),
                CommandAuth::Status(ref c) => c.to_string(),
                CommandAuth::Subscribe(ref c) => c.to_string(),
                CommandAuth::Unsubscribe(ref c) => c.to_string(),
            }
        )
    }
}

enum CommandNonAuth {
    Login(String, String),
    Authenticate(String, String),
    StartTls,
}

impl fmt::Display for CommandNonAuth {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CommandNonAuth::Login(ref userid, ref password) => {
                write!(f, "LOGIN \"{}\" \"{}\"", userid, password)
            }
            CommandNonAuth::Authenticate(ref auth_type, ref base64) => {
                write!(f, "AUTHENTICATE \"{}\" \"{}\"", auth_type, base64)
            }
            CommandNonAuth::StartTls => write!(f, "STARTTLS"),
        }
    }
}

enum Command {
    Any(CommandAny),
    Auth(CommandAuth),
    NonAuth(CommandNonAuth),
    Select(CommandSelect),
}
impl fmt::Display for Command {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Command::Any(c) => write!(f, "{}", c),
            Command::Auth(c) => write!(f, "{}", c),
            Command::NonAuth(c) => write!(f, "{}", c),
            Command::Select(c) => write!(f, "{}", c),
        }
    }
}

pub(super) struct ImapCommand {
    tag: usize,
    command: Command,
}

impl fmt::Display for ImapCommand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}\r\n", self.tag, self.command)
    }
}

enum SeqNumber {
    MsgNumber(NonZeroUsize),
    UID(NonZeroUsize),
    /** "*" represents the largest number in use.  In
    the case of message sequence numbers, it is the number of messages in a
    non-empty mailbox.  In the case of unique identifiers, it is the unique
    identifier of the last message in the mailbox or, if the mailbox is empty, the
    mailbox's current UIDNEXT value **/
    Largest,
}

impl fmt::Display for SeqNumber {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SeqNumber::MsgNumber(n) => write!(f, "{}", n),
            SeqNumber::UID(u) => write!(f, "{}", u),
            SeqNumber::Largest => write!(f, "*"),
        }
    }
}

struct SeqRange(SeqNumber, SeqNumber);
impl fmt::Display for SeqRange {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

struct SequenceSet {
    numbers: Vec<SeqNumber>,
    ranges: Vec<SeqRange>,
}
impl fmt::Display for SequenceSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}",
            if self.numbers.is_empty() {
                String::from("")
            } else {
                self.numbers.join(',')
            },
            if self.ranges.is_empty() {
                String::from("")
            } else {
                self.ranges.join(',')
            }
        )
    }
}

type Mailbox = String;
type FlagKeyword = String;
