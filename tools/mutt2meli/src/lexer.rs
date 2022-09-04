pub enum Statement {
    Command { inner: MuttrcCommand },
    Value { inner: MuttrcConfigurationValues },
}

pub enum MuttrcCommand {
    /// alias [-group name [...]] key address [, address [ ... ]]
    /// unalias [ *  | key ]
    ///
    /// alias defines an alias key for the given addresses. Each address will be resolved into either
    /// an email address (user@example.com) or a named email address (User Name <user@example.com>).
    /// The address may be specified in either format, or in the format "user@example.com (User Name)".
    /// unalias removes the alias corresponding to the given key or all aliases when "*" is used as an
    /// argument. The optional -group argument to alias causes the aliased address(es) to be added to
    /// the named group.
    Alias,
    Unalias,

    /// group [-group name] [-rx EXPR [ ... ]] [-addr address [ ... ]]
    /// ungroup [-group name ] [ * | [[-rx EXPR [ ... ]] [-addr address [ ... ]]]
    ///
    /// group is used to directly add either addresses or regular expressions to the specified group or
    /// groups. The different categories of arguments to the group command can be in any order. The
    /// flags -rx and -addr specify what the following strings (that cannot begin with a hyphen) should
    /// be interpreted as: either a regular expression or an email address, respectively. ungroup is
    /// used to remove addresses or regular expressions from the specified group or groups. The syntax
    /// is similar to the group command, however the special character * can be used to empty a group
    /// of all of its contents.
    /// These address groups can also be created implicitly by the alias, lists, subscribe and alternates commands by specifying the optional -group option.
    ///
    /// Once defined, these address groups can be used in patterns to search for and limit the display to messages matching a group.
    Group,
    Ungroup,

    /// alternates [-group name] regexp [ , regexp [ ... ]]
    /// unalternates [ *  | regexp [ , regexp [ ... ]] ]
    ///
    ///    alternates is used to inform mutt about alternate addresses where you receive mail; you can
    ///    use regular expressions to specify alternate addresses. This affects mutt's idea about
    ///    messages from you, and messages addressed to you. unalternates removes a regular expression
    ///    from the list of known alternates. The -group flag causes all of the subsequent regular
    ///    expressions to be added to the named group.
    Alternates,
    Unalternates,

    /// alternative_order type[/subtype] [ ... ]
    /// unalternative_order [ *  | type/subtype] [...]
    ///
    ///    alternative_order command permits you to define an order of preference which is used by mutt
    ///    to determine which part of a multipart/alternative body to display. A subtype of "*" matches
    ///    any subtype, as does an empty subtype. unalternative_order removes entries from the ordered
    ///    list or deletes the entire list when "*" is used as an argument.
    AlternativeOrder,
    UnalternativeOrder,

    /// auto_view type[/subtype] [ ... ]
    /// unauto_view type[/subtype] [ ... ]
    ///
    ///   This commands permits you to specify that mutt should automatically convert the given MIME
    ///   types to text/plain when displaying messages. For this to work, there must be a mailcap(5)
    ///   entry for the given MIME type with the copiousoutput flag set. A subtype of "*" matches any
    ///   subtype, as does an empty subtype.
    AutoView,
    UnautoView,

    /// mime_lookup type[/subtype] [ ... ]
    /// unmime_lookup type[/subtype] [ ... ]
    ///
    /// This command permits you to define a list of "data" MIME content types for which mutt will try
    /// to determine the actual file type from the file name, and not use a mailcap(5) entry given for
    /// the original MIME type. For instance, you may add the application/octet-stream MIME type to
    /// this list. bind map1,map2,... key function This command binds the given key for the given map
    /// or maps to the given function. Multiple maps may be specified by separating them with commas
    /// (no whitespace is allowed). Valid maps are: generic, alias, attach, browser, editor, index,
    /// compose, pager, pgp, postpone, mix.
    ///
    /// For more information on keys and functions, please consult the Mutt Manual. Note that the
    /// function name is to be specified without angle brackets. account-hook [!]regexp command This
    /// hook is executed whenever you access a remote mailbox. Useful to adjust configuration settings
    /// to different IMAP or POP servers. charset-hook alias charset This command defines an alias for
    /// a character set. This is useful to properly display messages which are tagged with a character
    /// set name not known to mutt. iconv-hook charset local-charset This command defines a
    /// system-specific name for a character set. This is useful when your system's iconv(3)
    /// implementation does not understand MIME character set names (such as iso-8859-1), but instead
    /// insists on being fed with implementation-specific character set names (such as 8859-1). In this
    /// specific case, you'd put this into your configuration file: iconv-hook iso-8859-1 8859-1
    /// message-hook [!]pattern command Before mutt displays (or formats for replying or forwarding) a
    /// message which matches the given pattern (or, when it is preceded by an exclamation mark, does
    /// not match the pattern), the given command is executed. When multiple message-hooks match, they
    /// are executed in the order in which they occur in the configuration file. folder-hook [!]regexp
    /// command When mutt enters a folder which matches regexp (or, when regexp is preceded by an
    /// exclamation mark, does not match regexp), the given command is executed. When several
    /// folder-hooks match a given mail folder, they are executed in the order given in the
    /// configuration file. macro map key sequence [ description ] This command binds the given
    /// sequence of keys to the given key in the given map or maps. For valid maps, see bind. To
    /// specify multiple maps, put only a comma between the maps.
    MimeLookup,
    UnmimeLookup,

    /// color object foreground background [  regexp ]
    /// color index foreground background [  pattern ]
    /// uncolor index pattern [ pattern ... ]
    ///
    ///     If your terminal supports color, these commands can be used to assign foreground/background combinations to certain objects. Valid objects are: attachment, body, bold, header, hdrdefault, index, indicator, markers, message, normal, quoted, quotedN, search, signature, status, tilde, tree, underline. The body and header objects allow you to restrict the colorization to a regular expression. The index object permits you to select colored messages by pattern.
    ///
    ///     Valid colors include: white, black, green, magenta, blue, cyan, yellow, red, default, colorN.
    Color,
    Uncolor,

    /// mono object attribute [ regexp ]
    /// mono index attribute [ pattern ]
    //
    /// For terminals which don't support color, you can still assign attributes to objects. Valid
    /// attributes include: none, bold, underline, reverse, and standout. [un]ignore pattern [ pattern
    /// ... ] The ignore command permits you to specify header fields which you usually don't wish to
    /// see. Any header field whose tag begins with an "ignored" pattern will be ignored. The unignore
    /// command permits you to define exceptions from the above mentioned list of ignored headers.
    Mono,

    /// lists [-group name] regexp [ regexp ... ]
    /// unlists regexp [ regexp ... ]
    /// subscribe [-group name] regexp [ regexp ... ]
    /// unsubscribe regexp [ regexp ... ]
    ///
    /// Mutt maintains two lists of mailing list address patterns, a list of subscribed mailing lists,
    /// and a list of known mailing lists. All subscribed mailing lists are known. Patterns use regular
    /// expressions.
    ///
    /// The lists command adds a mailing list address to the list of known mailing lists. The unlists
    /// command removes a mailing list from the lists of known and subscribed mailing lists. The
    /// subscribe command adds a mailing list to the lists of known and subscribed mailing lists. The
    /// unsubscribe command removes it from the list of subscribed mailing lists. The -group flag adds
    /// all of the subsequent regular expressions to the named group.
    /// mbox-hook [!]pattern mailbox
    /// When mutt changes to a mail folder which matches pattern, mailbox will be used as the "mbox"
    /// folder, i.e., read messages will be moved to that folder when the mail folder is left.
    /// The first matching mbox-hook applies.
    Lists,
    Unlists,
    Subscribe,
    Unsubscribe,

    /// mailboxes filename [ filename ... ]
    /// unmailboxes [ * | filename ... ]
    ///
    /// The mailboxes specifies folders which can receive mail and which will be checked for new
    /// messages. When changing folders, pressing space will cycle through folders with new mail. The
    /// unmailboxes command is used to remove a file name from the list of folders which can receive
    /// mail. If "*" is specified as the file name, the list is emptied.
    Mailboxes,
    Unmailboxes,

    /// my_hdr string
    /// unmy_hdr field
    ///     Using my_hdr, you can define headers which will be added to the messages you compose. unmy_hdr will remove the given user-defined headers.
    MyHdr,
    UnmyHdr,

    /// hdr_order header1 header2 [ ... ]
    ///     With this command, you can specify an order in which mutt will attempt to present headers to you when viewing messages.
    HdrOrder,
    /// save-hook [!]pattern filename
    ///     When a message matches pattern, the default file name when saving it will be the given filename.
    SaveHook,
    /// fcc-hook [!]pattern filename
    ///     When an outgoing message matches pattern, the default file name for storing a copy (fcc) will be the given filename.
    FccHook,
    /// fcc-save-hook [!]pattern filename
    ///     This command is an abbreviation for identical fcc-hook and save-hook commands.
    FccSaveHook,
    /// send-hook [!]pattern command
    ///     When composing a message matching pattern, command is executed. When multiple send-hooks match, they are executed in the order in which they occur in the configuration file.
    SendHook,
    /// send2-hook [!]pattern command
    ///     Whenever a message matching pattern is changed (either by editing it or by using the compose menu), command is executed. When multiple send2-hooks match, they are executed in the order in which they occur in the configuration file. Possible applications include setting the $sendmail variable when a message's from header is changed.
    ///     send2-hook execution is not triggered by use of enter-command from the compose menu.
    Send2Hook,
    /// reply-hook [!]pattern command
    ///     When replying to a message matching pattern, command is executed. When multiple reply-hooks match, they are executed in the order in which they occur in the configuration file, but all reply-hooks are matched and executed before send-hooks, regardless of their order in the configuration file.
    ReplyHook,
    /// crypt-hook pattern key-id
    ///     The crypt-hook command provides a method by which you can specify the ID of the public key to be used when encrypting messages to a certain recipient. The meaning of "key ID" is to be taken broadly: This can be a different e-mail address, a numerical key ID, or even just an arbitrary search string.
    CryptHook,
    /// push string
    ///     This command adds the named string to the keyboard buffer.
    Push,

    /// set [no|inv|&|?]variable[=value] [ ... ]
    /// toggle variable [ ... ]
    /// unset variable [ ... ]
    /// reset variable [ ... ]
    /// These commands are used to set and manipulate configuration variables.
    ///
    /// Mutt knows four basic types of variables: boolean, number, string and quadoption. Boolean
    /// variables can be set (true), unset (false), or toggled. Number variables can be assigned a
    /// positive integer value.
    ///
    /// String variables consist of any number of printable characters. Strings must be enclosed in
    /// quotes if they contain spaces or tabs. You may also use the "C" escape sequences \n and \t for
    /// newline and tab, respectively.
    ///
    /// Quadoption variables are used to control whether or not to be prompted for certain actions, or
    /// to specify a default action. A value of yes will cause the action to be carried out
    /// automatically as if you had answered yes to the question. Similarly, a value of no will cause
    /// the the action to be carried out as if you had answered "no." A value of ask-yes will cause a
    /// prompt with a default answer of "yes" and ask-no will provide a default answer of "no."
    ///
    /// The reset command resets all given variables to the compile time defaults. If you reset the special variable all, all variables will reset to their compile time defaults.
    Set,
    Toggle,
    Unset,
    Reset,

    /// source filename
    /// The given file will be evaluated as a configuration file.
    Source,

    /// spam pattern format
    /// nospam pattern
    ///
    ///These commands define spam-detection patterns from external spam filters, so that mutt can sort,
    ///limit, and search on ''spam tags'' or ''spam attributes'', or display them in the index. See the
    ///Mutt manual for details.
    Spam,
    Nospam,

    /// unhook [ * | hook-type ]
    /// This command will remove all hooks of a given type, or all hooks when "*" is used as an
    /// argument. hook-type can be any of the -hook commands documented above.
    Unhook,
}

/*
Patterns

In various places with mutt, including some of the above mentioned hook commands, you can specify patterns to match messages.

Constructing Patterns

A simple pattern consists of an operator of the form "~character", possibly followed by a parameter against which mutt is supposed to match the object specified by this operator. For some characters, the ~ may be replaced by another character to alter the behavior of the match. These are described in the list of operators, below.

With some of these operators, the object to be matched consists of several e-mail addresses. In these cases, the object is matched if at least one of these e-mail addresses matches. You can prepend a hat ("^") character to such a pattern to indicate that all addresses must match in order to match the object.

You can construct complex patterns by combining simple patterns with logical operators. Logical AND is specified by simply concatenating two simple patterns, for instance "~C mutt-dev ~s bug". Logical OR is specified by inserting a vertical bar ("|") between two patterns, for instance "~C mutt-dev | ~s bug". Additionally, you can negate a pattern by prepending a bang ("!") character. For logical grouping, use braces ("()"). Example: "!(~t mutt|~c mutt) ~f elkins".

Simple Patterns

Mutt understands the following simple patterns:
~A

all messages

~b EXPR

messages which contain EXPR in the message body.

=b STRING

messages which contain STRING in the message body. If IMAP is enabled, searches for STRING on the server, rather than downloading each message and searching it locally.

~B EXPR

messages which contain EXPR in the whole message.

~c EXPR

messages carbon-copied to EXPR

%c GROUP

messages carbon-copied to any member of GROUP

~C EXPR

messages either to: or cc: EXPR

%C GROUP

messages either to: or cc: to any member of GROUP

~d MIN-MAX

messages with "date-sent" in a Date range

~D

deleted messages

~e EXPR

messages which contain EXPR in the "Sender" field

%e GROUP

messages which contain a member of GROUP in the "Sender" field

~E

expired messages

~f EXPR

messages originating from EXPR

%f GROUP

messages originating form any member of GROUP

~F

flagged messages

~g

PGP signed messages

~G

PGP encrypted messages

~h EXPR

messages which contain EXPR in the message header

~H EXPR

messages with spam tags matching EXPR

~i EXPR

messages which match EXPR in the "Message-ID" field

~k

messages containing PGP key material

~l

messages addressed to a known mailing list (defined by either subscribe or list)

~L EXPR

messages either originated or received by EXPR

%L GROUP

messages either originated or received by any member of GROUP

~m MIN-MAX

message in the range MIN to MAX

~n MIN-MAX

messages with a score in the range MIN to MAX

~N

new messages

~O

old messages

~p

messages addressed to you (as defined by alternates)

~P

messages from you (as defined by alternates)

~Q

messages which have been replied to

~r MIN-MAX

messages with "date-received" in a Date range

~R

read messages

~s EXPR

messages having EXPR in the "Subject" field.

~S

superseded messages

~t EXPR

messages addressed to EXPR

~T

tagged messages

~u

messages addressed to a subscribed mailing list (defined by subscribe commands)

~U

unread messages

~v

message is part of a collapsed thread.

~V

cryptographically verified messages

~x EXPR

messages which contain EXPR in the "References" or "In-Reply-To" field

~X MIN-MAX

messages with MIN - MAX attachments

~y EXPR

messages which contain EXPR in the "X-Label" field

~z MIN-MAX

messages with a size in the range MIN to MAX

~=

duplicated messages (see $duplicate_threads)

~$

unreferenced message (requires threaded view)

~(PATTERN)

messages in threads containing messages matching a certain pattern, e.g. all threads containing
messages from you: ~(~P)
In the above, EXPR is a regular expression.

With the ~d, ~m, ~n, ~r, ~X, and ~z operators, you can also specify ranges in the forms <MAX, >MIN, MIN-, and -MAX.

With the ~z operator, the suffixes "K" and "M" are allowed to specify kilobyte and megabyte respectively.

Matching dates

The ~d and ~r operators are used to match date ranges, which are interpreted to be given in your local time zone.

A date is of the form DD[/MM[/[cc]YY]], that is, a two-digit date, optionally followed by a two-digit month, optionally followed by a year specifications. Omitted fields default to the current month and year.

Mutt understands either two or four digit year specifications. When given a two-digit year, mutt will interpret values less than 70 as lying in the 21st century (i.e., "38" means 2038 and not 1938, and "00" is interpreted as 2000), and values greater than or equal to 70 as lying in the 20th century.

Note that this behavior is Y2K compliant, but that mutt does have a Y2.07K problem.

If a date range consists of a single date, the operator in question will match that precise date. If the date range consists of a dash ("-"), followed by a date, this range will match any date before and up to the date given. Similarly, a date followed by a dash matches the date given and any later point of time. Two dates, separated by a dash, match any date which lies in the given range of time.

You can also modify any absolute date by giving an error range. An error range consists of one of the characters +, -, *, followed by a positive number, followed by one of the unit characters y, m, w, or d, specifying a unit of years, months, weeks, or days. + increases the maximum date matched by the given interval of time, - decreases the minimum date matched by the given interval of time, and * increases the maximum date and decreases the minimum date matched by the given interval of time. It is possible to give multiple error margins, which cumulate. Example: 1/1/2001-1w+2w*3d

You can also specify offsets relative to the current date. An offset is specified as one of the characters <, >, =, followed by a positive number, followed by one of the unit characters y, m, w, or d. > matches dates which are older than the specified amount of time, an offset which begins with the character < matches dates which are more recent than the specified amount of time, and an offset which begins with the character = matches points of time which are precisely the given amount of time ago.
*/

pub enum MuttrcConfigurationValues {
    /// abort_nosubject
    ///
    /// Type: quadoption
    /// Default: ask-yes
    ///
    /// If set to yes, when composing messages and no subject is given at the subject prompt, composition will be aborted. If set to no, composing messages with no subject given at the subject prompt will never be aborted.
    AbortNosubject,

    /// abort_unmodified
    ///
    /// Type: quadoption
    /// Default: yes
    ///
    /// If set to yes, composition will automatically abort after editing the message body if no changes are made to the file (this check only happens after the first edit of the file). When set to no, composition will never be aborted.
    AbortUnmodified,

    /// alias_file
    ///
    /// Type: path
    /// Default: "~/.muttrc"
    ///
    /// The default file in which to save aliases created by the <create-alias> function. Entries added to this file are encoded in the character set specified by $config_charset if it is set or the current character set otherwise.
    ///
    /// Note: Mutt will not automatically source this file; you must explicitly use the "source" command for it to be executed in case this option points to a dedicated alias file.
    ///
    /// The default for this option is the currently used muttrc file, or "~/.muttrc" if no user muttrc was found.
    AliasFile,

    /// alias_format
    ///
    /// Type: string
    /// Default: "%4n %2f %t %-10a   %r"
    ///
    /// Specifies the format of the data displayed for the "alias" menu. The following printf(3)-style sequences are available:
    /// %a
    ///
    /// alias name
    ///
    /// %f
    ///
    /// flags - currently, a "d" for an alias marked for deletion
    ///
    /// %n
    ///
    /// index number
    ///
    /// %r
    ///
    /// address which alias expands to
    ///
    /// %t
    ///
    /// character which indicates if the alias is tagged for inclusion
    AliasFormat,

    /// allow_8bit
    /// Type: boolean
    /// Default: yes
    ///
    /// Controls whether 8-bit data is converted to 7-bit using either Quoted- Printable or Base64 encoding when sending mail.
    Allow8bit,

    /// allow_ansi
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Controls whether ANSI color codes in messages (and color tags in rich text messages) are to be interpreted. Messages containing these codes are rare, but if this option is set, their text will be colored accordingly. Note that this may override your color choices, and even present a security problem, since a message could include a line like
    ///
    /// [-- PGP output follows ...
    ///
    /// and give it the same color as your attachment color (see also $crypt_timestamp).
    AllowAnsi,

    /// arrow_cursor
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, an arrow ("->") will be used to indicate the current entry in menus instead of highlighting the whole line. On slow network or modem links this will make response faster because there is less that has to be redrawn on the screen when moving to the next or previous entries in the menu.
    ArrowCursor,

    /// ascii_chars
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, Mutt will use plain ASCII characters when displaying thread and attachment trees, instead of the default ACS characters.
    AsciiChars,

    /// askbcc
    /// Type: boolean
    /// Default: no
    /// If set, Mutt will prompt you for blind-carbon-copy (Bcc) recipients before editing an outgoing message.
    Askbcc,
    /// askcc
    /// Type: boolean
    /// Default: no
    /// If set, Mutt will prompt you for carbon-copy (Cc) recipients before editing the body of an outgoing message.
    Askcc,

    /// assumed_charset
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This variable is a colon-separated list of character encoding schemes for messages without character encoding indication. Header field values and message body content without character encoding indication would be assumed that they are written in one of this list. By default, all the header fields and message body without any charset indication are assumed to be in "us-ascii".
    ///
    /// For example, Japanese users might prefer this:
    ///
    /// set assumed_charset="iso-2022-jp:euc-jp:shift_jis:utf-8"
    ///
    /// However, only the first content is valid for the message body.
    AssumedCharset,

    /// attach_charset
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This variable is a colon-separated list of character encoding schemes for text file attachments. Mutt uses this setting to guess which encoding files being attached are encoded in to convert them to a proper character set given in $send_charset.
    ///
    /// If unset, the value of $charset will be used instead. For example, the following configuration would work for Japanese text handling:
    ///
    /// set attach_charset="iso-2022-jp:euc-jp:shift_jis:utf-8"
    ///
    /// Note: for Japanese users, "iso-2022-*" must be put at the head of the value as shown above if included.
    AttachCharset,

    /// attach_format
    ///
    /// Type: string
    /// Default: "%u%D%I %t%4n %T%.40d%> [%.7m/%.10M, %.6e%?C?, %C?, %s] "
    ///
    /// This variable describes the format of the "attachment" menu. The following printf(3)-style sequences are understood:
    /// %C
    ///
    /// charset
    ///
    /// %c
    ///
    /// requires charset conversion ("n" or "c")
    ///
    /// %D
    ///
    /// deleted flag
    ///
    /// %d
    ///
    /// description
    ///
    /// %e
    ///
    /// MIME content-transfer-encoding
    ///
    /// %f
    ///
    /// filename
    ///
    /// %I
    ///
    /// disposition ("I" for inline, "A" for attachment)
    ///
    /// %m
    ///
    /// major MIME type
    ///
    /// %M
    ///
    /// MIME subtype
    ///
    /// %n
    ///
    /// attachment number
    ///
    /// %Q
    ///
    /// "Q", if MIME part qualifies for attachment counting
    ///
    /// %s
    ///
    /// size
    ///
    /// %t
    ///
    /// tagged flag
    ///
    /// %T
    ///
    /// graphic tree characters
    ///
    /// %u
    ///
    /// unlink (=to delete) flag
    ///
    /// %X
    ///
    /// number of qualifying MIME parts in this part and its children (please see the "attachments" section for possible speed effects)
    ///
    /// %>X
    ///
    /// right justify the rest of the string and pad with character "X"
    ///
    /// %|X
    ///
    /// pad to the end of the line with character "X"
    ///
    /// %*X
    ///
    /// soft-fill with character "X" as pad
    /// For an explanation of "soft-fill", see the $index_format documentation.
    AttachFormat,

    /// attach_sep
    ///
    /// Type: string
    /// Default: "\n"
    ///
    /// The separator to add between attachments when operating (saving, printing, piping, etc) on a list of tagged attachments.
    AttachSep,

    /// attach_split
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If this variable is unset, when operating (saving, printing, piping, etc) on a list of tagged attachments, Mutt will concatenate the attachments and will operate on them as a single attachment. The $attach_sep separator is added after each attachment. When set, Mutt will operate on the attachments one by one.
    AttachSplit,

    /// attribution
    ///
    /// Type: string
    /// Default: "On %d, %n wrote:"
    ///
    /// This is the string that will precede a message which has been included in a reply. For a full listing of defined printf(3)-like sequences see the section on $index_format.
    Attribution,

    /// auto_tag
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, functions in the index menu which affect a message will be applied to all tagged messages (if there are any). When unset, you must first use the <tag-prefix> function (bound to ";" by default) to make the next function apply to all tagged messages.
    AutoTag,

    /// autoedit
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set along with $edit_headers, Mutt will skip the initial send-menu (prompting for subject and recipients) and allow you to immediately begin editing the body of your message. The send-menu may still be accessed once you have finished editing the body of your message.
    ///
    /// Note: when this option is set, you cannot use send-hooks that depend on the recipients when composing a new (non-reply) message, as the initial list of recipients is empty.
    ///
    /// Also see $fast_reply.
    Autoedit,

    /// beep
    /// Type: boolean
    /// Default: yes
    /// When this variable is set, mutt will beep when an error occurs.
    Beep,

    /// beep_new
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When this variable is set, mutt will beep whenever it prints a message notifying you of new mail. This is independent of the setting of the $beep variable.
    BeepNew,

    /// bounce
    /// Type: quadoption
    /// Default: ask-yes
    /// Controls whether you will be asked to confirm bouncing messages. If set to yes you don't get asked if you want to bounce a message. Setting this variable to no is not generally useful, and thus not recommended, because you are unable to bounce messages.
    Bounce,

    /// bounce_delivered
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When this variable is set, mutt will include Delivered-To headers when bouncing messages. Postfix users may wish to unset this variable.
    BounceDelivered,

    /// braille_friendly
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When this variable is set, mutt will place the cursor at the beginning of the current line in menus, even when the $arrow_cursor variable is unset, making it easier for blind persons using Braille displays to follow these menus. The option is unset by default because many visual terminals don't permit making the cursor invisible.
    BrailleFriendly,

    /// certificate_file
    ///
    /// Type: path
    /// Default: "~/.mutt_certificates"
    ///
    /// This variable specifies the file where the certificates you trust are saved. When an unknown certificate is encountered, you are asked if you accept it or not. If you accept it, the certificate can also be saved in this file and further connections are automatically accepted.
    ///
    /// You can also manually add CA certificates in this file. Any server certificate that is signed with one of these CA certificates is also automatically accepted.
    ///
    /// Example:
    ///
    /// set certificate_file=~/.mutt/certificates
    CertificateFile,

    /// charset
    ///
    /// Type: string
    /// Default: ""
    ///
    /// Character set your terminal uses to display and enter textual data. It is also the fallback for $send_charset.
    ///
    /// Upon startup Mutt tries to derive this value from environment variables such as $LC_CTYPE or $LANG.
    ///
    /// Note: It should only be set in case Mutt isn't abled to determine the character set used correctly.
    Charset,

    /// check_mbox_size
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When this variable is set, mutt will use file size attribute instead of access time when checking for new mail in mbox and mmdf folders.
    ///
    /// This variable is unset by default and should only be enabled when new mail detection for these folder types is unreliable or doesn't work.
    ///
    /// Note that enabling this variable should happen before any "mailboxes" directives occur in configuration files regarding mbox or mmdf folders because mutt needs to determine the initial new mail status of such a mailbox by performing a fast mailbox scan when it is defined. Afterwards the new mail status is tracked by file size changes.
    CheckMboxSize,

    /// check_new
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Note: this option only affects maildir and MH style mailboxes.
    ///
    /// When set, Mutt will check for new mail delivered while the mailbox is open. Especially with MH mailboxes, this operation can take quite some time since it involves scanning the directory and checking each file to see if it has already been looked at. If this variable is unset, no check for new mail is performed while the mailbox is open.
    CheckNew,

    /// collapse_unread
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When unset, Mutt will not collapse a thread if it contains any unread messages.
    CollapseUnread,

    /// compose_format
    ///
    /// Type: string
    /// Default: "-- Mutt: Compose  [Approx. msg size: %l   Atts: %a]%>-"
    ///
    /// Controls the format of the status line displayed in the "compose" menu. This string is similar to $status_format, but has its own set of printf(3)-like sequences:
    /// %a
    ///
    /// total number of attachments
    ///
    /// %h
    ///
    /// local hostname
    ///
    /// %l
    ///
    /// approximate size (in bytes) of the current message
    ///
    /// %v
    ///
    /// Mutt version string
    /// See the text describing the $status_format option for more information on how to set $compose_format.
    ComposeFormat,

    /// config_charset
    ///
    /// Type: string
    /// Default: ""
    ///
    /// When defined, Mutt will recode commands in rc files from this encoding to the current character set as specified by $charset and aliases written to $alias_file from the current character set.
    ///
    /// Please note that if setting $charset it must be done before setting $config_charset.
    ///
    /// Recoding should be avoided as it may render unconvertable characters as question marks which can lead to undesired side effects (for example in regular expressions).
    ConfigCharset,

    /// confirmappend
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, Mutt will prompt for confirmation when appending messages to an existing mailbox.
    ConfirmAppend,
    /// confirmcreate
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, Mutt will prompt for confirmation when saving messages to a mailbox which does not yet exist before creating it.
    ConfirmCreate,

    /// connect_timeout
    ///
    /// Type: number
    /// Default: 30
    ///
    /// Causes Mutt to timeout a network connection (for IMAP, POP or SMTP) after this many seconds if the connection is not able to be established. A negative value causes Mutt to wait indefinitely for the connection attempt to succeed.
    ConnectTimeout,

    /// content_type
    ///
    /// Type: string
    /// Default: "text/plain"
    ///
    /// Sets the default Content-Type for the body of newly composed messages.
    ContentType,

    /// copy
    /// Type: quadoption
    /// Default: yes
    /// This variable controls whether or not copies of your outgoing messages will be saved for later references. Also see $record, $save_name, $force_name and "fcc-hook".
    Copy,

    /// crypt_autoencrypt
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Setting this variable will cause Mutt to always attempt to PGP encrypt outgoing messages. This is probably only useful in connection to the "send-hook" command. It can be overridden by use of the pgp menu, when encryption is not required or signing is requested as well. If $smime_is_default is set, then OpenSSL is used instead to create S/MIME messages and settings can be overridden by use of the smime menu instead. (Crypto only)
    CryptAutoencrypt,

    /// crypt_autopgp
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// This variable controls whether or not mutt may automatically enable PGP encryption/signing for messages. See also $crypt_autoencrypt, $crypt_replyencrypt, $crypt_autosign, $crypt_replysign and $smime_is_default.
    CryptAutopgp,

    /// crypt_autosign
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Setting this variable will cause Mutt to always attempt to cryptographically sign outgoing messages. This can be overridden by use of the pgp menu, when signing is not required or encryption is requested as well. If $smime_is_default is set, then OpenSSL is used instead to create S/MIME messages and settings can be overridden by use of the smime menu instead of the pgp menu. (Crypto only)
    CryptAutosign,

    /// crypt_autosmime
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// This variable controls whether or not mutt may automatically enable S/MIME encryption/signing for messages. See also $crypt_autoencrypt, $crypt_replyencrypt, $crypt_autosign, $crypt_replysign and $smime_is_default.
    CryptAutosmime,

    /// crypt_replyencrypt
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set, automatically PGP or OpenSSL encrypt replies to messages which are encrypted. (Crypto only)
    CryptReplyencrypt,

    /// crypt_replysign
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, automatically PGP or OpenSSL sign replies to messages which are signed.
    ///
    /// Note: this does not work on messages that are encrypted and signed! (Crypto only)
    CryptReplysign,

    /// crypt_replysignencrypted
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, automatically PGP or OpenSSL sign replies to messages which are encrypted. This makes sense in combination with $crypt_replyencrypt, because it allows you to sign all messages which are automatically encrypted. This works around the problem noted in $crypt_replysign, that mutt is not able to find out whether an encrypted message is also signed. (Crypto only)
    CryptReplysignencrypted,

    /// crypt_timestamp
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set, mutt will include a time stamp in the lines surrounding PGP or S/MIME output, so spoofing such lines is more difficult. If you are using colors to mark these lines, and rely on these, you may unset this setting. (Crypto only)
    CryptTimestamp,

    /// crypt_use_gpgme
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// This variable controls the use of the GPGME-enabled crypto backends. If it is set and Mutt was built with gpgme support, the gpgme code for S/MIME and PGP will be used instead of the classic code. Note that you need to set this option in .muttrc; it won't have any effect when used interactively.
    CryptuseGpgme,

    /// crypt_use_pka
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Controls whether mutt uses PKA (see http://www.g10code.de/docs/pka-intro.de.pdf) during signature verification (only supported by the GPGME backend).
    CryptusePka,

    /// crypt_verify_sig
    ///
    /// Type: quadoption
    /// Default: yes
    ///
    /// If "yes", always attempt to verify PGP or S/MIME signatures. If "ask-*", ask whether or not to verify the signature. If \Fi"no", never attempt to verify cryptographic signatures. (Crypto only)
    CryptverifySig,

    /// date_format
    ///
    /// Type: string
    /// Default: "!%a, %b %d, %Y at %I:%M:%S%p %Z"
    ///
    /// This variable controls the format of the date printed by the "%d" sequence in $index_format. This is passed to the strftime(3) function to process the date, see the man page for the proper syntax.
    ///
    /// Unless the first character in the string is a bang ("!"), the month and week day names are expanded according to the locale specified in the variable $locale. If the first character in the string is a bang, the bang is discarded, and the month and week day names in the rest of the string are expanded in the C locale (that is in US English).
    DateFormat,

    /// default_hook
    ///
    /// Type: string
    /// Default: "~f %s !~P | (~P ~C %s)"
    ///
    /// This variable controls how "message-hook", "reply-hook", "send-hook", "send2-hook", "save-hook", and "fcc-hook" will be interpreted if they are specified with only a simple regexp, instead of a matching pattern. The hooks are expanded when they are declared, so a hook will be interpreted according to the value of this variable at the time the hook is declared.
    ///
    /// The default value matches if the message is either from a user matching the regular expression given, or if it is from you (if the from address matches "alternates") and is to or cc'ed to a user matching the given regular expression.
    DefaultHook,

    /// delete
    /// Type: quadoption
    /// Default: ask-yes
    /// Controls whether or not messages are really deleted when closing or synchronizing a mailbox. If set to yes, messages marked for deleting will automatically be purged without prompting. If set to no, messages marked for deletion will be kept in the mailbox.
    Delete,

    /// delete_untag
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If this option is set, mutt will untag messages when marking them for deletion. This applies when you either explicitly delete a message, or when you save it to another folder.
    DeleteUntag,

    /// digest_collapse
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If this option is set, mutt's received-attachments menu will not show the subparts of individual messages in a multipart/digest. To see these subparts, press "v" on that menu.
    DigestCollapse,

    /// display_filter
    ///
    /// Type: path
    /// Default: ""
    ///
    /// When set, specifies a command used to filter messages. When a message is viewed it is passed as standard input to $display_filter, and the filtered message is read from the standard output.
    DisplayFilter,

    /// dsn_notify
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This variable sets the request for when notification is returned. The string consists of a comma separated list (no spaces!) of one or more of the following: never, to never request notification, failure, to request notification on transmission failure, delay, to be notified of message delays, success, to be notified of successful transmission.
    ///
    /// Example:
    ///
    /// set dsn_notify="failure,delay"
    ///
    /// Note: when using $sendmail for delivery, you should not enable this unless you are either using Sendmail 8.8.x or greater or a MTA providing a sendmail(1)-compatible interface supporting the -N option for DSN. For SMTP delivery, DSN support is auto-detected so that it depends on the server whether DSN will be used or not.
    DsnNotify,

    /// dsn_return
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This variable controls how much of your message is returned in DSN messages. It may be set to either hdrs to return just the message header, or full to return the full message.
    ///
    /// Example:
    ///
    /// set dsn_return=hdrs
    ///
    /// Note: when using $sendmail for delivery, you should not enable this unless you are either using Sendmail 8.8.x or greater or a MTA providing a sendmail(1)-compatible interface supporting the -R option for DSN. For SMTP delivery, DSN support is auto-detected so that it depends on the server whether DSN will be used or not.
    DsnReturn,

    /// duplicate_threads
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// This variable controls whether mutt, when $sort is set to threads, threads messages with the same Message-Id together. If it is set, it will indicate that it thinks they are duplicates of each other with an equals sign in the thread tree.
    DuplicateThreads,

    /// edit_headers
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// This option allows you to edit the header of your outgoing messages along with the body of your message.
    ///
    /// Note that changes made to the References: and Date: headers are ignored for interoperability reasons.
    EditHeaders,

    /// editor
    /// Type: path
    /// Default: ""
    /// This variable specifies which editor is used by mutt. It defaults to the value of the $VISUAL, or $EDITOR, environment variable, or to the string "vi" if neither of those are set.
    Editor,

    /// encode_from
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, mutt will quoted-printable encode messages when they contain the string "From " (note the trailing space) in the beginning of a line. This is useful to avoid the tampering certain mail delivery and transport agents tend to do with messages (in order to prevent tools from misinterpreting the line as a mbox message separator).
    EncodeFrom,

    /// envelope_from_address
    ///
    /// Type: e-mail address
    /// Default: ""
    ///
    /// Manually sets the envelope sender for outgoing messages. This value is ignored if $use_envelope_from is unset.
    EnvelopeFromAddress,

    /// escape
    /// Type: string
    /// Default: "~"
    /// Escape character to use for functions in the built-in editor.
    Escape,

    /// fast_reply
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, the initial prompt for recipients and subject are skipped when replying to messages, and the initial prompt for subject is skipped when forwarding messages.
    ///
    /// Note: this variable has no effect when the $autoedit variable is set.
    FastReply,

    /// fcc_attach
    ///
    /// Type: quadoption
    /// Default: yes
    ///
    /// This variable controls whether or not attachments on outgoing messages are saved along with the main body of your message.
    FccAttach,

    /// fcc_clear
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When this variable is set, FCCs will be stored unencrypted and unsigned, even when the actual message is encrypted and/or signed. (PGP only)
    FccClear,

    /// folder
    /// Type: path
    /// Default: "~/Mail"
    /// Specifies the default location of your mailboxes. A "+" or "=" at the beginning of a pathname will be expanded to the value of this variable. Note that if you change this variable (from the default) value you need to make sure that the assignment occurs before you use "+" or "=" for any other variables since expansion takes place when handling the "mailboxes" command.
    Folder,

    /// folder_format
    ///
    /// Type: string
    /// Default: "%2C %t %N %F %2l %-8.8u %-8.8g %8s %d %f"
    ///
    /// This variable allows you to customize the file browser display to your personal taste. This string is similar to $index_format, but has its own set of printf(3)-like sequences:
    /// %C
    ///
    /// current file number
    ///
    /// %d
    ///
    /// date/time folder was last modified
    ///
    /// %f
    ///
    /// filename ("/" is appended to directory names, "@" to symbolic links and "*" to executable files)
    ///
    /// %F
    ///
    /// file permissions
    ///
    /// %g
    ///
    /// group name (or numeric gid, if missing)
    ///
    /// %l
    ///
    /// number of hard links
    ///
    /// %N
    ///
    /// N if folder has new mail, blank otherwise
    ///
    /// %s
    ///
    /// size in bytes
    ///
    /// %t
    ///
    /// "*" if the file is tagged, blank otherwise
    ///
    /// %u
    ///
    /// owner name (or numeric uid, if missing)
    ///
    /// %>X
    ///
    /// right justify the rest of the string and pad with character "X"
    ///
    /// %|X
    ///
    /// pad to the end of the line with character "X"
    ///
    /// %*X
    ///
    /// soft-fill with character "X" as pad
    /// For an explanation of "soft-fill", see the $index_format documentation.
    FolderFormat,

    /// followup_to
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Controls whether or not the "Mail-Followup-To:" header field is generated when sending mail. When set, Mutt will generate this field when you are replying to a known mailing list, specified with the "subscribe" or "lists" commands.
    ///
    /// This field has two purposes. First, preventing you from receiving duplicate copies of replies to messages which you send to mailing lists, and second, ensuring that you do get a reply separately for any messages sent to known lists to which you are not subscribed.
    ///
    /// The header will contain only the list's address for subscribed lists, and both the list address and your own email address for unsubscribed lists. Without this header, a group reply to your message sent to a subscribed list will be sent to both the list and your address, resulting in two copies of the same email for you.
    FollowupTo,

    /// force_name
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// This variable is similar to $save_name, except that Mutt will store a copy of your outgoing message by the username of the address you are sending to even if that mailbox does not exist.
    ///
    /// Also see the $record variable.
    ForceName,

    /// forward_decode
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Controls the decoding of complex MIME messages into text/plain when forwarding a message. The message header is also RFC2047 decoded. This variable is only used, if $mime_forward is unset, otherwise $mime_forward_decode is used instead.
    ForwardDecode,

    /// forward_decrypt
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Controls the handling of encrypted messages when forwarding a message. When set, the outer layer of encryption is stripped off. This variable is only used if $mime_forward is set and $mime_forward_decode is unset. (PGP only)
    ForwardDecrypt,

    /// forward_edit
    ///
    /// Type: quadoption
    /// Default: yes
    ///
    /// This quadoption controls whether or not the user is automatically placed in the editor when forwarding messages. For those who always want to forward with no modification, use a setting of "no".
    ForwardEdit,

    /// forward_format
    ///
    /// Type: string
    /// Default: "[%a: %s]"
    ///
    /// This variable controls the default subject when forwarding a message. It uses the same format sequences as the $index_format variable.
    ForwardFormat,

    /// forward_quote
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, forwarded messages included in the main body of the message (when $mime_forward is unset) will be quoted using $indent_string.
    ForwardQuote,

    /// from
    /// Type: e-mail address
    /// Default: ""
    /// When set, this variable contains a default from address. It can be overridden using "my_hdr" (including from a "send-hook") and $reverse_name. This variable is ignored if $use_from is unset.
    ///
    /// This setting defaults to the contents of the environment variable $EMAIL.
    From,

    /// gecos_mask
    ///
    /// Type: regular expression
    /// Default: "^[^,]*"
    ///
    /// A regular expression used by mutt to parse the GECOS field of a password entry when expanding the alias. The default value will return the string up to the first "," encountered. If the GECOS field contains a string like "lastname, firstname" then you should set it to ".*".
    ///
    /// This can be useful if you see the following behavior: you address an e-mail to user ID "stevef" whose full name is "Steve Franklin". If mutt expands "stevef" to ""Franklin" stevef@foo.bar" then you should set the $gecos_mask to a regular expression that will match the whole name so mutt will expand "Franklin" to "Franklin, Steve".
    GecosMask,

    /// hdrs
    /// Type: boolean
    /// Default: yes
    /// When unset, the header fields normally added by the "my_hdr" command are not created. This variable must be unset before composing a new message or replying in order to take effect. If set, the user defined header fields are added to every new message.
    Hdrs,

    /// header
    /// Type: boolean
    /// Default: no
    /// When set, this variable causes Mutt to include the header of the message you are replying to into the edit buffer. The $weed setting applies.
    Header,

    /// header_cache
    ///
    /// Type: path
    /// Default: ""
    ///
    /// This variable points to the header cache database. If pointing to a directory Mutt will contain a header cache database file per folder, if pointing to a file that file will be a single global header cache. By default it is unset so no header caching will be used.
    ///
    /// Header caching can greatly improve speed when opening POP, IMAP MH or Maildir folders, see "caching" for details.
    HeaderCache,

    /// header_cache_compress
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When mutt is compiled with qdbm or tokyocabinet as header cache backend, this option determines whether the database will be compressed. Compression results in database files roughly being one fifth of the usual diskspace, but the decompression can result in a slower opening of cached folder(s) which in general is still much faster than opening non header cached folders.
    HeaderCacheCompress,

    /// help
    /// Type: boolean
    /// Default: yes
    /// When set, help lines describing the bindings for the major functions provided by each menu are displayed on the first line of the screen.
    ///
    /// Note: The binding will not be displayed correctly if the function is bound to a sequence rather than a single keystroke. Also, the help line may not be updated if a binding is changed while Mutt is running. Since this variable is primarily aimed at new users, neither of these should present a major problem.
    Help,

    /// hidden_host
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, mutt will skip the host name part of $hostname variable when adding the domain part to addresses. This variable does not affect the generation of Message-IDs, and it will not lead to the cut-off of first-level domains.
    HiddenHost,

    /// hide_limited
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, mutt will not show the presence of messages that are hidden by limiting, in the thread tree.
    HideLimited,

    /// hide_missing
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, mutt will not show the presence of missing messages in the thread tree.
    HideMissing,

    /// hide_thread_subject
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, mutt will not show the subject of messages in the thread tree that have the same subject as their parent or closest previously displayed sibling.
    HideThreadSubject,

    /// hide_top_limited
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, mutt will not show the presence of messages that are hidden by limiting, at the top of threads in the thread tree. Note that when $hide_limited is set, this option will have no effect.
    HideTopLimited,

    /// hide_top_missing
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, mutt will not show the presence of missing messages at the top of threads in the thread tree. Note that when $hide_missing is set, this option will have no effect.
    HideTopMissing,

    /// history
    ///
    /// Type: number
    /// Default: 10
    ///
    /// This variable controls the size (in number of strings remembered) of the string history buffer per category. The buffer is cleared each time the variable is set.
    History,

    /// history_file
    ///
    /// Type: path
    /// Default: "~/.mutthistory"
    ///
    /// The file in which Mutt will save its history.
    HistoryFile,

    /// honor_disposition
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, Mutt will not display attachments with a disposition of "attachment" inline even if it could render the part to plain text. These MIME parts can only be viewed from the attachment menu.
    ///
    /// If unset, Mutt will render all MIME parts it can properly transform to plain text.
    HonorDisposition,

    /// honor_followup_to
    ///
    /// Type: quadoption
    /// Default: yes
    ///
    /// This variable controls whether or not a Mail-Followup-To header is honored when group-replying to a message.
    HonorFollowupTo,

    /// hostname
    ///
    /// Type: string
    /// Default: ""
    ///
    /// Specifies the fully-qualified hostname of the system mutt is running on containing the host's name and the DNS domain it belongs to. It is used as the domain part (after "@") for local email addresses as well as Message-Id headers.
    ///
    /// Its value is determined at startup as follows: If the node's name as returned by the uname(3) function contains the hostname and the domain, these are used to construct $hostname. If there is no domain part returned, Mutt will look for a "domain" or "search" line in /etc/resolv.conf to determine the domain. Optionally, Mutt can be compiled with a fixed domain name in which case a detected one is not used.
    ///
    /// Also see $use_domain and $hidden_host.
    Hostname,

    /// ignore_linear_white_space
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// This option replaces linear-white-space between encoded-word and text to a single space to prevent the display of MIME-encoded "Subject:" field from being divided into multiple lines.
    IgnoreLinearWhiteSpace,

    /// ignore_list_reply_to
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Affects the behavior of the <reply> function when replying to messages from mailing lists (as defined by the "subscribe" or "lists" commands). When set, if the "Reply-To:" field is set to the same value as the "To:" field, Mutt assumes that the "Reply-To:" field was set by the mailing list to automate responses to the list, and will ignore this field. To direct a response to the mailing list when this option is set, use the <list-reply> function; <group-reply> will reply to both the sender and the list.
    IgnoreListReplyTo,

    /// imap_authenticators
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This is a colon-delimited list of authentication methods mutt may attempt to use to log in to an IMAP server, in the order mutt should try them. Authentication methods are either "login" or the right side of an IMAP "AUTH=xxx" capability string, e.g. "digest-md5", "gssapi" or "cram-md5". This option is case-insensitive. If it's unset (the default) mutt will try all available methods, in order from most-secure to least-secure.
    ///
    /// Example:
    ///
    /// set imap_authenticators="gssapi:cram-md5:login"
    ///
    /// Note: Mutt will only fall back to other authentication methods if the previous methods are unavailable. If a method is available but authentication fails, mutt will not connect to the IMAP server.
    ImapAuthenticators,

    /// imap_check_subscribed
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, mutt will fetch the set of subscribed folders from your server on connection, and add them to the set of mailboxes it polls for new mail just as if you had issued individual "mailboxes" commands.
    ImapCheckSubscribed,

    /// imap_delim_chars
    ///
    /// Type: string
    /// Default: "/."
    ///
    /// This contains the list of characters which you would like to treat as folder separators for displaying IMAP paths. In particular it helps in using the "=" shortcut for your folder variable.
    ImapDelimChars,

    /// imap_headers
    ///
    /// Type: string
    /// Default: ""
    ///
    /// Mutt requests these header fields in addition to the default headers ("Date:", "From:", "Subject:", "To:", "Cc:", "Message-Id:", "References:", "Content-Type:", "Content-Description:", "In-Reply-To:", "Reply-To:", "Lines:", "List-Post:", "X-Label:") from IMAP servers before displaying the index menu. You may want to add more headers for spam detection.
    ///
    /// Note: This is a space separated list, items should be uppercase and not contain the colon, e.g. "X-BOGOSITY X-SPAM-STATUS" for the "X-Bogosity:" and "X-Spam-Status:" header fields.
    ImapHeaders,

    /// imap_idle
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, mutt will attempt to use the IMAP IDLE extension to check for new mail in the current mailbox. Some servers (dovecot was the inspiration for this option) react badly to mutt's implementation. If your connection seems to freeze up periodically, try unsetting this.
    ImapIdle,

    /// imap_keepalive
    ///
    /// Type: number
    /// Default: 900
    ///
    /// This variable specifies the maximum amount of time in seconds that mutt will wait before polling open IMAP connections, to prevent the server from closing them before mutt has finished with them. The default is well within the RFC-specified minimum amount of time (30 minutes) before a server is allowed to do this, but in practice the RFC does get violated every now and then. Reduce this number if you find yourself getting disconnected from your IMAP server due to inactivity.
    ImapKeepalive,

    /// imap_list_subscribed
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// This variable configures whether IMAP folder browsing will look for only subscribed folders or all folders. This can be toggled in the IMAP browser with the <toggle-subscribed> function.
    ImapListSubscribed,

    /// imap_login
    ///
    /// Type: string
    /// Default: ""
    ///
    /// Your login name on the IMAP server.
    ///
    /// This variable defaults to the value of $imap_user.
    ImapLogin,

    /// imap_pass
    ///
    /// Type: string
    /// Default: ""
    ///
    /// Specifies the password for your IMAP account. If unset, Mutt will prompt you for your password when you invoke the <imap-fetch-mail> function or try to open an IMAP folder.
    ///
    /// Warning: you should only use this option when you are on a fairly secure machine, because the superuser can read your muttrc even if you are the only one who can read the file.
    ImapPass,

    /// imap_passive
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, mutt will not open new IMAP connections to check for new mail. Mutt will only check for new mail over existing IMAP connections. This is useful if you don't want to be prompted to user/password pairs on mutt invocation, or if opening the connection is slow.
    ImapPassive,

    /// imap_peek
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, mutt will avoid implicitly marking your mail as read whenever you fetch a message from the server. This is generally a good thing, but can make closing an IMAP folder somewhat slower. This option exists to appease speed freaks.
    ImapPeek,

    /// imap_pipeline_depth
    ///
    /// Type: number
    /// Default: 15
    ///
    /// Controls the number of IMAP commands that may be queued up before they are sent to the server. A deeper pipeline reduces the amount of time mutt must wait for the server, and can make IMAP servers feel much more responsive. But not all servers correctly handle pipelined commands, so if you have problems you might want to try setting this variable to 0.
    ///
    /// Note: Changes to this variable have no effect on open connections.
    ImapPipelineDepth,

    /// imap_servernoise
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, mutt will display warning messages from the IMAP server as error messages. Since these messages are often harmless, or generated due to configuration problems on the server which are out of the users' hands, you may wish to suppress them at some point.
    ImapServernoise,

    /// imap_user
    ///
    /// Type: string
    /// Default: ""
    ///
    /// The name of the user whose mail you intend to access on the IMAP server.
    ///
    /// This variable defaults to your user name on the local machine.
    ImapUser,

    /// implicit_autoview
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set to "yes", mutt will look for a mailcap entry with the "copiousoutput" flag set for every MIME attachment it doesn't have an internal viewer defined for. If such an entry is found, mutt will use the viewer defined in that entry to convert the body part to text form.
    ImplicitAutoview,

    /// include
    ///
    /// Type: quadoption
    /// Default: ask-yes
    ///
    /// Controls whether or not a copy of the message(s) you are replying to is included in your reply.
    Include,

    /// include_onlyfirst
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Controls whether or not Mutt includes only the first attachment of the message you are replying.
    IncludeOnlyfirst,

    /// indent_string
    ///
    /// Type: string
    /// Default: "> "
    ///
    /// Specifies the string to prepend to each line of text quoted in a message to which you are replying. You are strongly encouraged not to change this value, as it tends to agitate the more fanatical netizens.
    ///
    /// The value of this option is ignored if $text_flowed is set, too because the quoting mechanism is strictly defined for format=flowed.
    ///
    /// This option is a format string, please see the description of $index_format for supported printf(3)-style sequences.
    IndentString,

    /// index_format
    ///
    /// Type: string
    /// Default: "%4C %Z %{%b %d} %-15.15L (%?l?%4l&%4c?) %s"
    ///
    /// This variable allows you to customize the message index display to your personal taste.
    ///
    /// "Format strings" are similar to the strings used in the C function printf(3) to format output (see the man page for more details). The following sequences are defined in Mutt:
    /// %a
    ///
    /// address of the author
    ///
    /// %A
    ///
    /// reply-to address (if present; otherwise: address of author)
    ///
    /// %b
    ///
    /// filename of the original message folder (think mailbox)
    ///
    /// %B
    ///
    /// the list to which the letter was sent, or else the folder name (%b).
    ///
    /// %c
    ///
    /// number of characters (bytes) in the message
    ///
    /// %C
    ///
    /// current message number
    ///
    /// %d
    ///
    /// date and time of the message in the format specified by $date_format converted to sender's time zone
    ///
    /// %D
    ///
    /// date and time of the message in the format specified by $date_format converted to the local time zone
    ///
    /// %e
    ///
    /// current message number in thread
    ///
    /// %E
    ///
    /// number of messages in current thread
    ///
    /// %f
    ///
    /// sender (address + real name), either From: or Return-Path:
    ///
    /// %F
    ///
    /// author name, or recipient name if the message is from you
    ///
    /// %H
    ///
    /// spam attribute(s) of this message
    ///
    /// %i
    ///
    /// message-id of the current message
    ///
    /// %l
    ///
    /// number of lines in the message (does not work with maildir, mh, and possibly IMAP folders)
    ///
    /// %L
    ///
    /// If an address in the "To:" or "Cc:" header field matches an address defined by the users "subscribe" command, this displays "To <list-name>", otherwise the same as %F.
    ///
    /// %m
    ///
    /// total number of message in the mailbox
    ///
    /// %M
    ///
    /// number of hidden messages if the thread is collapsed.
    ///
    /// %N
    ///
    /// message score
    ///
    /// %n
    ///
    /// author's real name (or address if missing)
    ///
    /// %O
    ///
    /// original save folder where mutt would formerly have stashed the message: list name or recipient name if not sent to a list
    ///
    /// %P
    ///
    /// progress indicator for the built-in pager (how much of the file has been displayed)
    ///
    /// %s
    ///
    /// subject of the message
    ///
    /// %S
    ///
    /// status of the message ("N"/"D"/"d"/"!"/"r"/*)
    ///
    /// %t
    ///
    /// "To:" field (recipients)
    ///
    /// %T
    ///
    /// the appropriate character from the $to_chars string
    ///
    /// %u
    ///
    /// user (login) name of the author
    ///
    /// %v
    ///
    /// first name of the author, or the recipient if the message is from you
    ///
    /// %X
    ///
    /// number of attachments (please see the "attachments" section for possible speed effects)
    ///
    /// %y
    ///
    /// "X-Label:" field, if present
    ///
    /// %Y
    ///
    /// "X-Label:" field, if present, and (1) not at part of a thread tree, (2) at the top of a thread, or (3) "X-Label:" is different from preceding message's "X-Label:".
    ///
    /// %Z
    ///
    /// message status flags
    ///
    /// %{fmt}
    ///
    /// the date and time of the message is converted to sender's time zone, and "fmt" is expanded by the library function strftime(3); a leading bang disables locales
    ///
    /// %[fmt]
    ///
    /// the date and time of the message is converted to the local time zone, and "fmt" is expanded by the library function strftime(3); a leading bang disables locales
    ///
    /// %(fmt)
    ///
    /// the local date and time when the message was received. "fmt" is expanded by the library function strftime(3); a leading bang disables locales
    ///
    /// %<fmt>
    ///
    /// the current local time. "fmt" is expanded by the library function strftime(3); a leading bang disables locales.
    ///
    /// %>X
    ///
    /// right justify the rest of the string and pad with character "X"
    ///
    /// %|X
    ///
    /// pad to the end of the line with character "X"
    ///
    /// %*X
    ///
    /// soft-fill with character "X" as pad
    /// "Soft-fill" deserves some explanation: Normal right-justification will print everything to the left of the "%>", displaying padding and whatever lies to the right only if there's room. By contrast, soft-fill gives priority to the right-hand side, guaranteeing space to display it and showing padding only if there's still room. If necessary, soft-fill will eat text leftwards to make room for rightward text.
    ///
    /// Note that these expandos are supported in "save-hook", "fcc-hook" and "fcc-save-hook", too.
    IndexFormat,

    /// ispell
    /// Type: path
    /// Default: "/usr/bin/hunspell"
    /// How to invoke ispell (GNU's spell-checking software).
    Ispell,

    /// keep_flagged
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, read messages marked as flagged will not be moved from your spool mailbox to your $mbox mailbox, or as a result of a "mbox-hook" command.
    KeepFlagged,

    /// locale
    /// Type: string
    /// Default: "C"
    /// The locale used by strftime(3) to format dates. Legal values are the strings your system accepts for the locale environment variable $LC_TIME.
    Locale,

    /// mail_check
    ///
    /// Type: number
    /// Default: 5
    ///
    /// This variable configures how often (in seconds) mutt should look for new mail. Also see the $timeout variable.
    MailCheck,

    /// mailcap_path
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This variable specifies which files to consult when attempting to display MIME bodies not directly supported by Mutt.
    MailcapPath,

    /// mailcap_sanitize
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set, mutt will restrict possible characters in mailcap % expandos to a well-defined set of safe characters. This is the safe setting, but we are not sure it doesn't break some more advanced MIME stuff.
    ///
    /// DON'T CHANGE THIS SETTING UNLESS YOU ARE REALLY SURE WHAT YOU ARE DOING!
    MailcapSanitize,

    /// maildir_header_cache_verify
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Check for Maildir unaware programs other than mutt having modified maildir files when the header cache is in use. This incurs one stat(2) per message every time the folder is opened (which can be very slow for NFS folders).
    MaildirHeaderCacheVerify,

    /// maildir_trash
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, messages marked as deleted will be saved with the maildir trashed flag instead of unlinked. Note: this only applies to maildir-style mailboxes. Setting it will have no effect on other mailbox types.
    MaildirTrash,

    /// mark_old
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Controls whether or not mutt marks new unread messages as old if you exit a mailbox without reading them. With this option set, the next time you start mutt, the messages will show up with an "O" next to them in the index menu, indicating that they are old.
    MarkOld,

    /// markers
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Controls the display of wrapped lines in the internal pager. If set, a "+" marker is displayed at the beginning of wrapped lines.
    ///
    /// Also see the $smart_wrap variable.
    Markers,

    /// mask
    /// Type: regular expression
    /// Default: "!^\.[^.]"
    /// A regular expression used in the file browser, optionally preceded by the not operator "!". Only files whose names match this mask will be shown. The match is always case-sensitive.
    Mask,

    /// mbox
    /// Type: path
    /// Default: "~/mbox"
    /// This specifies the folder into which read mail in your $spoolfile folder will be appended.
    ///
    /// Also see the $move variable.
    Mbox,

    /// mbox_type
    ///
    /// Type: folder magic
    /// Default: mbox
    ///
    /// The default mailbox type used when creating new folders. May be any of "mbox", "MMDF", "MH" and "Maildir". This is overridden by the -m command-line option.
    MboxType,

    /// menu_context
    ///
    /// Type: number
    /// Default: 0
    ///
    /// This variable controls the number of lines of context that are given when scrolling through menus. (Similar to $pager_context.)
    MenuContext,

    /// menu_move_off
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When unset, the bottom entry of menus will never scroll up past the bottom of the screen, unless there are less entries than lines. When set, the bottom entry may move off the bottom.
    MenuMoveOff,

    /// menu_scroll
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, menus will be scrolled up or down one line when you attempt to move across a screen boundary. If unset, the screen is cleared and the next or previous page of the menu is displayed (useful for slow links to avoid many redraws).
    MenuScroll,

    /// message_cache_clean
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, mutt will clean out obsolete entries from the message cache when the mailbox is synchronized. You probably only want to set it every once in a while, since it can be a little slow (especially for large folders).
    MessageCacheClean,

    /// message_cachedir
    ///
    /// Type: path
    /// Default: ""
    ///
    /// Set this to a directory and mutt will cache copies of messages from your IMAP and POP servers here. You are free to remove entries at any time.
    ///
    /// When setting this variable to a directory, mutt needs to fetch every remote message only once and can perform regular expression searches as fast as for local folders.
    ///
    /// Also see the $message_cache_clean variable.
    MessageCachedir,

    /// message_format
    ///
    /// Type: string
    /// Default: "%s"
    ///
    /// This is the string displayed in the "attachment" menu for attachments of type message/rfc822. For a full listing of defined printf(3)-like sequences see the section on $index_format.
    MessageFormat,

    /// meta_key
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, forces Mutt to interpret keystrokes with the high bit (bit 8) set as if the user had pressed the Esc key and whatever key remains after having the high bit removed. For example, if the key pressed has an ASCII value of 0xf8, then this is treated as if the user had pressed Esc then "x". This is because the result of removing the high bit from 0xf8 is 0x78, which is the ASCII character "x".
    MetaKey,

    /// metoo
    /// Type: boolean
    /// Default: no
    /// If unset, Mutt will remove your address (see the "alternates" command) from the list of recipients when replying to a message.
    Metoo,

    /// mh_purge
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When unset, mutt will mimic mh's behavior and rename deleted messages to ,<old file name> in mh folders instead of really deleting them. This leaves the message on disk but makes programs reading the folder ignore it. If the variable is set, the message files will simply be deleted.
    ///
    /// This option is similar to $maildir_trash for Maildir folders.
    MhPurge,

    /// mh_seq_flagged
    ///
    /// Type: string
    /// Default: "flagged"
    ///
    /// The name of the MH sequence used for flagged messages.
    MhSeqFlagged,

    /// mh_seq_replied
    ///
    /// Type: string
    /// Default: "replied"
    ///
    /// The name of the MH sequence used to tag replied messages.
    MhSeqReplied,

    /// mh_seq_unseen
    ///
    /// Type: string
    /// Default: "unseen"
    ///
    /// The name of the MH sequence used for unseen messages.
    MhSeqUnseen,

    /// mime_forward
    ///
    /// Type: quadoption
    /// Default: no
    ///
    /// When set, the message you are forwarding will be attached as a separate message/rfc822 MIME part instead of included in the main body of the message. This is useful for forwarding MIME messages so the receiver can properly view the message as it was delivered to you. If you like to switch between MIME and not MIME from mail to mail, set this variable to "ask-no" or "ask-yes".
    ///
    /// Also see $forward_decode and $mime_forward_decode.
    MimeForward,

    /// mime_forward_decode
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Controls the decoding of complex MIME messages into text/plain when forwarding a message while $mime_forward is set. Otherwise $forward_decode is used instead.
    MimeForwardDecode,

    /// mime_forward_rest
    ///
    /// Type: quadoption
    /// Default: yes
    ///
    /// When forwarding multiple attachments of a MIME message from the attachment menu, attachments which cannot be decoded in a reasonable manner will be attached to the newly composed message if this option is set.
    MimeForwardRest,

    /// move
    /// Type: quadoption
    /// Default: no
    /// Controls whether or not Mutt will move read messages from your spool mailbox to your $mbox mailbox, or as a result of a "mbox-hook" command.
    Move,

    /// narrow_tree
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// This variable, when set, makes the thread tree narrower, allowing deeper threads to fit on the screen.
    NarrowTree,

    /// net_inc
    ///
    /// Type: number
    /// Default: 10
    ///
    /// Operations that expect to transfer a large amount of data over the network will update their progress every $net_inc kilobytes. If set to 0, no progress messages will be displayed.
    ///
    /// See also $read_inc, $write_inc and $net_inc.
    NetInc,

    /// pager
    /// Type: path
    /// Default: "builtin"
    /// This variable specifies which pager you would like to use to view messages. The value "builtin" means to use the built-in pager, otherwise this variable should specify the pathname of the external pager you would like to use.
    ///
    /// Using an external pager may have some disadvantages: Additional keystrokes are necessary because you can't call mutt functions directly from the pager, and screen resizes cause lines longer than the screen width to be badly formatted in the help menu.
    Pager,

    /// pager_context
    ///
    /// Type: number
    /// Default: 0
    ///
    /// This variable controls the number of lines of context that are given when displaying the next or previous page in the internal pager. By default, Mutt will display the line after the last one on the screen at the top of the next page (0 lines of context).
    ///
    /// This variable also specifies the amount of context given for search results. If positive, this many lines will be given before a match, if 0, the match will be top-aligned.
    PagerContext,

    /// pager_format
    ///
    /// Type: string
    /// Default: "-%Z- %C/%m: %-20.20n   %s%*  -- (%P)"
    ///
    /// This variable controls the format of the one-line message "status" displayed before each message in either the internal or an external pager. The valid sequences are listed in the $index_format section.
    PagerFormat,

    /// pager_index_lines
    ///
    /// Type: number
    /// Default: 0
    ///
    /// Determines the number of lines of a mini-index which is shown when in the pager. The current message, unless near the top or bottom of the folder, will be roughly one third of the way down this mini-index, giving the reader the context of a few messages before and after the message. This is useful, for example, to determine how many messages remain to be read in the current thread. One of the lines is reserved for the status bar from the index, so a setting of 6 will only show 5 lines of the actual index. A value of 0 results in no index being shown. If the number of messages in the current folder is less than $pager_index_lines, then the index will only use as many lines as it needs.
    PagerIndexLines,

    /// pager_stop
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, the internal-pager will not move to the next message when you are at the end of a message and invoke the <next-page> function.
    PagerStop,

    /// pgp_auto_decode
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, mutt will automatically attempt to decrypt traditional PGP messages whenever the user performs an operation which ordinarily would result in the contents of the message being operated on. For example, if the user displays a pgp-traditional message which has not been manually checked with the <check-traditional-pgp> function, mutt will automatically check the message for traditional pgp.
    PgpAutoDecode,

    /// pgp_autoinline
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// This option controls whether Mutt generates old-style inline (traditional) PGP encrypted or signed messages under certain circumstances. This can be overridden by use of the pgp menu, when inline is not required.
    ///
    /// Note that Mutt might automatically use PGP/MIME for messages which consist of more than a single MIME part. Mutt can be configured to ask before sending PGP/MIME messages when inline (traditional) would not work.
    ///
    /// Also see the $pgp_mime_auto variable.
    ///
    /// Also note that using the old-style PGP message format is strongly deprecated. (PGP only)
    PgpAutoinline,

    /// pgp_check_exit
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set, mutt will check the exit code of the PGP subprocess when signing or encrypting. A non-zero exit code means that the subprocess failed. (PGP only)
    PgpCheckExit,

    /// pgp_clearsign_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This format is used to create an old-style "clearsigned" PGP message. Note that the use of this format is strongly deprecated.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpClearsignCommand,

    /// pgp_decode_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This format strings specifies a command which is used to decode application/pgp attachments.
    ///
    /// The PGP command formats have their own set of printf(3)-like sequences:
    /// %p
    ///
    /// Expands to PGPPASSFD=0 when a pass phrase is needed, to an empty string otherwise. Note: This may be used with a %? construct.
    ///
    /// %f
    ///
    /// Expands to the name of a file containing a message.
    ///
    /// %s
    ///
    /// Expands to the name of a file containing the signature part of a multipart/signed attachment when verifying it.
    ///
    /// %a
    ///
    /// The value of $pgp_sign_as.
    ///
    /// %r
    ///
    /// One or more key IDs.
    /// For examples on how to configure these formats for the various versions of PGP which are floating around, see the pgp and gpg sample configuration files in the samples/ subdirectory which has been installed on your system alongside the documentation. (PGP only)
    PgpDecodeCommand,

    /// pgp_decrypt_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to decrypt a PGP encrypted message.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpDecryptCommand,

    /// pgp_encrypt_only_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to encrypt a body part without signing it.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpEncryptOnlyCommand,

    /// pgp_encrypt_sign_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to both sign and encrypt a body part.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpEncryptSignCommand,

    /// pgp_entry_format
    ///
    /// Type: string
    /// Default: "%4n %t%f %4l/0x%k %-4a %2c %u"
    ///
    /// This variable allows you to customize the PGP key selection menu to your personal taste. This string is similar to $index_format, but has its own set of printf(3)-like sequences:
    /// %n
    ///
    /// number
    ///
    /// %k
    ///
    /// key id
    ///
    /// %u
    ///
    /// user id
    ///
    /// %a
    ///
    /// algorithm
    ///
    /// %l
    ///
    /// key length
    ///
    /// %f
    ///
    /// flags
    ///
    /// %c
    ///
    /// capabilities
    ///
    /// %t
    ///
    /// trust/validity of the key-uid association
    ///
    /// %[<s>]
    ///
    /// date of the key where <s> is an strftime(3) expression
    /// (PGP only)
    PgpEntryFormat,

    /// pgp_export_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to export a public key from the user's key ring.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpExportCommand,

    /// pgp_getkeys_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is invoked whenever mutt will need public key information. Of the sequences supported by $pgp_decode_command, %r is the only printf(3)-like sequence used with this format. (PGP only)
    PgpGetkeysCommand,

    /// pgp_good_sign
    ///
    /// Type: regular expression
    /// Default: ""
    ///
    /// If you assign a text to this variable, then a PGP signature is only considered verified if the output from $pgp_verify_command contains the text. Use this variable if the exit code from the command is 0 even for bad signatures. (PGP only)
    PgpGoodSign,

    /// pgp_ignore_subkeys
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Setting this variable will cause Mutt to ignore OpenPGP subkeys. Instead, the principal key will inherit the subkeys' capabilities. Unset this if you want to play interesting key selection games. (PGP only)
    PgpIgnoreSubkeys,

    /// pgp_import_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to import a key from a message into the user's public key ring.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpImportCommand,

    /// pgp_list_pubring_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to list the public key ring's contents. The output format must be analogous to the one used by
    ///
    /// gpg --list-keys --with-colons.
    ///
    /// This format is also generated by the pgpring utility which comes with mutt.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpListPubringCommand,

    /// pgp_list_secring_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to list the secret key ring's contents. The output format must be analogous to the one used by:
    ///
    /// gpg --list-keys --with-colons.
    ///
    /// This format is also generated by the pgpring utility which comes with mutt.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpListSecringCommand,

    /// pgp_long_ids
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, use 64 bit PGP key IDs, if unset use the normal 32 bit key IDs. (PGP only)
    PgpLongIds,

    /// pgp_mime_auto
    ///
    /// Type: quadoption
    /// Default: ask-yes
    ///
    /// This option controls whether Mutt will prompt you for automatically sending a (signed/encrypted) message using PGP/MIME when inline (traditional) fails (for any reason).
    ///
    /// Also note that using the old-style PGP message format is strongly deprecated. (PGP only)
    PgpMimeAuto,

    /// pgp_replyinline
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Setting this variable will cause Mutt to always attempt to create an inline (traditional) message when replying to a message which is PGP encrypted/signed inline. This can be overridden by use of the pgp menu, when inline is not required. This option does not automatically detect if the (replied-to) message is inline; instead it relies on Mutt internals for previously checked/flagged messages.
    ///
    /// Note that Mutt might automatically use PGP/MIME for messages which consist of more than a single MIME part. Mutt can be configured to ask before sending PGP/MIME messages when inline (traditional) would not work.
    ///
    /// Also see the $pgp_mime_auto variable.
    ///
    /// Also note that using the old-style PGP message format is strongly deprecated. (PGP only)
    PgpReplyinline,

    /// pgp_retainable_sigs
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, signed and encrypted messages will consist of nested multipart/signed and multipart/encrypted body parts.
    ///
    /// This is useful for applications like encrypted and signed mailing lists, where the outer layer (multipart/encrypted) can be easily removed, while the inner multipart/signed part is retained. (PGP only)
    PgpRetainableSigs,

    /// pgp_show_unusable
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set, mutt will display non-usable keys on the PGP key selection menu. This includes keys which have been revoked, have expired, or have been marked as "disabled" by the user. (PGP only)
    PgpShowUnusable,

    /// pgp_sign_as
    ///
    /// Type: string
    /// Default: ""
    ///
    /// If you have more than one key pair, this option allows you to specify which of your private keys to use. It is recommended that you use the keyid form to specify your key (e.g. 0x00112233). (PGP only)
    PgpSignAs,

    /// pgp_sign_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to create the detached PGP signature for a multipart/signed PGP/MIME body part.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpSignCommand,

    /// pgp_sort_keys
    ///
    /// Type: sort order
    /// Default: address
    ///
    /// Specifies how the entries in the pgp menu are sorted. The following are legal values:
    /// address
    ///
    /// sort alphabetically by user id
    ///
    /// keyid
    ///
    /// sort alphabetically by key id
    ///
    /// date
    ///
    /// sort by key creation date
    ///
    /// trust
    ///
    /// sort by the trust of the key
    /// If you prefer reverse order of the above values, prefix it with "reverse-". (PGP only)
    PgpSortKeys,

    /// pgp_strict_enc
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set, Mutt will automatically encode PGP/MIME signed messages as quoted-printable. Please note that unsetting this variable may lead to problems with non-verifyable PGP signatures, so only change this if you know what you are doing. (PGP only)
    PgpStrictEnc,

    /// pgp_timeout
    ///
    /// Type: number
    /// Default: 300
    ///
    /// The number of seconds after which a cached passphrase will expire if not used. (PGP only)
    PgpTimeout,

    /// pgp_use_gpg_agent
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, mutt will use a possibly-running gpg-agent(1) process. (PGP only)
    PgpUseGpgAgent,

    /// pgp_verify_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to verify PGP signatures.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpVerifyCommand,

    /// pgp_verify_key_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to verify key information from the key selection menu.
    ///
    /// This is a format string, see the $pgp_decode_command command for possible printf(3)-like sequences. (PGP only)
    PgpVerifyKeyCommand,

    /// pipe_decode
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Used in connection with the <pipe-message> command. When unset, Mutt will pipe the messages without any preprocessing. When set, Mutt will weed headers and will attempt to decode the messages first.
    PipeDecode,

    /// pipe_sep
    ///
    /// Type: string
    /// Default: "\n"
    ///
    /// The separator to add between messages when piping a list of tagged messages to an external Unix command.
    PipeSep,

    /// pipe_split
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Used in connection with the <pipe-message> function following <tag-prefix>. If this variable is unset, when piping a list of tagged messages Mutt will concatenate the messages and will pipe them all concatenated. When set, Mutt will pipe the messages one by one. In both cases the messages are piped in the current sorted order, and the $pipe_sep separator is added after each message.
    PipeSplit,

    /// pop_auth_try_all
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set, Mutt will try all available authentication methods. When unset, Mutt will only fall back to other authentication methods if the previous methods are unavailable. If a method is available but authentication fails, Mutt will not connect to the POP server.
    PopAuthTryAll,

    /// pop_authenticators
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This is a colon-delimited list of authentication methods mutt may attempt to use to log in to an POP server, in the order mutt should try them. Authentication methods are either "user", "apop" or any SASL mechanism, e.g. "digest-md5", "gssapi" or "cram-md5". This option is case-insensitive. If this option is unset (the default) mutt will try all available methods, in order from most-secure to least-secure.
    ///
    /// Example:
    ///
    /// set pop_authenticators="digest-md5:apop:user"
    PopAuthenticators,

    /// pop_checkinterval
    ///
    /// Type: number
    /// Default: 60
    ///
    /// This variable configures how often (in seconds) mutt should look for new mail in the currently selected mailbox if it is a POP mailbox.
    PopCheckinterval,

    /// pop_delete
    ///
    /// Type: quadoption
    /// Default: ask-no
    ///
    /// If set, Mutt will delete successfully downloaded messages from the POP server when using the <fetch-mail> function. When unset, Mutt will download messages but also leave them on the POP server.
    PopDelete,

    /// pop_host
    ///
    /// Type: string
    /// Default: ""
    ///
    /// The name of your POP server for the <fetch-mail> function. You can also specify an alternative port, username and password, i.e.:
    ///
    /// [pop[s]://][username[:password]@]popserver[:port]
    ///
    /// where "[...]" denotes an optional part.
    PopHost,

    /// pop_last
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If this variable is set, mutt will try to use the "LAST" POP command for retrieving only unread messages from the POP server when using the <fetch-mail> function.
    PopLast,

    /// pop_pass
    ///
    /// Type: string
    /// Default: ""
    ///
    /// Specifies the password for your POP account. If unset, Mutt will prompt you for your password when you open a POP mailbox.
    ///
    /// Warning: you should only use this option when you are on a fairly secure machine, because the superuser can read your muttrc even if you are the only one who can read the file.
    PopPass,

    /// pop_reconnect
    ///
    /// Type: quadoption
    /// Default: ask-yes
    ///
    /// Controls whether or not Mutt will try to reconnect to the POP server if the connection is lost.
    PopReconnect,

    /// pop_user
    ///
    /// Type: string
    /// Default: ""
    ///
    /// Your login name on the POP server.
    ///
    /// This variable defaults to your user name on the local machine.
    PopUser,

    /// post_indent_string
    ///
    /// Type: string
    /// Default: ""
    ///
    /// Similar to the $attribution variable, Mutt will append this string after the inclusion of a message which is being replied to.
    PostIndentString,

    /// postpone
    ///
    /// Type: quadoption
    /// Default: ask-yes
    ///
    /// Controls whether or not messages are saved in the $postponed mailbox when you elect not to send immediately.
    ///
    /// Also see the $recall variable.
    Postpone,

    /// postponed
    ///
    /// Type: path
    /// Default: "~/postponed"
    ///
    /// Mutt allows you to indefinitely "postpone sending a message" which you are editing. When you choose to postpone a message, Mutt saves it in the mailbox specified by this variable.
    ///
    /// Also see the $postpone variable.
    Postponed,

    /// preconnect
    ///
    /// Type: string
    /// Default: ""
    ///
    /// If set, a shell command to be executed if mutt fails to establish a connection to the server. This is useful for setting up secure connections, e.g. with ssh(1). If the command returns a nonzero status, mutt gives up opening the server. Example:
    ///
    /// set preconnect="ssh -f -q -L 1234:mailhost.net:143 mailhost.net \
    /// sleep 20 < /dev/null > /dev/null"
    ///
    /// Mailbox "foo" on "mailhost.net" can now be reached as "{localhost:1234}foo".
    ///
    /// Note: For this example to work, you must be able to log in to the remote machine without having to enter a password.
    Preconnect,
    /// print
    /// Type: quadoption
    /// Default: ask-no
    /// Controls whether or not Mutt really prints messages. This is set to "ask-no" by default, because some people accidentally hit "p" often.
    Print,
    /// print_command
    ///
    /// Type: path
    /// Default: "lpr"
    ///
    /// This specifies the command pipe that should be used to print messages.
    PrintCommand,

    /// print_decode
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Used in connection with the <print-message> command. If this option is set, the message is decoded before it is passed to the external command specified by $print_command. If this option is unset, no processing will be applied to the message when printing it. The latter setting may be useful if you are using some advanced printer filter which is able to properly format e-mail messages for printing.
    PrintDecode,

    /// print_split
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Used in connection with the <print-message> command. If this option is set, the command specified by $print_command is executed once for each message which is to be printed. If this option is unset, the command specified by $print_command is executed only once, and all the messages are concatenated, with a form feed as the message separator.
    ///
    /// Those who use the enscript(1) program's mail-printing mode will most likely want to set this option.
    PrintSplit,

    /// prompt_after
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If you use an external $pager, setting this variable will cause Mutt to prompt you for a command when the pager exits rather than returning to the index menu. If unset, Mutt will return to the index menu when the external pager exits.
    PromptAfter,

    /// query_command
    ///
    /// Type: path
    /// Default: ""
    ///
    /// This specifies the command that mutt will use to make external address queries. The string should contain a "%s", which will be substituted with the query string the user types. See "query" for more information.
    QueryCommand,

    /// query_format
    ///
    /// Type: string
    /// Default: "%4c %t %-25.25a %-25.25n %?e?(%e)?"
    ///
    /// This variable describes the format of the "query" menu. The following printf(3)-style sequences are understood:
    /// %a
    ///
    /// destination address
    ///
    /// %c
    ///
    /// current entry number
    ///
    /// %e
    ///
    /// extra information *
    ///
    /// %n
    ///
    /// destination name
    ///
    /// %t
    ///
    /// "*" if current entry is tagged, a space otherwise
    ///
    /// %>X
    ///
    /// right justify the rest of the string and pad with "X"
    ///
    /// %|X
    ///
    /// pad to the end of the line with "X"
    ///
    /// %*X
    ///
    /// soft-fill with character "X" as pad
    /// For an explanation of "soft-fill", see the $index_format documentation.
    ///
    /// * = can be optionally printed if nonzero, see the $status_format documentation.
    QueryFormat,

    /// quit
    /// Type: quadoption
    /// Default: yes
    /// This variable controls whether "quit" and "exit" actually quit from mutt. If this option is set, they do quit, if it is unset, they have no effect, and if it is set to ask-yes or ask-no, you are prompted for confirmation when you try to quit.
    Quit,

    /// quote_regexp
    ///
    /// Type: regular expression
    /// Default: "^([ \t]*[|>:}#])+"
    ///
    /// A regular expression used in the internal pager to determine quoted sections of text in the body of a message. Quoted text may be filtered out using the <toggle-quoted> command, or colored according to the "color quoted" family of directives.
    ///
    /// Higher levels of quoting may be colored differently ("color quoted1", "color quoted2", etc.). The quoting level is determined by removing the last character from the matched text and recursively reapplying the regular expression until it fails to produce a match.
    ///
    /// Match detection may be overridden by the $smileys regular expression.
    QuoteRegexp,

    /// read_inc
    ///
    /// Type: number
    /// Default: 10
    ///
    /// If set to a value greater than 0, Mutt will display which message it is currently on when reading a mailbox or when performing search actions such as search and limit. The message is printed after this many messages have been read or searched (e.g., if set to 25, Mutt will print a message when it is at message 25, and then again when it gets to message 50). This variable is meant to indicate progress when reading or searching large mailboxes which may take some time. When set to 0, only a single message will appear before the reading the mailbox.
    ///
    /// Also see the $write_inc, $net_inc and $time_inc variables and the "tuning" section of the manual for performance considerations.
    ReadInc,

    /// read_only
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, all folders are opened in read-only mode.
    ReadOnly,

    /// realname
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This variable specifies what "real" or "personal" name should be used when sending messages.
    ///
    /// By default, this is the GECOS field from /etc/passwd. Note that this variable will not be used when the user has set a real name in the $from variable.
    Realname,

    /// recall
    /// Type: quadoption
    /// Default: ask-yes
    /// Controls whether or not Mutt recalls postponed messages when composing a new message.
    ///
    /// Setting this variable to is not generally useful, and thus not recommended.
    ///
    /// Also see $postponed variable.
    Recall,
    /// record
    /// Type: path
    /// Default: "~/sent"
    /// This specifies the file into which your outgoing messages should be appended. (This is meant as the primary method for saving a copy of your messages, but another way to do this is using the "my_hdr" command to create a "Bcc:" field with your email address in it.)
    ///
    /// The value of $record is overridden by the $force_name and $save_name variables, and the "fcc-hook" command.
    Record,

    /// reply_regexp
    ///
    /// Type: regular expression
    /// Default: "^(re([\[0-9\]+])*|aw):[ \t]*"
    ///
    /// A regular expression used to recognize reply messages when threading and replying. The default value corresponds to the English "Re:" and the German "Aw:".
    ReplyRegexp,

    /// reply_self
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If unset and you are replying to a message sent by you, Mutt will assume that you want to reply to the recipients of that message rather than to yourself.
    ///
    /// Also see the "alternates" command.
    ReplySelf,

    /// reply_to
    ///
    /// Type: quadoption
    /// Default: ask-yes
    ///
    /// If set, when replying to a message, Mutt will use the address listed in the Reply-to: header as the recipient of the reply. If unset, it will use the address in the From: header field instead. This option is useful for reading a mailing list that sets the Reply-To: header field to the list address and you want to send a private message to the author of a message.
    ReplyTo,

    /// resolve
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, the cursor will be automatically advanced to the next (possibly undeleted) message whenever a command that modifies the current message is executed.
    Resolve,

    /// reverse_alias
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// This variable controls whether or not Mutt will display the "personal" name from your aliases in the index menu if it finds an alias that matches the message's sender. For example, if you have the following alias:
    ///
    /// alias juser abd30425@somewhere.net (Joe User)
    ///
    /// and then you receive mail which contains the following header:
    ///
    /// From: abd30425@somewhere.net
    ///
    /// It would be displayed in the index menu as "Joe User" instead of "abd30425@somewhere.net." This is useful when the person's e-mail address is not human friendly.
    ReverseAlias,

    /// reverse_name
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// It may sometimes arrive that you receive mail to a certain machine, move the messages to another machine, and reply to some the messages from there. If this variable is set, the default From: line of the reply messages is built using the address where you received the messages you are replying to if that address matches your "alternates". If the variable is unset, or the address that would be used doesn't match your "alternates", the From: line will use your address on the current machine.
    ///
    /// Also see the "alternates" command.
    ReverseName,

    /// reverse_realname
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// This variable fine-tunes the behavior of the $reverse_name feature. When it is set, mutt will use the address from incoming messages as-is, possibly including eventual real names. When it is unset, mutt will override any such real names with the setting of the $realname variable.
    ReverseRealname,

    /// rfc2047_parameters
    /// Type: boolean
    /// Default: no
    ///
    /// When this variable is set, Mutt will decode RFC2047-encoded MIME parameters. You want to set this variable when mutt suggests you to save attachments to files named like:
    ///
    /// =?iso-8859-1?Q?file=5F=E4=5F991116=2Ezip?=
    ///
    /// When this variable is set interactively, the change won't be active until you change folders.
    ///
    /// Note that this use of RFC2047's encoding is explicitly prohibited by the standard, but nevertheless encountered in the wild.
    ///
    /// Also note that setting this parameter will not have the effect that mutt generates this kind of encoding. Instead, mutt will unconditionally use the encoding specified in RFC2231.
    Rfc2047Parameters,

    /// save_address
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, mutt will take the sender's full address when choosing a default folder for saving a mail. If $save_name or $force_name is set too, the selection of the Fcc folder will be changed as well.
    SaveAddress,

    /// save_empty
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When unset, mailboxes which contain no saved messages will be removed when closed (the exception is $spoolfile which is never removed). If set, mailboxes are never removed.
    ///
    /// Note: This only applies to mbox and MMDF folders, Mutt does not delete MH and Maildir directories.
    SaveEmpty,

    /// save_history
    ///
    /// Type: number
    /// Default: 0
    ///
    /// This variable controls the size of the history (per category) saved in the $history_file file.
    SaveHistory,

    /// save_name
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// This variable controls how copies of outgoing messages are saved. When set, a check is made to see if a mailbox specified by the recipient address exists (this is done by searching for a mailbox in the $folder directory with the username part of the recipient address). If the mailbox exists, the outgoing message will be saved to that mailbox, otherwise the message is saved to the $record mailbox.
    ///
    /// Also see the $force_name variable.
    SaveName,

    /// score
    /// Type: boolean
    /// Default: yes
    /// When this variable is unset, scoring is turned off. This can be useful to selectively disable scoring for certain folders when the $score_threshold_delete variable and related are used.
    Score,

    /// score_threshold_delete
    ///
    /// Type: number
    /// Default: -1
    ///
    /// Messages which have been assigned a score equal to or lower than the value of this variable are automatically marked for deletion by mutt. Since mutt scores are always greater than or equal to zero, the default setting of this variable will never mark a message for deletion.
    ScoreThresholdDelete,

    /// score_threshold_flag
    ///
    /// Type: number
    /// Default: 9999
    ///
    /// Messages which have been assigned a score greater than or equal to this variable's value are automatically marked "flagged".
    ScoreThresholdFlag,

    /// score_threshold_read
    ///
    /// Type: number
    /// Default: -1
    ///
    /// Messages which have been assigned a score equal to or lower than the value of this variable are automatically marked as read by mutt. Since mutt scores are always greater than or equal to zero, the default setting of this variable will never mark a message read.
    ScoreThresholdRead,

    /// search_context
    ///
    /// Type: number
    /// Default: 0
    ///
    /// For the pager, this variable specifies the number of lines shown before search results. By default, search results will be top-aligned.
    SearchContext,

    /// send_charset
    ///
    /// Type: string
    /// Default: "us-ascii:iso-8859-1:utf-8"
    ///
    /// A colon-delimited list of character sets for outgoing messages. Mutt will use the first character set into which the text can be converted exactly. If your $charset is not "iso-8859-1" and recipients may not understand "UTF-8", it is advisable to include in the list an appropriate widely used standard character set (such as "iso-8859-2", "koi8-r" or "iso-2022-jp") either instead of or after "iso-8859-1".
    ///
    /// In case the text cannot be converted into one of these exactly, mutt uses $charset as a fallback.
    SendCharset,

    /// sendmail
    ///
    /// Type: path
    /// Default: "/usr/sbin/sendmail -oem -oi"
    ///
    /// Specifies the program and arguments used to deliver mail sent by Mutt. Mutt expects that the specified program interprets additional arguments as recipient addresses.
    Sendmail,

    /// sendmail_wait
    ///
    /// Type: number
    /// Default: 0
    ///
    /// Specifies the number of seconds to wait for the $sendmail process to finish before giving up and putting delivery in the background.
    ///
    /// Mutt interprets the value of this variable as follows:
    /// >0
    ///
    /// number of seconds to wait for sendmail to finish before continuing
    ///
    /// 0
    ///
    /// wait forever for sendmail to finish
    ///
    /// <0
    ///
    /// always put sendmail in the background without waiting
    /// Note that if you specify a value other than 0, the output of the child process will be put in a temporary file. If there is some error, you will be informed as to where to find the output.
    SendmailWait,

    /// shell
    /// Type: path
    /// Default: ""
    /// Command to use when spawning a subshell. By default, the user's login shell from /etc/passwd is used.
    Shell,

    /// sig_dashes
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set, a line containing "-- " (note the trailing space) will be inserted before your $signature. It is strongly recommended that you not unset this variable unless your signature contains just your name. The reason for this is because many software packages use "-- \n" to detect your signature. For example, Mutt has the ability to highlight the signature in a different color in the built-in pager.
    SigDashes,

    /// sig_on_top
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, the signature will be included before any quoted or forwarded text. It is strongly recommended that you do not set this variable unless you really know what you are doing, and are prepared to take some heat from netiquette guardians.
    SigOnTop,

    /// signature
    ///
    /// Type: path
    /// Default: "~/.signature"
    ///
    /// Specifies the filename of your signature, which is appended to all outgoing messages. If the filename ends with a pipe ("|"), it is assumed that filename is a shell command and input should be read from its standard output.
    Signature,

    /// simple_search
    ///
    /// Type: string
    /// Default: "~f %s | ~s %s"
    ///
    /// Specifies how Mutt should expand a simple search into a real search pattern. A simple search is one that does not contain any of the "~" pattern operators. See "patterns" for more information on search patterns.
    ///
    /// For example, if you simply type "joe" at a search or limit prompt, Mutt will automatically expand it to the value specified by this variable by replacing "%s" with the supplied string. For the default value, "joe" would be expanded to: "~f joe | ~s joe".
    SimpleSearch,

    /// sleep_time
    ///
    /// Type: number
    /// Default: 1
    ///
    /// Specifies time, in seconds, to pause while displaying certain informational messages, while moving from folder to folder and after expunging messages from the current folder. The default is to pause one second, so a value of zero for this option suppresses the pause.
    SleepTime,

    /// smart_wrap
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Controls the display of lines longer than the screen width in the internal pager. If set, long lines are wrapped at a word boundary. If unset, lines are simply wrapped at the screen edge. Also see the $markers variable.
    SmartWrap,

    /// smileys
    ///
    /// Type: regular expression
    /// Default: "(>From )|(:[-^]?[][)(><}{|/DP])"
    ///
    /// The pager uses this variable to catch some common false positives of $quote_regexp, most notably smileys and not consider a line quoted text if it also matches $smileys. This mostly happens at the beginning of a line.
    Smileys,

    /// smime_ask_cert_label
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// This flag controls whether you want to be asked to enter a label for a certificate about to be added to the database or not. It is set by default. (S/MIME only)
    SmimeAskCertLabel,

    /// smime_ca_location
    ///
    /// Type: path
    /// Default: ""
    ///
    /// This variable contains the name of either a directory, or a file which contains trusted certificates for use with OpenSSL. (S/MIME only)
    SmimeCaLocation,

    /// smime_certificates
    ///
    /// Type: path
    /// Default: ""
    ///
    /// Since for S/MIME there is no pubring/secring as with PGP, mutt has to handle storage and retrieval of keys by itself. This is very basic right now, and keys and certificates are stored in two different directories, both named as the hash-value retrieved from OpenSSL. There is an index file which contains mailbox-address keyid pairs, and which can be manually edited. This option points to the location of the certificates. (S/MIME only)
    SmimeCertificates,

    /// smime_decrypt_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This format string specifies a command which is used to decrypt application/x-pkcs7-mime attachments.
    ///
    /// The OpenSSL command formats have their own set of printf(3)-like sequences similar to PGP's:
    /// %f
    ///
    /// Expands to the name of a file containing a message.
    ///
    /// %s
    ///
    /// Expands to the name of a file containing the signature part of a multipart/signed attachment when verifying it.
    ///
    /// %k
    ///
    /// The key-pair specified with $smime_default_key
    ///
    /// %c
    ///
    /// One or more certificate IDs.
    ///
    /// %a
    ///
    /// The algorithm used for encryption.
    ///
    /// %C
    ///
    /// CA location: Depending on whether $smime_ca_location points to a directory or file, this expands to "-CApath $smime_ca_location" or "-CAfile $smime_ca_location".
    /// For examples on how to configure these formats, see the smime.rc in the samples/ subdirectory which has been installed on your system alongside the documentation. (S/MIME only)
    SmimeDecryptCommand,

    /// smime_decrypt_use_default_key
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set (default) this tells mutt to use the default key for decryption. Otherwise, if managing multiple certificate-key-pairs, mutt will try to use the mailbox-address to determine the key to use. It will ask you to supply a key, if it can't find one. (S/MIME only)
    SmimeDecryptUseDefaultKey,

    /// smime_default_key
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This is the default key-pair to use for signing. This must be set to the keyid (the hash-value that OpenSSL generates) to work properly (S/MIME only)
    SmimeDefaultKey,

    /// smime_encrypt_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to create encrypted S/MIME messages.
    ///
    /// This is a format string, see the $smime_decrypt_command command for possible printf(3)-like sequences. (S/MIME only)
    SmimeEncryptCommand,

    /// smime_encrypt_with
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This sets the algorithm that should be used for encryption. Valid choices are "des", "des3", "rc2-40", "rc2-64", "rc2-128". If unset, "3des" (TripleDES) is used. (S/MIME only)
    SmimeEncryptWith,

    /// smime_get_cert_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to extract X509 certificates from a PKCS7 structure.
    ///
    /// This is a format string, see the $smime_decrypt_command command for possible printf(3)-like sequences. (S/MIME only)
    SmimeGetCertCommand,

    /// smime_get_cert_email_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to extract the mail address(es) used for storing X509 certificates, and for verification purposes (to check whether the certificate was issued for the sender's mailbox).
    ///
    /// This is a format string, see the $smime_decrypt_command command for possible printf(3)-like sequences. (S/MIME only)
    SmimeGetCertEmailCommand,

    /// smime_get_signer_cert_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to extract only the signers X509 certificate from a S/MIME signature, so that the certificate's owner may get compared to the email's "From:" field.
    ///
    /// This is a format string, see the $smime_decrypt_command command for possible printf(3)-like sequences. (S/MIME only)
    SmimeGetSignerCertCommand,

    /// smime_import_cert_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to import a certificate via smime_keys.
    ///
    /// This is a format string, see the $smime_decrypt_command command for possible printf(3)-like sequences. (S/MIME only)
    SmimeImportCertCommand,

    /// smime_is_default
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// The default behavior of mutt is to use PGP on all auto-sign/encryption operations. To override and to use OpenSSL instead this must be set. However, this has no effect while replying, since mutt will automatically select the same application that was used to sign/encrypt the original message. (Note that this variable can be overridden by unsetting $crypt_autosmime.) (S/MIME only)
    SmimeIsDefault,

    /// smime_keys
    ///
    /// Type: path
    /// Default: ""
    ///
    /// Since for S/MIME there is no pubring/secring as with PGP, mutt has to handle storage and retrieval of keys/certs by itself. This is very basic right now, and stores keys and certificates in two different directories, both named as the hash-value retrieved from OpenSSL. There is an index file which contains mailbox-address keyid pair, and which can be manually edited. This option points to the location of the private keys. (S/MIME only)
    SmimeKeys,

    /// smime_pk7out_command
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to extract PKCS7 structures of S/MIME signatures, in order to extract the public X509 certificate(s).
    /// This is a format string, see the $smime_decrypt_command command for possible printf(3)-like sequences. (S/MIME only)
    Smimepk7outCommand,

    /// smime_sign_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to created S/MIME signatures of type multipart/signed, which can be read by all mail clients.
    ///
    /// This is a format string, see the $smime_decrypt_command command for possible printf(3)-like sequences. (S/MIME only)
    SmimeSignCommand,

    /// smime_sign_opaque_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to created S/MIME signatures of type application/x-pkcs7-signature, which can only be handled by mail clients supporting the S/MIME extension.
    ///
    /// This is a format string, see the $smime_decrypt_command command for possible printf(3)-like sequences. (S/MIME only)
    SmimeSignOpaqueCommand,

    /// smime_timeout
    ///
    /// Type: number
    /// Default: 300
    ///
    /// The number of seconds after which a cached passphrase will expire if not used. (S/MIME only)
    SmimeTimeout,

    /// smime_verify_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to verify S/MIME signatures of type multipart/signed.
    ///
    /// This is a format string, see the $smime_decrypt_command command for possible printf(3)-like sequences. (S/MIME only)
    SmimeVerifyCommand,

    /// smime_verify_opaque_command
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This command is used to verify S/MIME signatures of type application/x-pkcs7-mime.
    ///
    /// This is a format string, see the $smime_decrypt_command command for possible printf(3)-like sequences. (S/MIME only)
    SmimeVerifyOpaqueCommand,

    /// smtp_authenticators
    ///
    /// Type: string
    /// Default: ""
    ///
    /// This is a colon-delimited list of authentication methods mutt may attempt to use to log in to an SMTP server, in the order mutt should try them. Authentication methods are any SASL mechanism, e.g. "digest-md5", "gssapi" or "cram-md5". This option is case-insensitive. If it is "unset" (the default) mutt will try all available methods, in order from most-secure to least-secure.
    ///
    /// Example:
    ///
    /// set smtp_authenticators="digest-md5:cram-md5"
    SmtpAuthenticators,

    /// smtp_pass
    ///
    /// Type: string
    /// Default: ""
    ///
    /// Specifies the password for your SMTP account. If unset, Mutt will prompt you for your password when you first send mail via SMTP. See $smtp_url to configure mutt to send mail via SMTP.
    ///
    /// Warning: you should only use this option when you are on a fairly secure machine, because the superuser can read your muttrc even if you are the only one who can read the file.
    SmtpPass,

    /// smtp_url
    ///
    /// Type: string
    /// Default: ""
    ///
    /// Defines the SMTP smarthost where sent messages should relayed for delivery. This should take the form of an SMTP URL, e.g.:
    ///
    /// smtp[s]://[user[:pass]@]host[:port]
    ///
    /// where "[...]" denotes an optional part. Setting this variable overrides the value of the $sendmail variable.
    SmtpUrl,

    /// sort
    /// Type: sort order
    /// Default: date
    /// Specifies how to sort messages in the "index" menu. Valid values are:
    /// - date or date-sent
    /// - date-received
    /// - from
    /// - mailbox-order (unsorted)
    /// - score
    /// - size
    ///
    /// - spam
    /// - subject
    /// - threads
    /// - to
    /// You may optionally use the "reverse-" prefix to specify reverse sorting order (example: "set sort=reverse-date-sent").
    Sort,

    /// sort_alias
    ///
    /// Type: sort order
    /// Default: alias
    ///
    /// Specifies how the entries in the "alias" menu are sorted. The following are legal values:
    /// - address (sort alphabetically by email address)
    /// - alias (sort alphabetically by alias name)
    /// - unsorted (leave in order specified in .muttrc)
    SortAlias,

    /// sort_aux
    ///
    /// Type: sort order
    /// Default: date
    ///
    /// When sorting by threads, this variable controls how threads are sorted in relation to other threads, and how the branches of the thread trees are sorted. This can be set to any value that $sort can, except "threads" (in that case, mutt will just use "date-sent"). You can also specify the "last-" prefix in addition to the "reverse-" prefix, but "last-" must come after "reverse-". The "last-" prefix causes messages to be sorted against its siblings by which has the last descendant, using the rest of $sort_aux as an ordering. For instance,
    ///
    /// set sort_aux=last-date-received
    ///
    /// would mean that if a new message is received in a thread, that thread becomes the last one displayed (or the first, if you have "set sort=reverse-threads".)
    ///
    /// Note: For reversed $sort order $sort_aux is reversed again (which is not the right thing to do, but kept to not break any existing configuration setting).
    SortAux,

    /// sort_browser
    ///
    /// Type: sort order
    /// Default: alpha
    ///
    /// Specifies how to sort entries in the file browser. By default, the entries are sorted alphabetically. Valid values:
    /// - alpha (alphabetically)
    /// - date
    ///
    /// - size
    /// - unsorted
    /// You may optionally use the "reverse-" prefix to specify reverse sorting order (example: "set sort_browser=reverse-date").
    SortBrowser,

    /// sort_re
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// This variable is only useful when sorting by threads with $strict_threads unset. In that case, it changes the heuristic mutt uses to thread messages by subject. With $sort_re set, mutt will only attach a message as the child of another message by subject if the subject of the child message starts with a substring matching the setting of $reply_regexp. With $sort_re unset, mutt will attach the message whether or not this is the case, as long as the non-$reply_regexp parts of both messages are identical.
    SortRe,

    /// spam_separator
    ///
    /// Type: string
    /// Default: ","
    ///
    /// This variable controls what happens when multiple spam headers are matched: if unset, each successive header will overwrite any previous matches value for the spam label. If set, each successive match will append to the previous, using this variable's value as a separator.
    SpamSeparator,

    /// spoolfile
    ///
    /// Type: path
    /// Default: ""
    ///
    /// If your spool mailbox is in a non-default place where Mutt cannot find it, you can specify its location with this variable. Mutt will initially set this variable to the value of the environment variable $MAIL or $MAILDIR if either is defined.
    Spoolfile,

    /// ssl_ca_certificates_file
    ///
    /// Type: path
    /// Default: ""
    ///
    /// This variable specifies a file containing trusted CA certificates. Any server certificate that is signed with one of these CA certificates is also automatically accepted.
    ///
    /// Example:
    ///
    /// set ssl_ca_certificates_file=/etc/ssl/certs/ca-certificates.crt
    SslCaCertificatesFile,

    /// ssl_client_cert
    ///
    /// Type: path
    /// Default: ""
    ///
    /// The file containing a client certificate and its associated private key.
    SslClientCert,

    /// ssl_force_tls
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If this variable is set, Mutt will require that all connections to remote servers be encrypted. Furthermore it will attempt to negotiate TLS even if the server does not advertise the capability, since it would otherwise have to abort the connection anyway. This option supersedes $ssl_starttls.
    SslForceTls,

    /// ssl_min_dh_prime_bits
    ///
    /// Type: number
    /// Default: 0
    ///
    /// This variable specifies the minimum acceptable prime size (in bits) for use in any Diffie-Hellman key exchange. A value of 0 will use the default from the GNUTLS library.
    SslMinDhPrimeBits,

    /// ssl_starttls
    ///
    /// Type: quadoption
    /// Default: yes
    ///
    /// If set (the default), mutt will attempt to use STARTTLS on servers advertising the capability. When unset, mutt will not attempt to use STARTTLS regardless of the server's capabilities.
    SslStarttls,

    /// ssl_use_sslv3
    /// Type: boolean
    /// Default: yes
    ///
    /// This variable specifies whether to attempt to use SSLv3 in the SSL authentication process.
    SslUseSslv3,

    /// ssl_use_tlsv1
    /// Type: boolean
    /// Default: yes
    ///
    /// This variable specifies whether to attempt to use TLSv1 in the SSL authentication process.
    SslUseTlsv1,

    /// ssl_verify_dates
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set (the default), mutt will not automatically accept a server certificate that is either not yet valid or already expired. You should only unset this for particular known hosts, using the <account-hook> function.
    SslVerifyDates,

    /// ssl_verify_host
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// If set (the default), mutt will not automatically accept a server certificate whose host name does not match the host used in your folder URL. You should only unset this for particular known hosts, using the <account-hook> function.
    SslVerifyHost,

    /// status_chars
    ///
    /// Type: string
    /// Default: "-*%A"
    ///
    /// Controls the characters used by the "%r" indicator in $status_format. The first character is used when the mailbox is unchanged. The second is used when the mailbox has been changed, and it needs to be resynchronized. The third is used if the mailbox is in read-only mode, or if the mailbox will not be written when exiting that mailbox (You can toggle whether to write changes to a mailbox with the <toggle-write> operation, bound by default to "%"). The fourth is used to indicate that the current folder has been opened in attach- message mode (Certain operations like composing a new mail, replying, forwarding, etc. are not permitted in this mode).
    StatusChars,

    /// status_format
    ///
    /// Type: string
    /// Default: "-%r-Mutt: %f [Msgs:%?M?%M/?%m%?n? New:%n?%?o? Old:%o?%?d? Del:%d?%?F? Flag:%F?%?t? Tag:%t?%?p? Post:%p?%?b? Inc:%b?%?l? %l?]---(%s/%S)-%>-(%P)---"
    ///
    /// Controls the format of the status line displayed in the "index" menu. This string is similar to $index_format, but has its own set of printf(3)-like sequences:
    /// %b
    ///
    /// number of mailboxes with new mail *
    ///
    /// %d
    ///
    /// number of deleted messages *
    ///
    /// %f
    ///
    /// the full pathname of the current mailbox
    ///
    /// %F
    ///
    /// number of flagged messages *
    ///
    /// %h
    ///
    /// local hostname
    ///
    /// %l
    ///
    /// size (in bytes) of the current mailbox *
    ///
    /// %L
    ///
    /// size (in bytes) of the messages shown (i.e., which match the current limit) *
    ///
    /// %m
    ///
    /// the number of messages in the mailbox *
    ///
    /// %M
    ///
    /// the number of messages shown (i.e., which match the current limit) *
    ///
    /// %n
    ///
    /// number of new messages in the mailbox *
    ///
    /// %o
    ///
    /// number of old unread messages *
    ///
    /// %p
    ///
    /// number of postponed messages *
    ///
    /// %P
    ///
    /// percentage of the way through the index
    ///
    /// %r
    ///
    /// modified/read-only/won't-write/attach-message indicator, according to $status_chars
    ///
    /// %s
    ///
    /// current sorting mode ($sort)
    ///
    /// %S
    ///
    /// current aux sorting method ($sort_aux)
    ///
    /// %t
    ///
    /// number of tagged messages *
    ///
    /// %u
    ///
    /// number of unread messages *
    ///
    /// %v
    ///
    /// Mutt version string
    ///
    /// %V
    ///
    /// currently active limit pattern, if any *
    ///
    /// %>X
    ///
    /// right justify the rest of the string and pad with "X"
    ///
    /// %|X
    ///
    /// pad to the end of the line with "X"
    ///
    /// %*X
    ///
    /// soft-fill with character "X" as pad
    /// For an explanation of "soft-fill", see the $index_format documentation.
    ///
    /// * = can be optionally printed if nonzero
    ///
    /// Some of the above sequences can be used to optionally print a string if their value is nonzero. For example, you may only want to see the number of flagged messages if such messages exist, since zero is not particularly meaningful. To optionally print a string based upon one of the above sequences, the following construct is used:
    ///
    /// %?<sequence_char>?<optional_string>?
    ///
    /// where sequence_char is a character from the table above, and optional_string is the string you would like printed if sequence_char is nonzero. optional_string may contain other sequences as well as normal text, but you may not nest optional strings.
    ///
    /// Here is an example illustrating how to optionally print the number of new messages in a mailbox:
    ///
    /// %?n?%n new messages.?
    ///
    /// You can also switch between two strings using the following construct:
    ///
    /// %?<sequence_char>?<if_string>&<else_string>?
    ///
    /// If the value of sequence_char is non-zero, if_string will be expanded, otherwise else_string will be expanded.
    ///
    /// You can force the result of any printf(3)-like sequence to be lowercase by prefixing the sequence character with an underscore ("_") sign. For example, if you want to display the local hostname in lowercase, you would use: "%_h".
    ///
    /// If you prefix the sequence character with a colon (":") character, mutt will replace any dots in the expansion by underscores. This might be helpful with IMAP folders that don't like dots in folder names.
    StatusFormat,

    /// status_on_top
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Setting this variable causes the "status bar" to be displayed on the first line of the screen rather than near the bottom. If $help is set, too it'll be placed at the bottom.
    StatusOnTop,

    /// strict_threads
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// If set, threading will only make use of the "In-Reply-To" and "References:" fields when you $sort by message threads. By default, messages with the same subject are grouped together in "pseudo threads.". This may not always be desirable, such as in a personal mailbox where you might have several unrelated messages with the subjects like "hi" which will get grouped together. See also $sort_re for a less drastic way of controlling this behavior.
    StrictThreads,

    /// suspend
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When unset, mutt won't stop when the user presses the terminal's susp key, usually "^Z". This is useful if you run mutt inside an xterm using a command like "xterm -e mutt".
    Suspend,

    /// text_flowed
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, mutt will generate "format=flowed" bodies with a content type of "text/plain; format=flowed". This format is easier to handle for some mailing software, and generally just looks like ordinary text. To actually make use of this format's features, you'll need support in your editor.
    ///
    /// Note that $indent_string is ignored when this option is set.
    TextFlowed,

    /// thorough_search
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// Affects the ~b and ~h search operations described in section "patterns". If set, the headers and body/attachments of messages to be searched are decoded before searching. If unset, messages are searched as they appear in the folder.
    ///
    /// Users searching attachments or for non-ASCII characters should set this value because decoding also includes MIME parsing/decoding and possible character set conversions. Otherwise mutt will attempt to match against the raw message received (for example quoted-printable encoded or with encoded headers) which may lead to incorrect search results.
    ThoroughSearch,

    /// thread_received
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, mutt uses the date received rather than the date sent to thread messages by subject.
    ThreadReceived,

    /// tilde
    /// Type: boolean
    /// Default: no
    /// When set, the internal-pager will pad blank lines to the bottom of the screen with a tilde ("~").
    Tilde,

    /// time_inc
    ///
    /// Type: number
    /// Default: 0
    ///
    /// Along with $read_inc, $write_inc, and $net_inc, this variable controls the frequency with which progress updates are displayed. It suppresses updates less than $time_inc milliseconds apart. This can improve throughput on systems with slow terminals, or when running mutt on a remote system.
    ///
    /// Also see the "tuning" section of the manual for performance considerations.
    TimeInc,

    /// timeout
    ///
    /// Type: number
    /// Default: 600
    ///
    /// When Mutt is waiting for user input either idling in menus or in an interactive prompt, Mutt would block until input is present. Depending on the context, this would prevent certain operations from working, like checking for new mail or keeping an IMAP connection alive.
    ///
    /// This variable controls how many seconds Mutt will at most wait until it aborts waiting for input, performs these operations and continues to wait for input.
    ///
    /// A value of zero or less will cause Mutt to never time out.
    Timeout,

    /// tmpdir
    /// Type: path
    /// Default: ""
    /// This variable allows you to specify where Mutt will place its temporary files needed for displaying and composing messages. If this variable is not set, the environment variable $TMPDIR is used. If $TMPDIR is not set then "/tmp" is used.
    Tmpdir,

    /// to_chars
    ///
    /// Type: string
    /// Default: " +TCFL"
    ///
    /// Controls the character used to indicate mail addressed to you. The first character is the one used when the mail is not addressed to your address. The second is used when you are the only recipient of the message. The third is when your address appears in the "To:" header field, but you are not the only recipient of the message. The fourth character is used when your address is specified in the "Cc:" header field, but you are not the only recipient. The fifth character is used to indicate mail that was sent by you. The sixth character is used to indicate when a mail was sent to a mailing-list you subscribe to.
    ToChars,

    /// tunnel
    /// Type: string
    /// Default: ""
    /// Setting this variable will cause mutt to open a pipe to a command instead of a raw socket. You may be able to use this to set up preauthenticated connections to your IMAP/POP3/SMTP server. Example:
    ///
    /// set tunnel="ssh -q mailhost.net /usr/local/libexec/imapd"
    ///
    /// Note: For this example to work you must be able to log in to the remote machine without having to enter a password.
    ///
    /// When set, Mutt uses the tunnel for all remote connections. Please see "account-hook" in the manual for how to use different tunnel commands per connection.
    Tunnel,

    /// uncollapse_jump
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, Mutt will jump to the next unread message, if any, when the current thread is uncollapsed.
    UncollapseJump,

    /// use_8bitmime
    /// Type: boolean
    /// Default: no
    ///
    /// Warning: do not set this variable unless you are using a version of sendmail which supports the -B8BITMIME flag (such as sendmail 8.8.x) or you may not be able to send mail.
    ///
    /// When set, Mutt will invoke $sendmail with the -B8BITMIME flag when sending 8-bit messages to enable ESMTP negotiation.
    Use8bitmime,

    /// use_domain
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, Mutt will qualify all local addresses (ones without the "@host" portion) with the value of $hostname. If unset, no addresses will be qualified.
    UseDomain,

    /// use_envelope_from
    ///
    /// Type: boolean
    /// Default: no
    ///
    /// When set, mutt will set the envelope sender of the message. If $envelope_from_address is set, it will be used as the sender address. If unset, mutt will attempt to derive the sender from the "From:" header.
    ///
    /// Note that this information is passed to sendmail command using the -f command line switch. Therefore setting this option is not useful if the $sendmail variable already contains -f or if the executable pointed to by $sendmail doesn't support the -f switch.
    UseEnvelopeFrom,

    /// use_from
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, Mutt will generate the "From:" header field when sending messages. If unset, no "From:" header field will be generated unless the user explicitly sets one using the "my_hdr" command.
    UseFrom,

    /// use_idn
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, Mutt will show you international domain names decoded. Note: You can use IDNs for addresses even if this is unset. This variable only affects decoding.
    UseIdn,

    /// use_ipv6
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, Mutt will look for IPv6 addresses of hosts it tries to contact. If this option is unset, Mutt will restrict itself to IPv4 addresses. Normally, the default should work.
    UseIpv6,

    /// user_agent
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// When set, mutt will add a "User-Agent:" header to outgoing messages, indicating which version of mutt was used for composing them.
    UserAgent,

    /// visual
    /// Type: path
    /// Default: ""
    /// Specifies the visual editor to invoke when the "~v" command is given in the built-in editor.
    Visual,

    /// wait_key
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Controls whether Mutt will ask you to press a key after an external command has been invoked by these functions: <shell-escape>, <pipe-message>, <pipe-entry>, <print-message>, and <print-entry> commands.
    ///
    /// It is also used when viewing attachments with "auto_view", provided that the corresponding mailcap entry has a needsterminal flag, and the external program is interactive.
    ///
    /// When set, Mutt will always ask for a key. When unset, Mutt will wait for a key only if the external command returned a non-zero status.
    WaitKey,

    /// weed
    /// Type: boolean
    /// Default: yes
    /// When set, mutt will weed headers when displaying, forwarding, printing, or replying to messages.
    Weed,

    /// wrap
    /// Type: number
    /// Default: 0
    /// When set to a positive value, mutt will wrap text at $wrap characters. When set to a negative value, mutt will wrap text so that there are $wrap characters of empty space on the right side of the terminal. Setting it to zero makes mutt wrap at the terminal width.
    Wrap,

    /// wrap_headers
    ///
    /// Type: number
    /// Default: 78
    ///
    /// This option specifies the number of characters to use for wrapping an outgoing message's headers. Allowed values are between 78 and 998 inclusive.
    ///
    /// Note: This option usually shouldn't be changed. RFC5233 recommends a line length of 78 (the default), so please only change this setting when you know what you're doing.
    WrapHeaders,

    /// wrap_search
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Controls whether searches wrap around the end.
    ///
    /// When set, searches will wrap around the first (or last) item. When unset, incremental searches will not wrap.
    WrapSearch,

    /// wrapmargin
    ///
    /// Type: number
    /// Default: 0
    ///
    /// (DEPRECATED) Equivalent to setting $wrap with a negative value.
    Wrapmargin,

    /// write_bcc
    ///
    /// Type: boolean
    /// Default: yes
    ///
    /// Controls whether mutt writes out the "Bcc:" header when preparing messages to be sent. Exim users may wish to unset this. If mutt is set to deliver directly via SMTP (see $smtp_url), this option does nothing: mutt will never write out the "Bcc:" header in this case.
    WriteBcc,

    /// write_inc
    ///
    /// Type: number
    /// Default: 10
    ///
    /// When writing a mailbox, a message will be printed every $write_inc messages to indicate progress. If set to 0, only a single message will be displayed before writing a mailbox.
    ///
    /// Also see the $read_inc, $net_inc and $time_inc variables and the "tuning" section of the manual for performance considerations.
    WriteInc,
}
