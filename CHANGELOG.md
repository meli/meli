# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Added shortcuts for focusing to sidebar menu and back to the e-mail view (`focus_left` and `focus_right`)
- `f76f4ea3` A new manual page, `meli.7` which contains a general tutorial for using meli.
- `cbe593cf` add configurable header preample suffix and prefix for editing
- `a484b397` Added instructions and information to error shown when libnotmuch could not be found.
- `a484b397` Added configuration setting `library_file_path` to notmuch backend if user wants to specify the library's location manually.
- `aa99b0d7` Implement configurable subject prefix stripping when replying
- `a73885ac` added RGB support to embedded terminal emulator.
- `f4e0970d` added ability to kill embed process with Ctrl-C, or Ctrl-Z and pressing 'q'.
- `9205f3b8` added a per account mail sort order parameter.
- `d921b3c3` implemented sorting with user sort order parameter if defined.
- `dc5afa13` use osascript/applescript for notifications on macos
- `d0de0485` add {in,de}crease_sidebar shortcuts
- `340d6451` add config setting for sidebar ratio
- `36e29cb6` Add configurable mailbox sort order

### Changed

- `f76f4ea3` Shortcut `open_thread` and `exit_thread` renamed to `open_entry` and `exit_entry`.
- `7650805c` Binary size reduced significantly.

### Fixed

- `a42a6ca8` show notifications in terminal if there is no other alternative.

## [alpha-0.7.2] - 2021-10-15

### Added

- Add forward mail option
- Add url_launcher config setting
- Add add_addresses_to_contacts command
- Add show_date_in_my_timezone pager config flag
- docs: add pager filter documentation
- mail/view: respect per-folder/account pager filter override
- pager: add filter command, esc to clear filter
- Show compile time features in with command argument

### Fixed

- melib/email/address: quote display_name if it contains ","
- melib/smtp: fix Cc and Bcc ignored when sending mail
- melib/email/address: quote display_name if it contains "."

## [alpha-0.7.1] - 2021-09-08

### Added

- Change all Down/Up shortcuts to j/k
- add 'GB18030' charset
- melib/nntp: implement refresh
- melib/nntp: update total/new counters on new articles
- melib/nntp: implement NNTP posting
- configs: throw error on extra unusued conf flags in some imap/nntp
- configs: throw error on missing `composing` section with explanation

### Fixed

- Fix compilation for netbsd-9.2
- conf: fixed some boolean flag values requiring to be string e.g. "true"

## [alpha-0.7.0] - 2021-09-03

### Added

Notable changes:

- add import command to import email from files into accounts
- add add-attachment-file-picker command and `file_picker_command` setting to
  use external commands to choose files when composing new mail
- ask confirm for delete
- add export-mbox command
- add export-mail command
- add TLS support with nntp
- add JMAP watch with polling
- add reload-config command
- add import-mail command
- imap: implement gmail XOAUTH2 authentication method
- imap: implement OAUTH2 authentication
- compose: treat inline message/rfc822 as attachments
- add gpg support via libgpgme

### Fixed

- Loading notmuch library on macos
- Limit dbus dependency to target_os = "linux"
- IMAP, notmuch, mbox backends: various performance fixes

## [alpha-0.6.2] - 2020-09-24

### Added
- Add customizable mailbox tree in sidebar
- Make `dbus` dependency opt-out (feature is `dbus-notifications`)
- Implemented JMAP async, search, tagging, syncing
- Preserve account order from configuration file
- Implemented IMAP `CONDSTORE` support for IMAP cache
- Add `timeout` setting for IMAP
- Implement TCP keepalive for IMAP
- Rewrote email address parsers.
- Implement `copy_messages` for maildir
- Implement selection with motions

### Fixed
- Fixed various problems with IMAP cache
- Fixed various problems with IMAP message counts
- Fixed various problems with IMAP connection hanging
- Fixed IMAP not reconnecting on dropped IDLE connections
- Fixed various problems with notmuch backend

## [alpha-0.6.1] - 2020-08-02

### Added

- added experimental NNTP backend
- added server extension support and use in account status tab

### Fixed

- imap: fixed IDLE connection getting stuck when using DEFLATE

## [alpha-0.6.0] - 2020-07-29

### Added

- Add `select` command to select threads that match search query
- Add support for mass copying/deleting/flagging/moving of messages
- IMAP: add support for COMPRESS=DEFLATE and others
  Extension use can be configured with individual flags such as `use_deflate`
- Rename EXECUTE mode to COMMAND
- add async IMAP backend
- add in-app SMTP support
- ui: Show decoded source by default when viewing an Envelope's source
- ui: Add search in pagers
- Add managesieve REPL binary for managesieve script management
- imap: `add server_password_command`
- configuration: Add per-folder and per-account configuration overrides.
  e.g. `accounts."imap.domain.tld".mailboxes."INBOX".index_style = "plain"`

  The selection is done for a specific field as follows:

  ```text
  if per-folder override is defined, return per-folder override
    else if per-account override is defined, return per-account override
      else return global setting field value.
  ```
- themes: Add Italics, Blink, Dim and Hidden text attributes
- ui: recognize readline shortcuts in Execute mode
- ui: hopefully smarter auto-completion in Execute mode
- demo NNTP python plugin
- ui: add `auto_choose_multipart_alternative`: Choose `text/html` alternative if `text/plain` is empty in `multipart/alternative` attachments.
- ui: custom date format strings
- ui: manual refresh for mailbox view
- ui: create mailbox command
- fs autocomplete
- ui: add support for [`NO_COLOR`](https://no-color.org/)
- enhanced, portable Makefile
- added Debian packaging
- added `default_header_values`: default header values used when creating a new draft
- ui: switch between sidebar and mailbox view with {left,right} keys for more intuitive navigation
- ui: add optional filter query for each mailbox view to view only the matching subset of messages (for example, you can hide all seen envelopes with `filter = "not flags:seen"`

### Changed

- Replace any use of 'folder' with 'mailbox' in user configuration
- Load libnotmuch dynamically
- Launch all user shell commands with `sh -c "..."`

### Fixed

- notmuch: add support for multiple accounts on same notmuch db

## [alpha-0.5.1] - 2020-02-09

### Added

- Added in-terminal floating notifications with history
- Added mailbox creation/deletion commands in IMAP accounts
- Added cli-docs compile time feature: Optionally build manpages to text with mandoc and print them from the command line.
- Added new theme keys

[unreleased]: #
[alpha-0.5.1]: https://github.com/meli/meli/releases/tag/alpha-0.5.1
[alpha-0.6.0]: https://github.com/meli/meli/releases/tag/alpha-0.6.0
[alpha-0.6.1]: https://github.com/meli/meli/releases/tag/alpha-0.6.1
[alpha-0.6.2]: https://github.com/meli/meli/releases/tag/alpha-0.6.2
[alpha-0.7.0]: https://github.com/meli/meli/releases/tag/alpha-0.7.0
[alpha-0.7.1]: https://github.com/meli/meli/releases/tag/alpha-0.7.1
[alpha-0.7.2]: https://github.com/meli/meli/releases/tag/alpha-0.7.2
