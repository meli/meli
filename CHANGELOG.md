# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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
