# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

<!-- ### Added -->

<!-- ### Bug Fixes -->

<!-- ### Changes -->

<!-- ### Refactoring -->

<!-- ### Documentation -->

<!-- ### Packaging -->

<!-- ### Miscellaneous Tasks -->

## [v0.8.12] - 2025-04-21

This release fixes compilation under macos which was broken with the previous release, [v0.8.11].

### Bug Fixes

- [**`a2bc4420`**](https://git.meli-email.org/meli/meli/commit/a2bc442044cd08a8433cc2060f590e4a1f2ec513) `Fix compilation under macos`

## [v0.8.11] - 2025-04-21

This is mostly a fixups release.

Contributors in alphabetical order:

- Guillaume Ranquet
- Manos Pitsidianakis
- n4n5

Highlights:

- [**`2870624d`**](https://git.meli-email.org/meli/meli/commit/2870624d44ac53762ab6c8a9c4b8e36c7e147ab4) `contacts: add notmuch address book support` in PR [`#551` "contacts: add notmuch address book support"](https://git.meli-email.org/meli/meli/pulls/551)

  The new `notmuch_address_book_query` account setting defines a query passed to
  `notmuch address` command to import contacts into meli.

  Contacts are parsed and imported read-only.

  Example query to import all contacts in the past 6 Months:

  ```toml
  notmuch_address_book_query = "--output=recipients --deduplicate=address date:6M.."
  ```
- IMAP connections now use connection pools to prevent an on-going operation blocking other operations.
  This is because IMAP connections are stateful and we cannot re-use a single TCP connection for parallel operations.
  The `use_connection_pool` account setting can be used to disable this behavior.
- Add setting for UI notifications (as opposed to system notifications).

  These are the notifications that appear as a floating dialog on the UI.

  The new `notifications.ui_notifications` setting has three possible
  values:

  - `show` (default)
  - `hide`
  - `system` (show them as system notifications)
- Fixed invalid terminal behavior by turning line wraparound mode off.
- Replying to self now sets e-mail receiver to previous recipients instead of self.
- IMAP now supports overriding server subscriptions in configuration.
- Composer now shows attachments after headers, making them more visible.
- `mbox` parsing is now more rigorous by not allowing mixed format behaviors and requiring a given format to parse.
- Remote account watching has been refactored to be more fault-tolerant and hiding errors from user if retrying is successful. 

### Added

- [**`2870624d`**](https://git.meli-email.org/meli/meli/commit/2870624d44ac53762ab6c8a9c4b8e36c7e147ab4) `contacts: add notmuch address book support` in PR [`#551` "contacts: add notmuch address book support"](https://git.meli-email.org/meli/meli/pulls/551)
- [**`bb4fc023`**](https://git.meli-email.org/meli/meli/commit/bb4fc023434d173f534cf297ac28da719d77420b) `melib/imap: add ConnectionMutex struct`
- [**`632a1db1`**](https://git.meli-email.org/meli/meli/commit/632a1db111d446aee7685debfe8c81c31a7bf2e0) `melib/imap: add conn pool to ConnectionMutex`
- [**`919fd00c`**](https://git.meli-email.org/meli/meli/commit/919fd00c5555f25fae168deeb309e8c87ad7ce22) `melib/error: hardcode native-tls error check`
- [**`5bcd62cd`**](https://git.meli-email.org/meli/meli/commit/5bcd62cd76827dd3abfac66d4e365c39c8f44442) `widgets: don't add padding by skipping cols in FormWidget`
- [**`9417f3d0`**](https://git.meli-email.org/meli/meli/commit/9417f3d057236ab5a3b8a6d285bfdf17e795775a) `melib/error: add TLSConnectionFailed kind variant` in PR [`#578` "Misc fixes"](https://git.meli-email.org/meli/meli/pulls/578)
- [**`c5c56b59`**](https://git.meli-email.org/meli/meli/commit/c5c56b59a5ca80391db15981bb7560f13f890960) `notifications: add DisplayMessageBox::arm()`
- [**`3e5fe792`**](https://git.meli-email.org/meli/meli/commit/3e5fe792bcaac45e2c51f074fff9054073d2928d) `conf: add notifications.ui_notifications setting`
- [**`6c1194a5`**](https://git.meli-email.org/meli/meli/commit/6c1194a5dc1565520a8908fd7c50439ecd6c2885) `notifications: add DisplayMessageBox::deactivate()`
- [**`4a03c667`**](https://git.meli-email.org/meli/meli/commit/4a03c6675b17d51da62cb1355376d61ca7f96d92) `notifications: add DisplayMessageBox::show_{prev,next}()`
- [**`9f2e4384`**](https://git.meli-email.org/meli/meli/commit/9f2e4384f065c71c8e07a37c5dbdb96b2b28de39) `notifications: make update_xbiff() a method` in PR [`#581` "Add `notifications.ui_notifications` setting to hide UI notifications or show them as system notifications"](https://git.meli-email.org/meli/meli/pulls/581)
- [**`009c5379`**](https://git.meli-email.org/meli/meli/commit/009c53797145e4024cd5e4709d444d80eea4c228) `melib/imap: add FIXME comment about imap-codec`
- [**`26cb414d`**](https://git.meli-email.org/meli/meli/commit/26cb414dc1d503783f18b9200ca56f9f5121493e) `melib/jmap: add serde(default) to Identity::id`
- [**`f9fa10f8`**](https://git.meli-email.org/meli/meli/commit/f9fa10f88228bb60a0e8949a8104d0f055d7b734) `melib/jmap: s/Id::new_uuid_v4()/Id::new_random()`
- [**`48ae7687`**](https://git.meli-email.org/meli/meli/commit/48ae7687c7abb0e76c523137cd756c9b51e48f7f) `melib/jmap: add serde(default) to Identity::may_delete`
- [**`99c0f5a3`**](https://git.meli-email.org/meli/meli/commit/99c0f5a3a7daac6aeea33b187b7adccdf5d60a96) `melib/jmap: Add State::new_random() method`
- [**`7a1ff0d2`**](https://git.meli-email.org/meli/meli/commit/7a1ff0d281b779d7e57204032b8f249c065251ed) `melib/notmuch: add FFI config types bindings`
- [**`335f0229`**](https://git.meli-email.org/meli/meli/commit/335f02293b8441bbf1eb7c51b8deef0207e7c44e) `melib/notmuch: add notmuch_database_reopen()`
- [**`e8420862`**](https://git.meli-email.org/meli/meli/commit/e8420862b075c27deb83bbce755caaa5d3857d0d) `melib/notmuch: add Message::msg_id_str() method`
- [**`c2b648af`**](https://git.meli-email.org/meli/meli/commit/c2b648af1b53e0152c536aecfdb2e43dbdedf21c) `melib/notmuch: add Message::find_message_by_path()`
- [**`5846c867`**](https://git.meli-email.org/meli/meli/commit/5846c86732148a8cac8a9d21490d9886dd900e64) `melib/notmuch: impl From<NotmuchError> for Error`
- [**`648cf6ab`**](https://git.meli-email.org/meli/meli/commit/648cf6ab39c16f800697fc6f0967e7660921a5af) `melib/imap: add use_connection_pool conf option`
- [**`df19008d`**](https://git.meli-email.org/meli/meli/commit/df19008dd9276eaf785f56411ca9395f91361fec) `melib/jmap: add session object to backend metadata`
- [**`3565f682`**](https://git.meli-email.org/meli/meli/commit/3565f682ad735e90bf608d03f259aa2d45429ec3) `melib: convert MailBackend::watch into a stream`
- [**`e2ddbb7b`**](https://git.meli-email.org/meli/meli/commit/e2ddbb7b4f749052104420c9cf3cf8a485821d9e) `melib/maildir: switch backend.rs and mod.rs`
- [**`18812159`**](https://git.meli-email.org/meli/meli/commit/18812159757829473d751563d40908ce51ef229a) `melib/maildir: enhance filesystem/IO error metadata`
- [**`bc99dfa9`**](https://git.meli-email.org/meli/meli/commit/bc99dfa96037eef5c44d0e9e717ee22f8dde635c) `melib/maildir: add ErrorKind::NotImplemented to unimplemented errors`
- [**`6d06da21`**](https://git.meli-email.org/meli/meli/commit/6d06da21348add117311666723570abe8a4c9718) `melib/maildir: s/MaildirPathTrait/MaildirFilePathExt/`
- [**`55d9bb8d`**](https://git.meli-email.org/meli/meli/commit/55d9bb8d5397b18dfa30444b3c708eb3c2d6e965) `melib/maildir: Add MaildirMailboxPathExt::validate_fs_subdirs()`
- [**`6c8bf721`**](https://git.meli-email.org/meli/meli/commit/6c8bf721c3ee61d7c412e6088b8f27c2e30a516d) `melib/smtp: add ReplyCode::value() method`
- [**`14f64167`**](https://git.meli-email.org/meli/meli/commit/14f641673576685a857a43f1c8ced4417f3f2e75) `listing: don't forward events to self.component twice`
- [**`a5f9b3ec`**](https://git.meli-email.org/meli/meli/commit/a5f9b3ec081d8fa6ecabc092584f33e7574cc198) `listing/offline: don't accumulate repeated messages`
- [**`28f3f5e2`**](https://git.meli-email.org/meli/meli/commit/28f3f5e2d014da26f35481485386c6d26cdd2ad6) `melib/error: add ErrorKind::is_disconnected()`
- [**`f5afcf77`**](https://git.meli-email.org/meli/meli/commit/f5afcf77539b973d30e9f340b1b2b8324f7f3505) `melib/imap: add support for LITERAL- extension` in PR [`#604` "melib/imap: add support for LITERAL- extension"](https://git.meli-email.org/meli/meli/pulls/604)
- [**`07603558`**](https://git.meli-email.org/meli/meli/commit/076035580c5d2aed00219fbe54c11f1a2f4e38b8) `composer: add custom_compose_hooks only once`
- [**`4a1dff4b`**](https://git.meli-email.org/meli/meli/commit/4a1dff4b95c9bc7a82c84407c2dc270dde5fdf01) `show error summary if not empty` in PR [`#586` "show error summary"](https://git.meli-email.org/meli/meli/pulls/586)
- [**`09467034`**](https://git.meli-email.org/meli/meli/commit/09467034ff73b23bb10c4e77f9fec8e7286f9e30) `themes: impl BitOr<Attr> for ThemeAttribute`
- [**`c11bda5c`**](https://git.meli-email.org/meli/meli/commit/c11bda5cfedece32e7c34ee21cebf905e3694736) `Break metadata json lines in account status`
- [**`e6d115fa`**](https://git.meli-email.org/meli/meli/commit/e6d115facc7d37a15e61eba0d384e9491d1e443c) `melib/logging: manually redact http auth info` in PR [`#570` "melib/logging: manually redact http auth info"](https://git.meli-email.org/meli/meli/pulls/570)
- [**`778c4baa`**](https://git.meli-email.org/meli/meli/commit/778c4baa4e6c09d6bd68a50c7a948e36a3df284c) `mail/listing: restore selection when refreshing same mailbox` in PR [`#571` "mail/listing: restore selection when refreshing same mailbox"](https://git.meli-email.org/meli/meli/pulls/571)

### Bug Fixes

- [**`30c599ab`**](https://git.meli-email.org/meli/meli/commit/30c599ab925172898bc46215c02bd772cebaa70b) `Makefile: fix some minor logic/UX issues` in PR [`#559` "Add some doc comments in `meli` crate"](https://git.meli-email.org/meli/meli/pulls/559)
- [**`1b300805`**](https://git.meli-email.org/meli/meli/commit/1b3008057a06c65fcfa0a61616b22e576e3d6e81) `pager: fix one-by-off error when drawing scrollbar`
- [**`5a02e81a`**](https://git.meli-email.org/meli/meli/commit/5a02e81ac0c61ee3e0fa1b7f295cc0b1c691d189) `melib/imap: fix ImapLineIterator splitting literals`
- [**`d2b2253a`**](https://git.meli-email.org/meli/meli/commit/d2b2253ad6bb84c7ab6c15ec81bdbb501c05fdf4) `melib/mbox: require MboxFormat for parsing`
- [**`5ffeb7c1`**](https://git.meli-email.org/meli/meli/commit/5ffeb7c1e6d900d5d6937c3c303a212b8b371f0a) `tools: fix mboxparse binary compilation`
- [**`e12412f9`**](https://git.meli-email.org/meli/meli/commit/e12412f907e0e226ac82d3da73450caa2c3656c7) `melib/email: fix base36 conversion`
- [**`db93e324`**](https://git.meli-email.org/meli/meli/commit/db93e3240ea5c498ce14015868bc0051dc28cfc2) `melib: fixup TryFrom<&[EnvelopeHash]> for EnvelopeHashBatch impl`
- [**`c3a93f91`**](https://git.meli-email.org/meli/meli/commit/c3a93f9112582fb4489f361605251dcba212585a) `melib/jmap: Fix SetResponse struct definition`
- [**`8b374cac`**](https://git.meli-email.org/meli/meli/commit/8b374cac0e1aacfe19932748eed91c1970d1c7fd) `melib/maildir: split create_mailbox_sync()`
- [**`6e09a7ef`**](https://git.meli-email.org/meli/meli/commit/6e09a7ef98b8defde5ae2501ba273c498414379a) `melib/datetime: parse RFC2822 date without seconds` in PR [`#599` "melib/email/parser: elaborate Address parsing errors"](https://git.meli-email.org/meli/meli/pulls/599)
- [**`007628e3`**](https://git.meli-email.org/meli/meli/commit/007628e37f6d49e86bf6ac7eecf4b31dfc206b12) `melib/tests/jmap: fix errata from RFC` in PR [`#602` "misc-fixes"](https://git.meli-email.org/meli/meli/pulls/602)
- [**`7ee89450`**](https://git.meli-email.org/meli/meli/commit/7ee894508fb21993cca7ae4493e210709fd2b2a6) `melib/nntp: fix error message typos`
- [**`1164f61d`**](https://git.meli-email.org/meli/meli/commit/1164f61db5dcf9eb1f7b3f38b2adb80501c05915) `Fix clippy::sliced_string_as_bytes`
- [**`752d3908`**](https://git.meli-email.org/meli/meli/commit/752d3908071e910cada674ce6e260b9a2251f44a) `melib: fix encoded-word encode and add tests` in PR [`#612` "melib: fix encoded-word encode and add tests"](https://git.meli-email.org/meli/meli/pulls/612)
- [**`0cc52727`**](https://git.meli-email.org/meli/meli/commit/0cc527271ff92e1f484fa0b8b5a92733175b2fe4) `melib/mailto: fix parsing header values with '?'` in PR [`#613` "melib/mailto: fix parsing header values with '?'"](https://git.meli-email.org/meli/meli/pulls/613)
- [**`8307a5f0`**](https://git.meli-email.org/meli/meli/commit/8307a5f05e4944f322abe1c1b141eb5b59e21323) `melib/tests: feature gate test_maildir_config`
- [**`0e1d9f00`**](https://git.meli-email.org/meli/meli/commit/0e1d9f0062351e6dc401e637e9a558ac49cdc20f) `melib/imap: add FETCH items variants in RequiredResponses` in PR [`#580` "IMAP fixes"](https://git.meli-email.org/meli/meli/pulls/580)
- [**`e8e52bd1`**](https://git.meli-email.org/meli/meli/commit/e8e52bd10062d2458673eca4aeaabbf6c8752c95) `melib/imap: only process parsed untagged responses`
- [**`d27e5d32`**](https://git.meli-email.org/meli/meli/commit/d27e5d32ee283a1d81691731de82549a1a03a788) `melib/jmap: don't abort refresh if Mailbox has no email_query_state`
- [**`e0bac37a`**](https://git.meli-email.org/meli/meli/commit/e0bac37a00203c666105d83b5bde6c100f2afbb0) `accounts: don't return after exec refresh_command` in PR [`#597` "conf/tests: flush config file to disk"](https://git.meli-email.org/meli/meli/pulls/597)
- [**`4b5ad739`**](https://git.meli-email.org/meli/meli/commit/4b5ad7395c69c8534e070533e77300b95ea2656c) `terminal: disable wraparound mode` in PR [`#616` "terminal: disable wraparound mode"](https://git.meli-email.org/meli/meli/pulls/616)

### Changes

- [**`94d04023`**](https://git.meli-email.org/meli/meli/commit/94d04023baf664bbe91e5e2db6cc7fcc6dee482f) `compose: s/attachments/attachment when # is 1`
- [**`1207217c`**](https://git.meli-email.org/meli/meli/commit/1207217cd796fa7309bf3675eb5371a7151a9040) `compose: s/Option<AccountHash>/AccountHash in EditAttachments`
- [**`aa363be6`**](https://git.meli-email.org/meli/meli/commit/aa363be6fbf7e26d82441dccc5307849b634f209) `utilities: respect scroll_{left,right} shortcuts in Form/Button widgets`
- [**`99f6b17b`**](https://git.meli-email.org/meli/meli/commit/99f6b17ba0565b35859f99be186fd05f475517c2) `melib/imap: retry IDLE with exponential backoff`
- [**`629e6938`**](https://git.meli-email.org/meli/meli/commit/629e69382ca32e925c08c96ea7a55d4d082b1d83) `compose: don't pad EditAttachments area`
- [**`269c2cdb`**](https://git.meli-email.org/meli/meli/commit/269c2cdb444f013d7883343b9247fe5a749fc4c2) `terminal/screen: Screen::resize*() optimization` in PR [`#568` "mail/status.rs: restore AccountStatus scrolling"](https://git.meli-email.org/meli/meli/pulls/568)
- [**`6e0502f0`**](https://git.meli-email.org/meli/meli/commit/6e0502f061ae629ed25514e5f930ef2c59e8de3d) `melib/imap: impl Default for ImapProtocol`
- [**`1cf11d11`**](https://git.meli-email.org/meli/meli/commit/1cf11d11f2448464f70e63d4cc956d202b73f1d9) `melib/mbox: update totals when fetching` in PR [`#591` "melib/mbox: update totals when fetching"](https://git.meli-email.org/meli/meli/pulls/591)
- [**`73102337`**](https://git.meli-email.org/meli/meli/commit/73102337dae1e3691cc562e785a377a770e5a677) `melib/mbox: check for CRLF when parsing`
- [**`94f4bbd2`**](https://git.meli-email.org/meli/meli/commit/94f4bbd258e61513a64a9e69599fe26a661838ca) `melib/gpgme: allow unused_unsafe on std::env stuff`
- [**`930296d2`**](https://git.meli-email.org/meli/meli/commit/930296d24ff6bc2d7631796ae09d3eb028dc1695) `melib/jmap: derive Clone for MailboxObject`
- [**`fee3d758`**](https://git.meli-email.org/meli/meli/commit/fee3d7587019d414a7c47d6e6b603aea49c18ee6) `melib/notmuch: split symbol name in call!() macro`
- [**`d8f4bb7c`**](https://git.meli-email.org/meli/meli/commit/d8f4bb7ca401fc4dbd0a05c534d7655591fcf795) `melib/imap: make keepalive at least 30 minutes` in PR [`#592` "misc-fixes"](https://git.meli-email.org/meli/meli/pulls/592)
- [**`3c26c9bd`**](https://git.meli-email.org/meli/meli/commit/3c26c9bd3ded7e43e46ac8e9a493dbbbba74ba4d) `melib: impl From for BackendEvent`
- [**`cb72e5cd`**](https://git.meli-email.org/meli/meli/commit/cb72e5cdf7519f189dd6179078ce352f6055b4d7) `melib: impl TryFrom<Vec> for BackendEvent`
- [**`9e405c5a`**](https://git.meli-email.org/meli/meli/commit/9e405c5a754c4cb07ab74f1db7db812fcbe6ea1e) `melib/maildir: validate fs path in list_mail_in_maildir_fs()`
- [**`f94adfdd`**](https://git.meli-email.org/meli/meli/commit/f94adfdd38cbf585f6aacbfb4d6514ec0454c200) `melib/maildir: split MaildirPathTrait`
- [**`177e4928`**](https://git.meli-email.org/meli/meli/commit/177e4928efc3b09f92e604efd555d28998ce24bd) `melib/maildir: log error instead of debug_assert!`
- [**`46e405f3`**](https://git.meli-email.org/meli/meli/commit/46e405f3fd92c8dd6882bf22a1ca2bd225af9841) `melib/maildir: use async channel in watch()`
- [**`94a458fa`**](https://git.meli-email.org/meli/meli/commit/94a458fad6d64c09b269e7244e68fcc63eef8a09) `melib/maildir: return path in save_to_mailbox()`
- [**`9f83c0a8`**](https://git.meli-email.org/meli/meli/commit/9f83c0a85f0ad94b58be3df8ad844ea5618e7df5) `melib/maildir: calc MailboxHash from cache in watch()`
- [**`3887d999`**](https://git.meli-email.org/meli/meli/commit/3887d9995ccca90703ebaa7bebf1e5a21d9930c0) `melib/jmap: impl Deserialize for Filter, Query`
- [**`3ccf499f`**](https://git.meli-email.org/meli/meli/commit/3ccf499f545f4cb308207d2d98f5f561991a72c9) `melib/jmap: impl Serialize for EmailObject`
- [**`6bf0844c`**](https://git.meli-email.org/meli/meli/commit/6bf0844ca55c674e370ee024facc85083dc56ba5) `melib/email/parser: elaborate Address parsing errors`
- [**`e89ee209`**](https://git.meli-email.org/meli/meli/commit/e89ee2096203e5f3570eab4c9618d7dd601617c4) `melib/nntp: increase tcp keepalive to 30 mins`
- [**`a67299eb`**](https://git.meli-email.org/meli/meli/commit/a67299ebb32da4149d3d470c77237fa1b00889ed) `melib/nntp: set appropriate ErrorKind to errors`
- [**`5c9651c6`**](https://git.meli-email.org/meli/meli/commit/5c9651c6ef148744ea47a9da62c0508914300d61) `melib/nntp: convert block comments to line`
- [**`592e06b1`**](https://git.meli-email.org/meli/meli/commit/592e06b172fcc6bd314374d8a51a98a0cf890164) `melib/nntp: retry fetch if disconnected`
- [**`cdce705b`**](https://git.meli-email.org/meli/meli/commit/cdce705b93661b5edbe4826f10522af0845aac16) `melib/nntp: try connect first before attempting operations`
- [**`e2e793d2`**](https://git.meli-email.org/meli/meli/commit/e2e793d21a7f8917a7ea9e9a03b39f49e96dc6b1) `composer: allow empty default header values`
- [**`1a3f6179`**](https://git.meli-email.org/meli/meli/commit/1a3f6179ddd186dd3654a15ffef7d449e7112078) `melib/nntp: run rustfmt`
- [**`55e8b41e`**](https://git.meli-email.org/meli/meli/commit/55e8b41e1447ad9e1d6e23ef771a6f133677ab95) `composer: use rfc5536 format for User-Agent value`
- [**`08e53da1`**](https://git.meli-email.org/meli/meli/commit/08e53da13451c2abbb5d0603cc2c74317a7d9ce0) `composer: replying to self should reply to previous recipients`
- [**`cb47b212`**](https://git.meli-email.org/meli/meli/commit/cb47b2126cd1cee10df7f88246e5e1c0f7854291) `conf: impl DotAddressable for ComposingSettings` in PR [`#605` "Misc composer stuff"](https://git.meli-email.org/meli/meli/pulls/605)
- [**`9eba4c1f`**](https://git.meli-email.org/meli/meli/commit/9eba4c1f133318e527006ef11731d6b40436f572) `melib/imap: BEGIN IMMEDIATE transactions`
- [**`2663c076`**](https://git.meli-email.org/meli/meli/commit/2663c07692367d5d15c0bd537da32e812dbe778b) `melib/imap: support server subscriptions override`
- [**`fd19cabd`**](https://git.meli-email.org/meli/meli/commit/fd19cabd58b2b022ae93af5376208f230186a9f4) `contacts/editor: use AccountHash instead of index`
- [**`e11ab834`**](https://git.meli-email.org/meli/meli/commit/e11ab83417788896bde232134ead297b79c3e60a) `view/envelope: use Context's cmd_buf` in PR [`#615` "misc"](https://git.meli-email.org/meli/meli/pulls/615)
- [**`6cd5bacd`**](https://git.meli-email.org/meli/meli/commit/6cd5bacd4eb781ac4d347d45e015504b003c7ee3) `melib/text: allow empty generated unicode tables`
- [**`22985e1b`**](https://git.meli-email.org/meli/meli/commit/22985e1be9084a46287b1235aa9114bdd2a2da81) `melib/jmap: ser/de SetError manually` in PR [`#594` "JMAP fixups"](https://git.meli-email.org/meli/meli/pulls/594)
- [**`ed1aca57`**](https://git.meli-email.org/meli/meli/commit/ed1aca57f7157be18c1ad597849f8ffda856c716) `melib/jmap: fix Get ser/de`
- [**`c9d1ae21`**](https://git.meli-email.org/meli/meli/commit/c9d1ae2192f758e021867383e466750264b89f73) `melib/maildir: remove stream module`
- [**`83892e9d`**](https://git.meli-email.org/meli/meli/commit/83892e9dd07227b5f1ace1104b36c636c876fc6f) `Fix clippy::manual_ok_err`
- [**`a94ebd11`**](https://git.meli-email.org/meli/meli/commit/a94ebd11c115327a7b587d960814c65b03939a4e) `accounts: check if online on watch errors`
- [**`37edaea6`**](https://git.meli-email.org/meli/meli/commit/37edaea6e079051050cf346871f912e1ff3a6612) `compose: rewrite to draw attachments after headers`

### Refactoring

- [**`6d998f54`**](https://git.meli-email.org/meli/meli/commit/6d998f54c8ffdeed41cc25357c78748d175c34a3) `melib/backends: remove obsolete ReadOnlyOp`
- [**`ef3313ce`**](https://git.meli-email.org/meli/meli/commit/ef3313cea2037c9c048e5506a0dad92997c0d3fb) `melib/contacts: fix formatting of a log::warn line`
- [**`2f0afd14`**](https://git.meli-email.org/meli/meli/commit/2f0afd14678473fdece568c4249bb0e647627750) `melib/email/headers: add #[inline] to small methods`
- [**`2a46c970`**](https://git.meli-email.org/meli/meli/commit/2a46c970c4152ffb76d3c08c5ae6008e1b49132b) `melib/backend: remove BackendOp`
- [**`f3470919`**](https://git.meli-email.org/meli/meli/commit/f34709198264227a7bf18d6fafcc47f5688ea4ff) `melib/imap/connection.rs: remove unnecessary let`
- [**`d06f1882`**](https://git.meli-email.org/meli/meli/commit/d06f1882eebff49187c06cd7138ae954c81eb9dc) `compose: rename Cursor to Focus`
- [**`5468835b`**](https://git.meli-email.org/meli/meli/commit/5468835b3a14e3559c0a88570eba2adba40eea4d) `melib/email: move imap stuff to melib::imap module`
- [**`5e3c1a2f`**](https://git.meli-email.org/meli/meli/commit/5e3c1a2f38ca2f0078b3957478f5bc42e1f7a1c4) `main.rs: use unbounded channel for thread events` in PR [`#565` "Misc"](https://git.meli-email.org/meli/meli/pulls/565)
- [**`483edecf`**](https://git.meli-email.org/meli/meli/commit/483edecfedcd7b9f6cf8c2a1915f102782ca6d98) `mail/status.rs: restore AccountStatus scrolling`
- [**`fe362a14`**](https://git.meli-email.org/meli/meli/commit/fe362a14d0e793e72c9362fd9ddcf7376f345df3) `melib/imap: fix mailbox update logic on expunge` in PR [`#567` "melib/imap: fix mailbox update logic on expunge"](https://git.meli-email.org/meli/meli/pulls/567)
- [**`f3caea36`**](https://git.meli-email.org/meli/meli/commit/f3caea360d99878fec6c553ce7203d16387a7462) `mail/listing: don't move cursor to top when refreshing` in PR [`#572` "mail/listing: don't move cursor to top when refreshing"](https://git.meli-email.org/meli/meli/pulls/572)
- [**`fe522f7a`**](https://git.meli-email.org/meli/meli/commit/fe522f7a426e9425e0414f1354639fd79620a968) `Fix new clippy complexity lint warning`
- [**`55062fd4`**](https://git.meli-email.org/meli/meli/commit/55062fd4fe78b3d0267cd01188acfcb7b23e0464) `mail/listing.rs: forward unhandled events`
- [**`e93704c8`**](https://git.meli-email.org/meli/meli/commit/e93704c864065293633ff7938dd174abf627f802) `mail/listing.rs: fix y offset calc in menu cursor with collapsed`
- [**`9d3c2f81`**](https://git.meli-email.org/meli/meli/commit/9d3c2f819290ecc8e13f7c0fc8a27a20e1ca2b24) `Rename ForkType to ForkedProcess`
- [**`21e19403`**](https://git.meli-email.org/meli/meli/commit/21e1940333d7be467e8554b1c251116641a2cddd) `types: replace ForkedProcess::Finished with UIEvent`
- [**`a2b7f1cc`**](https://git.meli-email.org/meli/meli/commit/a2b7f1ccf36bf502c2ed17d645f715391f87939a) `state.rs: rewrite State::try_wait_on_child()`
- [**`c9c125ef`**](https://git.meli-email.org/meli/meli/commit/c9c125ef89590915cbdbc685c5d66c726d1e9a4b) `state.rs: refactor kill_child() into method`
- [**`0d780354`**](https://git.meli-email.org/meli/meli/commit/0d7803549bf8fd4d30f0df5dbb60858b1e7a2625) `state.rs: refactor try_wait_on_child() into method`
- [**`c1e46c01`**](https://git.meli-email.org/meli/meli/commit/c1e46c01c8ac52c22f1adfa541172fe427db1271) `state.rs: use ForkedProcess::{kill, try_wait} on State::drop`
- [**`ea74055d`**](https://git.meli-email.org/meli/meli/commit/ea74055daefa204b11fb3f81e8cebc393fa56b6d) `main.rs: try wait on all forked process on SIGCHLD` in PR [`#579` "Refactor handling of forked processes"](https://git.meli-email.org/meli/meli/pulls/579)
- [**`26a86961`**](https://git.meli-email.org/meli/meli/commit/26a8696120d5dd5aeb5b9bffc403a37d0c8536b6) `melib/imap: remove unnecessary RequiredResponses consts`
- [**`b46e1311`**](https://git.meli-email.org/meli/meli/commit/b46e1311ee94d774c40d9854bad4ab5723fdd11a) `melib/imap: rename {EXAMINE,SELECT}_REQUIRED`
- [**`3dbeba1c`**](https://git.meli-email.org/meli/meli/commit/3dbeba1cf089670e38cf256523c313e4b9f539b2) `melib/imap: rename NO_REQUIRED to NO`
- [**`7272f8fe`**](https://git.meli-email.org/meli/meli/commit/7272f8fee0c329ed45b81165c9a8f13aa57caef5) `tools: fix lints and compilation` in PR [`#587` "melib/mbox: check for CRLF when parsing"](https://git.meli-email.org/meli/meli/pulls/587)
- [**`ee051a24`**](https://git.meli-email.org/meli/meli/commit/ee051a24396e268c48e290f966c8316a5626caa2) `melib/imap: allow lint on init_cache() arg`
- [**`4cdc1dbf`**](https://git.meli-email.org/meli/meli/commit/4cdc1dbfba57b0478dd79e4986ef3f36bfb96f41) `melib: move base36 to utils mod`
- [**`9d40e136`**](https://git.meli-email.org/meli/meli/commit/9d40e136f7acfb249854cf6002c89037baf39c1a) `melib/jmap: s/IdentityObject/Identity`
- [**`4c723d5c`**](https://git.meli-email.org/meli/meli/commit/4c723d5cfb4913472cfaa597d893fd8e6ea04ab3) `melib/notmuch: remove Clone derive from Message`
- [**`2a7a8003`**](https://git.meli-email.org/meli/meli/commit/2a7a8003a3e2a9c2256a426363ff81a3993e73a7) `melib/jmap: process destroyed ids in Email/changes` in PR [`#595` "JMAP fixups"](https://git.meli-email.org/meli/meli/pulls/595)
- [**`25b582c9`**](https://git.meli-email.org/meli/meli/commit/25b582c9e9ceaa9cb2b05303a51bc3a559c49066) `melib/maildir: Remove root_mailbox from MaildirWatch`
- [**`0cf72830`**](https://git.meli-email.org/meli/meli/commit/0cf728308b08dd796c2690c90a07ebdffbfbb129) `melib/maildir: fix save_to_mailbox() error msg`
- [**`d7b3f57c`**](https://git.meli-email.org/meli/meli/commit/d7b3f57cf8ffe6a255fb876941e74cc9d094991d) `melib/notmuch: rewrite watching logic`
- [**`dae9ebd8`**](https://git.meli-email.org/meli/meli/commit/dae9ebd8bff1f446828403a0ddb85e1305e3ea32) `compose: remove extra dot from dialog message`
- [**`0a3791cc`**](https://git.meli-email.org/meli/meli/commit/0a3791cc0c153e0dbe0b6d2482e1d497bd32bc5a) `melib/email/parser: change ParsingError display`
- [**`9f53de4f`**](https://git.meli-email.org/meli/meli/commit/9f53de4fc8e05096cc8c5f74d419c961b6e5ba70) `composer: remove extra_identity_addresses from To:`
- [**`a23b3dbc`**](https://git.meli-email.org/meli/meli/commit/a23b3dbc12c3e0e72fdfa091e7e82cef0a458ac0) `compose.rs: fix compilation error without gpgme`
- [**`c9d46617`**](https://git.meli-email.org/meli/meli/commit/c9d4661796602e6b1a60048ed21d44f72d23b8d7) `Fix clippy::len_zero`
- [**`324dd969`**](https://git.meli-email.org/meli/meli/commit/324dd969471934b9eda48e8776a3c7bc3fd562f3) `Fix clippy::literal_string_with_formatting_args`
- [**`63230603`**](https://git.meli-email.org/meli/meli/commit/63230603afe07377c1e79e8876feed7c1aebbd54) `Fix clippy::double_ended_iterator_last`
- [**`4e99a6c1`**](https://git.meli-email.org/meli/meli/commit/4e99a6c101bc66afc31f72a0455a541e46de7187) `Fix clippy::needless_option_take` in PR [`#611` "compose.rs: fix compilation error without gpgme and clippy lint fixes"](https://git.meli-email.org/meli/meli/pulls/611)
- [**`ccb5505b`**](https://git.meli-email.org/meli/meli/commit/ccb5505b4c4bcf88cdee343f85a0aa4f0ecf6930) `melib/contacts: remove Contacts::{created,last_edited}`
- [**`f3348f33`**](https://git.meli-email.org/meli/meli/commit/f3348f33f1bed0cca1e563c68de1146a4748e950) `accounts.rs: Remove std module imports`
- [**`83b0e5af`**](https://git.meli-email.org/meli/meli/commit/83b0e5af196302d174b67a986f4b3efd779c4e7d) `mail/compose.rs: remove duplicate field init`
- [**`6acdf521`**](https://git.meli-email.org/meli/meli/commit/6acdf5210d37b870616a7c893d5454d8418bc844) `listing: move cmd_buf logic to Context`
- [**`0710822c`**](https://git.meli-email.org/meli/meli/commit/0710822c68da167456ac532fbbd209c8403897a2) `state: make Context children field take ForkedProcess`
- [**`f4289f3f`**](https://git.meli-email.org/meli/meli/commit/f4289f3fb8ff6fbf50640c845bd2041a9122fabb) `scripts/rfc_to_rs_mdoc.py: reformat`
- [**`4021ab19`**](https://git.meli-email.org/meli/meli/commit/4021ab194004a3c1bb66364b3c8fc2dac8981946) `version_migrations: use array in const asserts` in PR [`#614` "Misc"](https://git.meli-email.org/meli/meli/pulls/614)
- [**`13551a02`**](https://git.meli-email.org/meli/meli/commit/13551a02133141bd6e4977cf754aa10e96878175) `melib/accounts: add #[inline] attribute to methods`
- [**`2f265404`**](https://git.meli-email.org/meli/meli/commit/2f265404a95878536e76cfd2d0a9e89f94a248e3) `melib/collections: add #[inline] to small methods`

### Documentation

- [**`0cd0864f`**](https://git.meli-email.org/meli/meli/commit/0cd0864fa163a3c505c76fd79b748788397cbd54) `docs: add more doc comments to version_migrations mod`
- [**`1f647155`**](https://git.meli-email.org/meli/meli/commit/1f6471556efcaf0bb8de6d976188449dbb5f8b84) `docs: add more doc comments to conf mod`
- [**`c34465ba`**](https://git.meli-email.org/meli/meli/commit/c34465bab53d61827355ba71a0e35aef489dddf8) `docs: add more doc comments to utilities::widgets mod`
- [**`bd461fa8`**](https://git.meli-email.org/meli/meli/commit/bd461fa820886bfcd697c36c2334a7361ec1e970) `docs: fix example in custom_compose_hooks` in PR [`#561` "docs: fix example in `custom_compose_hooks`"](https://git.meli-email.org/meli/meli/pulls/561)
- [**`ce40bc5c`**](https://git.meli-email.org/meli/meli/commit/ce40bc5c48d7570efc2f12e54a74a7bfd9b2268b) `docs/meli.conf.5: describe what "notifications" entail`
- [**`d37df5a5`**](https://git.meli-email.org/meli/meli/commit/d37df5a564552de7ba03248ed4c8197663a550b1) `docs/meli.conf.5: add missing "Optional" quantifier`
- [**`c3193324`**](https://git.meli-email.org/meli/meli/commit/c31933244258c103b15ae1495313dbbca17c785b) `docs/meli.1: add manage-jobs command`
- [**`a56b024a`**](https://git.meli-email.org/meli/meli/commit/a56b024ac7d516d4f1c8dc0cb8b77beba8d81e33) `docs/meli.1: add manage-mailboxes command`
- [**`767ee791`**](https://git.meli-email.org/meli/meli/commit/767ee79143e2048f3db32a5c2923056f82ba0d88) `DEVELOPMENT.md: explain how to run CI checks locally`
- [**`c0a1d154`**](https://git.meli-email.org/meli/meli/commit/c0a1d154c154d29d2fc46f51eadb08ad35128b4e) `DEVELOPMENT.md: document DCO requirement` in PR [`#583` "Minor doc additions"](https://git.meli-email.org/meli/meli/pulls/583)
- [**`875aee85`**](https://git.meli-email.org/meli/meli/commit/875aee855a9d518148fc2eafb7e93e0028cdf8a8) `README.md: update install instructions`
- [**`0d5dc707`**](https://git.meli-email.org/meli/meli/commit/0d5dc707197ae0ce6ad9b63d17b7df80f8382126) `README.md: minor touch-ups` in PR [`#590` "README.md: update install instructions"](https://git.meli-email.org/meli/meli/pulls/590)
- [**`8fc885d0`**](https://git.meli-email.org/meli/meli/commit/8fc885d0201cf1dca026e97f71cb3427ad1b5e8b) `README.md: mention matrix channel` in PR [`#603` "misc-fixes-minor"](https://git.meli-email.org/meli/meli/pulls/603)
- [**`91677f50`**](https://git.meli-email.org/meli/meli/commit/91677f5037f86bf5a834e37f4908329e1c3af223) `melib/email/headers: add documentation(s)`
- [**`816f3e41`**](https://git.meli-email.org/meli/meli/commit/816f3e41ee6a12ca878722c9a7acae09c70504b3) `notifications: add missing DisplayMessageBox docs`
- [**`6ccbd574`**](https://git.meli-email.org/meli/meli/commit/6ccbd57454e07b2f069f29ab9d4e9065c33e710c) `notifications: add missing NotificationCommand docs`
- [**`81429941`**](https://git.meli-email.org/meli/meli/commit/8142994127ad3ae7d7692f28a93b143492f21ac7) `melib/notmuch: add doc comments to Message methods`
- [**`407e8e6b`**](https://git.meli-email.org/meli/meli/commit/407e8e6b355fb09b009debc84ca246ec0342eb2e) `melib/smtp: add some rustdoc comments`

### Testing

- [**`1fa33cc3`**](https://git.meli-email.org/meli/meli/commit/1fa33cc36d54fd6c0b0af6fcbcce233497339ed4) `tests/test_cli_subcommands: purge subprocess envs` in PR [`#574` "`tests/test_cli_subcommands`: purge subprocess envs"](https://git.meli-email.org/meli/meli/pulls/574)
- [**`1d1cf5fe`**](https://git.meli-email.org/meli/meli/commit/1d1cf5fec1be9980c1510b650424630499d7d743) `melib: add test_imap_watch()`
- [**`f632adaa`**](https://git.meli-email.org/meli/meli/commit/f632adaa67d8b2a875d49b316ab11a4e3d4c63c8) `melib: add test_jmap_refresh()`
- [**`67d082b5`**](https://git.meli-email.org/meli/meli/commit/67d082b5b24c47f155466b433871da3cbcd6873e) `melib: add test_maildir_watch()`
- [**`84f31d73`**](https://git.meli-email.org/meli/meli/commit/84f31d7399a29fb0939f7a58b4786e585b40e7a4) `melib: add test_notmuch_watch()`
- [**`9679c80f`**](https://git.meli-email.org/meli/meli/commit/9679c80f08656158f5189b2a3a6f95d0bb0ac4c0) `melib: add test_notmuch_refresh()` in PR [`#582` "Convert `Backend::watch` function to return `Stream`"](https://git.meli-email.org/meli/meli/pulls/582)
- [**`a69f4c4b`**](https://git.meli-email.org/meli/meli/commit/a69f4c4b7f2a6a0f8f8d84860447cc65dda21d7d) `melib/smtp: add an SMTP transaction test`
- [**`e466d026`**](https://git.meli-email.org/meli/meli/commit/e466d026293ee9267a601dceb5c5a57e2eef2fbe) `conf/tests: speedup test_conf_theme_key_values()` in PR [`#589` "conf/tests: speedup `test_conf_theme_key_values`()"](https://git.meli-email.org/meli/meli/pulls/589)
- [**`e4957f8f`**](https://git.meli-email.org/meli/meli/commit/e4957f8f0065bd995581a146b184fb7550a77474) `conf.rs: do not run migrations in unit tests`
- [**`dc5bff8d`**](https://git.meli-email.org/meli/meli/commit/dc5bff8d4ae37d66eccf8922c61ef2b4c5a269cb) `conf/tests: flush config file to disk`

### Miscellaneous Tasks

- [**`220bdb62`**](https://git.meli-email.org/meli/meli/commit/220bdb62b51cb3421f72bf439092b436a3f670a7) `tools: run cargo-update`

### Continuous Integration

- [**`3c92dd6f`**](https://git.meli-email.org/meli/meli/commit/3c92dd6f31fc1bf3235a42d59010734a75d30eaf) `CI: Trigger build/lint workflows when */tests/* change`
- [**`0d64e019`**](https://git.meli-email.org/meli/meli/commit/0d64e019c71e56e7b7e9e7014689e746403f44bd) `ci: add --all-features flag to cargo-msrv checks`
- [**`f2f72c3f`**](https://git.meli-email.org/meli/meli/commit/f2f72c3f99005f6c986b57760c58f4c364e0f64d) `ci: install notmuch dependencies for testing`
- [**`6be8c0c0`**](https://git.meli-email.org/meli/meli/commit/6be8c0c0e0d4a8b9514b782ac1241dba573a7032) `ci: remove obsolete test exclusion` in PR [`#600` "melib/smtp: add an SMTP transaction test"](https://git.meli-email.org/meli/meli/pulls/600)

### Reverted Commits

- [**`fba30f3c`**](https://git.meli-email.org/meli/meli/commit/fba30f3c4bacf9e56946ce6eaa71db1335958f2c) `Revert "accounts: cancel any previous mailbox fetches"`

## [v0.8.10] - 2024-12-06

Highlights:

- added `pipe-attachment` command
- added sample scripts for using `meli` as a `mailto` scheme handler in `contrib/`
- fixed GPG encryption with libgpgme

Contributors in alphabetical order:

- Manos Pitsidianakis
- Matthias Geiger

### Added

- [**`5e77821f`**](https://git.meli-email.org/meli/meli/commit/5e77821f781b9f80f62df0a74f33f899a7cd8d92) `mail/view: add pipe-attachment command` in PR [`#540` "mail/view: add pipe-attachment command"](https://git.meli-email.org/meli/meli/pulls/540)
- [**`fa896f6b`**](https://git.meli-email.org/meli/meli/commit/fa896f6bb9dccd83952e2db57f78abdf0b514ffe) `contrib: add mailto: scheme handler scripts`
- [**`00ce9660`**](https://git.meli-email.org/meli/meli/commit/00ce9660ef783289fb35d0c7b23bd164f8f6efda) `melib/backends: add as_any/as_any_mut methods to BackendMailbox`
- [**`fd243fa5`**](https://git.meli-email.org/meli/meli/commit/fd243fa5abfbee704f480caef0831269a65f2762) `maildir: add mailbox creation tests`
- [**`de65eec3`**](https://git.meli-email.org/meli/meli/commit/de65eec3a9b42e4a82542dc4744f8222bbd5e23b) `meli/accounts: add mailbox_by_path() tests` in PR [`#535` "Rework maildir mailbox path logic, add tests"](https://git.meli-email.org/meli/meli/pulls/535)
- [**`6b363601`**](https://git.meli-email.org/meli/meli/commit/6b3636013c1e827491f776b0086d28c072cbd518) `melib/gpgme: impl Display for gpgme::Key`

### Bug Fixes

- [**`60c90d75`**](https://git.meli-email.org/meli/meli/commit/60c90d75497be47c3c7d5e6b06fcfc49577f9eaf) `melib/attachments: ensure MIME boundary prefixed with CRLF`
- [**`3433c5c3`**](https://git.meli-email.org/meli/meli/commit/3433c5c3d5924bfd4d980abce6e9dc0fbac8df87) `compose/pgp: rewrite key selection logic` in PR [`#541` "More gpgme/PGP fixes again"](https://git.meli-email.org/meli/meli/pulls/541)
- [**`12de82e7`**](https://git.meli-email.org/meli/meli/commit/12de82e7b40b24ebdb53a67e044b3cb9b2150134) `melib/conf: fix mutt_alias_file not being validated` in PR [`#550` "Remove sealed_test dependency"](https://git.meli-email.org/meli/meli/pulls/550)
- [**`c8e055a7`**](https://git.meli-email.org/meli/meli/commit/c8e055a718703004505ed690f7a3e36fa18d8ada) `Fix version migrations being triggered backwards` in PR [`#557` "Fix version migrations being triggered backwards"](https://git.meli-email.org/meli/meli/pulls/557)
- [**`efab99fd`**](https://git.meli-email.org/meli/meli/commit/efab99fda24e8b6b22804c3029d757579a3008a6) `terminal: check for NO_COLOR env var without unicode validation`
- [**`36a63e88`**](https://git.meli-email.org/meli/meli/commit/36a63e8878d9bfdba7994fb193294faa1462fafc) `melib/maildir: rewrite create_mailbox()`
- [**`fcab855f`**](https://git.meli-email.org/meli/meli/commit/fcab855fdad4af20f52f9f318551c1377fe5fbb6) `view: ensure envelope headers are always populated` in PR [`#538` "view: ensure envelope headers are always populated"](https://git.meli-email.org/meli/meli/pulls/538)
- [**`84564f44`**](https://git.meli-email.org/meli/meli/commit/84564f44a3c5823e11f1d6097c772667f1c62df2) `mailcap: don't drop File before opening it` in PR [`#552` "mailcap: don't drop File before opening it"](https://git.meli-email.org/meli/meli/pulls/552)

### Changes

- [**`ed85da51`**](https://git.meli-email.org/meli/meli/commit/ed85da51dff4a1de5622bae234381bbbf6603271) `Remove sealed_test dependency`

### Refactoring

- [**`03df2ac1`**](https://git.meli-email.org/meli/meli/commit/03df2ac12894ef11dfb51af1a634ef87a36daa9b) `meli/utilities: add print utilities for tests`
- [**`18e9d5c1`**](https://git.meli-email.org/meli/meli/commit/18e9d5c148f2126647a8e17713af37d456cce9f8) `conf.rs: impl From<melib::AccountSettings> for AccountConf`
- [**`1f2fec19`**](https://git.meli-email.org/meli/meli/commit/1f2fec198fb5127fcfc7bd3e1004b496b3be7de8) `Fix 1.83.0 lints` in PR [`#536` "CI: Add action to check for DCO signoffs in PRs"](https://git.meli-email.org/meli/meli/pulls/536)
- [**`192ecea2`**](https://git.meli-email.org/meli/meli/commit/192ecea2a43363fe2ffc0decefe22a703d8c53ca) `compose/gpg.rs: Fix msrv regression`

### Documentation

- [**`4a61a4b8`**](https://git.meli-email.org/meli/meli/commit/4a61a4b8577c1e33cdcd46cb74a18ddafcf037fb) `melib: include README.md as preamble of crate rustdocs`
- [**`80e53471`**](https://git.meli-email.org/meli/meli/commit/80e53471786a6986b71d7c8922472cfa5bf5f571) `BUILD.md: move melib specific stuff to melib/README.md`
- [**`91a17ece`**](https://git.meli-email.org/meli/meli/commit/91a17ece5c7f9651e57929487021fa2ed553d2c6) `melib/README.md: mention sqlite3-static feature`
- [**`b77a691b`**](https://git.meli-email.org/meli/meli/commit/b77a691b7d6f6373620847f492791fe0c694fa2a) `meli/README.md: Add cargo features section` in PR [`#549` "Document cargo features in READMEs"](https://git.meli-email.org/meli/meli/pulls/549)
- [**`91dc271d`**](https://git.meli-email.org/meli/meli/commit/91dc271d74e946790aa8a0818cc8c0db9f8fc0bb) `contrib: add a README.md file`
- [**`2e900be6`**](https://git.meli-email.org/meli/meli/commit/2e900be69898c9d734e24ed8e49635d2b1c7a97e) `contrib/README.md: add section about oauth2.py`
- [**`07812d2c`**](https://git.meli-email.org/meli/meli/commit/07812d2c8581b2292c3755ce5e76d0a521376f08) `contrib/README.md: elaborate a bit about mailto` in PR [`#545` "Add external mailto: handler support via scripts in contrib"](https://git.meli-email.org/meli/meli/pulls/545)
- [**`e784e8d2`**](https://git.meli-email.org/meli/meli/commit/e784e8d239f948c279200d1b28b2fd0326dfa96f) `scripts: add markdown_doc_lints.py`

### Miscellaneous Tasks


### Continuous Integration

- [**`77629851`**](https://git.meli-email.org/meli/meli/commit/776298511bd9c9a868ceecf6eaee93f9df4821fa) `CI: Add action to check for DCO signoffs in PRs`
- [**`f944ebed`**](https://git.meli-email.org/meli/meli/commit/f944ebed813aaa36c11506a783f951f031e50c25) `CI: Add error msg when cargo-derivefmt check fails`
- [**`d49344f9`**](https://git.meli-email.org/meli/meli/commit/d49344f9d855fb2d0fa1d74ee7bc74ca1ad974fe) `CI: Move MSRV checks from manifest to lints` in PR [`#553` "ci-workflow-fixes"](https://git.meli-email.org/meli/meli/pulls/553)
- [**`ece6bfc2`**](https://git.meli-email.org/meli/meli/commit/ece6bfc2ce4daf8a6d7de70bed4a1e7dfbec3ff6) `CI: non-zero exit if cargo-derivefmt-* targets fail`
- [**`2257b91b`**](https://git.meli-email.org/meli/meli/commit/2257b91b403003a91a9658e139b784d93d7ffe70) `CI: add actions/cache steps` in PR [`#554` "CI: add actions/cache steps"](https://git.meli-email.org/meli/meli/pulls/554)
- [**`a1c9524f`**](https://git.meli-email.org/meli/meli/commit/a1c9524f7405321f5e8d5ab2490719a327a0789b) `CI: fix check_dco.sh not working with other repos` in PR [`#555` "CI: fix check_dco.sh not working with other repos"](https://git.meli-email.org/meli/meli/pulls/555)

## [v0.8.9](https://git.meli-email.org/meli/meli/releases/tag/v0.8.9) - 2024-11-27

This is mostly a fixups release.

### Added

- [**`cf16bf65`**](https://git.meli-email.org/meli/meli/commit/cf16bf65f7d031084c73f070ee40efbfd40720e6) `meli/sqlite3: add tests for reindexing`
- [**`a389772d`**](https://git.meli-email.org/meli/meli/commit/a389772d96d845a1a009e54f1157460d640c1104) `accounts: suggest tips on mailbox_by_path error`

### Bug Fixes

- [**`25f0a3f8`**](https://git.meli-email.org/meli/meli/commit/25f0a3f814ff40b8e218fca7ba099a84399a2a1b) `conf/terminal: fix serde of ProgressSpinnerSequence`
- [**`c375b48e`**](https://git.meli-email.org/meli/meli/commit/c375b48ebf25065c495e0740f85a74db9dd6facd) `terminal: fix Synchronized Output response parsed as input` in PR [`#523` "terminal: fix Synchronized Output response parsed as input"](https://git.meli-email.org/meli/meli/pulls/523)
- [**`b7e215f9`**](https://git.meli-email.org/meli/meli/commit/b7e215f9c238f8364e2a1f0d10ac668d0cfe91ad) `melib/utils: fix test_fd_locks() on platforms without OFD support` in PR [`#524` "melib/utils: fix test_fd_locks() on platforms without OFD support"](https://git.meli-email.org/meli/meli/pulls/524)
- [**`25c32a6b`**](https://git.meli-email.org/meli/meli/commit/25c32a6b95dce00f6715115796e27bff0fcee413) `meli/docs/meli.conf.examples.5: fix .Dt macro arguments`
- [**`18ae5848`**](https://git.meli-email.org/meli/meli/commit/18ae58483694119985d9ce7b8f384798114a8d1e) `meli: fix reindex of previously indexed account with sqlite3 backend`
- [**`13e917d9`**](https://git.meli-email.org/meli/meli/commit/13e917d97b2c8ff8da403dc415eb1dffa8491a9b) `Fix some compilation errors with cfg feature attrs` in PR [`#531` "accounts: suggest tips on mailbox_by_path error"](https://git.meli-email.org/meli/meli/pulls/531)
- [**`8c176d38`**](https://git.meli-email.org/meli/meli/commit/8c176d38408a822d9b127f282f9c43fb1bada8d7) `contacts/editor: fix crash on saving contact` in PR [`#532` "contacts/editor: fix crash on saving contact"](https://git.meli-email.org/meli/meli/pulls/532)
- [**`fb5a88c2`**](https://git.meli-email.org/meli/meli/commit/fb5a88c22c7c7107f1a124d721d493be324ea25e) `melib/collection: ensure mailbox exists when inserting new envelopes` in PR [`#529` "Small account stuff fixes"](https://git.meli-email.org/meli/meli/pulls/529)

### Changes

- [**`7f8f1cf6`**](https://git.meli-email.org/meli/meli/commit/7f8f1cf65f644090ea450ecf9423585cc89b4a65) `melib/gpgme bindings renewal` in PR [`#533` "melib/gpgme bindings renewal"](https://git.meli-email.org/meli/meli/pulls/533)
- [**`9b7825bc`**](https://git.meli-email.org/meli/meli/commit/9b7825bc59fb9c86dda4f86c1116517ae3e88514) `Update futures-util dep, remove stderrlog dep`
- [**`4be69360`**](https://git.meli-email.org/meli/meli/commit/4be6936026bdf87563b3e6832d01fd9b112a414e) `Remove obsolete "encoding" dependency` in PR [`#530` "Remove/update obsolete dependencies"](https://git.meli-email.org/meli/meli/pulls/530)

### Refactoring

- [**`5af6e059`**](https://git.meli-email.org/meli/meli/commit/5af6e059b78ca67594ce773d935169c26ce31a70) `meli/accounts: use Arc<str> for account name`
- [**`567270e1`**](https://git.meli-email.org/meli/meli/commit/567270e177253cfbf8cee2df9e9a8f981ca9ab97) `melib: use Vec instead of SmallVec for search results`
- [**`2bd8d7ba`**](https://git.meli-email.org/meli/meli/commit/2bd8d7ba01df4eaf01488c2f01fd95905916c0b9) `conf/tests.rs: Rename test functions to follow path convention`

### Documentation

- [**`97242482`**](https://git.meli-email.org/meli/meli/commit/972424829c29d9cfb6d45d589e17fb30a9ff52c6) `meli/docs: add meli.conf.examples to CLI and tests`
- [**`0f096338`**](https://git.meli-email.org/meli/meli/commit/0f0963389913736b8a8a73b3928abeb1d59a5898) `README.md: Update ways to install, add gitlab mirror link` in PR [`#528` "Integrate `meli.conf.examples.5` into CLI and build, also update README with installation instructions"](https://git.meli-email.org/meli/meli/pulls/528)

### Continuous Integration

- [**`630df308`**](https://git.meli-email.org/meli/meli/commit/630df3083f794a6551b1006ac57b9ce20b92a329) `CI: Add arm64 runners in job matrices` in PR [`#527` "CI: Add arm64 runners in job matrices"](https://git.meli-email.org/meli/meli/pulls/527)
- [**`49ecbb56`**](https://git.meli-email.org/meli/meli/commit/49ecbb56f7a5a6c5d9b9659215348132e1c71ac4) `CI: .gitea/Makefile.lint: check if nightly exists`

## [v0.8.8](https://git.meli-email.org/meli/meli/releases/tag/v0.8.8) - 2024-11-19

*WARNING*: This release contains a breaking change in the configuration file: a
global composing option is not required anymore. Now, composing options are per
account.

### Added

- [**`f3d59ebf`**](https://git.meli-email.org/meli/meli/commit/f3d59ebf64db8b640bb23c42d3f6a9843c0547fe) `accounts: add force: bool arg to load()`
- [**`33836a32`**](https://git.meli-email.org/meli/meli/commit/33836a3263d4750501fb032898fcbfb3f727f455) `melib/error: add WrapResultIntoError helper trait`
- [**`3216324c`**](https://git.meli-email.org/meli/meli/commit/3216324cccdaa34afc5542def68abe34d73855f4) `melib/mbox: impl FromStr for MboxFormat`
- [**`94f345d7`**](https://git.meli-email.org/meli/meli/commit/94f345d731329e029016495f1c674b6a9b17401a) `Implement mailbox renaming command`
- [**`8d45ecc1`**](https://git.meli-email.org/meli/meli/commit/8d45ecc15d15c9885d5278dac7bba5f2c2829020) `melib/error: add related_path field`
- [**`bf3a4c5d`**](https://git.meli-email.org/meli/meli/commit/bf3a4c5dc1f08fafea83d698c4c8bf882509106a) `error: add ErrorChainDisplay struct for better output`
- [**`6be5fd26`**](https://git.meli-email.org/meli/meli/commit/6be5fd2610ae9d784c8fc9327f4a29fdc78f3179) `themes: add inheritance, and use themes when initializing grids`
- [**`0ee7fc4d`**](https://git.meli-email.org/meli/meli/commit/0ee7fc4d950e9a69677e4538c00bb1426108922a) `Print clickable path links with subcommands`
- [**`aed7a60f`**](https://git.meli-email.org/meli/meli/commit/aed7a60fb9c4f9f7eb61aaf1ac3505632b7896f1) `samples: add ibm-modern theme` in PR [`#469` "conf-refactor"](https://git.meli-email.org/meli/meli/pulls/469)
- [**`4bbf446b`**](https://git.meli-email.org/meli/meli/commit/4bbf446bc168da4c81e0c27d9a738e3b80546e0c) `utils: add unix file locks module`
- [**`6fbf569f`**](https://git.meli-email.org/meli/meli/commit/6fbf569fe0518925b6358eea5855c5c40a3fc110) `search: add Message-ID, and other header search support`
- [**`26d33ce5`**](https://git.meli-email.org/meli/meli/commit/26d33ce5230dbc0864715ca8d12c3a262ea7e0eb) `address: add separator argument to display_slice()`
- [**`32e3be8b`**](https://git.meli-email.org/meli/meli/commit/32e3be8b10baa7949e37c20243085aa37a860a30) `sqlite3: add optional directory field in DatabaseDescription`
- [**`dbbb1529`**](https://git.meli-email.org/meli/meli/commit/dbbb1529e412390c408a9bbd9d694e4da95a715b) `Add missing ComponentUnrealize handlers`
- [**`87d2cec9`**](https://git.meli-email.org/meli/meli/commit/87d2cec9d97cb1c9a756dc1c901c3837265e895a) `Add sealed_test dependency`
- [**`604ae111`**](https://git.meli-email.org/meli/meli/commit/604ae1112801f2b3a2f78f00d826c60949b3fe4d) `Impl From<&[u8]> for u64-based hash newtypes`
- [**`8205c7f5`**](https://git.meli-email.org/meli/meli/commit/8205c7f51a316c99852648b15fd7e3d80962875d) `melib: add JsContact module` in PR [`#479` "view-filters"](https://git.meli-email.org/meli/meli/pulls/479)
- [**`2af5c8b6`**](https://git.meli-email.org/meli/meli/commit/2af5c8b6fd225b0f58a4f9cf7876a6302d13c618) `terminal: add QuerySynchronizedOutputSupport WIP`
- [**`5c4faea5`**](https://git.meli-email.org/meli/meli/commit/5c4faea539de8761e717561c8d7442ef59face3d) `Add transpose shortcut and tests for text field`
- [**`e9b87b2e`**](https://git.meli-email.org/meli/meli/commit/e9b87b2e40303649f838ad6b5f56d5093332b233) `melib/maildr: add rename_regex config option`
- [**`8f0e1d66`**](https://git.meli-email.org/meli/meli/commit/8f0e1d664090029bb3b00529a96ba7f5630eae33) `Add human-readable identifiers in temp draft files`
- [**`601e3711`**](https://git.meli-email.org/meli/meli/commit/601e37117c0531431720c80f228ede7d3574849c) `Add vCard exports`
- [**`719e2eb2`**](https://git.meli-email.org/meli/meli/commit/719e2eb271a099f1ac912050c500a26b67bdec42) `listing: add customizable view divider like sidebar's` in PR [`#485` "listing: add customizable view divider like sidebar's"](https://git.meli-email.org/meli/meli/pulls/485)
- [**`ba3ad8ed`**](https://git.meli-email.org/meli/meli/commit/ba3ad8ed18db14412979729591aac7b004cd7d90) `listing: always show mail_view_divider` in PR [`#486` "listing: always show mail_view_divider"](https://git.meli-email.org/meli/meli/pulls/486)
- [**`46b2c3b1`**](https://git.meli-email.org/meli/meli/commit/46b2c3b1f716dadd226ebe421cc83c34642811d7) `Add listing.thread_layout config flag` in PR [`#487` "Add listing.thread_layout config flag"](https://git.meli-email.org/meli/meli/pulls/487)
- [**`aaea3a5a`**](https://git.meli-email.org/meli/meli/commit/aaea3a5ab4452ee1cdd708c8948a204bc7a69a59) `nntp: add timeout conf flag`
- [**`d4636bcc`**](https://git.meli-email.org/meli/meli/commit/d4636bcc70ecb2efc8a5b70579b446112ce8e67e) `nntp: interpret IMPLEMENTATION cap as metadata`
- [**`5f120309`**](https://git.meli-email.org/meli/meli/commit/5f120309f9f79f9e59e43437b72d98941953a62e) `nntp: add select_group_by_name() method`
- [**`9a9cd03d`**](https://git.meli-email.org/meli/meli/commit/9a9cd03d9ddbe3cb4d671f4370b45d29a0e6a53a) `nntp: add NntpType::article_message_id() method`
- [**`7cfcbb7a`**](https://git.meli-email.org/meli/meli/commit/7cfcbb7ab1ac56569ddbfd61ff9aa823007536db) `Add patch_retrieve module` in PR [`#489` "Add `patch_retrieve` module"](https://git.meli-email.org/meli/meli/pulls/489)
- [**`c82341f3`**](https://git.meli-email.org/meli/meli/commit/c82341f3af28447ece1902f276150769148bcadb) `File: try trimming filename if ENAMETOOLONG`
- [**`23395491`**](https://git.meli-email.org/meli/meli/commit/23395491dbdea9feeef27963017b4a34a2dbb5ce) `compose/pgp: add encrypt_for_self flag`
- [**`0b6988b7`**](https://git.meli-email.org/meli/meli/commit/0b6988b7cfe2c8ee23c53a4f228700394dc59781) `gpgme: add always trust flag to encrypt op`
- [**`be3b3ef8`**](https://git.meli-email.org/meli/meli/commit/be3b3ef89b78a3b7e6221e8150102e829e672267) `melib/utils: add fnmatch(3) interface`
- [**`32f7e50f`**](https://git.meli-email.org/meli/meli/commit/32f7e50fd44e5b602b051b51150a644eacd49968) `Add version migration support`
- [**`a6c7621c`**](https://git.meli-email.org/meli/meli/commit/a6c7621ce3227e810d796e566ee20b00359b37fa) `jscontact: add {created,updated} fields`
- [**`39592ad0`**](https://git.meli-email.org/meli/meli/commit/39592ad02c0730855d2ab16d8da6ed5f63235ad3) `jmap: implement changing mailbox subscription`
- [**`ca7eb792`**](https://git.meli-email.org/meli/meli/commit/ca7eb7928439e3f42031677f7e7cbb1b1fdb4013) `jmap: Implement deleting email`
- [**`b8e841bb`**](https://git.meli-email.org/meli/meli/commit/b8e841bbcd67e68a1a445821f058e7d12e963e28) `jmap: implement mailbox deletion`
- [**`77e7c3df`**](https://git.meli-email.org/meli/meli/commit/77e7c3df608cc1996ef6cf8628d008ce69b39993) `Add support for signatures` in PR [`#500` "Add support for signatures"](https://git.meli-email.org/meli/meli/pulls/500)
- [**`dba5b68b`**](https://git.meli-email.org/meli/meli/commit/dba5b68be8ede3b81eb3926bb5fb6affd9efc436) `components: add prelude module`
- [**`f656aff0`**](https://git.meli-email.org/meli/meli/commit/f656aff08efb32a808009f4a0b133ba872659e66) `composer: add discard-draft command`
- [**`789a88b2`**](https://git.meli-email.org/meli/meli/commit/789a88b28dbc819b7715432f8fcb3d908aa03f6a) `shortcuts: add select_motion equivalent to select_entry`
- [**`cb2dd5de`**](https://git.meli-email.org/meli/meli/commit/cb2dd5def5167b8f4ca471540252e7687c48c148) `listing/threaded: impl missing select functionality` in PR [`#514` "listing/threaded: impl missing filter functionality"](https://git.meli-email.org/meli/meli/pulls/514)
- [**`c1901c96`**](https://git.meli-email.org/meli/meli/commit/c1901c962d72d8177d813d6545b489731a8fbcd4) `melib/email/compose: add Content-Type header for utf8 text plain attachments`
- [**`0e77bd5b`**](https://git.meli-email.org/meli/meli/commit/0e77bd5b4ca04a18b686f137fad388a9a4d3b7c2) `melib/email/compose/tests: add multipart mixed attachment test` in PR [`#515` "Fix incorrect multipart/mixed rendering when sending text with attachments under certain circumstances"](https://git.meli-email.org/meli/meli/pulls/515)
- [**`7b1be139`**](https://git.meli-email.org/meli/meli/commit/7b1be139f25901e81b49bf04110d7fc8cd78a630) `melib: make mbox backend build by default`
- [**`7ff1db14`**](https://git.meli-email.org/meli/meli/commit/7ff1db143f51d2cfc06097293e960ee17ceaab10) `manage-mailboxes: add delete option` in PR [`#520` "manage-mailboxes: add delete option"](https://git.meli-email.org/meli/meli/pulls/520)

### Bug Fixes

- [**`6b05279a`**](https://git.meli-email.org/meli/meli/commit/6b05279a0987315c401516cac8ff0b016a8e02a8) `Update time dep to fix 1.80.0 breakage`
- [**`2084ce93`**](https://git.meli-email.org/meli/meli/commit/2084ce93756ea358ca826a33af014fad542ad07c) `Fix invalid cfg feature combinations for macos` in PR [`#471` "Fix invalid cfg feature combinations for macos"](https://git.meli-email.org/meli/meli/pulls/471)
- [**`4707ec9f`**](https://git.meli-email.org/meli/meli/commit/4707ec9f2a20254438e576d40e98c3ce4cbffbba) `text/line_break: fix ReflowState::{No,All} break`
- [**`86e25bc0`**](https://git.meli-email.org/meli/meli/commit/86e25bc017df714910798a811fb108bb7aa840af) `sqlite: fix database reset sequence`
- [**`4d4e189c`**](https://git.meli-email.org/meli/meli/commit/4d4e189cb98bd12b58fa8f5cd694ca6a143e175c) `imap: code style fixups`
- [**`335cca88`**](https://git.meli-email.org/meli/meli/commit/335cca88cb8a82e4fbe94b8491c771f68e4c2f41) `listing: fix highlight_self flag off by one error` in PR [`#477` "listing: fix highlight_self flag off by one error"](https://git.meli-email.org/meli/meli/pulls/477)
- [**`80915832`**](https://git.meli-email.org/meli/meli/commit/80915832213de0866b0cec01beacaac04b3fe051) `mailto: rewrite parsing` in PR [`#480` "mailto-rewrite"](https://git.meli-email.org/meli/meli/pulls/480)
- [**`65b32e77`**](https://git.meli-email.org/meli/meli/commit/65b32e7719de9155a29d91e63f5f42df4d17e299) `subcommands: Fix wrong help info in imap-shell prompt`
- [**`d0c81749`**](https://git.meli-email.org/meli/meli/commit/d0c81749ee862cca089ee5c3120dc9c6e4c8c7a3) `conf::data_types: minor style and error msg fixups`
- [**`7dbee81d`**](https://git.meli-email.org/meli/meli/commit/7dbee81dadddae09ad4149e378c608313910f44d) `view: fix nested filter jobs never being completed`
- [**`f78884ce`**](https://git.meli-email.org/meli/meli/commit/f78884ce022bb9f4554f860698844c082a14bd92) `melib/nntp: fix an ancient FIXME`
- [**`e0cfe8e4`**](https://git.meli-email.org/meli/meli/commit/e0cfe8e4d79a5a77e2f21811602f9c2ebe19359f) `Fix compilation for 32-bit architectures` in PR [`#492` "Fix compilation for 32-bit architectures"](https://git.meli-email.org/meli/meli/pulls/492)
- [**`1b708a99`**](https://git.meli-email.org/meli/meli/commit/1b708a99904cd5542aef984302d8133d5ee98082) `melib: attempt FromSql from Blob for u64 hash` in PR [`#506` "melib: attempt FromSql from Blob for u64 hash"](https://git.meli-email.org/meli/meli/pulls/506)
- [**`6c315580`**](https://git.meli-email.org/meli/meli/commit/6c315580b1e0dca1132febc7a8c2fceb08f4190d) `compose: fix add-attachment-file-picker`
- [**`c6e9e424`**](https://git.meli-email.org/meli/meli/commit/c6e9e4245d52885c7adedfc4defbfe96662d1b3e) `listing/threaded: impl missing filter functionality`
- [**`e7a164de`**](https://git.meli-email.org/meli/meli/commit/e7a164de0ce626712b7103a9a044f22a59b90298) `Configure some gpgme stuff under gpgme feature`

### Changes

- [**`8e300c46`**](https://git.meli-email.org/meli/meli/commit/8e300c46613be4ed35ae2dbd4e598e34c4531de4) `melib/jmap: call req text(). asap`
- [**`374ea8ba`**](https://git.meli-email.org/meli/meli/commit/374ea8bacba32ef908ee2d93e07dc78c116d4fed) `accounts: extract tests to tests.rs file`
- [**`7020cd66`**](https://git.meli-email.org/meli/meli/commit/7020cd66981f7ad3ab9cf413e156c248df593784) `meli: derive PartialEq/Eq for some types`
- [**`69065859`**](https://git.meli-email.org/meli/meli/commit/6906585942d9943de07ffef2be3e199c45fb716d) `accounts: split mailbox to enum out of JobRequest`
- [**`14f2d911`**](https://git.meli-email.org/meli/meli/commit/14f2d911933330d9a35480c420c6e64ad9cea21b) `melib/backends: change RefreshEvent field decl order`
- [**`56b1bf28`**](https://git.meli-email.org/meli/meli/commit/56b1bf28eb82d360a019ff9caa7bbfc7c154f9bc) `meli/accounts: batch process refresh events`
- [**`6513c188`**](https://git.meli-email.org/meli/meli/commit/6513c18810272135b6b57207b71304e5b3757f54) `melib/imap: on sync only update exists/unseen if loaded`
- [**`a8dad317`**](https://git.meli-email.org/meli/meli/commit/a8dad31776fe078fc346c7494e09dfe4cc18447f) `melib/imap: renamed cache module to sync`
- [**`9e9c04a3`**](https://git.meli-email.org/meli/meli/commit/9e9c04a3f67db859326c3854cc5f1a3162ff55f2) `Update indexmap dep to 2.3.0`
- [**`2b3828d8`**](https://git.meli-email.org/meli/meli/commit/2b3828d8d2ea36cbf0f9cd0038ba45a4ce45028d) `Update futures dependency to 0.3.30`
- [**`84812941`**](https://git.meli-email.org/meli/meli/commit/84812941475bfdf8ee3559d857ac0bcbd2e899ec) `melib/jmap: do not serialize server-set fields in Set create`
- [**`eda6620c`**](https://git.meli-email.org/meli/meli/commit/eda6620cb4f44027d318dba401b3a623ed564dd8) `jmap: detect supported Auth schemes on connect` in PR [`#467` "jmap: detect supported Auth schemes on connect"](https://git.meli-email.org/meli/meli/pulls/467)
- [**`35f12b15`**](https://git.meli-email.org/meli/meli/commit/35f12b1551884fc3d9af149b9574ad01462ebfac) `embedded: prevent double-close of pty fd` in PR [`#468` "embedded: prevent double-close of pty fd"](https://git.meli-email.org/meli/meli/pulls/468)
- [**`0bed37b5`**](https://git.meli-email.org/meli/meli/commit/0bed37b5a751b75a48a2167b39d9f5693fdb04dd) `melib: use IndexMap in conf fields`
- [**`f3ad824d`**](https://git.meli-email.org/meli/meli/commit/f3ad824df9dd6413123686f934eeff7f86e07e7c) `meli: use itoa to format offset indices in listings`
- [**`1cfb0b15`**](https://git.meli-email.org/meli/meli/commit/1cfb0b1538712ed37085d9b864f240a0f3f3a5bf) `Update nix dependency to 0.29.0`
- [**`9c1b4424`**](https://git.meli-email.org/meli/meli/commit/9c1b44245227d7c6511107521bf0af960f086032) `jobs: make cancel flag an AtomicBool`
- [**`f06a9072`**](https://git.meli-email.org/meli/meli/commit/f06a9072d6ae834ac7207e62ba14115d5041a62a) `jmap: fetch mailbox with receivedAt descending sort`
- [**`53b0d035`**](https://git.meli-email.org/meli/meli/commit/53b0d035e46d0178adb3c6620a5d5af02cc892de) `accounts: cancel any previous mailbox fetches`
- [**`60833ee5`**](https://git.meli-email.org/meli/meli/commit/60833ee51d6f16b25fd51dd344a959838bff013a) `accounts: make mailbox available as soon as possible`
- [**`28f45805`**](https://git.meli-email.org/meli/meli/commit/28f45805d7982a5be6d0ced893d88620e6548663) `mail/view: try cancel env fetch on Drop`
- [**`2bb9b20d`**](https://git.meli-email.org/meli/meli/commit/2bb9b20d951b372de85b3710f276adf92b60707d) `mail/view: do not highlight reply subjects in thread`
- [**`a4f344b3`**](https://git.meli-email.org/meli/meli/commit/a4f344b3963e8248443160edacc5e2582fe5f867) `Use create_new to avoid overwriting files`
- [**`d6197e8b`**](https://git.meli-email.org/meli/meli/commit/d6197e8b24bd3d5078bce733c997986168a5e7cd) `listing: clear count modifier on Home/End`
- [**`b798ca4a`**](https://git.meli-email.org/meli/meli/commit/b798ca4a95ca052f854c332b3231e9f5540c0fad) `imap: return cached response in {select,examine}_mailbox()`
- [**`151fcebe`**](https://git.meli-email.org/meli/meli/commit/151fcebe5b92ae07acbd64a12ae2c6dfd835a9e0) `imap: use BTreeMap for message sequence number store`
- [**`e48fcc33`**](https://git.meli-email.org/meli/meli/commit/e48fcc3367749d007afd237e6cbaf98514e65fc5) `imap/protocol_parser: also populate other_headers`
- [**`1e11c29c`**](https://git.meli-email.org/meli/meli/commit/1e11c29c889d97567a7aebf40b74937e41910747) `imap: resync cache first when fetching a mailbox`
- [**`1779ad5d`**](https://git.meli-email.org/meli/meli/commit/1779ad5d3a339d923fc88478459e1121f563ae9c) `imap: interpret empty server response as BYE`
- [**`2d320688`**](https://git.meli-email.org/meli/meli/commit/2d320688ce1520a52c5bd00c964f207c2446808c) `mail/listing: pre-lookup conf values`
- [**`4e967280`**](https://git.meli-email.org/meli/meli/commit/4e967280e17757488106b7489114dd0e76a53c96) `nntp: don't needlessly select group before ARTICLE` in PR [`#473` "Various"](https://git.meli-email.org/meli/meli/pulls/473)
- [**`67b88d24`**](https://git.meli-email.org/meli/meli/commit/67b88d24fcfe4d522c7045daf5d5ee7427352a16) `Update polling dependency from "2.8" to "3"`
- [**`14d74f36`**](https://git.meli-email.org/meli/meli/commit/14d74f36893116f01abce576e5467ff2cb9216c8) `Update smol dependency from "1" to "2"`
- [**`b950fcea`**](https://git.meli-email.org/meli/meli/commit/b950fceab4e6b82f21ed7ef013e37f1ba2b4be66) `melib: Use IndexMap in VCard`
- [**`32acc347`**](https://git.meli-email.org/meli/meli/commit/32acc3474fa0b6f2071ae9215aaadc1645e6346b) `view: show signature verification properly`
- [**`ac1349b8`**](https://git.meli-email.org/meli/meli/commit/ac1349b8507fe192c1a3c1c5c8e6b4384098f520) `command: alias pwd to cwd`
- [**`7c056e4b`**](https://git.meli-email.org/meli/meli/commit/7c056e4bdb421cf0e2d3a555567b7615af6a3cc5) `Retry loading mailbox on recoverable error` in PR [`#481` "Retry loading mailbox on recoverable error"](https://git.meli-email.org/meli/meli/pulls/481)
- [**`cbafdcf7`**](https://git.meli-email.org/meli/meli/commit/cbafdcf73483dfae89c7e9f3b0c93b95ce97605a) `terminal: color report WIP`
- [**`4a26cfa1`**](https://git.meli-email.org/meli/meli/commit/4a26cfa1067d19e11d5ec291063a5d2b4e3e8c03) `logging: disable tracing from output`
- [**`90974e7c`**](https://git.meli-email.org/meli/meli/commit/90974e7c0d303cf525690d24977e84037c2772c4) `imap: cache miss if row env hash != row hash`
- [**`4c44c440`**](https://git.meli-email.org/meli/meli/commit/4c44c440f699db2b09a49f3351c9c84fbf6999f8) `melib: #[ignore] shellexpand tests`
- [**`dc9e91df`**](https://git.meli-email.org/meli/meli/commit/dc9e91df1f0a002c07b5867e9a96c67d332536f2) `contacts/editor: Use FormButtonAction in form`
- [**`c0511901`**](https://git.meli-email.org/meli/meli/commit/c051190114c5e6fa46d3194d340f4daf9ccbe963) `Update debian/meli.{docs,examples} and Cargo exclude`
- [**`592ce159`**](https://git.meli-email.org/meli/meli/commit/592ce15903195a1ae0a42101cb1c23d7584385a6) `mbox: use Uuid::nil() as default envelope from`
- [**`6eeb4571`**](https://git.meli-email.org/meli/meli/commit/6eeb4571b74d156e11f688bb610e6fbaa0362202) `nntp: make all fields public`
- [**`b27bac7f`**](https://git.meli-email.org/meli/meli/commit/b27bac7f8528403346e2b282762a124f82b9c8bc) `nntp: use DEFLATE when available by default`
- [**`128b959f`**](https://git.meli-email.org/meli/meli/commit/128b959f36001b0dd1bd74ee32918fd7520bf6af) `nntp: prepend Newsgroups header if missing on NntpType::submit()`
- [**`a69122f8`**](https://git.meli-email.org/meli/meli/commit/a69122f8b00a5a059a0da1bfee44437f79c23553) `pgp: use default sign/encrypt keys when no keys are selected`
- [**`e6fa7093`**](https://git.meli-email.org/meli/meli/commit/e6fa7093bfc659dd70fb72c5cac27e9ec6aac7df) `view/envelope: trim headers values to 3 lines maximum`
- [**`7f0157a9`**](https://git.meli-email.org/meli/meli/commit/7f0157a966c8511208597f354924c2a2138b5024) `compose: make dialogs bigger in height` in PR [`#490` "pgp: use default sign/encrypt keys when no keys are selected"](https://git.meli-email.org/meli/meli/pulls/490)
- [**`e032acfa`**](https://git.meli-email.org/meli/meli/commit/e032acfab728197a47a7a346ed06b6892415ccdd) `view: pass filtered body to Composer as reply text` in PR [`#493` "view: pass filtered body to Composer as reply text"](https://git.meli-email.org/meli/meli/pulls/493)
- [**`49dcbc5e`**](https://git.meli-email.org/meli/meli/commit/49dcbc5e58bec846fea427ba6b09e8ff8fe1f79d) `terminal: Extend Ask default actions, prompts`
- [**`cd2e4bf3`**](https://git.meli-email.org/meli/meli/commit/cd2e4bf3a4f697eacb27c80f970cab6ec60c059d) `melib/utils: vendor urn crate`
- [**`5915f125`**](https://git.meli-email.org/meli/meli/commit/5915f125c3a624f58ba53137527414b7a7b5d044) `backends: use IsSubscribedFn in method signatures`
- [**`4f927bbe`**](https://git.meli-email.org/meli/meli/commit/4f927bbe6411fdc0703e6fe6743a80de9873cd0d) `nntp: properly return all nntp mailboxes`
- [**`b930cb49`**](https://git.meli-email.org/meli/meli/commit/b930cb49401d746b335ca2eaa086db9371b951ca) `maildir: do not use rename_regex when only updating flags`
- [**`27486f29`**](https://git.meli-email.org/meli/meli/commit/27486f2908069daf46ff941c333d43dda187a462) `Accept newer versions of base64 dependency`
- [**`c3cac77d`**](https://git.meli-email.org/meli/meli/commit/c3cac77deefffc7781ed569d113a78df53206da6) `Update imap-codec dependency to 2.0.0-alpha.4`
- [**`05f404ba`**](https://git.meli-email.org/meli/meli/commit/05f404ba1f70b997b33808e1d1d2daeb3e826f6c) `jobs: do not use AtomicU64` in PR [`#505` "jobs: do not use AtomicU64"](https://git.meli-email.org/meli/meli/pulls/505)
- [**`46916895`**](https://git.meli-email.org/meli/meli/commit/469168959f61f236826c2a3e7638344194a2d026) `melib/gpgme: s/NULL/NUL when referring to NUL byte`
- [**`81ace71b`**](https://git.meli-email.org/meli/meli/commit/81ace71b354168a0c7e5f9b5b6ab644105be1500) `terminal/embedded: lift error checking earlier`
- [**`24114811`**](https://git.meli-email.org/meli/meli/commit/241148119680cf8ee82ef183f9d241389ef24de8) `manage: parse scroll_{left,right} actions`
- [**`d2559e42`**](https://git.meli-email.org/meli/meli/commit/d2559e42e503d1720acfccae133c9b8d0845a90e) `imap: return all mailboxes, not just subscribed ones` in PR [`#509` "compose: fix add-attachment-file-picker"](https://git.meli-email.org/meli/meli/pulls/509)
- [**`320fddad`**](https://git.meli-email.org/meli/meli/commit/320fddad410eb4426dca93b94d510c496ff99220) `melib/gpgme: disable layout tests on non-x86_64 hosts` in PR [`#511` "melib/gpgme: disable layout tests on non-x86_64 hosts"](https://git.meli-email.org/meli/meli/pulls/511)
- [**`bcbcb012`**](https://git.meli-email.org/meli/meli/commit/bcbcb012efe5901287b9d3e746d871d91ca609ca) `melib/email/compose: ensure boundary always prefixed with CRLF`
- [**`d21c686d`**](https://git.meli-email.org/meli/meli/commit/d21c686da7e6f407f1fef9dadd66516280abd538) `melib/attachments: Make AttachmentBuilder::set_raw generic`
- [**`d5d34579`**](https://git.meli-email.org/meli/meli/commit/d5d34579148974364ea14e55357492c02b806c50) `melib/email/compose/tests: normalise test fn names`
- [**`e9ec6761`**](https://git.meli-email.org/meli/meli/commit/e9ec6761f96ee12732a98cd6d3ac258fc7f1eca6) `melib: make base64 dep mandatory`
- [**`30405216`**](https://git.meli-email.org/meli/meli/commit/3040521695b026273e8e960f3e9b517e88cdcf88) `melib: make notmuch feature depend on maildir feature`
- [**`35fa8e94`**](https://git.meli-email.org/meli/meli/commit/35fa8e94a6552b0195a1c4d73ea42bdf08bb6f8f) `melib/imap: gracefully retry without DEFLATE on BYE` in PR [`#517` "Fix some unrelated bugs I found while debugging build failure on armhf"](https://git.meli-email.org/meli/meli/pulls/517)

### Refactoring

- [**`20d73292`**](https://git.meli-email.org/meli/meli/commit/20d732926364a8e7a63c610b01f9677d70ecdc21) `melib: replace async-stream dep with async-fn-stream`
- [**`201081b6`**](https://git.meli-email.org/meli/meli/commit/201081b6d4abe2fd1f9cb32930eb6db92ba3ae8e) `meli/command: move tests to tests.rs`
- [**`84cfa358`**](https://git.meli-email.org/meli/meli/commit/84cfa358de8f0897b4d9d16216d8284e0a259e6c) `conf: remove need for global send_mail setting`
- [**`7be8912c`**](https://git.meli-email.org/meli/meli/commit/7be8912c1438853d72813277ba90197f603da23d) `Cargo.tomls: make formatting more consistent`
- [**`e6877e89`**](https://git.meli-email.org/meli/meli/commit/e6877e89c2c8d0fb93637670478d5694e9b9b571) `melib/jmap: refactor some parser imports`
- [**`f7ec6d6b`**](https://git.meli-email.org/meli/meli/commit/f7ec6d6bc5462fc594f65358f57db1b91514f086) `melib/jmap: implement mailbox rename`
- [**`15d24ab0`**](https://git.meli-email.org/meli/meli/commit/15d24ab0e3df7674d1d98bcdfaae5993d525a274) `meli/jobs: refactor spawn_{blocking,specialized} to spawn()`
- [**`6ee148c0`**](https://git.meli-email.org/meli/meli/commit/6ee148c04180b320a91d501e7b6bfbd1b96974b8) `Fix 1.80.0 clippy lints`
- [**`de72bc6a`**](https://git.meli-email.org/meli/meli/commit/de72bc6ac75ad46a0cfb1c0cabc3a67692be4354) `melib/error.rs: move network stuff to submodule`
- [**`a214a35c`**](https://git.meli-email.org/meli/meli/commit/a214a35c02fd3e84f5f527ab83141a55f549404e) `conf: refactor into submodules`
- [**`978cefbb`**](https://git.meli-email.org/meli/meli/commit/978cefbb507038aa9ffc77690ed5ac72c9d6275c) `Replace Escape ascii char with hex literal`
- [**`4b959f5c`**](https://git.meli-email.org/meli/meli/commit/4b959f5c4f20ff02a75ef708d975d8a1307b99e5) `Remove pcre feature/dependency`
- [**`036586a2`**](https://git.meli-email.org/meli/meli/commit/036586a2f703172acdd21a28d9ad04550d88bcb3) `Update serde dependency to 1.0.205`
- [**`191725b5`**](https://git.meli-email.org/meli/meli/commit/191725b5c2fb576b8ab7b5dd6ac5330b6f107fa5) `Fix some borrow checker error/warnings from upcoming 2024 edition`
- [**`11798be8`**](https://git.meli-email.org/meli/meli/commit/11798be8042db350c5d26433fa863fff1f0c6376) `Replace Envelope::message_id_display() with Display impls`
- [**`394236ba`**](https://git.meli-email.org/meli/meli/commit/394236ba8a2875eaeb0fb7cfe4d90855853523df) `email/address: Refactor References struct`
- [**`a7c73fc8`**](https://git.meli-email.org/meli/meli/commit/a7c73fc8cf8bf076a31810817780a6336caf0d69) `gpgme: refactor Rust interface, add tests`
- [**`41e1fdd5`**](https://git.meli-email.org/meli/meli/commit/41e1fdd55409bfecd226a30eb3100412fb23b345) `Fix cargo-derivefmt lints`
- [**`a44486d9`**](https://git.meli-email.org/meli/meli/commit/a44486d90480b9d891a91a4cab887af7b856e946) `imap: fix minor clippy lint`
- [**`0c0f8210`**](https://git.meli-email.org/meli/meli/commit/0c0f8210005be86f992c03f7b28e147cc8327cff) `Add a "move to Trash" shortcut`
- [**`d20a9d0a`**](https://git.meli-email.org/meli/meli/commit/d20a9d0afadbc92a4e8907d53e1ba311fda34985) `Fix new clippy lints`
- [**`e9a72072`**](https://git.meli-email.org/meli/meli/commit/e9a72072bfc989cffe3343195469dcb6d3041f4d) `Remove unused/obsolete plugins code and mentions`
- [**`2ddd28ee`**](https://git.meli-email.org/meli/meli/commit/2ddd28ee852de785c2a764f8ad6c99782723234e) `main.rs: always send a JobFinished event to all components`
- [**`571ae390`**](https://git.meli-email.org/meli/meli/commit/571ae390b8ae557cdf434a77a1e97068fcadfd6a) `pager.rs: don't set self dirty after filter selector` in PR [`#488` "view: fix nested filter jobs never being completed"](https://git.meli-email.org/meli/meli/pulls/488)
- [**`6bc0caf4`**](https://git.meli-email.org/meli/meli/commit/6bc0caf4e0c8cd91ccaf789f57e6a37daead41bc) `melib: remove redundant get_path_hash macro`
- [**`fc3308e4`**](https://git.meli-email.org/meli/meli/commit/fc3308e42829e1e65ad673c10a3cf49dcb53a967) `melib: Add Mail::as_mbox() method`
- [**`b1f24cbe`**](https://git.meli-email.org/meli/meli/commit/b1f24cbe95faeb4bd59c7ae94c0eb94d801f06a1) `view/filters: forward events on child filters`
- [**`1b201bf6`**](https://git.meli-email.org/meli/meli/commit/1b201bf6115f947263889174e17076ead88219a0) `Remove GlobMatch trait, replace usage with Fnmatch`
- [**`8af003ab`**](https://git.meli-email.org/meli/meli/commit/8af003abd0f17fb8d30a4a00e2cdcbf8d97e3926) `Rename addressbook stuff to "contacts"`
- [**`2069b4da`**](https://git.meli-email.org/meli/meli/commit/2069b4da098d68470441ff22602312cc34737574) `errors: impl From<xdg::BaseDirectoriesError>`
- [**`7dee32ae`**](https://git.meli-email.org/meli/meli/commit/7dee32ae88bf72929be0bee8e77eedaa63d4daf5) `contacts: refactor Card to its own module`
- [**`6d0d9680`**](https://git.meli-email.org/meli/meli/commit/6d0d96804057795b26001f6407f357152cbaed68) `jmap: move EmailObject state to Store`
- [**`0c590bbc`**](https://git.meli-email.org/meli/meli/commit/0c590bbc0cd9b7f1c85ef85a86e4adbeb54ad512) `contact-editor: remove empty space` in PR [`#495` "Add version migration support"](https://git.meli-email.org/meli/meli/pulls/495)
- [**`b2200ec3`**](https://git.meli-email.org/meli/meli/commit/b2200ec3abe9c5649d2628f88afe636f02a91be6) `Remove unused smtp tests` in PR [`#501` "Apply patches from upstream debian package"](https://git.meli-email.org/meli/meli/pulls/501)
- [**`ae294945`**](https://git.meli-email.org/meli/meli/commit/ae29494575a6433abf1ef7bd1eb4103dcd287773) `remove unused module file`
- [**`3558db51`**](https://git.meli-email.org/meli/meli/commit/3558db514adde57548f7d2bf185cb7c1fbf31124) `Move jobs and mailbox management Components together`
- [**`3a931035`**](https://git.meli-email.org/meli/meli/commit/3a9310352050e9dc012c41e1e905fd3dd24d64cb) `command: move Composer actions under TabActions`
- [**`441fda56`**](https://git.meli-email.org/meli/meli/commit/441fda568c753e1e512924f35ca42f079ef8369d) `terminal: move TextPresentation trait to melib`
- [**`ee897942`**](https://git.meli-email.org/meli/meli/commit/ee897942486aa7c083a9113f728bd1b808b76d2e) `lints: deny clippy::or_fun_call`
- [**`0d088962`**](https://git.meli-email.org/meli/meli/commit/0d088962d53f217ef1d06d7f67f72a05840505d6) `lints: Address clippy::too_long_first_doc_paragraph`
- [**`ecc9b482`**](https://git.meli-email.org/meli/meli/commit/ecc9b4823e3d99dad4ec589a9fb9d619e6226fb1) `Small repo cleanups`

### Documentation

- [**`a83b4176`**](https://git.meli-email.org/meli/meli/commit/a83b4176b0113acd8f9a26283bb102ddb2ff6425) `meli.1: small fixes`
- [**`72dea6f3`**](https://git.meli-email.org/meli/meli/commit/72dea6f3b236e116c1b165eb2d8169f0d3798d2f) `Manpage fixes`
- [**`a55f65e1`**](https://git.meli-email.org/meli/meli/commit/a55f65e131f769951a0c0ba4df82cd896b5a344b) `meli.conf.5: Fix wrong default value type in default_header_values`
- [**`57b45a9c`**](https://git.meli-email.org/meli/meli/commit/57b45a9c4ad7a6d8e15b29c26c307e72947d0d2f) `docs/historical-manpages: add DEP5 copyright file`
- [**`00236b86`**](https://git.meli-email.org/meli/meli/commit/00236b86f6e19c3a2f45f308d8e22ff0fd2a3dbe) `docs: add meli.conf.examples(5) WIP`
- [**`b88dc441`**](https://git.meli-email.org/meli/meli/commit/b88dc4412bc9ea4295ccbedaa499de3eacef911b) `Comment out svgfeature; no need to ship it` in PR [`#482` "milestone/0.8.8"](https://git.meli-email.org/meli/meli/pulls/482)
- [**`b048c95a`**](https://git.meli-email.org/meli/meli/commit/b048c95a8689e32e4b072c5d960293fef354e36e) `BUILD.md: add instructions for Android build`
- [**`593ed22b`**](https://git.meli-email.org/meli/meli/commit/593ed22ba166ee36ac52834ca0f040468a12c663) `pgp: perform gpgme's sign+encrypt manually` in PR [`#494` "pgp: perform gpgme's sign+encrypt manually"](https://git.meli-email.org/meli/meli/pulls/494)
- [**`50922d97`**](https://git.meli-email.org/meli/meli/commit/50922d97b82fdeaae57658e3fbff58fc21f5701d) `melib/README.md: update and fix feature table`
- [**`b912aabc`**](https://git.meli-email.org/meli/meli/commit/b912aabca22bb590eeab664b273f886e2fd99dac) `docs: add examples of file picker usage` in PR [`#516` "docs: add examples of file picker usage"](https://git.meli-email.org/meli/meli/pulls/516)

### Packaging

- [**`b55edd47`**](https://git.meli-email.org/meli/meli/commit/b55edd4727fcd2b0bc4b2712c97294e88a3ee001) `debian: update meli.docs and add meli.manpages`

### Miscellaneous Tasks

- [**`1232e16a`**](https://git.meli-email.org/meli/meli/commit/1232e16ad95143bb726e038a07f78366a53e7e83) `scripts/make_html_manual_page.py: don't prettify`
- [**`6d520605`**](https://git.meli-email.org/meli/meli/commit/6d520605ff8958a5a1343c81078708fda17bc6c2) `Vendor vobject crate`
- [**`b33433e4`**](https://git.meli-email.org/meli/meli/commit/b33433e457849a5212170efc6df135d6808acd5e) `Don't create backends as Box<dyn MailBackend>, but as Box<Self>`
- [**`2001b4dd`**](https://git.meli-email.org/meli/meli/commit/2001b4dd06fd755a7577f087abc2669e55d97134) `Make subscribed_mailboxes conf val optional`
- [**`6cfe4da0`**](https://git.meli-email.org/meli/meli/commit/6cfe4da0c1280d98a2962e8eeed92a9f38cfe018) `Enable rusqlite feature "modern_sqlite" always`
- [**`707a129e`**](https://git.meli-email.org/meli/meli/commit/707a129ea40b489dd5f62782628c19e08213dc98) `Coalesce repeating TUI notification messages`
- [**`f036f95e`**](https://git.meli-email.org/meli/meli/commit/f036f95eeebaa8273e226b6444c85352d641a828) `scripts: add generate_release_changelog_entry.sh`

### Continuous Integration

- [**`4684b601`**](https://git.meli-email.org/meli/meli/commit/4684b6016bfbba5e55e2c11e92be44025389ee02) `CI: remove env vars from action names` in PR [`#458` "Minor QoL fixes"](https://git.meli-email.org/meli/meli/pulls/458)
- [**`7419b465`**](https://git.meli-email.org/meli/meli/commit/7419b465eaf2ce158aaad00c5f1dc21edda246cb) `CI: unpin rust version after updating time dependency` in PR [`#460` "Update `time` dep to fix 1.80.0 breakage"](https://git.meli-email.org/meli/meli/pulls/460)
- [**`77da86eb`**](https://git.meli-email.org/meli/meli/commit/77da86eb0f08b55e4d6e496d0ac395bafe50b90c) `CI: Update cargo-derivefmt version`
- [**`1b3f2732`**](https://git.meli-email.org/meli/meli/commit/1b3f2732b25085a4101fcb9906bd70295d96dfb0) `CI: Move build.yaml actions to Makefile.build`
- [**`598a70f9`**](https://git.meli-email.org/meli/meli/commit/598a70f9df36a640345158ff15c779f9fa5fb3c3) `CI: move lints.yaml actions to Makefile.lint`
- [**`7e800a8f`**](https://git.meli-email.org/meli/meli/commit/7e800a8f3f7cb3a7ff9f8f1163f8f5b94523ccf4) `CI: move manifest_lints.yaml actions to Makefile.manifest-lints`
- [**`98652110`**](https://git.meli-email.org/meli/meli/commit/9865211076f88c098d83ae36d00b9a555bcc9356) `CI: prepend printf commands with @`
- [**`ad79bf84`**](https://git.meli-email.org/meli/meli/commit/ad79bf84c23c035a9c0709cf4b3e19e455f40334) `.gitea/Makefile.lint: attempt cargo-fmt with +nightly`

## [v0.8.7](https://git.meli-email.org/meli/meli/releases/tag/v0.8.7) - 2024-07-30

Contributors in alphabetical order:

- Andrei Zisu
- Damian Poddebniak
- Herby Gillot
- Manos Pitsidianakis

### Added

- [**`9fcb0a04`**](https://git.meli-email.org/meli/meli/commit/9fcb0a045169ae704f2dede57f2cbea84aaa9b80) `Add cargo-deny configuration file deny.toml`
- [**`7e8d19af`**](https://git.meli-email.org/meli/meli/commit/7e8d19afc7ba928b349a0752155928e4992b9787) `Add Envelope::sender_any`
- [**`9ab404c5`**](https://git.meli-email.org/meli/meli/commit/9ab404c57a5011a05555f70276c6e165d6623578) `Add pgp signed attachment support`
- [**`b4579075`**](https://git.meli-email.org/meli/meli/commit/b4579075a865100b5c44cc88b3a3f949d054bfe9) `Allow XOAUTH2 string passed as string`
- [**`0ffe7fa5`**](https://git.meli-email.org/meli/meli/commit/0ffe7fa5b31c5c20800daf001f87e489b05f24e0) `Add text/plain or text/html arg for text decoding`
- [**`e107d613`**](https://git.meli-email.org/meli/meli/commit/e107d613a065be8b47e2adcaabe0ad568ab765a0) `Add prelude module for import cleanup`
- [**`7200589a`**](https://git.meli-email.org/meli/meli/commit/7200589a9e079e30885357b77f6346762887f325) `Add ErrorKind::NotFound`
- [**`8c880dc7`**](https://git.meli-email.org/meli/meli/commit/8c880dc7471195287cf573783309032a232ce691) `Add {Error,ErrorKind}::is_recoverable()`
- [**`eb27773b`**](https://git.meli-email.org/meli/meli/commit/eb27773b470d652dd4846618644d06a1ac005097) `Add pager.named_filters setting`
- [**`84d93d65`**](https://git.meli-email.org/meli/meli/commit/84d93d65550a6e7b4053b872f769cc1e567abe4b) `Add support for ID extension (opt-in)`
- [**`af6838c2`**](https://git.meli-email.org/meli/meli/commit/af6838c20cd9ea6b75b036bf6443831cb3802f2e) `Add metadata field to MailBackendCapabilities`
- [**`d1499242`**](https://git.meli-email.org/meli/meli/commit/d1499242b2d250f4da991ecec99680a1814f99a9) `Add From<Infallible> impl`
- [**`814af0e9`**](https://git.meli-email.org/meli/meli/commit/814af0e94d44311345da37d1699277e09799340b) `Add --gzipped flag to man subcommand`
- [**`475860c9`**](https://git.meli-email.org/meli/meli/commit/475860c946d0b172d605a4c8cb58a2c0651fa0d8) `Accept - for stdio in {create,test}_config`
- [**`86f9b213`**](https://git.meli-email.org/meli/meli/commit/86f9b213bf1c987744419908727cc9d1f6d57888) `Add timeout conf field in validate()`
- [**`dd525bd9`**](https://git.meli-email.org/meli/meli/commit/dd525bd940db19196c6d2ce4a9a04114559064b5) `Use Error::is_recoverable`
- [**`6e1fea80`**](https://git.meli-email.org/meli/meli/commit/6e1fea805948a4c32e1ab1fa527a8e2947d352f8) `Show suggestions on Unauthorized error`
- [**`38620866`**](https://git.meli-email.org/meli/meli/commit/386208664b9a1695534ff946f4a2588bcc420940) `Detect DNS lookup std::io::Error`
- [**`a330ff96`**](https://git.meli-email.org/meli/meli/commit/a330ff96e939014ee3a1250ffb9ca5dc8f062e39) `Retry on DNS failure`
- [**`2429f17b`**](https://git.meli-email.org/meli/meli/commit/2429f17b4470207237ba0b9176b014527c551935) `On invalid conf value, print what value is expected`
- [**`6379fbe8`**](https://git.meli-email.org/meli/meli/commit/6379fbe8f492959e22dfaa1e71bba97f40cf32cb) `Add support for Undercurl attribute`
- [**`a13bf13f`**](https://git.meli-email.org/meli/meli/commit/a13bf13f2411230239482511155fad298060c815) `Add stub Undercurl support`
- [**`f5f1e068`**](https://git.meli-email.org/meli/meli/commit/f5f1e068783c419c5c6fd4c5b079ff7a0cf361da) `Add UIDPLUS support`
- [**`afccebf3`**](https://git.meli-email.org/meli/meli/commit/afccebf331b44e9c2707ee46b9dea9f76f31bbe4) `Add AUTH=PLAIN support`
- [**`9fb5bc41`**](https://git.meli-email.org/meli/meli/commit/9fb5bc41b4c71e4bc8878972a93013e78f4ebc5b) `Impl AUTH=ANONYMOUS (RFC4505)`

### Bug Fixes

- [**`ff3fe077`**](https://git.meli-email.org/meli/meli/commit/ff3fe0775838a25b4b17c0767143feeae2663e90) `Fix new 1.79.0 clippy lints`
- [**`430cbdfd`**](https://git.meli-email.org/meli/meli/commit/430cbdfd42c2bfd717c42568d10919ea27735869) `Fix python errors`
- [**`e3c1656e`**](https://git.meli-email.org/meli/meli/commit/e3c1656e05ecbdbb4360d1807293003850247d31) `Fix LOGINDISABLED support`
- [**`a82d1e1e`**](https://git.meli-email.org/meli/meli/commit/a82d1e1ebed9f4a2c9db3b223549bf52d08bb5d6) `Fix RowsState::rename_env stale data`
- [**`8dc4465c`**](https://git.meli-email.org/meli/meli/commit/8dc4465c58d1f7cbe33c5d5fac564169f1e375d2) `Fix toml value ser after update of toml dependency`
- [**`39e903b1`**](https://git.meli-email.org/meli/meli/commit/39e903b1d389878a6bc356f6693e3579498902b8) `Fix issues with ShellExpandTrait`
- [**`608301dc`**](https://git.meli-email.org/meli/meli/commit/608301dc3d7c8c81f5f650cf0d753ac2b5a85095) `Expand save-to paths asap`
- [**`100fa8b3`**](https://git.meli-email.org/meli/meli/commit/100fa8b3d1540d00154812003fd4c24acfaf5668) `Fix edge case in ShellExpandTrait`
- [**`a85b3a08`**](https://git.meli-email.org/meli/meli/commit/a85b3a089fae968a28f2fefb441379e27630ee17) `Allow default_mailbox to be any mailbox`
- [**`0dc24623`**](https://git.meli-email.org/meli/meli/commit/0dc2462358e1b1bdcf4e6a20eadf50722118da10) `Fix one by off error on menu unread count`
- [**`073aef86`**](https://git.meli-email.org/meli/meli/commit/073aef8671647822a0b0d8482572af2177cc00e3) `Fix lints/errors when compiling specific feature combos`
- [**`12695a00`**](https://git.meli-email.org/meli/meli/commit/12695a00daf82408cbc1b8a636394fa817be4cdf) `Fix MSRV breakage`
- [**`27ac3061`**](https://git.meli-email.org/meli/meli/commit/27ac3061e1488d0d6f309f3bc5bb040231679057) `Fix tag support not being printed`
- [**`97af00cd`**](https://git.meli-email.org/meli/meli/commit/97af00cd8358d63873eb271e7fe7b7ac4a58546d) `Respect timeout value from user configuration`
- [**`824de287`**](https://git.meli-email.org/meli/meli/commit/824de287b48d9e63f5fd4c325c8730a1f43406af) `Fix make_address! use`
- [**`f2e9cac3`**](https://git.meli-email.org/meli/meli/commit/f2e9cac38e8e8e02de7312e5b6871279cf675c97) `Use suggested minimum for maxObjectsInGet`
- [**`41d07fbc`**](https://git.meli-email.org/meli/meli/commit/41d07fbcef78890cdc6adf9b2cb601daf530258c) `NewState in EmailImportResponse cannot be null`
- [**`197132cc`**](https://git.meli-email.org/meli/meli/commit/197132cca0901aac4c3204fd2f7691baf02b6edf) `Support fetching with BODY[] for buggy servers`
- [**`91fdef98`**](https://git.meli-email.org/meli/meli/commit/91fdef9820a85381f2e3835f44c2df0f147dbaa2) `Return NotFound on cache miss`
- [**`96cc02a0`**](https://git.meli-email.org/meli/meli/commit/96cc02a00053e5b51218cb30a0a5551f74d33e89) `Do not use ErrorKind::Configuration`
- [**`e96e9789`**](https://git.meli-email.org/meli/meli/commit/e96e9789db72b610a1d4cdc2137fb03def383d78) `Don't discard pre-auth capabilities`
- [**`122a2a4d`**](https://git.meli-email.org/meli/meli/commit/122a2a4d766b2d861db7c35daa71bc11f730905a) `Drain event_queue when mailbox made available`

### Changes

- [**`27c4876f`**](https://git.meli-email.org/meli/meli/commit/27c4876fcaddb8b39320e58997bb8faa9ece2b67) `Prevent log flooding when drawing listing entries`
- [**`7bdc8f52`**](https://git.meli-email.org/meli/meli/commit/7bdc8f52b1f4b9f3c0d3a1307d496177caa172f8) `Highlight_self also when self is sender`
- [**`c4f7b77a`**](https://git.meli-email.org/meli/meli/commit/c4f7b77a393e5107b003f3a37cc0f52efcf635af) `Rework attachment rendering logic with filters`
- [**`1cce8c11`**](https://git.meli-email.org/meli/meli/commit/1cce8c1162b0894ffd5c6862f49a2af230b63b06) `Accept invalid "+" CRLF cont req`
- [**`c04b593b`**](https://git.meli-email.org/meli/meli/commit/c04b593bdffd5eabaf1b0d229d05664aa5e72d09) `Use BODY instead of RFC822`
- [**`084a222a`**](https://git.meli-email.org/meli/meli/commit/084a222a517832a36ff78243322a9b8134e86af4) `Remove subscribed mailboxes list`
- [**`5b6c1aa8`**](https://git.meli-email.org/meli/meli/commit/5b6c1aa88c43b86479e301244e99be43031b3ab3) `Don't show all background jobs`
- [**`f9a3b333`**](https://git.meli-email.org/meli/meli/commit/f9a3b33397faa595ed1ff91f5358c88e69b82532) `Return NotFound on empty FETCH`
- [**`15f3a3fb`**](https://git.meli-email.org/meli/meli/commit/15f3a3fba66ca58995e569aaec3dff10050fa0f2) `Retry fetch envelope only if err.is_recoverable()`
- [**`15eeac51`**](https://git.meli-email.org/meli/meli/commit/15eeac51916865f94ba551b867cb295581952387) `Enable dns_cache, tcp_keepalive & tcp_nodelay`
- [**`06437e60`**](https://git.meli-email.org/meli/meli/commit/06437e607c4298e2673ef7070bed7a3a50538985) `Set not_yet_seen to 0 when inserting existing`
- [**`0b113cdb`**](https://git.meli-email.org/meli/meli/commit/0b113cdbe11e8c294192c1d1d88e1de1d08b304f) `Use MELI_FEATURES in all cargo invocations`

### Refactoring

- [**`7856ea33`**](https://git.meli-email.org/meli/meli/commit/7856ea33f0bb3e44ee30df12a5947df1aa0dc624) `Transition more to imap-codec`
- [**`6f61176a`**](https://git.meli-email.org/meli/meli/commit/6f61176a9936047fa51bd203ce1fdf6f59d9c00e) `Remove unecessary mut modifier`
- [**`3251e7bd`**](https://git.meli-email.org/meli/meli/commit/3251e7bd61d828accc427dd49d49018a4156a368) `Scrub skip_serializing_if from attributes`
- [**`ebc1fa3b`**](https://git.meli-email.org/meli/meli/commit/ebc1fa3b8a0ca79ec8d494397b08f1e660a691f2) `Move module to self dir`
- [**`5110813e`**](https://git.meli-email.org/meli/meli/commit/5110813e87119e26157d78e086174a8da73a3bfc) `Refactor MaildirOp and watch()`
- [**`a9122c6e`**](https://git.meli-email.org/meli/meli/commit/a9122c6e349816751f56239876f041dd4e2ddf25) `Draw with x range argument`
- [**`3ebf5510`**](https://git.meli-email.org/meli/meli/commit/3ebf5510ea7a6e0ea6796288a677fa7eccd346ac) `Pass entire screen area when drawing overlay`
- [**`2dc1721a`**](https://git.meli-email.org/meli/meli/commit/2dc1721a49645329391c3e0a49a0dfce4069f767) `Move signal handling stuff to submodule`
- [**`738f7c46`**](https://git.meli-email.org/meli/meli/commit/738f7c4695e6bfcc5654d1bf7d6f3080e2f4850a) `Execute Opt subcommand in Opt::execute()`
- [**`46df4b57`**](https://git.meli-email.org/meli/meli/commit/46df4b573b2f9919780c0c9315ba941be05e8914) `Remove unused function stub`
- [**`52c75e92`**](https://git.meli-email.org/meli/meli/commit/52c75e92b5a26c2df09d0e23168b186be4832e10) `Use HeaderName constants`
- [**`6da4e2ec`**](https://git.meli-email.org/meli/meli/commit/6da4e2eca633f8487f22e67acee2317586f9fc03) `Replace stringify! in Debug impls with type checked macro`
- [**`85a55ed6`**](https://git.meli-email.org/meli/meli/commit/85a55ed633ed0c0420412a85b2743d465df5d61a) `Add some missing ErrorKinds to errors`
- [**`8b568f6e`**](https://git.meli-email.org/meli/meli/commit/8b568f6e3b3d28dcf14704c5f181637b22904f50) `Add if_in_state argument in Set::new()`
- [**`1e2e3da0`**](https://git.meli-email.org/meli/meli/commit/1e2e3da02f6a7737b7c992cfbd1fd93f963aabd3) `Treat color input "; ;" as "; 0 ;"`
- [**`7c47f702`**](https://git.meli-email.org/meli/meli/commit/7c47f70217dfea943f139f079b622f982beee462) `Extract test and parser modules to files`
- [**`d40ee692`**](https://git.meli-email.org/meli/meli/commit/d40ee6928fb3bd4d4c90f3fec3f5540b48de95e1) `Extract tests mod from protocol_parser`
- [**`1e50911c`**](https://git.meli-email.org/meli/meli/commit/1e50911c55980a02ccc2cc64546ccb8ef290bc5e) `Add utils module to protocol_parser`
- [**`d3a45b34`**](https://git.meli-email.org/meli/meli/commit/d3a45b344287c7003cf39308c239ce57c78a3757) `Make default shared lib name a const`
- [**`a9e9d952`**](https://git.meli-email.org/meli/meli/commit/a9e9d952d592fe7d8b0e7a7bc605a03de7bad136) `Change termination_string arg to Option`
- [**`fd76df78`**](https://git.meli-email.org/meli/meli/commit/fd76df7889c5884b2a204da3757dfe232fd7851a) `Use MELI_CONFIG env var in mock tests`
- [**`8552e499`**](https://git.meli-email.org/meli/meli/commit/8552e499e8e6eb5d9530bcad11055b0b9611c4e1) `Replace std::mem::{replace,take}`

### Documentation

- [**`dfc2bb43`**](https://git.meli-email.org/meli/meli/commit/dfc2bb43111541983eee6962cb665f342357634a) `Add link to MacPorts page for meli`
- [**`97aa6a8e`**](https://git.meli-email.org/meli/meli/commit/97aa6a8e6c3f49e3d37e68d07f2e3d7e356700b2) `Replace obsolete .Tn macro with .Em`
- [**`a8e82a30`**](https://git.meli-email.org/meli/meli/commit/a8e82a302ba043fb44563949ad7e03009c7808d8) `Add missing entries from JMAP`

### Miscellaneous Tasks

- [**`bbe2cffa`**](https://git.meli-email.org/meli/meli/commit/bbe2cffafe9718cb3b277f5c889ad31f0205dcfb) `Add rust-bindgen's friends.sh to scripts/`
- [**`a8956baf`**](https://git.meli-email.org/meli/meli/commit/a8956bafc8746ddc8407cda9afcccff5ce73386a) `Update to imap-codec v2.0.0-alpha.1`
- [**`c99633e1`**](https://git.meli-email.org/meli/meli/commit/c99633e1412a7ac805728f57158d9e0d9bbe305e) `Update futures dependency 0.3.28 -> 0.3.30`
- [**`fe604bf0`**](https://git.meli-email.org/meli/meli/commit/fe604bf0ea560cfd8dcdbfeab77b77ca6bdb2e83) `Update "openssl" dependency to 0.10.64`
- [**`9daf9437`**](https://git.meli-email.org/meli/meli/commit/9daf9437583f44f9fb1fa66288445e4cb47b5b2b) `Add test_cli_subcommands.rs`
- [**`9f783d9a`**](https://git.meli-email.org/meli/meli/commit/9f783d9a071129e2dd37b0ae23cb8d9845c2093f) `Pin assert_cmd ver to 2.0.13`
- [**`b7da1d0f`**](https://git.meli-email.org/meli/meli/commit/b7da1d0f9934ce1db2f8138c8c79880947dfae3a) `Check all targets in cargo-msrv verify test`
- [**`8a74920d`**](https://git.meli-email.org/meli/meli/commit/8a74920dc7266a5d1e3f9e4c45352c49e78e25b3) `Pin rust version to 1.79.0`

## [v0.8.6](https://git.meli-email.org/meli/meli/releases/tag/v0.8.6) - 2024-06-08

Contributors in alphabetical order:

- euxane
- Manos Pitsidianakis

### Added

- [**`735b44f2`**](https://git.meli-email.org/meli/meli/commit/735b44f2860307db1b664de43cea2b8c34a71b15) `Add 'highlight_self' theme attribute`
- [**`e187bb3f`**](https://git.meli-email.org/meli/meli/commit/e187bb3f0dd0166d69df4d84f1f63e1d005b4cbe) `Add tools subcommand with smtp shell for debugging`
- [**`571bd984`**](https://git.meli-email.org/meli/meli/commit/571bd984979031a08c3f908bda1f669b464afe78) `Add proper imap-shell in tools subcommand for debugging`
- [**`0e1e5b9e`**](https://git.meli-email.org/meli/meli/commit/0e1e5b9ea72cd5a691fb1044bee8b0f60cb1d936) `Add support for Alternate Scroll Mode (xterm)`
- [**`fe08d52a`**](https://git.meli-email.org/meli/meli/commit/fe08d52a7f4d9dfae1a9706b87aae8b77949c63d) `Add force_text_emoji_presentation option`

### Bug Fixes

- [**`3de4908d`**](https://git.meli-email.org/meli/meli/commit/3de4908d6b6fab293fedabde91e6f027fd08acfa) `man.7 Fix typo for toggle_expand_headers`
- [**`a8c7582f`**](https://git.meli-email.org/meli/meli/commit/a8c7582fa36ebb025877f341ea4226ffa2ff46bb) `Fix ENVELOPE parsing in untagged responses`
- [**`c65635ef`**](https://git.meli-email.org/meli/meli/commit/c65635ef600d09a354d2113d94f1aee153b2c721) `Fix compilation for macos`
- [**`06ec2790`**](https://git.meli-email.org/meli/meli/commit/06ec2790d0f3fd429e68987b4dfe6151afbc3d5b) `Fix str slice index panic`
- [**`f2b59a76`**](https://git.meli-email.org/meli/meli/commit/f2b59a7633d9578bfcadb205c8847d1409e8cfdc) `Add RequestUrlTemplate type`
- [**`7eed944a`**](https://git.meli-email.org/meli/meli/commit/7eed944abc54dfc8acd53a4e6382f311c66b5d6c) `Fix screwed up rfc8620 module split`
- [**`74a3539f`**](https://git.meli-email.org/meli/meli/commit/74a3539f882d4ccfbfc71e338f9fdcf4c8ed3e82) `Fix degenerate OOB cell access`
- [**`e8e76970`**](https://git.meli-email.org/meli/meli/commit/e8e7697001a8a41caee8dc108f6f76a9ea988c1e) `Fix edge case with strings/linebreaking`
- [**`81955187`**](https://git.meli-email.org/meli/meli/commit/819551876d594a5f4f6c4cdb481b0f8a8cfcf8cd) `Fix decryption error not shown`

### Refactoring

- [**`a9c3b151`**](https://git.meli-email.org/meli/meli/commit/a9c3b151f1a2418c6425ec953a2633892db6ff7d) `Impl highlight_self in all index styles`
- [**`57e3e643`**](https://git.meli-email.org/meli/meli/commit/57e3e643a1590775e819a49e6f6a02e0c0000466) `Remove excessive right padding in flags`
- [**`a4ebe3b7`**](https://git.meli-email.org/meli/meli/commit/a4ebe3b7d456c4283255d7cf5cc9d6903f1634d7) `Add ErrorKind::Platform`
- [**`4bdfb3a3`**](https://git.meli-email.org/meli/meli/commit/4bdfb3a31b3164dacce1b0d39eea1c3308ae8e8b) `Disable Nagle's algorithm by default`
- [**`4148aee5`**](https://git.meli-email.org/meli/meli/commit/4148aee59b1e0dda6a30fcf84c93b5a53733ae8a) `Refactor smtp,draft errors and email tests`
- [**`ed5a6b04`**](https://git.meli-email.org/meli/meli/commit/ed5a6b04f4e28f7fbea09cbc45fb8f781282bccb) `Add a symbols range to is_emoji check`
- [**`fc1122a2`**](https://git.meli-email.org/meli/meli/commit/fc1122a2aae9f52bb70cb71d1456669e4f7aa8f1) `Rename to backend_mailbox.rs`
- [**`50ecade7`**](https://git.meli-email.org/meli/meli/commit/50ecade74a5c4654fa31e3008ca56652b6288e7a) `Merge rfc8620/tests.rs to tests.rs`
- [**`a78f3f26`**](https://git.meli-email.org/meli/meli/commit/a78f3f261d89d5d991509dc71284f5ab13be734e) `Move submodules to jmap/`
- [**`f7838b1d`**](https://git.meli-email.org/meli/meli/commit/f7838b1ddf30e30ab61b6c2bb0438ecc5d909886) `Split to methods.rs and objects.rs`
- [**`74f0d12a`**](https://git.meli-email.org/meli/meli/commit/74f0d12afbec0d515743c4a1d177ac47a84a6c4f) `Remove obsolete imapshell.rs and smtp_conn.rs`
- [**`dce3852f`**](https://git.meli-email.org/meli/meli/commit/dce3852fe55b5614ef7f2f03d4efb76b2ddc256e) `Add capabilities module`
- [**`7ba7dc70`**](https://git.meli-email.org/meli/meli/commit/7ba7dc70c584d866b399184244457c0de83584d4) `Imports cleanup in all modules`
- [**`45bfcf87`**](https://git.meli-email.org/meli/meli/commit/45bfcf870710ae29e311a5a99df10d39fadd4b3b) `Minor refactors`
- [**`77867aee`**](https://git.meli-email.org/meli/meli/commit/77867aeed4e49d91b81fcb33b7bff5b0dd25daee) `Unwrap object module`
- [**`33999fc6`**](https://git.meli-email.org/meli/meli/commit/33999fc6abbd553a345944edaec44b7f32a98e19) `Re-add Submission to USING`
- [**`6be25ac3`**](https://git.meli-email.org/meli/meli/commit/6be25ac3df34ac8aec7721317feffca73f2fbadf) `Don't use client field for get/posts`

### Documentation

- [**`4722d7cc`**](https://git.meli-email.org/meli/meli/commit/4722d7ccb88c03e0dc4897f11b927c8e5d4b424a) `Also mention server_password_command for jmap`

### Miscellaneous Tasks

- [**`2bfe6086`**](https://git.meli-email.org/meli/meli/commit/2bfe60867835f17201bee348e03d073ae4d1e08e) `Hide self from "add contacts" options`
- [**`9ca34a68`**](https://git.meli-email.org/meli/meli/commit/9ca34a6864f3b71aca5c1a0dfb36191b55b3f636) `Update MSRV to 1.70.0`
- [**`50ff16c4`**](https://git.meli-email.org/meli/meli/commit/50ff16c44f1ecc85de904e43297149cb8a4cc73b) `Add LIGHT, DARK constant theme keys`
- [**`1abce964`**](https://git.meli-email.org/meli/meli/commit/1abce964c7c78aead04ce85ba7e318602eeab6d6) `Add Envelope::recipient_any method`
- [**`671d35e2`**](https://git.meli-email.org/meli/meli/commit/671d35e21ec3b2c62c6bca264acb37b5b2ae5aa9) `Update mailin-embedded dependency to 0.8.2`
- [**`39fbb164`**](https://git.meli-email.org/meli/meli/commit/39fbb164edf318d65b8cc3c7cfca4911e4c88235) `Change info_message_{next,prev} shortcuts to <, >`
- [**`58d73271`**](https://git.meli-email.org/meli/meli/commit/58d7327130aa2cefceb41d8dfc84015b6a9c7527) `Change new mail text content`
- [**`f0d1b9cf`**](https://git.meli-email.org/meli/meli/commit/f0d1b9cfd12490bc3698f0fe6d9e4e7249d91a59) `Add ayllu mirror link`
- [**`3bab5324`**](https://git.meli-email.org/meli/meli/commit/3bab5324c4273bf335b881b2e48ac4067bcb661e) `Improve Debug impl for ContentType etc`
- [**`e9dd6bec`**](https://git.meli-email.org/meli/meli/commit/e9dd6becc3722fc2e3a95626a8d8b61f9c12fd45) `Comment out content`
- [**`8dd87c1a`**](https://git.meli-email.org/meli/meli/commit/8dd87c1ac56b8277f89e4b9d76f5bc151e1a5f09) `Add ContentType::is_text_plain()`
- [**`01bc62e0`**](https://git.meli-email.org/meli/meli/commit/01bc62e0adfc16e7878299f3c74076739da5652f) `Add new_plaintext method`

## [v0.8.5](https://git.meli-email.org/meli/meli/releases/tag/v0.8.5) - 2024-05-05

Contributors in alphabetical order:

- Andrei Zisu
- Ethra
- Geert Stappers
- Guillaume Ranquet
- Manos Pitsidianakis

### Added

- [**`0e3a0c4b`**](https://git.meli-email.org/meli/meli/commit/0e3a0c4b7049139994a65c6fe914dd3587c6713e) `Add safe UI widget area drawing API`
- [**`0114e695`**](https://git.meli-email.org/meli/meli/commit/0114e695428579ef4461b289d7372e3b392b5e62) `Add next_search_result and previous_search_result shortcuts`
- [**`0b468d88`**](https://git.meli-email.org/meli/meli/commit/0b468d88ad53178d77fbf8a4d9014b4b8bbd134f) `Improve Error messages`
- [**`5af2e1ee`**](https://git.meli-email.org/meli/meli/commit/5af2e1ee6631fdfaa4f67243b97119debc32a484) `Add subcommand to print config file location`
- [**`62aee464`**](https://git.meli-email.org/meli/meli/commit/62aee4644bee3b4d0e549734272cf19c18a512ef) `Add subcommand to print log file location`
- [**`e2cdebe8`**](https://git.meli-email.org/meli/meli/commit/e2cdebe89cb0b0619d32efc0240efeb340e2cecd) `Add option to highlight self in mailing list threads`
- [**`cd448924`**](https://git.meli-email.org/meli/meli/commit/cd448924ed102531b7cf5676968baedc392c862e) `Add clear-selection command`
- [**`3a5306e9`**](https://git.meli-email.org/meli/meli/commit/3a5306e9dd50e3fe369295d8a5fef350a1ce9d77) `View manpages in pager inside meli`
- [**`a37d5fc1`**](https://git.meli-email.org/meli/meli/commit/a37d5fc1d19f5b195361ddb3ba045d848266285d) `Implement a key to command mapping`
- [**`ce4ba06c`**](https://git.meli-email.org/meli/meli/commit/ce4ba06ce9980990304b06b7ce3c34e50b0cdba7) `Add a flag set/unset command`
- [**`148f0433`**](https://git.meli-email.org/meli/meli/commit/148f0433d928e5f4bf64de88c029c1a4147cb6b1) `Implement flag set/unset action in UI`
- [**`417b24cd`**](https://git.meli-email.org/meli/meli/commit/417b24cd842775e1d44b972a7569c21b7c8ab936) `Print invalid command on error`
- [**`4e941a9e`**](https://git.meli-email.org/meli/meli/commit/4e941a9e8bca5778ebfa60515a7519df02309127) `Add default_mailbox setting`
- [**`974502c6`**](https://git.meli-email.org/meli/meli/commit/974502c6ffa263c8cfebf40c46b8694dce7fe982) `Impl Hash for Card`
- [**`ba7a97e9`**](https://git.meli-email.org/meli/meli/commit/ba7a97e90b4c474299a7b12fa74b7ea06c1535c8) `Add x axis scroll support`
- [**`ccf6f9a2`**](https://git.meli-email.org/meli/meli/commit/ccf6f9a26e95437fb24464f90736c653e3f5dfed) `Remember previous set [index_style] preferences`

### Bug Fixes

- [**`bcec745c`**](https://git.meli-email.org/meli/meli/commit/bcec745c241d7ed5d7d455ccdd65c6c95e1862b0) `Fix command and status bar drawing`
- [**`62b8465f`**](https://git.meli-email.org/meli/meli/commit/62b8465f2cd99789576d70008f1f321243b81fc3) `Fix ThreadView for new TUI API`
- [**`28fa66cc`**](https://git.meli-email.org/meli/meli/commit/28fa66cc2ad05e67708377fc99ffd65aa1b14386) `Fix ThreadedListing for new TUI API`
- [**`2c6f180d`**](https://git.meli-email.org/meli/meli/commit/2c6f180df987976c1f4cba7ceac878e697c73d27) `Fix macos compilation`
- [**`24971d19`**](https://git.meli-email.org/meli/meli/commit/24971d1960418bad92d89af9eb744933445baf99) `Fix compilation with 1.70.0 cargo`
- [**`34a2d52e`**](https://git.meli-email.org/meli/meli/commit/34a2d52e7ee8117f84d5177826c4d258c8dc28cd) `Fix rustdoc::redundant_explicit_links`
- [**`f63774fa`**](https://git.meli-email.org/meli/meli/commit/f63774fa6ddc9392be4191a549e7b99213132433) `Fix new clippy lints (1.75)`
- [**`33408146`**](https://git.meli-email.org/meli/meli/commit/33408146a178435d80871cdc91fad438144ded0f) `Fix feature permutation mis-compilations found with cargo-hack`
- [**`e3351d27`**](https://git.meli-email.org/meli/meli/commit/e3351d27555604524623bc8b6ae02f04a8414f97) `Fix set unseen updating all mboxes`
- [**`8185f2cf`**](https://git.meli-email.org/meli/meli/commit/8185f2cf7d539c714fd0a6128f40ba1fc9e0c43c) `Add deny clippy lints and fix them`
- [**`7861fb04`**](https://git.meli-email.org/meli/meli/commit/7861fb0402a652d692cb3c3be3b335d3b5a406fd) `Fix typos found with typos tool`
- [**`64e60cb0`**](https://git.meli-email.org/meli/meli/commit/64e60cb0ee79841ab40e3dba94ac27150a264c5c) `Fix select modifier regression`
- [**`60f26f9d`**](https://git.meli-email.org/meli/meli/commit/60f26f9dae1bb1e6379c2f6edd109c103179ddd6) `Fix some old pre-intradoc rustdoc links`
- [**`1fe36192`**](https://git.meli-email.org/meli/meli/commit/1fe361920809dfd4e51929e5a3e0376f46078422) `Make conf validation recognize AccountSettings extra keys`
- [**`c332c2f5`**](https://git.meli-email.org/meli/meli/commit/c332c2f5ff2cead87657bef489d223c6cb276697) `Fix new clippy lints (mostly clippy::blocks_in_conditions)`
- [**`070930e6`**](https://git.meli-email.org/meli/meli/commit/070930e671aa25f286ead73e6c9e80e23b5fef49) `Fix auto index build when missing`
- [**`26928e3a`**](https://git.meli-email.org/meli/meli/commit/26928e3ae90966e84d9e5f052631802ca634d99f) `Fix compilation for macos`
- [**`3884c0da`**](https://git.meli-email.org/meli/meli/commit/3884c0da1f3e7ba4ad85ff2fce087eb444e5ac20) `Small typographic fixups`
- [**`b820bd6d`**](https://git.meli-email.org/meli/meli/commit/b820bd6d9ceb5907d8550c9654fa8ce718d6b013) `Remove unused imap_trace! and fix comp`
- [**`a88b8c5e`**](https://git.meli-email.org/meli/meli/commit/a88b8c5ea055eb530ad523b673ea87fd19b70487) `Debian/changelog warning fix`
- [**`4ce616ae`**](https://git.meli-email.org/meli/meli/commit/4ce616aeca07b957cbf58c4a736a9a7d05b7cd04) `Fix lints.yaml rustup install step`
- [**`264782d2`**](https://git.meli-email.org/meli/meli/commit/264782d2280a63e82e16dfe005aeee522d102f82) `Various unimportant minor style/doc fixups`
- [**`475609fe`**](https://git.meli-email.org/meli/meli/commit/475609fe9264c37d3a8bc835ea654d2c2e047f9b) `Make {prev,next}_entry shortcut behavior consistent`
- [**`a69c674c`**](https://git.meli-email.org/meli/meli/commit/a69c674c072cdf1b6c9e3697e21673735718f119) `Fix new 1.77 clippy lints`
- [**`48cb9ee2`**](https://git.meli-email.org/meli/meli/commit/48cb9ee204929b3ae34e48be4f363572091b28f7) `Fix compilation for macos`
- [**`8a16cf6d`**](https://git.meli-email.org/meli/meli/commit/8a16cf6db482b7a65b1e6962c48a493fb1aaf0cf) `Fix wrong column index crash`
- [**`bc1b6531`**](https://git.meli-email.org/meli/meli/commit/bc1b65316d9e97812e107e849a6c93fbcc85f216) `Fix constant redrawing`
- [**`29cc1bce`**](https://git.meli-email.org/meli/meli/commit/29cc1bce5b61b575f76ff39888d4b420f1d3b7ce) `Remove obsolete file melib/src/text/tables.rs.gz`
- [**`ab041898`**](https://git.meli-email.org/meli/meli/commit/ab0418988748af508f461c7712f3a83ed36d11a7) `Fix new warnings for 1.78.0`
- [**`46e40856`**](https://git.meli-email.org/meli/meli/commit/46e40856baf4383b08a58d001b7ff9b0f5388fb7) `Fix UIConfirmationDialog highlight printing`
- [**`3b93fa8e`**](https://git.meli-email.org/meli/meli/commit/3b93fa8e7c4db8ce2e7714a84515ef772d480e53) `Don't draw messages above embedded terminal`
- [**`684fae3e`**](https://git.meli-email.org/meli/meli/commit/684fae3ed80aad6995aef22d6fae56f2c19f3e8f) `Copy old content to new buf when resizing`
- [**`5d915baa`**](https://git.meli-email.org/meli/meli/commit/5d915baa8162332340ba4e8c482abce99af40c11) `Use Screen::resize instead of CellBuffer::resize`
- [**`6a66afe9`**](https://git.meli-email.org/meli/meli/commit/6a66afe93eb928e6df93e79d0de386e0867fc3ec) `Make add contact dialog scrollable on overflow`
- [**`aa5737a0`**](https://git.meli-email.org/meli/meli/commit/aa5737a004adc0b23f4d9573465f5c3f2129fc3d) `Prevent drawing pager on embedded mode`
- [**`07072e2e`**](https://git.meli-email.org/meli/meli/commit/07072e2e3ffee70c0aa2f68622cf7173c8cc7193) `Prevent panic if envelope is deleted`
- [**`8ddd673d`**](https://git.meli-email.org/meli/meli/commit/8ddd673dd83d0d11115fabac8a85e068e3d3f233) `Update all mailboxes`
- [**`3691cd29`**](https://git.meli-email.org/meli/meli/commit/3691cd2962e9b3ae68128a6beb96f268d42b763f) `Send EnvelopeUpdate event after self.collection.update_flags()`
- [**`1fcb1d59`**](https://git.meli-email.org/meli/meli/commit/1fcb1d59b8d07ce1be861690b2b4c1d1b2b4fde3) `Remove rerun when build.rs changes`
- [**`933bf157`**](https://git.meli-email.org/meli/meli/commit/933bf157ae105c55ccb65880b4f3eef7039b54d5) `Ack \ as an atom`
- [**`a1cbb198`**](https://git.meli-email.org/meli/meli/commit/a1cbb1988b34951046045f724f52bed2925b3880) `Return Results instead of panicking`
- [**`b5ddc397`**](https://git.meli-email.org/meli/meli/commit/b5ddc397df982a885bb797f1ae762fe8f58fce56) `Remove unwrap() from get_events() loop`


### Changes

- [**`61a0c3c2`**](https://git.meli-email.org/meli/meli/commit/61a0c3c27f7c3691fb8f4503664f80667b0077e2) `Do not clear selection after action`
- [**`9af284b8`**](https://git.meli-email.org/meli/meli/commit/9af284b8db97b02788a2209bed705d20db6c2500) `Don't hide unread count for mailboxes that are partly truncated`
- [**`35408b16`**](https://git.meli-email.org/meli/meli/commit/35408b1689fcec1814e893a7bc34380e30ef7c57) `Run pager filter asynchronously`
- [**`e80ea9c9`**](https://git.meli-email.org/meli/meli/commit/e80ea9c9deee46f3de27c26a715bb06d1d09201a) `Changed default manpage install path`
- [**`742f038f`**](https://git.meli-email.org/meli/meli/commit/742f038f74051ab8499ec2aaa65bf4cd4c12e1dd) `Move sent_mailbox to settings`
- [**`86bbf1ea`**](https://git.meli-email.org/meli/meli/commit/86bbf1ea573c47107aee021b85651e54e7bdec79) `Refresh NotmuchMailbox counts when setting flags`
- [**`f0866a39`**](https://git.meli-email.org/meli/meli/commit/f0866a3965063ebe7dd2663a9ad1cba828ef91aa) `Make config error more user-friendly`
- [**`11f3077b`**](https://git.meli-email.org/meli/meli/commit/11f3077b06d2c7dce5dc35876a8bb419cbb642ca) `Add more possible values for manpage names`
- [**`1eca34b3`**](https://git.meli-email.org/meli/meli/commit/1eca34b3986ffd970d440440761c8516dbb863bb) `Set lowest priority to shortcut command UIEvents`
- [**`484712b0`**](https://git.meli-email.org/meli/meli/commit/484712b0c379aa95ba5792effd501bfa45c6c45c) `Check for unrecoverable errors in is_online`
- [**`8ec6f220`**](https://git.meli-email.org/meli/meli/commit/8ec6f220902d2994f9916a94e8ea43450b6917ab) `Use ShellExpandTrait::expand in more user-provided paths`

### Refactoring

- [**`0500e451`**](https://git.meli-email.org/meli/meli/commit/0500e451dab5f129d71a9279913531e77981e868) `Add missing EnvelopeRemove event handler`
- [**`ab14f819`**](https://git.meli-email.org/meli/meli/commit/ab14f81900a03a07ef00a6b3232cb29d78e8edf5) `Make write_string_to_grid a CellBuffer method`
- [**`e0adcdfe`**](https://git.meli-email.org/meli/meli/commit/e0adcdfe15b8a78c333de199ba734a83181f53be) `Move rest of methods under CellBuffer`
- [**`0a74c7d0`**](https://git.meli-email.org/meli/meli/commit/0a74c7d0e5c318dd29c8ace01e588d441e0fcfb6) `Overhaul refactor`
- [**`3b4acc15`**](https://git.meli-email.org/meli/meli/commit/3b4acc15a535c9bfd084b2e33f2cd00b5b5d4eb0) `Add tests`
- [**`7eedd860`**](https://git.meli-email.org/meli/meli/commit/7eedd860518e3f7f5000a1888e4fa58ddbfb43bc) `Remove address_list! macro`
- [**`f3e85738`**](https://git.meli-email.org/meli/meli/commit/f3e85738e7981755e96468213c02af78432f8cdd) `Move build.rs scripts to build directory`
- [**`77325486`**](https://git.meli-email.org/meli/meli/commit/773254864bd8436712905eeb0c725d1d05277e60) `Remove on-push hooks for actions w/ run on-pr`
- [**`08518e1c`**](https://git.meli-email.org/meli/meli/commit/08518e1ca81dd4eeabcaed9d3ec098b3b73f9a45) `Remove obsolete position.rs module`
- [**`ddab3179`**](https://git.meli-email.org/meli/meli/commit/ddab3179c2640488a3acd8cdeba1506a4e8f7641) `Move tests to tests module`
- [**`79520068`**](https://git.meli-email.org/meli/meli/commit/795200687020f8dbb82de619f8c5eecd07df6c49) `Remove doctests, add tests module`
- [**`4e7b6656`**](https://git.meli-email.org/meli/meli/commit/4e7b66567268ecb42eb55ac588ca401384835a38) `Sqlite caching refactor`
- [**`b5fd3f57`**](https://git.meli-email.org/meli/meli/commit/b5fd3f57a7d99b8918ca7bd24f715ccd6bfe448a) `Make self.view an Option`
- [**`a3aaec38`**](https://git.meli-email.org/meli/meli/commit/a3aaec382ab58c31cc8bc6122e5c67d6a97e4e9f) `Remove unused imports`
- [**`11a0586d`**](https://git.meli-email.org/meli/meli/commit/11a0586d562b8623e72a54ded7ba27b24c70206c) `Remove num_cpus dependency`
- [**`8f3dee9b`**](https://git.meli-email.org/meli/meli/commit/8f3dee9b22c7cea414bb49379f8240cb4a1beca3) `Extract mod manpages to standalone file`
- [**`89c7972e`**](https://git.meli-email.org/meli/meli/commit/89c7972e12a8eb0503795cf61c9b811882d4c19e) `Add suggestions to BadValue variant`
- [**`35a9f33a`**](https://git.meli-email.org/meli/meli/commit/35a9f33aab133d5368c140b40ed9477e251204d3) `Extract common FlagString logic`
- [**`1b0bdd0a`**](https://git.meli-email.org/meli/meli/commit/1b0bdd0a9a51ede2dd4fac0f75dd667894b704e2) `Split queries and mailbox into submodules`
- [**`506ae9f5`**](https://git.meli-email.org/meli/meli/commit/506ae9f594528051d0680059c1f82618fa0899c2) `Add ErrorKind::LinkedLibrary variant`
- [**`ebe1b3da`**](https://git.meli-email.org/meli/meli/commit/ebe1b3da7e6a209cd2ef2d3aa11809aaab29bc6f) `Wrap *mut struct fields in NonNull<_>`
- [**`ca7d7bb9`**](https://git.meli-email.org/meli/meli/commit/ca7d7bb95d387ef04fc568199e8767b4a428d7e4) `Use message freeze/thaw for flag changes`
- [**`4026e254`**](https://git.meli-email.org/meli/meli/commit/4026e254286dc061c0d3792dd3c6cda1bfdeb535) `Add some doc comments`
- [**`808aa494`**](https://git.meli-email.org/meli/meli/commit/808aa4942d1de1ecf0944d9d1c540b417f859991) `Rename text_processing to text for the whole brevity thing`
- [**`bebb473d`**](https://git.meli-email.org/meli/meli/commit/bebb473d1b6d9acfcd6df59573e2311dad98213f) `Derive extra traits for enums`
- [**`ab1b946f`**](https://git.meli-email.org/meli/meli/commit/ab1b946fd9d93a24a9802fbabe96705c15bc25e1) `Don't print details if it's an empty string.`
- [**`f685726e`**](https://git.meli-email.org/meli/meli/commit/f685726eac07c6b1d4f40dded6f3f55bcfcea5cc) `Add backtrace field to ParsingError`
- [**`73d5b24e`**](https://git.meli-email.org/meli/meli/commit/73d5b24e9847cf7dd033041e97f59274dece0b3d) `Merge integration tests in one crate`
- [**`31401fa3`**](https://git.meli-email.org/meli/meli/commit/31401fa35ca994632dee8b3f89bc4a921d64806b) `Add LazyCountSet::contains method`
- [**`0270db01`**](https://git.meli-email.org/meli/meli/commit/0270db0123eeb5abb41e53d07d308f1bd541f2a6) `From<&[u8]> -> From<B: AsRef<[u9]>>`
- [**`873a67d0`**](https://git.meli-email.org/meli/meli/commit/873a67d0fbe2512b03c0cf75b3ea7a89d4aecb1c) `Replace erroneous use of set_err_kind with set_kind`
- [**`51e3f163`**](https://git.meli-email.org/meli/meli/commit/51e3f163d403cbe127b8fc23c8fe0adf6beafd49) `Use Url instead of String in deserializing`
- [**`8014af25`**](https://git.meli-email.org/meli/meli/commit/8014af2563d4f1acf7016d8d67dea95add16ed2e) `Reduce debug prints`
- [**`f31b5c40`**](https://git.meli-email.org/meli/meli/commit/f31b5c4000a238bf17251aae0d7fd40c12dc6e37) `Don't print raw bytes as escaped unicode`
- [**`41e965b8`**](https://git.meli-email.org/meli/meli/commit/41e965b8a3743045356698a26dc4d1e21fcb0d36) `Split mbox/job stuff in submodules`
- [**`ec01a441`**](https://git.meli-email.org/meli/meli/commit/ec01a4412a825e9c7e4ecd253875ad4edae5893f) `Turn some sync connections to unsync`
- [**`3e914465`**](https://git.meli-email.org/meli/meli/commit/3e9144657b5f79764c762a0f1e8120a96bcce1f5) `Store children process metadata`
- [**`c53a32de`**](https://git.meli-email.org/meli/meli/commit/c53a32de4c04aa2ef7fedc4c83201cab952bc879) `Re-enables horizontal thread view`
- [**`36b7c00b`**](https://git.meli-email.org/meli/meli/commit/36b7c00b97b0b0ea07293bbdea0dc1cc94f3ad90) `Put doc text type names and co. in backtics`
- [**`634bd191`**](https://git.meli-email.org/meli/meli/commit/634bd1917a95919b65c37db612e0d082ef5f3aa4) `Convert log prints to traces`
- [**`1048ce68`**](https://git.meli-email.org/meli/meli/commit/1048ce6824391d9d81a6f711d8dbdad549c9c6b9) `Add hostname() utility function`
- [**`7645ff1b`**](https://git.meli-email.org/meli/meli/commit/7645ff1b875e3920389567eb5e61d800291e8a27) `Rename write_string{to_grid,}`
- [**`c2ae19d1`**](https://git.meli-email.org/meli/meli/commit/c2ae19d1208f2eb5cca341a04e019c3e285637a8) `Return Option from current_pos`
- [**`b61fc3ab`**](https://git.meli-email.org/meli/meli/commit/b61fc3ab6482dcef4f5cc1c09db3539b7e401f78) `Add HelpView struct for shortcuts widget`
- [**`3495ffd6`**](https://git.meli-email.org/meli/meli/commit/3495ffd61b5888f8538304ecb6e441819b373bdc) `Change UIEvent::Notification structure`
- [**`23c15261`**](https://git.meli-email.org/meli/meli/commit/23c15261e79c63791c569f225c1745df1b90ce2d) `Abstract envelope view filters away`
- [**`031d0f7d`**](https://git.meli-email.org/meli/meli/commit/031d0f7dc76700ac938e1ee4a767fab8deebb9f2) `Add area.is_empty() checks in cell iterators`
- [**`e37997d6`**](https://git.meli-email.org/meli/meli/commit/e37997d697f1f0b8faaa56a36f43c9f1da4bbb41) `Store Link URL value in Link type`
- [**`b6f769b2`**](https://git.meli-email.org/meli/meli/commit/b6f769b2f43087b71bf93667911cb22d7f690c95) `Add field names to row_attr! bool values`
- [**`0da97dd8`**](https://git.meli-email.org/meli/meli/commit/0da97dd8c11835d25e056c3dc0821b5574ab06f0) `Check row_updates in is_dirty()`
- [**`6506fffb`**](https://git.meli-email.org/meli/meli/commit/6506fffb9427ba13ba4368cd6b2c0dba12e5294c) `Rewrite email flag modifications`
- [**`23507932`**](https://git.meli-email.org/meli/meli/commit/23507932f94257a71f2ca8db23840ee0716072b6) `Update cache on set_flags`
- [**`470cae6b`**](https://git.meli-email.org/meli/meli/commit/470cae6b885c9b4851195fbb8274b1663bfa75cb) `Update thread cache on email flag modifications`
- [**`84f3641e`**](https://git.meli-email.org/meli/meli/commit/84f3641ec1401a0522811add0ed87a131be449b9) `Re-add on-screen message display`
- [**`54d21f25`**](https://git.meli-email.org/meli/meli/commit/54d21f25fdb716d36fd3678dd149eb880e16698d) `Re-add contact list and editor support`
- [**`458258e1`**](https://git.meli-email.org/meli/meli/commit/458258e1aab91f3883d6a9201a175462511349e9) `Re-enable compact listing style`
- [**`1c1be7d6`**](https://git.meli-email.org/meli/meli/commit/1c1be7d6c9bfc9f14c3a62ce464e1e15f2e6c4ec) `Add display_name(), display_slice(), display_name_slice() methods`
- [**`5dd71ef1`**](https://git.meli-email.org/meli/meli/commit/5dd71ef1cd93aebaadb0554eac692d0a0fa4aecd) `Upgrade JobsView component to new TUI API`
- [**`b5cc2a09`**](https://git.meli-email.org/meli/meli/commit/b5cc2a095f0268bb90cab150e903b0bbaffe1479) `Upgrade MailboxManager component to new TUI API`
- [**`ed8a5de2`**](https://git.meli-email.org/meli/meli/commit/ed8a5de2cb4b93ad766803d3590f7041f28cc419) `Re-enable EditAttachments component`
- [**`77a8d9e2`**](https://git.meli-email.org/meli/meli/commit/77a8d9e2c2094e84e06f5d624cb6f8afda24a400) `Make ModSequence publicly accessible`
- [**`64898a05`**](https://git.meli-email.org/meli/meli/commit/64898a0583e348fef3cd266a7196425e7015a871) `Make UIDStore constructor pub`

### Documentation

- [**`e4818803`**](https://git.meli-email.org/meli/meli/commit/e481880321fecef4978c02a5bb834652d8d77b46) `Various manpage touchups and URL updates`
- [**`38bca8f8`**](https://git.meli-email.org/meli/meli/commit/38bca8f8bc11eff28991bedac344bfebebdd22d7) `Mention use_oauth2=true for gmail oauth2`
- [**`660022ce`**](https://git.meli-email.org/meli/meli/commit/660022ce238ddda1de7aaac228c4d990bd643429) `Add mailaddr.7 manpage`
- [**`c5e9e676`**](https://git.meli-email.org/meli/meli/commit/c5e9e67604313acd73b33b7000a1ba59e38ced34) `Add historical-manpages dir`
- [**`5afc0785`**](https://git.meli-email.org/meli/meli/commit/5afc078587b86104dc5b615c1840dade64768a5f) `Update README.md, DEVELOPMENT.md and create BUILD.md`
- [**`d018f07a`**](https://git.meli-email.org/meli/meli/commit/d018f07aa51fc293bf696fa7d7beff8e59ac91a8) `Retouch manual pages`
- [**`3adba40e`**](https://git.meli-email.org/meli/meli/commit/3adba40e32a8a66271ea2a8f5ddf27858744ecd6) `Add macos manpage mirror url`

### Packaging

- [**`cd2ba80f`**](https://git.meli-email.org/meli/meli/commit/cd2ba80f8e5424be08421b4dcc5113977418f240) `Update metadata`
- [**`5f8d7c80`**](https://git.meli-email.org/meli/meli/commit/5f8d7c8039c0623b3950fd1a8eb566f943fc309d) `Update deb-dist target command with author metadata`
- [**`59c99fdc`**](https://git.meli-email.org/meli/meli/commit/59c99fdc79bb31fb42cb99d4b95613022396a499) `Update debian package metadata`
- [**`97eb6363`**](https://git.meli-email.org/meli/meli/commit/97eb6363756f18e91024dfd3f2407fdc70d6d9c3) `Add dpkg --print-architecture to deb filename`
- [**`7412c238`**](https://git.meli-email.org/meli/meli/commit/7412c238704c14b0007caeae9d0bc9d9e3db8abb) `Bump meli version to 0.8.5-rc.3`
- [**`500fe7f7`**](https://git.meli-email.org/meli/meli/commit/500fe7f7e43a4cf5b798593dd06713954189bacb) `Update CHANGELOG.md`
- [**`5ff4e8ae`**](https://git.meli-email.org/meli/meli/commit/5ff4e8ae68182db8d4535d8537d26a3f398c815b) `Run builds.yaml when any manifest file changes`
- [**`0a617410`**](https://git.meli-email.org/meli/meli/commit/0a617410ec1ce5f6fb43772e4ad43f45f58a7f4d) `Split test.yaml to test.yaml and lints.yaml`
- [**`3ba1603a`**](https://git.meli-email.org/meli/meli/commit/3ba1603af2a9e408659717b9c8dace7406a8b142) `Add manifest file only lints workflow`
- [**`1617212c`**](https://git.meli-email.org/meli/meli/commit/1617212c5b0948174155ece4a9d0584764bd7dac) `Add scripts/check_debian_changelog.sh lint`
- [**`c41f35fd`**](https://git.meli-email.org/meli/meli/commit/c41f35fdd55bf093656b68cc69eab4cf4b9a8ec4) `Use actions/checkout@v3`
- [**`876616d4`**](https://git.meli-email.org/meli/meli/commit/876616d45b7798131ecdda82bb90d1d481842f5c) `Use actions/upload-artifact@v3`
- [**`2419f4bd`**](https://git.meli-email.org/meli/meli/commit/2419f4bd40fb1a732cf1df42dde48ba8ca812072) `Add debian package build workflow`
- [**`10c3b0ea`**](https://git.meli-email.org/meli/meli/commit/10c3b0eabe1684699c775e03c4c58038ea7979af) `Bump version to 0.8.5-rc.1`
- [**`d16afc7d`**](https://git.meli-email.org/meli/meli/commit/d16afc7d8d9e2eddb81664673e9a4ef82da2e303) `Bump version to 0.8.5-rc.2`
- [**`da251455`**](https://git.meli-email.org/meli/meli/commit/da251455a0185e207e0ec2d51273f6ddbdb572a8) `Bump meli version to 0.8.5-rc.2`

### Miscellaneous Tasks

- [**`c4344529`**](https://git.meli-email.org/meli/meli/commit/c4344529e30b3385149d6dc3c1c4b34306a85491) `Add .git-blame-ignore-revs file`
- [**`f70496f1`**](https://git.meli-email.org/meli/meli/commit/f70496f14c2405dae3be932f0a3feffec50f34d4) `Add codemeta.json`
- [**`b3079715`**](https://git.meli-email.org/meli/meli/commit/b3079715f6d6b08a2ed3482bf718f5da8adac0ed) `Disable flakey test_smtp()`
- [**`8a95febb`**](https://git.meli-email.org/meli/meli/commit/8a95febb7807fa4a1b2f97ff2637da6fceab8284) `Set debuginfo=0 in test/lint builds`
- [**`81d1c053`**](https://git.meli-email.org/meli/meli/commit/81d1c0536bb15c8ba796fc36dddab7043a8eaaf4) `Add mandoc_lint.sh`
- [**`8de8addd`**](https://git.meli-email.org/meli/meli/commit/8de8addd11e97bfd9ec475bdf1f6907b4ff8da62) `Add cfg for musl builds`
- [**`70fc2b45`**](https://git.meli-email.org/meli/meli/commit/70fc2b455c5b6dbfbfc0f505e597942712298c7b) `Update nix dependency to 0.27`
- [**`fd64fe0b`**](https://git.meli-email.org/meli/meli/commit/fd64fe0bf87a75c628b93755f9ef923728df65ce) `Update codeberg.org URL`
- [**`30a3205e`**](https://git.meli-email.org/meli/meli/commit/30a3205e4fb0b9f6e578621c72160df5e4b8d09b) `Add clippy::doc_markdown`
- [**`c7aee725`**](https://git.meli-email.org/meli/meli/commit/c7aee7252587d2e42ca5df4a65731e183f7edaf2) `Add clippy::doc_markdown`
- [**`b8b24282`**](https://git.meli-email.org/meli/meli/commit/b8b24282a0dd098bea6500192bf5a74cdbecd8c3) `Update all instances of old domains with meli-email.org`
- [**`ae96038f`**](https://git.meli-email.org/meli/meli/commit/ae96038fbf5884d5f4e52d0b0488dedb6e5f4050) `Make unicode-segmentation a hard dependency`
- [**`255e9376`**](https://git.meli-email.org/meli/meli/commit/255e93764ad7218474e1b08d59d6808281b89b8b) `Update linkify dep from 0.8.1 to 0.10.0`
- [**`dedee908`**](https://git.meli-email.org/meli/meli/commit/dedee908d1e0b42773bade8e0604e94b14810e2d) `Update notify dep from 4.0.17 to 6.1.1`
- [**`c1c41c91`**](https://git.meli-email.org/meli/meli/commit/c1c41c9126005266f00d4979777718463dddf7b2) `Update README.md and add Codeberg mirror`
- [**`71f3ffe7`**](https://git.meli-email.org/meli/meli/commit/71f3ffe740276087f20d85d62440ef5d3fe426f6) `Update Makefile`
- [**`63a63253`**](https://git.meli-email.org/meli/meli/commit/63a63253d77f6e1b9a42ec55ecf0bbc45a011245) `Use type alias for c_char`
- [**`c751b2e8`**](https://git.meli-email.org/meli/meli/commit/c751b2e8450aa83b7a8f5e8afbeccadf333f74ba) `Re-enable conversations listing style`
- [**`3a709794`**](https://git.meli-email.org/meli/meli/commit/3a7097948308981204132a0eed2d28338f9d6b33) `Update minimum rust version from 1.65.0 to 1.68.2`
- [**`f900dbea`**](https://git.meli-email.org/meli/meli/commit/f900dbea468e822c5a510a72ecc6367549443927) `Use cargo-derivefmt to sort derives alphabetically`
- [**`e19f3e57`**](https://git.meli-email.org/meli/meli/commit/e19f3e572c0ac585a6c2023e50f8fd0bd2ea2dae) `Cargo-sort all Cargo.toml files`

## [v0.8.4](https://git.meli-email.org/meli/meli/releases/tag/v0.8.4) - 2023-11-22

### Fixed

- [**`ef30228e`**](https://git.meli-email.org/meli/meli/commit/ef30228e08efe6e36ab9858a5ba32876d6d8fdae) `Fix failing test`

### Miscellaneous Tasks

- [**`f81a1e23`**](https://git.meli-email.org/meli/meli/commit/f81a1e23382208390394be71e3aaa27ee505cb0f) `Bump version to 0.8.4`

## [v0.8.3](https://git.meli-email.org/meli/meli/releases/tag/v0.8.3) - 2023-11-22

### Added

- [**`3105a037`**](https://git.meli-email.org/meli/meli/commit/3105a0373b8754f37b326239c1cf7129fae06e1b) `Add quit command`

### Fixed

- [**`d3cbf184`**](https://git.meli-email.org/meli/meli/commit/d3cbf184e606d5b7ade9cfb125db01f45d7180ae) `Add extra_submission_headers fields in composer form and autocomplete for Newsgroups`
- [**`7aec5b8e`**](https://git.meli-email.org/meli/meli/commit/7aec5b8e78d80e7717a9aedd7344db6b108534f5) `Fix SMTP example doc`
- [**`f702dc22`**](https://git.meli-email.org/meli/meli/commit/f702dc220c9ab97ce0fddfae194d5e2935a20193) `Fix new clippy lints.`
- [**`688e39a6`**](https://git.meli-email.org/meli/meli/commit/688e39a67e6a467ca649acbe20b1f368fbc1e9f0) `Fix clippy lints`

### Changed

- [**`5a7919bb`**](https://git.meli-email.org/meli/meli/commit/5a7919bb03641be6d7bc5b9002d44e16ee358f12) `Use ConversationsListing::format_date`
- [**`0f3b5294`**](https://git.meli-email.org/meli/meli/commit/0f3b52945959b53c8d809eb434a91ec4c561b2d4) `Hoist format_date() to ListingTrait method`

### Refactoring

- [**`e1b55340`**](https://git.meli-email.org/meli/meli/commit/e1b55340fa258a2a7b118fd18c11614fb2b5e173) `Show error description when TIOCGWINSZ ioctl fails`
- [**`e95c275d`**](https://git.meli-email.org/meli/meli/commit/e95c275d68fe3dbd588046c110ae8b3fa966f6de) `Remove duplicate end sequence`
- [**`8a21be21`**](https://git.meli-email.org/meli/meli/commit/8a21be21775cb474a6b65e1c0bffd771c0df6f2f) `Replace splice with truncate`
- [**`2db021fa`**](https://git.meli-email.org/meli/meli/commit/2db021fa0a9a707cd7cdb6c8bf140bf5c8acf906) `Remove regexp from default features`
- [**`fa33a946`**](https://git.meli-email.org/meli/meli/commit/fa33a9468a16c50361353efa269fca79bd58e284) `Move managesieve-client binary to tools/`

### Miscellaneous Tasks

- [**`e88957ae`**](https://git.meli-email.org/meli/meli/commit/e88957ae6edfee7fabb41e9210f9d906866cda8d) `Add extra_submission_headers field in MailBackendCapabilities struct`
- [**`606f487f`**](https://git.meli-email.org/meli/meli/commit/606f487fc5e227f1727697a5911e27cbec174089) `Add IRC channel badge`
- [**`0e60bdf2`**](https://git.meli-email.org/meli/meli/commit/0e60bdf26eb842744f59257800ca8e30b1a43836) `Add "iterator" feature to signal-hook`
- [**`ac2a5dcd`**](https://git.meli-email.org/meli/meli/commit/ac2a5dcdd10d97f5ed9c8a8c83e1641b373dd31a) `Add display() method for Address`
- [**`43bfd413`**](https://git.meli-email.org/meli/meli/commit/43bfd4131d5cab39319d1943bcad46e929ec4d56) `Update ahash dependency`
- [**`af241d25`**](https://git.meli-email.org/meli/meli/commit/af241d25cbab20227a88ec4d557222cdeed98dde) `Bump version to 0.8.3`
- [**`7387b67e`**](https://git.meli-email.org/meli/meli/commit/7387b67eeee27aefbc4d20ca2a1d503aa0fb1838) `Enable "static" build for C library dependencies by default`
- [**`bfc78a08`**](https://git.meli-email.org/meli/meli/commit/bfc78a0803524e236bc883833838d3ad78918621) `Replace CRLF with LF when editing`
- [**`111a1160`**](https://git.meli-email.org/meli/meli/commit/111a1160adf2e0fef00a90350784307c859a198b) `Bump version to 0.8.3`

## [v0.8.2](https://git.meli-email.org/meli/meli/releases/tag/v0.8.2) - 2023-09-22

### Fixed

- [**`73b3ed55`**](https://git.meli-email.org/meli/meli/commit/73b3ed559d21dcc7cdee7f96119461e2447c1906) `Fix forward dialog not workng`
- [**`7888d8b2`**](https://git.meli-email.org/meli/meli/commit/7888d8b2a5dc977f0f18094a32dc73893a5cfc4f) `Fix doc test compilation`

### Changed

- [**`22525d40`**](https://git.meli-email.org/meli/meli/commit/22525d40fb48661f86657151e35fdf9c95c4b45e) `Go to end when pressing next/page down for the second time`
- [**`71474436`**](https://git.meli-email.org/meli/meli/commit/714744366f5e26fc1b6609e8e785d64489f9a68d) `Revert 22525d40 behavior when sidebar not focused`

### Miscellaneous Tasks

- [**`eb5d49c4`**](https://git.meli-email.org/meli/meli/commit/eb5d49c41ac58c5068011620c22e21b5fa115417) `Use Self in self methods`
- [**`3d85ca2e`**](https://git.meli-email.org/meli/meli/commit/3d85ca2edfca9abff4b3ffdd837b25e68c6586c2) `Bump version to 0.8.2`

## [v0.8.1](https://git.meli-email.org/meli/meli/releases/tag/v0.8.1) - 2023-09-13

### Added

- [**`6476985c`**](https://git.meli-email.org/meli/meli/commit/6476985ce6abbb9048ba5aec19f6c5144bfe89b7) `Add Cross.toml for aarch64-unknown-linux-gnu builds`
- [**`45d4f611`**](https://git.meli-email.org/meli/meli/commit/45d4f611b170d7b80afca5810c51fea1bf084c10) `Add install-man cli subcommand to install manpages on your system`
- [**`a4f0dbac`**](https://git.meli-email.org/meli/meli/commit/a4f0dbac26126c03886115e518b3cd2ede0b88cb) `Add current working directory tracking to Context`

### Fixed

- [**`49a38a23`**](https://git.meli-email.org/meli/meli/commit/49a38a23bf522a18e636385632cfe3533c4f525c) `Fix invalid Type link references`
- [**`85af5244`**](https://git.meli-email.org/meli/meli/commit/85af524458bc06421ac39689469474efb8164c1c) `Fix invalid mailto() results when body field exists`
- [**`c7825c76`**](https://git.meli-email.org/meli/meli/commit/c7825c76c3ac6be89f64f1f04afd9c0ca08bdf76) `Handle dialog Esc in the parent component`
- [**`dd4d0b79`**](https://git.meli-email.org/meli/meli/commit/dd4d0b79721d8cd5b29cdaca9cd01412974f2e13) `Fix typo`
- [**`c43aeb0e`**](https://git.meli-email.org/meli/meli/commit/c43aeb0eb103f2a8fd802f84eab56551c6e65418) `Fix invalid address parse on folded values`
- [**`7e3e9386`**](https://git.meli-email.org/meli/meli/commit/7e3e9386316ef344580d9e44edb3f8b0c196c3c5) `Fix out-of-bounds draw when terminal is small`
- [**`7e4ed2fa`**](https://git.meli-email.org/meli/meli/commit/7e4ed2fa107eca2ef309bcaa211440c315730b6c) `Fix some out of bounds drawing.`

### Changed

- [**`1b3bebe3`**](https://git.meli-email.org/meli/meli/commit/1b3bebe3049ae5c7cb2210ed95c355c9b5c709f8) `Open earliest unread email instead of first in thread`
- [**`49c36009`**](https://git.meli-email.org/meli/meli/commit/49c36009cec8c88d61d796162787990216bfeeab) `Don't initialize entire thread at once`
- [**`0a9c89b6`**](https://git.meli-email.org/meli/meli/commit/0a9c89b6b357fc3d002c3eb451fd67e7a49ce7f5) `Add toggle_layout shortcut`
- [**`64ba0459`**](https://git.meli-email.org/meli/meli/commit/64ba0459ee3652eaf451d10222853a898d85e337) `Init cursor at To: header field`
- [**`81974311`**](https://git.meli-email.org/meli/meli/commit/81974311c200b8ad66c0e626f8b8db6686e565ff) `Show current number command buffer`

### Refactoring

- [**`a337e226`**](https://git.meli-email.org/meli/meli/commit/a337e2269e584769314cdf325cdeb6e57cb0c622) `Refactor module structure`
- [**`b4f2f335`**](https://git.meli-email.org/meli/meli/commit/b4f2f3357613729e493e5f41a48def7610dc65aa) `Remove deflate feature; make it a hard dependency`
- [**`2dc29405`**](https://git.meli-email.org/meli/meli/commit/2dc29405868b9df0dfff25e341814526a478db00) `Add feature to use cache instead of downloading unicode data`
- [**`0132677f`**](https://git.meli-email.org/meli/meli/commit/0132677ff54a9618d3c59b08a188b73ae0c062c7) `Introduce CommandError with context`
- [**`3344a8db`**](https://git.meli-email.org/meli/meli/commit/3344a8dbf6b478a85d2b933fc1fa1a6001c600f4) `Remove unnecessary Clone derives`
- [**`b673af02`**](https://git.meli-email.org/meli/meli/commit/b673af02ac9e9d4be95daa2490ce24d0bc9b10d9) `Move to crate root`
- [**`54862f86`**](https://git.meli-email.org/meli/meli/commit/54862f8651cb7dfe3bca7f5924fe776b93ac6aee) `Add hide_sidebar_on_launch option`

### Miscellaneous Tasks

- [**`a615b470`**](https://git.meli-email.org/meli/meli/commit/a615b4701b7e852a9112b317e2e31997c6cbe82e) `Embed xdg-utils crate`
- [**`f0075b86`**](https://git.meli-email.org/meli/meli/commit/f0075b86cf636a3d39d4edf1ff6d58c112bbecf7) `Show descriptive tab names for composer and threads`
- [**`6d5ebb5b`**](https://git.meli-email.org/meli/meli/commit/6d5ebb5b04279fe6e4fbf598504cae2f012fa494) `Split code into submodules, add better error reporting`
- [**`63abf1e8`**](https://git.meli-email.org/meli/meli/commit/63abf1e890b93fcadf35f88b3dbea473c0d8f5cd) `Update README.md`
- [**`bb4d2000`**](https://git.meli-email.org/meli/meli/commit/bb4d20003690d72b62a66d46a1fc5ae914e2bf64) `Unify toggle_* parsers`
- [**`9b9c38f7`**](https://git.meli-email.org/meli/meli/commit/9b9c38f769abae0ff86e4b71e4db0ad65fdacfb4) `Don't flood user with sqlite3 errors if db is corrupted`
- [**`747e39bf`**](https://git.meli-email.org/meli/meli/commit/747e39bf55cfc19b6eeece3ca7c71bad98d92389) `Add print-used-paths subcommand`
- [**`39e99770`**](https://git.meli-email.org/meli/meli/commit/39e99770da4b51d0986a4b561fbe36b27d04565d) `Use Context::current_dir() when saving files to relative paths`
- [**`fe0a96f0`**](https://git.meli-email.org/meli/meli/commit/fe0a96f0855486207280430064a93cab94dffeb2) `Update to 2021 edition`
- [**`3944e4e6`**](https://git.meli-email.org/meli/meli/commit/3944e4e60e431247eefc0b3cf35af27fb011f37b) `Update to 2021 edition`
- [**`7eed8278`**](https://git.meli-email.org/meli/meli/commit/7eed82783a3dbac513e233be4f0bce06904fe8c8) `Bump version to 0.8.1`

## [v0.8.0](https://git.meli-email.org/meli/meli/releases/tag/v0.8.0) - 2023-08-29

### Added

- [**`36e29cb6`**](https://git.meli-email.org/meli/meli/commit/36e29cb6fd00c798ad83e3064e0ff78c8153dced) `Add configurable mailbox sort order`
- [**`81184b18`**](https://git.meli-email.org/meli/meli/commit/81184b182c5f5d65614653b817981fddc6a84ffa) `Add extra_identities configuration flag`
- [**`b716e438`**](https://git.meli-email.org/meli/meli/commit/b716e4383ea3163cabe760cd5512b7d70b218915) `Add collapse option for mailboxes in sidebar menu`
- [**`3d92b410`**](https://git.meli-email.org/meli/meli/commit/3d92b41075fc16214675cf141acd9c89fb6f5c49) `Add cli-docs feature to the default set`
- [**`104352e5`**](https://git.meli-email.org/meli/meli/commit/104352e5950598f4a659bd593d587910af8adc12) `Add table UI widget`
- [**`7d9cabb0`**](https://git.meli-email.org/meli/meli/commit/7d9cabb023b510e6175fd6b2523f0414a6da1f3f) `Add mailbox manager tab`
- [**`660bacb9`**](https://git.meli-email.org/meli/meli/commit/660bacb9262dac7457bd8c421cc70343a0db3cd5) `Add mailto command to open composer with initial values from mailto template`
- [**`3adf72ae`**](https://git.meli-email.org/meli/meli/commit/3adf72aed0772fea39fbd6cbaec680fb2995e92d) `Add support for utf-7 encoding`
- [**`d9c07def`**](https://git.meli-email.org/meli/meli/commit/d9c07def0f5db655aa11c5981d1419a336c3d91a) `Add command to select charset encoding for email`
- [**`8c671935`**](https://git.meli-email.org/meli/meli/commit/8c671935f9ad5bd2894c0ecdaec9c2f378e461ca) `Add compose (pre-submission) hooks for validation/linting`
- [**`96537e48`**](https://git.meli-email.org/meli/meli/commit/96537e48c5f5c8d54076ec5db76e94a499cbe1e6) `Add {Timer,Component}Id wrapper types over Uuid`
- [**`b5f205b7`**](https://git.meli-email.org/meli/meli/commit/b5f205b77b8911a1fb6019767bb026e5f4a7f79e) `Add availability to use server_password_command in the nntp backend like in the IMAP backend`
- [**`a5770c89`**](https://git.meli-email.org/meli/meli/commit/a5770c89f46b908d17d6eb4573c8337a952f99a8) `Add Woodpecker-CI check pipeline`
- [**`d4e605c0`**](https://git.meli-email.org/meli/meli/commit/d4e605c098ba13b8bc2d9f14d07ea45da38e9a2f) `Add tagref source code annotations`
- [**`cf9a04a5`**](https://git.meli-email.org/meli/meli/commit/cf9a04a5910c9d82e1acb10a2f4d40c2af0335ed) `Add metadata to Jobs, and add JobManager tab`
- [**`bb7e119a`**](https://git.meli-email.org/meli/meli/commit/bb7e119ade131e8fe1bcac39b616741af817808c) `Add gitea CI workflows`
- [**`1c79786e`**](https://git.meli-email.org/meli/meli/commit/1c79786ea210e53ee7d566455d83d74fe4699d28) `Add scripts/make_html_manual_page.py`
- [**`65e82d88`**](https://git.meli-email.org/meli/meli/commit/65e82d8896500e8ef586656e3bde4bc102b84aba) `Add meli/README.md symbolic link`

### Fixed

- [**`ce2068d3`**](https://git.meli-email.org/meli/meli/commit/ce2068d36bb5d8ad0bb8f886bc19cb4aab75c4e8) `Fix background watch using JSON paths incorrectly`
- [**`e9aaa7b0`**](https://git.meli-email.org/meli/meli/commit/e9aaa7b067903040acd7f3d7c685de94b3b98450) `Use *const c_char instead of *const i8 for portability`
- [**`aa3524dd`**](https://git.meli-email.org/meli/meli/commit/aa3524dd305f2cf293eaaf7120b812478255f79c) `Fix tag not being removed in set_flags()`
- [**`daa900ec`**](https://git.meli-email.org/meli/meli/commit/daa900ec9a566460833c020feba10933c0248162) `Fix embed terminal in macos`
- [**`7fca5f01`**](https://git.meli-email.org/meli/meli/commit/7fca5f01ef53069958403dd794ee0e5c310f4e45) `Fix jmap build with isahc 1.7.2`
- [**`ed3dbc85`**](https://git.meli-email.org/meli/meli/commit/ed3dbc85861ab61fee56077c7ba94306b0a96dc4) `Fix crashes when listing is empty`
- [**`824f614a`**](https://git.meli-email.org/meli/meli/commit/824f614a69e55a25d67832593cb8aadb9671e306) `Fix HtmlView not being redrawn when parent is dirty`
- [**`97ff3e78`**](https://git.meli-email.org/meli/meli/commit/97ff3e787fbfb5ff50e3ba787f067829509f7cd2) `Only add toml files to the themes`
- [**`9cb66ef8`**](https://git.meli-email.org/meli/meli/commit/9cb66ef818f6598eb779f931e201a8d38e86a484) `Fix all clippy warnings in meli crate`
- [**`0c0bee44`**](https://git.meli-email.org/meli/meli/commit/0c0bee4482d4fbfa675b97ca30405fdc77655936) `Add missing .PHONY targets, fix missing tab indentation`
- [**`a73885ac`**](https://git.meli-email.org/meli/meli/commit/a73885acb14cd94d4a6a54ebd5b39a001d7e21e1) `Improve embed terminal`
- [**`da9c80cc`**](https://git.meli-email.org/meli/meli/commit/da9c80ccfd7aa87842c2c3c089ba2b784a583ab6) `Enhance SubjectPrefix with strip_prefixes_from_list() method`
- [**`aa99b0d7`**](https://git.meli-email.org/meli/meli/commit/aa99b0d787463be4267913b801117bd4d2ea5003) `Implement configurable subject prefix stripping when replying`
- [**`cbe593cf`**](https://git.meli-email.org/meli/meli/commit/cbe593cf31308dcf549d7880eea2d82e5024dd73) `Add configurable header preample suffix and prefix for editing`
- [**`2de69d17`**](https://git.meli-email.org/meli/meli/commit/2de69d17f14e79ce2a35564d278b5e895d16a48f) `Fix erroneous placement of newlnes for wrap_header_preamble suffix`
- [**`94bd84b4`**](https://git.meli-email.org/meli/meli/commit/94bd84b45d53b0e0fae52198fbdc05179b87cccc) `Fix clippy lints for meli crate`
- [**`b138d9bc`**](https://git.meli-email.org/meli/meli/commit/b138d9bc6166b763febf035b50109d810e3c18c9) `Fix some clippy lints`
- [**`c6bdda03`**](https://git.meli-email.org/meli/meli/commit/c6bdda03cf451ab52a3d414cad1344bb32c82879) `Fix notmuch error shown on any missing backend`
- [**`16646976`**](https://git.meli-email.org/meli/meli/commit/16646976d75284665c1fa0d7b7e3e3cde3531d66) `Fix reply subject prefixes stripping original prefix`
- [**`88a1f0d4`**](https://git.meli-email.org/meli/meli/commit/88a1f0d4bc17b60f8f23ea71f33a81aee78f8769) `Fix FETCH response parsing bug`
- [**`59b95f83`**](https://git.meli-email.org/meli/meli/commit/59b95f83d2b388b30a3a855f68bf5952355597d7) `Fix docs`
- [**`282af86e`**](https://git.meli-email.org/meli/meli/commit/282af86e83807772f042b115af24ffe2e0575b9e) `Fix NAME sections manual pages for correct whatis(1) parsing`
- [**`bd22f986`**](https://git.meli-email.org/meli/meli/commit/bd22f986f0c06f6dae535733d484aa89f610ed46) `Fix clippy lints`
- [**`5ba7b2cd`**](https://git.meli-email.org/meli/meli/commit/5ba7b2cd7bb07abe8faafe5e45db6145b3f90bc9) `Fix clippy lints for meli binary`
- [**`7924aa8b`**](https://git.meli-email.org/meli/meli/commit/7924aa8bfe8f0fbcd557bb8bb3a9d3ebeab2220a) `Fix compilation`
- [**`b9030a68`**](https://git.meli-email.org/meli/meli/commit/b9030a684c0ad64951a388e49d5825c12b483fb4) `Fix selection not appearing immediately and invalid motions`
- [**`4f45b109`**](https://git.meli-email.org/meli/meli/commit/4f45b109745ebc29febc452b9bcb0cd88f131ffc) `Fix tag updates not showing up right away`
- [**`abc56eae`**](https://git.meli-email.org/meli/meli/commit/abc56eae431153d2e48f8b1eb3e0d2a140b600d8) `Fix SEEN flag update hiding mail view momentarily`
- [**`40c6647d`**](https://git.meli-email.org/meli/meli/commit/40c6647db83c5137b79c9bec233972a8a78aeb76) `Fix multipart/related with main text/html part not displayed correctly`
- [**`11140b4a`**](https://git.meli-email.org/meli/meli/commit/11140b4a76419a6f8c83db38823e83aeac8fbb98) `Fix test output`
- [**`3a10953f`**](https://git.meli-email.org/meli/meli/commit/3a10953f05ea4944a8a20c2c5d647d5862dca907) `Update fix-prefix-for-debian.patch`
- [**`939dc15e`**](https://git.meli-email.org/meli/meli/commit/939dc15e289e06a0fad72e44f9e91133892a4ec0) `Fix melib tests`
- [**`39d9c2af`**](https://git.meli-email.org/meli/meli/commit/39d9c2af3b7daf39c6aa7eab5f2d95f1b9c3a562) `Fix test smtp server logic`
- [**`34bb532e`**](https://git.meli-email.org/meli/meli/commit/34bb532e8d91c5f35bdc058821da63ac543ecfa6) `Mention w3m dependency`
- [**`b1a71887`**](https://git.meli-email.org/meli/meli/commit/b1a71887710153f0f98b25b2f224fbe37f7a6889) `Clippy fixes`
- [**`1f8ac228`**](https://git.meli-email.org/meli/meli/commit/1f8ac2287b960e0ed5c44dadbf68b924f035d321) `Fix ftplugin location and add example mail.vim file`
- [**`1eea8bab`**](https://git.meli-email.org/meli/meli/commit/1eea8bab77cc20fb911f13aa16322a217b36b06b) `Fix test_imap_fetch_response.`
- [**`daf42fd4`**](https://git.meli-email.org/meli/meli/commit/daf42fd456bad5ddf65ac515c2fb277896d1fea3) `Fix build error with quote 1.0.28`
- [**`6388bea9`**](https://git.meli-email.org/meli/meli/commit/6388bea9a063f776398ffc503fdb0789ce9af9f1) `Fix &[u8] index in HeaderMap`
- [**`c5ecacea`**](https://git.meli-email.org/meli/meli/commit/c5ecaceae1ab50a1c337f5cab9e97c0b061cb2d5) `Fix some search criteria in Query type`
- [**`27a4dcb9`**](https://git.meli-email.org/meli/meli/commit/27a4dcb916e0bed723490df9d82bfd7c83f10a83) `Fix some rustdoc lints`
- [**`fdc0861a`**](https://git.meli-email.org/meli/meli/commit/fdc0861ac0ac725e6e5031d120bd4682752c0267) `Fix expanded_hash argument off by one error`
- [**`0c0a678c`**](https://git.meli-email.org/meli/meli/commit/0c0a678cffec73940065923bb3837deb85075f9f) `Fix overlay widgets not being reaped after Unrealize event`
- [**`65179d48`**](https://git.meli-email.org/meli/meli/commit/65179d4816a39b0c92e9c6a981b491c60313634f) `Fix cursor/widget focus scrolling logic`
- [**`e64923ee`**](https://git.meli-email.org/meli/meli/commit/e64923eeaaf1fdf0ee485cceff0c57b2d43f165a) `Fix debug_assert condition`
- [**`5f29faa6`**](https://git.meli-email.org/meli/meli/commit/5f29faa640ebe7b14e76e56227a482207b8d952e) `Clippy lint fixes`
- [**`0b258a1f`**](https://git.meli-email.org/meli/meli/commit/0b258a1f058fa08b143a8e573883a4abe89dc7e1) `Clippy lint fixes`
- [**`ba7f5dce`**](https://git.meli-email.org/meli/meli/commit/ba7f5dce1c37c04768aa060b35f3803e6db3840e) `Fix display of threaded conversations tree structure`
- [**`1dc1d868`**](https://git.meli-email.org/meli/meli/commit/1dc1d86848eb6d187120bcaa00296f2b4e2025ca) `Fix infinite loop bug`
- [**`e8e49e74`**](https://git.meli-email.org/meli/meli/commit/e8e49e741b0f888d44da69f52aa3fff2e03e7ced) `Fix wrong per message offset`
- [**`e3dfeaad`**](https://git.meli-email.org/meli/meli/commit/e3dfeaad7e4f838af5fb2e6e398d3e1aa37fe511) `Fix compilation error when building without gpgme feature`
- [**`7998e1e7`**](https://git.meli-email.org/meli/meli/commit/7998e1e77ef057bab28434edefb79d7be6a4de33) `Add missing LC libc constants for openbsd target_os`
- [**`b5657201`**](https://git.meli-email.org/meli/meli/commit/b5657201db4828c6e61c52e7ce338ac1a6e6f9fc) `Fix doctest compilation errors`
- [**`c2ed3e28`**](https://git.meli-email.org/meli/meli/commit/c2ed3e283f6729ac7e112d00ae54dd99a2ada5e6) `Fix Source::* view showing only envelope body`
- [**`d93ee413`**](https://git.meli-email.org/meli/meli/commit/d93ee413a766f35a4ef88d9fc3ace9cf37d28dd1) `Add timestamp_to_string_utc`
- [**`6086a378`**](https://git.meli-email.org/meli/meli/commit/6086a3789d4d01818322dab1f1a9eb4c1f6a2b25) `Fix libgpgme segfault error and re-enable gpg`
- [**`ab418c1d`**](https://git.meli-email.org/meli/meli/commit/ab418c1d39d02840bc5c61996c1a5416e2f35464) `Refresh documentation, fix encryption/signing`
- [**`0219dc87`**](https://git.meli-email.org/meli/meli/commit/0219dc870798a16fd4d9f546d14c115f9e2c6bd8) `Respect max_objects_in_get when fetching email`
- [**`6280bc75`**](https://git.meli-email.org/meli/meli/commit/6280bc75e550332a73c1a51dd46475cd54cc0a34) `Fix blob download URL formatting`
- [**`2df73547`**](https://git.meli-email.org/meli/meli/commit/2df73547515fd3464e1fc2b88aa67462f583a8ec) `Fix overflow substracts`
- [**`8e698cab`**](https://git.meli-email.org/meli/meli/commit/8e698cabcfe58ddd566133ba2c33249c23180a74) `Fix unreachable-pub and disjoint-capture lint errors`
- [**`40d4ecef`**](https://git.meli-email.org/meli/meli/commit/40d4ecefa013caaa13af493233c693fb495360ca) `Accept invalid (non-ascii) address comment text`
- [**`4e654d2d`**](https://git.meli-email.org/meli/meli/commit/4e654d2d02044be7340b63f1250d37b2ca57b221) `Limit LIST ACTIVE command length to 512 octets`
- [**`84081f4e`**](https://git.meli-email.org/meli/meli/commit/84081f4ed7570dd8bcc23d90b9c4cbff55620636) `Minor style fix`
- [**`97d36868`**](https://git.meli-email.org/meli/meli/commit/97d3686815c011bb8f1d4e448f12b2294693730d) `Use Happy Eyeballs algorithm Ꙭ`
- [**`96f0b3e6`**](https://git.meli-email.org/meli/meli/commit/96f0b3e6b484c9cbb7eaddcaad2b59811b733545) `Fix shortcut section order`
- [**`64982b4c`**](https://git.meli-email.org/meli/meli/commit/64982b4cab0b0c2d396cb5dcf7add6f268fd4551) `Fix page{up,down} event bubbling up`
- [**`8551e1ba`**](https://git.meli-email.org/meli/meli/commit/8551e1ba0b4fa6d9587bbb249f11e9b80d24e4d3) `Fix new 1.72 default clippy lints`

### Changed

- [**`8563bccd`**](https://git.meli-email.org/meli/meli/commit/8563bccd1b6d48dc06dd521f77228c3cbecf7613) `Don't cache CellBuffer, only row info`
- [**`0f6f3e30`**](https://git.meli-email.org/meli/meli/commit/0f6f3e30c67f209e0b5e03d2dd2e1e48180d9855) `Add IMAP config in config parse test`
- [**`ce269c64`**](https://git.meli-email.org/meli/meli/commit/ce269c64e16db344f0e65461e56dbced2f1a4d64) `Don't fail on server_password_command`
- [**`9dc4d405`**](https://git.meli-email.org/meli/meli/commit/9dc4d4055cb2f854e835748315677bf4a2db2012) `Add focus_{left,right} shortcuts to switch focus`
- [**`4b96bd59`**](https://git.meli-email.org/meli/meli/commit/4b96bd591f18bf7c8a3c922d469b81072d1782a2) `Add ColorCache constructor to deduplicate code`
- [**`c06c3f58`**](https://git.meli-email.org/meli/meli/commit/c06c3f589315f017a412f31d80559a5a734d7b89) `Draw gap between list and mail view`
- [**`c9d26bb4`**](https://git.meli-email.org/meli/meli/commit/c9d26bb4158e2f423c795f82bcb2c91a0f0c46ec) `Add configurable custom hooks with shell commands`
- [**`02e86d1f`**](https://git.meli-email.org/meli/meli/commit/02e86d1fade9faefc14b890e3cec8ed2255bb839) `Check for subject overflow on draw`
- [**`8cab9d9d`**](https://git.meli-email.org/meli/meli/commit/8cab9d9da8710257f2b62832bfac802c2a35b368) `Add option to hide consecutive identical From values inside a thread`
- [**`363f4930`**](https://git.meli-email.org/meli/meli/commit/363f4930994d1d2e88220878b3848f176b8c5f97) `Add {previous,next}_entry shortcuts to quickly open other mail entries`
- [**`342df091`**](https://git.meli-email.org/meli/meli/commit/342df091a076bce1f8477dabbad193312d8cdd67) `Don't set all thread to seen when opening a thread entry`
- [**`74e15316`**](https://git.meli-email.org/meli/meli/commit/74e15316dbbf67254023e619924e522f80e77cb9) `Open message/rfc822 attachments in subview instead of new tab`
- [**`369c1dbd`**](https://git.meli-email.org/meli/meli/commit/369c1dbdac9842746270a3d3c5bf7ed2205cb644) `Show open command in status bar`
- [**`519257b0`**](https://git.meli-email.org/meli/meli/commit/519257b08f7029fe71efd2f61ab3a29a4b43b862) `Add relative_menu_indices setting for menubar`
- [**`8abc9358`**](https://git.meli-email.org/meli/meli/commit/8abc9358a70465b12a11168be1718ab06479d6e2) `Add newline after Version: 1 header`
- [**`561ba9c8`**](https://git.meli-email.org/meli/meli/commit/561ba9c87b57e1012ad89bde08506a2beacb7fff) `Add relative_list_indices setting for thread listing`
- [**`52874f9a`**](https://git.meli-email.org/meli/meli/commit/52874f9a97a4799fcff2e14c43cafe9692f21cb6) `Cancel previous jobs on MailView drop/update`
- [**`9037f084`**](https://git.meli-email.org/meli/meli/commit/9037f08495894c15a7817594ba91e0d5561c6e69) `Replace hardcoded Key::{Home,End} values with shortcut values`
- [**`31aa9ad2`**](https://git.meli-email.org/meli/meli/commit/31aa9ad29e33f285314d0d320a02f00071f61282) `Autogen mbox filename when exporting mail to directories`

### Refactoring

- [**`330a2b20`**](https://git.meli-email.org/meli/meli/commit/330a2b20ed492f6b6ea86c196d43d67430487faa) `Flush stdout in Ask() after printing`
- [**`340d6451`**](https://git.meli-email.org/meli/meli/commit/340d6451a330861af09fd02231c17ba4168d9654) `Add config setting for sidebar ratio`
- [**`d0de0485`**](https://git.meli-email.org/meli/meli/commit/d0de04854ec4770b54e4d8303a9b8ab9eb5d68b0) `Add {in,de}crease_sidebar shortcuts`
- [**`f5dc25ae`**](https://git.meli-email.org/meli/meli/commit/f5dc25ae0d5b8d6fb15a534fa49557385d6894d0) `Check that all conf flags are recognized in validation`
- [**`d3e62e3d`**](https://git.meli-email.org/meli/meli/commit/d3e62e3d74bdc55872bbdf92c01d18aa00b0affd) `Use conf shortcuts for scroll {up, down}`
- [**`23c23556`**](https://git.meli-email.org/meli/meli/commit/23c2355662d589c091dd3c86c8d91c7988eb941c) `Fill and align shortcut table columns`
- [**`5823178c`**](https://git.meli-email.org/meli/meli/commit/5823178cc26f66ba902a901522f0506b4348b22e) `Add test that looks in source code for invalid theme key references`
- [**`9205f3b8`**](https://git.meli-email.org/meli/meli/commit/9205f3b8afe28ef3a68959d590ed967946a5d622) `Handle a per account mail order parameter`
- [**`d921b3c3`**](https://git.meli-email.org/meli/meli/commit/d921b3c3209ff7fe865b5a3b90e20098b3ff211f) `Use mail sorting parameters from config`
- [**`f4e0970d`**](https://git.meli-email.org/meli/meli/commit/f4e0970d46e3ec73d684e2ddcc5011f61e87314d) `Add ability to kill embed process`
- [**`bde87af3`**](https://git.meli-email.org/meli/meli/commit/bde87af3877d4a0b071e331c93a07e0acf51bf7a) `Refactor filter() method in Listing trait`
- [**`a42a6ca8`**](https://git.meli-email.org/meli/meli/commit/a42a6ca868e4590a8b93560737173e80993ecaec) `Show notifications in terminal if no alternative`
- [**`eb5949dc`**](https://git.meli-email.org/meli/meli/commit/eb5949dc9bbcf05f86c58b3c93d1066204313e2a) `Switch summary<->details identifiers`
- [**`8c7b001a`**](https://git.meli-email.org/meli/meli/commit/8c7b001aa5d4cb6bbaf438f3f47cd91cc2fd6833) `Add thread_subject_pack command to pack different inner thread subjects in entry title`
- [**`388d4e35`**](https://git.meli-email.org/meli/meli/commit/388d4e35d65f8f770526c4c5f44767c55eda23f8) `Add in-progress messages while connecting in IMAP`
- [**`787c64c2`**](https://git.meli-email.org/meli/meli/commit/787c64c2da8af5cc0dafcb92c1d3bea6b54f3659) `Remove expect()s from create_config_file()`
- [**`b87d54ea`**](https://git.meli-email.org/meli/meli/commit/b87d54ea3f3f077b6330e798263be6a3d33b3b9c) `Impl Into<BTreeSet<EnvelopeHash>> for EnvelopeHashBatch`
- [**`e450ad0f`**](https://git.meli-email.org/meli/meli/commit/e450ad0f9cbc2d215a8f03d2d39260abe19fb5af) `Remove unused struct`
- [**`c54a31f7`**](https://git.meli-email.org/meli/meli/commit/c54a31f7cca728eec87f7cd670a4baec37dc919a) `Break line for error messages`
- [**`7935e49a`**](https://git.meli-email.org/meli/meli/commit/7935e49a00190cc7f2057abe353739c8dad4f74d) `Check properly if mailbox request is an error`
- [**`117d7fbe`**](https://git.meli-email.org/meli/meli/commit/117d7fbe046fe23c400a925ccba7317d8a1d3f08) `Make private fields public`
- [**`ffb12c6d`**](https://git.meli-email.org/meli/meli/commit/ffb12c6d1ae9a774de22a25d38bc6714a435c7ad) `Make all public struct fields public`
- [**`46a038dc`**](https://git.meli-email.org/meli/meli/commit/46a038dc68093b28b69c3af38de4dd09431efae2) `Remove interactive messages when #[cfg(test)]`
- [**`803d3414`**](https://git.meli-email.org/meli/meli/commit/803d3414fd73743ff5bfc0fefe5e3d76d88e58cb) `Implement some rfc5804 commands`
- [**`b776409d`**](https://git.meli-email.org/meli/meli/commit/b776409d6c9caec3732bada9e25637c2676af3b8) `Add thread, env hash index fields`
- [**`cc439b23`**](https://git.meli-email.org/meli/meli/commit/cc439b239ae27ae84fbcf50fbd82ec591c147c94) `Add RowsState struct`
- [**`db227dea`**](https://git.meli-email.org/meli/meli/commit/db227dea34caa747e136500356fddf95a91002e6) `Add error messages if mandoc,man binaries are missing`
- [**`ee9d458b`**](https://git.meli-email.org/meli/meli/commit/ee9d458b05ffa0214a4526daf1423916830526bc) `Implement mailbox {un,}sub actions`
- [**`7af89359`**](https://git.meli-email.org/meli/meli/commit/7af893597f5a3f3261bfff47dae0723bf1b17e53) `Replace use of Self::DESCRIPTION with Shortcuts struct consts`
- [**`eaecc5ea`**](https://git.meli-email.org/meli/meli/commit/eaecc5ea12f4a5ebe309d5654509c0771bbdc2f1) `Remove hardcoded major .so version for non linux/macos target_os`
- [**`f63ce388`**](https://git.meli-email.org/meli/meli/commit/f63ce388f7774ea015fdaa2362202c33f3ddacd4) `Move ManageMailboxes to Tab Actions`
- [**`3c847ad2`**](https://git.meli-email.org/meli/meli/commit/3c847ad26afcc4a4cdcfbdbf70f35be57d0da1ab) `Add beginning of sieve parser`
- [**`5443b7e8`**](https://git.meli-email.org/meli/meli/commit/5443b7e8f300a0084abde7354360ecbe909178bb) `Remove literal_map() parse combinator`
- [**`12cb717b`**](https://git.meli-email.org/meli/meli/commit/12cb717bda186b0ebdda18e2215e30b1426fb08a) `Add server_password_command to jmap`
- [**`428f752b`**](https://git.meli-email.org/meli/meli/commit/428f752b20cdb1c8ab01e7f3119001cfafca8ef1) `Remove obsolete crate::components::mail::get_display_name()`
- [**`91557c2c`**](https://git.meli-email.org/meli/meli/commit/91557c2c4366b481e80943e94f661c8b47150571) `Prevent list blank when refreshing account`
- [**`d332e457`**](https://git.meli-email.org/meli/meli/commit/d332e4578d69c4371418fb2bb3c0d75e1960e01f) `Add proper Display impl for HeaderName`
- [**`f537c249`**](https://git.meli-email.org/meli/meli/commit/f537c24909d13a53a95b43e265e4cb4c013334ac) `Move text field to its own module`
- [**`d33f9d54`**](https://git.meli-email.org/meli/meli/commit/d33f9d54c708699386a3f32e4056ccab6c68528b) `Remove unreachable!() in Key::serialize`
- [**`330887c4`**](https://git.meli-email.org/meli/meli/commit/330887c4f5bad5357508b9fa6f723e45ab307d2a) `Introduce imap-codec.`
- [**`4da53669`**](https://git.meli-email.org/meli/meli/commit/4da5366959145e166c40297abfdf1876e5addc50) `Remove bincode dep, use serde_json for sqlite3 values`
- [**`155fb41b`**](https://git.meli-email.org/meli/meli/commit/155fb41b93708ef8793250f9dea611bc317a86d5) `Remove unused Component::set_id method`
- [**`575509f1`**](https://git.meli-email.org/meli/meli/commit/575509f1edc756ad218bb76cf74460d83009c851) `Move mail view to listing parent component`
- [**`6858ee1f`**](https://git.meli-email.org/meli/meli/commit/6858ee1fab3bcddbda7335f49c30f36153e8d4b7) `Move subcommand handling to its own module`
- [**`b0e867eb`**](https://git.meli-email.org/meli/meli/commit/b0e867eb68dc3dba96de79f7481989187fa12df4) `Move src to meli/src`
- [**`48a10f72`**](https://git.meli-email.org/meli/meli/commit/48a10f724171bfae702b7b40438189adbbe75079) `Remove unused BackendOp::fetch_flags() method`
- [**`073d43b9`**](https://git.meli-email.org/meli/meli/commit/073d43b9b869fc9d46c5195c31ad6e7806cf486c) `Move data files to data subdir`
- [**`1e084c1d`**](https://git.meli-email.org/meli/meli/commit/1e084c1d854ed7efb2254f9e8d52ac13d8badffa) `Move backends out of the backends module`
- [**`a5446975`**](https://git.meli-email.org/meli/meli/commit/a5446975c2423654dea9551474a880e94ebdc006) `Move braille and screen to their own module files`
- [**`005bf388`**](https://git.meli-email.org/meli/meli/commit/005bf3881ec59d53e4f16473fb3b1857487dae23) `Move components/utilities -> utilities`
- [**`64ab65dd`**](https://git.meli-email.org/meli/meli/commit/64ab65ddffe3341bca775acb2289ee00e771fdb0) `Move components/contacts -> contacts`
- [**`7c9a4b4b`**](https://git.meli-email.org/meli/meli/commit/7c9a4b4b7c366c967a3378098d210124712fd293) `Move components/mail -> mail`
- [**`df638cce`**](https://git.meli-email.org/meli/meli/commit/df638cceec6016760037b650a77143a07cd1e738) `Remove stale failing doc code example`
- [**`da8e8104`**](https://git.meli-email.org/meli/meli/commit/da8e81044833975cadb08db836795a389c142e9c) `Remove leftover debug prints`
- [**`a1e70061`**](https://git.meli-email.org/meli/meli/commit/a1e7006186474f55cf4a14f53dbd32bdf8ca5993) `Move Sort{Order,Field} to utils mod`
- [**`66c21ab1`**](https://git.meli-email.org/meli/meli/commit/66c21ab1734bfbf4e604da505f6b6109008fd7c2) `Move StandardHeader to its own module`
- [**`946309c6`**](https://git.meli-email.org/meli/meli/commit/946309c6f3bbc59b53dc2b05732b40f3d445fd9f) `Do some small parser refactoring`
- [**`b95f7783`**](https://git.meli-email.org/meli/meli/commit/b95f778335bebd480f69fe66fabec4f8a6e2b587) `Move JmapSession to its own module`

### Documentation

- [**`a866b294`**](https://git.meli-email.org/meli/meli/commit/a866b29499b44032545df4941b6cfec4ee2db8bb) `Update valid shortcut entries from src/conf/shortcuts.rs`
- [**`f76f4ea3`**](https://git.meli-email.org/meli/meli/commit/f76f4ea3f7416a4a641d5891f19927aa354a3247) `Add meli.7, a general tutorial document`
- [**`5fa4b626`**](https://git.meli-email.org/meli/meli/commit/5fa4b6260c60409579fe964970719f9ab60482cc) `Add more screenshots`
- [**`7c711542`**](https://git.meli-email.org/meli/meli/commit/7c7115427dd5f6320a4305df3dc88a8567829720) `Complete guide document`
- [**`30cc5d3d`**](https://git.meli-email.org/meli/meli/commit/30cc5d3d0220452630780c3238f393b9e1f2b93a) `Add edit-config in manpages`
- [**`24103f33`**](https://git.meli-email.org/meli/meli/commit/24103f3310ca533791bdd07643fdb23a10c6031d) `Add external-tools.md document`
- [**`b6c93e49`**](https://git.meli-email.org/meli/meli/commit/b6c93e49f2af3001b206a288edea02c58e14aa5b) `Add use_tls option in IMAP connection settings`
- [**`34a54d3c`**](https://git.meli-email.org/meli/meli/commit/34a54d3c05efc3b56154179111c3e39e0f3fd8b1) `Add some TODO`([#222](https://git.meli-email.org/meli/meli/issues/222))`s.`

### Packaging

- [**`671ce9f6`**](https://git.meli-email.org/meli/meli/commit/671ce9f694a8e941826472caad8051998540bb1f) `Add missing build dependencies`

### Miscellaneous Tasks

- [**`25805229`**](https://git.meli-email.org/meli/meli/commit/2580522931fb29442598ac8932a13eaeb577bace) `Log vcard parsing failures`
- [**`5f003a31`**](https://git.meli-email.org/meli/meli/commit/5f003a31be95a3877d1006f8a22e424a1183163d) `Parse vCards with just LF instead of CRLF line endings`
- [**`d8e9a005`**](https://git.meli-email.org/meli/meli/commit/d8e9a00563c023abb0ff75aaa4ba3fa92626c5ce) `Add quoted REFERENCES field in parsing of responses`
- [**`81d12656`**](https://git.meli-email.org/meli/meli/commit/81d1265601c299dee6405f3f9b4e81f89d3cfe29) `Escape IMAP passwords properly`
- [**`0d8bedd2`**](https://git.meli-email.org/meli/meli/commit/0d8bedd2d5d3eb8eee831e75d1e14d45beefb847) `Make is_online() await for connection`
- [**`d4b690d5`**](https://git.meli-email.org/meli/meli/commit/d4b690d5d3a7f6a6b57afd7a6177db0db20a9c94) `Send password as byte literal on LOGIN`
- [**`2eb22a29`**](https://git.meli-email.org/meli/meli/commit/2eb22a290abb3f37bc77c3bc2771edfb60a1c314) `Stop hardcoding certain component colors`
- [**`2c23ca34`**](https://git.meli-email.org/meli/meli/commit/2c23ca34cdee769a0f78a0b0ef934e5f20dd9567) `Update most Cargo dependencies`
- [**`721891c2`**](https://git.meli-email.org/meli/meli/commit/721891c2955e9f5e223949bde2dd43604cec8390) `Update nom dependency`
- [**`4fdc90b3`**](https://git.meli-email.org/meli/meli/commit/4fdc90b31ea56c046dfe5bf9bee0a118f9c03db1) `Use open instead of xdg-open in macos`
- [**`9558b2ae`**](https://git.meli-email.org/meli/meli/commit/9558b2ae921aa35076f58d68b5898334a2797685) `Parse Cp1253 as windows1253 encoding`
- [**`6a843d49`**](https://git.meli-email.org/meli/meli/commit/6a843d49830f8c70f510c4232ea63eb204d35319) `Export list_mail_in_maildir_fs() function`
- [**`d6355a30`**](https://git.meli-email.org/meli/meli/commit/d6355a3043ec0b4b2a3e1c3fbb0ed66d2e87e7f4) `Impl Debug for ParsingError`
- [**`dc5afa13`**](https://git.meli-email.org/meli/meli/commit/dc5afa13dbea4da042c35e12291c5b5a2846c3ff) `Use osascript/applescript for notifications on macos`
- [**`e6d6e1f5`**](https://git.meli-email.org/meli/meli/commit/e6d6e1f588db9793e822cdbb1ce2edb2959170c6) `Don't unwrap if pseudoterminal creation fails`
- [**`ca84906d`**](https://git.meli-email.org/meli/meli/commit/ca84906d7ddb1351643998efaa56086e3ba9cf8e) `Escape all quotes in applescript on macos`
- [**`4a79b202`**](https://git.meli-email.org/meli/meli/commit/4a79b2021d2fb3edd046197b44b702bdb468fc5e) `Update dependency versions`
- [**`e29041f7`**](https://git.meli-email.org/meli/meli/commit/e29041f73354c59ef95916edd75e6ca7876e3c3a) `Rename src/bin.rs to src/main.rs`
- [**`7650805c`**](https://git.meli-email.org/meli/meli/commit/7650805c60cec2fe09cd2a59cb665731f5cca140) `Bring stripped binary size down to 7MiB`
- [**`ca488968`**](https://git.meli-email.org/meli/meli/commit/ca48896865778df2c79bc1d13f03b5f56136304c) `Add strip option to profile.release`
- [**`10497952`**](https://git.meli-email.org/meli/meli/commit/10497952f718b49f3a247741a64361f855b2d4f7) `Wrap stdout in BufWriter`
- [**`29042aba`**](https://git.meli-email.org/meli/meli/commit/29042aba593210f3be73010908d5092951b3b1a1) `Add mbox date format parse`
- [**`480000eb`**](https://git.meli-email.org/meli/meli/commit/480000ebbb67a80181fd27762ca649acf13df0f3) `Show error if account directory does not contain ".notmuch" subdirectory`
- [**`a484b397`**](https://git.meli-email.org/meli/meli/commit/a484b397c68fd126c17073ac9c9f02432c413341) `Show informative error messages if libloading fails`
- [**`4a20fc42`**](https://git.meli-email.org/meli/meli/commit/4a20fc42e1f5cad325d5aa439d1baab210aceed8) `Update CHANGELOG.md`
- [**`a72c96a2`**](https://git.meli-email.org/meli/meli/commit/a72c96a26afe9e54a0fcadb8c43448f1fdc09ce9) `Add 8BITMIME support to smtp client`
- [**`3c0f5d82`**](https://git.meli-email.org/meli/meli/commit/3c0f5d8274d8039b1a2c928f99194835bca7b83a) `Add BINARYMIME support to smtp client`
- [**`36883692`**](https://git.meli-email.org/meli/meli/commit/36883692782ed2355a0ec12ccf9f82aa2edcc8c1) `Add smtp test`
- [**`9cbbf71e`**](https://git.meli-email.org/meli/meli/commit/9cbbf71e0f8f9115e9e043982f20045cfc550eb7) `Add DecodeOptions struct for decoding`
- [**`0df46a63`**](https://git.meli-email.org/meli/meli/commit/0df46a63ec6e30983480f0eb50c8da3f74b4f0b3) `Show error if sqlite3 search backend is set but doesn't exist`
- [**`a7a50d30`**](https://git.meli-email.org/meli/meli/commit/a7a50d3078cb7466ab341ddfc30a80c7b1f8dfdb) `Box<_> some large fields in biggest types`
- [**`d8d43a16`**](https://git.meli-email.org/meli/meli/commit/d8d43a16fef045a2116ff126e7b6e27817b526fc) `Add html_open config setting`
- [**`0ed10711`**](https://git.meli-email.org/meli/meli/commit/0ed10711ef542cc13eaaef809fa557468b3d6696) `Add new_mail_script option`
- [**`c3fdafde`**](https://git.meli-email.org/meli/meli/commit/c3fdafde3b69c0abc78a62926e0c32fc3dd602d6) `Documentation touchups`
- [**`347be543`**](https://git.meli-email.org/meli/meli/commit/347be54305c60350b055a1da3a1abfa4d33d3f22) `Add NetworkErrorKind enum`
- [**`0c08cb73`**](https://git.meli-email.org/meli/meli/commit/0c08cb737ceaa5c738712905c7d57f956d449ed0) `Mark mailboxes as subscribed on personal accounts`
- [**`129573e0`**](https://git.meli-email.org/meli/meli/commit/129573e0fd9b42ebf14c2de176e65b92bf8479bd) `Rename root_path to root_mailbox`
- [**`7e09b180`**](https://git.meli-email.org/meli/meli/commit/7e09b1807ffa9bae54da35b02c83b5aaee455819) `Replace _Ref deref unwraps with expect()`
- [**`55ed9624`**](https://git.meli-email.org/meli/meli/commit/55ed962425ba25d2317946705ff6861a77eb770f) `Use server_url instead of server_hostname + server_port in config`
- [**`0ef4dde9`**](https://git.meli-email.org/meli/meli/commit/0ef4dde9392452f7cf7f18294f747fc6e0babb8d) `Wrap serde_json deserialize errors in human readable errors`
- [**`dd0baa82`**](https://git.meli-email.org/meli/meli/commit/dd0baa82e9789da23c8f9b06925776c7f80e2568) `Spawn user-given command strings with sh -c ".."`
- [**`3697b7d9`**](https://git.meli-email.org/meli/meli/commit/3697b7d960cc9dbe602fa84f861cea854b600b73) `Don't use LC_ category in place of LC_ masks in libc calls`
- [**`6d20abdd`**](https://git.meli-email.org/meli/meli/commit/6d20abdde7b4cec6ec1af7c097f01042ea05cfbb) `Add #[allow(deref_nullptr)] in bindgen tests`
- [**`17b42b1a`**](https://git.meli-email.org/meli/meli/commit/17b42b1a6c721fb2e369c2a300867c8db2beb959) `Add json deserialization tests`
- [**`64346dd3`**](https://git.meli-email.org/meli/meli/commit/64346dd3fe0ef40025ec6fdb01d18eb38f7e7f65) `Add map_res, quoted_slice, is_a, alt, take, take_literal`
- [**`56fc43bc`**](https://git.meli-email.org/meli/meli/commit/56fc43bcf869a867455b44d007b9d3d17422bc8d) `Add As{Ref,Mut} impls for RwRef{,Mut}`
- [**`63179841`**](https://git.meli-email.org/meli/meli/commit/631798413659a320dcd9574e0bca7b7d75cc8d6c) `Add --bin flag to meli cargo build target`
- [**`ded9adde`**](https://git.meli-email.org/meli/meli/commit/ded9adde614ac3d38045fa97a0f5144b80855fe7) `More descriptive "Unimplemented" messages`
- [**`2224a710`**](https://git.meli-email.org/meli/meli/commit/2224a7100f9bc6c44bc66117a88556003e74186e) `Reset imap cache on init error`
- [**`252d2bdf`**](https://git.meli-email.org/meli/meli/commit/252d2bdf2f12c8954f8b299000bbde6219d25335) `Replace hardcoded /bin/false with 'false'`
- [**`2427b097`**](https://git.meli-email.org/meli/meli/commit/2427b097c5c40f3212a105cb40f913c9860ae2a8) `Make tag_default background lighter on light theme`
- [**`7382e301`**](https://git.meli-email.org/meli/meli/commit/7382e30160a934ce97dd73c1be44640d5b4a4c75) `Convert EnvelopeHash from typedef to wrapper struct`
- [**`259aeb00`**](https://git.meli-email.org/meli/meli/commit/259aeb00877557ee85b5cc555d50e605b85b3109) `Convert {Account,Mailbox}Hash from typedef to wrapper struct`
- [**`5634f955`**](https://git.meli-email.org/meli/meli/commit/5634f9555315deb2d39ed8fce577a35f4d535ac1) `Rename MeliError struct to Error`
- [**`7606317f`**](https://git.meli-email.org/meli/meli/commit/7606317f24d076bdc7db873c2b15811728ed946a) `Add support for virtual mailbox hierarchy`
- [**`2878bbb8`**](https://git.meli-email.org/meli/meli/commit/2878bbb8c887275d26264bf7201a632161c4048a) `Add parser for mutt alias file`
- [**`de2f46fe`**](https://git.meli-email.org/meli/meli/commit/de2f46fe611726a445c1e06cbc35343e716aa335) `Rustfmt changes`
- [**`f9ac9b60`**](https://git.meli-email.org/meli/meli/commit/f9ac9b607a2bd01e42c81cfab3c933df28ff1676) `Temporarily disable libgpgme functions because of a bug`
- [**`256a3e25`**](https://git.meli-email.org/meli/meli/commit/256a3e252e2e4db9af9a04c7df1a52eeaf2bbfc9) `Update minimum supported rust version`
- [**`fbc1007f`**](https://git.meli-email.org/meli/meli/commit/fbc1007ff4f41bac888a1b53c156feec4f795403) `Deserialize null to empty vec for messageId`
- [**`d7ec97f0`**](https://git.meli-email.org/meli/meli/commit/d7ec97f03bc0e815e160a142f871dc764d416af1) `Small rustfmt change`
- [**`2447a2cb`**](https://git.meli-email.org/meli/meli/commit/2447a2cbfeaa8d6f7ec11a2a8a6f3be1ff2fea58) `Avoid relying on hardcoded hash values`
- [**`d679a744`**](https://git.meli-email.org/meli/meli/commit/d679a74450b35724301c81da1644bcedb1c54045) `Implement Bearer token authentication`
- [**`47e6d5d9`**](https://git.meli-email.org/meli/meli/commit/47e6d5d935a2b5124efbe847dac885b859200469) `Add edit-config CLI subcommand that opens config files on EDITOR`
- [**`3a02b6fb`**](https://git.meli-email.org/meli/meli/commit/3a02b6fb8024e6bb046fc167e7527aad1b192202) `Mention how to override w3m with html_filter`
- [**`85d4316a`**](https://git.meli-email.org/meli/meli/commit/85d4316a6a8703ac3e4923cf99ce8c4bb22bb4ae) `Replace old logging module with the log create`
- [**`1f1ea307`**](https://git.meli-email.org/meli/meli/commit/1f1ea307698a5a7f62f5ab2ea1594aef4d8f48a8) `On draw() set dirty on return`
- [**`77020e0c`**](https://git.meli-email.org/meli/meli/commit/77020e0c19873b8053321132ff5b58181c567fcd) `Update CHANGELOG.md`
- [**`682ea554`**](https://git.meli-email.org/meli/meli/commit/682ea5547e380deeb215503b39c8aa66c65b3cac) `Add .idea (CLion) to .gitignore.`
- [**`f63f6445`**](https://git.meli-email.org/meli/meli/commit/f63f6445addeccee1a6b830f1c101a043612ea4e) `Improve error message when m4 executable is missing.`
- [**`cc27639f`**](https://git.meli-email.org/meli/meli/commit/cc27639fca0dcb3a5ff9fceef8666dbbf047adaa) `Use Envelope attachments when editing and don't add already existing headers`
- [**`30866f75`**](https://git.meli-email.org/meli/meli/commit/30866f752b21802b64ce7d2e02c9962c1091c9d8) `Bypass rustfmt bug.`
- [**`235fceaf`**](https://git.meli-email.org/meli/meli/commit/235fceaf2168af50c3804cecfbf69e64ff42598c) `Add standard heeder constants in email::headers`
- [**`aebff3d3`**](https://git.meli-email.org/meli/meli/commit/aebff3d3d9864b8854aba5e7f43a61d515e8057f) `Implement mailto RFC properly`
- [**`954329d8`**](https://git.meli-email.org/meli/meli/commit/954329d848a5b3e73fca50ed1db9859118bed6dd) `Set file extensions to temp files, use open in macos`
- [**`58889bca`**](https://git.meli-email.org/meli/meli/commit/58889bcadd44d6aec2eddd17cf5ecb1e07531cbe) `Add show_extra_headers option`
- [**`23d95973`**](https://git.meli-email.org/meli/meli/commit/23d95973d4f574fe431441df97ceaef0e3e4762f) `Add search.rs module`
- [**`6bf1756d`**](https://git.meli-email.org/meli/meli/commit/6bf1756de844386ba312d15109ae29951896147b) `Implement more search criteria in Query type`
- [**`299c8e0f`**](https://git.meli-email.org/meli/meli/commit/299c8e0f993c4ac88005a5c9e708d9e214b20ac1) `Restructure pub use melib::* imports`
- [**`f8623d4b`**](https://git.meli-email.org/meli/meli/commit/f8623d4b2c386f51f1d11a23900503d8165ac9f3) `Implement more ResponseCode cases`
- [**`b92a80a2`**](https://git.meli-email.org/meli/meli/commit/b92a80a23afb96fbd63031704e4656cc8a00526c) `Resync even if UIDVALIDITY is missing from cache`
- [**`bf615e7d`**](https://git.meli-email.org/meli/meli/commit/bf615e7d933b474942d421eafc1015aeb28f8516) `Check for case when envelope has its own message id in References and In-Reply-To`
- [**`e0257c9d`**](https://git.meli-email.org/meli/meli/commit/e0257c9d8d6f234f71852a0080d443b063d5e6d7) `Run cargo-sort`
- [**`d7e6b40b`**](https://git.meli-email.org/meli/meli/commit/d7e6b40b7e1f501fdaaba54880e9c7a4b0e01288) `Auto re-index sqlite3 database if it's missing`
- [**`cd85d833`**](https://git.meli-email.org/meli/meli/commit/cd85d83324a009ea4b86ac22af395145a9e999ab) `Replace timestamp with Date value in message/rfc822 Display`
- [**`579372b4`**](https://git.meli-email.org/meli/meli/commit/579372b4a75e39c9e84010de16d7d46294bed04a) `Improve readability of Envelope.`
- [**`6c6d9f4b`**](https://git.meli-email.org/meli/meli/commit/6c6d9f4b4e0d16b5a73ae8e2a2fb2a6f124df7e6) `Improve ordering of flag_impl!s.`
- [**`8f14a237`**](https://git.meli-email.org/meli/meli/commit/8f14a2373e16b9b4af22f9388fae84235dd08123) `Put imap-codec logic under the imap_backend feature`
- [**`fd0faade`**](https://git.meli-email.org/meli/meli/commit/fd0faade066a18466e683361211bba569956bf63) `Add connection instance id string for debugging in logs`
- [**`5c9b3fb0`**](https://git.meli-email.org/meli/meli/commit/5c9b3fb0448fa3689ff33faba3dde03c49347f61) `Impl Component for Box<dyn Component>`
- [**`45bac6eb`**](https://git.meli-email.org/meli/meli/commit/45bac6eb16a5a093193d5beb4d80040ce161304a) `Tidy up use of debug!`
- [**`5699baec`**](https://git.meli-email.org/meli/meli/commit/5699baecfba9cb15aac04a6b400cfb6bc881e2c5) `Add utils::{futures, random}`
- [**`b05d9299`**](https://git.meli-email.org/meli/meli/commit/b05d92997546e438b202d336fc581c2514c63b9f) `Impl exponential backoff when retrying connection`
- [**`f5cfbd32`**](https://git.meli-email.org/meli/meli/commit/f5cfbd32e6ebbe83ad7e84d048f1fbf2e51ca605) `On set_flags, update {un,}seen sets in all mailboxes`
- [**`f0d88005`**](https://git.meli-email.org/meli/meli/commit/f0d88005fbabcd552593ba0fe785e89a3560ac1c) `Change message/rfc822 Display repr`
- [**`f98e36ce`**](https://git.meli-email.org/meli/meli/commit/f98e36cee514f643e0fe256857cf31e2e0f24080) `Replace old-style /*! module doc comments with //!`
- [**`1bcc0bbe`**](https://git.meli-email.org/meli/meli/commit/1bcc0bbece2f479950e8811261befedc0199dab9) `Add mbox parsing test`
- [**`619fbef1`**](https://git.meli-email.org/meli/meli/commit/619fbef129e249489e64a26e1d0dfbd02db2516a) `Recursively calculate update_show_subject()`
- [**`957abf4e`**](https://git.meli-email.org/meli/meli/commit/957abf4e7238ec74b2194a21533b69dd1a58c0a8) `Update cargo dependencies`
- [**`9d51b6bd`**](https://git.meli-email.org/meli/meli/commit/9d51b6bd525784bc108959519c8dd21d30a8b020) `Update imap-codec.`
- [**`7c33f899`**](https://git.meli-email.org/meli/meli/commit/7c33f8999b6a5efd911680f2b83a3ff3a682a715) `Use published imap-codec 0.10.0.`
- [**`3803d788`**](https://git.meli-email.org/meli/meli/commit/3803d788abc5157b9cc6368da7e54aced9604aec) `If auth is false checks if config has password entry`
- [**`866166eb`**](https://git.meli-email.org/meli/meli/commit/866166eb8e8b994c8c87aad92a3303f9f6449b2d) `Don't print parsing error for empty bytes`
- [**`5b5869a2`**](https://git.meli-email.org/meli/meli/commit/5b5869a2ec3fce2fc69aa5c83fbda7a767f2a402) `Re-enable print to stderr ifdef MELI_DEBUG_STDERR`
- [**`13fe64a0`**](https://git.meli-email.org/meli/meli/commit/13fe64a027895780efdb6bfee246d562741a4be1) `Cache pgp signature verification results`
- [**`5ceddf41`**](https://git.meli-email.org/meli/meli/commit/5ceddf412e3b215b712e55aea8e18887d2d39f1a) `Update CHANGELOG.md`
- [**`4e55fbc9`**](https://git.meli-email.org/meli/meli/commit/4e55fbc90d8b105788c7c5998cb26b2829ac87a2) `Add SEEN flag to all envs, since NNTP has no flags`
- [**`e9cd800f`**](https://git.meli-email.org/meli/meli/commit/e9cd800f49e2d0e155d434ff8e91462e20b9d4f5) `Add support for storing read status locally`
- [**`53cba4be`**](https://git.meli-email.org/meli/meli/commit/53cba4beee4f774b548881c1a3f207ca391d3df3) `Update README.md relative file paths`
- [**`c4c245ee`**](https://git.meli-email.org/meli/meli/commit/c4c245ee19137f64d836401f7c1de17c9eb42b6e) `Respect danger_accept_invalid_certs setting`
- [**`29b43e2c`**](https://git.meli-email.org/meli/meli/commit/29b43e2c88edcfdecffd076fbb773c8547425f12) `Replace mktime with timegm`
- [**`4874e30f`**](https://git.meli-email.org/meli/meli/commit/4874e30f3ce9b186ac7cd427cba4a8542bd5048e) `Add smtp-trace feature`
- [**`51e9fbe8`**](https://git.meli-email.org/meli/meli/commit/51e9fbe8f2c380f3c9ee6a9ee65e638c169b43ef) `Add account_name identifier to sqlite3 index database name`
- [**`129f1091`**](https://git.meli-email.org/meli/meli/commit/129f10911b01641940801586bfa5286307e4342f) `Rename imap_backend feature to imap`
- [**`fe027fa3`**](https://git.meli-email.org/meli/meli/commit/fe027fa300a9882730a558fffe6000527ef08ff8) `Rename maildir_backend feature to maildir`
- [**`fe7dcc50`**](https://git.meli-email.org/meli/meli/commit/fe7dcc508ee51f492df2de3884147531fada6f4e) `Rename notmuch_backend feature to notmuch`
- [**`e9f09a15`**](https://git.meli-email.org/meli/meli/commit/e9f09a153ca0a1a023efe924b314ea977ccc3c25) `Rename mbox_backend feature to mbox`
- [**`7db930ca`**](https://git.meli-email.org/meli/meli/commit/7db930cabd295e888f4f106d5e7ea411521340ff) `Rename jmap_backend feature to jmap`
- [**`89c90f22`**](https://git.meli-email.org/meli/meli/commit/89c90f224a68ec524f7dc7033955ce7b8196f493) `Add nntp feature`
- [**`b65934fa`**](https://git.meli-email.org/meli/meli/commit/b65934facc7aeeb8ab30603e16cef2b747f9a0e5) `Add nntp-trace feature`
- [**`8ecdb6df`**](https://git.meli-email.org/meli/meli/commit/8ecdb6df3189cae4b6fa21a177bde756cc4407cf) `Add imap-trace feature`
- [**`9216e7bc`**](https://git.meli-email.org/meli/meli/commit/9216e7bc657738ae9861583a837c1326398197e4) `Add opt id string for tracing`
- [**`ae25ffba`**](https://git.meli-email.org/meli/meli/commit/ae25ffba430572efe73fde05eaf8111453f814cf) `Don't do plain EHLO before starting Tls connection`
- [**`8cb2a515`**](https://git.meli-email.org/meli/meli/commit/8cb2a515e1ba31efe914db67504993bc081ed7f3) `Use localhost in lieu of 127.0.0.1 for CI`
- [**`0ee1b6e0`**](https://git.meli-email.org/meli/meli/commit/0ee1b6e01830c01871e93e27d735a39792202325) `Start background watch job in init`
- [**`448e0635`**](https://git.meli-email.org/meli/meli/commit/448e0635e00b533a4d9dc15ba65982097649b397) `Log error when command length exceeds 512 octets`
- [**`bf543855`**](https://git.meli-email.org/meli/meli/commit/bf543855dc143b25344b79303f017380c9773793) `Add PartialEq<str> for MessageID`
- [**`7c7f6e19`**](https://git.meli-email.org/meli/meli/commit/7c7f6e1923e8b3127cf7cbd4b18f1db3ed9c6583) `Don't increase Thread length for duplicates`
- [**`5c2b0471`**](https://git.meli-email.org/meli/meli/commit/5c2b04719b953373c6a657f22db295d08b94685e) `Normalize std::fmt::* imports`
- [**`0f60009e`**](https://git.meli-email.org/meli/meli/commit/0f60009ea909adfb8f4e85d942decb8bc60f7539) `Add RUSTFLAGS with -D warnings`
- [**`6578a566`**](https://git.meli-email.org/meli/meli/commit/6578a5666889434ed6ca2f276e365633956fe3d3) `Update cargo install directions`
- [**`4f6081b6`**](https://git.meli-email.org/meli/meli/commit/4f6081b6633aed1eeafd99c24aa2dc64397043ca) `Update to imap-codec 1.0.0-beta.`
- [**`dc2b0044`**](https://git.meli-email.org/meli/meli/commit/dc2b00442b04c21455a6fda59b4729d0cbd04eff) `Run rustfmt and cargo-sort`
- [**`b3858de2`**](https://git.meli-email.org/meli/meli/commit/b3858de2f4e12723ee922174c79cc36062bed54e) `Impl From<io::ErrorKind> for ErrorKind`
- [**`f93adb68`**](https://git.meli-email.org/meli/meli/commit/f93adb683a562f25e40ffa03f80d04d5ad8ca34f) `Replace change_color uses with change_theme`
- [**`f193bdf6`**](https://git.meli-email.org/meli/meli/commit/f193bdf685e06652ab5b2da2a9a01fa56620cda6) `Add column headers and sorting`
- [**`095d24f9`**](https://git.meli-email.org/meli/meli/commit/095d24f91447a2ecab6d6bc78e1705ea4394e9bd) `Add PULL_REQUEST_TEMPLATE.md`
- [**`ab57e942`**](https://git.meli-email.org/meli/meli/commit/ab57e9420db29efd42773e970f33751b7b3f6f26) `Add delete_contact shortcut`
- [**`3963103d`**](https://git.meli-email.org/meli/meli/commit/3963103d55db28f789fe39f0dd80cd0d57792b5d) `Prevent duplicate contact creation`
- [**`f162239f`**](https://git.meli-email.org/meli/meli/commit/f162239fcc87d9c4f8aba8c33a9812a5e691c8d9) `Change on: conditions for test.yaml`
- [**`974b3a53`**](https://git.meli-email.org/meli/meli/commit/974b3a53058181e3df992a2105abcbf1c392fc19) `Update bitflags, rusqlite dependencies`
- [**`4d22b669`**](https://git.meli-email.org/meli/meli/commit/4d22b669bf330f8f3168fc2f704ad63c21c5e821) `Update dependencies`
- [**`ffba203a`**](https://git.meli-email.org/meli/meli/commit/ffba203a3b7070cc9e71d9444556e108ff0e18ea) `Add support for Home and End key navigation`
- [**`3433f7c4`**](https://git.meli-email.org/meli/meli/commit/3433f7c41e0d0cbb48af821280537da41b9e53d0) `Update PULL_REQUEST_TEMPLATE.md`
- [**`f7a4741b`**](https://git.meli-email.org/meli/meli/commit/f7a4741bf1622ae60042fb6ab0a906fe50fb1e06) `Add jmap-trace feature`
- [**`c875dda4`**](https://git.meli-email.org/meli/meli/commit/c875dda4960e5688b17176ba82ad1e5da38b883b) `Add last_method_response field to Connection`
- [**`37a787e6`**](https://git.meli-email.org/meli/meli/commit/37a787e6bb5abd34fae2888944537dec1ee3842f) `Use IndexMap instead of HashMap`
- [**`6ebdc7f9`**](https://git.meli-email.org/meli/meli/commit/6ebdc7f9aec5531c2b562a4e0cfd320ead6a4c01) `Add Id<_>::empty() contructor`
- [**`4f9b9773`**](https://git.meli-email.org/meli/meli/commit/4f9b97736a4af8b8b4ba0017ad1175a1c2352db6) `Rename EmailImport to EmailImportObject`
- [**`11432ba2`**](https://git.meli-email.org/meli/meli/commit/11432ba2c381b07bb540f7f92664b3c351e3cf62) `Make null fields into Option<_>s`
- [**`d9467d5f`**](https://git.meli-email.org/meli/meli/commit/d9467d5fcd9543611ec8a034eb7e25d12a3dcc45) `Save all core capabilities to session store`
- [**`31982931`**](https://git.meli-email.org/meli/meli/commit/31982931f5f472717b4c3d900f16c0588682f48e) `Use Argument<OBJ> (value or resultreference) where appropriate`
- [**`29fd8522`**](https://git.meli-email.org/meli/meli/commit/29fd8522e6bc2b0b6196cb97c8868dc34c2ba2f0) `Implement Backend::create_mailbox()`
- [**`5d8f07c8`**](https://git.meli-email.org/meli/meli/commit/5d8f07c8058261c7c251b3fb010ad866110e91df) `Rename some objects better`
- [**`38bc1369`**](https://git.meli-email.org/meli/meli/commit/38bc1369cc136c482f48d1ed3172b7f510ff7762) `Add an Identity type.`
- [**`59513b26`**](https://git.meli-email.org/meli/meli/commit/59513b267097cac8fe757c6198f26e0179014604) `Implement Backend::submit(), server-side submission`
- [**`5459a84f`**](https://git.meli-email.org/meli/meli/commit/5459a84f3d2b4c91a89252fba63f4ef12d965b9b) `Update to imap-codec 1.0.0 (w/o -beta)`
- [**`290cfb86`**](https://git.meli-email.org/meli/meli/commit/290cfb86c0c942690c48a0d3298e9d2de3ec4d94) `Add a highlighted_selected theme key`
- [**`46636d87`**](https://git.meli-email.org/meli/meli/commit/46636d8748f2779f38a10c6bf38c4e07acf16f8a) `Bump version to 0.8.0`

### Continuous Integration

- [**`1d0405ed`**](https://git.meli-email.org/meli/meli/commit/1d0405ed5b5cd76f4fe79e73fb30f4d4dce1d441) `Add env vars`
- [**`6e27edcb`**](https://git.meli-email.org/meli/meli/commit/6e27edcb775ce831b784d2040672f2d2af2c020f) `Use cargo-nextest`
- [**`67d2da0f`**](https://git.meli-email.org/meli/meli/commit/67d2da0f88b0e7b9b74c5d05c6c17a45057b094a) `Disable smtp::test::test_smtp in test.yaml`

## [alpha-0.7.2] - 2021-10-15

### Added

- Add forward mail option
- Add `url_launcher` config setting
- Add `add_addresses_to_contacts` command
- Add `show_date_in_my_timezone` pager config flag
- docs: add pager filter documentation
- mail/view: respect per-folder/account pager filter override
- pager: add `filter` command, `Esc` to clear filter
- Show compile time features in with command argument

### Fixed

- melib/email/address: quote `display_name` if it contains ","
- melib/smtp: fix `Cc` and `Bcc` ignored when sending mail
- melib/email/address: quote `display_name` if it contains "."

## [alpha-0.7.1] - 2021-09-08

### Added

- Change all `Down/Up` shortcuts to `j/k`
- add `GB18030` charset
- melib/nntp: implement refresh
- melib/nntp: update total/new counters on new articles
- melib/nntp: implement NNTP posting
- configs: throw error on extra unused conf flags in some imap/nntp
- configs: throw error on missing `composing` section with explanation

### Fixed

- Fix compilation for netbsd-9.2
- conf: fixed some boolean flag values requiring to be string e.g. "true"

## [alpha-0.7.0] - 2021-09-03

### Added

Notable changes:

- add import command to import email from files into accounts
- add `add-attachment-file-picker` command and `file_picker_command` setting to
  use external commands to choose files when composing new mail
- ask confirm for delete
- add `export-mbox` command
- add `export-mail` command
- add TLS support with nntp
- add JMAP watch with polling
- add `reload-config` command
- add `import-mail` command
- imap: implement gmail `XOAUTH2` authentication method
- imap: implement `OAUTH2` authentication
- compose: treat inline `message/rfc822` as attachments
- add gpg support via `libgpgme`

### Fixed

- Loading notmuch library on macos
- Limit dbus dependency to `target_os = "linux"`
- IMAP, notmuch, mbox backends: various performance fixes

## [alpha-0.6.2] - 2020-09-24

### Added
- Add customizable mailbox tree in sidebar
- Make dbus dependency opt-out (feature is `dbus-notifications`)
- Implemented JMAP async, search, tagging, syncing
- Preserve account order from configuration file
- Implemented IMAP `CONDSTORE` support for IMAP cache
- Add timeout setting for IMAP
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

- Add select command to select threads that match search query
- Add support for mass copying/deleting/flagging/moving of messages
- IMAP: add support for `COMPRESS=DEFLATE` and others
  Extension use can be configured with individual flags such as `use_deflate`
- Rename `EXECUTE` mode to `COMMAND`
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
- ui: switch between sidebar and mailbox view with `{left,right}` keys for more intuitive navigation
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
- Added `cli-docs` compile time feature: Optionally build manpages to text with `mandoc` and print them from the command line.
- Added new theme keys

[Unreleased]: #
[alpha-0.5.1]: https://github.com/meli/meli/releases/tag/alpha-0.5.1
[alpha-0.6.0]: https://github.com/meli/meli/releases/tag/alpha-0.6.0
[alpha-0.6.1]: https://github.com/meli/meli/releases/tag/alpha-0.6.1
[alpha-0.6.2]: https://github.com/meli/meli/releases/tag/alpha-0.6.2
[alpha-0.7.0]: https://github.com/meli/meli/releases/tag/alpha-0.7.0
[alpha-0.7.1]: https://github.com/meli/meli/releases/tag/alpha-0.7.1
[alpha-0.7.2]: https://github.com/meli/meli/releases/tag/alpha-0.7.2
[v0.8.0]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.0
[v0.8.1]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.1
[v0.8.2]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.2
[v0.8.3]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.3
[v0.8.4]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.4
[v0.8.5]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.5
[v0.8.6]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.6
[v0.8.7]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.7
[v0.8.8]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.8
[v0.8.9]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.9
[v0.8.10]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.10
[v0.8.11]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.11
[v0.8.12]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.12
