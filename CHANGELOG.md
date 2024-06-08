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


## [v0.8.6](https://git.meli-email.org/meli/meli/releases/tag/v0.8.6) - 2024-06-08

Contributors in alphabetical order:

- euxane
- Manos Pitsidianakis

### Added

- [735b44f2](https://git.meli-email.org/meli/meli/commit/735b44f2860307db1b664de43cea2b8c34a71b15) Add 'highlight_self' theme attribute
- [e187bb3f](https://git.meli-email.org/meli/meli/commit/e187bb3f0dd0166d69df4d84f1f63e1d005b4cbe) Add tools subcommand with smtp shell for debugging
- [571bd984](https://git.meli-email.org/meli/meli/commit/571bd984979031a08c3f908bda1f669b464afe78) Add proper imap-shell in tools subcommand for debugging
- [0e1e5b9e](https://git.meli-email.org/meli/meli/commit/0e1e5b9ea72cd5a691fb1044bee8b0f60cb1d936) Add support for Alternate Scroll Mode (xterm)
- [fe08d52a](https://git.meli-email.org/meli/meli/commit/fe08d52a7f4d9dfae1a9706b87aae8b77949c63d) Add force_text_emoji_presentation option

### Bug Fixes

- [3de4908d](https://git.meli-email.org/meli/meli/commit/3de4908d6b6fab293fedabde91e6f027fd08acfa) man.7 Fix typo for toggle_expand_headers
- [a8c7582f](https://git.meli-email.org/meli/meli/commit/a8c7582fa36ebb025877f341ea4226ffa2ff46bb) Fix ENVELOPE parsing in untagged responses
- [c65635ef](https://git.meli-email.org/meli/meli/commit/c65635ef600d09a354d2113d94f1aee153b2c721) Fix compilation for macos
- [06ec2790](https://git.meli-email.org/meli/meli/commit/06ec2790d0f3fd429e68987b4dfe6151afbc3d5b) Fix str slice index panic
- [f2b59a76](https://git.meli-email.org/meli/meli/commit/f2b59a7633d9578bfcadb205c8847d1409e8cfdc) Add RequestUrlTemplate type
- [7eed944a](https://git.meli-email.org/meli/meli/commit/7eed944abc54dfc8acd53a4e6382f311c66b5d6c) Fix screwed up rfc8620 module split
- [74a3539f](https://git.meli-email.org/meli/meli/commit/74a3539f882d4ccfbfc71e338f9fdcf4c8ed3e82) Fix degenerate OOB cell access
- [e8e76970](https://git.meli-email.org/meli/meli/commit/e8e7697001a8a41caee8dc108f6f76a9ea988c1e) Fix edge case with strings/linebreaking
- [81955187](https://git.meli-email.org/meli/meli/commit/819551876d594a5f4f6c4cdb481b0f8a8cfcf8cd) Fix decryption error not shown

### Refactoring

- [a9c3b151](https://git.meli-email.org/meli/meli/commit/a9c3b151f1a2418c6425ec953a2633892db6ff7d) Impl highlight_self in all index styles
- [57e3e643](https://git.meli-email.org/meli/meli/commit/57e3e643a1590775e819a49e6f6a02e0c0000466) Remove excessive right padding in flags
- [a4ebe3b7](https://git.meli-email.org/meli/meli/commit/a4ebe3b7d456c4283255d7cf5cc9d6903f1634d7) Add ErrorKind::Platform
- [4bdfb3a3](https://git.meli-email.org/meli/meli/commit/4bdfb3a31b3164dacce1b0d39eea1c3308ae8e8b) Disable Nagle's algorithm by default
- [4148aee5](https://git.meli-email.org/meli/meli/commit/4148aee59b1e0dda6a30fcf84c93b5a53733ae8a) Refactor smtp,draft errors and email tests
- [ed5a6b04](https://git.meli-email.org/meli/meli/commit/ed5a6b04f4e28f7fbea09cbc45fb8f781282bccb) Add a symbols range to is_emoji check
- [fc1122a2](https://git.meli-email.org/meli/meli/commit/fc1122a2aae9f52bb70cb71d1456669e4f7aa8f1) Rename to backend_mailbox.rs
- [50ecade7](https://git.meli-email.org/meli/meli/commit/50ecade74a5c4654fa31e3008ca56652b6288e7a) Merge rfc8620/tests.rs to tests.rs
- [a78f3f26](https://git.meli-email.org/meli/meli/commit/a78f3f261d89d5d991509dc71284f5ab13be734e) Move submodules to jmap/
- [f7838b1d](https://git.meli-email.org/meli/meli/commit/f7838b1ddf30e30ab61b6c2bb0438ecc5d909886) Split to methods.rs and objects.rs
- [74f0d12a](https://git.meli-email.org/meli/meli/commit/74f0d12afbec0d515743c4a1d177ac47a84a6c4f) Remove obsolete imapshell.rs and smtp_conn.rs
- [dce3852f](https://git.meli-email.org/meli/meli/commit/dce3852fe55b5614ef7f2f03d4efb76b2ddc256e) Add capabilities module
- [7ba7dc70](https://git.meli-email.org/meli/meli/commit/7ba7dc70c584d866b399184244457c0de83584d4) Imports cleanup in all modules
- [45bfcf87](https://git.meli-email.org/meli/meli/commit/45bfcf870710ae29e311a5a99df10d39fadd4b3b) Minor refactors
- [77867aee](https://git.meli-email.org/meli/meli/commit/77867aeed4e49d91b81fcb33b7bff5b0dd25daee) Unwrap object module
- [33999fc6](https://git.meli-email.org/meli/meli/commit/33999fc6abbd553a345944edaec44b7f32a98e19) Re-add Submission to USING
- [6be25ac3](https://git.meli-email.org/meli/meli/commit/6be25ac3df34ac8aec7721317feffca73f2fbadf) Don't use client field for get/posts

### Documentation

- [4722d7cc](https://git.meli-email.org/meli/meli/commit/4722d7ccb88c03e0dc4897f11b927c8e5d4b424a) Also mention server_password_command for jmap

### Miscellaneous Tasks

- [2bfe6086](https://git.meli-email.org/meli/meli/commit/2bfe60867835f17201bee348e03d073ae4d1e08e) Hide self from "add contacts" options
- [9ca34a68](https://git.meli-email.org/meli/meli/commit/9ca34a6864f3b71aca5c1a0dfb36191b55b3f636) Update MSRV to 1.70.0
- [50ff16c4](https://git.meli-email.org/meli/meli/commit/50ff16c44f1ecc85de904e43297149cb8a4cc73b) Add LIGHT, DARK constant theme keys
- [1abce964](https://git.meli-email.org/meli/meli/commit/1abce964c7c78aead04ce85ba7e318602eeab6d6) Add Envelope::recipient_any method
- [671d35e2](https://git.meli-email.org/meli/meli/commit/671d35e21ec3b2c62c6bca264acb37b5b2ae5aa9) Update mailin-embedded dependency to 0.8.2
- [39fbb164](https://git.meli-email.org/meli/meli/commit/39fbb164edf318d65b8cc3c7cfca4911e4c88235) Change info_message_{next,prev} shortcuts to `<, >`
- [58d73271](https://git.meli-email.org/meli/meli/commit/58d7327130aa2cefceb41d8dfc84015b6a9c7527) Change new mail text content
- [f0d1b9cf](https://git.meli-email.org/meli/meli/commit/f0d1b9cfd12490bc3698f0fe6d9e4e7249d91a59) Add ayllu mirror link
- [3bab5324](https://git.meli-email.org/meli/meli/commit/3bab5324c4273bf335b881b2e48ac4067bcb661e) Improve Debug impl for ContentType etc
- [e9dd6bec](https://git.meli-email.org/meli/meli/commit/e9dd6becc3722fc2e3a95626a8d8b61f9c12fd45) Comment out content
- [8dd87c1a](https://git.meli-email.org/meli/meli/commit/8dd87c1ac56b8277f89e4b9d76f5bc151e1a5f09) Add ContentType::is_text_plain()
- [01bc62e0](https://git.meli-email.org/meli/meli/commit/01bc62e0adfc16e7878299f3c74076739da5652f) Add new_plaintext method

## [v0.8.5](https://git.meli-email.org/meli/meli/releases/tag/v0.8.5) - 2024-05-05

Contributors in alphabetical order:

- Andrei Zisu
- Ethra
- Geert Stappers
- Guillaume Ranquet
- Manos Pitsidianakis

### Added

- [0e3a0c4b](https://git.meli-email.org/meli/meli/commit/0e3a0c4b7049139994a65c6fe914dd3587c6713e) `Add safe UI widget area drawing API`
- [0114e695](https://git.meli-email.org/meli/meli/commit/0114e695428579ef4461b289d7372e3b392b5e62) `Add next_search_result and previous_search_result shortcuts`
- [0b468d88](https://git.meli-email.org/meli/meli/commit/0b468d88ad53178d77fbf8a4d9014b4b8bbd134f) `Improve Error messages`
- [5af2e1ee](https://git.meli-email.org/meli/meli/commit/5af2e1ee6631fdfaa4f67243b97119debc32a484) `Add subcommand to print config file location`
- [62aee464](https://git.meli-email.org/meli/meli/commit/62aee4644bee3b4d0e549734272cf19c18a512ef) `Add subcommand to print log file location`
- [e2cdebe8](https://git.meli-email.org/meli/meli/commit/e2cdebe89cb0b0619d32efc0240efeb340e2cecd) `Add option to highlight self in mailing list threads`
- [cd448924](https://git.meli-email.org/meli/meli/commit/cd448924ed102531b7cf5676968baedc392c862e) `Add clear-selection command`
- [3a5306e9](https://git.meli-email.org/meli/meli/commit/3a5306e9dd50e3fe369295d8a5fef350a1ce9d77) `View manpages in pager inside meli`
- [a37d5fc1](https://git.meli-email.org/meli/meli/commit/a37d5fc1d19f5b195361ddb3ba045d848266285d) `Implement a key to command mapping`
- [ce4ba06c](https://git.meli-email.org/meli/meli/commit/ce4ba06ce9980990304b06b7ce3c34e50b0cdba7) `Add a flag set/unset command`
- [148f0433](https://git.meli-email.org/meli/meli/commit/148f0433d928e5f4bf64de88c029c1a4147cb6b1) `Implement flag set/unset action in UI`
- [417b24cd](https://git.meli-email.org/meli/meli/commit/417b24cd842775e1d44b972a7569c21b7c8ab936) `Print invalid command on error`
- [4e941a9e](https://git.meli-email.org/meli/meli/commit/4e941a9e8bca5778ebfa60515a7519df02309127) `Add default_mailbox setting`
- [974502c6](https://git.meli-email.org/meli/meli/commit/974502c6ffa263c8cfebf40c46b8694dce7fe982) `Impl Hash for Card`
- [ba7a97e9](https://git.meli-email.org/meli/meli/commit/ba7a97e90b4c474299a7b12fa74b7ea06c1535c8) `Add x axis scroll support`
- [ccf6f9a2](https://git.meli-email.org/meli/meli/commit/ccf6f9a26e95437fb24464f90736c653e3f5dfed) `Remember previous set [index_style] preferences`

### Bug Fixes

- [bcec745c](https://git.meli-email.org/meli/meli/commit/bcec745c241d7ed5d7d455ccdd65c6c95e1862b0) `Fix command and status bar drawing`
- [62b8465f](https://git.meli-email.org/meli/meli/commit/62b8465f2cd99789576d70008f1f321243b81fc3) `Fix ThreadView for new TUI API`
- [28fa66cc](https://git.meli-email.org/meli/meli/commit/28fa66cc2ad05e67708377fc99ffd65aa1b14386) `Fix ThreadedListing for new TUI API`
- [2c6f180d](https://git.meli-email.org/meli/meli/commit/2c6f180df987976c1f4cba7ceac878e697c73d27) `Fix macos compilation`
- [24971d19](https://git.meli-email.org/meli/meli/commit/24971d1960418bad92d89af9eb744933445baf99) `Fix compilation with 1.70.0 cargo`
- [34a2d52e](https://git.meli-email.org/meli/meli/commit/34a2d52e7ee8117f84d5177826c4d258c8dc28cd) `Fix rustdoc::redundant_explicit_links`
- [f63774fa](https://git.meli-email.org/meli/meli/commit/f63774fa6ddc9392be4191a549e7b99213132433) `Fix new clippy lints (1.75)`
- [33408146](https://git.meli-email.org/meli/meli/commit/33408146a178435d80871cdc91fad438144ded0f) `Fix feature permutation mis-compilations found with cargo-hack`
- [e3351d27](https://git.meli-email.org/meli/meli/commit/e3351d27555604524623bc8b6ae02f04a8414f97) `Fix set unseen updating all mboxes`
- [8185f2cf](https://git.meli-email.org/meli/meli/commit/8185f2cf7d539c714fd0a6128f40ba1fc9e0c43c) `Add deny clippy lints and fix them`
- [7861fb04](https://git.meli-email.org/meli/meli/commit/7861fb0402a652d692cb3c3be3b335d3b5a406fd) `Fix typos found with `typos` tool`
- [64e60cb0](https://git.meli-email.org/meli/meli/commit/64e60cb0ee79841ab40e3dba94ac27150a264c5c) `Fix select modifier regression`
- [60f26f9d](https://git.meli-email.org/meli/meli/commit/60f26f9dae1bb1e6379c2f6edd109c103179ddd6) `Fix some old pre-intradoc rustdoc links`
- [1fe36192](https://git.meli-email.org/meli/meli/commit/1fe361920809dfd4e51929e5a3e0376f46078422) `Make conf validation recognize AccountSettings extra keys`
- [c332c2f5](https://git.meli-email.org/meli/meli/commit/c332c2f5ff2cead87657bef489d223c6cb276697) `Fix new clippy lints (mostly clippy::blocks_in_conditions)`
- [070930e6](https://git.meli-email.org/meli/meli/commit/070930e671aa25f286ead73e6c9e80e23b5fef49) `Fix auto index build when missing`
- [26928e3a](https://git.meli-email.org/meli/meli/commit/26928e3ae90966e84d9e5f052631802ca634d99f) `Fix compilation for macos`
- [3884c0da](https://git.meli-email.org/meli/meli/commit/3884c0da1f3e7ba4ad85ff2fce087eb444e5ac20) `Small typographic fixups`
- [b820bd6d](https://git.meli-email.org/meli/meli/commit/b820bd6d9ceb5907d8550c9654fa8ce718d6b013) `Remove unused imap_trace! and fix comp`
- [a88b8c5e](https://git.meli-email.org/meli/meli/commit/a88b8c5ea055eb530ad523b673ea87fd19b70487) `Debian/changelog warning fix`
- [4ce616ae](https://git.meli-email.org/meli/meli/commit/4ce616aeca07b957cbf58c4a736a9a7d05b7cd04) `Fix lints.yaml rustup install step`
- [264782d2](https://git.meli-email.org/meli/meli/commit/264782d2280a63e82e16dfe005aeee522d102f82) `Various unimportant minor style/doc fixups`
- [475609fe](https://git.meli-email.org/meli/meli/commit/475609fe9264c37d3a8bc835ea654d2c2e047f9b) `Make {prev,next}_entry shortcut behavior consistent`
- [a69c674c](https://git.meli-email.org/meli/meli/commit/a69c674c072cdf1b6c9e3697e21673735718f119) `Fix new 1.77 clippy lints`
- [48cb9ee2](https://git.meli-email.org/meli/meli/commit/48cb9ee204929b3ae34e48be4f363572091b28f7) `Fix compilation for macos`
- [8a16cf6d](https://git.meli-email.org/meli/meli/commit/8a16cf6db482b7a65b1e6962c48a493fb1aaf0cf) `Fix wrong column index crash`
- [bc1b6531](https://git.meli-email.org/meli/meli/commit/bc1b65316d9e97812e107e849a6c93fbcc85f216) `Fix constant redrawing`
- [29cc1bce](https://git.meli-email.org/meli/meli/commit/29cc1bce5b61b575f76ff39888d4b420f1d3b7ce) `Remove obsolete file melib/src/text/tables.rs.gz`
- [ab041898](https://git.meli-email.org/meli/meli/commit/ab0418988748af508f461c7712f3a83ed36d11a7) `Fix new warnings for 1.78.0`
- [46e40856](https://git.meli-email.org/meli/meli/commit/46e40856baf4383b08a58d001b7ff9b0f5388fb7) `Fix UIConfirmationDialog highlight printing`
- [3b93fa8e](https://git.meli-email.org/meli/meli/commit/3b93fa8e7c4db8ce2e7714a84515ef772d480e53) `Don't draw messages above embedded terminal`
- [684fae3e](https://git.meli-email.org/meli/meli/commit/684fae3ed80aad6995aef22d6fae56f2c19f3e8f) `Copy old content to new buf when resizing`
- [5d915baa](https://git.meli-email.org/meli/meli/commit/5d915baa8162332340ba4e8c482abce99af40c11) `Use Screen::resize instead of CellBuffer::resize`
- [6a66afe9](https://git.meli-email.org/meli/meli/commit/6a66afe93eb928e6df93e79d0de386e0867fc3ec) `Make add contact dialog scrollable on overflow`
- [aa5737a0](https://git.meli-email.org/meli/meli/commit/aa5737a004adc0b23f4d9573465f5c3f2129fc3d) `Prevent drawing pager on embedded mode`
- [07072e2e](https://git.meli-email.org/meli/meli/commit/07072e2e3ffee70c0aa2f68622cf7173c8cc7193) `Prevent panic if envelope is deleted`
- [8ddd673d](https://git.meli-email.org/meli/meli/commit/8ddd673dd83d0d11115fabac8a85e068e3d3f233) `Update all mailboxes`
- [3691cd29](https://git.meli-email.org/meli/meli/commit/3691cd2962e9b3ae68128a6beb96f268d42b763f) `Send EnvelopeUpdate event after self.collection.update_flags()`
- [1fcb1d59](https://git.meli-email.org/meli/meli/commit/1fcb1d59b8d07ce1be861690b2b4c1d1b2b4fde3) `Remove rerun when build.rs changes`
- [933bf157](https://git.meli-email.org/meli/meli/commit/933bf157ae105c55ccb65880b4f3eef7039b54d5) `Ack \ as an atom`
- [a1cbb198](https://git.meli-email.org/meli/meli/commit/a1cbb1988b34951046045f724f52bed2925b3880) `Return Results instead of panicking`
- [b5ddc397](https://git.meli-email.org/meli/meli/commit/b5ddc397df982a885bb797f1ae762fe8f58fce56) `Remove unwrap() from get_events() loop`


### Changes

- [61a0c3c2](https://git.meli-email.org/meli/meli/commit/61a0c3c27f7c3691fb8f4503664f80667b0077e2) `Do not clear selection after action`
- [9af284b8](https://git.meli-email.org/meli/meli/commit/9af284b8db97b02788a2209bed705d20db6c2500) `Don't hide unread count for mailboxes that are partly truncated`
- [35408b16](https://git.meli-email.org/meli/meli/commit/35408b1689fcec1814e893a7bc34380e30ef7c57) `Run pager filter asynchronously`
- [e80ea9c9](https://git.meli-email.org/meli/meli/commit/e80ea9c9deee46f3de27c26a715bb06d1d09201a) `Changed default manpage install path`
- [742f038f](https://git.meli-email.org/meli/meli/commit/742f038f74051ab8499ec2aaa65bf4cd4c12e1dd) `Move sent_mailbox to settings`
- [86bbf1ea](https://git.meli-email.org/meli/meli/commit/86bbf1ea573c47107aee021b85651e54e7bdec79) `Refresh NotmuchMailbox counts when setting flags`
- [f0866a39](https://git.meli-email.org/meli/meli/commit/f0866a3965063ebe7dd2663a9ad1cba828ef91aa) `Make config error more user-friendly`
- [11f3077b](https://git.meli-email.org/meli/meli/commit/11f3077b06d2c7dce5dc35876a8bb419cbb642ca) `Add more possible values for manpage names`
- [1eca34b3](https://git.meli-email.org/meli/meli/commit/1eca34b3986ffd970d440440761c8516dbb863bb) `Set lowest priority to shortcut command UIEvents`
- [484712b0](https://git.meli-email.org/meli/meli/commit/484712b0c379aa95ba5792effd501bfa45c6c45c) `Check for unrecoverable errors in is_online`
- [8ec6f220](https://git.meli-email.org/meli/meli/commit/8ec6f220902d2994f9916a94e8ea43450b6917ab) `Use ShellExpandTrait::expand in more user-provided paths`

### Refactoring

- [0500e451](https://git.meli-email.org/meli/meli/commit/0500e451dab5f129d71a9279913531e77981e868) `Add missing EnvelopeRemove event handler`
- [ab14f819](https://git.meli-email.org/meli/meli/commit/ab14f81900a03a07ef00a6b3232cb29d78e8edf5) `Make write_string_to_grid a CellBuffer method`
- [e0adcdfe](https://git.meli-email.org/meli/meli/commit/e0adcdfe15b8a78c333de199ba734a83181f53be) `Move rest of methods under CellBuffer`
- [0a74c7d0](https://git.meli-email.org/meli/meli/commit/0a74c7d0e5c318dd29c8ace01e588d441e0fcfb6) `Overhaul refactor`
- [3b4acc15](https://git.meli-email.org/meli/meli/commit/3b4acc15a535c9bfd084b2e33f2cd00b5b5d4eb0) `Add tests`
- [7eedd860](https://git.meli-email.org/meli/meli/commit/7eedd860518e3f7f5000a1888e4fa58ddbfb43bc) `Remove address_list! macro`
- [f3e85738](https://git.meli-email.org/meli/meli/commit/f3e85738e7981755e96468213c02af78432f8cdd) `Move build.rs scripts to build directory`
- [77325486](https://git.meli-email.org/meli/meli/commit/773254864bd8436712905eeb0c725d1d05277e60) `Remove on-push hooks for actions w/ run on-pr`
- [08518e1c](https://git.meli-email.org/meli/meli/commit/08518e1ca81dd4eeabcaed9d3ec098b3b73f9a45) `Remove obsolete position.rs module`
- [ddab3179](https://git.meli-email.org/meli/meli/commit/ddab3179c2640488a3acd8cdeba1506a4e8f7641) `Move tests to tests module`
- [79520068](https://git.meli-email.org/meli/meli/commit/795200687020f8dbb82de619f8c5eecd07df6c49) `Remove doctests, add tests module`
- [4e7b6656](https://git.meli-email.org/meli/meli/commit/4e7b66567268ecb42eb55ac588ca401384835a38) `Sqlite caching refactor`
- [b5fd3f57](https://git.meli-email.org/meli/meli/commit/b5fd3f57a7d99b8918ca7bd24f715ccd6bfe448a) `Make self.view an Option`
- [a3aaec38](https://git.meli-email.org/meli/meli/commit/a3aaec382ab58c31cc8bc6122e5c67d6a97e4e9f) `Remove unused imports`
- [11a0586d](https://git.meli-email.org/meli/meli/commit/11a0586d562b8623e72a54ded7ba27b24c70206c) `Remove num_cpus dependency`
- [8f3dee9b](https://git.meli-email.org/meli/meli/commit/8f3dee9b22c7cea414bb49379f8240cb4a1beca3) `Extract mod manpages to standalone file`
- [89c7972e](https://git.meli-email.org/meli/meli/commit/89c7972e12a8eb0503795cf61c9b811882d4c19e) `Add suggestions to BadValue variant`
- [35a9f33a](https://git.meli-email.org/meli/meli/commit/35a9f33aab133d5368c140b40ed9477e251204d3) `Extract common FlagString logic`
- [1b0bdd0a](https://git.meli-email.org/meli/meli/commit/1b0bdd0a9a51ede2dd4fac0f75dd667894b704e2) `Split queries and mailbox into submodules`
- [506ae9f5](https://git.meli-email.org/meli/meli/commit/506ae9f594528051d0680059c1f82618fa0899c2) `Add ErrorKind::LinkedLibrary variant`
- [ebe1b3da](https://git.meli-email.org/meli/meli/commit/ebe1b3da7e6a209cd2ef2d3aa11809aaab29bc6f) `Wrap *mut struct fields in NonNull<_>`
- [ca7d7bb9](https://git.meli-email.org/meli/meli/commit/ca7d7bb95d387ef04fc568199e8767b4a428d7e4) `Use message freeze/thaw for flag changes`
- [4026e254](https://git.meli-email.org/meli/meli/commit/4026e254286dc061c0d3792dd3c6cda1bfdeb535) `Add some doc comments`
- [808aa494](https://git.meli-email.org/meli/meli/commit/808aa4942d1de1ecf0944d9d1c540b417f859991) `Rename text_processing to text for the whole brevity thing`
- [bebb473d](https://git.meli-email.org/meli/meli/commit/bebb473d1b6d9acfcd6df59573e2311dad98213f) `Derive extra traits for enums`
- [ab1b946f](https://git.meli-email.org/meli/meli/commit/ab1b946fd9d93a24a9802fbabe96705c15bc25e1) `Don't print details if it's an empty string.`
- [f685726e](https://git.meli-email.org/meli/meli/commit/f685726eac07c6b1d4f40dded6f3f55bcfcea5cc) `Add backtrace field to ParsingError`
- [73d5b24e](https://git.meli-email.org/meli/meli/commit/73d5b24e9847cf7dd033041e97f59274dece0b3d) `Merge integration tests in one crate`
- [31401fa3](https://git.meli-email.org/meli/meli/commit/31401fa35ca994632dee8b3f89bc4a921d64806b) `Add LazyCountSet::contains method`
- [0270db01](https://git.meli-email.org/meli/meli/commit/0270db0123eeb5abb41e53d07d308f1bd541f2a6) `From<&[u8]> -> From<B: AsRef<[u9]>>`
- [873a67d0](https://git.meli-email.org/meli/meli/commit/873a67d0fbe2512b03c0cf75b3ea7a89d4aecb1c) `Replace erroneous use of set_err_kind with set_kind`
- [51e3f163](https://git.meli-email.org/meli/meli/commit/51e3f163d403cbe127b8fc23c8fe0adf6beafd49) `Use Url instead of String in deserializing`
- [8014af25](https://git.meli-email.org/meli/meli/commit/8014af2563d4f1acf7016d8d67dea95add16ed2e) `Reduce debug prints`
- [f31b5c40](https://git.meli-email.org/meli/meli/commit/f31b5c4000a238bf17251aae0d7fd40c12dc6e37) `Don't print raw bytes as escaped unicode`
- [41e965b8](https://git.meli-email.org/meli/meli/commit/41e965b8a3743045356698a26dc4d1e21fcb0d36) `Split mbox/job stuff in submodules`
- [ec01a441](https://git.meli-email.org/meli/meli/commit/ec01a4412a825e9c7e4ecd253875ad4edae5893f) `Turn some sync connections to unsync`
- [3e914465](https://git.meli-email.org/meli/meli/commit/3e9144657b5f79764c762a0f1e8120a96bcce1f5) `Store children process metadata`
- [c53a32de](https://git.meli-email.org/meli/meli/commit/c53a32de4c04aa2ef7fedc4c83201cab952bc879) `Re-enables horizontal thread view`
- [36b7c00b](https://git.meli-email.org/meli/meli/commit/36b7c00b97b0b0ea07293bbdea0dc1cc94f3ad90) `Put doc text type names and co. in backtics`
- [634bd191](https://git.meli-email.org/meli/meli/commit/634bd1917a95919b65c37db612e0d082ef5f3aa4) `Convert log prints to traces`
- [1048ce68](https://git.meli-email.org/meli/meli/commit/1048ce6824391d9d81a6f711d8dbdad549c9c6b9) `Add hostname() utility function`
- [7645ff1b](https://git.meli-email.org/meli/meli/commit/7645ff1b875e3920389567eb5e61d800291e8a27) `Rename write_string{to_grid,}`
- [c2ae19d1](https://git.meli-email.org/meli/meli/commit/c2ae19d1208f2eb5cca341a04e019c3e285637a8) `Return Option from current_pos`
- [b61fc3ab](https://git.meli-email.org/meli/meli/commit/b61fc3ab6482dcef4f5cc1c09db3539b7e401f78) `Add HelpView struct for shortcuts widget`
- [3495ffd6](https://git.meli-email.org/meli/meli/commit/3495ffd61b5888f8538304ecb6e441819b373bdc) `Change UIEvent::Notification structure`
- [23c15261](https://git.meli-email.org/meli/meli/commit/23c15261e79c63791c569f225c1745df1b90ce2d) `Abstract envelope view filters away`
- [031d0f7d](https://git.meli-email.org/meli/meli/commit/031d0f7dc76700ac938e1ee4a767fab8deebb9f2) `Add area.is_empty() checks in cell iterators`
- [e37997d6](https://git.meli-email.org/meli/meli/commit/e37997d697f1f0b8faaa56a36f43c9f1da4bbb41) `Store Link URL value in Link type`
- [b6f769b2](https://git.meli-email.org/meli/meli/commit/b6f769b2f43087b71bf93667911cb22d7f690c95) `Add field names to row_attr! bool values`
- [0da97dd8](https://git.meli-email.org/meli/meli/commit/0da97dd8c11835d25e056c3dc0821b5574ab06f0) `Check row_updates in is_dirty()`
- [6506fffb](https://git.meli-email.org/meli/meli/commit/6506fffb9427ba13ba4368cd6b2c0dba12e5294c) `Rewrite email flag modifications`
- [23507932](https://git.meli-email.org/meli/meli/commit/23507932f94257a71f2ca8db23840ee0716072b6) `Update cache on set_flags`
- [470cae6b](https://git.meli-email.org/meli/meli/commit/470cae6b885c9b4851195fbb8274b1663bfa75cb) `Update thread cache on email flag modifications`
- [84f3641e](https://git.meli-email.org/meli/meli/commit/84f3641ec1401a0522811add0ed87a131be449b9) `Re-add on-screen message display`
- [54d21f25](https://git.meli-email.org/meli/meli/commit/54d21f25fdb716d36fd3678dd149eb880e16698d) `Re-add contact list and editor support`
- [458258e1](https://git.meli-email.org/meli/meli/commit/458258e1aab91f3883d6a9201a175462511349e9) `Re-enable compact listing style`
- [1c1be7d6](https://git.meli-email.org/meli/meli/commit/1c1be7d6c9bfc9f14c3a62ce464e1e15f2e6c4ec) `Add display_name(), display_slice(), display_name_slice() methods`
- [5dd71ef1](https://git.meli-email.org/meli/meli/commit/5dd71ef1cd93aebaadb0554eac692d0a0fa4aecd) `Upgrade JobsView component to new TUI API`
- [b5cc2a09](https://git.meli-email.org/meli/meli/commit/b5cc2a095f0268bb90cab150e903b0bbaffe1479) `Upgrade MailboxManager component to new TUI API`
- [ed8a5de2](https://git.meli-email.org/meli/meli/commit/ed8a5de2cb4b93ad766803d3590f7041f28cc419) `Re-enable EditAttachments component`
- [77a8d9e2](https://git.meli-email.org/meli/meli/commit/77a8d9e2c2094e84e06f5d624cb6f8afda24a400) `Make ModSequence publicly accessible`
- [64898a05](https://git.meli-email.org/meli/meli/commit/64898a0583e348fef3cd266a7196425e7015a871) `Make UIDStore constructor pub`

### Documentation

- [e4818803](https://git.meli-email.org/meli/meli/commit/e481880321fecef4978c02a5bb834652d8d77b46) `Various manpage touchups and URL updates`
- [38bca8f8](https://git.meli-email.org/meli/meli/commit/38bca8f8bc11eff28991bedac344bfebebdd22d7) `Mention use_oauth2=true for gmail oauth2`
- [660022ce](https://git.meli-email.org/meli/meli/commit/660022ce238ddda1de7aaac228c4d990bd643429) `Add mailaddr.7 manpage`
- [c5e9e676](https://git.meli-email.org/meli/meli/commit/c5e9e67604313acd73b33b7000a1ba59e38ced34) `Add historical-manpages dir`
- [5afc0785](https://git.meli-email.org/meli/meli/commit/5afc078587b86104dc5b615c1840dade64768a5f) `Update README.md, DEVELOPMENT.md and create BUILD.md`
- [d018f07a](https://git.meli-email.org/meli/meli/commit/d018f07aa51fc293bf696fa7d7beff8e59ac91a8) `Retouch manual pages`
- [3adba40e](https://git.meli-email.org/meli/meli/commit/3adba40e32a8a66271ea2a8f5ddf27858744ecd6) `Add macos manpage mirror url`

### Packaging

- [cd2ba80f](https://git.meli-email.org/meli/meli/commit/cd2ba80f8e5424be08421b4dcc5113977418f240) `Update metadata`
- [5f8d7c80](https://git.meli-email.org/meli/meli/commit/5f8d7c8039c0623b3950fd1a8eb566f943fc309d) `Update deb-dist target command with author metadata`
- [59c99fdc](https://git.meli-email.org/meli/meli/commit/59c99fdc79bb31fb42cb99d4b95613022396a499) `Update debian package metadata`
- [97eb6363](https://git.meli-email.org/meli/meli/commit/97eb6363756f18e91024dfd3f2407fdc70d6d9c3) `Add dpkg --print-architecture to deb filename`
- [7412c238](https://git.meli-email.org/meli/meli/commit/7412c238704c14b0007caeae9d0bc9d9e3db8abb) `Bump meli version to 0.8.5-rc.3`
- [500fe7f7](https://git.meli-email.org/meli/meli/commit/500fe7f7e43a4cf5b798593dd06713954189bacb) `Update CHANGELOG.md`
- [5ff4e8ae](https://git.meli-email.org/meli/meli/commit/5ff4e8ae68182db8d4535d8537d26a3f398c815b) `Run builds.yaml when any manifest file changes`
- [0a617410](https://git.meli-email.org/meli/meli/commit/0a617410ec1ce5f6fb43772e4ad43f45f58a7f4d) `Split test.yaml to test.yaml and lints.yaml`
- [3ba1603a](https://git.meli-email.org/meli/meli/commit/3ba1603af2a9e408659717b9c8dace7406a8b142) `Add manifest file only lints workflow`
- [1617212c](https://git.meli-email.org/meli/meli/commit/1617212c5b0948174155ece4a9d0584764bd7dac) `Add scripts/check_debian_changelog.sh lint`
- [c41f35fd](https://git.meli-email.org/meli/meli/commit/c41f35fdd55bf093656b68cc69eab4cf4b9a8ec4) `Use actions/checkout@v3`
- [876616d4](https://git.meli-email.org/meli/meli/commit/876616d45b7798131ecdda82bb90d1d481842f5c) `Use actions/upload-artifact@v3`
- [2419f4bd](https://git.meli-email.org/meli/meli/commit/2419f4bd40fb1a732cf1df42dde48ba8ca812072) `Add debian package build workflow`
- [10c3b0ea](https://git.meli-email.org/meli/meli/commit/10c3b0eabe1684699c775e03c4c58038ea7979af) `Bump version to 0.8.5-rc.1`
- [d16afc7d](https://git.meli-email.org/meli/meli/commit/d16afc7d8d9e2eddb81664673e9a4ef82da2e303) `Bump version to 0.8.5-rc.2`
- [da251455](https://git.meli-email.org/meli/meli/commit/da251455a0185e207e0ec2d51273f6ddbdb572a8) `Bump meli version to 0.8.5-rc.2`

### Miscellaneous Tasks

- [c4344529](https://git.meli-email.org/meli/meli/commit/c4344529e30b3385149d6dc3c1c4b34306a85491) `Add .git-blame-ignore-revs file`
- [f70496f1](https://git.meli-email.org/meli/meli/commit/f70496f14c2405dae3be932f0a3feffec50f34d4) `Add codemeta.json`
- [b3079715](https://git.meli-email.org/meli/meli/commit/b3079715f6d6b08a2ed3482bf718f5da8adac0ed) `Disable flakey test_smtp()`
- [8a95febb](https://git.meli-email.org/meli/meli/commit/8a95febb7807fa4a1b2f97ff2637da6fceab8284) `Set debuginfo=0 in test/lint builds`
- [81d1c053](https://git.meli-email.org/meli/meli/commit/81d1c0536bb15c8ba796fc36dddab7043a8eaaf4) `Add mandoc_lint.sh`
- [8de8addd](https://git.meli-email.org/meli/meli/commit/8de8addd11e97bfd9ec475bdf1f6907b4ff8da62) `Add cfg for musl builds`
- [70fc2b45](https://git.meli-email.org/meli/meli/commit/70fc2b455c5b6dbfbfc0f505e597942712298c7b) `Update nix dependency to 0.27`
- [fd64fe0b](https://git.meli-email.org/meli/meli/commit/fd64fe0bf87a75c628b93755f9ef923728df65ce) `Update codeberg.org URL`
- [30a3205e](https://git.meli-email.org/meli/meli/commit/30a3205e4fb0b9f6e578621c72160df5e4b8d09b) `Add clippy::doc_markdown`
- [c7aee725](https://git.meli-email.org/meli/meli/commit/c7aee7252587d2e42ca5df4a65731e183f7edaf2) `Add clippy::doc_markdown`
- [b8b24282](https://git.meli-email.org/meli/meli/commit/b8b24282a0dd098bea6500192bf5a74cdbecd8c3) `Update all instances of old domains with meli-email.org`
- [ae96038f](https://git.meli-email.org/meli/meli/commit/ae96038fbf5884d5f4e52d0b0488dedb6e5f4050) `Make unicode-segmentation a hard dependency`
- [255e9376](https://git.meli-email.org/meli/meli/commit/255e93764ad7218474e1b08d59d6808281b89b8b) `Update `linkify` dep from 0.8.1 to 0.10.0`
- [dedee908](https://git.meli-email.org/meli/meli/commit/dedee908d1e0b42773bade8e0604e94b14810e2d) `Update `notify` dep from 4.0.17 to 6.1.1`
- [c1c41c91](https://git.meli-email.org/meli/meli/commit/c1c41c9126005266f00d4979777718463dddf7b2) `Update README.md and add Codeberg mirror`
- [71f3ffe7](https://git.meli-email.org/meli/meli/commit/71f3ffe740276087f20d85d62440ef5d3fe426f6) `Update Makefile`
- [63a63253](https://git.meli-email.org/meli/meli/commit/63a63253d77f6e1b9a42ec55ecf0bbc45a011245) `Use type alias for c_char`
- [c751b2e8](https://git.meli-email.org/meli/meli/commit/c751b2e8450aa83b7a8f5e8afbeccadf333f74ba) `Re-enable conversations listing style`
- [3a709794](https://git.meli-email.org/meli/meli/commit/3a7097948308981204132a0eed2d28338f9d6b33) `Update minimum rust version from 1.65.0 to 1.68.2`
- [f900dbea](https://git.meli-email.org/meli/meli/commit/f900dbea468e822c5a510a72ecc6367549443927) `Use cargo-derivefmt to sort derives alphabetically`
- [e19f3e57](https://git.meli-email.org/meli/meli/commit/e19f3e572c0ac585a6c2023e50f8fd0bd2ea2dae) `Cargo-sort all Cargo.toml files`

## [v0.8.4](https://git.meli-email.org/meli/meli/releases/tag/v0.8.4) - 2023-11-22

### Fixed

- [ef30228e](https://git.meli-email.org/meli/meli/commit/ef30228e08efe6e36ab9858a5ba32876d6d8fdae) Fix failing test

### Miscellaneous Tasks

- [f81a1e23](https://git.meli-email.org/meli/meli/commit/f81a1e23382208390394be71e3aaa27ee505cb0f) Bump version to 0.8.4

## [v0.8.3](https://git.meli-email.org/meli/meli/releases/tag/v0.8.3) - 2023-11-22

### Added

- [3105a037](https://git.meli-email.org/meli/meli/commit/3105a0373b8754f37b326239c1cf7129fae06e1b) Add quit command

### Fixed

- [d3cbf184](https://git.meli-email.org/meli/meli/commit/d3cbf184e606d5b7ade9cfb125db01f45d7180ae) Add extra_submission_headers fields in composer form and autocomplete for Newsgroups
- [7aec5b8e](https://git.meli-email.org/meli/meli/commit/7aec5b8e78d80e7717a9aedd7344db6b108534f5) Fix SMTP example doc
- [f702dc22](https://git.meli-email.org/meli/meli/commit/f702dc220c9ab97ce0fddfae194d5e2935a20193) Fix new clippy lints.
- [688e39a6](https://git.meli-email.org/meli/meli/commit/688e39a67e6a467ca649acbe20b1f368fbc1e9f0) Fix clippy lints

### Changed

- [5a7919bb](https://git.meli-email.org/meli/meli/commit/5a7919bb03641be6d7bc5b9002d44e16ee358f12) Use ConversationsListing::format_date
- [0f3b5294](https://git.meli-email.org/meli/meli/commit/0f3b52945959b53c8d809eb434a91ec4c561b2d4) Hoist format_date() to ListingTrait method

### Refactoring

- [e1b55340](https://git.meli-email.org/meli/meli/commit/e1b55340fa258a2a7b118fd18c11614fb2b5e173) Show error description when TIOCGWINSZ ioctl fails
- [e95c275d](https://git.meli-email.org/meli/meli/commit/e95c275d68fe3dbd588046c110ae8b3fa966f6de) Remove duplicate end sequence
- [8a21be21](https://git.meli-email.org/meli/meli/commit/8a21be21775cb474a6b65e1c0bffd771c0df6f2f) Replace splice with truncate
- [2db021fa](https://git.meli-email.org/meli/meli/commit/2db021fa0a9a707cd7cdb6c8bf140bf5c8acf906) Remove regexp from default features
- [fa33a946](https://git.meli-email.org/meli/meli/commit/fa33a9468a16c50361353efa269fca79bd58e284) Move managesieve-client binary to tools/

### Miscellaneous Tasks

- [e88957ae](https://git.meli-email.org/meli/meli/commit/e88957ae6edfee7fabb41e9210f9d906866cda8d) Add extra_submission_headers field in MailBackendCapabilities struct
- [606f487f](https://git.meli-email.org/meli/meli/commit/606f487fc5e227f1727697a5911e27cbec174089) Add IRC channel badge
- [0e60bdf2](https://git.meli-email.org/meli/meli/commit/0e60bdf26eb842744f59257800ca8e30b1a43836) Add "iterator" feature to signal-hook
- [ac2a5dcd](https://git.meli-email.org/meli/meli/commit/ac2a5dcdd10d97f5ed9c8a8c83e1641b373dd31a) Add display() method for Address
- [43bfd413](https://git.meli-email.org/meli/meli/commit/43bfd4131d5cab39319d1943bcad46e929ec4d56) Update ahash dependency
- [af241d25](https://git.meli-email.org/meli/meli/commit/af241d25cbab20227a88ec4d557222cdeed98dde) Bump version to 0.8.3
- [7387b67e](https://git.meli-email.org/meli/meli/commit/7387b67eeee27aefbc4d20ca2a1d503aa0fb1838) Enable "static" build for C library dependencies by default
- [bfc78a08](https://git.meli-email.org/meli/meli/commit/bfc78a0803524e236bc883833838d3ad78918621) Replace CRLF with LF when editing
- [111a1160](https://git.meli-email.org/meli/meli/commit/111a1160adf2e0fef00a90350784307c859a198b) Bump version to 0.8.3

## [v0.8.2](https://git.meli-email.org/meli/meli/releases/tag/v0.8.2) - 2023-09-22

### Fixed

- [73b3ed55](https://git.meli-email.org/meli/meli/commit/73b3ed559d21dcc7cdee7f96119461e2447c1906) Fix forward dialog not workng
- [7888d8b2](https://git.meli-email.org/meli/meli/commit/7888d8b2a5dc977f0f18094a32dc73893a5cfc4f) Fix doc test compilation

### Changed

- [22525d40](https://git.meli-email.org/meli/meli/commit/22525d40fb48661f86657151e35fdf9c95c4b45e) Go to end when pressing next/page down for the second time
- [71474436](https://git.meli-email.org/meli/meli/commit/714744366f5e26fc1b6609e8e785d64489f9a68d) Revert 22525d40 behavior when sidebar not focused

### Miscellaneous Tasks

- [eb5d49c4](https://git.meli-email.org/meli/meli/commit/eb5d49c41ac58c5068011620c22e21b5fa115417) Use Self in self methods
- [3d85ca2e](https://git.meli-email.org/meli/meli/commit/3d85ca2edfca9abff4b3ffdd837b25e68c6586c2) Bump version to 0.8.2

## [v0.8.1](https://git.meli-email.org/meli/meli/releases/tag/v0.8.1) - 2023-09-13

### Added

- [6476985c](https://git.meli-email.org/meli/meli/commit/6476985ce6abbb9048ba5aec19f6c5144bfe89b7) Add Cross.toml for aarch64-unknown-linux-gnu builds
- [45d4f611](https://git.meli-email.org/meli/meli/commit/45d4f611b170d7b80afca5810c51fea1bf084c10) Add install-man cli subcommand to install manpages on your system
- [a4f0dbac](https://git.meli-email.org/meli/meli/commit/a4f0dbac26126c03886115e518b3cd2ede0b88cb) Add current working directory tracking to Context

### Fixed

- [49a38a23](https://git.meli-email.org/meli/meli/commit/49a38a23bf522a18e636385632cfe3533c4f525c) Fix invalid Type link references
- [85af5244](https://git.meli-email.org/meli/meli/commit/85af524458bc06421ac39689469474efb8164c1c) Fix invalid mailto() results when body field exists
- [c7825c76](https://git.meli-email.org/meli/meli/commit/c7825c76c3ac6be89f64f1f04afd9c0ca08bdf76) Handle dialog Esc in the parent component
- [dd4d0b79](https://git.meli-email.org/meli/meli/commit/dd4d0b79721d8cd5b29cdaca9cd01412974f2e13) Fix typo
- [c43aeb0e](https://git.meli-email.org/meli/meli/commit/c43aeb0eb103f2a8fd802f84eab56551c6e65418) Fix invalid address parse on folded values
- [7e3e9386](https://git.meli-email.org/meli/meli/commit/7e3e9386316ef344580d9e44edb3f8b0c196c3c5) Fix out-of-bounds draw when terminal is small
- [7e4ed2fa](https://git.meli-email.org/meli/meli/commit/7e4ed2fa107eca2ef309bcaa211440c315730b6c) Fix some out of bounds drawing.

### Changed

- [1b3bebe3](https://git.meli-email.org/meli/meli/commit/1b3bebe3049ae5c7cb2210ed95c355c9b5c709f8) Open earliest unread email instead of first in thread
- [49c36009](https://git.meli-email.org/meli/meli/commit/49c36009cec8c88d61d796162787990216bfeeab) Don't initialize entire thread at once
- [0a9c89b6](https://git.meli-email.org/meli/meli/commit/0a9c89b6b357fc3d002c3eb451fd67e7a49ce7f5) Add toggle_layout shortcut
- [64ba0459](https://git.meli-email.org/meli/meli/commit/64ba0459ee3652eaf451d10222853a898d85e337) Init cursor at To: header field
- [81974311](https://git.meli-email.org/meli/meli/commit/81974311c200b8ad66c0e626f8b8db6686e565ff) Show current number command buffer

### Refactoring

- [a337e226](https://git.meli-email.org/meli/meli/commit/a337e2269e584769314cdf325cdeb6e57cb0c622) Refactor module structure
- [b4f2f335](https://git.meli-email.org/meli/meli/commit/b4f2f3357613729e493e5f41a48def7610dc65aa) Remove deflate feature; make it a hard dependency
- [2dc29405](https://git.meli-email.org/meli/meli/commit/2dc29405868b9df0dfff25e341814526a478db00) Add feature to use cache instead of downloading unicode data
- [0132677f](https://git.meli-email.org/meli/meli/commit/0132677ff54a9618d3c59b08a188b73ae0c062c7) Introduce CommandError with context
- [3344a8db](https://git.meli-email.org/meli/meli/commit/3344a8dbf6b478a85d2b933fc1fa1a6001c600f4) Remove unnecessary Clone derives
- [b673af02](https://git.meli-email.org/meli/meli/commit/b673af02ac9e9d4be95daa2490ce24d0bc9b10d9) Move to crate root
- [54862f86](https://git.meli-email.org/meli/meli/commit/54862f8651cb7dfe3bca7f5924fe776b93ac6aee) Add hide_sidebar_on_launch option

### Miscellaneous Tasks

- [a615b470](https://git.meli-email.org/meli/meli/commit/a615b4701b7e852a9112b317e2e31997c6cbe82e) Embed xdg-utils crate
- [f0075b86](https://git.meli-email.org/meli/meli/commit/f0075b86cf636a3d39d4edf1ff6d58c112bbecf7) Show descriptive tab names for composer and threads
- [6d5ebb5b](https://git.meli-email.org/meli/meli/commit/6d5ebb5b04279fe6e4fbf598504cae2f012fa494) Split code into submodules, add better error reporting
- [63abf1e8](https://git.meli-email.org/meli/meli/commit/63abf1e890b93fcadf35f88b3dbea473c0d8f5cd) Update README.md
- [bb4d2000](https://git.meli-email.org/meli/meli/commit/bb4d20003690d72b62a66d46a1fc5ae914e2bf64) Unify toggle_* parsers
- [9b9c38f7](https://git.meli-email.org/meli/meli/commit/9b9c38f769abae0ff86e4b71e4db0ad65fdacfb4) Don't flood user with sqlite3 errors if db is corrupted
- [747e39bf](https://git.meli-email.org/meli/meli/commit/747e39bf55cfc19b6eeece3ca7c71bad98d92389) Add print-used-paths subcommand
- [39e99770](https://git.meli-email.org/meli/meli/commit/39e99770da4b51d0986a4b561fbe36b27d04565d) Use Context::current_dir() when saving files to relative paths
- [fe0a96f0](https://git.meli-email.org/meli/meli/commit/fe0a96f0855486207280430064a93cab94dffeb2) Update to 2021 edition
- [3944e4e6](https://git.meli-email.org/meli/meli/commit/3944e4e60e431247eefc0b3cf35af27fb011f37b) Update to 2021 edition
- [7eed8278](https://git.meli-email.org/meli/meli/commit/7eed82783a3dbac513e233be4f0bce06904fe8c8) Bump version to 0.8.1

## [v0.8.0](https://git.meli-email.org/meli/meli/releases/tag/v0.8.0) - 2023-08-29

### Added

- [36e29cb6](https://git.meli-email.org/meli/meli/commit/36e29cb6fd00c798ad83e3064e0ff78c8153dced) Add configurable mailbox sort order
- [81184b18](https://git.meli-email.org/meli/meli/commit/81184b182c5f5d65614653b817981fddc6a84ffa) Add extra_identities configuration flag
- [b716e438](https://git.meli-email.org/meli/meli/commit/b716e4383ea3163cabe760cd5512b7d70b218915) Add collapse option for mailboxes in sidebar menu
- [3d92b410](https://git.meli-email.org/meli/meli/commit/3d92b41075fc16214675cf141acd9c89fb6f5c49) Add cli-docs feature to the default set
- [104352e5](https://git.meli-email.org/meli/meli/commit/104352e5950598f4a659bd593d587910af8adc12) Add table UI widget
- [7d9cabb0](https://git.meli-email.org/meli/meli/commit/7d9cabb023b510e6175fd6b2523f0414a6da1f3f) Add mailbox manager tab
- [660bacb9](https://git.meli-email.org/meli/meli/commit/660bacb9262dac7457bd8c421cc70343a0db3cd5) Add `mailto` command to open composer with initial values from mailto template
- [3adf72ae](https://git.meli-email.org/meli/meli/commit/3adf72aed0772fea39fbd6cbaec680fb2995e92d) Add support for utf-7 encoding
- [d9c07def](https://git.meli-email.org/meli/meli/commit/d9c07def0f5db655aa11c5981d1419a336c3d91a) Add command to select charset encoding for email
- [8c671935](https://git.meli-email.org/meli/meli/commit/8c671935f9ad5bd2894c0ecdaec9c2f378e461ca) Add compose (pre-submission) hooks for validation/linting
- [96537e48](https://git.meli-email.org/meli/meli/commit/96537e48c5f5c8d54076ec5db76e94a499cbe1e6) Add {Timer,Component}Id wrapper types over Uuid
- [b5f205b7](https://git.meli-email.org/meli/meli/commit/b5f205b77b8911a1fb6019767bb026e5f4a7f79e) Add availability to use server_password_command in the nntp backend like in the IMAP backend
- [a5770c89](https://git.meli-email.org/meli/meli/commit/a5770c89f46b908d17d6eb4573c8337a952f99a8) Add Woodpecker-CI check pipeline
- [d4e605c0](https://git.meli-email.org/meli/meli/commit/d4e605c098ba13b8bc2d9f14d07ea45da38e9a2f) Add tagref source code annotations
- [cf9a04a5](https://git.meli-email.org/meli/meli/commit/cf9a04a5910c9d82e1acb10a2f4d40c2af0335ed) Add metadata to Jobs, and add JobManager tab
- [bb7e119a](https://git.meli-email.org/meli/meli/commit/bb7e119ade131e8fe1bcac39b616741af817808c) Add gitea CI workflows
- [1c79786e](https://git.meli-email.org/meli/meli/commit/1c79786ea210e53ee7d566455d83d74fe4699d28) Add scripts/make_html_manual_page.py
- [65e82d88](https://git.meli-email.org/meli/meli/commit/65e82d8896500e8ef586656e3bde4bc102b84aba) Add meli/README.md symbolic link

### Fixed

- [ce2068d3](https://git.meli-email.org/meli/meli/commit/ce2068d36bb5d8ad0bb8f886bc19cb4aab75c4e8) Fix background watch using JSON paths incorrectly
- [e9aaa7b0](https://git.meli-email.org/meli/meli/commit/e9aaa7b067903040acd7f3d7c685de94b3b98450) Use *const c_char instead of *const i8 for portability
- [aa3524dd](https://git.meli-email.org/meli/meli/commit/aa3524dd305f2cf293eaaf7120b812478255f79c) Fix tag not being removed in set_flags()
- [daa900ec](https://git.meli-email.org/meli/meli/commit/daa900ec9a566460833c020feba10933c0248162) Fix embed terminal in macos
- [7fca5f01](https://git.meli-email.org/meli/meli/commit/7fca5f01ef53069958403dd794ee0e5c310f4e45) Fix jmap build with isahc 1.7.2
- [ed3dbc85](https://git.meli-email.org/meli/meli/commit/ed3dbc85861ab61fee56077c7ba94306b0a96dc4) Fix crashes when listing is empty
- [824f614a](https://git.meli-email.org/meli/meli/commit/824f614a69e55a25d67832593cb8aadb9671e306) Fix HtmlView not being redrawn when parent is dirty
- [97ff3e78](https://git.meli-email.org/meli/meli/commit/97ff3e787fbfb5ff50e3ba787f067829509f7cd2) Only add toml files to the themes
- [9cb66ef8](https://git.meli-email.org/meli/meli/commit/9cb66ef818f6598eb779f931e201a8d38e86a484) Fix all clippy warnings in `meli` crate
- [0c0bee44](https://git.meli-email.org/meli/meli/commit/0c0bee4482d4fbfa675b97ca30405fdc77655936) Add missing .PHONY targets, fix missing tab indentation
- [a73885ac](https://git.meli-email.org/meli/meli/commit/a73885acb14cd94d4a6a54ebd5b39a001d7e21e1) Improve embed terminal
- [da9c80cc](https://git.meli-email.org/meli/meli/commit/da9c80ccfd7aa87842c2c3c089ba2b784a583ab6) Enhance SubjectPrefix with strip_prefixes_from_list() method
- [aa99b0d7](https://git.meli-email.org/meli/meli/commit/aa99b0d787463be4267913b801117bd4d2ea5003) Implement configurable subject prefix stripping when replying
- [cbe593cf](https://git.meli-email.org/meli/meli/commit/cbe593cf31308dcf549d7880eea2d82e5024dd73) Add configurable header preample suffix and prefix for editing
- [2de69d17](https://git.meli-email.org/meli/meli/commit/2de69d17f14e79ce2a35564d278b5e895d16a48f) Fix erroneous placement of newlnes for wrap_header_preamble suffix
- [94bd84b4](https://git.meli-email.org/meli/meli/commit/94bd84b45d53b0e0fae52198fbdc05179b87cccc) Fix clippy lints for `meli` crate
- [b138d9bc](https://git.meli-email.org/meli/meli/commit/b138d9bc6166b763febf035b50109d810e3c18c9) Fix some clippy lints
- [c6bdda03](https://git.meli-email.org/meli/meli/commit/c6bdda03cf451ab52a3d414cad1344bb32c82879) Fix notmuch error shown on any missing backend
- [16646976](https://git.meli-email.org/meli/meli/commit/16646976d75284665c1fa0d7b7e3e3cde3531d66) Fix reply subject prefixes stripping original prefix
- [88a1f0d4](https://git.meli-email.org/meli/meli/commit/88a1f0d4bc17b60f8f23ea71f33a81aee78f8769) Fix FETCH response parsing bug
- [59b95f83](https://git.meli-email.org/meli/meli/commit/59b95f83d2b388b30a3a855f68bf5952355597d7) Fix docs
- [282af86e](https://git.meli-email.org/meli/meli/commit/282af86e83807772f042b115af24ffe2e0575b9e) Fix NAME sections manual pages for correct whatis(1) parsing
- [bd22f986](https://git.meli-email.org/meli/meli/commit/bd22f986f0c06f6dae535733d484aa89f610ed46) Fix clippy lints
- [5ba7b2cd](https://git.meli-email.org/meli/meli/commit/5ba7b2cd7bb07abe8faafe5e45db6145b3f90bc9) Fix clippy lints for meli binary
- [7924aa8b](https://git.meli-email.org/meli/meli/commit/7924aa8bfe8f0fbcd557bb8bb3a9d3ebeab2220a) Fix compilation
- [b9030a68](https://git.meli-email.org/meli/meli/commit/b9030a684c0ad64951a388e49d5825c12b483fb4) Fix selection not appearing immediately and invalid motions
- [4f45b109](https://git.meli-email.org/meli/meli/commit/4f45b109745ebc29febc452b9bcb0cd88f131ffc) Fix tag updates not showing up right away
- [abc56eae](https://git.meli-email.org/meli/meli/commit/abc56eae431153d2e48f8b1eb3e0d2a140b600d8) Fix SEEN flag update hiding mail view momentarily
- [40c6647d](https://git.meli-email.org/meli/meli/commit/40c6647db83c5137b79c9bec233972a8a78aeb76) Fix multipart/related with main text/html part not displayed correctly
- [11140b4a](https://git.meli-email.org/meli/meli/commit/11140b4a76419a6f8c83db38823e83aeac8fbb98) Fix test output
- [3a10953f](https://git.meli-email.org/meli/meli/commit/3a10953f05ea4944a8a20c2c5d647d5862dca907) Update fix-prefix-for-debian.patch
- [939dc15e](https://git.meli-email.org/meli/meli/commit/939dc15e289e06a0fad72e44f9e91133892a4ec0) Fix melib tests
- [39d9c2af](https://git.meli-email.org/meli/meli/commit/39d9c2af3b7daf39c6aa7eab5f2d95f1b9c3a562) Fix test smtp server logic
- [34bb532e](https://git.meli-email.org/meli/meli/commit/34bb532e8d91c5f35bdc058821da63ac543ecfa6) Mention w3m dependency
- [b1a71887](https://git.meli-email.org/meli/meli/commit/b1a71887710153f0f98b25b2f224fbe37f7a6889) Clippy fixes
- [1f8ac228](https://git.meli-email.org/meli/meli/commit/1f8ac2287b960e0ed5c44dadbf68b924f035d321) Fix ftplugin location and add example mail.vim file
- [1eea8bab](https://git.meli-email.org/meli/meli/commit/1eea8bab77cc20fb911f13aa16322a217b36b06b) Fix `test_imap_fetch_response`.
- [daf42fd4](https://git.meli-email.org/meli/meli/commit/daf42fd456bad5ddf65ac515c2fb277896d1fea3) Fix build error with quote 1.0.28
- [6388bea9](https://git.meli-email.org/meli/meli/commit/6388bea9a063f776398ffc503fdb0789ce9af9f1) Fix &[u8] index in HeaderMap
- [c5ecacea](https://git.meli-email.org/meli/meli/commit/c5ecaceae1ab50a1c337f5cab9e97c0b061cb2d5) Fix some search criteria in Query type
- [27a4dcb9](https://git.meli-email.org/meli/meli/commit/27a4dcb916e0bed723490df9d82bfd7c83f10a83) Fix some rustdoc lints
- [fdc0861a](https://git.meli-email.org/meli/meli/commit/fdc0861ac0ac725e6e5031d120bd4682752c0267) Fix expanded_hash argument off by one error
- [0c0a678c](https://git.meli-email.org/meli/meli/commit/0c0a678cffec73940065923bb3837deb85075f9f) Fix overlay widgets not being reaped after Unrealize event
- [65179d48](https://git.meli-email.org/meli/meli/commit/65179d4816a39b0c92e9c6a981b491c60313634f) Fix cursor/widget focus scrolling logic
- [e64923ee](https://git.meli-email.org/meli/meli/commit/e64923eeaaf1fdf0ee485cceff0c57b2d43f165a) Fix debug_assert condition
- [5f29faa6](https://git.meli-email.org/meli/meli/commit/5f29faa640ebe7b14e76e56227a482207b8d952e) Clippy lint fixes
- [0b258a1f](https://git.meli-email.org/meli/meli/commit/0b258a1f058fa08b143a8e573883a4abe89dc7e1) Clippy lint fixes
- [ba7f5dce](https://git.meli-email.org/meli/meli/commit/ba7f5dce1c37c04768aa060b35f3803e6db3840e) Fix display of threaded conversations tree structure
- [1dc1d868](https://git.meli-email.org/meli/meli/commit/1dc1d86848eb6d187120bcaa00296f2b4e2025ca) Fix infinite loop bug
- [e8e49e74](https://git.meli-email.org/meli/meli/commit/e8e49e741b0f888d44da69f52aa3fff2e03e7ced) Fix wrong per message offset
- [e3dfeaad](https://git.meli-email.org/meli/meli/commit/e3dfeaad7e4f838af5fb2e6e398d3e1aa37fe511) Fix compilation error when building without `gpgme` feature
- [7998e1e7](https://git.meli-email.org/meli/meli/commit/7998e1e77ef057bab28434edefb79d7be6a4de33) Add missing LC libc constants for openbsd target_os
- [b5657201](https://git.meli-email.org/meli/meli/commit/b5657201db4828c6e61c52e7ce338ac1a6e6f9fc) Fix doctest compilation errors
- [c2ed3e28](https://git.meli-email.org/meli/meli/commit/c2ed3e283f6729ac7e112d00ae54dd99a2ada5e6) Fix Source::* view showing only envelope body
- [d93ee413](https://git.meli-email.org/meli/meli/commit/d93ee413a766f35a4ef88d9fc3ace9cf37d28dd1) Add timestamp_to_string_utc
- [6086a378](https://git.meli-email.org/meli/meli/commit/6086a3789d4d01818322dab1f1a9eb4c1f6a2b25) Fix libgpgme segfault error and re-enable gpg
- [ab418c1d](https://git.meli-email.org/meli/meli/commit/ab418c1d39d02840bc5c61996c1a5416e2f35464) Refresh documentation, fix encryption/signing
- [0219dc87](https://git.meli-email.org/meli/meli/commit/0219dc870798a16fd4d9f546d14c115f9e2c6bd8) Respect max_objects_in_get when fetching email
- [6280bc75](https://git.meli-email.org/meli/meli/commit/6280bc75e550332a73c1a51dd46475cd54cc0a34) Fix blob download URL formatting
- [2df73547](https://git.meli-email.org/meli/meli/commit/2df73547515fd3464e1fc2b88aa67462f583a8ec) Fix overflow substracts
- [8e698cab](https://git.meli-email.org/meli/meli/commit/8e698cabcfe58ddd566133ba2c33249c23180a74) Fix unreachable-pub and disjoint-capture lint errors
- [40d4ecef](https://git.meli-email.org/meli/meli/commit/40d4ecefa013caaa13af493233c693fb495360ca) Accept invalid (non-ascii) address comment text
- [4e654d2d](https://git.meli-email.org/meli/meli/commit/4e654d2d02044be7340b63f1250d37b2ca57b221) Limit LIST ACTIVE command length to 512 octets
- [84081f4e](https://git.meli-email.org/meli/meli/commit/84081f4ed7570dd8bcc23d90b9c4cbff55620636) Minor style fix
- [97d36868](https://git.meli-email.org/meli/meli/commit/97d3686815c011bb8f1d4e448f12b2294693730d) Use Happy Eyeballs algorithm Ꙭ
- [96f0b3e6](https://git.meli-email.org/meli/meli/commit/96f0b3e6b484c9cbb7eaddcaad2b59811b733545) Fix shortcut section order
- [64982b4c](https://git.meli-email.org/meli/meli/commit/64982b4cab0b0c2d396cb5dcf7add6f268fd4551) Fix page{up,down} event bubbling up
- [8551e1ba](https://git.meli-email.org/meli/meli/commit/8551e1ba0b4fa6d9587bbb249f11e9b80d24e4d3) Fix new 1.72 default clippy lints

### Changed

- [8563bccd](https://git.meli-email.org/meli/meli/commit/8563bccd1b6d48dc06dd521f77228c3cbecf7613) Don't cache CellBuffer, only row info
- [0f6f3e30](https://git.meli-email.org/meli/meli/commit/0f6f3e30c67f209e0b5e03d2dd2e1e48180d9855) Add IMAP config in config parse test
- [ce269c64](https://git.meli-email.org/meli/meli/commit/ce269c64e16db344f0e65461e56dbced2f1a4d64) Don't fail on `server_password_command`
- [9dc4d405](https://git.meli-email.org/meli/meli/commit/9dc4d4055cb2f854e835748315677bf4a2db2012) Add focus_{left,right} shortcuts to switch focus
- [4b96bd59](https://git.meli-email.org/meli/meli/commit/4b96bd591f18bf7c8a3c922d469b81072d1782a2) Add ColorCache constructor to deduplicate code
- [c06c3f58](https://git.meli-email.org/meli/meli/commit/c06c3f589315f017a412f31d80559a5a734d7b89) Draw gap between list and mail view
- [c9d26bb4](https://git.meli-email.org/meli/meli/commit/c9d26bb4158e2f423c795f82bcb2c91a0f0c46ec) Add configurable custom hooks with shell commands
- [02e86d1f](https://git.meli-email.org/meli/meli/commit/02e86d1fade9faefc14b890e3cec8ed2255bb839) Check for subject overflow on draw
- [8cab9d9d](https://git.meli-email.org/meli/meli/commit/8cab9d9da8710257f2b62832bfac802c2a35b368) Add option to hide consecutive identical From values inside a thread
- [363f4930](https://git.meli-email.org/meli/meli/commit/363f4930994d1d2e88220878b3848f176b8c5f97) Add {previous,next}_entry shortcuts to quickly open other mail entries
- [342df091](https://git.meli-email.org/meli/meli/commit/342df091a076bce1f8477dabbad193312d8cdd67) Don't set all thread to seen when opening a thread entry
- [74e15316](https://git.meli-email.org/meli/meli/commit/74e15316dbbf67254023e619924e522f80e77cb9) Open message/rfc822 attachments in subview instead of new tab
- [369c1dbd](https://git.meli-email.org/meli/meli/commit/369c1dbdac9842746270a3d3c5bf7ed2205cb644) Show `open` command in status bar
- [519257b0](https://git.meli-email.org/meli/meli/commit/519257b08f7029fe71efd2f61ab3a29a4b43b862) Add relative_menu_indices setting for menubar
- [8abc9358](https://git.meli-email.org/meli/meli/commit/8abc9358a70465b12a11168be1718ab06479d6e2) Add newline after Version: 1 header
- [561ba9c8](https://git.meli-email.org/meli/meli/commit/561ba9c87b57e1012ad89bde08506a2beacb7fff) Add relative_list_indices setting for thread listing
- [52874f9a](https://git.meli-email.org/meli/meli/commit/52874f9a97a4799fcff2e14c43cafe9692f21cb6) Cancel previous jobs on MailView drop/update
- [9037f084](https://git.meli-email.org/meli/meli/commit/9037f08495894c15a7817594ba91e0d5561c6e69) Replace hardcoded Key::{Home,End} values with shortcut values
- [31aa9ad2](https://git.meli-email.org/meli/meli/commit/31aa9ad29e33f285314d0d320a02f00071f61282) Autogen mbox filename when exporting mail to directories

### Refactoring

- [330a2b20](https://git.meli-email.org/meli/meli/commit/330a2b20ed492f6b6ea86c196d43d67430487faa) Flush stdout in Ask() after printing
- [340d6451](https://git.meli-email.org/meli/meli/commit/340d6451a330861af09fd02231c17ba4168d9654) Add config setting for sidebar ratio
- [d0de0485](https://git.meli-email.org/meli/meli/commit/d0de04854ec4770b54e4d8303a9b8ab9eb5d68b0) Add {in,de}crease_sidebar shortcuts
- [f5dc25ae](https://git.meli-email.org/meli/meli/commit/f5dc25ae0d5b8d6fb15a534fa49557385d6894d0) Check that all conf flags are recognized in validation
- [d3e62e3d](https://git.meli-email.org/meli/meli/commit/d3e62e3d74bdc55872bbdf92c01d18aa00b0affd) Use conf shortcuts for scroll {up, down}
- [23c23556](https://git.meli-email.org/meli/meli/commit/23c2355662d589c091dd3c86c8d91c7988eb941c) Fill and align shortcut table columns
- [5823178c](https://git.meli-email.org/meli/meli/commit/5823178cc26f66ba902a901522f0506b4348b22e) Add test that looks in source code for invalid theme key references
- [9205f3b8](https://git.meli-email.org/meli/meli/commit/9205f3b8afe28ef3a68959d590ed967946a5d622) Handle a per account mail order parameter
- [d921b3c3](https://git.meli-email.org/meli/meli/commit/d921b3c3209ff7fe865b5a3b90e20098b3ff211f) Use mail sorting parameters from config
- [f4e0970d](https://git.meli-email.org/meli/meli/commit/f4e0970d46e3ec73d684e2ddcc5011f61e87314d) Add ability to kill embed process
- [bde87af3](https://git.meli-email.org/meli/meli/commit/bde87af3877d4a0b071e331c93a07e0acf51bf7a) Refactor filter() method in Listing trait
- [a42a6ca8](https://git.meli-email.org/meli/meli/commit/a42a6ca868e4590a8b93560737173e80993ecaec) Show notifications in terminal if no alternative
- [eb5949dc](https://git.meli-email.org/meli/meli/commit/eb5949dc9bbcf05f86c58b3c93d1066204313e2a) Switch summary<->details identifiers
- [8c7b001a](https://git.meli-email.org/meli/meli/commit/8c7b001aa5d4cb6bbaf438f3f47cd91cc2fd6833) Add `thread_subject_pack` command to pack different inner thread subjects in entry title
- [388d4e35](https://git.meli-email.org/meli/meli/commit/388d4e35d65f8f770526c4c5f44767c55eda23f8) Add in-progress messages while connecting in IMAP
- [787c64c2](https://git.meli-email.org/meli/meli/commit/787c64c2da8af5cc0dafcb92c1d3bea6b54f3659) Remove expect()s from create_config_file()
- [b87d54ea](https://git.meli-email.org/meli/meli/commit/b87d54ea3f3f077b6330e798263be6a3d33b3b9c) Impl Into<BTreeSet<EnvelopeHash>> for EnvelopeHashBatch
- [e450ad0f](https://git.meli-email.org/meli/meli/commit/e450ad0f9cbc2d215a8f03d2d39260abe19fb5af) Remove unused struct
- [c54a31f7](https://git.meli-email.org/meli/meli/commit/c54a31f7cca728eec87f7cd670a4baec37dc919a) Break line for error messages
- [7935e49a](https://git.meli-email.org/meli/meli/commit/7935e49a00190cc7f2057abe353739c8dad4f74d) Check properly if mailbox request is an error
- [117d7fbe](https://git.meli-email.org/meli/meli/commit/117d7fbe046fe23c400a925ccba7317d8a1d3f08) Make private fields public
- [ffb12c6d](https://git.meli-email.org/meli/meli/commit/ffb12c6d1ae9a774de22a25d38bc6714a435c7ad) Make all public struct fields public
- [46a038dc](https://git.meli-email.org/meli/meli/commit/46a038dc68093b28b69c3af38de4dd09431efae2) Remove interactive messages when #[cfg(test)]
- [803d3414](https://git.meli-email.org/meli/meli/commit/803d3414fd73743ff5bfc0fefe5e3d76d88e58cb) Implement some rfc5804 commands
- [b776409d](https://git.meli-email.org/meli/meli/commit/b776409d6c9caec3732bada9e25637c2676af3b8) Add thread, env hash index fields
- [cc439b23](https://git.meli-email.org/meli/meli/commit/cc439b239ae27ae84fbcf50fbd82ec591c147c94) Add RowsState struct
- [db227dea](https://git.meli-email.org/meli/meli/commit/db227dea34caa747e136500356fddf95a91002e6) Add error messages if `mandoc`,`man` binaries are missing
- [ee9d458b](https://git.meli-email.org/meli/meli/commit/ee9d458b05ffa0214a4526daf1423916830526bc) Implement mailbox {un,}sub actions
- [7af89359](https://git.meli-email.org/meli/meli/commit/7af893597f5a3f3261bfff47dae0723bf1b17e53) Replace use of Self::DESCRIPTION with Shortcuts struct consts
- [eaecc5ea](https://git.meli-email.org/meli/meli/commit/eaecc5ea12f4a5ebe309d5654509c0771bbdc2f1) Remove hardcoded major .so version for non linux/macos target_os
- [f63ce388](https://git.meli-email.org/meli/meli/commit/f63ce388f7774ea015fdaa2362202c33f3ddacd4) Move ManageMailboxes to Tab Actions
- [3c847ad2](https://git.meli-email.org/meli/meli/commit/3c847ad26afcc4a4cdcfbdbf70f35be57d0da1ab) Add beginning of sieve parser
- [5443b7e8](https://git.meli-email.org/meli/meli/commit/5443b7e8f300a0084abde7354360ecbe909178bb) Remove literal_map() parse combinator
- [12cb717b](https://git.meli-email.org/meli/meli/commit/12cb717bda186b0ebdda18e2215e30b1426fb08a) Add server_password_command to jmap
- [428f752b](https://git.meli-email.org/meli/meli/commit/428f752b20cdb1c8ab01e7f3119001cfafca8ef1) Remove obsolete crate::components::mail::get_display_name()
- [91557c2c](https://git.meli-email.org/meli/meli/commit/91557c2c4366b481e80943e94f661c8b47150571) Prevent list blank when refreshing account
- [d332e457](https://git.meli-email.org/meli/meli/commit/d332e4578d69c4371418fb2bb3c0d75e1960e01f) Add proper Display impl for HeaderName
- [f537c249](https://git.meli-email.org/meli/meli/commit/f537c24909d13a53a95b43e265e4cb4c013334ac) Move text field to its own module
- [d33f9d54](https://git.meli-email.org/meli/meli/commit/d33f9d54c708699386a3f32e4056ccab6c68528b) Remove unreachable!() in Key::serialize
- [330887c4](https://git.meli-email.org/meli/meli/commit/330887c4f5bad5357508b9fa6f723e45ab307d2a) Introduce imap-codec.
- [4da53669](https://git.meli-email.org/meli/meli/commit/4da5366959145e166c40297abfdf1876e5addc50) Remove bincode dep, use serde_json for sqlite3 values
- [155fb41b](https://git.meli-email.org/meli/meli/commit/155fb41b93708ef8793250f9dea611bc317a86d5) Remove unused Component::set_id method
- [575509f1](https://git.meli-email.org/meli/meli/commit/575509f1edc756ad218bb76cf74460d83009c851) Move mail view to listing parent component
- [6858ee1f](https://git.meli-email.org/meli/meli/commit/6858ee1fab3bcddbda7335f49c30f36153e8d4b7) Move subcommand handling to its own module
- [b0e867eb](https://git.meli-email.org/meli/meli/commit/b0e867eb68dc3dba96de79f7481989187fa12df4) Move src to meli/src
- [48a10f72](https://git.meli-email.org/meli/meli/commit/48a10f724171bfae702b7b40438189adbbe75079) Remove unused BackendOp::fetch_flags() method
- [073d43b9](https://git.meli-email.org/meli/meli/commit/073d43b9b869fc9d46c5195c31ad6e7806cf486c) Move data files to data subdir
- [1e084c1d](https://git.meli-email.org/meli/meli/commit/1e084c1d854ed7efb2254f9e8d52ac13d8badffa) Move backends out of the backends module
- [a5446975](https://git.meli-email.org/meli/meli/commit/a5446975c2423654dea9551474a880e94ebdc006) Move braille and screen to their own module files
- [005bf388](https://git.meli-email.org/meli/meli/commit/005bf3881ec59d53e4f16473fb3b1857487dae23) Move components/utilities -> utilities
- [64ab65dd](https://git.meli-email.org/meli/meli/commit/64ab65ddffe3341bca775acb2289ee00e771fdb0) Move components/contacts -> contacts
- [7c9a4b4b](https://git.meli-email.org/meli/meli/commit/7c9a4b4b7c366c967a3378098d210124712fd293) Move components/mail -> mail
- [df638cce](https://git.meli-email.org/meli/meli/commit/df638cceec6016760037b650a77143a07cd1e738) Remove stale failing doc code example
- [da8e8104](https://git.meli-email.org/meli/meli/commit/da8e81044833975cadb08db836795a389c142e9c) Remove leftover debug prints
- [a1e70061](https://git.meli-email.org/meli/meli/commit/a1e7006186474f55cf4a14f53dbd32bdf8ca5993) Move Sort{Order,Field} to utils mod
- [66c21ab1](https://git.meli-email.org/meli/meli/commit/66c21ab1734bfbf4e604da505f6b6109008fd7c2) Move StandardHeader to its own module
- [946309c6](https://git.meli-email.org/meli/meli/commit/946309c6f3bbc59b53dc2b05732b40f3d445fd9f) Do some small parser refactoring
- [b95f7783](https://git.meli-email.org/meli/meli/commit/b95f778335bebd480f69fe66fabec4f8a6e2b587) Move JmapSession to its own module

### Documentation

- [a866b294](https://git.meli-email.org/meli/meli/commit/a866b29499b44032545df4941b6cfec4ee2db8bb) Update valid shortcut entries from src/conf/shortcuts.rs
- [f76f4ea3](https://git.meli-email.org/meli/meli/commit/f76f4ea3f7416a4a641d5891f19927aa354a3247) Add meli.7, a general tutorial document
- [5fa4b626](https://git.meli-email.org/meli/meli/commit/5fa4b6260c60409579fe964970719f9ab60482cc) Add more screenshots
- [7c711542](https://git.meli-email.org/meli/meli/commit/7c7115427dd5f6320a4305df3dc88a8567829720) Complete guide document
- [30cc5d3d](https://git.meli-email.org/meli/meli/commit/30cc5d3d0220452630780c3238f393b9e1f2b93a) Add edit-config in manpages
- [24103f33](https://git.meli-email.org/meli/meli/commit/24103f3310ca533791bdd07643fdb23a10c6031d) Add external-tools.md document
- [b6c93e49](https://git.meli-email.org/meli/meli/commit/b6c93e49f2af3001b206a288edea02c58e14aa5b) Add use_tls option in IMAP connection settings
- [34a54d3c](https://git.meli-email.org/meli/meli/commit/34a54d3c05efc3b56154179111c3e39e0f3fd8b1) Add some `TODO([#222](https://git.meli-email.org/meli/meli/issues/222))`s.

### Packaging

- [671ce9f6](https://git.meli-email.org/meli/meli/commit/671ce9f694a8e941826472caad8051998540bb1f) Add missing build dependencies

### Miscellaneous Tasks

- [25805229](https://git.meli-email.org/meli/meli/commit/2580522931fb29442598ac8932a13eaeb577bace) Log vcard parsing failures
- [5f003a31](https://git.meli-email.org/meli/meli/commit/5f003a31be95a3877d1006f8a22e424a1183163d) Parse vCards with just LF instead of CRLF line endings
- [d8e9a005](https://git.meli-email.org/meli/meli/commit/d8e9a00563c023abb0ff75aaa4ba3fa92626c5ce) Add quoted REFERENCES field in parsing of responses
- [81d12656](https://git.meli-email.org/meli/meli/commit/81d1265601c299dee6405f3f9b4e81f89d3cfe29) Escape IMAP passwords properly
- [0d8bedd2](https://git.meli-email.org/meli/meli/commit/0d8bedd2d5d3eb8eee831e75d1e14d45beefb847) Make is_online() await for connection
- [d4b690d5](https://git.meli-email.org/meli/meli/commit/d4b690d5d3a7f6a6b57afd7a6177db0db20a9c94) Send password as byte literal on LOGIN
- [2eb22a29](https://git.meli-email.org/meli/meli/commit/2eb22a290abb3f37bc77c3bc2771edfb60a1c314) Stop hardcoding certain component colors
- [2c23ca34](https://git.meli-email.org/meli/meli/commit/2c23ca34cdee769a0f78a0b0ef934e5f20dd9567) Update most Cargo dependencies
- [721891c2](https://git.meli-email.org/meli/meli/commit/721891c2955e9f5e223949bde2dd43604cec8390) Update nom dependency
- [4fdc90b3](https://git.meli-email.org/meli/meli/commit/4fdc90b31ea56c046dfe5bf9bee0a118f9c03db1) Use `open` instead of `xdg-open` in macos
- [9558b2ae](https://git.meli-email.org/meli/meli/commit/9558b2ae921aa35076f58d68b5898334a2797685) Parse Cp1253 as windows1253 encoding
- [6a843d49](https://git.meli-email.org/meli/meli/commit/6a843d49830f8c70f510c4232ea63eb204d35319) Export list_mail_in_maildir_fs() function
- [d6355a30](https://git.meli-email.org/meli/meli/commit/d6355a3043ec0b4b2a3e1c3fbb0ed66d2e87e7f4) Impl Debug for ParsingError
- [dc5afa13](https://git.meli-email.org/meli/meli/commit/dc5afa13dbea4da042c35e12291c5b5a2846c3ff) Use osascript/applescript for notifications on macos
- [e6d6e1f5](https://git.meli-email.org/meli/meli/commit/e6d6e1f588db9793e822cdbb1ce2edb2959170c6) Don't unwrap if pseudoterminal creation fails
- [ca84906d](https://git.meli-email.org/meli/meli/commit/ca84906d7ddb1351643998efaa56086e3ba9cf8e) Escape all quotes in applescript on macos
- [4a79b202](https://git.meli-email.org/meli/meli/commit/4a79b2021d2fb3edd046197b44b702bdb468fc5e) Update dependency versions
- [e29041f7](https://git.meli-email.org/meli/meli/commit/e29041f73354c59ef95916edd75e6ca7876e3c3a) Rename src/bin.rs to src/main.rs
- [7650805c](https://git.meli-email.org/meli/meli/commit/7650805c60cec2fe09cd2a59cb665731f5cca140) Bring stripped binary size down to 7MiB
- [ca488968](https://git.meli-email.org/meli/meli/commit/ca48896865778df2c79bc1d13f03b5f56136304c) Add strip option to profile.release
- [10497952](https://git.meli-email.org/meli/meli/commit/10497952f718b49f3a247741a64361f855b2d4f7) Wrap stdout in BufWriter
- [29042aba](https://git.meli-email.org/meli/meli/commit/29042aba593210f3be73010908d5092951b3b1a1) Add mbox date format parse
- [480000eb](https://git.meli-email.org/meli/meli/commit/480000ebbb67a80181fd27762ca649acf13df0f3) Show error if account directory does not contain ".notmuch" subdirectory
- [a484b397](https://git.meli-email.org/meli/meli/commit/a484b397c68fd126c17073ac9c9f02432c413341) Show informative error messages if libloading fails
- [4a20fc42](https://git.meli-email.org/meli/meli/commit/4a20fc42e1f5cad325d5aa439d1baab210aceed8) Update CHANGELOG.md
- [a72c96a2](https://git.meli-email.org/meli/meli/commit/a72c96a26afe9e54a0fcadb8c43448f1fdc09ce9) Add 8BITMIME support to smtp client
- [3c0f5d82](https://git.meli-email.org/meli/meli/commit/3c0f5d8274d8039b1a2c928f99194835bca7b83a) Add BINARYMIME support to smtp client
- [36883692](https://git.meli-email.org/meli/meli/commit/36883692782ed2355a0ec12ccf9f82aa2edcc8c1) Add smtp test
- [9cbbf71e](https://git.meli-email.org/meli/meli/commit/9cbbf71e0f8f9115e9e043982f20045cfc550eb7) Add DecodeOptions struct for decoding
- [0df46a63](https://git.meli-email.org/meli/meli/commit/0df46a63ec6e30983480f0eb50c8da3f74b4f0b3) Show error if sqlite3 search backend is set but doesn't exist
- [a7a50d30](https://git.meli-email.org/meli/meli/commit/a7a50d3078cb7466ab341ddfc30a80c7b1f8dfdb) Box<_> some large fields in biggest types
- [d8d43a16](https://git.meli-email.org/meli/meli/commit/d8d43a16fef045a2116ff126e7b6e27817b526fc) Add html_open config setting
- [0ed10711](https://git.meli-email.org/meli/meli/commit/0ed10711ef542cc13eaaef809fa557468b3d6696) Add new_mail_script option
- [c3fdafde](https://git.meli-email.org/meli/meli/commit/c3fdafde3b69c0abc78a62926e0c32fc3dd602d6) Documentation touchups
- [347be543](https://git.meli-email.org/meli/meli/commit/347be54305c60350b055a1da3a1abfa4d33d3f22) Add NetworkErrorKind enum
- [0c08cb73](https://git.meli-email.org/meli/meli/commit/0c08cb737ceaa5c738712905c7d57f956d449ed0) Mark mailboxes as subscribed on personal accounts
- [129573e0](https://git.meli-email.org/meli/meli/commit/129573e0fd9b42ebf14c2de176e65b92bf8479bd) Rename root_path to root_mailbox
- [7e09b180](https://git.meli-email.org/meli/meli/commit/7e09b1807ffa9bae54da35b02c83b5aaee455819) Replace _Ref deref unwraps with expect()
- [55ed9624](https://git.meli-email.org/meli/meli/commit/55ed962425ba25d2317946705ff6861a77eb770f) Use server_url instead of server_hostname + server_port in config
- [0ef4dde9](https://git.meli-email.org/meli/meli/commit/0ef4dde9392452f7cf7f18294f747fc6e0babb8d) Wrap serde_json deserialize errors in human readable errors
- [dd0baa82](https://git.meli-email.org/meli/meli/commit/dd0baa82e9789da23c8f9b06925776c7f80e2568) Spawn user-given command strings with sh -c ".."
- [3697b7d9](https://git.meli-email.org/meli/meli/commit/3697b7d960cc9dbe602fa84f861cea854b600b73) Don't use LC_ category in place of LC_ masks in libc calls
- [6d20abdd](https://git.meli-email.org/meli/meli/commit/6d20abdde7b4cec6ec1af7c097f01042ea05cfbb) Add #[allow(deref_nullptr)] in bindgen tests
- [17b42b1a](https://git.meli-email.org/meli/meli/commit/17b42b1a6c721fb2e369c2a300867c8db2beb959) Add json deserialization tests
- [64346dd3](https://git.meli-email.org/meli/meli/commit/64346dd3fe0ef40025ec6fdb01d18eb38f7e7f65) Add map_res, quoted_slice, is_a, alt, take, take_literal
- [56fc43bc](https://git.meli-email.org/meli/meli/commit/56fc43bcf869a867455b44d007b9d3d17422bc8d) Add As{Ref,Mut} impls for RwRef{,Mut}
- [63179841](https://git.meli-email.org/meli/meli/commit/631798413659a320dcd9574e0bca7b7d75cc8d6c) Add --bin flag to meli cargo build target
- [ded9adde](https://git.meli-email.org/meli/meli/commit/ded9adde614ac3d38045fa97a0f5144b80855fe7) More descriptive "Unimplemented" messages
- [2224a710](https://git.meli-email.org/meli/meli/commit/2224a7100f9bc6c44bc66117a88556003e74186e) Reset imap cache on init error
- [252d2bdf](https://git.meli-email.org/meli/meli/commit/252d2bdf2f12c8954f8b299000bbde6219d25335) Replace hardcoded /bin/false with 'false'
- [2427b097](https://git.meli-email.org/meli/meli/commit/2427b097c5c40f3212a105cb40f913c9860ae2a8) Make tag_default background lighter on light theme
- [7382e301](https://git.meli-email.org/meli/meli/commit/7382e30160a934ce97dd73c1be44640d5b4a4c75) Convert EnvelopeHash from typedef to wrapper struct
- [259aeb00](https://git.meli-email.org/meli/meli/commit/259aeb00877557ee85b5cc555d50e605b85b3109) Convert {Account,Mailbox}Hash from typedef to wrapper struct
- [5634f955](https://git.meli-email.org/meli/meli/commit/5634f9555315deb2d39ed8fce577a35f4d535ac1) Rename MeliError struct to Error
- [7606317f](https://git.meli-email.org/meli/meli/commit/7606317f24d076bdc7db873c2b15811728ed946a) Add support for virtual mailbox hierarchy
- [2878bbb8](https://git.meli-email.org/meli/meli/commit/2878bbb8c887275d26264bf7201a632161c4048a) Add parser for mutt alias file
- [de2f46fe](https://git.meli-email.org/meli/meli/commit/de2f46fe611726a445c1e06cbc35343e716aa335) Rustfmt changes
- [f9ac9b60](https://git.meli-email.org/meli/meli/commit/f9ac9b607a2bd01e42c81cfab3c933df28ff1676) Temporarily disable libgpgme functions because of a bug
- [256a3e25](https://git.meli-email.org/meli/meli/commit/256a3e252e2e4db9af9a04c7df1a52eeaf2bbfc9) Update minimum supported rust version
- [fbc1007f](https://git.meli-email.org/meli/meli/commit/fbc1007ff4f41bac888a1b53c156feec4f795403) Deserialize `null` to empty vec for messageId
- [d7ec97f0](https://git.meli-email.org/meli/meli/commit/d7ec97f03bc0e815e160a142f871dc764d416af1) Small rustfmt change
- [2447a2cb](https://git.meli-email.org/meli/meli/commit/2447a2cbfeaa8d6f7ec11a2a8a6f3be1ff2fea58) Avoid relying on hardcoded hash values
- [d679a744](https://git.meli-email.org/meli/meli/commit/d679a74450b35724301c81da1644bcedb1c54045) Implement Bearer token authentication
- [47e6d5d9](https://git.meli-email.org/meli/meli/commit/47e6d5d935a2b5124efbe847dac885b859200469) Add edit-config CLI subcommand that opens config files on EDITOR
- [3a02b6fb](https://git.meli-email.org/meli/meli/commit/3a02b6fb8024e6bb046fc167e7527aad1b192202) Mention how to override w3m with html_filter
- [85d4316a](https://git.meli-email.org/meli/meli/commit/85d4316a6a8703ac3e4923cf99ce8c4bb22bb4ae) Replace old logging module with the `log` create
- [1f1ea307](https://git.meli-email.org/meli/meli/commit/1f1ea307698a5a7f62f5ab2ea1594aef4d8f48a8) On draw() set dirty on return
- [77020e0c](https://git.meli-email.org/meli/meli/commit/77020e0c19873b8053321132ff5b58181c567fcd) Update CHANGELOG.md
- [682ea554](https://git.meli-email.org/meli/meli/commit/682ea5547e380deeb215503b39c8aa66c65b3cac) Add `.idea` (CLion) to `.gitignore`.
- [f63f6445](https://git.meli-email.org/meli/meli/commit/f63f6445addeccee1a6b830f1c101a043612ea4e) Improve error message when `m4` executable is missing.
- [cc27639f](https://git.meli-email.org/meli/meli/commit/cc27639fca0dcb3a5ff9fceef8666dbbf047adaa) Use Envelope attachments when editing and don't add already existing headers
- [30866f75](https://git.meli-email.org/meli/meli/commit/30866f752b21802b64ce7d2e02c9962c1091c9d8) Bypass rustfmt bug.
- [235fceaf](https://git.meli-email.org/meli/meli/commit/235fceaf2168af50c3804cecfbf69e64ff42598c) Add standard heeder constants in email::headers
- [aebff3d3](https://git.meli-email.org/meli/meli/commit/aebff3d3d9864b8854aba5e7f43a61d515e8057f) Implement mailto RFC properly
- [954329d8](https://git.meli-email.org/meli/meli/commit/954329d848a5b3e73fca50ed1db9859118bed6dd) Set file extensions to temp files, use `open` in macos
- [58889bca](https://git.meli-email.org/meli/meli/commit/58889bcadd44d6aec2eddd17cf5ecb1e07531cbe) Add show_extra_headers option
- [23d95973](https://git.meli-email.org/meli/meli/commit/23d95973d4f574fe431441df97ceaef0e3e4762f) Add search.rs module
- [6bf1756d](https://git.meli-email.org/meli/meli/commit/6bf1756de844386ba312d15109ae29951896147b) Implement more search criteria in Query type
- [299c8e0f](https://git.meli-email.org/meli/meli/commit/299c8e0f993c4ac88005a5c9e708d9e214b20ac1) Restructure pub use melib::* imports
- [f8623d4b](https://git.meli-email.org/meli/meli/commit/f8623d4b2c386f51f1d11a23900503d8165ac9f3) Implement more ResponseCode cases
- [b92a80a2](https://git.meli-email.org/meli/meli/commit/b92a80a23afb96fbd63031704e4656cc8a00526c) Resync even if UIDVALIDITY is missing from cache
- [bf615e7d](https://git.meli-email.org/meli/meli/commit/bf615e7d933b474942d421eafc1015aeb28f8516) Check for case when envelope has its own message id in References and In-Reply-To
- [e0257c9d](https://git.meli-email.org/meli/meli/commit/e0257c9d8d6f234f71852a0080d443b063d5e6d7) Run cargo-sort
- [d7e6b40b](https://git.meli-email.org/meli/meli/commit/d7e6b40b7e1f501fdaaba54880e9c7a4b0e01288) Auto re-index sqlite3 database if it's missing
- [cd85d833](https://git.meli-email.org/meli/meli/commit/cd85d83324a009ea4b86ac22af395145a9e999ab) Replace timestamp with Date value in message/rfc822 Display
- [579372b4](https://git.meli-email.org/meli/meli/commit/579372b4a75e39c9e84010de16d7d46294bed04a) Improve readability of `Envelope`.
- [6c6d9f4b](https://git.meli-email.org/meli/meli/commit/6c6d9f4b4e0d16b5a73ae8e2a2fb2a6f124df7e6) Improve ordering of `flag_impl!`s.
- [8f14a237](https://git.meli-email.org/meli/meli/commit/8f14a2373e16b9b4af22f9388fae84235dd08123) Put imap-codec logic under the imap_backend feature
- [fd0faade](https://git.meli-email.org/meli/meli/commit/fd0faade066a18466e683361211bba569956bf63) Add connection instance id string for debugging in logs
- [5c9b3fb0](https://git.meli-email.org/meli/meli/commit/5c9b3fb0448fa3689ff33faba3dde03c49347f61) Impl Component for Box<dyn Component>
- [45bac6eb](https://git.meli-email.org/meli/meli/commit/45bac6eb16a5a093193d5beb4d80040ce161304a) Tidy up use of debug!
- [5699baec](https://git.meli-email.org/meli/meli/commit/5699baecfba9cb15aac04a6b400cfb6bc881e2c5) Add utils::{futures, random}
- [b05d9299](https://git.meli-email.org/meli/meli/commit/b05d92997546e438b202d336fc581c2514c63b9f) Impl exponential backoff when retrying connection
- [f5cfbd32](https://git.meli-email.org/meli/meli/commit/f5cfbd32e6ebbe83ad7e84d048f1fbf2e51ca605) On set_flags, update {un,}seen sets in all mailboxes
- [f0d88005](https://git.meli-email.org/meli/meli/commit/f0d88005fbabcd552593ba0fe785e89a3560ac1c) Change message/rfc822 Display repr
- [f98e36ce](https://git.meli-email.org/meli/meli/commit/f98e36cee514f643e0fe256857cf31e2e0f24080) Replace old-style /*! module doc comments with //!
- [1bcc0bbe](https://git.meli-email.org/meli/meli/commit/1bcc0bbece2f479950e8811261befedc0199dab9) Add mbox parsing test
- [619fbef1](https://git.meli-email.org/meli/meli/commit/619fbef129e249489e64a26e1d0dfbd02db2516a) Recursively calculate update_show_subject()
- [957abf4e](https://git.meli-email.org/meli/meli/commit/957abf4e7238ec74b2194a21533b69dd1a58c0a8) Update cargo dependencies
- [9d51b6bd](https://git.meli-email.org/meli/meli/commit/9d51b6bd525784bc108959519c8dd21d30a8b020) Update `imap-codec`.
- [7c33f899](https://git.meli-email.org/meli/meli/commit/7c33f8999b6a5efd911680f2b83a3ff3a682a715) Use published imap-codec 0.10.0.
- [3803d788](https://git.meli-email.org/meli/meli/commit/3803d788abc5157b9cc6368da7e54aced9604aec) If auth is false checks if config has password entry
- [866166eb](https://git.meli-email.org/meli/meli/commit/866166eb8e8b994c8c87aad92a3303f9f6449b2d) Don't print parsing error for empty bytes
- [5b5869a2](https://git.meli-email.org/meli/meli/commit/5b5869a2ec3fce2fc69aa5c83fbda7a767f2a402) Re-enable print to stderr ifdef MELI_DEBUG_STDERR
- [13fe64a0](https://git.meli-email.org/meli/meli/commit/13fe64a027895780efdb6bfee246d562741a4be1) Cache pgp signature verification results
- [5ceddf41](https://git.meli-email.org/meli/meli/commit/5ceddf412e3b215b712e55aea8e18887d2d39f1a) Update CHANGELOG.md
- [4e55fbc9](https://git.meli-email.org/meli/meli/commit/4e55fbc90d8b105788c7c5998cb26b2829ac87a2) Add SEEN flag to all envs, since NNTP has no flags
- [e9cd800f](https://git.meli-email.org/meli/meli/commit/e9cd800f49e2d0e155d434ff8e91462e20b9d4f5) Add support for storing read status locally
- [53cba4be](https://git.meli-email.org/meli/meli/commit/53cba4beee4f774b548881c1a3f207ca391d3df3) Update README.md relative file paths
- [c4c245ee](https://git.meli-email.org/meli/meli/commit/c4c245ee19137f64d836401f7c1de17c9eb42b6e) Respect danger_accept_invalid_certs setting
- [29b43e2c](https://git.meli-email.org/meli/meli/commit/29b43e2c88edcfdecffd076fbb773c8547425f12) Replace mktime with timegm
- [4874e30f](https://git.meli-email.org/meli/meli/commit/4874e30f3ce9b186ac7cd427cba4a8542bd5048e) Add smtp-trace feature
- [51e9fbe8](https://git.meli-email.org/meli/meli/commit/51e9fbe8f2c380f3c9ee6a9ee65e638c169b43ef) Add account_name identifier to sqlite3 index database name
- [129f1091](https://git.meli-email.org/meli/meli/commit/129f10911b01641940801586bfa5286307e4342f) Rename `imap_backend` feature to `imap`
- [fe027fa3](https://git.meli-email.org/meli/meli/commit/fe027fa300a9882730a558fffe6000527ef08ff8) Rename `maildir_backend` feature to `maildir`
- [fe7dcc50](https://git.meli-email.org/meli/meli/commit/fe7dcc508ee51f492df2de3884147531fada6f4e) Rename `notmuch_backend` feature to `notmuch`
- [e9f09a15](https://git.meli-email.org/meli/meli/commit/e9f09a153ca0a1a023efe924b314ea977ccc3c25) Rename `mbox_backend` feature to `mbox`
- [7db930ca](https://git.meli-email.org/meli/meli/commit/7db930cabd295e888f4f106d5e7ea411521340ff) Rename `jmap_backend` feature to `jmap`
- [89c90f22](https://git.meli-email.org/meli/meli/commit/89c90f224a68ec524f7dc7033955ce7b8196f493) Add `nntp` feature
- [b65934fa](https://git.meli-email.org/meli/meli/commit/b65934facc7aeeb8ab30603e16cef2b747f9a0e5) Add nntp-trace feature
- [8ecdb6df](https://git.meli-email.org/meli/meli/commit/8ecdb6df3189cae4b6fa21a177bde756cc4407cf) Add imap-trace feature
- [9216e7bc](https://git.meli-email.org/meli/meli/commit/9216e7bc657738ae9861583a837c1326398197e4) Add opt id string for tracing
- [ae25ffba](https://git.meli-email.org/meli/meli/commit/ae25ffba430572efe73fde05eaf8111453f814cf) Don't do plain EHLO before starting Tls connection
- [8cb2a515](https://git.meli-email.org/meli/meli/commit/8cb2a515e1ba31efe914db67504993bc081ed7f3) Use localhost in lieu of 127.0.0.1 for CI
- [0ee1b6e0](https://git.meli-email.org/meli/meli/commit/0ee1b6e01830c01871e93e27d735a39792202325) Start background watch job in init
- [448e0635](https://git.meli-email.org/meli/meli/commit/448e0635e00b533a4d9dc15ba65982097649b397) Log error when command length exceeds 512 octets
- [bf543855](https://git.meli-email.org/meli/meli/commit/bf543855dc143b25344b79303f017380c9773793) Add PartialEq<str> for MessageID
- [7c7f6e19](https://git.meli-email.org/meli/meli/commit/7c7f6e1923e8b3127cf7cbd4b18f1db3ed9c6583) Don't increase Thread length for duplicates
- [5c2b0471](https://git.meli-email.org/meli/meli/commit/5c2b04719b953373c6a657f22db295d08b94685e) Normalize std::fmt::* imports
- [0f60009e](https://git.meli-email.org/meli/meli/commit/0f60009ea909adfb8f4e85d942decb8bc60f7539) Add RUSTFLAGS with -D warnings
- [6578a566](https://git.meli-email.org/meli/meli/commit/6578a5666889434ed6ca2f276e365633956fe3d3) Update cargo install directions
- [4f6081b6](https://git.meli-email.org/meli/meli/commit/4f6081b6633aed1eeafd99c24aa2dc64397043ca) Update to `imap-codec 1.0.0-beta`.
- [dc2b0044](https://git.meli-email.org/meli/meli/commit/dc2b00442b04c21455a6fda59b4729d0cbd04eff) Run rustfmt and cargo-sort
- [b3858de2](https://git.meli-email.org/meli/meli/commit/b3858de2f4e12723ee922174c79cc36062bed54e) Impl From<io::ErrorKind> for ErrorKind
- [f93adb68](https://git.meli-email.org/meli/meli/commit/f93adb683a562f25e40ffa03f80d04d5ad8ca34f) Replace change_color uses with change_theme
- [f193bdf6](https://git.meli-email.org/meli/meli/commit/f193bdf685e06652ab5b2da2a9a01fa56620cda6) Add column headers and sorting
- [095d24f9](https://git.meli-email.org/meli/meli/commit/095d24f91447a2ecab6d6bc78e1705ea4394e9bd) Add PULL_REQUEST_TEMPLATE.md
- [ab57e942](https://git.meli-email.org/meli/meli/commit/ab57e9420db29efd42773e970f33751b7b3f6f26) Add delete_contact shortcut
- [3963103d](https://git.meli-email.org/meli/meli/commit/3963103d55db28f789fe39f0dd80cd0d57792b5d) Prevent duplicate contact creation
- [f162239f](https://git.meli-email.org/meli/meli/commit/f162239fcc87d9c4f8aba8c33a9812a5e691c8d9) Change `on:` conditions for test.yaml
- [974b3a53](https://git.meli-email.org/meli/meli/commit/974b3a53058181e3df992a2105abcbf1c392fc19) Update bitflags, rusqlite dependencies
- [4d22b669](https://git.meli-email.org/meli/meli/commit/4d22b669bf330f8f3168fc2f704ad63c21c5e821) Update dependencies
- [ffba203a](https://git.meli-email.org/meli/meli/commit/ffba203a3b7070cc9e71d9444556e108ff0e18ea) Add support for Home and End key navigation
- [3433f7c4](https://git.meli-email.org/meli/meli/commit/3433f7c41e0d0cbb48af821280537da41b9e53d0) Update PULL_REQUEST_TEMPLATE.md
- [f7a4741b](https://git.meli-email.org/meli/meli/commit/f7a4741bf1622ae60042fb6ab0a906fe50fb1e06) Add jmap-trace feature
- [c875dda4](https://git.meli-email.org/meli/meli/commit/c875dda4960e5688b17176ba82ad1e5da38b883b) Add last_method_response field to Connection
- [37a787e6](https://git.meli-email.org/meli/meli/commit/37a787e6bb5abd34fae2888944537dec1ee3842f) Use IndexMap instead of HashMap
- [6ebdc7f9](https://git.meli-email.org/meli/meli/commit/6ebdc7f9aec5531c2b562a4e0cfd320ead6a4c01) Add Id<_>::empty() contructor
- [4f9b9773](https://git.meli-email.org/meli/meli/commit/4f9b97736a4af8b8b4ba0017ad1175a1c2352db6) Rename EmailImport to EmailImportObject
- [11432ba2](https://git.meli-email.org/meli/meli/commit/11432ba2c381b07bb540f7f92664b3c351e3cf62) Make `null` fields into Option<_>s
- [d9467d5f](https://git.meli-email.org/meli/meli/commit/d9467d5fcd9543611ec8a034eb7e25d12a3dcc45) Save all core capabilities to session store
- [31982931](https://git.meli-email.org/meli/meli/commit/31982931f5f472717b4c3d900f16c0588682f48e) Use Argument<OBJ> (value or resultreference) where appropriate
- [29fd8522](https://git.meli-email.org/meli/meli/commit/29fd8522e6bc2b0b6196cb97c8868dc34c2ba2f0) Implement Backend::create_mailbox()
- [5d8f07c8](https://git.meli-email.org/meli/meli/commit/5d8f07c8058261c7c251b3fb010ad866110e91df) Rename some objects better
- [38bc1369](https://git.meli-email.org/meli/meli/commit/38bc1369cc136c482f48d1ed3172b7f510ff7762) Add an Identity type.
- [59513b26](https://git.meli-email.org/meli/meli/commit/59513b267097cac8fe757c6198f26e0179014604) Implement Backend::submit(), server-side submission
- [5459a84f](https://git.meli-email.org/meli/meli/commit/5459a84f3d2b4c91a89252fba63f4ef12d965b9b) Update to imap-codec 1.0.0 (w/o `-beta`)
- [290cfb86](https://git.meli-email.org/meli/meli/commit/290cfb86c0c942690c48a0d3298e9d2de3ec4d94) Add a highlighted_selected theme key
- [46636d87](https://git.meli-email.org/meli/meli/commit/46636d8748f2779f38a10c6bf38c4e07acf16f8a) Bump version to 0.8.0

### Continuous Integration

- [1d0405ed](https://git.meli-email.org/meli/meli/commit/1d0405ed5b5cd76f4fe79e73fb30f4d4dce1d441) Add env vars
- [6e27edcb](https://git.meli-email.org/meli/meli/commit/6e27edcb775ce831b784d2040672f2d2af2c020f) Use cargo-nextest
- [67d2da0f](https://git.meli-email.org/meli/meli/commit/67d2da0f88b0e7b9b74c5d05c6c17a45057b094a) Disable smtp::test::test_smtp in test.yaml

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
- configs: throw error on extra unused conf flags in some imap/nntp
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
[v0.8.0]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.0
[v0.8.1]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.1
[v0.8.2]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.2
[v0.8.3]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.3
[v0.8.4]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.4
[v0.8.5]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.5
[v0.8.6]: https://git.meli-email.org/meli/meli/releases/tag/v0.8.6
