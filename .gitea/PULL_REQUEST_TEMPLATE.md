---

name: "Pull Request"
about: "Standard pull request template."
title: "WIP: "
ref: "master"

---

**If your PR is ready to merge/review, remove the `WIP: ` prefix from the title.**

### Summary of the PR

### Requirements

Before submitting your PR, please make sure you have addressed the following requirements:

* [ ] All commits in this PR are signed (with `git commit -s`), and the commit has a message describing the motivation behind the change, if appropriate.
* [ ] All added/changed public-facing functionality has entries in the "Upcoming Release" section of CHANGELOG.md (if no such section exists, please create one).
* [ ] All added/changed public-facing functionality, especially configuration options, must be documented in the manual pages.
* [ ] Any newly added `unsafe` code is properly documented.
* [ ] Each commit has been formatted with `rustfmt`. Run `make fmt` in the project root.
* [ ] Each commit has been linted with `clippy`. Run `make lint` in the project root.
* [ ] Each commit does not break any test. Run `make test` in the project root. If you have `cargo-nextest` installed, you can run `cargo nextest run --all --no-fail-fast --all-features --future-incompat-report` instead.
