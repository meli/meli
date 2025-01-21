# Development

Code style follows the `rustfmt.toml` file.

## Commit messages

The `meli` project requires per-commit developer sign-offs just like the Linux
project.

Quoting <https://wiki.linuxfoundation.org/dco>:

> The DCO is a per-commit sign-off made by a contributor stating that they agree
> to the terms published at <https://developercertificate.org> for that
> particular contribution.
>
> When creating a commit with the Git CLI, a sign-off can be added with the -s
> option: <https://git-scm.com/docs/git-commit#git-commit--s>. The sign-off is
> stored as part of the commit message itself, as a line of the format:
>
> ```text
> Signed-off-by: Full Name <email>
> ```

The sign-off line must be the final line of your commit message, without any
empty lines after.

Always use `git commit -s` to have `git` add a proper sign-off trailer line in
your commit message.

## CI

All pull requests require CI checks to pass.
You can run the same checks locally without a CI runner.

The CI workflows are written to execute the following `Makefile`s:

- [`.gitea/Makefile.build`](.gitea/Makefile.build)
  Runs build checks with `cargo-check`, `cargo-test --no-run`, `make
  build-rustdoc` and also runs cargo tests with `cargo-nextest` and `rustdoc`
  tests with `make test-docs`.
- [`.gitea/Makefile.lint`](.gitea/Makefile.lint)
  Performs linter checks with `rustfmt`, `clippy`, `cargo-msrv` and `cargo-derivefmt`.
- [`.gitea/Makefile.manifest-lint`](.gitea/Makefile.manifest-lint)
  Performs linter checks for manifest files with `cargo-sort` and the
  [`check_debian_changelog.sh`](./scripts/check_debian_changelog.sh) script.

This means you don't have to run the CI with a pull request to see if the
checks pass, you can do the equivalent checks locally with something like:

```sh
for m in .gitea/Makefile.*; do
  make -f "${m}" || break
done
```

Or run all checks in a specific `Makefile`:

```sh
make -f .gitea/Makefile.lint
```

Or run a specific check in a specific `Makefile`:

```sh
make -f .gitea/Makefile.lint clippy
```

## Trace logs

Enable trace logs to `stderr` with:

```sh
export MELI_DEBUG_STDERR=yes
```

This means you will have to to redirect `stderr` to a file like `meli 2> trace.log`.

Tracing is opt-in by build features:

```sh
cargo build --features=debug-tracing,imap-trace,smtp-trace
```

## use `.git-blame-ignore-revs` file _optional_

Use this file to ignore formatting commits from `git-blame`.
It needs to be set up per project because `git-blame` will fail if it's missing.

```sh
git config blame.ignoreRevsFile .git-blame-ignore-revs
```

## Formatting with `rustfmt`

```sh
make fmt
```

## Linting with `clippy`

```sh
make lint
```

## Testing

```sh
make test
```

How to run specific tests:

```sh
cargo test -p {melib, meli} (-- --nocapture) (--test test_name)
```

## Profiling

```sh
perf record -g target/debug/meli
perf script | stackcollapse-perf | rust-unmangle | flamegraph > perf.svg
```
<!--  -->
<!-- ## Running fuzz targets -->
<!--  -->
<!-- Note: `cargo-fuzz` requires the nightly toolchain. -->
<!--  -->
<!-- ```sh -->
<!-- cargo +nightly fuzz run envelope_parse -- -dict=fuzz/envelope_tokens.dict -->
<!-- ``` -->
