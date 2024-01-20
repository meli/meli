# Development

Code style follows the `rustfmt.toml` file.

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
