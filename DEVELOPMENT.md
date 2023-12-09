# Development

## use `.git-blame-ignore-revs` file _optional_

Use this file to ignore formatting commits from `git-blame`.
It needs to be set up per project because `git-blame` will fail if it's missing.

```sh
git config blame.ignoreRevsFile .git-blame-ignore-revs
```

## Testing

How to run specific tests:

```sh
cargo test -p {melib, meli} (-- --nocapture) (--test test_name)
```

## Profiling

```sh
perf record -g target/debug/bin
perf script | stackcollapse-perf | rust-unmangle | flamegraph > perf.svg
```

## Running fuzz targets

Note: `cargo-fuzz` requires the nightly toolchain.

```sh
cargo +nightly fuzz run envelope_parse -- -dict=fuzz/envelope_tokens.dict
```
