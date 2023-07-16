# Development

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
