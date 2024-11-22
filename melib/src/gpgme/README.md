<!-- SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later -->
# Interfacing with `libgpgme`

In order to support both 32-bit and 64-bit compilation targets, the generated
bindings for `libgpgme` have been generated twice, once on an 64-bit host and
once on an 32-bit host (`arm-unknown-linux-gnueabihf` in specific).

There are two shell scripts in this directory:

- `./bindgen-gpgme-funcs.sh`: This script invokes `bindgen` CLI to generate **only** bindings for the
  functions we need from `libgpgme`.
  The output is included inside `./bindings.rs` and wrapped with a declarative
  macro that converts the function declarations into type definitions, because this is the only way we
  can access functions via symbols with the `libloading` crate.
  Otherwise, the conversion would have to be done manually.

  Note that running this script to 32-bit and 64-bit hosts should have the same output.
- `./bindgen-gpgme-rest.sh`: This script invokes `bindgen` CLI to generate
  bindings for types and global variables from the `libgpgme` header files.
  This part is where target pointer width is important, so as a result we have
  checked in two source files: `./bindings_rest.rs` for the "normal" world and
  `./bindings_rest_32.rs` for the still 32-bit world.

`./bindings.rs` includes the correct version based on the `target_pointer_width` value at compile time.

Is this the best we can do? No, but it's the best we can do for now.

*NOTE*: Generating bindings with `bindgen` on 32-bit hosts should require
appending `-- -D_FILE_OFFSET_BITS=64` at the `bindgen` invocation in those
scripts; the `-- [...]` part means these arguments get redirected to `clang`,
and what happens is we define a preprocessor symbol `_FILE_OFFSET_BITS` with
the value `64`.
