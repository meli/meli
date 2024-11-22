#!/bin/zsh
# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

# Find out which functions we call with:
#
# rg call melib/src/gpgme | sed -r -e 's/^.+, (gpgme_[^)]+)[)].*$/\1/p' | sort | sort -u

bindgen \
  -o bindings_funcs.rs \
  --raw-line "// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later" \
  --raw-line "" \
  --raw-line "" \
  --raw-line "convert_to_typedefs! {" \
  --generate "functions" \
  --generate-block \
  --rust-target "1.68" \
  --use-core \
  --rustfmt-configuration-file `realpath ../../../rustfmt.toml` \
  --merge-extern-blocks \
  --sort-semantically \
  --flexarray-dst \
  --wrap-unsafe-ops \
  --no-prepend-enum-name \
  --blocklist-type FILE \
  --blocklist-type _IO_FILE \
  --blocklist-type _IO_lock_t \
  --allowlist-function gpgme_check_version \
  --allowlist-function gpgme_ctx_get_engine_info \
  --allowlist-function gpgme_ctx_set_engine_info \
  --allowlist-function gpgme_data_new \
  --allowlist-function gpgme_data_new_from_file \
  --allowlist-function gpgme_data_new_from_mem \
  --allowlist-function gpgme_data_read \
  --allowlist-function gpgme_data_release \
  --allowlist-function gpgme_data_seek \
  --allowlist-function gpgme_data_write \
  --allowlist-function gpgme_get_armor \
  --allowlist-function gpgme_get_ctx_flag \
  --allowlist-function gpgme_get_offline \
  --allowlist-function gpgme_get_pinentry_mode \
  --allowlist-function gpgme_key_ref \
  --allowlist-function gpgme_key_unref \
  --allowlist-function gpgme_new \
  --allowlist-function gpgme_op_decrypt_result \
  --allowlist-function gpgme_op_decrypt_start \
  --allowlist-function gpgme_op_encrypt_result \
  --allowlist-function gpgme_op_encrypt_start \
  --allowlist-function gpgme_op_import \
  --allowlist-function gpgme_op_import_result \
  --allowlist-function gpgme_op_keylist_end \
  --allowlist-function gpgme_op_keylist_start \
  --allowlist-function gpgme_op_sign_start \
  --allowlist-function gpgme_op_verify_result \
  --allowlist-function gpgme_op_verify_start \
  --allowlist-function gpgme_release \
  --allowlist-function gpgme_set_armor \
  --allowlist-function gpgme_set_ctx_flag \
  --allowlist-function gpgme_set_io_cbs \
  --allowlist-function gpgme_set_offline \
  --allowlist-function gpgme_set_passphrase_cb \
  --allowlist-function gpgme_set_pinentry_mode \
  --allowlist-function gpgme_set_protocol \
  --allowlist-function gpgme_signers_add \
  --allowlist-function gpgme_signers_clear \
  --allowlist-function gpgme_strerror_r \
  --allowlist-function gpgme_strerror \
  --no-size_t-is-usize \
  --disable-header-comment \
  --emit-diagnostics \
  --experimental \
  /usr/include/gpgme.h

sed --in-place -e 's/\s*extern "C" [{]//' bindings_funcs.rs
