#!/bin/zsh
# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

bindgen \
  -o bindings_rest.rs \
  --raw-line "// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later" \
  --ignore-functions \
  --bitfield-enum 'gpgme_encrypt_flags_t' \
  --bitfield-enum 'gpgme_decrypt_flags_t' \
  --bitfield-enum 'gpgme_sigsum_t' \
  --impl-debug \
  --impl-partialeq \
  --with-derive-default \
  --with-derive-hash \
  --with-derive-partialeq \
  --with-derive-partialord \
  --with-derive-eq \
  --with-derive-ord \
  --generate-block \
  --generate-cstr \
  --rust-target "1.68" \
  --use-core \
  --rustfmt-configuration-file `realpath ../../../rustfmt.toml` \
  --merge-extern-blocks \
  --sort-semantically \
  --flexarray-dst \
  --wrap-unsafe-ops \
  --no-prepend-enum-name \
  --rustified-enum 'gpgme_status_code_t.*' \
  --rustified-enum 'gpg_err_source_t.*' \
  --rustified-enum 'gpg_err_code_t.*' \
  --rustified-enum 'gpgme_data_encoding_t.*' \
  --rustified-enum 'gpgme_data_type_t' \
  --rustified-enum 'gpgme_pubkey_algo_t' \
  --rustified-enum 'gpgme_hash_algo_t' \
  --rustified-enum 'gpgme_sig_mode_t' \
  --rustified-enum 'gpgme_validity_t' \
  --rustified-enum 'gpgme_tofu_policy_t' \
  --rustified-enum 'gpgme_keyorg_t' \
  --rustified-enum 'gpgme_protocol_t' \
  --rustified-enum 'gpgme_pinentry_mode_t' \
  --rustified-enum 'gpgme_event_io_t' \
  --rustified-enum 'gpgme_conf_level_t' \
  --rustified-enum 'gpgme_conf_type_t' \
  --rustified-enum '_gpgme_sig_stat_t' \
  --rustified-enum '_gpgme_attr_t' \
  --blocklist-type FILE \
  --blocklist-type _IO_FILE \
  --blocklist-type _IO_lock_t \
  --allowlist-var GPGME_VERSION \
  --allowlist-type gpgme_io_event_done_data \
  --no-size_t-is-usize \
  --emit-diagnostics \
  --experimental \
  --allowlist-file /usr/include/gpgme.h \
  /usr/include/gpgme.h
