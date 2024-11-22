// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later


convert_to_typedefs! {


    pub fn gpgme_strerror(err: gpgme_error_t) -> *const ::core::ffi::c_char;
    pub fn gpgme_strerror_r(
        err: gpg_error_t,
        buf: *mut ::core::ffi::c_char,
        buflen: size_t,
    ) -> ::core::ffi::c_int;
    pub fn gpgme_new(ctx: *mut gpgme_ctx_t) -> gpgme_error_t;
    pub fn gpgme_release(ctx: gpgme_ctx_t);
    pub fn gpgme_set_ctx_flag(
        ctx: gpgme_ctx_t,
        name: *const ::core::ffi::c_char,
        value: *const ::core::ffi::c_char,
    ) -> gpgme_error_t;
    pub fn gpgme_get_ctx_flag(
        ctx: gpgme_ctx_t,
        name: *const ::core::ffi::c_char,
    ) -> *const ::core::ffi::c_char;
    pub fn gpgme_set_protocol(ctx: gpgme_ctx_t, proto: gpgme_protocol_t) -> gpgme_error_t;
    pub fn gpgme_set_armor(ctx: gpgme_ctx_t, yes: ::core::ffi::c_int);
    pub fn gpgme_get_armor(ctx: gpgme_ctx_t) -> ::core::ffi::c_int;
    pub fn gpgme_set_offline(ctx: gpgme_ctx_t, yes: ::core::ffi::c_int);
    pub fn gpgme_get_offline(ctx: gpgme_ctx_t) -> ::core::ffi::c_int;
    pub fn gpgme_set_pinentry_mode(ctx: gpgme_ctx_t, mode: gpgme_pinentry_mode_t) -> gpgme_error_t;
    pub fn gpgme_get_pinentry_mode(ctx: gpgme_ctx_t) -> gpgme_pinentry_mode_t;
    pub fn gpgme_set_passphrase_cb(
        ctx: gpgme_ctx_t,
        cb: gpgme_passphrase_cb_t,
        hook_value: *mut ::core::ffi::c_void,
    );
    pub fn gpgme_ctx_get_engine_info(ctx: gpgme_ctx_t) -> gpgme_engine_info_t;
    pub fn gpgme_ctx_set_engine_info(
        ctx: gpgme_ctx_t,
        proto: gpgme_protocol_t,
        file_name: *const ::core::ffi::c_char,
        home_dir: *const ::core::ffi::c_char,
    ) -> gpgme_error_t;
    pub fn gpgme_signers_clear(ctx: gpgme_ctx_t);
    pub fn gpgme_signers_add(ctx: gpgme_ctx_t, key: gpgme_key_t) -> gpgme_error_t;
    pub fn gpgme_set_io_cbs(ctx: gpgme_ctx_t, io_cbs: gpgme_io_cbs_t);
    pub fn gpgme_data_read(
        dh: gpgme_data_t,
        buffer: *mut ::core::ffi::c_void,
        size: size_t,
    ) -> ssize_t;
    pub fn gpgme_data_write(
        dh: gpgme_data_t,
        buffer: *const ::core::ffi::c_void,
        size: size_t,
    ) -> ssize_t;
    pub fn gpgme_data_seek(dh: gpgme_data_t, offset: off_t, whence: ::core::ffi::c_int) -> off_t;
    pub fn gpgme_data_new(r_dh: *mut gpgme_data_t) -> gpgme_error_t;
    pub fn gpgme_data_release(dh: gpgme_data_t);
    pub fn gpgme_data_new_from_mem(
        r_dh: *mut gpgme_data_t,
        buffer: *const ::core::ffi::c_char,
        size: size_t,
        copy: ::core::ffi::c_int,
    ) -> gpgme_error_t;
    pub fn gpgme_data_new_from_file(
        r_dh: *mut gpgme_data_t,
        fname: *const ::core::ffi::c_char,
        copy: ::core::ffi::c_int,
    ) -> gpgme_error_t;
    pub fn gpgme_key_ref(key: gpgme_key_t);
    pub fn gpgme_key_unref(key: gpgme_key_t);
    pub fn gpgme_op_encrypt_result(ctx: gpgme_ctx_t) -> gpgme_encrypt_result_t;
    pub fn gpgme_op_encrypt_start(
        ctx: gpgme_ctx_t,
        recp: *mut gpgme_key_t,
        flags: gpgme_encrypt_flags_t,
        plain: gpgme_data_t,
        cipher: gpgme_data_t,
    ) -> gpgme_error_t;
    pub fn gpgme_op_decrypt_result(ctx: gpgme_ctx_t) -> gpgme_decrypt_result_t;
    pub fn gpgme_op_decrypt_start(
        ctx: gpgme_ctx_t,
        cipher: gpgme_data_t,
        plain: gpgme_data_t,
    ) -> gpgme_error_t;
    pub fn gpgme_op_sign_start(
        ctx: gpgme_ctx_t,
        plain: gpgme_data_t,
        sig: gpgme_data_t,
        flags: gpgme_sig_mode_t,
    ) -> gpgme_error_t;
    pub fn gpgme_op_verify_result(ctx: gpgme_ctx_t) -> gpgme_verify_result_t;
    pub fn gpgme_op_verify_start(
        ctx: gpgme_ctx_t,
        sig: gpgme_data_t,
        signed_text: gpgme_data_t,
        plaintext: gpgme_data_t,
    ) -> gpgme_error_t;
    pub fn gpgme_op_import_result(ctx: gpgme_ctx_t) -> gpgme_import_result_t;
    pub fn gpgme_op_import(ctx: gpgme_ctx_t, keydata: gpgme_data_t) -> gpgme_error_t;
    pub fn gpgme_op_keylist_start(
        ctx: gpgme_ctx_t,
        pattern: *const ::core::ffi::c_char,
        secret_only: ::core::ffi::c_int,
    ) -> gpgme_error_t;
    pub fn gpgme_op_keylist_end(ctx: gpgme_ctx_t) -> gpgme_error_t;
    pub fn gpgme_check_version(
        req_version: *const ::core::ffi::c_char,
    ) -> *const ::core::ffi::c_char;
}
