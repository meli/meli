fn main() {
    #[cfg(feature = "notmuch_backend")]
    {
        println!("cargo:rustc-link-lib=notmuch");
    }
}
