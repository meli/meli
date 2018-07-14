fn false_val () -> bool {
    false
}

fn zero_val () -> usize {
    0
}

/// Settings for the pager function.
#[derive(Debug, Deserialize, Default)]
pub struct PagerSettings {
    #[serde(default = "zero_val")]
    /// Number of context lines when going to next page.
    /// Default: 0
    pub pager_context: usize,

    #[serde(default = "false_val")]
    /// Stop at the end instead of displaying next mail.
    /// Default: false
    pub pager_stop: bool,
    /// Always show headers when scrolling.
    /// Default: false
    pub headers_sticky: bool,
}
