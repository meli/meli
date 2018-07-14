fn false_val () -> bool {
    true
}

fn true_val () -> bool {
    true
}

fn zero_val () -> usize {
    0
}
fn eighty_percent () -> usize {
    80
}

/// Settings for the pager function.
#[derive(Debug, Deserialize)]
pub struct PagerSettings {
    /// Number of context lines when going to next page.
    /// Default: 0
    #[serde(default = "zero_val")]
    pub pager_context: usize,

    /// Stop at the end instead of displaying next mail.
    /// Default: false
    #[serde(default = "false_val")]
    pub pager_stop: bool,

    /// Always show headers when scrolling.
    /// Default: true
    #[serde(default = "true_val")]
    pub headers_sticky: bool,

    /// The height of the pager in mail view, in percent.
    /// Default: 80
    #[serde(default = "eighty_percent")]
    pub pager_ratio: usize,
}
