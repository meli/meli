fn false_val () -> bool {
    false
}

fn zero_val () -> usize {
    0
}

#[derive(Debug, Deserialize, Default)]
pub struct PagerSettings {
    #[serde(default = "zero_val")]
    pager_context: usize,
    #[serde(default = "false_val")]
    pager_stop: bool,
}
