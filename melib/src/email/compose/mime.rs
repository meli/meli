use super::*;

pub fn encode_header(value: &str) -> String {
    eprintln!("encoding \"{}\"", value);
    let mut ret = String::with_capacity(5 / 3 * value.len());
    for word in value.split_whitespace() {
        if word.is_ascii() {
            ret.push_str(word);
        } else {
            ret.push_str(
                format!("=?UTF-8?B?{}?=", BASE64_MIME.encode(word.as_bytes()).trim()).as_str(),
            );
        }
        ret.push(' ');
    }
    ret.pop();
    ret
}
