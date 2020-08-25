use melib;
use melib::email::Draft;
use xdg_utils::query_mime_info;

#[test]
fn build_draft() {
    let mut new_draft = Draft::default();
    let mut attachment = melib::email::attachment_from_file(&"./tests/test_image.gif")
        .expect("Could not open test_image.gif.");
    if let Ok(mime_type) = query_mime_info("./tests/test_image.gif") {
        match attachment.content_type {
            melib::email::ContentType::Other { ref mut tag, .. } => {
                *tag = mime_type;
            }
            _ => {}
        }
    }
    new_draft.headers_mut().remove("User-Agent");
    new_draft.headers_mut().remove("Date");

    new_draft.attachments_mut().push(attachment);
    new_draft.set_body("hello world.".to_string());
    let raw = new_draft.finalise().expect("could not finalise draft");
    let boundary_def = raw.find("bzz_bzz__bzz__").unwrap();
    let boundary_end = boundary_def + raw[boundary_def..].find("\"").unwrap();
    let boundary = raw[boundary_def..boundary_end].to_string();
    let boundary_str = &boundary["bzz_bzz__bzz__".len()..];

    let raw = raw.replace(boundary_str, "");
    assert_eq!(include_str!("generated.mail"), &raw);
}
