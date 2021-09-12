use melib::email::Draft;

#[test]
fn build_draft() {
    let mut new_draft = Draft::default();
    let attachment = melib::email::attachment_from_file(&"./tests/test_image.gif")
        .expect("Could not open test_image.gif.");
    new_draft.headers_mut().remove("User-Agent");
    new_draft.headers_mut().remove("Date");

    new_draft.attachments_mut().push(attachment);
    new_draft.set_body("hello world.".to_string());
    let raw = new_draft.finalise().expect("could not finalise draft");
    let boundary_def = raw.find("bzz_bzz__bzz__").unwrap();
    let boundary_end = boundary_def + raw[boundary_def..].find('\"').unwrap();
    let boundary = raw[boundary_def..boundary_end].to_string();
    let boundary_str = &boundary["bzz_bzz__bzz__".len()..];

    let raw = raw.replace(boundary_str, "");
    assert_eq!(include_str!("generated_email.eml"), &raw);
}
