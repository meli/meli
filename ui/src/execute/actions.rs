/*!
 * User actions that need to be handled by the UI
 */

#[derive(Debug)]
pub enum MailListingAction {
    ToggleThreaded,
}

#[derive(Debug)]
pub enum Action {
    MailListing(MailListingAction),
    ViewMailbox(usize),
}
