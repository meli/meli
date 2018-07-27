/*!
  Notification handling components.
  */
use notify_rust::Notification as notify_Notification;

use super::*;

/// Passes notifications to the OS using the XDG specifications.
pub struct XDGNotifications {}

impl Component for XDGNotifications {
    fn draw(&mut self, _grid: &mut CellBuffer, _area: Area, _context: &mut Context) {}
    fn process_event(&mut self, event: &UIEvent, _context: &mut Context) {
        match event.event_type {
            UIEventType::Notification(ref t) => {
                notify_Notification::new()
                    .summary("Refresh Event")
                    .body(t)
                    .icon("dialog-information")
                    .show()
                    .unwrap();
            }
            _ => {}
        }
    }
}
