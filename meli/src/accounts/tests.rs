//
// meli
//
// Copyright 2024 Emmanouil Pitsidianakis <manos@pitsidianak.is>
//
// This file is part of meli.
//
// meli is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// meli is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with meli. If not, see <http://www.gnu.org/licenses/>.
//
// SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later

use melib::{
    backends::{Mailbox, MailboxHash},
    error::Result,
    MailboxPermissions, SpecialUsageMailbox,
};

use crate::accounts::{FileMailboxConf, MailboxEntry, MailboxStatus};

#[test]
fn test_mailbox_utf7() {
    #[derive(Debug)]
    struct TestMailbox(String);

    impl melib::BackendMailbox for TestMailbox {
        fn hash(&self) -> MailboxHash {
            unimplemented!()
        }

        fn name(&self) -> &str {
            &self.0
        }

        fn path(&self) -> &str {
            &self.0
        }

        fn children(&self) -> &[MailboxHash] {
            unimplemented!()
        }

        fn clone(&self) -> Mailbox {
            unimplemented!()
        }

        fn special_usage(&self) -> SpecialUsageMailbox {
            unimplemented!()
        }

        fn parent(&self) -> Option<MailboxHash> {
            unimplemented!()
        }

        fn permissions(&self) -> MailboxPermissions {
            unimplemented!()
        }

        fn is_subscribed(&self) -> bool {
            unimplemented!()
        }

        fn set_is_subscribed(&mut self, _: bool) -> Result<()> {
            unimplemented!()
        }

        fn set_special_usage(&mut self, _: SpecialUsageMailbox) -> Result<()> {
            unimplemented!()
        }

        fn count(&self) -> Result<(usize, usize)> {
            unimplemented!()
        }

        fn as_any(&self) -> &dyn std::any::Any {
            self
        }

        fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
            self
        }
    }
    for (n, d) in [
        ("~peter/mail/&U,BTFw-/&ZeVnLIqe-", "~peter/mail/台北/日本語"),
        ("&BB4EQgQ,BEAEMAQyBDsENQQ9BD0ESwQ1-", "Отправленные"),
    ] {
        let ref_mbox = TestMailbox(n.to_string());
        let mut conf: melib::MailboxConf = Default::default();
        conf.extra.insert("encoding".to_string(), "utf7".into());

        let entry = MailboxEntry::new(
            MailboxStatus::None,
            n.to_string(),
            Box::new(ref_mbox),
            FileMailboxConf {
                mailbox_conf: conf,
                ..Default::default()
            },
        );
        assert_eq!(&entry.path, d);
    }
}
