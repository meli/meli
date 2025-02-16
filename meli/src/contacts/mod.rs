/*
 * meli
 *
 * Copyright 2023 - Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

use melib::{AccountHash, Card};

use crate::{
    types::{sanitize_filename, File, NotificationType, UIEvent},
    Context,
};

pub mod editor;
pub mod list;

pub fn export_to_vcard(card: &Card, account_hash: AccountHash, context: &mut Context) {
    let mut output_dir = context.accounts[&account_hash]
        .settings
        .account
        .vcard_folder()
        .map(|s| std::path::Path::new(s).to_path_buf());
    let filename = sanitize_filename(format!(
        "{prefix}{name}{suffix}{space}{additionalname}",
        prefix = card.name_prefix(),
        name = card.name(),
        suffix = card.name_suffix(),
        space = if card.additionalname().trim().is_empty() {
            ""
        } else {
            " "
        },
        additionalname = card.additionalname()
    ));
    let res = File::create_temp_file(
        card.to_vcard_string().as_bytes(),
        filename.as_deref(),
        output_dir.as_mut(),
        Some("vcf"),
        false,
    );
    match res {
        Ok(f) => {
            context.replies.push_back(UIEvent::Notification {
                title: Some("Exported .vcf".into()),
                body: format!("Exported contact to vcard file to\n{}", f.path().display()).into(),
                kind: Some(NotificationType::Info),
                source: None,
            });
        }
        Err(err) => {
            context.replies.push_back(UIEvent::Notification {
                title: Some("Could not export contact.".into()),
                body: err.to_string().into(),
                kind: Some(NotificationType::Error(err.kind)),
                source: Some(err),
            });
        }
    }
}
