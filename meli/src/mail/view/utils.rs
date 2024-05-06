/*
 * meli
 *
 * Copyright 2017-2018 Manos Pitsidianakis
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

use std::{fs::File, io::Write, os::unix::fs::PermissionsExt, path::Path};

use melib::{Result, ShellExpandTrait};

pub fn save_attachment(path: &Path, bytes: &[u8]) -> Result<()> {
    let mut f = File::create(path.expand())?;
    let mut permissions = f.metadata()?.permissions();
    permissions.set_mode(0o600); // Read/write for owner only.
    f.set_permissions(permissions)?;
    f.write_all(bytes)?;
    f.flush()?;
    Ok(())
}

pub fn desktop_exec_to_command(command: &str, path: String, is_url: bool) -> String {
    /* Purge unused field codes */
    let command = command
        .replace("%i", "")
        .replace("%c", "")
        .replace("%k", "");
    if command.contains("%f") {
        command.replacen("%f", &path.replace(' ', "\\ "), 1)
    } else if command.contains("%F") {
        command.replacen("%F", &path.replace(' ', "\\ "), 1)
    } else if command.contains("%u") || command.contains("%U") {
        let from_pattern = if command.contains("%u") { "%u" } else { "%U" };
        if is_url {
            command.replacen(from_pattern, &path, 1)
        } else {
            command.replacen(
                from_pattern,
                &format!("file://{}", path).replace(' ', "\\ "),
                1,
            )
        }
    } else if is_url {
        format!("{} {}", command, path)
    } else {
        format!("{} {}", command, path.replace(' ', "\\ "))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_desktop_exec() {
        assert_eq!(
            "ristretto /tmp/file".to_string(),
            desktop_exec_to_command("ristretto %F", "/tmp/file".to_string(), false)
        );
        assert_eq!(
            "/usr/lib/firefox-esr/firefox-esr file:///tmp/file".to_string(),
            desktop_exec_to_command(
                "/usr/lib/firefox-esr/firefox-esr %u",
                "/tmp/file".to_string(),
                false
            )
        );
        assert_eq!(
            "/usr/lib/firefox-esr/firefox-esr www.example.com".to_string(),
            desktop_exec_to_command(
                "/usr/lib/firefox-esr/firefox-esr %u",
                "www.example.com".to_string(),
                true
            )
        );
        assert_eq!(
            "/usr/bin/vlc --started-from-file www.example.com".to_string(),
            desktop_exec_to_command(
                "/usr/bin/vlc --started-from-file %U",
                "www.example.com".to_string(),
                true
            )
        );
        assert_eq!(
            "zathura --fork file:///tmp/file".to_string(),
            desktop_exec_to_command("zathura --fork %U", "file:///tmp/file".to_string(), true)
        );
    }
}
