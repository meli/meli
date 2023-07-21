/*
 * meli
 *
 * Copyright 2023 Manos Pitsidianakis
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

use melib::{backends::ImapType, futures, smol, AccountSettings, BackendEventConsumer, Result};
use std::net::TcpListener;
use std::path::Path;
use test_binary::build_test_binary;

#[derive(Debug)]
enum TestError {}

impl<Err: std::fmt::Display> From<Err> for TestError {
    #[track_caller]
    fn from(err: Err) -> Self {
        panic!("error: {}: {}", std::any::type_name::<Err>(), err);
    }
}

type TestResult = std::result::Result<(), TestError>;

struct Server {
    child: std::process::Child,
}

impl From<std::process::Child> for Server {
    fn from(child: std::process::Child) -> Self {
        Self { child }
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        _ = self.child.kill();
    }
}

#[cfg(test)]
fn stunnel_conf_gen(tls_port: u16, connect_port: u16, cert: &Path, key: &Path) -> String {
    format!(
        r#"
[imaps]
accept  = {tls_port}
connect = {connect_port}
cert = {cert}
key = {key}
"#,
        cert = cert.display(),
        key = key.display()
    )
}

#[cfg(test)]
async fn imap_session(acc: AccountSettings) -> TestResult {
    let imap = ImapType::new(
        &acc,
        Box::new(|_| true),
        BackendEventConsumer::new(std::sync::Arc::new(|_, _| ())),
    )?;
    let _is_online = imap.is_online()?.await?;
    let _mailboxes = imap.mailboxes()?.await?;
    Ok(())
}

#[cfg(test)]
fn get_available_port() -> Result<u16> {
    let listener = TcpListener::bind("127.0.0.1:0")?;
    Ok(listener.local_addr()?.port())
}

#[cfg(test)]
fn imap_conn_setup(tempdir: &tempfile::TempDir, stunnel: bool) -> TestResult {
    let exec_script = |path: &Path, cwd: &Path| -> TestResult {
        let mut script = std::process::Command::new(path).current_dir(cwd).spawn()?;
        script.wait()?;
        Ok(())
    };

    std::thread::spawn(move || {
        let ex = smol::Executor::new();
        futures::executor::block_on(ex.run(futures::future::pending::<()>()));
    });

    // 1. make localhost cert

    exec_script(
        &Path::new("./tests/bins/make_localhost_cert.sh").canonicalize()?,
        tempdir.path(),
    )?;

    if stunnel {
        // 2. build stunnel

        std::fs::copy(
            Path::new("./tests/bins/build_static_stunnel_binary.sh").canonicalize()?,
            &tempdir.path().join("build_static_stunnel_binary.sh"),
        )?;
        std::fs::copy(
            Path::new("./tests/bins/stunnel-5.70.tar.xz").canonicalize()?,
            &tempdir.path().join("stunnel-5.70.tar.xz"),
        )?;

        exec_script(
            &tempdir
                .path()
                .join("build_static_stunnel_binary.sh")
                .canonicalize()?,
            tempdir.path(),
        )?;
    }
    Ok(())
}

#[test]
fn test_imap_plaintext_connection() -> TestResult {
    let tempdir = tempfile::tempdir().unwrap();
    imap_conn_setup(&tempdir, false)?;

    let port = get_available_port()?;

    let test_bin_path =
        build_test_binary("tokio-server", "tests/bins").expect("error building test binary");

    let test_bin_subproc: Server = std::process::Command::new(test_bin_path)
        .arg(&format!("127.0.0.1:{}", port))
        .spawn()
        .expect("error running test binary")
        .into();

    let insecure = AccountSettings {
        extra: [
            ("server_hostname".to_string(), "localhost".to_string()),
            ("server_username".to_string(), "alice".to_string()),
            ("server_password".to_string(), "password".to_string()),
            ("server_port".to_string(), port.to_string()),
            ("use_starttls".to_string(), "false".to_string()),
            ("use_tls".to_string(), "false".to_string()),
        ]
        .iter()
        .cloned()
        .collect(),
        ..Default::default()
    };

    eprintln!("• running mock plaintext IMAP server at 127.0.0.1:{port}...");
    futures::executor::block_on(imap_session(insecure))?;

    drop(test_bin_subproc);

    Ok(())
}

#[test]
fn test_imap_starttls_connection() -> TestResult {
    let tempdir = tempfile::tempdir().unwrap();
    imap_conn_setup(&tempdir, false)?;

    let port = get_available_port()?;
    let test_bin_path =
        build_test_binary("tokio-server", "tests/bins").expect("error building test binary");

    let test_bin_subproc: Server = std::process::Command::new(test_bin_path)
        .arg(&format!("127.0.0.1:{}", port))
        .spawn()
        .expect("error running test binary")
        .into();

    let starttls = AccountSettings {
        extra: [
            ("server_hostname".to_string(), "localhost".to_string()),
            ("server_username".to_string(), "alice".to_string()),
            ("server_password".to_string(), "password".to_string()),
            ("server_port".to_string(), port.to_string()),
            ("use_starttls".to_string(), "true".to_string()),
        ]
        .iter()
        .cloned()
        .collect(),
        ..Default::default()
    };

    eprintln!("• running mock IMAP server at 127.0.0.1:{port} and connecting with STARTTLS...");
    futures::executor::block_on(imap_session(starttls))?;

    drop(test_bin_subproc);

    Ok(())
}

#[test]
fn test_imap_tls_connection() -> TestResult {
    let tempdir = tempfile::tempdir().unwrap();
    imap_conn_setup(&tempdir, true)?;
    let port = get_available_port()?;
    let tls_port = get_available_port()?;

    let stunnel = tempdir.path().join("stunnel");
    assert!(stunnel.is_file());
    let stunnel_conf = tempdir.path().join("stunnel.conf");
    std::fs::write(
        &stunnel_conf,
        stunnel_conf_gen(
            tls_port,
            port,
            &tempdir.path().join("localhost.crt"),
            &tempdir.path().join("localhost.key"),
        )
        .as_bytes(),
    )?;

    let test_bin_path =
        build_test_binary("tokio-server", "tests/bins").expect("error building test binary");

    let test_bin_subproc: Server = std::process::Command::new(test_bin_path)
        .arg(&format!("127.0.0.1:{}", port))
        .spawn()
        .expect("error running test binary")
        .into();

    let tls_subproc: Server = std::process::Command::new(&stunnel)
        .arg(&stunnel_conf)
        .spawn()
        .expect("error running test binary")
        .into();

    let set = AccountSettings {
        extra: [
            ("server_hostname".to_string(), "127.0.0.1".to_string()),
            ("server_username".to_string(), "alice".to_string()),
            ("server_password".to_string(), "password".to_string()),
            ("server_port".to_string(), tls_port.to_string()),
            ("use_starttls".to_string(), "false".to_string()),
            ("use_tls".to_string(), "true".to_string()),
            (
                "danger_accept_invalid_certs".to_string(),
                "true".to_string(),
            ),
        ]
        .iter()
        .cloned()
        .collect(),
        ..Default::default()
    };

    eprintln!("• running mock IMAP server at 127.0.0.1:{port} and a TLS tunnel at 127.0.0.1:{tls_port}...");
    futures::executor::block_on(imap_session(set))?;
    drop(tls_subproc);
    drop(test_bin_subproc);

    Ok(())
}
