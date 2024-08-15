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

use std::{borrow::Cow, ffi::CString, future::Future};

use sealed_test::prelude::*;

use crate::{
    gpgme::{Context, EngineInfo, Key, LocateKey, Protocol},
    Address, Result,
};

const PUBKEY: &[u8]=b"-----BEGIN PGP PUBLIC KEY BLOCK-----\r\nVersion: GnuPG v2.1.0-gitb3c71eb (GNU/Linux)\r\n\r\nmQGiBDo41NoRBADSfQazKGYf8nokq6zUKH/6INtV6MypSzSGmX2XErnARkIIPPYj\r\ncQRQ8zCbGV7ZU2ezVbzhFLUSJveE8PZUzzCrLp1O2NSyBTRcR5HVSXW95nJfY8eV\r\npOvZRAKul0BVLh81kYTsrfzaaCjh9VWNP26LoeN2r+PjZyktXe7gM3C4SwCgoTxK\r\nWUVi9HoT2HCLY7p7oig5hEcEALdCJal0UYomX3nJapIVLVZg3vkidr1RICYMb2vz\r\n58i17h8sxEtobD1vdIKNejulntaRAXs4n0tDYD9z7pRlwG1CLz1R9WxYzeOOqUDr\r\nfnVXdmU8L/oVWABat8v1V7QQhjMMf+41fuzVwDMMGqjVPLhu4X6wp3A8uyM3YDnQ\r\nVMN1A/4n2G5gHoOvjqxn8Ch5tBAdMGfO8gH4RjQOwzm2R1wPQss/yzUN1+tlMZGX\r\nK2dQ2FCWC/hDUSNaEQRlI15wxxBNZ2RQwlzE2A8v113DpvyzOtv0QO95gJ1teCXC\r\n7j/BN9asgHaBBc39JLO/TcpuI7Hf8PQ5VcP2F0UE3lczGhXbLLRESm9lIFJhbmRv\r\nbSBIYWNrZXIgKHRlc3Qga2V5IHdpdGggcGFzc3BocmFzZSAiYWJjIikgPGpvZUBl\r\neGFtcGxlLmNvbT6IYgQTEQIAIgUCTbdXqQIbIwYLCQgHAwIGFQgCCQoLBBYCAwEC\r\nHgECF4AACgkQr4IkT5zZ/VUcCACfQvSPi//9/gBv8SVrK6O4DiyD+jAAn3LEnfF1\r\n4j6MjwlqXTqol2VgQn1yuQENBDo41N0QBACedJb7Qhm50JSPe1V+rSZKLHT5nc3l\r\n2k1n7//wNsJkgDW2J7snIRjGtSzeNxMPh+hVzFidzAf3sbOlARQoBrMPPKpnJWtm\r\n6LEDf2lSwO36l0/bo6qDRmiFRJoHWytTJEjxVwRclVt4bXqHfNw9FKhZZbcKeAN2\r\noHgmBVSU6edHdwADBQP+OGAkEG4PcfSb8x191R+wkV/q2hA5Ay9z289Dx2rO28CO\r\n4M2fhhcjSmgr6x0DsrkfESCiG47UGJ169eu+QqJwk3HiF4crGN9rE5+VelBVFtrd\r\nMWkX2rPLGQWyw8iCZKbeH8g/ujmkaLovSmalzDcLe4v1xSLaP7Fnfzit0iIGZAGI\r\nRgQYEQIABgUCOjjU3QAKCRCvgiRPnNn9VVSaAJ9+rj1lIQnRl20i8Rom2Hwbe3re\r\n9QCfSYFnkZUw0yKF2DfCfqrDzdGAsbaIRgQYEQIABgUCOjjU3gAKCRCvgiRPnNn9\r\nVe4iAJ9FrGMlFR7s+GWf1scTeeyrthKrPQCfSpc/Yps72aFI7hPfyIa9MuerVZ4=\r\n=QRit\r\n-----END PGP PUBLIC KEY BLOCK-----\r\n";

const SECKEY: &[u8] = b"-----BEGIN PGP PRIVATE KEY BLOCK-----\r\nVersion: GnuPG v2.1.0-gitb3c71eb (GNU/Linux)\r\n\r\nlQHpBDo41NoRBADSfQazKGYf8nokq6zUKH/6INtV6MypSzSGmX2XErnARkIIPPYj\r\ncQRQ8zCbGV7ZU2ezVbzhFLUSJveE8PZUzzCrLp1O2NSyBTRcR5HVSXW95nJfY8eV\r\npOvZRAKul0BVLh81kYTsrfzaaCjh9VWNP26LoeN2r+PjZyktXe7gM3C4SwCgoTxK\r\nWUVi9HoT2HCLY7p7oig5hEcEALdCJal0UYomX3nJapIVLVZg3vkidr1RICYMb2vz\r\n58i17h8sxEtobD1vdIKNejulntaRAXs4n0tDYD9z7pRlwG1CLz1R9WxYzeOOqUDr\r\nfnVXdmU8L/oVWABat8v1V7QQhjMMf+41fuzVwDMMGqjVPLhu4X6wp3A8uyM3YDnQ\r\nVMN1A/4n2G5gHoOvjqxn8Ch5tBAdMGfO8gH4RjQOwzm2R1wPQss/yzUN1+tlMZGX\r\nK2dQ2FCWC/hDUSNaEQRlI15wxxBNZ2RQwlzE2A8v113DpvyzOtv0QO95gJ1teCXC\r\n7j/BN9asgHaBBc39JLO/TcpuI7Hf8PQ5VcP2F0UE3lczGhXbLP4HAwL0A7A1a/jY\r\n6s5JxysLUpKA31U2SrKxePmkmzYSuAiValUVdfkmLRrLSwmNJSy5NcrBHGimja1O\r\nfUUmPTg465j1+vD/tERKb2UgUmFuZG9tIEhhY2tlciAodGVzdCBrZXkgd2l0aCBw\r\nYXNzcGhyYXNlICJhYmMiKSA8am9lQGV4YW1wbGUuY29tPohiBBMRAgAiBQJNt1ep\r\nAhsjBgsJCAcDAgYVCAIJCgsEFgIDAQIeAQIXgAAKCRCvgiRPnNn9VRwIAJ9C9I+L\r\n//3+AG/xJWsro7gOLIP6MACfcsSd8XXiPoyPCWpdOqiXZWBCfXKdAWAEOjjU3RAE\r\nAJ50lvtCGbnQlI97VX6tJkosdPmdzeXaTWfv//A2wmSANbYnuychGMa1LN43Ew+H\r\n6FXMWJ3MB/exs6UBFCgGsw88qmcla2bosQN/aVLA7fqXT9ujqoNGaIVEmgdbK1Mk\r\nSPFXBFyVW3hteod83D0UqFlltwp4A3ageCYFVJTp50d3AAMFA/44YCQQbg9x9Jvz\r\nHX3VH7CRX+raEDkDL3Pbz0PHas7bwI7gzZ+GFyNKaCvrHQOyuR8RIKIbjtQYnXr1\r\n675ConCTceIXhysY32sTn5V6UFUW2t0xaRfas8sZBbLDyIJkpt4fyD+6OaRoui9K\r\nZqXMNwt7i/XFIto/sWd/OK3SIgZkAf4HAwIoimqPHVJZM85dNw6JtvLKFvvmkm3X\r\nuoCUG5nU6cgk6vetUYiykuKpU4zG3mDtdZdIZf76hJJ6lZTSHH9frLy7bRYPfu/k\r\nU1AFd1T1OxENiEYEGBECAAYFAjo41N0ACgkQr4IkT5zZ/VVUmgCffq49ZSEJ0Zdt\r\nIvEaJth8G3t63vUAn0mBZ5GVMNMihdg3wn6qw83RgLG2iEYEGBECAAYFAjo41N4A\r\nCgkQr4IkT5zZ/VXuIgCfRaxjJRUe7Phln9bHE3nsq7YSqz0An0qXP2KbO9mhSO4T\r\n38iGvTLnq1We\r\n=m0YJ\r\n-----END PGP PRIVATE KEY BLOCK-----\r\n";

#[sealed_test]
fn test_gpgme_verify_sig() {
    fn make_fut(
        secret: bool,
        local: bool,
        pattern: String,
        ctx: &mut Context,
    ) -> Result<impl Future<Output = Result<Vec<Key>>>> {
        if local {
            ctx.set_auto_key_locate(LocateKey::LOCAL)?;
        } else {
            ctx.set_auto_key_locate(LocateKey::WKD | LocateKey::LOCAL)?;
        }
        ctx.keylist(secret, Some(pattern))
    }

    let tempdir = tempfile::tempdir().unwrap();

    unsafe {
        std::env::set_var("GNUPGHOME", tempdir.path());
    }
    unsafe {
        std::env::set_var("GPG_AGENT_INFO", "");
    }

    let mut gpgme_ctx = match Context::new() {
        Ok(v) => v,
        Err(err) if err.kind.is_not_found() => {
            eprintln!("INFO: libgpgme could not be loaded, skipping this test.");
            return;
        }
        err => err.unwrap(),
    };
    gpgme_ctx.set_protocol(Protocol::OpenPGP).unwrap();
    let current_engine_info = gpgme_ctx.engine_info().unwrap();
    let prev_len = current_engine_info.len();
    let Some(EngineInfo {
        file_name: Some(engine_file_name),
        ..
    }) = current_engine_info
        .into_iter()
        .find(|eng| eng.protocol == Protocol::OpenPGP)
    else {
        eprintln!("WARN: No openpg protocol engine returned from gpgme.");
        return;
    };
    gpgme_ctx
        .set_engine_info(
            Protocol::OpenPGP,
            Some(Cow::Owned(CString::new(engine_file_name).unwrap())),
            Some(Cow::Owned(
                CString::new(tempdir.path().display().to_string()).unwrap(),
            )),
        )
        .unwrap();
    let new_engine_info = gpgme_ctx.engine_info().unwrap();
    assert_eq!(
        new_engine_info.len(),
        prev_len,
        "new_engine_info was expected to have {} entry/ies but has {}: {:#?}",
        prev_len,
        new_engine_info.len(),
        new_engine_info
    );
    assert_eq!(
        new_engine_info[0].home_dir,
        Some(tempdir.path().display().to_string()),
        "new_engine_info was expected to have temp dir as home_dir but has: {:#?}",
        new_engine_info[0].home_dir
    );
    let mut pubkey_data = Some(gpgme_ctx.new_data_mem(PUBKEY).unwrap());
    for _ in 0..2 {
        let keylist: Vec<Key> = smol::block_on(async {
            make_fut(false, true, "".to_string(), &mut gpgme_ctx)
                .unwrap()
                .await
        })
        .unwrap();

        if let Some(pubkey_data) = pubkey_data.take() {
            assert_eq!(
                &keylist,
                &[],
                "keylist should have been empty but is: {:?}",
                keylist
            );
            gpgme_ctx.import_key(pubkey_data).unwrap();
        } else {
            let assert_key = |key: &Key| {
                key.fingerprint() == "ADAB7FCC1F4DE2616ECFA402AF82244F9CD9FD55"
                    && key.primary_uid()
                        == Some(Address::new(
                            Some("Joe Random Hacker".into()),
                            "joe@example.com".into(),
                        ))
                    && key.can_encrypt()
                    && key.can_sign()
                    && !key.secret()
                    && !key.revoked()
                    && !key.expired()
                    && !key.invalid()
            };
            assert_eq!(keylist.len(), 1);
            assert!(
                assert_key(&keylist[0]),
                "keylist expected to have {:?} but got {:?}",
                "ADAB7FCC1F4DE2616ECFA402AF82244F9CD9FD55",
                keylist[0]
            );
        }
    }
    gpgme_ctx
        .import_key(gpgme_ctx.new_data_mem(SECKEY).unwrap())
        .unwrap();
    gpgme_ctx
        .import_key(gpgme_ctx.new_data_mem(SECKEY).unwrap())
        .unwrap_err();
}
