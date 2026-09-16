#!/usr/bin/env python3
# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later
# Copyright 2026 Manos Pitsidianakis

import subprocess
import sys
import json
import re
import os

status_fd = os.pipe()
logger_fd = os.pipe()

if len(sys.argv) <= 1:
    print(sys.argv, "needs two arguments, the signature file and the signed data file")
    sys.exit(1)

sig_file = open(sys.argv[1])
signed_file = open(sys.argv[2])

auto_key_locate = "local"
if "AUTO_KEY_LOCATE" in os.environ:
    auto_key_locate = os.environ["AUTO_KEY_LOCATE"]

# TODO
is_cleartext = "CLEARTEXT" in os.environ

try:
    s = subprocess.run(
        [
            "gpg",
            "--enable-special-filenames",
            f"--auto-key-locate={auto_key_locate}",
            "--disable-dirmngr",
            "--batch",
            "--status-fd",
            str(status_fd[1]),
            "--logger-fd",
            str(logger_fd[1]),
            "--no-tty",
            "--charset=utf8",
            "--enable-progress-filter",
            "--exit-on-status-write-error",
            "--verify",
            "--",
            f"-&{sig_file.fileno()}",
            f"-&{signed_file.fileno()}",
        ],
        timeout=2,
        check=False,
        capture_output=True,
        text=False,
        pass_fds=[status_fd[1], logger_fd[1], sig_file.fileno(), signed_file.fileno()],
    )
except subprocess.CalledProcessError as exc:
    os.close(status_fd[1])
    os.close(logger_fd[1])
    status = os.fdopen(status_fd[0])
    status = status.read()
    logger = os.fdopen(logger_fd[0])
    logger = logger.read()
    print(
        json.dumps(
            {
                "returncode": exc.returncode,
                "cmd": exc.cmd,
                "stdout": exc.stdout.decode("utf-8"),
                "stderr": exc.stdout.decode("utf-8"),
                "status_fd": status,
                "logger_fd": logger,
            }
        )
    )
    sys.exit(1)

os.close(status_fd[1])
os.close(logger_fd[1])
status = os.fdopen(status_fd[0])
status = status.read()
logger = os.fdopen(logger_fd[0])
logger = logger.read()

fpr = r"^\[GNUPG:\] KEY_CONSIDERED (?P<fingerprint>[^ ]*).*$"
trust = r"^\[GNUPG:\] TRUST_(?P<trust>[^ ]*) (?P<error_token>\d\d*)(?: (?P<validation_model>\w+))?.*$"
validity = r"^\[GNUPG:\] VALIDSIG (?P<fingerprint_in_hex>\w+) (?P<sig_creation_date>\d{4}-\d{2}-\d{2}) (?P<sig_timestamp>(?:(?:\d{4}-\d{2}-\d{2})|(:?\d+))) (?P<expire_timestamp>(?:(?:\d{4}-\d{2}-\d{2})|(:?\d+))) (?P<sig_version>\d+) (?P<reserved>\d+) (?P<pubkey_algo>\d+) (?P<hash_algo>\d+) (?P<sig_class>\d+)(?: (?P<primary_key_fpr>\w+))?$"
summary = r"^\[GNUPG:\] (?P<summary>(?:GOODSIG|BADSIG|REVKEYSIG|EXPKEYSIG|EXPSIG)) (?P<keyid>\w+)"
err = r"^\[GNUPG:\] ERRSIG (?P<keyid>\w+) (?P<pkalgo>\w+) (?P<hashalgo>\w+) (?P<sig_class>\w+) (?P<time>(?:(?:\d{4}-\d{2}-\d{2})|(:?\d+))) (?P<rc>\d+)"

err = re.compile(err, flags=re.M).search(status)

if err:
    print(json.dumps(err.group(0)))
else:
    fingerprint = re.compile(fpr, flags=re.M).search(status).group("fingerprint")
    trust = re.compile(trust, flags=re.M).search(status)
    trust_level = trust.group("trust")
    trust_error = trust.group("error_token") if trust.group("error_token") else 0
    validation_model = trust.group("validation_model")
    validity = bool(re.compile(validity, flags=re.M).search(status))
    summary = re.compile(summary, flags=re.M).search(status).group("summary")

    summary_val = []

    if summary == "GOODSIG":
        # TODO: When is it GREEN?
        # summary_val.append("GREEN")
        pass
    elif summary == "BADSIG":
        summary_val.append("RED")
    elif summary == "REVKEYSIG":
        summary_val.append("KEY_REVOKED")
    elif summary == "EXPKEYSIG":
        summary_val.append("KEY_EXPIRED")
    elif summary == "EXPSIG":
        summary_val.append("SIG_EXPIRED")
    print(
        json.dumps(
            [
                {
                    "summary": summary_val,
                    "cert": {
                        "keyid": fingerprint,
                    },
                    "validity": trust_level,
                    "cleartext": is_cleartext,
                }
            ]
        )
    )
