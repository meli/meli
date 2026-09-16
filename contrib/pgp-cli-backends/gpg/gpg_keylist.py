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

if len(sys.argv) > 2:
    print(sys.argv, "needs at most one argument")
    sys.exit(1)

pattern = []
if len(sys.argv) == 2:
    pattern.append(sys.argv[1])

auto_key_locate = "local"
if "AUTO_KEY_LOCATE" in os.environ:
    auto_key_locate = os.environ["AUTO_KEY_LOCATE"]
list_keys = "--list-keys"
if "SECRET" in os.environ:
    list_keys = "--list-secret-keys"

try:
    s = subprocess.run(
        [
            "gpg",
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
            "--with-colons",
            list_keys,
            "--",
            *pattern,
        ],
        timeout=2,
        check=False,
        capture_output=True,
        text=False,
        pass_fds=[status_fd[1], logger_fd[1]],
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


err = r"^\[GNUPG:\] FAILURE (?P<desc>[^ ]+) (?P<error_code>\w+)$"
err = re.compile(err, flags=re.M).search(status)

if err:
    print(json.dumps(err.group(0)))
else:
    keys = []

    primary_uid = None
    fingerprint = None
    revoked = False
    expired = False
    disabled = False
    invalid = False
    can_encrypt = False
    can_sign = False
    secret = False

    # https://github.com/gpg/gnupg/blob/master/doc/DETAILS#format-of-the-colon-listings

    for line in s.stdout.decode("utf-8").splitlines():
        if line.startswith("pub"):
            keys.append(
                {
                    "primary_uid": primary_uid,
                    "fingerprint": fingerprint,
                }
            )
            if len(keys) > 0:
                primary_uid = None
                fingerprint = None
                revoked = False
                expired = False
                disabled = False
                invalid = False
                can_encrypt = False
                can_sign = False
                secret = False
        if line.startswith("fpr:::::::::"):
            (fpr, _) = line.removeprefix("fpr:::::::::").split(":")
            fingerprint = fpr
            continue
        fields = line.split(":")
        match fields[0]:
            case "uid":
                primary_uid = fields[9]
            case _:
                continue
    del keys[0]

    keys.append(
        {
            "primary_uid": primary_uid,
            "fingerprint": fingerprint,
        }
    )
    # TODO: decode user id (it's escaped)
    # TODO: secret, capabilities, revoked, expired

    print(json.dumps(keys))
