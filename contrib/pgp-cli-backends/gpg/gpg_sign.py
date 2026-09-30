#!/usr/bin/env python3
# SPDX-License-Identifier: EUPL-1.2 OR GPL-3.0-or-later
# Copyright 2026 Manos Pitsidianakis

import subprocess
import sys
import json
import re
import os


file = open(sys.argv[1], "r")
keys = sys.argv[2:]

if len(keys) == 0:
    print("No keys passed as arguments to", sys.argv[0])
    sys.exit(1)

status_fd = os.pipe()
logger_fd = os.pipe()

auto_key_locate = "local"
if "AUTO_KEY_LOCATE" in os.environ:
    auto_key_locate = os.environ["AUTO_KEY_LOCATE"]

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
            "--sign",
            "--detach",
            "--armor",
            "-u",
            *keys,
            "--output",
            "-",
            "--",
            f"-&{file.fileno()}",
        ],
        timeout=2,
        check=True,
        capture_output=True,
        text=False,
        pass_fds=[status_fd[1], logger_fd[1], file.fileno()],
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
                "stderr": exc.stderr.decode("utf-8"),
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

SIG_CREATED = r"^\[GNUPG:\] SIG_CREATED (?P<type>[DCS])\w* (?P<pk_algo>\d+) (?P<hash_algo>\d+) (?P<class>\d+) (?P<timestamp>(?:(?:\d{4}-\d{2}-\d{2})|(:?\d+))) (?P<keyfpr>\w+)\n"

sig = re.compile(SIG_CREATED, flags=re.M).search(status)
fingerprint = sig.group("keyfpr")

try:
    hash_algo = int(sig.group("hash_algo"))
except ValueError as exc:
    print(
        json.dumps(
            {
                "returncode": 0,
                "cmd": None,
                "stdout": s.stdout.decode("utf-8"),
                "stderr": s.stderr.decode("utf-8"),
                "status_fd": status,
                "logger_fd": logger,
            }
        )
    )
    sys.exit(1)

if hash_algo == 0:
    hash_algo = "None"
elif hash_algo == 1:
    hash_algo = "MD5"
elif hash_algo == 2:
    hash_algo = "SHA1"
elif hash_algo == 3:
    hash_algo = "RMD160"
elif hash_algo == 5:
    hash_algo = "MD2"
elif hash_algo == 6:
    hash_algo = "TIGER"
elif hash_algo == 7:
    hash_algo = "HAVAL"
elif hash_algo == 8:
    hash_algo = "SHA256"
elif hash_algo == 9:
    hash_algo = "SHA384"
elif hash_algo == 10:
    hash_algo = "SHA512"
elif hash_algo == 11:
    hash_algo = "SHA224"
elif hash_algo == 301:
    hash_algo = "MD4"
elif hash_algo == 302:
    hash_algo = "CRC32"
elif hash_algo == 303:
    hash_algo = "CRC32RFC1510"
elif hash_algo == 304:
    hash_algo = "CRC24RFC2440"
else:
    hash_algo = str(other)

print(
    json.dumps(
        [
            {"fingerprint": fingerprint, "hash_algorithm": hash_algo},
            [b for b in s.stdout],
        ]
    )
)
