#! /usr/bin/env python3
"""
meli - sample plugin

Copyright 2019 Manos Pitsidianakis

This file is part of meli.

meli is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

meli is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with meli. If not, see <http://www.gnu.org/licenses/>.
"""

import sys
import subprocess
print(sys.path, file=sys.stderr)
from libmeliapi import Client

if __name__ == "__main__":
    server_address = './soworkfile'
    client = Client(server_address)
    client.connect()
    try:
        _bytes = client.read()
        print('got bytes {!r}'.format(_bytes),file=sys.stderr, )

        # run() returns a CompletedProcess object if it was successful
        # errors in the created process are raised here too
        process = subprocess.run(['tiv','-w', '120','-h', '40', _bytes[0]], check=True, stdout=subprocess.PIPE, universal_newlines=True)
        output = process.stdout
        print('tiv output len {}'.format(len(output)),file=sys.stderr, )
        #print('tiv output bytes {!r}'.format(output),file=sys.stderr, )

        message = { "t": "ansi", "c": output }
        #print('sending {!r}'.format(message),file=sys.stderr, )
        print('returned :', client.send(message), file=sys.stderr,)
    except Exception as msg:
        print(msg, file=sys.stderr,)

