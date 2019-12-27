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
import time
import subprocess
import msgpack
import nntplib
import libmeliapi
import itertools

def chunks(iterable, n):
    while True:
        try:
            yield itertools.chain((next(iterable),), itertools.islice(iterable, n-1))
        except:
            break


if __name__ == "__main__":
    import importlib
    importlib.reload(libmeliapi)
    server_address = './soworkfile'
    client = libmeliapi.Client(server_address)
    client.connect()
    #client.setblocking(True)
    try:
        counter = 0
        while True:
            print("[nntp-plugin]: loop = ", counter, flush=True, file=sys.stderr)
            counter += 1
            req = client.read()
            if req is None:
                time.sleep(0.15)
                continue
            #client.setblocking(True)
            client.ack()
            print("[nntp-plugin]: ", "req: ", req, flush=True, file=sys.stderr)
            sys.stderr.flush()
            if isinstance(req, msgpack.ExtType):
                print("[nntp-plugin]: ", req, flush=True, file=sys.stderr)
                if req.data == b'is_online':
                    client.backend_fn_ok_send(None)
                elif req.data == b'get':
                    s = nntplib.NNTP('news.gmane.org')
                    resp, count, first, last, name = s.group('gmane.comp.python.committers')
                    print('Group', name, 'has', count, 'articles, range', first, 'to', last, flush=True, file=sys.stderr)

                    resp, overviews = s.over((last - 9, last))
                    ids = []
                    for id, over in overviews:
                        ids.append(id)
                        print(id, nntplib.decode_header(over['subject']), flush=True, file=sys.stderr)
                    for chunk in chunks(iter(ids), 2):
                        ret = []
                        for _id in chunk:
                            resp, info = s.article(_id)
                            #print(_id, " line0 = ", str(info.lines[0], 'utf-8', 'ignore'))
                            elem = b'\n'.join(info.lines)
                            ret.append(str(elem, 'utf-8', 'ignore'))
                        print("ret len = ", len(ret), flush=True,file=sys.stderr)
                        client.backend_fn_ok_send(ret)
                        time.sleep(0.85)
                    s.quit()
                    client.backend_fn_ok_send(None)
            #client.setblocking(True)
            time.sleep(0.15)


    except Exception as msg:
        print("[nntp-plugin]: ", msg, flush=True, file=sys.stderr,)
        sys.stderr.flush()

