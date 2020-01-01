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

class NNTPClient(libmeliapi.Client):
    def __init__(self, stream_address, server_address, newsgroup):
        super().__init__(stream_address)
        self.bytes_cache = {}
        self.conn = nntplib.NNTP(server_address)
        self.newsgroup = newsgroup
    def backend_req(self, req):
        print("[nntp-plugin]: backend_req = ", req, flush=True, file=sys.stderr)
        if req.data == b'is_online':
            self.ok_send(None)
        elif req.data == b'get':
            resp, count, first, last, name = self.conn.group(self.newsgroup)
            print('Group', name, 'has', count, 'articles, range', first, 'to', last, flush=True, file=sys.stderr)

            resp, overviews = self.conn.over((0, last))
            for chunk in chunks(iter(reversed(overviews)), 100):
                ret = []
                for id, over in chunk:
                    #print(id, nntplib.decode_header(over['subject']), flush=True, file=sys.stderr)
                    env = {}
                    env["hash"] = id
                    env["subject"] = nntplib.decode_header(over["subject"])
                    env["from"] = nntplib.decode_header(over["from"])
                    env["date"] = nntplib.decode_header(over["date"])
                    env["message_id"] = nntplib.decode_header(over["message-id"])
                    env["references"] = nntplib.decode_header(over["references"])
                    try:
                        env["to"] = nntplib.decode_header(over["to"])
                    except KeyError:
                        env["to"] = self.newsgroup
                    ret.append(env)
                print("ret len = ", len(ret), flush=True,file=sys.stderr)
                self.ok_send(ret)
            self.ok_send(None)
    def backend_op_req(self, req):
        print("[nntp-plugin]: backend_op_req = ", req, flush=True, file=sys.stderr)
        if req.data == b'as_bytes':
            _hash = self.read()
            print("[nntp-plugin]: hash = ", _hash, flush=True, file=sys.stderr)
            self.ack()
            try:
                try:
                    self.ok_send(self.bytes_cache[_hash])
                except KeyError:
                    resp, info = self.conn.article(_hash)
                    #print(_id, " line0 = ", str(info.lines[0], 'utf-8', 'ignore'))
                    elem = b'\n'.join(info.lines)
                    self.bytes_cache[_hash] = str(elem, 'utf-8', 'ignore')
                    self.ok_send(self.bytes_cache[_hash])
            except Exception as e:
                self.err_send(str(e))


if __name__ == "__main__":
    import importlib
    importlib.reload(libmeliapi)
    stream_address = './soworkfile'
    server_address = 'news.gmane.org'
    newsgroup = 'gmane.comp.python.committers'
    client = NNTPClient(stream_address, server_address, newsgroup)
    client.connect()
    #client.setblocking(True)
    try:
        while True:
            req = client.read()
            if req is None:
                time.sleep(0.15)
                continue
            #client.setblocking(True)
            client.ack()
            print("[nntp-plugin]: ", "req: ", req, flush=True, file=sys.stderr)
            sys.stderr.flush()
            if isinstance(req, msgpack.ExtType):
                if req.code == client.backend_fn_type:
                    client.backend_req(req)
                elif req.code == client.backend_op_fn_type:
                    client.backend_op_req(req)
                print("[nntp-plugin]: ", req, flush=True, file=sys.stderr)
            #client.setblocking(True)
            time.sleep(0.15)
    except:
        raise RuntimeError("Something bad happened")
