"""
meli - python3 api plugin

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

from collections import deque
import errno
import json
import msgpack
import socket
import struct
import sys
import time

class IPCError(Exception):
    pass

class UnknownMessageClass(IPCError):
    pass

class InvalidSerialization(IPCError):
    pass

class ConnectionClosed(IPCError):
    pass


def _read_objects(sock):
    unpacker = msgpack.Unpacker()
    ret = []
    #reader = socket.socket.makefile(sock, 'rb')
    while True:
        try:
            buf = sock.recv(1024**2)
            if not buf:
                break
            unpacker.feed(buf)
            for o in unpacker:
                ret.append(o)
        except:
            break
    return ret

    #try:
    #    for unpack in unpacker:
    #        return unpack
    #except Exception as e:
    #    print("[libmeliapi]: ", "_read_objects error ", e, file=sys.stderr,)
    #    return None
    #finally:
    #    reader.flush()

def _write_objects(sock, objects):
    sys.stderr.flush()
    print("[libmeliapi]: ", "_write_objects ", objects, flush=True, file=sys.stderr, )
    data = msgpack.packb(objects)
    #print("[libmeliapi]: ", "_write_objects data ", data, flush=True, file=sys.stderr, )
    sent = 0

    while sent < len(data):
        try:
            _len = min(len(data[sent:]), 2048)
            sent += sock.send(data[sent:sent+_len])
        except IOError as e:
            print("[libmeliapi]: IOError: ", e, e.errno, flush=True, file=sys.stderr, )
            sys.stderr.flush()
            if e.errno == errno.EWOULDBLOCK:
                break
            elif e.errno == errno.EAGAIN:
                time.sleep(0.001)
                continue
            else:
                raise

class Client(object):
    def __init__(self, server_address):
        self.buffer = deque()
        self.addr = server_address
        address_family = socket.AF_UNIX
        self.sock = socket.socket(address_family, socket.SOCK_STREAM)
        self.sock.setblocking(0)

    def connect(self):
        try:
            self.sock.connect(self.addr)

            print("[libmeliapi]: ", "self.send({ \"version\": \"dev\" }) = ",self.send({ "version": "dev" }), flush=True, file=sys.stderr)
            self.expect_ack()
            self._session = self.read()
            self.ack()
            print("[libmeliapi]: ", "self.buffer =", self.buffer, flush=True, file=sys.stderr, )
            print("[libmeliapi]: ", "connected, session id is", self._session, flush=True, file=sys.stderr)
        except socket.error as msg:
            print("[libmeliapi]: ", msg, flush=True, file=sys.stderr, )
            sys.stderr.flush()
            sys.exit(1)

    def close(self):
        self.sock.close()

    def setblocking(self, new_val):
        self.sock.setblocking(new_val)

    def __enter__(self):
        self.connect()
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.close()

    def send(self, objects):
        sys.stderr.flush()
        #print("[libmeliapi]: ", "stuck in send ", self.buffer, flush=True, file=sys.stderr, )
        _write_objects(self.sock, objects)
        #print("[libmeliapi]: ", "unstuck wrote objs", flush=True, file=sys.stderr, )
        #print("[libmeliapi]: ", "wrote object ", objects, file=sys.stderr)
        time.sleep(0.001)

    def ack(self):
        sys.stderr.flush()
        _write_objects(self.sock, 0x06)
        time.sleep(0.001)

    def expect_ack(self):
        #print("[libmeliapi]: expect_ack, ", self.buffer, flush=True, file=sys.stderr, )
        while True:
            time.sleep(0.1)
            read_list = _read_objects(self.sock)
            self.buffer.extend(read_list)
            try:
                self.buffer.remove(0x6)
                #print("[libmeliapi]: got_ack, ", self.buffer, flush=True, file=sys.stderr, )
                return
            except ValueError:
                pass

    def read(self):
        sys.stderr.flush()
        #print("[libmeliapi]: ", "stuck in read ", self.buffer, flush=True, file=sys.stderr, )
        read_list = _read_objects(self.sock)
        time.sleep(0.01)
        self.buffer.extend(read_list)
        #print("[libmeliapi]: ", "unstuck read self.buffer =", self.buffer, flush=True, file=sys.stderr, )
        if len(self.buffer) > 0:
            return self.buffer.popleft()
        else:
            return None

    @property
    def backend_fn_type(self):
        return 0

    @property
    def backend_op_fn_type(self):
        return 1

    def ok_send(self, objects):
        self.send({"t": "ok", "c": objects })
        self.expect_ack()

    def err_send(self, objects):
        self.send({"t": "err", "c": objects })
        self.expect_ack()
