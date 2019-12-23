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

import msgpack
import socket
import time
import struct
import json
import sys


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
    #    print("_read_objects error ", e, file=sys.stderr,)
    #    return None
    #finally:
    #    reader.flush()

def _write_objects(sock, objects):
    data = msgpack.packb(objects)
    sock.sendall(data)

def _recursive_subclasses(cls):
    classmap = {}
    for subcls in cls.__subclasses__():
        classmap[subcls.__name__] = subcls
        classmap.update(_recursive_subclasses(subcls))
    return classmap


class Client(object):
    def __init__(self, server_address):
        self.addr = server_address
        if isinstance(self.addr, str):
            address_family = socket.AF_UNIX
        else:
            address_family = socket.AF_INET
        self.sock = socket.socket(address_family, socket.SOCK_STREAM)
        self.sock.setblocking(0)

    def connect(self):
        try:
            self.sock.connect(self.addr)
            print("connected", file=sys.stderr)
        except socket.error as msg:
            print(msg,file=sys.stderr, )
            sys.exit(1)

    def close(self):
        self.sock.close()

    def __enter__(self):
        self.connect()
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.close()

    def send(self, objects):
        _write_objects(self.sock, objects)
        print("wrote object ", objects, file=sys.stderr)
        return self.read()

    def read(self):
        return _read_objects(self.sock)

if __name__ == "__main__":
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    server_address = './soworkfile'
    client = Client(server_address)
    client.connect()
    client.send({ "version": "dev" })
    counter = 0
    try:
        while True:
            message = "This is the message.  And this is the well {}.".format(counter)
            counter += 1
            time.sleep(0.05)
            print('sending {!r}'.format(message),file=sys.stderr, )
            print('returned :', client.send(message), file=sys.stderr,)
    except Exception as msg:
        print(msg, file=sys.stderr,)

