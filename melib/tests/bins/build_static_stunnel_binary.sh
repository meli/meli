#!/bin/bash
set -eux

tar -xJvf stunnel-5.70.tar.xz
cd stunnel-5.70
./configure LDFLAGS='--static' --with-pic --disable-systemd --disable-fips --disable-ipv6 --disable-largefile --disable-libtool-lock --enable-static --enable-static=yes --enable-shared=no --disable-dependency-tracking
make
cd ..
cp -i stunnel-5.70/src/stunnel .
