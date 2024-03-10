# meli - scripts/make_html_manual_page.py
#
# Copyright 2023 Manos Pitsidianakis
#
# This file is part of meli.
#
# meli is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# meli is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with meli. If not, see <http://www.gnu.org/licenses/>.

from urllib.parse import urlparse
import json
from datetime import datetime
import http.client
from http import HTTPMethod, HTTPStatus
import functools


@functools.cache
def give_me_get(url_):
    o = urlparse(url_)
    conn = http.client.HTTPSConnection(o.hostname, timeout=6)
    conn.request(HTTPMethod.GET, o.path)
    response = conn.getresponse()
    if response.status in (
        HTTPStatus.FOUND,
        HTTPStatus.TEMPORARY_REDIRECT,
        HTTPStatus.PERMANENT_REDIRECT,
        HTTPStatus.MOVED_PERMANENTLY,
    ):
        give_me_get.redirects += 1
        if give_me_get.redirects > 3:
            return None
        if response.getheader("Location"):
            return give_me_get(response.getheader("Location"))
        return None
    give_me_get.redirects = 0
    if response.status == http.HTTPStatus.OK:
        return response.read()
    return None


give_me_get.redirects = 0


@functools.cache
def give_me_head(url_):
    o = urlparse(url_)
    conn = http.client.HTTPSConnection(o.hostname, timeout=6)
    conn.request(HTTPMethod.HEAD, o.path)
    response = conn.getresponse()
    if response.status in (
        HTTPStatus.FOUND,
        HTTPStatus.TEMPORARY_REDIRECT,
        HTTPStatus.PERMANENT_REDIRECT,
        HTTPStatus.MOVED_PERMANENTLY,
    ):
        give_me_head.redirects += 1
        if give_me_head.redirects > 3:
            return None
        if response.getheader("Location"):
            return give_me_head(response.getheader("Location"))
        return None
    give_me_head.redirects = 0
    if response.status == http.HTTPStatus.OK:
        return url_
    return None


give_me_head.redirects = 0


def fetch_rfc_json(ident: str) -> str:
    url = f"https://datatracker.ietf.org/doc/{ident.lower()}/doc.json"
    body = give_me_get(url)
    response_dict = json.loads(body)
    with open(f"/tmp/rfcs/{ident}.json", "a") as f:
        f.write(json.dumps(response_dict))
    url_path = f"/doc/{ident}/"
    retval = ".Rs\n"
    retval += f".%B {response_dict['name'].upper()} {response_dict['title']}\n"
    retval += f".%I {response_dict['stream']}\n"
    dateval = datetime.strptime(response_dict["time"].split(" ")[0], "%Y-%m-%d")
    for rev in response_dict["rev_history"]:
        if rev["name"] != ident:
            continue
        dateval = datetime.strptime(rev["published"].split("T")[0], "%Y-%m-%d")
        if "url" in rev:
            url_path = rev["url"]
    retval += f".%D {dateval.strftime('%B %d, %Y')}\n"
    if "authors" in response_dict:
        for author in response_dict["authors"]:
            if "name" not in author:
                continue
            retval += f".%A {author['name']}\n"
    doc_url = give_me_head(f"https://datatracker.ietf.org{url_path}")
    if doc_url:
        retval += f".%U {doc_url}\n"
    retval += ".Re"
    return retval


RFCS = [
    "rfc1524",
    "rfc2047",
    "rfc2183",
    "rfc2369",
    "rfc2426",
    "rfc3156",
    "rfc3461",
    "rfc3501",
    "rfc3676",
    "rfc3691",
    "rfc3977",
    "rfc4549",
    "rfc4616",
    "rfc4954",
    "rfc5321",
    "rfc5322",
    "rfc6048",
    "rfc6152",
    "rfc6350",
    "rfc6532",
    "rfc6868",
    "rfc7162",
    "rfc8620",
    "rfc8621",
]

for rfc in RFCS:
    print(fetch_rfc_json(rfc))
