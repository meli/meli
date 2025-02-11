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


"""
This script generates MDOC(7) bibliographic ("reference") blocks from a
hardcoded list of RFC ids. They can be used for inclusion in a manual page.

The output is formatted like this example from mdoc(7):


 Rs
     Begin a bibliographic (“reference”) block. Does not have any head
     arguments. The block macro may only contain %A, %B, %C, %D, %I, %J, %N,
     %O, %P, %Q, %R, %T, %U, and %V child macros (at least one must be
     specified).

     Examples:

     .Rs
     .%A J. E. Hopcroft
     .%A J. D. Ullman
     .%B Introduction to Automata Theory, Languages, and Computation
     .%I Addison-Wesley
     .%C Reading, Massachusetts
     .%D 1979
     .Re

     If an Rs block is used within a SEE ALSO section, a vertical space is
     asserted before the rendered output, else the block continues on the
     current line.

For example, the output for RFC1524 which describes the `mailcap` format is:

  .Rs
  .%B RFC1524 A User Agent Configuration Mechanism For Multimedia Mail Format Information
  .%I Legacy
  .%D September 01, 1993
  .%A Dr. Nathaniel S. Borenstein
  .%U https://datatracker.ietf.org/doc/rfc1524/
  .Re
"""

from urllib.parse import urlparse
import json
from datetime import datetime
import http.client
from http import HTTPMethod, HTTPStatus
import functools
import argparse
import re
import time
import random
import sys


@functools.cache
def get_request(url_):
    """Perform an HTTP GET request, follow any redirections and return the final HTTP content."""
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
        get_request.redirects += 1
        if get_request.redirects > 3:
            return None
        if response.getheader("Location"):
            return get_request(response.getheader("Location"))
        return None
    get_request.redirects = 0
    if response.status == http.HTTPStatus.OK:
        return response.read()
    return None


get_request.redirects = 0


@functools.cache
def head_request(url_):
    """Perform an HTTP HEAD request, follow any redirections and return the final URL."""
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
        head_request.redirects += 1
        if head_request.redirects > 3:
            return None
        if response.getheader("Location"):
            return head_request(response.getheader("Location"))
        return None
    head_request.redirects = 0
    if response.status == http.HTTPStatus.OK:
        return url_
    return None


head_request.redirects = 0


def fetch_rfc_json(ident: str) -> str:
    """Fetch JSON source for an "rfcNNNN" from IETF's Datatracker."""
    url = f"https://datatracker.ietf.org/doc/{ident.lower()}/doc.json"
    body = get_request(url)
    return json.loads(body)


def rfc_to_citation(ident: str) -> str:
    """Convert an "rfcNNNN" input to an mdoc(7) citation block"""
    # Fetch json as a dictionary
    response_dict = fetch_rfc_json(ident)
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
    doc_url = head_request(f"https://datatracker.ietf.org{url_path}")
    if doc_url:
        retval += f".%U {doc_url}\n"
    retval += ".Re"
    return retval


DEFAULT_RFCS = [
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

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description=(
            'Generates MDOC(7) bibliographic ("reference") blocks from '
            "a hardcoded list of RFC ids. They can be used for inclusion in a manual "
            "page. If no RFC id arguments are given, a default hard-coded list is "
            "processed instead."
        ),
    )

    def rfc_title(string):
        """Validate `string` as an rfc id number, with a case-insensive "RFC" prefix"""
        if re.search(r"^[rR][fF][cC]\d{3,5}$", string, re.MULTILINE):
            return string.lower()
        raise ValueError(f"{string} is not of the form 'rfcNNNN' (case insensitive)")

    parser.add_argument(
        "-o",
        "--output",
        type=argparse.FileType("w"),
        default=sys.stdout,
        help="Output file. Default is stdout",
    )
    parser.add_argument(
        metavar="RFC_TITLE",
        dest="rfc_list",
        nargs="*",
        type=rfc_title,
        default=DEFAULT_RFCS,
        help="space-seperated RFC titles of the form RFCNNNN",
    )
    args = parser.parse_args()
    for rfc in args.rfc_list:
        print(rfc_to_citation(rfc), file=args.output)
        time.sleep(random.uniform(0, 2))
