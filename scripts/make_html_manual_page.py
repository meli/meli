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

import subprocess
from threading import Timer
from html.parser import HTMLParser
import argparse
import sys
import re
import shutil
import textwrap
from subprocess import PIPE
from urllib.parse import urlparse
from pathlib import Path
import http.client
from http import HTTPMethod, HTTPStatus
import signal
import functools
from bs4 import BeautifulSoup

NO_TTY = False


def clear_line(signum, frame):
    if NO_TTY:
        return
    columns = shutil.get_terminal_size().columns
    message = chr(27) + "[0G"  # go to start of line
    sys.stdout.write(message)
    message = " " * columns + "\r"
    sys.stdout.write(message)
    sys.stdout.flush()
    draw_progress.max_cols = 0


signal.signal(signal.SIGWINCH, clear_line)

MIRRORS = [
    "http://linux.die.net/man/%S/%N",
    "http://man7.org/linux/man-pages/man%S/%N.%S.html",
    "http://manpages.debian.org/stable/%N.%S.en.html",
    "http://man.bsd.lv/%N.%S",
    "http://man.archlinux.org/man/%N.%S",
    "http://man.voidlinux.org/%N.%S",
    "http://man.bsd.lv/OpenBSD-7.0/%N.%S",
    "http://man.bsd.lv/FreeBSD-13.0/%N.%S",
    "http://man.bsd.lv/POSIX-2013/%N.%S",  # last resorts
    "http://man.bsd.lv/UNIX-7/%N.%S",
    "https://www.unix.com/man-page/mojave/%S/%N/",
]


def add_progress(count=1):
    add_progress.count += count


add_progress.count = 0.0


def draw_progress(total, count=None, status=""):
    if NO_TTY:
        return
    columns = shutil.get_terminal_size().columns
    status = textwrap.shorten(
        status, width=columns - draw_progress.bar_len - len("100.0% ...") - 8
    )
    if count is None:
        count = add_progress.count
    bar_len = draw_progress.bar_len
    filled_len = int(round(bar_len * count / float(total)))

    percents = round(100.0 * count / float(total), 1)
    bar = "=" * filled_len + "-" * (bar_len - filled_len)

    message = f"[{bar}] {percents}% ...{status}"
    draw_progress.max_cols = max(len(message) + 1, draw_progress.max_cols)
    spaces = " " * (draw_progress.max_cols - len(message))
    message += f"{spaces}\r"
    sys.stdout.write(message)
    sys.stdout.flush()


draw_progress.max_cols = 0
draw_progress.bar_len = 62


class ManFixer(HTMLParser):
    whitespace = r"\s{2,}"
    output = ""
    extract_href = False

    def reset(self):
        self.output = ""
        super().reset()

    def handle_starttag(self, tag, attrs):
        attrs = {a[0]: a[1] for a in attrs}
        if tag == "a" and self.extract_href and "href" in attrs:
            self.output += re.sub(self.whitespace, " ", attrs["href"]).replace(
                "\ufeff", ""
            )
            self.output += " "

    def handle_endtag(self, tag):
        pass

    def handle_data(self, data):
        self.output += re.sub(self.whitespace, " ", data).replace("\ufeff", "")

    @staticmethod
    def extract(input_):
        parser = ManFixer()
        parser.feed(input_)
        return parser.output


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
        # print("for ", url_, "following redirect", response.status)
        give_me_head.redirects += 1
        if give_me_head.redirects > 3:
            return None
        if response.getheader("Location"):
            # print("for ", url_, "following redirect to ", response.getheader("Location"))
            return give_me_head(response.getheader("Location"))
        print("bailout")
        return None
    # print("for ", url_, "code is ", response.status)
    give_me_head.redirects = 0
    if response.status == http.HTTPStatus.OK:
        return url_
    return None


give_me_head.redirects = 0


def man_to_path(man: str) -> str:
    exp = r"(.+)(\d{1,})$"
    result = re.match(exp, man)
    if not result:
        return man
    return f"{result[2]}/{result[1][:-1]}"


def draw_spinner():
    if NO_TTY:
        return
    message = (
        chr(27)
        + "["
        + str(draw_spinner.columns - 1)
        + "C"
        + draw_spinner.frames[draw_spinner.index]
        + "\r"
    )
    sys.stdout.write(message)
    sys.stdout.flush()
    draw_spinner.index += 1
    draw_spinner.index = draw_spinner.index % len(draw_spinner.frames)
    draw_spinner.timer = Timer(draw_spinner.interval, draw_spinner)
    draw_spinner.timer.start()


draw_spinner.interval = 0.1
draw_spinner.columns = shutil.get_terminal_size().columns
draw_spinner.frames = ["|", "/", "-", "\\"]
draw_spinner.index = 0

if __name__ == "__main__":
    draw_spinner.timer = Timer(draw_spinner.interval, draw_spinner)
    parser = argparse.ArgumentParser(
        description="Generates a <table> element from a mdoc manpage."
    )
    parser.add_argument("page", type=str, help="mdoc file")
    parser.add_argument(
        "--output",
        "-o",
        type=str,
        help="name of output file",
        required=False,
        default=None,
    )
    parser.add_argument(
        "--name",
        "-n",
        type=str,
        help="name used for html IDs. defaults to file name stem.",
        required=False,
        default=None,
    )
    parser.add_argument(
        "--refs",
        type=bool,
        help="find external manpages and hyperlink to them",
        required=False,
        default=True,
    )
    parser.add_argument(
        "--no-tty",
        help="don't draw progress animation",
        required=False,
        default=False,
        action="store_true",
    )
    parser.add_argument(
        "--include-refs",
        type=str,
        help="comma separated list of manpages to relatively hyperlink",
        required=False,
        default="",
    )
    parser.add_argument(
        "--exclude-refs",
        type=str,
        help="comma separated list of manpages to not hyperlink",
        required=False,
        default="",
    )
    parser.add_argument(
        "--mandoc",
        type=str,
        help="alternative mandoc binary path",
        required=False,
        default="mandoc",
    )
    parser.add_argument(
        "--no-css",
        help="don't prepend <style> element",
        required=False,
        action="store_true",
        default=False,
    )

    args = parser.parse_args()
    if args.exclude_refs:
        args.exclude_refs = [s.strip() for s in args.exclude_refs.split(",")]
    if args.include_refs:
        args.include_refs = [s.strip() for s in args.include_refs.split(",")]
    if not args.output:
        args.output = Path.cwd() / (Path(args.page).name + ".html")
    if not args.name:
        args.name = Path(args.page).name

    manpage = None
    with open(args.page, "r", encoding="utf-8") as f:
        manpage = f.read()
    if args.refs:
        refs_url = ',man="%N\t%S"'
    else:
        refs_url = ""

    NO_TTY = args.no_tty
    html_output = subprocess.run(
        f'{args.mandoc} -I os="rendered by mandoc" -Kutf-8 -Ofragment,toc,includes="#%I"{refs_url} -Thtml "{args.page}" | sed \'s/\s*<\/pre/<\/pre/\'',
        stdout=PIPE,
        shell=True,
        check=True,
    ).stdout.decode("utf-8")

    html_output = re.sub(
        r"(?:(?:[⟨])|(?:&#x27E8;))(.+)(?:(?:[⟩])|(?:&#x27E9;))",
        '<kbd class="manpage-kbd">\\1</kbd>',
        html_output,
        flags=re.MULTILINE,
    )

    soup = BeautifulSoup(html_output, "html.parser")
    targets = set()
    for target in soup.find_all(lambda tag: tag.has_attr("id")):
        id_ = target.get("id")
        targets.add(id_)
    root_table = next(soup.children)
    root_table["id"] = args.name

    if args.refs:
        total = len(soup.find_all("a"))
        print(f"Replacing `href` attributes in {total} hyperlinks...")

        draw_spinner.timer.start()
        for link in soup.find_all("a"):
            href = link.get("href")
            if href.startswith("#") and href[1:] in targets:
                link["href"] = "#" + args.name + "_" + href[1:]
                add_progress()
                draw_progress(total)
            elif href.startswith("#"):
                add_progress()
                draw_progress(total)
            else:
                exp = r"(.+)\t(.+)$"
                result = re.match(exp, href)
                if result:
                    link["href"] = f"./{result[1]}.{result[2]}.html"
                    name = result[1]
                    section = result[2]
                    if (
                        name in args.exclude_refs
                        or f"{name}.{section}" in args.exclude_refs
                    ):
                        add_progress()
                        draw_progress(
                            total,
                            status=f"{name}.{section}: Excluding ref because it is in --exclude-refs list. Leaving it as {link['href']}",
                        )
                        continue
                    found = False
                    for url in MIRRORS:
                        add_progress(1.0 / (len(MIRRORS) * 1.0))
                        if found:
                            continue
                        draw_progress(
                            total,
                            status=f"{name}.{section}: searching for an online mirror",
                        )
                        url_ = url.replace("%N", name).replace("%S", section)
                        try:
                            got = give_me_head(url_)
                            if got:
                                link["href"] = got
                                found = True
                                continue
                        except Exception as exc:
                            if "handshake operation timed out" not in str(exc):
                                print(f"got {exc} for url {url_}")
                    if (
                        not found
                        and args.include_refs
                        and (
                            name in args.include_refs
                            or f"{name}.{section}" in args.include_refs
                        )
                    ):
                        link["href"] = ""
                        draw_progress(
                            total,
                            status=f"{name}.{section}: Excluding ref because it was not found online and is not in --include-refs list. Leaving it empty.",
                        )
                else:
                    add_progress()
                    draw_progress(total)
        draw_spinner.timer.cancel()
        clear_line(None, None)

    for target in soup.find_all(lambda tag: tag.has_attr("id")):
        id_ = target.get("id")
        if id_ in targets:
            id_ = args.name + "_" + id_
            target["id"] = id_

    with open(args.output, "w", encoding="utf-8") as f:
        if not args.no_css:
            f.write(
                """
            <style>
            code.Ic, code.Li, code.Cm, code.Nm, kbd.manpage-kbd{
                display: inline-block;
            }
            kbd {
              background-color: #eee;
              border-radius: 3px;
              border: 1px solid #b4b4b4;
              box-shadow:
                0 1px 1px rgba(0, 0, 0, 0.2),
                0 2px 0 0 rgba(255, 255, 255, 0.7) inset;
              color: #333;
              display: inline-block;
              font-size: 0.85em;
              font-weight: 700;
              line-height: 1;
              padding: 2px 4px;
              white-space: nowrap;
            }
            code {
              background-color: #eee;
              border-radius: 3px;
              font-family: courier, monospace;
              padding: 0 3px;
            }
            </style>
            """
            )
        f.write(soup.prettify())
    print("Written to ", args.output)
