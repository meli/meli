# meli - Makefile
#
# Copyright 2017-2020 Manos Pitsidianakis
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

# Options
PREFIX ?= /usr/local
BINDIR ?= ${PREFIX}/bin
MANDIR ?= ${PREFIX}/share/man

CARGO_TARGET_DIR ?= target
MIN_RUSTC ?= 1.39.0
CARGO_BIN ?= cargo

# Installation parameters
MANPAGES ?= meli.1 meli.conf.5 meli-themes.5
ifdef MELI_FEATURES
  FEATURES ?= --features="$(MELI_FEATURES)" 
else
  FEATURES ?=
endif

ifndef MANPATH
  MANPATH = `manpath`
endif
VERSION ?= `sed -n "s/^version\s*=\s*\"\(.*\)\"/\1/p" Cargo.toml`

# Output parameters
BOLD ?= tput bold
UNDERLINE ?= tput smul
ANSI_RESET ?= tput sgr0
ifdef NO_COLOR
	RED ?= 
  GREEN ?= 
	CARGO_COLOR ?= --color=never 
else
	RED ?= tput setaf 1
  GREEN ?= tput setaf 2
endif

.POSIX:
.SUFFIXES:
help:
	@echo "For a quick start, build and install locally:\n `${BOLD}``${GREEN}`PREFIX=~/.local make install`${ANSI_RESET}`\n"
	@echo "Available subcommands:"
	@echo " - `${BOLD}`install`${ANSI_RESET}` (installs binary and documentation)"
	@echo " - `${BOLD}`uninstall`${ANSI_RESET}`"
	@echo "Secondary subcommands:"
	@echo " - `${BOLD}`clean`${ANSI_RESET}` (cleans build artifacts)"
	@echo " - `${BOLD}`check-deps`${ANSI_RESET}` (checks dependencies)"
	@echo " - `${BOLD}`install-bin`${ANSI_RESET}` (installs binary to BINDIR)"
	@echo " - `${BOLD}`install-doc`${ANSI_RESET}` (installs manpages to MANDIR)"
	@echo " - `${BOLD}`help`${ANSI_RESET}` (prints this information)"
	@echo "\nENVIRONMENT variables of interest:"
	@echo "* PREFIX = ${PREFIX}"
	@echo "* MELI_FEATURES = ${MELI_FEATURES}"
	@echo "* BINDIR = ${BINDIR}"
	@echo "* MANDIR = ${MANDIR}"

meli: check-deps
	${CARGO_BIN} build ${CARGO_COLOR}--target-dir="${CARGO_TARGET_DIR}" $(FEATURES)--release


.PHONY: check-deps
check-deps:
	@if ! echo ${MIN_RUSTC}\\n`${CARGO_BIN} --version | cut -d ' ' -f 2` | sort -CV; then echo "rust version >= `${RED}`${MIN_RUSTC}`${ANSI_RESET}` required, found: `which ${CARGO_BIN}` `${CARGO_BIN} --version | cut -d ' ' -f 2`" \
		"\nYour options:\n - Set CARGO_BIN to a supported version\n - Install a supported version from your distribution's package manager\n - Install a supported version from `${UNDERLINE}`https://rustup.rs/`${ANSI_RESET}`" ; exit 1; fi


.PHONY: clean
clean:
	-rm -rf ./${CARGO_TARGET_DIR}/

.PHONY: distclean
distclean: clean
	@rm -f meli-${VERSION}.tar.gz

.PHONY: uninstall
uninstall:
	rm -f $(DESTDIR)${BINDIR}/meli
	-rm $(DESTDIR)${MANDIR}/man1/meli.1.gz
	-rm $(DESTDIR)${MANDIR}/man5/meli.conf.5.gz
	-rm $(DESTDIR)${MANDIR}/man5/meli-themes.5.gz

.PHONY: install-doc
install-doc:
ifndef NO_MAN
		@mkdir -p $(DESTDIR)${MANDIR}/man1
		@mkdir -p $(DESTDIR)${MANDIR}/man5
		@echo " - `${BOLD}`Installing manpages to `${ANSI_RESET}`${DESTDIR}${MANDIR}:"
		@for MANPAGE in ${MANPAGES}; do \
			SECTION=`echo $${MANPAGE} | rev | cut -d "." -f 1`; \
			MANPAGEPATH=${DESTDIR}${MANDIR}/man$${SECTION}/$${MANPAGE}.gz; \
			echo "  * installing $${MANPAGE} → `${GREEN}`$${MANPAGEPATH}`${ANSI_RESET}`"; \
			gzip < $${MANPAGE} > $${MANPAGEPATH} \
    ; done
	@case ":${MANPATH}:" in \
  *:${DESTDIR}${MANDIR}:*) echo -n "";; \
	*) echo "\n`${RED}``${BOLD}`WARNING`${ANSI_RESET}`: `${UNDERLINE}`Path ${DESTDIR}${MANDIR} is not contained in your MANPATH variable or the output of \`manpath\` command.`${ANSI_RESET}` \`man\` might fail finding the installed manpages. Consider adding it if necessary.\nMANPATH variable / output of \`manpath\`: ${MANPATH}" ;; \
	esac
else
		@echo "NO_MAN is defined, so no documentation is going to be installed."
endif

.PHONY: install-bin
install-bin: meli
	@mkdir -p $(DESTDIR)${BINDIR}
	@echo " - `${BOLD}`Installing binary to `${ANSI_RESET}``${GREEN}`${DESTDIR}${BINDIR}/meli`${ANSI_RESET}`"
	@case ":${PATH}:" in \
  *:${DESTDIR}${BINDIR}:*) echo -n "";; \
	*) echo "\n`${RED}``${BOLD}`WARNING`${ANSI_RESET}`: `${UNDERLINE}`Path ${DESTDIR}${BINDIR} is not contained in your PATH variable.`${ANSI_RESET}` Consider adding it if necessary.\nPATH variable: ${PATH}";; \
	esac
	@install -D ./${CARGO_TARGET_DIR}/release/meli $(DESTDIR)${BINDIR}/meli


.PHONY: install
.NOTPARALLEL: yes
install: meli install-bin install-doc
ifndef NO_MAN
	@echo "\n You're ready to go. You might want to read the \"STARTING WITH meli\" section in the manpage (\`man meli\`)"
endif
	@echo " - Report bugs in the mailing list or git issue tracker `${UNDERLINE}`https://git.meli.delivery`${ANSI_RESET}`"
	@echo " - If you have a specific feature or workflow you want to use, you can post in the mailing list or git issue tracker."

.PHONY: dist
dist:
	@git archive --format=tar.gz --prefix=meli-${VERSION}/ HEAD >meli-${VERSION}.tar.gz
	@echo meli-${VERSION}.tar.gz
