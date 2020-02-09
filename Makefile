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
FEATURES ?= --features "${MELI_FEATURES}"

MANPATHS := `manpath 2> /dev/null`
VERSION ?= `sed -n "s/^version\s*=\s*\"\(.*\)\"/\1/p" Cargo.toml`

# Output parameters
BOLD ?= `[ -z $${TERM} ] && echo "" || tput bold`
UNDERLINE ?= `[ -z $${TERM} ] && echo "" || tput smul`
ANSI_RESET ?= `[ -z $${TERM} ] && echo "" || tput sgr0`
CARGO_COLOR ?= `[ -z $${NO_COLOR+x} ] && echo "" || echo "--color=never "`
RED ?= `[ -z $${NO_COLOR+x} ] && ([ -z $${TERM} ] && echo "" || tput setaf 1) || echo ""`
GREEN ?= `[ -z $${NO_COLOR+x} ] && ([ -z $${TERM} ] && echo "" || tput setaf 2) || echo ""`

.POSIX:
.SUFFIXES:
help:
	@echo "For a quick start, build and install locally:\n ${BOLD}${GREEN}PREFIX=~/.local make install${ANSI_RESET}\n"
	@echo "Available subcommands:"
	@echo " - ${BOLD}meli${ANSI_RESET} (builds meli with optimizations in \$$CARGO_TARGET_DIR)"
	@echo " - ${BOLD}install${ANSI_RESET} (installs binary in \$$BINDIR and documentation to \$$MANDIR)"
	@echo " - ${BOLD}uninstall${ANSI_RESET}"
	@echo "Secondary subcommands:"
	@echo " - ${BOLD}clean${ANSI_RESET} (cleans build artifacts)"
	@echo " - ${BOLD}check-deps${ANSI_RESET} (checks dependencies)"
	@echo " - ${BOLD}install-bin${ANSI_RESET} (installs binary to \$$BINDIR)"
	@echo " - ${BOLD}install-doc${ANSI_RESET} (installs manpages to \$$MANDIR)"
	@echo " - ${BOLD}help${ANSI_RESET} (prints this information)"

	@echo " - ${BOLD}dist${ANSI_RESET} (creates release tarball named meli-"${VERSION}".tar.gz in this directory)"
	@echo " - ${BOLD}deb-dist${ANSI_RESET} (builds debian package in the parent directory)"
	@echo " - ${BOLD}distclean${ANSI_RESET} (cleans distribution build artifacts)"
	@echo "\nENVIRONMENT variables of interest:"
	@echo "* PREFIX = ${UNDERLINE}${PREFIX}${ANSI_RESET}"
	@echo -n "* MELI_FEATURES = ${UNDERLINE}"
	@[ -z $${MELI_FEATURES+x} ] && echo -n "unset" || echo -n ${MELI_FEATURES}
	@echo ${ANSI_RESET}
	@echo "* BINDIR = ${UNDERLINE}${BINDIR}${ANSI_RESET}"
	@echo "* MANDIR = ${UNDERLINE}${MANDIR}${ANSI_RESET}"
	@echo -n "* MANPATH = ${UNDERLINE}"
	@[ $${MANPATH+x} ] && echo -n $${MANPATH} || echo -n "unset"
	@echo ${ANSI_RESET}
	@echo "* output of manpath(1) = ${UNDERLINE}${MANPATHS}${ANSI_RESET}"
	@echo -n "* NO_MAN ${UNDERLINE}"
	@[ $${NO_MAN+x} ] && echo -n "set" || echo -n "unset"
	@echo ${ANSI_RESET}
	@echo -n "* NO_COLOR ${UNDERLINE}"
	@[ $${NO_COLOR+x} ] && echo -n "set" || echo -n "unset"
	@echo ${ANSI_RESET}
	@#@echo "* CARGO_COLOR = ${CARGO_COLOR}"

meli: check-deps
	@${CARGO_BIN} build ${CARGO_COLOR}--target-dir="${CARGO_TARGET_DIR}" ${FEATURES} --release


.PHONY: check-deps
check-deps:
	@(if ! echo ${MIN_RUSTC}\\n`${CARGO_BIN} --version | cut -d ' ' -f 2` | sort -CV; then echo "rust version >= ${RED}${MIN_RUSTC}${ANSI_RESET} required, found: `which ${CARGO_BIN}` `${CARGO_BIN} --version | cut -d ' ' -f 2`" \
		"\nYour options:\n - Set CARGO_BIN to a supported version\n - Install a supported version from your distribution's package manager\n - Install a supported version from ${UNDERLINE}https://rustup.rs/${ANSI_RESET}" ; exit 1; fi)


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
	@(if [ -z $${NO_MAN+x} ]; then \
		mkdir -p $(DESTDIR)${MANDIR}/man1 ; \
		mkdir -p $(DESTDIR)${MANDIR}/man5 ; \
		echo " - ${BOLD}Installing manpages to ${ANSI_RESET}${DESTDIR}${MANDIR}:" ; \
		for MANPAGE in ${MANPAGES}; do \
			SECTION=`echo $${MANPAGE} | rev | cut -d "." -f 1`; \
			MANPAGEPATH=${DESTDIR}${MANDIR}/man$${SECTION}/$${MANPAGE}.gz; \
			echo "  * installing $${MANPAGE} → ${GREEN}$${MANPAGEPATH}${ANSI_RESET}"; \
			gzip -n < $${MANPAGE} > $${MANPAGEPATH} \
    ; done ; \
	(case ":${MANPATHS}:" in \
  *:${DESTDIR}${MANDIR}:*) echo -n "";; \
	*) echo "\n${RED}${BOLD}WARNING${ANSI_RESET}: ${UNDERLINE}Path ${DESTDIR}${MANDIR} is not contained in your MANPATH variable or the output of \`manpath\` command.${ANSI_RESET} \`man\` might fail finding the installed manpages. Consider adding it if necessary.\nMANPATH variable / output of \`manpath\`: ${MANPATHS}" ;; \
	esac) ; \
	else echo "NO_MAN is defined, so no documentation is going to be installed." ; fi)

.PHONY: install-bin
install-bin: meli
	@mkdir -p $(DESTDIR)${BINDIR}
	@echo " - ${BOLD}Installing binary to ${ANSI_RESET}${GREEN}${DESTDIR}${BINDIR}/meli${ANSI_RESET}"
	@case ":${PATH}:" in \
  *:${DESTDIR}${BINDIR}:*) echo -n "";; \
	*) echo "\n${RED}${BOLD}WARNING${ANSI_RESET}: ${UNDERLINE}Path ${DESTDIR}${BINDIR} is not contained in your PATH variable.${ANSI_RESET} Consider adding it if necessary.\nPATH variable: ${PATH}";; \
	esac
	@install -D ./${CARGO_TARGET_DIR}/release/meli $(DESTDIR)${BINDIR}/meli


.PHONY: install
.NOTPARALLEL: yes
install: meli install-bin install-doc
	@(if [ -z $${NO_MAN+x} ]; then \
	echo "\n You're ready to go. You might want to read the \"STARTING WITH meli\" section in the manpage (\`man meli\`)" ;\
	fi)
	@echo " - Report bugs in the mailing list or git issue tracker ${UNDERLINE}https://git.meli.delivery${ANSI_RESET}"
	@echo " - If you have a specific feature or workflow you want to use, you can post in the mailing list or git issue tracker."

.PHONY: dist
dist:
	@git archive --format=tar.gz --prefix=meli-${VERSION}/ HEAD >meli-${VERSION}.tar.gz
	@echo meli-${VERSION}.tar.gz

.PHONY: deb-dist
deb-dist:
	@dpkg-buildpackage -b -rfakeroot -us -uc
	@echo ${BOLD}${GREEN}Generated${ANSI_RESET}  ../meli_${VERSION}-1_amd64.deb
