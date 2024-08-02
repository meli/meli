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
.POSIX:
.SUFFIXES:
CARGO_TARGET_DIR ?= target
CARGO_BIN ?= cargo
TAGREF_BIN ?= tagref
CARGO_ARGS ?=
RUSTFLAGS ?= -D warnings -W unreachable-pub -W rust-2021-compatibility
CARGO_SORT_BIN = cargo-sort
CARGO_HACK_BIN = cargo-hack
PRINTF = /usr/bin/printf

# Options
PREFIX ?= /usr/local
EXPANDED_PREFIX := `cd ${PREFIX} && pwd -P`
BINDIR ?= ${EXPANDED_PREFIX}/bin
MANDIR ?= ${EXPANDED_PREFIX}/share/man

# Installation parameters
DOCS_SUBDIR ?= meli/docs/
MANPAGES ?= meli.1 meli.conf.5 meli-themes.5 meli.7
FEATURES != [ -z "$${MELI_FEATURES}" ] && ($(PRINTF) -- '--all-features') || ($(PRINTF) -- '--features %s' "$${MELI_FEATURES}")

MANPATHS != ACCUM="";for m in `manpath 2> /dev/null | tr ':' ' '`; do if [ -d "$${m}" ]; then REAL_PATH=`cd $${m} && pwd` ACCUM="$${ACCUM}:$${REAL_PATH}";fi;done;echo $${ACCUM}'\c' | sed 's/^://'
VERSION = `grep -m1 version meli/Cargo.toml | head -n1 | cut -d'"' -f 2 | head -n1`
MIN_RUSTC = `grep -m1 rust-version meli/Cargo.toml | head -n1 | cut -d'"' -f 2 | head -n1`
GIT_COMMIT = `git show-ref -s --abbrev HEAD`
DATE = `date -I`

# Output parameters
BOLD ?= `[ -z $${TERM} ] && echo "" || tput bold`
UNDERLINE ?= `[ -z $${TERM} ] && echo "" || tput smul`
ANSI_RESET ?= `[ -z $${TERM} ] && echo "" || tput sgr0`
CARGO_COLOR ?= `[ -z $${NO_COLOR+x} ] && echo "" || echo "--color=never "`
RED ?= `[ -z $${NO_COLOR+x} ] && ([ -z $${TERM} ] && echo "" || tput setaf 1) || echo ""`
GREEN ?= `[ -z $${NO_COLOR+x} ] && ([ -z $${TERM} ] && echo "" || tput setaf 2) || echo ""`
YELLOW ?= `[ -z $${NO_COLOR+x} ] && ([ -z $${TERM} ] && echo "" || tput setaf 3) || echo ""`

.PHONY: meli
meli: check-deps
	${CARGO_BIN} build ${CARGO_ARGS} ${CARGO_COLOR}--target-dir="${CARGO_TARGET_DIR}" ${FEATURES} --release --bin meli

.PHONY: help
help:
	@echo "For a quick start, build and install locally:\n\n${BOLD}${GREEN}make PREFIX=~/.local install${ANSI_RESET}\n"
	@echo "Available subcommands:"
	@echo " - ${BOLD}meli${ANSI_RESET} (builds meli with optimizations in \$$CARGO_TARGET_DIR)"
	@echo " - ${BOLD}install${ANSI_RESET} (installs binary in \$$BINDIR and documentation to \$$MANDIR)"
	@echo " - ${BOLD}uninstall${ANSI_RESET}"
	@echo "\nSecondary subcommands:"
	@echo " - ${BOLD}clean${ANSI_RESET} (cleans build artifacts)"
	@echo " - ${BOLD}check-deps${ANSI_RESET} (checks dependencies)"
	@echo " - ${BOLD}install-bin${ANSI_RESET} (installs binary to \$$BINDIR)"
	@echo " - ${BOLD}install-doc${ANSI_RESET} (installs manpages to \$$MANDIR)"
	@echo " - ${BOLD}help${ANSI_RESET} (prints this information)"

	@echo " - ${BOLD}dist${ANSI_RESET} (creates release tarball named meli-"${VERSION}".tar.gz in this directory)"
	@echo " - ${BOLD}deb-dist${ANSI_RESET} (builds debian package in the parent directory)"
	@echo " - ${BOLD}distclean${ANSI_RESET} (cleans distribution build artifacts)"
	@echo " - ${BOLD}build-rustdoc${ANSI_RESET} (builds rustdoc documentation for all packages in \$$CARGO_TARGET_DIR)"
	@echo "\nENVIRONMENT variables of interest:"
	@$(PRINTF) "* MELI_FEATURES "
	@[ -z $${MELI_FEATURES+x} ] && echo "unset" || echo "= ${UNDERLINE}"$${MELI_FEATURES}${ANSI_RESET}
	@$(PRINTF) "* PREFIX "
	@[ -z ${EXPANDED_PREFIX} ] && echo "unset" || echo "= ${UNDERLINE}"${EXPANDED_PREFIX}${ANSI_RESET}
	@$(PRINTF) "* BINDIR = %s\n" "${UNDERLINE}${BINDIR}${ANSI_RESET}"
	@$(PRINTF) "* MANDIR "
	@[ -z ${MANDIR} ] && echo "unset" || echo "= ${UNDERLINE}"${MANDIR}${ANSI_RESET}
	@$(PRINTF) "* MANPATH = "
	@[ $${MANPATH+x} ] && echo ${UNDERLINE}$${MANPATH}${ANSI_RESET} || echo "unset"
	@echo "* (cleaned) output of manpath(1) = ${UNDERLINE}${MANPATHS}${ANSI_RESET}"
	@$(PRINTF) "* NO_MAN "
	@[ $${NO_MAN+x} ] && echo "set" || echo "unset"
	@$(PRINTF) "* NO_COLOR "
	@[ $${NO_COLOR+x} ] && echo "set" || echo "unset"
	@echo "* CARGO_BIN = ${UNDERLINE}${CARGO_BIN}${ANSI_RESET}"
	@$(PRINTF) "* CARGO_ARGS "
	@[ -z $${CARGO_ARGS+x} ] && echo "unset" || echo "= ${UNDERLINE}"$${CARGO_ARGS}${ANSI_RESET}
	@$(PRINTF) "* AUTHOR (for deb-dist) "
	@[ -z $${AUTHOR+x} ] && echo "unset" || echo "= ${UNDERLINE}"$${AUTHOR}${ANSI_RESET}
	@echo "* MIN_RUSTC = ${UNDERLINE}${MIN_RUSTC}${ANSI_RESET}"
	@echo "* VERSION = ${UNDERLINE}${VERSION}${ANSI_RESET}"
	@echo "* GIT_COMMIT = ${UNDERLINE}${GIT_COMMIT}${ANSI_RESET}"
	@#@echo "* CARGO_COLOR = ${CARGO_COLOR}"

.PHONY: check
check: check-tagrefs
	RUSTFLAGS='${RUSTFLAGS}' ${CARGO_BIN} check ${CARGO_ARGS} ${CARGO_COLOR}--target-dir="${CARGO_TARGET_DIR}" ${FEATURES} --all --tests --examples --benches --bins

.PHONY: fmt
fmt:
	$(CARGO_BIN) +nightly fmt --all || $(CARGO_BIN) fmt --all
	@OUT=$$($(CARGO_SORT_BIN) melib -w 2>&1 && $(CARGO_SORT_BIN) meli -w 2>&1) || $(PRINTF) "WARN: %s cargo-sort failed or binary not found in PATH.\n" "$$OUT"

.PHONY: lint
lint:
	RUSTFLAGS='${RUSTFLAGS}' $(CARGO_BIN) clippy --no-deps ${FEATURES} --all --tests --examples --benches --bins

.PHONY: test
test: test-docs
	RUSTFLAGS='${RUSTFLAGS}' ${CARGO_BIN} test ${CARGO_ARGS} ${CARGO_COLOR}--target-dir="${CARGO_TARGET_DIR}" ${FEATURES} --all --tests --examples --benches --bins

.PHONY: test-docs
test-docs:
	RUSTFLAGS='${RUSTFLAGS}' ${CARGO_BIN} test ${CARGO_ARGS} ${CARGO_COLOR}--target-dir="${CARGO_TARGET_DIR}" ${FEATURES} --all --doc

.PHONY: test-feature-permutations
test-feature-permutations:
	$(CARGO_HACK_BIN) hack --feature-powerset

.PHONY: check-deps
check-deps:
	@(if ! echo ${MIN_RUSTC}\\n`${CARGO_BIN} --version | grep ^cargo | cut -d ' ' -f 2` | sort -CV; then echo "rust version >= ${RED}${MIN_RUSTC}${ANSI_RESET} required, found: `which ${CARGO_BIN}` `${CARGO_BIN} --version | cut -d ' ' -f 2`" \
		"\nYour options:\n - Set CARGO_BIN to a supported version\n - Install a supported version from your distribution's package manager\n - Install a supported version from ${UNDERLINE}https://rustup.rs/${ANSI_RESET}" ; exit 1; fi)


.PHONY: clean
clean:
	-rm -rf ./${CARGO_TARGET_DIR}/

.PHONY: distclean
distclean:
	rm -f meli-${VERSION}.tar.gz
	rm -rf .pc # rm debian stuff

.PHONY: uninstall
uninstall:
	rm -f $(DESTDIR)${BINDIR}/meli
	for MANPAGE in ${MANPAGES}; do \
			SECTION=`echo $${MANPAGE} | rev | cut -d "." -f 1`; \
			MANPAGEPATH="${DESTDIR}${MANDIR}/man$${SECTION}/$${MANPAGE}.gz"; \
			rm -f "$${MANAGEPATH}"
	; done

.PHONY: install-doc
install-doc:
	@(if [ -z $${NO_MAN+x} ]; then \
		echo " - ${BOLD}Installing manpages to ${ANSI_RESET}${DESTDIR}${MANDIR}:" ; \
		for MANPAGE in ${MANPAGES}; do \
			SECTION=`echo $${MANPAGE} | rev | cut -d "." -f 1`; \
			mkdir -p $(DESTDIR)${MANDIR}/man$${SECTION} ; \
			MANPAGEPATH=${DESTDIR}${MANDIR}/man$${SECTION}/$${MANPAGE}.gz; \
			echo "  * installing $${MANPAGE} → ${GREEN}$${MANPAGEPATH}${ANSI_RESET}"; \
			gzip -n < ${DOCS_SUBDIR}$${MANPAGE} > $${MANPAGEPATH} \
		; done ; \
	(case ":${MANPATHS}:" in \
	*:${DESTDIR}${MANDIR}:*) echo "\c";; \
	*) echo "\n${RED}${BOLD}WARNING${ANSI_RESET}: ${UNDERLINE}Path ${DESTDIR}${MANDIR} is not contained in your MANPATH variable or the output of \`manpath\` command.${ANSI_RESET} \`man\` might fail finding the installed manpages. Consider adding it if necessary.\nMANPATH variable / output of \`manpath\`: ${MANPATHS}" ;; \
	esac) ; \
	else echo "NO_MAN is defined, so no documentation is going to be installed." ; fi)

.PHONY: install-bin
install-bin: meli
	mkdir -p $(DESTDIR)${BINDIR}
	@echo " - ${BOLD}Installing binary to ${ANSI_RESET}${GREEN}${DESTDIR}${BINDIR}/meli${ANSI_RESET}"
	@case ":${PATH}:" in \
	*:${DESTDIR}${BINDIR}:*) echo "\n";; \
	*) echo "\n${RED}${BOLD}WARNING${ANSI_RESET}: ${UNDERLINE}Path ${DESTDIR}${BINDIR} is not contained in your PATH variable.${ANSI_RESET} Consider adding it if necessary.\nPATH variable: ${PATH}";; \
	esac
	mkdir -p $(DESTDIR)${BINDIR}
	rm -f  $(DESTDIR)${BINDIR}/meli
	cp ./${CARGO_TARGET_DIR}/release/meli $(DESTDIR)${BINDIR}/meli
	chmod 755 $(DESTDIR)${BINDIR}/meli


.PHONY: install
.NOTPARALLEL: yes
install: meli install-bin install-doc
	@(if [ -z $${NO_MAN+x} ]; then \
	$(PRINTF) "\n You're ready to go. You might want to read the \"STARTING WITH meli\" section in the manpage (\`man meli\`)" ;\
	$(PRINTF) "\n or the tutorial in meli(7) (\`man 7 meli\`).\n" ;\
	fi)
	@$(PRINTF) " - Report bugs in the mailing list or git issue tracker ${UNDERLINE}https://git.meli-email.org${ANSI_RESET}\n"
	@$(PRINTF) " - If you have a specific feature or workflow you want to use, you can post in the mailing list or git issue tracker.\n"

.PHONY: dist
dist:
	git archive --format=tar.gz --prefix=meli-${VERSION}/ HEAD >meli-${VERSION}.tar.gz
	@echo meli-${VERSION}.tar.gz

AUTHOR ?= grep -m1 authors meli/Cargo.toml | head -n1 | cut -d'"' -f 2 | head -n1
.PHONY: deb-dist
deb-dist:
	@$(PRINTF) "Override AUTHOR environment variable to set package metadata.\n"
	dpkg-buildpackage -b -rfakeroot -us -uc --build-by="${AUTHOR}" --release-by="${AUTHOR}"
	@echo ${BOLD}${GREEN}Generated${ANSI_RESET}  ../meli_${VERSION}-1_`dpkg --print-architecture`.deb

.PHONY: build-rustdoc
build-rustdoc:
	RUSTDOCFLAGS="--crate-version ${VERSION}_${GIT_COMMIT}_${DATE}" ${CARGO_BIN} doc ${CARGO_ARGS} ${CARGO_COLOR}--target-dir="${CARGO_TARGET_DIR}" --all-features --no-deps --workspace --document-private-items --open

.PHONY: check-tagrefs
check-tagrefs:
	@(if ! command -v "$(TAGREF_BIN)" > /dev/null;\
		then \
				$(PRINTF) "Warning: tagref binary not in PATH.\n" 1>&2;\
				exit;\
		else \
				$(TAGREF_BIN);\
		fi)

.PHONY: test-makefile
test-makefile:
	@$(PRINTF) "Checking that current version is detected. "
	@([ ! -z "${VERSION}" ] && $(PRINTF) "${GREEN}OK${ANSI_RESET}\n") || $(PRINTF) "${RED}ERROR${ANSI_RESET}\nVERSION env var is empty, check its definition.\n" 1>&2
	@$(PRINTF) "Checking that 'date -I' works on this platform. "
	@export DATEVAL=$$(printf "%s" ${DATE} | wc -c | tr -d "[:blank:]" 2>&1); ([ "$${DATEVAL}" = "10" ] && $(PRINTF) "${GREEN}OK${ANSI_RESET}\n") || $(PRINTF) "${RED}ERROR${ANSI_RESET}\n'date -I' does not produce a YYYY-MM-DD output on this platform.\n" 1>&2
	@$(PRINTF) "Checking that the git commit SHA can be detected. "
	@([ ! -z "$(GIT_COMMIT)" ] && $(PRINTF) "${GREEN}OK${ANSI_RESET}\n") || $(PRINTF) "${YELLOW}WARN${ANSI_RESET}\nGIT_COMMIT env var is empty.\n" 1>&2

# Checking if mdoc changes produce new lint warnings from mandoc(1) compared to HEAD version:
#
# example invocation: `mandoc_lint meli.1`
#
# with diff(1)
# ============
#function mandoc_lint () {
#diff <(mandoc -T lint <(git show HEAD:./meli/docs/$1) 2> /dev/null | cut -d':' -f 3-) <(mandoc -T lint ./meli/docs/$1 2> /dev/null | cut -d':' -f 3-)
#}
#
# with sdiff(1) (side by side)
# ============================
#
#function mandoc_lint () {
#sdiff <(mandoc -T lint <(git show HEAD:./meli/docs/$1) 2> /dev/null | cut -d':' -f 3-) <(mandoc -T lint ./meli/docs/$1 2> /dev/null | cut -d':' -f 3-)
#}
#
# with delta(1)
# =============
#
#function mandoc_lint () {
#delta --side-by-side <(mandoc -T lint <(git show HEAD:./meli/docs/$1) 2> /dev/null | cut -d':' -f 3-) <(mandoc -T lint ./meli/docs/$1 2> /dev/null | cut -d':' -f 3-)
#}
