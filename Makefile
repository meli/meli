.POSIX:
.SUFFIXES:
meli:
	cargo build $(FEATURES)--release

PREFIX=/usr/local

ifdef MELI_FEATURES
  FEATURES ?= --features="$(MELI_FEATURES)" 
else
  FEATURES ?=
endif

.PHONY: clean
clean: rm -ri ./target/


.PHONY: uninstall
uninstall: rm -f $(DESTDIR)$(PREFIX)/bin/meli
	rm $(DESTDIR)$(PREFIX)/share/man/man1/meli.1.gz
	rm $(DESTDIR)$(PREFIX)/share/man/man5/meli.conf.5.gz

.PHONY: install
install: meli
	mkdir -p $(DESTDIR)$(PREFIX)/bin
	mkdir -p $(DESTDIR)$(PREFIX)/share/man/man1
	mkdir -p $(DESTDIR)$(PREFIX)/share/man/man5
	cp -f target/release/meli $(DESTDIR)$(PREFIX)/bin
	gzip < meli.1 > $(DESTDIR)$(PREFIX)/share/man/man1/meli.1.gz
	gzip < meli.conf.5 > $(DESTDIR)$(PREFIX)/share/man/man5/meli.conf.5.gz
