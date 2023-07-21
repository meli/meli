.POSIX:
.SUFFIXES:
CARGO_BIN ?= cargo

.PHONY: clean
clean: clean-client clean-server clean-support

.PHONY: clean-client
clean-client:
	@cd tokio-client && ${CARGO_BIN} clean

.PHONY: clean-server
clean-server:
	@cd tokio-server && ${CARGO_BIN} clean

.PHONY: clean-support
clean-support:
	@cd tokio-support && ${CARGO_BIN} clean
