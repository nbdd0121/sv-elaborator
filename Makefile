all: build

ifeq (, $(shell which cargo))
$(error "cargo is not installed. please visit https://rustup.rs")
endif

.PHONY: all build

build:
	cargo build
