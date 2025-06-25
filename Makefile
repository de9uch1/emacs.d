#!/usr/bin/env make -f
# -*- mode: makefile-gmake -*-

ELC_TARGETS := $(foreach f,$(wildcard share/*.el),$(basename $(f)).elc)

.DELETE_ON_ERROR:
.DEFAULT: all
.PHONY:	all build

all: build
build: init.elc early-init.elc custom.elc $(ELC_TARGETS) tmp/package-quickstart.elc
	cd share/lspce && env RUSTFLAGS=-Awarnings cargo build -r -q && ln -snf target/release/liblspce_module.so lspce-module.so 

init.elc: init.el
	emacs --batch -f batch-byte-compile $<
	emacs --batch -f batch-native-compile $<
early-init.elc: early-init.el
	emacs --batch -f batch-byte-compile $<
	emacs --batch -f batch-native-compile $<
custom.elc: custom.el
	emacs --batch -f batch-byte-compile $<
	emacs --batch -f batch-native-compile $<
share/%.elc: share/%.el
	emacs --batch -f batch-byte-compile $<
	emacs --batch -f batch-native-compile $<
tmp/package-quickstart.elc: tmp/package-quickstart.el
	emacs --batch -f batch-byte-compile $<
	emacs --batch -f batch-native-compile $<
