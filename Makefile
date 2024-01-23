#!/usr/bin/env make -f
# -*- mode: makefile-gmake -*-

ELC_TARGETS := $(foreach f,$(wildcard share/*.el),$(basename $(f)).elc)

.DELETE_ON_ERROR:
.DEFAULT: all
.PHONY:	all build

all: build
build: init.elc early-init.elc $(ELC_TARGETS)
init.elc: init.el
	emacs --batch -f batch-byte-compile $<
early-init.elc: early-init.el
	emacs --batch -f batch-byte-compile $<
share/%.elc: share/%.el
	emacs --batch -f batch-byte-compile $<
