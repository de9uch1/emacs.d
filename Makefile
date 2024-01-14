#!/usr/bin/env make -f
# -*- mode: makefile-gmake -*-

.DELETE_ON_ERROR:
.DEFAULT: all
.PHONY:	all build

all: build
build: init.elc early-init.elc share/*.elc
init.elc: init.el
	emacs --batch -f batch-byte-compile $<
early-init.elc: early-init.el
	emacs --batch -f batch-byte-compile $<
share/%.elc: share/%.el
	emacs --batch -f batch-byte-compile $<
