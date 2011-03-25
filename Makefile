# Makefile
# --------
# Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of estring.

SETUP = ocaml setup.ml
NAME = $(shell oasis query Name | tail -n 1)
VERSION = $(shell oasis query Version | tail -n 1)

.PHONY: build
build: setup.data
	$(SETUP) -build

.PHONY: doc
doc: setup.data build
	$(SETUP) -doc

.PHONY: test
test: setup.data build
	$(SETUP) -test

.PHONY: all
all: setup.ml
	$(SETUP) -all

.PHONY: install
install: setup.data
	$(SETUP) -install

.PHONY: uninstall
uninstall: setup.data
	$(SETUP) -uninstall

.PHONY: reinstall
reinstall: setup.data
	$(SETUP) -reinstall

.PHONY: clean
clean: setup.ml
	$(SETUP) -clean

.PHONY: distclean
distclean: setup.ml
	$(SETUP) -distclean

setup.data: setup.ml
	$(SETUP) -configure

setup.ml:
	$(error Please run the "configure" script first)

.PHONY: dist
dist:
	DARCS_REPO=$(shell pwd) darcs dist --dist-name $(NAME)-$(VERSION)
