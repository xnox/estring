# Makefile
# --------
# Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of estring.

OC = ocamlbuild
OF = ocamlfind
PREFIX = /usr/local

# Targets
LIB = estring
TEST = test test_unicode test_default
TEST_ERROR_LOC_COUNT = 6

MODULES = eChar eList ePrintf eString eString_pervasives eUChar eUnicode

.PHONY: all clean lib lib-byte lib-native test test-error-loc install just-install uninstall

all:
	$(OC) $(LIB:=.cma) $(LIB:=.cmxa) \
	  $(TEST:%=test/%.d.byte) \
	  estring.docdir/index.html \
	  manual.cmo

# +------------------+
# | Specific targets |
# +------------------+

lib-byte:
	$(OC) $(LIB:=.cma)

lib-native:
	$(OC) $(LIB:=.cmxa)

lib:
	$(OC) $(LIB:=.cma) $(LIB:=.cmxa)

test:
	$(OC) $(TEST:%=test/%.d.byte)

test-error-loc:
	for i in `seq 1 $(TEST_ERROR_LOC_COUNT)`; do \
	  $(OC) test/test_error_loc/test$$i.cmo || true; \
	done

# +---------------+
# | Documentation |
# +---------------+

doc:
	$(OC) estring.docdir/index.html

dot:
	$(OC) estring.docdir/index.dot

# +--------------------+
# | Installation stuff |
# +--------------------+

install: all just-install

just-install:
	$(OF) install estring META manual.ml \
	 _build/syntax/pa_estring.cmo \
	 $(LIB:%=_build/%.cma) \
	 $(LIB:%=_build/%.cmxa) \
	 $(LIB:%=_build/%.a) \
	 $(MODULES:%=src/%.mli) \
	 $(MODULES:%=_build/src/%.cmi)
	mkdir -p $(PREFIX)/share/doc/estring/html
	install -vm 0644 LICENSE $(PREFIX)/share/doc/estring
	install -vm 0644 _build/estring.docdir/* $(PREFIX)/share/doc/estring/html

uninstall:
	$(OF) remove estring
	rm -rvf $(PREFIX)/share/doc/estring

# +-------+
# | Other |
# +-------+

clean:
	$(OC) -clean

# "make" is shorter than "ocamlbuild"...
%:
	$(OC) $*
