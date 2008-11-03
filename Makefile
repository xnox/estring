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

.PHONY: all clean lib lib-byte lib-native test test-error-loc

all: lib

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

# +-------+
# | Other |
# +-------+

clean:
	$(OC) -clean

# "make" is shorter than "ocamlbuild"...
%:
	$(OC) $*
