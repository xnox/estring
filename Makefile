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
TEST = test

.PHONY: all clean lib lib-byte lib-native test

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

# +-------+
# | Other |
# +-------+

clean:
	$(OC) -clean

# "make" is shorter than "ocamlbuild"...
%:
	$(OC) $*
