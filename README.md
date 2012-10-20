estring - extension for string literals
=======================================

Estring is a syntax extension which aims to make the use of custom
string types more convenient.

The idea is to replace this kind of code:

    (My_string.of_string "string literals")

by:

    m"string literals"

Dependencies
------------

* [OCaml](http://caml.inria.fr/ocaml/) (>= 3.11)
* [findlib](http://projects.camlcity.org/projects/findlib.html)

For building the development version, you also need to install
[oasis](http://oasis.forge.ocamlcore.org/) (>= 0.3.0).

Installation
------------

To build and install estring:

    $ ./configure
    $ make
    $ make install

### Documentation _(optional)_

To build the documentation:

    $ make doc

It will then be installed by `make install`.

### Tests _(optionnal)_

To build and execute tests:

    $ ./configure --enable-tests
    $ make test

Usage
-----

Files in the "sample" directory show how to define string convertors.

For the distribution of your project, you can either add a dependency
to the estring package or embed it in your sources. Several instances
of estring can work together so this will not break anything.
