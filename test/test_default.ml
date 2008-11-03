(*
 * test_default.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

let _ = print_estring "default strings are estrings\n"

#estring_default u

let _ = print_unicode "default strings are unicode strings\n"

#estring_default n

let _ = print_string "default strings are native strings\n"

#estring_default e

let _ = print_estring "default strings are estrings again\n"
