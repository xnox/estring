(* -*- coding: utf-8 -*-
 *
 * test_unicode.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

open EPrintf

let _ =
  print_unicode u"This is a unicode string!\n";
  print_unicode u"This is a unicode string with non-ascii characters: ééàà!\n";
  println p"format accepting a unicode string: %us" u"èù”đð«€¶ŧŋð";
  println p"same thing, but escaped: %uS" u"èù”đð«€¶ŧŋð";
  let str = u"æ«¢ßðđŧ←←↓j¶ðđß" in
  println p"str = {unicode}" str;
  println p"number of unicode chars in str: %d" (List.length str);
  println p"number of chars in str: %d" (List.length (estring_of_unicode str))

IFDEF HAVE_PRIVATE THEN

let f = function
  | u"plop" -> 1
  | u"ééàà" -> 2
  | _ -> 3

let _ =
  println p"test matching 1: %d" (f u"plop");
  println p"test matching 2: %d" (f u"ééàà");
  println p"test matching 3: %d" (f u"foo")

END
