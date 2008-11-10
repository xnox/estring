(*
 * test_format.ml
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

open EPrintf

let rec genlist acc = function
  | 0 -> acc
  | n -> genlist (n :: acc) (n - 1)

let _ =
  println p"list: {list int}" (genlist [] 256)
