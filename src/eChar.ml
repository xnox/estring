(*
 * eChar.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

type t = char

external is_printable : char -> bool = "caml_is_printable"

let prepend_escaped ch acc = match ch with
  | '\'' -> '\\' :: '\'' :: acc
  | '\\' -> '\\' :: '\\' :: acc
  | '\n' -> '\\' :: 'n' :: acc
  | '\t' -> '\\' :: 't' :: acc
  | c ->
      if is_printable c then
        c :: acc
      else
        let n = Char.code c in
        '\\'
        :: Char.unsafe_chr (48 + n / 100)
        :: Char.unsafe_chr (48 + (n / 10) mod 10)
        :: Char.unsafe_chr (48 + n mod 10)
        :: acc

let escaped ch = prepend_escaped ch []
