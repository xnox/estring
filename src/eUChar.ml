(*
 * eUChar.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

type t = int

external to_int : t -> int = "%identity"
external code : t -> int = "%identity"

let is_ascii x = (x land (lnot 0x7f)) = 0
let is_latin1 x = (x land (lnot 0xff)) = 0

let to_char x =
  if is_latin1 x then
    Char.unsafe_chr x
  else
    failwith "EUChar.to_char"

let of_char x = Char.code x land 0xff

let of_int x =
  if x < 0 || x > 0x10ffff then
    invalid_arg "EUChar.of_int"
  else
    x

let chr = of_int

let utf8_length x =
  (* 7-bits code points *)
  if x < 0x80 then
    1

  (* 11-bits code points *)
  else if x <= 0x800 then
    2

  (* 16-bits code points *)
  else if x <= 0x10000 then
    3

  (* 21-bits code points *)
  else
    4

(* [utf8_trail minimum acc count l]

   fetch [count] trailing bytes from [l] (of the form 0b10xxxxxx) and
   add them to [acc].

   [minimum] is the minimum value for the resulting code points, this
   is to reject overlong UTF8-encoded code points.

   For example:

   0b11000001 0b10abcdef

   can also be represented by:

   0b01abcdef

   So it is overlong.
*)
let rec utf8_trail minimum acc count l = match count, l with
  | 0, l ->
      if acc < minimum then
        failwith "EUChar.utf8_next"
      else
        (acc, l)
  | _, ch :: l ->
      let n = Char.code ch in
      if n land 0xc0 = 0x80 then
        utf8_trail minimum ((acc lsl 6) lor (n land 0x3f)) (count - 1) l
      else
        failwith "EUChar.utf8_next"
  | _, [] ->
      failwith "EUChar.utf8_next"

let utf8_next = function
  | [] ->
      invalid_arg "EUChar.utf8_next"
  | ch :: l ->
      let n = Char.code ch in
      if n land 0x80 = 0 then
        (n, l)
      else if n land 0xe0 = 0xc0 then
        utf8_trail 0x80 n 1 l
      else if n land 0xf0 = 0xe0 then
        utf8_trail 0x800 n 2 l
      else if n land 0xf8 = 0xf0 then
        utf8_trail 0x10000 (n land 0x7f) 3 l
      else
        failwith "EUChar.utf8_next"

let rec utf8_try_trail minimum total acc count l = match count, l with
  | 0, l ->
      if acc < minimum then
        `Failure(Printf.sprintf "overlong %d-bytes character" total)
      else
        `Success(acc, l)
  | _, ch :: l ->
      let n = Char.code ch in
      if n land 0xc0 = 0x80 then
        utf8_try_trail minimum total ((acc lsl 6) lor (n land 0x3f)) (count - 1) l
      else
        `Failure(Printf.sprintf "invalid trailing code(%d/%d): 0x%02x" (total - count + 1) total n)
  | _, [] ->
      `Failure(Printf.sprintf "missing trailing code(%d/%d)" (total - count + 1) total)

let utf8_try_next = function
  | [] ->
      `Failure "empty string"
  | ch :: l ->
      let n = Char.code ch in
      if n land 0x80 = 0 then
        `Success(n, l)
      else if n land 0xe0 = 0xc0 then
        utf8_try_trail 0x80 1 n 1 l
      else if n land 0xf0 = 0xe0 then
        utf8_try_trail 0x800 1 n 2 l
      else if n land 0xf8 = 0xf0 then
        utf8_try_trail 0x10000 1 (n land 0x7f) 3 l
      else
        `Failure(Printf.sprintf "invalid code: 0x%02x" n)

let mkch = Char.unsafe_chr

let utf8_prepend x l =
  if x < 0x80 then
    mkch x :: l
  else if x <= 0x800 then
    mkch ((x lsr 6) lor 0xc0) :: mkch ((x land 0x3f) lor 0x80) :: l
  else if x <= 0x10000 then
    mkch ((x lsr 12) lor 0xe0) :: mkch (((x lsr 6) land 0x3f) lor 0x80) :: mkch ((x land 0x3f) lor 0x80) :: l
  else
    mkch ((x lsr 18) lor 0xf0) :: mkch (((x lsr 12) land 0x3f) lor 0xe0) :: mkch (((x lsr 6) land 0x3f) lor 0x80) :: mkch ((x land 0x3f) lor 0x80) :: l

let to_utf8 ch = utf8_prepend ch []

external is_printable : char -> bool = "caml_is_printable"

let rec prepend_escaped_rec rest = function
  | [] -> rest
  | '\'' :: l -> Char.code '\\' :: Char.code '\'' :: prepend_escaped_rec rest l
  | '\\' :: l -> Char.code '\\' :: Char.code '\\' :: prepend_escaped_rec rest l
  | '\n' :: l -> Char.code '\\' :: Char.code 'n' :: prepend_escaped_rec rest l
  | '\t' :: l -> Char.code '\\' :: Char.code 't' :: prepend_escaped_rec rest l
  | c :: l ->
      let n = Char.code c in
      if is_ascii n && is_printable c then
        n :: prepend_escaped_rec rest l
      else
        Char.code '\\'
        :: 48 + n / 100
        :: 48 + (n / 10) mod 10
        :: 48 + n mod 10
        :: prepend_escaped_rec rest l

let prepend_escaped ch acc = prepend_escaped_rec acc (to_utf8 ch)

let escaped ch = prepend_escaped ch []
