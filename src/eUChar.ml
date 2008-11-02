(*
 * eUChar.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(* Unicode characters which have a length smaller or equal to 3 are
   internally represented by their code, and four-byte unicode
   characters are taken modulo 2^31 *)

type t = int

let is_ascii x = (x land 0x7fffff80) = 0

let to_char x =
  if is_ascii x then
    Char.unsafe_chr x
  else
    failwith "EUChar.to_char"

let of_char x =
  let n = Char.code x in
  if is_ascii n then
    n
  else
    invalid_arg "EUChar.to_char"

let to_int x = x

let to_int32 x =
  if x land 0x40000000 <> 0 then
    Int32.logor (Int32.of_int x) 0x80000000l
  else
    Int32.of_int x

let of_int32 x =
  if Int32.logand x 0xffffff80l = 0x00000000l then
    Int32.to_int x
  else if Int32.logand x 0xffffe0c0l = 0x0000c080l then
    Int32.to_int x
  else if Int32.logand x 0xfff0c0c0l = 0x00e08080l then
    Int32.to_int x
  else if Int32.logand x 0xf8c0c0c0l = 0xf0808080l then
    (Int32.to_int x) land 0x7fffffff
  else
    invalid_arg "EUChar.of_int32"

let chr = of_int32
let code = to_int32

let rec trail acc count l = match count, l with
  | 0, l ->
      (acc, l)
  | _, ch :: l ->
      let n = Char.code ch in
      if n land 0xc0 = 0x80 then
        trail ((acc lsl 8) lor n) (count - 1) l
      else
        failwith "EUChar.next"
  | _, [] ->
      failwith "EUChar.next"

let next = function
  | [] ->
      invalid_arg "EUChar.next"
  | ch :: l ->
      let n = Char.code ch in
      if n land 0x80 = 0 then
        (n, l)
      else if n land 0xe0 = 0xc then
        trail n 1 l
      else if n land 0xf0 = 0xe0 then
        trail n 2 l
      else if n land 0xf8 = 0xf0 then
        trail (n land 0x7f) 3 l
      else
        failwith "EUChar.next"

let mkch = Char.unsafe_chr

let estring_prepend x l =
  if x land 0x7fffff80 = 0x00000000 then
    mkch x :: l
  else if x land 0x7fffe0c0 = 0x0000c080 then
    mkch (x lsr 8) :: mkch (x land 0xff) :: l
  else if x land 0x7ff0c0c0 = 0x00e08080 then
    mkch (x lsr 16) :: mkch ((x lsr 8) land 0xff) :: mkch (x land 0xff) :: l
  else if x land 0x78c0c0c0 = 0x70808080 then
    mkch ((x lsr 24) lor 0x80) :: mkch ((x lsr 16) land 0xff) :: mkch ((x lsr 8) land 0xff) :: mkch (x land 0xff) :: l
  else
    (* This would never happen if we do not use Obj.magic *)
    assert false

let to_estring ch = estring_prepend ch []

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

let prepend_escaped ch acc = prepend_escaped_rec acc (to_estring ch)

let escaped ch = prepend_escaped ch []

let estring_prepend_escaped ch acc = EString.prepend_escaped (to_estring ch) acc
let estring_escaped ch = EString.escaped (to_estring ch)
