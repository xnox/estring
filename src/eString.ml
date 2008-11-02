(*
 * eString.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

type t = char list

let of_string str =
  let rec aux acc = function
    | -1 -> acc
    | i -> aux (String.unsafe_get str i :: acc) (i - 1)
  in
  aux [] (String.length str - 1)

let to_string l =
  let str = String.create (List.length l) in
  let rec aux i = function
    | [] -> str
    | x :: l ->
        String.unsafe_set str i x;
        aux (i + 1) l
  in
  aux 0 l

let to_bool = function
  | [ 't'; 'r'; 'u'; 'e' ] -> true
  | [ 'f'; 'a'; 'l'; 's'; 'e' ] -> false
  | _ -> invalid_arg "bool_of_estring"

let of_bool = function
  | true -> [ 't'; 'r'; 'u'; 'e' ]
  | false -> [ 'f'; 'a'; 'l'; 's'; 'e' ]

module type Operations = sig
  type t
  val name : string
  val shift_left : t -> int -> t
  val logor : t -> t -> t
  val add : t -> t -> t
  val neg : t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val of_int : int -> t
  val to_int : t -> int
  val zero : t
  val ten : t
end

module Make_int_functions(Op : Operations) =
struct
  open Op

  let fail _ = failwith (name ^ "_of_estring")

  let rec parse_hex acc = function
    | [] -> acc
    | '0'..'9' as ch :: l -> parse_hex (logor (shift_left acc 4) (of_int (Char.code ch - Char.code '0'))) l
    | 'a'..'f' as ch :: l -> parse_hex (logor (shift_left acc 4) (of_int (Char.code ch - Char.code 'a' + 10))) l
    | 'A'..'F' as ch :: l -> parse_hex (logor (shift_left acc 4) (of_int (Char.code ch - Char.code 'A' + 10))) l
    | _ -> fail ()

  let rec parse_dec acc = function
    | [] -> acc
    | '0'..'9' as ch :: l -> parse_dec (add (mul acc ten) (of_int (Char.code ch - Char.code '0'))) l
    | _ -> fail ()

  let rec parse_oct acc = function
    | [] -> acc
    | '0'..'7' as ch :: l -> parse_oct (logor (shift_left acc 3) (of_int (Char.code ch - Char.code '0'))) l
    | _ -> fail ()

  let rec parse_bin acc = function
    | [] -> acc
    | '0'..'1' as ch :: l -> parse_bin (logor (shift_left acc 1) (of_int (Char.code ch - Char.code '0'))) l
    | _ -> fail ()

  let if_not_null f = function
    | [] -> fail ()
    | l -> f zero l

  let parse_int_pos = function
    | '0' :: 'x' :: l
    | '0' :: 'X' :: l -> if_not_null parse_hex l
    | '0' :: 'o' :: l
    | '0' :: 'O' :: l -> if_not_null parse_oct l
    | '0' :: 'b' :: l
    | '0' :: 'B' :: l -> if_not_null parse_bin l
    | l -> if_not_null parse_dec l

  let parse_int = function
    | '-' :: l -> neg (parse_int_pos l)
    | l -> parse_int_pos l

  let rec print_int_rec acc n =
    if n = zero then
      acc
    else
      print_int_rec (Char.unsafe_chr (to_int n mod 10 + Char.code '0') :: acc) (div n ten)

  let print_int n =
    if n = zero then
      [ '0' ]
    else
      if n < zero then
        '-' :: print_int_rec [] (neg n)
      else
        print_int_rec [] n
end

module Int_functions =
  Make_int_functions(struct
                       type t = int
                       let name = "int"
                       let shift_left = ( lsl )
                       let logor = ( lor )
                       let add = ( + )
                       let zero = 0
                       let ten = 10
                       let neg x = -x
                       let mul = ( * )
                       let div = ( / )
                       let of_int x = x
                       let to_int x = x
                     end)
module Int32_functions =
  Make_int_functions(struct
                       include Int32
                       let name = "int32"
                       let ten = 10l
                     end)
module Int64_functions =
  Make_int_functions(struct
                       include Int64
                       let name = "int64"
                       let ten = 10L
                     end)
module Nativeint_functions =
  Make_int_functions(struct
                       include Nativeint
                       let name = "nativeint"
                       let ten = 10n
                     end)

let to_int = Int_functions.parse_int
let of_int = Int_functions.print_int
let to_int32 = Int32_functions.parse_int
let of_int32 = Int32_functions.print_int
let to_int64 = Int64_functions.parse_int
let of_int64 = Int64_functions.print_int
let to_nativeint = Nativeint_functions.parse_int
let of_nativeint = Nativeint_functions.print_int
let to_float x = float_of_string (to_string x)
let of_float x = of_string (string_of_float x)

external is_printable : char -> bool = "caml_is_printable"

let prepend_escaped_of_char ch acc = match ch with
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

let escaped_of_char ch = prepend_escaped_of_char ch []

let rec prepend_escaped str acc = match str with
  | [] -> acc
  | c :: l -> prepend_escaped_of_char c (prepend_escaped l acc)

let escaped str = prepend_escaped str []
