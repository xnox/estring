(*
 * eString_pervasives.ml
 * ---------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

open EPrintf
open EString

open Format

type estring = char list
type ('a, 'b) printer = ('a, 'b) EPrintf.printer
type uchar = EUChar.t
type unicode = uchar list

let string_of_estring = to_string
let estring_of_string = of_string
let bool_of_estring = to_bool
let estring_of_bool = of_bool
let int_of_estring = to_int
let estring_of_int = of_int
let int32_of_estring = to_int32
let estring_of_int32 = of_int32
let int64_of_estring = to_int64
let estring_of_int64 = of_int64
let float_of_estring = to_float
let estring_of_float = of_float
let nativeint_of_estring = to_nativeint
let estring_of_nativeint = of_nativeint

let print_estring l = List.iter print_char l
let prerr_estring l = List.iter print_char l
let output_estring oc l = List.iter (output_char oc) l

let print_unicode l = List.iter (fun ch -> print_estring (EUChar.to_utf8 ch)) l
let prerr_unicode l = List.iter (fun ch -> prerr_estring (EUChar.to_utf8 ch)) l
let output_unicode oc l = List.iter (fun ch -> output_estring oc (EUChar.to_utf8 ch)) l

let uchar_of_char = EUChar.of_char
let char_of_uchar = EUChar.to_char
let uchar_of_int = EUChar.of_int
external int_of_uchar : uchar -> int = "%identity"

let unicode_of_estring = EUnicode.of_utf8
let estring_of_unicode = EUnicode.to_utf8

let unicode = unicode_of_estring

let pp_print_estring pp l =
  (* pp_print_char create a string of length 1, so it seems
     unefficient to use it here *)
  pp_print_string pp (string_of_estring l)

let pp_print_unicode pp l =
  pp_print_estring pp (EUnicode.to_utf8 l)

let print__c cont pp ch =
  pp_print_char pp ch;
  cont pp

let print__C cont pp ch =
  pp_print_string pp "'";
  pp_print_estring pp (EChar.escaped ch);
  pp_print_string pp "'";
  cont pp

let print__es cont pp l =
  pp_print_estring pp l;
  cont pp

let print__eS cont pp l =
  pp_print_string pp "\"";
  pp_print_estring pp (EString.escaped l);
  pp_print_string pp "\"";
  cont pp

let print__ns cont pp str =
  pp_print_string pp str;
  cont pp

let print__nS cont pp str =
  pp_print_string pp "'";
  pp_print_string pp (String.escaped str);
  pp_print_string pp "'";
  cont pp

let print__s = print__ns
let print__S = print__nS

let print__uc cont pp ch =
  pp_print_string pp (string_of_estring (EUChar.to_utf8 ch));
  cont pp

let print__uC cont pp ch =
  pp_print_string pp "'";
  pp_print_estring pp (EUnicode.to_utf8 (EUChar.escaped ch));
  pp_print_string pp "'";
  cont pp

let print__us cont pp str =
  pp_print_string pp (string_of_estring (EUnicode.to_utf8 str));
  cont pp

let print__uS cont pp str =
  pp_print_string pp "\"";
  pp_print_estring pp (EUnicode.to_utf8 (EUnicode.escaped str));
  pp_print_string pp "\"";
  cont pp

module type Operations = sig
  type t
  val name : string
  val shift_right_logical : t -> int -> t
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

  let rec to_HEX_rec acc n =
    if n = zero then
      acc
    else
      let x = to_int n land 15 in
      if x < 10 then
        to_HEX_rec (Char.unsafe_chr (x + Char.code '0') :: acc) (shift_right_logical n 4)
      else
        to_HEX_rec (Char.unsafe_chr (x - 10 + Char.code 'A') :: acc) (shift_right_logical n 4)

  let rec to_hex_rec acc n =
    if n = zero then
      acc
    else
      let x = to_int n land 15 in
      if x < 10 then
        to_hex_rec (Char.unsafe_chr (x + Char.code '0') :: acc) (shift_right_logical n 4)
      else
        to_hex_rec (Char.unsafe_chr (x - 10 + Char.code 'a') :: acc) (shift_right_logical n 4)

  let rec to_dec_rec acc n =
    if n = zero then
      acc
    else
      let x = to_int n mod 10 in
      to_dec_rec (Char.unsafe_chr (x + Char.code '0') :: acc) (div n ten)

  let rec to_oct_rec acc n =
    if n = zero then
      acc
    else
      let x = to_int n land 7 in
      to_oct_rec (Char.unsafe_chr (x + Char.code '0') :: acc) (shift_right_logical n 3)

  let make_to f n =
    if n = zero then
      ['0']
    else f [] n

  let to_HEX = make_to to_HEX_rec
  let to_hex = make_to to_hex_rec
  let to_dec = make_to to_dec_rec
  let to_oct = make_to to_oct_rec

  let pHEX cont pp x = pp_print_estring pp (make_to to_HEX_rec x); cont pp
  let phex cont pp x = pp_print_estring pp (make_to to_hex_rec x); cont pp
  let pdec cont pp x = pp_print_estring pp (make_to to_dec_rec x); cont pp
  let poct cont pp x = pp_print_estring pp (make_to to_oct_rec x); cont pp
end

module Int_functions =
  Make_int_functions(struct
                       type t = int
                       let name = "int"
                       let shift_left = ( lsl )
                       let shift_right_logical = ( lsr )
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

let print__d cont pp x = pp_print_int pp x; cont pp
let print__i = print__d
let print__X = Int_functions.pHEX
let print__x = Int_functions.phex
let print__u = Int_functions.pdec
let print__o = Int_functions.poct
let print__ld cont pp x = pp_print_string pp (Int32.to_string x); cont pp
let print__li = print__ld
let print__lX = Int32_functions.pHEX
let print__lx = Int32_functions.phex
let print__lu = Int32_functions.pdec
let print__lo = Int32_functions.poct
let print__Ld cont pp x = pp_print_string pp (Int64.to_string x); cont pp
let print__Li = print__Ld
let print__LX = Int64_functions.pHEX
let print__Lx = Int64_functions.phex
let print__Lu = Int64_functions.pdec
let print__Lo = Int64_functions.poct
let print__nd cont pp x = pp_print_string pp (Nativeint.to_string x); cont pp
let print__ni = print__nd
let print__nX = Nativeint_functions.pHEX
let print__nx = Nativeint_functions.phex
let print__nu = Nativeint_functions.pdec
let print__no = Nativeint_functions.poct

let print__n = print__d
let print__N = print__d
let print__l = print__d
let print__L = print__d

let print__B cont pp x = pp_print_estring pp (estring_of_bool x); cont pp

let print__a cont pp f x = f pp x; cont pp

let print__any cont pp x =
  pp_print_string pp "<abstract>";
  cont pp

let print__int = print__d

let print__int32 cont pp x =
  pp_print_string pp (Int32.to_string x);
  pp_print_string pp "l";
  cont pp

let print__int64 cont pp x =
  pp_print_string pp (Int64.to_string x);
  pp_print_string pp "L";
  cont pp

let print__nativeint cont pp x =
  pp_print_string pp (Nativeint.to_string x);
  pp_print_string pp "n";
  cont pp

let print__char = print__C

let print__uchar cont pp uch =
  pp_print_string pp "U\"";
  pp_print_unicode pp (EUChar.escaped uch);
  pp_print_string pp "\"";
  cont pp

let print__bool = print__B

let print__string cont pp str =
  pp_print_string pp "n\"";
  pp_print_string pp str;
  pp_print_string pp "\"";
  cont pp

let print__unicode cont pp ustr =
  pp_print_string pp "n\"";
  pp_print_unicode pp ustr;
  pp_print_string pp "\"";
  cont pp

let print__estring cont pp estr =
  pp_print_string pp "\"";
  pp_print_estring pp estr;
  pp_print_string pp "\"";
  cont pp

let rec plist cont elt_printer pp = function
  | [] ->
      pp_close_box pp ();
      pp_print_string pp "]";
      cont pp
  | x :: l ->
      pp_print_string pp ";";
      pp_print_space pp ();
      elt_printer (fun pp -> plist cont elt_printer pp l) pp x

let print__list elt_printer cont pp = function
  | [] ->
      pp_print_string pp "[]";
      cont pp
  | x :: l ->
      pp_print_string pp "[";
      pp_open_box pp 0;
      elt_printer (fun pp -> plist cont elt_printer pp l) pp x

let print__array elt_printer cont pp arr =
  match Array.length arr with
    | 0 ->
        pp_print_string pp "[||]";
        cont pp
    | len ->
        pp_print_string pp "[|";
        pp_open_box pp 0;
        elt_printer (fun _ -> ()) pp arr.(0);
        for i = 1 to len - 1 do
          pp_print_string pp ";";
          pp_print_space pp ();
          elt_printer (fun _ -> ()) pp arr.(i)
        done;
        pp_close_box pp ();
        pp_print_string pp "|]";
        cont pp

let print__option a_printer cont pp = function
  | None ->
      pp_print_string pp "None";
      cont pp
  | Some x ->
      pp_print_string pp "Some(";
      a_printer (fun pp -> pp_print_string pp ")"; cont pp) pp x
