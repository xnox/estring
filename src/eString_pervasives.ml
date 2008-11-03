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

type estring = char list
type ('a, 'b) printer = ('a, 'b) EPrintf.printer
type uchar = EUChar.t
type unicode = uchar list

let rec fold f acc = function
  | [] -> acc
  | x :: l -> fold f (f x acc) l

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

let print_unicode l = List.iter (fun ch -> print_estring (EUChar.to_estring ch)) l
let prerr_unicode l = List.iter (fun ch -> prerr_estring (EUChar.to_estring ch)) l
let output_unicode oc l = List.iter (fun ch -> output_estring oc (EUChar.to_estring ch)) l

let uchar_of_char = EUChar.of_char
let char_of_uchar = EUChar.to_char
let uchar_of_int32 = EUChar.of_int32
let int32_of_uchar = EUChar.to_int32

let unicode_of_estring = EUnicode.of_estring
let estring_of_unicode = EUnicode.to_estring

let unicode = unicode_of_estring

let print__c = { print = fun cont out acc ch -> cont (out.add ch acc) }
let print__C = { print = fun cont out acc ch -> cont (out.add '\'' (fold out.add (out.add '\'' acc) (EChar.escaped ch))) }
let print__es = { print = fun cont out acc str -> cont (fold out.add acc str) }
let print__eS = { print = fun cont out acc str -> cont (out.add '"' (fold out.add (out.add '"' acc) (escaped str))) }
let print__s = { print = fun cont out acc str ->
                   let acc = ref acc in
                   for i = 0 to String.length str - 1 do
                     acc := out.add (String.unsafe_get str i) !acc
                   done;
                   cont !acc }
let print__S = { print = fun cont out acc str ->
                   let acc = ref (out.add '"' acc) in
                   for i = 0 to String.length str - 1 do
                     acc := fold out.add !acc (EChar.escaped (String.unsafe_get str i))
                   done;
                   cont (out.add '"' !acc) }
let print__ns = print__s
let print__nS = print__S
let print__any = { print = fun cont out acc _ -> print__s.print cont out acc "<abstract>" }

let printuc out ch acc = fold out.add acc (EUChar.to_estring ch)
let printuC out ch acc = fold out.add acc (EUChar.estring_escaped ch)

let print__uc = { print = fun cont out acc ch -> cont (printuc out ch acc) }
let print__uC = { print = fun cont out acc ch -> cont (out.add '\'' ((printuC out ch (out.add '\'' acc)))) }
let print__us = { print = fun cont out acc str -> cont (fold (printuc out) acc str) }
let print__uS = { print = fun cont out acc str -> cont (out.add '"' (fold (printuC out) (out.add '"' acc) str)) }

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

  let pHEX = { print = fun cont out acc x -> cont (fold out.add acc (make_to to_HEX_rec x)) }
  let phex = { print = fun cont out acc x -> cont (fold out.add acc (make_to to_hex_rec x)) }
  let pdec = { print = fun cont out acc x -> cont (fold out.add acc (make_to to_dec_rec x)) }
  let poct = { print = fun cont out acc x -> cont (fold out.add acc (make_to to_oct_rec x)) }
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

let print__d = { print = fun cont out acc x -> cont (fold out.add acc (estring_of_int x)) }
let print__i = print__d
let print__X = Int_functions.pHEX
let print__x = Int_functions.phex
let print__u = Int_functions.pdec
let print__o = Int_functions.poct
let print__ld = { print = fun cont out acc x -> cont (fold out.add acc (estring_of_int32 x)) }
let print__li = print__ld
let print__lX = Int32_functions.pHEX
let print__lx = Int32_functions.phex
let print__lu = Int32_functions.pdec
let print__lo = Int32_functions.poct
let print__Ld = { print = fun cont out acc x -> cont (fold out.add acc (estring_of_int64 x)) }
let print__Li = print__Ld
let print__LX = Int64_functions.pHEX
let print__Lx = Int64_functions.phex
let print__Lu = Int64_functions.pdec
let print__Lo = Int64_functions.poct
let print__nd = { print = fun cont out acc x -> cont (fold out.add acc (estring_of_nativeint x)) }
let print__ni = print__nd
let print__nX = Nativeint_functions.pHEX
let print__nx = Nativeint_functions.phex
let print__nu = Nativeint_functions.pdec
let print__no = Nativeint_functions.poct

let print__n = print__d
let print__N = print__d
let print__l = print__d
let print__L = print__d

let print__B = { print = fun cont out acc x -> cont (fold out.add acc (estring_of_bool x)) }

let print__int = print__d
let print__int32 = { print = fun cont out acc x -> cont (out.add 'l' (fold out.add acc (estring_of_int32 x))) }
let print__int64 = { print = fun cont out acc x -> cont (out.add 'L' (fold out.add acc (estring_of_int64 x))) }
let print__nativeint = { print = fun cont out acc x -> cont (out.add 'L' (fold out.add acc (estring_of_nativeint x))) }
let print__char = print__C
let print__uchar = { print = fun cont out acc x -> print__uC.print cont out (out.add 'u' acc) x }
let print__bool = print__B
let print__string = { print = fun cont out acc x -> print__S.print cont out (out.add 'n' acc) x }
let print__unicode = { print = fun cont out acc x -> print__uS.print cont out (out.add 'u' acc) x }
let print__estring = print__eS

let rec plist x cont out l acc = match l with
  | [] -> cont (out.add ']' acc)
  | e :: l -> x.print (plist x cont out l) out (out.add ' ' (out.add ';' acc)) e

let print__list x = { print = fun cont out acc l -> match l with
                        | [] -> cont (out.add ']' (out.add '[' acc))
                        | e :: l -> x.print (plist x cont out l) out (out.add '[' acc) e }

let rec parray x cont out arr i acc =
  if i = Array.length arr then
    cont (out.add ']' (out.add '|' acc))
  else
    x.print (parray x cont out arr (i + 1)) out (out.add ' ' (out.add ';' acc)) (Array.unsafe_get arr i)

let print__array x = { print = fun cont out acc arr ->
                         match Array.length arr with
                           | 0 -> cont (out.add ']' (out.add '|' (out.add '|' (out.add '[' acc))))
                           | _ -> x.print (parray x cont out arr 1) out
                               (out.add '|' (out.add '[' acc)) (Array.unsafe_get arr 0) }

let print__option x = { print = fun cont out acc -> function
                          | None -> print__s.print cont out acc "None"
                          | Some e -> print__s.print
                              (fun acc ->
                                 x.print (fun acc -> cont (out.add ')' acc)) out acc e)
                                out acc "Some(" }
