(*
 * eUnicode.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

type t = EUChar.t list

let rec length_rec acc = function
  | [] -> acc
  | x :: l -> length_rec (acc + EUChar.length x) l

let length l = length_rec 0 l

let rec parse_utf8 = function
  | [] -> []
  | l ->
      let ch, rest = EUChar.next l in
      ch :: parse_utf8 rest

let of_estring l =
  try
    parse_utf8 l
  with
      _ -> failwith "EUnicode.of_estring"

let rec try_of_estring_rec pos real_pos = function
    | [] -> `Success []
    | l ->
        match EUChar.try_next l with
          | `Success(ch, rest) ->
              begin match try_of_estring_rec (pos + 1) (real_pos + EUChar.length ch) rest with
                | `Success l -> `Success(ch :: l)
                | `Failure _ as f -> f
              end
          | `Failure msg ->
              `Failure (real_pos, pos, msg)

let try_of_estring l = try_of_estring_rec 0 0 l

let rec to_estring = function
  | [] -> []
  | ch :: l -> EUChar.estring_prepend ch (to_estring l)

let estring_prepend l acc = List.fold_right EUChar.estring_prepend l acc
let to_estring l = estring_prepend l []

let estring_prepend_escaped l acc = List.fold_right EUChar.estring_prepend_escaped l acc
let estring_escaped l = estring_prepend_escaped l []

let prepend_escaped l acc = List.fold_right EUChar.prepend_escaped l acc
let escaped l = prepend_escaped l []
