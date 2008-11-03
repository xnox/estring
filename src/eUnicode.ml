(*
 * eUnicode.ml
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

type t = EUChar.t list

let rec utf8_length_rec acc = function
  | [] -> acc
  | x :: l -> utf8_length_rec (acc + EUChar.utf8_length x) l

let utf8_length l = utf8_length_rec 0 l

let rec parse_utf8 = function
  | [] -> []
  | l ->
      let ch, rest = EUChar.utf8_next l in
      ch :: parse_utf8 rest

let of_utf8 l =
  try
    parse_utf8 l
  with
      _ -> failwith "EUnicode.of_utf8"

let rec try_of_utf8_rec pos real_pos = function
    | [] -> `Success []
    | l ->
        match EUChar.utf8_try_next l with
          | `Success(ch, rest) ->
              begin match try_of_utf8_rec (pos + 1) (real_pos + EUChar.utf8_length ch) rest with
                | `Success l -> `Success(ch :: l)
                | `Failure _ as f -> f
              end
          | `Failure msg ->
              `Failure (real_pos, pos, msg)

let try_of_utf8 l = try_of_utf8_rec 0 0 l

let rec to_utf8 = function
  | [] -> []
  | ch :: l -> EUChar.utf8_prepend ch (to_utf8 l)

let utf8_prepend l acc = List.fold_right EUChar.utf8_prepend l acc
let to_estring l = utf8_prepend l []

let prepend_escaped l acc = List.fold_right EUChar.prepend_escaped l acc
let escaped l = prepend_escaped l []
