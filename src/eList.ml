(*
 * eList.ml
 * --------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

let rec take n l =
  if n <= 0 then
    []
  else match l with
    | [] -> []
    | x :: l -> x :: take (n - 1) l

let rec drop n l =
  if n <= 0 then
    l
  else match l with
    | [] -> []
    | x :: l -> drop (n - 1) l

let sub pos length l = take length (drop pos l)

let rec concat_rec sep = function
  | [] -> []
  | s :: l -> sep @ (s @ concat_rec sep l)

let concat sep = function
  | [] -> []
  | s :: l -> s @ concat_rec sep l

let filter_map f l =
  List.fold_right (fun x acc -> match f x with
                     | None -> acc
                     | Some(v) -> v :: acc) l []

let rec find_map f = function
  | [] -> None
  | x :: l -> match f x with
      | None -> find_map f l
      | y -> y

let rec assoc x = function
  | [] -> None
  | (k, v) :: _ when k = x -> Some(v)
  | _ :: l -> assoc x l

let rec assq x = function
  | [] -> None
  | (k, v) :: _ when k == x -> Some(v)
  | _ :: l -> assq x l

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

let rec split f l =
  List.fold_right (fun x (a, b) -> match f x with
                     | Left x -> (x :: a, b)
                     | Right x -> (a, x :: b)) l ([], [])
