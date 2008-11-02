(*
 * eList.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(** Extended functions on list *)

(** The following functions are functions which may be usefull for
    estring processing *)

val take : int -> 'a list -> 'a list
  (** [take count l] take the first [count] element of [l] *)

val drop : int -> 'a list -> 'a list
  (** [drop count l] drop the first [count] element of [l] *)

val sub : int -> int -> 'a list -> 'a list
  (** [sub pos length l] same as [take length (drop pos l)] *)

val concat : 'a list -> 'a list list -> 'a list
  (** Same as [String.concat] but for lists *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
  (** [filter_map f l] *)

val find_map : ('a -> 'b option) -> 'a list -> 'b option
  (** [find_map f l] *)

val assoc : 'a -> ('a * 'b) list -> 'b option
  (** [assoc key l] same as [List.assoc] but return an option instead
      of failing *)

val assq : 'a -> ('a * 'b) list -> 'b option
  (** [assoc key l] same as [List.assq] but return an option instead
      of failing *)

type ('a, 'b) either =
  | Left of 'a
  | Right of 'b

val split : ('a -> ('b, 'c) either) -> 'a list -> 'b list * 'c list
  (** [split f l] split [l] according to [f] *)
