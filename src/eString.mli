(*
 * eString.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(** Immutable strings *)

type t = char list
    (** An immutable string is just a list of char. *)

val of_string : string -> t
  (** Convert a native caml string to a char list *)

val to_string : t -> string
  (** Convert a char list to a string *)

val prepend_escaped : t -> t -> t
  (** [append_escaped str acc] prepend an escaped version of [str] to
      [acc] *)

val escaped : t -> t
  (** [escaped str] same as [prepend_escaped str []] *)

val prepend_escaped_of_char : char -> t -> t
  (** [prepend_escaped_of_char ch acc] prepend an escaped version of
      [ch] to acc *)

val escaped_of_char : char -> t
  (** [escaped_of_char ch] same as [prepend_escaped_of_char ch []] *)

(** {6 Parsing/printing} *)

val of_bool : bool -> t
val to_bool : t -> bool

val of_int : int -> t
val to_int : t -> int

val of_int32 : int32 -> t
val to_int32 : t -> int32

val of_int64 : int64 -> t
val to_int64 : t -> int64

val of_nativeint : nativeint -> t
val to_nativeint : t -> nativeint

val of_float : float -> t
val to_float : t -> float
