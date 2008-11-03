(*
 * eUnicode.mli
 * ------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(** Unicode string manipulation *)

type t = EUChar.t list
    (** A unicode string is a list of unicode characters *)

(** {6 Unicode string manipulation} *)

val length : t -> int
  (** [length str] @return the number of bytes of the string
      representation of [str] *)

val of_estring : EString.t -> t
  (** [of_estring str] try to parse [str] as a unicode string.

      @raise [Failure "EUnicode.of_estring"] if [str] does not contain
      a valid unicode string *)

val try_of_estring : EString.t -> [ `Success of t | `Failure of int * int * string ]
  (** [try_of_estring str] same as [of_estring] but instead of raising
      an exception, return a failure with:

      - the position in bytes in [str]
      - the number of unicode char successfully read
      - an error message *)

val estring_prepend : t -> EString.t -> EString.t
  (** [estring_prepend str acc] prepend the string representation of
      [str] to [acc] *)

val to_estring : t -> EString.t
  (** [to_estring str] same as [estring_prepend str []] *)

(** {6 Escaping} *)

val estring_prepend_escaped : t -> EString.t -> EString.t
  (** [estring_prepend_escaped str acc] prepend an escaped version of
      [str] to [acc] *)

val estring_escaped : t -> EString.t
  (** [estring_escaped str] same as [estring_prepend_escaped str []] *)

val prepend_escaped : t -> t -> t
  (** [prepend_escaped str acc] prepend the escaped version of [str]
      to [acc] *)

val escaped : t -> t
  (** [escaped str] same as [prepend_escaped str []] *)

