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

(** {6 UTF8} *)

val of_utf8 : EString.t -> t
  (** [of_utf8 str] if [str] is a valid UTF8-encoded string, then
      @return the unicode string represented by [str], otherwise
      @raise [Failure "EUnicode.of_utf8"]. *)

val utf8_length : t -> int
  (** [length ustr] @return the number of bytes of the UTF8
      representation of [ustr]. *)

val try_of_utf8 : EString.t -> [ `Success of t | `Failure of int * int * string ]
  (** [try_of_utf8 str] same as [of_utf8] but instead of raising an
      exception, return a failure with:

      - the position in bytes in [str]
      - the number of unicode char successfully read
      - an error message *)

val utf8_prepend : t -> EString.t -> EString.t
  (** [utf8_prepend ustr acc] prepend the UTF8 representation of
      [ustr] to [acc] *)

val to_utf8 : t -> EString.t
  (** [to_utf8 str] same as [utf8_prepend str []] *)

(** {6 Escaping} *)

val prepend_escaped : t -> t -> t
  (** [prepend_escaped ustr acc] prepend the escaped version of [ustr]
      to [acc] *)

val escaped : t -> t
  (** [escaped ustr] same as [prepend_escaped str []] *)

