(*
 * eUChar.mli
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(** Unicode characters *)

IFDEF HAVE_PRIVATE THEN
type t = private int
ELSE
type t
END
  (** A unicode character, it can be written [u'X'] with [pa_estring]. *)

(** {6 Char manipulation} *)

val is_ascii : t -> bool
  (** Tell weather a unicode character is an ascii character or not *)

val of_char : char -> t
  (** [of_char ch] convert the ascii character [ch] to a unicode
      character.

      @raise [Invalid_argument "EUChar.of_char"] if [ch] is not an
      ascii character. *)

val to_char : t -> char
  (** Convert a unicode characte to an ascii char.

      @raise [Failure "EUChar.to_char"] if [ch] is not an ascii
      character. *)

val of_int32 : int32 -> t
  (** Create a unicode character from its integer code.

      @raise [Invalid_argument "of_int32"] if the given code is not
      valid. *)

val to_int32 : t -> int32
  (** Return the code of a unicode character. *)

val to_int : t -> int
  (** @return the internal representation of a unicode character. *)

val chr : int32 -> t
  (** Same as [of_int32] *)

val code : t -> int32
  (** Same as [to_int32] *)

(** {6 Parsing/printing} *)

val next : EString.t -> t * EString.t
  (** [next str] extract the first unicode char of [str] then return
      it with the rest of [str].

      @raise [Failure "EUChar.next"] if [str] do not begin with a
      valid unicode character.

      @raise [Invalid_argument "EUChar.next"] if [str] is empty. *)

val estring_prepend : t -> EString.t -> EString.t
  (** [estring_prepend ch str] add [ch] to [str] *)

val to_estring : t -> EString.t
  (** [to_string ch] same as [estring_prepend ch []] *)

(** {6 Escaping} *)

val estring_prepend_escaped : t -> EString.t -> EString.t
  (** [estring_prepend_escaped ch acc] escape [ch] and add if to
      [acc] *)

val estring_escaped : t -> EString.t
  (** [estring_escaped ch] same as [estring_prepend_escaped ch []] *)

val prepend_escaped : t -> t list -> t list
  (** [prepend_escaped ch acc] escape [ch] and add it [acc] *)

val escaped : t -> t list
  (** [escaped ch] same as [prepend_escaped ch []] *)
