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
  (** A unicode character, all code points from U+0000 to U+10FFFF are
      allowed *)

(** Note: unicode character can be written be written [U"X"] with
    [pa_estring]. *)

(** {6 Char manipulation} *)

val is_ascii : t -> bool
  (** Tell weather a unicode character is an ascii character or not *)

val is_latin1 : t -> bool
  (** Tell weather a unicode character is a ISO8859-1 Latin-1
      character or not *)

val of_char : char -> t
  (** [of_char ch] convert the ISO8859-1 Latin-1 character [ch] to a
      unicode character *)

val to_char : t -> char
  (** Convert a unicode character to an ISO8859-1 Latin-1 character.

      @raise [Failure "EUChar.to_char"] if [ch] is not an ISO8859-1
      Latin-1 character. *)

val of_int : int -> t
  (** Create a unicode character from its integer code.

      @raise [Invalid_argument "of_int"] if the given code is not
      valid. *)

external to_int : t -> int = "%identity"
  (** Return the code point of a unicode character. *)

val chr : int -> t
  (** Same as {!of_int} *)

external code : t -> int = "%identity"
  (** Same as {!to_int} *)

(** {6 UTF8} *)

val utf8_length : t -> int
  (** [length ch] @return the number of bytes taken by hte UTF8
      representation of [ch]. It is one of 1, 2, 3, 4 *)

val utf8_next : EString.t -> t * EString.t
  (** [utf8_next str] [str] must be a valid UTF8 string. It extract
      the first UTF8-encoded unicode character of [str], then return
      it with the rest of [str].

      @raise [Failure "EUChar.utf8_next"] if [str] is not a valid UTF8
      string.

      @raise [Invalid_argument "EUChar.utf8_next"] if [str] is
      empty. *)

val utf8_try_next : EString.t -> [ `Success of (t * EString.t) | `Failure of string ]
  (** [utf8_try_next str] same as {!next} but instead of raising an
      exception, return a failure with an error message *)

val utf8_prepend : t -> EString.t -> EString.t
  (** [utf8_prepend ch str] prepend the UTF8 representation of [ch] to
      [str] *)

val to_utf8 : t -> EString.t
  (** [to_string ch] same as [utf8_prepend ch []] *)

(** {6 Escaping} *)

val prepend_escaped : t -> t list -> t list
  (** [prepend_escaped ch acc] prepend an escaped version of [ch] to
      [acc] *)

val escaped : t -> t list
  (** [escaped ch] same as [prepend_escaped ch []] *)
