(*
 * eChar.mli
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(** Extended char functions *)

type t = char

val prepend_escaped : t -> t list -> t list
  (** [prepend_escaped ch acc] prepend an escaped version of [ch] to
      [acc] *)

val escaped : t -> t list
  (** [escaped ch] same as [prepend_escaped ch []] *)
