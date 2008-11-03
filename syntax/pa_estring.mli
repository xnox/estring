(*
 * pa_estring.mli
 * --------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(** The pa_estring syntax extension *)

open EString_pervasives
open Camlp4.PreCast.Ast
open Camlp4.PreCast

(** {6 List with locations} *)

type 'a llist =
  | Nil of Loc.t
  | Cons of Loc.t * 'a * 'a llist

val loc_of_llist : 'a llist -> Loc.t
  (** Return the location of the first element of a llist *)

val llength : 'a llist -> int
  (** @return the length of a llist *)

val lfoldr : (Loc.t -> 'a -> 'acc -> 'acc) -> (Loc.t -> 'acc) -> 'a llist -> 'acc
  (** [lfoldr f g l] fold_right-like function for llist.

      For example:

      {[
        lfoldr f g Cons(loc1, 1, Cons(loc2, 2, Nil loc3))
      ]}

      is the same as:

      {[
        f loc1 1 (f loc2 2 (g loc3))
      ]}
  *)

val list_of_llist : 'a llist -> 'a list
  (** @return the list of elements contained in a llist *)

val ldrop : int -> 'a llist -> 'a llist
  (** [ldrop count ll] @return [ll] without its first [count] element.

      - @return [ll] if [n <= 0]
      - @return [[]] if [n > llength ll] *)

val llist_expr : (Loc.t -> 'a -> expr) -> 'a llist -> expr
  (** [llist_expr f ll] @return the expression representing a list
      with element obtained by applying [f] on element of [ll] *)

val llist_patt : (Loc.t -> 'a -> patt) -> 'a llist -> patt
  (** [llist_patt f ll] same as {!llist_expr} but for patterns *)

(** {6 Strings pattern/expressions} *)

val estring_expr : char llist -> expr
  (** [estring_expr ll] @return the expression corresponding to the
      estring with elements from [ll] *)

val estring_patt : char llist -> patt
  (** [estring_patt ll] @return the pattern corresponging to the
      estring with elements from [ll] *)

val uchar_expr : Loc.t -> uchar -> expr
  (** [uchar_expr loc uch] @return the expression representing the
      unicode character [uch] *)

val uchar_patt : Loc.t -> uchar -> patt
  (** [uchar_patt loc uch] @return the expression representing the
      unicode character [uch].

      This will fail with ocaml < 3.11 *)

val unicode_expr : uchar llist -> expr
  (** [unicode_expr ll] @return the expression corresponding to the
      unicode with elements from [ll] *)

val unicode_patt : uchar llist -> patt
  (** [unicode_patt ll] @return the pattern corresponging to the
      unicode with elements from [ll].

      This will fail with ocaml < 3.11 *)

(** {6 UTF8 parsing} *)

val parse_utf8 : char llist -> uchar llist
  (** [parse_utf8 ll] parse [ll] as an UTF8-encoded string. It will
      fail if [ll] does not represent a valid UTF8-encoded string *)

val parse_utf8_char : char llist -> uchar
  (** [parse_utf8_char ll] same as {!parse_utf8} but [ll] must
      contains exactly one UTF8-encoded unicode character.

      It will fail if it it not the case *)

(** {6 String unescaping} *)

val unescape : Loc.t -> estring -> char llist
  (** [unescape loc l] @return the unescaped version of [ll] where
      each unescaped character position has been computed *)
