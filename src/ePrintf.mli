(*
 * ePrintf.mli
 * -----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(** Extended printf facility *)

open Format

(** {6 Printers} *)

type ('a, 'b) printer =  (formatter -> 'b) -> formatter -> 'a
 (** A printer is a funtion which take a continuation, a formater,
     then any arguments and print them on the formatter *)

val econst : EString.t -> ('a, 'a) printer
  (** [econst str] printer which do not take any argument and output
      [str] *)

val nconst : string -> ('a, 'a) printer
  (** [nconst str] same [const] but use a native ocaml string *)

val cons : ('a, 'b) printer -> ('b, 'c) printer -> ('a, 'c) printer
  (** [cons p1 p2] concatenate two printer *)

val nil : ('a, 'a) printer
  (** [nil] printer which do nothing *)

val print__flush : ('a, 'a) printer
  (** [print__flush] printer which print nothing and flush the
      formatter *)

(** {6 Printing functions} *)

val printf : ('a, unit) printer -> 'a
  (** [printf fmt] print on stdout *)

val println : ('a, unit) printer -> 'a
  (** [println fmt] print on stdout then print a newline and flush
      stdout *)

val eprintf : ('a, unit) printer -> 'a
  (** [eprintf fmt] print on stderr *)

val eprintln : ('a, unit) printer -> 'a
  (** [eprintln fmt] print on stderr then print a newline and flush
      stderr *)

val fprintf : formatter -> ('a, unit) printer -> 'a
  (** [fprintf pp fmt] print on the output channel oc *)

val sprintf : ('a, EString.t) printer -> 'a
  (** [sprintf fmt] return the result as an estring *)

val nprintf : ('a, string) printer -> 'a
  (** [sprintf fmt] return the result as a native string *)

val bprintf : Buffer.t -> ('a, unit) printer -> 'a
  (** [bprintf buf] print in a buffer *)

val iprintf : 'b -> ('a, 'b) printer -> 'a
  (** [iprintf result fmt] drop parameters and return [result] *)

(** {6 Printing with continuation} *)

val kfprintf : (formatter -> 'b) -> formatter -> ('a, 'b) printer -> 'a
  (** [kfprintf cont pp fmt] print on [oc] using [fmt] then call
      [cont] *)

val ksprintf : (EString.t -> 'b) -> ('a, 'b) printer -> 'a
  (** [ksprintf cont fmt] create a string using [fmt] then pass it to
      [cont].

      Note: the string passed to [cont] is in reverse order *)

val knprintf : (string -> 'b) -> ('a, 'b) printer -> 'a
  (** [knprintf cont fmt] create a native string using [fmt] then pass
      it to [cont] *)
