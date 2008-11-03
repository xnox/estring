(*
 * eString_pervasives.mli
 * ----------------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(** Pervasives for estring *)

type estring = char list
    (** New default string representation: list of characters *)

(** {6 estring conversion functions} *)

val string_of_estring : estring -> string
val estring_of_string : string -> estring
  (** Convertion between native mutable strings and estrings *)

(** The following functions behave like the corresponding functions of
    [Pervasives] but deal with estring instead. *)

val bool_of_estring : estring -> bool
val estring_of_bool : bool -> estring

val int_of_estring : estring -> int
val estring_of_int : int -> estring

val float_of_estring : estring -> float
val estring_of_float : float -> estring

val int32_of_estring : estring -> int32
val estring_of_int32 : int32 -> estring

val int64_of_estring : estring -> int64
val estring_of_int64 : int64 -> estring

val nativeint_of_estring : estring -> nativeint
val estring_of_nativeint : nativeint -> estring

(** {6 Unicode} *)

type uchar = EUChar.t
    (** A unicode character *)

type unicode = uchar list
    (** A unicode string. Unicode string can be written [u"string"]
        with the syntax extension *)

val unicode_of_estring : estring -> unicode
  (** Same as {!EUnicode.of_estring} *)

val estring_of_unicode : unicode -> estring
  (** Same as {!EUnicode.to_estring} *)

val unicode : estring -> unicode
  (** Same as [unicode_of_estring] *)

val uchar_of_char : char -> uchar
  (** Same as {!EUChar.of_char} *)

val char_of_uchar : uchar -> char
  (** Same as {!EUChar.to_char} *)

val uchar_of_int32 : int32 -> uchar
  (** Same as {!EUChar.of_int32} *)

val int32_of_uchar : uchar -> int32
  (** Same as {!EUChar.to_int32} *)

(** {6 Output functions} *)

val print_estring : estring -> unit
  (** [print_estring str] print [str] on stdout *)

val prerr_estring : estring -> unit
  (** [print_estring str] print [str] on stderr *)

val output_estring : out_channel -> estring -> unit
  (** [output_estring oc str] print [str] on [oc] *)

val print_unicode : unicode -> unit
  (** [print_unicode str] print [str] on stdout *)

val prerr_unicode : unicode -> unit
  (** [print_unicode str] print [str] on stderr *)

val output_unicode : out_channel -> unicode -> unit
  (** [output_unicode oc str] print [str] on [oc] *)

(** {6 Predefined printers} *)

type ('a, 'b) printer = ('a, 'b) EPrintf.printer

(** The following printers are the equivalent of classical conversion
    characters: *)
val print__B : (bool -> 'a, 'a) printer
val print__c : (char -> 'a, 'a) printer
val print__C : (char -> 'a, 'a) printer
val print__s : (string -> 'a, 'a) printer
val print__S : (string -> 'a, 'a) printer
val print__d : (int -> 'a, 'a) printer
val print__i : (int -> 'a, 'a) printer
val print__n : (int -> 'a, 'a) printer
val print__N : (int -> 'a, 'a) printer
val print__l : (int -> 'a, 'a) printer
val print__L : (int -> 'a, 'a) printer
val print__u : (int -> 'a, 'a) printer
val print__x : (int -> 'a, 'a) printer
val print__X : (int -> 'a, 'a) printer
val print__o : (int -> 'a, 'a) printer
val print__ld : (int32 -> 'a, 'a) printer
val print__li : (int32 -> 'a, 'a) printer
val print__lu : (int32 -> 'a, 'a) printer
val print__lx : (int32 -> 'a, 'a) printer
val print__lX : (int32 -> 'a, 'a) printer
val print__lo : (int32 -> 'a, 'a) printer
val print__Ld : (int64 -> 'a, 'a) printer
val print__Li : (int64 -> 'a, 'a) printer
val print__Lu : (int64 -> 'a, 'a) printer
val print__Lx : (int64 -> 'a, 'a) printer
val print__Lx : (int64 -> 'a, 'a) printer
val print__Lo : (int64 -> 'a, 'a) printer
val print__nd : (nativeint -> 'a, 'a) printer
val print__ni : (nativeint -> 'a, 'a) printer
val print__nu : (nativeint -> 'a, 'a) printer
val print__nx : (nativeint -> 'a, 'a) printer
val print__nx : (nativeint -> 'a, 'a) printer
val print__no : (nativeint -> 'a, 'a) printer

val print__ns : (string -> 'a, 'a) printer
  (** Print a native string *)

val print__nS : (string -> 'a, 'a) printer
  (** Print an escaped native string *)

val print__es : (estring -> 'a, 'a) printer
  (** Print an estring *)

val print__eS : (estring -> 'a, 'a) printer
  (** Print an escaped estring *)

val print__us : (unicode -> 'a, 'a) printer
  (** Print an unicode string *)

val print__uS : (unicode -> 'a, 'a) printer
  (** Print an escaped unicode string *)

val print__uc : (uchar -> 'a, 'a) printer
  (** Print an unicode char *)

val print__uC : (uchar -> 'a, 'a) printer
  (** Print an escaped unicode char *)

(** Printer for basic types *)

val print__any : ('a -> 'b, 'b) printer
val print__int : (int -> 'a, 'a) printer
val print__int32 : (int32 -> 'a, 'a) printer
val print__int64 : (int64 -> 'a, 'a) printer
val print__nativeint : (nativeint -> 'a, 'a) printer
val print__char : (char -> 'a, 'a) printer
val print__uchar : (uchar -> 'a, 'a) printer
val print__bool : (bool -> 'a, 'a) printer
val print__string : (string -> 'a, 'a) printer
val print__estring : (estring -> 'a, 'a) printer
val print__unicode : (unicode -> 'a, 'a) printer
val print__list : ('a -> 'b, 'b) printer -> ('a list -> 'b, 'b) printer
val print__array : ('a -> 'b, 'b) printer -> ('a array -> 'b, 'b) printer
val print__option : ('a -> 'b, 'b) printer -> ('a option -> 'b, 'b) printer
