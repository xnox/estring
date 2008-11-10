(*
 * manual.ml
 * ---------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(** estring manual *)

(** +--------------+
    | Introduction |
    +--------------+ *)

(** The main goal estring is to allow to use other representation than
    mutable buffers for strings constant in OCaml source code.

    The first motivation for writing estring was to find a convenient
    way to have immutable strings in ocaml which satisfy these constraints:

    (1) we must be able to use pattern match immutable strings
    (2) immutable string constants must be easy to define, i.e.
    something better than [My_module.freeze "my constant string"]

    Actually a common solution is two write something like that:
*)

module M : sig
  type t

  val of_string : string -> t
  val to_string : t -> string

end = struct
  type t = string

  let of_string = String.copy
  let to_string = String.copy
end

(** But it has a lot of disadvantages:

    - it is a bit anoying to write [M.of_string "a string constant"]
    everywhere
    - each time we want to convert to/from a string, this require a
    copy of the string at runtime, to ensure that a value of type
    [M.t] could not be modified
    - it is not possible to pattern match a value of type [M.t]...

    So estring come from the idea that a natural representation for
    immmutable string which satisfy (1) and (2) is simply as a list of
    characters. (2) being easy to satisfy due the existence of camlp4.

    It is now more than that, and estring has the following features:

    - manipulation and definition of unicode string represented as
    list of unicode characters
    - a new printf/scanf facility which extend the ones from the standart
    library in these way:
      * the format string is not parsed at runtime but instead converted
        at parsing time, so Obj.magic is not needed
      * it is possible to define new printer/scanner and include them
        directly in the format string
*)

(** +-------------------+
    | String specifiers |
    +-------------------+ *)

(** Constant strings are filtered and converted at parsing time by
    pa_estring. The way there are converted is choosen from the
    ``specifier'' attached to a string.

    A specifier is one character located just before the string, for
    example in: *)

let str = u"string"

(** ["string"] has the specifier [u]. If a string do not have a
    specifier, the default one is used instead. The default specifier
    is [e], but it is possible to change it using the directive
    [estring_default].

    So for instance, in the following sequence: *)

let a = "a"

#estring_default n

let b = "b"

#estring_default e

let c = "c"

(** [a] and [c] has the specifier [e], while [b] has the specifier
    [n].

    Here is the list of all specifiers, and their effect:

    - [n]: it stands for ``native'', the string will not be modified.

    - [e]: it stands for ``estring'', the string will be converted to
      a list of characters, for example: *)

let str = e"abc"

(**   will become: *)

let str = ['a'; 'b'; 'c']

(**  - [u]: it stands for ``unicode'', the string must be a valid UTF8
       string a will be converted to a list of unicode characters.
       Look at the unicode section for details.

     - [U]: the string must contains only one unicode characters, and
       will be replaced by it.

     - [p]: it stands for ``printer'', the string will be converted to
     a printer expression. Look at the format section for details.

     - [s]: it stands for ``scanner'', the string will be converted to
     a scanner expression. Look at the format section for details.
*)

(** +-------------+
    | The library |
    +-------------+ *)

(** The runtime library is composed of:

    - {!EList}: it is just a set of functions on list which may be
      usefull when processing strings seen as list of characters

    - {!EChar} and {!EString}: modules for specific estring manipulation
      such as estring escaping or convertions

    - {!EUChar} and {!EUnicode}: modules for unicode characters and
      unicode strings (seen as a list of unicode characters)
      manipultation

    - {!EPrintf} and {!EScanf}: new printf and scanf facility

    - {!EString_pervasives}, when using the syntax extensions, it will
      be automatically opened. It provides the printers/scanners for
      standart types + functions similar to the one from [Pervasives]
      but which works on estrings, and unicode strings

    Estring install the following packages with ocamlfind:

    - [estring.lib], which the runtime library
    - [estring.syntax], which is the syntax extension
    - [estring], which is [estring.lib] + [estring.syntax]

    For example, to compile this file, once estring is installed:

    $ ocamlfind ocamlc -package estring -syntax camlp4o -linkpkg manual.ml -o manual
*)

(** +-----------------+
    | Unicode support |
    +-----------------+ *)

(** Unicode strings are represented as list of unicode characters,
    with type {!EUChar.t}, which is basically an int. By the way since
    all integers are not valid unicode characters, this type is
    abstract, or private if using ocaml >= 3.11.

    Strings defined with the specifier [u] must be valid UTF8-encoded
    unicode strings. They are checked at parsing time and will be converted
    to a list integers like that:
*)

let ustr1 = u"Hello, world\n!"

(** will become: *)

let ustr2 =
  (Obj.magic
     [ 0x48; 0x65; 0x6c; 0x6c; 0x6f; 0x2c; 0x20;
       0x77; 0x6f; 0x72; 0x6c; 0x64; 0x0a; 0x21 ] : EUChar.t list)

(** Verification: *)

let _ = match ustr1 = ustr2 with
  | true -> print_estring "it works!\n"
  | false -> print_estring "fail!!\n"

(** Although, if you use ocaml >= 3.11 which support private types for
    any kind of types, {!EUChar.t} is defined as a private int, so it
    is possible to use unicode string in patterns. Otherwise it is
    not.

    It is also possible to define one single unicode character like
    that:
*)

let uchr = U"e"

(** +----------------+
    | Format strings |
    +----------------+ *)

(** Format strings consists on:

    - normal characters

    - short conversion specifications, of the form:

        %x or %xy

      where [x] and [xy] are alphabetical characters

    - long conversion specifications, of the form:

        [{expr}]

      where [expr] is any expression in which lower identifier will
      be prefixed by ``print__'' or ``scan__''.

    Format strings are expended at parsing time to printer/scanner
    expression.

    For example:
*)

let fmt = p"x = %es, y = {option int}"

(** will be replaced by: *)

let fmt cont pp =
  EPrintf.cons (EPrintf.nconst n"x = ")
    (EPrintf.cons print__es
       (EPrintf.cons (EPrintf.nconst n", y = ") (print__option print__int)))
    cont pp

(** +----------+
    | Printers |
    +----------+ *)

(** Printers are defined as follow:

    {[
       type ('a, 'b) printer = {
         print : 'acc. ('acc -> 'b) -> 'acc writer -> 'acc -> 'a;
         (** [print cont add acc ...] *)
       }
    ]}

    and a writer:

    {[
      type 'acc writer = {
        add : char -> 'acc -> 'acc;
        (** [add char acc] add one character to the output *)

        flush : 'acc -> 'acc;
        (** [flush acc] flush output *)
       }
    ]}

    Defining new printers is easy, for example:
*)

open EPrintf

type 'a foo =
  | A of 'a
  | B of estring
  | C

let print__foo print__a cont pp = function
  | A x -> p"A({a})" cont pp x
  | B x -> p"B({estring})" cont pp x
  | C -> p"C" cont pp

(** and now: *)

let _ = println p"truc = {list (foo int)}" [A 1; B e"plop"; C]
