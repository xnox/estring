(*
 * pa_estring.ml
 * -------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

open Printf
open Camlp4.PreCast
open EString_pervasives

(* +-------+
   | Utils |
   +-------+ *)

(* List with locations *)
type 'a llist =
  | Nil of Loc.t
  | Cons of Loc.t * 'a * 'a llist

let loc_of_llist = function
  | Nil loc -> loc
  | Cons(loc, x, l) -> loc

let rec foldr f g = function
  | Nil loc -> g loc
  | Cons(loc, x, l) -> f loc x (foldr f g l)

let rec list_of_llist = function
  | Nil _ -> []
  | Cons(_, x, l) -> x :: list_of_llist l

let rec ldrop n l =
  if n <= 0 then
    l
  else match l with
    | Cons(_, _, l) -> ldrop (n - 1) l
    | l -> l

(* [estring_expr l] @return the expression representing the list of
   chars [l] *)
let estring_expr l = foldr (fun _loc ch acc -> <:expr< $chr:Char.escaped ch$ :: $acc$ >>) (fun _loc -> <:expr< [] >>) l

(* [estring_patt l] @return the pattern representing the list of
   chars [l] *)
let estring_patt l = foldr (fun _loc ch acc -> <:patt< $chr:Char.escaped ch$ :: $acc$ >>) (fun _loc -> <:patt< [] >>) l

(* [unicode_expr l] @return the expression representing the list
   of unicode characters [l] *)
let unicode_expr l =
  let e = foldr begin fun _loc ch acc ->
    <:expr< $int:sprintf "0x%x" (EUChar.to_int ch)$ :: $acc$ >>
  end (fun _loc -> <:expr< [] >>) l in
  (* Here we know that the characters are correct so we can use
     Obj.magic safely *)
  let _loc = loc_of_llist l in
  <:expr< (Obj.magic $e$ : EUChar.t list) >>

(* [uchar_expr _loc uch] @return the expression representing [uch] *)
let uchar_expr _loc uch = <:expr< (Obj.magic $int:sprintf "0x%x" (EUChar.to_int uch)$ : EUChar.t) >>

IFDEF HAVE_PRIVATE THEN

(* [unicode_patt l] @return the pattern representing the list of
   unicode characters [l] *)
let unicode_patt l = foldr begin fun _loc ch acc ->
  <:patt< $int:sprintf "0x%x" (EUChar.to_int ch)$ :: $acc$ >>
end (fun _loc -> <:patt< [] >>) l

(* [uchar_expr _loc uch] @return the pattern representing [uch] *)
let uchar_expr _loc uch = <:expr< $int:sprintf "0x%x" (EUChar.to_int uch)$ >>

ELSE

(* If we do not have private types, then [UChar.t] is an abstract type
   and matching is not possible :( *)
let fail loc = Loc.raise loc (Stream.Error "you need ocaml >= 3.11 to use unicode in patterns")
let unicode_patt l = fail (loc_of_llist l)
let uchar_patt loc uch = fail loc

END

(* [parse_utf8_rec ll l] parse an estring as an UTF8-encoded unicode
   string. [l] is the estring and [ll] is the corresponding llist of
   chars *)
let rec parse_utf8_rec ll = function
  | [] -> Nil(loc_of_llist ll)
  | l -> match EUChar.utf8_try_next l with
      | `Success(uch, l) ->
          Cons(loc_of_llist ll, uch, parse_utf8_rec (ldrop (EUChar.utf8_length uch) ll) l)
      | `Failure msg ->
          Loc.raise (loc_of_llist ll)
            (Stream.Error
               (sprintf "failed to decode unicode string: %s" msg))

let parse_utf8 ll = parse_utf8_rec ll (list_of_llist ll)

(* [parse_uchar ll] parse a string containing exactly one UTF8-encoded
   unicode character *)
let parse_utf8_char ll =
  let l = list_of_llist ll in
  match EUChar.utf8_try_next l with
    | `Success(uch, []) -> uch
    | `Success(uch, _) ->
        Loc.raise (loc_of_llist (ldrop (EUChar.utf8_length uch) ll))
          (Stream.Error "data remaining after unicode character")
    | `Failure msg ->
        Loc.raise (loc_of_llist ll)
          (Stream.Error
             (sprintf "failed to decode unicode character: %s" msg))

(* +--------------------+
   | Strings unescaping |
   +--------------------+ *)

(* String appears in the camlp4 ast as they apears in the source
   code. So if we want to process a string then we need to first
   unescape it. Camlp4 provide such a function
   (Camlp4.Struct.Token.Eval.string) but the problem is that we do not
   know exactly the location of unescaped characters:

   For instance: "\tx\tA" will be unescaped in " x A", and the
   position of "A" in the resulting string will be changed.

   So here is an implementation of an unescaping function which also
   compute the location of each unescaped characters. *)

module Unescape =
struct
  let add n loc = Loc.move `start n loc
  let inc loc = add 1 loc
  let addl n loc = Loc.move_line n loc
  let incl loc = addl 1 loc
  let resetl loc = addl 0 loc

  let dec x = Char.code x - Char.code '0'
  let hex = function
    | '0'..'9' as x -> Char.code x - Char.code '0'
    | 'a'..'f' as x -> Char.code x - Char.code 'a' + 10
    | 'A'..'F' as x -> Char.code x - Char.code 'A' + 10
    | x -> assert false

  let rec skip_indent cont loc = function
    | (' ' | '\t') :: l -> skip_indent cont (inc loc) l
    | l -> cont loc l

  let skip_opt_linefeed cont loc = function
    | '\n' :: l -> cont (incl loc) l
    | l -> cont loc l

  let rec string loc = function
    | [] -> Nil loc
    | '\\' :: l ->
        let loc = inc loc in
        begin match l with
          | '\n' :: l -> skip_indent string (incl loc) l
          | '\r' :: l -> skip_opt_linefeed (skip_indent string) (resetl loc) l
          | 'n' :: l -> Cons(loc, '\n', string (inc loc) l)
          | 'r' :: l -> Cons(loc, '\r', string (inc loc) l)
          | 't' :: l -> Cons(loc, '\t', string (inc loc) l)
          | 'b' :: l -> Cons(loc, '\b', string (inc loc) l)
          | '\\' :: l -> Cons(loc, '\\', string (inc loc) l)
          | '"' :: l  -> Cons(loc, '"', string (inc loc) l)
          | '\'' :: l -> Cons(loc, '\'', string (inc loc) l)
          | ' ' :: l -> Cons(loc, ' ', string (inc loc) l)
          | ('0'..'9' as c1) :: ('0'..'9' as c2) :: ('0'..'9' as c3) :: l ->
              Cons(loc,
                   char_of_int (100 * (dec c1) + 10 * (dec c2) + (dec c3)),
                   string (add 3 loc) l)
          | 'x'
            :: ('0'..'9' | 'a'..'f' | 'A'..'F' as c1)
            :: ('0'..'9' | 'a'..'f' | 'A'..'F' as c2) :: l ->
              Cons(loc,
                   char_of_int (16 * (hex c1) + (hex c2)),
                   string (add 3 loc) l)
          | _ -> Loc.raise loc (Stream.Error "illegal backslash")
        end
    | '\r' :: l -> Cons(loc, '\r', string (resetl loc) l)
    | '\n' :: l -> Cons(loc, '\n', string (incl loc) l)
    | ch :: l -> Cons(loc, ch, string (inc loc) l)
end

let unescape = Unescape.string

(* +--------------------+
   | String annotations |
   +--------------------+ *)

(* In order to recognize expressions/patterns of the form [u"string"],
   [p"string"], ... without recognising [u "string"], [X.u"string"] we
   need to add a token filter.

   Since the expansions is done at filtering time, we also need to
   annotate strings with their specifier. For that we replace a string
   "plop" with the specifier 's' by: "splop"
*)

let make_annotated_stream stm =
  (* The previous token *)
  let previous = ref EOI

  (* The default specifier, it is first e for ``estring'' *)
  and default_specifier = ref "e" in

  let rec func pos =
    try
      let prev = !previous
      and tok, loc = Stream.next stm in

      previous := tok;

      match tok with
          (* Parse directive [#estring_default] *)
        | KEYWORD "#" ->
            begin match Stream.npeek 3 stm with
              | [(LIDENT "estring_default", _); (BLANKS _, _); (LIDENT specifier, loc')] ->
                  begin match specifier with
                    | "e" | "n" | "u" ->
                        Stream.junk stm;
                        Stream.junk stm;
                        Stream.junk stm;
                        default_specifier := specifier;
                        func pos
                    | _ ->
                        Loc.raise loc'
                          (Stream.Error
                             (sprintf "invalid default estring specifier %S, must be one of e, n or u" specifier))
                  end
              | _ ->
                  Some(tok, loc)
            end

        | LIDENT("e" | "n" | "u" | "p" | "s" as id) when prev <> KEYWORD "." ->
            begin match Stream.peek stm with
              | Some(STRING(s, orig), loc) ->
                  Stream.junk stm;
                  Some(STRING(id ^ s, id ^ orig), loc)
              | _ -> Some(tok, loc)
            end

        | UIDENT("U" as id) when prev <> KEYWORD "." ->
            begin match Stream.peek stm with
              | Some(STRING(s, orig), loc) ->
                  Stream.junk stm;
                  Some(STRING(id ^ s, id ^ orig), loc)
              | _ -> Some(tok, loc)
            end

        | STRING(s, orig) ->
            let spec = !default_specifier in
            Some(STRING(spec ^ s, spec ^ orig), loc)

        | _ ->
            Some(tok, loc)
    with
        Stream.Failure -> None

  in
  Stream.from func

(* +--------------------+
   | Identifier mapping |
   +--------------------+ *)

(* In long specification of format string, i.e. things between '{' and
   '}' we prepend a string to any identifier, like "print__" or "scan__".

   This avoid name colisition between combinators *)

let prefix_ident prefix =
  let rec aux = function
    | <:ident@_loc< $id:x$.$y$ >> -> <:ident< $id:x$.$aux y$ >>
    | <:ident@_loc< $lid:x$ >> -> <:ident< $lid:prefix ^ "__" ^ x$ >>
    | x -> x
  in
object
  inherit Ast.map
  method ident = aux
end

let print_mapper = prefix_ident "print"
let scan_mapper = prefix_ident "scan"

(* +-----------------------+
   | Format string parsing |
   +-----------------------+ *)

exception Premature_end of Loc.t
exception Premature_long_spec_end of Loc.t
exception Invalid_conversion_char of Loc.t * char

(* [get_long_spec deep l] try to extract the long specification ending
   in [l], [deep] being the numbre of '{' not yet closed.

   It return the long specification and the rest of [l] *)
let rec get_long_spec deep = function
  | Nil loc ->
      raise (Premature_long_spec_end loc)
  | Cons(_, '%', Nil loc) ->
      raise (Premature_end loc)
  | Cons(_, '%', Cons(_, x, rest)) ->
      let spec, rest = get_long_spec deep rest in
      ('%' :: x :: spec, rest)
  | Cons(_, '}', rest) ->
      if deep = 0 then
        ([], rest)
      else
        let spec, rest = get_long_spec (deep - 1) rest in
        ('}' :: spec, rest)
  | Cons(_, '{', rest) ->
      let spec, rest = get_long_spec (deep + 1) rest in
      ('{' :: spec, rest)
  | Cons(_, ch, rest) ->
      let spec, rest = get_long_spec deep rest in
      (ch :: spec, rest)

(* [get_const_and_spec mapper l] read the constant part of [l],
   i.e. the part without conversion specification, then read one
   conversion specification if available and return the constant part,
   the specification and the rest.

   [mapper] is used to map long specification expression. It is either
   [print_mapper] or [scan_mapper] *)
let rec get_const_and_spec mapper = function
  | Nil _ ->
      ([], None)

  | Cons(_loc, '%', l) -> begin match l with
      | Cons(_, ('a'..'z' | 'A'..'Z' as ch1), Cons(_, ('a'..'z' | 'A'..'Z' as ch2), l)) ->
          ([], Some(<:expr< $lid:sprintf "print__%c%c" ch1 ch2$ >>, l))

      | Cons(_, ('a'..'z' | 'A'..'Z' as ch), l) ->
          ([], Some(<:expr< $lid:sprintf "print__%c" ch$ >>, l))

      | Cons(_, '!', l) ->
          ([], Some(<:expr< EPrintf.print__flush >>, l))

      | Cons(_, ('{' | '}' | '%' as ch), l) ->
          let const, next = get_const_and_spec mapper l in
          (ch :: const, next)

      | Cons(loc, ch, l) ->
          raise (Invalid_conversion_char(loc, ch))

      | Nil loc ->
          raise (Premature_end loc)
    end

  | Cons(_, '{', l) ->
      let spec, rest = get_long_spec 0 l in
      let e_spec = mapper#expr (Gram.parse Syntax.expr_eoi (loc_of_llist l) (Stream.of_list spec)) in
      ([], Some(e_spec, rest))

  | Cons(_, ch, l) ->
      let const, next = get_const_and_spec mapper l in
      (ch :: const, next)

(* [nconst_expr _loc l] expression for a constant string printer *)
let nconst_expr _loc l = <:expr< EPrintf.nconst $str:String.escaped (string_of_estring l)$ >>

(* [make_format mapper l] create a format expression from a format
   string *)
let rec make_format mapper l =
  let _loc = loc_of_llist l in
  match get_const_and_spec mapper l with
    | [], None ->
        <:expr< EPrintf.nil >>
    | [], Some(espec, Nil _) ->
        espec
    | [], Some(espec, rest) ->
        <:expr< EPrintf.cons $espec$ $make_format mapper rest$ >>
    | l, None ->
        nconst_expr _loc l
    | l, Some(espec, Nil _) ->
        <:expr< EPrintf.cons $nconst_expr _loc l$ $espec$ >>
    | l, Some(espec, rest) ->
        <:expr< EPrintf.cons $nconst_expr _loc l$
                 (EPrintf.cons $espec$ $make_format mapper rest$) >>

(* Handle format parsing error *)
let safe_make_format mapper l =
  try
    make_format mapper l
  with
    | Premature_end loc ->
        Loc.raise loc (Stream.Error "premature end of format")
    | Premature_long_spec_end loc ->
        Loc.raise loc (Stream.Error "'}' missing in format")
    | Invalid_conversion_char(loc, ch) ->
        Loc.raise loc (Stream.Error (Printf.sprintf "invalid conversion specification character: %C" ch))

(* +--------------------+
   | Strings conversion |
   +--------------------+ *)

(* Handle missing or unknown specifiers, this is only for debugging
   purpose since it should never happen *)
let handle_specifier_error = function
  | Cons(loc, ch, _) -> Loc.raise loc (Stream.Error (sprintf "unknown specifier %C" ch))
  | Nil loc -> Loc.raise loc (Stream.Error (sprintf "specifier missing"))

(* [expr_of_annotated_string l] create an expression from an
   annotated string *)
let expr_of_annotated_string = function
  | Cons(_loc, 'e', l) -> estring_expr l
  | Cons(_loc, 'u', l) -> unicode_expr (parse_utf8 l)
  | Cons(_loc, 'U', l) -> uchar_expr _loc (parse_utf8_char l)
  | Cons(_loc, 'n', l) -> <:expr< $str:String.escaped (string_of_estring (list_of_llist l))$ >>
  | Cons(_loc, 'p', l) -> safe_make_format print_mapper l
  | Cons(_loc, 's', l) -> safe_make_format scan_mapper l
  | l -> handle_specifier_error l

(* [expr_of_annotated_string l] create an pattern from an
   annotated string *)
let patt_of_annotated_string = function
  | Cons(_loc, 'e', l) -> estring_patt l
  | Cons(_loc, 'u', l) -> unicode_patt (parse_utf8 l)
  | Cons(_loc, 'U', l) -> uchar_patt _loc (parse_utf8_char l)
  | Cons(_loc, 'n', l) -> <:patt< $str:String.escaped (string_of_estring (list_of_llist l))$ >>
  | Cons(_loc, ('p' | 's'), _) -> Loc.raise _loc (Stream.Error "format string are not allowed in pattern")
  | l -> handle_specifier_error l

(* [is_uchar id] tell weather a identifier must be interpreted as an
   unicode character *)
let is_uchar id = String.length id > 3 &&
  id.[0] = 'u' && id.[1] = '\'' && id.[String.length id - 1] = '\''

let parse_string loc s = unescape loc (estring_of_string s)

let map = object
  inherit Ast.map as super

  method expr e = match super#expr e with
    | <:expr@loc< $str:s$ >> -> expr_of_annotated_string (parse_string loc s)
    | e -> e

  method patt p = match super#patt p with
    | <:patt@loc< $str:s$ >> -> patt_of_annotated_string (parse_string loc s)
    | p -> p
end

(* +--------------+
   | Registration |
   +--------------+ *)

let _ =
  (* Register the token filter for annotations *)
  Gram.Token.Filter.define_filter (Gram.get_filter ()) (fun filter stm -> filter (make_annotated_stream stm));

  (* Register the string mapper *)
  AstFilters.register_str_item_filter map#str_item;

  (* Register filters for automatically opening [EString_pervasives]
     in implementation and interface modules *)
  AstFilters.register_str_item_filter
    (fun s ->
       let _loc = Ast.loc_of_str_item s in
       <:str_item< open EString_pervasives;; $s$ >>);
  AstFilters.register_sig_item_filter
    (fun s ->
       let _loc = Ast.loc_of_sig_item s in
       <:sig_item< open EString_pervasives;; $s$ >>)
