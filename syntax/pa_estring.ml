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

let unescape = Camlp4.Struct.Token.Eval.string ~strict:()

(* [estring_expr loc l] @return the expression representing the list
   of chars [l] *)
let estring_expr _loc l = List.fold_right (fun ch acc -> <:expr< $chr:Char.escaped ch$ :: $acc$ >>) l <:expr< [] >>

(* [estring_patt loc l] @return the pattern representing the list of
   chars [l] *)
let estring_patt _loc l = List.fold_right (fun ch acc -> <:patt< $chr:Char.escaped ch$ :: $acc$ >>) l <:patt< [] >>

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

        | LIDENT id when String.length id = 1 && prev <> KEYWORD "." ->
            begin match Stream.peek stm with
              | Some(STRING(s, orig), loc') ->
                  begin match id with
                    | "e" | "n" | "u" | "p" | "s" ->
                        Stream.junk stm;
                        Some(STRING(id ^ s, id ^ orig), loc')
                    | _ ->
                        Loc.raise loc
                          (Stream.Error
                             (sprintf "invalid estring specifier %S, must be one of e, n, u, p or s" id))
                  end
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

exception Premature_end
exception Premature_long_spec_end
exception Invalid_conversion_char of char

(* [get_long_spec deep l] try to extract the long specification ending
   in [l], [deep] being the numbre of '{' not yet closed.

   It return the long specification and the rest of [l] *)
let rec get_long_spec deep = function
  | [] ->
      raise Premature_long_spec_end
  | ['%'] ->
      raise Premature_end
  | '%' :: ch :: rest ->
      let spec, rest = get_long_spec deep rest in
      ('%' :: ch :: spec, rest)
  | '}' :: rest ->
      if deep = 0 then
        ([], rest)
      else
        let spec, rest = get_long_spec (deep - 1) rest in
        ('}' :: spec, rest)
  | '{' :: rest ->
      let spec, rest = get_long_spec (deep + 1) rest in
      ('{' :: spec, rest)
  | ch :: rest ->
      let spec, rest = get_long_spec deep rest in
      (ch :: spec, rest)

(* [get_const_and_spec mapper loc l] read the constant part of [l],
   i.e. the part without conversion specification, then read one
   conversion specification if available and return the constant part,
   the specification and the rest.

   [mapper] is used to map long specification expression. It is either
   [print_mapper] or [scan_mapper] *)
let rec get_const_and_spec mapper _loc = function
  | [] ->
      ([], None)

  | '%' :: l -> begin match l with
      | ('a'..'z' | 'A'..'Z' as ch1) :: ('a'..'z' | 'A'..'Z' as ch2) :: l ->
          ([], Some(<:expr< $lid:sprintf "print__%c%c" ch1 ch2$ >>, l))

      | ('a'..'z' | 'A'..'Z' as ch) :: l ->
          ([], Some(<:expr< $lid:sprintf "print__%c" ch$ >>, l))

      | '!' :: l ->
          ([], Some(<:expr< EPrintf.print__flush >>, l))

      | ('{' | '}' | '%' as ch) :: l ->
          let const, next = get_const_and_spec mapper _loc l in
          (ch :: const, next)

      | ch :: l ->
          raise (Invalid_conversion_char ch)

      | [] ->
          raise Premature_end
    end

  | '{' :: l ->
      let spec, rest = get_long_spec 0 l in
      let e_spec = mapper#expr (Gram.parse Syntax.expr_eoi _loc (Stream.of_list spec)) in
      ([], Some(e_spec, rest))

  | ch :: l ->
      let const, next = get_const_and_spec mapper _loc l in
      (ch :: const, next)

(* [nconst_expr loc l] expression for a constant string printer *)
let nconst_expr _loc l = <:expr< EPrintf.nconst $str:String.escaped (string_of_estring l)$ >>

(* [make_format mapper loc l] create a format expression from a format
   string *)
let rec make_format mapper _loc l = match get_const_and_spec mapper _loc l with
  | [], None ->
      <:expr< EPrintf.nil >>
  | [], Some(espec, []) ->
      espec
  | [], Some(espec, rest) ->
      <:expr< EPrintf.cons $espec$ $make_format mapper _loc rest$ >>
  | l, None ->
      nconst_expr _loc l
  | l, Some(espec, []) ->
      <:expr< EPrintf.cons $nconst_expr _loc l$ $espec$ >>
  | l, Some(espec, rest) ->
      <:expr< EPrintf.cons $nconst_expr _loc l$
                (EPrintf.cons $espec$ $make_format mapper _loc rest$) >>

(* Handle format parsing error *)
let safe_make_format mapper loc l =
  try
    make_format mapper loc l
  with
    | Premature_end ->
        Loc.raise loc (Stream.Error (sprintf "Premature end of format string %S" (string_of_estring l)))
    | Premature_long_spec_end ->
        Loc.raise loc (Stream.Error (sprintf "'}' missing in format string %S" (string_of_estring l)))
    | Invalid_conversion_char ch ->
        Loc.raise loc (Stream.Error (sprintf "invalid conversion specification character %C in string %S" ch (string_of_estring l)))

(* +--------------------+
   | Strings conversion |
   +--------------------+ *)

(* Handle missing or unknown specifiers, this is only for debugging
   purpose since it should never happen *)
let handle_specifier_error loc = function
  | ch :: _ -> Loc.raise loc (Stream.Error (sprintf "unknown specifier %C" ch))
  | [] -> Loc.raise loc (Stream.Error (sprintf "specifier missing"))

(* [expr_of_annotated_string _loc l] create an expression from an
   annotated string *)
let expr_of_annotated_string _loc = function
  | 'e' :: l -> estring_expr _loc l
  | 'n' :: l -> <:expr< $str:String.escaped (string_of_estring l)$ >>
  | 'p' :: l -> safe_make_format print_mapper _loc l
  | 's' :: l -> safe_make_format scan_mapper _loc l
  | l -> handle_specifier_error _loc l

(* [expr_of_annotated_string _loc l] create an pattern from an
   annotated string *)
let patt_of_annotated_string _loc = function
  | 'e' :: l -> estring_patt _loc l
  | 'n' :: l -> <:patt< $str:String.escaped (string_of_estring l)$ >>
  | ('p' | 's') :: _ -> Loc.raise _loc (Stream.Error "format string are not allowed in pattern")
  | l -> handle_specifier_error _loc l

let map = object
  inherit Ast.map as super

  method expr e = match super#expr e with
    | <:expr@loc< $str:s$ >> -> expr_of_annotated_string loc (estring_of_string (unescape s))
    | e -> e

  method patt p = match super#patt p with
    | <:patt@loc< $str:s$ >> -> patt_of_annotated_string loc (estring_of_string (unescape s))
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
