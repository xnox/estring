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

let unescape = Camlp4.Struct.Token.Eval.string ~strict:()

let explode s =
  let rec aux acc = function
    | -1 -> acc
    | i -> aux (s.[i] :: acc) (i - 1)
  in
  aux [] (String.length s - 1)

let implode l =
  let s = String.create (List.length l) in
  let rec aux i = function
    | [] -> s
    | x :: l -> String.unsafe_set s i x; aux (i + 1) l
  in
  aux 0 l

let estring_expr _loc l = List.fold_right (fun ch acc -> <:expr< $chr:Char.escaped ch$ :: $acc$ >>) l <:expr< [] >>
let estring_patt _loc l = List.fold_right (fun ch acc -> <:patt< $chr:Char.escaped ch$ :: $acc$ >>) l <:patt< [] >>

let rec map_ident = function
  | <:ident@_loc< $id:x$.$y$ >> -> <:ident< $id:x$.$map_ident y$ >>
  | <:ident@_loc< $lid:x$ >> -> <:ident< $lid:"print__" ^ x$ >>
  | x -> x

let map = object
  inherit Ast.map
  method ident = map_ident
end

exception Premature_end
exception Premature_long_spec_end
exception Invalid_conversion_char of char

let rec get_long_spec deep = function
  | [] -> raise Premature_long_spec_end
  | ['%'] -> raise Premature_end
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

let rec get_const_and_spec _loc = function
  | [] -> ([], None)
  | ['%'] ->
      raise Premature_end
  | '%' :: ch :: l ->
      begin match ch with
        | 'l' | 'n' | 'L' ->
            begin match l with
              | [] ->
                  ([], Some(<:expr< $lid:sprintf "print__%c" ch$ >>, l))
              | ch' :: l' ->
                  match ch' with
                    | 'd' | 'i' | 'u' | 'x' | 'X' | 'o' ->
                        ([], Some(<:expr< $lid:sprintf "print__%c%c" ch ch'$ >>, l'))
                    | ('s' | 'S') when ch = 'n' ->
                        ([], Some(<:expr< $lid:sprintf "print__%c%c" ch ch'$ >>, l'))
                    | _ ->
                        ([], Some(<:expr< $lid:sprintf "print__%c" ch$ >>, l))
            end
        | 'a'..'z' | 'A'..'Z' ->
            ([], Some(<:expr< $lid:sprintf "print__%c" ch$ >>, l))
        | '!' ->
            ([], Some(<:expr< EPrintf.print__flush >>, l))
        | '{' | '}' | '%' ->
            let const, next = get_const_and_spec _loc l in
            (ch :: const, next)
        | _ ->
            raise (Invalid_conversion_char ch)
      end
  | '{' :: l ->
      let spec, rest = get_long_spec 0 l in
      let e_spec = map#expr (Gram.parse Syntax.expr_eoi _loc (Stream.of_list spec)) in
      ([], Some(e_spec, rest))
  | ch :: l ->
      let const, next = get_const_and_spec _loc l in
      (ch :: const, next)

let nconst_expr _loc l = <:expr< EPrintf.nconst $str:String.escaped (implode l)$ >>

let rec make_format _loc l = match get_const_and_spec _loc l with
  | [], None ->
      <:expr< EPrintf.nil >>
  | [], Some(espec, []) ->
      espec
  | [], Some(espec, rest) ->
      <:expr< EPrintf.cons $espec$ $make_format _loc rest$ >>
  | l, None ->
      nconst_expr _loc l
  | l, Some(espec, []) ->
      <:expr< EPrintf.cons $nconst_expr _loc l$ $espec$ >>
  | l, Some(espec, rest) ->
      <:expr< EPrintf.cons $nconst_expr _loc l$
                (EPrintf.cons $espec$ $make_format _loc rest$) >>

let expr_of_annotated_string _loc = function
  | 's' :: l -> estring_expr _loc l
  | 'n' :: l -> <:expr< $str:String.escaped (implode l)$ >>
  | 'f' :: l ->
      begin try
        make_format _loc l
      with
        | Premature_end ->
            Loc.raise _loc (Stream.Error (sprintf "Premature end of format string %S" (implode l)))
        | Premature_long_spec_end ->
            Loc.raise _loc (Stream.Error (sprintf "'}' missing in format string %S" (implode l)))
        | Invalid_conversion_char ch ->
            Loc.raise _loc (Stream.Error (sprintf "invalid conversion specification character %C in string %S" ch (implode l)))
      end
  | _ -> assert false

let patt_of_annotated_string _loc = function
  | 's' :: l -> estring_patt _loc l
  | 'n' :: l -> <:patt< $str:String.escaped (implode l)$ >>
  | 'f' :: _ -> Loc.raise _loc (Stream.Error "format string are not allowed in pattern")
  | _ -> assert false

let map = object
  inherit Ast.map as super

  method expr e = match super#expr e with
    | <:expr@loc< $str:s$ >> -> expr_of_annotated_string loc (explode (unescape s))
    | e -> e

  method patt p = match super#patt p with
    | <:patt@loc< $str:s$ >> -> patt_of_annotated_string loc (explode (unescape s))
    | p -> p
end

let annot annot = function
  | STRING(s, orig) -> STRING(annot ^ s, annot ^ orig)
  | tok -> tok

let make_annotated_stream stm =
  let previous_is_dot = ref false in
  Stream.from begin fun _ ->
    try
      let tok, loc = Stream.next stm
      and b = !previous_is_dot in

      previous_is_dot := (match tok with
                            | KEYWORD "." -> true
                            | _ -> false);

      match b, tok with
        | true, _ -> Some(annot "s" tok, loc)
        | false, LIDENT id ->
            begin match Stream.peek stm with
              | Some(STRING _ as tok', loc') ->
                  begin match id with
                    | "f" | "n" ->
                        Stream.junk stm;
                        Some(annot id tok', loc')
                    | _ -> Some(tok, loc)
                  end
              | _ -> Some(tok, loc)
            end
        | false, _ -> Some(annot "s" tok, loc)

    with
        Stream.Failure -> None
  end

let _ =
  Gram.Token.Filter.define_filter (Gram.get_filter ()) (fun filter stm -> filter (make_annotated_stream stm));
  AstFilters.register_str_item_filter map#str_item;
  AstFilters.register_str_item_filter
    (fun s ->
       let _loc = Ast.loc_of_str_item s in
       <:str_item< open EString_pervasives;; $s$ >>);
  AstFilters.register_sig_item_filter
    (fun s ->
       let _loc = Ast.loc_of_sig_item s in
       <:sig_item< open EString_pervasives;; $s$ >>)
