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

type specifier = string

let lookup tbl key =
  try
    Some(Hashtbl.find tbl key)
  with
      Not_found -> None

(* +---------------------+
   | Lists with location |
   +---------------------+ *)

type 'a llist =
  | Nil of Loc.t
  | Cons of Loc.t * 'a * 'a llist

let loc_of_llist = function
  | Nil loc -> loc
  | Cons(loc, x, l) -> loc

let rec llength_rec acc = function
  | Nil _ -> acc
  | Cons(_, _, ll) -> llength_rec (acc + 1) ll

let llength ll = llength_rec 0 ll

let rec lfoldr f g = function
  | Nil loc -> g loc
  | Cons(loc, x, l) -> f loc x (lfoldr f g l)

let rec list_of_llist = function
  | Nil _ -> []
  | Cons(_, x, l) -> x :: list_of_llist l

let rec llist_of_list loc = function
  | [] -> Nil loc
  | x :: l -> Cons(loc, x, llist_of_list (Loc.move `start 1 loc) l)

let rec ldrop n l =
  if n <= 0 then
    l
  else match l with
    | Cons(_, _, l) -> ldrop (n - 1) l
    | l -> l

let rec ltake n l =
  if n <= 0 then
    Nil (loc_of_llist l)
  else match l with
    | Cons(loc, x, l) -> Cons(loc, x, ltake (n - 1) l)
    | l -> l

let rec lappend ll1 ll2 = match ll1 with
  | Nil _ -> ll1
  | Cons(loc, x, ll) -> Cons(loc, x, lappend ll ll2)

let llist_expr f ll = lfoldr (fun _loc x acc -> <:expr< $f _loc x$ :: $acc$ >>) (fun _loc -> <:expr< [] >>) ll
let llist_patt f ll = lfoldr (fun _loc x acc -> <:patt< $f _loc x$ :: $acc$ >>) (fun _loc -> <:patt< [] >>) ll

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

let unescape loc str =
  let l = ref [] in
  for i = String.length str - 1 downto 0 do
    l := str.[i] :: !l
  done;
  Unescape.string loc !l

(* +------------------------+
   | Specifier registration |
   +------------------------+ *)

let expr_specifiers = Hashtbl.create 42
let patt_specifiers = Hashtbl.create 42

let register_expr_specifier ?(shared=false) specifier f =
  Hashtbl.add expr_specifiers specifier (shared, f)

let register_patt_specifier specifier f =
  Hashtbl.add patt_specifiers specifier f

(* +------------------------------+
   | String specifier recognition |
   +------------------------------+ *)

(* In order to recognize expressions/patterns of the form [u"string"],
   [p"string"], ... without recognising [u "string"], [X.u"string"] we
   need to add a token filter.

   By the way the expansion need to be done at filtering time, so we
   need to remember the specifier a string use. For that we keep in a
   global table the specifier associated to each string location.
*)

let specifier_table = Hashtbl.create 42

let specifier loc = lookup specifier_table loc

let wrap_stream stm =
  (* The previous token *)
  let previous = ref EOI in

  let rec func pos =
    try
      let prev = !previous
      and tok, loc = Stream.next stm in

      previous := tok;

      match tok with
        | (LIDENT id | UIDENT id) when prev <> KEYWORD "." &&
            (lookup expr_specifiers id <> None || lookup patt_specifiers id <> None) ->
            begin match Stream.peek stm with
              | Some(STRING(s, orig), loc) ->
                  Stream.junk stm;
                  Hashtbl.add specifier_table loc id;
                  Some(STRING(s, orig), loc)
              | _ ->
                  Some(tok, loc)
            end

        | _ ->
            Some(tok, loc)
    with
        Stream.Failure -> None

  in
  Stream.from func

(* +--------------------+
   | Strings conversion |
   +--------------------+ *)

let shared_exprs = ref []

let register_shared_expr =
  let nb = ref 0 in
  fun expr ->
    let id = "__estring_constant_" ^ string_of_int !nb in
    incr nb;
    shared_exprs := (id, expr) :: !shared_exprs;
    id

let map = object
  inherit Ast.map as super

  method expr e = match super#expr e with
    | <:expr@_loc< $str:str$ >> as e -> begin
        match specifier _loc with
          | Some specifier -> begin
              match lookup expr_specifiers specifier with
                | Some(shared, f) ->
                    let expr = f _loc str in
                    if shared then
                      let id = register_shared_expr expr in
                      <:expr< $lid:id$ >>
                    else
                      expr
                | None ->
                    Loc.raise _loc
                      (Failure
                         (sprintf "pa_estring: unknown string expression specifier: %S" specifier))
            end
          | None -> e
      end

    | e -> e

  method patt p = match super#patt p with
    | <:patt@_loc< $str:str$ >> as p -> begin
        match specifier _loc with
          | Some specifier -> begin
              match lookup patt_specifiers specifier with
                | Some f ->
                    f _loc str
                | None ->
                    Loc.raise _loc
                      (Failure
                         (sprintf "pa_estring: unknown string pattern specifier: %S" specifier))
            end
          | None -> p
      end

    | p -> p
end

(* +--------------+
   | Registration |
   +--------------+ *)

let _ =
  (* Register the token filter for specifiers *)
  Gram.Token.Filter.define_filter (Gram.get_filter ()) (fun filter stm -> filter (wrap_stream stm));

  (* Register the mapper *)
  AstFilters.register_str_item_filter
    (fun si ->
       let si = map#str_item si in
       let _loc = Ast.loc_of_str_item si in
       (* Add all shared expression at the beginning of the file *)
       <:str_item<
         $Ast.stSem_of_list
           (List.map
              (fun (id, expr) ->
                 let _loc = Ast.loc_of_expr expr in
                 <:str_item< let $lid:id$ = $expr$ >>) !shared_exprs)$
         $si$
       >>)
