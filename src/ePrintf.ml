(*
 * ePrintf.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

open Format

type ('a, 'b) printer =  (formatter -> 'b) -> formatter -> 'a

let unit _ = ()
let newline pp = pp_print_newline pp ()

let list_formatter lref =
  make_formatter
    (fun str ofs len ->
       for i = ofs to ofs + len - 1 do
         lref := String.unsafe_get str i :: !lref
       done)
    unit
let null_formatter = make_formatter (fun _ _ _ -> ()) unit

let rec fold f acc = function
  | [] -> acc
  | x :: l -> fold f (f x acc) l

let econst str cont pp = pp_print_string pp (EString.to_string str); cont pp
let nconst str cont pp = pp_print_string pp str; cont pp
let cons a b cont pp = a (b cont) pp
let nil cont pp = cont pp
let print__flush cont pp = pp_print_flush pp (); cont pp

let printf fmt = fmt unit std_formatter
let println fmt = fmt newline std_formatter
let eprintf fmt = fmt unit err_formatter
let eprintln fmt = fmt newline err_formatter
let fprintf pp fmt = fmt unit pp
let sprintf fmt =
  let l = ref [] in
  let pp = list_formatter l in
  fmt (fun _ -> pp_print_flush pp (); !l) pp
let nprintf fmt =
  let buf = Buffer.create 42 in
  let pp = formatter_of_buffer buf in
  fmt (fun _ -> pp_print_flush pp (); Buffer.contents buf) pp
let bprintf buf fmt = fmt unit (formatter_of_buffer buf)
let iprintf result fmt = fmt (fun _ -> result) null_formatter

let kfprintf cont pp fmt = fmt cont pp
let knprintf cont fmt =
  let buf = Buffer.create 42 in
  let pp = formatter_of_buffer buf in
  fmt (fun _ -> pp_print_flush pp (); cont (Buffer.contents buf)) pp
let ksprintf cont fmt =
  let l = ref [] in
  let pp = list_formatter l in
  fmt (fun _ -> pp_print_flush pp (); cont !l) pp

let print__open_box x cont pp = pp_open_box pp x; cont pp
let print__open_hbox cont pp = pp_open_hbox pp (); cont pp
let print__open_vbox x cont pp = pp_open_vbox pp x; cont pp
let print__open_hvbox x cont pp = pp_open_hvbox pp x; cont pp
let print__open_hovbox x cont pp = pp_open_hovbox pp x; cont pp
let print__close_box cont pp = pp_close_box pp (); cont pp
let print__print_cut cont pp = pp_print_cut pp (); cont pp
let print__print_space cont pp = pp_print_space pp (); cont pp
let print__force_newline cont pp = pp_force_newline pp (); cont pp
let print__print_break nspaces offset cont pp = pp_print_break pp nspaces offset; cont pp
let print__print_flush cont pp = pp_print_flush pp (); cont pp
let print__print_newline cont pp = pp_print_newline pp (); cont pp
let print__open_tag t cont pp = pp_open_tag pp t; cont pp
let print__close_tag cont pp = pp_close_tag pp (); cont pp
