(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | Before_options ->
             Options.make_links := false

         | After_rules ->
             flag ["ocaml"; "compile"; "pa_estring"] & S[A"-ppopt"; A "estring.cma"];
             flag ["ocaml"; "ocamldep"; "pa_estring"] & S[A"-ppopt"; A "estring.cma"];
             flag ["ocaml"; "doc"; "pa_estring"] & S[A"-ppopt"; A "estring.cma"];
             dep ["ocaml"; "ocamldep"; "pa_estring"] ["estring.cma"];

             flag ["ocaml"; "compile"; "pa_string_list"] & S[A"-ppopt"; A "sample/pa_string_list.cmo"];
             flag ["ocaml"; "ocamldep"; "pa_string_list"] & S[A"-ppopt"; A "sample/pa_string_list.cmo"];
             flag ["ocaml"; "doc"; "pa_string_list"] & S[A"-ppopt"; A "sample/pa_string_list.cmo"];
             dep ["ocaml"; "ocamldep"; "pa_string_list"] ["sample/pa_string_list.cmo"]


         | _ ->
             ())
