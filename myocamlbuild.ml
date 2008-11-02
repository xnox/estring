(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

open Printf
open Ocamlbuild_plugin

let run_and_read = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings

let exec cmd =
  blank_sep_strings &
    Lexing.from_string &
    run_and_read cmd

(* this lists all supported packages *)
let find_packages () = exec "ocamlfind list | cut -d' ' -f1"

(* this is supposed to list available syntaxes, but I don't know how
   to do it. *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]

(* ocamlfind command *)
let ocamlfind x = S[A"ocamlfind"; x]

(* Utils *)

let flag_all_stages_except_link tag f =
  flag ["ocaml"; "compile"; tag] f;
  flag ["ocaml"; "ocamldep"; tag] f;
  flag ["ocaml"; "doc"; tag] f

let flag_all_stages tag f =
  flag_all_stages_except_link tag f;
  flag ["ocaml"; "link"; tag] f

let mk_mods_path p = List.map (fun s -> p ^ "/" ^ String.uncapitalize s)

let _ =
  dispatch begin function
    | Before_options ->

        (* override default commands by ocamlfind ones *)
        Options.ocamlc   := ocamlfind & A"ocamlc";
        Options.ocamlopt := ocamlfind & A"ocamlopt";
        Options.ocamldep := ocamlfind & A"ocamldep";
        Options.ocamldoc := ocamlfind & A"ocamldoc"

    | After_rules ->

        (* Internal use of estring *)
        ocaml_lib ~dir:"src" "estring";
        dep ["ocaml"; "byte"; "use_estring"] ["estring.cma"];
        dep ["ocaml"; "native"; "use_estring"] ["estring.cmxa"];

        (* When one link an OCaml library/binary/package, one should use -linkpkg *)
        flag ["ocaml"; "link"] & A"-linkpkg";

        (* For each ocamlfind package one inject the -package option
           when compiling, computing dependencies, generating
           documentation and linking. *)
        List.iter
          (fun pkg -> flag_all_stages ("pkg_" ^ pkg) (S[A"-package"; A pkg]))
          (find_packages ());

        (* Like -package but for extensions syntax. Morover -syntax is
           useless when linking. *)
        List.iter
          (fun syntax -> flag_all_stages_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax]))
          (find_syntaxes ());

        (* Internal use of pa_estring *)
        flag_all_stages_except_link "pa_estring" & S[A"-ppopt"; A("syntax/pa_estring.cmo")];
        dep ["ocaml"; "ocamldep"; "pa_estring"] ["syntax/pa_estring.cmo"]
    | _ -> ()
  end
