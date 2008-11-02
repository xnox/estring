(*
 * test.ml
 * -------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

open EPrintf

let rec get_upper = function
  | [] -> []
  | 'A'..'Z' as ch :: l -> ch :: get_upper l
  | _ :: l -> get_upper l

let x = 'a' :: 'b' :: "cdef"

type plop = A | B of int

let print__plop = { print = fun cont out acc -> function
                      | A -> f"A".print cont out acc
                      | B x -> f"B %d".print cont out acc x }

let _ =
  print_estring "Hello world!\n";
  print_estring (get_upper "aMgsdfAJ sdf" @ "\n");
  print_estring (EList.take 5 "Hello, how are you?" @ "\n");
  print_string n"native caml string\n";
  print_estring (sprintf f"sprintf: %s, %d" "aaaa" 123 @ "\n");
  printf f"a: %s, b: %ns\n" "plop" n"plip";
  printf f"escaped char %C, escaped string %S\n" '\n' "aéaà";
  printf f"%d %X %x %lo %Lx\n%!" 1 43 32422 64l 0x1234abfL;
  printf f"estring: {estring}\n" "plop";
  printf f"int list: {list int}\n" [1; 2; 3];
  printf f"int32 array: {array int32}\n" [|1l; 2l; 3l|];
  printf f"estring option: {option estring}\n" (Some "toto");
  printf f"plop list: {list plop}\n" [A; B 2]
