(*
 * ePrintf.ml
 * ----------
 * Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of estring.
 *)

type 'acc writer = {
  add : char -> 'acc -> 'acc;
  flush : 'acc -> 'acc;
}

let channel_writer = { add = (fun ch oc -> output_char oc ch; oc);
                       flush = (fun oc -> flush oc; oc) }
let estring_writer = { add = (fun ch acc -> ch :: acc);
                       flush = (fun acc -> acc) }
let buffer_writer = { add = (fun ch buf -> Buffer.add_char buf ch; buf);
                      flush = (fun buf -> buf) }
let null_writer = { add = (fun _ acc -> acc);
                    flush = (fun acc -> acc) }

type ('a, 'b) printer = {
  print : 'acc. ('acc -> 'b) -> 'acc writer -> 'acc -> 'a;
}

let rec fold f acc = function
  | [] -> acc
  | x :: l -> fold f (f x acc) l

let econst str = { print = fun cont out acc -> cont (fold out.add acc str) }
let nconst str = { print = fun cont out acc ->
                     let acc = ref acc in
                     for i = 0 to String.length str do
                       acc := out.add (String.unsafe_get str i) !acc
                     done;
                     cont !acc }
let cons a b = { print = fun cont out acc -> a.print (b.print cont out) out acc }
let nil = { print = fun cont out acc -> cont acc }
let print__flush = { print = fun cont out acc -> cont (out.flush acc) }

let unit _ = ()

let printf fmt = fmt.print unit channel_writer stdout
let eprintf fmt = fmt.print unit channel_writer stderr
let fprintf oc fmt = fmt.print unit channel_writer oc
let sprintf fmt = fmt.print List.rev estring_writer []
let nprintf fmt =
  let buf = Buffer.create 42 in
  fmt.print Buffer.contents buffer_writer buf
let bprintf buf fmt = fmt.print unit buffer_writer buf
let ifprintf result fmt = fmt.print (fun x -> x) null_writer result

let kfprintf cont oc fmt = fmt.print cont channel_writer oc
let ksprintf cont fmt = fmt.print cont estring_writer []
let knprintf cont fmt =
  let buf = Buffer.create 42 in
  fmt.print (fun buf -> cont (Buffer.contents buf)) buffer_writer buf
let kbprintf cont buf fmt = fmt.print cont buffer_writer buf
