(*
  This file is part of the Lawspec compiler, a specification language for tax and social benefits
  computation rules.
  Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

(** Ssource files to be compiled *)
let source_files : string list ref = ref []

(** Prints debug information *)
let debug_flag = ref false

let parse_cli_args () =
  (* Code block to retrieve and parse command-line arguments. *)
  let speclist = Arg.align [
      ("--debug", Arg.Set debug_flag,
       " Prints debugging information");
    ]
  in let usage_msg =
       "Parser and compiler for Lawspec."
  in
  let anon_func (file: string) : unit =
    source_files := file::!source_files
  in Arg.parse speclist anon_func usage_msg



(**{1 Terminal formatting }*)

(**{2 Markers}*)

(** Prints [[DEBUG]] in purple on the terminal standard output *)
let debug_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.magenta] "[DEBUG] "

(** Prints [[ERROR]] in red on the terminal error output *)
let error_marker () = ANSITerminal.eprintf [ANSITerminal.Bold; ANSITerminal.red] "[ERROR] "

(** Prints [[WARNING]] in yellow on the terminal standard output *)
let warning_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.yellow] "[WARNING] "

(** Prints [[RESULT]] in green on the terminal standard output *)
let result_marker () = ANSITerminal.printf [ANSITerminal.Bold; ANSITerminal.green] "[RESULT] "

(**{2 Printers}*)

(** All the printers below print their argument after the correct marker *)

let debug_print (s: string) =
  if !debug_flag then begin
    debug_marker ();
    Printf.printf "%s\n" s;
    flush stdout;
    flush stdout
  end


let error_print (s: string) =
  error_marker ();
  Printf.eprintf "%s\n" s;
  flush stdout;
  flush stdout

let warning_print (s: string) =
  warning_marker ();
  Printf.printf "%s\n" s;
  flush stdout;
  flush stdout

let result_print (s: string) =
  result_marker ();
  Printf.printf "%s\n" s;
  flush stdout;
  flush stdout
