(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

type backend_lang = En | Fr | Pl

(** Source files to be compiled *)
let source_files : string list ref = ref []

let locale_lang : backend_lang ref = ref En

let contents : string ref = ref ""

(** Prints debug information *)
let debug_flag = ref false

(* Styles the terminal output *)
let style_flag = ref true

(* Max number of digits to show for decimal results *)
let max_prec_digits = ref 20

let trace_flag = ref false

let optimize_flag = ref false

open Cmdliner

let file =
  Arg.(
    required
    & pos 1 (some file) None
    & info [] ~docv:"FILE" ~doc:"Catala master file to be compiled")

let debug = Arg.(value & flag & info [ "debug"; "d" ] ~doc:"Prints debug information")

let unstyled = Arg.(value & flag & info [ "unstyled" ] ~doc:"Removes styling from terminal output")

let optimize = Arg.(value & flag & info [ "optimize"; "O" ] ~doc:"Run compiler optimizations")

let trace_opt =
  Arg.(
    value & flag & info [ "trace"; "t" ] ~doc:"Displays a trace of the interpreter's computation")

let wrap_weaved_output =
  Arg.(
    value & flag
    & info [ "wrap"; "w" ] ~doc:"Wraps literate programming output with a minimal preamble")

let backend =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"BACKEND"
        ~doc:
          "Backend selection among: Interpret, OCaml, Python, LaTeX, Makefile, Html, Dcalc, \
           Scopelang")

type backend_option = Latex | Makefile | Html | Run | OCaml | Python | Dcalc | Scopelang

let language =
  Arg.(
    value
    & opt (some string) None
    & info [ "l"; "language" ] ~docv:"LANG" ~doc:"Input language among: en, fr, pl")

let max_prec_digits_opt =
  Arg.(
    value
    & opt (some int) None
    & info [ "p"; "max_digits_printed" ] ~docv:"LANG"
        ~doc:"Maximum number of significant digits printed for decimal results (default 20)")

let ex_scope =
  Arg.(
    value & opt (some string) None & info [ "s"; "scope" ] ~docv:"SCOPE" ~doc:"Scope to be executed")

let output =
  Arg.(
    value
    & opt (some string) None
    & info [ "output"; "o" ] ~docv:"OUTPUT"
        ~doc:"$(i, OUTPUT) is the file that will contain the output of the compiler")

let catala_t f =
  Term.(
    const f $ file $ debug $ unstyled $ wrap_weaved_output $ backend $ language
    $ max_prec_digits_opt $ trace_opt $ optimize $ ex_scope $ output)

let version = "0.5.0"

let info =
  let doc =
    "Compiler for Catala, a specification language for tax and social benefits computation rules."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Catala is a domain-specific language for deriving faithful-by-construction algorithms \
         from legislative texts.";
      `S Manpage.s_authors;
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `P "Nicolas Chataing <nicolas.chataing@ens.fr>";
      `S Manpage.s_examples;
      `P "Typical usage:";
      `Pre "catala LaTeX file.catala";
      `S Manpage.s_bugs;
      `P "Please file bug reports at https://github.com/CatalaLang/catala/issues";
    ]
  in
  let exits = Term.default_exits @ [ Term.exit_info ~doc:"on error." 1 ] in
  Term.info "catala" ~version ~doc ~exits ~man

(**{1 Terminal formatting}*)

(**{2 Markers}*)

let time : float ref = ref (Unix.gettimeofday ())

let print_with_style (styles : ANSITerminal.style list) (str : ('a, unit, string) format) =
  if !style_flag then ANSITerminal.sprintf styles str else Printf.sprintf str

let time_marker () =
  let new_time = Unix.gettimeofday () in
  let old_time = !time in
  time := new_time;
  let delta = (new_time -. old_time) *. 1000. in
  if delta > 50. then
    Printf.printf "%s"
      (print_with_style [ ANSITerminal.Bold; ANSITerminal.black ] "[TIME] %.0f ms\n" delta)

(** Prints [\[DEBUG\]] in purple on the terminal standard output *)
let debug_marker () =
  time_marker ();
  print_with_style [ ANSITerminal.Bold; ANSITerminal.magenta ] "[DEBUG] "

(** Prints [\[ERROR\]] in red on the terminal error output *)
let error_marker () = print_with_style [ ANSITerminal.Bold; ANSITerminal.red ] "[ERROR] "

(** Prints [\[WARNING\]] in yellow on the terminal standard output *)
let warning_marker () = print_with_style [ ANSITerminal.Bold; ANSITerminal.yellow ] "[WARNING] "

(** Prints [\[RESULT\]] in green on the terminal standard output *)
let result_marker () = print_with_style [ ANSITerminal.Bold; ANSITerminal.green ] "[RESULT] "

(** Prints [\[LOG\]] in red on the terminal error output *)
let log_marker () = print_with_style [ ANSITerminal.Bold; ANSITerminal.black ] "[LOG] "

(**{2 Printers}*)

(** All the printers below print their argument after the correct marker *)

let concat_with_line_depending_prefix_and_suffix (prefix : int -> string) (suffix : int -> string)
    (ss : string list) =
  match ss with
  | hd :: rest ->
      let out, _ =
        List.fold_left
          (fun (acc, i) s ->
            ((acc ^ prefix i ^ s ^ if i = List.length ss - 1 then "" else suffix i), i + 1))
          ((prefix 0 ^ hd ^ if 0 = List.length ss - 1 then "" else suffix 0), 1)
          rest
      in
      out
  | [] -> prefix 0

(** The int argument of the prefix corresponds to the line number, starting at 0 *)
let add_prefix_to_each_line (s : string) (prefix : int -> string) =
  concat_with_line_depending_prefix_and_suffix
    (fun i -> prefix i)
    (fun _ -> "\n")
    (String.split_on_char '\n' s)

let debug_print (s : string) =
  if !debug_flag then begin
    Printf.printf "%s\n" (debug_marker () ^ s);
    flush stdout;
    flush stdout
  end

let error_print (s : string) =
  Printf.eprintf "%s\n" (error_marker () ^ s);
  flush stderr;
  flush stderr

let warning_print (s : string) =
  Printf.printf "%s\n" (warning_marker () ^ s);
  flush stdout;
  flush stdout

let result_print (s : string) =
  Printf.printf "%s\n" (result_marker () ^ s);
  flush stdout;
  flush stdout

let log_print (s : string) =
  Printf.printf "%s\n" (log_marker () ^ s);
  flush stdout;
  flush stdout
