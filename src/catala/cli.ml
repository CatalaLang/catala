(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Ssource files to be compiled *)
let source_files : string list ref = ref []

(** Prints debug information *)
let debug_flag = ref false

open Cmdliner

let file =
  Arg.(
    required
    & pos 1 (some file) None
    & info [] ~docv:"FILE" ~doc:"Catala master file to be compiled")

let debug = Arg.(value & flag & info [ "debug"; "d" ] ~doc:"Prints debug information")

let wrap_weaved_output =
  Arg.(
    value & flag
    & info [ "wrap"; "w" ] ~doc:"Wraps literate programming output with a minimal preamble")

let backend =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"BACKEND" ~doc:"Backend selection among: LaTeX, Makefile, Html, Interpret")

type backend_option = Latex | Makefile | Html | Interpret

let language =
  Arg.(
    value
    & opt (some string) None
    & info [ "l"; "language" ] ~docv:"LANG" ~doc:"Input language among: en, fr, nv (default nv)")

type language_option = [ `Fr | `En | `NonVerbose ]

type reduced_lang_option = [ `Fr | `En ]

let reduce_lang (lang : language_option) : reduced_lang_option =
  match lang with `En | `NonVerbose -> `En | `Fr -> `Fr

let output =
  Arg.(
    value
    & opt (some string) None
    & info [ "output"; "o" ] ~docv:"OUTPUT"
        ~doc:
          "$(i, OUTPUT) is the file that will contain the extracted output (for compiler backends)")

let pygmentize_loc =
  Arg.(
    value
    & opt (some string) None
    & info [ "pygmentize"; "p" ] ~docv:"PYGMENTIZE"
        ~doc:"Location of a custom pygmentize executable for LaTeX source code highlighting")

let catala_t f =
  Term.(const f $ file $ debug $ wrap_weaved_output $ pygmentize_loc $ backend $ language $ output)

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
      `S Manpage.s_examples;
      `P "Typical usage:";
      `Pre "catala LaTeX file.catala";
      `S Manpage.s_bugs;
      `P "Please file bug reports at https://gitlab.inria.fr/verifisc/catala/issues";
    ]
  in
  let exits = Term.default_exits @ [ Term.exit_info ~doc:"on error." 1 ] in
  Term.info "catala"
    ~version:
      ( match Build_info.V1.version () with
      | None -> "n/a"
      | Some v -> Build_info.V1.Version.to_string v )
    ~doc ~exits ~man

(**{1 Terminal formatting}*)

(**{2 Markers}*)

(** Prints [\[DEBUG\]] in purple on the terminal standard output *)
let debug_marker = ANSITerminal.sprintf [ ANSITerminal.Bold; ANSITerminal.magenta ] "[DEBUG] "

(** Prints [\[ERROR\]] in red on the terminal error output *)
let error_marker = ANSITerminal.sprintf [ ANSITerminal.Bold; ANSITerminal.red ] "[ERROR] "

(** Prints [\[WARNING\]] in yellow on the terminal standard output *)
let warning_marker = ANSITerminal.sprintf [ ANSITerminal.Bold; ANSITerminal.yellow ] "[WARNING] "

(** Prints [\[RESULT\]] in green on the terminal standard output *)
let result_marker = ANSITerminal.sprintf [ ANSITerminal.Bold; ANSITerminal.green ] "[RESULT] "

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
    Printf.printf "%s\n" (add_prefix_to_each_line s (fun _ -> debug_marker));
    flush stdout;
    flush stdout
  end

let error_print (s : string) =
  Printf.eprintf "%s\n" (add_prefix_to_each_line s (fun _ -> error_marker));
  flush stdout;
  flush stdout

let warning_print (s : string) =
  Printf.printf "%s\n" (add_prefix_to_each_line s (fun _ -> warning_marker));
  flush stdout;
  flush stdout

let result_print (s : string) =
  Printf.printf "%s\n" (add_prefix_to_each_line s (fun _ -> result_marker));
  flush stdout;
  flush stdout
