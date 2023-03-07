(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type backend_lang = En | Fr | Pl

type backend_option_builtin =
  [ `Latex
  | `Makefile
  | `Html
  | `Interpret
  | `Typecheck
  | `OCaml
  | `Python
  | `Solidity
  | `Scalc
  | `Lcalc
  | `Dcalc
  | `Scopelang
  | `Proof ]

type 'a backend_option = [ backend_option_builtin | `Plugin of 'a ]

let backend_option_to_string = function
  | `Interpret -> "Interpret"
  | `Makefile -> "Makefile"
  | `OCaml -> "Ocaml"
  | `Scopelang -> "Scopelang"
  | `Dcalc -> "Dcalc"
  | `Latex -> "Latex"
  | `Proof -> "Proof"
  | `Html -> "Html"
  | `Python -> "Python"
  | `Solidity -> "Solidity"
  | `Typecheck -> "Typecheck"
  | `Scalc -> "Scalc"
  | `Lcalc -> "Lcalc"
  | `Plugin s -> s

let backend_option_of_string backend =
  match String.lowercase_ascii backend with
  | "interpret" -> `Interpret
  | "makefile" -> `Makefile
  | "ocaml" -> `OCaml
  | "scopelang" -> `Scopelang
  | "dcalc" -> `Dcalc
  | "latex" -> `Latex
  | "proof" -> `Proof
  | "html" -> `Html
  | "python" -> `Python
  | "solidity" -> `Solidity
  | "typecheck" -> `Typecheck
  | "scalc" -> `Scalc
  | "lcalc" -> `Lcalc
  | s -> `Plugin s

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
let disable_counterexamples = ref false
let avoid_exceptions_flag = ref false

open Cmdliner

let file =
  Arg.(
    required
    & pos 1 (some file) None
    & info [] ~docv:"FILE" ~doc:"Catala master file to be compiled.")

let debug =
  Arg.(value & flag & info ["debug"; "d"] ~doc:"Prints debug information.")

type when_enum = Auto | Always | Never

let when_opt = Arg.enum ["auto", Auto; "always", Always; "never", Never]

let color =
  Arg.(
    value
    & opt ~vopt:Always when_opt Auto
    & info ["color"]
        ~doc:
          "Allow output of colored and styled text. If set to $(i,auto), \
           enabled when the standard output is to a terminal.")

let unstyled =
  Arg.(
    value
    & flag
    & info ["unstyled"; "u"]
        ~doc:
          "Removes styling (colors, etc.) from terminal output. Equivalent to \
           $(b,--color=never)")

let optimize =
  Arg.(value & flag & info ["optimize"; "O"] ~doc:"Run compiler optimizations.")

let trace_opt =
  Arg.(
    value
    & flag
    & info ["trace"; "t"]
        ~doc:
          "Displays a trace of the interpreter's computation or generates \
           logging instructions in translate programs.")

let avoid_exceptions =
  Arg.(
    value
    & flag
    & info ["avoid_exceptions"]
        ~doc:"Compiles the default calculus without exceptions")

let closure_conversion =
  Arg.(
    value
    & flag
    & info ["closure_conversion"]
        ~doc:"Performs closure conversion on the lambda calculus")

let wrap_weaved_output =
  Arg.(
    value
    & flag
    & info ["wrap"; "w"]
        ~doc:"Wraps literate programming output with a minimal preamble.")

let print_only_law =
  Arg.(
    value
    & flag
    & info ["print_only_law"]
        ~doc:
          "In literate programming output, skip all code and metadata sections \
           and print only the text of the law.")

let backend =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"COMMAND"
        ~doc:
          "Backend selection (see the list of commands for available options).")

let plugins_dirs =
  let doc = "Set the given directory to be searched for backend plugins." in
  let env = Cmd.Env.info "CATALA_PLUGINS" ~doc in
  let default =
    let ( / ) = Filename.concat in
    [
      Filename.dirname Sys.executable_name
      / Filename.parent_dir_name
      / "lib"
      / "catala"
      / "plugins";
    ]
  in
  Arg.(value & opt_all dir default & info ["plugin-dir"] ~docv:"DIR" ~env ~doc)

let language =
  Arg.(
    value
    & opt (some string) None
    & info ["l"; "language"] ~docv:"LANG"
        ~doc:"Input language among: en, fr, pl.")

let max_prec_digits_opt =
  Arg.(
    value
    & opt (some int) None
    & info
        ["p"; "max_digits_printed"]
        ~docv:"DIGITS"
        ~doc:
          "Maximum number of significant digits printed for decimal results \
           (default 20).")

let disable_counterexamples_opt =
  Arg.(
    value
    & flag
    & info
        ["disable_counterexamples"]
        ~doc:
          "Disables the search for counterexamples in proof mode. Useful when \
           you want a deterministic output from the Catala compiler, since \
           provers can have some randomness in them.")

let ex_scope =
  Arg.(
    value
    & opt (some string) None
    & info ["s"; "scope"] ~docv:"SCOPE" ~doc:"Scope to be focused on.")

let output =
  Arg.(
    value
    & opt (some string) None
    & info ["output"; "o"] ~docv:"OUTPUT"
        ~doc:
          "$(i, OUTPUT) is the file that will contain the output of the \
           compiler. Defaults to $(i,FILE).$(i,EXT) where $(i,EXT) depends on \
           the chosen backend. Use $(b,-o -) for stdout.")

type options = {
  debug : bool;
  color : when_enum;
  wrap_weaved_output : bool;
  avoid_exceptions : bool;
  backend : string;
  plugins_dirs : string list;
  language : string option;
  max_prec_digits : int option;
  trace : bool;
  disable_counterexamples : bool;
  optimize : bool;
  ex_scope : string option;
  output_file : string option;
  closure_conversion : bool;
  print_only_law : bool;
}

let options =
  let make
      debug
      color
      unstyled
      wrap_weaved_output
      avoid_exceptions
      closure_conversion
      backend
      plugins_dirs
      language
      max_prec_digits
      trace
      disable_counterexamples
      optimize
      ex_scope
      output_file
      print_only_law : options =
    {
      debug;
      color = (if unstyled then Never else color);
      wrap_weaved_output;
      avoid_exceptions;
      backend;
      plugins_dirs;
      language;
      max_prec_digits;
      trace;
      disable_counterexamples;
      optimize;
      ex_scope;
      output_file;
      closure_conversion;
      print_only_law;
    }
  in
  Term.(
    const make
    $ debug
    $ color
    $ unstyled
    $ wrap_weaved_output
    $ avoid_exceptions
    $ closure_conversion
    $ backend
    $ plugins_dirs
    $ language
    $ max_prec_digits_opt
    $ trace_opt
    $ disable_counterexamples_opt
    $ optimize
    $ ex_scope
    $ output
    $ print_only_law)

let catala_t f = Term.(const f $ file $ options)

let set_option_globals options : unit =
  debug_flag := options.debug;
  (style_flag :=
     match options.color with
     | Always -> true
     | Never -> false
     | Auto -> Unix.isatty Unix.stdout);
  trace_flag := options.trace;
  optimize_flag := options.optimize;
  disable_counterexamples := options.disable_counterexamples;
  avoid_exceptions_flag := options.avoid_exceptions

let version = "0.7.0"

let info =
  let doc =
    "Compiler for Catala, a specification language for tax and social benefits \
     computation rules."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Catala is a domain-specific language for deriving \
         faithful-by-construction algorithms from legislative texts.";
      `S Manpage.s_commands;
      `I
        ( "$(b,Intepret)",
          "Runs the interpreter on the Catala program, executing the scope \
           specified by the $(b,-s) option assuming no additional external \
           inputs." );
      `I
        ( "$(b,Typecheck)",
          "Parses and typechecks a Catala program, without interpreting it." );
      `I
        ( "$(b,Proof)",
          "Generates and proves verification conditions about the well-behaved \
           execution of the Catala program." );
      `I ("$(b,OCaml)", "Generates an OCaml translation of the Catala program.");
      `I ("$(b,Python)", "Generates a Python translation of the Catala program.");
      `I
        ( "$(b,Solidity)",
          "Generates a Solidity translation of the Catala program." );
      `I
        ( "$(b,LaTeX)",
          "Weaves a LaTeX literate programming output of the Catala program." );
      `I
        ( "$(b,HTML)",
          "Weaves an HTML literate programming output of the Catala program." );
      `I
        ( "$(b,Makefile)",
          "Generates a Makefile-compatible list of the file dependencies of a \
           Catala program." );
      `I
        ( "$(b,Scopelang)",
          "Prints a debugging verbatim of the scope language intermediate \
           representation of the Catala program. Use the $(b,-s) option to \
           restrict the output to a particular scope." );
      `I
        ( "$(b,Dcalc)",
          "Prints a debugging verbatim of the default calculus intermediate \
           representation of the Catala program. Use the $(b,-s) option to \
           restrict the output to a particular scope." );
      `I
        ( "$(b,Lcalc)",
          "Prints a debugging verbatim of the lambda calculus intermediate \
           representation of the Catala program. Use the $(b,-s) option to \
           restrict the output to a particular scope." );
      `I
        ( "$(b,Scalc)",
          "Prints a debugging verbatim of the statement calculus intermediate \
           representation of the Catala program. Use the $(b,-s) option to \
           restrict the output to a particular scope." );
      `S Manpage.s_authors;
      `P "The authors are listed by alphabetical order.";
      `P "Nicolas Chataing <nicolas.chataing@ens.fr>";
      `P "Alain Delaët-Tixeuil <alain.delaet--tixeuil@inria.fr>";
      `P "Aymeric Fromherz <aymeric.fromherz@inria.fr>";
      `P "Louis Gesbert <louis.gesbert@ocamlpro.com>";
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `P "Emile Rolley <erolley@tutamail.com>";
      `S Manpage.s_examples;
      `Pre "catala Interpret -s Foo file.catala_en";
      `Pre "catala Ocaml -o target/file.ml file.catala_en";
      `S Manpage.s_bugs;
      `P
        "Please file bug reports at https://github.com/CatalaLang/catala/issues";
    ]
  in
  let exits = Cmd.Exit.defaults @ [Cmd.Exit.info ~doc:"on error." 1] in
  Cmd.info "catala" ~version ~doc ~exits ~man

(**{1 Terminal formatting}*)

(**{2 Markers}*)

let time : float ref = ref (Unix.gettimeofday ())

let with_style
    (styles : ANSITerminal.style list)
    (str : ('a, unit, string) format) =
  if !style_flag then ANSITerminal.sprintf styles str else Printf.sprintf str

let format_with_style (styles : ANSITerminal.style list) fmt (str : string) =
  if !style_flag then
    Format.pp_print_as fmt (String.length str)
      (ANSITerminal.sprintf styles "%s" str)
  else Format.pp_print_string fmt str

let call_unstyled f =
  let prev = !style_flag in
  style_flag := false;
  let res = f () in
  style_flag := prev;
  res

let time_marker () =
  let new_time = Unix.gettimeofday () in
  let old_time = !time in
  time := new_time;
  let delta = (new_time -. old_time) *. 1000. in
  if delta > 50. then
    Printf.printf "%s"
      (with_style
         [ANSITerminal.Bold; ANSITerminal.black]
         "[TIME] %.0f ms\n" delta)

(** Prints [\[DEBUG\]] in purple on the terminal standard output *)
let debug_marker () =
  time_marker ();
  with_style [ANSITerminal.Bold; ANSITerminal.magenta] "[DEBUG] "

(** Prints [\[ERROR\]] in red on the terminal error output *)
let error_marker () =
  with_style [ANSITerminal.Bold; ANSITerminal.red] "[ERROR] "

(** Prints [\[WARNING\]] in yellow on the terminal standard output *)
let warning_marker () =
  with_style [ANSITerminal.Bold; ANSITerminal.yellow] "[WARNING] "

(** Prints [\[RESULT\]] in green on the terminal standard output *)
let result_marker () =
  with_style [ANSITerminal.Bold; ANSITerminal.green] "[RESULT] "

(** Prints [\[LOG\]] in red on the terminal error output *)
let log_marker () = with_style [ANSITerminal.Bold; ANSITerminal.black] "[LOG] "

(**{2 Printers}*)

(** All the printers below print their argument after the correct marker *)

let concat_with_line_depending_prefix_and_suffix
    (prefix : int -> string)
    (suffix : int -> string)
    (ss : string list) =
  match ss with
  | [] -> prefix 0
  | _ :: _ ->
    let len = List.length ss in
    let suffix i = if i < len - 1 then suffix i else "" in
    String.concat ""
    @@ List.concat
    @@ List.mapi
         (fun i s -> [prefix i; (if s = "" then "" else " "); s; suffix i])
         ss

(** The int argument of the prefix corresponds to the line number, starting at 0 *)
let add_prefix_to_each_line (s : string) (prefix : int -> string) =
  concat_with_line_depending_prefix_and_suffix
    (fun i -> prefix i)
    (fun _ -> "\n")
    (String.split_on_char '\n' s)

let debug_print (format : ('a, out_channel, unit) format) =
  if !debug_flag then Printf.printf ("%s" ^^ format ^^ "\n%!") (debug_marker ())
  else Printf.ifprintf stdout format

let debug_format (format : ('a, Format.formatter, unit) format) =
  if !debug_flag then
    Format.printf ("%s@[<hov>" ^^ format ^^ "@]@.") (debug_marker ())
  else Format.ifprintf Format.std_formatter format

let error_print (format : ('a, out_channel, unit) format) =
  Printf.eprintf ("%s" ^^ format ^^ "\n%!") (error_marker ())

let warning_print (format : ('a, out_channel, unit) format) =
  Printf.printf ("%s" ^^ format ^^ "\n%!") (warning_marker ())

let result_print (format : ('a, out_channel, unit) format) =
  Printf.printf ("%s" ^^ format ^^ "\n%!") (result_marker ())

let result_format (format : ('a, Format.formatter, unit) format) =
  Format.printf ("%s" ^^ format ^^ "\n%!") (result_marker ())

let log_print (format : ('a, out_channel, unit) format) =
  Printf.printf ("%s" ^^ format ^^ "\n%!") (log_marker ())

let log_format (format : ('a, Format.formatter, unit) format) =
  Format.printf ("%s@[<hov>" ^^ format ^^ "@]@.") (log_marker ())
