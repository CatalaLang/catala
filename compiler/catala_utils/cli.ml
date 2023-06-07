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
  | `Interpret_Lcalc
  | `Typecheck
  | `OCaml
  | `Python
  | `Scalc
  | `Lcalc
  | `Dcalc
  | `Scopelang
  | `Exceptions
  | `Proof ]

type 'a backend_option = [ backend_option_builtin | `Plugin of 'a ]

(** Associates a {!type: Cli.backend_lang} with its string represtation. *)
let languages = ["en", En; "fr", Fr; "pl", Pl]

let language_code =
  let rl = List.map (fun (a, b) -> b, a) languages in
  fun l -> List.assoc l rl

let backend_option_to_string = function
  | `Interpret -> "Interpret"
  | `Interpret_Lcalc -> "Interpret_Lcalc"
  | `Makefile -> "Makefile"
  | `OCaml -> "Ocaml"
  | `Scopelang -> "Scopelang"
  | `Dcalc -> "Dcalc"
  | `Latex -> "Latex"
  | `Proof -> "Proof"
  | `Html -> "Html"
  | `Python -> "Python"
  | `Typecheck -> "Typecheck"
  | `Scalc -> "Scalc"
  | `Lcalc -> "Lcalc"
  | `Exceptions -> "Exceptions"
  | `Plugin s -> s

let backend_option_of_string backend =
  match String.lowercase_ascii backend with
  | "interpret" -> `Interpret
  | "interpret_lcalc" -> `Interpret_Lcalc
  | "makefile" -> `Makefile
  | "ocaml" -> `OCaml
  | "scopelang" -> `Scopelang
  | "dcalc" -> `Dcalc
  | "latex" -> `Latex
  | "proof" -> `Proof
  | "html" -> `Html
  | "python" -> `Python
  | "typecheck" -> `Typecheck
  | "scalc" -> `Scalc
  | "lcalc" -> `Lcalc
  | "exceptions" -> `Exceptions
  | s -> `Plugin s

(** Source files to be compiled *)
let source_files : string list ref = ref []

let locale_lang : backend_lang ref = ref En
let contents : string ref = ref ""

(** Prints debug information *)
let debug_flag = ref false

type when_enum = Auto | Always | Never

(* Styles the terminal output *)
let style_flag = ref Auto

(* Max number of digits to show for decimal results *)
let max_prec_digits = ref 20
let trace_flag = ref false
let disable_warnings_flag = ref false
let optimize_flag = ref false
let disable_counterexamples = ref false
let avoid_exceptions_flag = ref false
let check_invariants_flag = ref false

type message_format_enum = Human | GNU

let message_format_flag = ref Human

open Cmdliner

let file =
  Arg.(
    required
    & pos 1 (some file) None
    & info [] ~docv:"FILE" ~doc:"Catala master file to be compiled.")

let debug =
  Arg.(value & flag & info ["debug"; "d"] ~doc:"Prints debug information.")

let when_opt = Arg.enum ["auto", Auto; "always", Always; "never", Never]

let color =
  Arg.(
    value
    & opt ~vopt:Always when_opt Auto
    & info ["color"]
        ~doc:
          "Allow output of colored and styled text. If set to $(i,auto), \
           enabled when the standard output is to a terminal.")

let message_format_opt = Arg.enum ["human", Human; "gnu", GNU]

let message_format =
  Arg.(
    value
    & opt message_format_opt Human
    & info ["message_format"]
        ~doc:
          "Selects the format of error and warning messages emitted by the \
           compiler. If set to $(i,human), the messages will be nicely \
           displayed and meant to be read by a human. If set to $(i, gnu), the \
           messages will be rendered according to the GNU coding standards.")

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

let disable_warnings_opt =
  Arg.(
    value
    & flag
    & info ["disable_warnings"]
        ~doc:"Disable all the warnings emitted by the compiler.")

let check_invariants_opt =
  Arg.(
    value
    & flag
    & info ["check_invariants"] ~doc:"Check structural invariants on the AST.")

let avoid_exceptions =
  Arg.(
    value
    & flag
    & info ["avoid_exceptions"]
        ~doc:"Compiles the default calculus without exceptions.")

let closure_conversion =
  Arg.(
    value
    & flag
    & info ["closure_conversion"]
        ~doc:"Performs closure conversion on the lambda calculus.")

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

let ex_variable =
  Arg.(
    value
    & opt (some string) None
    & info ["v"; "variable"] ~docv:"VARIABLE" ~doc:"Variable to be focused on.")

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
  message_format : message_format_enum;
  wrap_weaved_output : bool;
  avoid_exceptions : bool;
  backend : string;
  plugins_dirs : string list;
  language : string option;
  max_prec_digits : int option;
  trace : bool;
  disable_warnings : bool;
  disable_counterexamples : bool;
  check_invariants : bool;
  optimize : bool;
  ex_scope : string option;
  ex_variable : string option;
  output_file : string option;
  closure_conversion : bool;
  print_only_law : bool;
}

let options =
  let make
      debug
      color
      message_format
      unstyled
      wrap_weaved_output
      avoid_exceptions
      closure_conversion
      backend
      plugins_dirs
      language
      max_prec_digits
      disable_warnings
      trace
      disable_counterexamples
      optimize
      check_invariants
      ex_scope
      ex_variable
      output_file
      print_only_law : options =
    {
      debug;
      color = (if unstyled then Never else color);
      message_format;
      wrap_weaved_output;
      avoid_exceptions;
      backend;
      plugins_dirs;
      language;
      max_prec_digits;
      disable_warnings;
      trace;
      disable_counterexamples;
      optimize;
      check_invariants;
      ex_scope;
      ex_variable;
      output_file;
      closure_conversion;
      print_only_law;
    }
  in
  Term.(
    const make
    $ debug
    $ color
    $ message_format
    $ unstyled
    $ wrap_weaved_output
    $ avoid_exceptions
    $ closure_conversion
    $ backend
    $ plugins_dirs
    $ language
    $ max_prec_digits_opt
    $ disable_warnings_opt
    $ trace_opt
    $ disable_counterexamples_opt
    $ optimize
    $ check_invariants_opt
    $ ex_scope
    $ ex_variable
    $ output
    $ print_only_law)

let catala_t f = Term.(const f $ file $ options)

let set_option_globals options : unit =
  debug_flag := options.debug;
  style_flag := options.color;
  (match options.max_prec_digits with
  | None -> ()
  | Some i -> max_prec_digits := i);
  disable_warnings_flag := options.disable_warnings;
  trace_flag := options.trace;
  optimize_flag := options.optimize;
  check_invariants_flag := options.check_invariants;
  disable_counterexamples := options.disable_counterexamples;
  avoid_exceptions_flag := options.avoid_exceptions;
  message_format_flag := options.message_format

let version = "0.8.0"

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
        ( "$(b,Intepret_Lcalc)",
          "Runs the interpreter on the lcalc pass on the Catala program, \
           executing the scope specified by the $(b,-s) option assuming no \
           additional external inputs." );
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
      `I
        ( "$(b,Exceptions)",
          "Prints the exception tree for the definitions of a particular \
           variable, for debugging purposes. Use the $(b,-s) option to select \
           the scope and the $(b,-v) option to select the variable. Use \
           foo.bar to access state bar of variable foo or variable bar of \
           subscope foo." );
      `I
        ( "$(b,pygmentize)",
          "This special command is a wrapper around the $(b,pygmentize) \
           command that enables support for colorising Catala code." );
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

(* let with_style
 *     (styles : ANSITerminal.style list)
 *     (str : ('a, unit, string) format) =
 *   if !style_flag then ANSITerminal.sprintf styles str else Printf.sprintf str
 * 
 * let format_with_style (styles : ANSITerminal.style list) fmt (str : string) =
 *   if !style_flag then
 *     Format.pp_print_as fmt (String.length str)
 *       (ANSITerminal.sprintf styles "%s" str)
 *   else Format.pp_print_string fmt str
 * 
 * let call_unstyled f =
 *   let prev = !style_flag in
 *   style_flag := false;
 *   let res = f () in
 *   style_flag := prev;
 *   res
 * 
 * let concat_with_line_depending_prefix_and_suffix
 *     (prefix : int -> string)
 *     (suffix : int -> string)
 *     (ss : string list) =
 *   match ss with
 *   | [] -> prefix 0
 *   | _ :: _ ->
 *     let len = List.length ss in
 *     let suffix i = if i < len - 1 then suffix i else "" in
 *     String.concat ""
 *     @@ List.concat
 *     @@ List.mapi
 *          (fun i s -> [prefix i; (if s = "" then "" else " "); s; suffix i])
 *          ss
 * 
 * (\** The int argument of the prefix corresponds to the line number, starting at 0 *\)
 * let add_prefix_to_each_line (s : string) (prefix : int -> string) =
 *   concat_with_line_depending_prefix_and_suffix
 *     (fun i -> prefix i)
 *     (fun _ -> "\n")
 *     (String.split_on_char '\n' s) *)
