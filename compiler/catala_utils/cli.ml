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

type backend_option =
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

(** Associates a {!type: Cli.backend_lang} with its string represtation. *)
let languages = ["en", En; "fr", Fr; "pl", Pl]

let language_code =
  let rl = List.map (fun (a, b) -> b, a) languages in
  fun l -> List.assoc l rl

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
    & pos 0 (some file) None
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
    & info ["unstyled"]
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

let link_modules =
  Arg.(
    value
    & opt_all file []
    & info ["use"; "u"] ~docv:"FILE"
        ~doc:
          "Specifies an additional module to be linked to the Catala program. \
           $(i,FILE) must be a catala file with a metadata section expressing \
           what is exported ; for interpretation, a compiled OCaml shared \
           module by the same basename (either .cmo or .cmxs) will be \
           expected.")

type global_options = {
  debug : bool;
  color : when_enum;
  message_format : message_format_enum;
  wrap_weaved_output : bool;
  avoid_exceptions : bool;
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
  link_modules : string list;
}

let global_options =
  let make
      debug
      color
      message_format
      unstyled
      wrap_weaved_output
      avoid_exceptions
      closure_conversion
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
      print_only_law
      link_modules : global_options =
    {
      debug;
      color = (if unstyled then Never else color);
      message_format;
      wrap_weaved_output;
      avoid_exceptions;
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
      link_modules;
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
    $ print_only_law
    $ link_modules)

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

let subcommands handler =
  [
    Cmd.v
      (Cmd.info "interpret"
         ~doc:
           "Runs the interpreter on the Catala program, executing the scope \
            specified by the $(b,-s) option assuming no additional external \
            inputs.")
      Term.(const (handler `Interpret) $ file $ global_options);
    Cmd.v
      (Cmd.info "interpret_lcalc"
         ~doc:
           "Runs the interpreter on the lcalc pass on the Catala program, \
            executing the scope specified by the $(b,-s) option assuming no \
            additional external inputs.")
      Term.(const (handler `Interpret_Lcalc) $ file $ global_options);
    Cmd.v
      (Cmd.info "typecheck"
         ~doc:"Parses and typechecks a Catala program, without interpreting it.")
      Term.(const (handler `Typecheck) $ file $ global_options);
    Cmd.v
      (Cmd.info "proof"
         ~doc:
           "Generates and proves verification conditions about the \
            well-behaved execution of the Catala program.")
      Term.(const (handler `Proof) $ file $ global_options);
    Cmd.v
      (Cmd.info "ocaml"
         ~doc:"Generates an OCaml translation of the Catala program.")
      Term.(const (handler `OCaml) $ file $ global_options);
    Cmd.v
      (Cmd.info "python"
         ~doc:"Generates a Python translation of the Catala program.")
      Term.(const (handler `Python) $ file $ global_options);
    Cmd.v
      (Cmd.info "latex"
         ~doc:
           "Weaves a LaTeX literate programming output of the Catala program.")
      Term.(const (handler `Latex) $ file $ global_options);
    Cmd.v
      (Cmd.info "html"
         ~doc:
           "Weaves an HTML literate programming output of the Catala program.")
      Term.(const (handler `Html) $ file $ global_options);
    Cmd.v
      (Cmd.info "makefile"
         ~doc:
           "Generates a Makefile-compatible list of the file dependencies of a \
            Catala program.")
      Term.(const (handler `Makefile) $ file $ global_options);
    Cmd.v
      (Cmd.info "scopelang"
         ~doc:
           "Prints a debugging verbatim of the scope language intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(const (handler `Scopelang) $ file $ global_options);
    Cmd.v
      (Cmd.info "dcalc"
         ~doc:
           "Prints a debugging verbatim of the default calculus intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(const (handler `Dcalc) $ file $ global_options);
    Cmd.v
      (Cmd.info "lcalc"
         ~doc:
           "Prints a debugging verbatim of the lambda calculus intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(const (handler `Lcalc) $ file $ global_options);
    Cmd.v
      (Cmd.info "scalc"
         ~doc:
           "Prints a debugging verbatim of the statement calculus intermediate \
            representation of the Catala program. Use the $(b,-s) option to \
            restrict the output to a particular scope.")
      Term.(const (handler `Scalc) $ file $ global_options);
    Cmd.v
      (Cmd.info "exceptions"
         ~doc:
           "Prints the exception tree for the definitions of a particular \
            variable, for debugging purposes. Use the $(b,-s) option to select \
            the scope and the $(b,-v) option to select the variable. Use \
            foo.bar to access state bar of variable foo or variable bar of \
            subscope foo.")
      Term.(const (handler `Exceptions) $ file $ global_options);
    Cmd.v
      (Cmd.info "pygmentize"
         ~doc:
           "This special command is a wrapper around the $(b,pygmentize) \
            command that enables support for colorising Catala code.")
      Term.(const (fun _ -> assert false) $ file);
  ]

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

let catala_t ?(extra = []) handler = Cmd.group info (subcommands handler @ extra)
