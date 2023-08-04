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

(* Types used by flags & options *)

type backend_lang = En | Fr | Pl
type when_enum = Auto | Always | Never
type message_format_enum = Human | GNU
type compilation_method = Expression | Statement
type input_file = FileName of string | Contents of string

(** Associates a {!type: Cli.backend_lang} with its string represtation. *)
let languages = ["en", En; "fr", Fr; "pl", Pl]

let language_code =
  let rl = List.map (fun (a, b) -> b, a) languages in
  fun l -> List.assoc l rl

let message_format_opt = ["human", Human; "gnu", GNU]
let compilation_method_opt = ["expression", Expression; "statement", Statement]

type options = {
  mutable input_file : input_file;
  mutable language : backend_lang option;
  mutable debug : bool;
  mutable color : when_enum;
  mutable message_format : message_format_enum;
  mutable trace : bool;
  mutable plugins_dirs : string list;
  mutable disable_warnings : bool;
  mutable max_prec_digits : int;
}

(* Note: we force that the global options (ie options common to all commands)
   and the options available through global refs are the same. While this is a
   bit arbitrary, it makes some sense code-wise and provides some safeguard
   against explosion of the number of global references. Reducing the number of
   globals further would be nice though. *)
let globals =
  {
    input_file = Contents "";
    language = None;
    debug = false;
    color = Auto;
    message_format = Human;
    trace = false;
    plugins_dirs = [];
    disable_warnings = false;
    max_prec_digits = 20;
  }

let enforce_globals
    ?input_file
    ?language
    ?debug
    ?color
    ?message_format
    ?trace
    ?plugins_dirs
    ?disable_warnings
    ?max_prec_digits
    () =
  Option.iter (fun x -> globals.input_file <- x) input_file;
  Option.iter (fun x -> globals.language <- x) language;
  Option.iter (fun x -> globals.debug <- x) debug;
  Option.iter (fun x -> globals.color <- x) color;
  Option.iter (fun x -> globals.message_format <- x) message_format;
  Option.iter (fun x -> globals.trace <- x) trace;
  Option.iter (fun x -> globals.plugins_dirs <- x) plugins_dirs;
  Option.iter (fun x -> globals.disable_warnings <- x) disable_warnings;
  Option.iter (fun x -> globals.max_prec_digits <- x) max_prec_digits;
  globals

open Cmdliner

(* Arg converters for our custom types *)

let when_opt = Arg.enum ["auto", Auto; "always", Always; "never", Never]

(** CLI flags and options *)

module Flags = struct
  open Cmdliner
  open Arg

  module Global = struct
    let info = info ~docs:Manpage.s_common_options

    let input_file =
      let converter =
        conv ~docv:"FILE"
          ( (fun s ->
              Result.map (fun f -> FileName f) (conv_parser non_dir_file s)),
            fun ppf -> function
              | FileName f -> conv_printer non_dir_file ppf f
              | _ -> assert false )
      in
      required
      & pos 0 (some converter) None
      & Arg.info [] ~docv:"FILE" ~docs:Manpage.s_arguments
          ~doc:"Catala master file to be compiled."

    let language =
      value
      & opt (some (enum languages)) None
      & info ["l"; "language"] ~docv:"LANG"
          ~doc:
            "Locale variant of the input language to use when it can not be \
             inferred from the file extension."

    let debug =
      value
      & flag
      & info ["debug"; "d"]
          ~env:(Cmd.Env.info "CATALA_DEBUG")
          ~doc:"Prints debug information."

    let color =
      let unstyled =
        value
        & flag
        & info ["unstyled"]
            ~doc:"Removes styling (colors, etc.) from terminal output."
            ~deprecated:"Use $(b,--color=)$(i,never) instead"
      in
      let color =
        value
        & opt ~vopt:Always when_opt Auto
        & info ["color"]
            ~env:(Cmd.Env.info "CATALA_COLOR")
            ~doc:
              "Allow output of colored and styled text. Use $(i,auto), to \
               enable when the standard output is to a terminal, $(i,never) to \
               disable."
      in
      Term.(
        const (fun color unstyled -> if unstyled then Never else color)
        $ color
        $ unstyled)

    let message_format =
      value
      & opt (enum message_format_opt) Human
      & info ["message_format"]
          ~doc:
            "Selects the format of error and warning messages emitted by the \
             compiler. If set to $(i,human), the messages will be nicely \
             displayed and meant to be read by a human. If set to $(i, gnu), \
             the messages will be rendered according to the GNU coding \
             standards."

    let trace =
      value
      & flag
      & info ["trace"; "t"]
          ~doc:
            "Displays a trace of the interpreter's computation or generates \
             logging instructions in translate programs."

    let plugins_dirs =
      let doc = "Set the given directory to be searched for backend plugins." in
      let env = Cmd.Env.info "CATALA_PLUGINS" in
      let default =
        let ( / ) = Filename.concat in
        [
          Filename.dirname Sys.executable_name
          / Filename.parent_dir_name
          / "lib"
          / "catala"
          / "plugins";
          "_build" / "default" / "compiler" / "plugins";
        ]
      in
      value & opt_all string default & info ["plugin-dir"] ~docv:"DIR" ~env ~doc

    let disable_warnings =
      value
      & flag
      & info ["disable_warnings"]
          ~doc:"Disable all the warnings emitted by the compiler."

    let max_prec_digits =
      value
      & opt int 20
      & info
          ["p"; "max_digits_printed"]
          ~docv:"NUM"
          ~doc:
            "Maximum number of significant digits printed for decimal results."

    let flags =
      let make
          language
          debug
          color
          message_format
          trace
          plugins_dirs
          disable_warnings
          max_prec_digits : options =
        if debug then Printexc.record_backtrace true;
        (* This sets some global refs for convenience, but most importantly
           returns the options record. *)
        enforce_globals ~language ~debug ~color ~message_format ~trace
          ~plugins_dirs ~disable_warnings ~max_prec_digits ()
      in
      Term.(
        const make
        $ language
        $ debug
        $ color
        $ message_format
        $ trace
        $ plugins_dirs
        $ disable_warnings
        $ max_prec_digits)

    let options =
      let make input_file options : options =
        (* Set some global refs for convenience *)
        globals.input_file <- input_file;
        { options with input_file }
      in
      Term.(const make $ input_file $ flags)
  end

  let check_invariants =
    value
    & flag
    & info ["check_invariants"] ~doc:"Check structural invariants on the AST."

  let wrap_weaved_output =
    value
    & flag
    & info ["wrap"; "w"]
        ~doc:"Wraps literate programming output with a minimal preamble."

  let print_only_law =
    value
    & flag
    & info ["print_only_law"]
        ~doc:
          "In literate programming output, skip all code and metadata sections \
           and print only the text of the law."

  let ex_scope =
    required
    & opt (some string) None
    & info ["s"; "scope"] ~docv:"SCOPE" ~doc:"Scope to be focused on."

  let ex_scope_opt =
    value
    & opt (some string) None
    & info ["s"; "scope"] ~docv:"SCOPE" ~doc:"Scope to be focused on."

  let ex_variable =
    required
    & opt (some string) None
    & info ["v"; "variable"] ~docv:"VARIABLE" ~doc:"Variable to be focused on."

  let output =
    value
    & opt (some string) None
    & info ["output"; "o"] ~docv:"OUTPUT"
        ~env:(Cmd.Env.info "CATALA_OUT")
        ~doc:
          "$(i, OUTPUT) is the file that will contain the output of the \
           compiler. Defaults to $(i,FILE).$(i,EXT) where $(i,EXT) depends on \
           the chosen backend. Use $(b,-o -) for stdout."

  let optimize =
    value & flag & info ["optimize"; "O"] ~doc:"Run compiler optimizations."

  let avoid_exceptions =
    value
    & flag
    & info ["avoid_exceptions"]
        ~doc:"Compiles the default calculus without exceptions."

  let closure_conversion =
    value
    & flag
    & info ["closure_conversion"]
        ~doc:
          "Performs closure conversion on the lambda calculus. Implies \
           $(b,--avoid-exceptions) and $(b,--optimize)."

  let link_modules =
    value
    & opt_all file []
    & info ["use"; "u"] ~docv:"FILE"
        ~doc:
          "Specifies an additional module to be linked to the Catala program. \
           $(i,FILE) must be a catala file with a metadata section expressing \
           what is exported ; for interpretation, a compiled OCaml shared \
           module by the same basename (either .cmo or .cmxs) will be \
           expected."

  let disable_counterexamples =
    value
    & flag
    & info
        ["disable_counterexamples"]
        ~doc:
          "Disables the search for counterexamples. Useful when you want a \
           deterministic output from the Catala compiler, since provers can \
           have some randomness in them."

  let scalc_try_with_compilation =
    value
    & opt (enum compilation_method_opt) Statement
    & info
        ["scalc_try_with_compilation"]
        ~doc:
          "How should try ... with ... constructs be compiled from Lcalc to \
           Scalc ? Choice is between $(i,expression) or $(i,statement)."
end

let version = "0.8.0"
let s_plugins = "INSTALLED PLUGINS"

let info =
  let doc =
    "Compiler for Catala, a specification language for tax and social benefits \
     computation rules."
  in
  let man =
    [
      `S Manpage.s_synopsis;
      `P "$(mname) [$(i,COMMAND)] $(i,FILE) [$(i,OPTION)]…";
      `P
        "Use $(mname) [$(i,COMMAND)] $(b,--hel)p for documentation on a \
         specific command";
      `S Manpage.s_description;
      `P
        "Catala is a domain-specific language for deriving \
         faithful-by-construction algorithms from legislative texts.";
      `S Manpage.s_commands;
      `S s_plugins;
      `S Manpage.s_authors;
      `P "The authors are listed by alphabetical order:";
      `P "Nicolas Chataing <$(i,nicolas.chataing@ens.fr)>";
      `Noblank;
      `P "Alain Delaët-Tixeuil <$(i,alain.delaet--tixeuil@inria.fr)>";
      `Noblank;
      `P "Aymeric Fromherz <$(i,aymeric.fromherz@inria.fr)>";
      `Noblank;
      `P "Louis Gesbert <$(i,louis.gesbert@ocamlpro.com)>";
      `Noblank;
      `P "Denis Merigoux <$(i,denis.merigoux@inria.fr)>";
      `Noblank;
      `P "Emile Rolley <$(i,erolley@tutamail.com)>";
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

exception Exit_with of int
