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

type file = string
type raw_file = file
type backend_lang = En | Fr | Pl
type when_enum = Auto | Always | Never
type message_format_enum = Human | GNU
type input_src = FileName of file | Contents of string * file | Stdin of file

(** Associates a {!type: Cli.backend_lang} with its string represtation. *)
let languages = ["en", En; "fr", Fr; "pl", Pl]

let language_code =
  let rl = List.map (fun (a, b) -> b, a) languages in
  fun l -> List.assoc l rl

let input_src_file = function FileName f | Contents (_, f) | Stdin f -> f
let message_format_opt = ["human", Human; "gnu", GNU]

type options = {
  mutable input_src : input_src;
  mutable language : backend_lang option;
  mutable debug : bool;
  mutable color : when_enum;
  mutable message_format : message_format_enum;
  mutable trace : bool;
  mutable plugins_dirs : file list;
  mutable disable_warnings : bool;
  mutable max_prec_digits : int;
  mutable path_rewrite : raw_file -> file;
}

(* Note: we force that the global options (ie options common to all commands)
   and the options available through global refs are the same. While this is a
   bit arbitrary, it makes some sense code-wise and provides some safeguard
   against explosion of the number of global references. Reducing the number of
   globals further would be nice though. *)
let globals =
  {
    input_src = Stdin "-stdin-";
    language = None;
    debug = false;
    color = Auto;
    message_format = Human;
    trace = false;
    plugins_dirs = [];
    disable_warnings = false;
    max_prec_digits = 20;
    path_rewrite = (fun _ -> assert false);
  }

let enforce_globals
    ?input_src
    ?language
    ?debug
    ?color
    ?message_format
    ?trace
    ?plugins_dirs
    ?disable_warnings
    ?max_prec_digits
    ?path_rewrite
    () =
  Option.iter (fun x -> globals.input_src <- x) input_src;
  Option.iter (fun x -> globals.language <- x) language;
  Option.iter (fun x -> globals.debug <- x) debug;
  Option.iter (fun x -> globals.color <- x) color;
  Option.iter (fun x -> globals.message_format <- x) message_format;
  Option.iter (fun x -> globals.trace <- x) trace;
  Option.iter (fun x -> globals.plugins_dirs <- x) plugins_dirs;
  Option.iter (fun x -> globals.disable_warnings <- x) disable_warnings;
  Option.iter (fun x -> globals.max_prec_digits <- x) max_prec_digits;
  Option.iter (fun x -> globals.path_rewrite <- x) path_rewrite;
  globals

open Cmdliner

(* Arg converters for our custom types *)

let when_opt = Arg.enum ["auto", Auto; "always", Always; "never", Never]

(* Some helpers for catala sources *)

let extensions = [".catala_fr", Fr; ".catala_en", En; ".catala_pl", Pl]

let file_lang filename =
  List.assoc_opt (Filename.extension filename) extensions
  |> function
  | Some lang -> lang
  | None -> (
    match globals.language with
    | Some lang -> lang
    | None ->
      Format.kasprintf failwith
        "Could not infer language variant from the extension of \
         @{<yellow>%s@}, and @{<bold>--language@} was not specified"
        filename)

let reverse_path ?(from_dir = Sys.getcwd ()) ~to_dir f =
  if Filename.is_relative from_dir then invalid_arg "File.with_reverse_path"
  else if not (Filename.is_relative f) then f
  else if not (Filename.is_relative to_dir) then Filename.concat from_dir f
  else
    let rec aux acc rbase = function
      | [] -> acc
      | dir :: p -> (
        if dir = Filename.parent_dir_name then
          match rbase with
          | base1 :: rbase -> aux (base1 :: acc) rbase p
          | [] -> aux acc [] p
        else
          match acc with
          | dir1 :: acc when dir1 = dir -> aux acc rbase p
          | _ -> aux (Filename.parent_dir_name :: acc) rbase p)
    in
    let path_to_list path =
      String.split_on_char Filename.dir_sep.[0] path
      |> List.filter (function "" | "." -> false | _ -> true)
    in
    let rbase = List.rev (path_to_list from_dir) in
    String.concat Filename.dir_sep
      (aux (path_to_list f) rbase (path_to_list to_dir))

(** CLI flags and options *)

module Flags = struct
  open Cmdliner
  open Arg

  module Global = struct
    let info = info ~docs:Manpage.s_common_options

    let input_src =
      let converter =
        conv ~docv:"FILE"
          ( (fun s ->
              if s = "-" then Ok (Stdin "-stdin-")
              else Result.map (fun f -> FileName f) (conv_parser non_dir_file s)),
            fun ppf -> function
              | Stdin _ -> Format.pp_print_string ppf "-"
              | FileName f -> conv_printer non_dir_file ppf f
              | _ -> assert false )
      in
      required
      & pos ~rev:true 0 (some converter) None
      & Arg.info [] ~docv:"FILE" ~docs:Manpage.s_arguments
          ~doc:"Catala master file to be compiled ($(b,-) for stdin)."

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
        let exec_dir = Filename.(dirname Sys.argv.(0)) in
        let dev_plugin_dir = exec_dir / "plugins" in
        if Sys.file_exists dev_plugin_dir then
          (* When running tests in place, may need to lookup in _build/default
             besides the exec *)
          [dev_plugin_dir]
        else
          (* Otherwise, assume a standard layout: "<prefix>/bin/catala" besides
             "<prefix>/lib/catala" *)
          [Filename.(dirname exec_dir) / "lib" / "catala" / "plugins"]
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

    let name_flag =
      value
      & opt (some string) None
      & info ["name"] ~docv:"FILE"
          ~doc:
            "Treat the input as coming from a file with the given name. Useful \
             e.g. when reading from stdin"

    let directory =
      value
      & opt (some dir) None
      & info ["C"; "directory"] ~docv:"DIR"
          ~doc:
            "Behave as if run from the given directory for file and error \
             reporting. Does not affect resolution of files in arguments."

    let flags =
      let make
          language
          debug
          color
          message_format
          trace
          plugins_dirs
          disable_warnings
          max_prec_digits
          directory : options =
        if debug then Printexc.record_backtrace true;
        let path_rewrite =
          match directory with
          | None -> fun f -> f
          | Some to_dir -> (
            function "-" -> "-" | f -> reverse_path ~to_dir f)
        in
        (* This sets some global refs for convenience, but most importantly
           returns the options record. *)
        enforce_globals ~language ~debug ~color ~message_format ~trace
          ~plugins_dirs ~disable_warnings ~max_prec_digits ~path_rewrite ()
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
        $ max_prec_digits
        $ directory)

    let options =
      let make input_src name directory options : options =
        (* Set some global refs for convenience *)
        let input_src =
          match name with
          | None -> input_src
          | Some name -> (
            match input_src with
            | FileName f -> FileName f
            | Contents (str, _) -> Contents (str, name)
            | Stdin _ -> Stdin name)
        in
        let input_src =
          match input_src with
          | FileName f -> FileName (options.path_rewrite f)
          | Contents (str, f) -> Contents (str, options.path_rewrite f)
          | Stdin f -> Stdin (options.path_rewrite f)
        in
        let plugins_dirs = List.map options.path_rewrite options.plugins_dirs in
        Option.iter Sys.chdir directory;
        globals.input_src <- input_src;
        globals.plugins_dirs <- plugins_dirs;
        { options with input_src; plugins_dirs }
      in
      Term.(const make $ input_src $ name_flag $ directory $ flags)
  end

  let include_dirs =
    value
    & opt_all string []
    & info ["I"; "include"] ~docv:"DIR"
        ~doc:"Include directory to lookup for compiled module files."

  let check_invariants =
    value
    & flag
    & info ["check_invariants"] ~doc:"Check structural invariants on the AST."

  let no_typing =
    value
    & flag
    & info ["no-typing"] ~doc:"Don't check the consistency of types"

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
           $(b,--avoid-exceptions)."

  let disable_counterexamples =
    value
    & flag
    & info
        ["disable_counterexamples"]
        ~doc:
          "Disables the search for counterexamples. Useful when you want a \
           deterministic output from the Catala compiler, since provers can \
           have some randomness in them."
end

(* Retrieve current version from dune *)
let version =
  Option.value ~default:"dev"
    Build_info.V1.(Option.map Version.to_string (version ()))

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
