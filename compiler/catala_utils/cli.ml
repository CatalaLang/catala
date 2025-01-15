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

open Global

(* Manipulation of types used by flags & options *)

(** Associates a {!type: Global.backend_lang} with its string represtation. *)
let languages = ["en", En; "fr", Fr; "pl", Pl]

let language_code =
  let rl = List.map (fun (a, b) -> b, a) languages in
  fun l -> List.assoc l rl

let message_format_opt = ["human", (Human : message_format_enum); "gnu", GNU]
let trace_format_opt = ["human", (Human : trace_format_enum); "json", JSON]

open Cmdliner

(* Arg converters for our custom types *)

let when_opt = Arg.enum ["auto", Auto; "always", Always; "never", Never]

let raw_file =
  Arg.conv ~docv:"FILE"
    ( (fun f -> Result.map raw_file (Arg.conv_parser Arg.string f)),
      fun ppf f -> Format.pp_print_string ppf (f :> string) )

(* Some helpers for catala sources *)

let extensions = [".catala_fr", Fr; ".catala_en", En; ".catala_pl", Pl]

let file_lang filename =
  List.assoc_opt (Filename.extension filename) extensions
  |> function
  | Some lang -> lang
  | None -> (
    match Global.options.language with
    | Some lang -> lang
    | None ->
      Format.kasprintf failwith
        "Could not infer language variant from the extension of \
         @{<yellow>%s@}, and @{<bold>--language@} was not specified"
        filename)

let exec_dir =
  let cmd = Sys.argv.(0) in
  if String.contains cmd '/' then
    (* Do not use Sys.executable_name, which may resolve symlinks: we want the
       original path. (e.g. _build/install/default/bin/foo is a symlink) *)
    Filename.dirname cmd
  else (* searched in PATH *)
    Filename.dirname Sys.executable_name

(** CLI flags and options *)

let s_plugins = "INSTALLED PLUGINS"
let s_debug = "DEBUGGING COMMANDS"

module Flags = struct
  open Cmdliner
  open Arg

  module Global = struct
    let info = info ~docs:Manpage.s_common_options

    let input_src =
      let converter =
        conv ~docv:"FILE"
          ( (fun s ->
              if s = "-" then Ok (Stdin (Global.raw_file "-stdin-"))
              else
                Result.map
                  (fun f -> FileName (Global.raw_file f))
                  (conv_parser non_dir_file s)),
            fun ppf -> function
              | Stdin _ -> Format.pp_print_string ppf "-"
              | FileName f -> conv_printer non_dir_file ppf (f :> file)
              | _ -> assert false )
      in
      required
      & pos 0 (some converter) None
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
      & info ["message-format"]
          ~doc:
            "Selects the format of error and warning messages emitted by the \
             compiler. If set to $(i,human), the messages will be nicely \
             displayed and meant to be read by a human. If set to $(i, gnu), \
             the messages will be rendered according to the GNU coding \
             standards."

    let trace =
      let converter =
        conv ~docv:"FILE"
          ( (fun s ->
              if s = "-" then Ok (`Stdout)
              else Ok (`FileName (Global.raw_file s))),
            fun ppf -> function
              | `Stdout -> Format.pp_print_string ppf "-"
              | `FileName f -> Format.pp_print_string ppf (f:>string) )
      in
      value
      & opt (some converter) None ~vopt:(Some `Stdout)
      & info ["trace"; "t"]
          ~docv: "FILE"
          ~env:(Cmd.Env.info "CATALA_TRACE")
          ~doc:
            "Displays a trace of the interpreter's computation or generates \
             logging instructions in translate programs. If set as a flag, outputs
             trace to stdout. If $(docv) is defined, outputs the trace to a file while interpreting.
             Defining a filename does not affect code generation."

    let trace_format =
      value
      & opt (enum trace_format_opt) Human
      & info ["trace-format"]
          ~doc:
            "Selects the format of trace logs emitted by the interpreter. If \
             set to $(i,human), the messages will be nicely displayed and \
             meant to be read by a human. If set to $(i, json), the messages \
             will be emitted as a JSON structured object."

    let plugins_dirs =
      let doc = "Set the given directory to be searched for backend plugins." in
      let env = Cmd.Env.info "CATALA_PLUGINS" in
      let default =
        let ( / ) = Filename.concat in
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
      & info ["disable-warnings"]
          ~doc:"Disable all the warnings emitted by the compiler."

    let max_prec_digits =
      value
      & opt int 20
      & info
          ["p"; "max-digits-printed"]
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

    let stop_on_error =
      value
      & flag
      & info ["x"; "stop-on-error"]
          ~doc:"Stops the compilation as soon as an error is encountered."

    let no_fail_on_assert =
      value
      & flag
      & info ["no-fail-on-assert"]
          ~doc:
            "Instead of reporting an error on assertion failure, reports a \
             warning and carry on with the interpretation as usual."

    let flags =
      let make
          language
          debug
          color
          message_format
          trace
          trace_format
          plugins_dirs
          disable_warnings
          max_prec_digits
          directory
          stop_on_error
          no_fail_on_assert : options =
        if debug then Printexc.record_backtrace true;
        let path_rewrite =
          match directory with
          | None -> fun (f : Global.raw_file) -> (f :> file)
          | Some to_dir -> (
            fun (f : Global.raw_file) ->
              match (f :> file) with
              | "-" -> "-"
              | f -> File.reverse_path ~to_dir f)
        in
        let trace = match trace with
        | None -> None
        | Some `Stdout -> Some (lazy (Message.std_ppf ()))
        | Some `FileName f -> Some (lazy (Message.formatter_of_out_channel (open_out (path_rewrite f)) () ))
        in
        (* This sets some global refs for convenience, but most importantly
           returns the options record. *)
        Global.enforce_options ~language ~debug ~color ~message_format ~trace
          ~trace_format ~plugins_dirs ~disable_warnings
          ~max_prec_digits ~path_rewrite ~stop_on_error ~no_fail_on_assert ()
      in
      Term.(
        const make
        $ language
        $ debug
        $ color
        $ message_format
        $ trace
        $ trace_format
        $ plugins_dirs
        $ disable_warnings
        $ max_prec_digits
        $ directory
        $ stop_on_error
        $ no_fail_on_assert)

    let options =
      let make input_src name directory options : options =
        (* Set some global refs for convenience *)
        let input_src =
          let rename f =
            match name with None -> f | Some n -> Global.raw_file n
          in
          match input_src with
          | FileName f -> FileName (options.path_rewrite f)
          | Contents (str, f) -> Contents (str, options.path_rewrite (rename f))
          | Stdin f -> Stdin (options.path_rewrite (rename f))
        in
        Option.iter Sys.chdir directory;
        Global.enforce_options ~input_src ()
      in
      Term.(const make $ input_src $ name_flag $ directory $ flags)
  end

  let include_dirs =
    let arg =
      Arg.(
        value
        & opt_all (list ~sep:':' raw_file) []
        & info ["I"; "include"] ~docv:"DIR"
            ~env:(Cmd.Env.info "CATALA_INCLUDE")
            ~doc:
              "Make modules from the given directory available from \
               everywhere. Several dirs can be specified by repeating the flag \
               or separating them with '$(b,:)'.")
    in
    Term.(const List.flatten $ arg)

  let check_invariants =
    value
    & flag
    & info ["check-invariants"] ~doc:"Check structural invariants on the AST."

  let autotest =
    value
    & flag
    & info ["autotest"]
        ~env:(Cmd.Env.info "CATALA_AUTOTEST")
        ~doc:
          "Insert automatic test assertions in the compiled program. This \
           detects all scopes that have no input or context variables, runs \
           the interpreter to pre-compute their values, then adds runtime \
           assertions to the program that ensure that the actual output of the \
           scopes match their pre-computed values. If used on a testing \
           program with a given backend, this guarantees consistency between \
           the backend and the interpreter."

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
    & info ["print-only-law"]
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
    & opt (some raw_file) None
    & info ["output"; "o"] ~docv:"OUTPUT"
        ~env:(Cmd.Env.info "CATALA_OUT")
        ~doc:
          "$(i, OUTPUT) is the file that will contain the output of the \
           compiler. Defaults to $(i,FILE).$(i,EXT) where $(i,EXT) depends on \
           the chosen backend. Use $(b,-o -) for stdout."

  let optimize =
    value
    & flag
    & info ["optimize"; "O"]
        ~env:(Cmd.Env.info "CATALA_OPTIMIZE")
        ~doc:"Run compiler optimizations."

  let keep_special_ops =
    value
    & flag
    & info ["keep-special-ops"]
        ~doc:
          "During closure conversion (between Lcalc and Scalc), do not convert \
           the functional arguments of higher-order operators."

  let monomorphize_types =
    value
    & flag
    & info ["monomorphize-types"]
        ~doc:
          "In LCalc, replaces the polymorphic option type by monomorphized \
           versions of the enumeration, and transform tuples into named \
           structs. "

  let expand_ops =
    value
    & flag
    & info ["expand-ops"]
        ~doc:
          "In LCalc, expand equality operators to only rely on comparisons of \
           literals. "

  let dead_value_assignment =
    value
    & flag
    & info ["dead-value-assignment"]
        ~doc:
          "During the Lcalc->Scalc translation, insert dummy variable \
           assignments before raising terminal exception to please gradual \
           typing tools that check exhaustivity of variable definitions in \
           every code branch."

  let no_struct_literals =
    value
    & flag
    & info ["no-struct-literals"]
        ~doc:
          "During the Lcalc->Scalc translation, insert temporary variable \
           assignments to hold the result of array and structure \
           initializations (matches the absence of struct literals of C89)."

  let closure_conversion =
    value
    & flag
    & info ["closure-conversion"]
        ~doc:"Performs closure conversion on the lambda calculus."

  let disable_counterexamples =
    value
    & flag
    & info
        ["disable-counterexamples"]
        ~doc:
          "Disables the search for counterexamples. Useful when you want a \
           deterministic output from the Catala compiler, since provers can \
           have some randomness in them."

  let extra_files =
    value
    & pos_right 0 file []
    & Arg.info [] ~docv:"FILES" ~docs:Manpage.s_arguments
        ~doc:"Additional input files."

  let lcalc =
    value
    & flag
    & info ["lcalc"]
        ~doc:
          "Compile all the way to lcalc before interpreting (the default is to \
           interpret at dcalc stage). For debugging purposes."

  let extension =
    value
    & opt_all string []
    & info ["extension"; "e"] ~docv:"EXT"
        ~doc:
          "Replace the original file extensions with $(i,.EXT). If repeated, \
           the file will be listed once which each supplied extension."

  let prefix =
    value
    & opt (some string) None
    & info ["prefix"] ~docv:"PATH"
        ~doc:"Prepend the given path to each of the files in the returned list."
end

(* Retrieve current version from dune *)
let version = Version.v

let man_header =
  [
    `S Manpage.s_description;
    `P
      "Catala is a domain-specific language for deriving \
       faithful-by-construction algorithms from legislative texts.";
  ]

let man_footer =
  [
    `S Manpage.s_common_options;
    `S Manpage.s_authors;
    `P "The authors are listed by alphabetical order:";
    `P "Vincent Botbol <$(i,vincent.botbol@inria.fr)>";
    `Noblank;
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
    `S Manpage.s_bugs;
    `P "Please file bug reports at https://github.com/CatalaLang/catala/issues";
  ]

let man_base = man_header @ man_footer

let info =
  let doc =
    "Compiler for Catala, a specification language for tax and social benefits \
     computation rules."
  in
  let man =
    man_header
    @ [
        `S Manpage.s_synopsis;
        `P "$(mname) [$(i,COMMAND)] $(i,FILE) [$(i,OPTION)]…";
        `P
          "Use $(mname) [$(i,COMMAND)] $(b,--hel)p for documentation on a \
           specific command";
        `S Manpage.s_commands;
        `S s_plugins;
        `S s_debug;
        `P
          "These commands are intended for debugging of the Catala compiler \
           itself, and unlikely to be useful to the end-user";
        `S Manpage.s_examples;
        `Pre "catala Interpret -s Foo file.catala_en";
        `Pre "catala Ocaml -o target/file.ml file.catala_en";
      ]
    @ man_footer
  in
  let exits = Cmd.Exit.defaults @ [Cmd.Exit.info ~doc:"on error." 1] in
  Cmd.info "catala" ~version ~doc ~exits ~man

exception Exit_with of int
