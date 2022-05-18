(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020 Inria,
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

open Cmdliner
open Utils
open Ninja_utils
module Nj = Ninja_utils

(** {1 Command line interface} *)

let files_or_folders =
  Arg.(
    non_empty & pos_right 0 file []
    & info [] ~docv:"FILE(S)" ~doc:"File(s) or folder(s) to process")

let command =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"COMMAND" ~doc:"Command selection among: test, run")

let debug =
  Arg.(value & flag & info ["debug"; "d"] ~doc:"Prints debug information")

let reset_test_outputs =
  Arg.(
    value & flag
    & info ["r"; "reset"]
        ~doc:
          "Used with the `test` command, resets the test output to whatever is \
           output by the Catala compiler.")

let catalac =
  Arg.(
    value
    & opt (some string) None
    & info ["e"; "exe"] ~docv:"EXE"
        ~doc:"Catala compiler executable, defaults to `catala`")

let ninja_output =
  Arg.(
    value
    & opt (some string) None
    & info ["o"; "output"] ~docv:"OUTPUT"
        ~doc:
          "$(i, OUTPUT) is the file that will contain the build.ninja file \
           output. If not specified, the build.ninja file will be outputed in \
           the temporary directory of the system.")

let scope =
  Arg.(
    value
    & opt (some string) None
    & info ["s"; "scope"] ~docv:"SCOPE"
        ~doc:
          "Used with the `run` command, selects which scope of a given Catala \
           file to run.")

let makeflags =
  Arg.(
    value
    & opt (some string) None
    & info ["makeflags"] ~docv:"LANG"
        ~doc:
          "Provides the contents of a $(i, MAKEFLAGS) variable to pass on to \
           Ninja. Currently recognizes the -i and -j options.")

let catala_opts =
  Arg.(
    value
    & opt (some string) None
    & info ["c"; "catala-opts"] ~docv:"LANG"
        ~doc:"Options to pass to the Catala compiler")

let clerk_t f =
  Term.(
    const f $ files_or_folders $ command $ catalac $ catala_opts $ makeflags
    $ debug $ scope $ reset_test_outputs $ ninja_output)

let version = "0.5.0"

let info =
  let doc =
    "Build system for Catala, a specification language for tax and social \
     benefits computation rules."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(b,clerk) is a build system for Catala, a specification language for \
         tax and social benefits computation rules";
      `S Manpage.s_commands;
      `I
        ( "test",
          "Tests a Catala source file given expected outputs provided in a \
           directory called `output` at the same level that the tested file. \
           If the tested file is `foo.catala_en`, then `output` should contain \
           expected output files like `foo.catala_en.$(i,BACKEND)` where  \
           $(i,BACKEND) is chosen among: `Interpret`, `Dcalc`, `Scalc`, \
           `Lcalc`, `Typecheck, `Scopelang`, `html`, `tex`, `py`, `ml` and `d` \
           (for Makefile dependencies). For the `Interpret` backend, the scope \
           to test is selected by naming the expected output file \
           `foo.catala_en.$(i,SCOPE).interpret`. When the argument of \
           $(b,clerk) is a folder, it recursively looks for Catala files \
           coupled with `output` directories and matching expected output on \
           which to perform tests." );
      `I
        ( "run",
          "Runs the Catala interpreter on a given scope of a given file. See \
           the `-s` option." );
      `S Manpage.s_authors;
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `P "Emile Rolley <emile.rolley@tuta.io>";
      `S Manpage.s_examples;
      `P "Typical usage:";
      `Pre "clerk test file.catala_en";
      `S Manpage.s_bugs;
      `P
        "Please file bug reports at https://github.com/CatalaLang/catala/issues";
    ]
  in
  let exits = Cmd.Exit.defaults @ [Cmd.Exit.info ~doc:"on error." 1] in
  Cmd.info "clerk" ~version ~doc ~exits ~man

(**{1 Testing}*)

type expected_output_descr = {
  base_filename : string;
  output_dir : string;
  complete_filename : string;
  backend : Cli.backend_option;
  scope : string option;
}

let catala_suffix_regex = Re.Pcre.regexp "\\.catala_(\\w){2}"

let filename_to_expected_output_descr (output_dir : string) (filename : string)
    : expected_output_descr option =
  let complete_filename = filename in
  let first_extension = Filename.extension filename in
  let filename = Filename.remove_extension filename in
  let backend =
    match String.lowercase_ascii first_extension with
    | ".dcalc" -> Some Cli.Dcalc
    | ".d" -> Some Cli.Makefile
    | ".html" -> Some Cli.Html
    | ".interpret" -> Some Cli.Interpret
    | ".lcalc" -> Some Cli.Lcalc
    | ".ml" -> Some Cli.OCaml
    | ".proof" -> Some Cli.Proof
    | ".py" -> Some Cli.Python
    | ".scalc" -> Some Cli.Scalc
    | ".scopelang" -> Some Cli.Scopelang
    | ".tex" -> Some Cli.Latex
    | ".typecheck" -> Some Cli.Typecheck
    | _ -> None
  in
  match backend with
  | None -> None
  | Some backend ->
    let second_extension = Filename.extension filename in
    let base_filename, scope =
      if Re.Pcre.pmatch ~rex:catala_suffix_regex second_extension then
        filename, None
      else
        let scope_name_regex = Re.Pcre.regexp "\\.(.+)" in
        let scope_name =
          try (Re.Pcre.extract ~rex:scope_name_regex second_extension).(1)
          with Not_found -> ""
        in
        Filename.remove_extension filename, Some scope_name
    in
    Some { output_dir; complete_filename; base_filename; backend; scope }

(** [readdir_sort dirname] returns the sorted subdirectories of [dirname] in an
    array or an empty array if the [dirname] doesn't exist. *)
let readdir_sort (dirname : string) : string array =
  try
    let dirs = Sys.readdir dirname in
    Array.fast_sort String.compare dirs;
    dirs
  with Sys_error _ -> Array.make 0 ""

(** Given a file, looks in the relative [output] directory if there are files
    with the same base name that contain expected outputs for different *)
let search_for_expected_outputs (file : string) : expected_output_descr list =
  let output_dir = Filename.dirname file ^ Filename.dir_sep ^ "output/" in
  let output_files = readdir_sort output_dir in
  List.filter_map
    (fun output_file ->
      match filename_to_expected_output_descr output_dir output_file with
      | None -> None
      | Some expected_output ->
        if expected_output.base_filename = Filename.basename file then
          Some expected_output
        else None)
    (Array.to_list output_files)

let add_reset_rules_aux
    ~(redirect : string)
    ~(with_scope_output_rule : string)
    ~(without_scope_output_rule : string)
    (catala_exe_opts : string)
    (rules : Rule.t Nj.RuleMap.t) : Rule.t Nj.RuleMap.t =
  let reset_common_cmd_exprs =
    Nj.Expr.
      [
        Var "catala_cmd";
        Var "tested_file";
        Var "extra_flags";
        Lit "--unstyled";
        Lit redirect;
        Var "expected_output";
        Lit "2>&1";
        Lit "| true";
      ]
  in
  let reset_with_scope_rule =
    Nj.Rule.make with_scope_output_rule
      ~command:
        Nj.Expr.(
          Seq
            ([Lit catala_exe_opts; Lit "-s"; Var "scope"]
            @ reset_common_cmd_exprs))
      ~description:
        Nj.Expr.(
          Seq
            [
              Lit "RESET scope";
              Var "scope";
              Lit "of file";
              Var "tested_file";
              Lit "with the";
              Var "catala_cmd";
              Lit "command";
            ])
  in
  let reset_without_scope_rule =
    Nj.Rule.make without_scope_output_rule
      ~command:Nj.Expr.(Seq (Lit catala_exe_opts :: reset_common_cmd_exprs))
      ~description:
        Nj.Expr.(
          Seq
            [
              Lit "RESET";
              Lit "file";
              Var "tested_file";
              Lit "with the";
              Var "catala_cmd";
              Lit "command";
            ])
  in
  Nj.RuleMap.(
    rules
    |> add reset_with_scope_rule.name reset_with_scope_rule
    |> add reset_without_scope_rule.name reset_without_scope_rule)

let add_test_rules_aux
    ~(test_common_cmd_exprs : Expr.t list)
    ~(with_scope_output_rule : string)
    ~(without_scope_output_rule : string)
    (catala_exe_opts : string)
    (rules : Rule.t Nj.RuleMap.t) : Rule.t Nj.RuleMap.t =
  let test_with_scope_rule =
    Nj.Rule.make with_scope_output_rule
      ~command:
        Nj.Expr.(
          Seq
            ([Lit catala_exe_opts; Lit "-s"; Var "scope"]
            @ test_common_cmd_exprs))
      ~description:
        Nj.Expr.(
          Seq
            [
              Lit "TEST scope";
              Var "scope";
              Lit "of file";
              Var "tested_file";
              Lit "with the";
              Var "catala_cmd";
              Lit "command";
            ])
  in
  let test_without_scope_rule =
    Nj.Rule.make without_scope_output_rule
      ~command:Nj.Expr.(Seq (Lit catala_exe_opts :: test_common_cmd_exprs))
      ~description:
        Nj.Expr.(
          Seq
            [
              Lit "TEST on file";
              Var "tested_file";
              Lit "with the";
              Var "catala_cmd";
              Lit "command";
            ])
  in
  Nj.RuleMap.(
    rules
    |> add test_with_scope_rule.name test_with_scope_rule
    |> add test_without_scope_rule.name test_without_scope_rule)

(** [add_reset_rules catala_exe_opts rules] adds ninja rules used to reset test
    files into [rules] and returns it.*)
let add_reset_rules (catala_exe_opts : string) (rules : Rule.t Nj.RuleMap.t) :
    Rule.t Nj.RuleMap.t =
  add_reset_rules_aux ~with_scope_output_rule:"reset_with_scope"
    ~without_scope_output_rule:"reset_without_scope" ~redirect:">"
    catala_exe_opts rules

(** [add_test_rules catala_exe_opts rules] adds ninja rules used to test files
    into [rules] and returns it.*)
let add_test_rules (catala_exe_opts : string) (rules : Rule.t Nj.RuleMap.t) :
    Rule.t Nj.RuleMap.t =
  let test_common_cmd_exprs =
    Nj.Expr.
      [
        Var "catala_cmd";
        Var "tested_file";
        Var "extra_flags";
        Lit "--unstyled";
        Lit "2>&1 | colordiff -u -b";
        Var "expected_output";
        Lit "-";
      ]
  in
  add_test_rules_aux ~test_common_cmd_exprs
    ~with_scope_output_rule:"test_with_scope"
    ~without_scope_output_rule:"test_without_scope" catala_exe_opts rules

(** [add_reset_with_ouput_rules catala_exe_opts rules] adds ninja rules used to
    reset test files using an output flag into [rules] and returns it.*)
let add_reset_with_output_rules
    (catala_exe_opts : string)
    (rules : Rule.t Nj.RuleMap.t) : Rule.t Nj.RuleMap.t =
  add_reset_rules_aux ~with_scope_output_rule:"reset_with_scope_and_output"
    ~without_scope_output_rule:"reset_without_scope_and_output" ~redirect:"-o"
    catala_exe_opts rules

(** [add_test_with_output_rules catala_exe_opts rules] adds ninja rules used to
    test files using an output flag into [rules] and returns it.*)
let add_test_with_output_rules
    (catala_exe_opts : string)
    (rules : Rule.t Nj.RuleMap.t) : Rule.t Nj.RuleMap.t =
  let test_common_cmd_exprs =
    Nj.Expr.
      [
        Var "catala_cmd";
        Var "tested_file";
        Var "extra_flags";
        Lit "--unstyled";
        Lit "-o";
        Var "tmp_file";
        Lit "; colordiff -u -b";
        Var "expected_output";
        Var "tmp_file";
      ]
  in
  add_test_rules_aux ~test_common_cmd_exprs
    ~with_scope_output_rule:"test_with_scope_and_output"
    ~without_scope_output_rule:"test_without_scope_and_output" catala_exe_opts
    rules

(** [ninja_start catala_exe] returns the inital [ninja] data structure with
    rules needed to reset and test files. *)
let ninja_start (catala_exe : string) (catala_opts : string) : ninja =
  let catala_exe_opts = catala_exe ^ " " ^ catala_opts in
  let run_and_display_final_message =
    Nj.Rule.make "run_and_display_final_message"
      ~command:Nj.Expr.(Seq [Lit ":"])
      ~description:
        Nj.Expr.(
          Seq [Lit "All tests"; Var "test_file_or_folder"; Lit "passed!"])
  in
  {
    rules =
      Nj.RuleMap.(
        empty
        |> add_reset_rules catala_exe_opts
        |> add_test_rules catala_exe_opts
        |> add_test_with_output_rules catala_exe_opts
        |> add_reset_with_output_rules catala_exe_opts
        |> add run_and_display_final_message.name run_and_display_final_message);
    builds = Nj.BuildMap.empty;
  }

(** [collect_all_ninja_build ninja tested_file catala_exe catala_opts reset_test_outputs]
    creates and returns all ninja build statements needed to test the
    [tested_file]. *)
let collect_all_ninja_build
    (ninja : ninja)
    (tested_file : string)
    (reset_test_outputs : bool) : (string * ninja) option =
  let expected_outputs = search_for_expected_outputs tested_file in
  if List.length expected_outputs = 0 then (
    Cli.debug_print "No expected outputs were found for test file %s"
      tested_file;
    None)
  else
    let ninja, test_names =
      List.fold_left
        (fun (ninja, test_names) expected_output ->
          let vars =
            [
              ( "catala_cmd",
                Nj.Expr.Lit
                  (Cli.catala_backend_option_to_string expected_output.backend)
              );
              "tested_file", Nj.Expr.Lit tested_file;
              ( "expected_output",
                Nj.Expr.Lit
                  (expected_output.output_dir
                 ^ expected_output.complete_filename) );
            ]
          and output_build_kind = if reset_test_outputs then "reset" else "test"
          and catala_backend =
            Cli.catala_backend_option_to_string expected_output.backend
          in

          let get_rule_infos ?(rule_postfix = "") :
              string option -> string * string * (string * Nj.Expr.t) list =
            function
            | Some scope ->
              ( Printf.sprintf "%s_%s_%s_%s" output_build_kind scope
                  catala_backend tested_file
                |> Nj.Build.unpath,
                output_build_kind ^ "_with_scope" ^ rule_postfix,
                ("scope", Nj.Expr.Lit scope) :: vars )
            | None ->
              ( Printf.sprintf "%s_%s_%s" output_build_kind catala_backend
                  tested_file
                |> Nj.Build.unpath,
                output_build_kind ^ "_without_scope" ^ rule_postfix,
                vars )
          in

          let ninja_add_new_rule
              (rule_output : string)
              (rule : string)
              (vars : (string * Nj.Expr.t) list)
              (ninja : ninja) : ninja =
            {
              ninja with
              builds =
                Nj.BuildMap.add rule_output
                  (Nj.Build.make_with_vars ~outputs:[Nj.Expr.Lit rule_output]
                     ~rule ~vars)
                  ninja.builds;
            }
          in

          match expected_output.backend with
          | Cli.Interpret | Cli.Proof | Cli.Typecheck | Cli.Dcalc
          | Cli.Scopelang | Cli.Scalc | Cli.Lcalc ->
            let rule_output, rule_name, rule_vars =
              get_rule_infos expected_output.scope
            in
            let rule_vars =
              match expected_output.backend with
              | Cli.Proof ->
                ("extra_flags", Nj.Expr.Lit "--disable_counterexamples")
                :: rule_vars
                (* Counterexamples can be different at each call because of the
                   randomness inside SMT solver, so we can't expect their value
                   to remain constant. Hence we disable the counterexamples when
                   testing the replication of failed proofs. *)
              | _ -> rule_vars
            in
            ( ninja_add_new_rule rule_output rule_name rule_vars ninja,
              test_names ^ " $\n  " ^ rule_output )
          | Cli.Python | Cli.OCaml | Cli.Latex | Cli.Html | Cli.Makefile ->
            let tmp_file = Filename.temp_file "clerk_" ("_" ^ catala_backend) in
            let rule_output, rule_name, rule_vars =
              get_rule_infos ~rule_postfix:"_and_output" expected_output.scope
            in
            let rule_vars = ("tmp_file", Nj.Expr.Lit tmp_file) :: rule_vars in
            ( ninja_add_new_rule rule_output rule_name rule_vars ninja,
              test_names ^ " $\n  " ^ rule_output ))
        (ninja, "") expected_outputs
    in
    let test_name =
      tested_file
      |> (if reset_test_outputs then Printf.sprintf "reset_file_%s"
         else Printf.sprintf "test_file_%s")
      |> Nj.Build.unpath
    in
    Some
      ( test_name,
        {
          ninja with
          builds =
            Nj.BuildMap.add test_name
              (Nj.Build.make_with_inputs ~outputs:[Nj.Expr.Lit test_name]
                 ~rule:"phony" ~inputs:[Nj.Expr.Lit test_names])
              ninja.builds;
        } )

(** [add_root_test_build ninja all_file_names all_test_builds] add the 'test'
    ninja build declaration calling the rule 'run_and_display_final_message' for
    [all_test_builds] which correspond to [all_file_names]. *)
let add_root_test_build
    (ninja : ninja)
    (all_file_names : string list)
    (all_test_builds : string) : ninja =
  let file_names_str =
    List.hd all_file_names ^ ""
    ^ List.fold_left
        (fun acc name -> acc ^ "; " ^ name)
        "" (List.tl all_file_names)
  in
  {
    ninja with
    builds =
      Nj.BuildMap.add "test"
        (Nj.Build.make_with_vars_and_inputs ~outputs:[Nj.Expr.Lit "test"]
           ~rule:"run_and_display_final_message"
           ~inputs:[Nj.Expr.Lit all_test_builds]
           ~vars:
             [
               ( "test_file_or_folder",
                 Nj.Expr.Lit ("in [ " ^ file_names_str ^ " ]") );
             ])
        ninja.builds;
  }

(**{1 Running}*)

let run_file
    (file : string)
    (catala_exe : string)
    (catala_opts : string)
    (scope : string) : int =
  let command =
    String.concat " "
      (List.filter
         (fun s -> s <> "")
         [catala_exe; catala_opts; "-s " ^ scope; "Interpret"; file])
  in
  Cli.debug_print "Running: %s" command;
  Sys.command command

(** {1 Driver} *)

let get_catala_files_in_folder (dir : string) : string list =
  let rec loop result = function
    | f :: fs ->
      let f_is_dir =
        try Sys.is_directory f
        with Sys_error e ->
          Cli.warning_print "skipping %s" e;
          false
      in
      if f_is_dir then
        readdir_sort f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
      else loop (f :: result) fs
    | [] -> result
  in
  let all_files_in_folder = loop [] [dir] in
  List.filter (Re.Pcre.pmatch ~rex:catala_suffix_regex) all_files_in_folder

type ninja_building_context = {
  last_valid_ninja : ninja;
  curr_ninja : ninja option;
  all_file_names : string list;
  all_test_builds : string;
  all_failed_names : string list;
}
(** Record used to keep tracks of the current context while building the
    [Ninja_utils.ninja].*)

(** [ninja_building_context_init ninja_init] returns the empty context
    corresponding to [ninja_init]. *)
let ninja_building_context_init (ninja_init : Nj.ninja) : ninja_building_context
    =
  {
    last_valid_ninja = ninja_init;
    curr_ninja = Some ninja_init;
    all_file_names = [];
    all_test_builds = "";
    all_failed_names = [];
  }

(** [collect_in_directory ctx file_or_folder ninja_start reset_test_outputs]
    updates the building context [ctx] by adding new ninja build statements
    needed to test files in [folder].*)
let collect_in_folder
    (ctx : ninja_building_context)
    (folder : string)
    (ninja_start : Nj.ninja)
    (reset_test_outputs : bool) : ninja_building_context =
  let ninja, test_file_names =
    List.fold_left
      (fun (ninja, test_file_names) file ->
        match collect_all_ninja_build ninja file reset_test_outputs with
        | None ->
          (* Skips none Catala file. *)
          ninja, test_file_names
        | Some (test_file_name, ninja) ->
          ninja, test_file_names ^ " $\n  " ^ test_file_name)
      (ninja_start, "")
      (get_catala_files_in_folder folder)
  in
  let test_dir_name =
    Printf.sprintf "test_dir_%s" (folder |> Nj.Build.unpath)
  in
  let curr_ninja =
    if 0 = String.length test_file_names then None
    else
      Some
        {
          ninja with
          builds =
            Nj.BuildMap.add test_dir_name
              (Nj.Build.make_with_vars_and_inputs
                 ~outputs:[Nj.Expr.Lit test_dir_name]
                 ~rule:"run_and_display_final_message"
                 ~inputs:[Nj.Expr.Lit test_file_names]
                 ~vars:
                   [
                     ( "test_file_or_folder",
                       Nj.Expr.Lit ("in folder '" ^ folder ^ "'") );
                   ])
              ninja.builds;
        }
  in
  if Option.is_some curr_ninja then
    {
      ctx with
      last_valid_ninja = ninja_start;
      curr_ninja;
      all_file_names = folder :: ctx.all_file_names;
      all_test_builds = ctx.all_test_builds ^ " $\n  " ^ test_dir_name;
    }
  else
    {
      ctx with
      last_valid_ninja = ninja_start;
      curr_ninja;
      all_failed_names = folder :: ctx.all_failed_names;
    }

(** [collect_in_file ctx file_or_folder ninja_start reset_test_outputs] updates
    the building context [ctx] by adding new ninja build statements needed to
    test the [tested_file].*)
let collect_in_file
    (ctx : ninja_building_context)
    (tested_file : string)
    (ninja_start : Nj.ninja)
    (reset_test_outputs : bool) : ninja_building_context =
  match collect_all_ninja_build ninja_start tested_file reset_test_outputs with
  | Some (test_file_name, ninja) ->
    {
      ctx with
      last_valid_ninja = ninja;
      curr_ninja = Some ninja;
      all_file_names = tested_file :: ctx.all_file_names;
      all_test_builds = ctx.all_test_builds ^ " $\n  " ^ test_file_name;
    }
  | None ->
    {
      ctx with
      last_valid_ninja = ninja_start;
      curr_ninja = None;
      all_failed_names = tested_file :: ctx.all_failed_names;
    }

(** {1 Return code values} *)

let return_ok = 0
let return_err = 1

(** {1 Driver} *)

(** [add_root_test_build ctx files_or_folders reset_test_outputs] updates the
    [ctx] by adding ninja build statements needed to test or
    [reset_test_outputs] [files_or_folders]. *)
let add_test_builds
    (ctx : ninja_building_context)
    (files_or_folders : string list)
    (reset_test_outputs : bool) : ninja_building_context =
  files_or_folders
  |> List.fold_left
       (fun ctx file_or_folder ->
         let curr_ninja =
           match ctx.curr_ninja with
           | Some ninja -> ninja
           | None -> ctx.last_valid_ninja
         in
         if Sys.is_directory file_or_folder then
           collect_in_folder ctx file_or_folder curr_ninja reset_test_outputs
         else collect_in_file ctx file_or_folder curr_ninja reset_test_outputs)
       ctx

let makeflags_to_ninja_flags (makeflags : string option) =
  match makeflags with
  | None -> ""
  | Some makeflags ->
    let ignore_rex = Re.(compile @@ word (char 'i')) in
    let has_ignore = Re.execp ignore_rex makeflags in
    let jobs_rex = Re.(compile @@ seq [str "-j"; group (rep digit)]) in
    let number_of_jobs =
      try int_of_string (Re.Group.get (Re.exec jobs_rex makeflags) 1)
      with _ -> 0
    in
    String.concat " "
      [(if has_ignore then "-k0" else ""); "-j" ^ string_of_int number_of_jobs]

let driver
    (files_or_folders : string list)
    (command : string)
    (catala_exe : string option)
    (catala_opts : string option)
    (makeflags : string option)
    (debug : bool)
    (scope : string option)
    (reset_test_outputs : bool)
    (ninja_output : string option) : int =
  if debug then Cli.debug_flag := true;
  let ninja_flags = makeflags_to_ninja_flags makeflags in
  let files_or_folders = List.sort_uniq String.compare files_or_folders
  and catala_exe = Option.fold ~none:"catala" ~some:Fun.id catala_exe
  and catala_opts = Option.fold ~none:"" ~some:Fun.id catala_opts
  and ninja_output =
    Option.fold
      ~none:(Filename.temp_file "clerk_build" ".ninja")
      ~some:Fun.id ninja_output
  in
  match String.lowercase_ascii command with
  | "test" -> (
    Cli.debug_print "building ninja rules...";
    let ctx =
      add_test_builds
        (ninja_building_context_init (ninja_start catala_exe catala_opts))
        files_or_folders reset_test_outputs
    in
    let there_is_some_fails = 0 <> List.length ctx.all_failed_names in
    let ninja =
      match ctx.curr_ninja with
      | Some ninja -> ninja
      | None -> ctx.last_valid_ninja
    in
    if there_is_some_fails then
      List.iter
        (fun f ->
          f
          |> Cli.with_style [ANSITerminal.magenta] "%s"
          |> Cli.warning_print "No test case found for %s")
        ctx.all_failed_names;
    if 0 = List.compare_lengths ctx.all_failed_names files_or_folders then
      return_ok
    else
      try
        File.with_formatter_of_file ninja_output (fun fmt ->
            Cli.debug_print "writing %s..." ninja_output;
            Nj.format fmt
              (add_root_test_build ninja ctx.all_file_names ctx.all_test_builds));
        let ninja_cmd = "ninja " ^ ninja_flags ^ " test -f " ^ ninja_output in
        Cli.debug_print "executing '%s'..." ninja_cmd;
        Sys.command ninja_cmd
      with Sys_error e ->
        Cli.error_print "can not write in %s" e;
        return_err)
  | "run" -> (
    match scope with
    | Some scope ->
      let res =
        List.fold_left
          (fun ret f -> ret + run_file f catala_exe catala_opts scope)
          0 files_or_folders
      in
      if 0 <> res then return_err else return_ok
    | None ->
      Cli.error_print "Please provide a scope to run with the -s option";
      return_err)
  | _ ->
    Cli.error_print "The command \"%s\" is unknown to clerk." command;
    return_err

let main () = exit (Cmdliner.Cmd.eval' (Cmdliner.Cmd.v info (clerk_t driver)))
