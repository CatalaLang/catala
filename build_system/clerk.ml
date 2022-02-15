(* This file is part of the Catala build system, a specification language for tax and social
   benefits computation rules. Copyright (C) 2020 Inria, contributors: Denis Merigoux
   <denis.merigoux@inria.fr>, Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Cmdliner
open Utils
open Ninja_utils
module Nj = Ninja_utils

(** {1 Command line interface} *)

let file_or_folder =
  Arg.(required & pos 1 (some file) None & info [] ~docv:"FILE(S)" ~doc:"File or folder to process")

let command =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"COMMAND" ~doc:"Command selection among: test, run")

let debug = Arg.(value & flag & info [ "debug"; "d" ] ~doc:"Prints debug information")

let reset_test_outputs =
  Arg.(
    value & flag
    & info [ "r"; "reset" ]
        ~doc:
          "Used with the `test` command, resets the test output to whatever is output by the \
           Catala compiler.")

let catalac =
  Arg.(
    value
    & opt (some string) None
    & info [ "e"; "exe" ] ~docv:"EXE" ~doc:"Catala compiler executable, defaults to `catala`")

let scope =
  Arg.(
    value
    & opt (some string) None
    & info [ "s"; "scope" ] ~docv:"SCOPE"
        ~doc:"Used with the `run` command, selects which scope of a given Catala file to run.")

let catala_opts =
  Arg.(
    value
    & opt (some string) None
    & info [ "c"; "catala-opts" ] ~docv:"LANG" ~doc:"Options to pass to the Catala compiler")

let clerk_t f =
  Term.(
    const f $ file_or_folder $ command $ catalac $ catala_opts $ debug $ scope $ reset_test_outputs)

let version = "0.5.0"

let info =
  let doc =
    "Build system for Catala, a specification language for tax and social benefits computation \
     rules."
  in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(b,clerk) is a build system for Catala, a specification language for tax and social \
         benefits computation rules";
      `S Manpage.s_commands;
      `I
        ( "test",
          "Tests a Catala source file given expected outputs provided in a directory called \
           `output` at the same level that the tested file. If the tested file is `foo.catala_en`, \
           then `output` should contain expected output files like `foo.catala_en.$(i,BACKEND)` \
           where  $(i,BACKEND) is chosen among: `Interpret`, `Dcalc`, `Scopelang`, `html`, `tex`, \
           `py`, `ml` and `d` (for Makefile dependencies). For the `Interpret` backend, the scope \
           to test is selected by naming the expected output file \
           `foo.catala_en.$(i,SCOPE).interpret`. When the argument of $(b,clerk) is a folder, it \
           recursively looks for Catala files coupled with `output` directories and matching \
           expected output on which to perform tests." );
      `I
        ("run", "Runs the Catala interpreter on a given scope of a given file. See the `-s` option.");
      `S Manpage.s_authors;
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `S Manpage.s_examples;
      `P "Typical usage:";
      `Pre "clerk test file.catala_en";
      `S Manpage.s_bugs;
      `P "Please file bug reports at https://github.com/CatalaLang/catala/issues";
    ]
  in
  let exits = Cmd.Exit.defaults @ [ Cmd.Exit.info ~doc:"on error." 1 ] in
  Cmd.info "clerk" ~version ~doc ~exits ~man

(**{1 Testing}*)

let catala_backend_to_string (backend : Cli.backend_option) : string =
  match backend with
  | Cli.Interpret -> "Interpret"
  | Cli.Makefile -> "Makefile"
  | Cli.OCaml -> "OCaml"
  | Cli.Scopelang -> "Scopelang"
  | Cli.Dcalc -> "Dcalc"
  | Cli.Latex -> "Latex"
  | Cli.Proof -> "Proof"
  | Cli.Html -> "Html"
  | Cli.Python -> "Python"
  | Cli.Typecheck -> "Typecheck"
  | Cli.Scalc -> "Scalc"
  | Cli.Lcalc -> "Lcalc"

type expected_output_descr = {
  base_filename : string;
  output_dir : string;
  complete_filename : string;
  backend : Cli.backend_option;
  scope : string option;
}

let catala_suffix_regex = Re.Pcre.regexp "\\.catala_(\\w){2}"

let filename_to_expected_output_descr (output_dir : string) (filename : string) :
    expected_output_descr option =
  let complete_filename = filename in
  let first_extension = Filename.extension filename in
  let filename = Filename.remove_extension filename in
  let backend =
    match String.lowercase_ascii first_extension with
    | ".interpret" -> Some Cli.Interpret
    | ".d" -> Some Cli.Makefile
    | ".ml" -> Some Cli.OCaml
    | ".scopelang" -> Some Cli.Scopelang
    | ".dcalc" -> Some Cli.Dcalc
    | ".tex" -> Some Cli.Latex
    | ".html" -> Some Cli.Html
    | ".py" -> Some Cli.Python
    | ".proof" -> Some Cli.Proof
    | ".typecheck" -> Some Cli.Typecheck
    | _ -> None
  in
  match backend with
  | None -> None
  | Some backend ->
      let second_extension = Filename.extension filename in
      let base_filename, scope =
        if Re.Pcre.pmatch ~rex:catala_suffix_regex second_extension then (filename, None)
        else
          let scope_name_regex = Re.Pcre.regexp "\\.(.+)" in
          let scope_name = (Re.Pcre.extract ~rex:scope_name_regex second_extension).(1) in
          (Filename.remove_extension filename, Some scope_name)
      in
      Some { output_dir; complete_filename; base_filename; backend; scope }

(** Given a file, looks in the relative [output] directory if there are files with the same base
    name that contain expected outputs for different *)
let search_for_expected_outputs (file : string) : expected_output_descr list =
  let output_dir = Filename.dirname file ^ Filename.dir_sep ^ "output/" in
  let output_files = try Sys.readdir output_dir with Sys_error _ -> Array.make 0 "" in
  List.filter_map
    (fun output_file ->
      match filename_to_expected_output_descr output_dir output_file with
      | None -> None
      | Some expected_output ->
          if expected_output.base_filename = Filename.basename file then Some expected_output
          else None)
    (Array.to_list output_files)

type testing_result = { error_code : int; number_of_tests_run : int; number_correct : int }

(*         let command = *)
(*           String.concat " " *)
(*             (List.filter (fun s -> s <> "") reproducible_catala_command *)
(*             @ (match expected_output.backend with *)
(*               | Cli.Proof -> *)
(*             @ *)
(*             match expected_output.backend with *)
(*             | Cli.Interpret | Cli.Proof | Cli.Typecheck -> *)
(*                 if reset_test_outputs then *)
(*                   [ *)
(*                     ">"; *)
(*                     Format.asprintf "%s%s" expected_output.output_dir *)
(*                       expected_output.complete_filename; *)
(*                     "2>&1 "; *)
(*                   ] *)
(*                 else *)
(*                   [ *)
(*                     "2>&1 "; *)
(*                     "|"; *)
(*                     Format.asprintf "colordiff -u -b %s%s -" expected_output.output_dir *)
(*                       expected_output.complete_filename; *)
(*                   ] *)
(*             | Cli.Python | Cli.OCaml | Cli.Dcalc | Cli.Scopelang | Cli.Latex | Cli.Html *)
(*             | Cli.Makefile -> *)
(* (* for those backends, the output of the Catala compiler will be written in a *) (* temporary
   file which later we're going to diff with the *) *)
(*                 if reset_test_outputs then *)
(*                   [ *)
(*                     "-o"; *)
(*                     Format.asprintf "%s%s" expected_output.output_dir *)
(*                       expected_output.complete_filename; *)
(*                   ] *)
(*                 else *)
(*                   let temp_file = *)
(*                     Filename.temp_file "clerk_" *)
(*                       ("_" ^ catala_backend_to_string expected_output.backend) *)
(*                   in *)
(*                   [ *)
(*                     "-o"; *)
(*                     temp_file; *)
(*                     ";"; *)
(*                     Format.asprintf "colordiff -u -b %s%s %s" expected_output.output_dir *)
(*                       expected_output.complete_filename temp_file; *)
(*                   ]) *)
(*         in *)

(** [add_reset_rules catala_exe_opts rules] adds ninja rules used to reset test files into [rules]
    and returns it.*)
let add_reset_rules (catala_exe_opts : string) (rules : Rule.t Nj.RuleMap.t) : Rule.t Nj.RuleMap.t =
  let reset_common_cmd_exprs =
    Nj.Expr.
      [
        Var "catala_cmd";
        Var "tested_file";
        Var "extra_flags";
        Lit "--unstyled";
        Lit ">";
        Var "expected_output";
        Lit "2>&1";
      ]
  in
  let reset_with_scope_rule =
    Nj.Rule.make "reset_with_scope"
      ~command:
        Nj.Expr.(Seq ([ Lit catala_exe_opts; Lit "-s"; Var "scope" ] @ reset_common_cmd_exprs))
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
    Nj.Rule.make "reset_without_scope"
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

(** [add_test_rules catala_exe_opts rules] adds ninja rules used to test files into [rules] and
    returns it.*)
let add_test_rules (catala_exe_opts : string) (rules : Rule.t Nj.RuleMap.t) : Rule.t Nj.RuleMap.t =
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
  let test_with_scope_rule =
    Nj.Rule.make "test_with_scope"
      ~command:
        Nj.Expr.(Seq ([ Lit catala_exe_opts; Lit "-s"; Var "scope" ] @ test_common_cmd_exprs))
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
    Nj.Rule.make "test_without_scope"
      ~command:Nj.Expr.(Seq (Lit catala_exe_opts :: test_common_cmd_exprs))
      ~description:
        Nj.Expr.(
          Seq
            [
              Lit "TEST on file"; Var "tested_file"; Lit "with the"; Var "catala_cmd"; Lit "command";
            ])
  in
  Nj.RuleMap.(
    rules
    |> add test_with_scope_rule.name test_with_scope_rule
    |> add test_without_scope_rule.name test_without_scope_rule)

(** [add_reset_with_ouput_rules catala_exe_opts rules] adds ninja rules used to reset test files
    using an output flag into [rules] and returns it.

    TODO: to factorize with add_reset_rules, only Lit '-o' is changing. *)
let add_reset_with_output_rules (catala_exe_opts : string) (rules : Rule.t Nj.RuleMap.t) :
    Rule.t Nj.RuleMap.t =
  let reset_common_cmd_exprs =
    Nj.Expr.
      [
        Var "catala_cmd";
        Var "tested_file";
        Var "extra_flags";
        Lit "--unstyled";
        Lit "-o";
        Var "expected_output";
        Lit "2>&1";
      ]
  in
  let reset_with_scope_and_output_rule =
    Nj.Rule.make "reset_with_scope_and_output"
      ~command:
        Nj.Expr.(Seq ([ Lit catala_exe_opts; Lit "-s"; Var "scope" ] @ reset_common_cmd_exprs))
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
  let reset_without_scope_and_output_rule =
    Nj.Rule.make "reset_without_scope_and_output"
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
    |> add reset_with_scope_and_output_rule.name reset_with_scope_and_output_rule
    |> add reset_without_scope_and_output_rule.name reset_without_scope_and_output_rule)

(** [add_test_with_output_rules catala_exe_opts rules] adds ninja rules used to test files using an
    output flag into [rules] and returns it.*)
let add_test_with_output_rules (catala_exe_opts : string) (rules : Rule.t Nj.RuleMap.t) :
    Rule.t Nj.RuleMap.t =
  (* catala Ocaml build_system/tests/test_array/good/aggregation_2.catala_en --unstyled -o
     /tmp/clerk_7a8ebd_Ocaml ; colordiff -u -b
     build_system/tests/test_array/good/output/aggregation_2.catala_en.ml /tmp/clerk_7a8ebd_Ocaml*)
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
  Cli.debug_print (Printf.sprintf "==== catala_exe_opts = %s ========" catala_exe_opts);
  let test_with_scope_and_output_rule =
    Nj.Rule.make "test_with_scope_and_output"
      ~command:
        Nj.Expr.(Seq ([ Lit catala_exe_opts; Lit "-s"; Var "scope" ] @ test_common_cmd_exprs))
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
  let test_without_scope_and_output_rule =
    Nj.Rule.make "test_without_scope_and_output"
      ~command:Nj.Expr.(Seq (Lit catala_exe_opts :: test_common_cmd_exprs))
      ~description:
        Nj.Expr.(
          Seq
            [
              Lit "TEST on file"; Var "tested_file"; Lit "with the"; Var "catala_cmd"; Lit "command";
            ])
  in
  Nj.RuleMap.(
    rules
    |> add test_with_scope_and_output_rule.name test_with_scope_and_output_rule
    |> add test_without_scope_and_output_rule.name test_without_scope_and_output_rule)

(** [ninja_start catala_exe] returns the inital [ninja] data structure with rules needed to reset
    and test files. *)
let ninja_start (catala_exe : string) (catala_opts : string) : ninja =
  let catala_exe_opts = catala_exe ^ " " ^ catala_opts in
  let run_and_display_final_message =
    Nj.Rule.make "run_and_display_final_message"
      ~command:Nj.Expr.(Seq [ Lit ":" ])
      ~description:Nj.Expr.(Seq [ Lit "All tests"; Var "test_file_or_folder"; Lit "passed!" ])
  in
  {
    rules =
      Nj.RuleMap.(
        empty |> add_reset_rules catala_exe_opts |> add_test_rules catala_exe_opts
        |> add_test_with_output_rules catala_exe_opts
        |> add_reset_with_output_rules catala_exe_opts
        |> add run_and_display_final_message.name run_and_display_final_message);
    builds = Nj.BuildMap.empty;
  }

(** [collect_all_ninja_build ninja tested_file catala_exe catala_opts reset_test_outputs] creates
    and returns all ninja build declarations needed to test the [tested_file]. *)
let collect_all_ninja_build (ninja : ninja) (tested_file : string) (reset_test_outputs : bool) :
    (string * ninja) option =
  let expected_outputs = search_for_expected_outputs tested_file in
  if List.length expected_outputs = 0 then (
    Cli.debug_print (Format.asprintf "No expected outputs were found for test file %s" tested_file);
    None)
  else
    let ninja, test_names =
      List.fold_left
        (fun (ninja, test_names) expected_output ->
          match expected_output.backend with
          | Cli.Interpret | Cli.Proof | Cli.Typecheck | Cli.Dcalc | Cli.Scopelang ->
              let vars =
                [
                  ("catala_cmd", Nj.Expr.Lit (catala_backend_to_string expected_output.backend));
                  ("tested_file", Nj.Expr.Lit tested_file);
                  ( "expected_output",
                    Nj.Expr.Lit (expected_output.output_dir ^ expected_output.complete_filename) );
                ]
              in
              let output_build_kind = if reset_test_outputs then "reset" else "test" in
              let test_name, rule, vars =
                match expected_output.scope with
                | Some scope ->
                    ( Printf.sprintf "%s_%s_%s" output_build_kind scope tested_file
                      |> Nj.Build.unpath,
                      output_build_kind ^ "_with_scope",
                      ("scope", Nj.Expr.Lit scope) :: vars )
                | None ->
                    ( Printf.sprintf "%s_%s_%s" output_build_kind
                        (catala_backend_to_string expected_output.backend)
                        tested_file
                      |> Nj.Build.unpath,
                      output_build_kind ^ "_without_scope",
                      vars )
              in
              let vars =
                match expected_output.backend with
                | Cli.Proof ->
                    ("extra_flags", Nj.Expr.Lit "--disable_counterexamples") :: vars
                    (* Counterexamples can be different at each call because of the randomness
                       inside SMT solver, so we can't expect their value to remain constant. Hence
                       we disable the counterexamples when testing the replication of failed
                       proofs. *)
                | _ -> vars
              in
              ( {
                  ninja with
                  builds =
                    Nj.BuildMap.add test_name
                      (Nj.Build.make_with_vars ~outputs:[ Nj.Expr.Lit test_name ] ~rule ~vars)
                      ninja.builds;
                },
                test_names ^ " $\n  " ^ test_name )
          | Cli.Python | Cli.OCaml | Cli.Latex | Cli.Html | Cli.Makefile ->
              let tmp_file =
                Filename.temp_file "clerk_" ("_" ^ catala_backend_to_string expected_output.backend)
              in
              let vars =
                [
                  ("catala_cmd", Nj.Expr.Lit (catala_backend_to_string expected_output.backend));
                  ("tested_file", Nj.Expr.Lit tested_file);
                  ( "expected_output",
                    Nj.Expr.Lit (expected_output.output_dir ^ expected_output.complete_filename) );
                  ("tmp_file", Nj.Expr.Lit tmp_file);
                ]
              in
              let output_build_kind = if reset_test_outputs then "reset" else "test" in
              let test_name, rule, vars =
                match expected_output.scope with
                | Some scope ->
                    ( Printf.sprintf "%s_%s_%s" output_build_kind scope tested_file
                      |> Nj.Build.unpath,
                      output_build_kind ^ "_with_scope_and_output",
                      ("scope", Nj.Expr.Lit scope) :: vars )
                | None ->
                    ( Printf.sprintf "%s_%s_%s" output_build_kind
                        (catala_backend_to_string expected_output.backend)
                        tested_file
                      |> Nj.Build.unpath,
                      output_build_kind ^ "_without_scope_and_output",
                      vars )
              in
              ( {
                  ninja with
                  builds =
                    Nj.BuildMap.add test_name
                      (Nj.Build.make_with_vars ~outputs:[ Nj.Expr.Lit test_name ] ~rule ~vars)
                      ninja.builds;
                },
                test_names ^ " $\n  " ^ test_name ))
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
              (Nj.Build.make_with_inputs ~outputs:[ Nj.Expr.Lit test_name ] ~rule:"phony"
                 ~inputs:[ Nj.Expr.Lit test_names ])
              ninja.builds;
        } )

(** [add_root_test_build ninja re_test_file_or_dir test_file_or_dir] collects all build outputs
    matching the [re_test_file_or_dir] regexp and concates them into the 'test' build declaration --
    the root of the build declarations. The [test_file_or_dir] parameter is only used for
    pretty-printing purposes. *)
let add_root_test_build (ninja : ninja) (re_test_file_or_dir : Re.Pcre.regexp)
    (test_file_or_dir_msg : string) : ninja =
  let all_test_files =
    Nj.BuildMap.bindings ninja.builds
    |> List.filter_map (fun (name, _) ->
           let len =
             try Array.length (Re.Pcre.(extract ~rex:re_test_file_or_dir) name) with _ -> 0
           in
           if 0 < len then Some name else None)
    |> String.concat " $\n"
  in
  {
    ninja with
    builds =
      Nj.BuildMap.add "test"
        (Nj.Build.make_with_vars_and_inputs ~outputs:[ Nj.Expr.Lit "test" ]
           ~rule:"run_and_display_final_message" ~inputs:[ Nj.Expr.Lit all_test_files ]
           ~vars:[ ("test_file_or_folder", Nj.Expr.Lit test_file_or_dir_msg) ])
        ninja.builds;
  }

(**{1 Running}*)

let run_file (file : string) (catala_exe : string) (catala_opts : string) (scope : string) : int =
  let command =
    String.concat " "
      (List.filter (fun s -> s <> "") [ catala_exe; catala_opts; "-s " ^ scope; "Interpret"; file ])
  in
  Cli.debug_print ("Running: " ^ command);
  Sys.command command

(** {1 Driver} *)

let get_catala_files_in_folder (dir : string) : string list =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        (* TODO: needs to be refactored if we want the build.ninja file structured with
           directories. *)
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  let all_files_in_folder = loop [] [ dir ] in
  List.filter (Re.Pcre.pmatch ~rex:catala_suffix_regex) all_files_in_folder

let driver (file_or_folder : string) (command : string) (catala_exe : string option)
    (catala_opts : string option) (debug : bool) (scope : string option) (reset_test_outputs : bool)
    : int =
  if debug then Cli.debug_flag := true;
  let catala_exe = Option.fold ~none:"catala" ~some:Fun.id catala_exe in
  let catala_opts = Option.fold ~none:"" ~some:Fun.id catala_opts in
  match String.lowercase_ascii command with
  | "test" -> (
      let ninja_opt, re_test_file_or_dir =
        if Sys.is_directory file_or_folder then
          let ninja, test_file_names =
            List.fold_left
              (fun (ninja, test_file_names) file ->
                match collect_all_ninja_build ninja file reset_test_outputs with
                | None ->
                    (* Skips none Catala file. *)
                    (ninja, test_file_names)
                | Some (test_file_name, ninja) ->
                    (ninja, test_file_names ^ " $\n  " ^ test_file_name))
              (ninja_start catala_exe catala_opts, "")
              (get_catala_files_in_folder file_or_folder)
          in
          let test_dir_name = Printf.sprintf "test_dir_%s" (file_or_folder |> Nj.Build.unpath) in
          ( Some
              {
                ninja with
                builds =
                  Nj.BuildMap.add test_dir_name
                    (Nj.Build.make_with_inputs ~outputs:[ Nj.Expr.Lit test_dir_name ] ~rule:"phony"
                       ~inputs:[ Nj.Expr.Lit test_file_names ])
                    ninja.builds;
              },
            Re.Pcre.regexp "^(test|reset)_dir_" )
        else (
          Cli.debug_print "building ninja rules...";
          let ninja_opt =
            Option.map
              (fun (_test_file_name, ninja) -> ninja)
              (collect_all_ninja_build
                 (ninja_start catala_exe catala_opts)
                 file_or_folder reset_test_outputs)
          in
          (ninja_opt, Re.Pcre.regexp "^(test|reset)_file_"))
      in
      match ninja_opt with
      | Some ninja ->
          let out = open_out "build.ninja" in
          Cli.debug_print "writing build.ninja...";
          let ninja =
            add_root_test_build ninja re_test_file_or_dir
              (if Sys.is_directory file_or_folder then "in folder \"" ^ file_or_folder ^ "\""
              else "for file \"" ^ file_or_folder ^ "\"")
          in
          Nj.write out ninja;
          close_out out;
          Cli.debug_print "executing 'ninja test'...";
          Sys.command "ninja test"
      | None -> -1)
  | "run" -> (
      match scope with
      | Some scope -> run_file file_or_folder catala_exe catala_opts scope
      | None ->
          Cli.error_print "Please provide a scope to run with the -s option";
          1)
  | _ ->
      Cli.error_print (Format.asprintf "The command \"%s\" is unknown to clerk." command);
      1

let _ =
  let return_code = Cmdliner.Cmd.eval' (Cmdliner.Cmd.v info (clerk_t driver)) in
  exit return_code
