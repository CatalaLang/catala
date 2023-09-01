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
open Catala_utils
open Ninja_utils
module Nj = Ninja_utils

(** {1 Command line interface} *)

let files_or_folders =
  Arg.(
    non_empty
    & pos_right 0 file []
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
    value
    & flag
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
    & info ["o"; "output"] ~docv:"FILE"
        ~doc:
          "$(i,FILE) is the file that will contain the build.ninja file \
           output. If not specified, the build.ninja file will be output in \
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
    const f
    $ files_or_folders
    $ command
    $ catalac
    $ catala_opts
    $ makeflags
    $ debug
    $ scope
    $ reset_test_outputs
    $ ninja_output)

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
      (* "runtest" is for internal use and not documented here *)
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
  tested_filename : string;  (** Name of the file that's being tested *)
  output_dir : string;
      (** Name of the output directory where all expected outputs are stored *)
  id : string;
      (** Id of this precise unit test that will be associated to an expected
          output *)
  cmd : string;
      (** Catala command to launch to run the test, excluding "catala" at the
          begin, and the name of the file to test *)
}

let catala_suffix_regex = Re.Pcre.regexp "\\.catala_(\\w){2}$"

(** [readdir_sort dirname] returns the sorted subdirectories of [dirname] in an
    array or an empty array if the [dirname] doesn't exist. *)
let readdir_sort (dirname : string) : string array =
  try
    let dirs = Sys.readdir dirname in
    Array.fast_sort String.compare dirs;
    dirs
  with Sys_error _ -> [||]

(** Given a file, looks in the relative [output] directory if there are files
    with the same base name that contain expected outputs for different *)
let search_for_expected_outputs (file : string) : expected_output_descr list =
  let output_dir = Filename.dirname file ^ Filename.dir_sep ^ "output/" in
  File.with_in_channel file (fun ic ->
    (* Matches something of the form: {v
    ```catala-test { id="foo" }
    catala Interpret -s A
    ```
    v} *)
      let test_rex =
        Re.compile (Re.(
          seq
            [
              bol;
              str "```catala-test";
              opt
              @@ seq
                   [
                     rep space;
                     char '{';
                     rep space;
                     str "id";
                     rep space;
                     char '=';
                     rep space;
                     char '"';
                     group (rep1 (diff any (char '"')));
                     char '"';
                     rep space;
                     char '}';
                   ];
              rep space;
              char '\n';
              seq [str "catala"; rep space; group (rep1 notnl)];
            ]))
      in
      let file_str = really_input_string ic (in_channel_length ic) in
      let test_declarations = Re.all test_rex file_str in
      List.map
        (fun groups ->
          let id =
            match Re.Group.get_opt groups 1 with
            | Some x -> x
            | None ->
              Message.raise_error
                "A test declaration is missing its identifier in the file %s"
                file
          in
          let cmd = Re.Group.get groups 2 in
          { tested_filename = file; output_dir; cmd; id })
        test_declarations)
        [@ocamlformat "disable"]

(** Var references used in the Clerk file *)
module Var = struct
  let tested_file = Nj.Expr.Var "tested_file"
  let catala_cmd = Nj.Expr.Var "catala_cmd"
  let expected_output = Nj.Expr.Var "expected_output"
  let test_file_or_folder = Nj.Expr.Var "test_file_or_folder"

  let name = function
    | Nj.Expr.Var n -> n
    | _ -> invalid_arg "Clerk_driver.Var.name"
end

let pipe_diff_cmd =
  let open Nj.Expr in
  let has_patdiff = Sys.command "type patdiff >/dev/null 2>&1" = 0 in
  if has_patdiff then
    Seq
      [
        Lit "|";
        Lit "patdiff";
        Seq [Lit "-alt-new"; Lit "current-output"];
        Var.tested_file;
        Lit "/dev/stdin";
      ]
  else Seq [Lit "| diff -u -b --color"; Var.tested_file; Lit "-"]

let inline_test_rule catala_exe catala_opts =
  let open Nj.Expr in
  Nj.Rule.make "inline_tests"
    ~command:
      (Seq
         [
           Lit Sys.argv.(0);
           Lit "runtest";
           Seq [Lit "--exe"; Lit catala_exe];
           Seq
             [
               Lit "--catala-opts";
               Lit ("\"" ^ String.escaped catala_opts ^ "\"");
             ];
           Var.tested_file;
           pipe_diff_cmd;
         ])
    ~description:(Seq [Lit "INLINE TESTS of file"; Var.tested_file])

let inline_reset_rule catala_exe catala_opts =
  let open Nj.Expr in
  Nj.Rule.make "inline_tests_reset"
    ~command:
      (Seq
         [
           Lit Sys.argv.(0);
           Lit "runtest";
           Lit ("--exe=" ^ catala_exe);
           Lit ("--catala-opts=" ^ catala_opts);
           Lit "--reset";
           Var.tested_file;
         ])
    ~description:(Seq [Lit "RESET INLINE TESTS of file"; Var.tested_file])

let add_reset_rules_aux
    ~(redirect : string)
    ~(rule_name : string)
    (catala_exe_opts : string)
    (rules : Rule.t Nj.RuleMap.t) : Rule.t Nj.RuleMap.t =
  let reset_common_cmd_exprs =
    Nj.Expr.
      [
        Var.catala_cmd;
        Var.tested_file;
        Lit redirect;
        Var.expected_output;
        Lit "2>&1";
        Lit "|| true";
      ]
  in
  let reset_rule =
    Nj.Rule.make rule_name
      ~command:Nj.Expr.(Seq (Lit catala_exe_opts :: reset_common_cmd_exprs))
      ~description:
        Nj.Expr.(
          Seq
            [
              Lit "RESET";
              Lit "file";
              Var.tested_file;
              Lit "with the";
              Var.catala_cmd;
              Lit "command";
            ])
  in
  Nj.RuleMap.(rules |> add reset_rule.name reset_rule)

let add_test_rules_aux
    ~(rule_name : string)
    (catala_exe_opts : string)
    (rules : Rule.t Nj.RuleMap.t) : Rule.t Nj.RuleMap.t =
  let test_rule =
    Nj.Rule.make rule_name
      ~command:
        Nj.Expr.(
          Seq
            (Lit catala_exe_opts
            :: [
                 Var.catala_cmd;
                 Var.tested_file;
                 Lit "2>&1 | colordiff -u -b";
                 Var.expected_output;
                 Lit "-";
               ]))
      ~description:
        Nj.Expr.(
          Seq
            [
              Lit "TEST on file";
              Var.tested_file;
              Lit "with the";
              Var.catala_cmd;
              Lit "command";
            ])
  in
  Nj.RuleMap.(rules |> add test_rule.name test_rule)

(** [add_reset_rules catala_exe_opts rules] adds ninja rules used to reset test
    files into [rules] and returns it.*)
let add_reset_rules (catala_exe_opts : string) (rules : Rule.t Nj.RuleMap.t) :
    Rule.t Nj.RuleMap.t =
  add_reset_rules_aux ~rule_name:"reset_rule" ~redirect:">" catala_exe_opts
    rules

(** [add_test_rules catala_exe_opts rules] adds ninja rules used to test files
    into [rules] and returns it.*)
let add_test_rules (catala_exe_opts : string) (rules : Rule.t Nj.RuleMap.t) :
    Rule.t Nj.RuleMap.t =
  add_test_rules_aux ~rule_name:"test_rule" catala_exe_opts rules

(** [ninja_start catala_exe] returns the inital [ninja] data structure with
    rules needed to reset and test files. *)
let ninja_start (catala_exe : string) (catala_opts : string) : ninja =
  let catala_exe_opts = catala_exe ^ " " ^ catala_opts in
  let add_rule r rules = Nj.RuleMap.(rules |> add r.Nj.Rule.name r) in
  let run_and_display_final_message =
    Nj.Rule.make "run_and_display_final_message"
      ~command:Nj.Expr.(Seq [Lit ":"])
      ~description:
        Nj.Expr.(Seq [Lit "All tests"; Var.test_file_or_folder; Lit "passed!"])
  in
  {
    rules =
      Nj.RuleMap.(
        empty
        |> add_reset_rules catala_exe_opts
        |> add_test_rules catala_exe_opts
        |> add_rule (inline_test_rule catala_exe catala_opts)
        |> add_rule (inline_reset_rule catala_exe catala_opts)
        |> add run_and_display_final_message.name run_and_display_final_message);
    builds = Nj.BuildMap.empty;
  }

let collect_inline_ninja_builds
    (ninja : ninja)
    (tested_file : string)
    (reset_test_outputs : bool) : (string * ninja) option =
  if not (Clerk_runtest.has_inline_tests tested_file) then None
  else
    let ninja =
      let vars = [Var.(name tested_file), Nj.Expr.Lit tested_file] in
      let rule_to_call =
        if reset_test_outputs then "inline_tests_reset" else "inline_tests"
      in
      let rule_output = tested_file ^ ".out" in
      {
        ninja with
        builds =
          Nj.BuildMap.add rule_output
            (Nj.Build.make_with_vars ~outputs:[Nj.Expr.Lit rule_output]
               ~rule:rule_to_call ~vars)
            ninja.builds;
      }
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
                 ~rule:"phony"
                 ~inputs:[Nj.Expr.Lit (tested_file ^ ".out")])
              ninja.builds;
        } )

(** [collect_all_ninja_build ninja tested_file catala_exe catala_opts reset_test_outputs]
    creates and returns all ninja build statements needed to test the
    [tested_file]. *)
let collect_all_ninja_build
    (ninja : ninja)
    (tested_file : string)
    (reset_test_outputs : bool) : (string * ninja) option =
  let expected_outputs = search_for_expected_outputs tested_file in
  if expected_outputs = [] then (
    Message.emit_debug "No expected outputs were found for test file %s"
      tested_file;
    None)
  else
    let ninja, test_names =
      List.fold_left
        (fun (ninja, test_names) expected_output ->
          let expected_output_file =
            expected_output.output_dir
            ^ Filename.basename expected_output.tested_filename
            ^ "."
            ^ expected_output.id
          in
          let vars =
            [
              Var.(name catala_cmd), Nj.Expr.Lit expected_output.cmd;
              Var.(name tested_file), Nj.Expr.Lit tested_file;
              Var.(name expected_output), Nj.Expr.Lit expected_output_file;
            ]
          and rule_to_call =
            if reset_test_outputs then "reset_rule" else "test_rule"
          in
          let ninja_add_new_build
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
          ( ninja_add_new_build
              (expected_output_file ^ ".PHONY")
              rule_to_call vars ninja,
            test_names ^ " $\n  " ^ expected_output_file ^ ".PHONY" ))
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
    List.hd all_file_names
    ^ ""
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
               ( Var.(name test_file_or_folder),
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
         [catala_exe; "Interpret"; file; catala_opts; "-s " ^ scope])
  in
  Message.emit_debug "Running: %s" command;
  Sys.command command

(** {1 Driver} *)

let get_catala_files_in_folder (dir : string) : string list =
  let rec loop result = function
    | f :: fs ->
      let f_is_dir =
        try Sys.is_directory f
        with Sys_error e ->
          Message.emit_warning "skipping %s" e;
          false
      in
      if f_is_dir then
        readdir_sort f
        |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs
        |> loop result
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
    let collect f (ninja, test_file_names) file =
      match f ninja file reset_test_outputs with
      | None ->
        (* Skips none Catala file. *)
        ninja, test_file_names
      | Some (test_file_name, ninja) ->
        ninja, test_file_names ^ " $\n  " ^ test_file_name
    in
    List.fold_left
      (fun acc file ->
        let acc = collect collect_all_ninja_build acc file in
        collect collect_inline_ninja_builds acc file)
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
                     ( Var.(name test_file_or_folder),
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
  let add ctx f ninja_start tested_file =
    match f ninja_start tested_file reset_test_outputs with
    | Some (test_file_name, ninja) ->
      {
        last_valid_ninja = ninja;
        curr_ninja = Some ninja;
        all_file_names = tested_file :: ctx.all_file_names;
        all_test_builds = ctx.all_test_builds ^ " $\n  " ^ test_file_name;
        all_failed_names = List.filter (( <> ) tested_file) ctx.all_failed_names;
      }
    | None ->
      {
        ctx with
        last_valid_ninja = ninja_start;
        curr_ninja = None;
        all_failed_names = tested_file :: ctx.all_failed_names;
      }
  in
  let ctx = add ctx collect_all_ninja_build ninja_start tested_file in
  let ninja = Option.value ~default:ninja_start ctx.curr_ninja in
  add ctx collect_inline_ninja_builds ninja tested_file

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
      try ["-j" ^ Re.Group.get (Re.exec jobs_rex makeflags) 1] with _ -> []
    in
    String.concat " " ((if has_ignore then ["-k0"] else []) @ number_of_jobs)

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
  try
    let _options = Cli.enforce_globals ~debug () in
    let ninja_flags = makeflags_to_ninja_flags makeflags in
    let files_or_folders = List.sort_uniq String.compare files_or_folders
    and catala_exe = Option.fold ~none:"catala" ~some:Fun.id catala_exe
    and catala_opts = Option.fold ~none:"" ~some:Fun.id catala_opts
    and with_ninja_output k =
      match ninja_output with
      | Some f -> k f
      | None -> (
        let f = Filename.temp_file "clerk_build_" ".ninja" in
        match k f with
        | exception e ->
          if not debug then Sys.remove f;
          raise e
        | r ->
          Sys.remove f;
          r)
    in
    match String.lowercase_ascii command with
    | "test" -> (
      Message.emit_debug "building ninja rules...";
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
          (Message.emit_warning "No test case found for @{<magenta>%s@}")
          ctx.all_failed_names;
      if 0 = List.compare_lengths ctx.all_failed_names files_or_folders then
        return_ok
      else
        with_ninja_output
        @@ fun nin ->
        match
          File.with_formatter_of_file nin (fun fmt ->
              Message.emit_debug "writing %s..." nin;
              Nj.format fmt
                (add_root_test_build ninja ctx.all_file_names
                   ctx.all_test_builds))
        with
        | () ->
          let ninja_cmd =
            "ninja -k 0 -f " ^ nin ^ " " ^ ninja_flags ^ " test"
          in
          Message.emit_debug "executing '%s'..." ninja_cmd;
          Sys.command ninja_cmd
        | exception Sys_error e -> Message.raise_error "can not write in %s" e)
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
        Message.raise_error "Please provide a scope to run with the -s option")
    | "runtest" -> (
      match files_or_folders with
      | [f] ->
        Clerk_runtest.run_inline_tests ~reset:reset_test_outputs f catala_exe
          (List.filter (( <> ) "") (String.split_on_char ' ' catala_opts));
        0
      | _ -> Message.raise_error "Please specify a single catala file to test")
    | _ -> Message.raise_error "The command \"%s\" is unknown to clerk." command
  with Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit content Error;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    return_err

let main () = exit (Cmdliner.Cmd.eval' (Cmdliner.Cmd.v info (clerk_t driver)))
