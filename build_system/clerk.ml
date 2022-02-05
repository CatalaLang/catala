open Cmdliner
open Utils
open Ninja_utils
module Nj = Ninja_utils

(**{1 Command line interface}*)

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
  let exits = Term.default_exits @ [ Term.exit_info ~doc:"on error." 1 ] in
  Term.info "clerk" ~version ~doc ~exits ~man

(**{1 Testing}*)

let catala_backend_to_string (backend : Cli.backend_option) : string =
  match backend with
  | Cli.Interpret -> "Interpret"
  | Cli.Makefile -> "Makefile"
  | Cli.OCaml -> "Ocaml"
  | Cli.Scopelang -> "Scopelang"
  | Cli.Dcalc -> "Dcalc"
  | Cli.Latex -> "Latex"
  | Cli.Proof -> "Proof"
  | Cli.Html -> "Html"
  | Cli.Python -> "Python"
  | Cli.Typecheck -> "Typecheck"

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

let test_file (tested_file : string) (catala_exe : string) (catala_opts : string)
    (reset_test_outputs : bool) : testing_result =
  let expected_outputs = search_for_expected_outputs tested_file in
  if List.length expected_outputs = 0 then (
    Cli.debug_print (Format.asprintf "No expected outputs were found for test file %s" tested_file);
    { error_code = 0; number_of_tests_run = 0; number_correct = 0 })
  else
    List.fold_left
      (fun (exit : testing_result) expected_output ->
        let catala_backend = catala_backend_to_string expected_output.backend in
        let reproducible_catala_command =
          [
            catala_exe;
            catala_opts;
            (match expected_output.scope with None -> "" | Some scope -> "-s " ^ scope);
            catala_backend;
            tested_file;
            "--unstyled";
          ]
        in
        let command =
          String.concat " "
            (List.filter (fun s -> s <> "") reproducible_catala_command
            @ (match expected_output.backend with
              | Cli.Proof ->
                  [ "--disable_counterexamples" ]
                  (* Counterexamples can be different at each call because of the randomness inside
                     SMT solver, so we can't expect their value to remain constant. Hence we disable
                     the counterexamples when testing the replication of failed proofs. *)
              | _ -> [])
            @
            match expected_output.backend with
            | Cli.Interpret | Cli.Proof | Cli.Typecheck ->
                if reset_test_outputs then
                  [
                    ">";
                    Format.asprintf "%s%s" expected_output.output_dir
                      expected_output.complete_filename;
                    "2>&1 ";
                  ]
                else
                  [
                    "2>&1 ";
                    "|";
                    Format.asprintf "colordiff -u -b %s%s -" expected_output.output_dir
                      expected_output.complete_filename;
                  ]
            | Cli.Python | Cli.OCaml | Cli.Dcalc | Cli.Scopelang | Cli.Latex | Cli.Html
            | Cli.Makefile ->
                (* for those backends, the output of the Catala compiler will be written in a
                   temporary file which later we're going to diff with the *)
                if reset_test_outputs then
                  [
                    "-o";
                    Format.asprintf "%s%s" expected_output.output_dir
                      expected_output.complete_filename;
                  ]
                else
                  let temp_file =
                    Filename.temp_file "clerk_"
                      ("_" ^ catala_backend_to_string expected_output.backend)
                  in
                  [
                    "-o";
                    temp_file;
                    ";";
                    Format.asprintf "colordiff -u -b %s%s %s" expected_output.output_dir
                      expected_output.complete_filename temp_file;
                  ])
        in
        Cli.debug_print ("Running: " ^ command);
        let result = Sys.command command in
        if result <> 0 && not reset_test_outputs then (
          Cli.error_print
            (Format.asprintf "Test failed: %s@\nTo reproduce, run %s from folder %s"
               (Cli.print_with_style [ ANSITerminal.magenta ] "%s%s" expected_output.output_dir
                  expected_output.complete_filename)
               (Cli.print_with_style [ ANSITerminal.yellow ] "%s"
                  (String.concat " " (List.filter (fun s -> s <> "") reproducible_catala_command)))
               (Cli.print_with_style [ ANSITerminal.yellow ] "%s" (Sys.getcwd ())));
          {
            error_code = 1;
            number_of_tests_run = exit.number_of_tests_run + 1;
            number_correct = exit.number_correct;
          })
        else (
          Cli.result_print
            (Format.asprintf "Test %s: %s"
               (if reset_test_outputs then "reset" else "passed")
               (Cli.print_with_style [ ANSITerminal.magenta ] "%s%s" expected_output.output_dir
                  expected_output.complete_filename));
          {
            error_code = exit.error_code;
            number_of_tests_run = exit.number_of_tests_run + 1;
            number_correct = exit.number_correct + 1;
          }))
      { error_code = 0; number_of_tests_run = 0; number_correct = 0 }
      expected_outputs

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
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  let all_files_in_folder = loop [] [ dir ] in
  List.filter (Re.Pcre.pmatch ~rex:catala_suffix_regex) all_files_in_folder

let ninja_start catala_exe =
  let test_scope_rule =
    Nj.Rule.make "test_scope"
      ~command:
        Nj.Expr.(
          Seq
            [
              Lit (catala_exe ^ " -s");
              Var "scope";
              Lit "Interpret";
              Var "tested_file";
              (* TODO: find a way to add breaks *)
              Lit "--unstyle | colordiff -u -b";
              Var "expected_output";
              Lit "-";
            ])
      ~description:
        Nj.Expr.(Seq [ Lit "Testing scope"; Var "scope"; Lit "of file"; Var "tested_file" ])
  in
  { rules = Nj.RuleMap.(empty |> add "test_scope" test_scope_rule) }

let driver (file_or_folder : string) (command : string) (catala_exe : string option)
    (catala_opts : string option) (debug : bool) (scope : string option) (reset_test_outputs : bool)
    : int =
  if debug then Cli.debug_flag := true;
  let catala_exe = Option.fold ~none:"catala" ~some:Fun.id catala_exe in
  let catala_opts = Option.fold ~none:"" ~some:Fun.id catala_opts in
  match String.lowercase_ascii command with
  | "test" ->
      let results =
        if Sys.is_directory file_or_folder then (
          let results =
            List.fold_left
              (fun (exit : testing_result) file ->
                let result = test_file file catala_exe catala_opts reset_test_outputs in
                {
                  error_code =
                    (if result.error_code <> 0 && exit.error_code = 0 then result.error_code
                    else exit.error_code);
                  number_of_tests_run = exit.number_of_tests_run + result.number_of_tests_run;
                  number_correct = exit.number_correct + result.number_correct;
                })
              { error_code = 0; number_of_tests_run = 0; number_correct = 0 }
              (get_catala_files_in_folder file_or_folder)
          in
          Cli.result_print
            (Format.asprintf "Number of tests passed in folder %s: %s"
               (Cli.print_with_style [ ANSITerminal.magenta ] "%s" file_or_folder)
               (Cli.print_with_style
                  [
                    (if results.number_correct = results.number_of_tests_run then ANSITerminal.green
                    else ANSITerminal.red);
                  ]
                  "%d/%d" results.number_correct results.number_of_tests_run));
          results)
        else test_file file_or_folder catala_exe catala_opts reset_test_outputs
      in
      results.error_code
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
  let return_code = Cmdliner.Term.eval (clerk_t driver, info) in
  match return_code with
  | `Ok 0 -> Cmdliner.Term.exit (`Ok 0)
  | _ -> Cmdliner.Term.exit (`Error `Term)
