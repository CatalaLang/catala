open Cmdliner
open Utils

(**{1 Command line interface}*)

let file_or_folder =
  Arg.(required & pos 1 (some file) None & info [] ~docv:"FILE(S)" ~doc:"File or folder to process")

let command =
  Arg.(
    required
    & pos 0 (some string) None
    & info [] ~docv:"COMMAND" ~doc:"Command selection among: Test")

let debug = Arg.(value & flag & info [ "debug"; "d" ] ~doc:"Prints debug information")

let reset_test_outputs = Arg.(value & flag & info [ "r"; "reset" ] ~doc:"Reset tests outputs")

let catalac =
  Arg.(
    value
    & opt (some string) None
    & info [ "e"; "exe" ] ~docv:"EXE" ~doc:"Catala compiler executable, defaults to \"catala\"")

let catala_opts =
  Arg.(
    value
    & opt (some string) None
    & info [ "c"; "catala-opts" ] ~docv:"LANG" ~doc:"Options to pass to the Catala compiler")

let clerk_t f =
  Term.(const f $ file_or_folder $ command $ catalac $ catala_opts $ debug $ reset_test_outputs)

let version = "0.5.0"

let info =
  let doc =
    "Build system for Catala, a specification language for tax and social benefits computation \
     rules."
  in
  let exits = Term.default_exits @ [ Term.exit_info ~doc:"on error." 1 ] in
  Term.info "clerk" ~version ~doc ~exits

(**{1 Testing}*)

let catala_backend_to_string (backend : Cli.backend_option) : string =
  match backend with
  | Cli.Interpret -> "Interpret"
  | Cli.Makefile -> "Makefile"
  | Cli.OCaml -> "Ocaml"
  | Cli.Scopelang -> "Scopelang"
  | Cli.Dcalc -> "Dcalc"
  | Cli.Latex -> "Latex"
  | Cli.Html -> "Html"
  | Cli.Python -> "Python"

type expected_output_descr = {
  base_filename : string;
  output_dir : string;
  complete_filename : string;
  backend : Cli.backend_option;
  scope : string option;
}

let filename_to_expected_output_descr (output_dir : string) (filename : string) :
    expected_output_descr option =
  let complete_filename = filename in
  let first_extension = Filename.extension filename in
  let filename = Filename.remove_extension filename in
  let backend =
    match String.lowercase_ascii first_extension with
    | ".interpret" -> Some Cli.Interpret
    | ".makefile" -> Some Cli.Makefile
    | ".ocaml" -> Some Cli.OCaml
    | ".scopelang" -> Some Cli.Scopelang
    | ".dcalc" -> Some Cli.Dcalc
    | ".latex" -> Some Cli.Latex
    | ".html" -> Some Cli.Html
    | ".python" -> Some Cli.Python
    | _ -> None
  in
  match backend with
  | None -> None
  | Some backend ->
      let second_extension = Filename.extension filename in
      let catala_suffix_regex = Re.Pcre.regexp "\\.catala_(\\w){2}" in
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

let test_file (tested_file : string) (catala_exe : string option) (catala_opts : string option)
    (reset_test_outputs : bool) : int =
  let catala_exe = Option.fold ~none:"catala" ~some:Fun.id catala_exe in
  let catala_opts = Option.fold ~none:"" ~some:Fun.id catala_opts in
  let expected_outputs = search_for_expected_outputs tested_file in
  if List.length expected_outputs = 0 then (
    Cli.error_print (Format.asprintf "No expected outputs were found for test file %s" tested_file);
    0)
  else
    List.fold_left
      (fun exit expected_output ->
        let catala_backend = catala_backend_to_string expected_output.backend in
        let command =
          String.concat " "
            (List.filter
               (fun s -> s <> "")
               [
                 catala_exe;
                 catala_opts;
                 (match expected_output.scope with None -> "" | Some scope -> "-s " ^ scope);
                 "--unstyled";
                 catala_backend;
                 tested_file;
               ]
            @
            if reset_test_outputs then
              [
                ">";
                Format.asprintf "%s%s" expected_output.output_dir expected_output.complete_filename;
                "2>&1 ";
              ]
            else
              [
                "2>&1 ";
                "|";
                Format.asprintf "colordiff -u -b %s%s -" expected_output.output_dir
                  expected_output.complete_filename;
              ])
        in
        Cli.debug_print ("Running: " ^ command);
        let result = Sys.command command in
        if result <> 0 && not reset_test_outputs then (
          Cli.error_print
            (Format.asprintf "Test failed: %s"
               (Cli.print_with_style [ ANSITerminal.magenta ] "%s%s" expected_output.output_dir
                  expected_output.complete_filename));
          1)
        else (
          Cli.result_print
            (Format.asprintf "Test %s: %s"
               (if reset_test_outputs then "reset" else "passed")
               (Cli.print_with_style [ ANSITerminal.magenta ] "%s%s" expected_output.output_dir
                  expected_output.complete_filename));
          exit))
      0 expected_outputs

(** {1 Driver} *)

let driver (file_or_folder : string) (command : string) (catala_exe : string option)
    (catala_opts : string option) (debug : bool) (reset_test_outputs : bool) : int =
  if debug then Cli.debug_flag := true;
  match String.lowercase_ascii command with
  | "test" ->
      if Sys.is_directory file_or_folder then (
        Cli.error_print "Testing directories is not supported at this time";
        0)
      else test_file file_or_folder catala_exe catala_opts reset_test_outputs
  | _ ->
      Cli.error_print (Format.asprintf "The command \"%s\" is unknown to clerk." command);
      1

let _ =
  let return_code = Cmdliner.Term.eval (clerk_t driver, info) in
  match return_code with
  | `Ok 0 -> Cmdliner.Term.exit (`Ok 0)
  | _ -> Cmdliner.Term.exit (`Error `Term)
