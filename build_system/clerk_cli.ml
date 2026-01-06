(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

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

(** {1 Command line interface} *)

let catala_exe =
  Arg.(
    value
    & opt (some string) None
    & info ["e"; "exe"] ~docv:"EXE" ~doc:"Catala compiler executable.")

let catala_opts =
  Arg.(
    value
    & opt_all string []
    & info ["c"; "catala-opts"] ~docv:"FLAG"
        ~doc:
          "Option to pass to the Catala compiler. Can be repeated. If neither \
           this nor $(b,--test-flags) is specified, the flags for the \
           different backends default to $(b,-O).")

let autotest =
  Arg.(
    value
    & flag
    & info ["autotest"]
        ~doc:
          "When compiling to the backends, enable the Catala $(i,--autotest) \
           option that runs an evaluation of test scopes (understood as scopes \
           that need no input) and instruments their compiled version with \
           assertions that the results match. This shouldn't be specified \
           directly using $(i,--catala-opts=--autotest) because that wouldn't \
           guarantee that the necessary artifacts for interpretation are \
           present.")

let quiet =
  Arg.(
    value
    & flag
    & info ["quiet"]
        ~doc:
          "Silences the $(i,ninja) build tool that usually prints its output \
           on the standard output.")

let prepare_only =
  Arg.(
    value
    & flag
    & info ["prepare-only"]
        ~doc:"Compile dependencies of the target(s) but do not run it.")

let build_dir =
  Arg.(
    value
    & opt (some string) None
    & info ["build-dir"] ~docv:"DIR"
        ~env:(Cmd.Env.info "CLERK_BUILD_DIR")
        ~doc:
          "Directory where intermediate compilation artifacts should be \
           written. Defaults to '_build'.")

let target_dir =
  Arg.(
    value
    & opt (some string) None
    & info ["target-dir"] ~docv:"DIR"
        ~env:(Cmd.Env.info "CLERK_TARGET_DIR")
        ~doc:
          "Directory where final compilation targets should be written. \
           Defaults to '_target'.")

let include_dirs =
  let arg =
    Arg.(
      value
      & opt_all (list ~sep:':' string) []
      & info ["I"; "include"] ~docv:"DIR"
          ~env:(Cmd.Env.info "CATALA_INCLUDE")
          ~doc:
            "Make modules from the given directory available from everywhere. \
             Several dirs can be specified by repeating the flag or separating \
             them with '$(b,:)'.")
  in
  Term.(const List.flatten $ arg)

let test_flags =
  Arg.(
    value
    & opt ~vopt:[""] (list string) []
    & info ["test-flags"] ~docv:"FLAGS"
        ~env:(Cmd.Env.info "CATALA_TEST_FLAGS")
        ~doc:
          "Flags to pass to the catala interpreter on $(b,catala test-scope) \
           tests. Comma-separated list. A subset may also be applied to the \
           compilation of modules, as needed.\n\
           WARNING: flag shortcuts are not allowed here (i.e. don't use \
           non-ambiguous prefixes such as $(b,--closure) for \
           $(b,--closure-conversion))\n\
           NOTE: if this is set, all cli tests that are $(i,not) $(b,catala \
           test-scope) are skipped to avoid redundant testing.")

let runtest_report =
  Arg.(
    value
    & opt (some string) None
    & info ["report"] ~docv:"FILE"
        ~doc:
          "If set, $(i,clerk runtest) will output a tests result summary in \
           binary format to the given $(b,FILE).")

let runtest_out =
  Arg.(
    value
    & pos 1 (some string) None
    & info [] ~docv:"OUTFILE"
        ~doc:"Write the test outcome to file $(b,OUTFILE) instead of stdout.")

let backend =
  Arg.(
    value
    & opt
        (enum
           [
             "interpret", `Interpret;
             "ocaml", `OCaml;
             "c", `C;
             "python", `Python;
             "java", `Java;
           ])
        `Interpret
    & info ["backend"] ~docv:"BACKEND"
        ~doc:
          "Run the program using the given backend. $(docv) must be one of \
           $(b,interpret), $(b,ocaml), $(b,c), $(b,python), $(b,java).")

let run_command =
  Arg.(
    value
    & opt string "interpret"
    & info ["command"] ~docv:"CMD"
        ~doc:
          "The catala command to run on the input files. Normally \
           $(b,interpret), this flag can be used to run $(b,typecheck) or a \
           custom plugin instead. This is ignored if $(i,--backend) isn't \
           $(b,interpret).")

let vars_override =
  Arg.(
    value
    & opt_all (pair ~sep:'=' string string) []
    & info ["vars"] ~docv:"VAR=VALUE"
        ~doc:
          "Override the given build variable with the given value. Use \
           $(i,clerk list-vars) to list the available variables.")

let config_file =
  Arg.(
    value
    & opt (some file) None
    & info ["config"] ~docv:"FILE"
        ~doc:
          "Clerk configuration file to use, instead of looking up \
           \"clerk.toml\" in parent directories.")

let color =
  Arg.(
    value
    & opt ~vopt:Global.Always Cli.when_opt Auto
    & info ["color"]
        ~env:(Cmd.Env.info "CATALA_COLOR")
        ~doc:
          "Allow output of colored and styled text. Use $(i,auto), to enable \
           when the standard output is to a terminal, $(i,never) to disable.")

let debug =
  Arg.(value & flag & info ["debug"; "d"] ~doc:"Prints debug information")

let ninja_output =
  Arg.(
    value
    & opt (some string) None
    & info ["o"; "output"] ~docv:"FILE"
        ~doc:
          "$(i,FILE) is the file that will contain the build.ninja file \
           output. If not specified, the build.ninja file is set to \
           $(i,<builddir>/clerk.ninja) in debug mode, and a temporary file \
           otherwise.")

let files_or_folders =
  Arg.(
    value
    & pos_all string []
    & info [] ~docv:"FILE" ~doc:"File(s) or folder(s) to process")

let files =
  Arg.(value & pos_all file [] & info [] ~docv:"FILE" ~doc:"File(s) to process")

let targets =
  Arg.(
    value
    & pos_all string []
    & info [] ~docv:"TARGET" ~doc:"Clerk targets to build")

let single_file =
  Arg.(
    required
    & pos 0 (some file) None
    & info [] ~docv:"FILE" ~doc:"File to process")

let reset_test_outputs =
  Arg.(
    value
    & flag
    & info ["r"; "reset"]
        ~doc:
          "Used with the `test` command, resets the test output to whatever is \
           output by the Catala compiler.")

let scope =
  Arg.(
    value
    & opt (some string) None
    & info ["s"; "scope"] ~docv:"SCOPE"
        ~doc:
          "Used with the `run` command, selects which scope of a given Catala \
           file to run.")

let scope_input = Catala_utils.Cli.Flags.scope_input

let clerk_targets_or_files =
  Arg.(
    value
    & pos_all string []
    & info [] ~docv:"TARGET(S)"
        ~doc:"Clerk target(s) or individual file(s) to process")

let clerk_targets_or_files_or_folders =
  Arg.(
    value
    & pos_all string []
    & info [] ~docv:"TARGET(S)"
        ~doc:"Clerk target(s), individual file(s) or folder(s) to process")

let report_verbosity =
  Arg.(
    value
    & vflag `Failures
        [
          ( `Summary,
            info ["summary"] ~doc:"Only display a summary of the test results."
          );
          ( `Short,
            info ["short"] ~doc:"Don't display detailed test failures diff." );
          ( `Failures,
            info ["failures"]
              ~doc:"Show details of files with failed tests only." );
          ( `Verbose,
            info ["verbose"; "v"]
              ~doc:"Display the full list of tests that have been run." );
        ])

let report_format =
  Arg.(
    value
    & vflag `Terminal
        [
          ( `Terminal,
            info ["terminal"]
              ~doc:
                "Output the test report in a human-friendly format on the \
                 terminal." );
          ( `JUnitXML,
            info ["xml"]
              ~doc:"Output the test report in JUnit-compatible XML format." );
          ( `VSCodeJSON,
            info ["json"]
              ~doc:"Output the test report as a VSCode-compatible JSON format."
          );
        ])

let code_coverage =
  Arg.(
    value
    & flag
    & info ["code-coverage"]
        ~env:(Cmd.Env.info "CATALA_MEASURE_COVERAGE")
        ~doc:"Measure code coverage in the test report.")

let diff_command =
  Arg.(
    value
    & opt ~vopt:(Some None) (some (some string)) None
    & info ["diff"]
        ~env:(Cmd.Env.info "CATALA_DIFF_COMMAND")
        ~doc:
          "Use a standard $(i,diff) command instead of the default \
           side-by-side view. If no argument is supplied, the command will be \
           $(b,patdiff) if available or $(b,diff) otherwise. A supplied \
           argument will be used as diff command with arguments pointing to \
           the reference file and the output file.")

let ninja_flags =
  let env =
    Cmd.Env.info
      ~doc:
        "make-compatible flags handling. Currently recognizes the -i and -j \
         options and forwards them through to Ninja."
      "MAKEFLAGS"
  in
  let makeflags =
    Arg.(
      value
      & opt (some string) None
      & info ["makeflags"] ~env ~docv:"FLAG"
          ~doc:
            "Provides the contents of a $(i, MAKEFLAGS) variable to pass on to \
             Ninja. Currently recognizes the -i and -j options.")
  in
  let makeflags_to_ninja_flags (makeflags : string option) =
    match makeflags with
    | None -> ["-k0"]
    | Some makeflags ->
      let ignore_rex = Re.(compile @@ word (char 'i')) in
      let has_ignore = Re.execp ignore_rex makeflags in
      let jobs_rex = Re.(compile @@ seq [str "-j"; group (rep digit)]) in
      let number_of_jobs =
        try ["-j" ^ Re.Group.get (Re.exec jobs_rex makeflags) 1] with _ -> []
      in
      (if has_ignore then ["-k0"] else []) @ number_of_jobs
  in
  Term.(const makeflags_to_ninja_flags $ makeflags)

let whole_program =
  let open Arg in
  value
  & flag
  & info ["W"; "whole-program"]
      ~doc:
        "Use Catala $(i,--whole-program) option when testing or executing \
         Catala scopes."

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
      `S Manpage.s_authors;
      `P "Denis Merigoux <denis.merigoux@inria.fr>";
      `P "Emile Rolley <emile.rolley@tuta.io>";
      `P "Louis Gesbert <louis.gesbert@inria.fr>";
      `S Manpage.s_examples;
      `P "Typical usage:";
      `Pre "clerk test file.catala_en";
      `S Manpage.s_bugs;
      `P
        "Please file bug reports at https://github.com/CatalaLang/catala/issues";
    ]
  in
  let exits = Cmd.Exit.defaults @ [Cmd.Exit.info ~doc:"on error." 1] in
  Cmd.info "clerk" ~version:Catala_utils.Cli.version ~doc ~exits ~man

(** {2 Initialisation of options} *)

type config = {
  options : Clerk_config.t;
  fix_path : File.t -> File.t;
  ninja_file : File.t option;
  test_flags : string list;
}

let init
    test_flags
    config_file
    ninja_file
    catala_exe
    catala_opts
    build_dir
    target_dir
    include_dirs
    color
    debug
    whole_program
    output_format =
  if debug then Printexc.record_backtrace true;
  let _options = Catala_utils.Global.enforce_options ~debug ~color () in
  let default_config_file = "clerk.toml" in
  let set_root_dir dir =
    Message.debug "Entering directory %a" File.format dir;
    Sys.chdir dir
  in
  (* fix_path adjusts paths specified from the command-line relative to the user
     cwd to be instead relative to the project root *)
  let fix_path, config =
    let from_dir = Sys.getcwd () in
    match config_file with
    | None -> (
      match
        File.(find_in_parents (fun dir -> exists (dir / default_config_file)))
      with
      | Some (root, rel) ->
        set_root_dir root;
        ( Catala_utils.File.reverse_path ~from_dir ~to_dir:rel,
          Clerk_config.read default_config_file )
      | None -> (
        match
          File.(
            find_in_parents (function dir ->
                exists (dir / "catala.opam") || exists (dir / ".git")))
        with
        | Some (root, rel) ->
          set_root_dir root;
          ( Catala_utils.File.reverse_path ~from_dir ~to_dir:rel,
            Clerk_config.default_config )
        | None ->
          ( Catala_utils.File.make_relative_to ~dir:from_dir,
            Clerk_config.default_config )))
    | Some f ->
      let root = Filename.dirname f in
      let config = Clerk_config.read f in
      set_root_dir root;
      (fun d -> Catala_utils.File.reverse_path ~from_dir ~to_dir:root d), config
  in
  let build_dir =
    let dir =
      match build_dir with None -> config.global.build_dir | Some dir -> dir
    in
    let dir =
      match test_flags with
      | [] -> dir
      | flags -> File.((dir / "test") ^ String.concat "" flags)
    in
    let dir = File.clean_path dir in
    File.ensure_dir dir;
    dir
    (* Note: it could be safer here to use File.(Sys.getcwd () / "_build") by
       default, but Ninja treats relative and absolute paths separately so that
       you wouldn't then be able to build target _build/foo.ml but would have to
       write the full path every time *)
  in
  let target_dir =
    let dir =
      match target_dir with None -> config.global.target_dir | Some dir -> dir
    in
    let dir = File.clean_path dir in
    File.ensure_dir dir;
    dir
  in
  let include_dirs, bad_dirs =
    List.partition_map
      (fun dir ->
        match File.check_directory dir with
        | Some d -> Left (fix_path d)
        | None -> Right dir)
      (List.sort_uniq compare (config.global.include_dirs @ include_dirs))
  in
  List.iter
    (fun bad_dir ->
      Message.warning
        "Ignoring included directory '%s': it is not a directory or file does \
         not exist."
        bad_dir)
    bad_dirs;
  let test_flags =
    if whole_program then "--whole-program" :: test_flags else test_flags
  in
  let catala_opts =
    match output_format with
    | Global.Human -> catala_opts
    | JSON -> ["--format"; "json"] @ catala_opts
  in
  {
    options =
      {
        config with
        global =
          {
            config.global with
            build_dir;
            target_dir;
            catala_exe;
            catala_opts = config.global.catala_opts @ catala_opts;
            include_dirs;
          };
      };
    fix_path;
    ninja_file;
    test_flags;
  }

let init_term ?(allow_test_flags = false) () =
  let test_flags = if allow_test_flags then test_flags else Term.const [] in
  Term.(
    const init
    $ test_flags
    $ config_file
    $ ninja_output
    $ catala_exe
    $ catala_opts
    $ build_dir
    $ target_dir
    $ include_dirs
    $ color
    $ debug
    $ whole_program
    $ Cli.Flags.output_format)
