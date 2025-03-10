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

(* Version is synchronised with the catala version *)
let version = Catala_utils.Cli.version

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
          "When compiling to the backends, enable the Catala $(i--autotest) \
           option that runs an evaluation of test scopes (understood as scopes \
           that need no input) and instruments their compiled version with \
           assertions that the results match. This shouldn't be specified \
           directly using $(i,--catala-opts=--autotest) because that wouldn't \
           guarantee that the necessary artifacts for interpretation are \
           present.")

let build_dir =
  Arg.(
    value
    & opt (some string) None
    & info ["build-dir"] ~docv:"DIR"
        ~env:(Cmd.Env.info "CLERK_BUILD_DIR")
        ~doc:
          "Directory where compilation artifacts should be written. Defaults \
           to '_build'.")

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
           NOTE: if this is set, all inline tests that are $(i,not) $(b,catala \
           test-scope) are skipped to avoid redundant testing.")

let runtest_report =
  Arg.(
    value
    & opt (some string) None
    & info ["report"] ~docv:"FILE"
        ~doc:
          "If set, $(i,clerk runtest) will output a tests result summary in \
           binary format to the given $(b,FILE)")

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
             "interpret", `Interpret; "ocaml", `OCaml; "c", `C; "python", `Python;
           ])
        `Interpret
    & info ["backend"] ~docv:"BACKEND"
        ~doc:
          "Run the program using the given backend. $(docv) must be one of \
           $(b,interpret), $(b,ocaml), $(b,c), $(b,python)")

let ignore_modules =
  Arg.(
    value
    & flag
    & info ["ignore-modules"]
        ~doc:
          "Silently ignore the files on the command-line that belong to \
           modules. May be useful to automatically run stand-alone tests when \
           using a general glob-pattern.")

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

module Global : sig
  val color : Catala_utils.Global.when_enum Term.t
  val debug : bool Term.t

  val term :
    (config_file:File.t option ->
    catala_exe:File.t option ->
    catala_opts:string list ->
    autotest:bool ->
    build_dir:File.t option ->
    include_dirs:string list ->
    vars_override:(string * string) list ->
    color:Global.when_enum ->
    debug:bool ->
    ninja_output:File.t option ->
    'a) ->
    'a Term.t
end = struct
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
             otherwise")

  let term f =
    Term.(
      const
        (fun
          config_file
          catala_exe
          catala_opts
          autotest
          build_dir
          include_dirs
          vars_override
          color
          debug
          ninja_output
        ->
          f ~config_file ~catala_exe ~catala_opts ~autotest ~build_dir
            ~include_dirs ~vars_override ~color ~debug ~ninja_output)
      $ config_file
      $ catala_exe
      $ catala_opts
      $ autotest
      $ build_dir
      $ include_dirs
      $ vars_override
      $ color
      $ debug
      $ ninja_output)
end

let files_or_folders =
  Arg.(
    value
    & pos_all string []
    & info [] ~docv:"FILE(S)" ~doc:"File(s) or folder(s) to process")

let files =
  Arg.(
    value & pos_all file [] & info [] ~docv:"FILE(S)" ~doc:"File(s) to process")

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

let targets =
  Arg.(
    value
    & pos_all string []
    & info [] ~docv:"TARGETS"
        ~doc:
          "Flags or targets to forward to Ninja directly (use $(b,-- \
           ninja_flags) to separate Ninja flags from Clerk flags)")

let report_verbosity =
  Arg.(
    value
    & vflag `Failures
        [
          ( `Summary,
            info ["summary"] ~doc:"Only display a summary of the test results" );
          ( `Short,
            info ["short"] ~doc:"Don't display detailed test failures diff" );
          ( `Failures,
            info ["failures"]
              ~doc:"Show details of files with failed tests only" );
          ( `Verbose,
            info ["verbose"; "v"]
              ~doc:"Display the full list of tests that have been run" );
        ])

let report_xml =
  Arg.(
    value
    & flag
    & info ["xml"]
        ~env:(Cmd.Env.info "CATALA_XML_REPORT")
        ~doc:"Output the test report in JUnit-compatible XML format")

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
           the reference file and the output file")

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
  Cmd.info "clerk" ~version ~doc ~exits ~man
