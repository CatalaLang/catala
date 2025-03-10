(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020 Inria,
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

open Catala_utils
open Ninja_utils
module Nj = Ninja_utils
module Scan = Clerk_scan

(* Version is synchronised with the catala version *)
let version = Catala_utils.Cli.version

(** {1 Command line interface} *)

type backend = OCaml | Python | C | Tests (* | JS *)

let all_backends = [OCaml; Python; C; Tests]
let ninja_exec = "ninja"

module Cli = struct
  open Cmdliner

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
            "Option to pass to the Catala compiler. Can be repeated. If \
             neither this nor $(b,--test-flags) is specified, the flags for \
             the different backends default to $(b,-O).")

  let autotest =
    Arg.(
      value
      & flag
      & info ["autotest"]
          ~doc:
            "When compiling to the backends, enable the Catala $(i--autotest) \
             option that runs an evaluation of test scopes (understood as \
             scopes that need no input) and instruments their compiled version \
             with assertions that the results match. This shouldn't be \
             specified directly using $(i,--catala-opts=--autotest) because \
             that wouldn't guarantee that the necessary artifacts for \
             interpretation are present.")

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
              "Make modules from the given directory available from \
               everywhere. Several dirs can be specified by repeating the flag \
               or separating them with '$(b,:)'.")
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
             NOTE: if this is set, all inline tests that are $(i,not) \
             $(b,catala test-scope) are skipped to avoid redundant testing.")

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
               "interpret", `Interpret;
               "ocaml", `OCaml;
               "c", `C;
               "python", `Python;
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
             modules. May be useful to automatically run stand-alone tests \
             when using a general glob-pattern.")

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
              "Allow output of colored and styled text. Use $(i,auto), to \
               enable when the standard output is to a terminal, $(i,never) to \
               disable.")

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
      value
      & pos_all file []
      & info [] ~docv:"FILE(S)" ~doc:"File(s) to process")

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
            "Used with the `test` command, resets the test output to whatever \
             is output by the Catala compiler.")

  let scope =
    Arg.(
      value
      & opt (some string) None
      & info ["s"; "scope"] ~docv:"SCOPE"
          ~doc:
            "Used with the `run` command, selects which scope of a given \
             Catala file to run.")

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
              info ["summary"] ~doc:"Only display a summary of the test results"
            );
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
             side-by-side view. If no argument is supplied, the command will \
             be $(b,patdiff) if available or $(b,diff) otherwise. A supplied \
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
              "Provides the contents of a $(i, MAKEFLAGS) variable to pass on \
               to Ninja. Currently recognizes the -i and -j options.")
    in
    let makeflags_to_ninja_flags (makeflags : string option) =
      match makeflags with
      | None -> ["-k0"]
      | Some makeflags ->
        let ignore_rex = Re.(compile @@ word (char 'i')) in
        let has_ignore = Re.execp ignore_rex makeflags in
        let jobs_rex = Re.(compile @@ seq [str "-j"; group (rep digit)]) in
        let number_of_jobs =
          try ["-j" ^ Re.Group.get (Re.exec jobs_rex makeflags) 1]
          with _ -> []
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
          "$(b,clerk) is a build system for Catala, a specification language \
           for tax and social benefits computation rules";
        `S Manpage.s_authors;
        `P "Denis Merigoux <denis.merigoux@inria.fr>";
        `P "Emile Rolley <emile.rolley@tuta.io>";
        `P "Louis Gesbert <louis.gesbert@inria.fr>";
        `S Manpage.s_examples;
        `P "Typical usage:";
        `Pre "clerk test file.catala_en";
        `S Manpage.s_bugs;
        `P
          "Please file bug reports at \
           https://github.com/CatalaLang/catala/issues";
      ]
    in
    let exits = Cmd.Exit.defaults @ [Cmd.Exit.info ~doc:"on error." 1] in
    Cmd.info "clerk" ~version ~doc ~exits ~man
end

(** {1 System analysis} *)

(** Some functions that poll the surrounding systems (think [./configure]) *)
module Poll = struct
  (** This module is sensitive to the CWD at first use. Therefore it's expected
      that [chdir] has been run beforehand to the project root. *)
  let root = lazy (Sys.getcwd ())

  (** Scans for a parent directory being the root of the Catala source repo *)
  let catala_project_root : File.t option Lazy.t =
    root
    |> Lazy.map
       @@ fun root ->
       if File.(exists (root / "catala.opam") && exists (root / "dune-project"))
       then Some root
       else None

  let exec_dir : File.t = Catala_utils.Cli.exec_dir
  let clerk_exe : File.t Lazy.t = lazy (Unix.realpath Sys.executable_name)

  let catala_exe : File.t Lazy.t =
    lazy
      (let f = File.(exec_dir / "catala") in
       if Sys.file_exists f then Unix.realpath f
       else
         match catala_project_root with
         | (lazy (Some root)) ->
           Unix.realpath
             File.(root / "_build" / "default" / "compiler" / "catala.exe")
         | _ -> File.check_exec "catala")

  let build_dir : dir:File.t -> unit -> File.t =
   fun ~dir () ->
    let d = File.clean_path dir in
    File.ensure_dir d;
    d
  (* Note: it could be safer here to use File.(Sys.getcwd () / "_build") by
     default, but Ninja treats relative and absolute paths separately so that
     you wouldn't then be able to build target _build/foo.ml but would have to
     write the full path every time *)

  (** Locates the main [lib] directory containing the OCaml libs *)
  let ocaml_libdir : File.t Lazy.t =
    lazy
      (try String.trim (File.process_out "opam" ["var"; "lib"])
       with Failure _ -> (
         try String.trim (File.process_out "ocamlc" ["-where"])
         with Failure _ -> (
           match File.(check_directory (exec_dir /../ "lib")) with
           | Some d -> d
           | None ->
             Message.error
               "Could not locate the OCaml library directory, make sure OCaml \
                or opam is installed")))

  (** Locates the directory containing the OCaml runtime to link to *)
  let ocaml_runtime_dir : File.t Lazy.t =
    lazy
      (let d =
         match Lazy.force catala_project_root with
         | Some root ->
           (* Relative dir when running from catala source *)
           File.(
             root
             / "_build"
             / "install"
             / "default"
             / "lib"
             / "catala"
             / "runtime_ocaml")
         | None -> (
           match
             File.check_directory
               File.(exec_dir /../ "lib" / "catala" / "runtime_ocaml")
           with
           | Some d -> d
           | None -> File.(Lazy.force ocaml_libdir / "catala" / "runtime_ocaml")
           )
       in
       match File.check_directory d with
       | Some dir ->
         Message.debug "Catala runtime libraries found at @{<bold>%s@}." dir;
         dir
       | None ->
         Message.error
           "@[<hov>Could not locate the Catala runtime library at %s.@ Make \
            sure that either catala is correctly installed,@ or you are \
            running from the root of a compiled source tree.@]"
           d)

  let ocaml_include_and_lib_flags : (string list * string list) Lazy.t =
    lazy
      (let link_libs = ["zarith"; "dates_calc"] in
       let includes_libs =
         List.map
           (fun lib ->
             match File.(check_directory (Lazy.force ocaml_libdir / lib)) with
             | None ->
               Message.error
                 "Required OCaml library not found at %a.@ Try `opam install \
                  %s'"
                 File.format
                 File.(Lazy.force ocaml_libdir / lib)
                 lib
             | Some d ->
               ( ["-I"; d],
                 String.map (function '-' -> '_' | c -> c) lib ^ ".cmxa" ))
           link_libs
       in
       let includes, libs = List.split includes_libs in
       ( List.concat includes @ ["-I"; Lazy.force ocaml_runtime_dir],
         libs @ [File.(Lazy.force ocaml_runtime_dir / "runtime_ocaml.cmxa")] ))

  let ocaml_include_flags : string list Lazy.t =
    lazy (fst (Lazy.force ocaml_include_and_lib_flags))

  let ocaml_link_flags : string list Lazy.t =
    lazy (snd (Lazy.force ocaml_include_and_lib_flags))

  let c_runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force ocaml_runtime_dir /../ "runtime_c")

  let python_runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force ocaml_runtime_dir /../ "runtime_python" / "src")
end

(**{1 Building rules}*)

(** Ninja variable names *)
module Var = struct
  include Nj.Var

  (** Global vars: always defined, at toplevel *)

  let ninja_required_version = make "ninja_required_version"
  let builddir = make "builddir"
  let clerk_exe = make "CLERK_EXE"
  let clerk_flags = make "CLERK_FLAGS"
  let catala_exe = make "CATALA_EXE"
  let catala_flags = make "CATALA_FLAGS"

  let make, all_vars_ref =
    let all_vars_ref = ref String.Map.empty in
    ( (fun s ->
        let v = make s in
        all_vars_ref := String.Map.add s v !all_vars_ref;
        v),
      all_vars_ref )

  let catala_flags_ocaml = make "CATALA_FLAGS_OCAML"
  let catala_flags_c = make "CATALA_FLAGS_C"
  let catala_flags_python = make "CATALA_FLAGS_PYTHON"
  let ocamlc_exe = make "OCAMLC_EXE"
  let ocamlopt_exe = make "OCAMLOPT_EXE"
  let ocaml_flags = make "OCAML_FLAGS"
  let runtime_ocaml_libs = make "RUNTIME_OCAML_LIBS"
  let cc_exe = make "CC"
  let c_flags = make "CFLAGS"
  let runtime_c_libs = make "RUNTIME_C_LIBS"
  let python = make "PYTHON"
  let runtime_python_dir = make "RUNTIME_PYTHON"
  let all_vars = all_vars_ref.contents

  (** Rule vars, Used in specific rules *)

  let input = make "in"
  let output = make "out"
  let src = make "src"
  let target = make "target"
  let includes = make "includes"

  (* let scope = make "scope" *)
  let test_id = make "test-id"
  let ( ! ) = Var.v
end

let base_bindings
    catala_exe
    catala_flags0
    build_dir
    include_dirs
    vars_override
    test_flags
    enabled_backends
    autotest =
  let includes =
    List.fold_right
      (fun dir flags ->
        if Filename.is_relative dir then
          "-I" :: File.(Var.(!builddir) / dir) :: flags
        else "-I" :: dir :: flags)
      include_dirs []
  in
  let catala_flags = ("--directory=" ^ Var.(!builddir)) :: catala_flags0 in
  let catala_flags_ocaml =
    (if autotest then ["--autotest"] else [])
    @
    if test_flags = [] && catala_flags0 = [] then ["-O"]
    else
      List.filter
        (function
          | "-O" | "--optimize" | "--closure-conversion" -> true | _ -> false)
        test_flags
  in
  let catala_flags_c =
    (if autotest then ["--autotest"] else [])
    @
    if test_flags = [] && catala_flags0 = [] then ["-O"]
    else
      List.filter
        (function "-O" | "--optimize" -> true | _ -> false)
        test_flags
  in
  let catala_flags_python =
    (if autotest then ["--autotest"] else [])
    @
    if test_flags = [] && catala_flags0 = [] then ["-O"]
    else
      List.filter
        (function
          | "-O" | "--optimize" | "--closure-conversion" -> true | _ -> false)
        test_flags
  in
  let def var value =
    let value =
      match List.assoc_opt (Var.name var) vars_override with
      | Some v -> [v]
      | None -> Lazy.force value
    in
    Nj.binding var value
  in
  [
    def Var.ninja_required_version (lazy ["1.7"]);
    (* use of implicit outputs *)
    def Var.builddir (lazy [build_dir]);
    def Var.clerk_exe (lazy [Lazy.force Poll.clerk_exe]);
    def Var.catala_exe
      (lazy
        [
          (match catala_exe with
          | Some e -> File.check_exec e
          | None -> Lazy.force Poll.catala_exe);
        ]);
    def Var.catala_flags (lazy (catala_flags @ includes));
    def Var.clerk_flags
      (lazy
        ("-e"
         :: Var.(!catala_exe)
         :: ("--test-flags=" ^ String.concat "," test_flags)
         :: includes
        @ List.map (fun f -> "--catala-opts=" ^ f) catala_flags));
  ]
  @ (if List.mem OCaml enabled_backends then
       [
         def Var.catala_flags_ocaml (lazy catala_flags_ocaml);
         def Var.ocamlc_exe (lazy ["ocamlc"]);
         def Var.ocamlopt_exe (lazy ["ocamlopt"]);
         def Var.ocaml_flags
           (lazy (Lazy.force Poll.ocaml_include_flags @ includes));
         def Var.runtime_ocaml_libs (lazy (Lazy.force Poll.ocaml_link_flags));
       ]
     else [])
  @ (if List.mem Python enabled_backends then
       [
         def Var.catala_flags_python (lazy catala_flags_python);
         def Var.python (lazy ["python3"]);
         def Var.runtime_python_dir (lazy [Lazy.force Poll.python_runtime_dir]);
       ]
     else [])
  @
  if List.mem C enabled_backends then
    [
      def Var.catala_flags_c (lazy catala_flags_c);
      def Var.cc_exe (lazy ["cc"]);
      def Var.runtime_c_libs
        (lazy
          [
            "-I" ^ Lazy.force Poll.c_runtime_dir;
            "-L" ^ Lazy.force Poll.c_runtime_dir;
            "-lcatala_runtime";
            "-lgmp";
          ]);
      def Var.c_flags
        (lazy
          ([
             "-std=c89";
             "-pedantic";
             "-Wall";
             "-Wno-unused-function";
             "-Wno-unused-variable";
             "-Wno-unused-but-set-variable";
             "-Werror";
             "-g";
             Var.(!runtime_c_libs);
           ]
          @ includes));
    ]
  else []

let[@ocamlformat "disable"] static_base_rules enabled_backends =
  let open Var in
  [
    Nj.rule "copy"
      ~command:["cp"; "-f"; !input; !output]
      ~description:["<copy>"; !input];
  ] @ (if List.mem OCaml enabled_backends then [
      Nj.rule "catala-ocaml"
        ~command:[!catala_exe; "ocaml"; !catala_flags; !catala_flags_ocaml;
                  !input; "-o"; !output]
        ~description:["<catala>"; "ocaml"; "⇒"; !output];

      Nj.rule "ocaml-object"
        ~command:[!ocamlc_exe; "-i"; !ocaml_flags; !includes; !input; ">"; !input^"i"; "&&";
                  !ocamlc_exe; "-opaque"; !ocaml_flags; !includes; !input^"i"; "&&";
                  !ocamlc_exe; "-c"; !ocaml_flags; !includes; !input; "&&";
                  !ocamlopt_exe; "-c"; "-intf-suffix"; ".ml"; !ocaml_flags; !includes; !input]
        ~description:["<ocaml>"; "⇒"; !output];

      Nj.rule "ocaml-module"
        ~command:
          [!ocamlopt_exe; "-shared"; !ocaml_flags; !input; "-o"; !output]
        ~description:["<ocaml>"; "⇒"; !output];
    ] else []) @
  (if List.mem C enabled_backends then [
    Nj.rule "catala-c"
      ~command:[!catala_exe; "c"; !catala_flags; !catala_flags_c;
                !input; "-o"; !output]
      ~description:["<catala>"; "c"; "⇒"; !output];

    Nj.rule "c-object"
      ~command:
        [!cc_exe; !input; !c_flags; !includes; "-c"; "-o"; !output]
      ~description:["<cc>"; "⇒"; !output];
  ] else []) @
  (if List.mem Python enabled_backends then [
      Nj.rule "python"
        ~command:[!catala_exe; "python"; !catala_flags; !catala_flags_python;
                  !input; "-o"; !output]
        ~description:["<catala>"; "python"; "⇒"; !output];
    ] else []) @
  (if List.mem Tests enabled_backends then
     [Nj.rule "tests"
        ~command:
          [!clerk_exe; "runtest"; !clerk_flags; !input;
           "--report"; !output;]
        ~description:["<catala>"; "tests"; "⇐"; !input];

      Nj.rule "dir-tests"
        ~command:["cat"; !input; ">"; !output; ";"]
        ~description:["<test>"; !test_id];
     ]
   else [])

let gen_build_statements
    (include_dirs : string list)
    (enabled_backends : backend list)
    (autotest : bool)
    (same_dir_modules : (string * File.t) list)
    (item : Scan.item) : Nj.ninja =
  let open File in
  let ( ! ) = Var.( ! ) in
  let src = item.file_name in
  let target =
    match item.module_def with
    | None -> !Var.builddir / Filename.remove_extension src
    | Some n -> !Var.builddir / Filename.dirname src / n
  in
  let include_flags =
    "-I"
    :: (!Var.builddir / src /../ "")
    :: List.concat_map
         (fun d ->
           ["-I"; (if Filename.is_relative d then !Var.builddir / d else d)])
         include_dirs
  in
  let def_vars =
    [
      Nj.binding Var.src [src];
      Nj.binding Var.target [target];
      Nj.binding Var.includes include_flags;
    ]
  in
  let modules = List.rev item.used_modules in
  let modfile ext ?(mod_ext = ext) modname =
    match List.assoc_opt modname same_dir_modules with
    | Some f -> (!Var.builddir / Filename.dirname f / modname) ^ ext
    | None -> modname ^ mod_ext
  in
  let module_target x = modfile "@ml-module" x in
  let catala_src = !Var.builddir / !Var.src in
  let include_deps =
    Nj.build "copy" ~inputs:[!Var.src]
      ~implicit_in:
        (List.map (( / ) !Var.builddir) item.included_files
        @ List.map
            (fun m ->
              try !Var.builddir / List.assoc m same_dir_modules
              with Not_found -> m ^ "@src")
            modules)
      ~outputs:[catala_src]
  in
  let module_deps =
    Option.map
      (fun _ ->
        Nj.build "phony"
          ~inputs:[!Var.target ^ ".cmi"; !Var.target ^ ".cmxs"]
          ~implicit_in:(List.map (modfile "@ml-module") modules)
          ~outputs:[!Var.target ^ "@ml-module"])
      item.module_def
  in
  let ocaml, c, python =
    if item.extrnal then
      ( Nj.build "copy" ~implicit_in:[catala_src]
          ~inputs:[src -.- "ml"]
          ~outputs:[!Var.target ^ ".ml"],
        List.to_seq
          [
            Nj.build "copy" ~implicit_in:[catala_src]
              ~inputs:[src -.- "c"]
              ~outputs:[!Var.target ^ ".c"];
            Nj.build "copy" ~implicit_in:[catala_src]
              ~inputs:[src -.- "h"]
              ~outputs:[!Var.target ^ ".h"];
          ],
        Nj.build "copy" ~implicit_in:[catala_src]
          ~inputs:[src -.- "py"]
          ~outputs:[!Var.target ^ ".py"] )
    else
      let inputs = [catala_src] in
      let implicit_in =
        (* autotest requires interpretation at compile-time, which makes use of
           the dependent OCaml modules (cmxs) *)
        !Var.catala_exe
        :: (if autotest then List.map module_target modules else [])
      in
      ( Nj.build "catala-ocaml" ~inputs ~implicit_in
          ~outputs:[!Var.target ^ ".ml"],
        Seq.return
          (Nj.build "catala-c" ~inputs ~implicit_in
             ~outputs:[!Var.target ^ ".c"]
             ~implicit_out:[!Var.target ^ ".h"]),
        Nj.build "python" ~inputs ~implicit_in ~outputs:[!Var.target ^ ".py"] )
  in
  let ocamlopt =
    let obj =
      Nj.build "ocaml-object"
        ~inputs:[!Var.target ^ ".ml"]
        ~implicit_in:(!Var.catala_exe :: List.map module_target modules)
        ~outputs:
          (List.map (( ^ ) !Var.target) [".mli"; ".cmi"; ".cmo"; ".cmx"; ".o"])
        ~vars:[Var.includes, [!Var.includes]]
    in
    match item.module_def with
    | Some _ ->
      [
        obj;
        Nj.build "ocaml-module"
          ~inputs:[!Var.target ^ ".cmx"]
          ~outputs:[!Var.target ^ ".cmxs"];
      ]
    | None -> [obj]
  in
  let cc =
    Nj.build "c-object"
      ~inputs:[!Var.target ^ ".c"]
      ~implicit_in:
        (!Var.catala_exe
        :: (!Var.target ^ ".h")
        :: List.map (modfile ".h" ~mod_ext:"@c-module") modules)
      ~outputs:[!Var.target ^ ".c.o"]
      ~vars:[Var.includes, [!Var.includes]]
  in
  let expose_module =
    (* Note: these rules define global (top-level) aliases for module targets of
       modules that are in include-dirs, so that Ninja can find them from
       wherever; they are only in implicit-in, because once they are built the
       compilers will find them independently through their '-I' arguments.

       This works but it might make things simpler to resolve these aliases at
       the Clerk level ; this would force an initial scan of the included dirs
       but then we could use the already resolved target files directly and get
       rid of these aliases. *)
    match item.module_def with
    | Some m when List.mem (dirname src) include_dirs ->
      Nj.build "phony" ~outputs:[m ^ "@src"] ~inputs:[!Var.builddir / !Var.src]
      ::
      (if List.mem OCaml enabled_backends then
         [
           Nj.build "phony"
             ~outputs:[m ^ "@ml-module"]
             ~inputs:[module_target !Var.target];
         ]
       else [])
      @ (if List.mem C enabled_backends then
           [
             Nj.build "phony"
               ~outputs:[m ^ "@c-module"]
               ~inputs:[modfile ".h" !Var.target; modfile ".c.o" !Var.target];
             Nj.build "phony"
               ~outputs:[m ^ "@h-module"]
               ~inputs:[modfile ".h" !Var.target];
           ]
         else [])
      @
      if List.mem Python enabled_backends then
        [
          Nj.build "phony"
            ~outputs:[m ^ "@py-module"]
            ~inputs:[modfile ".py" !Var.target];
        ]
      else []
    | _ -> []
  in
  let legacy_test_reference test =
    (src /../ "output" / Filename.basename src) -.- test.Scan.id
  in
  let tests =
    let out_tests_references =
      List.map (fun test -> legacy_test_reference test) item.legacy_tests
    in
    let out_tests_prepare =
      List.map
        (fun f -> Nj.build "copy" ~inputs:[f] ~outputs:[!Var.builddir / f])
        out_tests_references
    in
    let tests =
      if (not item.has_inline_tests) && item.legacy_tests = [] then []
      else
        [
          Nj.build "tests" ~inputs:[catala_src]
            ~implicit_in:
              ((!Var.clerk_exe :: List.map (modfile "@ml-module") modules)
              @ List.map (( / ) !Var.builddir) out_tests_references)
            ~outputs:[catala_src ^ "@test"; catala_src ^ "@out"]
            ~implicit_out:
              (List.map
                 (fun o -> (!Var.builddir / o) ^ "@out")
                 out_tests_references);
        ]
    in
    out_tests_prepare @ tests
  in
  let statements_list =
    Seq.return (Nj.comment "")
    :: List.to_seq def_vars
    :: Seq.return include_deps
    :: List.to_seq expose_module
    ::
    (if List.mem OCaml enabled_backends then
       [Option.to_seq module_deps; Seq.return ocaml; List.to_seq ocamlopt]
     else [])
    @ (if List.mem C enabled_backends then [c; Seq.return cc] else [])
    @ (if List.mem Python enabled_backends then [Seq.return python] else [])
    @ if List.mem Tests enabled_backends then [List.to_seq tests] else []
  in
  Seq.concat (List.to_seq statements_list)

let gen_build_statements_dir
    (include_dirs : string list)
    (enabled_backends : backend list)
    (autotest : bool)
    (items : Scan.item list) : Nj.ninja =
  let same_dir_modules =
    List.filter_map
      (fun item ->
        Option.map (fun name -> name, item.Scan.file_name) item.Scan.module_def)
      items
  in
  Seq.flat_map
    (gen_build_statements include_dirs enabled_backends autotest
       same_dir_modules)
    (List.to_seq items)

let dir_test_rules dir subdirs enabled_backends items =
  let open File in
  if List.mem Tests enabled_backends then
    let inputs =
      List.rev_append
        (List.rev_map (fun s -> (Var.(!builddir) / s) ^ "@test") subdirs)
        (List.filter_map
           (fun item ->
             if item.Scan.legacy_tests = [] && not item.Scan.has_inline_tests
             then None
             else Some ((Var.(!builddir) / item.Scan.file_name) ^ "@test"))
           items)
    in
    List.to_seq
      [
        Nj.Comment "";
        Nj.build "dir-tests"
          ~outputs:[(Var.(!builddir) / dir) ^ "@test"]
          ~inputs
          ~vars:[Var.test_id, [dir]];
      ]
  else Seq.empty

let build_statements include_dirs enabled_backends autotest dir =
  Scan.tree dir
  |> Seq.map
     @@ fun (dir, subdirs, items) ->
     ( items,
       Seq.append
         (gen_build_statements_dir include_dirs enabled_backends autotest items)
         (dir_test_rules dir subdirs enabled_backends items) )

let ( @+ ) = Seq.append

let gen_ninja_file
    catala_exe
    catala_flags
    build_dir
    include_dirs
    vars_override
    test_flags
    enabled_backends
    autotest
    dir =
  let var_bindings =
    base_bindings catala_exe catala_flags build_dir include_dirs vars_override
      test_flags enabled_backends autotest
  in
  let prologue =
    Seq.return
      (Nj.Comment (Printf.sprintf "File generated by Clerk v.%s\n" version))
    @+ Seq.return (Nj.Comment "- Global variables - #\n")
    @+ List.to_seq var_bindings
    @+ Seq.return (Nj.Comment "\n- Base rules - #\n")
    @+ List.to_seq (static_base_rules enabled_backends)
    @+ Seq.return (Nj.build "phony" ~outputs:["always"])
    @+ Seq.return (Nj.Comment "\n- Project-specific build statements - #")
  in
  let items, main =
    Seq.unzip (build_statements include_dirs enabled_backends autotest dir)
  in
  ( (prologue
    @+ Seq.concat main
    @+
    if List.mem Tests enabled_backends then
      Seq.return (Nj.build "phony" ~outputs:["test"] ~inputs:[".@test"])
    else Seq.empty),
    Seq.flat_map List.to_seq items,
    List.filter_map (function Nj.Binding b -> Some b | _ -> None) var_bindings )

(** {1 Driver} *)

(* Last argument is a continuation taking as arguments the enabled backends,
   [build_dir], the [fix_path] function, and the ninja file name *)
let ninja_init
    ~config_file
    ~catala_exe
    ~catala_opts
    ~autotest
    ~build_dir
    ~include_dirs
    ~vars_override
    ~color
    ~debug
    ~ninja_output :
    enabled_backends:backend list ->
    extra:def Seq.t ->
    test_flags:string list ->
    (build_dir:File.t ->
    fix_path:(File.t -> File.t) ->
    nin_file:File.t ->
    items:Scan.item Seq.t ->
    var_bindings:Nj.Binding.t list ->
    'a) ->
    'a =
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
        | None -> Fun.id, Clerk_config.default_config))
    | Some f ->
      let root = Filename.dirname f in
      let config = Clerk_config.read f in
      set_root_dir root;
      ( (fun d ->
          let r = Catala_utils.File.reverse_path ~from_dir ~to_dir:root d in
          Message.debug "%a => %a" File.format d File.format r;
          r),
        config )
  in
  let build_dir =
    let dir =
      match build_dir with None -> config.global.build_dir | Some dir -> dir
    in
    Poll.build_dir ~dir ()
  in
  let catala_opts = config.global.catala_opts @ catala_opts in
  let include_dirs = config.global.include_dirs @ include_dirs in
  let with_ninja_output k =
    match ninja_output with
    | Some f -> k f
    | None when debug -> k File.(build_dir / "clerk.ninja")
    | None -> File.with_temp_file "clerk_build_" ".ninja" k
  in
  fun ~enabled_backends ~extra ~test_flags k ->
    Message.debug "building ninja rules...";
    let enabled_backends =
      if autotest && not (List.mem OCaml enabled_backends) then
        OCaml :: enabled_backends
      else enabled_backends
    in
    let build_dir =
      match test_flags with
      | [] -> build_dir
      | flags -> File.((build_dir / "test") ^ String.concat "" flags)
    in
    with_ninja_output
    @@ fun nin_file ->
    let items, var_bindings =
      File.with_formatter_of_file nin_file (fun nin_ppf ->
          let ninja_contents, build_items, var_bindings =
            gen_ninja_file catala_exe catala_opts build_dir include_dirs
              vars_override test_flags enabled_backends autotest "."
          in
          let ninja_contents =
            ninja_contents
            @+ Seq.return (Nj.comment "\n - Command-specific targets - #")
            @+ extra
          in
          Nj.format nin_ppf ninja_contents;
          build_items, var_bindings)
    in
    k ~build_dir ~fix_path ~nin_file ~items ~var_bindings

let cleaned_up_env () =
  let passthrough_vars =
    ["CATALA_BIN="; "CATALA_INCLUDE="; "CATALA_TEST_FLAGS="]
  in
  Unix.environment ()
  |> Array.to_seq
  |> Seq.filter (fun s ->
         (not (String.starts_with ~prefix:"CATALA_" s))
         || List.exists
              (fun prefix -> String.starts_with ~prefix s)
              passthrough_vars
         ||
         (Message.warning "Ignoring environment variable %s" s;
          false))
  |> Array.of_seq

let ninja_cmdline ninja_flags nin_file targets =
  (ninja_exec :: "-f" :: nin_file :: ninja_flags)
  @ (if Catala_utils.Global.options.debug then ["-v"] else [])
  @ targets

let run_command ~clean_up_env ?(setenv = []) cmdline =
  if cmdline = [] then 0
  else
    let cmd = List.hd cmdline in
    let env = if clean_up_env then cleaned_up_env () else Unix.environment () in
    let env =
      let cut s =
        let i = String.index s '=' in
        String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)
      in
      env
      |> Array.to_seq
      |> Seq.map cut
      |> String.Map.of_seq
      |> String.Map.add_seq (List.to_seq setenv)
      |> String.Map.to_seq
      |> Seq.map (fun (var, value) -> var ^ "=" ^ value)
      |> Array.of_seq
    in
    let npid =
      Unix.create_process_env cmd (Array.of_list cmdline) env Unix.stdin
        Unix.stdout Unix.stderr
    in
    let return_code =
      match Unix.waitpid [] npid with
      | _, Unix.WEXITED n -> n
      | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> 128 - n
    in
    return_code

open Cmdliner

let target_backend t =
  let aux = function
    | ".c" | ".h" -> C
    | ".ml" | ".mli" | ".cmi" | ".cmo" | ".cmx" | ".cmxs" -> OCaml
    | ".py" -> Python
    | ".catala_en" | ".catala_fr" | ".catala_pl" -> OCaml
    | "" -> Message.error "Target without extension: @{<red>%S@}" t
    | ext ->
      Message.error
        "Unhandled extension @{<red;bold>%s@}} for target @{<red>%S@}" ext t
  in
  match Filename.extension t with
  | ".exe" -> (
    match Filename.extension (Filename.chop_extension t) with
    | "" -> OCaml
    | e -> aux e)
  | ext -> aux ext

let original_cwd = Sys.getcwd ()

let build_cmd =
  let run ninja_init (targets : string list) (ninja_flags : string list) =
    let enabled_backends =
      targets |> List.map target_backend |> List.sort_uniq compare
    in
    ninja_init ~enabled_backends ~extra:Seq.empty ~test_flags:[]
    @@ fun ~build_dir ~fix_path ~nin_file ~items:_ ~var_bindings:_ ->
    if targets <> [] then (
      let ninja_targets, _clerk_targets =
        List.partition_map
          (fun t0 ->
            let t = fix_path t0 in
            let t =
              if String.starts_with ~prefix:(build_dir ^ "/") t then t
              else File.(build_dir / t)
            in
            match Filename.extension t with
            | ".ml" | ".mli" | ".cmi" | ".cmo" | ".cmx" | ".cmxs" -> Left t
            | ".c" | ".h" -> Left t (* .o ? *)
            | ".py" -> Left t
            | ".catala_en" | ".catala_fr" | ".catala_pl" -> Left t
            | ".exe" -> Right t
            | _ -> assert false (* @out @test *))
          targets
      in
      let ninja_cmd = ninja_cmdline ninja_flags nin_file ninja_targets in
      Message.debug "executing '%s'..." (String.concat " " ninja_cmd);
      let exit_code = run_command ~clean_up_env:false ninja_cmd in
      if exit_code = 0 then
        Message.result
          "@[<v 4>Build successful. The targets can be found in the following \
           files:@,\
           %a@]"
          (Format.pp_print_list (fun ppf f ->
               Format.fprintf ppf "@{<cyan>%s@}"
                 (String.remove_prefix
                    ~prefix:File.(original_cwd / "")
                    File.(Sys.getcwd () / f))))
          ninja_targets;
      raise (Catala_utils.Cli.Exit_with exit_code))
    else
      (* List targets command TODO *)
      let ninja_cmd = ninja_cmdline ninja_flags nin_file ["-t"; "targets"] in
      let result = File.process_out (List.hd ninja_cmd) (List.tl ninja_cmd) in
      let targets =
        String.split_on_char '\n' result
        |> List.filter_map (fun line ->
               match String.split_on_char ':' line with
               | [] | [""] | "always" :: _ -> None
               | target :: _ -> Some (String.trim target))
      in
      Format.eprintf "Available targets:@.";
      List.iter print_endline targets;
      0
  in
  let doc =
    "Base build command for individual file targets. Given the corresponding \
     Catala module is declared, this can be used to build .ml, .cmxs, .c, .py \
     files, etc. Targets, along with their dependencies, are always written \
     into $(i,build-dir) (by default $(b,_build)). If a catala extension is \
     used as target, this compiles all its dependencies."
  in
  Cmd.v (Cmd.info ~doc "build")
    Term.(
      const run $ Cli.Global.term ninja_init $ Cli.targets $ Cli.ninja_flags)

let raw_cmd =
  let run ninja_init (targets : string list) (ninja_flags : string list) =
    ninja_init ~enabled_backends:all_backends ~extra:Seq.empty ~test_flags:[]
    @@ fun ~build_dir:_ ~fix_path ~nin_file ~items:_ ~var_bindings:_ ->
    if targets <> [] then (
      let targets =
        List.map
          (fun f ->
            if String.exists (function '/' | '.' -> true | _ -> false) f then
              fix_path f
            else f)
          targets
      in
      let ninja_cmd = ninja_cmdline ninja_flags nin_file targets in
      Message.debug "executing '%s'..." (String.concat " " ninja_cmd);
      raise
        (Catala_utils.Cli.Exit_with (run_command ~clean_up_env:false ninja_cmd)))
    else
      (* List targets command *)
      let ninja_cmd = ninja_cmdline ninja_flags nin_file ["-t"; "targets"] in
      let result = File.process_out (List.hd ninja_cmd) (List.tl ninja_cmd) in
      let targets =
        String.split_on_char '\n' result
        |> List.filter_map (fun line ->
               match String.split_on_char ':' line with
               | [] | [""] | "always" :: _ -> None
               | target :: _ -> Some (String.trim target))
      in
      Format.eprintf "Available targets:@.";
      List.iter print_endline targets;
      0
  in
  let doc =
    "Low-level build command: can be used to forward build targets or options \
     directly to Ninja. Without a target argument, lists all available raw \
     targets to stdout."
  in
  Cmd.v
    (Cmd.info ~doc "raw-target")
    Term.(
      const run $ Cli.Global.term ninja_init $ Cli.targets $ Cli.ninja_flags)

let set_report_verbosity = function
  | `Summary -> Clerk_report.set_display_flags ~files:`None ~tests:`None ()
  | `Short ->
    Clerk_report.set_display_flags ~files:`Failed ~tests:`Failed ~diffs:false ()
  | `Failures ->
    if Global.options.debug then Clerk_report.set_display_flags ~files:`All ()
  | `Verbose -> Clerk_report.set_display_flags ~files:`All ~tests:`All ()

let test_cmd =
  let run
      ninja_init
      (files_or_folders : string list)
      (reset_test_outputs : bool)
      (test_flags : string list)
      verbosity
      xml
      (diff_command : string option option)
      (ninja_flags : string list) =
    set_report_verbosity verbosity;
    Clerk_report.set_display_flags ~diff_command ();
    ninja_init ~enabled_backends:[OCaml; Tests] ~extra:Seq.empty ~test_flags
    @@ fun ~build_dir ~fix_path ~nin_file ~items:_ ~var_bindings:_ ->
    let targets =
      let fs = if files_or_folders = [] then ["."] else files_or_folders in
      List.map File.(fun f -> (build_dir / fix_path f) ^ "@test") fs
    in
    let ninja_cmd = ninja_cmdline ninja_flags nin_file targets in
    Message.debug "executing '%s'..." (String.concat " " ninja_cmd);
    match run_command ~clean_up_env:true ninja_cmd with
    | 0 ->
      Message.debug "gathering test results...";
      let open Clerk_report in
      let reports = List.flatten (List.map read_many targets) in
      if reset_test_outputs then
        let () =
          if xml then
            Message.error
              "Options @{<bold>--xml@} and @{<bold>--reset@} are incompatible";
          let ppf = Message.formatter_of_out_channel stdout () in
          match List.filter (fun f -> f.successful < f.total) reports with
          | [] ->
            Format.fprintf ppf
              "[@{<green>DONE@}] All tests passed, nothing to reset@."
          | need_reset ->
            List.iter
              (fun f ->
                let files =
                  List.fold_left
                    (fun files t ->
                      if t.success then files
                      else
                        File.Map.add (fst t.result).Lexing.pos_fname
                          (String.remove_prefix
                             ~prefix:File.(build_dir / "")
                             (fst t.expected).Lexing.pos_fname)
                          files)
                    File.Map.empty f.tests
                in
                File.Map.iter
                  (fun result expected -> File.copy ~src:result ~dst:expected)
                  files)
              need_reset;
            Format.fprintf ppf
              "[@{<green>DONE@}] @{<yellow;bold>%d@} test files were \
               @{<yellow>RESET@}@."
              (List.length need_reset)
        in
        raise (Catala_utils.Cli.Exit_with 0)
      else if (if xml then print_xml else summary) ~build_dir reports then
        raise (Catala_utils.Cli.Exit_with 0)
      else raise (Catala_utils.Cli.Exit_with 1)
    | 1 -> raise (Catala_utils.Cli.Exit_with 10) (* Ninja build failed *)
    | err -> raise (Catala_utils.Cli.Exit_with err)
    (* Other Ninja error ? *)
  in
  let doc =
    "Scan the given files or directories for catala tests, build their \
     requirement and run them all. With $(b,--reset) the expected results are \
     updated in-place ; otherwise, 0 is returned if the output matches the \
     reference, or 1 is returned and a diff is printed to stdout"
  in
  Cmd.v (Cmd.info ~doc "test")
    Term.(
      const run
      $ Cli.Global.term ninja_init
      $ Cli.files_or_folders
      $ Cli.reset_test_outputs
      $ Cli.test_flags
      $ Cli.report_verbosity
      $ Cli.report_xml
      $ Cli.diff_command
      $ Cli.ninja_flags)

let rec get_var =
  (* replaces ${var} with its value, recursively *)
  let re_single_var, re_var =
    let open Re in
    let re = seq [str "${"; group (rep1 (diff any (char '}'))); char '}'] in
    compile (whole_string re), compile re
  in
  fun var_bindings v ->
    let s = List.assoc v var_bindings in
    let get_var = get_var (List.remove_assoc v var_bindings) in
    List.concat_map
      (fun s ->
        match Re.exec_opt re_single_var s with
        | Some g -> get_var (Var.make (Re.Group.get g 1))
        | None ->
          [
            Re.replace ~all:true re_var
              ~f:(fun g ->
                String.concat " " (get_var (Var.make (Re.Group.get g 1))))
              s;
          ])
      s

let ninja_version =
  lazy
    (try
       File.process_out
         ~check_exit:(function 0 -> () | _ -> raise Exit)
         ninja_exec ["--version"]
       |> String.split_on_char '.'
       |> List.map int_of_string
     with Exit | Failure _ -> [])

let run_cmd =
  let run
      ninja_init
      (files_or_folders : File.t list)
      backend
      cmd
      ignore_modules
      (scope : string option)
      (ninja_flags : string list) =
    let enabled_backends =
      match backend with
      | `Interpret | `OCaml -> [OCaml]
      | `C -> [C]
      | `Python -> [Python]
    in
    let open File in
    ninja_init ~enabled_backends ~extra:Seq.empty ~test_flags:[]
    @@ fun ~build_dir ~fix_path ~nin_file ~items ~var_bindings ->
    Message.debug "Enabled backends: %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string)
      (List.map
         (function
           | OCaml -> "OCaml" | C -> "C" | Python -> "Python" | Tests -> "Tests")
         enabled_backends);
    let items = List.of_seq items in
    let target_items =
      List.concat_map
        (fun file ->
          let ffile = fix_path file in
          let is_dir = try Sys.is_directory ffile with Sys_error _ -> false in
          let filter item =
            if is_dir then
              item.Scan.module_def = None
              && String.starts_with ~prefix:(ffile / "") item.file_name
            else
              item.Scan.module_def = Some file
              || item.Scan.file_name = ffile
              || Filename.remove_extension item.file_name = ffile
          in
          match List.filter filter items with
          | [] ->
            Message.error "No source file or module matching %a found" format
              file
          | targets ->
            if ignore_modules then
              List.filter (fun item -> item.Scan.module_def = None) targets
            else targets)
        files_or_folders
    in
    if target_items = [] then Message.error "Nothing to run";
    let modules =
      List.fold_left
        (fun acc it ->
          match it.Scan.module_def with
          | Some m -> String.Map.add m it acc
          | None -> acc)
        String.Map.empty items
    in
    let rem_dups =
      let rec aux seen = function
        | it :: r ->
          if String.Set.mem it.Scan.file_name seen then aux seen r
          else it :: aux (String.Set.add it.Scan.file_name seen) r
        | [] -> []
      in
      aux String.Set.empty
    in
    let link_deps item =
      let rec traverse acc item =
        List.fold_left
          (fun acc m ->
            if List.exists (fun it -> it.Scan.module_def = Some m) acc then acc
            else
              let it = String.Map.find m modules in
              traverse (it :: acc) it)
          acc item.Scan.used_modules
      in
      rem_dups (traverse [] item)
      (* |> fun r -> Message.debug "%a => %a" File.format item.Scan.file_name
         (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf i ->
         File.format ppf i.Scan.file_name)) r; r *)
    in
    let make_target item =
      let open File in
      let f =
        match item.Scan.module_def with
        | None -> item.Scan.file_name
        | Some m ->
          (dirname item.Scan.file_name / m)
          ^ Filename.extension item.Scan.file_name
      in
      let base =
        match backend with
        | `Interpret -> item.Scan.file_name
        | `OCaml -> f -.- "cmx"
        | `C -> f -.- "c.o"
        | `Python -> f -.- "py"
      in
      build_dir / base
    in
    let base_targets = List.map (fun it -> it, make_target it) target_items in
    let multi_targets =
      match base_targets with _ :: _ :: _ -> true | _ -> false
    in
    let ninja_cmd =
      let ninja_targets =
        List.fold_left
          (fun acc (it, t) ->
            if String.Set.mem t acc then acc
            else
              String.Set.add t
              @@ List.fold_left
                   (fun acc it -> String.Set.add (make_target it) acc)
                   acc (link_deps it))
          String.Set.empty base_targets
      in
      let flags =
        if Lazy.force ninja_version >= [1; 12] then "--quiet" :: ninja_flags
        else ninja_flags
      in
      ninja_cmdline flags nin_file (String.Set.elements ninja_targets)
    in
    Message.debug "Building dependencies: '%s'..." (String.concat " " ninja_cmd);
    let exit_code = run_command ~clean_up_env:false ninja_cmd in
    if exit_code <> 0 then raise (Catala_utils.Cli.Exit_with exit_code);
    let get_var = get_var var_bindings in
    let run_cmd item target =
      match backend with
      | `Interpret ->
        let catala_flags =
          get_var Var.catala_flags
          @
          match scope with
          | None -> []
          | Some s -> [Printf.sprintf "--scope=%s" s]
        in
        get_var Var.catala_exe @ [cmd; target] @ catala_flags
      | `OCaml ->
        get_var Var.ocamlopt_exe
        @ get_var Var.ocaml_flags
        @ get_var Var.runtime_ocaml_libs
        @ List.map
            (fun it ->
              build_dir
              / Filename.dirname it.Scan.file_name
              / Option.get it.Scan.module_def
              ^ ".cmx")
            (link_deps item)
        @ [target; "-o"; target -.- "exe"]
      | `C ->
        get_var Var.cc_exe
        @ List.map
            (fun it ->
              build_dir
              / Filename.dirname it.Scan.file_name
              / Option.get it.Scan.module_def
              ^ ".c.o")
            (link_deps item)
        @ [target]
        @ get_var Var.c_flags
        @ ["-o"; target -.- "exe"]
      | `Python ->
        (* a "linked" python module is a "Module.py" folder containing the
           module .py file along with the runtime and all dependencies, plus a
           __init__.py file *)
        let tdir = Filename.remove_extension target ^ "_python" in
        remove tdir;
        ensure_dir tdir;
        List.iter
          (fun it ->
            let src =
              build_dir
              / Filename.dirname it.Scan.file_name
              / Option.get it.Scan.module_def
              ^ ".py"
            in
            copy_in ~src ~dir:tdir)
          (link_deps item);
        copy_in ~src:(target -.- "py") ~dir:tdir;
        close_out (open_out (tdir / "__init__.py"));
        []
    in
    let run_artifact src =
      match backend with
      | `Interpret -> 0
      | `OCaml ->
        let cmd = (src -.- "exe") :: Option.to_list scope in
        Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
        run_command ~clean_up_env:false cmd
      | `C ->
        let cmd =
          (src -.- "exe")
          :: Option.to_list scope (* NOTE: not handled yet by the backend *)
        in
        Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
        run_command ~clean_up_env:false cmd
      | `Python ->
        let cmd =
          let base = Filename.(basename (remove_extension src)) in
          get_var Var.python @ ["-m"; base ^ "_python." ^ base]
        in
        let pythonpath =
          String.concat ":"
            ((File.dirname src :: get_var Var.runtime_python_dir)
            @ [Option.value ~default:"" (Sys.getenv_opt "PYTHONPATH")])
        in
        Message.debug "Executing artifact: 'PYTHONPATH=%s %s'..." pythonpath
          (String.concat " " cmd);
        run_command ~clean_up_env:false ~setenv:["PYTHONPATH", pythonpath] cmd
    in
    let cmd_exit_code =
      List.fold_left
        (fun code (item, target) ->
          if multi_targets then
            Format.fprintf (Message.err_ppf ()) "@{<blue>>@} @{<cyan>%s@}@."
              (String.remove_prefix
                 ~prefix:File.(original_cwd / "")
                 File.(
                   Sys.getcwd ()
                   / String.remove_prefix ~prefix:(build_dir ^ "/") target
                   -.- ""));
          let cmd = run_cmd item target in
          Message.debug "Running command: '%s'..." (String.concat " " cmd);
          match run_command ~clean_up_env:false cmd with
          | 0 -> max code (run_artifact target)
          | n -> max code n)
        0 base_targets
    in
    raise (Catala_utils.Cli.Exit_with cmd_exit_code)
  in
  let doc =
    "Runs the Catala interpreter on the given files, after building their \
     dependencies. The scope to be executed can be specified using the $(i,-s) \
     option."
  in
  Cmd.v (Cmd.info ~doc "run")
    Term.(
      const run
      $ Cli.Global.term ninja_init
      $ Cli.files_or_folders
      $ Cli.backend
      $ Cli.run_command
      $ Cli.ignore_modules
      $ Cli.scope
      $ Cli.ninja_flags)

let runtest_cmd =
  let run catala_exe catala_opts include_dirs test_flags report out file =
    let catala_opts =
      List.fold_left
        (fun opts dir -> "-I" :: dir :: opts)
        catala_opts include_dirs
    in
    let test_flags = List.filter (( <> ) "") test_flags in
    Clerk_runtest.run_tests
      ~catala_exe:(Option.value ~default:"catala" catala_exe)
      ~catala_opts ~test_flags ~report ~out file;
    0
  in
  let doc =
    "Mainly for internal purposes. Runs inline tests from a Catala file, and \
     outputs their results to stdout"
  in
  Cmd.v (Cmd.info ~doc "runtest")
    Term.(
      const run
      $ Cli.catala_exe
      $ Cli.catala_opts
      $ Cli.include_dirs
      $ Cli.test_flags
      $ Cli.runtest_report
      $ Cli.runtest_out
      $ Cli.single_file)

let report_cmd =
  let run color debug verbosity xml diff_command build_dir files =
    let _options = Catala_utils.Global.enforce_options ~debug ~color () in
    let build_dir = Option.value ~default:"_build" build_dir in
    set_report_verbosity verbosity;
    Clerk_report.set_display_flags ~diff_command ();
    let open Clerk_report in
    let tests = List.flatten (List.map read_many files) in
    let success = (if xml then print_xml else summary) ~build_dir tests in
    exit (if success then 0 else 1)
  in
  let doc =
    "Mainly for internal purposes. Reads a test report file and displays a \
     summary of the results, returning 0 on success and 1 if any test failed."
  in
  Cmd.v (Cmd.info ~doc "report")
    Term.(
      const run
      $ Cli.Global.color
      $ Cli.Global.debug
      $ Cli.report_verbosity
      $ Cli.report_xml
      $ Cli.diff_command
      $ Cli.build_dir
      $ Cli.files)

let list_vars_cmd =
  let run catala_exe catala_flags build_dir include_dirs =
    let build_dir = Option.value ~default:"_build" build_dir in
    let var_bindings =
      base_bindings catala_exe catala_flags build_dir include_dirs [] []
        all_backends false
      |> List.filter_map (function Nj.Binding b -> Some b | _ -> None)
    in
    Format.eprintf "Defined variables:@.";
    Format.open_vbox 0;
    String.Map.iter
      (fun s v ->
        Format.printf "%s=%S@," s
          (String.concat " " (List.assoc v var_bindings)))
      Var.all_vars;
    Format.close_box ();
    0
  in
  let doc =
    "List pre-defined build variables that can be overriden using the \
     $(i,--var) flag"
    (* TODO: or the clerk.toml file *)
  in
  Cmd.v
    (Cmd.info ~doc "list-vars")
    Term.(
      const run
      $ Cli.catala_exe
      $ Cli.catala_opts
      $ Cli.build_dir
      $ Cli.include_dirs)

let main_cmd =
  Cmd.group Cli.info
    [
      build_cmd;
      test_cmd;
      run_cmd;
      runtest_cmd;
      report_cmd;
      raw_cmd;
      list_vars_cmd;
    ]

let main () =
  try exit (Cmdliner.Cmd.eval' ~catch:false main_cmd) with
  | Catala_utils.Cli.Exit_with n -> exit n
  | Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit content Error;
    if Catala_utils.Global.options.debug then
      Printexc.print_raw_backtrace stderr bt;
    exit Cmd.Exit.some_error
  | Message.CompilerErrors contents ->
    List.iter (fun c -> Message.Content.emit c Error) contents;
    exit Cmd.Exit.some_error
  | Sys_error msg ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit
      (Message.Content.of_string ("System error: " ^ msg))
      Error;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    exit Cmd.Exit.internal_error
  | e ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit
      (Message.Content.of_string ("Unexpected error: " ^ Printexc.to_string e))
      Error;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    exit Cmd.Exit.internal_error
