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

open Catala_utils
open Ninja_utils
module Nj = Ninja_utils
module Scan = Clerk_scan

(* Version is synchronised with the catala version *)
let version = Catala_utils.Cli.version

(** {1 Command line interface} *)

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
          ~doc:"Option to pass to the Catala compiler. Can be repeated.")

  let build_dir =
    Arg.(
      value
      & opt (some string) None
      & info ["build-dir"] ~docv:"DIR"
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
      & opt (list string) []
      & info ["test-flags"] ~docv:"FLAGS"
          ~env:(Cmd.Env.info "CATALA_TEST_FLAGS")
          ~doc:
            "Flags to pass to the catala interpreter on $(b,catala test-scope) \
             tests. Comma-separated list. A subset may also be applied to the \
             compilation of modules, as needed.\n\
             WARNING: flag shortcuts are not allowed here (i.e. don't use \
             non-ambiguous prefixes such as $(b,--avoid-ex) for \
             $(b,--avoid-exceptions))\n\
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

  module Global : sig
    val color : Catala_utils.Global.when_enum Term.t
    val debug : bool Term.t

    val term :
      (chdir:File.t option ->
      catala_exe:File.t option ->
      catala_opts:string list ->
      build_dir:File.t option ->
      include_dirs:string list ->
      color:Global.when_enum ->
      debug:bool ->
      ninja_output:File.t option ->
      'a) ->
      'a Term.t
  end = struct
    let chdir =
      Arg.(
        value
        & opt (some string) None
        & info ["C"] ~docv:"DIR"
            ~doc:"Change to the given directory before processing")

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
            chdir
            catala_exe
            catala_opts
            build_dir
            include_dirs
            color
            debug
            ninja_output
          ->
            f ~chdir ~catala_exe ~catala_opts ~build_dir ~include_dirs ~color
              ~debug ~ninja_output)
        $ chdir
        $ catala_exe
        $ catala_opts
        $ build_dir
        $ include_dirs
        $ color
        $ debug
        $ ninja_output)
  end

  let files_or_folders =
    Arg.(
      value
      & pos_all string []
      & info [] ~docv:"FILE(S)" ~doc:"File(s) or folder(s) to process")

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
      required
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

  let use_patdiff =
    Arg.(
      value
      & flag
      & info ["patdiff"]
          ~env:(Cmd.Env.info "CATALA_USE_PATDIFF")
          ~doc:
            "Enable use of the 'patdiff' command for showing test failure \
             details (no effect if the command is not available)")

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
  let project_root_absrel : (File.t option * File.t) Lazy.t =
    lazy
      (let open File in
       let home = try Sys.getenv "HOME" with Not_found -> "" in
       let rec lookup dir rel =
         if
           Sys.file_exists (dir / "catala.opam")
           || Sys.file_exists (dir / ".git")
           || Sys.file_exists (dir / "clerk.toml")
         then Some dir, rel
         else if dir = home then None, Filename.current_dir_name
         else
           let parent = Filename.dirname dir in
           if parent = dir then None, Filename.current_dir_name
           else lookup parent (rel / Filename.parent_dir_name)
       in
       lookup (Sys.getcwd ()) Filename.current_dir_name)

  let project_root = lazy (fst (Lazy.force project_root_absrel))
  let project_root_relative = lazy (snd (Lazy.force project_root_absrel))

  (** Scans for a parent directory being the root of the Catala source repo *)
  let catala_project_root : File.t option Lazy.t =
    lazy
      (match Lazy.force project_root with
      | Some root
        when Sys.file_exists File.(root / "catala.opam")
             && Sys.file_exists File.(root / "dune-project") ->
        Some root
      | _ -> None)

  let exec_dir : File.t = Catala_utils.Cli.exec_dir
  let clerk_exe : File.t Lazy.t = lazy (Unix.realpath Sys.executable_name)

  let catala_exe : File.t Lazy.t =
    lazy
      (let f = File.(exec_dir / "catala") in
       if Sys.file_exists f then Unix.realpath f
       else
         match Lazy.force project_root with
         | Some root when Sys.file_exists File.(root / "catala.opam") ->
           Unix.realpath
             File.(root / "_build" / "default" / "compiler" / "catala.exe")
         | _ -> File.check_exec "catala")

  let build_dir : ?dir:File.t -> unit -> File.t =
   fun ?(dir = "_build") () ->
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
end

(* Adjusts paths specified from the command-line relative to the user cwd to be
   instead relative to the project root *)
let fix_path =
  let from_dir = Sys.getcwd () in
  fun d ->
    let to_dir = Lazy.force Poll.project_root_relative in
    Catala_utils.File.reverse_path ~from_dir ~to_dir d

(**{1 Building rules}*)

(** Ninja variable names *)
module Var = struct
  include Nj.Var

  (** Global vars: always defined, at toplevel *)

  let ninja_required_version = make "ninja_required_version"
  let builddir = make "builddir"
  let clerk_exe = make "CLERK_EXE"
  let catala_exe = make "CATALA_EXE"
  let catala_flags = make "CATALA_FLAGS"
  let catala_flags_ocaml = make "CATALA_FLAGS_OCAML"
  let catala_flags_python = make "CATALA_FLAGS_PYTHON"
  let clerk_flags = make "CLERK_FLAGS"
  let ocamlc_exe = make "OCAMLC_EXE"
  let ocamlopt_exe = make "OCAMLOPT_EXE"
  let ocaml_flags = make "OCAML_FLAGS"
  let runtime_ocaml_libs = make "RUNTIME_OCAML_LIBS"

  (** Rule vars, Used in specific rules *)

  let input = make "in"
  let output = make "out"
  let pool = make "pool"
  let src = make "src"
  let orig_src = make "orig-src"
  let scope = make "scope"
  let test_id = make "test-id"
  let ( ! ) = Var.v
end

let base_bindings catala_exe catala_flags build_dir include_dirs test_flags =
  let includes =
    List.fold_right
      (fun dir flags ->
        if Filename.is_relative dir then
          "-I" :: File.(Var.(!builddir) / dir) :: flags
        else "-I" :: dir :: flags)
      include_dirs []
  in
  let catala_flags = ("--directory=" ^ Var.(!builddir)) :: catala_flags in
  let catala_flags_ocaml =
    List.filter
      (function
        | "--avoid-exceptions" | "-O" | "--optimize" | "--closure-conversion" ->
          true | _ -> false)
      test_flags
  in
  let catala_flags_python =
    List.filter
      (function
        | "--avoid-exceptions" | "-O" | "--optimize" | "--closure-conversion" ->
          true
        | _ -> false)
      test_flags
  in
  let ocaml_flags = Lazy.force Poll.ocaml_include_flags in
  [
    Nj.binding Var.ninja_required_version ["1.7"];
    (* use of implicit outputs *)
    Nj.binding Var.builddir [build_dir];
    Nj.binding Var.clerk_exe [Lazy.force Poll.clerk_exe];
    Nj.binding Var.catala_exe
      [
        (match catala_exe with
        | Some e -> File.check_exec e
        | None -> Lazy.force Poll.catala_exe);
      ];
    Nj.binding Var.catala_flags (catala_flags @ includes);
    Nj.binding Var.catala_flags_ocaml catala_flags_ocaml;
    Nj.binding Var.catala_flags_python catala_flags_python;
    Nj.binding Var.clerk_flags
      ("-e"
       :: Var.(!catala_exe)
       :: ("--test-flags=" ^ String.concat "," test_flags)
       :: includes
      @ List.map (fun f -> "--catala-opts=" ^ f) catala_flags);
    Nj.binding Var.ocamlc_exe ["ocamlc"];
    Nj.binding Var.ocamlopt_exe ["ocamlopt"];
    Nj.binding Var.ocaml_flags (ocaml_flags @ includes);
    Nj.binding Var.runtime_ocaml_libs (Lazy.force Poll.ocaml_link_flags);
  ]

let[@ocamlformat "disable"] static_base_rules =
  let open Var in
  let shellout l = Format.sprintf "$$(%s)" (String.concat " " l) in
  [
    Nj.rule "copy"
      ~command:["cp"; "-f"; !input; !output]
      ~description:["<copy>"; !input];

    Nj.rule "catala-ocaml"
      ~command:[!catala_exe; "ocaml"; !catala_flags; !catala_flags_ocaml;
                !input; "-o"; !output]
      ~description:["<catala>"; "ocaml"; "⇒"; !output];

    Nj.rule "ocaml-object"
      ~command:[!ocamlc_exe; "-i"; !ocaml_flags; !input; ">"; !input^"i"; "&&";
                !ocamlc_exe; "-opaque"; !ocaml_flags; !input^"i"; "&&";
                !ocamlc_exe; "-c"; !ocaml_flags; !input; "&&";
                !ocamlopt_exe; "-c"; "-intf-suffix"; ".ml"; !ocaml_flags; !input]
      ~description:["<ocaml>"; "⇒"; !output];

    Nj.rule "ocaml-module"
      ~command:
        [!ocamlopt_exe; "-shared"; !ocaml_flags; !input; "-o"; !output]
      ~description:["<ocaml>"; "⇒"; !output];

    Nj.rule "ocaml-exec"
      ~command: [
        !ocamlopt_exe; !ocaml_flags; !runtime_ocaml_libs;
        shellout [!catala_exe; "depends";
                  "--prefix="^ !builddir; "--extension=cmx";
                  !catala_flags; !orig_src];
        "-o"; !output;
      ]
      ~description:["<ocaml>"; "⇒"; !output];

    Nj.rule "python"
      ~command:[!catala_exe; "python"; !catala_flags; !catala_flags_python;
                !input; "-o"; !output]
      ~description:["<catala>"; "python"; "⇒"; !output];

    Nj.rule "tests"
      ~command:
        [!clerk_exe; "runtest"; !clerk_flags; !input;
         "--report"; !output;]
      ~description:["<catala>"; "tests"; "⇐"; !input];

    Nj.rule "interpret"
      ~command:
        [!catala_exe; "interpret"; !catala_flags; !input; "--scope=" ^ !scope]
      ~description:["<catala>"; "interpret"; !scope; "⇐"; !input]
      ~vars:[pool, ["console"]];

    Nj.rule "dir-tests"
      ~command:["cat"; !input; ">"; !output; ";"]
      ~description:["<test>"; !test_id];
  ]

let gen_build_statements
    (include_dirs : string list)
    (same_dir_modules : (string * File.t) list)
    (item : Scan.item) : Nj.ninja =
  let open File in
  let ( ! ) = Var.( ! ) in
  let src = item.file_name in
  let modules = List.rev item.used_modules in
  let modfile ext modname =
    match List.assoc_opt modname same_dir_modules with
    | Some f -> (!Var.builddir / Filename.remove_extension f) ^ ext
    | None -> modname ^ ext
  in
  let inc x = !Var.builddir / x in
  let modd x = modfile "@module" x in
  let def_src = Nj.binding Var.src [Filename.remove_extension src] in
  let srcv = !Var.src ^ Filename.extension src in
  let include_deps =
    Nj.build "copy" ~inputs:[srcv]
      ~implicit_in:(List.map inc item.included_files @ List.map modd modules)
      ~outputs:[inc srcv]
  in
  let target_file ext = (!Var.builddir / !Var.src) ^ "." ^ ext in
  let module_deps =
    Option.map
      (fun m ->
        Nj.build "phony"
          ~inputs:[inc srcv; target_file "cmi"; target_file "cmxs"]
          ~outputs:[modd m])
      item.module_def
  in
  let ml_file = target_file "ml" in
  let py_file = target_file "py" in
  let ocaml, python =
    if item.extrnal then
      ( Nj.build "copy"
          ~implicit_in:[inc srcv]
          ~inputs:[src -.- "ml"]
          ~outputs:[ml_file],
        Nj.build "copy"
          ~implicit_in:[inc srcv]
          ~inputs:[src -.- "py"]
          ~outputs:[py_file] )
    else
      ( Nj.build "catala-ocaml"
          ~inputs:[inc srcv]
          ~implicit_in:[!Var.catala_exe] ~outputs:[ml_file],
        Nj.build "python"
          ~inputs:[inc srcv]
          ~implicit_in:[!Var.catala_exe] ~outputs:[py_file] )
  in
  let ocamlopt =
    let obj =
      Nj.build "ocaml-object" ~inputs:[ml_file]
        ~implicit_in:(!Var.catala_exe :: List.map modd modules)
        ~outputs:(List.map target_file ["mli"; "cmi"; "cmo"; "cmx"; "o"])
        ~vars:
          [
            ( Var.ocaml_flags,
              !Var.ocaml_flags
              :: "-I"
              :: (!Var.builddir / src /../ "")
              :: List.concat_map
                   (fun d ->
                     [
                       "-I";
                       (if Filename.is_relative d then !Var.builddir / d else d);
                     ])
                   include_dirs );
          ]
    in
    let modexec =
      match item.module_def with
      | Some _ ->
        Nj.build "ocaml-module"
          ~inputs:[target_file "cmx"]
          ~outputs:[target_file "cmxs"]
      | None ->
        Nj.build "ocaml-exec"
          ~inputs:[target_file "cmx"]
          ~outputs:[target_file "exe"]
          ~vars:[Var.orig_src, [inc srcv]]
    in
    [obj; modexec]
  in
  let expose_module =
    match item.module_def with
    | Some m when List.mem (dirname src) include_dirs ->
      Some (Nj.build "phony" ~outputs:[m ^ "@module"] ~inputs:[modd m])
    | _ -> None
  in
  let interp_deps =
    !Var.catala_exe
    :: List.map
         (fun m ->
           match List.assoc_opt m same_dir_modules with
           | Some f -> (!Var.builddir / Filename.remove_extension f) ^ ".cmxs"
           | None -> m ^ "@module")
         modules
  in
  let interpret =
    Nj.build "interpret"
      ~outputs:[srcv ^ "@interpret"]
      ~inputs:[inc srcv]
      ~implicit_in:interp_deps
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
        (fun f -> Nj.build "copy" ~inputs:[f] ~outputs:[inc f])
        out_tests_references
    in
    let tests =
      if (not item.has_inline_tests) && item.legacy_tests = [] then []
      else
        [
          Nj.build "tests"
            ~inputs:[inc srcv]
            ~implicit_in:
              ((!Var.clerk_exe :: interp_deps)
              @ List.map inc out_tests_references)
            ~outputs:[inc srcv ^ "@test"; inc srcv ^ "@out"]
            ~implicit_out:
              (List.map (fun o -> inc o ^ "@out") out_tests_references);
        ]
    in
    out_tests_prepare @ tests
  in
  Seq.concat
  @@ List.to_seq
       [
         Seq.return (Nj.comment "");
         Seq.return def_src;
         Seq.return include_deps;
         Option.to_seq module_deps;
         Option.to_seq expose_module;
         Seq.return ocaml;
         List.to_seq ocamlopt;
         Seq.return python;
         List.to_seq tests;
         Seq.return interpret;
       ]

let gen_build_statements_dir
    (include_dirs : string list)
    (items : Scan.item list) : Nj.ninja =
  let same_dir_modules =
    List.filter_map
      (fun item ->
        Option.map (fun name -> name, item.Scan.file_name) item.Scan.module_def)
      items
  in
  Seq.flat_map
    (gen_build_statements include_dirs same_dir_modules)
    (List.to_seq items)

let dir_test_rules dir subdirs items =
  let open File in
  let inputs =
    List.rev_append
      (List.rev_map (fun s -> (Var.(!builddir) / s) ^ "@test") subdirs)
      (List.filter_map
         (fun item ->
           if item.Scan.legacy_tests = [] && not item.Scan.has_inline_tests then
             None
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

let build_statements include_dirs dir =
  Scan.tree dir
  |> Seq.flat_map
     @@ fun (dir, subdirs, items) ->
     Seq.append
       (gen_build_statements_dir include_dirs items)
       (dir_test_rules dir subdirs items)

let gen_ninja_file catala_exe catala_flags build_dir include_dirs test_flags dir
    =
  let ( @+ ) = Seq.append in
  Seq.return
    (Nj.Comment (Printf.sprintf "File generated by Clerk v.%s\n" version))
  @+ Seq.return (Nj.Comment "- Global variables - #\n")
  @+ List.to_seq
       (base_bindings catala_exe catala_flags build_dir include_dirs test_flags)
  @+ Seq.return (Nj.Comment "\n- Base rules - #\n")
  @+ List.to_seq static_base_rules
  @+ Seq.return (Nj.build "phony" ~outputs:["always"])
  @+ Seq.return (Nj.Comment "\n- Project-specific build statements - #")
  @+ build_statements include_dirs dir
  @+ Seq.return (Nj.build "phony" ~outputs:["test"] ~inputs:[".@test"])

(** {1 Driver} *)

let ninja_init
    ~chdir
    ~catala_exe
    ~catala_opts
    ~build_dir
    ~include_dirs
    ~color
    ~debug
    ~ninja_output :
    extra:def Seq.t -> test_flags:string list -> (File.t -> File.t -> 'a) -> 'a
    =
  let _options = Catala_utils.Global.enforce_options ~debug ~color () in
  let chdir =
    match chdir with None -> Lazy.force Poll.project_root | some -> some
  in
  Option.iter Sys.chdir chdir;
  let build_dir = Poll.build_dir ?dir:build_dir () in
  let with_ninja_output k =
    match ninja_output with
    | Some f -> k f
    | None when debug -> k File.(build_dir / "clerk.ninja")
    | None -> File.with_temp_file "clerk_build_" ".ninja" k
  in
  fun ~extra ~test_flags k ->
    Message.debug "building ninja rules...";
    let build_dir =
      match test_flags with
      | [] -> build_dir
      | flags -> File.((build_dir / "test") ^ String.concat "" flags)
    in
    with_ninja_output
    @@ fun nin_file ->
    File.with_formatter_of_file nin_file (fun nin_ppf ->
        let ninja_contents =
          Seq.concat
          @@ List.to_seq
               [
                 gen_ninja_file catala_exe catala_opts build_dir include_dirs
                   test_flags ".";
                 Seq.return (Nj.comment "\n - Command-specific targets - #");
                 extra;
               ]
        in
        Nj.format nin_ppf ninja_contents);
    k build_dir nin_file

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
  ("ninja" :: "-f" :: nin_file :: ninja_flags)
  @ (if Catala_utils.Global.options.debug then ["-v"] else [])
  @ targets

let run_ninja ~clean_up_env cmdline =
  let cmd = List.hd cmdline in
  let env = if clean_up_env then cleaned_up_env () else Unix.environment () in
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

let build_cmd =
  let run ninja_init (targets : string list) (ninja_flags : string list) =
    ninja_init ~extra:Seq.empty ~test_flags:[]
    @@ fun _build_dir nin_file ->
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
    raise (Catala_utils.Cli.Exit_with (run_ninja ~clean_up_env:false ninja_cmd))
  in
  let doc =
    "Low-level build command: can be used to forward build targets or options \
     directly to Ninja"
  in
  Cmd.v (Cmd.info ~doc "build")
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
      (use_patdiff : bool)
      (ninja_flags : string list) =
    set_report_verbosity verbosity;
    Clerk_report.set_display_flags ~use_patdiff ();
    ninja_init ~extra:Seq.empty ~test_flags
    @@ fun build_dir nin_file ->
    let targets =
      let fs = if files_or_folders = [] then ["."] else files_or_folders in
      List.map File.(fun f -> (build_dir / fix_path f) ^ "@test") fs
    in
    let ninja_cmd = ninja_cmdline ninja_flags nin_file targets in
    Message.debug "executing '%s'..." (String.concat " " ninja_cmd);
    match run_ninja ~clean_up_env:true ninja_cmd with
    | 0 ->
      Message.debug "gathering test results...";
      let open Clerk_report in
      let reports = List.flatten (List.map read_many targets) in
      if reset_test_outputs then
        let () =
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
                  (fun result expected ->
                    Format.kasprintf Sys.command "cp -f %a %a@." File.format
                      result File.format expected
                    |> ignore)
                  files)
              need_reset;
            Format.fprintf ppf
              "[@{<green>DONE@}] @{<yellow;bold>%d@} test files were \
               @{<yellow>RESET@}@."
              (List.length need_reset)
        in
        raise (Catala_utils.Cli.Exit_with 0)
      else if summary ~build_dir reports then
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
      $ Cli.use_patdiff
      $ Cli.ninja_flags)

let run_cmd =
  let run
      ninja_init
      (files_or_folders : string list)
      (scope : string)
      (ninja_flags : string list) =
    let extra =
      Seq.cons
        (Nj.binding Var.scope [scope])
        (Seq.return
           (Nj.default
              (List.map (fun file -> file ^ "@interpret") files_or_folders)))
    in
    ninja_init ~extra ~test_flags:[]
    @@ fun _build_dir nin_file ->
    let ninja_cmd = ninja_cmdline ninja_flags nin_file [] in
    Message.debug "executing '%s'..." (String.concat " " ninja_cmd);
    raise (Catala_utils.Cli.Exit_with (run_ninja ~clean_up_env:false ninja_cmd))
  in
  let doc =
    "Runs the Catala interpreter on the given files, after building their \
     dependencies. The scope to be executed must be specified using the \
     $(i,-s) option."
  in
  Cmd.v (Cmd.info ~doc "run")
    Term.(
      const run
      $ Cli.Global.term ninja_init
      $ Cli.files_or_folders
      $ Cli.scope
      $ Cli.ninja_flags)

let runtest_cmd =
  let run catala_exe catala_opts include_dirs test_flags report out file =
    let catala_opts =
      List.fold_left
        (fun opts dir -> "-I" :: dir :: opts)
        catala_opts include_dirs
    in
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
  let run color debug verbosity use_patdiff build_dir file =
    let _options = Catala_utils.Global.enforce_options ~debug ~color () in
    let build_dir = Option.value ~default:"_build" build_dir in
    set_report_verbosity verbosity;
    Clerk_report.set_display_flags ~use_patdiff ();
    let open Clerk_report in
    let tests = read_many file in
    let success = summary ~build_dir tests in
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
      $ Cli.use_patdiff
      $ Cli.build_dir
      $ Cli.single_file)

let main_cmd =
  Cmd.group Cli.info [build_cmd; test_cmd; run_cmd; runtest_cmd; report_cmd]

let main () =
  try exit (Cmdliner.Cmd.eval' ~catch:false main_cmd) with
  | Catala_utils.Cli.Exit_with n -> exit n
  | Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit content Error;
    if Catala_utils.Global.options.debug then
      Printexc.print_raw_backtrace stderr bt;
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
