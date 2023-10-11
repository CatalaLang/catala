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
      & opt string "_build"
      & info ["build-dir"] ~docv:"DIR"
          ~doc:"Directory where compilation artifacts should be written")

  module Global : sig
    val term :
      (chdir:File.t option ->
      catala_exe:File.t option ->
      catala_opts:string list ->
      color:Cli.when_enum ->
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
        & opt ~vopt:Cli.Always Cli.when_opt Auto
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
        const (fun chdir catala_exe catala_opts color debug ninja_output ->
            f ~chdir ~catala_exe ~catala_opts ~color ~debug ~ninja_output)
        $ chdir
        $ catala_exe
        $ catala_opts
        $ color
        $ debug
        $ ninja_output)
  end

  let files_or_folders =
    Arg.(
      value
      & pos_all file []
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
      | None -> ""
      | Some makeflags ->
        let ignore_rex = Re.(compile @@ word (char 'i')) in
        let has_ignore = Re.execp ignore_rex makeflags in
        let jobs_rex = Re.(compile @@ seq [str "-j"; group (rep digit)]) in
        let number_of_jobs =
          try ["-j" ^ Re.Group.get (Re.exec jobs_rex makeflags) 1]
          with _ -> []
        in
        String.concat " " ((if has_ignore then ["-k0"] else []) @ number_of_jobs)
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
  (** Scans for a parent directory being the root of the Catala source repo *)
  let catala_project_root : File.t option Lazy.t =
    let rec aux dir =
      if Sys.file_exists File.(dir / "catala.opam") then Some dir
      else
        let dir' = File.dirname dir in
        if dir' = dir then None else aux dir'
    in
    lazy (aux (Sys.getcwd ()))

  let exec_dir : File.t =
    (* Do not use Sys.executable_name, which may resolve symlinks: we want the
       original path. (e.g. _build/install/default/bin/foo is a symlink) *)
    Filename.dirname Sys.argv.(0)

  let clerk_exe : File.t Lazy.t = lazy (Unix.realpath Sys.executable_name)

  let catala_exe : File.t Lazy.t =
    lazy
      (let f = File.(exec_dir / "catala") in
       if Sys.file_exists f then Unix.realpath f
       else
         match Lazy.force catala_project_root with
         | Some root ->
           Unix.realpath
             File.(root / "_build" / "default" / "compiler" / "catala.exe")
         | None ->
           Unix.realpath
           @@ File.process_out
                ~check_exit:(function
                  | 0 -> ()
                  | _ ->
                    Message.raise_error
                      "Could not find the @{<yellow>catala@} program, please \
                       fix your installation")
                "/bin/sh"
                ["-c"; "command -v catala"])

  let build_dir : File.t Lazy.t =
    lazy
      (let d = "_build" in
       match Sys.is_directory d with
       | exception Sys_error _ ->
         Sys.mkdir d 0o770;
         d
       | true -> d
       | false ->
         Message.raise_error "Build directory %a exists but is not a directory"
           File.format d)
  (* Note: it could be safer here to use File.(Sys.getcwd () / "_build"), but
     Ninja treats relative and absolute paths separately so that you wouldn't
     then be able to build target _build/foo.ml but would have to write the full
     path every time *)
  (* TODO: probably detect the project root and put this there instead *)

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
             Message.raise_error
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
         Message.emit_debug "Catala runtime libraries found at @{<bold>%s@}."
           dir;
         dir
       | None ->
         Message.raise_error
           "@[<hov>Could not locate the Catala runtime library.@ Make sure \
            that either catala is correctly installed,@ or you are running \
            from the root of a compiled source tree.@]")

  let ocaml_link_flags : string list Lazy.t =
    lazy
      (let link_libs =
         ["yojson"; "ppx_yojson_conv_lib"; "zarith"; "dates_calc"]
       in
       let link_libs_flags =
         List.concat_map
           (fun lib ->
             match File.(check_directory (Lazy.force ocaml_libdir / lib)) with
             | None ->
               Message.raise_error
                 "Required OCaml library not found at %a.@ Try `opam install \
                  %s'"
                 File.format
                 File.(Lazy.force ocaml_libdir / lib)
                 lib
             | Some d ->
               [
                 "-I";
                 d;
                 String.map (function '-' -> '_' | c -> c) lib ^ ".cmxa";
               ])
           link_libs
       in
       let runtime_dir = Lazy.force ocaml_runtime_dir in
       link_libs_flags @ [File.(runtime_dir / "runtime_ocaml.cmxa")])

  let has_command cmd =
    let check_cmd = Printf.sprintf "type %s >/dev/null 2>&1" cmd in
    Sys.command check_cmd = 0

  let diff_command =
    lazy
      (if has_command "patdiff" then
         ["patdiff"; "-alt-old"; "reference"; "-alt-new"; "current-output"]
       else
         [
           "diff";
           "-u";
           "-b";
           "--color";
           "--label";
           "reference";
           "--label";
           "current-output";
         ])
end

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
  let clerk_flags = make "CLERK_FLAGS"
  let ocamlopt_exe = make "OCAMLOPT_EXE"
  let ocamlopt_flags = make "OCAMLOPT_FLAGS"
  let runtime_ocaml_libs = make "RUNTIME_OCAML_LIBS"
  let diff = make "DIFF"
  let post_test = make "POST_TEST"

  (** Rule vars, Used in specific rules *)

  let input = make "in"
  let output = make "out"
  let pool = make "pool"
  let src = make "src"
  let scope = make "scope"
  let test_id = make "test-id"
  let test_command = make "test-command"
  let ( ! ) = Var.v
end

let base_bindings catala_exe catala_flags =
  let catala_flags = ("--directory=" ^ Var.(!builddir)) :: catala_flags in
  [
    Nj.binding Var.ninja_required_version ["1.7"];
    (* use of implicit outputs *)
    Nj.binding Var.builddir [Lazy.force Poll.build_dir];
    Nj.binding Var.clerk_exe [Lazy.force Poll.clerk_exe];
    Nj.binding Var.catala_exe
      [
        (match catala_exe with
        | Some e -> e
        | None -> Lazy.force Poll.catala_exe);
      ];
    Nj.binding Var.catala_flags catala_flags;
    Nj.binding Var.clerk_flags
      ("-e"
      :: Var.(!catala_exe)
      :: List.map (fun f -> "--catala-opts=" ^ f) catala_flags);
    Nj.binding Var.ocamlopt_exe ["ocamlopt"];
    Nj.binding Var.ocamlopt_flags ["-I"; Lazy.force Poll.ocaml_runtime_dir];
    Nj.binding Var.runtime_ocaml_libs (Lazy.force Poll.ocaml_link_flags);
    Nj.binding Var.diff (Lazy.force Poll.diff_command);
    Nj.binding Var.post_test [Var.(!diff)];
  ]

let static_base_rules =
  let open Var in
  [
    Nj.rule "copy"
      ~command:["cp"; "-f"; !input; !output]
      ~description:["<copy>"; !input];
    Nj.rule "catala-ocaml"
      ~command:[!catala_exe; "ocaml"; !catala_flags; !input; "-o"; !output]
      ~description:["<catala>"; "ocaml"; "⇒"; !output];
    Nj.rule "ocaml-module"
      ~command:
        [!ocamlopt_exe; "-shared"; !ocamlopt_flags; !input; "-o"; !output]
      ~description:["<ocaml>"; "⇒"; !output];
    Nj.rule "ocaml-exec"
      ~command:
        [
          !ocamlopt_exe;
          !ocamlopt_flags;
          !runtime_ocaml_libs;
          !input;
          "-o";
          !output;
        ]
      ~description:["<ocaml>"; "⇒"; !output];
    Nj.rule "out-test"
      ~command:
        [
          !catala_exe;
          !test_command;
          !catala_flags;
          !input;
          ">";
          !output;
          "2>&1";
          "||";
          "true";
        ]
      ~description:
        ["<catala>"; "test"; !test_id; "⇐"; !input; "(" ^ !test_command ^ ")"];
    Nj.rule "inline-tests"
      ~command:
        [!clerk_exe; "runtest"; !clerk_flags; !input; ">"; !output; "2>&1"]
      ~description:["<catala>"; "inline-tests"; "⇐"; !input];
    Nj.rule "post-test" ~command:[!post_test; !input]
      ~description:["<test-validation>"];
    Nj.rule "interpret"
      ~command:
        [!catala_exe; "interpret"; !catala_flags; !input; "--scope=" ^ !scope]
      ~description:["<catala>"; "interpret"; !scope; "⇐"; !input]
      ~vars:[pool, ["console"]];
  ]

let gen_build_statements (item : Scan.item) : Nj.ninja =
  let open File in
  let ( ! ) = Var.( ! ) in
  let src = item.file_name in
  let modules = List.rev item.used_modules in
  let inc x = File.(!Var.builddir / x) in
  let modd x = File.(src /../ x) ^ "@module" in
  let def_src = Nj.binding Var.src [Filename.remove_extension src] in
  let srcv = !Var.src ^ Filename.extension src in
  let include_deps =
    Nj.build "copy" ~inputs:[srcv]
      ~implicit_in:(List.map inc item.included_files @ List.map modd modules)
      ~outputs:[inc srcv]
  in
  let module_deps =
    Option.map
      (fun m -> Nj.build "phony" ~inputs:[inc srcv] ~outputs:[modd m])
      item.module_def
  in
  let ml_file =
    match item.module_def with
    | Some m -> (!Var.builddir / src /../ m) ^ ".ml"
    | None -> (!Var.builddir / !Var.src) ^ ".ml"
  in
  let ocaml =
    Nj.build "catala-ocaml"
      ~inputs:[inc srcv]
      ~implicit_in:[!Var.catala_exe] ~outputs:[ml_file]
  in
  let ocamlopt =
    let implicit_out_exts = ["cmi"; "cmx"; "cmt"; "o"] in
    match item.module_def with
    | Some m ->
      let target ext = (!Var.builddir / src /../ m) ^ "." ^ ext in
      Nj.build "ocaml-module" ~inputs:[ml_file]
        ~implicit_in:
          (List.map (fun m -> (!Var.builddir / src /../ m) ^ ".cmi") modules)
        ~outputs:[target "cmxs"]
        ~implicit_out:(List.map target implicit_out_exts)
        ~vars:
          [
            ( Var.ocamlopt_flags,
              [!Var.ocamlopt_flags; "-I"; File.(!Var.builddir / src /../ "")] );
          ]
    | None ->
      let target ext = (!Var.builddir / !Var.src) ^ "." ^ ext in
      let inputs =
        List.map (fun m -> (!Var.builddir / src /../ m) ^ ".cmx") modules
        @ [ml_file]
      in
      Nj.build "ocaml-exec" ~inputs
        ~outputs:[target "exe"]
        ~implicit_out:(List.map target implicit_out_exts)
  in
  let interp_deps =
    !Var.catala_exe
    :: List.map (fun m -> (!Var.builddir / src /../ m) ^ ".cmxs") modules
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
    let legacy_tests =
      List.fold_left
        (fun acc test ->
          let vars =
            [Var.test_id, [test.Scan.id]; Var.test_command, test.Scan.cmd]
          in
          let reference = legacy_test_reference test in
          let test_out =
            (!Var.builddir / src /../ "output" / Filename.basename src)
            -.- test.id
          in
          Nj.build "out-test"
            ~inputs:[inc srcv]
            ~implicit_in:interp_deps ~outputs:[test_out] ~vars
          :: (* The test reference is an input because of the cases when we run
                diff; it should actually be an output for the cases when we
                reset but that shouldn't cause trouble. *)
             Nj.build "post-test" ~inputs:[reference; test_out]
               ~outputs:[reference ^ "@post"]
          :: acc)
        [] item.legacy_tests
    in
    let inline_tests =
      if not item.has_inline_tests then []
      else
        [
          Nj.build "inline-tests"
            ~inputs:[inc srcv]
            ~implicit_in:(!Var.clerk_exe :: interp_deps)
            ~outputs:[(!Var.builddir / srcv) ^ "@out"];
        ]
    in
    let tests =
      if item.has_inline_tests then
        [
          Nj.build "post-test"
            ~outputs:[srcv ^ "@test"]
            ~inputs:[srcv; inc (srcv ^ "@out")]
            ~implicit_in:
              (List.map
                 (fun test -> legacy_test_reference test ^ "@post")
                 item.legacy_tests);
        ]
      else if item.legacy_tests <> [] then
        [
          Nj.build "phony"
            ~outputs:[srcv ^ "@test"]
            ~inputs:
              (List.map
                 (fun test -> legacy_test_reference test ^ "@post")
                 item.legacy_tests);
        ]
      else []
    in
    legacy_tests @ inline_tests @ tests
  in
  Seq.concat
  @@ List.to_seq
       [
         Seq.return (Nj.comment "");
         Seq.return def_src;
         Seq.return include_deps;
         Option.to_seq module_deps;
         Seq.return ocaml;
         Seq.return ocamlopt;
         List.to_seq tests;
         Seq.return interpret;
       ]

let test_targets_by_dir items =
  let stmt target_sfx dir sub =
    Nj.build "phony"
      ~outputs:[dir ^ target_sfx]
      ~inputs:(List.map (fun s -> s ^ target_sfx) sub)
  in
  let alias dir sub = List.to_seq [stmt "@test" dir sub; Nj.comment ""] in
  (* This relies on the fact that the sequence is returned ordered by
     directory *)
  let rec aux curdir seq =
    let prefix = if curdir = "" then "" else curdir ^ "/" in
    match seq () with
    | Seq.Cons (item, seq) as node
      when String.starts_with ~prefix item.Scan.file_name -> (
      if item.Scan.legacy_tests = [] && not item.Scan.has_inline_tests then
        aux curdir seq
      else
        match
          String.split_on_char '/'
            (String.remove_prefix ~prefix item.Scan.file_name)
        with
        | [] -> assert false
        | [_] ->
          let rules, cur, seq = aux curdir seq in
          rules, item.Scan.file_name :: cur, seq
        | subdir :: _ ->
          let subdir = File.(curdir / subdir) in
          let rules, sub, seq = aux subdir (fun () -> node) in
          let rules =
            if sub = [] then rules else Seq.append rules (alias subdir sub)
          in
          let rules1, cur, seq = aux curdir seq in
          Seq.append rules rules1, subdir :: cur, seq)
    | node -> Seq.empty, [], fun () -> node
  in
  let rules, top, seq = aux "" items in
  assert (Seq.is_empty seq);
  Seq.append rules
  @@ List.to_seq
       [
         Nj.build "phony" ~outputs:["test"]
           ~inputs:(List.map (fun s -> s ^ "@test") top);
       ]

let build_statements dir =
  (* Unfortunately we need to express the module name bindings first, so need to
     iterate twice using Seq.memoize *)
  Scan.tree dir
  |> Seq.memoize
  |> fun items ->
  Seq.concat
  @@ List.to_seq
       [
         Seq.flat_map gen_build_statements items;
         Seq.return (Nj.comment "\n- Global targets - #\n");
         test_targets_by_dir items;
       ]

let gen_ninja_file catala_exe catala_flags dir =
  let ( @+ ) = Seq.append in
  Seq.return
    (Nj.Comment (Printf.sprintf "File generated by Clerk v.%s\n" version))
  @+ Seq.return (Nj.Comment "- Global variables - #\n")
  @+ List.to_seq (base_bindings catala_exe catala_flags)
  @+ Seq.return (Nj.Comment "\n- Base rules - #\n")
  @+ List.to_seq static_base_rules
  @+ Seq.return (Nj.Comment "- Project-specific build statements - #")
  @+ build_statements dir

(** {1 Driver} *)

let ninja_init ~chdir ~catala_exe ~catala_opts ~color ~debug ~ninja_output :
    extra:def Seq.t -> (File.t -> 'a) -> 'a =
  Option.iter Sys.chdir chdir;
  let _options = Catala_utils.Cli.enforce_globals ~debug ~color () in
  let with_ninja_output k =
    match ninja_output with
    | Some f -> k f
    | None when debug -> k File.(Lazy.force Poll.build_dir / "clerk.ninja")
    | None -> File.with_temp_file "clerk_build_" ".ninja" k
  in
  fun ~extra k ->
    Message.emit_debug "building ninja rules...";
    with_ninja_output
    @@ fun nin_file ->
    File.with_formatter_of_file nin_file (fun nin_ppf ->
        let ninja_contents =
          Seq.concat
          @@ List.to_seq
               [
                 gen_ninja_file catala_exe catala_opts ".";
                 Seq.return (Nj.comment "\n - Command-specific targets - #");
                 extra;
               ]
        in
        Nj.format nin_ppf ninja_contents);
    k nin_file

let ninja_cmdline ninja_flags nin_file targets =
  String.concat " "
    ("ninja"
     :: "-k"
     :: "0"
     :: "-f"
     :: nin_file
     :: (if ninja_flags = "" then [] else [ninja_flags])
    @ (if Catala_utils.Cli.globals.debug then ["-v"] else [])
    @ targets)

open Cmdliner

let build_cmd =
  let run ninja_init (targets : string list) (ninja_flags : string) =
    ninja_init ~extra:Seq.empty
    @@ fun nin_file ->
    let ninja_cmd = ninja_cmdline ninja_flags nin_file targets in
    Message.emit_debug "executing '%s'..." ninja_cmd;
    Sys.command ninja_cmd
  in
  let doc =
    "Low-level build command: can be used to forward build targets or options \
     directly to Ninja"
  in
  Cmd.v (Cmd.info ~doc "build")
    Term.(
      const run $ Cli.Global.term ninja_init $ Cli.targets $ Cli.ninja_flags)

let test_cmd =
  let run
      ninja_init
      (files_or_folders : string list)
      (reset_test_outputs : bool)
      (ninja_flags : string) =
    let targets =
      match files_or_folders with
      | [] -> ["test"]
      | files -> List.map (fun f -> f ^ "@test") files
    in
    let extra =
      List.to_seq
        ((if reset_test_outputs then
            [
              Nj.binding Var.post_test
                [
                  "test_reset() { if ! diff -q $$1 $$2 >/dev/null; then cp -f \
                   $$2 $$1; fi; }";
                  ";";
                  "test_reset";
                ];
            ]
          else [])
        @ [Nj.default targets])
    in
    ninja_init ~extra
    @@ fun nin_file ->
    let ninja_cmd = ninja_cmdline ninja_flags nin_file [] in
    Message.emit_debug "executing '%s'..." ninja_cmd;
    Sys.command ninja_cmd
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
      $ Cli.ninja_flags)

let run_cmd =
  let run
      ninja_init
      (files_or_folders : string list)
      (scope : string)
      (ninja_flags : string) =
    let extra =
      Seq.cons
        (Nj.binding Var.scope [scope])
        (Seq.return
           (Nj.default
              (List.map (fun file -> file ^ "@interpret") files_or_folders)))
    in
    ninja_init ~extra
    @@ fun nin_file ->
    let ninja_cmd = ninja_cmdline ninja_flags nin_file [] in
    Message.emit_debug "executing '%s'..." ninja_cmd;
    Sys.command ninja_cmd
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
  let run catala_exe catala_opts build_dir file =
    Clerk_runtest.run_inline_tests
      (Option.value ~default:"catala" catala_exe)
      catala_opts build_dir file;
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
      $ Cli.build_dir
      $ Cli.single_file)

let main_cmd = Cmd.group Cli.info [build_cmd; test_cmd; run_cmd; runtest_cmd]

let main () =
  try exit (Cmdliner.Cmd.eval' ~catch:false main_cmd) with
  | Catala_utils.Cli.Exit_with n -> exit n
  | Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit content Error;
    if Catala_utils.Cli.globals.debug then
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
