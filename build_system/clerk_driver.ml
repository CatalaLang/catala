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

(* Version is synchronised with the catala version *)
let version =
  Catala_utils.Cli.version

(** {1 Command line interface} *)

let files_or_folders =
  Arg.(
    value
    & pos_right 0 file []
    & info [] ~docv:"FILE(S)" ~doc:"File(s) or folder(s) to process")

let command =
  Arg.(
    required
    & pos 0 (some (enum ["test", `Test; "run", `Run; "runtest", `Runtest])) None
    & info [] ~docv:"COMMAND" ~doc:"Main command to run")

let debug =
  Arg.(value & flag & info ["debug"; "d"] ~doc:"Prints debug information")

let chdir =
  Arg.(value & opt (some string) None
       & info ["C"] ~docv:"DIR"
         ~doc:"Change to the given directory before processing")

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
        ~doc:"Catala compiler executable.")

let ninja_output =
  Arg.(
    value
    & opt (some string) None
    & info ["o"; "output"] ~docv:"FILE"
        ~doc:
          "$(i,FILE) is the file that will contain the build.ninja file \
           output. If not specified, the build.ninja file will be output in \
           the temporary directory of the system and cleaned up on exit.")

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
    & info ["makeflags"] ~docv:"FLAG"
        ~doc:
          "Provides the contents of a $(i, MAKEFLAGS) variable to pass on to \
           Ninja. Currently recognizes the -i and -j options.")

let catala_opts =
  Arg.(
    value
    & opt_all string []
    & info ["c"; "catala-opts"] ~docv:"FLAG"
        ~doc:"Option to pass to the Catala compiler. Can be repeated.")

let color =
  Arg.(value
       & opt ~vopt:Cli.Always Cli.when_opt Auto
       & info ["color"]
         ~env:(Cmd.Env.info "CATALA_COLOR")
         ~doc:
           "Allow output of colored and styled text. Use $(i,auto), to \
            enable when the standard output is to a terminal, $(i,never) to \
            disable.")

let clerk_t f =
  Term.(
    const f
    $ files_or_folders
    $ command
    $ chdir
    $ catalac
    $ catala_opts
    $ makeflags
    $ debug
    $ color
    $ scope
    $ reset_test_outputs
    $ ninja_output)

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

(**{1 Collecting items from files}*)

type expected_output_descr = {
  tested_filename : string;  (** Name of the file that's being tested *)
  output_dir : string;
      (** Name of the output directory where all expected outputs are stored *)
  id : string;
      (** Id of this precise unit test that will be associated to an expected
          output *)
  cmd : string list;
      (** Catala command to launch to run the test, excluding "catala" at the
          begin, and the name of the file to test *)
}
(** Structure describing a single "legacy test", ie test with a separate output
    file *)

type catala_build_item = {
  file_name : File.t;
  module_def : string option;
  used_modules : string list;
  included_files : File.t list;
  legacy_tests : expected_output_descr list;
  has_inline_tests : bool;
}
(** Contains all the data extracted from a single Catala file. Lists are in reverse file order. *)

let catala_suffix_regex =
  Re.(compile (seq [str ".catala_"; group (seq [alpha; alpha]); eos]))

let scan_catala_file (file : File.t) (lang : Cli.backend_lang) :
    catala_build_item =
  let module L = Surface.Lexer_common in
  let rec parse lines n acc =
    match Seq.uncons lines with
    | None -> acc
    | Some ((_, L.LINE_TEST id), lines) ->
      let test, lines, n = parse_test id lines (n+1) in
      parse lines n { acc with legacy_tests = test :: acc.legacy_tests }
    | Some ((_, line), lines) ->
      parse lines (n+1) @@
      match line with
      | L.LINE_INCLUDE f ->
        let f = if Filename.is_relative f then File.(file /../ f) else f in
        { acc with included_files = f :: acc.included_files }
      | L.LINE_MODULE_DEF m ->
        { acc with module_def = Some m }
      | L.LINE_MODULE_USE m ->
        { acc with used_modules = m :: acc.used_modules }
      | L.LINE_INLINE_TEST ->
        { acc with has_inline_tests = true }
      | _ -> acc
  and parse_test id lines n =
    let test =
      { id;
        tested_filename = file;
        output_dir = File.(file /../ "output" / "");
        cmd = [] }
    in
    let err n =
      [Format.asprintf "<invalid test syntax at %a:%d>" File.format file n]
    in
    match Seq.uncons lines with
    | Some ((str, L.LINE_ANY), lines)
      when String.starts_with ~prefix:"catala " str ->
      let cmd = String.trim (String.remove_prefix ~prefix:"catala " str) in
      let cmd, lines, n = parse_block lines (n+1) [cmd] in
      { test with cmd = List.flatten (List.map (String.split_on_char ' ') cmd) },
      lines, (n+1)
    | Some (_, lines) ->
      { test with cmd = err n}, lines, n+1
    | None ->
      { test with cmd = err n}, lines, n
  and parse_block lines n acc =
    match Seq.uncons lines with
    | Some ((_, L.LINE_BLOCK_END), lines) -> List.rev acc, lines, n+1
    | Some ((str, _), lines) -> String.trim str :: acc, lines, n+1
    | None -> List.rev acc, lines, n
  in
  parse
    (Surface.Parser_driver.lines file lang) 1
    {
      file_name = file;
      module_def = None;
      used_modules = [];
      included_files = [];
      legacy_tests = [];
      has_inline_tests = false;
    }

let get_lang file =
  Option.bind (Re.exec_opt catala_suffix_regex file) @@ fun g ->
  List.assoc_opt (Re.Group.get g 1) Cli.languages

let scan_tree (dir : File.t) : catala_build_item Seq.t =
  File.scan_tree
    (fun f ->
      match get_lang f with
      | None -> None
      | Some lang -> Some (scan_catala_file f lang))
    dir

(** {1 System analysis} *)

(** Some functions that poll the surrounding systems (think [./configure]) *)
module Poll = struct

  (** Scans for a parent directory being the root of the Catala source repo *)
  let catala_project_root: File.t option Lazy.t =
    let rec aux dir =
      if Sys.file_exists File.(dir / "catala.opam") then Some dir
      else
        let dir' = File.dirname dir in
        if dir' = dir then None else aux dir'
    in
    lazy (aux (Sys.getcwd ()))

  let exec_dir: File.t =
    (* Do not use Sys.executable_name, which may resolve symlinks: we want the original path.
       (e.g. _build/install/default/bin/foo is a symlink) *)
    Filename.dirname Sys.argv.(0)

  let clerk_exe: File.t Lazy.t = lazy Sys.executable_name

  let catala_exe: File.t Lazy.t = lazy (
    let f = File.(exec_dir / "catala") in
    if Sys.file_exists f then f else
      match Lazy.force catala_project_root with
      | Some root -> File.(root/"_build"/"default"/"compiler"/"catala.exe")
      | None -> "catala" (* Dynamically resolved from PATH *)
  )

  let build_dir: File.t Lazy.t = lazy (
    File.(Sys.getcwd () / "_build")
  )
  (* TODO: probably detect the project root and put this there instead *)

  (** Locates the main [lib] directory containing the OCaml libs *)
  let ocaml_libdir: File.t Lazy.t =
    lazy
      (try String.trim (File.process_out "opam" ["var"; "lib"])
       with Failure _ -> (
           try String.trim (File.process_out "ocamlc" ["-where"])
           with Failure _ -> (
               match File.(check_directory (exec_dir /../ "lib")) with
               | Some d -> d
               | None ->
                 Message.raise_error
                   "Could not locate the OCaml library directory, make sure OCaml or \
                    opam is installed")))

  (** Locates the directory containing the OCaml runtime to link to *)
  let ocaml_runtime_dir: File.t Lazy.t =
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
             | None -> File.(Lazy.force ocaml_libdir / "catala" / "runtime"))
         (* FIXME check this, not "runtime_ocaml" ?? *)
       in
       match File.check_directory d with
       | Some dir ->
         Message.emit_debug "Catala runtime libraries found at @{<bold>%s@}." dir;
         dir
       | None ->
         Message.raise_error
           "@[<hov>Could not locate the Catala runtime library.@ Make sure that \
            either catala is correctly installed,@ or you are running from the \
            root of a compiled source tree.@]")

  let ocaml_link_flags: string list Lazy.t = lazy (
    let link_libs =
      [
        "biniou";
        "easy-format";
        "yojson";
        "ppx_yojson_conv_lib";
        "zarith";
        "dates_calc";
      ]
    in
    let link_libs_flags =
      List.concat_map
        (fun lib ->
           match File.(check_directory (Lazy.force ocaml_libdir / lib)) with
           | None ->
             Message.raise_error
               "Required OCaml library not found at %a.@ Try `opam \
                install %s'"
               File.format File.(Lazy.force ocaml_libdir / lib)
               lib
           | Some d ->
             ["-I"; d; String.map (function '-' -> '_' | c -> c) lib ^ ".cmxa"])
        link_libs
    in
    let runtime_dir = Lazy.force ocaml_runtime_dir in
    link_libs_flags @ [File.(runtime_dir / "runtime_ocaml.cmxa")])

  let has_command cmd =
    let check_cmd = Printf.sprintf "type %s >/dev/null 2>&1" cmd in
    Sys.command check_cmd = 0

  let diff_command = lazy (
    if has_command "patdiff" then
      ["patdiff"; "-alt-old"; "reference"; "-alt-new"; "current-output"]
    else
      ["diff"; "-u"; "-b"; "--color"; "--label"; "referenec"; "--label"; "current-output"]
  )

end

(**{1 Building rules}*)

(** Ninja variable names *)
module Var = struct
  include Nj.Var

  (** Global vars: always defined, at toplevel *)

  let ninja_required_version = make "ninja_required_version"
  let builddir = make "builddir"
  let default = make "default"

  let clerk_exe = make "CLERK_EXE"
  let catala_exe = make "CATALA_EXE"
  let catala_flags = make "CATALA_FLAGS"
  let clerk_flags = make "CLERK_FLAGS"
  let ocamlopt_exe = make "OCAMLOPT_EXE"
  let ocamlopt_flags = make "OCAMLOPT_FLAGS"
  let runtime_ocaml_libs = make "RUNTIME_OCAML_LIBS"
  let diff = make "DIFF"

  let module_dir module_name = make ("module_dir_" ^ module_name)
  let module_src module_name = make ("module_src_" ^ module_name)
  (** Source file of a given Catala module *)

  (** Rule vars, Used in specific rules *)

  let input = make "in"
  let output = make "out"
  let pool = make "pool"
  let modules_src = make "modules_src"
  let modules_use = make "modules_use"
  let include_flags = make "include_flags"

  let scope = make "scope"
  let test_id = make "test-id"
  let test_out = make "test-out"
  let test_command = make "test-command"

  let ( ! ) = Var.v
end

let base_bindings catala_exe catala_flags =
  [
  Nj.binding Var.ninja_required_version ["1.7"]; (* use of implicit outputs *)
  Nj.binding Var.builddir [Lazy.force Poll.build_dir];

  Nj.binding Var.clerk_exe [Lazy.force Poll.clerk_exe];
  Nj.binding Var.catala_exe [match catala_exe with Some e -> e | None -> Lazy.force Poll.catala_exe];
  Nj.binding Var.catala_flags ("--build-dir" :: Var.(!builddir) :: catala_flags);
  Nj.binding Var.clerk_flags ("-e" :: Var.(!catala_exe) :: (List.map (fun f -> "--catala-opts=" ^ f) catala_flags));
  Nj.binding Var.ocamlopt_exe ["ocamlopt"];
  Nj.binding Var.ocamlopt_flags ["-I"; Lazy.force Poll.ocaml_runtime_dir];
  Nj.binding Var.runtime_ocaml_libs (Lazy.force Poll.ocaml_link_flags);
  Nj.binding Var.diff (Lazy.force Poll.diff_command);
]

let static_base_rules =
  let open Var in
  [
    (* Nj.rule "interpret-scope"
     *   ~command:[!catala_exe; "interpret"; !catala_flags; !modules_src; !input; "-s"; !scope]
     *   ~description:["<catala>"; "run"; !scope; "⇐"; !input]; *)

    Nj.rule "ocaml"
      ~command:[!catala_exe; "ocaml"; !catala_flags; !modules_src; !input; "-o"; !output]
      ~description:["<catala>"; "ocaml"; "⇒"; !output];

    Nj.rule "ocaml-module"
      ~command:[!ocamlopt_exe; "-shared"; !include_flags; !ocamlopt_flags; !input; "-o"; !output]
      ~description:["<ocaml>"; "⇒"; !output];

    Nj.rule "ocaml-exec"
      ~command:[!ocamlopt_exe; !ocamlopt_flags; !runtime_ocaml_libs; !input; "-o"; !output]
      ~description:["<ocaml>"; "⇒"; !output];

    Nj.rule "out-test"
      ~command:[!catala_exe; !test_command; !catala_flags; !input; !modules_use; "2>&1"; "|"; !diff; !test_out; "/dev/stdin"]
      ~description:["<catala>"; "test"; !test_id; "⇐"; !input; "("^ !test_command ^ ")"];

    Nj.rule "out-reset"
      ~command:[!catala_exe; !test_command; !catala_flags; !input; !modules_use; ">"; !output; "2>&1"; "||"; "true"]
      ~description:["<catala>"; "reset"; !test_id; "⇐"; !input; "("^ !test_command ^ ")"];

    Nj.rule "inline-tests"
      ~command:[!clerk_exe; "runtest"; !clerk_flags; "--catala-opts=--build-dir=" ^ !builddir; !input; !modules_use; "2>&1"; "|"; !diff; !input; "/dev/stdin"]
      ~description:["<catala>"; "inline-tests"; "⇐"; !input];

    Nj.rule "inline-reset"
      ~command:[!clerk_exe; "runtest"; !clerk_flags; "--catala-opts=--build-dir=" ^ !builddir; !input; !modules_use; "--reset"]
      ~description:["<catala>"; "inline-reset"; "⇐"; !input];

    Nj.rule "interpret"
      ~command:[!catala_exe; "interpret"; !catala_flags; !input; !modules_use; "--scope=" ^ !scope ]
      ~description:["<catala>"; "interpret"; !scope; "⇐"; !input]
      ~vars:[pool, ["console"]];
  ]

let gen_module_def (item: catala_build_item) : Nj.ninja =
  match item.module_def with
  | None -> Seq.empty
  | Some modname ->
    List.to_seq [
      Nj.binding (Var.module_dir modname) [Filename.dirname item.file_name];
      Nj.binding (Var.module_src modname) [Filename.basename item.file_name];
    ]

let gen_build_statements (item: catala_build_item) : Nj.ninja =
  let open File in
  let ( ! ) = Var.( ! ) in
  let src = item.file_name in
  let modules = List.rev item.used_modules in
  let header =
    Nj.comment ("\nDefinitions from " ^ src)
  in
  let ocaml =
    Nj.build "ocaml"
      ~inputs:[src]
      ~implicit_in:((if modules = [] then [] else [!Var.modules_src]) @ item.included_files)
      ~outputs:[!Var.builddir / src -.- "ml"]
      ~vars:(if modules = [] then [] else
               [Var.modules_src,
                List.map (fun m -> !(Var.module_dir m) / !(Var.module_src m)) modules])
  in
  let ocamlopt =
    let target ext = match item.module_def with
      | Some m -> !Var.builddir / src /../ m ^ "." ^ ext
      | None -> !Var.builddir / src -.- ext
    in
    let implicit_out =
      [target "cmi"; target "cmx"; (* target "cmt"; target "o" *)]
    in
    let vars = [Var.include_flags, ["-I"; !Var.builddir / Filename.dirname src]] in
    match item.module_def with
    | Some _ ->
      Nj.build "ocaml-module"
        ~inputs:[!Var.builddir / src -.- "ml"]
        ~implicit_in:(List.map (fun m -> src /../ m ^ ".cmi") modules)
        ~outputs:[target "cmxs"]
        ~implicit_out
        ~vars
    | None ->
      let inputs =
        List.map (fun m -> !Var.builddir / src /../ m ^ ".cmx") modules @
        [ !Var.builddir / src -.- "ml" ]
      in
      Nj.build "ocaml-exec"
        ~inputs
        ~outputs:[target "exe"]
        ~implicit_out
        ~vars
  in
  let interpret_deps =
    Nj.build "phony"
      ~outputs:["interpret-deps@" ^ src]
      ~inputs:(
        item.included_files @
        List.map (fun m -> !(Var.module_dir m) / !(Var.module_src m)) modules @
        List.map (fun m -> !Var.builddir / !(Var.module_dir m) / m ^ ".cmxs") modules
      )
  in
  let interpret =
    Nj.build "interpret"
      ~outputs:["interpret@" ^ src]
      ~inputs:[src]
      ~implicit_in:["interpret-deps@" ^ src]
      ~vars:(if item.used_modules = [] then [] else [
          Var.modules_use, List.map (fun m -> "--use=" ^ !(Var.module_dir m) / !(Var.module_src m)) item.used_modules
        ])
  in
  let tests =
    let inputs = [src] in
    let implicit_in = ["interpret-deps@" ^ src] in
    let vars =
      if item.used_modules = [] then [] else [
      Var.modules_use, List.map (fun m -> "--catala-opts=--use=" ^ !(Var.module_dir m) / !(Var.module_src m)) item.used_modules
    ]
    in
    let legacy_tests =
      List.fold_left (fun acc test ->
          let vars = vars @ [
            Var.test_id, [test.id];
            Var.test_command, test.cmd;
            Var.test_out, [Filename.dirname src / Filename.basename src -.- "out" / !Var.test_id];
          ] in
          Nj.build "out-test"  ~inputs ~implicit_in ~outputs:["outtest@"^src^"@"^test.id] ~vars ::
          Nj.build "out-reset" ~inputs ~implicit_in ~outputs:["outtest-reset@"^src^"@"^test.id] ~implicit_out:[!Var.test_out] ~vars ::
          acc
        )
        [] item.legacy_tests
    in
    let inline_tests =
      if not item.has_inline_tests then [] else
        [
          Nj.build "inline-tests" ~inputs ~implicit_in ~vars ~outputs:["inline@" ^ src];
          Nj.build "inline-reset" ~inputs ~implicit_in ~vars ~outputs:["inline-reset@" ^ src];
        ]
    in
    let tests =
      if item.legacy_tests = [] && not item.has_inline_tests then []
      else
        [Nj.build "phony"
           ~outputs:["test@" ^ src]
           ~inputs:
             ((if item.has_inline_tests then
                 ["inline@" ^ item.file_name]
               else []) @
              List.map (fun test ->
                  "outtest@" ^ item.file_name ^"@"^ test.id)
                item.legacy_tests);
         Nj.build "phony"
           ~outputs:["test-reset@" ^ src]
           ~inputs:
             ((if item.has_inline_tests then
                 ["inline-reset@" ^ item.file_name]
               else []) @
              List.map (fun test ->
                  "outtest-reset@" ^ item.file_name ^"@"^ test.id)
                item.legacy_tests)]
    in
    legacy_tests @ inline_tests @ tests
  in
  Seq.concat @@ List.to_seq [
    Seq.return header;
    Seq.return ocaml;
    Seq.return ocamlopt;
    Seq.return interpret_deps;
    List.to_seq tests;
    Seq.return interpret;
  ]

let build_statements dir =
  (* Unfortunately we need to express the module name bindings first, so need to iterate twice using Seq.memoize *)
  scan_tree dir |> Seq.memoize |> fun items ->
  Seq.concat @@ List.to_seq [
    Seq.flat_map gen_module_def items;
    Seq.flat_map gen_build_statements items;
    Seq.return (Nj.comment "\n- Global targets - #\n");
    let items_with_tests = Seq.filter
        (fun item -> item.legacy_tests <> [] || item.has_inline_tests)
        items
    in
    if Seq.is_empty items_with_tests then Seq.empty
    else
      let get_targets prefix = Seq.map (fun item -> prefix ^ item.file_name) items_with_tests |> List.of_seq in
      List.to_seq [
        Nj.build "phony" ~outputs:["test"] ~inputs:(get_targets "test@");
        Nj.build "phony" ~outputs:["test-reset"] ~inputs:(get_targets "test-reset@");
      ]
  ]

let gen_ninja_file catala_exe catala_flags dir =
  let ( @+ ) = Seq.append in
  Seq.return (Nj.Comment (Printf.sprintf "File generated by Clerk v.%s\n" version)) @+
  Seq.return (Nj.Comment "- Global variables - #\n") @+
  List.to_seq (base_bindings catala_exe catala_flags) @+
  Seq.return (Nj.Comment "- Base rules - #\n") @+
  List.to_seq static_base_rules @+
  Seq.return (Nj.Comment "- Project-specific build statements - #") @+
  build_statements dir

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

(** {1 Return code values} *)

let return_ok = 0
let return_err = 1

(** {1 Driver} *)

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
    (command : [> ])
    (chdir : string option)
    (catala_exe : string option)
    (catala_opts : string list)
    (makeflags : string option)
    (debug : bool)
    (color : Cli.when_enum)
    (scope : string option)
    (reset_test_outputs : bool)
    (ninja_output : string option) : int =
  try
    Option.iter Sys.chdir chdir;
    let _options = Cli.enforce_globals ~debug ~color () in
    let ninja_flags = makeflags_to_ninja_flags makeflags in
    let with_ninja_file ?(extra=Seq.empty) k =
      let with_ninja_output k =
        match ninja_output with
        | Some f -> k f
        | None -> File.with_temp_file "clerk_build_" ".ninja" k
      in
      Message.emit_debug "building ninja rules...";
      with_ninja_output
      @@ fun nin_file ->
      File.with_formatter_of_file nin_file (fun nin_ppf ->
          let ninja_contents =
            Seq.concat @@ List.to_seq [
              gen_ninja_file catala_exe catala_opts ".";
              Seq.return (Nj.comment "\n - Command-specific targets - #");
              extra
            ]
          in
          Nj.format nin_ppf ninja_contents);
      k nin_file
    in
    match command with
    | `Test ->
      let targets =
        let target = if reset_test_outputs then "test-reset" else "test" in
        match files_or_folders with
        | [] -> [target]
        | files -> List.map (fun f -> target ^ "@" ^ f) files
      in
      let extra = Seq.return (Nj.default targets) in
      with_ninja_file ~extra @@ fun nin_file ->
      let ninja_cmd =
        String.concat " " ["ninja -k 0 -f"; nin_file; ninja_flags]
      in
      Message.emit_debug "executing '%s'..." ninja_cmd;
      Sys.command ninja_cmd
    | `Run ->
      let extra =
        Seq.append
          (match scope with
           | Some scope -> Seq.return (Nj.binding Var.scope [scope])
           | None -> Seq.empty)
          (Seq.return
             (Nj.default (List.map (fun file -> "interpret@"^file) files_or_folders)))
      in
      with_ninja_file ~extra @@ fun nin_file ->
      let ninja_cmd =
        String.concat " " ["ninja -k 0 -f"; nin_file; ninja_flags]
      in
      Message.emit_debug "executing '%s'..." ninja_cmd;
      Sys.command ninja_cmd
    | `Runtest -> (
        match files_or_folders with
        | [f] ->
          Clerk_runtest.run_inline_tests ~reset:reset_test_outputs f
            (Option.value ~default:"catala" catala_exe)
            catala_opts;
          0
        | _ -> Message.raise_error "Please specify a single catala file to test")
  with Message.CompilerError content ->
    let bt = Printexc.get_raw_backtrace () in
    Message.Content.emit content Error;
    if Printexc.backtrace_status () then Printexc.print_raw_backtrace stderr bt;
    return_err

let main () = exit (Cmdliner.Cmd.eval' (Cmdliner.Cmd.v info (clerk_t driver)))
