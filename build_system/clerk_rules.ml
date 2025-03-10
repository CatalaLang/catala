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

open Catala_utils
open Clerk_cli
module Nj = Ninja_utils
module Scan = Clerk_scan

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

type backend = OCaml | Python | C | Tests (* | JS *)

let all_backends = [OCaml; Python; C; Tests]

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
  let ( ! ) = Nj.Var.v
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
    extra:Nj.def Seq.t ->
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
