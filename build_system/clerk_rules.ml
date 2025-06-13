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
module Nj = Ninja_utils
module Scan = Clerk_scan
module Poll = Clerk_poll

(**{1 Building rules}*)

type backend = OCaml | Python | C | Java | Tests (* | JS *)

let all_backends = [OCaml; Python; C; Java; Tests]

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
  let catala_flags_java = make "CATALA_FLAGS_JAVA"
  let ocamlc_exe = make "OCAMLC_EXE"
  let ocamlopt_exe = make "OCAMLOPT_EXE"
  let ocaml_flags = make "OCAML_FLAGS"
  let runtime_ocaml_libs = make "RUNTIME_OCAML_LIBS"
  let cc_exe = make "CC"
  let c_flags = make "CFLAGS"
  let runtime_c_libs = make "RUNTIME_C_LIBS"
  let python = make "PYTHON"
  let runtime_python_dir = make "RUNTIME_PYTHON"
  let javac = make "JAVAC"
  let javac_flags = make "JAVAC_FLAGS"
  let jar = make "jar"
  let java = make "JAVA"
  let runtime_java_jar = make "RUNTIME_JAVA_JAR"
  let all_vars = all_vars_ref.contents

  (* Definition spreading different rules *)

  let tdir = make "tdir"
  let includes = make "includes"

  (* Rule vars, Used in specific rules *)

  let input = make "in"
  let output = make "out"
  let src = make "src"
  let dst = make "dst"
  let class_path = make "class_path"

  (* let scope = make "scope" *)
  let test_id = make "test-id"
  let ( ! ) = Nj.Var.v
end

let base_bindings
    ~catala_exe
    ~catala_flags:catala_flags0
    ~build_dir
    ~include_dirs
    ?(vars_override = [])
    ?(test_flags = [])
    ?(enabled_backends = all_backends)
    ~autotest
    () =
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
  let catala_flags_java =
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
  @ (if List.mem Java enabled_backends then
       [
         def Var.catala_flags_java (lazy catala_flags_java);
         def Var.java (lazy ["java"]);
         def Var.javac (lazy ["javac"]);
         def Var.jar (lazy ["jar"]);
         def Var.javac_flags (lazy ["-implicit:none"]);
         def Var.runtime_java_jar (lazy [Lazy.force Poll.java_runtime]);
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
      ~command:
        (if Sys.win32 then
           ["cmd"; "/c"; "copy"; "/Y"; !input; !output]
         else 
           ["cp"; "-f"; !input; !output])
      ~description:["<copy>"; !input];
  ] @ (if List.mem OCaml enabled_backends then [
      Nj.rule "catala-ocaml"
        ~command:[!catala_exe; "ocaml"; !catala_flags; !catala_flags_ocaml;
                  !input; "-o"; !output]
        ~description:["<catala>"; "ocaml"; "⇒"; !output];

      Nj.rule "ocaml-bytobject"
        ~command:[
          !ocamlc_exe; "-c"; !ocaml_flags; !includes; !input
        ]
        ~description:["<ocaml>"; "⇒"; !output];

      Nj.rule "ocaml-natobject"
        ~command:[
          !ocamlopt_exe; "-c"; !ocaml_flags; !includes; !input
        ]
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
  (if List.mem Java enabled_backends then [
      Nj.rule "catala-java"
        ~command:[!catala_exe; "java"; !catala_flags; !catala_flags_java;
                  !input; "-o"; !output]
        ~description:["<catala>"; "java"; "⇒"; !output];
      Nj.rule "java-class"
        ~command:[!javac; "-cp"; !runtime_java_jar ^":" ^ !class_path; !javac_flags; !input ]
        ~description:["<catala>"; "java"; "⇒"; !output];
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
  let dir = dirname src in
  let def_vars =
    [
      Nj.binding Var.src [basename src];
      Nj.binding Var.dst [basename (Scan.target_file_name item)];
    ]
  in
  let include_flags backend =
    "-I"
    :: (!Var.tdir / backend)
    :: List.concat_map
         (fun d ->
           [
             "-I";
             (if Filename.is_relative d then !Var.builddir / d else d) / backend;
           ])
         include_dirs
  in
  let target ?backend ext =
    let ext =
      match ext.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> "." ^ ext
      | _ -> ext
    in
    let bdir =
      match backend with
      | None -> fun f -> f ^ ext
      | Some b -> fun f -> (b / f) ^ ext
    in
    !Var.tdir / bdir !Var.dst
  in
  let modules = List.rev_map Mark.remove item.used_modules in
  let included_files = List.map Mark.remove item.included_files in
  let modfile ?(backend = "ocaml") ext modname =
    match List.assoc_opt modname same_dir_modules with
    | Some _ -> (!Var.tdir / backend / String.to_id modname) ^ ext
    | None -> modname ^ "@" ^ backend ^ "-module"
  in
  let module_target x = modfile "@ocaml-module" x in
  let catala_src = !Var.tdir / !Var.src in
  let include_deps =
    Nj.build "copy"
      ~inputs:[dir / !Var.src]
      ~implicit_in:
        (List.map
           (fun f ->
             if dir / basename f = f then !Var.tdir / basename f
             else !Var.builddir / f)
           included_files
        @ List.map
            (fun m ->
              try !Var.tdir / basename (List.assoc m same_dir_modules)
              with Not_found -> m ^ "@src")
            modules)
      ~outputs:[catala_src]
  in
  let module_deps =
    match item.module_def with
    | None -> []
    | Some _ ->
      (if List.mem OCaml enabled_backends then
         [
           Nj.build "phony"
             ~inputs:
               [target ~backend:"ocaml" "cmi"; target ~backend:"ocaml" "cmxs"]
             ~implicit_in:(List.map (modfile "@ocaml-module") modules)
             ~outputs:[target ~backend:"ocaml" "@ocaml-module"];
         ]
       else [])
      @
      if List.mem C enabled_backends then
        [
          Nj.build "phony"
            ~inputs:[target ~backend:"c" "h"]
            ~implicit_in:(List.map (modfile ~backend:"c" "@c-module") modules)
            ~outputs:[target ~backend:"c" "@c-module"];
        ]
      else []
  in
  let has_scope_tests = Lazy.force item.has_scope_tests in
  let ocaml, c, python, java =
    if item.extrnal then
      ( List.to_seq
          [
            Nj.build "copy" ~implicit_in:[catala_src]
              ~inputs:[src -.- "ml"]
              ~outputs:[target ~backend:"ocaml" "ml"];
            Nj.build "copy" ~implicit_in:[catala_src]
              ~inputs:[src -.- "mli"]
              ~outputs:[target ~backend:"ocaml" "mli"];
          ],
        List.to_seq
          [
            Nj.build "copy" ~implicit_in:[catala_src]
              ~inputs:[src -.- "c"]
              ~outputs:[target ~backend:"c" "c"];
            Nj.build "copy" ~implicit_in:[catala_src]
              ~inputs:[src -.- "h"]
              ~outputs:[target ~backend:"c" "h"];
          ],
        Nj.build "copy" ~implicit_in:[catala_src]
          ~inputs:[src -.- "py"]
          ~outputs:[target ~backend:"python" "py"],
        List.to_seq
          [
            Nj.build "copy" ~implicit_in:[catala_src]
              ~inputs:[src -.- "java"]
              ~outputs:[target ~backend:"java" "java"];
          ] )
    else
      let inputs = [catala_src] in
      let implicit_in =
        (* autotest requires interpretation at compile-time, which makes use of
           the dependent OCaml modules (cmxs) *)
        !Var.catala_exe
        :: (if autotest then List.map module_target modules else [])
      in
      let implicit_out backend ext =
        if has_scope_tests then [target ~backend ("+main." ^ ext)] else []
      in
      ( Seq.return
          (Nj.build "catala-ocaml" ~inputs ~implicit_in
             ~outputs:[target ~backend:"ocaml" "ml"]
             ~implicit_out:
               (target ~backend:"ocaml" "mli" :: implicit_out "ocaml" "ml")),
        Seq.return
          (Nj.build "catala-c" ~inputs ~implicit_in
             ~outputs:[target ~backend:"c" "c"]
             ~implicit_out:(target ~backend:"c" "h" :: implicit_out "c" "c")),
        Nj.build "python" ~inputs ~implicit_in
          ~outputs:[target ~backend:"python" "py"],
        Seq.return
          (Nj.build "catala-java" ~inputs ~implicit_in
             ~outputs:[target ~backend:"java" "java"]) )
  in
  let ocamlopt =
    let obj =
      [
        Nj.build "ocaml-bytobject"
          ~inputs:[target ~backend:"ocaml" "mli"; target ~backend:"ocaml" "ml"]
          ~implicit_in:(List.map module_target modules)
          ~outputs:(List.map (target ~backend:"ocaml") ["cmi"; "cmo"])
          ~vars:[Var.includes, include_flags "ocaml"];
        Nj.build "ocaml-natobject"
          ~inputs:[target ~backend:"ocaml" "ml"]
          ~implicit_in:
            (target ~backend:"ocaml" "cmi" :: List.map module_target modules)
          ~outputs:(List.map (target ~backend:"ocaml") ["cmx"; "o"])
          ~vars:[Var.includes, include_flags "ocaml"];
      ]
    in
    (match item.module_def with
    | Some _ ->
      obj
      @ [
          Nj.build "ocaml-module"
            ~inputs:[target ~backend:"ocaml" "cmx"]
            ~outputs:[target ~backend:"ocaml" "cmxs"];
        ]
    | None -> obj)
    @
    if has_scope_tests then
      [
        Nj.build "ocaml-natobject"
          ~inputs:[target ~backend:"ocaml" "+main.ml"]
          ~implicit_in:
            [target ~backend:"ocaml" "cmi"; target ~backend:"ocaml" "cmx"]
          ~outputs:
            (List.map
               (fun ext -> target ~backend:"ocaml" ("+main." ^ ext))
               ["cmx"; "o"])
          ~vars:[Var.includes, include_flags "ocaml" @ ["-w"; "-24"]];
      ]
    else []
  in
  let cc =
    Nj.build "c-object"
      ~inputs:[target ~backend:"c" "c"]
      ~implicit_in:
        (target ~backend:"c" "h"
        :: List.map (modfile ~backend:"c" "@c-module") modules)
      ~outputs:[target ~backend:"c" "o"]
      ~vars:[Var.includes, include_flags "c"]
    ::
    (if has_scope_tests then
       [
         Nj.build "c-object"
           ~inputs:[target ~backend:"c" "+main.c"]
           ~implicit_in:
             (target ~backend:"c" "h"
             :: List.map (modfile ~backend:"c" "@c-module") modules)
           ~outputs:[target ~backend:"c" "+main.o"]
           ~vars:[Var.includes, include_flags "c"];
       ]
     else [])
  in
  let javac =
    let java_class_path =
      String.concat ":"
        ((!Var.tdir / "java")
        :: List.map
             (fun d ->
               (if Filename.is_relative d then !Var.builddir / d else d)
               / "java")
             include_dirs)
    in
    Nj.build "java-class"
      ~inputs:[target ~backend:"java" "java"]
      ~implicit_in:(List.map (modfile ~backend:"java" ".class") modules)
      ~outputs:[target ~backend:"java" "class"]
      ~vars:[Var.class_path, [java_class_path]]
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
    match Option.map Mark.remove item.module_def with
    | Some m when List.mem (dirname src) include_dirs ->
      Nj.build "phony" ~outputs:[m ^ "@src"] ~inputs:[catala_src]
      ::
      (if List.mem OCaml enabled_backends then
         [
           Nj.build "phony"
             ~outputs:[m ^ "@ocaml-module"]
             ~inputs:[module_target m];
         ]
       else [])
      @ (if List.mem C enabled_backends then
           [
             Nj.build "phony"
               ~outputs:[m ^ "@c-module"]
               ~inputs:[modfile ~backend:"c" ".h" m];
           ]
         else [])
      @ (if List.mem Python enabled_backends then
           [
             Nj.build "phony"
               ~outputs:[m ^ "@py-module"]
               ~inputs:[modfile ~backend:"python" ".py" m];
           ]
         else [])
      @
      if List.mem Java enabled_backends then
        [
          Nj.build "phony"
            ~outputs:[m ^ "@java-module"]
            ~inputs:[modfile ~backend:"java" ".class" m];
        ]
      else []
    | _ -> []
  in
  let tests =
    if not (item.has_inline_tests || Lazy.force item.has_scope_tests) then []
    else
      [
        Nj.build "tests" ~inputs:[catala_src]
          ~implicit_in:
            (!Var.clerk_exe :: List.map (modfile "@ocaml-module") modules)
          ~outputs:[catala_src ^ "@test"; catala_src ^ "@out"];
      ]
  in
  let statements_list =
    Seq.return (Nj.comment "")
    :: List.to_seq def_vars
    :: Seq.return include_deps
    :: List.to_seq expose_module
    :: List.to_seq module_deps
    ::
    (if List.mem OCaml enabled_backends then [ocaml; List.to_seq ocamlopt]
     else [])
    @ (if List.mem C enabled_backends then [c; List.to_seq cc] else [])
    @ (if List.mem Python enabled_backends then [Seq.return python] else [])
    @ (if List.mem Java enabled_backends then [java; Seq.return javac] else [])
    @ if List.mem Tests enabled_backends then [List.to_seq tests] else []
  in
  Seq.concat (List.to_seq statements_list)

let gen_build_statements_dir
    (dir : string)
    (include_dirs : string list)
    (enabled_backends : backend list)
    (autotest : bool)
    (items : Scan.item list) : Nj.ninja =
  let same_dir_modules =
    List.filter_map
      (fun item ->
        Option.map Mark.remove item.Scan.module_def
        |> Option.map (fun name -> name, item.Scan.file_name))
      items
  in
  let check_conflicts seen item =
    let fname = item.Scan.file_name in
    let s = Scan.target_file_name item in
    match String.Map.find_opt s seen with
    | Some f1 ->
      Message.error
        "Conflicting file names:@ %S@ and@ %S@ would both generate the same \
         target file@ %S.@ Please rename one of them."
        (File.basename f1) (File.basename fname) (File.basename s)
    | None -> String.Map.add s fname seen
  in
  let _names = List.fold_left check_conflicts String.Map.empty items in
  let open File in
  let ( ! ) = Var.( ! ) in
  Seq.cons (Nj.comment "")
  @@ Seq.cons (Nj.comment ("--- " ^ dir ^ " ---"))
  @@ Seq.cons (Nj.comment "")
  @@ Seq.cons (Nj.binding Var.tdir [!Var.builddir / dir])
  @@ Seq.flat_map
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
             if
               not
                 (item.Scan.has_inline_tests
                 || Lazy.force item.Scan.has_scope_tests)
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
         (gen_build_statements_dir dir include_dirs enabled_backends autotest
            items)
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
    base_bindings ~catala_exe ~catala_flags ~build_dir ~include_dirs
      ~vars_override ~test_flags ~enabled_backends ~autotest ()
  in
  let prologue =
    Seq.return
      (Nj.Comment
         (Printf.sprintf "File generated by Clerk v.%s\n"
            Catala_utils.Cli.version))
    @+ Seq.return (Nj.Comment "- Global variables - #\n")
    @+ List.to_seq var_bindings
    @+ Seq.return (Nj.Comment "\n- Base rules - #\n")
    @+ List.to_seq (static_base_rules enabled_backends)
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
    ~autotest
    ~config_file
    ~catala_exe
    ~catala_opts
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
    let d = File.clean_path dir in
    File.ensure_dir d;
    d
    (* Note: it could be safer here to use File.(Sys.getcwd () / "_build") by
       default, but Ninja treats relative and absolute paths separately so that
       you wouldn't then be able to build target _build/foo.ml but would have to
       write the full path every time *)
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
