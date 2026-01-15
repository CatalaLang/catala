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

let backend_from_config = function
  | Clerk_config.OCaml -> OCaml
  | Clerk_config.Python -> Python
  | Clerk_config.C -> C
  | Clerk_config.Java -> Java
  | _ -> invalid_arg __FUNCTION__

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
  let ocaml_include = make "OCAML_INCLUDE"
  let runtime = make "CATALA_RUNTIME"
  let cc_exe = make "CC"
  let c_flags = make "CFLAGS"
  let c_include = make "C_INCLUDE_FLAGS"
  let python = make "PYTHON"
  let javac = make "JAVAC"
  let javac_flags = make "JAVAC_FLAGS"
  let jar = make "jar"
  let java = make "JAVA"
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

let base_bindings ~code_coverage ~autotest ~enabled_backends ~config =
  let options = config.Clerk_cli.options in
  let includes ?backend () =
    List.fold_right
      (fun dir flags ->
        if Filename.is_relative dir then
          "-I"
          :: File.(
               Var.(!builddir)
               / match backend with Some b -> dir / b | None -> dir)
          :: flags
        else "-I" :: dir :: flags)
      options.global.include_dirs []
  in
  let catala_flags =
    ("--stdlib=" ^ File.(Var.(!builddir) / Scan.libcatala))
    :: ("--directory=" ^ Var.(!builddir))
    :: options.global.catala_opts
  in
  let test_flags = config.Clerk_cli.test_flags in
  let use_default_flags = test_flags = [] && options.global.catala_opts = [] in
  let catala_flags_ocaml =
    (if autotest then ["--autotest"] else [])
    @
    if use_default_flags then ["-O"]
    else
      List.filter
        (function
          | "-O" | "--optimize" | "--closure-conversion" -> true | _ -> false)
        test_flags
  in
  let catala_flags_c =
    (if autotest then ["--autotest"] else [])
    @
    if use_default_flags then ["-O"]
    else
      List.filter
        (function "-O" | "--optimize" -> true | _ -> false)
        test_flags
  in
  let catala_flags_python =
    (if autotest then ["--autotest"] else [])
    @
    if use_default_flags then ["-O"]
    else
      List.filter
        (function
          | "-O" | "--optimize" | "--closure-conversion" -> true | _ -> false)
        test_flags
  in
  let catala_flags_java =
    (if autotest then ["--autotest"] else [])
    @
    if use_default_flags then ["-O"]
    else
      List.filter
        (function
          | "-O" | "--optimize" | "--closure-conversion" -> true | _ -> false)
        test_flags
  in
  let def var value =
    let value =
      match List.assoc_opt (Var.name var) options.variables with
      | Some vl -> vl
      | None -> Lazy.force value
    in
    var, value
  in
  [
    def Var.ninja_required_version (lazy ["1.7"]);
    (* use of implicit outputs *)
    def Var.builddir (lazy [options.global.build_dir]);
    def Var.runtime (lazy [Lazy.force Poll.runtime_dir]);
    def Var.clerk_exe (lazy [Lazy.force Poll.clerk_exe]);
    def Var.catala_exe
      (lazy
        [
          (match options.global.catala_exe with
          | Some e -> File.check_exec e
          | None -> Lazy.force Poll.catala_exe);
        ]);
    def Var.catala_flags
      (lazy
        (catala_flags
        @ (if Message.has_color stderr then ["--color=always"] else [])
        @ includes ()));
    def Var.clerk_flags
      (lazy
        ("-e"
         :: Var.(!catala_exe)
         :: ("--test-flags=" ^ String.concat "," test_flags)
         :: includes ()
        @ (if code_coverage then ["--code-coverage"] else [])
        @ List.map
            (fun f -> "--catala-opts=" ^ f)
            (catala_flags @ if code_coverage then ["--whole-program"] else [])));
  ]
  @ (if List.mem OCaml enabled_backends then
       [
         def Var.catala_flags_ocaml (lazy catala_flags_ocaml);
         def Var.ocamlc_exe (lazy ["ocamlc"]);
         def Var.ocamlopt_exe (lazy ["ocamlopt"]);
         def Var.ocaml_flags (lazy []);
         def Var.ocaml_include
           (lazy
             (Lazy.force Poll.ocaml_include_flags @ includes ~backend:"ocaml" ()));
       ]
     else [])
  @ (if List.mem Python enabled_backends then
       [
         def Var.catala_flags_python (lazy catala_flags_python);
         def Var.python (lazy ["python3"]);
       ]
     else [])
  @ (if List.mem Java enabled_backends then
       [
         def Var.catala_flags_java (lazy catala_flags_java);
         def Var.java (lazy ["java"]);
         def Var.javac (lazy ["javac"]);
         def Var.jar (lazy ["jar"]);
         def Var.javac_flags (lazy ["-implicit:none"]);
       ]
     else [])
  @
  if List.mem C enabled_backends then
    [
      def Var.catala_flags_c (lazy catala_flags_c);
      def Var.cc_exe (lazy ["cc"]);
      def Var.c_flags
        (lazy
          [
            "-std=c89";
            "-pedantic";
            "-Wall";
            "-Wno-unused-function";
            "-Wno-unused-variable";
            "-Wno-unused-but-set-variable";
            "-Werror";
            "-fPIC";
            "-g";
          ]);
      def Var.c_include
        (lazy
          (["-I"; File.(Var.(!builddir) / Scan.libcatala / "c")]
          @ includes ~backend:"c" ()));
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
  ] @ (if List.mem OCaml enabled_backends then
         let runtime_include =
           File.(Var.(!builddir) / Scan.libcatala / "ocaml")
         in
         [
      Nj.rule "catala-ocaml"
        ~command:[!catala_exe; "ocaml"; !catala_flags; !catala_flags_ocaml;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; "ocaml"; "⇒"; !output];
      Nj.rule "ocaml-bytobject"
        ~command:[
          !ocamlc_exe; "-c"; !ocaml_flags; !ocaml_include; "-I"; runtime_include; !includes; !input
        ]
        ~description:["<ocaml>"; "⇒"; !output];

      Nj.rule "ocaml-natobject"
        ~command:[
          !ocamlopt_exe; "-c"; !ocaml_flags; !ocaml_include; "-I"; runtime_include; !includes; !input
        ]
        ~description:["<ocaml>"; "⇒"; !output];

      Nj.rule "ocaml-module"
        ~command:
          [!ocamlopt_exe; "-shared"; !ocaml_flags; !ocaml_include; "-I"; runtime_include; !input;
           "-o"; !output]
        ~description:["<ocaml>"; "⇒"; !output];
    ] else []) @
  (if List.mem C enabled_backends then [
    Nj.rule "catala-c"
      ~command:[!catala_exe; "c"; !catala_flags; !catala_flags_c;
                "-o"; !output; "--"; !input]
      ~description:["<catala>"; "c"; "⇒"; !output];

    Nj.rule "c-object"
      ~command:
        [!cc_exe; !input; !c_flags; !c_include; !includes; "-c"; "-o"; !output]
      ~description:["<cc>"; "⇒"; !output];
  ] else []) @
  (if List.mem Python enabled_backends then [
      Nj.rule "catala-python"
        ~command:[!catala_exe; "python"; !catala_flags; !catala_flags_python;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; "python"; "⇒"; !output];
    ] else []) @
  (if List.mem Java enabled_backends then [
      Nj.rule "catala-java"
        ~command:[!catala_exe; "java"; !catala_flags; !catala_flags_java;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; "java"; "⇒"; !output];
      Nj.rule "java-class"
        ~command:[!javac; "-cp"; File.(Var.(!builddir) / Scan.libcatala / "java")^":" ^ !class_path; !javac_flags; !input]
        ~description:["<catala>"; "java"; "⇒"; !output];
    ] else []) @
  (if List.mem Tests enabled_backends then
     [Nj.rule "tests"
        ~command:
          [!clerk_exe; "runtest"; !clerk_flags; !input;
           "--report"; !output;]
        ~description:["<catala>"; "tests"; "⇐"; !input];

      Nj.rule "dir-tests"
        ~command:
        (if Sys.win32 then
          ["cmd"; "/c"; "type"; "nul" ; !input; ">"; !output; ";"]
        else
          ["cat"; !input; ">"; !output; ";"]
        )
        ~description:["<test>"; !test_id];
     ]
   else [])

let gen_build_statements
    (include_dirs : string list)
    (enabled_backends : backend list)
    (autotest : bool)
    (same_dir_modules : (string * File.t) list)
    ~is_stdlib
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
           (fun (f, _) ->
             if dir / basename f = f then !Var.tdir / basename f
             else !Var.builddir / f)
           item.included_files
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
  let extern_src backend ext missing =
    let f = src -.- ext in
    match check_file f with
    | Some f -> f, missing
    | None -> (
      match File.check_file (dirname f / backend / basename f) with
      | Some f -> f, missing
      | None -> f, f :: missing)
  in
  let check_missing backend missing =
    if missing <> [] then
      Message.error
        ~pos:(Mark.get (Option.get item.module_def))
        "@[<v>@[<hov>Module @{<blue>%s@} is marked as external,@ but@ the@ \
         following@ files@ are@ missing:@ %a@]@,\
         @,\
         @[<hov 2>@{<bold>Hint:@} to generate a template, you can use:@ \
         @{<magenta>catala %s --gen-external %s@}@]@]"
        (Mark.remove (Option.get item.module_def))
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           File.format)
        missing backend src
  in
  let ocaml, c, python, java =
    if item.extrnal then
      let ocaml =
        if not (List.mem OCaml enabled_backends) then Seq.empty
        else
          let ml, missing = extern_src "ocaml" "ml" [] in
          let mli, missing = extern_src "ocaml" "mli" missing in
          check_missing "ocaml" missing;
          List.to_seq
            [
              Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[ml]
                ~outputs:[target ~backend:"ocaml" "ml"];
              Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[mli]
                ~outputs:[target ~backend:"ocaml" "mli"];
            ]
      in
      let c =
        if not (List.mem C enabled_backends) then Seq.empty
        else
          let c, missing = extern_src "c" "c" [] in
          let h, missing = extern_src "c" "h" missing in
          check_missing "c" missing;
          List.to_seq
            [
              Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[c]
                ~outputs:[target ~backend:"c" "c"];
              Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[h]
                ~outputs:[target ~backend:"c" "h"];
            ]
      in
      let python =
        if not (List.mem Python enabled_backends) then Seq.empty
        else
          let py, missing = extern_src "python" "py" [] in
          check_missing "python" missing;
          List.to_seq
            [
              Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[py]
                ~outputs:[target ~backend:"python" "py"];
            ]
      in
      let java =
        if not (List.mem Java enabled_backends) then Seq.empty
        else
          let java, missing = extern_src "java" "java" [] in
          check_missing "java" missing;
          List.to_seq
            [
              Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[java]
                ~outputs:[target ~backend:"java" "java"];
            ]
      in
      ocaml, c, python, java
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
      let vars =
        if is_stdlib then
          Some [Var.catala_flags, [Var.(!catala_flags); "--no-stdlib"]]
        else None
      in
      ( Seq.return
          (Nj.build "catala-ocaml" ?vars ~inputs ~implicit_in
             ~outputs:[target ~backend:"ocaml" "ml"]
             ~implicit_out:
               (target ~backend:"ocaml" "mli" :: implicit_out "ocaml" "ml")),
        Seq.return
          (Nj.build "catala-c" ?vars ~inputs ~implicit_in
             ~outputs:[target ~backend:"c" "c"]
             ~implicit_out:(target ~backend:"c" "h" :: implicit_out "c" "c")),
        Seq.return
          (Nj.build "catala-python" ?vars ~inputs ~implicit_in
             ~outputs:[target ~backend:"python" "py"]),
        Seq.return
          (Nj.build "catala-java" ?vars ~inputs ~implicit_in
             ~outputs:[target ~backend:"java" "java"]) )
  in
  let ocamlopt =
    let obj =
      [
        Nj.build "ocaml-bytobject"
          ~inputs:[target ~backend:"ocaml" "mli"; target ~backend:"ocaml" "ml"]
          ~implicit_in:(List.map module_target modules @ ["@runtime-cmi"])
          ~outputs:(List.map (target ~backend:"ocaml") ["cmi"; "cmo"])
          ~vars:
            [
              Var.includes, include_flags "ocaml";
              ( Var.ocaml_flags,
                [
                  Var.(!ocaml_flags);
                  "-opaque";
                  "-w";
                  "@1..3@5..28@31..39@43@46..47@49..57@61..62@67@69-40";
                  "-strict-sequence";
                  "-strict-formats";
                  "-short-paths";
                  "-keep-locs";
                  "-warn-error";
                  "-a+8";
                  "-w";
                  "-67";
                  "-bin-annot";
                  "-no-alias-deps";
                ] );
            ];
        Nj.build "ocaml-natobject"
          ~inputs:[target ~backend:"ocaml" "ml"]
          ~implicit_in:
            ((target ~backend:"ocaml" "cmi" :: List.map module_target modules)
            @ ["@runtime-cmi"])
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
        :: "@runtime-c"
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
             :: "@runtime-c"
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
      ~implicit_in:
        ("@runtime-java" :: List.map (modfile ~backend:"java" ".class") modules)
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
    match item.module_def with
    | Some m when item.is_stdlib || List.mem dir include_dirs ->
      let modname = Mark.remove m in
      Nj.build "phony" ~outputs:[modname ^ "@src"] ~inputs:[catala_src]
      ::
      (if List.mem OCaml enabled_backends then
         [
           Nj.build "phony"
             ~outputs:[modname ^ "@ocaml-module"]
             ~inputs:[module_target modname];
         ]
       else [])
      @ (if List.mem C enabled_backends then
           [
             Nj.build "phony"
               ~outputs:[modname ^ "@c-module"]
               ~inputs:[modfile ~backend:"c" "@c-module" modname];
           ]
         else [])
      @ (if List.mem Python enabled_backends then
           [
             Nj.build "phony"
               ~outputs:[modname ^ "@python-module"]
               ~inputs:[modfile ~backend:"python" ".py" modname];
           ]
         else [])
      @
      if List.mem Java enabled_backends then
        [
          Nj.build "phony"
            ~outputs:[modname ^ "@java-module"]
            ~inputs:[modfile ~backend:"java" ".class" modname];
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
    @ (if List.mem Python enabled_backends then [python] else [])
    @ (if List.mem Java enabled_backends then [java; Seq.return javac] else [])
    @ if List.mem Tests enabled_backends then [List.to_seq tests] else []
  in
  Seq.concat (List.to_seq statements_list)

let gen_build_statements_dir
    ~is_stdlib
    (dir : string)
    (include_dirs : string list)
    (enabled_backends : backend list)
    (autotest : bool)
    (items : Scan.item list) : Nj.ninja =
  let same_dir_modules =
    List.filter_map
      (fun item ->
        Option.map
          (fun name -> Mark.remove name, item.Scan.file_name)
          item.Scan.module_def)
      items
  in
  let check_conflicts seen item =
    let fname = item.Scan.file_name in
    let s = Scan.target_file_name item in
    match String.Map.find_opt s seen with
    | Some f1 ->
      Message.error
        "Conflicting file names:@ %a@ and@ %a@ would both generate the same \
         target file@ %a.@ Please rename one of them."
        File.format (File.basename f1) File.format (File.basename fname)
        File.format (File.basename s)
    | None -> String.Map.add s fname seen
  in
  let _names = List.fold_left check_conflicts String.Map.empty items in
  let dir =
    if Filename.is_relative dir (* Detect stdlib modules *) then dir
    else Scan.libcatala
  in
  let open File in
  let ( ! ) = Var.( ! ) in
  Seq.cons (Nj.comment "")
  @@ Seq.cons (Nj.comment ("--- " ^ dir ^ " ---"))
  @@ Seq.cons (Nj.comment "")
  @@ Seq.cons (Nj.binding Var.tdir [!Var.builddir / dir])
  @@ Seq.flat_map
       (gen_build_statements ~is_stdlib include_dirs enabled_backends autotest
          same_dir_modules)
       (List.to_seq items)

let dir_test_rules dir subdirs enabled_backends items =
  let open File in
  if List.mem Tests enabled_backends then
    let subdirs =
      List.filter
        (fun d ->
          Lazy.force Poll.catala_source_tree_root = None
          || not (String.starts_with d ~prefix:"stdlib"))
        subdirs
    in
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

let runtime_build_statements ~config enabled_backends =
  let open File in
  let stdbase = Var.(!builddir) / Scan.libcatala in
  let runtime_orig =
    (* content of the variable [Var.(!runtime)] *)
    match
      List.assoc_opt Var.(name runtime) config.Clerk_cli.options.variables
    with
    | Some r -> lazy (String.concat " " r)
    | None -> Poll.runtime_dir
  in
  (if List.mem OCaml enabled_backends then
     let ocaml_src = Var.(!runtime) / "ocaml" in
     let dates_base = stdbase / "ocaml" / "dates_calc" in
     let ocaml_base = stdbase / "ocaml" / "catala_runtime" in
     let runtime_cmi, dates_cmi =
       (* This one is tricky: in order for the catala interpreter to be able to
          dynlink compiled Catala modules, we need to be sure that they have
          been linked with a runtime abiding by the exact same cmi. Hence we
          need to distribute the cmi with the runtime library, and to fetch it
          from dune's _build when in the catala tree *)
       if Lazy.force Poll.catala_source_tree_root = None then
         ocaml_src / "catala_runtime.cmi", ocaml_src / "dates_calc.cmi"
       else
         ( Lazy.force Poll.runtime_dir
           /../ "_build"
           / "default"
           / "runtimes"
           / "ocaml"
           / "catala_runtime.cmi",
           Lazy.force Poll.runtime_dir
           /../ "_build"
           / "default"
           / "runtimes"
           / "ocaml"
           / "dates_calc.cmi" )
       (* This won't work if dune is not in its standard configuration and
          "default" profile, but that won't affect anything outside of running
          clerk from the catala source tree so it should be fine *)
     in
     [
       Nj.build "phony"
         ~inputs:
           [
             dates_base -.- "mli";
             dates_base -.- "cmi";
             ocaml_base -.- "mli";
             ocaml_base -.- "cmi";
             Var.(!catala_exe);
           ]
         ~outputs:["@runtime-cmi"];
       Nj.build "phony"
         ~inputs:
           [
             dates_base -.- "ml";
             dates_base -.- "mli";
             ocaml_base -.- "ml";
             ocaml_base -.- "mli";
           ]
         ~outputs:["@runtime-ocaml-src"];
       Nj.build "phony"
         ~inputs:[ocaml_base -.- "cmx"]
         ~implicit_in:[dates_base -.- "cmi"]
         ~outputs:["@runtime-ocaml"];
       Nj.build "copy"
         ~inputs:[ocaml_src / "catala_runtime.mli"]
         ~outputs:[ocaml_base -.- "mli"];
       Nj.build "copy" ~inputs:[runtime_cmi] ~outputs:[ocaml_base -.- "cmi"];
       Nj.build "copy" ~inputs:[dates_cmi] ~outputs:[dates_base -.- "cmi"];
       Nj.build "copy"
         ~inputs:[ocaml_src / "catala_runtime.ml"]
         ~outputs:[ocaml_base -.- "ml"];
       Nj.build "copy"
         ~inputs:[dates_cmi -.- "ml"]
         ~outputs:[dates_base -.- "ml"];
       Nj.build "copy"
         ~inputs:[dates_cmi -.- "mli"]
         ~outputs:[dates_base -.- "mli"];
       Nj.build "ocaml-natobject"
         ~inputs:[dates_base -.- "ml"; ocaml_base -.- "ml"]
         ~implicit_in:[dates_base -.- "cmi"; ocaml_base -.- "cmi"]
         ~outputs:[ocaml_base -.- "cmx"; ocaml_base -.- "o"];
     ]
   else [])
  @ (if List.mem C enabled_backends then
       let c_base = stdbase / "c" / "catala_runtime" in
       let c_src = Var.(!runtime) / "c" in
       [
         Nj.build "phony"
           ~inputs:
             [
               c_base -.- "c";
               c_base -.- "h";
               (c_base /../ "dates_calc") -.- "c";
               (c_base /../ "dates_calc") -.- "h";
             ]
           ~outputs:["@runtime-c-src"];
         Nj.build "phony"
           ~inputs:
             [
               c_base -.- "o";
               c_base -.- "h";
               (c_base /../ "dates_calc") -.- "o";
               (c_base /../ "dates_calc") -.- "h";
               Var.(!catala_exe);
             ]
           ~outputs:["@runtime-c"];
         Nj.build "copy"
           ~inputs:[c_src / "catala_runtime.h"]
           ~outputs:[c_base -.- "h"];
         Nj.build "copy"
           ~inputs:[c_src / "catala_runtime.c"]
           ~outputs:[c_base -.- "c"];
         Nj.build "copy"
           ~inputs:[c_src / "dates_calc.h"]
           ~outputs:[(c_base /../ "dates_calc") -.- "h"];
         Nj.build "copy"
           ~inputs:[c_src / "dates_calc.c"]
           ~outputs:[(c_base /../ "dates_calc") -.- "c"];
         Nj.build "c-object"
           ~inputs:[c_base -.- "c"]
           ~implicit_in:[c_base -.- "h"]
           ~outputs:[c_base -.- "o"];
         Nj.build "c-object"
           ~inputs:[(c_base /../ "dates_calc") -.- "c"]
           ~implicit_in:[(c_base /../ "dates_calc") -.- "h"]
           ~outputs:[(c_base /../ "dates_calc") -.- "o"];
       ]
     else [])
  @ (if List.mem Python enabled_backends then
       let python_base = stdbase / "python" / "catala_runtime" in
       let python_src = Var.(!runtime) / "python" / "src" / "catala" in
       [
         Nj.build "phony"
           ~inputs:
             [
               python_base -.- "py";
               python_base /../ "dates.py";
               Var.(!catala_exe);
             ]
           ~outputs:["@runtime-python"];
         Nj.build "copy"
           ~inputs:[python_src / "dates.py"]
           ~outputs:[python_base /../ "dates.py"];
         Nj.build "copy"
           ~inputs:[python_src / "catala_runtime.py"]
           ~outputs:[python_base -.- "py"];
       ]
     else [])
  @
  if List.mem Java enabled_backends then
    let java_base = stdbase / "java" in
    let java_src = Var.(!runtime) / "java" in
    let java_orig_prefix = Lazy.force runtime_orig / "java" in
    let java_files =
      File.scan_tree
        (fun f ->
          let base = File.basename f in
          if
            Filename.check_suffix base ".java"
            && base = String.capitalize_ascii base
          then Some (File.remove_prefix java_orig_prefix f)
          else None)
        java_orig_prefix
      |> Seq.flat_map (fun (_, _, files) -> List.to_seq files)
      |> Seq.map (File.remove_prefix java_src)
      |> List.of_seq
    in
    let java_list_file =
      let base =
        config.Clerk_cli.options.global.build_dir / Scan.libcatala / "java"
      in
      File.with_out_channel (base / "java.files") (fun oc ->
          List.iter (fun s -> output_string oc ((base / s) ^ "\n")) java_files);
      java_base / "java.files"
    in
    Nj.build "phony"
      ~inputs:(List.map (fun f -> (java_base / f) -.- "java") java_files)
      ~outputs:["@runtime-java-src"]
    :: Nj.build "phony"
         ~inputs:(List.map (fun f -> (java_base / f) -.- "class") java_files)
         ~outputs:["@runtime-java"]
    :: Nj.build "java-class" ~inputs:[]
         ~implicit_in:
           (java_list_file :: List.map (fun f -> java_base / f) java_files)
         ~outputs:(List.map (fun f -> (java_base / f) -.- "class") java_files)
         ~vars:[Var.javac_flags, [Var.(!javac_flags); "@" ^ java_list_file]]
    :: List.map
         (fun f ->
           Nj.build "copy" ~inputs:[java_src / f] ~outputs:[java_base / f])
         java_files
  else []

let output_ninja_file_header pp ~config ~enabled_backends ~var_bindings =
  pp
    (Nj.Comment
       (Printf.sprintf "File generated by Clerk v.%s\n" Catala_utils.Cli.version));
  pp (Nj.Comment "- Global variables - #\n");
  List.iter (fun (var, contents) -> pp (Nj.binding var contents)) var_bindings;
  pp (Nj.Comment "\n- Base rules - #\n");
  List.iter pp (static_base_rules enabled_backends);
  pp (Nj.Comment "\n- Runtime build statements - #\n");
  List.iter pp (runtime_build_statements ~config enabled_backends)

let output_ninja_file_item_statements
    nin_ppf
    ~config
    ~enabled_backends
    ~autotest
    ~is_stdlib
    item_tree
    next =
  let rec print_and_get_items seq () =
    match seq () with
    | Seq.Cons ((dir, subdirs, items), seq) ->
      Nj.format nin_ppf
      @@ gen_build_statements_dir dir ~is_stdlib
           config.Clerk_cli.options.global.include_dirs enabled_backends
           autotest items;
      if not is_stdlib then
        Nj.format nin_ppf @@ dir_test_rules dir subdirs enabled_backends items;
      Seq.append (List.to_seq items) (print_and_get_items seq) ()
    | Seq.Nil -> next ()
  in
  print_and_get_items (Seq.once item_tree)

let output_ninja_file
    nin_ppf
    ~config
    ~enabled_backends
    ~autotest
    ~var_bindings
    stdlib_tree
    project_tree =
  let pp nj =
    Nj.format_def nin_ppf nj;
    Format.pp_print_cut nin_ppf ()
  in
  output_ninja_file_header pp ~config ~enabled_backends ~var_bindings;
  pp (Nj.Comment "\n- Standard library build statements - #");
  Seq.memoize
  @@ output_ninja_file_item_statements nin_ppf ~config ~enabled_backends
       ~autotest ~is_stdlib:true stdlib_tree
  @@ Seq.append (fun () ->
      pp (Nj.Comment "\n- Project-specific build statements - #");
      Seq.Nil)
  @@ output_ninja_file_item_statements nin_ppf ~config ~enabled_backends
       ~autotest ~is_stdlib:false project_tree
  @@ fun () ->
  pp (Nj.Comment "\n- Global rules and defaults - #\n");
  if List.mem Tests enabled_backends then
    pp
      (Nj.build "phony" ~outputs:["test"]
         ~inputs:[File.(Var.(!builddir / ".@test"))]);
  Seq.Nil

(** {1 Driver} *)

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

let ninja_exec = try Sys.getenv "NINJA_BIN" with Not_found -> "ninja"

let ninja_version =
  lazy
    (try
       File.process_out
         ~check_exit:(function 0 -> () | _ -> raise Exit)
         ninja_exec ["--version"]
       |> String.trim
       |> String.split_on_char '.'
       |> List.map int_of_string
     with Exit | Failure _ -> [])

let with_ninja_process
    ~config
    ~clean_up_env
    ~ninja_flags
    ~quiet
    (callback : Format.formatter -> 'a) =
  let env = if clean_up_env then cleaned_up_env () else Unix.environment () in
  let fname =
    match config.Clerk_cli.ninja_file with
    | Some fname -> Some fname
    | None ->
      if Global.options.debug then
        Some File.(config.options.global.build_dir / "clerk.ninja")
      else None
  in
  let ninja_process nin_file nin_fd =
    let args =
      let ninja_flags =
        (* Newer versions of ninja have a flag to not print "nothing to do", we
           use that if available. *)
        if (not Global.options.debug) && Lazy.force ninja_version >= [1; 12]
        then "--quiet" :: ninja_flags
        else ninja_flags
      in
      ("-f" :: nin_file :: ninja_flags)
      @ if Catala_utils.Global.options.debug then ["-v"] else []
    in
    let cmdline = ninja_exec :: args in
    Message.debug "executing '%s'..." (String.concat " " cmdline);
    let npid =
      let out =
        (* In --quiet, we redirect all ninja's output to /dev/null *)
        if quiet then Unix.openfile Filename.null [O_WRONLY] 0o111
        else Unix.stdout
      in
      Fun.protect
        ~finally:(fun () -> if quiet then Unix.close out)
        (fun () ->
          Unix.create_process_env ninja_exec (Array.of_list cmdline) env nin_fd
            out Unix.stderr)
    in
    let rec wait () =
      match Unix.waitpid [] npid with
      | _, Unix.WEXITED n -> n
      | _, (Unix.WSIGNALED n | Unix.WSTOPPED n) -> 128 - n
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
    in
    ( npid,
      fun () ->
        match wait () with 0 -> () | n -> raise (Catala_utils.Cli.Exit_with n) )
  in
  match fname with
  | Some fname ->
    let ret = File.with_formatter_of_file fname callback in
    let _, wait = ninja_process fname Unix.stdin in
    wait ();
    ret
  | None when Sys.os_type = "Win32" ->
    (* ninja requires the name of the file on the cli. No /dev/stdin on
       Windows *)
    File.with_temp_file "clerk_build_" ".ninja"
    @@ fun fname ->
    let ret = File.with_formatter_of_file fname callback in
    let _, wait = ninja_process fname Unix.stdin in
    wait ();
    ret
  | None ->
    let ninja_in, clerk_out = Unix.pipe ~cloexec:true () in
    let npid, wait = ninja_process "/dev/stdin" ninja_in in
    let callback_ret =
      try
        Unix.close ninja_in;
        File.with_formatter_of_out_channel
          (Unix.out_channel_of_descr clerk_out)
          callback
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        Message.debug "Exception caught, killing the ninja sub-process";
        Unix.kill npid Sys.sigkill;
        (try wait () with _ -> ());
        Unix.close clerk_out;
        Printexc.raise_with_backtrace e bt
    in
    Unix.close clerk_out;
    wait ();
    callback_ret

let run_ninja
    ~config
    ?(enabled_backends = all_backends)
    ~quiet
    ~code_coverage
    ~autotest
    ?(clean_up_env = false)
    ?(ninja_flags = [])
    callback =
  let enabled_backends =
    if autotest then OCaml :: enabled_backends else enabled_backends
  in
  let var_bindings =
    base_bindings ~code_coverage ~config ~enabled_backends ~autotest
  in
  with_ninja_process ~config ~clean_up_env ~ninja_flags ~quiet (fun nin_ppf ->
      let insource = Lazy.force Poll.catala_source_tree_root <> None in
      let stdlib_dir = Lazy.force Poll.stdlib_dir in
      let stdlib_tree =
        Scan.tree stdlib_dir |> Seq.map (fun (f, fl, items) -> f, fl, items)
      in
      let item_tree =
        Scan.tree "."
        |> Seq.filter_map (fun (f, fl, items) ->
            if insource && String.starts_with f ~prefix:"stdlib" then None
            else
              let items =
                List.map
                  (fun it ->
                    let used_modules =
                      match Scan.get_lang it.Scan.file_name with
                      | Some lg ->
                        let lg =
                          if Global.has_localised_stdlib lg then lg
                          else Global.En
                        in
                        ( "Stdlib_" ^ Cli.language_code lg,
                          Pos.from_info f 0 0 0 0 )
                        :: it.Scan.used_modules
                      | None -> it.Scan.used_modules
                    in
                    { it with Scan.used_modules })
                  items
              in
              Some (f, fl, items))
      in
      let items =
        output_ninja_file nin_ppf ~config ~enabled_backends ~autotest
          ~var_bindings stdlib_tree item_tree
      in
      let ret = callback nin_ppf (List.of_seq items) var_bindings in
      Format.pp_print_newline nin_ppf ();
      ret)
