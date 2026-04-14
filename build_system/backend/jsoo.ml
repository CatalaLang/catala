(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>.

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

module Backend = struct
  open Clerk_utils
  open Clerk_utils.Var
  open Clerk_lib
  open File
  module Nj = Ninja_utils

  let name = "jsoo"
  let module_ext = "@jsoo-module"
  let subdir = "jsoo"
  let src_extensions = ["ml"; "mli"]
  let obj_extensions = []
  let jsoo_include = make "JSOO_INCLUDE"
  let catala_flags_jsoo = make "CATALA_FLAGS_JSOO"
  let js_of_ocaml_exe = make "JS_OF_OCAML_EXE"
  let js_of_ocaml_flags = make "JS_OF_OCAML_FLAGS"
  let ppx_jsoo = make "PPX_JSOO"
  let jsoo_command = make "JSOO_COMMAND"
  let runtime_jsoo = make "CATALA_RUNTIME_JSOO"

  let runtime_jsoo_dir =
    lazy
      (let d =
         match
           File.check_directory
             File.(
               Catala_utils.Cli.exec_dir /../ "lib" / "catala-js" / "runtime")
         with
         | Some d -> File.clean_path d
         | None ->
           File.(
             clean_path (Lazy.force Poll.ocaml_libdir / "catala-js" / "runtime"))
       in
       match File.check_directory d with
       | Some dir ->
         Message.debug "Catala runtime libraries found at @{<bold>%s@}." dir;
         [dir]
       | None ->
         Message.error
           "@[<hov>Could not locate the Catala runtime library at %s.@ Make \
            sure that either catala is correctly installed,@ or you are \
            running from the root of a compiled source tree.@]"
           d)

  let ppx = make "ppx"
  let runtime_targets ~only_source:_ = ["@runtime-jsoo"]

  let jsoo_modfile ?(suffix = "") ~ext (dir, _, modname) =
    Var.(!builddir) / dir / name / (String.to_id modname ^ suffix ^ ext)

  let modfile ~is_stdlib:_ = Ninja.modfile ~backend:name

  type jsoo_cmo = { cmo : string list; jsoo_cmo : string list }

  let retrieve_cmos ~stdlib_tree ~project_tree ~externls targets =
    let pos_targets = List.map Mark.ghost targets in
    let project_items = Scan.linking_tree project_tree pos_targets in
    let stdlib_opt =
      match project_items with
      | [] -> None
      | (_, _, item) :: _ -> Ninja.get_stdlib_module item.Scan.file_name
    in
    let stdlib_module = Option.value ~default:"Stdlib_en" stdlib_opt in
    (* Retrieve the topological order for compiling the stdlib *)
    let stdlib_items =
      Scan.linking_tree stdlib_tree [Mark.ghost stdlib_module]
    in
    (* Get the name of the module (each element of the stdlib should have a
       module name) so the filter doesn't really apply *)
    let stdlib_modules =
      List.filter_map
        (fun (_, _, item) ->
          Option.map
            (fun module_name -> Scan.libcatala, [], Mark.remove module_name)
            item.Scan.module_def)
        stdlib_items
    in
    (* Get the name of the modules, be careful maybe some file are missed *)
    let project_modules =
      List.filter_map
        (fun (dir, sub_dirs, item) ->
          Option.map
            (fun name -> dir, sub_dirs, Mark.remove name)
            item.Scan.module_def)
        project_items
    in
    let stdlib_cmo = List.map (jsoo_modfile ~ext:".cmo") stdlib_modules in
    let stdlib_jsoo_cmo =
      List.filter_map
        (fun (dir, sub_dir, modname) ->
          (* Internal files are written as external librairies in catala and
             only expose english function. I think it's not necessary to expose
             those function because you could write your catala project in
             french for example *)
          if List.mem modname externls then None
          else
            let cmo_file =
              jsoo_modfile ~suffix:"_jsoo" ~ext:".cmo" (dir, sub_dir, modname)
            in
            Some cmo_file)
        stdlib_modules
    in
    let project_cmo = List.map (jsoo_modfile ~ext:".cmo") project_modules in
    let project_jsoo_cmo =
      List.filter_map
        (fun (dir, sub_dirs, modname) ->
          if List.mem modname externls then None
          else
            Some
              (jsoo_modfile ~suffix:"_jsoo" ~ext:".cmo" (dir, sub_dirs, modname)))
        project_modules
    in
    ( { cmo = stdlib_cmo; jsoo_cmo = stdlib_jsoo_cmo },
      { cmo = project_cmo; jsoo_cmo = project_jsoo_cmo } )

  module Flags = struct
    (* Using this flag in ocaml command will use the js_of_ocaml ppx on ocaml
       files *)

    let ocaml_libdir = Clerk_utils.Poll.ocaml_libdir

    let ppx_jsoo_exec : string list Lazy.t =
      lazy
        (let lib = "js_of_ocaml-ppx" in
         match File.(check_directory (Lazy.force ocaml_libdir / lib)) with
         | None ->
           Message.error
             "Required OCaml library not found at %a.@ Try `opam install %s'"
             File.format
             File.(Lazy.force ocaml_libdir / lib)
             lib
         | Some lib ->
           let ppx =
             let open File in
             let exec = lib / "ppx.exe" in
             Format.sprintf "'%s -as-ppx'" exec
           in
           ["-ppx"; ppx])

    (* Using this flag in ocaml command will include js_of_ocaml *)
    let jsoo_include_and_lib_flags : (string list * string list) Lazy.t =
      lazy
        (let link_libs = ["js_of_ocaml"] in
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
                   String.map (function '-' -> '_' | c -> c) lib ^ ".cma" ))
             link_libs
         in
         let includes, libs = List.split includes_libs in
         List.concat includes, libs)

    let jsoo_include_flags : string list Lazy.t =
      lazy (fst (Lazy.force jsoo_include_and_lib_flags))

    let default
        ~variables
        ~autotest
        ~use_default_flags
        ~test_flags
        ~include_dirs =
      let jsoo_flags =
        Common.Flags.catala_backend_flags ~autotest ~use_default_flags
          ~test_flags ~accepts_closure_conversion:true
      in
      let def = Common.Flags.def ~variables in
      (if not (List.mem Clerk_config.OCaml []) then
         Ocaml.Backend.Flags.default ~variables ~autotest ~use_default_flags
           ~test_flags ~include_dirs
       else [])
      @ [
          def runtime_jsoo runtime_jsoo_dir;
          def jsoo_include
            (lazy
              (Lazy.force jsoo_include_flags
              @ Lazy.force Ocaml.Backend.Flags.ocaml_include
              @ Common.Flags.includes ~backend:"jsoo" include_dirs));
          def catala_flags_jsoo (lazy jsoo_flags);
          def js_of_ocaml_exe (lazy ["js_of_ocaml"]);
          def js_of_ocaml_flags (lazy ["--no-sourcemap"]);
          def ppx_jsoo (lazy (Lazy.force ppx_jsoo_exec));
        ]
  end

  let target_uniq_fileame ~dir ~module_targets ext =
    File.((dir / String.concat "_" module_targets) ^ "." ^ ext)

  let extra_rules ~externls ~stdlib_tree ~project_tree module_targets =
    let open File in
    let stdlib, project =
      retrieve_cmos ~stdlib_tree ~project_tree ~externls module_targets
    in
    (* Retrieve the compiled files needed for compiling the project to bytes,
       the order is important *)
    let _opam_dir = Lazy.force Flags.ocaml_libdir in
    let zarith = _opam_dir / "zarith" / "zarith.cma" in
    let js_of_ocaml_runtime =
      _opam_dir / "js_of_ocaml-compiler" / "runtime" / "jsoo_runtime.cma"
    in
    let js_of_ocaml = _opam_dir / "js_of_ocaml" / "js_of_ocaml.cma" in
    let dates_calc =
      File.(Var.(!builddir) / Scan.libcatala / "jsoo" / "dates_calc.cmo")
    in
    let catala_runtime =
      File.(Var.(!builddir) / Scan.libcatala / "jsoo" / "catala_runtime.cmo")
    in
    let dates_calc_jsoo =
      File.(Var.(!builddir) / Scan.libcatala / "jsoo" / "dates_calc_jsoo.cmo")
    in
    let catala_runtime_jsoo =
      File.(
        Var.(!builddir) / Scan.libcatala / "jsoo" / "catala_runtime_jsoo.cmo")
    in
    (* Retrieving the cmo for the stdlib *)
    let big_integer = _opam_dir / "zarith_stubs_js" / "biginteger.js" in
    let big_integer_input =
      if File.exists big_integer then [big_integer] else []
    in
    let runtime_js = _opam_dir / "zarith_stubs_js" / "runtime.js" in
    let jsoo_file =
      target_uniq_fileame ~dir:Var.(!tdir) ~module_targets "exe"
    in
    let catala_js = target_uniq_fileame ~dir:Var.(!tdir) ~module_targets "js" in
    [
      Nj.Comment "\n- OCaml compilation binary for JSOO - #\n";
      Nj.comment "";
      Nj.comment "";
      Nj.build "jsoo-exe"
        ~inputs:
          (zarith
          :: js_of_ocaml_runtime
          :: js_of_ocaml
          :: dates_calc
          :: catala_runtime
          :: dates_calc_jsoo
          :: catala_runtime_jsoo
          :: (stdlib.cmo @ stdlib.jsoo_cmo @ project.cmo @ project.jsoo_cmo))
        ~outputs:[jsoo_file];
      Nj.build "js_of_ocaml"
        ~inputs:(runtime_js :: jsoo_file :: big_integer_input)
        ~outputs:[catala_js];
      Nj.build "phony" ~inputs:[catala_js] ~outputs:["@javascript-file"];
      Nj.comment "";
    ]

  let copy_to_target ~build_dir ~prefix_dir ~target ~install_targets:_ =
    let runtime_dir = File.(build_dir / Scan.libcatala / subdir) in
    Message.warning
      "The javascript file has been generated using an OCaml library, if you \
       want to see those file you can dive in the multiple @{<bold>jsoo@} \
       directories in @{<cyan>%s@}"
      runtime_dir;
    let dir = prefix_dir / "js" in
    ensure_dir dir;
    let src =
      target_uniq_fileame ~dir:build_dir
        ~module_targets:target.Clerk_config.tmodules "js"
    in
    copy_in ~dir ~src

  let[@ocamlformat "disable"] static_base_rules backends =
             let runtime_include =
           File.(Var.(!builddir) / Scan.libcatala / "jsoo")
         in
         (if not (List.mem Ocaml.Backend.name backends) then
      Ocaml.Backend.static_base_rules backends
        else
          []) @
         [
      Nj.rule "jsoo-bytobject"
        ~command:[
          !ocamlc_exe; "-c"; !ocaml_flags; !jsoo_include; "-I"; runtime_include; !includes; !ppx; !input
        ]
        ~description:["<ocaml>"; "⇒"; !output];
      Nj.rule "jsoo-exe"
        ~command:[
          !ocamlc_exe; !ocaml_flags; !jsoo_include; "-I"; runtime_include; !includes; !ppx; !input; "-o"; !output;
        ]
        ~description:["<ocaml>"; "⇒"; !output];
      Nj.rule "catala-jsoo"
        ~command:[!catala_exe; "jsoo"; !catala_flags; !catala_flags_jsoo;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; "jsoo"; "⇒"; !output];
      Nj.rule "catala-binding-jsoo"
        ~command:[!catala_exe; "binding-jsoo"; !catala_flags; !catala_flags_jsoo;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; "jsoo"; "⇒"; !output];
      Nj.rule "js_of_ocaml"
        ~command:[!js_of_ocaml_exe; !jsoo_command; !js_of_ocaml_flags;
                  "-o"; !output; "--"; !input]
        ~description:["<js_of_ocaml>"; "jsoo"; "⇒"; !output];
    ]

  let external_copy item =
    let catala_src = !Var.tdir / !Var.src in
    if item.Scan.is_stdlib then (
      (* For an external from the stdlib, implementation are in OCaml *)
      let ml, missing =
        Ninja.extern_src ~filename:item.Scan.file_name ~backend:"ocaml"
          ~ext:"ml" ~missing:[]
      in
      let mli, missing =
        Ninja.extern_src ~filename:item.Scan.file_name ~backend:"ocaml"
          ~ext:"mli" ~missing
      in
      Ninja.check_missing ~backend:"ocaml" ~module_def:item.Scan.module_def
        ~missing ~filename:item.file_name;
      List.to_seq
        [
          Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[ml]
            ~outputs:[Ninja.target ~backend:"jsoo" "ml"];
          Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[mli]
            ~outputs:[Ninja.target ~backend:"jsoo" "mli"];
        ])
    else
      (* catala-jsoo will build the interface to export in JS *)
      (* We will also need an file with *)
      let js, missing =
        Ninja.extern_src ~filename:item.Scan.file_name ~backend:"jsoo" ~ext:"js"
          ~missing:[]
      in
      let missing_files =
        if missing = [] then []
        else
          [
            Nj.build "catala-jsoo" ~inputs:[catala_src]
              ~vars:[Var.(catala_flags, [!catala_flags; "--gen-external"])]
              ~implicit_in:[!Var.catala_exe] ~outputs:[js];
          ]
      in
      List.to_seq
        (missing_files
        @ [
            Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[js]
              ~outputs:[Ninja.target ~backend:"jsoo" "js"];
            Nj.build "catala-binding-jsoo" ~inputs:[catala_src]
              ~implicit_in:[Ninja.target ~backend:"jsoo" "js"; !Var.catala_exe]
              ~outputs:[Ninja.target ~backend:"jsoo" "ml"]
              ~implicit_out:[Ninja.target ~backend:"jsoo" "mli"];
          ])

  (* runtime_jsoo_statements handles the compilation of the jsoo runtime into
     byte object (.cmo) for later compilation *)
  let runtime_jsoo_statements
      ~catala_runtime_jsoo
      ~jsoo_src
      ~dates_ocaml_base
      ~runtime_ocaml_base
      ~dates_jsoo_base
      ~runtime_jsoo_base =
    let open File in
    [
      Nj.build "phony"
        ~inputs:
          [
            dates_jsoo_base -.- "cmo";
            runtime_jsoo_base -.- "cmo";
            (* runtime-jsoo-src represent the copy of the original ocaml runtime
               to the jsoo directory. We could call it @runtime-ocaml-src but
               that would make a duplicate rule in ninja file if the OCaml
               backend is enabled along Jsoo *)
            "@runtime-jsoo-src";
            "@runtime-jsoo-interfaces-src";
            "@runtime-jsoo-only";
            "@original-runtime";
          ]
        ~outputs:["@runtime-jsoo"];
      Nj.build "phony"
        ~inputs:[runtime_jsoo_base -.- "cmo"; dates_jsoo_base -.- "cmo"]
        ~implicit_in:["@runtime-jsoo-cmi"] ~outputs:["@runtime-jsoo-only"];
      Nj.build "phony"
        ~inputs:[runtime_jsoo_base -.- "cmi"; dates_jsoo_base -.- "cmi"]
        ~outputs:["@runtime-jsoo-cmi"];
      Nj.build "phony"
        ~inputs:[runtime_ocaml_base -.- "cmo"; dates_ocaml_base -.- "cmo"]
        ~outputs:["@original-runtime"];
      Nj.comment "Jsoo interfaces copy";
      Nj.build "copy"
        ~inputs:[jsoo_src / "dates_calc_jsoo.cmi"]
        ~outputs:[dates_jsoo_base -.- "cmi"];
      Nj.build "copy"
        ~inputs:[jsoo_src / "catala_runtime_jsoo.cmi"]
        ~outputs:[runtime_jsoo_base -.- "cmi"];
      Nj.comment "Jsoo files compilation";
      Nj.build "jsoo-bytobject"
        ~inputs:[runtime_jsoo_base -.- "ml"]
        ~implicit_in:
          [
            runtime_jsoo_base -.- "cmi";
            runtime_ocaml_base -.- "cmi";
            dates_jsoo_base -.- "cmi";
          ]
        ~vars:
          [
            Var.includes, [Var.(!jsoo_include); "-I"; catala_runtime_jsoo];
            ppx, [Var.(!ppx_jsoo)];
          ]
        ~outputs:[runtime_jsoo_base -.- "cmo"];
      Nj.build "jsoo-bytobject"
        ~inputs:[dates_jsoo_base -.- "ml"]
        ~implicit_in:[dates_jsoo_base -.- "cmi"; dates_ocaml_base -.- "cmi"]
        ~vars:
          [
            Var.includes, [Var.(!jsoo_include); "-I"; catala_runtime_jsoo];
            ppx, [Var.(!ppx_jsoo)];
          ]
        ~outputs:[dates_jsoo_base -.- "cmo"];
    ]

  let runtime_jsoo ~stdbase ~dates_ocaml_base ~runtime_ocaml_base =
    let open File in
    let jsoo_src = Var.(!runtime_jsoo) / "jsoo" in
    let jsoo_base = stdbase / "jsoo" in
    let dates_jsoo_base = jsoo_base / "dates_calc_jsoo" in
    let runtime_jsoo_base = jsoo_base / "catala_runtime_jsoo" in
    let runtime =
      runtime_jsoo_statements ~catala_runtime_jsoo:jsoo_base ~jsoo_src
        ~dates_ocaml_base ~runtime_ocaml_base ~dates_jsoo_base
        ~runtime_jsoo_base
    in
    [
      Nj.build "phony"
        ~inputs:
          [
            dates_jsoo_base -.- "ml";
            dates_jsoo_base -.- "mli";
            runtime_jsoo_base -.- "ml";
            runtime_jsoo_base -.- "mli";
          ]
        ~outputs:["@runtime-jsoo-interfaces-src"];
      Nj.build "copy"
        ~inputs:[jsoo_src / "catala_runtime_jsoo.mli"]
        ~outputs:[runtime_jsoo_base -.- "mli"];
      Nj.build "copy"
        ~inputs:[jsoo_src / "catala_runtime_jsoo.ml"]
        ~outputs:[runtime_jsoo_base -.- "ml"];
      Nj.build "copy"
        ~inputs:[jsoo_src / "dates_calc_jsoo.mli"]
        ~outputs:[dates_jsoo_base -.- "mli"];
      Nj.build "copy"
        ~inputs:[jsoo_src / "dates_calc_jsoo.ml"]
        ~outputs:[dates_jsoo_base -.- "ml"];
    ]
    @ runtime

  let runtime_build_statements ~options:_ ~stdbase =
    (* If the Jsoo backend is activated, it needs cmo files of OCaml runtime as
       js_of_ocaml only works on bytecode *)
    let ocaml_src = Var.(!runtime) / "ocaml" in
    let dates_base = stdbase / "jsoo" / "dates_calc" in
    let ocaml_base = stdbase / "jsoo" / "catala_runtime" in
    Ocaml.Backend.runtime_ocaml "jsoo" ~ocaml_src ~dates_base ~ocaml_base
    @ [
        Nj.build "jsoo-bytobject"
          ~inputs:[dates_base -.- "ml"]
          ~implicit_in:[dates_base -.- "cmi"]
          ~outputs:[dates_base -.- "cmo"];
        Nj.build "jsoo-bytobject"
          ~inputs:[ocaml_base -.- "ml"]
          ~implicit_in:[dates_base -.- "cmi"; ocaml_base -.- "cmi"]
          ~outputs:[ocaml_base -.- "cmo"];
      ]
    @ runtime_jsoo ~stdbase ~dates_ocaml_base:dates_base
        ~runtime_ocaml_base:ocaml_base

  let catala ?vars ~is_stdlib:_ ~inputs ~implicit_in _has_scope_tests =
    List.to_seq
      [
        Nj.build "catala-ocaml" ?vars ~inputs ~implicit_in
          ~outputs:[Ninja.target ~backend:"jsoo" "ml"]
          ~implicit_out:[Ninja.target ~backend:"jsoo" "mli"];
        Nj.build "catala-jsoo" ?vars ~inputs ~implicit_in
          ~outputs:[Ninja.target ~suffix:"_jsoo" ~backend:"jsoo" "ml"]
          ~implicit_out:[Ninja.target ~suffix:"_jsoo" ~backend:"jsoo" "mli"];
      ]

  let build_object
      ~externls
      ~include_dirs
      ~same_dir_modules
      ~item
      _has_scope_tests =
    let catala_runtime_jsoo =
      File.(Var.(!builddir) / Scan.libcatala / "jsoo")
    in
    let modules = List.rev_map Mark.remove item.Scan.used_modules in
    (* If a catala file is marked as external, its interface is not exposed in
       JS. So we sould filter out those file. *)
    let modules_jsoo =
      List.filter_map
        (fun used ->
          if List.mem used externls then None
          else
            Some
              (Ninja.modfile ~suffix:"jsoo" ~backend:"jsoo" same_dir_modules
                 ".cmi" used))
        modules
    in
    let includes = Common.Flags.include_flags ~backend:"jsoo" include_dirs in
    let jsoo_obj =
      [
        Nj.build "jsoo-bytobject"
          ~inputs:
            [
              Ninja.target ~backend:"jsoo" "mli";
              Ninja.target ~backend:"jsoo" "ml";
            ]
          ~implicit_in:
            (Var.(!catala_exe)
            :: List.map
                 (Ninja.modfile ~backend:"jsoo" same_dir_modules ".cmo")
                 modules)
          ~outputs:(List.map (Ninja.target ~backend:"jsoo") ["cmi"; "cmo"])
          ~vars:
            ([
               ( Var.includes,
                 includes
                 @ (if item.extrnal then [Var.(!jsoo_include)] else [])
                 @ ["-I"; catala_runtime_jsoo] );
             ]
            @ if item.extrnal then [ppx, [Var.(!ppx_jsoo)]] else []);
      ]
      @
      if item.extrnal then []
      else
        [
          Nj.build "jsoo-bytobject"
            ~inputs:
              [
                Ninja.target ~suffix:"jsoo" ~backend:"jsoo" "mli";
                Ninja.target ~suffix:"jsoo" ~backend:"jsoo" "ml";
              ]
            ~implicit_in:
              ([
                 Ninja.target ~backend:"jsoo" ".cmo";
                 "@runtime-jsoo-cmi";
                 Var.(!catala_exe);
               ]
              @ modules_jsoo)
            ~outputs:
              (List.map
                 (Ninja.target ~suffix:"jsoo" ~backend:"jsoo")
                 ["cmi"; "cmo"])
            ~vars:
              [
                ( Var.includes,
                  includes @ [Var.(!jsoo_include); "-I"; catala_runtime_jsoo] );
                ppx, [Var.(!ppx_jsoo)];
              ];
        ]
    in
    List.to_seq jsoo_obj

  let expose_module ~same_dir_modules ~used_modules =
    [
      Nj.build "phony"
        ~inputs:[Ninja.target ~backend:"jsoo" ~suffix:"jsoo" "mli"]
        ~implicit_in:
          (List.map
             (Clerk_utils.Ninja.modfile ~suffix:"jsoo" ~backend:"jsoo"
                same_dir_modules ".cmi")
             used_modules)
        ~outputs:[Ninja.target ~backend:"jsoo" "@jsoo-module"];
    ]

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / "jsoo")

  let extra_default = ["@javascript-file"]
end
