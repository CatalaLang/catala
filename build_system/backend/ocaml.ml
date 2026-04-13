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

open Clerk_utils
open Catala_utils

module Backend = struct
  open Var
  module Nj = Ninja_utils

  let name = "ocaml"
  let module_ext = "@" ^ name ^ "-module"
  let subdir = name
  let src_extensions = ["ml"; "mli"]
  let obj_extensions = ["cmi"; "cmo"; "cmx"; "o"; "cmxs"]

  let runtime_targets ~only_source =
    [(if only_source then "@runtime-" ^ name ^ "-src" else "@runtime-" ^ name)]

  module Flags = struct
    let ocaml_include_and_lib : (string list * string list) Lazy.t =
      lazy
        (let link_libs = ["zarith"] in
         let includes_libs =
           List.map
             (fun lib ->
               match
                 File.(check_directory (Lazy.force Poll.ocaml_libdir / lib))
               with
               | None ->
                 Message.error
                   "Required OCaml library not found at %a.@ Try `opam install \
                    %s'"
                   File.format
                   File.(Lazy.force Poll.ocaml_libdir / lib)
                   lib
               | Some d ->
                 ( ["-I"; d],
                   String.map (function '-' -> '_' | c -> c) lib ^ ".cmxa" ))
             link_libs
         in
         let includes, libs = List.split includes_libs in
         List.concat includes, libs)

    let ocaml_link : string list Lazy.t =
      lazy (snd (Lazy.force ocaml_include_and_lib))

    let ocaml_include : string list Lazy.t =
      lazy (fst (Lazy.force ocaml_include_and_lib))

    let default
        ~variables
        ~autotest
        ~use_default_flags
        ~test_flags
        ~include_dirs =
      let open Common.Flags in
      let catala_flags_ocaml =
        Common.Flags.catala_backend_flags ~autotest ~use_default_flags
          ~test_flags ~accepts_closure_conversion:true
      in
      let def = def ~variables in
      [
        def Var.catala_flags_ocaml (lazy catala_flags_ocaml);
        def Var.ocamlc_exe (lazy ["ocamlc"]);
        def Var.ocamlopt_exe (lazy ["ocamlopt"]);
        def Var.ocaml_flags (lazy []);
        def Var.ocaml_include
          (lazy
            (Lazy.force ocaml_include
            @ Common.Flags.includes ~backend:name include_dirs));
      ]
  end

  let[@ocamlformat "disable"] static_base_rules =
    let runtime_include = File.(Var.(!builddir) / Scan.libcatala / name) in
         [
      Nj.rule "catala-ocaml"
        ~command:[!catala_exe; name; !catala_flags; !catala_flags_ocaml;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; name; "⇒"; !output];
      Nj.rule "ocaml-bytobject"
        ~command:[
          !ocamlc_exe; "-c"; !ocaml_flags; !ocaml_include; "-I"; runtime_include; !includes; !input
        ]
        ~description:["<" ^ name ^ ">"; "⇒"; !output];

      Nj.rule "ocaml-natobject"
        ~command:[
          !ocamlopt_exe; "-c"; !ocaml_flags; !ocaml_include; "-I"; runtime_include; !includes; !input
        ]
        ~description:["<" ^ name ^ ">"; "⇒"; !output];

      Nj.rule "ocaml-module"
        ~command:
          [!ocamlopt_exe; "-shared"; !ocaml_flags; !ocaml_include; "-I"; runtime_include; !input;
           "-o"; !output]
        ~description:["<" ^ name ^ ">"; "⇒"; !output];
    ]

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / name)

  let modfile ~is_stdlib:_ = Ninja.modfile ~backend:name

  let external_copy item =
    let open File in
    let catala_src = !Var.tdir / !Var.src in
    let ml, missing =
      Ninja.extern_src ~filename:item.Scan.file_name ~backend:name ~ext:"ml"
        ~missing:[]
    in
    let mli, missing =
      Ninja.extern_src ~filename:item.Scan.file_name ~backend:name ~ext:"mli"
        ~missing
    in
    Ninja.check_missing ~backend:name ~module_def:item.Scan.module_def ~missing
      ~filename:item.file_name;
    List.to_seq
      [
        Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[ml]
          ~outputs:[Ninja.target ~backend:name "ml"];
        Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[mli]
          ~outputs:[Ninja.target ~backend:name "mli"];
      ]

  let runtime_ocaml backend ~ocaml_src ~dates_base ~ocaml_base =
    let open File in
    let runtime_cmi, dates_cmi =
      (* This one is tricky: in order for the catala interpreter to be able to
         dynlink compiled Catala modules, we need to be sure that they have been
         linked with a runtime abiding by the exact same cmi. Hence we need to
         distribute the cmi with the runtime library, and to fetch it from
         dune's _build when in the catala tree *)
      if Lazy.force Poll.catala_source_tree_root = None then
        ocaml_src / "catala_runtime.cmi", ocaml_src / "dates_calc.cmi"
      else
        ( Lazy.force Poll.runtime_dir
          /../ "_build"
          / "default"
          / "runtimes"
          / name
          / "catala_runtime.cmi",
          Lazy.force Poll.runtime_dir
          /../ "_build"
          / "default"
          / "runtimes"
          / name
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
          ]
        ~outputs:["@runtime-cmi-" ^ backend];
      Nj.build "phony"
        ~inputs:
          [
            dates_base -.- "ml";
            dates_base -.- "mli";
            ocaml_base -.- "ml";
            ocaml_base -.- "mli";
          ]
        ~outputs:["@runtime-" ^ backend ^ "-src"];
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
    ]

  let runtime_build_statements ~options:_ ~stdbase =
    let open File in
    let ocaml_src = Var.(!runtime) / name in
    let dates_base = stdbase / name / "dates_calc" in
    let ocaml_base = stdbase / name / "catala_runtime" in
    runtime_ocaml name ~ocaml_src ~dates_base ~ocaml_base
    @ [
        Nj.build "phony"
          ~inputs:[ocaml_base -.- "cmx"]
          ~implicit_in:[dates_base -.- "cmi"]
          ~outputs:["@runtime-" ^ name];
        Nj.build "ocaml-natobject"
          ~inputs:[dates_base -.- "ml"; ocaml_base -.- "ml"]
          ~implicit_in:[dates_base -.- "cmi"; ocaml_base -.- "cmi"]
          ~outputs:[ocaml_base -.- "cmx"; ocaml_base -.- "o"];
      ]

  let catala ?vars ~is_stdlib:_ ~inputs ~implicit_in has_scope_tests =
    let implicit_out =
      if has_scope_tests then [Ninja.target ~backend:name "+main.ml"] else []
    in
    Seq.return
      (Nj.build "catala-ocaml" ?vars ~inputs ~implicit_in
         ~outputs:[Ninja.target ~backend:name "ml"]
         ~implicit_out:(Ninja.target ~backend:name "mli" :: implicit_out))

  let module_target same_dir_modules =
    Ninja.modfile ~backend:name same_dir_modules module_ext

  let includes = Common.Flags.include_flags ~backend:name

  let build_object
      ~externls:_
      ~include_dirs
      ~same_dir_modules
      ~item
      has_scope_tests =
    let open Ninja in
    let open Scan in
    let modules = List.rev_map Mark.remove item.used_modules in
    let implicit_modules = List.map (module_target same_dir_modules) modules in
    let obj =
      [
        Nj.build "ocaml-bytobject"
          ~inputs:[target ~backend:name "mli"; target ~backend:name "ml"]
          ~implicit_in:(implicit_modules @ ["@runtime-cmi-" ^ name])
          ~outputs:(List.map (target ~backend:name) ["cmi"; "cmo"])
          ~vars:
            [
              Var.includes, includes include_dirs;
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
          ~inputs:[target ~backend:name "ml"]
          ~implicit_in:
            ((target ~backend:name "cmi" :: implicit_modules)
            @ ["@runtime-cmi-" ^ name])
          ~outputs:(List.map (target ~backend:name) ["cmx"; "o"])
          ~vars:[Var.includes, includes include_dirs];
      ]
    in
    let obj =
      (match item.module_def with
        | Some _ ->
          obj
          @ [
              Nj.build "ocaml-module"
                ~inputs:[target ~backend:name "cmx"]
                ~outputs:[target ~backend:name "cmxs"];
            ]
        | None -> obj)
      @
      if has_scope_tests then
        [
          Nj.build "ocaml-natobject"
            ~inputs:[target ~backend:name "+main.ml"]
            ~implicit_in:
              [target ~backend:name "cmi"; target ~backend:name "cmx"]
            ~outputs:
              (List.map
                 (fun ext -> target ~backend:name ("+main." ^ ext))
                 ["cmx"; "o"])
            ~vars:[Var.includes, includes include_dirs @ ["-w"; "-24"]];
        ]
      else []
    in
    List.to_seq obj

  let expose_module ~same_dir_modules ~used_modules =
    [
      Nj.build "phony"
        ~inputs:
          [Ninja.target ~backend:name "cmi"; Ninja.target ~backend:name "cmxs"]
        ~implicit_in:(List.map (module_target same_dir_modules) used_modules)
        ~outputs:[Ninja.target ~backend:name module_ext];
    ]

  let extra_rules ~externls:_ ~stdlib_tree:_ ~project_tree:_ _module_targets =
    []

  let extra_default = []
end
