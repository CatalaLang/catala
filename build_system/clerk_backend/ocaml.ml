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

let name = "ocaml"
let catala_flags_ocaml = Var.make "CATALA_FLAGS_OCAML"
let ocamlc_exe = Var.make "OCAMLC_EXE"
let ocamlopt_exe = Var.make "OCAMLOPT_EXE"
let ocaml_flags = Var.make "OCAML_FLAGS"
let ocaml_include = Var.make "OCAML_INCLUDE"

module OCaml_Flags = struct
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

  let ocaml_include_value : string list Lazy.t =
    lazy (fst (Lazy.force ocaml_include_and_lib))

  let default ~variables ~autotest ~use_default_flags ~test_flags ~include_dirs
      =
    let open Flags in
    let catala_flags =
      catala_backend_flags ~autotest ~use_default_flags ~test_flags
        ~accepts_closure_conversion:true
    in
    let def = def ~variables in
    [
      def catala_flags_ocaml (lazy catala_flags);
      def ocamlc_exe (lazy ["ocamlc"]);
      def ocamlopt_exe (lazy ["ocamlopt"]);
      def ocaml_flags (lazy []);
      def ocaml_include
        (lazy
          (Lazy.force ocaml_include_value @ Flags.includes ~name include_dirs));
    ]
end

let linking_command ~build_dir ~var_bindings link_deps item target =
  let open File in
  let target_objs =
    let base = Filename.chop_extension target in
    let suffix = "+main" in
    if String.ends_with ~suffix base then
      [
        String.sub base 0 (String.length base - String.length suffix) -.- "cmx";
        target -.- "cmx";
      ]
    else [target -.- "cmx"]
  in
  Var.get var_bindings ocamlopt_exe
  @ List.map (Var.expand var_bindings) (Lazy.force OCaml_Flags.ocaml_link)
  @ [build_dir / Scan.libcatala / name / "dates_calc.cmx"]
  @ [build_dir / Scan.libcatala / name / "catala_runtime.cmx"]
  @ Var.get var_bindings ocaml_flags
  @ Var.get var_bindings ocaml_include
  @ List.map
      (fun it ->
        let f = Scan.target_file_name it in
        (build_dir / dirname f / name / basename f) ^ ".cmx")
      (link_deps item)
  @ target_objs
  @ ["-o"; target -.- "exe"]

let run_artifact
    ~test
    ~(trace : [ `FileName of Global.raw_file | `Stdout ] option)
    ?scope
    ?quiet
    src =
  let open File in
  let cmd =
    ((src -.- "exe") :: Option.to_list scope)
    @ (if test && not Global.options.debug then ["--test"] else [])
    @ (match trace with
      | None -> []
      | Some `Stdout -> ["--trace"]
      | Some (`FileName f) -> ["--trace=" ^ (f :> string)])
    @ if Global.options.output_format = JSON then ["--json"] else []
  in
  Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
  Clerk_cli.run_command_line ?quiet cmd

module Spec : Sig.Spec = struct
  open Var.Op
  module Nj = Ninja_utils

  let name = name
  let src_extensions = ["ml"; "mli"]
  let module_extensions = ["cmi"]
  let obj_extension = "cmx"
  let all_obj_extensions = ["cmi"; "cmo"; "cmx"; "o"; "cmxs"]
  let var_defs = OCaml_Flags.default
  let stdlib_subdir = "."

  let[@ocamlformat "disable"] rules =
    let runtime_include = File.(Var.(!builddir) / Scan.libcatala / name) in
    let description = ["<" ^ name ^ ">"; "⇒"; !Var.output] in
    [
      Nj.rule "catala-ocaml" ~description:["<catala>"; name; "⇒"; !Var.output]
        ~command:[!Var.catala_exe; name; !Var.catala_flags; !catala_flags_ocaml;
                  "-o"; !Var.output; "--"; !Var.input];

      Nj.rule "ocaml-bytobject" ~description
        ~command:[
          !ocamlc_exe; "-c"; !ocaml_flags; !ocaml_include;
          "-I"; runtime_include;
          !Var.includes;
          !Var.input
        ];

      Nj.rule "ocaml-natobject" ~description
        ~command:[
          !ocamlopt_exe; "-c"; !ocaml_flags; !ocaml_include;
          "-I"; runtime_include;
          !Var.includes;
          !Var.input
        ];

      Nj.rule "ocaml-module" ~description
        ~command:
          [!ocamlopt_exe; "-shared"; !ocaml_flags; !ocaml_include;
           "-I"; runtime_include;
           !Var.input;
           "-o"; !Var.output];
    ]

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / name)

  let build_runtime ~options:_ ~stdbase =
    let open File in
    let ocaml_src = Var.(!runtime) / name in
    let dates_base = stdbase / name / "dates_calc" in
    let ocaml_base = stdbase / name / "catala_runtime" in
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
            Var.(!catala_exe);
          ]
        ~outputs:["@ocaml/runtime.cmi"];
      Nj.build "phony"
        ~inputs:
          [
            dates_base -.- "ml";
            dates_base -.- "mli";
            ocaml_base -.- "ml";
            ocaml_base -.- "mli";
          ]
        ~outputs:["@ocaml/runtime/src"];
      Nj.build "phony"
        ~inputs:[ocaml_base -.- "cmx"]
        ~implicit_in:[dates_base -.- "cmi"]
        ~outputs:["@ocaml/runtime/obj"];
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

  let catala ?vars ~is_stdlib:_ ~inputs ~implicit_in ~has_scope_tests =
    let implicit_out =
      if has_scope_tests then [Common.target ~name "+main.ml"] else []
    in
    Seq.return
      (Nj.build "catala-ocaml" ?vars ~inputs ~implicit_in
         ~outputs:[Common.target ~name "ml"]
         ~implicit_out:(Common.target ~name "mli" :: implicit_out))

  let build_object ~include_dirs ~same_dir_modules:_ item =
    let open Scan in
    let modules = List.rev_map Mark.remove item.used_modules in
    let implicit_modules = List.map (Common.interface_dep ~name) modules in
    let obj =
      [
        Nj.build "ocaml-bytobject"
          ~inputs:[Common.target ~name "mli"; Common.target ~name "ml"]
          ~implicit_in:(implicit_modules @ ["@ocaml/runtime.cmi"])
          ~outputs:(List.map (Common.target ~name) ["cmi"; "cmo"])
          ~vars:
            [
              Var.includes, Flags.include_flags ~name include_dirs;
              ocaml_flags, [Var.(!ocaml_flags); "-opaque"; "-no-alias-deps"];
            ];
        Nj.build "ocaml-natobject"
          ~inputs:[Common.target ~name "ml"]
          ~implicit_in:
            ((Common.target ~name "cmi" :: implicit_modules)
            @ ["@ocaml/runtime.cmi"])
          ~outputs:(List.map (Common.target ~name) ["cmx"; "o"])
          ~vars:[Var.includes, Flags.include_flags ~name include_dirs];
      ]
    in
    let obj =
      (match item.module_def with
        | Some _ ->
          obj
          @ [
              Nj.build "ocaml-module"
                ~inputs:[Common.target ~name "cmx"]
                ~outputs:[Common.target ~name "cmxs"];
            ]
          @
          (* if item.is_stdlib || List.mem (File.dirname item.file_name) include_dirs then *)
          let ext =
            match Sys.backend_type with Native -> "cmxs" | _ -> "cmo"
          in
          [
            Nj.build "phony"
              ~inputs:[Common.target ~name ext]
              ~implicit_in:(List.map Common.catala_obj_target modules)
              ~outputs:["@catala/obj/" ^ !Var.dst];
          ]
          (* else [] *)
        | None -> obj)
      @
      if Lazy.force item.has_scope_tests > 0 then
        [
          Nj.build "ocaml-natobject"
            ~inputs:[Common.target ~name "+main.ml"]
            ~implicit_in:[Common.target ~name "cmi"; Common.target ~name "cmx"]
            ~outputs:
              (List.map
                 (fun ext -> Common.target ~name ("+main." ^ ext))
                 ["cmx"; "o"])
            ~vars:
              [
                ( Var.includes,
                  Flags.include_flags ~name include_dirs @ ["-w"; "-24"] );
              ];
        ]
      else []
    in
    List.to_seq obj

  let write_target_def_file ~options ~dir target =
    let open File in
    with_out_channel (dir / "dune")
    @@ fun oc ->
    if target.Clerk_config.tname = Scan.libcatala then
      Printf.fprintf oc
        "(library\n\
        \ (name libcatala)%s\n\
        \ (wrapped false)\n\
        \ (libraries zarith catala.dates_calc))\n"
        (match options.Clerk_config.global.project_name with
        | None -> ""
        | Some n -> Printf.sprintf "\n (public_name %s.%s)" n target.tname)
    else
      Printf.fprintf oc
        "(library\n (name %s)%s\n (wrapped false)\n (libraries %s))\n"
        (String.to_id target.tname)
        (match options.Clerk_config.global.project_name with
        | None -> ""
        | Some n -> Printf.sprintf "\n (public_name %s.%s)" n target.tname)
        (String.concat " "
           (List.map String.to_id ("libcatala" :: target.dependencies)))

  let install_runtime ~options =
    let open File in
    let extensions =
      src_extensions
      @
      if options.Clerk_config.global.include_objects then ["cmi"; "cmx"] else []
    in
    let dir = options.global.target_dir / name / Scan.libcatala in
    remove dir;
    ensure_dir dir;
    List.iter
      (fun ext ->
        let src_libcatala =
          (options.global.build_dir / Scan.libcatala / name / "catala_runtime")
          -.- ext
        in
        let src =
          Lazy.force Poll.stdlib_dir / name / ("catala_runtime" -.- ext)
        in
        if File.exists src_libcatala then copy_in ~dir ~src:src_libcatala
        else if File.exists src then copy_in ~dir ~src)
      extensions;
    File.with_out_channel (options.global.target_dir / name / "dune-project")
    @@ fun oc ->
    Printf.fprintf oc "(lang dune 3.13)\n";
    match options.global.project_name with
    | None -> ()
    | Some p -> Printf.fprintf oc "(name %s)\n(package (name %s))\n" p p
end

include Common.Make_backend (Spec)
