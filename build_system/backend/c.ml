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
open Clerk_lib

let catala_flags_c = Var.make_vector "CATALA_FLAGS_C"
let cc_exe = Var.make_vector "CC"
let c_flags = Var.make_vector "CFLAGS"
let c_include = Var.make_vector "C_INCLUDE_FLAGS"

let linking_command ~build_dir ~var_bindings link_deps item target =
  let open File in
  Var.get_var var_bindings (Var.name cc_exe)
  @ [build_dir / Scan.libcatala / "c" / "dates_calc.o"]
  @ [build_dir / Scan.libcatala / "c" / "catala_runtime.o"]
  @ List.map
      (fun it ->
        let f = Scan.target_file_name it in
        (build_dir / dirname f / "c" / basename f) ^ ".o")
      (link_deps item)
  @ ["-lgmp"]
  @ [target -.- "o"; File.remove_extension target ^ "+main.o"]
  @ Var.get_var var_bindings (Var.name c_flags)
  @ Var.get_var var_bindings (Var.name c_include)
  @ ["-o"; target -.- "exe"]

let run_artifact ~test ?scope src =
  let open File in
  let cmd =
    ((src -.- "exe") :: Option.to_list scope)
    @ (if test && not Global.options.debug then ["--test"] else [])
    @ if Global.options.output_format = JSON then ["--json"] else []
  in
  Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
  Clerk_cli.run_command_line cmd

module Backend = struct
  open Var
  open File
  module Nj = Var.Nj

  let name = "c"
  let module_ext = "@" ^ name ^ "-module"
  let src_extensions = ["c"; "h"]
  let obj_extensions = ["o"]

  let runtime_targets ~only_source =
    [(if only_source then "@runtime-" ^ name ^ "-src" else "@runtime-" ^ name)]

  module Flags = struct
    let default
        ~variables
        ~autotest
        ~use_default_flags
        ~test_flags
        ~include_dirs =
      let open Common.Flags in
      let catala_flags =
        Common.Flags.catala_backend_flags ~autotest ~use_default_flags
          ~test_flags ~accepts_closure_conversion:false
      in
      let def v x = def ~variables v x in
      [
        def catala_flags_c (lazy catala_flags);
        def cc_exe (lazy ["cc"]);
        def c_flags
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
        def c_include
          (lazy
            (["-I"; File.(Var.(!builddir) / Scan.libcatala / name)]
            @ Common.Flags.includes ~backend:name include_dirs));
      ]
  end

  let[@ocamlformat "disable"] static_base_rules =
  [
    Nj.rule "catala-c"
      ~command:[Word !catala_exe; Word name; Splice catala_flags; Splice catala_flags_c;
                Word "-o"; Raw !output; Word "--"; Raw !input]
      ~description:[Word "<catala>"; Word name; Word "⇒"; Raw !output];
    Nj.rule "c-object"
      ~command:
        [Splice cc_exe; Raw !input; Splice c_flags; Splice c_include; Splice includes;
         Word "-c"; Word "-o"; Raw !output]
      ~description:[Word "<cc>"; Word "⇒"; Raw !output];
  ]

  let external_copy item =
    let catala_src = !Var.tdir / !Var.src in
    let c, missing =
      Ninja.extern_src ~backend:name ~ext:"c" ~missing:[]
        ~filename:item.Scan.file_name
    in
    let h, missing =
      Ninja.extern_src ~backend:name ~ext:"h" ~missing
        ~filename:item.Scan.file_name
    in
    Ninja.check_missing ~backend:name ~module_def:item.Scan.module_def ~missing
      ~filename:item.Scan.file_name;
    List.to_seq
      [
        Nj.build "copy" ~implicit_in:[Word catala_src] ~inputs:[Word c]
          ~outputs:[Word (Ninja.target ~backend:name "c")];
        Nj.build "copy" ~implicit_in:[Word catala_src] ~inputs:[Word h]
          ~outputs:[Word (Ninja.target ~backend:name "h")];
      ]

  let runtime_build_statements ~options:_ ~stdbase =
    let c_base = stdbase / name / "catala_runtime" in
    let c_src = Var.(!runtime) / name in
    [
      Nj.build "phony"
        ~inputs:
          [
            Word (c_base -.- "c");
            Word (c_base -.- "h");
            Word ((c_base /../ "dates_calc") -.- "c");
            Word ((c_base /../ "dates_calc") -.- "h");
          ]
        ~outputs:[Word ("@runtime-" ^ name ^ "-src")];
      Nj.build "phony"
        ~inputs:
          [
            Word (c_base -.- "o");
            Word (c_base -.- "h");
            Word ((c_base /../ "dates_calc") -.- "o");
            Word ((c_base /../ "dates_calc") -.- "h");
            Word Var.(!catala_exe);
          ]
        ~outputs:[Word ("@runtime-" ^ name)];
      Nj.build "copy"
        ~inputs:[Word (c_src / "catala_runtime.h")]
        ~outputs:[Word (c_base -.- "h")];
      Nj.build "copy"
        ~inputs:[Word (c_src / "catala_runtime.c")]
        ~outputs:[Word (c_base -.- "c")];
      Nj.build "copy"
        ~inputs:[Word (c_src / "dates_calc.h")]
        ~outputs:[Word ((c_base /../ "dates_calc") -.- "h")];
      Nj.build "copy"
        ~inputs:[Word (c_src / "dates_calc.c")]
        ~outputs:[Word ((c_base /../ "dates_calc") -.- "c")];
      Nj.build "c-object"
        ~inputs:[Word (c_base -.- "c")]
        ~implicit_in:[Word (c_base -.- "h")]
        ~outputs:[Word (c_base -.- "o")];
      Nj.build "c-object"
        ~inputs:[Word ((c_base /../ "dates_calc") -.- "c")]
        ~implicit_in:[Word ((c_base /../ "dates_calc") -.- "h")]
        ~outputs:[Word ((c_base /../ "dates_calc") -.- "o")];
    ]

  let catala ?vars ~is_stdlib:_ ~inputs ~implicit_in has_scope_tests =
    let implicit_out =
      if has_scope_tests then
        [Nj.Expr.Word (Ninja.target ~backend:name "+main.c")]
      else []
    in
    Seq.return
      (Nj.build "catala-c" ?vars ~inputs ~implicit_in
         ~outputs:[Word (Ninja.target ~backend:name "c")]
         ~implicit_out:
           (Nj.Expr.Word (Ninja.target ~backend:name "h") :: implicit_out))

  let modfile ~is_stdlib:_ = Ninja.modfile ~backend:name

  let module_target same_dir_modules =
    Ninja.modfile ~backend:name same_dir_modules module_ext

  let includes = Common.Flags.include_flags ~backend:name

  let build_object ~include_dirs ~same_dir_modules ~item has_scope_tests =
    let open Scan in
    let modules = List.rev_map Mark.remove item.used_modules in
    let implicit_modules =
      List.map
        (fun m -> Nj.Expr.Word (module_target same_dir_modules m))
        modules
    in
    let obj =
      Nj.build "c-object"
        ~inputs:[Word (Ninja.target ~backend:name "c")]
        ~implicit_in:
          (Nj.Expr.Word (Ninja.target ~backend:name "h")
          :: Word ("@runtime-" ^ name)
          :: implicit_modules)
        ~outputs:[Word (Ninja.target ~backend:name "o")]
        ~vars:[Nj.Binding.make_any Var.includes (includes include_dirs)]
      ::
      (if has_scope_tests then
         [
           Nj.build "c-object"
             ~inputs:[Word (Ninja.target ~backend:name "+main.c")]
             ~implicit_in:
               (Nj.Expr.Word (Ninja.target ~backend:name "h")
               :: Word ("@runtime-" ^ name)
               :: implicit_modules)
             ~outputs:[Word (Ninja.target ~backend:name "+main.o")]
             ~vars:[Nj.Binding.make_any Var.includes (includes include_dirs)];
         ]
       else [])
    in
    List.to_seq obj

  let expose_module ~same_dir_modules ~used_modules =
    [
      Nj.build "phony"
        ~inputs:[Word (Ninja.target ~backend:name "h")]
        ~implicit_in:
          (List.map
             (fun m -> Nj.Expr.Word (module_target same_dir_modules m))
             used_modules)
        ~outputs:[Word (Ninja.target ~backend:name module_ext)];
    ]

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / name)
end
