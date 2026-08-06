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

let catala_flags_c = Var.make_vector "CATALA_FLAGS_C"
let cc_exe = Var.make_vector "CC"
let c_flags = Var.make_vector "CFLAGS"
let c_include = Var.make_vector "C_INCLUDE_FLAGS"

let linking_command ~build_dir ~var_bindings link_deps item target =
  let open File in
  let target_objs =
    let base = Filename.chop_extension target in
    let suffix = "+main" in
    if String.ends_with ~suffix base then
      [
        String.sub base 0 (String.length base - String.length suffix) -.- "o";
        target -.- "o";
      ]
    else [target -.- "o"]
  in
  Var.get var_bindings cc_exe
  @ [build_dir / Scan.libcatala / "c" / "dates_calc.o"]
  @ [build_dir / Scan.libcatala / "c" / "catala_runtime.o"]
  @ List.map
      (fun it ->
        let f = Scan.target_file_name it in
        (build_dir / dirname f / "c" / basename f) ^ ".o")
      (link_deps item)
  @ ["-lgmp"]
  @ target_objs
  @ Var.get var_bindings c_flags
  @ Var.get var_bindings c_include
  @ ["-o"; target -.- "exe"]

let run_artifact ~test ?scope ?quiet src =
  let open File in
  let cmd =
    ((src -.- "exe") :: Option.to_list scope)
    @ (if test && not Global.options.debug then ["--test"] else [])
    @ if Global.options.output_format = JSON then ["--json"] else []
  in
  Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
  Clerk_cli.run_command_line ?quiet cmd

module Spec : Sig.Spec = struct
  open Var
  open File
  module Nj = Ninja_utils

  let name = "c"
  let src_extensions = ["c"; "h"]
  let module_extensions = ["h"]
  let obj_extension = "o"
  let all_obj_extensions = ["o"]
  let stdlib_subdir = "."

  let var_defs ~variables ~autotest ~use_default_flags ~test_flags ~include_dirs
      =
    let open Flags in
    let catala_flags =
      catala_backend_flags ~autotest ~use_default_flags ~test_flags
        ~accepts_closure_conversion:false
    in
    let def = def ~variables in
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
          @ Flags.includes ~name include_dirs));
    ]

  let[@ocamlformat "disable"] rules =
  [
    Nj.rule "catala-c"
      ~command:[!!catala_exe; Word name; !!catala_flags; !!catala_flags_c;
                Word "-o"; !!output; Word "--"; !!input]
      ~description:[Word "<catala>"; Word name; Word "⇒"; !!output];
    Nj.rule "c-object"
      ~command:
        [!!cc_exe; !!input; !!c_flags; !!c_include; !!includes;
         Word "-c"; Word "-o"; !!output]
      ~description:[Word "<cc>"; Word "⇒"; !!output];
  ]

  let build_runtime ~options:_ ~stdbase =
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
        ~outputs:[Word "@c/runtime/src"];
      Nj.build "phony"
        ~inputs:
          [
            Word (c_base -.- "o");
            Word (c_base -.- "h");
            Word ((c_base /../ "dates_calc") -.- "o");
            Word ((c_base /../ "dates_calc") -.- "h");
            !!catala_exe;
          ]
        ~outputs:[Word "@c/runtime/obj"];
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

  let catala ?vars ~is_stdlib:_ ~inputs ~implicit_in ~has_scope_tests =
    let implicit_out =
      if has_scope_tests then [Common.target ~name "+main.c"] else []
    in
    Seq.return
      (Nj.build "catala-c" ?vars ~inputs ~implicit_in
         ~outputs:[Common.target ~name "c"]
         ~implicit_out:(Common.target ~name "h" :: implicit_out))

  let build_object ~include_dirs ~same_dir_modules:_ item =
    let open Scan in
    let modules = List.rev_map Mark.remove item.used_modules in
    let implicit_modules = List.map (Common.interface_dep ~name) modules in
    let obj =
      Nj.build "c-object"
        ~inputs:[Common.target ~name "c"]
        ~implicit_in:
          (Common.target ~name "h" :: Word "@c/runtime/src" :: implicit_modules)
        ~outputs:[Common.target ~name "o"]
        ~vars:
          [
            Nj.Binding.make Var.includes (Flags.include_flags ~name include_dirs);
          ]
      ::
      (if Lazy.force item.has_scope_tests > 0 then
         [
           Nj.build "c-object"
             ~inputs:[Common.target ~name "+main.c"]
             ~implicit_in:
               (Common.target ~name "h"
               :: Word "@c/runtime/src"
               :: implicit_modules)
             ~outputs:[Common.target ~name "+main.o"]
             ~vars:
               [
                 Nj.Binding.make Var.includes
                   (Flags.include_flags ~name include_dirs);
               ];
         ]
       else [])
    in
    List.to_seq obj

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / name)

  let write_target_def_file ~options:_ ~dir:_ _target = ()
  (* TODO: generate a Makefile ? Or at least a depfile ? *)

  let install_runtime ~options =
    let open File in
    let extensions =
      src_extensions
      @ if options.Clerk_config.global.include_objects then ["o"] else []
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
      extensions
end

include Common.Make_backend (Spec)
