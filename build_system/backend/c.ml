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

module Flags = struct
  let default ~variables ~autotest ~use_default_flags ~test_flags ~include_dirs
      =
    let open Common.Flags in
    let catala_flags_c =
      (if autotest then ["--autotest"] else [])
      @
      if use_default_flags then ["-O"]
      else
        List.filter
          (function "-O" | "--optimize" -> true | _ -> false)
          test_flags
    in
    let def = def ~variables in
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
          @ Common.Flags.includes ~backend:"c" include_dirs));
    ]
end

module Backend = struct
  open Var
  open File
  module Nj = Ninja_utils

  let[@ocamlformat "disable"] static_base_rules =
  [
    Nj.rule "catala-c"
      ~command:[!catala_exe; "c"; !catala_flags; !catala_flags_c;
                "-o"; !output; "--"; !input]
      ~description:["<catala>"; "c"; "⇒"; !output];
    Nj.rule "c-object"
      ~command:
        [!cc_exe; !input; !c_flags; !c_include; !includes; "-c"; "-o"; !output]
      ~description:["<cc>"; "⇒"; !output];
  ]

  let external_copy item =
    let catala_src = !Var.tdir / !Var.src in
    let c, missing =
      Ninja.extern_src ~backend:"c" ~ext:"c" ~missing:[]
        ~filename:item.Scan.file_name
    in
    let h, missing =
      Ninja.extern_src ~backend:"c" ~ext:"h" ~missing
        ~filename:item.Scan.file_name
    in
    Ninja.check_missing ~backend:"c" ~module_def:item.Scan.module_def ~missing
      ~filename:item.Scan.file_name;
    List.to_seq
      [
        Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[c]
          ~outputs:[Ninja.target ~backend:"c" "c"];
        Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[h]
          ~outputs:[Ninja.target ~backend:"c" "h"];
      ]

  let runtime_build_statements ~options:_ ~stdbase =
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

  let catala ?vars ~inputs ~implicit_in has_scope_tests =
    let implicit_out =
      if has_scope_tests then [Ninja.target ~backend:"c" "+main.c"] else []
    in
    Seq.return
      (Nj.build "catala-c" ?vars ~inputs ~implicit_in
         ~outputs:[Ninja.target ~backend:"c" "c"]
         ~implicit_out:(Ninja.target ~backend:"c" "h" :: implicit_out))

  let module_target same_dir_modules =
    Ninja.modfile ~backend:"c" same_dir_modules "@c-module"

  let includes = Common.Flags.include_flags ~backend:"c"

  let build_object ~include_dirs ~same_dir_modules ~item has_scope_tests =
    let open Scan in
    let modules = List.rev_map Mark.remove item.used_modules in
    let implicit_modules = List.map (module_target same_dir_modules) modules in
    Nj.build "c-object"
      ~inputs:[Ninja.target ~backend:"c" "c"]
      ~implicit_in:
        (Ninja.target ~backend:"c" "h" :: "@runtime-c" :: implicit_modules)
      ~outputs:[Ninja.target ~backend:"c" "o"]
      ~vars:[Var.includes, includes include_dirs]
    ::
    (if has_scope_tests then
       [
         Nj.build "c-object"
           ~inputs:[Ninja.target ~backend:"c" "+main.c"]
           ~implicit_in:
             (Ninja.target ~backend:"c" "h" :: "@runtime-c" :: implicit_modules)
           ~outputs:[Ninja.target ~backend:"c" "+main.o"]
           ~vars:[Var.includes, includes include_dirs];
       ]
     else [])

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / "c")
end
