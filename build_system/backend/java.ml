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
  let default
      ~variables
      ~autotest
      ~use_default_flags
      ~test_flags
      ~include_dirs:_ =
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
    let def = Common.Flags.def ~variables in
    [
      def Var.catala_flags_java (lazy catala_flags_java);
      def Var.java (lazy ["java"]);
      def Var.javac (lazy ["javac"]);
      def Var.jar (lazy ["jar"]);
      def Var.javac_flags (lazy ["-implicit:none"]);
    ]
end

module Backend = struct
  open Var
  open File
  module Nj = Ninja_utils

  let stdlib_target ext =
    let ext =
      match ext.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> "." ^ ext
      | _ -> ext
    in
    let bdir f = ("java" / "catala" / "stdlib" / f) ^ ext in
    !Var.tdir / bdir !Var.dst

  let modfile ~is_stdlib same_dir_modules ext modname =
    let backend = "java" in
    match List.assoc_opt modname same_dir_modules with
    | Some _ when is_stdlib ->
      (!Var.tdir / backend / "catala" / "stdlib" / String.to_id modname) ^ ext
    | _ -> Ninja.modfile ~backend same_dir_modules ext modname

  let[@ocamlformat "disable"] static_base_rules =
    [
      Nj.rule "catala-java"
        ~command:[!catala_exe; "java"; !catala_flags; !catala_flags_java;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; "java"; "⇒"; !output];
      Nj.rule "java-class"
        ~command:[!javac; "-cp"; File.(Var.(!builddir) / Scan.libcatala / "java")^":" ^ !class_path; !javac_flags; !input]
        ~description:["<catala>"; "java"; "⇒"; !output];
    ]

  let external_copy item =
    let catala_src = !Var.tdir / !Var.src in
    let java, missing =
      Ninja.extern_src ~filename:item.Scan.file_name ~backend:"java" ~ext:"java"
        ~missing:[]
    in
    Ninja.check_missing ~backend:"java" ~module_def:item.Scan.module_def
      ~missing ~filename:item.Scan.file_name;
    List.to_seq
      [
        Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[java]
          ~outputs:
            [
              (if item.is_stdlib then stdlib_target "java"
               else Ninja.target ~backend:"java" "java");
            ];
      ]

  let catala ?vars ~is_stdlib ~inputs ~implicit_in _has_scope_tests =
    Seq.return
      (Nj.build "catala-java" ?vars ~inputs ~implicit_in
         ~outputs:
           [
             (if is_stdlib then stdlib_target "java"
              else Ninja.target ~backend:"java" "java");
           ])

  let build_object ~include_dirs ~same_dir_modules ~item _has_scope_tests =
    let modules = List.rev_map Mark.remove item.Scan.used_modules in
    let java_class_path =
      String.concat ":"
        ((!Var.tdir / "java")
        :: List.map
             (fun d ->
               (if Filename.is_relative d then !Var.builddir / d else d)
               / "java")
             include_dirs)
    in
    let module_target =
      modfile ~is_stdlib:item.is_stdlib same_dir_modules ".class"
    in
    [
      Nj.build "java-class"
        ~inputs:
          [
            (if item.is_stdlib then stdlib_target "java"
             else Ninja.target ~backend:"java" "java");
          ]
        ~implicit_in:("@runtime-java" :: List.map module_target modules)
        ~outputs:
          [
            (if item.is_stdlib then stdlib_target "class"
             else Ninja.target ~backend:"java" "class");
          ]
        ~vars:[Var.class_path, [java_class_path]];
    ]

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / "java")
end
