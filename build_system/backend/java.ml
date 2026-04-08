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
open Var
open File

module Backend = struct
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

  module Flags = struct
    let default
        ~variables
        ~autotest
        ~use_default_flags
        ~test_flags
        ~include_dirs:_ =
      let catala_flags_java =
        Common.Flags.catala_backend_flags ~autotest ~use_default_flags
          ~test_flags ~accepts_closure_conversion:true
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

  let runtime_build_statements ~options ~stdbase =
    let java_base = stdbase / "java" in
    let java_src = Var.(!runtime) / "java" in
    let runtime_orig =
      match
        List.assoc_opt
          Var.(name runtime)
          options.Clerk_lib.Clerk_config.variables
      with
      | Some r -> lazy (String.concat " " r)
      | None -> Poll.runtime_dir
    in
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
      let base = options.global.build_dir / Scan.libcatala / "java" in
      File.with_out_channel ~bin:false (base / "java.files") (fun oc ->
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
    Seq.return
      (Nj.build "java-class"
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
         ~vars:[Var.class_path, [java_class_path]])

  let expose_module ~same_dir_modules:_ ~used_modules:_ = []

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / "java")
end
