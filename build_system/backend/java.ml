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
open File

type Clerk_lib.Clerk_config.backend_config +=
  | Java_config of { package_prefix : string option }

let java_config =
  let open Clerk_lib.Clerk_toml_encoding in
  conv
    (function
      | Java_config { package_prefix; _ } -> package_prefix
      | _ ->
        Message.error ~internal:true
          "Unexpected non-java configuration while encoding backend \
           configuration")
    (fun package_prefix -> Java_config { package_prefix })
    (obj1 (opt_field ~name:"package-prefix" string))

let () =
  Clerk_lib.Clerk_config.(
    register_backend_config ~name:"java" ~backend:Java java_config)

let catala_flags_java = Var.make "CATALA_FLAGS_JAVA"
let javac = Var.make "JAVAC"
let javac_flags = Var.make "JAVAC_FLAGS"
let jar = Var.make "jar"
let java = Var.make "JAVA"

let linking_command ~build_dir ~var_bindings link_deps item target =
  let open Var in
  let jar_target = target -.- "jar" in
  let classes =
    let class_files =
      target
      :: List.filter_map
           (fun it ->
             if it.Scan.is_stdlib then None
             else
               let f = Scan.target_file_name it in
               Some ((build_dir / dirname f / "java" / basename f) -.- "class"))
           (link_deps item)
    in
    let (h : (string, string list) Hashtbl.t) = Hashtbl.create 5 in
    (* 'javac' generates one file per inner class. Sadly, we do generate a lot
       of those. We need to pack those in the jar as well. *)
    let fetch_inner_classes class_file =
      let basename = File.(remove_extension (basename class_file)) in
      let dirname = Filename.dirname class_file in
      let dir_classes =
        Hashtbl.find_opt h dirname
        |> function
        | Some dir_classes -> dir_classes
        | None ->
          let dir_contents = Sys.readdir dirname in
          let dir_classes =
            Seq.filter
              (String.ends_with ~suffix:".class")
              (Array.to_seq dir_contents)
            |> List.of_seq
          in
          Hashtbl.replace h dirname dir_classes;
          dir_classes
      in
      List.filter_map
        (fun clazz ->
          if String.starts_with ~prefix:(basename ^ "$") clazz then
            Some (dirname / clazz)
          else None)
        dir_classes
    in
    List.concat_map
      (fun class_file -> class_file :: fetch_inner_classes class_file)
      class_files
  in
  let java_dir_prefix = build_dir / Scan.libcatala / "java" in
  let runtime_class_files =
    File.scan_tree
      (fun f -> if Filename.check_suffix f ".class" then Some f else None)
      java_dir_prefix
    |> Seq.flat_map (fun (_, _, files) -> List.to_seq files)
    |> List.of_seq
  in
  get_var var_bindings jar
  @ ["--create"; "--file"; jar_target]
  @ List.concat_map
      (fun clazz -> ["-C"; Filename.dirname clazz; Filename.basename clazz])
      classes
  @ List.concat_map
      (fun clazz ->
        ["-C"; java_dir_prefix; File.remove_prefix java_dir_prefix clazz])
      runtime_class_files

let run_artifact ~var_bindings ~test src =
  let open Clerk_lib in
  let target_main = File.remove_extension (Filename.basename src) in
  let cmd =
    Var.get_var var_bindings java
    @ ["-cp"; src -.- "jar"; target_main]
    @ (if test then ["--test"] else [])
    @ if Global.options.output_format = JSON then ["--json"] else []
  in
  Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
  Clerk_cli.run_command_line cmd

module Backend = struct
  open Var
  module Nj = Ninja_utils

  let name = "java"
  let module_ext = ".class"
  let src_extensions = ["java"]
  let obj_extensions = ["class"]

  let runtime_targets ~only_source =
    [(if only_source then "@runtime-" ^ name ^ "-src" else "@runtime-" ^ name)]

  let stdlib_target ext =
    let ext =
      match ext.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> "." ^ ext
      | _ -> ext
    in
    let bdir f = (name / "catala" / "stdlib" / f) ^ ext in
    !Var.tdir / bdir !Var.dst

  let modfile ~is_stdlib same_dir_modules ext modname =
    match List.assoc_opt modname same_dir_modules with
    | Some _ when is_stdlib ->
      (!Var.tdir / name / "catala" / "stdlib" / String.to_id modname) ^ ext
    | _ -> Ninja.modfile ~backend:name same_dir_modules ext modname

  module Flags = struct
    let default
        ~variables
        ~autotest
        ~use_default_flags
        ~test_flags
        ~include_dirs:_ =
      let catala_flags =
        Common.Flags.catala_backend_flags ~autotest ~use_default_flags
          ~test_flags ~accepts_closure_conversion:true
      in
      let def = Common.Flags.def ~variables in
      [
        def catala_flags_java (lazy catala_flags);
        def java (lazy ["java"]);
        def javac (lazy ["javac"]);
        def jar (lazy ["jar"]);
        def javac_flags (lazy ["-implicit:none"]);
      ]
  end

  let[@ocamlformat "disable"] static_base_rules =
    [
      Nj.rule "catala-java"
        ~command:[!catala_exe; name; !catala_flags; !catala_flags_java;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; name; "⇒"; !output];
      Nj.rule "java-class"
        ~command:[!javac; "-cp"; File.(Var.(!builddir) / Scan.libcatala / name)^":" ^ !class_path; !javac_flags; !input]
        ~description:["<catala>"; name; "⇒"; !output];
    ]

  let external_copy item =
    let catala_src = !Var.tdir / !Var.src in
    let java, missing =
      Ninja.extern_src ~filename:item.Scan.file_name ~backend:name ~ext:"java"
        ~missing:[]
    in
    Ninja.check_missing ~backend:name ~module_def:item.Scan.module_def ~missing
      ~filename:item.Scan.file_name;
    List.to_seq
      [
        Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[java]
          ~outputs:
            [
              (if item.is_stdlib then stdlib_target "java"
               else Ninja.target ~backend:name "java");
            ];
      ]

  let runtime_build_statements ~options ~stdbase =
    let java_base = stdbase / name in
    let java_src = Var.(!runtime) / name in
    let runtime_orig =
      match
        List.assoc_opt
          Var.(name runtime)
          options.Clerk_lib.Clerk_config.variables
      with
      | Some r -> lazy (String.concat " " r)
      | None -> Poll.runtime_dir
    in
    let java_orig_prefix = Lazy.force runtime_orig / name in
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
      let base = options.global.build_dir / Scan.libcatala / name in
      File.with_out_channel ~bin:false
        (base / (name ^ ".files"))
        (fun oc ->
          List.iter (fun s -> output_string oc ((base / s) ^ "\n")) java_files);
      java_base / (name ^ ".files")
    in
    Nj.build "phony"
      ~inputs:(List.map (fun f -> (java_base / f) -.- "java") java_files)
      ~outputs:["@runtime-" ^ name ^ "-src"]
    :: Nj.build "phony"
         ~inputs:(List.map (fun f -> (java_base / f) -.- "class") java_files)
         ~outputs:["@runtime-" ^ name]
    :: Nj.build "java-class" ~inputs:[]
         ~implicit_in:
           (java_list_file :: List.map (fun f -> java_base / f) java_files)
         ~outputs:(List.map (fun f -> (java_base / f) -.- "class") java_files)
         ~vars:[javac_flags, [Var.(!javac_flags); "@" ^ java_list_file]]
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
              else Ninja.target ~backend:name "java");
           ])

  let build_object ~include_dirs ~same_dir_modules ~item _has_scope_tests =
    let modules = List.rev_map Mark.remove item.Scan.used_modules in
    let java_class_path =
      String.concat ":"
        ((!Var.tdir / name)
        :: List.map
             (fun d ->
               (if Filename.is_relative d then !Var.builddir / d else d) / name)
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
              else Ninja.target ~backend:name "java");
           ]
         ~implicit_in:(("@runtime-" ^ name) :: List.map module_target modules)
         ~outputs:
           [
             (if item.is_stdlib then stdlib_target "class"
              else Ninja.target ~backend:name "class");
           ]
         ~vars:[Var.class_path, [java_class_path]])

  let expose_module ~same_dir_modules:_ ~used_modules:_ = []

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / name)
end
