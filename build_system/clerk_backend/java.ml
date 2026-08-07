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

let catala_flags_java = Var.make_vector "CATALA_FLAGS_JAVA"
let javac = Var.make_vector "JAVAC"
let javac_flags = Var.make_vector "JAVAC_FLAGS"
let jar = Var.make_vector "jar"
let java = Var.make_vector "JAVA"

let linking_command ~build_dir ~var_bindings link_deps item target =
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
          let dir_contents =
            try Sys.readdir dirname with Sys_error _ -> [||]
          in
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
  let entries =
    List.map
      (fun clazz -> Filename.dirname clazz, Filename.basename clazz)
      classes
    @ List.map
        (fun clazz -> java_dir_prefix, File.remove_prefix java_dir_prefix clazz)
        runtime_class_files
  in
  (* fixme: this function isn't advised as doing side-effects *)
  let argfile = jar_target ^ ".jarargs" in
  File.with_out_channel ~bin:false argfile (fun oc ->
      output_string oc (Backend_paths.jar_argfile_content entries));
  Var.get var_bindings jar @ ["--create"; "--file"; jar_target; "@" ^ argfile]

let run_artifact ~var_bindings ~test ~trace ?scope ?quiet src =
  let target_main = File.remove_extension (Filename.basename src) in
  let cmd =
    Var.get var_bindings java
    @ ["-cp"; src -.- "jar"; target_main]
    @ Option.to_list scope
    @ (if test && not Global.options.debug then ["--test"] else [])
    @ (if Global.options.output_format = JSON then ["--json"] else [])
    @ if trace then ["--trace"] else []
  in
  Message.debug "Executing artifact: '%s'..." (String.concat " " cmd);
  Clerk_cli.run_command_line ?quiet cmd

module Spec : Sig.Spec = struct
  open Var
  module Nj = Ninja_utils

  let name = "java"
  let src_extensions = ["java"]
  let module_extensions = ["class"]

  (* Maybe "java" could be enough for `javac` ? But we would need to adjust the
     linking cmd *)
  let obj_extension = "class"
  let all_obj_extensions = ["class"]
  let stdlib_subdir = "catala" / "stdlib"

  let var_defs
      ~variables
      ~autotest
      ~use_default_flags
      ~test_flags
      ~include_dirs:_ =
    let catala_flags =
      Flags.catala_backend_flags ~autotest ~use_default_flags ~test_flags
        ~accepts_closure_conversion:true
    in
    let def = Flags.def ~variables in
    [
      def catala_flags_java (lazy catala_flags);
      def java (lazy ["java"]);
      def javac (lazy ["javac"]);
      def jar (lazy ["jar"]);
      def javac_flags (lazy ["-implicit:none"]);
    ]

  let[@ocamlformat "disable"] rules =
    [
      Nj.rule "catala-java"
        ~command:[!!catala_exe; Word name; !!catala_flags; !!catala_flags_java;
                  Word "-o"; !!output; Word "--"; !!input]
        ~description:[Word "<catala>"; Word name; Word "⇒"; !!output];
      Nj.rule "java-class"
        ~command:[!!javac; Word "-cp"; Word File.(!builddir / Scan.libcatala / name ^":" ^ !class_path); !!javac_flags; !!input]
        ~description:[Word "<catala>"; Word name; Word "⇒"; !!output];
    ]

  let build_runtime ~options ~stdbase =
    let java_base = stdbase / name in
    let java_src = Var.(!runtime) / name in
    let runtime_orig =
      match
        List.assoc_opt Var.(name runtime) options.Clerk_config.variables
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
    let open Nj.Expr in
    Nj.build "phony"
      ~inputs:(List.map (fun f -> Word ((java_base / f) -.- "java")) java_files)
      ~outputs:[Word "@java/runtime/src"]
    :: Nj.build "phony"
         ~inputs:
           (List.map (fun f -> Word ((java_base / f) -.- "class")) java_files)
         ~outputs:[Word "@java/runtime/obj"]
    :: Nj.build "java-class" ~inputs:[]
         ~implicit_in:(List.map (fun f -> Word (java_base / f)) java_files)
         ~outputs:
           (List.map (fun f -> Word ((java_base / f) -.- "class")) java_files)
         ~vars:
           [
             Nj.Binding.make javac_flags
               [!!javac_flags; Word ("@" ^ java_list_file)];
           ]
    :: List.map
         (fun f ->
           Nj.build "copy"
             ~inputs:[Word (java_src / f)]
             ~outputs:[Word (java_base / f)])
         java_files

  let catala ?vars ~is_stdlib ~inputs ~implicit_in ~has_scope_tests:_ =
    Seq.return
      (Nj.build "catala-java" ?vars ~inputs ~implicit_in
         ~outputs:
           [
             (if is_stdlib then
                Word ((!Var.tdir / name / stdlib_subdir / !Var.dst) -.- "java")
              else Common.target ~name "java");
           ])

  let build_object ~include_dirs ~same_dir_modules:_ item =
    let modules = List.rev_map Mark.remove item.Scan.used_modules in
    let java_class_path = Backend_paths.classpath ~backend:name include_dirs in
    Seq.return
      (Nj.build "java-class"
         ~inputs:
           [
             (if item.is_stdlib then
                Word ((!Var.tdir / name / stdlib_subdir / !Var.dst) -.- "java")
              else Common.target ~name "java");
           ]
         ~implicit_in:
           (Word ("@" ^ name ^ "/runtime/obj")
           :: List.map (Common.interface_dep ~name) modules)
         ~outputs:
           [
             (if item.is_stdlib then
                Word ((!Var.tdir / name / stdlib_subdir / !Var.dst) -.- "class")
              else Common.target ~name "class");
           ]
         ~vars:[Nj.Binding.make Var.class_path java_class_path])

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / name)

  let write_target_def_file ~options:_ ~dir:_ _target = ()
  (* TODO: generate some kind of Java project file ? *)

  let install_runtime ~options =
    let open File in
    let extensions =
      src_extensions
      @ if options.Clerk_config.global.include_objects then ["class"] else []
    in
    let dir = options.global.target_dir / name / Scan.libcatala in
    remove dir;
    ensure_dir dir;
    List.iter
      (fun subdir ->
        copy_dir ()
          ~filter:(fun f -> List.exists (Filename.check_suffix f) extensions)
          ~src:(options.global.build_dir / Scan.libcatala / name / subdir)
          ~dst:(dir / subdir))
      ["catala"; "org"]
end

include Common.Make_backend (Spec)
