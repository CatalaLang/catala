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

module Flags = struct
  let def ~variables var value =
    let value =
      match List.assoc_opt (Var.name var) variables with
      | Some vl -> vl
      | None -> Lazy.force value
    in
    var, value

  let includes ?backend include_dirs =
    List.fold_right
      (fun dir flags ->
        if Filename.is_relative dir then
          "-I"
          :: File.(
               Var.(!builddir)
               / match backend with Some b -> dir / b | None -> dir)
          :: flags
        else "-I" :: dir :: flags)
      include_dirs []

  let include_flags ~backend include_dirs =
    let open File in
    "-I"
    :: Var.(!tdir / backend)
    :: List.concat_map
         (fun d ->
           [
             "-I";
             (if Filename.is_relative d then Var.(!builddir) / d else d)
             / backend;
           ])
         include_dirs

  let default ~code_coverage ~config =
    let options = config.Clerk_cli.options in
    let open Clerk_config in
    let catala_flags =
      ("--stdlib=" ^ File.(Var.(!builddir) / Scan.libcatala))
      :: ("--directory=" ^ Var.(!builddir))
      :: options.global.catala_opts
    in
    let includes = includes options.global.include_dirs in
    let test_flags = config.Clerk_cli.test_flags in
    let def = def ~variables:options.variables in
    [
      def Var.ninja_required_version (lazy ["1.7"]);
      (* use of implicit outputs *)
      def Var.builddir (lazy [options.global.build_dir]);
      def Var.runtime (lazy [Lazy.force Poll.runtime_dir]);
      def Var.clerk_exe (lazy [Lazy.force Poll.clerk_exe]);
      def Var.catala_exe
        (lazy
          [
            (match options.global.catala_exe with
            | Some e -> File.check_exec e
            | None -> Lazy.force Poll.catala_exe);
          ]);
      def Var.catala_flags
        (lazy
          (catala_flags
          @ (if Message.has_color stderr then ["--color=always"] else [])
          @ includes));
      def Var.clerk_flags
        (lazy
          ("-e"
           :: Var.(!catala_exe)
           :: ("--test-flags=" ^ String.concat "," test_flags)
           :: includes
          @ (if code_coverage then ["--code-coverage"] else [])
          @ List.map
              (fun f -> "--catala-opts=" ^ f)
              (catala_flags @ if code_coverage then ["--whole-program"] else [])
          ));
    ]
end

module Ninja = struct
  module Nj = Ninja_utils

  let static_base_rules =
    let open Var in
    [
      Nj.rule "copy"
        ~command:
          (if Sys.win32 then
             ["cmd"; "/c"; "copy /by >nul"; !input; "+nul"; !output]
             (* The "+nul" forces the timestamp of the new file to be updated *)
           else ["cp"; "-f"; !input; !output])
        ~description:["<copy>"; !input];
    ]
end
