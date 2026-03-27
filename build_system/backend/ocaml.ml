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

  let default ~variables ~autotest ~use_default_flags ~test_flags ~include_dirs
      =
    let open Common.Flags in
    let catala_flags_ocaml =
      (if autotest then ["--autotest"] else [])
      @
      if use_default_flags then ["-O"]
      else
        List.filter
          (function
            | "-O" | "--optimize" | "--closure-conversion" -> true | _ -> false)
          test_flags
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
          @ Common.Flags.includes ~backend:"ocaml" include_dirs));
    ]
end

module Backend = struct
  module Nj = Ninja_utils

  let[@ocamlformat "disable"] static_base_rules =
    let open Var in
    let runtime_include = File.(Var.(!builddir) / Scan.libcatala / "ocaml") in
         [
      Nj.rule "catala-ocaml"
        ~command:[!catala_exe; "ocaml"; !catala_flags; !catala_flags_ocaml;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; "ocaml"; "⇒"; !output];
      Nj.rule "ocaml-bytobject"
        ~command:[
          !ocamlc_exe; "-c"; !ocaml_flags; !ocaml_include; "-I"; runtime_include; !includes; !input
        ]
        ~description:["<ocaml>"; "⇒"; !output];

      Nj.rule "ocaml-natobject"
        ~command:[
          !ocamlopt_exe; "-c"; !ocaml_flags; !ocaml_include; "-I"; runtime_include; !includes; !input
        ]
        ~description:["<ocaml>"; "⇒"; !output];

      Nj.rule "ocaml-module"
        ~command:
          [!ocamlopt_exe; "-shared"; !ocaml_flags; !ocaml_include; "-I"; runtime_include; !input;
           "-o"; !output]
        ~description:["<ocaml>"; "⇒"; !output];
    ]

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / "ocaml")
end
