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
module Flags = struct end

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
end
