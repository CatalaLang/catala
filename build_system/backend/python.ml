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

let linking_command ~build_dir link_deps item target =
  (* a "linked" python module is a "Module.py" folder containing the module .py
     file along with the runtime and all dependencies, plus a __init__.py
     file *)
  let open File in
  let tdir = Filename.remove_extension target in
  remove tdir;
  ensure_dir tdir;
  List.iter
    (fun it ->
      let src =
        let f = Scan.target_file_name it in
        (build_dir / dirname f / "python" / basename f) ^ ".py"
      in
      copy_in ~src ~dir:tdir)
    (link_deps item);
  copy_in ~src:(target -.- "py") ~dir:tdir;
  close_out (open_out (tdir / "__init__.py"));
  []

module Backend = struct
  open Var
  open File
  module Nj = Ninja_utils

  module Flags = struct
    let default
        ~variables
        ~autotest
        ~use_default_flags
        ~test_flags
        ~include_dirs:_ =
      let catala_flags_python =
        Common.Flags.catala_backend_flags ~autotest ~use_default_flags
          ~test_flags ~accepts_closure_conversion:true
      in
      let def = Common.Flags.def ~variables in
      [
        def Var.catala_flags_python (lazy catala_flags_python);
        def Var.python (lazy ["python3"]);
      ]
  end

  let[@ocamlformat "disable"] static_base_rules =
    [
      Nj.rule "catala-python"
        ~command:[!catala_exe; "python"; !catala_flags; !catala_flags_python;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; "python"; "⇒"; !output];
    ]

  let external_copy item =
    let catala_src = !Var.tdir / !Var.src in
    let py, missing =
      Ninja.extern_src ~filename:item.Scan.file_name ~backend:"python" ~ext:"py"
        ~missing:[]
    in
    Ninja.check_missing ~backend:"python" ~module_def:item.Scan.module_def
      ~missing ~filename:item.Scan.file_name;
    List.to_seq
      [
        Nj.build "copy" ~implicit_in:[catala_src] ~inputs:[py]
          ~outputs:[Ninja.target ~backend:"python" "py"];
      ]

  let modfile ~is_stdlib:_ = Ninja.modfile ~backend:"python"

  let runtime_build_statements ~options:_ ~stdbase =
    let python_base = stdbase / "python" / "catala_runtime" in
    let python_src = Var.(!runtime) / "python" / "src" / "catala" in
    [
      Nj.build "phony"
        ~inputs:
          [python_base -.- "py"; python_base /../ "dates.py"; Var.(!catala_exe)]
        ~outputs:["@runtime-python"];
      Nj.build "copy"
        ~inputs:[python_src / "dates.py"]
        ~outputs:[python_base /../ "dates.py"];
      Nj.build "copy"
        ~inputs:[python_src / "catala_runtime.py"]
        ~outputs:[python_base -.- "py"];
    ]

  let catala ?vars ~is_stdlib:_ ~inputs ~implicit_in _has_scope_tests =
    Seq.return
      (Nj.build "catala-python" ?vars ~inputs ~implicit_in
         ~outputs:[Ninja.target ~backend:"python" "py"])

  let build_object ~include_dirs:_ ~same_dir_modules:_ ~item:_ _has_scope_tests
      =
    Seq.empty

  let expose_module ~same_dir_modules:_ ~used_modules:_ = []

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / "python" / "src" / "catala")
end
