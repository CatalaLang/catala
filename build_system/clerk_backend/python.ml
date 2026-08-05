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

let catala_flags_python = Var.make "CATALA_FLAGS_PYTHON"
let python = Var.make "PYTHON"

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

let run_artifact config ~test ~trace ?scope ~var_bindings ?quiet src =
  let open File in
  let build_dir = config.Clerk_cli.options.global.build_dir in
  let cmd =
    let base = Filename.basename (File.remove_extension src) in
    Var.get var_bindings python
    @ ["-m"; base ^ "." ^ base]
    @ Option.to_list scope
    @ (if test && not Global.options.debug then ["--test"] else [])
    @ (if Global.options.output_format = JSON then ["--json"] else [])
    @ if trace then ["--trace"] else []
  in
  let pythonpath =
    String.concat ":"
      [
        build_dir / Scan.libcatala / "python";
        File.dirname src;
        Option.value ~default:"" (Sys.getenv_opt "PYTHONPATH");
      ]
  in
  Message.debug "Executing artifact: 'PYTHONPATH=%s %s'..." pythonpath
    (String.concat " " cmd);
  Clerk_cli.run_command_line ~setenv:["PYTHONPATH", pythonpath] ?quiet cmd

module Spec : Sig.Spec = struct
  open Var
  open File
  module Nj = Ninja_utils

  let name = "python"
  let src_extensions = ["py"]
  let module_extensions = ["py"]
  let obj_extension = "py"
  let all_obj_extensions = ["py"]
  let stdlib_subdir = "."

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
    [def catala_flags_python (lazy catala_flags); def python (lazy ["python3"])]

  let[@ocamlformat "disable"] rules =
    [
      Nj.rule "catala-python"
        ~command:[!catala_exe; name; !catala_flags; !catala_flags_python;
                  "-o"; !output; "--"; !input]
        ~description:["<catala>"; name; "⇒"; !output];
    ]

  let build_runtime ~options:_ ~stdbase =
    let python_base = stdbase / name / "catala_runtime" in
    let python_src = Var.(!runtime) / name / "src" / "catala" in
    [
      Nj.build "phony"
        ~inputs:
          [python_base -.- "py"; python_base /../ "dates.py"; Var.(!catala_exe)]
        ~outputs:["@python/runtime/src"; "@python/runtime/obj"];
      Nj.build "copy"
        ~inputs:[python_src / "dates.py"]
        ~outputs:[python_base /../ "dates.py"];
      Nj.build "copy"
        ~inputs:[python_src / "catala_runtime.py"]
        ~outputs:[python_base -.- "py"];
    ]

  let catala ?vars ~is_stdlib:_ ~inputs ~implicit_in ~has_scope_tests:_ =
    Seq.return
      (Nj.build "catala-python" ?vars ~inputs ~implicit_in
         ~outputs:[Common.target ~name "py"])

  let build_object ~include_dirs:_ ~same_dir_modules:_ _ = Seq.empty

  let runtime_dir : File.t Lazy.t =
    lazy File.(Lazy.force Poll.runtime_dir / name / "src" / "catala")

  let write_target_def_file ~options:_ ~dir target =
    let open File in
    File.with_out_channel (dir / "__init__.py") (fun oc ->
        Printf.fprintf oc "__all__ = [%s]\n"
          (String.concat ", " target.Clerk_config.tmodules));
    File.with_out_channel (dir / "py.typed") ignore

  let install_runtime ~options =
    let open File in
    let dir = options.Clerk_config.global.target_dir / name / Scan.libcatala in
    remove dir;
    ensure_dir dir;
    copy_dir ()
      ~filter:(fun f -> Filename.check_suffix f ".py" && f <> "__init__.py")
      ~src:(Lazy.force Poll.stdlib_dir / name / "src" / "catala")
      ~dst:dir
end

include Common.Make_backend (Spec)
