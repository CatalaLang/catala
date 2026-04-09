(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Emile Rolley <emile.rolley@tuta.io>.

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils

let jsoo
    includes
    stdlib
    output
    optimize
    check_invariants
    autotest
    closure_conversion
    options =
  let open Driver.Commands in
  let prg, type_ordering, _ =
    Driver.Passes.lcalc options ~includes ~stdlib ~optimize ~check_invariants
      ~autotest ~typed:Shared_ast.Expr.typed ~closure_conversion
      ~keep_special_ops:true ~monomorphize_types:false
      ~lift_pos:(Some Lcalc.To_ocaml.op_needs_pos)
      ~renaming:(Some Lcalc.To_ocaml.renaming)
  in
  Message.debug "Compiling program to generate Js_of_ocaml interface...";
  get_output_format options output
    ~ext:(if Global.options.gen_external then "template.ml" else "ml")
    ~suffix:"_jsoo"
  @@ fun output_file fmt ->
  let hashf = Hash.finalise ~monomorphize_types:false in
  Lcalc.To_jsoo_interface.format_program output_file fmt prg ~hashf
    type_ordering

let jsoo_cmd =
  let open Cmdliner in
  Term.(
    const jsoo
    $ Cli.Flags.include_dirs
    $ Cli.Flags.stdlib_dir
    $ Cli.Flags.output
    $ Cli.Flags.optimize
    $ Cli.Flags.check_invariants
    $ Cli.Flags.autotest
    $ Cli.Flags.closure_conversion)

let () =
  Driver.Plugin.register "jsoo" jsoo_cmd
    ~doc:
      "This plugin is for demonstration purposes and should be equivalent to \
       using the built-in Python backend"
