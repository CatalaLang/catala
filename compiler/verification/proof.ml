(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributors:
   Denis Merigoux <denis.merigoux@inria.fr>,
   Louis Gesbert <louis.gesbert@inria.fr>.

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
open Shared_ast
open Cmdliner

let proof
    includes
    stdlib
    optimize
    ex_scope_opt
    check_invariants
    disable_counterexamples
    options =
  let prg, _ =
    Driver.Passes.dcalc options ~includes ~stdlib ~optimize ~check_invariants
      ~autotest:false ~typed:Expr.typed
  in
  Globals.setup ~optimize ~disable_counterexamples;
  let vcs =
    Conditions.generate_verification_conditions prg
      (Option.map (Driver.Commands.get_scope_uid prg.decl_ctx) ex_scope_opt)
  in
  Solver.solve_vc prg.decl_ctx vcs

let proof_term =
  Term.(
    const proof
    $ Cli.Flags.include_dirs
    $ Cli.Flags.stdlib_dir
    $ Cli.Flags.optimize
    $ Cli.Flags.ex_scope_opt
    $ Cli.Flags.check_invariants
    $ Cli.Flags.disable_counterexamples)

let () =
  Driver.Plugin.register "proof" proof_term
    ~man:Cli.man_base
    ~doc:
      "Generates and proves verification conditions about the \
       well-behaved execution of the Catala program."
