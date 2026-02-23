(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2026 Inria,
   contributors: Aymeric Fromherz <aymeric.fromherz@inria.fr>, Louis Gesbert
   <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

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
      ~autotest:false ~typed:Shared_ast.Expr.typed
  in
  Verification.Globals.setup ~optimize ~disable_counterexamples;
  let vcs =
    Verification.Conditions.generate_verification_conditions prg
      (Option.map (Driver.Commands.get_scope_uid prg.decl_ctx) ex_scope_opt)
  in
  Verification.Solver.solve_vc prg.decl_ctx vcs

let proof_cmd =
  let open Cmdliner.Term in
  let open Catala_utils.Cli in
  const proof
  $ Flags.include_dirs
  $ Flags.stdlib_dir
  $ Flags.optimize
  $ Flags.ex_scope_opt
  $ Flags.check_invariants
  $ Flags.disable_counterexamples

let () =
  Driver.Plugin.register "proof-plugin" proof_cmd
    ~doc:
      "This plugin provides formal verification of properties of Catala \
       programs, based on an encoding into the Z3 smt-solver"
