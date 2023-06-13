(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
   Aymeric Fromherz <aymeric.fromherz@inria.fr>

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

(** [solve_vc] is the main entry point of this module. It takes a list of
    expressions [vcs] corresponding to verification conditions that must be
    discharged by Z3, and attempts to solve them **)
let solve_vc
    (decl_ctx : Shared_ast.decl_ctx)
    (vcs : Conditions.verification_condition list) : unit =
  (* Right now we only use the Z3 backend but the functorial interface should
     make it easy to mix and match different proof backends. *)
  Z3backend.Io.init_backend ();
  let z3_vcs =
    List.map
      (fun vc ->
        ( vc,
          try
            let ctx = Z3backend.Io.make_context decl_ctx in
            let ctx =
              Z3backend.Io.encode_asserts ctx vc.Conditions.vc_asserts
            in
            let ctx, z3_vc =
              Z3backend.Io.translate_expr ctx vc.Conditions.vc_guard
            in
            Z3backend.Io.Success (z3_vc, ctx)
          with Failure msg -> Fail msg ))
      vcs
  in
  let all_proven =
    List.fold_left
      (fun all_proven vc ->
        if Z3backend.Io.encode_and_check_vc decl_ctx vc then all_proven
        else false)
      true z3_vcs
  in
  if all_proven then
    Message.emit_result "No errors found during the proof mode run."
