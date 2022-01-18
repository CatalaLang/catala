(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2022 Inria, contributor: Aymeric Fromherz
   <aymeric.fromherz@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Dcalc.Ast
open Z3
open Z3backend

(** [solve_vc] is the main entry point of this module. It takes a list of expressions [vcs]
    corresponding to verification conditions that must be discharged by Z3, and attempts to solve
    them **)
let solve_vc (prgm : program) (decl_ctx : decl_ctx) (vcs : Conditions.verification_condition list) :
    unit =
  Cli.debug_print (Format.asprintf "Running Z3 version %s" Version.to_string);

  let cfg = [ ("model", "true"); ("proof", "false") ] in
  let z3_ctx = mk_context cfg in
  let make_backend_context (free_vars_typ : typ Pos.marked VarMap.t) =
    {
      ctx_z3 = z3_ctx;
      ctx_decl = decl_ctx;
      ctx_var =
        VarMap.union
          (fun _ _ _ -> failwith "[Z3 encoding]: A Variable cannot be both free and bound")
          (variable_types prgm) free_vars_typ;
      ctx_funcdecl = VarMap.empty;
      ctx_z3vars = StringMap.empty;
      ctx_z3datatypes = EnumMap.empty;
      ctx_z3matchsubsts = VarMap.empty;
      ctx_z3structs = StructMap.empty;
    }
  in
  let z3_vcs =
    List.map
      (fun vc ->
        ( vc,
          try
            let ctx, z3_vc =
              Z3backend.translate_expr
                (make_backend_context vc.Conditions.vc_free_vars_typ)
                (Bindlib.unbox (Dcalc.Optimizations.remove_all_logs vc.Conditions.vc_guard))
            in
            Z3backend.Io.Success (z3_vc, ctx)
          with Failure msg -> Fail msg ))
      vcs
  in
  List.iter (Z3backend.Io.encode_and_check_vc decl_ctx) z3_vcs
