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
open Shared_ast

let rec vars_used_in_vc vc_scope_ctx (e : typed Dcalc.Ast.expr) :
  typed Dcalc.Ast.expr Var.Set.t =
  (* We search recursively in the possible definitions of each free
     variable. *)
  let free_vars = Expr.free_vars e in
  let possible_values_of_free_vars =
    Var.Map.filter
      (fun v _ -> Var.Set.mem v free_vars)
      vc_scope_ctx.Conditions.vc_scope_possible_variable_values
  in
  Var.Map.fold
    (fun _ possible_values vars_used ->
       List.fold_left
         (fun vars_used possible_value ->
            Var.Set.union (vars_used_in_vc vc_scope_ctx possible_value) vars_used)
         vars_used possible_values)
    possible_values_of_free_vars free_vars

module TypedValuesDcalcVarMap = struct
  type 'x t = (typed Dcalc.Ast.expr, (('x, typed) marked) option) Var.Map.t

  let map (f : 'a -> 'b) (m : 'a t) : 'b t =
    Var.Map.map (Option.map (fun (x, tx) -> f x, tx)) m
end

module TypedValuesDcalcVarMapBoxLifting = Bindlib.Lift (TypedValuesDcalcVarMap)

type mopsa_program = {
  main_guard : typed Dcalc.Ast.expr;
  declared_variables :
    ((dcalc, typed) gexpr Var.t * typed Dcalc.Ast.expr option) list;
        (* ((typed Dcalc.Ast.expr * (typed Dcalc.Ast.expr) option)) list; *)
}

(* The goal of this part is to extract additions from expressions, and perform
   them before as assignments. This will simplify the analysis and communication
   with Mopsa. *)
let turn_vc_into_mopsa_compatible_program
    (vc : Conditions.verification_condition) vc_scope_ctx : mopsa_program =
  let vars_used_in_vc = vars_used_in_vc vc_scope_ctx vc.vc_guard in
  let vars_used_in_vc_with_known_values =
    Var.Set.filter
      (fun v ->
         Var.Map.mem v vc_scope_ctx.Conditions.vc_scope_possible_variable_values)
      vars_used_in_vc
  in
  let vars_used_in_vc_defined_outside_of_scope =
    Var.Set.inter
      vc_scope_ctx.Conditions.vc_scope_variables_defined_outside_of_scope
      vars_used_in_vc
  in
  let decls_to_top =
    let others =
      Var.Set.diff
        vars_used_in_vc
        (Var.Set.union
           vars_used_in_vc_defined_outside_of_scope
           vars_used_in_vc_with_known_values) in
    List.fold_left (fun decls v -> (v, None)::decls) [] (Var.Set.elements (Var.Set.union vars_used_in_vc_defined_outside_of_scope others))
  in
  let assignments =
    List.fold_left (fun decls (var, values) ->
        if Var.Set.mem var vars_used_in_vc_with_known_values then
          (* FIXME handle multiple values *)
          let () = assert((List.length values) = 1) in
          (var, (Some (List.hd values)))::decls
        else
          decls
      ) []
      (Var.Map.bindings vc_scope_ctx.vc_scope_possible_variable_values) in
  let declared_variables = decls_to_top @ assignments in 
  let trivial_map_union = Var.Map.union (fun _ _ _ -> assert false) in
  let rec transform_field_accesses_into_variables e =
    match Mark.remove e with
    | EStructAccess
        { e = (EVar v, _); } when List.exists (fun (v2, _) -> Var.compare v v2 = 0) declared_variables ->
      Message.emit_debug "Variable %a in assignements, but we may need to patch with %a" Print.var v (Print.expr ()) e;
      let var = Var.make (Format.asprintf "%a" (Print.expr ()) e) in
      Var.Map.singleton var None, Expr.evar var (Mark.get e)
    | _ ->
      Expr.map_gather ~acc:Var.Map.empty ~join:trivial_map_union
        ~f:transform_field_accesses_into_variables e
  in
  let rec split_expression_into_atomic_parts (e : typed Dcalc.Ast.expr) :
      (typed Dcalc.Ast.expr, ((dcalc, typed) boxed_gexpr) option) Var.Map.t
      * (dcalc, typed) boxed_gexpr =
    match Mark.remove e with 
    | EApp
        {
          f =
            ( EOp { op = Op.Add_dat_dur Dates_calc.Dates.AbortOnRound; tys = _ },
              _ ) as f;
          args;
        } ->
      let acc, args =
        List.fold_left_map
          (fun acc arg ->
            let toadd, arg = split_expression_into_atomic_parts arg in
            trivial_map_union acc toadd, arg)
          Var.Map.empty args
      in
      let dummy_var =
        let pos = Expr.pos e in
        let basename =
          Filename.basename (Pos.get_file pos) |> Filename.chop_extension
        in
        Var.make
          (Format.asprintf "var_%s_%d-%d_%d-%d" basename
             (Pos.get_start_line pos) (Pos.get_start_column pos)
             (Pos.get_end_line pos) (Pos.get_end_column pos))
      in
      let new_e = Expr.eapp (Expr.box f) args (Mark.get e) in
      Var.Map.add dummy_var (Some new_e) acc, Expr.evar dummy_var (Mark.get e)
    | _ ->
      Expr.map_gather ~acc:Var.Map.empty ~join:trivial_map_union
        ~f:split_expression_into_atomic_parts e
  in
  let new_vars_fields, (e, e_mark) =
    transform_field_accesses_into_variables vc.vc_guard in
  let new_vars_fields, e =
    Bindlib.unbox
      (Bindlib.box_apply2
         (fun new_vars e ->
            new_vars, (e, e_mark))
         (TypedValuesDcalcVarMapBoxLifting.lift_box new_vars_fields)
         e) in
  let new_vars, (simple_guard, simple_guard_mark) =
    split_expression_into_atomic_parts e
  in
  let new_vars, simple_guard =
    (* This manipulation is done so that we only unbox once to keep all
       variables in sync. *)
    Bindlib.unbox
      (Bindlib.box_apply2
         (fun new_vars simple_guard ->
           new_vars, (simple_guard, simple_guard_mark))
         (TypedValuesDcalcVarMapBoxLifting.lift_box new_vars)
         simple_guard) in
  {
    main_guard = simple_guard;
    declared_variables = declared_variables @ (Var.Map.bindings new_vars_fields) @ (Var.Map.bindings new_vars)
  }

(** This function is temporarily here but should be moved to a MOPSA backend. *)
let solve_date_vc
    (decl_ctx : decl_ctx)
    (scope_name : ScopeName.t)
    (vc_scope_ctx : Conditions.verification_conditions_scope)
    (vc : Conditions.verification_condition) =
  Message.emit_debug "%s"
    (Z3backend.Io.print_negative_result vc scope_name
       (Z3backend.Io.make_context decl_ctx)
       None);
  let prog = turn_vc_into_mopsa_compatible_program vc vc_scope_ctx in
  let prog_string =
    List.fold_left
      (fun acc (var, oexpr) ->
         acc^(match oexpr with
         | None ->
           (* FIXME typ *)
           (* let (Typed { ty = match_ty; _ }) = Mark.get var in *)
           Format.asprintf "%a = T@." (*(Print.typ decl_ctx) (Expr.ty )*) Print.var_debug var
         | Some expr ->
           Format.asprintf "%a %a = %a@." (Print.typ decl_ctx) (Expr.ty expr)
             Print.var_debug var (Print.expr ()) expr)
      )
      "" prog.declared_variables
    ^ Format.asprintf "assert(sync(%a))" (Print.expr ()) prog.main_guard
  in
  Format.eprintf "Prog:@.%s@." prog_string

(** [solve_vc] is the main entry point of this module. It takes a list of
    expressions [vcs] corresponding to verification conditions that must be
    discharged by Z3, and attempts to solve them **)
let solve_vcs
    (decl_ctx : Shared_ast.decl_ctx)
    (vcs : Conditions.verification_conditions_scope ScopeName.Map.t) : unit =
  (* Right now we only use the Z3 backend but the functorial interface should
     make it easy to mix and match different proof backends. *)
  Z3backend.Io.init_backend ();
  let all_proven =
    ScopeName.Map.fold
      (fun scope_name scope_vcs all_proven ->
        let dates_vc =
          List.filter
            (fun vc ->
              match vc.Conditions.vc_kind with
              | Conditions.DateComputation -> true
              | _ -> false)
            scope_vcs.Conditions.vc_scope_list
        in
        List.iter (solve_date_vc decl_ctx scope_name scope_vcs) dates_vc;
        let z3_vcs =
          List.map
            (fun vc ->
              ( vc,
                try
                  let ctx = Z3backend.Io.make_context decl_ctx in
                  let ctx =
                    Z3backend.Io.encode_asserts ctx scope_vcs.vc_scope_asserts
                  in
                  let ctx, z3_vc =
                    Z3backend.Io.translate_expr ctx vc.Conditions.vc_guard
                  in
                  Z3backend.Io.Success (z3_vc, ctx)
                with Failure msg -> Fail msg ))
            (List.filter
               (fun vc ->
                 match vc.Conditions.vc_kind with
                 | Conditions.NoEmptyError | Conditions.NoOverlappingExceptions
                   ->
                   true
                 | Conditions.DateComputation -> false)
               scope_vcs.Conditions.vc_scope_list)
        in
        List.fold_left
          (fun all_proven vc ->
            if Z3backend.Io.check_vc decl_ctx scope_name scope_vcs vc then
              all_proven
            else false)
          all_proven z3_vcs)
      vcs true
  in
  if all_proven then
    Message.emit_result "No potential errors found during the proof mode run."
  else
    Message.raise_error "Some potential errors were found during the proof run."
