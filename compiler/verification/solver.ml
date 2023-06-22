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

module TypedValuesDcalcVarMap = struct
  type 'x t = (typed Dcalc.Ast.expr, ('x, typed) marked) Var.Map.t

  let map (f : 'a -> 'b) (m : 'a t) : 'b t =
    Var.Map.map (fun (x, tx) -> f x, tx) m
end

module TypedValuesDcalcVarMapBoxLifting = Bindlib.Lift (TypedValuesDcalcVarMap)

type mopsa_program = {
  main_guard : typed Dcalc.Ast.expr;
  other_values : (typed Dcalc.Ast.expr, typed Dcalc.Ast.expr) Var.Map.t;
}

(* The goal of this part is to extract additions from expressions, and perform
   them before as assignments. This will simplify the analysis and communication
   with Mopsa. *)
let turn_vc_into_mopsa_compatible_program
    (vc : Conditions.verification_condition) : mopsa_program =
  let trivial_map_union = Var.Map.union (fun _ _ _ -> assert false) in
  let rec split_expression_into_atomic_parts (e : typed Dcalc.Ast.expr) :
      (typed Dcalc.Ast.expr, (dcalc, typed) boxed_gexpr) Var.Map.t
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
      Var.Map.add dummy_var new_e acc, Expr.evar dummy_var (Mark.get e)
    | _ ->
      Expr.map_gather ~acc:Var.Map.empty ~join:trivial_map_union
        ~f:split_expression_into_atomic_parts e
  in
  let new_vars, (simple_guard, simple_guard_mark) =
    split_expression_into_atomic_parts vc.vc_guard
  in
  let new_vars, simple_guard =
    (* This manipulation is done so that we only unbox once to keep all
       variables in sync. *)
    Bindlib.unbox
      (Bindlib.box_apply2
         (fun new_vars simple_guard ->
           new_vars, (simple_guard, simple_guard_mark))
         (TypedValuesDcalcVarMapBoxLifting.lift_box new_vars)
         simple_guard)
  in
  { main_guard = simple_guard; other_values = new_vars }

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
  let interesting_vars =
    let rec collect_vars = function
      | EVar v, _ -> Var.Set.singleton v
      | e ->
        Expr.shallow_fold
          (fun e -> Var.Set.union (collect_vars e))
          e Var.Set.empty
    in
    let rec related_vars todos acc =
      match Var.Set.choose_opt todos with
      | None -> acc
      | Some t ->
        let new_vars =
          List.fold_left
            (fun acc e -> Var.Set.union acc (collect_vars e))
            Var.Set.empty
            (Option.value ~default:[]
               (Var.Map.find_opt t
                  vc_scope_ctx.Conditions.vc_scope_possible_variable_values))
        in
        related_vars
          (Var.Set.union (Var.Set.remove t todos) new_vars)
          (Var.Set.union acc new_vars)
    in
    related_vars (collect_vars vc.vc_guard) (collect_vars vc.vc_guard)
  in
  Message.emit_debug "For: %a@.Assumptions: %a@.Relevant values: %a@."
    (Print.expr ()) vc.vc_guard (Print.expr ())
    vc_scope_ctx.Conditions.vc_scope_asserts
    (fun fmt vars_possible_values ->
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
        (fun fmt (var, values) ->
          if Var.Set.mem var interesting_vars then Format.fprintf fmt "<IMP>";
          Format.fprintf fmt "@[<hov 2>%a@ = @ %a@]" Print.var var
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ |@ ")
               (fun fmt expr -> Print.expr () fmt expr))
            values;
          if Var.Set.mem var interesting_vars then Format.fprintf fmt "</IMP>")
        fmt
        (Var.Map.bindings vars_possible_values))
    vc_scope_ctx.Conditions.vc_scope_possible_variable_values;
  let prog = turn_vc_into_mopsa_compatible_program vc in
  let prog_string =
    Var.Map.fold
      (fun var expr acc ->
        Format.asprintf "%a %a = %a@." (Print.typ decl_ctx) (Expr.ty expr)
          Print.var_debug var (Print.expr ()) expr
        ^ acc)
      prog.other_values ""
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
