(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

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

(* For selected operators, inserts the reified code position as first
   argument *)
let rec add_op_pos_expr ~(op_needs_pos : 'a Operator.t -> naked_typ -> bool) =
  function
  | EAppOp { op; args = a :: _ as args; tys }, m
    when op_needs_pos (Mark.remove op) (Mark.remove (Expr.ty a)) ->
    (* FIXME: is tys specialised enough at this point ? *)
    let pos = Expr.mark_pos m in
    let args = List.map (add_op_pos_expr ~op_needs_pos) args in
    Expr.eappop ~op
      ~args:(Expr.make_pos pos m :: args)
      ~tys:((TLit TPos, pos) :: tys) m
  | e -> Expr.map ~f:(add_op_pos_expr ~op_needs_pos) ~op:Fun.id e

let add_op_pos_program
    ~(op_needs_pos : 'a Operator.t -> naked_typ -> bool)
    (prg : 'm Ast.program) =
  Program.map_exprs prg ~varf:Fun.id ~f:(add_op_pos_expr ~op_needs_pos)

let process_program
    ~(op_needs_pos : lcalc Operator.t -> naked_typ -> bool)
    (prg : 'm Ast.program) : 'm Ast.program =
  let program = add_op_pos_program ~op_needs_pos prg in
  let positions =
    let rec gather_pos = function
      | EPos p, _ -> Pos.Set.singleton p
      | e ->
        Expr.shallow_fold
          (fun e acc -> Pos.Set.union acc (gather_pos e))
          e Pos.Set.empty
    in
    let positions, exports =
      Program.fold_exprs_full program
        ~f:(fun acc e _ty -> Pos.Set.union acc (gather_pos e))
        ~init:Pos.Set.empty
    in
    List.fold_left
      (fun acc (_, e) -> Pos.Set.union acc (gather_pos e))
      positions exports
  in
  if Pos.Set.is_empty positions then program
  else
    let pos_table_expr, pos_table_ty =
      let m = Program.get_mark_witness program in
      let pos = Pos.void in
      let elts = Pos.Set.elements positions in
      let ty = TArray (TLit TPos, pos), pos in
      ( (EArray (List.map (fun pos -> EPos pos, m) elts), Expr.with_ty ~pos m ty),
        ty )
    in
    let pos_table_name = TopdefName.fresh [] ("loc", Pos.void) in
    let pos_table_topdef =
      Topdef (pos_table_name, pos_table_ty, Public, pos_table_expr)
      |> Bindlib.box (* the def uses no variables *)
    in
    let pos_indexes =
      Seq.zip (Pos.Set.to_seq positions) (Seq.ints 0) |> Pos.Map.of_seq
    in
    let pos_table_var = Var.make "loc" in
    (* Turns the position expressions into lookups in the position table *)
    let rec rewrite_pos_expr = function
      | EPos p, m ->
        let index = Pos.Map.find p pos_indexes in
        Expr.eappop ~op:(Op.ArrayAccess index, p)
          ~args:[Expr.evar pos_table_var (Expr.with_ty m pos_table_ty)]
          ~tys:[pos_table_ty] m
      | e -> Expr.map ~f:rewrite_pos_expr ~op:Fun.id e
    in
    let code_items =
      Scope.map_exprs program.code_items ~varf:Fun.id ~f:rewrite_pos_expr
    in
    let code_items = BoundList.cons pos_table_var pos_table_topdef code_items in
    Expr.Box.assert_closed code_items;
    {
      program with
      code_items = Bindlib.unbox code_items;
      decl_ctx =
        {
          program.decl_ctx with
          ctx_topdefs =
            TopdefName.Map.add pos_table_name (pos_table_ty, Public)
              program.decl_ctx.ctx_topdefs;
        };
    }
