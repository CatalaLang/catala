(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

let rec translate_arithmetic_expression
    (e: Ast.arithmetic_expression Pos.marked)
  : Ir.arithmetic_expression Pos.marked * Ir.command list =
  match Pos.unmark e with
  | Ast.ArithmeticBinop (op, e1, e2) ->
    let new_e1, cmds1 = wrap_int_expr e1 in
    let new_e2, cmds2 = wrap_int_expr e2 in
    Pos.same_pos_as (Ir.ArithmeticBinop (op, new_e1, new_e2)) e, cmds2@cmds1
  | Ast.ArithmeticMinus e1 ->
    let new_e1, cmds1 = wrap_int_expr e1 in
    Pos.same_pos_as (Ir.ArithmeticMinus new_e1) e, cmds1
  | Ast.Conditional (e1, e2, e3) ->
    let new_e1, cmds1 = wrap_bool_expr e1 in
    let new_e2, cmds2 = wrap_int_expr e2 in
    let new_e3, cmds3 = wrap_int_expr e3 in
    Pos.same_pos_as (Ir.Conditional (new_e1, new_e2, new_e3)) e, cmds3@cmds2@cmds1
  | _ ->
    let new_e, cmds = wrap_int_expr e in
    Pos.same_pos_as (Ir.IntLiteral new_e) e, cmds

and wrap_int_expr (e: Ast.arithmetic_expression Pos.marked) :
  Ir.int_literal Pos.marked * Ir.command list =
  match Pos.unmark  e with
  | Ast.IntLiteral i -> Pos.same_pos_as (Ir.Int i) e, []
  | Ast.IntVar v -> Pos.same_pos_as (Ir.IntVar v) e, []
  | _ ->
    let new_e, cmds = translate_arithmetic_expression e in
    let int_var = Ast.IntVariable.new_var (Pos.same_pos_as "vi" e) (Pos.same_pos_as "" e) in
    (Pos.same_pos_as (Ir.IntVar int_var) e, (Ir.IntDef (int_var, new_e))::cmds)

and translate_logical_expression
    (e: Ast.logical_expression Pos.marked)
  : Ir.logical_expression Pos.marked * Ir.command list =
  match Pos.unmark e with
  | Ast.LogicalBinop (op, e1, e2) ->
    let new_e1, cmds1 = wrap_bool_expr e1 in
    let new_e2, cmds2 = wrap_bool_expr e2 in
    Pos.same_pos_as (Ir.LogicalBinop (op, new_e1, new_e2)) e, cmds2@cmds1
  | Ast.LogicalNot e1 ->
    let new_e1, cmds1 = wrap_bool_expr e1 in
    Pos.same_pos_as (Ir.LogicalNot new_e1) e, cmds1
  | Ast.Comparison (op, e1, e2) ->
    let new_e1, cmds1 = wrap_int_expr e1 in
    let new_e2, cmds2 = wrap_int_expr e2 in
    Pos.same_pos_as (Ir.Comparison (op, new_e1, new_e2)) e, cmds2@cmds1
  | _ ->
    let new_e, cmds = wrap_bool_expr e in
    Pos.same_pos_as (Ir.BoolLiteral new_e) e, cmds


and wrap_bool_expr (e: Ast.logical_expression Pos.marked) :
  Ir.bool_literal Pos.marked * Ir.command list =
  match Pos.unmark  e with
  | Ast.BoolLiteral i -> Pos.same_pos_as (Ir.Bool i) e, []
  | Ast.BoolVar v -> Pos.same_pos_as (Ir.BoolVar v) e, []
  | _ ->
    let new_e, cmds = translate_logical_expression e in
    let bool_var = Ast.BoolVariable.new_var (Pos.same_pos_as "vb" e) (Pos.same_pos_as "" e) in
    (Pos.same_pos_as (Ir.BoolVar bool_var) e, (Ir.BoolDef (bool_var, new_e))::cmds)


let translate_body (body: Ast.command list) : Ir.command list =
  let new_body = List.fold_left (fun new_body cmd ->
      match cmd with
      | Ast.BoolDef (bool_var, e) ->
        let new_e, new_cmds = translate_logical_expression e in
        (Ir.BoolDef (bool_var, new_e))::new_cmds@new_body
      | Ast.IntDef (int_var, e) ->
        let new_e, new_cmds = translate_arithmetic_expression e in
        (Ir.IntDef (int_var, new_e))::new_cmds@new_body
      | Ast.Constraint e ->
        let new_e, new_cmds = translate_logical_expression e in
        (Ir.Constraint new_e)::new_cmds@new_body
    ) [] body
  in
  List.rev new_body

let translate_program (p: Ast.program) : Ir.program =
  {
    Ir.program_idmap = p.Ast.program_idmap;
    Ir.program_mult_factor = p.Ast.program_mult_factor;
    Ir.program_functions = Ast.FunctionVariableMap.map (fun func ->
        {
          Ir.inputs = func.Ast.inputs;
          Ir.outputs = func.Ast.outputs;
          Ir.body = translate_body func.Ast.body
        }
      ) p.Ast.program_functions

  }
