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


type bool_literal =
  | Bool of bool
  | BoolVar of Ast.BoolVariable.t

type int_literal =
  | Int of Int64.t
  | IntVar of Ast.IntVariable.t


type logical_expression =
  | Comparison of Ast.comparison_op Pos.marked * int_literal Pos.marked * int_literal Pos.marked
  | LogicalBinop of Ast.logical_binop Pos.marked * bool_literal Pos.marked * bool_literal Pos.marked
  | LogicalNot of bool_literal Pos.marked
  | BoolLiteral of bool_literal Pos.marked

type arithmetic_expression =
  | ArithmeticBinop of Ast.arithmetic_binop Pos.marked * int_literal Pos.marked * int_literal Pos.marked
  | ArithmeticMinus of int_literal Pos.marked
  | Conditional of bool_literal Pos.marked * int_literal Pos.marked * int_literal Pos.marked
  | IntLiteral of int_literal Pos.marked

type command =
  | BoolDef of Ast.BoolVariable.t * logical_expression Pos.marked
  | IntDef of Ast.IntVariable.t * arithmetic_expression Pos.marked
  | Constraint of logical_expression Pos.marked


type func = {
  body: command list;
  inputs: Ast.variables;
  outputs: Ast.variables;
}

type program = {
  program_functions: func Ast.FunctionVariableMap.t;
  program_mult_factor: int;
  program_idmap: Ast.idmap
}

let nb_commands (p: program) : int =
  Ast.FunctionVariableMap.fold
    (fun _ func acc ->
       acc + List.length func.body
    ) p.program_functions 0
