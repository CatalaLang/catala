(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
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
