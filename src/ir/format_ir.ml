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

open Ir

let format_int_literal fmt (v: int_literal) =
  match v with
  | Int i -> Format.fprintf fmt "%s" (Int64.to_string i)
  | IntVar var -> Format_ast.format_int_var fmt var

let format_bool_literal fmt (v: bool_literal) =
  match v with
  | Bool b -> Format.fprintf fmt "%b" b
  | BoolVar var -> Format_ast.format_bool_var fmt var

let format_logical_expression fmt (e: logical_expression) = match e with
  | Comparison (op, v1, v2) ->
    Format.fprintf fmt "%a %a %a"
      format_int_literal (Pos.unmark v1)
      Format_ast.format_comparison_op (Pos.unmark op)
      format_int_literal (Pos.unmark v2)
  | LogicalBinop (op, v1, v2) ->
    Format.fprintf fmt "%a %a %a"
      format_bool_literal (Pos.unmark v1)
      Format_ast.format_logical_binop (Pos.unmark op)
      format_bool_literal (Pos.unmark v2)
  | LogicalNot v1 ->
    Format.fprintf fmt "!%a" format_bool_literal (Pos.unmark v1)
  | BoolLiteral b -> format_bool_literal fmt (Pos.unmark b)

let format_arithmetic_expression fmt (e: arithmetic_expression) = match e with
  | ArithmeticBinop (op, v1, v2) ->
    Format.fprintf fmt "%a %a %a"
      format_int_literal (Pos.unmark v1)
      Format_ast.format_arithmetic_binop (Pos.unmark op)
      format_int_literal (Pos.unmark v2)
  | Conditional (v1, v2, v3) ->
    Format.fprintf fmt "if %a then %a else %a"
      format_bool_literal (Pos.unmark v1)
      format_int_literal (Pos.unmark v2)
      format_int_literal (Pos.unmark v3)
  | ArithmeticMinus v1 ->
    Format.fprintf fmt "- %a" format_int_literal (Pos.unmark v1)
  | IntLiteral v -> format_int_literal fmt (Pos.unmark v)

let format_command fmt (c: command) = match c with
  | BoolDef (bv, e) ->
    Format.fprintf fmt "%a : bool := %a"
      Format_ast.format_bool_var bv
      format_logical_expression (Pos.unmark e)
  | IntDef (iv, e) ->
    Format.fprintf fmt "%a : int := %a"
      Format_ast.format_int_var iv
      format_arithmetic_expression (Pos.unmark e)
  | Constraint e ->
    Format.fprintf fmt "assert(%a)"
      format_logical_expression (Pos.unmark e)

let format_func fmt (f: func) =
  Format.fprintf fmt "function(%a, %a) -> %a, %a\n%a"
    (Format_ast.pp_print_list_comma Format_ast.format_int_var) (fst f.inputs)
    (Format_ast.pp_print_list_comma Format_ast.format_bool_var) (snd f.inputs)
    (Format_ast.pp_print_list_comma Format_ast.format_int_var) (fst f.outputs)
    (Format_ast.pp_print_list_comma Format_ast.format_bool_var) (snd f.outputs)
    (Format_ast.pp_print_list_endline format_command) f.body

let format_program fmt (p: program) =
  Ast.FunctionVariableMap.map_printer Format_ast.format_function_var format_func fmt p.program_functions
