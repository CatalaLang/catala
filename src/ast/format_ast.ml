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

open Ast


let pp_print_list_comma eldisplay fmt l =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ") eldisplay fmt l

let pp_print_list_endline eldisplay fmt l =
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") eldisplay fmt l


let format_typ fmt (t: typ) =
  Format.pp_print_string fmt
    (match t with
     | Int -> "integer"
     | Bool -> "boolean")

let format_comparison_op fmt (op : comparison_op) =
  Format.pp_print_string fmt
    (match op with
     | Lt -> "<"
     | Lte -> "<="
     | Gt -> ">"
     | Gte -> ">="
     | Neq -> "!="
     | Eq -> "==")

let format_logical_binop fmt (op: logical_binop) =
  Format.pp_print_string fmt
    (match op with
     | And -> "&&"
     | Or -> "||")

let format_arithmetic_binop fmt (op: arithmetic_binop) =
  Format.pp_print_string fmt
    (match op with
     | Add -> "+"
     | Sub -> "-"
     | Mul -> "*"
     | Div -> "/")

let format_bool_var fmt (b: BoolVariable.t) =
  Format.fprintf fmt "%s_%d"
    (Pos.unmark b.BoolVariable.name)
    b.BoolVariable.id

let format_int_var fmt (b: IntVariable.t) =
  Format.fprintf fmt "%s_%d"
    (Pos.unmark b.IntVariable.name)
    b.IntVariable.id

let format_function_var fmt (b: FunctionVariable.t) =
  Format.fprintf fmt "%s_%d"
    (Pos.unmark b.FunctionVariable.name)
    b.FunctionVariable.id

let rec format_logical_expression fmt (e: logical_expression) = match e with
  | Comparison (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)"
      format_arithmetic_expression (Pos.unmark e1)
      format_comparison_op (Pos.unmark op)
      format_arithmetic_expression (Pos.unmark e2)
  | LogicalBinop (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)"
      format_logical_expression (Pos.unmark e1)
      format_logical_binop (Pos.unmark op)
      format_logical_expression (Pos.unmark e2)
  | LogicalNot e1 ->
    Format.fprintf fmt "!%a" format_logical_expression (Pos.unmark e1)
  | BoolLiteral b ->
    Format.fprintf fmt "%b" b
  | BoolVar v ->
    format_bool_var fmt v

and format_arithmetic_expression fmt (e: arithmetic_expression) = match e with
  | ArithmeticBinop (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)"
      format_arithmetic_expression (Pos.unmark e1)
      format_arithmetic_binop (Pos.unmark op)
      format_arithmetic_expression (Pos.unmark e2)
  | Conditional (e1, e2, e3) ->
    Format.fprintf fmt "(if %a then %a else %a)"
      format_logical_expression (Pos.unmark e1)
      format_arithmetic_expression (Pos.unmark e2)
      format_arithmetic_expression (Pos.unmark e3)
  | ArithmeticMinus e1 ->
    Format.fprintf fmt "- %a" format_arithmetic_expression (Pos.unmark e1)
  | IntLiteral i ->
    Format.pp_print_string fmt (Int64.to_string i)
  | IntVar v ->
    format_int_var fmt v

let format_command fmt (c: command) = match c with
  | BoolDef (bv, e) ->
    Format.fprintf fmt "%a : bool := %a"
      format_bool_var bv
      format_logical_expression (Pos.unmark e)
  | IntDef (iv, e) ->
    Format.fprintf fmt "%a : int := %a"
      format_int_var iv
      format_arithmetic_expression (Pos.unmark e)
  | Constraint e ->
    Format.fprintf fmt "assert(%a)"
      format_logical_expression (Pos.unmark e)

let format_func fmt (f: func) =
  Format.fprintf fmt "function(%a, %a) -> %a, %a\n%a"
    (pp_print_list_comma format_int_var) (fst f.inputs)
    (pp_print_list_comma format_bool_var) (snd f.inputs)
    (pp_print_list_comma format_int_var) (fst f.outputs)
    (pp_print_list_comma format_bool_var) (snd f.outputs)
    (pp_print_list_endline format_command) f.body

let format_program fmt (p: program) =
  FunctionVariableMap.map_printer format_function_var format_func fmt p.program_functions
