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

let format_typ (t: typ) : string = match t with
  | Int -> "integer"
  | Bool -> "boolean"

let format_comparison_op (op : comparison_op) : string = match op with
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="
  | Neq -> "!="
  | Eq -> "=="

let format_logical_binop (op: logical_binop) : string = match op with
  | And -> "&&"
  | Or -> "||"

let format_arithmetic_binop (op: arithmetic_binop) : string = match op with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let format_bool_var (b: BoolVariable.t) : string =
  Printf.sprintf "%s_%d"
    (Pos.unmark b.BoolVariable.name)
    b.BoolVariable.id

let format_int_var (b: IntVariable.t) : string =
  Printf.sprintf "%s_%d"
    (Pos.unmark b.IntVariable.name)
    b.IntVariable.id

let format_function_var (b: FunctionVariable.t) : string =
  Printf.sprintf "%s_%d"
    (Pos.unmark b.FunctionVariable.name)
    b.FunctionVariable.id

let rec format_logical_expression (e: logical_expression) : string = match e with
  | Comparison (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (format_arithmetic_expression (Pos.unmark e1))
      (format_comparison_op (Pos.unmark op))
      (format_arithmetic_expression (Pos.unmark e2))
  | LogicalBinop (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (format_logical_expression (Pos.unmark e1))
      (format_logical_binop (Pos.unmark op))
      (format_logical_expression (Pos.unmark e2))
  | LogicalNot e1 ->
    Printf.sprintf "!%s" (format_logical_expression (Pos.unmark e1))
  | BoolLiteral b -> string_of_bool b
  | BoolVar v -> format_bool_var v

and format_arithmetic_expression (e: arithmetic_expression) : string = match e with
  | ArithmeticBinop (op, e1, e2) ->
    Printf.sprintf "(%s %s %s)"
      (format_arithmetic_expression (Pos.unmark e1))
      (format_arithmetic_binop (Pos.unmark op))
      (format_arithmetic_expression (Pos.unmark e2))
  | Conditional (e1, e2, e3) ->
    Printf.sprintf "(if %s then %s else %s)"
      (format_logical_expression (Pos.unmark e1))
      (format_arithmetic_expression (Pos.unmark e2))
      (format_arithmetic_expression (Pos.unmark e3))
  | ArithmeticMinus e1 ->
    Printf.sprintf "- %s" (format_arithmetic_expression (Pos.unmark e1))
  | IntLiteral i -> Int64.to_string i
  | IntVar v -> format_int_var v

let format_command (c: command) : string = match c with
  | BoolDef (bv, e) ->
    Printf.sprintf "%s : bool := %s"
      (format_bool_var bv)
      (format_logical_expression (Pos.unmark e))
  | IntDef (iv, e) ->
    Printf.sprintf "%s : int := %s"
      (format_int_var iv)
      (format_arithmetic_expression (Pos.unmark e))
  | Constraint e ->
    Printf.sprintf "assert(%s)"
      (format_logical_expression (Pos.unmark e))

let format_func (f: func) : string =
  Printf.sprintf "function(%s, %s) -> %s, %s\n%s"
    (String.concat "," (List.map (fun v -> format_int_var v) (fst f.inputs)))
    (String.concat "," (List.map (fun v -> format_bool_var v) (snd f.inputs)))
    (String.concat "," (List.map (fun v -> format_int_var v) (fst f.outputs)))
    (String.concat "," (List.map (fun v -> format_bool_var v) (snd f.outputs)))
    (String.concat "\n" (List.map (fun c -> format_command c) f.body))

let format_program (p: program) : string =
  FunctionVariableMap.fold (fun fvar f acc ->
      acc ^ begin
        Printf.sprintf "%s ::= %s\n\n"
          (format_function_var fvar)
          (format_func f)
      end
    ) p.program_functions ""
