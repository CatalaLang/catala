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

let format_int_literal (v: int_literal) : string =
  match v with
  | Int i -> Int64.to_string i
  | IntVar var -> Format_ast.format_int_var var

let format_bool_literal (v: bool_literal) : string =
  match v with
  | Bool b -> string_of_bool b
  | BoolVar var -> Format_ast.format_bool_var var

let format_logical_expression (e: logical_expression) : string = match e with
  | Comparison (op, v1, v2) ->
    Printf.sprintf "%s %s %s"
      (format_int_literal (Pos.unmark v1))
      (Format_ast.format_comparison_op (Pos.unmark op))
      (format_int_literal (Pos.unmark v2))
  | LogicalBinop (op, v1, v2) ->
    Printf.sprintf "%s %s %s"
      (format_bool_literal (Pos.unmark v1))
      (Format_ast.format_logical_binop (Pos.unmark op))
      (format_bool_literal (Pos.unmark v2))
  | LogicalNot v1 ->
    Printf.sprintf "!%s" (format_bool_literal (Pos.unmark v1))
  | BoolLiteral b -> format_bool_literal (Pos.unmark b)

let format_arithmetic_expression (e: arithmetic_expression) : string = match e with
  | ArithmeticBinop (op, v1, v2) ->
    Printf.sprintf "%s %s %s"
      (format_int_literal (Pos.unmark v1))
      (Format_ast.format_arithmetic_binop (Pos.unmark op))
      (format_int_literal (Pos.unmark v2))
  | Conditional (v1, v2, v3) ->
    Printf.sprintf "if %s then %s else %s"
      (format_bool_literal (Pos.unmark v1))
      (format_int_literal (Pos.unmark v2))
      (format_int_literal (Pos.unmark v3))
  | ArithmeticMinus v1 ->
    Printf.sprintf "- %s" (format_int_literal (Pos.unmark v1))
  | IntLiteral v -> format_int_literal (Pos.unmark v)

let format_command (c: command) : string = match c with
  | BoolDef (bv, e) ->
    Printf.sprintf "%s : bool := %s"
      (Format_ast.format_bool_var bv)
      (format_logical_expression (Pos.unmark e))
  | IntDef (iv, e) ->
    Printf.sprintf "%s : int := %s"
      (Format_ast.format_int_var iv)
      (format_arithmetic_expression (Pos.unmark e))
  | Constraint e ->
    Printf.sprintf "assert(%s)"
      (format_logical_expression (Pos.unmark e))

let format_func (f: func) : string =
  Printf.sprintf "function(%s, %s) -> %s, %s\n%s"
    (String.concat "," (List.map (fun v -> Format_ast.format_bool_var v) (snd f.inputs)))
    (String.concat "," (List.map (fun v -> Format_ast.format_int_var v) (fst f.inputs)))
    (String.concat "," (List.map (fun v -> Format_ast.format_int_var v) (fst f.outputs)))
    (String.concat "," (List.map (fun v -> Format_ast.format_bool_var v) (snd f.outputs)))
    (String.concat "\n" (List.map (fun c -> format_command c) f.body))

let format_program (p: program) : string =
  Ast.FunctionVariableMap.fold (fun fvar f acc ->
      acc ^ begin
        Printf.sprintf "%s ::= %s\n\n"
          (Format_ast.format_function_var fvar)
          (format_func f)
      end
    ) p.program_functions ""
