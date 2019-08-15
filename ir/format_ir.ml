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
