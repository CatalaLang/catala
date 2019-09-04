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

type data = {
  known_bool_vars : bool_literal Ast.BoolVariableMap.t;
  known_int_vars : int_literal Ast.IntVariableMap.t;
}

let empty_data = {
  known_bool_vars = Ast.BoolVariableMap.empty;
  known_int_vars = Ast.IntVariableMap.empty;
}

let partially_evaluate_logical_expr (e: logical_expression Pos.marked) (_ : data)
  : logical_expression Pos.marked = e

let partially_evaluate_arithmetic_expr (e: arithmetic_expression Pos.marked) (_ : data)
  : arithmetic_expression Pos.marked = e

let partially_evaluate_command
    (c: command)
    (data: data)
  : (command * data) = match c with
  | BoolDef (var, e) ->
    let new_e = partially_evaluate_logical_expr e data in
    let data =
      { data with
        known_bool_vars = match Pos.unmark new_e with
          | BoolLiteral b -> Ast.BoolVariableMap.add var (Pos.unmark b) data.known_bool_vars
          | _ -> data.known_bool_vars
      } in
    BoolDef (var, new_e), data
  | IntDef (var, e) ->
    let new_e = partially_evaluate_arithmetic_expr e data in
    let data =
      { data with
        known_int_vars = match Pos.unmark new_e with
          | IntLiteral i -> Ast.IntVariableMap.add var (Pos.unmark i) data.known_int_vars
          | _ -> data.known_int_vars
      } in
    IntDef (var, new_e), data
  | Constraint e ->
    let new_e = partially_evaluate_logical_expr e data in
    Constraint new_e, data

let optimize (p: program) : program =
  { p with
    program_functions = Ast.FunctionVariableMap.map (fun func ->
        { func with
          body =
            let data = empty_data in
            let new_body, _ = List.fold_left (fun (new_body, data) cmd ->
                let new_cmd, data = partially_evaluate_command cmd data in
                new_cmd::new_body, data
              ) ([], data) func.body
            in
            List.rev new_body
        }
      ) p.program_functions
  }
