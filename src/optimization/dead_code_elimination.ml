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
  used_bool_vars : unit Ast.BoolVariableMap.t;
  used_int_vars: unit Ast.IntVariableMap.t;
}

let empty_data (func: func) = {
  used_bool_vars = List.fold_left (fun acc var ->
      Ast.BoolVariableMap.add var () acc
    ) Ast.BoolVariableMap.empty (snd func.outputs);
  used_int_vars =  List.fold_left (fun acc var ->
      Ast.IntVariableMap.add var () acc
    ) Ast.IntVariableMap.empty (fst func.outputs);
}

let add_bool_use (var: Ast.BoolVariable.t) (data: data) : data =
  { data with
    used_bool_vars = Ast.BoolVariableMap.add var () data.used_bool_vars
  }

let process_bool_value (v : bool_literal Pos.marked) (data: data) : data =
  match Pos.unmark v with
  | Bool _ -> data
  | BoolVar v -> add_bool_use v data


let add_int_use (var: Ast.IntVariable.t) (data: data) : data =
  { data with
    used_int_vars = Ast.IntVariableMap.add var () data.used_int_vars
  }

let process_int_value (v : int_literal Pos.marked) (data: data) : data =
  match Pos.unmark v with
  | Int _ -> data
  | IntVar v -> add_int_use v data


let process_bool_expr (e: logical_expression Pos.marked) (data: data) : data =
  match Pos.unmark e with
  | Comparison (_, v1, v2) ->
    let data = process_int_value v1 data in
    let data = process_int_value v2 data in
    data
  | LogicalBinop (_, v1, v2) ->
    let data = process_bool_value v1 data in
    let data = process_bool_value v2 data in
    data
  | LogicalNot v1 ->
    let data = process_bool_value v1 data in
    data
  | BoolLiteral v -> process_bool_value v data

let process_int_expr (e: arithmetic_expression Pos.marked) (data: data) : data =
  match Pos.unmark e with
  | ArithmeticBinop (_, v1, v2) ->
    let data = process_int_value v1 data in
    let data = process_int_value v2 data in
    data
  | ArithmeticMinus v1 ->
    let data = process_int_value v1 data in
    data
  | Conditional (v1, v2, v3) ->
    let data = process_bool_value v1 data in
    let data = process_int_value v2 data in
    let data = process_int_value v3 data in
    data
  | IntLiteral v -> process_int_value v data

let process_command (c: command) (data: data)
  : bool * data = match c with
  | BoolDef (var, e) ->
    let is_necessary = Ast.BoolVariableMap.mem var data.used_bool_vars in
    let data =
      if is_necessary then
        process_bool_expr e data
      else data
    in
    (is_necessary, data)
  | IntDef (var, e) ->
    let is_necessary = Ast.IntVariableMap.mem var data.used_int_vars in
    let data =
      if is_necessary then
        process_int_expr e data
      else data
    in
    (is_necessary, data)
  | Constraint e ->
    (true, process_bool_expr e data)

let optimize (p: program) : program =
  { p with
    program_functions = Ast.FunctionVariableMap.map (fun func ->
        { func with
          body =
            let data = empty_data func in
            let new_body, _ = List.fold_right (fun cmd (new_body, data) ->
                let is_necessary, data = process_command cmd data in
                (if is_necessary then cmd::new_body else new_body), data
              ) func.body ([], data)
            in
            new_body
        }
      ) p.program_functions
  }
