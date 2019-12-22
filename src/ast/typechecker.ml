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

type ctx = {
  ctx_defined_variables : variables;
}

let rec typecheck_logical_expression
    (e: logical_expression Pos.marked)
    (ctx: ctx)
  : unit = match Pos.unmark e with
  | Comparison (_, e1, e2) ->
    typecheck_arithmetic_expression e1 ctx;
    typecheck_arithmetic_expression e2 ctx
  | LogicalBinop (_, e1, e2) ->
    typecheck_logical_expression e1 ctx;
    typecheck_logical_expression e2 ctx
  | LogicalNot e1 ->
    typecheck_logical_expression e1 ctx;
  | BoolLiteral _ -> ()
  | BoolVar var ->
    if not (List.mem var (snd ctx.ctx_defined_variables)) then
      Errors.verifisc_type_error
        "boolean variable %s used %a is undefined"
        (Pos.unmark var.BoolVariable.name)
        Pos.format_position (Pos.get_position e)

and typecheck_arithmetic_expression
    (e: arithmetic_expression Pos.marked)
    (ctx: ctx)
  : unit = match Pos.unmark e with
  | ArithmeticBinop (_, e1, e2) ->
    typecheck_arithmetic_expression e1 ctx;
    typecheck_arithmetic_expression e2 ctx
  | ArithmeticMinus e1 ->
    typecheck_arithmetic_expression e1 ctx
  | Conditional (e1, e2, e3) ->
    typecheck_logical_expression e1 ctx;
    typecheck_arithmetic_expression e2 ctx;
    typecheck_arithmetic_expression e3 ctx
  | IntLiteral _ -> ()
  | IntVar var ->
    if not (List.mem var (fst ctx.ctx_defined_variables)) then
      Errors.verifisc_type_error
        "integer variable %s used %a is undefined"
        (Pos.unmark var.IntVariable.name)
        Pos.format_position (Pos.get_position e)

let typecheck (program : program) : unit =
  FunctionVariableMap.iter (fun _ func ->
      let ctx = {
        ctx_defined_variables = func.inputs;
      } in
      let ctx = List.fold_left (fun ctx cmd ->
          match cmd with
          | BoolDef (var, e) ->
            if List.mem var (snd ctx.ctx_defined_variables) then
              Errors.verifisc_type_error
                "Forbidden variable redefiniton: %s %a"
                (Pos.unmark var.Ast.BoolVariable.name)
                Pos.format_position (Pos.get_position e);
            typecheck_logical_expression e ctx;
            {
              ctx_defined_variables =
                (fst ctx.ctx_defined_variables,
                 var::(snd ctx.ctx_defined_variables)
                )}
          | IntDef (var, e) ->
            if List.mem var (fst ctx.ctx_defined_variables) then
              Errors.verifisc_type_error
                "Forbidden variable redefiniton: %s %a"
                    (Pos.unmark var.Ast.IntVariable.name)
                    Pos.format_position (Pos.get_position e);
            typecheck_arithmetic_expression e ctx;
            {
              ctx_defined_variables =
                (var::(fst ctx.ctx_defined_variables),
                 snd ctx.ctx_defined_variables)
            }
          | Constraint e ->
            typecheck_logical_expression e ctx;
            ctx
        ) ctx func.body in
      List.iter (fun output_var ->
          if not (List.mem output_var (fst ctx.ctx_defined_variables)) then
            Errors.verifisc_type_error "integer output variable %s is undefined"
              (Pos.unmark output_var.IntVariable.name)
        ) (fst func.outputs);
      List.iter (fun output_var ->
          if not (List.mem output_var (snd ctx.ctx_defined_variables)) then
            Errors.verifisc_type_error "boolean output variable %s is undefined"
                    (Pos.unmark output_var.BoolVariable.name)
        ) (snd func.outputs)
    ) program.program_functions
