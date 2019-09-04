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

(**
   This module describes the core backend language for describing tax specifications : Verifisc.
   Programs in Verifisc only deal with boolean logic and integer arithmetic modulo 2^64. The
   language is imperative, each function consisting of variable definitions and constraints that
   should hold during the program execution.

   This language is meant for formal analysis of the tax specification.
*)

module Variable (_ : sig end) = struct
  type t = {
    name: string Pos.marked;
    id: int;
    descr: string Pos.marked;
  }

  let counter : int ref = ref 0

  let fresh_id () : int=
    let v = !counter in
    counter := !counter + 1;
    v

  let new_var
      (name: string Pos.marked)
      (descr: string Pos.marked)
    : t =
    {
      name; id = fresh_id (); descr;
    }

  let compare (var1 :t) (var2 : t) =
    compare var1.id var2.id
end

module BoolVariable = Variable ()
module BoolVariableMap = Map.Make(BoolVariable)
module IntVariable = Variable ()
module IntVariableMap = Map.Make(IntVariable)
module FunctionVariable = Variable ()
module FunctionVariableMap = Map.Make(FunctionVariable)

type typ =
  | Int
  | Bool

type comparison_op = Lt | Lte | Gt | Gte | Neq | Eq

type logical_binop = And | Or

type arithmetic_binop = Add | Sub | Mul | Div

type logical_expression =
  | Comparison of comparison_op Pos.marked * arithmetic_expression Pos.marked * arithmetic_expression Pos.marked
  | LogicalBinop of logical_binop Pos.marked * logical_expression Pos.marked * logical_expression Pos.marked
  | LogicalNot of logical_expression Pos.marked
  | BoolLiteral of bool
  | BoolVar of BoolVariable.t

and arithmetic_expression =
  | ArithmeticBinop of arithmetic_binop Pos.marked * arithmetic_expression Pos.marked * arithmetic_expression Pos.marked
  | ArithmeticMinus of arithmetic_expression Pos.marked
  | Conditional of logical_expression Pos.marked * arithmetic_expression Pos.marked * arithmetic_expression Pos.marked
  | IntLiteral of Int64.t
  | IntVar of IntVariable.t

type command =
  | BoolDef of BoolVariable.t * logical_expression Pos.marked
  | IntDef of IntVariable.t * arithmetic_expression Pos.marked
  | Constraint of logical_expression Pos.marked

type variables = IntVariable.t list * BoolVariable.t list

type func = {
  body: command list;
  inputs: variables;
  outputs: variables;
}

type idmap_var =
  | IDBoolVar of BoolVariable.t
  | IDIntVar of IntVariable.t

type idmap = idmap_var list Pos.VarNameToID.t

type program = {
  program_functions: func FunctionVariableMap.t;
  program_mult_factor: int;
  program_idmap: idmap
}
