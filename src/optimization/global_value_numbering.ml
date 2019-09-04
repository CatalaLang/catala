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

module ValueNumber = struct
  type t = int

  let counter = ref 0

  let fresh () =
    let out = !counter in
    counter := out + 1;
    out

  let compare = compare
end

module ValueNumberMap = Map.Make(ValueNumber)

module BooleanNumberExp = struct
  type t =
    | Comparison of Ast.comparison_op * ValueNumber.t * ValueNumber.t
    | LogicalBinop of Ast.logical_binop * ValueNumber.t * ValueNumber.t
    | LogicalNot of ValueNumber.t
    | BoolLiteral of bool_literal
  let compare = compare
end

module BooleanNumberExpMap = Map.Make(BooleanNumberExp)

module ArithmeticNumberExp = struct
  type t =
    | ArithmeticBinop of Ast.arithmetic_binop * ValueNumber.t * ValueNumber.t
    | ArithmeticMinus of ValueNumber.t
    | Conditional of ValueNumber.t * ValueNumber.t * ValueNumber.t
    | IntLiteral of int_literal
  let compare = compare
end

module ArithmeticNumberExpMap = Map.Make(ArithmeticNumberExp)

type data = {
  int_numbering : ValueNumber.t ArithmeticNumberExpMap.t;
  int_definitions :  int_literal Pos.marked ValueNumberMap.t;
  bool_numbering : ValueNumber.t BooleanNumberExpMap.t;
  bool_definitions :  bool_literal Pos.marked ValueNumberMap.t;
}

let empty_data = {
  int_numbering = ArithmeticNumberExpMap.empty;
  int_definitions = ValueNumberMap.empty;
  bool_numbering = BooleanNumberExpMap.empty;
  bool_definitions = ValueNumberMap.empty;
}

let update_data_bool (expn : BooleanNumberExp.t) (data : data) : ValueNumber.t * data =
  begin match BooleanNumberExpMap.find_opt expn data.bool_numbering with
    | Some vn -> vn, data
    | None ->
      let vn = ValueNumber.fresh () in
      (vn, {data with bool_numbering = BooleanNumberExpMap.add expn vn data.bool_numbering})
  end

let get_bool_literal (v: bool_literal Pos.marked) (data: data) : ValueNumber.t * data =
  update_data_bool (BoolLiteral (Pos.unmark v)) data

let update_data_int (expn : ArithmeticNumberExp.t) (data : data) : ValueNumber.t * data =
  begin match ArithmeticNumberExpMap.find_opt expn data.int_numbering with
    | Some vn -> vn, data
    | None ->
      let vn = ValueNumber.fresh () in
      (vn, {data with int_numbering = ArithmeticNumberExpMap.add expn vn data.int_numbering})
  end

let get_int_literal (v: int_literal Pos.marked) (data: data) : ValueNumber.t * data =
  update_data_int (IntLiteral (Pos.unmark v)) data

let logical_expr_to_value_number
    (e: logical_expression Pos.marked)
    (data : data)
  : ValueNumber.t * data =
  match Pos.unmark e with
  | Comparison (op, v1, v2) ->
    let nv1, data = get_int_literal v1 data in
    let nv2, data = get_int_literal v2 data in
    let expn = BooleanNumberExp.Comparison (Pos.unmark op, nv1, nv2) in
    update_data_bool expn data
  | LogicalBinop (op, v1, v2) ->
    let nv1, data = get_bool_literal v1 data in
    let nv2, data = get_bool_literal v2 data in
    let expn = BooleanNumberExp.LogicalBinop (Pos.unmark op, nv1, nv2) in
    update_data_bool expn data
  | LogicalNot v1 ->
    let nv1, data = get_bool_literal v1 data in
    let expn = BooleanNumberExp.LogicalNot nv1 in
    update_data_bool expn data
  | BoolLiteral v -> get_bool_literal v data

let arithmetic_expr_to_value_number
    (e: arithmetic_expression Pos.marked)
    (data : data)
  : ValueNumber.t * data = match Pos.unmark e with
  | ArithmeticBinop (op, v1, v2) ->
    let nv1, data = get_int_literal v1 data in
    let nv2, data = get_int_literal v2 data in
    let expn = ArithmeticNumberExp.ArithmeticBinop (Pos.unmark op, nv1, nv2) in
    update_data_int expn data
  | ArithmeticMinus v1 ->
    let nv1, data = get_int_literal v1 data in
    let expn = ArithmeticNumberExp.ArithmeticMinus nv1 in
    update_data_int expn data
  | Conditional (v1, v2, v3) ->
    let nv1, data = get_bool_literal v1 data in
    let nv2, data = get_int_literal v2 data in
    let nv3, data = get_int_literal v3 data in
    let expn = ArithmeticNumberExp.Conditional (nv1, nv2, nv3) in
    update_data_int expn data
  | IntLiteral v -> get_int_literal v data

let bool_definition_to_expression (def: bool_literal Pos.marked) : logical_expression Pos.marked =
  Pos.same_pos_as (BoolLiteral def) def

let int_definition_to_expression (def: int_literal Pos.marked) : arithmetic_expression Pos.marked =
  Pos.same_pos_as (IntLiteral def) def

let gvn_bool_exp (e: logical_expression Pos.marked) (data: data)
  : logical_expression Pos.marked * data * ValueNumber.t =
  let expn, data = logical_expr_to_value_number e data in
  match Pos.unmark e with
  | BoolLiteral _ -> (e, data, expn)
  | _ -> begin match ValueNumberMap.find_opt expn data.bool_definitions with
      | Some def -> (bool_definition_to_expression def, data, expn)
      | None -> e, data, expn
    end

let gvn_int_exp (e: arithmetic_expression Pos.marked) (data: data)
  : arithmetic_expression Pos.marked * data * ValueNumber.t =
  let expn, data = arithmetic_expr_to_value_number e data in
  match Pos.unmark e with
  | IntLiteral _ -> (e, data, expn)
  | _ -> begin match ValueNumberMap.find_opt expn  data.int_definitions with
      | Some def -> (int_definition_to_expression def, data, expn)
      | None -> e, data, expn
    end

let gvn_command
    (c: command)
    (data: data)
  : (command * data) = match c with
  | BoolDef (var, e) ->
    let new_e,data, expn = gvn_bool_exp e data in
    let data =
      { data with
        bool_definitions = ValueNumberMap.update expn (fun def -> match def with
            | None -> Some (BoolVar var, Pos.get_position e)
            | Some _ -> def (* we always keep the old definition ! *)
          ) data.bool_definitions
      } in
    BoolDef (var, new_e), data
  | IntDef (var, e) ->
    let new_e, data, expn = gvn_int_exp e data in
    let data =
      { data with
        int_definitions = ValueNumberMap.update expn (fun def -> match def with
            | None -> Some (IntVar var, Pos.get_position e)
            | Some _ -> def (* we always keep the old definition ! *)
          ) data.int_definitions
      } in
    IntDef (var, new_e), data
  | Constraint e ->
    let new_e,data, _ = gvn_bool_exp e data in
    Constraint new_e, data

let optimize (p: program) : program =
  { p with
    program_functions = Ast.FunctionVariableMap.map (fun func ->
        { func with
          body =
            let data = empty_data in
            let new_body, _ = List.fold_left (fun (new_body, data) cmd ->
                let new_cmd, data = gvn_command cmd data in
                new_cmd::new_body, data
              ) ([], data) func.body
            in
            List.rev new_body
        }
      ) p.program_functions
  }
