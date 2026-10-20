(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
module Runtime = Catala_runtime

let value_of_literal (lit : Ast.literal) : Runtime.Value.t option =
  let percent d = Q.div d (Q.of_int 100) in
  match lit with
  | LNumber ((Int i, _), None) ->
    Some (V (Integer, Runtime.integer_of_string i))
  | LNumber ((Int i, _), Some (Percent, _)) ->
    Some (V (Decimal, percent (Runtime.decimal_of_string i)))
  | LNumber ((Dec (i, f), _), None) ->
    Some (V (Decimal, Runtime.decimal_of_string (i ^ "." ^ f)))
  | LNumber ((Dec (i, f), _), Some (Percent, _)) ->
    Some (V (Decimal, percent (Runtime.decimal_of_string (i ^ "." ^ f))))
  | LNumber ((Int i, _), Some (Year, _)) ->
    Some (V (Duration, Runtime.duration_of_numbers (int_of_string i) 0 0))
  | LNumber ((Int i, _), Some (Month, _)) ->
    Some (V (Duration, Runtime.duration_of_numbers 0 (int_of_string i) 0))
  | LNumber ((Int i, _), Some (Day, _)) ->
    Some (V (Duration, Runtime.duration_of_numbers 0 0 (int_of_string i)))
  | LNumber ((Dec _, _), Some ((Year | Month | Day), _)) -> None
  | LBool b -> Some (V (Bool, b))
  | LMoneyAmount m ->
    let cents =
      Runtime.integer_of_string (m.money_amount_units ^ m.money_amount_cents)
    in
    Some
      (V
         ( Money,
           if m.money_amount_sign then cents else Runtime.Oper.o_minus_mon cents
         ))
  | LDate d -> (
    try
      Some
        (V
           ( Date,
             Runtime.date_of_numbers d.literal_date_year d.literal_date_month
               d.literal_date_day ))
    with Failure _ -> None)

(* Surface expressions that denote a value: literals, and enum constructors,
   structs, lists and tuples built from them. As with [Runtime.Value.parse], the
   descriptors of compound values are generic (enums are
   [(0, constructor, payload)], structs their [(field, value)] list...) since
   their declarations aren't known at this stage. Returns [None] on anything
   that needs evaluating. *)
let rec value_of_expression (e : Ast.expression) : Runtime.Value.t option =
  let values el =
    List.fold_right
      (fun e acc ->
        match acc, value_of_expression e with
        | Some l, Some v -> Some (v :: l)
        | _ -> None)
      el (Some [])
  in
  let enum name constr payload =
    Runtime.Value.V (Enum { name; constr = Fun.id }, (0, constr, payload))
  in
  match Mark.remove e with
  | Paren e -> value_of_expression e
  | Literal lit -> value_of_literal lit
  | Unop ((Minus _, _), e) -> (
    match value_of_expression e with
    | Some (V (Integer, i)) -> Some (V (Integer, Runtime.Oper.o_minus_int i))
    | Some (V (Decimal, d)) -> Some (V (Decimal, Runtime.Oper.o_minus_rat d))
    | Some (V (Money, m)) -> Some (V (Money, Runtime.Oper.o_minus_mon m))
    | Some (V (Duration, d)) -> Some (V (Duration, Runtime.Oper.o_minus_dur d))
    | _ -> None)
  | EnumInject ((constr, _), payload) -> (
    let payload =
      match payload with
      | None -> Some None
      | Some e -> Option.map Option.some (value_of_expression e)
    in
    match payload with
    | None -> None
    | Some payload -> (
      (* The optional enum is named as [Shared_ast.Print] embeds it *)
      let lang = Runtime.Print.get_lang () in
      let optional =
        match lang with
        | `En -> "Optional"
        | `Fr -> "Optionnel"
        | `Pl -> "Opcjonalny"
      in
      match constr with
      | CBuiltin Present ->
        Some
          (enum optional
             (match lang with
             | `En -> "Present"
             | `Fr -> "Présent"
             | `Pl -> "Obecny")
             payload)
      | CBuiltin Absent ->
        Some
          (enum optional
             (match lang with `En | `Fr -> "Absent" | `Pl -> "Nieobecny")
             payload)
      | CConstr (_, (constr, _)) -> Some (enum "" constr payload)))
  | StructLit (((_, (name, _)), _), fields) ->
    let names, exprs = List.split fields in
    Option.map
      (fun vs ->
        Runtime.Value.V
          ( Struct { name; fields = Fun.id },
            List.combine (List.map Mark.remove names) vs ))
      (values exprs)
  | ArrayLit el ->
    Option.map
      (fun vs -> Runtime.Value.V (Array Fun.id, Array.of_list vs))
      (values el)
  | Tuple el ->
    Option.map (fun vs -> Runtime.Value.V (Tuple Fun.id, vs)) (values el)
  | _ -> None
