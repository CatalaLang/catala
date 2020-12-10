(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module Pos = Utils.Pos
module Errors = Utils.Errors
module Cli = Utils.Cli
module A = Ast

let is_empty_error (e : A.expr Pos.marked) : bool =
  match Pos.unmark e with ELit LEmptyError -> true | _ -> false

let evaluate_operator (op : A.operator Pos.marked) (args : A.expr Pos.marked list) :
    A.expr Pos.marked =
  Pos.same_pos_as
    ( match (Pos.unmark op, List.map Pos.unmark args) with
    | A.Binop A.And, [ ELit (LBool b1); ELit (LBool b2) ] -> A.ELit (LBool (b1 && b2))
    | A.Binop A.Or, [ ELit (LBool b1); ELit (LBool b2) ] -> A.ELit (LBool (b1 || b2))
    | A.Binop (A.Add KInt), [ ELit (LInt i1); ELit (LInt i2) ] -> A.ELit (LInt (Z.add i1 i2))
    | A.Binop (A.Sub KInt), [ ELit (LInt i1); ELit (LInt i2) ] -> A.ELit (LInt (Z.sub i1 i2))
    | A.Binop (A.Mult KInt), [ ELit (LInt i1); ELit (LInt i2) ] -> A.ELit (LInt (Z.mul i1 i2))
    | A.Binop (A.Div KInt), [ ELit (LInt i1); ELit (LInt i2) ] ->
        if i2 <> Z.zero then A.ELit (LInt (Z.div i1 i2))
        else
          Errors.raise_multispanned_error "division by zero at runtime"
            [
              (Some "The division operator:", Pos.get_position op);
              (Some "The null denominator:", Pos.get_position (List.nth args 2));
            ]
    | A.Binop (A.Add KRat), [ ELit (LRat i1); ELit (LRat i2) ] -> A.ELit (LRat (Q.add i1 i2))
    | A.Binop (A.Sub KRat), [ ELit (LRat i1); ELit (LRat i2) ] -> A.ELit (LRat (Q.sub i1 i2))
    | A.Binop (A.Mult KRat), [ ELit (LRat i1); ELit (LRat i2) ] -> A.ELit (LRat (Q.mul i1 i2))
    | A.Binop (A.Div KRat), [ ELit (LRat i1); ELit (LRat i2) ] ->
        if i2 <> Q.zero then A.ELit (LRat (Q.div i1 i2))
        else
          Errors.raise_multispanned_error "division by zero at runtime"
            [
              (Some "The division operator:", Pos.get_position op);
              (Some "The null denominator:", Pos.get_position (List.nth args 2));
            ]
    | A.Binop (A.Add KMoney), [ ELit (LMoney i1); ELit (LMoney i2) ] ->
        A.ELit (LMoney (Z.add i1 i2))
    | A.Binop (A.Sub KMoney), [ ELit (LMoney i1); ELit (LMoney i2) ] ->
        A.ELit (LMoney (Z.sub i1 i2))
    | A.Binop (A.Mult KMoney), [ ELit (LMoney i1); ELit (LRat i2) ] ->
        let rat_result = Q.mul (Q.of_bigint i1) i2 in
        let res, remainder = Z.div_rem (Q.num rat_result) (Q.den rat_result) in
        (* we perform nearest rounding when multiplying an amount of money by a decimal !*)
        let out =
          if Z.(of_int 2 * remainder >= Q.den rat_result) then Z.add res (Z.of_int 1) else res
        in
        A.ELit (LMoney out)
    | A.Binop (A.Div KMoney), [ ELit (LMoney i1); ELit (LMoney i2) ] ->
        if i2 <> Z.zero then A.ELit (LRat (Q.div (Q.of_bigint i1) (Q.of_bigint i2)))
        else
          Errors.raise_multispanned_error "division by zero at runtime"
            [
              (Some "The division operator:", Pos.get_position op);
              (Some "The null denominator:", Pos.get_position (List.nth args 2));
            ]
    | A.Binop (A.Add KDuration), [ ELit (LDuration i1); ELit (LDuration i2) ] ->
        A.ELit (LDuration (Z.( + ) i1 i2))
    | A.Binop (A.Sub KDuration), [ ELit (LDuration i1); ELit (LDuration i2) ] ->
        A.ELit (LDuration (Z.( - ) i1 i2))
    | A.Binop (A.Sub KDate), [ ELit (LDate i1); ELit (LDate i2) ] ->
        A.ELit (LDuration (Z.of_int (ODuration.To.day (ODate.Unix.between i2 i1))))
    | A.Binop (A.Add KDate), [ ELit (LDate i1); ELit (LDuration i2) ] ->
        A.ELit (LDate (ODate.Unix.advance_by_days i1 (Z.to_int i2)))
    | A.Binop (A.Lt KInt), [ ELit (LInt i1); ELit (LInt i2) ] -> A.ELit (LBool (i1 < i2))
    | A.Binop (A.Lte KInt), [ ELit (LInt i1); ELit (LInt i2) ] -> A.ELit (LBool (i1 <= i2))
    | A.Binop (A.Gt KInt), [ ELit (LInt i1); ELit (LInt i2) ] -> A.ELit (LBool (i1 > i2))
    | A.Binop (A.Gte KInt), [ ELit (LInt i1); ELit (LInt i2) ] -> A.ELit (LBool (i1 >= i2))
    | A.Binop (A.Lt KRat), [ ELit (LRat i1); ELit (LRat i2) ] -> A.ELit (LBool Q.(i1 < i2))
    | A.Binop (A.Lte KRat), [ ELit (LRat i1); ELit (LRat i2) ] -> A.ELit (LBool Q.(i1 <= i2))
    | A.Binop (A.Gt KRat), [ ELit (LRat i1); ELit (LRat i2) ] -> A.ELit (LBool Q.(i1 > i2))
    | A.Binop (A.Gte KRat), [ ELit (LRat i1); ELit (LRat i2) ] -> A.ELit (LBool Q.(i1 >= i2))
    | A.Binop (A.Lt KMoney), [ ELit (LMoney i1); ELit (LMoney i2) ] -> A.ELit (LBool (i1 < i2))
    | A.Binop (A.Lte KMoney), [ ELit (LMoney i1); ELit (LMoney i2) ] -> A.ELit (LBool (i1 <= i2))
    | A.Binop (A.Gt KMoney), [ ELit (LMoney i1); ELit (LMoney i2) ] -> A.ELit (LBool (i1 > i2))
    | A.Binop (A.Gte KMoney), [ ELit (LMoney i1); ELit (LMoney i2) ] -> A.ELit (LBool (i1 >= i2))
    | A.Binop (A.Lt KDuration), [ ELit (LDuration i1); ELit (LDuration i2) ] ->
        A.ELit (LBool (i1 < i2))
    | A.Binop (A.Lte KDuration), [ ELit (LDuration i1); ELit (LDuration i2) ] ->
        A.ELit (LBool (i1 <= i2))
    | A.Binop (A.Gt KDuration), [ ELit (LDuration i1); ELit (LDuration i2) ] ->
        A.ELit (LBool (i1 > i2))
    | A.Binop (A.Gte KDuration), [ ELit (LDuration i1); ELit (LDuration i2) ] ->
        A.ELit (LBool (i1 >= i2))
    | A.Binop (A.Lt KDate), [ ELit (LDate i1); ELit (LDate i2) ] ->
        A.ELit (LBool (ODate.Unix.compare i1 i2 < 0))
    | A.Binop (A.Lte KDate), [ ELit (LDate i1); ELit (LDate i2) ] ->
        A.ELit (LBool (ODate.Unix.compare i1 i2 <= 0))
    | A.Binop (A.Gt KDate), [ ELit (LDate i1); ELit (LDate i2) ] ->
        A.ELit (LBool (ODate.Unix.compare i1 i2 > 0))
    | A.Binop (A.Gte KDate), [ ELit (LDate i1); ELit (LDate i2) ] ->
        A.ELit (LBool (ODate.Unix.compare i1 i2 >= 0))
    | A.Binop A.Eq, [ ELit (LDuration i1); ELit (LDuration i2) ] -> A.ELit (LBool (i1 = i2))
    | A.Binop A.Eq, [ ELit (LDate i1); ELit (LDate i2) ] ->
        A.ELit (LBool (ODate.Unix.compare i1 i2 = 0))
    | A.Binop A.Eq, [ ELit (LMoney i1); ELit (LMoney i2) ] -> A.ELit (LBool (i1 = i2))
    | A.Binop A.Eq, [ ELit (LRat i1); ELit (LRat i2) ] -> A.ELit (LBool Q.(i1 = i2))
    | A.Binop A.Eq, [ ELit (LInt i1); ELit (LInt i2) ] -> A.ELit (LBool (i1 = i2))
    | A.Binop A.Eq, [ ELit (LBool b1); ELit (LBool b2) ] -> A.ELit (LBool (b1 = b2))
    | A.Binop A.Eq, [ _; _ ] -> A.ELit (LBool false) (* comparing functions return false *)
    | A.Binop A.Neq, [ ELit (LDuration i1); ELit (LDuration i2) ] -> A.ELit (LBool (i1 <> i2))
    | A.Binop A.Neq, [ ELit (LDate i1); ELit (LDate i2) ] ->
        A.ELit (LBool (ODate.Unix.compare i1 i2 <> 0))
    | A.Binop A.Neq, [ ELit (LMoney i1); ELit (LMoney i2) ] -> A.ELit (LBool (i1 <> i2))
    | A.Binop A.Neq, [ ELit (LRat i1); ELit (LRat i2) ] -> A.ELit (LBool Q.(i1 <> i2))
    | A.Binop A.Neq, [ ELit (LInt i1); ELit (LInt i2) ] -> A.ELit (LBool (i1 <> i2))
    | A.Binop A.Neq, [ ELit (LBool b1); ELit (LBool b2) ] -> A.ELit (LBool (b1 <> b2))
    | A.Binop A.Neq, [ _; _ ] -> A.ELit (LBool true)
    | A.Binop _, ([ ELit LEmptyError; _ ] | [ _; ELit LEmptyError ]) -> A.ELit LEmptyError
    | A.Unop (A.Minus KInt), [ ELit (LInt i) ] -> A.ELit (LInt (Z.sub Z.zero i))
    | A.Unop (A.Minus KRat), [ ELit (LRat i) ] -> A.ELit (LRat (Q.sub Q.zero i))
    | A.Unop A.Not, [ ELit (LBool b) ] -> A.ELit (LBool (not b))
    | A.Unop A.ErrorOnEmpty, [ e' ] ->
        if e' = A.ELit LEmptyError then
          Errors.raise_spanned_error
            "This variable evaluated to an empty term (no rule that defined it applied in this \
             situation)"
            (Pos.get_position op)
        else e'
    | A.Unop _, [ ELit LEmptyError ] -> A.ELit LEmptyError
    | _ ->
        Errors.raise_multispanned_error
          "Operator applied to the wrong arguments\n(should nothappen if the term was well-typed)"
          ( [ (Some "Operator:", Pos.get_position op) ]
          @ List.mapi
              (fun i arg ->
                ( Some (Format.asprintf "Argument n°%d, value %a" (i + 1) Print.format_expr arg),
                  Pos.get_position arg ))
              args ) )
    op

let rec evaluate_expr (e : A.expr Pos.marked) : A.expr Pos.marked =
  match Pos.unmark e with
  | EVar _ ->
      Errors.raise_spanned_error
        "free variable found at evaluation (should not happen if term was well-typed"
        (Pos.get_position e)
  | EApp (e1, args) -> (
      let e1 = evaluate_expr e1 in
      let args = List.map evaluate_expr args in
      match Pos.unmark e1 with
      | EAbs (_, binder, _) ->
          if Bindlib.mbinder_arity binder = List.length args then
            evaluate_expr (Bindlib.msubst binder (Array.of_list (List.map Pos.unmark args)))
          else
            Errors.raise_spanned_error
              (Format.asprintf "wrong function call, expected %d arguments, got %d"
                 (Bindlib.mbinder_arity binder) (List.length args))
              (Pos.get_position e)
      | EOp op -> Pos.same_pos_as (Pos.unmark (evaluate_operator (Pos.same_pos_as op e1) args)) e
      | ELit LEmptyError -> Pos.same_pos_as (A.ELit LEmptyError) e
      | _ ->
          Errors.raise_spanned_error
            "function has not been reduced to a lambda at evaluation (should not happen if the \
             term was well-typed"
            (Pos.get_position e) )
  | EAbs _ | ELit _ | EOp _ -> e (* thse are values *)
  | ETuple es -> Pos.same_pos_as (A.ETuple (List.map (fun (e', i) -> (evaluate_expr e', i)) es)) e
  | ETupleAccess (e1, n, _) -> (
      let e1 = evaluate_expr e1 in
      match Pos.unmark e1 with
      | ETuple es -> (
          match List.nth_opt es n with
          | Some (e', _) -> e'
          | None ->
              Errors.raise_spanned_error
                (Format.asprintf
                   "the tuple has %d components but the %i-th element was requested (should not \
                    happen if the term was well-type)"
                   (List.length es) n)
                (Pos.get_position e1) )
      | _ ->
          Errors.raise_spanned_error
            (Format.asprintf
               "the expression should be a tuple with %d components but is not (should not happen \
                if the term was well-typed)"
               n)
            (Pos.get_position e1) )
  | EInj (e1, n, i, ts) ->
      let e1' = evaluate_expr e1 in
      Pos.same_pos_as (A.EInj (e1', n, i, ts)) e
  | EMatch (e1, es) -> (
      let e1 = evaluate_expr e1 in
      match Pos.unmark e1 with
      | A.EInj (e1, n, _, _) ->
          let es_n, _ =
            match List.nth_opt es n with
            | Some es_n -> es_n
            | None ->
                Errors.raise_spanned_error
                  "sum type index error (should not happend if the term was well-typed)"
                  (Pos.get_position e)
          in
          let new_e = Pos.same_pos_as (A.EApp (es_n, [ e1 ])) e in
          evaluate_expr new_e
      | A.ELit A.LEmptyError -> Pos.same_pos_as (A.ELit A.LEmptyError) e
      | _ ->
          Errors.raise_spanned_error
            "Expected a term having a sum type as an argument to a match (should not happend if \
             the term was well-typed"
            (Pos.get_position e1) )
  | EDefault (just, cons, subs) -> (
      let just = evaluate_expr just in
      match Pos.unmark just with
      | ELit LEmptyError -> Pos.same_pos_as (A.ELit LEmptyError) e
      | ELit (LBool true) -> (
          match evaluate_expr cons with
          | ELit LEmptyError, pos ->
              evaluate_expr
                (Pos.same_pos_as
                   (Ast.EDefault ((ELit (LBool false), pos), (Ast.ELit LEmptyError, pos), subs))
                   e)
          | e' -> e' )
      | ELit (LBool false) -> (
          let subs_orig = subs in
          let subs = List.map evaluate_expr subs in
          let empty_count = List.length (List.filter is_empty_error subs) in
          match List.length subs - empty_count with
          | 0 -> Pos.same_pos_as (A.ELit LEmptyError) e
          | 1 -> List.find (fun sub -> not (is_empty_error sub)) subs
          | _ ->
              Errors.raise_multispanned_error
                "There is a conflict between multiple rules for assigning the same variable."
                ( ( if Pos.get_position e = Pos.no_pos then []
                  else
                    [
                      ( Some "This rule is not triggered, so we consider rules of lower priority:",
                        Pos.get_position e );
                    ] )
                @ List.map
                    (fun (_, sub) -> (Some "This justification is true:", Pos.get_position sub))
                    (List.filter
                       (fun (sub, _) -> not (is_empty_error sub))
                       (List.map2 (fun x y -> (x, y)) subs subs_orig)) ) )
      | _ ->
          Errors.raise_spanned_error
            "Default justification has not been reduced to a boolean at evaluation (should not \
             happen if the term was well-typed"
            (Pos.get_position e) )
  | EIfThenElse (cond, et, ef) -> (
      match Pos.unmark (evaluate_expr cond) with
      | ELit (LBool true) -> evaluate_expr et
      | ELit (LBool false) -> evaluate_expr ef
      | _ ->
          Errors.raise_spanned_error
            "Expected a boolean literal for the result of this condition (should not happen if the \
             term was well-typed)"
            (Pos.get_position cond) )
  | EAssert e' -> (
      match Pos.unmark (evaluate_expr e') with
      | ELit (LBool true) -> Pos.same_pos_as (Ast.ELit LUnit) e'
      | ELit (LBool false) -> Errors.raise_spanned_error "Assertion failed" (Pos.get_position e')
      | _ ->
          Errors.raise_spanned_error
            "Expected a boolean literal for the result of this assertion (should not happen if the \
             term was well-typed)"
            (Pos.get_position e') )

let empty_thunked_term : Ast.expr Pos.marked =
  let silent = Ast.Var.make ("_", Pos.no_pos) in
  Bindlib.unbox
    (Ast.make_abs
       (Array.of_list [ silent ])
       (Bindlib.box (Ast.ELit Ast.LEmptyError, Pos.no_pos))
       Pos.no_pos
       [ (Ast.TLit Ast.TUnit, Pos.no_pos) ]
       Pos.no_pos)

let interpret_program (e : Ast.expr Pos.marked) : (Ast.Var.t * Ast.expr Pos.marked) list =
  match Pos.unmark (evaluate_expr e) with
  | Ast.EAbs (_, binder, taus) -> (
      let application_term = List.map (fun _ -> empty_thunked_term) taus in
      let to_interpret = (Ast.EApp (e, application_term), Pos.no_pos) in
      match Pos.unmark (evaluate_expr to_interpret) with
      | Ast.ETuple args ->
          let vars, _ = Bindlib.unmbind binder in
          List.map2 (fun (arg, _) var -> (var, arg)) args (Array.to_list vars)
      | _ ->
          Errors.raise_spanned_error "The interpretation of a program should always yield a tuple"
            (Pos.get_position e) )
  | _ ->
      Errors.raise_spanned_error
        "The interpreter can only interpret terms starting with functions having thunked arguments"
        (Pos.get_position e)
