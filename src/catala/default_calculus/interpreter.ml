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
module A = Ast

let is_empty_error (e : A.expr Pos.marked) : bool =
  match Pos.unmark e with ELit LEmptyError -> true | _ -> false

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
            evaluate_expr (Bindlib.msubst binder (Array.of_list args))
          else
            Errors.raise_spanned_error
              (Format.asprintf "wrong function call, expected %d arguments, got %d"
                 (Bindlib.mbinder_arity binder) (List.length args))
              (Pos.get_position e)
      | ELit LEmptyError -> Pos.same_pos_as (A.ELit LEmptyError) e
      | _ ->
          Errors.raise_spanned_error
            "function has not been reduced to a lambda at evaluation (should not happen if the \
             term was well-typed"
            (Pos.get_position e) )
  | EAbs _ | ELit _ -> e (* thse are values *)
  | ETuple es -> Pos.same_pos_as (A.ETuple (List.map evaluate_expr es)) e
  | ETupleAccess (e1, n) -> (
      let e1 = evaluate_expr e1 in
      match Pos.unmark e1 with
      | ETuple es -> (
          match List.nth_opt es n with
          | Some e' -> e'
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
  | EDefault (just, cons, subs) -> (
      let just = evaluate_expr just in
      match Pos.unmark just with
      | ELit LEmptyError -> Pos.same_pos_as (A.ELit LEmptyError) e
      | ELit LTrue -> evaluate_expr cons
      | ELit LFalse -> (
          let subs = List.map evaluate_expr subs in
          let empty_count = List.length (List.filter is_empty_error subs) in
          match List.length subs - empty_count with
          | 0 -> Pos.same_pos_as (A.ELit LEmptyError) e
          | 1 -> List.find (fun sub -> not (is_empty_error sub)) subs
          | _ ->
              Errors.raise_multispanned_error
                "there is a conflict between multiple rules for assigning a single value."
                ( [
                    ( Some "This rule is not triggered, so we consider rules of lower priority:",
                      Pos.get_position e );
                  ]
                @ List.map
                    (fun sub ->
                      ( Some "This value is available because the justification of its rule is true:",
                        Pos.get_position sub ))
                    (List.filter (fun sub -> not (is_empty_error sub)) subs) ) )
      | _ ->
          Errors.raise_spanned_error
            "Default justification has not been reduced to a boolean at evaluation (should not \
             happen if the term was well-typed"
            (Pos.get_position e) )
  | EIfThenElse (cond, et, ef) -> (
      match Pos.unmark (evaluate_expr cond) with
      | ELit LTrue -> evaluate_expr et
      | ELit LFalse -> evaluate_expr ef
      | _ ->
          Errors.raise_spanned_error
            "expected a boolean literal for the result of this condition (should not happen if the \
             term was well-typed)"
            (Pos.get_position cond) )
