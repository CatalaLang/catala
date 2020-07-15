(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Ast
open Lambda

exception TypingError of string

let subscope_ident (y : string) (x : string) : string = y ^ "::" ^ x

(** The optional argument subdef allows to choose between differents uids in case the expression is
    a redefinition of a subvariable *)
let rec expr_to_lambda ?(subdef : uid option) (scope : Context.uid) (ctxt : Context.context)
    ((expr, pos) : Ast.expression Pos.marked) : Lambda.term =
  let rec_helper = expr_to_lambda ?subdef scope ctxt in
  match expr with
  | IfThenElse (e_if, e_then, e_else) ->
      let l_if, if_typ = rec_helper e_if in
      let l_then, then_typ = rec_helper e_then in
      let l_else, else_typ = rec_helper e_else in
      if if_typ <> TBool then raise (TypingError "should be booleans")
      else if then_typ <> else_typ then
        raise (TypingError "then and else branch should have the same type")
      else ((EIfThenElse ((l_if, if_typ), (l_then, then_typ), (l_else, else_typ)), pos), then_typ)
  | Binop (op, e1, e2) ->
      let l1, typ1 = rec_helper e1 in
      let l2, typ2 = rec_helper e2 in
      let final_typ =
        match Pos.unmark op with
        | And | Or ->
            if typ1 <> TBool then raise (TypingError "Operand should be booleans")
            else if typ2 <> TBool then raise (TypingError "Operand should be booleans")
            else TBool
        | Add | Sub | Mult | Div ->
            if typ1 <> TInt then raise (TypingError "should be int")
            else if typ2 <> TInt then raise (TypingError "should be int")
            else TInt
        | Lt | Lte | Gt | Gte ->
            if typ1 <> TInt then raise (TypingError "should be int")
            else if typ2 <> TInt then raise (TypingError "should be int")
            else TBool
        | Eq | Neq -> if typ1 <> typ2 then raise (TypingError "should have same types") else TBool
      in
      let op_untyped_term = EOp (Binop (Pos.unmark op)) in
      let op_term = (Pos.same_pos_as op_untyped_term op, TArrow (typ1, TArrow (typ1, final_typ))) in
      ((EApp (op_term, [ (l1, typ1); (l2, typ2) ]), pos), final_typ)
  | Unop (op, e) ->
      let l, typ = rec_helper e in
      let () =
        match Pos.unmark op with
        | Minus ->
            if typ <> TInt then raise (TypingError "Neg operator expects an integer argument")
            else ()
        | Not ->
            if typ <> TBool then raise (TypingError "Not operator expects a boolean argument")
            else ()
      in
      let op_untyped_term = EOp (Unop (Pos.unmark op)) in
      let op_term = (Pos.same_pos_as op_untyped_term op, TArrow (typ, typ)) in
      ((EApp (op_term, [ (l, typ) ]), pos), typ)
  | Literal l ->
      let typ = match l with Number _ | MoneyAmount _ | Date _ -> TInt | Bool _ -> TBool in
      ((ELiteral l, pos), typ)
  | Ident x -> (
      match Context.get_var_uid scope ctxt x with
      | None -> assert false
      | Some (uid, data) ->
          let untyped_term = match subdef with Some _ -> ECallerVar uid | None -> EVar uid in
          ((untyped_term, pos), data.uid_typ) )
  | Dotted (e, x) -> (
      (* For now we only accept dotted identifiers of the type y.x where y is a sub-scope *)
      match Pos.unmark e with
      | Ident y -> (
          let sub_uid =
            match Context.get_subscope_uid scope ctxt y with
            | None -> assert false
            | Some uid -> uid
          in
          match subdef with
          | None -> (
              (* No redefinition : take the uid from the current scope and it becomes an EVar *)
              let ident = subscope_ident y (Pos.unmark x) in
              match Context.get_var_uid scope ctxt ident with
              | None -> assert false
              | Some (uid, data) -> ((EVar uid, pos), data.uid_typ) )
          | Some uid when uid <> sub_uid -> (
              (* Redefinition of a var from another scope : uid from the current scope -> ECallerVar *)
              let ident = subscope_ident y (Pos.unmark x) in
              match Context.get_var_uid scope ctxt ident with
              | None -> assert false
              | Some (uid, data) -> ((ECallerVar uid, pos), data.uid_typ) )
          | Some sub_uid -> (
              (* Redefinition of a var from the same scope, uid from the subscope -> EVar *)
              match Context.get_var_uid sub_uid ctxt (Pos.unmark x) with
              | None -> assert false
              | Some (uid, data) -> ((EVar uid, pos), data.uid_typ) ) )
      | _ -> assert false )
  | _ -> assert false
