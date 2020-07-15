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
      ((EIfThenElse (rec_helper e_if, rec_helper e_then, rec_helper e_else), pos), None)
  | Binop (op, e1, e2) ->
      let op_term = (Pos.same_pos_as (EOp (Binop (Pos.unmark op))) op, None) in
      let op_1 = ((EApp (op_term, rec_helper e1), pos), None) in
      ((EApp (op_1, rec_helper e2), pos), None)
  | Unop (op, e) ->
      let op_term = (Pos.same_pos_as (EOp (Unop (Pos.unmark op))) op, None) in
      ((EApp (op_term, rec_helper e), pos), None)
  | Literal l -> ((ELiteral l, pos), None)
  | Ident x -> (
      match Context.get_var_uid scope ctxt x with
      | None -> assert false
      | Some uid -> ((EVar uid, pos), None) )
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
              (* No redefinition : take the uid from the current scope *)
              let ident = subscope_ident y (Pos.unmark x) in
              match Context.get_var_uid scope ctxt ident with
              | None -> assert false
              | Some uid -> ((EVar uid, pos), None) )
          | Some uid when uid <> sub_uid -> (
              (* Redefinition of a var from another scope : uid from the current scope *)
              let ident = subscope_ident y (Pos.unmark x) in
              match Context.get_var_uid scope ctxt ident with
              | None -> assert false
              | Some uid -> ((EVar uid, pos), None) )
          | Some sub_uid -> (
              (* Redefinition of a var from the same scope, uid from the subscope *)
              match Context.get_var_uid sub_uid ctxt (Pos.unmark x) with
              | None -> assert false
              | Some uid -> ((EVar uid, pos), None) ) )
      | _ -> assert false )
  | _ -> assert false

let rec typing (ctxt : Context.context) (((t, pos), _) : Lambda.term) : Lambda.term * Lambda.typ =
  match t with
  | EVar uid ->
      let typ = match Context.get_uid_typ ctxt uid with None -> assert false | Some typ -> typ in
      let term = ((EVar uid, pos), Some typ) in
      (term, typ)
  | EFun (_binding, _t) -> assert false
  | EApp (t1, t2) -> (
      let t1, typ1 = typing ctxt t1 in
      let t2, typ2 = typing ctxt t2 in
      match typ1 with
      | TArrow (arg_typ, ret_typ) ->
          if arg_typ <> typ2 then assert false else (((EApp (t1, t2), pos), Some ret_typ), ret_typ)
      | TBool | TInt -> assert false )
  | EIfThenElse (t_if, t_then, t_else) ->
      let t_if, typ_if = typing ctxt t_if in
      let t_then, typ_then = typing ctxt t_then in
      let t_else, typ_else = typing ctxt t_else in
      if typ_if <> TBool then assert false
      else if typ_then <> typ_else then assert false
      else (((EIfThenElse (t_if, t_then, t_else), pos), Some typ_then), typ_then)
  | ELiteral l ->
      let typ = match l with Number _ | MoneyAmount _ | Date _ -> TInt | Bool _ -> TBool in
      (((t, pos), Some typ), typ)
  | EOp op ->
      let typ =
        match op with
        | Binop binop -> (
            match binop with
            | And | Or -> TArrow (TBool, TArrow (TBool, TBool))
            | Add | Sub | Mult | Div -> TArrow (TInt, TArrow (TInt, TInt))
            | Lt | Lte | Gt | Gte | Eq | Neq -> TArrow (TInt, TArrow (TInt, TBool)) )
        | Unop Minus -> TArrow (TInt, TInt)
        | Unop Not -> TArrow (TBool, TBool)
      in
      (((t, pos), Some typ), typ)
