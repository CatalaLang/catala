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

(** Checks that a term is well typed and annotate it *)
let rec type_term (ctxt : Name_resolution.context) (((t, pos), _) : Lambda_ast.term) :
    Lambda_ast.term =
  match t with
  | EVar uid ->
      let typ = Name_resolution.get_uid_typ ctxt uid in
      ((EVar uid, pos), typ)
  | ELocalVar _ -> assert false
  | EFun (bindings, body) ->
      (* Note that given the context formation process, the binder will already be present in the
         context (since we are working with uids), however it's added there for the sake of clarity *)
      let ctxt_data =
        List.fold_left
          (fun data (uid, arg_typ) ->
            let uid_data : Name_resolution.uid_data =
              { uid_typ = arg_typ; uid_sort = Name_resolution.IdBinder }
            in
            Uid.UidMap.add uid uid_data data)
          ctxt.data bindings
      in

      let body = typing { ctxt with data = ctxt_data } body in
      let ret_typ = Lambda_ast.get_typ body in
      let rec build_typ = function
        | [] -> ret_typ
        | (_, arg_t) :: args -> TArrow (arg_t, build_typ args)
      in
      let fun_typ = build_typ bindings in
      ((EFun (bindings, body), pos), fun_typ)
  | EApp (f, args) ->
      let f = typing ctxt f in
      let f_typ = Lambda_ast.get_typ f in
      let args = List.map (typing ctxt) args in
      let args_typ = List.map Lambda_ast.get_typ args in
      let rec check_arrow_typ f_typ args_typ =
        match (f_typ, args_typ) with
        | typ, [] -> typ
        | TArrow (arg_typ, ret_typ), fst_typ :: typs ->
            if arg_typ = fst_typ then check_arrow_typ ret_typ typs else assert false
        | _ -> assert false
      in
      let ret_typ = check_arrow_typ f_typ args_typ in
      ((EApp (f, args), pos), ret_typ)
  | EIfThenElse (t_if, t_then, t_else) ->
      let t_if = typing ctxt t_if in
      let typ_if = Lambda_ast.get_typ t_if in
      let t_then = typing ctxt t_then in
      let typ_then = Lambda_ast.get_typ t_then in
      let t_else = typing ctxt t_else in
      let typ_else = Lambda_ast.get_typ t_else in
      if typ_if <> TBool then assert false
      else if typ_then <> typ_else then assert false
      else ((EIfThenElse (t_if, t_then, t_else), pos), typ_then)
  | EInt _ | EDec _ -> ((t, pos), TInt)
  | EBool _ -> ((t, pos), TBool)
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
      ((t, pos), typ)
  | EDefault t ->
      let defaults =
        IntMap.map
          (fun (just, cons) ->
            let just = typing ctxt just in
            if Lambda_ast.get_typ just <> TBool then
              let cons = typing ctxt cons in
              (just, cons)
            else assert false)
          t.defaults
      in
      let typ_cons = IntMap.choose defaults |> snd |> snd |> Lambda_ast.get_typ in
      IntMap.iter
        (fun _ (_, cons) -> if Lambda_ast.get_typ cons <> typ_cons then assert false else ())
        defaults;
      ((EDefault { t with defaults }, pos), typ_cons)
