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
let rec type_term (ctxt : Name_resolution.context) (local_ctx : Lambda_ast.typ Uid.LocalVarMap.t)
    (((t, pos), _) : Lambda_ast.term) : Lambda_ast.term =
  match t with
  | EVar (s, uid) ->
      (* so here we can ignore the subscope uid because the uid of the variable already corresponds
         to the uid of the variable in its original scope *)
      let typ = Name_resolution.get_var_typ ctxt uid in
      ((EVar (s, uid), pos), typ)
  | ELocalVar uid ->
      let typ = Uid.LocalVarMap.find uid local_ctx in
      ((ELocalVar uid, pos), typ)
  | EFun (bindings, body) ->
      let local_ctx =
        List.fold_left
          (fun local_ctx (binding, ty) -> Uid.LocalVarMap.add binding ty local_ctx)
          local_ctx bindings
      in
      let body = type_term ctxt local_ctx body in
      let ret_typ = Lambda_ast.get_typ body in
      let rec build_typ = function
        | [] -> ret_typ
        | (_, arg_t) :: args -> TArrow (arg_t, build_typ args)
      in
      let fun_typ = build_typ bindings in
      ((EFun (bindings, body), pos), fun_typ)
  | EApp (f, args) ->
      let f = type_term ctxt local_ctx f in
      let f_typ = Lambda_ast.get_typ f in
      let args = List.map (type_term ctxt local_ctx) args in
      let args_typ =
        List.map (fun arg -> (Lambda_ast.get_typ arg, Pos.get_position (fst arg))) args
      in
      let rec check_arrow_typ f_typ args_typ =
        match (f_typ, args_typ) with
        | typ, [] -> typ
        | Lambda_ast.TArrow (arg_typ, ret_typ), fst_typ :: typs ->
            let fst_typ_s = Pos.unmark fst_typ in
            if arg_typ = fst_typ_s then check_arrow_typ ret_typ typs
            else
              Errors.raise_multispanned_error "error when comparing types of function arguments"
                [
                  ( Some (Printf.sprintf "expected type %s" (Format_lambda.format_typ f_typ)),
                    Pos.get_position (fst f) );
                  ( Some (Printf.sprintf "got type %s" (Format_lambda.format_typ fst_typ_s)),
                    Pos.get_position fst_typ );
                ]
        | _ ->
            Errors.raise_multispanned_error "wrong number of arguments for function call"
              [
                ( Some (Printf.sprintf "expected type %s" (Format_lambda.format_typ f_typ)),
                  Pos.get_position (fst f) );
                ( Some
                    (Printf.sprintf "got type %s"
                       (String.concat " -> "
                          (List.map (fun (ty, _) -> Format_lambda.format_typ ty) args_typ))),
                  Pos.get_position (List.hd args_typ) );
              ]
      in
      let ret_typ = check_arrow_typ f_typ args_typ in
      ((EApp (f, args), pos), ret_typ)
  | EIfThenElse (t_if, t_then, t_else) ->
      let t_if = type_term ctxt local_ctx t_if in
      let typ_if = Lambda_ast.get_typ t_if in
      let t_then = type_term ctxt local_ctx t_then in
      let typ_then = Lambda_ast.get_typ t_then in
      let t_else = type_term ctxt local_ctx t_else in
      let typ_else = Lambda_ast.get_typ t_else in
      if typ_if <> TBool then
        Errors.raise_spanned_error
          (Format.sprintf "expecting type bool, got type %s" (Format_lambda.format_typ typ_if))
          (Pos.get_position (fst t_if))
      else if typ_then <> typ_else then
        Errors.raise_multispanned_error
          "expecting same types for the true and false branches of the conditional"
          [
            ( Some (Format.sprintf "the true branch has type %s" (Format_lambda.format_typ typ_then)),
              Pos.get_position (fst t_then) );
            ( Some
                (Format.sprintf "the false branch has type %s" (Format_lambda.format_typ typ_else)),
              Pos.get_position (fst t_else) );
          ]
      else ((EIfThenElse (t_if, t_then, t_else), pos), typ_then)
  | EInt _ | EDec _ -> ((t, pos), TInt)
  | EBool _ -> ((t, pos), TBool)
  | EOp op ->
      let typ =
        match op with
        | Binop binop -> (
            match binop with
            | And | Or -> Lambda_ast.TArrow (TBool, TArrow (TBool, TBool))
            | Add | Sub | Mult | Div -> TArrow (TInt, TArrow (TInt, TInt))
            | Lt | Lte | Gt | Gte | Eq | Neq -> TArrow (TInt, TArrow (TInt, TBool)) )
        | Unop Minus -> TArrow (TInt, TInt)
        | Unop Not -> TArrow (TBool, TBool)
      in
      ((t, pos), typ)
  | EDefault t ->
      let defaults =
        List.map
          (fun (just, cons) ->
            let just_t = type_term ctxt local_ctx just in
            if Lambda_ast.get_typ just_t <> TBool then
              let cons = type_term ctxt local_ctx cons in
              (just_t, cons)
            else
              Errors.raise_spanned_error
                (Format.sprintf "expected type of default condition to be bool, got %s"
                   (Format_lambda.format_typ (Lambda_ast.get_typ just)))
                (Pos.get_position (fst just)))
          t.defaults
      in
      let typ_cons = List.hd defaults |> snd |> snd in
      List.iter
        (fun (_, cons) ->
          if Lambda_ast.get_typ cons <> typ_cons then
            Errors.raise_spanned_error
              (Format.sprintf "expected default condition to be of type %s, got type %s"
                 (Format_lambda.format_typ (Lambda_ast.get_typ cons))
                 (Format_lambda.format_typ typ_cons))
              (Pos.get_position (fst cons))
          else ())
        defaults;
      ((EDefault { t with defaults }, pos), typ_cons)
