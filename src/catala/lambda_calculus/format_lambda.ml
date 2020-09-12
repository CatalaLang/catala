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

module UidMap = Uid.UidMap
module IdentMap = Map.Make (String)

(** Print the context in a readable manner *)
let print_context (ctxt : Name_resolution.context) : string =
  let rec typ_to_string = function
    | Lambda_ast.TBool -> "bool"
    | Lambda_ast.TInt -> "num"
    | Lambda_ast.TDummy -> "(.)"
    | Lambda_ast.TArrow (t1, t2) ->
        Printf.sprintf "%s -> (%s)" (typ_to_string t1) (typ_to_string t2)
  in
  let print_var ((var_id, var_uid) : Uid.ident * Uid.t) : string =
    let data = UidMap.find var_uid ctxt.data in
    let info =
      match data.uid_sort with
      | IdScope -> "\tscope"
      | IdScopeVar None -> Printf.sprintf "\ttyp : %s\tvar" (typ_to_string data.uid_typ)
      | IdScopeVar (Some _) -> Printf.sprintf "\ttyp : %s\tfun" (typ_to_string data.uid_typ)
      | IdSubScope uid -> Printf.sprintf "\tsubscope : %d" uid
      | IdSubScopeVar (var_uid, sub_scope_uid) ->
          Printf.sprintf "\ttype : %s\tsubvar(%d, scope %d)" (typ_to_string data.uid_typ) var_uid
            sub_scope_uid
      | IdBinder -> Printf.sprintf "\ttyp : %s\tbinder" (typ_to_string data.uid_typ)
    in
    Printf.sprintf "%s (uid : %n)%s\n" var_id var_uid info
  in
  let print_scope ((scope_ident, scope_uid) : Uid.ident * Uid.t) : string =
    Printf.sprintf "Scope %s (uid : %n):\n" scope_ident scope_uid
    ^ ( (UidMap.find scope_uid ctxt.scopes).var_id_to_uid |> IdentMap.bindings |> List.map print_var
      |> String.concat "" )
    ^ Printf.sprintf "\n"
  in
  ctxt.scope_id_to_uid |> IdentMap.bindings |> List.map print_scope |> String.concat ""

(* Printing functions for Lambda_ast.term *)

(** Operator printer *)
let print_op (op : Lambda_ast.op) : string =
  match op with
  | Binop binop -> (
      match binop with
      | And -> "and"
      | Or -> "or"
      | Add -> "+"
      | Sub -> "-"
      | Mult -> "*"
      | Div -> "/"
      | Lt -> "<"
      | Lte -> "<="
      | Gt -> ">"
      | Gte -> ">="
      | Eq -> "="
      | Neq -> "!=" )
  | Unop Not -> "not"
  | Unop Minus -> "-"

(** Print Lambda_ast.term *)
let rec print_term (((t, _), _) : Lambda_ast.term) : string =
  match t with
  | EVar uid -> Printf.sprintf "%s(%d)" (Uid.get_ident uid) uid
  | EFun (binders, body) ->
      let sbody = print_term body in
      Printf.sprintf "fun %s -> %s"
        (binders |> List.map (fun (x, _) -> Printf.sprintf "%d" x) |> String.concat " ")
        sbody
  | EApp (f, args) ->
      Printf.sprintf "(%s) [%s]" (print_term f) (args |> List.map print_term |> String.concat ";")
  | EIfThenElse (tif, tthen, telse) ->
      Printf.sprintf "IF %s THEN %s ELSE %s" (print_term tif) (print_term tthen) (print_term telse)
  | EInt i -> Printf.sprintf "%d" i
  | EBool b -> if b then "true" else "false"
  | EDec (i, f) -> Printf.sprintf "%d.%d" i f
  | EOp op -> print_op op
  | EDefault t -> print_default_term t

and print_default_term (term : Lambda_ast.default_term) : string =
  term.defaults |> Lambda_ast.IntMap.bindings
  |> List.map (fun (_, (cond, body)) ->
         Printf.sprintf "\t%s => %s" (print_term cond) (print_term body))
  |> String.concat "\n"
