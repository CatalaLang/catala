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

module IdentMap = Map.Make (String)

(* Printing functions for Lambda_ast.term *)

let rec format_typ (ty : Lambda_ast.typ) : string =
  match ty with
  | TBool -> "bool"
  | TInt -> "int"
  | TArrow (t1, t2) -> Format.sprintf "(%s) -> (%s)" (format_typ t1) (format_typ t2)
  | TDummy -> "??"

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
  | EVar (s, uid) ->
      Printf.sprintf "%s%s"
        (match s with None -> "" | Some s -> Uid.SubScope.format_t s ^ ".")
        (Uid.Var.format_t uid)
  | ELocalVar uid -> Uid.LocalVar.format_t uid
  | EFun (binders, body) ->
      let sbody = print_term body in
      Printf.sprintf "fun %s -> %s"
        (binders |> List.map (fun (uid, _) -> Uid.LocalVar.format_t uid) |> String.concat " ")
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
  term.defaults
  |> List.map (fun (cond, body) -> Printf.sprintf "\t%s => %s" (print_term cond) (print_term body))
  |> String.concat "\n"
