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
open Ast

let rec format_typ (fmt : Format.formatter) (typ : typ Pos.marked) : unit =
  match Pos.unmark typ with
  | TUnit -> Format.fprintf fmt "unit"
  | TBool -> Format.fprintf fmt "bool"
  | TInt -> Format.fprintf fmt "int"
  | TTuple ts ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") format_typ)
        ts
  | TArrow (t1, t2) -> Format.fprintf fmt "%a -> %a" format_typ t1 format_typ t2

let format_lit (fmt : Format.formatter) (l : lit Pos.marked) : unit =
  match Pos.unmark l with
  | LBool b -> Format.fprintf fmt "%b" b
  | LInt i -> Format.fprintf fmt "%s" (Int64.to_string i)
  | LEmptyError -> Format.fprintf fmt "∅"

let format_binop (fmt : Format.formatter) (op : binop Pos.marked) : unit =
  Format.fprintf fmt "%s"
    ( match Pos.unmark op with
    | Add -> "+"
    | Sub -> "-"
    | Mult -> "*"
    | Div -> "/"
    | And -> "&&"
    | Or -> "||"
    | Eq -> "=="
    | Neq -> "!="
    | Lt -> "<"
    | Lte -> "<="
    | Gt -> ">"
    | Gte -> ">=" )

let format_unop (fmt : Format.formatter) (op : unop Pos.marked) : unit =
  Format.fprintf fmt "%s" (match Pos.unmark op with Minus -> "-" | Not -> "~")

let rec format_expr (fmt : Format.formatter) (e : expr Pos.marked) : unit =
  match Pos.unmark e with
  | EVar v -> Format.fprintf fmt "%s" (Bindlib.name_of v)
  | ETuple es ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",") format_expr)
        es
  | ETupleAccess (e1, n) -> Format.fprintf fmt "%a.%d" format_expr e1 n
  | ELit l -> Format.fprintf fmt "%a" format_lit (Pos.same_pos_as l e)
  | EAbs (_, binder, taus) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      Format.fprintf fmt "λ %a -> @\n@[<h 2>  %a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
           (fun fmt (x, tau) -> Format.fprintf fmt "(%s: %a)" (Bindlib.name_of x) format_typ tau))
        xs_tau format_expr body
  | EApp ((EOp (Binop op), _), [ arg1; arg2 ]) ->
      Format.fprintf fmt "@[%a %a %a@]" format_expr arg1 format_binop (op, Pos.no_pos) format_expr
        arg2
  | EApp ((EOp (Unop op), _), [ arg1 ]) ->
      Format.fprintf fmt "@[%a %a@]" format_unop (op, Pos.no_pos) format_expr arg1
  | EApp (f, args) ->
      Format.fprintf fmt "@[%a %a@]" format_expr f
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") format_expr)
        args
  | EIfThenElse (e1, e2, e3) ->
      Format.fprintf fmt "if @[<h 2>%a@] then @[<h 2>%a@] else @[<h 2>%a@]" format_expr e1
        format_expr e2 format_expr e3
  | EOp (Binop op) -> Format.fprintf fmt "%a" format_binop (op, Pos.no_pos)
  | EOp (Unop op) -> Format.fprintf fmt "%a" format_unop (op, Pos.no_pos)
  | EDefault (just, cons, subs) ->
      Format.fprintf fmt "⟨ %a ⊢ %a | %a 〉" format_expr just format_expr cons
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") format_expr)
        subs
