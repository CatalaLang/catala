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

let needs_parens (e : expr Pos.marked) : bool =
  match Pos.unmark e with EAbs _ -> true | _ -> false

let format_var (fmt : Format.formatter) (v : Var.t) : unit =
  Format.fprintf fmt "%s_%d" (Bindlib.name_of v) (Bindlib.uid_of v)

let format_location (fmt : Format.formatter) (l : location) : unit =
  match l with
  | ScopeVar v -> Format.fprintf fmt "%a" ScopeVar.format_t (Pos.unmark v)
  | SubScopeVar (_, subindex, subvar) ->
      Format.fprintf fmt "%a.%a" SubScopeName.format_t (Pos.unmark subindex) ScopeVar.format_t
        (Pos.unmark subvar)

let typ_needs_parens (e : typ Pos.marked) : bool =
  match Pos.unmark e with TArrow _ -> true | _ -> false

let rec format_typ (fmt : Format.formatter) (typ : typ Pos.marked) : unit =
  let format_typ_with_parens (fmt : Format.formatter) (t : typ Pos.marked) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  match Pos.unmark typ with
  | TLit TUnit -> Format.fprintf fmt "unit"
  | TLit TBool -> Format.fprintf fmt "bool"
  | TLit TInt -> Format.fprintf fmt "int"
  | TLit TRat -> Format.fprintf fmt "dec"
  | TLit TMoney -> Format.fprintf fmt "money"
  | TStruct s -> Format.fprintf fmt "%a" Ast.StructName.format_t s
  | TEnum e -> Format.fprintf fmt "%a" Ast.EnumName.format_t e
  | TArrow (t1, t2) ->
      Format.fprintf fmt "@[<hov 2>%a →@ %a@]" format_typ_with_parens t1 format_typ t2

let rec format_expr (fmt : Format.formatter) (e : expr Pos.marked) : unit =
  let format_with_parens (fmt : Format.formatter) (e : expr Pos.marked) =
    if needs_parens e then Format.fprintf fmt "(%a)" format_expr e
    else Format.fprintf fmt "%a" format_expr e
  in
  match Pos.unmark e with
  | ELocation l -> Format.fprintf fmt "%a" format_location l
  | EVar v -> Format.fprintf fmt "%a" format_var (Pos.unmark v)
  | ELit l -> Format.fprintf fmt "%a" Dcalc.Print.format_lit (Pos.same_pos_as l e)
  | EStruct (name, fields) ->
      Format.fprintf fmt "@[%a @[<hov 2>{@ %a@ }@]@]" Ast.StructName.format_t name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (field_name, field_expr) ->
             Format.fprintf fmt "%a = %a" Ast.StructFieldName.format_t field_name format_expr
               field_expr))
        (Ast.StructFieldMap.bindings fields)
  | EStructAccess (e1, field, _) ->
      Format.fprintf fmt "%a.%a" format_expr e1 Ast.StructFieldName.format_t field
  | EEnumInj (e1, cons, _) ->
      Format.fprintf fmt "%a@ %a" Ast.EnumConstructor.format_t cons format_expr e1
  | EMatch (e1, _, cases) ->
      Format.fprintf fmt "@[<hov 2>@[match@ %a@ with@]@ %a@]" format_expr e1
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ |@ ")
           (fun fmt (cons_name, case_expr) ->
             Format.fprintf fmt "@[<hov 2>%a@ →@ %a@]" Ast.EnumConstructor.format_t cons_name
               format_expr case_expr))
        (Ast.EnumConstructorMap.bindings cases)
  | EApp ((EAbs (_, binder, taus), _), args) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      let xs_tau_arg = List.map2 (fun (x, tau) arg -> (x, tau, arg)) xs_tau args in
      Format.fprintf fmt "@[%a%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
           (fun fmt (x, tau, arg) ->
             Format.fprintf fmt "@[@[<hov 2>let@ %a@ :@ %a@ =@ %a@]@ in@\n@]" format_var x
               format_typ tau format_expr arg))
        xs_tau_arg format_expr body
  | EAbs (_, binder, taus) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      Format.fprintf fmt "@[<hov 2>λ@ %a@ →@ %a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
           (fun fmt (x, tau) -> Format.fprintf fmt "@[(%a:@ %a)@]" format_var x format_typ tau))
        xs_tau format_expr body
  | EApp ((EOp (Binop op), _), [ arg1; arg2 ]) ->
      Format.fprintf fmt "@[%a@ %a@ %a@]" format_with_parens arg1 Dcalc.Print.format_binop
        (op, Pos.no_pos) format_with_parens arg2
  | EApp ((EOp (Unop op), _), [ arg1 ]) ->
      Format.fprintf fmt "@[%a@ %a@]" Dcalc.Print.format_unop (op, Pos.no_pos) format_with_parens
        arg1
  | EApp (f, args) ->
      Format.fprintf fmt "@[%a@ %a@]" format_expr f
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ") format_with_parens)
        args
  | EIfThenElse (e1, e2, e3) ->
      Format.fprintf fmt "if@ @[<hov 2>%a@]@ then@ @[<hov 2>%a@]@ else@ @[<hov 2>%a@]" format_expr
        e1 format_expr e2 format_expr e3
  | EOp (Binop op) -> Format.fprintf fmt "%a" Dcalc.Print.format_binop (op, Pos.no_pos)
  | EOp (Unop op) -> Format.fprintf fmt "%a" Dcalc.Print.format_unop (op, Pos.no_pos)
  | EDefault (just, cons, subs) ->
      if List.length subs = 0 then
        Format.fprintf fmt "@[⟨%a ⊢ %a⟩@]" format_expr just format_expr cons
      else
        Format.fprintf fmt "@[<hov 2>⟨%a ⊢ %a |@ %a⟩@]" format_expr just format_expr cons
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") format_expr)
          subs
