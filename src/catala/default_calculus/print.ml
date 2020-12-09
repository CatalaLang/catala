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
  | TTuple ts ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " *@ ") format_typ)
        ts
  | TEnum ts ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " +@ ") format_typ)
        ts
  | TArrow (t1, t2) ->
      Format.fprintf fmt "@[<hov 2>%a →@ %a@]" format_typ_with_parens t1 format_typ t2

let format_lit (fmt : Format.formatter) (l : lit Pos.marked) : unit =
  match Pos.unmark l with
  | LBool b -> Format.fprintf fmt "%b" b
  | LInt i -> Format.fprintf fmt "%s" (Z.to_string i)
  | LEmptyError -> Format.fprintf fmt "∅"
  | LUnit -> Format.fprintf fmt "()"
  | LRat i ->
      let sign = Q.sign i in
      let n = Z.abs (Q.num i) in
      let d = Z.abs (Q.den i) in
      let int_part = Z.ediv n d in
      let n = ref (Z.erem n d) in
      let digits = ref [] in
      let leading_zeroes (digits : Z.t list) : int =
        match
          List.fold_right
            (fun digit num_leading_zeroes ->
              match num_leading_zeroes with
              | `End _ -> num_leading_zeroes
              | `Begin i -> if Z.(digit = zero) then `Begin (i + 1) else `End i)
            digits (`Begin 0)
        with
        | `End i -> i
        | `Begin i -> i
      in
      while
        !n <> Z.zero && List.length !digits - leading_zeroes !digits < !Utils.Cli.max_prec_digits
      do
        n := Z.mul !n (Z.of_int 10);
        digits := Z.ediv !n d :: !digits;
        n := Z.erem !n d
      done;
      Format.fprintf fmt "%s%a.%a%s"
        (if sign < 0 then "-" else "")
        Z.pp_print int_part
        (Format.pp_print_list
           ~pp_sep:(fun _fmt () -> ())
           (fun fmt digit -> Format.fprintf fmt "%a" Z.pp_print digit))
        (List.rev !digits)
        ( if List.length !digits - leading_zeroes !digits = !Utils.Cli.max_prec_digits then "…"
        else "" )
  | LMoney e -> Format.fprintf fmt "$%.2f" Q.(to_float (of_bigint e / of_int 100))

let format_op_kind (fmt : Format.formatter) (k : op_kind) =
  Format.fprintf fmt "%s" (match k with KInt -> "" | KRat -> "." | KMoney -> "$")

let format_binop (fmt : Format.formatter) (op : binop Pos.marked) : unit =
  match Pos.unmark op with
  | Add k -> Format.fprintf fmt "+%a" format_op_kind k
  | Sub k -> Format.fprintf fmt "-%a" format_op_kind k
  | Mult k -> Format.fprintf fmt "*%a" format_op_kind k
  | Div k -> Format.fprintf fmt "/%a" format_op_kind k
  | And -> Format.fprintf fmt "%s" "&&"
  | Or -> Format.fprintf fmt "%s" "||"
  | Eq -> Format.fprintf fmt "%s" "=="
  | Neq -> Format.fprintf fmt "%s" "!="
  | Lt _ -> Format.fprintf fmt "%s" "<"
  | Lte _ -> Format.fprintf fmt "%s" "<="
  | Gt _ -> Format.fprintf fmt "%s" ">"
  | Gte _ -> Format.fprintf fmt "%s" ">="

let format_unop (fmt : Format.formatter) (op : unop Pos.marked) : unit =
  Format.fprintf fmt "%s"
    (match Pos.unmark op with Minus _ -> "-" | Not -> "~" | ErrorOnEmpty -> "error_on_empty")

let needs_parens (e : expr Pos.marked) : bool =
  match Pos.unmark e with EAbs _ -> true | _ -> false

let format_var (fmt : Format.formatter) (v : Var.t) : unit =
  Format.fprintf fmt "%s" (Bindlib.name_of v)

let rec format_expr (fmt : Format.formatter) (e : expr Pos.marked) : unit =
  let format_with_parens (fmt : Format.formatter) (e : expr Pos.marked) =
    if needs_parens e then Format.fprintf fmt "(%a)" format_expr e
    else Format.fprintf fmt "%a" format_expr e
  in
  match Pos.unmark e with
  | EVar v -> Format.fprintf fmt "%a" format_var (Pos.unmark v)
  | ETuple es ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
           (fun fmt (e, struct_field) ->
             match struct_field with
             | Some struct_field ->
                 Format.fprintf fmt "@[<hov 2>\"%a\":@ %a@]" Uid.MarkedString.format_info
                   struct_field format_expr e
             | None -> Format.fprintf fmt "@[%a@]" format_expr e))
        es
  | ETupleAccess (e1, n, i) -> (
      match i with
      | None -> Format.fprintf fmt "%a.%d" format_expr e1 n
      | Some i -> Format.fprintf fmt "%a.\"%a\"" format_expr e1 Uid.MarkedString.format_info i )
  | EInj (e, _n, i, _ts) -> Format.fprintf fmt "%a %a" Uid.MarkedString.format_info i format_expr e
  | EMatch (e, es) ->
      Format.fprintf fmt "@[<hov 2>match %a with %a@]" format_expr e
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " |@ ")
           (fun fmt (e, c) ->
             Format.fprintf fmt "%a %a" Uid.MarkedString.format_info c format_expr e))
        es
  | ELit l -> Format.fprintf fmt "%a" format_lit (Pos.same_pos_as l e)
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
      Format.fprintf fmt "@[%a@ %a@ %a@]" format_with_parens arg1 format_binop (op, Pos.no_pos)
        format_with_parens arg2
  | EApp ((EOp (Unop op), _), [ arg1 ]) ->
      Format.fprintf fmt "@[%a@ %a@]" format_unop (op, Pos.no_pos) format_with_parens arg1
  | EApp (f, args) ->
      Format.fprintf fmt "@[%a@ %a@]" format_expr f
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ") format_with_parens)
        args
  | EIfThenElse (e1, e2, e3) ->
      Format.fprintf fmt "if@ @[<hov 2>%a@]@ then@ @[<hov 2>%a@]@ else@ @[<hov 2>%a@]" format_expr
        e1 format_expr e2 format_expr e3
  | EOp (Binop op) -> Format.fprintf fmt "%a" format_binop (op, Pos.no_pos)
  | EOp (Unop op) -> Format.fprintf fmt "%a" format_unop (op, Pos.no_pos)
  | EDefault (just, cons, subs) ->
      if List.length subs = 0 then
        Format.fprintf fmt "@[⟨%a ⊢ %a⟩@]" format_expr just format_expr cons
      else
        Format.fprintf fmt "@[<hov 2>⟨%a ⊢ %a |@ %a⟩@]" format_expr just format_expr cons
          (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") format_expr)
          subs
