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

open Utils
open Ast

let format_lit (fmt : Format.formatter) (l : lit Pos.marked) : unit =
  Dcalc.Print.format_lit fmt
    (Pos.same_pos_as
       ( match Pos.unmark l with
       | LBool b -> Dcalc.Ast.LBool b
       | LInt i -> Dcalc.Ast.LInt i
       | LUnit -> Dcalc.Ast.LUnit
       | LRat i -> Dcalc.Ast.LRat i
       | LMoney e -> Dcalc.Ast.LMoney e
       | LDate d -> Dcalc.Ast.LDate d
       | LDuration d -> Dcalc.Ast.LDuration d )
       l)

let format_op_kind (fmt : Format.formatter) (k : Dcalc.Ast.op_kind) =
  Format.fprintf fmt "%s"
    (match k with KInt -> "" | KRat -> "." | KMoney -> "$" | KDate -> "@" | KDuration -> "^")

let format_log_entry (fmt : Format.formatter) (entry : Dcalc.Ast.log_entry) : unit =
  match entry with
  | VarDef -> Format.fprintf fmt ":="
  | BeginCall -> Format.fprintf fmt "→ "
  | EndCall -> Format.fprintf fmt "%s" "← "
  | PosRecordIfTrueBool -> Format.fprintf fmt "☛ "

let format_binop (fmt : Format.formatter) (op : Dcalc.Ast.binop Pos.marked) : unit =
  match Pos.unmark op with
  | Add k -> Format.fprintf fmt "+%a" format_op_kind k
  | Sub k -> Format.fprintf fmt "-%a" format_op_kind k
  | Mult k -> Format.fprintf fmt "*%a" format_op_kind k
  | Div k -> Format.fprintf fmt "/%a" format_op_kind k
  | And -> Format.fprintf fmt "%s" "&&"
  | Or -> Format.fprintf fmt "%s" "||"
  | Eq -> Format.fprintf fmt "%s" "="
  | Neq -> Format.fprintf fmt "%s" "!="
  | Lt k -> Format.fprintf fmt "%s%a" "<" format_op_kind k
  | Lte k -> Format.fprintf fmt "%s%a" "<=" format_op_kind k
  | Gt k -> Format.fprintf fmt "%s%a" ">" format_op_kind k
  | Gte k -> Format.fprintf fmt "%s%a" ">=" format_op_kind k
  | Map -> Format.fprintf fmt "List.map"
  | Filter -> Format.fprintf fmt "List.filter"

let format_ternop (fmt : Format.formatter) (op : Dcalc.Ast.ternop Pos.marked) : unit =
  match Pos.unmark op with Fold -> Format.fprintf fmt "List.fold_left"

let format_unop (fmt : Format.formatter) (op : Dcalc.Ast.unop Pos.marked) : unit =
  Format.fprintf fmt "%s"
    ( match Pos.unmark op with
    | Minus _ -> "-"
    | Not -> "~"
    | ErrorOnEmpty -> "error_empty"
    | Log (entry, infos) ->
        Format.asprintf "@[<hov 2>log@ \"%a|%a\"@]" format_log_entry entry
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ".")
             (fun fmt info -> Utils.Uid.MarkedString.format_info fmt info))
          infos
    | Length -> "length"
    | IntToRat -> "int_to_rat"
    | GetDay -> "get_day"
    | GetMonth -> "get_month"
    | GetYear -> "get_year" )

let format_struct_name (fmt : Format.formatter) (v : Dcalc.Ast.StructName.t) : unit =
  Format.fprintf fmt "%s"
    (String.lowercase_ascii (Format.asprintf "%a" Dcalc.Ast.StructName.format_t v))

let format_enum_name (fmt : Format.formatter) (v : Dcalc.Ast.EnumName.t) : unit =
  Format.fprintf fmt "%s"
    (String.lowercase_ascii (Format.asprintf "%a" Dcalc.Ast.EnumName.format_t v))

let rec format_typ (fmt : Format.formatter) (typ : Dcalc.Ast.typ Pos.marked) : unit =
  let format_typ = format_typ in
  let format_typ_with_parens (fmt : Format.formatter) (t : Dcalc.Ast.typ Pos.marked) =
    Format.fprintf fmt "(%a)" format_typ t
  in
  let format_typ = format_typ_with_parens in
  match Pos.unmark typ with
  | TLit l -> Format.fprintf fmt "%a" Dcalc.Print.format_tlit l
  | TTuple (ts, None) ->
      Format.fprintf fmt "@[<hov 2>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ *@ ")
           (fun fmt t -> Format.fprintf fmt "%a" format_typ t))
        ts
  | TTuple (_, Some s) -> Format.fprintf fmt "%a" format_struct_name s
  | TEnum (_, e) -> Format.fprintf fmt "%a" format_enum_name e
  | TArrow (t1, t2) ->
      Format.fprintf fmt "@[<hov 2>%a ->@ %a@]" format_typ_with_parens t1 format_typ t2
  | TArray t1 -> Format.fprintf fmt "@[%a@ array@]" format_typ t1
  | TAny -> Format.fprintf fmt "_"

let format_var (fmt : Format.formatter) (v : Var.t) : unit =
  Format.fprintf fmt "%s" (String.lowercase_ascii (Bindlib.name_of v))

let needs_parens (_e : expr Pos.marked) : bool = true

let format_exception (fmt : Format.formatter) (exc : except) : unit =
  match exc with
  | ConflictError -> Format.fprintf fmt "ConflictError"
  | EmptyError -> Format.fprintf fmt "EmptyError"
  | Crash -> Format.fprintf fmt "Crash"

let rec format_expr (ctx : Dcalc.Ast.decl_ctx) (fmt : Format.formatter) (e : expr Pos.marked) : unit
    =
  let format_expr = format_expr ctx in
  let format_with_parens (fmt : Format.formatter) (e : expr Pos.marked) =
    if needs_parens e then Format.fprintf fmt "(%a)" format_expr e
    else Format.fprintf fmt "%a" format_expr e
  in
  let format_expr = format_with_parens in
  match Pos.unmark e with
  | EVar v -> Format.fprintf fmt "%a" format_var (Pos.unmark v)
  | ETuple (es, None) ->
      Format.fprintf fmt "@[<hov 2>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt e -> Format.fprintf fmt "%a" format_expr e))
        es
  | ETuple (es, Some s) ->
      Format.fprintf fmt "%a {@[<hov 2>%a@]}" Dcalc.Ast.StructName.format_t s
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (e, struct_field) ->
             Format.fprintf fmt "\"%a\":@ %a" Dcalc.Ast.StructFieldName.format_t struct_field
               format_expr e))
        (List.combine es (List.map fst (Dcalc.Ast.StructMap.find s ctx.ctx_structs)))
  | EArray es ->
      Format.fprintf fmt "@[<hov 2>[%a]@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt e -> Format.fprintf fmt "%a" format_expr e))
        es
  | ETupleAccess (e1, n, s, _ts) -> (
      match s with
      | None -> Format.fprintf fmt "%a.%d" format_expr e1 n
      | Some s ->
          Format.fprintf fmt "%a.\"%a\"" format_expr e1 Dcalc.Ast.StructFieldName.format_t
            (fst (List.nth (Dcalc.Ast.StructMap.find s ctx.ctx_structs) n)) )
  | EInj (e, n, en, _ts) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" Dcalc.Ast.EnumConstructor.format_t
        (fst (List.nth (Dcalc.Ast.EnumMap.find en ctx.ctx_enums) n))
        format_expr e
  | EMatch (e, es, e_name) ->
      Format.fprintf fmt "@[<hov 2>match@ %a@ with@ %a@]" format_expr e
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ |@ ")
           (fun fmt (e, c) ->
             Format.fprintf fmt "%a@ %a" Dcalc.Ast.EnumConstructor.format_t c
               (fun fmt e ->
                 match Pos.unmark e with
                 | EAbs (_, binder, _) ->
                     let xs, body = Bindlib.unmbind binder in
                     Format.fprintf fmt "(%a)@ ->@ %a"
                       (Format.pp_print_list
                          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
                          (fun fmt x -> Format.fprintf fmt "%a" format_var x))
                       (Array.to_list xs) format_expr body
                 | _ -> assert false
                 (* should not happen *))
               e))
        (List.combine es (List.map fst (Dcalc.Ast.EnumMap.find e_name ctx.ctx_enums)))
  | ELit l -> Format.fprintf fmt "%a" format_lit (Pos.same_pos_as l e)
  | EApp ((EAbs (_, binder, taus), _), args) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      let xs_tau_arg = List.map2 (fun (x, tau) arg -> (x, tau, arg)) xs_tau args in
      Format.fprintf fmt "@[%a%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
           (fun fmt (x, tau, arg) ->
             Format.fprintf fmt "@[@[<hov 2>let@ %a@ :@ %a@ =@ %a@]@ in@\n@]" format_var x
               format_typ tau format_expr arg))
        xs_tau_arg format_expr body
  | EAbs (_, binder, taus) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      Format.fprintf fmt "@[<hov 2>fun@ %a ->@ %a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
           (fun fmt (x, tau) ->
             Format.fprintf fmt "@[<hov 2>(%a:@ %a)@]" format_var x format_typ tau))
        xs_tau format_expr body
  | EApp ((EOp (Binop ((Dcalc.Ast.Map | Dcalc.Ast.Filter) as op)), _), [ arg1; arg2 ]) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" Dcalc.Print.format_binop (op, Pos.no_pos)
        format_with_parens arg1 format_with_parens arg2
  | EApp ((EOp (Binop op), _), [ arg1; arg2 ]) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" format_with_parens arg1 Dcalc.Print.format_binop
        (op, Pos.no_pos) format_with_parens arg2
  | EApp ((EOp (Unop op), _), [ arg1 ]) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_unop (op, Pos.no_pos) format_with_parens arg1
  | EApp (f, args) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_expr f
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ") format_with_parens)
        args
  | EIfThenElse (e1, e2, e3) ->
      Format.fprintf fmt "@[<hov 2> if@ @[<hov 2>%a@]@ then@ @[<hov 2>%a@]@ else@ @[<hov 2>%a@]@]"
        format_expr e1 format_expr e2 format_expr e3
  | EOp (Ternop op) -> Format.fprintf fmt "%a" format_ternop (op, Pos.no_pos)
  | EOp (Binop op) -> Format.fprintf fmt "%a" format_binop (op, Pos.no_pos)
  | EOp (Unop op) -> Format.fprintf fmt "%a" format_unop (op, Pos.no_pos)
  | EAssert e' -> Format.fprintf fmt "@[<hov 2>assert@ (%a)@]" format_expr e'
  | ERaise exc -> Format.fprintf fmt "raise@ %a" format_exception exc
  | ECatch (e1, exc, e2) ->
      Format.fprintf fmt "@[<hov 2>try@ %a@ with %a -> %a@]" format_expr e1 format_exception exc
        format_expr e2

let format_ctx (fmt : Format.formatter) (ctx : D.decl_ctx) : unit =
  Format.fprintf fmt "%a\n\n%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
       (fun fmt (struct_name, struct_fields) ->
         Format.fprintf fmt "type %a = {@\n@[<hov 2>  %a@]@\n}" format_struct_name struct_name
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
              (fun _fmt (struct_field, struct_field_type) ->
                Format.fprintf fmt "%a:@ %a;" Dcalc.Ast.StructFieldName.format_t struct_field
                  format_typ struct_field_type))
           struct_fields))
    (Dcalc.Ast.StructMap.bindings ctx.Dcalc.Ast.ctx_structs)
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "\n")
       (fun _fmt (enum_name, enum_cons) ->
         Format.fprintf fmt "type %a =@\n@[<hov 2>  %a@]@\n" format_enum_name enum_name
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
              (fun _fmt (enum_cons, enum_cons_type) ->
                Format.fprintf fmt "| %a@ of@ %a" Dcalc.Ast.EnumConstructor.format_t enum_cons
                  format_typ enum_cons_type))
           enum_cons))
    (Dcalc.Ast.EnumMap.bindings ctx.Dcalc.Ast.ctx_enums)

let format_program (ctx : D.decl_ctx) (fmt : Format.formatter) (e : Ast.expr Pos.marked) : unit =
  Format.fprintf fmt "%a\n\n%a" format_ctx ctx (format_expr ctx) e
