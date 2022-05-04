(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Ast

let is_uppercase (x : CamomileLibraryDefault.Camomile.UChar.t) : bool =
  try
    match CamomileLibraryDefault.Camomile.UCharInfo.general_category x with
    | `Ll -> false
    | `Lu -> true
    | _ -> false
  with _ -> true

let begins_with_uppercase (s : string) : bool =
  let first_letter = CamomileLibraryDefault.Camomile.UTF8.get s 0 in
  is_uppercase first_letter

(** {b Note:} (EmileRolley) seems to be factorizable with
    Dcalc.Print.format_lit. *)
let format_lit (fmt : Format.formatter) (l : lit Pos.marked) : unit =
  match Pos.unmark l with
  | LBool b -> Dcalc.Print.format_lit_style fmt (string_of_bool b)
  | LInt i -> Dcalc.Print.format_lit_style fmt (Runtime.integer_to_string i)
  | LUnit -> Dcalc.Print.format_lit_style fmt "()"
  | LRat i ->
      Dcalc.Print.format_lit_style fmt
        (Runtime.decimal_to_string ~max_prec_digits:!Utils.Cli.max_prec_digits i)
  | LMoney e -> (
      match !Utils.Cli.locale_lang with
      | En ->
          Dcalc.Print.format_lit_style fmt
            (Format.asprintf "$%s" (Runtime.money_to_string e))
      | Fr ->
          Dcalc.Print.format_lit_style fmt
            (Format.asprintf "%s €" (Runtime.money_to_string e))
      | Pl ->
          Dcalc.Print.format_lit_style fmt
            (Format.asprintf "%s PLN" (Runtime.money_to_string e)))
  | LDate d -> Dcalc.Print.format_lit_style fmt (Runtime.date_to_string d)
  | LDuration d ->
      Dcalc.Print.format_lit_style fmt (Runtime.duration_to_string d)

let format_exception (fmt : Format.formatter) (exn : except) : unit =
  Dcalc.Print.format_operator fmt
    (match exn with
    | EmptyError -> "EmptyError"
    | ConflictError -> "ConflictError"
    | Crash -> "Crash"
    | NoValueProvided -> "NoValueProvided")

let format_keyword (fmt : Format.formatter) (s : string) : unit =
  Format.fprintf fmt "%a" (Utils.Cli.format_with_style [ ANSITerminal.red ]) s

let format_punctuation (fmt : Format.formatter) (s : string) : unit =
  Format.fprintf fmt "%a" (Utils.Cli.format_with_style [ ANSITerminal.cyan ]) s

let needs_parens (e : expr Pos.marked) : bool =
  match Pos.unmark e with EAbs _ | ETuple (_, Some _) -> true | _ -> false

let format_var (fmt : Format.formatter) (v : Var.t) : unit =
  Format.fprintf fmt "%s_%d" (Bindlib.name_of v) (Bindlib.uid_of v)

let rec format_expr
    ?(debug : bool = false)
    (ctx : Dcalc.Ast.decl_ctx)
    (fmt : Format.formatter)
    (e : expr Pos.marked) : unit =
  let format_expr = format_expr ctx ~debug in
  let format_with_parens (fmt : Format.formatter) (e : expr Pos.marked) =
    if needs_parens e then
      Format.fprintf fmt "%a%a%a" format_punctuation "(" format_expr e
        format_punctuation ")"
    else Format.fprintf fmt "%a" format_expr e
  in
  match Pos.unmark e with
  | EVar v -> Format.fprintf fmt "%a" format_var (Pos.unmark v)
  | ETuple (es, None) ->
      Format.fprintf fmt "@[<hov 2>%a%a%a@]" format_punctuation "("
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt e -> Format.fprintf fmt "%a" format_expr e))
        es format_punctuation ")"
  | ETuple (es, Some s) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" Dcalc.Ast.StructName.format_t s
        format_punctuation "{"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (e, struct_field) ->
             Format.fprintf fmt "%a%a%a%a %a" format_punctuation "\""
               Dcalc.Ast.StructFieldName.format_t struct_field
               format_punctuation "\"" format_punctuation ":" format_expr e))
        (List.combine es
           (List.map fst (Dcalc.Ast.StructMap.find s ctx.ctx_structs)))
        format_punctuation "}"
  | EArray es ->
      Format.fprintf fmt "@[<hov 2>%a%a%a@]" format_punctuation "["
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt e -> Format.fprintf fmt "%a" format_expr e))
        es format_punctuation "]"
  | ETupleAccess (e1, n, s, _ts) -> (
      match s with
      | None ->
          Format.fprintf fmt "%a%a%d" format_expr e1 format_punctuation "." n
      | Some s ->
          Format.fprintf fmt "%a%a%a%a%a" format_expr e1 format_punctuation "."
            format_punctuation "\"" Dcalc.Ast.StructFieldName.format_t
            (fst (List.nth (Dcalc.Ast.StructMap.find s ctx.ctx_structs) n))
            format_punctuation "\"")
  | EInj (e, n, en, _ts) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" Dcalc.Print.format_enum_constructor
        (fst (List.nth (Dcalc.Ast.EnumMap.find en ctx.ctx_enums) n))
        format_expr e
  | EMatch (e, es, e_name) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@]" format_keyword "match"
        format_expr e format_keyword "with"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           (fun fmt (e, c) ->
             Format.fprintf fmt "@[<hov 2>%a %a%a@ %a@]" format_punctuation "|"
               Dcalc.Print.format_enum_constructor c format_punctuation ":"
               format_expr e))
        (List.combine es
           (List.map fst (Dcalc.Ast.EnumMap.find e_name ctx.ctx_enums)))
  | ELit l -> Format.fprintf fmt "%a" format_lit (Pos.same_pos_as l e)
  | EApp ((EAbs ((binder, _), taus), _), args) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      let xs_tau_arg =
        List.map2 (fun (x, tau) arg -> (x, tau, arg)) xs_tau args
      in
      Format.fprintf fmt "%a%a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
           (fun fmt (x, tau, arg) ->
             Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@ %a@ %a@]@\n"
               format_keyword "let" format_var x format_punctuation ":"
               (Dcalc.Print.format_typ ctx)
               tau format_punctuation "=" format_expr arg format_keyword "in"))
        xs_tau_arg format_expr body
  | EAbs ((binder, _), taus) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      Format.fprintf fmt "@[<hov 2>%a %a %a@ %a@]" format_punctuation "λ"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
           (fun fmt (x, tau) ->
             Format.fprintf fmt "%a%a%a %a%a" format_punctuation "(" format_var
               x format_punctuation ":"
               (Dcalc.Print.format_typ ctx)
               tau format_punctuation ")"))
        xs_tau format_punctuation "→" format_expr body
  | EApp
      ( (EOp (Binop ((Dcalc.Ast.Map | Dcalc.Ast.Filter) as op)), _),
        [ arg1; arg2 ] ) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" Dcalc.Print.format_binop
        (op, Pos.no_pos) format_with_parens arg1 format_with_parens arg2
  | EApp ((EOp (Binop op), _), [ arg1; arg2 ]) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" format_with_parens arg1
        Dcalc.Print.format_binop (op, Pos.no_pos) format_with_parens arg2
  | EApp ((EOp (Unop (Log _)), _), [ arg1 ]) when not debug ->
      Format.fprintf fmt "%a" format_with_parens arg1
  | EApp ((EOp (Unop op), _), [ arg1 ]) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" Dcalc.Print.format_unop
        (op, Pos.no_pos) format_with_parens arg1
  | EApp (f, args) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_expr f
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
           format_with_parens)
        args
  | EIfThenElse (e1, e2, e3) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@ %a@]" format_keyword "if"
        format_expr e1 format_keyword "then" format_expr e2 format_keyword
        "else" format_expr e3
  | EOp (Ternop op) ->
      Format.fprintf fmt "%a" Dcalc.Print.format_ternop (op, Pos.no_pos)
  | EOp (Binop op) ->
      Format.fprintf fmt "%a" Dcalc.Print.format_binop (op, Pos.no_pos)
  | EOp (Unop op) ->
      Format.fprintf fmt "%a" Dcalc.Print.format_unop (op, Pos.no_pos)
  | ECatch (e1, exn, e2) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a ->@ %a@]" format_keyword "try"
        format_with_parens e1 format_keyword "with" format_exception exn
        format_with_parens e2
  | ERaise exn ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_keyword "raise"
        format_exception exn
  | EAssert e' ->
      Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" format_keyword "assert"
        format_punctuation "(" format_expr e' format_punctuation ")"

let format_scope
    ?(debug : bool = false)
    (ctx : Dcalc.Ast.decl_ctx)
    (fmt : Format.formatter)
    ((n, s) : Dcalc.Ast.ScopeName.t * Ast.expr Dcalc.Ast.scope_body) : unit =
  Format.fprintf fmt "@[<hov 2>%a %a =@ %a@]" format_keyword "let"
    Dcalc.Ast.ScopeName.format_t n (format_expr ctx ~debug)
    (Bindlib.unbox
       (Dcalc.Ast.build_whole_scope_expr ~make_abs:Ast.make_abs
          ~make_let_in:Ast.make_let_in ~box_expr:Ast.box_expr ctx s
          (Pos.get_position (Dcalc.Ast.ScopeName.get_info n))))
