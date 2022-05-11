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

let typ_needs_parens (e : typ Pos.marked) : bool =
  match Pos.unmark e with TArrow _ | TArray _ -> true | _ -> false

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

let format_uid_list
    (fmt : Format.formatter)
    (infos : Uid.MarkedString.info list) : unit =
  Format.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ".")
       (fun fmt info ->
         Format.fprintf fmt "%a"
           (Utils.Cli.format_with_style
              (if begins_with_uppercase (Pos.unmark info) then
               [ANSITerminal.red]
              else []))
           (Format.asprintf "%a" Utils.Uid.MarkedString.format_info info)))
    infos

let format_keyword (fmt : Format.formatter) (s : string) : unit =
  Format.fprintf fmt "%a" (Utils.Cli.format_with_style [ANSITerminal.red]) s

let format_base_type (fmt : Format.formatter) (s : string) : unit =
  Format.fprintf fmt "%a" (Utils.Cli.format_with_style [ANSITerminal.yellow]) s

let format_punctuation (fmt : Format.formatter) (s : string) : unit =
  Format.fprintf fmt "%a" (Utils.Cli.format_with_style [ANSITerminal.cyan]) s

let format_operator (fmt : Format.formatter) (s : string) : unit =
  Format.fprintf fmt "%a" (Utils.Cli.format_with_style [ANSITerminal.green]) s

let format_lit_style (fmt : Format.formatter) (s : string) : unit =
  Format.fprintf fmt "%a" (Utils.Cli.format_with_style [ANSITerminal.yellow]) s

let format_tlit (fmt : Format.formatter) (l : typ_lit) : unit =
  format_base_type fmt
    (match l with
    | TUnit -> "unit"
    | TBool -> "bool"
    | TInt -> "integer"
    | TRat -> "decimal"
    | TMoney -> "money"
    | TDuration -> "duration"
    | TDate -> "date")

let format_enum_constructor (fmt : Format.formatter) (c : EnumConstructor.t) :
    unit =
  Format.fprintf fmt "%a"
    (Utils.Cli.format_with_style [ANSITerminal.magenta])
    (Format.asprintf "%a" EnumConstructor.format_t c)

let rec format_typ
    (ctx : Ast.decl_ctx)
    (fmt : Format.formatter)
    (typ : typ Pos.marked) : unit =
  let format_typ = format_typ ctx in
  let format_typ_with_parens (fmt : Format.formatter) (t : typ Pos.marked) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  match Pos.unmark typ with
  | TLit l -> Format.fprintf fmt "%a" format_tlit l
  | TTuple (ts, None) ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ %a@ " format_operator "*")
         (fun fmt t -> Format.fprintf fmt "%a" format_typ t))
      ts
  | TTuple (_args, Some s) ->
    Format.fprintf fmt "@[<hov 2>%a%a%a%a@]" Ast.StructName.format_t s
      format_punctuation "{"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () ->
           Format.fprintf fmt "%a@ " format_punctuation ";")
         (fun fmt (field, typ) ->
           Format.fprintf fmt "%a%a%a%a@ %a" format_punctuation "\""
             StructFieldName.format_t field format_punctuation "\""
             format_punctuation ":" format_typ typ))
      (StructMap.find s ctx.ctx_structs)
      format_punctuation "}"
  | TEnum (_, e) ->
    Format.fprintf fmt "@[<hov 2>%a%a%a%a@]" Ast.EnumName.format_t e
      format_punctuation "["
      (Format.pp_print_list
         ~pp_sep:(fun fmt () ->
           Format.fprintf fmt "@ %a@ " format_punctuation "|")
         (fun fmt (case, typ) ->
           Format.fprintf fmt "%a%a@ %a" format_enum_constructor case
             format_punctuation ":" format_typ typ))
      (EnumMap.find e ctx.ctx_enums)
      format_punctuation "]"
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>%a %a@ %a@]" format_typ_with_parens t1
      format_operator "→" format_typ t2
  | TArray t1 ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_base_type "array" format_typ
      t1
  | TAny -> format_base_type fmt "any"

(* (EmileRolley) NOTE: seems to be factorizable with Lcalc.Print.format_lit. *)
let format_lit (fmt : Format.formatter) (l : lit Pos.marked) : unit =
  match Pos.unmark l with
  | LBool b -> format_lit_style fmt (string_of_bool b)
  | LInt i -> format_lit_style fmt (Runtime.integer_to_string i)
  | LEmptyError -> format_lit_style fmt "∅ "
  | LUnit -> format_lit_style fmt "()"
  | LRat i ->
    format_lit_style fmt
      (Runtime.decimal_to_string ~max_prec_digits:!Utils.Cli.max_prec_digits i)
  | LMoney e -> (
    match !Utils.Cli.locale_lang with
    | En ->
      format_lit_style fmt (Format.asprintf "$%s" (Runtime.money_to_string e))
    | Fr ->
      format_lit_style fmt (Format.asprintf "%s €" (Runtime.money_to_string e))
    | Pl ->
      format_lit_style fmt
        (Format.asprintf "%s PLN" (Runtime.money_to_string e)))
  | LDate d -> format_lit_style fmt (Runtime.date_to_string d)
  | LDuration d -> format_lit_style fmt (Runtime.duration_to_string d)

let format_op_kind (fmt : Format.formatter) (k : op_kind) =
  Format.fprintf fmt "%s"
    (match k with
    | KInt -> ""
    | KRat -> "."
    | KMoney -> "$"
    | KDate -> "@"
    | KDuration -> "^")

let format_binop (fmt : Format.formatter) (op : binop Pos.marked) : unit =
  format_operator fmt
    (match Pos.unmark op with
    | Add k -> Format.asprintf "+%a" format_op_kind k
    | Sub k -> Format.asprintf "-%a" format_op_kind k
    | Mult k -> Format.asprintf "*%a" format_op_kind k
    | Div k -> Format.asprintf "/%a" format_op_kind k
    | And -> "&&"
    | Or -> "||"
    | Xor -> "xor"
    | Eq -> "="
    | Neq -> "!="
    | Lt k -> Format.asprintf "%s%a" "<" format_op_kind k
    | Lte k -> Format.asprintf "%s%a" "<=" format_op_kind k
    | Gt k -> Format.asprintf "%s%a" ">" format_op_kind k
    | Gte k -> Format.asprintf "%s%a" ">=" format_op_kind k
    | Concat -> "++"
    | Map -> "map"
    | Filter -> "filter")

let format_ternop (fmt : Format.formatter) (op : ternop Pos.marked) : unit =
  match Pos.unmark op with Fold -> format_keyword fmt "fold"

let format_log_entry (fmt : Format.formatter) (entry : log_entry) : unit =
  Format.fprintf fmt "@<2>%s"
    (match entry with
    | VarDef _ -> Utils.Cli.with_style [ANSITerminal.blue] "≔ "
    | BeginCall -> Utils.Cli.with_style [ANSITerminal.yellow] "→ "
    | EndCall -> Utils.Cli.with_style [ANSITerminal.yellow] "← "
    | PosRecordIfTrueBool -> Utils.Cli.with_style [ANSITerminal.green] "☛ ")

let format_unop (fmt : Format.formatter) (op : unop Pos.marked) : unit =
  Format.fprintf fmt "%s"
    (match Pos.unmark op with
    | Minus _ -> "-"
    | Not -> "~"
    | Log (entry, infos) ->
      Format.asprintf "log@[<hov 2>[%a|%a]@]" format_log_entry entry
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ".")
           (fun fmt info -> Utils.Uid.MarkedString.format_info fmt info))
        infos
    | Length -> "length"
    | IntToRat -> "int_to_rat"
    | GetDay -> "get_day"
    | GetMonth -> "get_month"
    | GetYear -> "get_year"
    | RoundMoney -> "round_money"
    | RoundDecimal -> "round_decimal")

let needs_parens (e : expr Pos.marked) : bool =
  match Pos.unmark e with EAbs _ | ETuple (_, Some _) -> true | _ -> false

let format_var (fmt : Format.formatter) (v : Var.t) : unit =
  Format.fprintf fmt "%s_%d" (Bindlib.name_of v) (Bindlib.uid_of v)

let rec format_expr
    ?(debug : bool = false)
    (ctx : Ast.decl_ctx)
    (fmt : Format.formatter)
    (e : expr Pos.marked) : unit =
  let format_expr = format_expr ~debug ctx in
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
    Format.fprintf fmt "@[<hov 2>%a@ @[<hov 2>%a%a%a@]@]"
      Ast.StructName.format_t s format_punctuation "{"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () ->
           Format.fprintf fmt "%a@ " format_punctuation ";")
         (fun fmt (e, struct_field) ->
           Format.fprintf fmt "%a%a%a%a@ %a" format_punctuation "\""
             Ast.StructFieldName.format_t struct_field format_punctuation "\""
             format_punctuation "=" format_expr e))
      (List.combine es (List.map fst (Ast.StructMap.find s ctx.ctx_structs)))
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
      Format.fprintf fmt "%a%a%a%a%a" format_expr e1 format_operator "."
        format_punctuation "\"" Ast.StructFieldName.format_t
        (fst (List.nth (Ast.StructMap.find s ctx.ctx_structs) n))
        format_punctuation "\"")
  | EInj (e, n, en, _ts) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_enum_constructor
      (fst (List.nth (Ast.EnumMap.find en ctx.ctx_enums) n))
      format_expr e
  | EMatch (e, es, e_name) ->
    Format.fprintf fmt "@[<hov 0>%a@ @[<hov 2>%a@]@ %a@ %a@]" format_keyword
      "match" format_expr e format_keyword "with"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt (e, c) ->
           Format.fprintf fmt "@[<hov 2>%a %a%a@ %a@]" format_punctuation "|"
             format_enum_constructor c format_punctuation ":" format_expr e))
      (List.combine es (List.map fst (Ast.EnumMap.find e_name ctx.ctx_enums)))
  | ELit l -> format_lit fmt (Pos.same_pos_as l e)
  | EApp ((EAbs ((binder, _), taus), _), args) ->
    let xs, body = Bindlib.unmbind binder in
    let xs_tau = List.map2 (fun x tau -> x, tau) (Array.to_list xs) taus in
    let xs_tau_arg = List.map2 (fun (x, tau) arg -> x, tau, arg) xs_tau args in
    Format.fprintf fmt "%a%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
         (fun fmt (x, tau, arg) ->
           Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@ %a@ %a@]@\n"
             format_keyword "let" format_var x format_punctuation ":"
             (format_typ ctx) tau format_punctuation "=" format_expr arg
             format_keyword "in"))
      xs_tau_arg format_expr body
  | EAbs ((binder, _), taus) ->
    let xs, body = Bindlib.unmbind binder in
    let xs_tau = List.map2 (fun x tau -> x, tau) (Array.to_list xs) taus in
    Format.fprintf fmt "@[<hov 2>%a @[<hov 2>%a@] %a@ %a@]" format_punctuation
      "λ"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (x, tau) ->
           Format.fprintf fmt "%a%a%a %a%a" format_punctuation "(" format_var x
             format_punctuation ":" (format_typ ctx) tau format_punctuation ")"))
      xs_tau format_punctuation "→" format_expr body
  | EApp ((EOp (Binop ((Ast.Map | Ast.Filter) as op)), _), [arg1; arg2]) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" format_binop (op, Pos.no_pos)
      format_with_parens arg1 format_with_parens arg2
  | EApp ((EOp (Binop op), _), [arg1; arg2]) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" format_with_parens arg1
      format_binop (op, Pos.no_pos) format_with_parens arg2
  | EApp ((EOp (Unop (Log _)), _), [arg1]) when not debug ->
    format_expr fmt arg1
  | EApp ((EOp (Unop op), _), [arg1]) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_unop (op, Pos.no_pos)
      format_with_parens arg1
  | EApp (f, args) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_expr f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         format_with_parens)
      args
  | EIfThenElse (e1, e2, e3) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@ %a@]" format_keyword "if"
      format_expr e1 format_keyword "then" format_expr e2 format_keyword "else"
      format_expr e3
  | EOp (Ternop op) -> Format.fprintf fmt "%a" format_ternop (op, Pos.no_pos)
  | EOp (Binop op) -> Format.fprintf fmt "%a" format_binop (op, Pos.no_pos)
  | EOp (Unop op) -> Format.fprintf fmt "%a" format_unop (op, Pos.no_pos)
  | EDefault (exceptions, just, cons) ->
    if List.length exceptions = 0 then
      Format.fprintf fmt "@[<hov 2>%a%a@ %a@ %a%a@]" format_punctuation "⟨"
        format_expr just format_punctuation "⊢" format_expr cons
        format_punctuation "⟩"
    else
      Format.fprintf fmt "@[<hov 2>%a%a@ %a@ %a@ %a@ %a%a@]" format_punctuation
        "⟨"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () ->
             Format.fprintf fmt "%a@ " format_punctuation ",")
           format_expr)
        exceptions format_punctuation "|" format_expr just format_punctuation
        "⊢" format_expr cons format_punctuation "⟩"
  | ErrorOnEmpty e' ->
    Format.fprintf fmt "%a@ %a" format_operator "error_empty" format_with_parens
      e'
  | EAssert e' ->
    Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" format_keyword "assert"
      format_punctuation "(" format_expr e' format_punctuation ")"

let format_scope
    ?(debug : bool = false)
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    ((n, s) : Ast.ScopeName.t * Ast.expr scope_body) =
  Format.fprintf fmt "@[<hov 2>%a %a =@ %a@]" format_keyword "let"
    Ast.ScopeName.format_t n (format_expr ctx ~debug)
    (Bindlib.unbox
       (Ast.build_whole_scope_expr ~make_abs:Ast.make_abs
          ~make_let_in:Ast.make_let_in ~box_expr:Ast.box_expr ctx s
          (Pos.get_position (Ast.ScopeName.get_info n))))
