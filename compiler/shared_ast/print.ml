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
open String_common
open Definitions

let typ_needs_parens (ty : typ) : bool =
  match Marked.unmark ty with TArrow _ | TArray _ -> true | _ -> false

let uid_list (fmt : Format.formatter) (infos : Uid.MarkedString.info list) :
    unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.')
    (fun fmt info ->
      Utils.Cli.format_with_style
        (if begins_with_uppercase (Marked.unmark info) then [ANSITerminal.red]
        else [])
        fmt
        (Utils.Uid.MarkedString.to_string info))
    fmt infos

let keyword (fmt : Format.formatter) (s : string) : unit =
  Utils.Cli.format_with_style [ANSITerminal.red] fmt s

let base_type (fmt : Format.formatter) (s : string) : unit =
  Utils.Cli.format_with_style [ANSITerminal.yellow] fmt s

let punctuation (fmt : Format.formatter) (s : string) : unit =
  Utils.Cli.format_with_style [ANSITerminal.cyan] fmt s

let operator (fmt : Format.formatter) (s : string) : unit =
  Utils.Cli.format_with_style [ANSITerminal.green] fmt s

let lit_style (fmt : Format.formatter) (s : string) : unit =
  Utils.Cli.format_with_style [ANSITerminal.yellow] fmt s

let tlit (fmt : Format.formatter) (l : typ_lit) : unit =
  base_type fmt
    (match l with
    | TUnit -> "unit"
    | TBool -> "bool"
    | TInt -> "integer"
    | TRat -> "decimal"
    | TMoney -> "money"
    | TDuration -> "duration"
    | TDate -> "date")

let location (type a) (fmt : Format.formatter) (l : a glocation) : unit =
  match l with
  | DesugaredScopeVar (v, _st) ->
    Format.fprintf fmt "%a" ScopeVar.format_t (Marked.unmark v)
  | ScopelangScopeVar v ->
    Format.fprintf fmt "%a" ScopeVar.format_t (Marked.unmark v)
  | SubScopeVar (_, subindex, subvar) ->
    Format.fprintf fmt "%a.%a" SubScopeName.format_t (Marked.unmark subindex)
      ScopeVar.format_t (Marked.unmark subvar)

let enum_constructor (fmt : Format.formatter) (c : EnumConstructor.t) : unit =
  Format.fprintf fmt "%a"
    (Utils.Cli.format_with_style [ANSITerminal.magenta])
    (Format.asprintf "%a" EnumConstructor.format_t c)

let rec typ (ctx : decl_ctx) (fmt : Format.formatter) (ty : typ) : unit =
  let typ = typ ctx in
  let typ_with_parens (fmt : Format.formatter) (t : typ) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" typ t
    else Format.fprintf fmt "%a" typ t
  in
  match Marked.unmark ty with
  | TLit l -> tlit fmt l
  | TTuple ts ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ %a@ " operator "*")
         (fun fmt t -> Format.fprintf fmt "%a" typ t))
      ts
  | TStruct s ->
    Format.fprintf fmt "@[<hov 2>%a%a%a%a@]" StructName.format_t s punctuation
      "{"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ";")
         (fun fmt (field, mty) ->
           Format.fprintf fmt "%a%a%a%a@ %a" punctuation "\""
             StructFieldName.format_t field punctuation "\"" punctuation ":" typ
             mty))
      (StructMap.find s ctx.ctx_structs)
      punctuation "}"
  | TEnum e ->
    Format.fprintf fmt "@[<hov 2>%a%a%a%a@]" EnumName.format_t e punctuation "["
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ %a@ " punctuation "|")
         (fun fmt (case, mty) ->
           Format.fprintf fmt "%a%a@ %a" enum_constructor case punctuation ":"
             typ mty))
      (EnumMap.find e ctx.ctx_enums)
      punctuation "]"
  | TOption t -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" base_type "option" typ t
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>%a %a@ %a@]" typ_with_parens t1 operator "→"
      typ t2
  | TArray t1 ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" base_type "collection" typ t1
  | TAny -> base_type fmt "any"

let lit (type a) (fmt : Format.formatter) (l : a glit) : unit =
  match l with
  | LBool b -> lit_style fmt (string_of_bool b)
  | LInt i -> lit_style fmt (Runtime.integer_to_string i)
  | LEmptyError -> lit_style fmt "∅ "
  | LUnit -> lit_style fmt "()"
  | LRat i ->
    lit_style fmt
      (Runtime.decimal_to_string ~max_prec_digits:!Utils.Cli.max_prec_digits i)
  | LMoney e -> (
    match !Utils.Cli.locale_lang with
    | En -> lit_style fmt (Format.asprintf "$%s" (Runtime.money_to_string e))
    | Fr -> lit_style fmt (Format.asprintf "%s €" (Runtime.money_to_string e))
    | Pl -> lit_style fmt (Format.asprintf "%s PLN" (Runtime.money_to_string e))
    )
  | LDate d -> lit_style fmt (Runtime.date_to_string d)
  | LDuration d -> lit_style fmt (Runtime.duration_to_string d)

let op_kind (fmt : Format.formatter) (k : op_kind) =
  Format.fprintf fmt "%s"
    (match k with
    | KInt -> ""
    | KRat -> "."
    | KMoney -> "$"
    | KDate -> "@"
    | KDuration -> "^")

let binop (fmt : Format.formatter) (op : binop) : unit =
  operator fmt
    (match op with
    | Add k -> Format.asprintf "+%a" op_kind k
    | Sub k -> Format.asprintf "-%a" op_kind k
    | Mult k -> Format.asprintf "*%a" op_kind k
    | Div k -> Format.asprintf "/%a" op_kind k
    | And -> "&&"
    | Or -> "||"
    | Xor -> "xor"
    | Eq -> "="
    | Neq -> "!="
    | Lt k -> Format.asprintf "%s%a" "<" op_kind k
    | Lte k -> Format.asprintf "%s%a" "<=" op_kind k
    | Gt k -> Format.asprintf "%s%a" ">" op_kind k
    | Gte k -> Format.asprintf "%s%a" ">=" op_kind k
    | Concat -> "++"
    | Map -> "map"
    | Filter -> "filter")

let ternop (fmt : Format.formatter) (op : ternop) : unit =
  match op with Fold -> keyword fmt "fold"

let log_entry (fmt : Format.formatter) (entry : log_entry) : unit =
  Format.fprintf fmt "@<2>%a"
    (fun fmt -> function
      | VarDef _ -> Utils.Cli.format_with_style [ANSITerminal.blue] fmt "≔ "
      | BeginCall -> Utils.Cli.format_with_style [ANSITerminal.yellow] fmt "→ "
      | EndCall -> Utils.Cli.format_with_style [ANSITerminal.yellow] fmt "← "
      | PosRecordIfTrueBool ->
        Utils.Cli.format_with_style [ANSITerminal.green] fmt "☛ ")
    entry

let unop (fmt : Format.formatter) (op : unop) : unit =
  match op with
  | Minus _ -> Format.pp_print_string fmt "-"
  | Not -> Format.pp_print_string fmt "~"
  | Log (entry, infos) ->
    Format.fprintf fmt "log@[<hov 2>[%a|%a]@]" log_entry entry
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ".")
         (fun fmt info -> Utils.Uid.MarkedString.format_info fmt info))
      infos
  | Length -> Format.pp_print_string fmt "length"
  | IntToRat -> Format.pp_print_string fmt "int_to_rat"
  | MoneyToRat -> Format.pp_print_string fmt "money_to_rat"
  | RatToMoney -> Format.pp_print_string fmt "rat_to_money"
  | GetDay -> Format.pp_print_string fmt "get_day"
  | GetMonth -> Format.pp_print_string fmt "get_month"
  | GetYear -> Format.pp_print_string fmt "get_year"
  | FirstDayOfMonth -> Format.pp_print_string fmt "first_day_of_month"
  | LastDayOfMonth -> Format.pp_print_string fmt "last_day_of_month"
  | RoundMoney -> Format.pp_print_string fmt "round_money"
  | RoundDecimal -> Format.pp_print_string fmt "round_decimal"

let except (fmt : Format.formatter) (exn : except) : unit =
  operator fmt
    (match exn with
    | EmptyError -> "EmptyError"
    | ConflictError -> "ConflictError"
    | Crash -> "Crash"
    | NoValueProvided -> "NoValueProvided")

let var fmt v =
  Format.fprintf fmt "%s_%d" (Bindlib.name_of v) (Bindlib.uid_of v)

let needs_parens (type a) (e : (a, _) gexpr) : bool =
  match Marked.unmark e with EAbs _ | ETuple (_, Some _) -> true | _ -> false

let rec expr :
          'a.
          ?debug:bool -> decl_ctx -> Format.formatter -> ('a, 't) gexpr -> unit
    =
  fun (type a) ?(debug : bool = false) (ctx : decl_ctx) (fmt : Format.formatter)
      (e : (a, 't) gexpr) ->
   let expr e = expr ~debug ctx e in
   let with_parens fmt e =
     if needs_parens e then (
       punctuation fmt "(";
       expr fmt e;
       punctuation fmt ")")
     else expr fmt e
   in
   match Marked.unmark e with
   | EVar v -> Format.fprintf fmt "%a" var v
   | ETuple (es, None) ->
     Format.fprintf fmt "@[<hov 2>%a%a%a@]" punctuation "("
       (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
          (fun fmt e -> Format.fprintf fmt "%a" expr e))
       es punctuation ")"
   | ETuple (es, Some s) ->
     Format.fprintf fmt "@[<hov 2>%a@ @[<hov 2>%a%a%a@]@]" StructName.format_t s
       punctuation "{"
       (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ";")
          (fun fmt (e, struct_field) ->
            Format.fprintf fmt "%a%a%a%a@ %a" punctuation "\""
              StructFieldName.format_t struct_field punctuation "\"" punctuation
              "=" expr e))
       (List.combine es (List.map fst (StructMap.find s ctx.ctx_structs)))
       punctuation "}"
   | EArray es ->
     Format.fprintf fmt "@[<hov 2>%a%a%a@]" punctuation "["
       (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
          (fun fmt e -> Format.fprintf fmt "%a" expr e))
       es punctuation "]"
   | ETupleAccess (e1, n, s, _ts) -> (
     match s with
     | None -> Format.fprintf fmt "%a%a%d" expr e1 punctuation "." n
     | Some s ->
       Format.fprintf fmt "%a%a%a%a%a" expr e1 operator "." punctuation "\""
         StructFieldName.format_t
         (fst (List.nth (StructMap.find s ctx.ctx_structs) n))
         punctuation "\"")
   | EInj (e, n, en, _ts) ->
     Format.fprintf fmt "@[<hov 2>%a@ %a@]" enum_constructor
       (fst (List.nth (EnumMap.find en ctx.ctx_enums) n))
       expr e
   | EMatch (e, es, e_name) ->
     Format.fprintf fmt "@[<hov 0>%a@ @[<hov 2>%a@]@ %a@ %a@]" keyword "match"
       expr e keyword "with"
       (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
          (fun fmt (e, c) ->
            Format.fprintf fmt "@[<hov 2>%a %a%a@ %a@]" punctuation "|"
              enum_constructor c punctuation ":" expr e))
       (List.combine es (List.map fst (EnumMap.find e_name ctx.ctx_enums)))
   | ELit l -> lit fmt l
   | EApp ((EAbs (binder, taus), _), args) ->
     let xs, body = Bindlib.unmbind binder in
     let xs_tau = List.mapi (fun i tau -> xs.(i), tau) taus in
     let xs_tau_arg = List.map2 (fun (x, tau) arg -> x, tau, arg) xs_tau args in
     Format.fprintf fmt "%a%a"
       (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
          (fun fmt (x, tau, arg) ->
            Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@ %a@ %a@]@\n"
              keyword "let" var x punctuation ":" (typ ctx) tau punctuation "="
              expr arg keyword "in"))
       xs_tau_arg expr body
   | EAbs (binder, taus) ->
     let xs, body = Bindlib.unmbind binder in
     let xs_tau = List.mapi (fun i tau -> xs.(i), tau) taus in
     Format.fprintf fmt "@[<hov 2>%a @[<hov 2>%a@] %a@ %a@]" punctuation "λ"
       (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
          (fun fmt (x, tau) ->
            Format.fprintf fmt "%a%a%a %a%a" punctuation "(" var x punctuation
              ":" (typ ctx) tau punctuation ")"))
       xs_tau punctuation "→" expr body
   | EApp ((EOp (Binop ((Map | Filter) as op)), _), [arg1; arg2]) ->
     Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" binop op with_parens arg1
       with_parens arg2
   | EApp ((EOp (Binop op), _), [arg1; arg2]) ->
     Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" with_parens arg1 binop op
       with_parens arg2
   | EApp ((EOp (Unop (Log _)), _), [arg1]) when not debug -> expr fmt arg1
   | EApp ((EOp (Unop op), _), [arg1]) ->
     Format.fprintf fmt "@[<hov 2>%a@ %a@]" unop op with_parens arg1
   | EApp (f, args) ->
     Format.fprintf fmt "@[<hov 2>%a@ %a@]" expr f
       (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
          with_parens)
       args
   | EIfThenElse (e1, e2, e3) ->
     Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@ %a@]" keyword "if" expr e1
       keyword "then" expr e2 keyword "else" expr e3
   | EOp (Ternop op) -> Format.fprintf fmt "%a" ternop op
   | EOp (Binop op) -> Format.fprintf fmt "%a" binop op
   | EOp (Unop op) -> Format.fprintf fmt "%a" unop op
   | EDefault (exceptions, just, cons) ->
     if List.length exceptions = 0 then
       Format.fprintf fmt "@[<hov 2>%a%a@ %a@ %a%a@]" punctuation "⟨" expr just
         punctuation "⊢" expr cons punctuation "⟩"
     else
       Format.fprintf fmt "@[<hov 2>%a%a@ %a@ %a@ %a@ %a%a@]" punctuation "⟨"
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ",")
            expr)
         exceptions punctuation "|" expr just punctuation "⊢" expr cons
         punctuation "⟩"
   | ErrorOnEmpty e' ->
     Format.fprintf fmt "%a@ %a" operator "error_empty" with_parens e'
   | EAssert e' ->
     Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" keyword "assert" punctuation "("
       expr e' punctuation ")"
   | ECatch (e1, exn, e2) ->
     Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a ->@ %a@]" keyword "try"
       with_parens e1 keyword "with" except exn with_parens e2
   | ERaise exn ->
     Format.fprintf fmt "@[<hov 2>%a@ %a@]" keyword "raise" except exn
   | ELocation loc -> location fmt loc
   | EStruct (name, fields) ->
     Format.fprintf fmt " @[<hov 2>%a@ %a@ %a@ %a@]" StructName.format_t name
       punctuation "{"
       (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ";")
          (fun fmt (field_name, field_expr) ->
            Format.fprintf fmt "%a%a%a%a@ %a" punctuation "\""
              StructFieldName.format_t field_name punctuation "\"" punctuation
              "=" expr field_expr))
       (StructFieldMap.bindings fields)
       punctuation "}"
   | EStructAccess (e1, field, _) ->
     Format.fprintf fmt "%a%a%a%a%a" expr e1 punctuation "." punctuation "\""
       StructFieldName.format_t field punctuation "\""
   | EEnumInj (e1, cons, _) ->
     Format.fprintf fmt "%a@ %a" EnumConstructor.format_t cons expr e1
   | EMatchS (e1, _, cases) ->
     Format.fprintf fmt "@[<hov 0>%a@ @[<hov 2>%a@]@ %a@ %a@]" keyword "match"
       expr e1 keyword "with"
       (Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
          (fun fmt (cons_name, case_expr) ->
            Format.fprintf fmt "@[<hov 2>%a %a@ %a@ %a@]" punctuation "|"
              enum_constructor cons_name punctuation "→" expr case_expr))
       (EnumConstructorMap.bindings cases)
