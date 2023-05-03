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

open Catala_utils
open Definitions

let typ_needs_parens (ty : typ) : bool =
  match Marked.unmark ty with TArrow _ | TArray _ -> true | _ -> false

let uid_list (fmt : Format.formatter) (infos : Uid.MarkedString.info list) :
    unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.')
    (fun fmt info ->
      Cli.format_with_style
        (if String.begins_with_uppercase (Marked.unmark info) then
         [ANSITerminal.red]
        else [])
        fmt
        (Uid.MarkedString.to_string info))
    fmt infos

let keyword (fmt : Format.formatter) (s : string) : unit =
  Cli.format_with_style [ANSITerminal.red] fmt s

let base_type (fmt : Format.formatter) (s : string) : unit =
  Cli.format_with_style [ANSITerminal.yellow] fmt s

let punctuation (fmt : Format.formatter) (s : string) : unit =
  Format.pp_print_as fmt 1 (Cli.with_style [ANSITerminal.cyan] "%s" s)

let op_style (fmt : Format.formatter) (s : string) : unit =
  Cli.format_with_style [ANSITerminal.green] fmt s

let lit_style (fmt : Format.formatter) (s : string) : unit =
  Cli.format_with_style [ANSITerminal.yellow] fmt s

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
  | DesugaredScopeVar (v, _st) -> ScopeVar.format_t fmt (Marked.unmark v)
  | ScopelangScopeVar v -> ScopeVar.format_t fmt (Marked.unmark v)
  | SubScopeVar (_, subindex, subvar) ->
    Format.fprintf fmt "%a.%a" SubScopeName.format_t (Marked.unmark subindex)
      ScopeVar.format_t (Marked.unmark subvar)
  | ToplevelVar v -> TopdefName.format_t fmt (Marked.unmark v)

let enum_constructor (fmt : Format.formatter) (c : EnumConstructor.t) : unit =
  Cli.format_with_style [ANSITerminal.magenta] fmt
    (Format.asprintf "%a" EnumConstructor.format_t c)

let struct_field (fmt : Format.formatter) (c : StructField.t) : unit =
  Cli.format_with_style [ANSITerminal.magenta] fmt
    (Format.asprintf "%a" StructField.format_t c)

let rec typ (ctx : decl_ctx option) (fmt : Format.formatter) (ty : typ) : unit =
  let typ = typ ctx in
  let typ_with_parens (fmt : Format.formatter) (t : typ) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" typ t else typ fmt t
  in
  match Marked.unmark ty with
  | TLit l -> tlit fmt l
  | TTuple ts ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " %a@ " op_style "*")
         typ)
      ts
  | TStruct s -> (
    match ctx with
    | None -> StructName.format_t fmt s
    | Some ctx ->
      let fields = StructName.Map.find s ctx.ctx_structs in
      if StructField.Map.is_empty fields then StructName.format_t fmt s
      else
        Format.fprintf fmt "@[<hv 2>%a %a@,%a@;<0 -2>%a@]" StructName.format_t s
          punctuation "{"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () ->
               punctuation fmt ";";
               Format.pp_print_space fmt ())
             (fun fmt (field_name, field_typ) ->
               Format.fprintf fmt "@[<hv 2>%a%a@ %a@]" struct_field field_name
                 punctuation ":" typ field_typ))
          (StructField.Map.bindings fields)
          punctuation "}")
  | TEnum e -> (
    match ctx with
    | None -> Format.fprintf fmt "@[<hov 2>%a@]" EnumName.format_t e
    | Some ctx ->
      Format.fprintf fmt "@[<hov 2>%a%a%a%a@]" EnumName.format_t e punctuation
        "["
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ %a@ " punctuation "|")
           (fun fmt (case, mty) ->
             Format.fprintf fmt "%a%a@ %a" enum_constructor case punctuation ":"
               typ mty))
        (EnumConstructor.Map.bindings (EnumName.Map.find e ctx.ctx_enums))
        punctuation "]")
  | TOption t -> Format.fprintf fmt "@[<hov 2>%a@ %a@]" base_type "option" typ t
  | TArrow ([t1], t2) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" typ_with_parens t1 op_style "→"
      typ t2
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@ %a@ %a@]" op_style "("
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " op_style ",")
         typ_with_parens)
      t1 op_style ")" op_style "→" typ t2
  | TArray t1 ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" base_type "collection" typ t1
  | TAny -> base_type fmt "any"

let lit (fmt : Format.formatter) (l : lit) : unit =
  match l with
  | LBool b -> lit_style fmt (string_of_bool b)
  | LInt i -> lit_style fmt (Runtime.integer_to_string i)
  | LUnit -> lit_style fmt "()"
  | LRat i ->
    lit_style fmt
      (Runtime.decimal_to_string ~max_prec_digits:!Cli.max_prec_digits i)
  | LMoney e -> (
    match !Cli.locale_lang with
    | En -> lit_style fmt (Format.asprintf "$%s" (Runtime.money_to_string e))
    | Fr -> lit_style fmt (Format.asprintf "%s €" (Runtime.money_to_string e))
    | Pl -> lit_style fmt (Format.asprintf "%s PLN" (Runtime.money_to_string e))
    )
  | LDate d -> lit_style fmt (Runtime.date_to_string d)
  | LDuration d -> lit_style fmt (Runtime.duration_to_string d)

let log_entry (fmt : Format.formatter) (entry : log_entry) : unit =
  Format.fprintf fmt "@<2>%a"
    (fun fmt -> function
      | VarDef _ -> Cli.format_with_style [ANSITerminal.blue] fmt "≔ "
      | BeginCall -> Cli.format_with_style [ANSITerminal.yellow] fmt "→ "
      | EndCall -> Cli.format_with_style [ANSITerminal.yellow] fmt "← "
      | PosRecordIfTrueBool ->
        Cli.format_with_style [ANSITerminal.green] fmt "☛ ")
    entry

let operator_to_string : type a. a Op.t -> string =
  let open Op in
  function
  | Not -> "~"
  | Length -> "length"
  | GetDay -> "get_day"
  | GetMonth -> "get_month"
  | GetYear -> "get_year"
  | FirstDayOfMonth -> "first_day_of_month"
  | LastDayOfMonth -> "last_day_of_month"
  | ToRat -> "to_rat"
  | ToRat_int -> "to_rat_int"
  | ToRat_mon -> "to_rat_mon"
  | ToMoney -> "to_mon"
  | ToMoney_rat -> "to_mon_rat"
  | Round -> "round"
  | Round_rat -> "round_rat"
  | Round_mon -> "round_mon"
  | Log _ -> "Log"
  | Minus -> "-"
  | Minus_int -> "-!"
  | Minus_rat -> "-."
  | Minus_mon -> "-$"
  | Minus_dur -> "-^"
  | And -> "&&"
  | Or -> "||"
  | Xor -> "xor"
  | Eq -> "="
  | Map -> "map"
  | Reduce -> "reduce"
  | Concat -> "++"
  | Filter -> "filter"
  | Add -> "+"
  | Add_int_int -> "+!"
  | Add_rat_rat -> "+."
  | Add_mon_mon -> "+$"
  | Add_dat_dur AbortOnRound -> "+@"
  | Add_dat_dur RoundUp -> "+@u"
  | Add_dat_dur RoundDown -> "+@d"
  | Add_dur_dur -> "+^"
  | Sub -> "-"
  | Sub_int_int -> "-!"
  | Sub_rat_rat -> "-."
  | Sub_mon_mon -> "-$"
  | Sub_dat_dat -> "-@"
  | Sub_dat_dur -> "-@^"
  | Sub_dur_dur -> "-^"
  | Mult -> "*"
  | Mult_int_int -> "*!"
  | Mult_rat_rat -> "*."
  | Mult_mon_rat -> "*$"
  | Mult_dur_int -> "*^"
  | Div -> "/"
  | Div_int_int -> "/!"
  | Div_rat_rat -> "/."
  | Div_mon_mon -> "/$"
  | Div_mon_rat -> "/$."
  | Div_dur_dur -> "/^"
  | Lt -> "<"
  | Lt_int_int -> "<!"
  | Lt_rat_rat -> "<."
  | Lt_mon_mon -> "<$"
  | Lt_dur_dur -> "<^"
  | Lt_dat_dat -> "<@"
  | Lte -> "<="
  | Lte_int_int -> "<=!"
  | Lte_rat_rat -> "<=."
  | Lte_mon_mon -> "<=$"
  | Lte_dur_dur -> "<=^"
  | Lte_dat_dat -> "<=@"
  | Gt -> ">"
  | Gt_int_int -> ">!"
  | Gt_rat_rat -> ">."
  | Gt_mon_mon -> ">$"
  | Gt_dur_dur -> ">^"
  | Gt_dat_dat -> ">@"
  | Gte -> ">="
  | Gte_int_int -> ">=!"
  | Gte_rat_rat -> ">=."
  | Gte_mon_mon -> ">=$"
  | Gte_dur_dur -> ">=^"
  | Gte_dat_dat -> ">=@"
  | Eq_int_int -> "=!"
  | Eq_rat_rat -> "=."
  | Eq_mon_mon -> "=$"
  | Eq_dur_dur -> "=^"
  | Eq_dat_dat -> "=@"
  | Fold -> "fold"
  | HandleDefault -> "handle_default"
  | HandleDefaultOpt -> "handle_default_opt"

let operator_to_shorter_string : type a. a Op.t -> string =
  let open Op in
  function
  | Not -> "~"
  | Length -> "length"
  | GetDay -> "get_day"
  | GetMonth -> "get_month"
  | GetYear -> "get_year"
  | FirstDayOfMonth -> "first_day_of_month"
  | LastDayOfMonth -> "last_day_of_month"
  | ToRat_int | ToRat_mon | ToRat -> "to_rat"
  | ToMoney_rat | ToMoney -> "to_mon"
  | Round_rat | Round_mon | Round -> "round"
  | Log _ -> "Log"
  | Minus_int | Minus_rat | Minus_mon | Minus_dur | Minus -> "-"
  | And -> "&&"
  | Or -> "||"
  | Xor -> "xor"
  | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dur_dur | Eq_dat_dat | Eq -> "="
  | Map -> "map"
  | Reduce -> "reduce"
  | Concat -> "++"
  | Filter -> "filter"
  | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _ | Add_dur_dur | Add
    ->
    "+"
  | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur
  | Sub_dur_dur | Sub ->
    "-"
  | Mult_int_int | Mult_rat_rat | Mult_mon_rat | Mult_dur_int | Mult -> "*"
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_rat | Div_dur_dur | Div ->
    "/"
  | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dur_dur | Lt_dat_dat | Lt -> "<"
  | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dur_dur | Lte_dat_dat | Lte ->
    "<="
  | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dur_dur | Gt_dat_dat | Gt -> ">"
  | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dur_dur | Gte_dat_dat | Gte ->
    ">="
  | Fold -> "fold"
  | HandleDefault -> "handle_default"
  | HandleDefaultOpt -> "handle_default_opt"

let operator : type a. ?debug:bool -> Format.formatter -> a operator -> unit =
 fun ?(debug = true) fmt op ->
  let open Op in
  match op with
  | Log (entry, infos) ->
    Format.fprintf fmt "%a%a%a%a"
      (Cli.format_with_style [ANSITerminal.blue])
      "#{" log_entry entry
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> punctuation fmt ".")
         (fun fmt info ->
           Cli.format_with_style [ANSITerminal.blue] fmt
             (Uid.MarkedString.to_string info)))
      infos
      (Cli.format_with_style [ANSITerminal.blue])
      "}"
  | op ->
    op_style fmt
      (if debug then operator_to_string op else operator_to_shorter_string op)

let except (fmt : Format.formatter) (exn : except) : unit =
  op_style fmt
    (match exn with
    | EmptyError -> "EmptyError"
    | ConflictError -> "ConflictError"
    | Crash -> "Crash"
    | NoValueProvided -> "NoValueProvided")

let var_debug fmt v =
  Format.fprintf fmt "%s_%d" (Bindlib.name_of v) (Bindlib.uid_of v)

let var fmt v = Format.pp_print_string fmt (Bindlib.name_of v)

(* Define precedence levels for auto parentheses *)
module Precedence = struct
  type op = Xor | And | Or | Comp | Mul | Div | Add | Sub

  type t =
    | Contained
      (* No parens needed, the term has unambiguous beginning and end *)
    | Op of op
    | App (* Function application, right-associative *)
    | Abs (* lambda, *)
    | Dot (* *)

  let expr : type a. (a, 't) gexpr -> t =
   fun e ->
    match Marked.unmark e with
    | ELit _ -> Contained (* Todo: unop if < 0 *)
    | EApp { f = EOp { op; _ }, _; _ } -> (
      match op with
      | Not | GetDay | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth
      | Length | Log _ | Minus | Minus_int | Minus_rat | Minus_mon | Minus_dur
      | ToRat | ToRat_int | ToRat_mon | ToMoney | ToMoney_rat | Round
      | Round_rat | Round_mon ->
        App
      | And -> Op And
      | Or -> Op Or
      | Xor -> Op Xor
      | Eq | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dur_dur | Eq_dat_dat ->
        Op Comp
      | Lt | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur ->
        Op Comp
      | Lte | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat
      | Lte_dur_dur ->
        Op Comp
      | Gt | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur ->
        Op Comp
      | Gte | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat
      | Gte_dur_dur ->
        Op Comp
      | Add | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _
      | Add_dur_dur ->
        Op Add
      | Sub | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat
      | Sub_dat_dur | Sub_dur_dur ->
        Op Sub
      | Mult | Mult_int_int | Mult_rat_rat | Mult_mon_rat | Mult_dur_int ->
        Op Mul
      | Div | Div_int_int | Div_rat_rat | Div_mon_rat | Div_mon_mon
      | Div_dur_dur ->
        Op Div
      | HandleDefault | HandleDefaultOpt | Map | Concat | Filter | Reduce | Fold
        ->
        App)
    | EApp _ -> App
    | EOp _ -> Contained
    | EArray _ -> Contained
    | EVar _ -> Contained
    | EAbs _ -> Abs
    | EIfThenElse _ -> Contained
    | EStruct _ -> Contained
    | EInj _ -> App
    | EMatch _ -> App
    | ETuple _ -> Contained
    | ETupleAccess _ -> Dot
    | ELocation _ -> Contained
    | EScopeCall _ -> App
    | EDStructAccess _ | EStructAccess _ -> Dot
    | EAssert _ -> App
    | EDefault _ -> Contained
    | EEmptyError -> Contained
    | EErrorOnEmpty _ -> App
    | ERaise _ -> App
    | ECatch _ -> App

  let needs_parens ~context ?(rhs = false) e =
    match expr context, expr e with
    | _, Contained -> false
    | Dot, Dot -> rhs
    | _, Dot -> false
    | Dot, _ -> true
    | App, App -> not rhs
    | App, Op _ -> true
    | App, Abs -> true
    | Abs, _ -> false
    | Op a, Op b -> (
      match a, b with
      | _, Xor -> true
      | And, And | Or, Or -> false
      | And, Or | Or, And -> true
      | (And | Or | Xor), _ -> false
      | _, (And | Or | Comp) -> true
      | Comp, _ -> false
      | Add, (Add | Sub) -> false
      | Sub, (Add | Sub) -> rhs
      | (Add | Sub), (Mul | Div) -> false
      | (Mul | Div), (Add | Sub) -> true
      | Mul, (Mul | Div) -> false
      | Div, (Mul | Div) -> rhs)
    | Op _, App -> not rhs
    | Op _, _ -> true
    | Contained, _ -> false
end

let rec expr_aux :
    type a.
    debug:bool ->
    Bindlib.ctxt ->
    ANSITerminal.style list ->
    Format.formatter ->
    (a, 't) gexpr ->
    unit =
 fun ~debug bnd_ctx colors fmt e ->
  let exprb bnd_ctx colors e = expr_aux ~debug bnd_ctx colors e in
  let exprc colors e = exprb bnd_ctx colors e in
  let expr e = exprc colors e in
  let var = if debug then var_debug else var in
  let rec skip_log : type a. (a, 't) gexpr -> (a, 't) gexpr = function
    | EApp { f = EOp { op = Log _; _ }, _; args = [e] }, _ when not debug ->
      skip_log e
    | e -> e
  in
  let e = skip_log e in
  let paren ~rhs expr fmt e1 =
    if Precedence.needs_parens ~rhs ~context:e (skip_log e1) then (
      Format.pp_open_hvbox fmt 1;
      Cli.format_with_style [List.hd colors] fmt "(";
      expr (List.tl colors) fmt e1;
      Format.pp_close_box fmt ();
      Cli.format_with_style [List.hd colors] fmt ")")
    else expr colors fmt e1
  in
  let lhs ex = paren ~rhs:false ex in
  let rhs ex = paren ~rhs:true ex in
  match Marked.unmark e with
  | EVar v -> var fmt v
  | ETuple es ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@]" punctuation "("
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt e -> lhs exprc fmt e))
      es punctuation ")"
  | EArray es ->
    Format.fprintf fmt "@[<hv 2>%a %a@] %a" punctuation "["
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt e -> lhs exprc fmt e))
      es punctuation "]"
  | ETupleAccess { e; index; _ } ->
    lhs exprc fmt e;
    punctuation fmt ".";
    Format.pp_print_int fmt index
  | ELit l -> lit fmt l
  | EApp { f = EAbs _, _; _ } ->
    let rec pr bnd_ctx colors fmt = function
      | EApp { f = EAbs { binder; tys }, _; args }, _ ->
        let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
        let xs_tau = List.mapi (fun i tau -> xs.(i), tau) tys in
        let xs_tau_arg =
          List.map2 (fun (x, tau) arg -> x, tau, arg) xs_tau args
        in
        Format.pp_print_list
          (fun fmt (x, tau, arg) ->
            Format.fprintf fmt
              "@[<hv 2>@[<hov 4>%a %a %a@ %a@ %a@]@ %a@;<1 -2>%a@]" keyword
              "let" var x punctuation ":" (typ None) tau punctuation "="
              (exprc colors) arg keyword "in")
          fmt xs_tau_arg;
        Format.pp_print_cut fmt ();
        rhs (pr bnd_ctx) fmt body
      | e -> rhs (exprb bnd_ctx) fmt e
    in
    Format.pp_open_vbox fmt 0;
    pr bnd_ctx colors fmt e;
    Format.pp_close_box fmt ()
  | EAbs { binder; tys } ->
    let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
    let expr = exprb bnd_ctx in
    let xs_tau = List.mapi (fun i tau -> xs.(i), tau) tys in
    Format.fprintf fmt "@[<hv 0>%a @[<hv 2>%a@]@ @]%a@ %a" punctuation "λ"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun fmt (x, tau) ->
           punctuation fmt "(";
           Format.pp_open_hvbox fmt 2;
           var fmt x;
           punctuation fmt ":";
           Format.pp_print_space fmt ();
           typ None fmt tau;
           Format.pp_close_box fmt ();
           punctuation fmt ")"))
      xs_tau punctuation "→" (rhs expr) body
  | EApp { f = EOp { op = (Map | Filter) as op; _ }, _; args = [arg1; arg2] } ->
    Format.fprintf fmt "@[<hv 2>%a %a@ %a@]" (operator ~debug) op (lhs exprc)
      arg1 (rhs exprc) arg2
  | EApp { f = EOp { op = Log _ as op; _ }, _; args = [arg1] } ->
    Format.fprintf fmt "@[<hv 0>%a@ %a@]" (operator ~debug) op (rhs exprc) arg1
  | EApp { f = EOp { op = op0; _ }, _; args = [_; _] } ->
    let prec = Precedence.expr e in
    let rec pr colors fmt = function
      (* Flatten sequences of the same associative op *)
      | EApp { f = EOp { op; _ }, _; args = [arg1; arg2] }, _ when op = op0 -> (
        (match prec with
        | Op (And | Or | Mul | Add | Div | Sub) -> lhs pr fmt arg1
        | _ -> lhs exprc fmt arg1);
        Format.pp_print_space fmt ();
        (operator ~debug) fmt op;
        Format.pp_print_char fmt ' ';
        match prec with
        | Op (And | Or | Mul | Add) -> rhs pr fmt arg2
        | _ -> rhs exprc fmt arg2)
      | e -> exprc colors fmt e
    in
    Format.pp_open_hvbox fmt 0;
    pr colors fmt e;
    Format.pp_close_box fmt ()
  | EApp { f = EOp { op; _ }, _; args = [arg1] } ->
    Format.fprintf fmt "@[<hv 2>%a@ %a@]" (operator ~debug) op (rhs exprc) arg1
  | EApp { f; args } ->
    Format.fprintf fmt "@[<hv 2>%a@ %a@]" (lhs exprc) f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (rhs exprc))
      args
  | EIfThenElse _ ->
    let rec pr els fmt = function
      | EIfThenElse { cond; etrue; efalse }, _ ->
        Format.fprintf fmt "@[<hv 2>%a@ %a@]@ @[<hv 2>%a@ %a@]@ %a" keyword
          (if els then "else if" else "if")
          expr cond keyword "then" expr etrue (pr true) efalse
      | e -> Format.fprintf fmt "@[<hv 2>%a@ %a@]" keyword "else" (rhs exprc) e
    in
    Format.pp_open_hvbox fmt 0;
    pr false fmt e;
    Format.pp_close_box fmt ()
  | EOp { op; _ } -> operator ~debug fmt op
  | EDefault { excepts; just; cons } ->
    if List.length excepts = 0 then
      Format.fprintf fmt "@[<hv 1>%a%a@ %a %a%a@]" punctuation "⟨" expr just
        punctuation "⊢" expr cons punctuation "⟩"
    else
      Format.fprintf fmt
        "@[<hv 0>@[<hov 2>%a %a@]@ @[<hov 2>%a %a@ %a %a@] %a@]" punctuation "⟨"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ",")
           (lhs exprc))
        excepts punctuation "|" expr just punctuation "⊢" expr cons punctuation
        "⟩"
  | EEmptyError -> lit_style fmt "∅"
  | EErrorOnEmpty e' ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" op_style "error_empty" (rhs exprc) e'
  | EAssert e' ->
    Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" keyword "assert" punctuation "("
      (rhs exprc) e' punctuation ")"
  | ECatch { body; exn; handler } ->
    Format.fprintf fmt "@[<hv 0>@[<hov 2>%a@ %a@]@ @[<hov 2>%a@ %a ->@ %a@]@]"
      keyword "try" expr body keyword "with" except exn (rhs exprc) handler
  | ERaise exn ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" keyword "raise" except exn
  | ELocation loc -> location fmt loc
  | EDStructAccess { e; field; _ } ->
    Format.fprintf fmt "@[<hv 2>%a%a@,%a%a%a@]" (lhs exprc) e punctuation "."
      punctuation "\"" IdentName.format_t field punctuation "\""
  | EStruct { name; fields } ->
    if StructField.Map.is_empty fields then (
      punctuation fmt "{";
      StructName.format_t fmt name;
      punctuation fmt "}")
    else
      Format.fprintf fmt "@[<hv 2>%a %a@ %a@;<1 -2>%a@]" punctuation "{"
        StructName.format_t name
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           (fun fmt (field_name, field_expr) ->
             Format.fprintf fmt "@[<hv 2>%a %a@ %a%a@]" struct_field field_name
               punctuation "=" (lhs exprc) field_expr punctuation ";"))
        (StructField.Map.bindings fields)
        punctuation "}"
  | EStructAccess { e; field; _ } ->
    Format.fprintf fmt "@[<hv 2>%a%a@,%a@]" (lhs exprc) e punctuation "."
      struct_field field
  | EInj { e; cons; _ } ->
    Format.fprintf fmt "@[<hv 2>%a@ %a@]" EnumConstructor.format_t cons
      (rhs exprc) e
  | EMatch { e; cases; _ } ->
    Format.fprintf fmt "@[<v 0>@[<hv 2>%a@ %a@ %a@]@ %a@]" keyword "match"
      (lhs exprc) e keyword "with"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt (cons_name, case_expr) ->
           match case_expr with
           | EAbs { binder; _ }, _ ->
             let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
             let expr = exprb bnd_ctx in
             Format.fprintf fmt "@[<hov 2>%a %a@ %a@ %a@ %a@]" punctuation "|"
               enum_constructor cons_name
               (Format.pp_print_seq ~pp_sep:Format.pp_print_space var)
               (Array.to_seq xs) punctuation "→" (rhs expr) body
           | e ->
             Format.fprintf fmt "@[<hov 2>%a %a@ %a@ %a@]" punctuation "|"
               enum_constructor cons_name punctuation "→" (rhs exprc) e))
      (EnumConstructor.Map.bindings cases)
  | EScopeCall { scope; args } ->
    Format.pp_open_hovbox fmt 2;
    ScopeName.format_t fmt scope;
    Format.pp_print_space fmt ();
    keyword fmt "of";
    Format.pp_print_space fmt ();
    Format.pp_open_hvbox fmt 2;
    punctuation fmt "{";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ";")
      (fun fmt (field_name, field_expr) ->
        Format.fprintf fmt "%a%a%a%a@ %a" punctuation "\"" ScopeVar.format_t
          field_name punctuation "\"" punctuation "=" (rhs exprc) field_expr)
      fmt
      (ScopeVar.Map.bindings args);
    Format.pp_close_box fmt ();
    punctuation fmt "}";
    Format.pp_close_box fmt ()

let rec colors =
  ANSITerminal.blue
  :: ANSITerminal.cyan
  :: ANSITerminal.green
  :: ANSITerminal.yellow
  :: ANSITerminal.red
  :: ANSITerminal.magenta
  :: colors

let typ_debug = typ None
let typ ctx = typ (Some ctx)

let expr ?(debug = !Cli.debug_flag) () ppf e =
  expr_aux ~debug Bindlib.empty_ctxt colors ppf e

let scope_let_kind ?debug:(_debug = true) _ctx fmt k =
  match k with
  | DestructuringInputStruct -> keyword fmt "get"
  | ScopeVarDefinition -> keyword fmt "set"
  | SubScopeVarDefinition -> keyword fmt "sub_set"
  | CallingSubScope -> keyword fmt "call"
  | DestructuringSubScopeResults -> keyword fmt "sub_get"
  | Assertion -> keyword fmt "assert"

let[@ocamlformat "disable"] rec
  scope_body_expr ?(debug = false) ctx fmt b : unit =
  match b with
  | Result e -> Format.fprintf fmt "%a %a" keyword "return" (expr ~debug ()) e
  | ScopeLet
      {
        scope_let_kind = kind;
        scope_let_typ;
        scope_let_expr;
        scope_let_next;
        _;
      } ->
    let x, next = Bindlib.unbind scope_let_next in
    Format.fprintf fmt
      "@[<hv 2>@[<hov 4>%a %a %a %a@ %a@ %a@]@ %a@;<1 -2>%a@]@,%a"
      keyword "let"
      (scope_let_kind ~debug ctx) kind
      (if debug then var_debug else var) x
      punctuation ":"
      (typ ctx) scope_let_typ
      punctuation "="
      (expr ~debug ()) scope_let_expr
      keyword "in"
      (scope_body_expr ~debug ctx) next

let scope_body ?(debug = false) ctx fmt (n, l) : unit =
  let {
    scope_body_input_struct;
    scope_body_output_struct;
    scope_body_expr = body;
  } =
    l
  in

  let input_typ = TStruct scope_body_input_struct, Pos.no_pos in
  let output_typ = TStruct scope_body_output_struct, Pos.no_pos in

  let x, body = Bindlib.unbind body in

  let () =
    Format.pp_open_vbox fmt 2;
    let () =
      Format.pp_open_hvbox fmt 2;
      let () =
        Format.pp_open_hovbox fmt 4;
        keyword fmt "let scope";
        Format.pp_print_space fmt ();
        ScopeName.format_t fmt n;
        Format.pp_close_box fmt ()
      in
      Format.pp_print_space fmt ();
      punctuation fmt "(";
      let () =
        Format.pp_open_hvbox fmt 2;
        (if debug then var_debug else var) fmt x;
        punctuation fmt ":";
        Format.pp_print_space fmt ();
        (if debug then typ_debug else typ ctx) fmt input_typ;
        punctuation fmt ")";
        Format.pp_close_box fmt ()
      in
      Format.pp_print_cut fmt ();
      punctuation fmt ":";
      Format.pp_print_string fmt " ";
      let () =
        Format.pp_open_hvbox fmt 2;
        (if debug then typ_debug else typ ctx) fmt output_typ;
        Format.pp_close_box fmt ()
      in
      Format.pp_print_space fmt ();
      punctuation fmt "=";
      Format.pp_close_box fmt ()
    in

    Format.pp_print_cut fmt ();

    scope_body_expr ~debug ctx fmt body;
    Format.pp_close_box fmt ()
  in
  ()

let enum
    ?(debug = false)
    decl_ctx
    fmt
    ((n, c) : EnumName.t * typ EnumConstructor.Map.t) =
  Format.fprintf fmt "@[<h 0>%a %a %a@;%a@]" keyword "type" EnumName.format_t n
    punctuation "="
    (fun fmt b ->
      ListLabels.iter b ~f:(fun (n, ty) ->
          Format.fprintf fmt "@[<hov2> %a %a %a %a@]@;" punctuation "|"
            EnumConstructor.format_t n keyword "of"
            (if debug then typ_debug else typ decl_ctx)
            ty))
    (EnumConstructor.Map.bindings c)

let struct_
    ?(debug = false)
    decl_ctx
    fmt
    ((n, c) : StructName.t * typ StructField.Map.t) =
  Format.fprintf fmt "@[<hv 0>@[<hv 2>@[<h>%a %a %a@;%a@]@;%a@]%a@]@;" keyword
    "type" StructName.format_t n punctuation "=" punctuation "{"
    (fun fmt b ->
      ListLabels.iter b ~f:(fun (n, ty) ->
          Format.fprintf fmt "@[<h 2>%a%a %a%a@]@ " StructField.format_t n
            keyword ":"
            (if debug then typ_debug else typ decl_ctx)
            ty punctuation ";"))
    (StructField.Map.bindings c)
    punctuation "}"

let decl_ctx ?(debug = false) decl_ctx (fmt : Format.formatter) (ctx : decl_ctx)
    : unit =
  let { ctx_enums; ctx_structs; _ } = ctx in

  Format.fprintf fmt "@[<v>%a@;@;%a@] @;"
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut (enum ~debug decl_ctx))
    (EnumName.Map.bindings ctx_enums)
    (Format.pp_print_list ~pp_sep:Format.pp_print_cut (struct_ ~debug decl_ctx))
    (StructName.Map.bindings ctx_structs)

let scope
    ?(debug : bool = false)
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    ((n, s) : ScopeName.t * 'm scope_body) : unit =
  Format.pp_open_vbox fmt 0;
  scope_body ~debug ctx fmt (n, s);
  Format.pp_close_box fmt ()

let code_item ?(debug = false) decl_ctx fmt c =
  match c with
  | ScopeDef (n, b) -> scope ~debug decl_ctx fmt (n, b)
  | Topdef (n, ty, e) ->
    Format.fprintf fmt "@[%a %a %a %a %a %a @]" keyword "let topval"
      TopdefName.format_t n op_style ":" (typ decl_ctx) ty op_style "="
      (expr ~debug ()) e

let rec code_item_list ?(debug = false) decl_ctx fmt c =
  match c with
  | Nil -> ()
  | Cons (c, b) ->
    let _x, cl = Bindlib.unbind b in

    Format.fprintf fmt "%a @.%a"
      (code_item ~debug decl_ctx)
      c
      (code_item_list ~debug decl_ctx)
      cl

let program ?(debug = false) fmt p =
  decl_ctx ~debug p.decl_ctx fmt p.decl_ctx;
  code_item_list ~debug p.decl_ctx fmt p.code_items
