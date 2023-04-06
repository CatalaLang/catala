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
  Cli.format_with_style [ANSITerminal.cyan] fmt s

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
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ %a@ " op_style "*")
         typ)
      ts
  | TStruct s -> (
    match ctx with
    | None -> Format.fprintf fmt "@[<hov 2>%a@]" StructName.format_t s
    | Some ctx ->
      Format.fprintf fmt "@[<hov 2> %a%a%a%a@]" punctuation "{"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ";")
           (fun fmt (field, mty) ->
             Format.fprintf fmt "%a%a%a%a@ %a" punctuation "\""
               StructField.format_t field punctuation "\"" punctuation ":" typ
               mty))
        (StructField.Map.bindings (StructName.Map.find s ctx.ctx_structs))
        punctuation "}_" StructName.format_t s)
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
  | TOption t ->
    Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" base_type "option" punctuation
      "(" typ t punctuation ")"
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
  | HandleDefault -> "handledefault"
  | HandleDefaultOpt -> "handledefault_opt"

let operator : type a. Format.formatter -> a Op.t -> unit =
 fun fmt op ->
  let open Op in
  match op with
  | Log (entry, infos) ->
    Format.fprintf fmt "%a@[<hov 2>[%a|%a]@]" op_style "log" log_entry entry
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ".")
         (fun fmt info -> Uid.MarkedString.format fmt info))
      infos
  | op -> Format.fprintf fmt "%a" op_style (operator_to_string op)

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

let needs_parens (type a) (e : (a, _) gexpr) : bool =
  match Marked.unmark e with EAbs _ | EStruct _ -> true | _ -> false

let rec expr_aux :
    type a.
    ?debug:bool ->
    decl_ctx option ->
    Bindlib.ctxt ->
    Format.formatter ->
    (a, 't) gexpr ->
    unit =
 fun ?(debug = false) ctx bnd_ctx fmt e ->
  let exprb bnd_ctx e = expr_aux ~debug ctx bnd_ctx e in
  let expr e = exprb bnd_ctx e in
  let var = if debug then var_debug else var in
  let rainbow =
    [
      ANSITerminal.white;
      ANSITerminal.red;
      ANSITerminal.blue;
      ANSITerminal.yellow;
      ANSITerminal.green;
      ANSITerminal.magenta;
      ANSITerminal.cyan;
    ]
  in
  let rainbow_state = ref 0 in
  let with_parens fmt e =
    if needs_parens e then (
      let floyd =
        incr rainbow_state;
        [
          (* ANSITerminal.Blink; *)
          List.nth rainbow (!rainbow_state mod List.length rainbow);
        ]
      in

      Cli.format_with_style floyd fmt "(";
      expr fmt e;
      Cli.format_with_style floyd fmt ")")
    else expr fmt e
  in
  match Marked.unmark e with
  | EVar v -> var fmt v
  | ETuple es ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@]" punctuation "("
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt e -> expr fmt e))
      es punctuation ")"
  | EArray es ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@]" punctuation "["
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt e -> expr fmt e))
      es punctuation "]"
  | ETupleAccess { e; index; _ } ->
    expr fmt e;
    punctuation fmt ".";
    Format.pp_print_int fmt index
  | ELit l -> lit fmt l
  | EApp { f = EAbs { binder; tys }, _; args } ->
    let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
    let expr = exprb bnd_ctx in
    let xs_tau = List.mapi (fun i tau -> xs.(i), tau) tys in
    let xs_tau_arg = List.map2 (fun (x, tau) arg -> x, tau, arg) xs_tau args in
    Format.fprintf fmt "%a%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
         (fun fmt (x, tau, arg) ->
           Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@ %a@ %a@]@\n" keyword
             "let" var x punctuation ":" (typ ctx) tau punctuation "=" expr arg
             keyword "in"))
      xs_tau_arg expr body
  | EAbs { binder; tys } ->
    let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
    let expr = exprb bnd_ctx in
    let xs_tau = List.mapi (fun i tau -> xs.(i), tau) tys in
    Format.fprintf fmt "@[<hov 2>%a @[<hov 2>%a@] %a@ %a@]" punctuation "λ"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (x, tau) ->
           Format.fprintf fmt "%a%a%a %a%a" punctuation "(" var x punctuation
             ":" (typ ctx) tau punctuation ")"))
      xs_tau punctuation "→" expr body
  | EApp { f = EOp { op = (Map | Filter) as op; _ }, _; args = [arg1; arg2] } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" operator op with_parens arg1
      with_parens arg2
  | EApp { f = EOp { op; _ }, _; args = [arg1; arg2] } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" with_parens arg1 operator op
      with_parens arg2
  | EApp { f = EOp { op = Log _; _ }, _; args = [arg1] } when not debug ->
    expr fmt arg1
  | EApp { f = EOp { op; _ }, _; args = [arg1] } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" operator op with_parens arg1
  | EApp { f; args } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" expr f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         with_parens)
      args
  | EIfThenElse { cond; etrue; efalse } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@ %a@]" keyword "if" expr
      cond keyword "then" expr etrue keyword "else" expr efalse
  | EOp { op; _ } -> operator fmt op
  | EDefault { excepts; just; cons } ->
    if List.length excepts = 0 then
      Format.fprintf fmt "@[<hov 2>%a%a@ %a@ %a%a@]" punctuation "⟨" expr just
        punctuation "⊢" expr cons punctuation "⟩"
    else
      Format.fprintf fmt "@[<hov 2>%a%a@ %a@ %a@ %a@ %a%a@]" punctuation "⟨"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ",")
           expr)
        excepts punctuation "|" expr just punctuation "⊢" expr cons punctuation
        "⟩"
  | EEmptyError -> lit_style fmt "∅ "
  | EErrorOnEmpty e' ->
    Format.fprintf fmt "%a@ %a" op_style "error_empty" with_parens e'
  | EAssert e' ->
    Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" keyword "assert" punctuation "("
      expr e' punctuation ")"
  | ECatch { body; exn; handler } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a ->@ %a@]" keyword "try"
      with_parens body keyword "with" except exn with_parens handler
  | ERaise exn ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" keyword "raise" except exn
  | ELocation loc -> location fmt loc
  | EDStructAccess { e; field; _ } ->
    Format.fprintf fmt "%a%a%a%a%a" expr e punctuation "." punctuation "\""
      IdentName.format_t field punctuation "\""
  | EStruct { name; fields } ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a%a@]" punctuation "{"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ";")
         (fun fmt (field_name, field_expr) ->
           Format.fprintf fmt "%a%a%a%a@ %a" punctuation "\""
             StructField.format_t field_name punctuation "\"" punctuation "="
             expr field_expr))
      (StructField.Map.bindings fields)
      punctuation "}_" StructName.format_t name
  | EStructAccess { e; field; _ } ->
    Format.fprintf fmt "%a%a%a%a%a" expr e punctuation "." punctuation "\""
      StructField.format_t field punctuation "\""
  | EInj { e; cons; _ } ->
    Format.fprintf fmt "%a@ %a" EnumConstructor.format_t cons with_parens e
  | EMatch { e; cases; _ } ->
    Format.fprintf fmt "@[<v 0>@[<hov 2>%a@ %a@]@ %a@ %a@]" keyword "match" expr
      e keyword "with"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt (cons_name, case_expr) ->
           Format.fprintf fmt "@[<hov 2>%a %a@ %a@ %a@]" punctuation "|"
             enum_constructor cons_name punctuation "→" expr case_expr))
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
          field_name punctuation "\"" punctuation "=" expr field_expr)
      fmt
      (ScopeVar.Map.bindings args);
    Format.pp_close_box fmt ();
    punctuation fmt "}";
    Format.pp_close_box fmt ()

let typ_debug = typ None
let typ ctx = typ (Some ctx)
let expr_debug ?debug = expr_aux ?debug None Bindlib.empty_ctxt
let expr ?debug ctx = expr_aux ?debug (Some ctx) Bindlib.empty_ctxt
