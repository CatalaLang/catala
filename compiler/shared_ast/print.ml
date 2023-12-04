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
  match Mark.remove ty with TArrow _ | TArray _ -> true | _ -> false

let uid_list (fmt : Format.formatter) (infos : Uid.MarkedString.info list) :
    unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_char fmt '.')
    (fun fmt info ->
      Format.fprintf fmt
        (if String.begins_with_uppercase (Mark.remove info) then "@{<red>%s@}"
         else "%s")
        (Uid.MarkedString.to_string info))
    fmt infos

let with_color f color fmt x =
  (* equivalent to [Format.fprintf fmt "@{<color>%s@}" s] *)
  Format.pp_open_stag fmt Ocolor_format.(Ocolor_style_tag (Fg (C4 color)));
  f fmt x;
  Format.pp_close_stag fmt ()

let pp_color_string = with_color Format.pp_print_string

(* Cyclic list used to choose nested paren colors *)
let rec colors =
  let open Ocolor_types in
  blue :: cyan :: green :: yellow :: red :: magenta :: colors

let keyword (fmt : Format.formatter) (s : string) : unit =
  pp_color_string Ocolor_types.red fmt s

let base_type (fmt : Format.formatter) (s : string) : unit =
  pp_color_string Ocolor_types.yellow fmt s

let punctuation (fmt : Format.formatter) (s : string) : unit =
  with_color (fun fmt -> Format.pp_print_as fmt 1) Ocolor_types.cyan fmt s

let op_style (fmt : Format.formatter) (s : string) : unit =
  pp_color_string Ocolor_types.green fmt s

let lit_style (fmt : Format.formatter) (s : string) : unit =
  pp_color_string Ocolor_types.yellow fmt s

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
  | DesugaredScopeVar { name; _ } -> ScopeVar.format fmt (Mark.remove name)
  | ScopelangScopeVar { name; _ } -> ScopeVar.format fmt (Mark.remove name)
  | SubScopeVar { alias = subindex; var = subvar; _ } ->
    Format.fprintf fmt "%a.%a" SubScopeName.format (Mark.remove subindex)
      ScopeVar.format (Mark.remove subvar)
  | ToplevelVar { name } -> TopdefName.format fmt (Mark.remove name)

let external_ref fmt er =
  match Mark.remove er with
  | External_value v -> TopdefName.format fmt v
  | External_scope s -> ScopeName.format fmt s

let rec typ_gen
    (ctx : decl_ctx option)
    ~(colors : Ocolor_types.color4 list)
    (fmt : Format.formatter)
    (ty : typ) : unit =
  let typ = typ_gen ctx in
  let typ_with_parens ~colors (fmt : Format.formatter) (t : typ) =
    if typ_needs_parens t then (
      Format.pp_open_hvbox fmt 1;
      pp_color_string (List.hd colors) fmt "(";
      typ ~colors:(List.tl colors) fmt t;
      Format.pp_close_box fmt ();
      pp_color_string (List.hd colors) fmt ")")
    else typ ~colors fmt t
  in
  match Mark.remove ty with
  | TLit l -> tlit fmt l
  | TTuple ts ->
    Format.pp_open_hvbox fmt 2;
    pp_color_string (List.hd colors) fmt "(";
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt " %a@ " op_style "*")
       (typ ~colors:(List.tl colors)))
      fmt ts;
    Format.pp_close_box fmt ();
    pp_color_string (List.hd colors) fmt ")"
  | TStruct s -> (
    match ctx with
    | None -> StructName.format fmt s
    | Some ctx ->
      let fields = StructName.Map.find s ctx.ctx_structs in
      if StructField.Map.is_empty fields then StructName.format fmt s
      else
        Format.fprintf fmt "@[<hv 2>%a %a@,%a@;<0 -2>%a@]" StructName.format s
          (pp_color_string (List.hd colors))
          "{"
          (StructField.Map.format_bindings
             ~pp_sep:(fun fmt () ->
               op_style fmt ";";
               Format.pp_print_space fmt ())
             (fun fmt pp_field_name field_typ ->
               Format.fprintf fmt "@[<hv 2>%t%a@ %a@]" pp_field_name punctuation
                 ":"
                 (typ ~colors:(List.tl colors))
                 field_typ))
          fields
          (pp_color_string (List.hd colors))
          "}")
  | TEnum e -> (
    match ctx with
    | None -> Format.fprintf fmt "@[<hov 2>%a@]" EnumName.format e
    | Some ctx ->
      let def = EnumName.Map.find e ctx.ctx_enums in
      Format.fprintf fmt "@[<hov 2>%a%a%a%a@]" EnumName.format e punctuation "["
        (EnumConstructor.Map.format_bindings
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ %a@ " punctuation "|")
           (fun fmt pp_case mty ->
             Format.fprintf fmt "%t%a@ %a" pp_case punctuation ":" (typ ~colors)
               mty))
        def punctuation "]")
  | TOption t ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" base_type "eoption" (typ ~colors) t
  | TArrow ([t1], t2) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" (typ_with_parens ~colors) t1
      op_style "→" (typ ~colors) t2
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>%a%a%a@ %a@ %a@]"
      (pp_color_string (List.hd colors))
      "("
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " op_style ",")
         (typ_with_parens ~colors:(List.tl colors)))
      t1
      (pp_color_string (List.hd colors))
      ")" op_style "→"
      (typ ~colors:(List.tl colors))
      t2
  | TArray t1 ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" base_type "collection" (typ ~colors)
      t1
  | TDefault t1 ->
    punctuation fmt "⟨";
    typ ~colors fmt t1;
    punctuation fmt "⟩"
  | TAny -> base_type fmt "any"
  | TClosureEnv -> base_type fmt "closure_env"

let typ_debug = typ_gen None ~colors
let typ ctx = typ_gen (Some ctx) ~colors

let lit (fmt : Format.formatter) (l : lit) : unit =
  match l with
  | LBool b -> lit_style fmt (string_of_bool b)
  | LInt i -> lit_style fmt (Runtime.integer_to_string i)
  | LUnit -> lit_style fmt "()"
  | LRat i ->
    lit_style fmt
      (Runtime.decimal_to_string ~max_prec_digits:Cli.globals.max_prec_digits i)
  | LMoney e ->
    lit_style fmt (Format.asprintf "¤%s" (Runtime.money_to_string e))
  | LDate d -> lit_style fmt (Runtime.date_to_string d)
  | LDuration d -> lit_style fmt (Runtime.duration_to_string d)

let log_entry (fmt : Format.formatter) (entry : log_entry) : unit =
  match entry with
  | VarDef _ -> Format.fprintf fmt "@{<blue>@<1>%s @}" "≔"
  | BeginCall -> Format.fprintf fmt "@{<yellow>@<1>%s @}" "→"
  | EndCall -> Format.fprintf fmt "@{<yellow>@<1>%s @}" "←"
  | PosRecordIfTrueBool -> Format.fprintf fmt "@{<green>@<1>%s @}" "☛"

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
  | ToClosureEnv -> "to_closure_env"
  | FromClosureEnv -> "from_closure_env"

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
  | ToClosureEnv -> "to_closure_env"
  | FromClosureEnv -> "from_closure_env"

let operator : type a. ?debug:bool -> Format.formatter -> a operator -> unit =
 fun ?(debug = true) fmt op ->
  let open Op in
  match op with
  | Log (entry, infos) ->
    Format.fprintf fmt "@{<blue>#{@}%a%a@{<blue>}@}" log_entry entry
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> punctuation fmt ".")
         (fun fmt info ->
           Format.fprintf fmt "@{<blue>%s@}" (Uid.MarkedString.to_string info)))
      infos
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
    match Mark.remove e with
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
      | ToClosureEnv | FromClosureEnv ->
        App)
    | EApp _ -> App
    | EOp _ -> Contained
    | EArray _ -> Contained
    | EVar _ -> Contained
    | EExternal _ -> Contained
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
    | EPureDefault _ -> Contained
    | EEmptyError -> Contained
    | EErrorOnEmpty _ -> App
    | ERaise _ -> App
    | ECatch _ -> App
    | ECustom _ -> Contained

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

module type EXPR_PARAM = sig
  val bypass : Format.formatter -> ('a, 't) gexpr -> bool
  val operator : Format.formatter -> 'a operator -> unit
  val var : Format.formatter -> ('a, 't) gexpr Var.t -> unit
  val lit : Format.formatter -> lit -> unit
  val pre_map : ('a, 't) gexpr -> ('a, 't) gexpr
end

module ExprGen (C : EXPR_PARAM) = struct
  let rec expr_aux :
      type a.
      Bindlib.ctxt ->
      Ocolor_types.color4 list ->
      Format.formatter ->
      (a, 't) gexpr ->
      unit =
   fun bnd_ctx colors fmt e ->
    let exprb bnd_ctx colors e = expr_aux bnd_ctx colors e in
    let exprc colors e = exprb bnd_ctx colors e in
    let expr e = exprc colors e in
    let var = C.var in
    let operator = C.operator in
    let e = C.pre_map e in
    let paren ~rhs ?(colors = colors) expr fmt e1 =
      if Precedence.needs_parens ~rhs ~context:e (C.pre_map e1) then (
        Format.pp_open_hvbox fmt 1;
        pp_color_string (List.hd colors) fmt "(";
        expr (List.tl colors) fmt e1;
        Format.pp_close_box fmt ();
        pp_color_string (List.hd colors) fmt ")")
      else expr colors fmt e1
    in
    let default_punct = with_color (fun fmt -> Format.pp_print_as fmt 1) in
    let lhs ?(colors = colors) ex = paren ~colors ~rhs:false ex in
    let rhs ex = paren ~rhs:true ex in
    if C.bypass fmt e then ()
    else
      match Mark.remove e with
      | EVar v -> var fmt v
      | EExternal { name } -> external_ref fmt name
      | ETuple es ->
        Format.fprintf fmt "@[<hov 2>%a%a%a@]"
          (pp_color_string (List.hd colors))
          "("
          (Format.pp_print_list
             ~pp_sep:(fun fmt () ->
               Format.fprintf fmt "%a@ " (pp_color_string (List.hd colors)) ",")
             (fun fmt e -> lhs ~colors:(List.tl colors) exprc fmt e))
          es
          (pp_color_string (List.hd colors))
          ")"
      | EArray es ->
        Format.fprintf fmt "@[<hv 2>%a@,@[<hov>%a@]@;<0 -2>%a@]" punctuation "["
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
             (fun fmt e -> lhs exprc fmt e))
          es punctuation "]"
      | ETupleAccess { e; index; _ } ->
        lhs exprc fmt e;
        punctuation fmt ".";
        Format.pp_print_int fmt index
      | ELit l -> C.lit fmt l
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
                  "let" var x punctuation ":" (typ_gen None ~colors) tau
                  punctuation "=" (exprc colors) arg keyword "in")
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
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             (fun fmt (x, tau) ->
               punctuation fmt "(";
               Format.pp_open_hvbox fmt 2;
               var fmt x;
               punctuation fmt ":";
               Format.pp_print_space fmt ();
               typ_gen None ~colors fmt tau;
               Format.pp_close_box fmt ();
               punctuation fmt ")"))
          xs_tau punctuation "→" (rhs expr) body
      | EApp
          { f = EOp { op = (Map | Filter) as op; _ }, _; args = [arg1; arg2] }
        ->
        Format.fprintf fmt "@[<hv 2>%a %a@ %a@]" operator op (lhs exprc) arg1
          (rhs exprc) arg2
      | EApp { f = EOp { op = Log _ as op; _ }, _; args = [arg1] } ->
        Format.fprintf fmt "@[<hv 0>%a@ %a@]" operator op (rhs exprc) arg1
      | EApp { f = EOp { op = op0; _ }, _; args = [_; _] } ->
        let prec = Precedence.expr e in
        let rec pr colors fmt = function
          (* Flatten sequences of the same associative op *)
          | EApp { f = EOp { op; _ }, _; args = [arg1; arg2] }, _ when op = op0
            -> (
            (match prec with
            | Op (And | Or | Mul | Add | Div | Sub) -> lhs pr fmt arg1
            | _ -> lhs exprc fmt arg1);
            Format.pp_print_space fmt ();
            operator fmt op;
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
        Format.fprintf fmt "@[<hv 2>%a@ %a@]" operator op (rhs exprc) arg1
      | EApp { f; args } ->
        Format.fprintf fmt "@[<hv 2>%a@ %a@]" (lhs exprc) f
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
             (rhs exprc))
          args
      | EIfThenElse _ ->
        let rec pr els fmt = function
          | EIfThenElse { cond; etrue; efalse }, _ ->
            Format.fprintf fmt "@[<hv 2>@[<hv 2>%a@ %a@;<1 -2>%a@]@ %a@]@ %a"
              keyword
              (if els then "else if" else "if")
              expr cond keyword "then" expr etrue (pr true) efalse
          | e ->
            Format.fprintf fmt "@[<hv 2>%a@ %a@]" keyword "else" (rhs exprc) e
        in
        Format.pp_open_hvbox fmt 0;
        pr false fmt e;
        Format.pp_close_box fmt ()
      | EOp { op; _ } -> operator fmt op
      | EDefault { excepts; just; cons } ->
        if List.length excepts = 0 then
          Format.fprintf fmt "@[<hv 1>%a%a@ %a %a%a@]"
            (default_punct (List.hd colors))
            "⟨"
            (exprc (List.tl colors))
            just
            (default_punct (List.hd colors))
            "⊢"
            (exprc (List.tl colors))
            cons
            (default_punct (List.hd colors))
            "⟩"
        else
          Format.fprintf fmt
            "@[<hv 0>@[<hov 2>%a %a@]@ @[<hov 2>%a %a@ %a %a@] %a@]"
            (default_punct (List.hd colors))
            "⟨"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () ->
                 Format.fprintf fmt "%a@ " (default_punct (List.hd colors)) ",")
               (lhs ~colors:(List.tl colors) exprc))
            excepts
            (default_punct (List.hd colors))
            "|"
            (exprc (List.tl colors))
            just
            (default_punct (List.hd colors))
            "⊢"
            (exprc (List.tl colors))
            cons
            (default_punct (List.hd colors))
            "⟩"
      | EPureDefault e ->
        Format.fprintf fmt "%a%a%a"
          (default_punct (List.hd colors))
          "⟨" expr e
          (default_punct (List.hd colors))
          "⟩"
      | EEmptyError -> lit_style fmt "∅"
      | EErrorOnEmpty e' ->
        Format.fprintf fmt "@[<hov 2>%a@ %a@]" op_style "error_empty"
          (rhs exprc) e'
      | EAssert e' ->
        Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" keyword "assert" punctuation
          "(" (rhs exprc) e' punctuation ")"
      | ECatch { body; exn; handler } ->
        Format.fprintf fmt
          "@[<hv 0>@[<hov 2>%a@ %a@]@ @[<hov 2>%a@ %a ->@ %a@]@]" keyword "try"
          expr body keyword "with" except exn (rhs exprc) handler
      | ERaise exn ->
        Format.fprintf fmt "@[<hov 2>%a@ %a@]" keyword "raise" except exn
      | ELocation loc -> location fmt loc
      | EDStructAccess { e; field; _ } ->
        Format.fprintf fmt "@[<hv 2>%a%a@,%a%a%a@]" (lhs exprc) e punctuation
          "." punctuation "\"" Ident.format field punctuation "\""
      | EStruct { name; fields } ->
        if StructField.Map.is_empty fields then (
          punctuation fmt "{";
          StructName.format fmt name;
          punctuation fmt "}")
        else
          Format.fprintf fmt "@[<hv 2>%a %a@ %a@;<1 -2>%a@]" punctuation "{"
            StructName.format name
            (StructField.Map.format_bindings ~pp_sep:Format.pp_print_space
               (fun fmt pp_field_name field_expr ->
                 Format.fprintf fmt "@[<hv 2>%t %a@ %a%a@]" pp_field_name
                   punctuation "=" (lhs exprc) field_expr punctuation ";"))
            fields punctuation "}"
      | EStructAccess { e; field; _ } ->
        Format.fprintf fmt "@[<hv 2>%a%a@,%a@]" (lhs exprc) e punctuation "."
          StructField.format field
      | EInj { e; cons; _ } ->
        Format.fprintf fmt "@[<hv 2>%a@ %a@]" EnumConstructor.format cons
          (rhs exprc) e
      | EMatch { e; cases; _ } ->
        Format.fprintf fmt "@[<v 0>@[<hv 2>%a@ %a@;<1 -2>%a@]@ %a@]" keyword
          "match" (lhs exprc) e keyword "with"
          (EnumConstructor.Map.format_bindings
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
             (fun fmt pp_cons_name case_expr ->
               match case_expr with
               | EAbs { binder; _ }, _ ->
                 let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
                 let expr = exprb bnd_ctx in
                 Format.fprintf fmt "@[<hov 2>%a %t@ %a@ %a@ %a@]" punctuation
                   "|" pp_cons_name
                   (Format.pp_print_seq ~pp_sep:Format.pp_print_space var)
                   (Array.to_seq xs) punctuation "→" (rhs expr) body
               | e ->
                 Format.fprintf fmt "@[<hov 2>%a %t@ %a@ %a@]" punctuation "|"
                   pp_cons_name punctuation "→" (rhs exprc) e))
          cases
      | EScopeCall { scope; args } ->
        Format.pp_open_hovbox fmt 2;
        ScopeName.format fmt scope;
        Format.pp_print_space fmt ();
        keyword fmt "of";
        Format.pp_print_space fmt ();
        Format.pp_open_hvbox fmt 2;
        punctuation fmt "{";
        ScopeVar.Map.format_bindings
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " punctuation ";")
          (fun fmt pp_field_name field_expr ->
            Format.fprintf fmt "%a%t%a%a@ %a" punctuation "\"" pp_field_name
              punctuation "\"" punctuation "=" (rhs exprc) field_expr)
          fmt args;
        Format.pp_close_box fmt ();
        punctuation fmt "}";
        Format.pp_close_box fmt ()
      | ECustom _ -> Format.pp_print_string fmt "<obj>"

  let expr ppf e = expr_aux Bindlib.empty_ctxt colors ppf e
end

module ExprConciseParam = struct
  let bypass _ _ = false
  let operator o = operator ~debug:false o
  let var = var
  let lit = lit

  let rec pre_map : type a. (a, 't) gexpr -> (a, 't) gexpr = function
    | EApp { f = EOp { op = Log _; _ }, _; args = [e] }, _ -> pre_map e
    | e -> e
end

module ExprConcise = ExprGen (ExprConciseParam)

module ExprDebugParam = struct
  let bypass _ _ = false
  let operator o = operator ~debug:true o
  let var = var_debug
  let lit = lit
  let pre_map e = e
end

module ExprDebug = ExprGen (ExprDebugParam)

let expr ?(debug = Cli.globals.debug) () ppf e =
  if debug then ExprDebug.expr ppf e else ExprConcise.expr ppf e

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
        ScopeName.format fmt n;
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
    (pp_name : Format.formatter -> unit)
    (c : typ EnumConstructor.Map.t) =
  Format.fprintf fmt "@[<h 0>%a %t %a@ %a@]" keyword "type" pp_name punctuation
    "="
    (EnumConstructor.Map.format_bindings
       ~pp_sep:(fun _ _ -> ())
       (fun fmt pp_n ty ->
         Format.fprintf fmt "@[<hov2> %a %t %a %a@]@;" punctuation "|" pp_n
           keyword "of"
           (if debug then typ_debug else typ decl_ctx)
           ty))
    c

let struct_
    ?(debug = false)
    decl_ctx
    fmt
    (pp_name : Format.formatter -> unit)
    (c : typ StructField.Map.t) =
  Format.fprintf fmt "@[<hv 2>%a %t %a %a@ %a@;<1 -2>%a@]@," keyword "type"
    pp_name punctuation "=" punctuation "{"
    (StructField.Map.format_bindings ~pp_sep:Format.pp_print_space
       (fun fmt pp_n ty ->
         Format.fprintf fmt "@[<h 2>%t%a %a%a@]" pp_n keyword ":"
           (if debug then typ_debug else typ decl_ctx)
           ty punctuation ";"))
    c punctuation "}"

let decl_ctx ?(debug = false) decl_ctx (fmt : Format.formatter) (ctx : decl_ctx)
    : unit =
  let { ctx_enums; ctx_structs; _ } = ctx in
  Format.fprintf fmt "@[<v>%a@;@;%a@] @;"
    (EnumName.Map.format_bindings (enum ~debug decl_ctx))
    ctx_enums
    (StructName.Map.format_bindings (struct_ ~debug decl_ctx))
    ctx_structs

let scope
    ?(debug : bool = false)
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    ((n, s) : ScopeName.t * 'm scope_body) : unit =
  Format.pp_open_vbox fmt 0;
  scope_body ~debug ctx fmt (n, s);
  Format.pp_close_box fmt ()

let code_item ?(debug = false) ?name decl_ctx fmt c =
  match c with
  | ScopeDef (n, b) ->
    let n =
      match debug, name with
      | true, Some n -> ScopeName.fresh [] (n, Pos.no_pos)
      | _ -> n
    in
    scope ~debug decl_ctx fmt (n, b)
  | Topdef (n, ty, e) ->
    let n =
      match debug, name with
      | true, Some n -> TopdefName.fresh [] (n, Pos.no_pos)
      | _ -> n
    in
    Format.fprintf fmt "@[<v 2>@[<hov 2>%a@ %a@ %a@ %a@ %a@]@ %a@]" keyword
      "let topval" TopdefName.format n op_style ":" (typ decl_ctx) ty op_style
      "=" (expr ~debug ()) e

let rec code_item_list ?(debug = false) decl_ctx fmt c =
  match c with
  | Nil -> ()
  | Cons (c, b) ->
    let x, cl = Bindlib.unbind b in
    Format.fprintf fmt "%a @.%a"
      (code_item ~debug ~name:(Format.asprintf "%a" var_debug x) decl_ctx)
      c
      (code_item_list ~debug decl_ctx)
      cl

let program ?(debug = false) fmt p =
  decl_ctx ~debug p.decl_ctx fmt p.decl_ctx;
  code_item_list ~debug p.decl_ctx fmt p.code_items

(* - User-facing value printer - *)

module UserFacing = struct
  (* Refs:
     https://en.wikipedia.org/wiki/Wikipedia:Manual_of_Style/Dates_and_numbers#Grouping_of_digits
     https://fr.wikipedia.org/wiki/Wikip%C3%A9dia:Conventions_concernant_les_nombres#Pour_un_comptage_ou_une_mesure *)
  let bigsep (lang : Cli.backend_lang) =
    match lang with En -> ",", 3 | Fr -> " ", 3 | Pl -> ",", 3

  let decsep (lang : Cli.backend_lang) =
    match lang with En -> "." | Fr -> "," | Pl -> "."

  let unit (_lang : Cli.backend_lang) ppf () = Format.pp_print_string ppf "()"

  let bool (lang : Cli.backend_lang) ppf b =
    let s =
      match lang, b with
      | En, true -> "true"
      | En, false -> "false"
      | Fr, true -> "vrai"
      | Fr, false -> "faux"
      | Pl, true -> "prawda"
      | Pl, false -> "falsz"
    in
    Format.pp_print_string ppf s

  let integer (lang : Cli.backend_lang) ppf n =
    let sep, nsep = bigsep lang in
    let nsep = Z.pow (Z.of_int 10) nsep in
    if Z.sign n < 0 then Format.pp_print_char ppf '-';
    let rec aux n =
      let a, b = Z.div_rem n nsep in
      if Z.equal a Z.zero then Z.pp_print ppf b
      else (
        aux a;
        Format.fprintf ppf "%s%03d" sep (Z.to_int b))
    in
    aux (Z.abs n)

  let money (lang : Cli.backend_lang) ppf n =
    (match lang with En -> Format.pp_print_string ppf "$" | Fr | Pl -> ());
    let units, cents = Z.div_rem n (Z.of_int 100) in
    integer lang ppf units;
    Format.pp_print_string ppf (decsep lang);
    Format.fprintf ppf "%02d" (Z.to_int (Z.abs cents));
    match lang with
    | En -> ()
    | Fr -> Format.pp_print_string ppf " €"
    | Pl -> Format.pp_print_string ppf " PLN"

  let decimal (lang : Cli.backend_lang) ppf r =
    let den = Q.den r in
    let int_part, rem = Z.div_rem (Q.num r) den in
    let rem = Z.abs rem in
    (* Printing the integer part *)
    integer lang ppf int_part;
    (* Printing the decimals *)
    let bigsep, nsep = bigsep lang in
    let rec aux ndigit rem_digits rem =
      let n, rem = Z.div_rem (Z.mul rem (Z.of_int 10)) den in
      let rem_digits, stop =
        match rem_digits with
        | None ->
          if Z.equal n Z.zero then None, false
          else
            let r = Cli.globals.max_prec_digits in
            Some (r - 1), r <= 1
        | Some r -> Some (r - 1), r <= 1
      in
      if ndigit mod nsep = 0 then
        Format.pp_print_string ppf (if ndigit = 0 then decsep lang else bigsep);
      Format.pp_print_int ppf (Z.to_int n);
      if Z.gt rem Z.zero then
        if stop then Format.pp_print_as ppf 1 "…"
        else aux (ndigit + 1) rem_digits rem
    in
    let rec ndigits n =
      if Z.equal n Z.zero then 0 else 1 + ndigits (Z.div n (Z.of_int 10))
    in
    aux 0
      (if Z.equal int_part Z.zero then None
       else Some (Cli.globals.max_prec_digits - ndigits int_part))
      rem
  (* It would be nice to print ratios as % but that's impossible to guess.
     Trying would lead to inconsistencies where some comparable numbers are in %
     and some others not, adding confusion. *)

  let date (lang : Cli.backend_lang) ppf d =
    let y, m, d = Dates_calc.Dates.date_to_ymd d in
    match lang with
    | En | Pl -> Format.fprintf ppf "%04d-%02d-%02d" y m d
    | Fr -> Format.fprintf ppf "%02d/%02d/%04d" d m y

  let duration (lang : Cli.backend_lang) ppf dr =
    let y, m, d = Dates_calc.Dates.period_to_ymds dr in
    let rec filter0 = function
      | (0, _) :: (_ :: _ as r) -> filter0 r
      | x :: r -> x :: List.filter (fun (n, _) -> n <> 0) r
      | [] -> []
    in
    let splur n s = if abs n > 1 then n, s ^ "s" else n, s in
    Format.pp_print_char ppf '[';
    (match lang with
    | En -> [splur y "year"; splur m "month"; splur d "day"]
    | Fr -> [splur y "an"; m, "mois"; splur d "jour"]
    | Pl -> [y, "rok"; m, "miesiac"; d, "dzien"])
    |> filter0
    |> Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
         (fun ppf (n, s) -> Format.fprintf ppf "%d %s" n s)
         ppf;
    Format.pp_print_char ppf ']'

  let lit_raw (lang : Cli.backend_lang) ppf lit : unit =
    match lit with
    | LUnit -> unit lang ppf ()
    | LBool b -> bool lang ppf b
    | LInt i -> integer lang ppf i
    | LRat r -> decimal lang ppf r
    | LMoney e -> money lang ppf e
    | LDate d -> date lang ppf d
    | LDuration dr -> duration lang ppf dr

  let lit_to_string (lang : Cli.backend_lang) lit =
    let buf = Buffer.create 32 in
    let ppf = Format.formatter_of_buffer buf in
    lit_raw lang ppf lit;
    Format.pp_print_flush ppf ();
    Buffer.contents buf

  let lit (lang : Cli.backend_lang) ppf lit : unit =
    with_color (lit_raw lang) Ocolor_types.yellow ppf lit

  let rec value :
      type a.
      ?fallback:(Format.formatter -> (a, 't) gexpr -> unit) ->
      Cli.backend_lang ->
      Format.formatter ->
      (a, 't) gexpr ->
      unit =
   fun ?(fallback = fun _ _ -> invalid_arg "UserPrint.value: not a value") lang
       ppf e ->
    match Mark.remove e with
    | ELit l -> lit lang ppf l
    | EArray l | ETuple l ->
      Format.fprintf ppf "@[<hv 2>[@,@[<hov>%a@]@;<0 -2>]@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
           (value ~fallback lang))
        l
    | EStruct { name; fields } ->
      Format.fprintf ppf "@[<hv 2>%a {@ %a@;<1 -2>}@]" StructName.format name
        (StructField.Map.format_bindings ~pp_sep:Format.pp_print_space
           (fun ppf pp_fld e ->
             Format.fprintf ppf "@[<hov 2>-- %t:@ %a@]" pp_fld
               (value ~fallback lang) e))
        fields
    | EInj { name = _; cons; e } ->
      Format.fprintf ppf "@[<hov 2>%a@ %a@]" EnumConstructor.format cons
        (value ~fallback lang) e
    | EEmptyError -> Format.pp_print_string ppf "ø"
    | EAbs _ -> Format.pp_print_string ppf "<function>"
    | EExternal _ -> Format.pp_print_string ppf "<external>"
    | EApp _ | EOp _ | EVar _ | EIfThenElse _ | EMatch _ | ETupleAccess _
    | EStructAccess _ | EAssert _ | EDefault _ | EPureDefault _
    | EErrorOnEmpty _ | ERaise _ | ECatch _ | ELocation _ | EScopeCall _
    | EDStructAccess _ | ECustom _ ->
      fallback ppf e

  (* This function is already in module [Expr], but [Expr] depends on this
     module *)
  let rec skip_wrappers : type a. (a, 'm) gexpr -> (a, 'm) gexpr = function
    | EApp { f = EOp { op = Log _; _ }, _; args = [e] }, _ -> skip_wrappers e
    | EApp { f = EApp { f = EOp { op = Log _; _ }, _; args = [f] }, _; args }, m
      ->
      skip_wrappers (EApp { f; args }, m)
    | EErrorOnEmpty e, _ -> skip_wrappers e
    | EDefault { excepts = []; just = ELit (LBool true), _; cons = e }, _ ->
      skip_wrappers e
    | e -> e

  let expr :
      type a. Cli.backend_lang -> Format.formatter -> (a, 't) gexpr -> unit =
   fun lang ->
    let rec aux_value : type a t. Format.formatter -> (a, t) gexpr -> unit =
     fun ppf e -> value ~fallback lang ppf e
    and fallback : type a t. Format.formatter -> (a, t) gexpr -> unit =
     fun ppf e ->
      let module E = ExprGen (struct
        let bypass : type a t. Format.formatter -> (a, t) gexpr -> bool =
         fun ppf e ->
          match Mark.remove e with
          | EArray _ | ETuple _ | EStruct _ | EInj _ | EEmptyError | EAbs _
          | EExternal _ ->
            aux_value ppf e;
            true
          | _ -> false

        let operator o = operator ~debug:false o
        let var = var
        let lit = lit lang
        let pre_map = skip_wrappers
      end) in
      E.expr ppf e
    in
    aux_value
end
