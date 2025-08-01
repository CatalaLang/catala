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
    | TDate -> "date"
    | TPos -> "source_position")

let location (type a) (fmt : Format.formatter) (l : a glocation) : unit =
  match l with
  | DesugaredScopeVar { name; _ } -> ScopeVar.format fmt (Mark.remove name)
  | ScopelangScopeVar { name; _ } -> ScopeVar.format fmt (Mark.remove name)
  | ToplevelVar { name; _ } -> TopdefName.format fmt (Mark.remove name)

let external_ref fmt er =
  match Mark.remove er with
  | External_value v -> TopdefName.format fmt v
  | External_scope s -> ScopeName.format fmt s

let attr ppf = function
  | Pos.Law_pos _ -> ()
  | Src (path, value, _pos) ->
    Format.fprintf ppf "#[%a"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '.')
         Format.pp_print_string)
      (Mark.remove path);
    (match value with
    | Unit -> ()
    | String (str, _) -> Format.fprintf ppf " = %S" str
    | _ -> Format.fprintf ppf " = <expr>");
    Format.fprintf ppf "]@ "
  | DebugPrint { label } ->
    Format.fprintf ppf "#[debug.print%a]@ "
      (fun ppf -> function
        | None -> ()
        | Some label -> Format.fprintf ppf " = %S" label)
      label
  | Test -> Format.fprintf ppf "#[test]@ "
  | _ -> Format.fprintf ppf "#[?]@ "

let attrs ppf x = List.iter (attr ppf) (Pos.attrs x)

let tvar ppf tv =
  let name = Bindlib.name_of tv in
  let name =
    if name.[0] = '\'' then
      let num = String.sub name 1 (String.length name - 1) in
      if num = "1" then "any type" else "any type (" ^ num ^ ")"
    else name
  in
  Format.fprintf ppf "@{<bold;yellow><%s%s>@}" name
    (if Global.options.debug then "_" ^ string_of_int (Bindlib.uid_of tv)
     else "")

let rec typ_gen :
    colors:Ocolor_types.color4 list ->
    Bindlib.ctxt ->
    Format.formatter ->
    typ ->
    unit =
 fun ~colors bctx fmt ty ->
  let typ_gen ?(colors = colors) ?(bctx = bctx) () = typ_gen ~colors bctx in
  let typ_with_parens ?(colors = colors) (fmt : Format.formatter) t =
    if typ_needs_parens t then (
      Format.pp_open_hvbox fmt 1;
      pp_color_string (List.hd colors) fmt "(";
      typ_gen ~colors:(List.tl colors) () fmt t;
      Format.pp_close_box fmt ();
      pp_color_string (List.hd colors) fmt ")")
    else typ_gen ~colors () fmt t
  in
  attrs fmt (Mark.get ty);
  match Mark.remove ty with
  | TLit l -> tlit fmt l
  | TTuple ts ->
    Format.pp_open_hvbox fmt 1;
    pp_color_string (List.hd colors) fmt "(";
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " op_style ",")
       (typ_gen ~colors:(List.tl colors) ()))
      fmt ts;
    Format.pp_close_box fmt ();
    pp_color_string (List.hd colors) fmt ")"
  | TStruct s -> StructName.format fmt s
  | TEnum e -> Format.fprintf fmt "@[<hov 2>%a@]" EnumName.format e
  | TOption t ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" base_type "option" (typ_gen ()) t
  | TArrow ([t1], t2) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" (typ_with_parens ~colors) t1
      op_style "→" (typ_gen ()) t2
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
      (typ_gen ~colors:(List.tl colors) ())
      t2
  | TArray t1 ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" base_type "list of" (typ_gen ()) t1
  | TDefault t1 ->
    punctuation fmt "⟨";
    typ_gen () fmt t1;
    punctuation fmt "⟩"
  | TVar tv -> tvar fmt tv
  | TForAll tb ->
    let tvs, ty, bctx = Bindlib.unmbind_in bctx tb in
    if Global.options.debug then
      Format.fprintf fmt "∀ %a .@ "
        (Format.pp_print_seq ~pp_sep:Format.pp_print_space tvar)
        (Array.to_seq tvs);
    typ_gen ~bctx () fmt ty
  | TClosureEnv -> base_type fmt "closure_env"

let typ ?(colors = colors) fmt ty = typ_gen ~colors Bindlib.empty_ctxt fmt ty

let lit (fmt : Format.formatter) (l : lit) : unit =
  match l with
  | LBool b -> lit_style fmt (string_of_bool b)
  | LInt i -> lit_style fmt (Runtime.integer_to_string i)
  | LUnit -> lit_style fmt "()"
  | LRat i ->
    lit_style fmt
      (Runtime.decimal_to_string ~max_prec_digits:Global.options.max_prec_digits
         i)
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
  | ToInt -> "to_int"
  | ToInt_rat -> "to_int_rat"
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
  | Map2 -> "map2"
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
  | Sub_dat_dur AbortOnRound -> "-@"
  | Sub_dat_dur RoundUp -> "-@^u"
  | Sub_dat_dur RoundDown -> "-@d"
  | Sub_dur_dur -> "-^"
  | Mult -> "*"
  | Mult_int_int -> "*!"
  | Mult_rat_rat -> "*."
  | Mult_mon_int -> "*$!"
  | Mult_mon_rat -> "*$."
  | Mult_dur_int -> "*^"
  | Div -> "/"
  | Div_int_int -> "/!"
  | Div_rat_rat -> "/."
  | Div_mon_mon -> "/$"
  | Div_mon_int -> "/$!"
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
  | Eq_boo_boo -> "=="
  | Eq_int_int -> "=!"
  | Eq_rat_rat -> "=."
  | Eq_mon_mon -> "=$"
  | Eq_dur_dur -> "=^"
  | Eq_dat_dat -> "=@"
  | Fold -> "fold"
  | HandleExceptions -> "handle_exceptions"
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
  | ToInt | ToInt_rat -> "to_int"
  | ToRat_int | ToRat_mon | ToRat -> "to_rat"
  | ToMoney_rat | ToMoney -> "to_mon"
  | Round_rat | Round_mon | Round -> "round"
  | Log _ -> "Log"
  | Minus_int | Minus_rat | Minus_mon | Minus_dur | Minus -> "-"
  | And -> "&&"
  | Or -> "||"
  | Xor -> "xor"
  | Eq_boo_boo | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dur_dur | Eq_dat_dat
  | Eq ->
    "="
  | Map -> "map"
  | Map2 -> "map2"
  | Reduce -> "reduce"
  | Concat -> "++"
  | Filter -> "filter"
  | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _ | Add_dur_dur | Add
    ->
    "+"
  | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur _
  | Sub_dur_dur | Sub ->
    "-"
  | Mult_int_int | Mult_rat_rat | Mult_mon_int | Mult_mon_rat | Mult_dur_int
  | Mult ->
    "*"
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int | Div_mon_rat
  | Div_dur_dur | Div ->
    "/"
  | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dur_dur | Lt_dat_dat | Lt -> "<"
  | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dur_dur | Lte_dat_dat | Lte ->
    "<="
  | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dur_dur | Gt_dat_dat | Gt -> ">"
  | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dur_dur | Gte_dat_dat | Gte ->
    ">="
  | Fold -> "fold"
  | HandleExceptions -> "handle_exceptions"
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

let runtime_error ppf err =
  Format.fprintf ppf "@{<red>%s@}" (Runtime.error_to_string err)

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
    | EAppOp { op; _ } -> (
      match Mark.remove op with
      | Not | GetDay | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth
      | Length | Log _ | Minus | Minus_int | Minus_rat | Minus_mon | Minus_dur
      | ToInt | ToInt_rat | ToRat | ToRat_int | ToRat_mon | ToMoney
      | ToMoney_rat | Round | Round_rat | Round_mon ->
        App
      | And -> Op And
      | Or -> Op Or
      | Xor -> Op Xor
      | Eq | Eq_boo_boo | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dur_dur
      | Eq_dat_dat ->
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
      | Sub_dat_dur _ | Sub_dur_dur ->
        Op Sub
      | Mult | Mult_int_int | Mult_rat_rat | Mult_mon_int | Mult_mon_rat
      | Mult_dur_int ->
        Op Mul
      | Div | Div_int_int | Div_rat_rat | Div_mon_int | Div_mon_rat
      | Div_mon_mon | Div_dur_dur ->
        Op Div
      | HandleExceptions | Map | Map2 | Concat | Filter | Reduce | Fold
      | ToClosureEnv | FromClosureEnv ->
        App)
    | EApp _ -> App
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
    | EDStructAmend _ -> App
    | EDStructAccess _ | EStructAccess _ -> Dot
    | EAssert _ -> App
    | EFatalError _ -> App
    | EDefault _ -> Contained
    | EPureDefault _ -> Contained
    | EEmpty -> Contained
    | EErrorOnEmpty _ -> App
    | EPos _ -> Contained
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
      type a t.
      Bindlib.ctxt ->
      Ocolor_types.color4 list ->
      Format.formatter ->
      (a, t) gexpr ->
      unit =
   fun bnd_ctx colors fmt e ->
    attrs fmt
      (match Mark.get e with
      | Untyped { pos } | Typed { pos; _ } | Custom { pos; _ } -> pos);
    (* Uncomment for type annotations everywhere *)
    (* (fun f ->
     *    match Mark.get e with
     *    | Typed {ty; _} ->
     *      Format.fprintf fmt "@[<hv 1>(%a:@ %a)@]"
     *        f e
     *        (typ ~colors) ty
     *    | _ -> f fmt e)
     * @@ fun fmt e -> *)
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
          | EApp { f = EAbs { binder; pos = _; tys }, _; args; _ }, _ ->
            let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
            let xs_tau = List.mapi (fun i tau -> xs.(i), tau) tys in
            let xs_tau_arg =
              List.map2 (fun (x, tau) arg -> x, tau, arg) xs_tau args
            in
            Format.pp_print_list
              (fun fmt (x, tau, arg) ->
                Format.fprintf fmt
                  "@[<hv 2>@[<hov 4>%a %a %a@ %a@ %a@]@ %a@;<1 -2>%a@]" keyword
                  "let" var x punctuation ":" (typ ~colors) tau punctuation "="
                  (exprc colors) arg keyword "in")
              fmt xs_tau_arg;
            Format.pp_print_cut fmt ();
            rhs (pr bnd_ctx) fmt body
          | e -> rhs (exprb bnd_ctx) fmt e
        in
        Format.pp_open_vbox fmt 0;
        pr bnd_ctx colors fmt e;
        Format.pp_close_box fmt ()
      | EAbs { binder; pos = _; tys } ->
        let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
        let expr = exprb bnd_ctx in
        let xs_tau = List.mapi (fun i tau -> xs.(i), tau) tys in
        Format.fprintf fmt "@[<hv 0>%a @[<hv 2>%a@]@ @]%a@ %a" punctuation "λ"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             (fun fmt (x, tau) ->
               match tau with
               | TLit TUnit, _ ->
                 punctuation fmt "(";
                 punctuation fmt ")"
               | _ ->
                 punctuation fmt "(";
                 Format.pp_open_hvbox fmt 2;
                 var fmt x;
                 punctuation fmt ":";
                 Format.pp_print_space fmt ();
                 typ ~colors fmt tau;
                 Format.pp_close_box fmt ();
                 punctuation fmt ")"))
          xs_tau punctuation "→" (rhs expr) body
      | EAppOp { op = ((Map | Filter) as op), _; args = [arg1; arg2]; _ } ->
        Format.fprintf fmt "@[<hv 2>%a %a@ %a@]" operator op (lhs exprc) arg1
          (rhs exprc) arg2
      | EAppOp { op = (Log _ as op), _; args = [arg1]; _ } ->
        Format.fprintf fmt "@[<hv 0>%a@ %a@]" operator op (rhs exprc) arg1
      | EAppOp { op = op0, _; args = [_; _]; _ } ->
        let prec = Precedence.expr e in
        let rec pr colors fmt = function
          (* Flatten sequences of the same associative op *)
          | EAppOp { op = op, _; args = [arg1; arg2]; _ }, _ when op = op0 -> (
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
      | EAppOp { op = op, _; args = [arg1]; _ } ->
        Format.fprintf fmt "@[<hv 2>%a@ %a@]" operator op (rhs exprc) arg1
      | EAppOp { op = op, _; args; _ } ->
        Format.fprintf fmt "@[<hv 2>%a@ %a@]" operator op
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
             (rhs exprc))
          args
      | EApp { f; args; _ } ->
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
      | EEmpty -> lit_style fmt "∅"
      | EErrorOnEmpty e' ->
        Format.fprintf fmt "@[<hov 2>%a@ %a@]" op_style "error_empty"
          (rhs exprc) e'
      | EPos p -> Format.fprintf fmt "<%s>" (Pos.to_string_shorter p)
      | EAssert e' ->
        Format.fprintf fmt "@[<hov 2>%a@ %a%a%a@]" keyword "assert" punctuation
          "(" (rhs exprc) e' punctuation ")"
      | EFatalError err ->
        Format.fprintf fmt "@[<hov 2>%a@ @{<red>%s@}@]" keyword "error"
          (Runtime.error_to_string err)
      | ELocation loc -> location fmt loc
      | EDStructAccess { e; field; _ } ->
        Format.fprintf fmt "@[<hv 2>%a%a@,%a%a%a@]" (lhs exprc) e punctuation
          "." punctuation "\"" Ident.format field punctuation "\""
      | EDStructAmend { e; fields; _ } ->
        Format.fprintf fmt "@[<hv 2>@[<hov>%a %a@ with@]@ %a@;<1 -2>%a@]"
          punctuation "{" (lhs exprc) e
          (Ident.Map.format_bindings ~pp_sep:Format.pp_print_space
             (fun fmt pp_field_name field_expr ->
               Format.fprintf fmt "@[<hv 2>%t %a@ %a%a@]" pp_field_name
                 punctuation "=" (lhs exprc) field_expr punctuation ";"))
          fields punctuation "}"
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
      | EInj { e = ELit LUnit, _; cons; _ } ->
        Format.fprintf fmt "@[<hv 2>%a@]" EnumConstructor.format cons
      | EInj { e; cons; _ } ->
        Format.fprintf fmt "@[<hv 2>%a@ %a@]" EnumConstructor.format cons
          (paren ~rhs:false exprc) e
      | EMatch { e; cases; _ } ->
        Format.fprintf fmt "@[<v 0>@[<hv 2>%a@ %a@;<1 -2>%a@]@ %a@]" keyword
          "match" (lhs exprc) e keyword "with"
          (EnumConstructor.Map.format_bindings
             (fun fmt pp_cons_name case_expr ->
               match case_expr with
               | EAbs { binder; tys; _ }, _ ->
                 let xs, body, bnd_ctx = Bindlib.unmbind_in bnd_ctx binder in
                 let expr = exprb bnd_ctx in
                 let pp_args fmt =
                   match tys with
                   | [(TLit TUnit, _)] -> ()
                   | _ ->
                     Format.pp_print_seq ~pp_sep:Format.pp_print_space var fmt
                       (Array.to_seq xs);
                     Format.pp_print_space fmt ()
                 in
                 Format.fprintf fmt "@[<hov 2>%a %t@ %t%a@ %a@]" punctuation "|"
                   pp_cons_name pp_args punctuation "→" (rhs expr) body
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
          (fun fmt pp_field_name (_, field_expr) ->
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
    | EAppOp { op = Log _, _; args = [e]; _ }, _ -> pre_map e
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

let expr ?(debug = Global.options.debug) () ppf e =
  if debug then ExprDebug.expr ppf e else ExprConcise.expr ppf e

let scope_let_kind ?debug:(_debug = true) fmt k =
  match k with
  | DestructuringInputStruct -> keyword fmt "get"
  | ScopeVarDefinition -> keyword fmt "set"
  | SubScopeVarDefinition -> keyword fmt "sub_set"
  | CallingSubScope -> keyword fmt "call"
  | DestructuringSubScopeResults -> keyword fmt "sub_get"
  | Assertion -> keyword fmt "assert"

let typ = typ ?colors:None

let[@ocamlformat "disable"]
  scope_body_expr ?(debug = false) fmt b : unit =
  let print_scope_let x sl =
    Format.fprintf fmt
      "@[<hv 2>@[<hov 4>%a %a %a %a@ %a@ %a@]@ %a@;<1 -2>%a@]@,"
      keyword "let"
      (scope_let_kind ~debug) sl.scope_let_kind
      (if debug then var_debug else var) x
      punctuation ":"
      typ sl.scope_let_typ
      punctuation "="
      (expr ~debug ()) sl.scope_let_expr
      keyword "in"
  in
  let last = BoundList.iter ~f:print_scope_let b in
  Format.fprintf fmt "%a %a" keyword "return" (expr ~debug ()) last

let scope_body ?(debug = false) fmt (n, l) : unit =
  let {
    scope_body_input_struct;
    scope_body_output_struct;
    scope_body_expr = body;
    scope_body_visibility = _vis;
  } =
    l
  in

  let input_typ = TStruct scope_body_input_struct, Pos.void in
  let output_typ = TStruct scope_body_output_struct, Pos.void in

  let x, body = Bindlib.unbind body in

  let () =
    Format.pp_open_vbox fmt 2;
    let () =
      Format.pp_open_hvbox fmt 2;
      let () =
        Format.pp_open_hovbox fmt 4;
        keyword fmt "let scope";
        Format.fprintf fmt "@ @{<hi_magenta>%s@}@]" n
      in
      Format.pp_print_space fmt ();
      punctuation fmt "(";
      let () =
        Format.pp_open_hvbox fmt 2;
        (if debug then var_debug else var) fmt x;
        punctuation fmt ":";
        Format.pp_print_space fmt ();
        typ fmt input_typ;
        punctuation fmt ")";
        Format.pp_close_box fmt ()
      in
      Format.pp_print_cut fmt ();
      punctuation fmt ":";
      Format.pp_print_string fmt " ";
      let () =
        Format.pp_open_hvbox fmt 2;
        typ fmt output_typ;
        Format.pp_close_box fmt ()
      in
      Format.pp_print_space fmt ();
      punctuation fmt "=";
      Format.pp_close_box fmt ()
    in

    Format.pp_print_cut fmt ();

    scope_body_expr ~debug fmt body;
    Format.pp_close_box fmt ()
  in
  ()

let enum
    ?debug:(_ = false)
    fmt
    (pp_name : Format.formatter -> unit)
    name
    (c : typ EnumConstructor.Map.t) =
  attrs fmt (Mark.get (EnumName.get_info name));
  Format.fprintf fmt "@[<v 2>%a %t %a@ %a@]@," keyword "type" pp_name
    punctuation "="
    (EnumConstructor.Map.format_bindings_i ~pp_sep:Format.pp_print_space
       (fun fmt pp_n n ty ->
         Format.fprintf fmt "@[<hov2>%a %a%t%a@]" punctuation "|" attrs
           (Mark.get (EnumConstructor.get_info n))
           pp_n
           (fun ppf -> function
             | TLit TUnit, _ -> ()
             | ty -> Format.fprintf ppf " %a %a" keyword "of" typ ty)
           ty))
    c

let struct_
    ?debug:(_ = false)
    fmt
    (pp_name : Format.formatter -> unit)
    name
    (c : typ StructField.Map.t) =
  attrs fmt (Mark.get (StructName.get_info name));
  Format.fprintf fmt "@[<hv 2>%a %t %a %a@ %a@;<1 -2>%a@]@," keyword "type"
    pp_name punctuation "=" punctuation "{"
    (StructField.Map.format_bindings_i ~pp_sep:Format.pp_print_space
       (fun fmt pp_n n ty ->
         Format.fprintf fmt "@[<hov 0>%a@[<h 2>%t%a %a%a@]@]" attrs
           (Mark.get (StructField.get_info n))
           pp_n keyword ":" typ ty punctuation ";"))
    c punctuation "}"

let decl_ctx ?(debug = false) (fmt : Format.formatter) (ctx : decl_ctx) : unit =
  let { ctx_enums; ctx_structs; _ } = ctx in
  Format.fprintf fmt "@[<v>%a@,%a@,@]"
    (EnumName.Map.format_bindings_i (enum ~debug))
    (* Remove the Optional type, which is necessarily the first in the ctx *)
    (EnumName.Map.remove (fst (EnumName.Map.min_binding ctx_enums)) ctx_enums)
    (StructName.Map.format_bindings_i (struct_ ~debug))
    ctx_structs

let scope
    ?(debug : bool = false)
    (fmt : Format.formatter)
    ((n, s) : string * 'm scope_body) : unit =
  Format.pp_open_vbox fmt 0;
  scope_body ~debug fmt (n, s);
  Format.pp_close_box fmt ()

let code_item ?(debug = false) name fmt c =
  match c with
  | ScopeDef (n, b) ->
    attrs fmt (Mark.get (ScopeName.get_info n));
    scope ~debug fmt (name, b)
  | Topdef (n, ty, _vis, e) ->
    attrs fmt (Mark.get (TopdefName.get_info n));
    Format.fprintf fmt
      "@[<v 2>@[<hov 2>%a@ @{<hi_green>%s@}@ %a@ %a@ %a@]@ %a@]" keyword
      "let topval" name op_style ":" typ ty op_style "=" (expr ~debug ()) e

let code_item_list ?(debug = false) fmt c =
  Format.pp_open_vbox fmt 0;
  Format.pp_print_seq
    (fun fmt (id, item) ->
      let name = Format.asprintf "%a" (if debug then var_debug else var) id in
      code_item ~debug name fmt item;
      Format.pp_print_cut fmt ())
    fmt (BoundList.to_seq c);
  Format.pp_close_box fmt ()

let program ?(debug = false) fmt p =
  decl_ctx ~debug fmt p.decl_ctx;
  code_item_list ~debug fmt p.code_items

(* - User-facing value printer - *)

(* This function is re-exported from module [Expr], but defined here where it's
   first needed *)
let rec skip_wrappers : type a. (a, 'm) gexpr -> (a, 'm) gexpr = function
  | EAppOp { op = Log _, _; args = [e]; tys = _ }, _ -> skip_wrappers e
  | EApp { f = EAppOp { op = Log _, _; args = [f]; _ }, _; args; tys }, m ->
    skip_wrappers (EApp { f; args; tys }, m)
  | EErrorOnEmpty e, _ -> skip_wrappers e
  | EDefault { excepts = []; just = ELit (LBool true), _; cons = e }, _ ->
    skip_wrappers e
  | EPureDefault e, _ -> skip_wrappers e
  | e -> e

module UserFacing = struct
  (* Refs:
     https://en.wikipedia.org/wiki/Wikipedia:Manual_of_Style/Dates_and_numbers#Grouping_of_digits
     https://fr.wikipedia.org/wiki/Wikip%C3%A9dia:Conventions_concernant_les_nombres#Pour_un_comptage_ou_une_mesure *)
  let bigsep (lang : Global.backend_lang) =
    match lang with En -> ",", 3 | Fr -> " ", 3 | Pl -> ",", 3

  let decsep (lang : Global.backend_lang) =
    match lang with En -> "." | Fr -> "," | Pl -> "."

  let unit (_lang : Global.backend_lang) ppf () =
    Format.pp_print_string ppf "()"

  let bool (lang : Global.backend_lang) ppf b =
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

  let integer (lang : Global.backend_lang) ppf n =
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

  let money (lang : Global.backend_lang) ppf n =
    let num = Z.abs n in
    let units, cents = Z.div_rem num (Z.of_int 100) in
    if Z.sign n < 0 then Format.pp_print_char ppf '-';
    (match lang with En -> Format.pp_print_string ppf "$" | Fr | Pl -> ());
    integer lang ppf units;
    Format.pp_print_string ppf (decsep lang);
    Format.fprintf ppf "%02d" (Z.to_int (Z.abs cents));
    match lang with
    | En -> ()
    | Fr -> Format.pp_print_string ppf " €"
    | Pl -> Format.pp_print_string ppf " PLN"

  let decimal (lang : Global.backend_lang) ppf r =
    let den = Q.den r in
    let num = Z.abs (Q.num r) in
    let int_part, rem = Z.div_rem num den in
    let rem = Z.abs rem in
    (* Printing the integer part *)
    if Q.sign r < 0 then Format.pp_print_char ppf '-';
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
            let r = Global.options.max_prec_digits in
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
       else Some (Global.options.max_prec_digits - ndigits int_part))
      rem
  (* It would be nice to print ratios as % but that's impossible to guess.
     Trying would lead to inconsistencies where some comparable numbers are in %
     and some others not, adding confusion. *)

  let date (lang : Global.backend_lang) ppf d =
    let y, m, d = Runtime.date_to_years_months_days d in
    match lang with
    | En | Pl -> Format.fprintf ppf "%04d-%02d-%02d" y m d
    | Fr -> Format.fprintf ppf "%02d/%02d/%04d" d m y

  let duration (lang : Global.backend_lang) ppf dr =
    let y, m, d = Runtime.duration_to_years_months_days dr in
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

  let lit_raw (lang : Global.backend_lang) ppf lit : unit =
    match lit with
    | LUnit -> unit lang ppf ()
    | LBool b -> bool lang ppf b
    | LInt i -> integer lang ppf i
    | LRat r -> decimal lang ppf r
    | LMoney e -> money lang ppf e
    | LDate d -> date lang ppf d
    | LDuration dr -> duration lang ppf dr

  let lit_to_string (lang : Global.backend_lang) lit =
    let buf = Buffer.create 32 in
    let ppf = Format.formatter_of_buffer buf in
    lit_raw lang ppf lit;
    Format.pp_print_flush ppf ();
    Buffer.contents buf

  let lit (lang : Global.backend_lang) ppf lit : unit =
    with_color (lit_raw lang) Ocolor_types.yellow ppf lit

  let rec value :
      type a.
      ?fallback:(Format.formatter -> (a, 't) gexpr -> unit) ->
      Global.backend_lang ->
      Format.formatter ->
      (a, 't) gexpr ->
      unit =
   fun ?(fallback = fun _ _ -> invalid_arg "UserPrint.value: not a value") lang
       ppf e ->
    match Mark.remove e with
    | ELit l -> lit lang ppf l
    | EArray l ->
      Format.fprintf ppf "@[<hv 2>[@,@[<hov>%a@]@;<0 -2>]@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
           (value ~fallback lang))
        l
    | ETuple [(EAbs { tys = (TClosureEnv, _) :: _; _ }, _); _] ->
      Format.pp_print_string ppf "<function>"
    | ETuple l ->
      Format.fprintf ppf "@[<hv 2>(@,@[<hov>%a@]@;<0 -2>)@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           (value ~fallback lang))
        l
    | EStruct { name; fields } ->
      Format.fprintf ppf "@[<hv 2>%a {@ %a@;<1 -2>}@]" StructName.format name
        (StructField.Map.format_bindings ~pp_sep:Format.pp_print_space
           (fun ppf pp_fld e ->
             Format.fprintf ppf "@[<hov 2>-- %t:@ %a@]" pp_fld
               (value ~fallback lang) e))
        fields
    | EInj { name = _; cons; e = ELit LUnit, _ } ->
      Format.fprintf ppf "@[<hov 2>%a@]" EnumConstructor.format cons
    | EInj { name = _; cons; e } ->
      Format.fprintf ppf "@[<hov 2>%a@ %a@]" EnumConstructor.format cons
        (value ~fallback lang) e
    | EEmpty -> Format.pp_print_string ppf "ø"
    | ECustom _ | EAbs _ -> Format.pp_print_string ppf "<function>"
    | EExternal _ -> Format.pp_print_string ppf "<external>"
    | EApp _ | EAppOp _ | EVar _ | EIfThenElse _ | EMatch _ | ETupleAccess _
    | EStructAccess _ | EAssert _ | EFatalError _ | EDefault _ | EPureDefault _
    | EErrorOnEmpty _ | EPos _ | ELocation _ | EScopeCall _ | EDStructAmend _
    | EDStructAccess _ ->
      fallback ppf e

  let expr :
      type a. Global.backend_lang -> Format.formatter -> (a, 't) gexpr -> unit =
   fun lang ->
    let rec aux_value : type a t. Format.formatter -> (a, t) gexpr -> unit =
     fun ppf e -> value ~fallback lang ppf e
    and fallback : type a t. Format.formatter -> (a, t) gexpr -> unit =
     fun ppf e ->
      let module E = ExprGen (struct
        let bypass : type a t. Format.formatter -> (a, t) gexpr -> bool =
         fun ppf e ->
          match Mark.remove e with
          | EArray _ | ETuple _ | EStruct _ | EInj _ | EEmpty | EAbs _
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
