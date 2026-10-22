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
open Ast

let pp_comma (ppf : Format.formatter) () : unit = Format.fprintf ppf ",@ "
let pp_semi (ppf : Format.formatter) () : unit = Format.fprintf ppf ";@ "

(** Qualification prefix: prints [Mod1.Mod2.] (nothing on an empty path) *)
let format_path (fmt : Format.formatter) (path : path) : unit =
  List.iter (fun m -> Format.fprintf fmt "%s." (Mark.remove m)) path

(* Binder and parameter lists are always short: keep them on a single line *)
let format_lidents (fmt : Format.formatter) (l : lident Mark.pos list) : unit =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ", ")
    (fun ppf id -> Format.pp_print_string ppf (Mark.remove id))
    fmt l

let format_primitive_typ (fmt : Format.formatter) (t : primitive_typ) : unit =
  match t with
  | Integer -> Format.pp_print_string fmt "integer"
  | Decimal -> Format.pp_print_string fmt "decimal"
  | Boolean -> Format.pp_print_string fmt "boolean"
  | Money -> Format.pp_print_string fmt "money"
  | Duration -> Format.pp_print_string fmt "duration"
  | Date -> Format.pp_print_string fmt "date"
  | Position -> Format.pp_print_string fmt "position"
  | Named (path, constructor) ->
    Format.fprintf fmt "%a%s" format_path path (Mark.remove constructor)
  | Var None -> Format.fprintf fmt "anything"
  | Var (Some id) ->
    Format.fprintf fmt "anything@ of@ type@ ";
    Format.pp_print_string fmt (Mark.remove id)

let rec format_base_typ_data (fmt : Format.formatter) (t : base_typ_data) : unit
    =
  match t with
  | Primitive t -> format_primitive_typ fmt t
  | Collection t ->
    Format.fprintf fmt "list@ of@ %a" format_base_typ_data (Mark.remove t)
  | Option t ->
    Format.fprintf fmt "option@ of@ %a" format_base_typ_data (Mark.remove t)
  | TTuple ts ->
    Format.fprintf fmt "(@[<hov>%a@])"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " *@ ")
         (fun ppf t -> format_base_typ_data ppf (Mark.remove t)))
      ts

let format_base_typ (fmt : Format.formatter) (t : base_typ) : unit =
  match t with
  | Condition -> Format.pp_print_string fmt "condition"
  | Data d -> format_base_typ_data fmt d

(** [x contenu entier, y contenu date] *)
let format_typ_args
    (fmt : Format.formatter)
    (args : (lident Mark.pos * base_typ Mark.pos) list) : unit =
  Format.pp_print_list ~pp_sep:pp_comma
    (fun ppf (name, t) ->
      Format.fprintf ppf "%s contenu %a" (Mark.remove name) format_base_typ
        (Mark.remove t))
    fmt args

let format_func_typ (fmt : Format.formatter) (f : func_typ) : unit =
  Format.fprintf fmt "%a dépend de %a" format_base_typ
    (Mark.remove f.return_typ) format_typ_args f.arg_typ

let format_typ (fmt : Format.formatter) (t : typ) : unit =
  match Mark.remove t with
  | Base b -> format_base_typ fmt b
  | Func f -> format_func_typ fmt f

let format_enum_constr (fmt : Format.formatter) (c : enum_constr) : unit =
  match c with
  | CBuiltin Present -> Format.pp_print_string fmt "Present"
  | CBuiltin Absent -> Format.pp_print_string fmt "Absent"
  | CConstr (path, c) ->
    Format.fprintf fmt "%a%s" format_path path (Mark.remove c)

let format_builtin (fmt : Format.formatter) (b : builtin_expression) : unit =
  match b with
  | Impossible -> Format.pp_print_string fmt "impossible"
  | Cardinal -> Format.pp_print_string fmt "number"
  | ToInteger -> Format.pp_print_string fmt "integer"
  | ToDecimal -> Format.pp_print_string fmt "decimal"
  | ToMoney -> Format.pp_print_string fmt "money"
  | Round -> Format.pp_print_string fmt "round"
  | External t -> Format.fprintf fmt "external %a" format_typ t

let binop_to_str (binop : binop) : string * bool =
  match binop with
  | And -> "&&", true
  | Or -> "||", true
  | Xor -> "xor", true
  | Add _ -> "+", true
  | Sub _ -> "-", true
  | Mult _ -> "*", true
  | Div _ -> "/", true
  | Lt _ -> "<", true
  | Lte _ -> "<=", true
  | Gt _ -> ">", true
  | Gte _ -> ">=", true
  | Eq -> "=", true
  | Neq -> "<>", true
  | ListConcat -> "List.concat", false
  | ListMember -> "List.mem", false
  | ListExists -> "List.exists", false
  | ListForall -> "List.for_all", false
  | ListFind -> "List.find", false
  | ListMap -> "List.map", false
  | ListFilter -> "List.filter", false
  | ListMax -> "List.max", false
  | ListMin -> "List.min", false
  | ListSort `Asc -> "List.sort_asc", false
  | ListSort `Desc -> "List.sort_desc", false

let unop_to_str (unop : unop) : string =
  match unop with Not -> "not" | Minus _ -> "-" | ListSum _ -> "List.sum"

let ternop_to_str (ternop : ternop) : string =
  match ternop with
  | ListArgMin -> "List.arg_min"
  | ListArgMax -> "List.arg_max"
  | ListFold -> "List.fold"

let literal_number_unit_str (unity : literal_unit Mark.pos option) : string =
  match unity with
  | None -> ""
  | Some (Percent, _) -> "%"
  | Some (Year, _) -> " year"
  | Some (Month, _) -> " month"
  | Some (Day, _) -> " day"

let format_lit (fmt : Format.formatter) (lit : literal) : unit =
  match lit with
  | LNumber ((Int str, _), unity) ->
    Format.fprintf fmt "%s%s" str (literal_number_unit_str unity)
  | LNumber ((Dec (str, dec), _), unity) ->
    Format.fprintf fmt "%s.%s%s" str dec (literal_number_unit_str unity)
  | LBool b -> Format.fprintf fmt "%b" b
  | LMoneyAmount amount ->
    (* The currency depends on the source language, which the AST does not
       record: use the neutral [¤] sign, as [Shared_ast.Print] does *)
    Format.fprintf fmt "¤%s%s.%s"
      (if amount.money_amount_sign then "" else "-")
      amount.money_amount_units amount.money_amount_cents
  | LDate { literal_date_day; literal_date_month; literal_date_year } ->
    Format.fprintf fmt "|%04d-%02d-%02d|" literal_date_year literal_date_month
      literal_date_day

let rec format_expr (fmt : Format.formatter) (expr : expression) : unit =
  format_naked_expr fmt (Mark.remove expr)

and format_naked_expr (fmt : Format.formatter) (expr : naked_expression) : unit
    =
  match expr with
  | Paren expr -> Format.fprintf fmt "(%a)" format_expr expr
  | MatchWith (expr, match_cases) ->
    Format.fprintf fmt "@[<hv>match (%a) with@ | %a@]" format_expr expr
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ | ")
         (fun ppf m -> format_match_case ppf (Mark.remove m)))
      (Mark.remove match_cases)
  | IfThenElse (cond, t, f) ->
    Format.fprintf fmt "@[<hv 2>if (%a)@ then %a@ else %a@]" format_expr cond
      format_expr t format_expr f
  | Unop (unop, exp) ->
    Format.fprintf fmt "%s %a" (unop_to_str (Mark.remove unop)) format_expr exp
  | Binop (binop, exp1, exp2) ->
    let str, infix = binop_to_str (Mark.remove binop) in
    if infix then
      Format.fprintf fmt "%a %s %a" format_expr exp1 str format_expr exp2
    else Format.fprintf fmt "%s %a %a" str format_expr exp1 format_expr exp2
  | Ternop (ternop, exp1, exp2, exp3) ->
    Format.fprintf fmt "%s %a %a %a"
      (ternop_to_str (Mark.remove ternop))
      format_expr exp1 format_expr exp2 format_expr exp3
  | TestMatchCase (match_expr, match_case) ->
    Format.fprintf fmt "%a is %a" format_expr match_expr format_pattern
      (Mark.remove match_case)
  | FunCall (exp, exp_l) ->
    Format.fprintf fmt "%a (%a)" format_expr exp
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         format_expr)
      exp_l
  | ScopeCall (scope, args) ->
    let path, scope_name = Mark.remove scope in
    Format.fprintf fmt "ScopeCall %a%s on {%a}" format_path path
      (Mark.remove scope_name) format_fields args
  | LetIn (lident, exp1, exp2) ->
    Format.fprintf fmt "@[<hv 2>let (%a) =@ %a@]@ in@ %a" format_lidents lident
      format_expr exp1 format_expr exp2
  | Builtin built_exp -> format_builtin fmt built_exp
  | Literal lit -> format_lit fmt lit
  | EnumInject (enum_constr, exp_inject) -> (
    match exp_inject with
    | None -> format_enum_constr fmt (Mark.remove enum_constr)
    | Some exp ->
      Format.fprintf fmt "@[<hov 2>%a content@ (%a)@]" format_enum_constr
        (Mark.remove enum_constr) format_expr exp)
  | StructLit (def_struct, expr_l) ->
    let path, struct_name = Mark.remove def_struct in
    Format.fprintf fmt "@[<hv 2>%a%s {@ %a@;<1 -2>}@]" format_path path
      (Mark.remove struct_name) format_fields expr_l
  | StructReplace (expr_str, replace_l) ->
    Format.fprintf fmt "@[<hv 2>%a but replacing {@ %a@;<1 -2>}@]" format_expr
      expr_str format_fields replace_l
  | ArrayLit exp_l ->
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
         format_expr)
      exp_l
  | Tuple exp_l ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         format_expr)
      exp_l
  | Ident (path, lident, state) ->
    Format.fprintf fmt "%a%s%s" format_path path (Mark.remove lident)
      (Option.fold ~none:"" ~some:(fun state -> "#" ^ Mark.remove state) state)
  | Dotted (exp, path) ->
    let path, id = Mark.remove path in
    Format.fprintf fmt "%a.%a%s" format_expr exp format_path path
      (Mark.remove id)
  | TupleAccess (exp, access) ->
    Format.fprintf fmt "%a.%d" format_expr exp (Mark.remove access)
  | Assert (assert_expr, expr, _pos) ->
    Format.fprintf fmt "@[<hv 2>assert@ %a@]@ in@ %a" format_expr assert_expr
      format_expr expr
  | Lambda (lident, exp) ->
    Format.fprintf fmt "@[<hov 2>(fun (%a) ->@ %a)@]" format_lidents lident
      format_expr exp
  | ListZip (lident, nexp) ->
    Format.fprintf fmt "@[<hov 2>zip (%a) among@ %a@]" format_lidents lident
      format_naked_expr nexp

and format_fields
    (fmt : Format.formatter)
    (fields : (lident Mark.pos * expression) list) : unit =
  Format.pp_print_list ~pp_sep:pp_semi
    (fun ppf (field, exp) ->
      Format.fprintf ppf "@[<hov 2>%s =@ %a@]" (Mark.remove field) format_expr
        exp)
    fmt fields

and format_match_case (fmt : Format.formatter) (match_case : match_case) : unit
    =
  match match_case with
  | WildCard exp -> Format.fprintf fmt "_ -> %a" format_expr exp
  | MatchCase explicit ->
    Format.fprintf fmt "%a -> %a" format_pattern
      (Mark.remove explicit.match_case_pattern)
      format_expr explicit.match_case_expr

and format_pattern (fmt : Format.formatter) (pattern : match_case_pattern) :
    unit =
  let constrs, binders = pattern in
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_char ppf '.')
    (fun ppf c -> format_enum_constr ppf (Mark.remove c))
    fmt constrs;
  if binders <> [] then Format.fprintf fmt " content %a" format_lidents binders

(** {1 Structural printing}

    Prints the AST constructors as they are (e.g.
    [Tuple [Ident ([], "x", None); Literal (LBool false)]]) rather than the
    source syntax. Positions are omitted. Useful for debugging. *)

let debug_list pp fmt l =
  Format.fprintf fmt "@[<hov 1>[%a]@]"
    (Format.pp_print_list ~pp_sep:pp_semi pp)
    l

let debug_option pp fmt = function
  | None -> Format.pp_print_string fmt "None"
  | Some x -> Format.fprintf fmt "@[<hov 2>Some@ (%a)@]" pp x

let debug_string fmt s = Format.fprintf fmt "%S" s
let debug_path = debug_list (fun fmt m -> debug_string fmt (Mark.remove m))
let debug_lidents = debug_list (fun fmt id -> debug_string fmt (Mark.remove id))

let debug_op_kind fmt (k : op_kind) =
  Format.pp_print_string fmt
    (match k with
    | KPoly -> "KPoly"
    | KInt -> "KInt"
    | KDec -> "KDec"
    | KMoney -> "KMoney"
    | KDate -> "KDate"
    | KDuration -> "KDuration")

let debug_binop fmt (op : binop) =
  let k name kind = Format.fprintf fmt "%s %a" name debug_op_kind kind in
  match op with
  | And -> Format.pp_print_string fmt "And"
  | Or -> Format.pp_print_string fmt "Or"
  | Xor -> Format.pp_print_string fmt "Xor"
  | Add kind -> k "Add" kind
  | Sub kind -> k "Sub" kind
  | Mult kind -> k "Mult" kind
  | Div kind -> k "Div" kind
  | Lt kind -> k "Lt" kind
  | Lte kind -> k "Lte" kind
  | Gt kind -> k "Gt" kind
  | Gte kind -> k "Gte" kind
  | Eq -> Format.pp_print_string fmt "Eq"
  | Neq -> Format.pp_print_string fmt "Neq"
  | ListConcat -> Format.pp_print_string fmt "ListConcat"
  | ListMember -> Format.pp_print_string fmt "ListMember"
  | ListExists -> Format.pp_print_string fmt "ListExists"
  | ListForall -> Format.pp_print_string fmt "ListForall"
  | ListFind -> Format.pp_print_string fmt "ListFind"
  | ListMap -> Format.pp_print_string fmt "ListMap"
  | ListFilter -> Format.pp_print_string fmt "ListFilter"
  | ListMax -> Format.pp_print_string fmt "ListMax"
  | ListMin -> Format.pp_print_string fmt "ListMin"
  | ListSort `Asc -> Format.pp_print_string fmt "ListSort `Asc"
  | ListSort `Desc -> Format.pp_print_string fmt "ListSort `Desc"

let debug_unop fmt (op : unop) =
  match op with
  | Not -> Format.pp_print_string fmt "Not"
  | Minus kind -> Format.fprintf fmt "Minus %a" debug_op_kind kind
  | ListSum t ->
    Format.fprintf fmt "ListSum (%a)" format_primitive_typ (Mark.remove t)

let debug_ternop fmt (op : ternop) =
  Format.pp_print_string fmt
    (match op with
    | ListArgMin -> "ListArgMin"
    | ListArgMax -> "ListArgMax"
    | ListFold -> "ListFold")

let debug_builtin fmt (b : builtin_expression) =
  match b with
  | Impossible -> Format.pp_print_string fmt "Impossible"
  | Cardinal -> Format.pp_print_string fmt "Cardinal"
  | ToInteger -> Format.pp_print_string fmt "ToInteger"
  | ToDecimal -> Format.pp_print_string fmt "ToDecimal"
  | ToMoney -> Format.pp_print_string fmt "ToMoney"
  | Round -> Format.pp_print_string fmt "Round"
  | External t -> Format.fprintf fmt "External (%a)" format_typ t

let debug_enum_constr fmt (c : enum_constr) =
  match c with
  | CBuiltin Present -> Format.pp_print_string fmt "CBuiltin Present"
  | CBuiltin Absent -> Format.pp_print_string fmt "CBuiltin Absent"
  | CConstr (path, c) ->
    Format.fprintf fmt "@[<hov 2>CConstr@ (%a,@ %S)@]" debug_path path
      (Mark.remove c)

let debug_lit fmt (lit : literal) =
  let debug_unit fmt (u : literal_unit) =
    Format.pp_print_string fmt
      (match u with
      | Percent -> "Percent"
      | Year -> "Year"
      | Month -> "Month"
      | Day -> "Day")
  in
  match lit with
  | LNumber ((n, _), u) ->
    let debug_number fmt = function
      | Int i -> Format.fprintf fmt "Int %S" i
      | Dec (i, f) -> Format.fprintf fmt "Dec (%S, %S)" i f
    in
    Format.fprintf fmt "@[<hov 2>LNumber@ (%a,@ %a)@]" debug_number n
      (debug_option (fun fmt u -> debug_unit fmt (Mark.remove u)))
      u
  | LBool b -> Format.fprintf fmt "LBool %b" b
  | LMoneyAmount m ->
    Format.fprintf fmt "LMoneyAmount { sign = %b; units = %S; cents = %S }"
      m.money_amount_sign m.money_amount_units m.money_amount_cents
  | LDate d ->
    Format.fprintf fmt "LDate { day = %d; month = %d; year = %d }"
      d.literal_date_day d.literal_date_month d.literal_date_year

let rec format_expr_debug (fmt : Format.formatter) (expr : expression) : unit =
  format_naked_expr_debug fmt (Mark.remove expr)

and format_naked_expr_debug (fmt : Format.formatter) (expr : naked_expression) :
    unit =
  let e = format_expr_debug in
  let fields =
    debug_list (fun fmt (f, x) ->
        Format.fprintf fmt "@[<hov 2>(%S,@ %a)@]" (Mark.remove f) e x)
  in
  let qualified fmt ((path, name), _) =
    Format.fprintf fmt "(%a, %S)" debug_path path (Mark.remove name)
  in
  match expr with
  | Paren x -> Format.fprintf fmt "@[<hov 2>Paren@ (%a)@]" e x
  | MatchWith (x, (cases, _)) ->
    Format.fprintf fmt "@[<hov 2>MatchWith@ (%a,@ %a)@]" e x
      (debug_list (fun fmt c -> format_match_case_debug fmt (Mark.remove c)))
      cases
  | IfThenElse (c, t, f) ->
    Format.fprintf fmt "@[<hov 2>IfThenElse@ (%a,@ %a,@ %a)@]" e c e t e f
  | Unop ((op, _), x) ->
    Format.fprintf fmt "@[<hov 2>Unop@ (%a,@ %a)@]" debug_unop op e x
  | Binop ((op, _), x, y) ->
    Format.fprintf fmt "@[<hov 2>Binop@ (%a,@ %a,@ %a)@]" debug_binop op e x e y
  | Ternop ((op, _), x, y, z) ->
    Format.fprintf fmt "@[<hov 2>Ternop@ (%a,@ %a,@ %a,@ %a)@]" debug_ternop op
      e x e y e z
  | TestMatchCase (x, (pat, _)) ->
    Format.fprintf fmt "@[<hov 2>TestMatchCase@ (%a,@ %a)@]" e x
      format_pattern_debug pat
  | FunCall (f, args) ->
    Format.fprintf fmt "@[<hov 2>FunCall@ (%a,@ %a)@]" e f (debug_list e) args
  | ScopeCall (name, args) ->
    Format.fprintf fmt "@[<hov 2>ScopeCall@ (%a,@ %a)@]" qualified name fields
      args
  | LetIn (ids, x, body) ->
    Format.fprintf fmt "@[<hov 2>LetIn@ (%a,@ %a,@ %a)@]" debug_lidents ids e x
      e body
  | Builtin b -> Format.fprintf fmt "Builtin %a" debug_builtin b
  | Literal lit -> Format.fprintf fmt "@[<hov 2>Literal@ (%a)@]" debug_lit lit
  | EnumInject ((c, _), payload) ->
    Format.fprintf fmt "@[<hov 2>EnumInject@ (%a,@ %a)@]" debug_enum_constr c
      (debug_option e) payload
  | StructLit (name, fl) ->
    Format.fprintf fmt "@[<hov 2>StructLit@ (%a,@ %a)@]" qualified name fields
      fl
  | StructReplace (x, fl) ->
    Format.fprintf fmt "@[<hov 2>StructReplace@ (%a,@ %a)@]" e x fields fl
  | ArrayLit l -> Format.fprintf fmt "@[<hov 2>ArrayLit@ %a@]" (debug_list e) l
  | Tuple l -> Format.fprintf fmt "@[<hov 2>Tuple@ %a@]" (debug_list e) l
  | Ident (path, id, state) ->
    Format.fprintf fmt "@[<hov 2>Ident@ (%a,@ %S,@ %a)@]" debug_path path
      (Mark.remove id)
      (debug_option (fun fmt s -> debug_string fmt (Mark.remove s)))
      state
  | Dotted (x, ((path, field), _)) ->
    Format.fprintf fmt "@[<hov 2>Dotted@ (%a,@ (%a,@ %S))@]" e x debug_path path
      (Mark.remove field)
  | TupleAccess (x, (n, _)) ->
    Format.fprintf fmt "@[<hov 2>TupleAccess@ (%a,@ %d)@]" e x n
  | Assert (x, y, _) ->
    Format.fprintf fmt "@[<hov 2>Assert@ (%a,@ %a)@]" e x e y
  | Lambda (ids, body) ->
    Format.fprintf fmt "@[<hov 2>Lambda@ (%a,@ %a)@]" debug_lidents ids e body
  | ListZip (ids, x) ->
    Format.fprintf fmt "@[<hov 2>ListZip@ (%a,@ %a)@]" debug_lidents ids
      format_naked_expr_debug x

and format_match_case_debug (fmt : Format.formatter) (case : match_case) : unit
    =
  match case with
  | WildCard x ->
    Format.fprintf fmt "@[<hov 2>WildCard@ (%a)@]" format_expr_debug x
  | MatchCase { match_case_pattern = pat, _; match_case_expr } ->
    Format.fprintf fmt "@[<hov 2>MatchCase@ (%a,@ %a)@]" format_pattern_debug
      pat format_expr_debug match_case_expr

and format_pattern_debug (fmt : Format.formatter) (pat : match_case_pattern) :
    unit =
  let constrs, binders = pat in
  Format.fprintf fmt "@[<hov 1>(%a,@ %a)@]"
    (debug_list (fun fmt c -> debug_enum_constr fmt (Mark.remove c)))
    constrs debug_lidents binders
