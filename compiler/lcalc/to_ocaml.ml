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
open Shared_ast
open Ast
open String_common
module D = Dcalc.Ast

let find_struct (s : StructName.t) (ctx : decl_ctx) :
    (StructFieldName.t * typ Marked.pos) list =
  try StructMap.find s ctx.ctx_structs
  with Not_found ->
    let s_name, pos = StructName.get_info s in
    Errors.raise_spanned_error pos
      "Internal Error: Structure %s was not found in the current environment."
      s_name

let find_enum (en : EnumName.t) (ctx : decl_ctx) :
    (EnumConstructor.t * typ Marked.pos) list =
  try EnumMap.find en ctx.ctx_enums
  with Not_found ->
    let en_name, pos = EnumName.get_info en in
    Errors.raise_spanned_error pos
      "Internal Error: Enumeration %s was not found in the current environment."
      en_name

let format_lit (fmt : Format.formatter) (l : lit Marked.pos) : unit =
  match Marked.unmark l with
  | LBool b -> Dcalc.Print.format_lit fmt (LBool b)
  | LInt i ->
    Format.fprintf fmt "integer_of_string@ \"%s\"" (Runtime.integer_to_string i)
  | LUnit -> Dcalc.Print.format_lit fmt LUnit
  | LRat i ->
    Format.fprintf fmt "decimal_of_string \"%a\"" Dcalc.Print.format_lit
      (LRat i)
  | LMoney e ->
    Format.fprintf fmt "money_of_cents_string@ \"%s\""
      (Runtime.integer_to_string (Runtime.money_to_cents e))
  | LDate d ->
    Format.fprintf fmt "date_of_numbers (%d) (%d) (%d)"
      (Runtime.integer_to_int (Runtime.year_of_date d))
      (Runtime.integer_to_int (Runtime.month_number_of_date d))
      (Runtime.integer_to_int (Runtime.day_of_month_of_date d))
  | LDuration d ->
    let years, months, days = Runtime.duration_to_years_months_days d in
    Format.fprintf fmt "duration_of_numbers (%d) (%d) (%d)" years months days

let format_op_kind (fmt : Format.formatter) (k : op_kind) =
  Format.fprintf fmt "%s"
    (match k with
    | KInt -> "!"
    | KRat -> "&"
    | KMoney -> "$"
    | KDate -> "@"
    | KDuration -> "^")

let format_binop (fmt : Format.formatter) (op : binop Marked.pos) :
    unit =
  match Marked.unmark op with
  | Add k -> Format.fprintf fmt "+%a" format_op_kind k
  | Sub k -> Format.fprintf fmt "-%a" format_op_kind k
  | Mult k -> Format.fprintf fmt "*%a" format_op_kind k
  | Div k -> Format.fprintf fmt "/%a" format_op_kind k
  | And -> Format.fprintf fmt "%s" "&&"
  | Or -> Format.fprintf fmt "%s" "||"
  | Eq -> Format.fprintf fmt "%s" "="
  | Neq | Xor -> Format.fprintf fmt "%s" "<>"
  | Lt k -> Format.fprintf fmt "%s%a" "<" format_op_kind k
  | Lte k -> Format.fprintf fmt "%s%a" "<=" format_op_kind k
  | Gt k -> Format.fprintf fmt "%s%a" ">" format_op_kind k
  | Gte k -> Format.fprintf fmt "%s%a" ">=" format_op_kind k
  | Concat -> Format.fprintf fmt "@"
  | Map -> Format.fprintf fmt "Array.map"
  | Filter -> Format.fprintf fmt "array_filter"

let format_ternop (fmt : Format.formatter) (op : ternop Marked.pos) :
    unit =
  match Marked.unmark op with Fold -> Format.fprintf fmt "Array.fold_left"

let format_uid_list (fmt : Format.formatter) (uids : Uid.MarkedString.info list)
    : unit =
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (fun fmt info ->
         Format.fprintf fmt "\"%a\"" Utils.Uid.MarkedString.format_info info))
    uids

let format_string_list (fmt : Format.formatter) (uids : string list) : unit =
  let sanitize_quotes = Re.compile (Re.char '"') in
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (fun fmt info ->
         Format.fprintf fmt "\"%s\""
           (Re.replace sanitize_quotes ~f:(fun _ -> "\\\"") info)))
    uids

let format_unop (fmt : Format.formatter) (op : unop Marked.pos) : unit
    =
  match Marked.unmark op with
  | Minus k -> Format.fprintf fmt "~-%a" format_op_kind k
  | Not -> Format.fprintf fmt "%s" "not"
  | Log (_entry, _infos) ->
    Errors.raise_spanned_error (Marked.get_mark op)
      "Internal error: a log operator has not been caught by the expression \
       match"
  | Length -> Format.fprintf fmt "%s" "array_length"
  | IntToRat -> Format.fprintf fmt "%s" "decimal_of_integer"
  | MoneyToRat -> Format.fprintf fmt "%s" "decimal_of_money"
  | RatToMoney -> Format.fprintf fmt "%s" "money_of_decimal"
  | GetDay -> Format.fprintf fmt "%s" "day_of_month_of_date"
  | GetMonth -> Format.fprintf fmt "%s" "month_number_of_date"
  | GetYear -> Format.fprintf fmt "%s" "year_of_date"
  | FirstDayOfMonth -> Format.fprintf fmt "%s" "first_day_of_month"
  | LastDayOfMonth -> Format.fprintf fmt "%s" "last_day_of_month"
  | RoundMoney -> Format.fprintf fmt "%s" "money_round"
  | RoundDecimal -> Format.fprintf fmt "%s" "decimal_round"

let avoid_keywords (s : string) : string =
  match s with
  (* list taken from
     http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#sss:keywords *)
  | "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint" | "do"
  | "done" | "downto" | "else" | "end" | "exception" | "external" | "false"
  | "for" | "fun" | "function" | "functor" | "if" | "in" | "include" | "inherit"
  | "initializer" | "land" | "lazy" | "let" | "lor" | "lsl" | "lsr" | "lxor"
  | "match" | "method" | "mod" | "module" | "mutable" | "new" | "nonrec"
  | "object" | "of" | "open" | "or" | "private" | "rec" | "sig" | "struct"
  | "then" | "to" | "true" | "try" | "type" | "val" | "virtual" | "when"
  | "while" | "with" ->
    s ^ "_user"
  | _ -> s

let format_struct_name (fmt : Format.formatter) (v : StructName.t) :
    unit =
  Format.asprintf "%a" StructName.format_t v
  |> to_ascii
  |> to_snake_case
  |> avoid_keywords
  |> Format.fprintf fmt "%s"

let format_to_module_name
    (fmt : Format.formatter)
    (name : [< `Ename of EnumName.t | `Sname of StructName.t ]) =
  (match name with
  | `Ename v -> Format.asprintf "%a" EnumName.format_t v
  | `Sname v -> Format.asprintf "%a" StructName.format_t v)
  |> to_ascii
  |> to_snake_case
  |> avoid_keywords
  |> String.split_on_char '_'
  |> List.map String.capitalize_ascii
  |> String.concat ""
  |> Format.fprintf fmt "%s"

let format_struct_field_name
    (fmt : Format.formatter)
    ((sname_opt, v) :
      StructName.t option * StructFieldName.t) : unit =
  (match sname_opt with
  | Some sname ->
    Format.fprintf fmt "%a.%s" format_to_module_name (`Sname sname)
  | None -> Format.fprintf fmt "%s")
    (avoid_keywords
       (to_ascii (Format.asprintf "%a" StructFieldName.format_t v)))

let format_enum_name (fmt : Format.formatter) (v : EnumName.t) : unit
    =
  Format.fprintf fmt "%s"
    (avoid_keywords
       (to_snake_case
          (to_ascii (Format.asprintf "%a" EnumName.format_t v))))

let format_enum_cons_name
    (fmt : Format.formatter)
    (v : EnumConstructor.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords
       (to_ascii (Format.asprintf "%a" EnumConstructor.format_t v)))

let rec typ_embedding_name (fmt : Format.formatter) (ty : typ Marked.pos) :
    unit =
  match Marked.unmark ty with
  | TLit TUnit -> Format.fprintf fmt "embed_unit"
  | TLit TBool -> Format.fprintf fmt "embed_bool"
  | TLit TInt -> Format.fprintf fmt "embed_integer"
  | TLit TRat -> Format.fprintf fmt "embed_decimal"
  | TLit TMoney -> Format.fprintf fmt "embed_money"
  | TLit TDate -> Format.fprintf fmt "embed_date"
  | TLit TDuration -> Format.fprintf fmt "embed_duration"
  | TTuple (_, Some s_name) ->
    Format.fprintf fmt "embed_%a" format_struct_name s_name
  | TEnum (_, e_name) -> Format.fprintf fmt "embed_%a" format_enum_name e_name
  | TArray ty -> Format.fprintf fmt "embed_array (%a)" typ_embedding_name ty
  | _ -> Format.fprintf fmt "unembeddable"

let typ_needs_parens (e : typ Marked.pos) : bool =
  match Marked.unmark e with TArrow _ | TArray _ -> true | _ -> false

let rec format_typ (fmt : Format.formatter) (typ : typ Marked.pos) :
    unit =
  let format_typ_with_parens
      (fmt : Format.formatter)
      (t : typ Marked.pos) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  match Marked.unmark typ with
  | TLit l -> Format.fprintf fmt "%a" Dcalc.Print.format_tlit l
  | TTuple (ts, None) ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ *@ ")
         format_typ_with_parens)
      ts
  | TTuple (_, Some s) ->
    Format.fprintf fmt "%a.t" format_to_module_name (`Sname s)
  | TEnum ([t], e) when EnumName.compare e Ast.option_enum = 0 ->
    Format.fprintf fmt "@[<hov 2>(%a)@] %a" format_typ_with_parens t
      format_enum_name e
  | TEnum (_, e) when EnumName.compare e Ast.option_enum = 0 ->
    Errors.raise_spanned_error (Marked.get_mark typ)
      "Internal Error: found an typing parameter for an eoption type of the \
       wrong length."
  | TEnum (_ts, e) -> Format.fprintf fmt "%a.t" format_to_module_name (`Ename e)
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hov 2>%a ->@ %a@]" format_typ_with_parens t1
      format_typ_with_parens t2
  | TArray t1 -> Format.fprintf fmt "@[%a@ array@]" format_typ_with_parens t1
  | TAny -> Format.fprintf fmt "_"

let format_var (fmt : Format.formatter) (v : 'm var) : unit =
  let lowercase_name = to_snake_case (to_ascii (Bindlib.name_of v)) in
  let lowercase_name =
    Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\\.")
      ~subst:(fun _ -> "_dot_")
      lowercase_name
  in
  let lowercase_name = avoid_keywords (to_ascii lowercase_name) in
  if
    List.mem lowercase_name ["handle_default"; "handle_default_opt"]
    || begins_with_uppercase (Bindlib.name_of v)
  then Format.fprintf fmt "%s" lowercase_name
  else if lowercase_name = "_" then Format.fprintf fmt "%s" lowercase_name
  else (
    Cli.debug_print "lowercase_name: %s " lowercase_name;
    Format.fprintf fmt "%s_" lowercase_name)

let needs_parens (e : 'm marked_expr) : bool =
  match Marked.unmark e with
  | EApp ((EAbs (_, _), _), _)
  | ELit (LBool _ | LUnit)
  | EVar _ | ETuple _ | EOp _ ->
    false
  | _ -> true

let format_exception (fmt : Format.formatter) (exc : except Marked.pos) : unit =
  match Marked.unmark exc with
  | ConflictError ->
    let pos = Marked.get_mark exc in
    Format.fprintf fmt
      "(ConflictError@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ \
       start_column=%d;@ end_line=%d; end_column=%d;@ law_headings=%a}@])"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos)
  | EmptyError -> Format.fprintf fmt "EmptyError"
  | Crash -> Format.fprintf fmt "Crash"
  | NoValueProvided ->
    let pos = Marked.get_mark exc in
    Format.fprintf fmt
      "(NoValueProvided@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ \
       start_column=%d;@ end_line=%d; end_column=%d;@ law_headings=%a}@])"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos)

let rec format_expr
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    (e : 'm marked_expr) : unit =
  let format_expr = format_expr ctx in
  let format_with_parens (fmt : Format.formatter) (e : 'm marked_expr) =
    if needs_parens e then Format.fprintf fmt "(%a)" format_expr e
    else Format.fprintf fmt "%a" format_expr e
  in
  match Marked.unmark e with
  | EVar v -> Format.fprintf fmt "%a" format_var v
  | ETuple (es, None) ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt e -> Format.fprintf fmt "%a" format_with_parens e))
      es
  | ETuple (es, Some s) ->
    if List.length es = 0 then Format.fprintf fmt "()"
    else
      Format.fprintf fmt "{@[<hov 2>%a@]}"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (e, struct_field) ->
             Format.fprintf fmt "@[<hov 2>%a =@ %a@]" format_struct_field_name
               (Some s, struct_field) format_with_parens e))
        (List.combine es (List.map fst (find_struct s ctx)))
  | EArray es ->
    Format.fprintf fmt "@[<hov 2>[|%a|]@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
         (fun fmt e -> Format.fprintf fmt "%a" format_with_parens e))
      es
  | ETupleAccess (e1, n, s, ts) -> (
    match s with
    | None ->
      Format.fprintf fmt "let@ %a@ = %a@ in@ x"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt i -> Format.fprintf fmt "%s" (if i = n then "x" else "_")))
        (List.mapi (fun i _ -> i) ts)
        format_with_parens e1
    | Some s ->
      Format.fprintf fmt "%a.%a" format_with_parens e1 format_struct_field_name
        (Some s, fst (List.nth (find_struct s ctx) n)))
  | EInj (e, n, en, _ts) ->
    Format.fprintf fmt "@[<hov 2>%a.%a@ %a@]" format_to_module_name (`Ename en)
      format_enum_cons_name
      (fst (List.nth (find_enum en ctx) n))
      format_with_parens e
  | EMatch (e, es, e_name) ->
    Format.fprintf fmt "@[<hv>@[<hov 2>match@ %a@]@ with@\n| %a@]"
      format_with_parens e
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ | ")
         (fun fmt (e, c) ->
           Format.fprintf fmt "@[<hov 2>%a.%a %a@]" format_to_module_name
             (`Ename e_name) format_enum_cons_name c
             (fun fmt e ->
               match Marked.unmark e with
               | EAbs (binder, _) ->
                 let xs, body = Bindlib.unmbind binder in
                 Format.fprintf fmt "%a ->@ %a"
                   (Format.pp_print_list
                      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
                      (fun fmt x -> Format.fprintf fmt "%a" format_var x))
                   (Array.to_list xs) format_with_parens body
               | _ -> assert false
               (* should not happen *))
             e))
      (List.combine es (List.map fst (find_enum e_name ctx)))
  | ELit l -> Format.fprintf fmt "%a" format_lit (Marked.mark (Expr.pos e) l)
  | EApp ((EAbs (binder, taus), _), args) ->
    let xs, body = Bindlib.unmbind binder in
    let xs_tau = List.map2 (fun x tau -> x, tau) (Array.to_list xs) taus in
    let xs_tau_arg = List.map2 (fun (x, tau) arg -> x, tau, arg) xs_tau args in
    Format.fprintf fmt "(%a%a)"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
         (fun fmt (x, tau, arg) ->
           Format.fprintf fmt "@[<hov 2>let@ %a@ :@ %a@ =@ %a@]@ in@\n"
             format_var x format_typ tau format_with_parens arg))
      xs_tau_arg format_with_parens body
  | EAbs (binder, taus) ->
    let xs, body = Bindlib.unmbind binder in
    let xs_tau = List.map2 (fun x tau -> x, tau) (Array.to_list xs) taus in
    Format.fprintf fmt "@[<hov 2>fun@ %a ->@ %a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (x, tau) ->
           Format.fprintf fmt "@[<hov 2>(%a:@ %a)@]" format_var x format_typ tau))
      xs_tau format_expr body
  | EApp
      ((EOp (Binop ((Map | Filter) as op)), _), [arg1; arg2])
    ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" format_binop (op, Pos.no_pos)
      format_with_parens arg1 format_with_parens arg2
  | EApp ((EOp (Binop op), _), [arg1; arg2]) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" format_with_parens arg1
      format_binop (op, Pos.no_pos) format_with_parens arg2
  | EApp ((EApp ((EOp (Unop (Log (BeginCall, info))), _), [f]), _), [arg])
    when !Cli.trace_flag ->
    Format.fprintf fmt "(log_begin_call@ %a@ %a)@ %a" format_uid_list info
      format_with_parens f format_with_parens arg
  | EApp ((EOp (Unop (Log (VarDef tau, info))), _), [arg1])
    when !Cli.trace_flag ->
    Format.fprintf fmt "(log_variable_definition@ %a@ (%a)@ %a)" format_uid_list
      info typ_embedding_name (tau, Pos.no_pos) format_with_parens arg1
  | EApp ((EOp (Unop (Log (PosRecordIfTrueBool, _))), m), [arg1])
    when !Cli.trace_flag ->
    let pos = Expr.mark_pos m in
    Format.fprintf fmt
      "(log_decision_taken@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ \
       start_column=%d;@ end_line=%d; end_column=%d;@ law_headings=%a}@]@ %a)"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos) format_with_parens arg1
  | EApp ((EOp (Unop (Log (EndCall, info))), _), [arg1])
    when !Cli.trace_flag ->
    Format.fprintf fmt "(log_end_call@ %a@ %a)" format_uid_list info
      format_with_parens arg1
  | EApp ((EOp (Unop (Log _)), _), [arg1]) ->
    Format.fprintf fmt "%a" format_with_parens arg1
  | EApp ((EOp (Unop op), _), [arg1]) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_unop (op, Pos.no_pos)
      format_with_parens arg1
  | EApp ((EVar x, pos), args)
    when Var.compare x (Var.translate Ast.handle_default) = 0
         || Var.compare x (Var.translate Ast.handle_default_opt) = 0 ->
    Format.fprintf fmt
      "@[<hov 2>%a@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ \
       start_column=%d;@ end_line=%d; end_column=%d;@ law_headings=%a}@]@ %a@]"
      format_var x
      (Pos.get_file (Expr.mark_pos pos))
      (Pos.get_start_line (Expr.mark_pos pos))
      (Pos.get_start_column (Expr.mark_pos pos))
      (Pos.get_end_line (Expr.mark_pos pos))
      (Pos.get_end_column (Expr.mark_pos pos))
      format_string_list
      (Pos.get_law_info (Expr.mark_pos pos))
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         format_with_parens)
      args
  | EApp (f, args) ->
    Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_with_parens f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         format_with_parens)
      args
  | EIfThenElse (e1, e2, e3) ->
    Format.fprintf fmt
      "@[<hov 2> if@ @[<hov 2>%a@]@ then@ @[<hov 2>%a@]@ else@ @[<hov 2>%a@]@]"
      format_with_parens e1 format_with_parens e2 format_with_parens e3
  | EOp (Ternop op) -> Format.fprintf fmt "%a" format_ternop (op, Pos.no_pos)
  | EOp (Binop op) -> Format.fprintf fmt "%a" format_binop (op, Pos.no_pos)
  | EOp (Unop op) -> Format.fprintf fmt "%a" format_unop (op, Pos.no_pos)
  | EAssert e' ->
    Format.fprintf fmt
      "@[<hov 2>if@ %a@ then@ ()@ else@ raise (AssertionFailed @[<hov \
       2>{filename = \"%s\";@ start_line=%d;@ start_column=%d;@ end_line=%d; \
       end_column=%d;@ law_headings=%a}@])@]"
      format_with_parens e'
      (Pos.get_file (Expr.pos e'))
      (Pos.get_start_line (Expr.pos e'))
      (Pos.get_start_column (Expr.pos e'))
      (Pos.get_end_line (Expr.pos e'))
      (Pos.get_end_column (Expr.pos e'))
      format_string_list
      (Pos.get_law_info (Expr.pos e'))
  | ERaise exc -> Format.fprintf fmt "raise@ %a" format_exception (exc, Expr.pos e)
  | ECatch (e1, exc, e2) ->
    Format.fprintf fmt
      "@,@[<hv>@[<hov 2>try@ %a@]@ with@]@ @[<hov 2>%a@ ->@ %a@]"
      format_with_parens e1 format_exception
      (exc, Expr.pos e)
      format_with_parens e2

let format_struct_embedding
    (fmt : Format.formatter)
    ((struct_name, struct_fields) :
      StructName.t * (StructFieldName.t * typ Marked.pos) list) =
  if List.length struct_fields = 0 then
    Format.fprintf fmt "let embed_%a (_: %a.t) : runtime_value = Unit@\n@\n"
      format_struct_name struct_name format_to_module_name (`Sname struct_name)
  else
    Format.fprintf fmt
      "@[<hov 2>let embed_%a (x: %a.t) : runtime_value =@ Struct([\"%a\"],@ \
       @[<hov 2>[%a]@])@]@\n\
       @\n"
      format_struct_name struct_name format_to_module_name (`Sname struct_name)
      StructName.format_t struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@\n")
         (fun _fmt (struct_field, struct_field_type) ->
           Format.fprintf fmt "(\"%a\",@ %a@ x.%a)" StructFieldName.format_t
             struct_field typ_embedding_name struct_field_type
             format_struct_field_name
             (Some struct_name, struct_field)))
      struct_fields

let format_enum_embedding
    (fmt : Format.formatter)
    ((enum_name, enum_cases) :
      EnumName.t * (EnumConstructor.t * typ Marked.pos) list) =
  if List.length enum_cases = 0 then
    Format.fprintf fmt "let embed_%a (_: %a.t) : runtime_value = Unit@\n@\n"
      format_to_module_name (`Ename enum_name) format_enum_name enum_name
  else
    Format.fprintf fmt
      "@[<hv 2>@[<hov 2>let embed_%a@ @[<hov 2>(x:@ %a.t)@]@ : runtime_value \
       =@]@ Enum([\"%a\"],@ @[<hov 2>match x with@ %a@])@]@\n\
       @\n"
      format_enum_name enum_name format_to_module_name (`Ename enum_name)
      EnumName.format_t enum_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun _fmt (enum_cons, enum_cons_type) ->
           Format.fprintf fmt "@[<hov 2>| %a x ->@ (\"%a\", %a x)@]"
             format_enum_cons_name enum_cons EnumConstructor.format_t
             enum_cons typ_embedding_name enum_cons_type))
      enum_cases

let format_ctx
    (type_ordering : Scopelang.Dependency.TVertex.t list)
    (fmt : Format.formatter)
    (ctx : decl_ctx) : unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    if List.length struct_fields = 0 then
      Format.fprintf fmt
        "@[<v 2>module %a = struct@\n@[<hov 2>type t = unit@]@]@\nend@\n"
        format_to_module_name (`Sname struct_name)
    else
      Format.fprintf fmt
        "@[<v>@[<v 2>module %a = struct@ @[<hv 2>type t = {@,\
         %a@;\
         <0-2>}@]@]@ end@]@\n"
        format_to_module_name (`Sname struct_name)
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun _fmt (struct_field, struct_field_type) ->
             Format.fprintf fmt "@[<hov 2>%a:@ %a@]" format_struct_field_name
               (None, struct_field) format_typ struct_field_type))
        struct_fields;
    if !Cli.trace_flag then
      format_struct_embedding fmt (struct_name, struct_fields)
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    Format.fprintf fmt
      "module %a = struct@\n@[<hov 2>@ type t =@\n@[<hov 2>  %a@]@\nend@]@\n"
      format_to_module_name (`Ename enum_name)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun _fmt (enum_cons, enum_cons_type) ->
           Format.fprintf fmt "@[<hov 2>| %a@ of@ %a@]" format_enum_cons_name
             enum_cons format_typ enum_cons_type))
      enum_cons;
    if !Cli.trace_flag then format_enum_embedding fmt (enum_name, enum_cons)
  in
  let is_in_type_ordering s =
    List.exists
      (fun struct_or_enum ->
        match struct_or_enum with
        | Scopelang.Dependency.TVertex.Enum _ -> false
        | Scopelang.Dependency.TVertex.Struct s' -> s = s')
      type_ordering
  in
  let scope_structs =
    List.map
      (fun (s, _) -> Scopelang.Dependency.TVertex.Struct s)
      (StructMap.bindings
         (StructMap.filter
            (fun s _ -> not (is_in_type_ordering s))
            ctx.ctx_structs))
  in
  List.iter
    (fun struct_or_enum ->
      match struct_or_enum with
      | Scopelang.Dependency.TVertex.Struct s ->
        Format.fprintf fmt "%a@\n" format_struct_decl (s, find_struct s ctx)
      | Scopelang.Dependency.TVertex.Enum e ->
        Format.fprintf fmt "%a@\n" format_enum_decl (e, find_enum e ctx))
    (type_ordering @ scope_structs)

let rec format_scope_body_expr
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    (scope_lets : ('m Ast.expr, 'm) scope_body_expr) : unit =
  match scope_lets with
  | Result e -> format_expr ctx fmt e
  | ScopeLet scope_let ->
    let scope_let_var, scope_let_next =
      Bindlib.unbind scope_let.scope_let_next
    in
    Format.fprintf fmt "@[<hov 2>let %a: %a = %a in@]@\n%a" format_var
      scope_let_var format_typ scope_let.scope_let_typ (format_expr ctx)
      scope_let.scope_let_expr
      (format_scope_body_expr ctx)
      scope_let_next

let rec format_scopes
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    (scopes : ('m Ast.expr, 'm) scopes) : unit =
  match scopes with
  | Nil -> ()
  | ScopeDef scope_def ->
    let scope_input_var, scope_body_expr =
      Bindlib.unbind scope_def.scope_body.scope_body_expr
    in
    let scope_var, scope_next = Bindlib.unbind scope_def.scope_next in
    Format.fprintf fmt "@\n@\n@[<hov 2>let %a (%a: %a.t) : %a.t =@\n%a@]%a"
      format_var scope_var format_var scope_input_var format_to_module_name
      (`Sname scope_def.scope_body.scope_body_input_struct)
      format_to_module_name
      (`Sname scope_def.scope_body.scope_body_output_struct)
      (format_scope_body_expr ctx)
      scope_body_expr (format_scopes ctx) scope_next

let format_program
    (fmt : Format.formatter)
    (p : 'm Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) : unit =
  Cli.call_unstyled (fun _ ->
      Format.fprintf fmt
        "(** This file has been generated by the Catala compiler, do not edit! \
         *)@\n\
         @\n\
         open Runtime_ocaml.Runtime@\n\
         @\n\
         [@@@@@@ocaml.warning \"-4-26-27-32-41-42\"]@\n\
         @\n\
         %a%a@\n\
         @?"
        (format_ctx type_ordering) p.decl_ctx (format_scopes p.decl_ctx)
        p.scopes)
