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
open Backends
module D = Dcalc.Ast

let format_lit (fmt : Format.formatter) (l : lit Pos.marked) : unit =
  match Pos.unmark l with
  | LBool b -> Dcalc.Print.format_lit fmt (Pos.same_pos_as (Dcalc.Ast.LBool b) l)
  | LInt i -> Format.fprintf fmt "integer_of_string@ \"%s\"" (Runtime.integer_to_string i)
  | LUnit -> Dcalc.Print.format_lit fmt (Pos.same_pos_as Dcalc.Ast.LUnit l)
  | LRat i ->
      Format.fprintf fmt "decimal_of_string \"%a\"" Dcalc.Print.format_lit
        (Pos.same_pos_as (Dcalc.Ast.LRat i) l)
  | LMoney e ->
      Format.fprintf fmt "money_of_cents_string@ \"%s\""
        (Runtime.integer_to_string (Runtime.money_to_cents e))
  | LDate d ->
      Format.fprintf fmt "date_of_numbers %d %d %d"
        (Runtime.integer_to_int (Runtime.year_of_date d))
        (Runtime.integer_to_int (Runtime.month_number_of_date d))
        (Runtime.integer_to_int (Runtime.day_of_month_of_date d))
  | LDuration d ->
      let years, months, days = Runtime.duration_to_years_months_days d in
      Format.fprintf fmt "duration_of_numbers %d %d %d" years months days

let format_op_kind (fmt : Format.formatter) (k : Dcalc.Ast.op_kind) =
  Format.fprintf fmt "%s"
    (match k with KInt -> "!" | KRat -> "&" | KMoney -> "$" | KDate -> "@" | KDuration -> "^")

let _format_log_entry (fmt : Format.formatter) (entry : Dcalc.Ast.log_entry) : unit =
  Format.fprintf fmt "%s"
    (match entry with
    | VarDef _ -> ":="
    | BeginCall -> "→ "
    | EndCall -> "← "
    | PosRecordIfTrueBool -> "☛ ")

let format_binop (fmt : Format.formatter) (op : Dcalc.Ast.binop Pos.marked) : unit =
  match Pos.unmark op with
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

let format_ternop (fmt : Format.formatter) (op : Dcalc.Ast.ternop Pos.marked) : unit =
  match Pos.unmark op with Fold -> Format.fprintf fmt "Array.fold_left"

let format_uid_list (fmt : Format.formatter) (uids : Uid.MarkedString.info list) : unit =
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (fun fmt info -> Format.fprintf fmt "\"%a\"" Utils.Uid.MarkedString.format_info info))
    uids

let format_string_list (fmt : Format.formatter) (uids : string list) : unit =
  Format.fprintf fmt "@[<hov 2>[%a]@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (fun fmt info -> Format.fprintf fmt "\"%s\"" info))
    uids

let format_unop (fmt : Format.formatter) (op : Dcalc.Ast.unop Pos.marked) : unit =
  match Pos.unmark op with
  | Minus k -> Format.fprintf fmt "~-%a" format_op_kind k
  | Not -> Format.fprintf fmt "%s" "not"
  | Log (_entry, _infos) ->
      (* Errors.raise_spanned_error "Internal error: a log operator has not been caught by the
         expression match" (Pos.get_position op) *)
      Format.fprintf fmt "Fun.id"
  | Length -> Format.fprintf fmt "%s" "array_length"
  | IntToRat -> Format.fprintf fmt "%s" "decimal_of_integer"
  | GetDay -> Format.fprintf fmt "%s" "day_of_month_of_date"
  | GetMonth -> Format.fprintf fmt "%s" "month_number_of_date"
  | GetYear -> Format.fprintf fmt "%s" "year_of_date"

let avoid_keywords (s : string) : string =
  if
    match s with
    (* list taken from http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#sss:keywords *)
    | "and" | "as" | "assert" | "asr" | "begin" | "class" | "constraint" | "do" | "done" | "downto"
    | "else" | "end" | "exception" | "external" | "false" | "for" | "fun" | "function" | "functor"
    | "if" | "in" | "include" | "inherit" | "initializer" | "land" | "lazy" | "let" | "lor" | "lsl"
    | "lsr" | "lxor" | "match" | "method" | "mod" | "module" | "mutable" | "new" | "nonrec"
    | "object" | "of" | "open" | "or" | "private" | "rec" | "sig" | "struct" | "then" | "to"
    | "true" | "try" | "type" | "val" | "virtual" | "when" | "while" | "with" ->
        true
    | _ -> false
  then s ^ "_"
  else s

let format_struct_name (fmt : Format.formatter) (v : Dcalc.Ast.StructName.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords
       (to_lowercase (to_ascii (Format.asprintf "%a" Dcalc.Ast.StructName.format_t v))))

let format_struct_field_name (fmt : Format.formatter) (v : Dcalc.Ast.StructFieldName.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords (to_ascii (Format.asprintf "%a" Dcalc.Ast.StructFieldName.format_t v)))

let format_enum_name (fmt : Format.formatter) (v : Dcalc.Ast.EnumName.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords (to_lowercase (to_ascii (Format.asprintf "%a" Dcalc.Ast.EnumName.format_t v))))

let format_enum_cons_name (fmt : Format.formatter) (v : Dcalc.Ast.EnumConstructor.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords (to_ascii (Format.asprintf "%a" Dcalc.Ast.EnumConstructor.format_t v)))

let rec typ_embedding_name (fmt : Format.formatter) (ty : D.typ Pos.marked) : unit =
  match Pos.unmark ty with
  | D.TLit D.TUnit -> Format.fprintf fmt "embed_unit"
  | D.TLit D.TBool -> Format.fprintf fmt "embed_bool"
  | D.TLit D.TInt -> Format.fprintf fmt "embed_integer"
  | D.TLit D.TRat -> Format.fprintf fmt "embed_decimal"
  | D.TLit D.TMoney -> Format.fprintf fmt "embed_money"
  | D.TLit D.TDate -> Format.fprintf fmt "embed_date"
  | D.TLit D.TDuration -> Format.fprintf fmt "embed_duration"
  | D.TTuple (_, Some s_name) -> Format.fprintf fmt "embed_%a" format_struct_name s_name
  | D.TEnum (_, e_name) -> Format.fprintf fmt "embed_%a" format_enum_name e_name
  | D.TArray ty -> Format.fprintf fmt "embed_array (%a)" typ_embedding_name ty
  | _ -> Format.fprintf fmt "unembeddable"

let typ_needs_parens (e : Dcalc.Ast.typ Pos.marked) : bool =
  match Pos.unmark e with TArrow _ | TArray _ -> true | _ -> false

let rec format_typ (fmt : Format.formatter) (typ : Dcalc.Ast.typ Pos.marked) : unit =
  let format_typ = format_typ in
  let format_typ_with_parens (fmt : Format.formatter) (t : Dcalc.Ast.typ Pos.marked) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  match Pos.unmark typ with
  | TLit l -> Format.fprintf fmt "%a" Dcalc.Print.format_tlit l
  | TTuple (ts, None) ->
      Format.fprintf fmt "@[<hov 2>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ *@ ")
           (fun fmt t -> Format.fprintf fmt "%a" format_typ_with_parens t))
        ts
  | TTuple (_, Some s) -> Format.fprintf fmt "%a" format_struct_name s
  | TEnum (_, e) -> Format.fprintf fmt "%a" format_enum_name e
  | TArrow (t1, t2) ->
      Format.fprintf fmt "@[<hov 2>%a ->@ %a@]" format_typ_with_parens t1 format_typ_with_parens t2
  | TArray t1 -> Format.fprintf fmt "@[%a@ array@]" format_typ_with_parens t1
  | TAny -> Format.fprintf fmt "_"

let format_var (fmt : Format.formatter) (v : Var.t) : unit =
  let lowercase_name =
    to_lowercase (to_ascii (Bindlib.name_of v) ^ "_" ^ string_of_int (Bindlib.uid_of v))
  in
  let lowercase_name =
    Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\\.") ~subst:(fun _ -> "_dot_") lowercase_name
  in
  let lowercase_name = avoid_keywords (to_ascii lowercase_name) in
  if lowercase_name = "handle_default" || Dcalc.Print.begins_with_uppercase (Bindlib.name_of v) then
    Format.fprintf fmt "%s" lowercase_name
  else if lowercase_name = "_" then Format.fprintf fmt "%s" lowercase_name
  else Format.fprintf fmt "%s_" lowercase_name

let needs_parens (e : expr Pos.marked) : bool =
  match Pos.unmark e with
  | EApp ((EAbs (_, _), _), _) | ELit (LBool _ | LUnit) | EVar _ | ETuple _ | EOp _ -> false
  | _ -> true

let format_exception (fmt : Format.formatter) (exc : except Pos.marked) : unit =
  match Pos.unmark exc with
  | ConflictError -> Format.fprintf fmt "ConflictError"
  | EmptyError -> Format.fprintf fmt "EmptyError"
  | Crash -> Format.fprintf fmt "Crash"
  | NoValueProvided ->
      let pos = Pos.get_position exc in
      Format.fprintf fmt
        "(NoValueProvided@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ start_column=%d;@ \
         end_line=%d; end_column=%d;@ law_headings=%a}@])"
        (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
        (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list (Pos.get_law_info pos)

let rec format_expr (ctx : Dcalc.Ast.decl_ctx) (fmt : Format.formatter) (e : expr Pos.marked) : unit
    =
  let format_expr = format_expr ctx in
  let format_with_parens (fmt : Format.formatter) (e : expr Pos.marked) =
    if needs_parens e then Format.fprintf fmt "(%a)" format_expr e
    else Format.fprintf fmt "%a" format_expr e
  in
  match Pos.unmark e with
  | EVar v -> Format.fprintf fmt "%a" format_var (Pos.unmark v)
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
               Format.fprintf fmt "@[<hov 2>%a =@ %a@]" format_struct_field_name struct_field
                 format_with_parens e))
          (List.combine es (List.map fst (Dcalc.Ast.StructMap.find s ctx.ctx_structs)))
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
            (fst (List.nth (Dcalc.Ast.StructMap.find s ctx.ctx_structs) n)))
  | EInj (e, n, en, _ts) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_enum_cons_name
        (fst (List.nth (Dcalc.Ast.EnumMap.find en ctx.ctx_enums) n))
        format_with_parens e
  | EMatch (e, es, e_name) ->
      Format.fprintf fmt "@[<hov 2>match@ %a@]@ with@\n%a" format_with_parens e
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n| ")
           (fun fmt (e, c) ->
             Format.fprintf fmt "%a %a" format_enum_cons_name c
               (fun fmt e ->
                 match Pos.unmark e with
                 | EAbs ((binder, _), _) ->
                     let xs, body = Bindlib.unmbind binder in
                     Format.fprintf fmt "%a ->@[<hov 2>@ %a@]"
                       (Format.pp_print_list
                          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
                          (fun fmt x -> Format.fprintf fmt "%a" format_var x))
                       (Array.to_list xs) format_with_parens body
                 | _ -> assert false
                 (* should not happen *))
               e))
        (List.combine es (List.map fst (Dcalc.Ast.EnumMap.find e_name ctx.ctx_enums)))
  | ELit l -> Format.fprintf fmt "%a" format_lit (Pos.same_pos_as l e)
  | EApp ((EAbs ((binder, _), taus), _), args) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      let xs_tau_arg = List.map2 (fun (x, tau) arg -> (x, tau, arg)) xs_tau args in
      Format.fprintf fmt "%a%a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "")
           (fun fmt (x, tau, arg) ->
             Format.fprintf fmt "@[<hov 2>let@ %a@ :@ %a@ =@ %a@]@ in@\n" format_var x format_typ
               tau format_with_parens arg))
        xs_tau_arg format_with_parens body
  | EAbs ((binder, _), taus) ->
      let xs, body = Bindlib.unmbind binder in
      let xs_tau = List.map2 (fun x tau -> (x, tau)) (Array.to_list xs) taus in
      Format.fprintf fmt "@[<hov 2>fun@ %a ->@ %a@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
           (fun fmt (x, tau) ->
             Format.fprintf fmt "@[<hov 2>(%a:@ %a)@]" format_var x format_typ tau))
        xs_tau format_expr body
  | EApp ((EOp (Binop ((Dcalc.Ast.Map | Dcalc.Ast.Filter) as op)), _), [ arg1; arg2 ]) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" format_binop (op, Pos.no_pos) format_with_parens
        arg1 format_with_parens arg2
  | EApp ((EOp (Binop op), _), [ arg1; arg2 ]) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@]" format_with_parens arg1 format_binop
        (op, Pos.no_pos) format_with_parens arg2
  | EApp ((EApp ((EOp (Unop (D.Log (D.BeginCall, info))), _), [ f ]), _), [ arg ])
    when !Cli.trace_flag ->
      Format.fprintf fmt "(log_begin_call@ %a@ %a@ %a)" format_uid_list info format_with_parens f
        format_with_parens arg
  | EApp ((EOp (Unop (D.Log (D.VarDef tau, info))), _), [ arg1 ]) when !Cli.trace_flag ->
      Format.fprintf fmt "(log_variable_definition@ %a@ (%a)@ %a)" format_uid_list info
        typ_embedding_name (tau, Pos.no_pos) format_with_parens arg1
  | EApp ((EOp (Unop (D.Log (D.PosRecordIfTrueBool, _))), pos), [ arg1 ]) when !Cli.trace_flag ->
      Format.fprintf fmt
        "(log_decision_taken@ @[<hov 2>{filename = \"%s\";@ start_line=%d;@ start_column=%d;@ \
         end_line=%d; end_column=%d;@ law_headings=%a}@]@ %a)"
        (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
        (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list (Pos.get_law_info pos)
        format_with_parens arg1
  | EApp ((EOp (Unop (D.Log (D.EndCall, info))), _), [ arg1 ]) when !Cli.trace_flag ->
      Format.fprintf fmt "(log_end_call@ %a@ %a)" format_uid_list info format_with_parens arg1
  | EApp ((EOp (Unop (D.Log _)), _), [ arg1 ]) -> Format.fprintf fmt "%a" format_with_parens arg1
  | EApp ((EOp (Unop op), _), [ arg1 ]) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_unop (op, Pos.no_pos) format_with_parens arg1
  | EApp (f, args) ->
      Format.fprintf fmt "@[<hov 2>%a@ %a@]" format_with_parens f
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ") format_with_parens)
        args
  | EIfThenElse (e1, e2, e3) ->
      Format.fprintf fmt "@[<hov 2> if@ @[<hov 2>%a@]@ then@ @[<hov 2>%a@]@ else@ @[<hov 2>%a@]@]"
        format_with_parens e1 format_with_parens e2 format_with_parens e3
  | EOp (Ternop op) -> Format.fprintf fmt "%a" format_ternop (op, Pos.no_pos)
  | EOp (Binop op) -> Format.fprintf fmt "%a" format_binop (op, Pos.no_pos)
  | EOp (Unop op) -> Format.fprintf fmt "%a" format_unop (op, Pos.no_pos)
  | EAssert e' ->
      Format.fprintf fmt "@[<hov 2>if @ %a@ then@ ()@ else@ raise AssertionFailed@]"
        format_with_parens e'
  | ERaise exc -> Format.fprintf fmt "raise@ %a" format_exception (exc, Pos.get_position e)
  | ECatch (e1, exc, e2) ->
      Format.fprintf fmt "@[<hov 2>try@ %a@ with@ %a@ ->@ %a@]" format_with_parens e1
        format_exception
        (exc, Pos.get_position e)
        format_with_parens e2
  | ESome e1 -> Format.fprintf fmt "@[<hov 2> Some@ %a@ @]" format_with_parens e1
  | ENone -> Format.fprintf fmt "None"
  | EMatchopt (e1, e2, e3) ->
      let x = Ast.Var.make ("x", Pos.no_pos) in
      Format.fprintf fmt
        "@[<hov 2>match@ %a@]@ with@\n| None ->@[<hov 2>@ %a@]@\n| Some %a ->@[<hov 2>@ %a@ %a@]"
        format_expr e1 format_with_parens e2 format_var x format_with_parens e3 format_var x

let format_struct_embedding (fmt : Format.formatter)
    ((struct_name, struct_fields) : D.StructName.t * (D.StructFieldName.t * D.typ Pos.marked) list)
    =
  if List.length struct_fields = 0 then
    Format.fprintf fmt "let embed_%a (_: %a) : runtime_value = Unit@\n@\n" format_struct_name
      struct_name format_struct_name struct_name
  else
    Format.fprintf fmt
      "@[<hov 2>let embed_%a (x: %a) : runtime_value =@ Struct([\"%a\"],@ @[<hov 2>[%a]@])@]@\n@\n"
      format_struct_name struct_name format_struct_name struct_name D.StructName.format_t
      struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@\n")
         (fun _fmt (struct_field, struct_field_type) ->
           Format.fprintf fmt "(\"%a\",@ %a@ x.%a)" D.StructFieldName.format_t struct_field
             typ_embedding_name struct_field_type format_struct_field_name struct_field))
      struct_fields

let format_enum_embedding (fmt : Format.formatter)
    ((enum_name, enum_cases) : D.EnumName.t * (D.EnumConstructor.t * D.typ Pos.marked) list) =
  if List.length enum_cases = 0 then
    Format.fprintf fmt "let embed_%a (_: %a) : runtime_value = Unit@\n@\n" format_enum_name
      enum_name format_enum_name enum_name
  else
    Format.fprintf fmt
      "@[<hov 2>let embed_%a (x: %a) : runtime_value =@ Enum([\"%a\"],@ @[<hov 2>match x with@ \
       %a@])@]@\n\
       @\n"
      format_enum_name enum_name format_enum_name enum_name D.EnumName.format_t enum_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun _fmt (enum_cons, enum_cons_type) ->
           Format.fprintf fmt "@[<hov 2>| %a x ->@ (\"%a\", %a x)@]" format_enum_cons_name enum_cons
             D.EnumConstructor.format_t enum_cons typ_embedding_name enum_cons_type))
      enum_cases

let format_ctx (type_ordering : Scopelang.Dependency.TVertex.t list) (fmt : Format.formatter)
    (ctx : D.decl_ctx) : unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    if List.length struct_fields = 0 then
      Format.fprintf fmt "type %a = unit@\n@\n" format_struct_name struct_name
    else
      Format.fprintf fmt "type %a = {@\n@[<hov 2>  %a@]@\n}@\n@\n" format_struct_name struct_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           (fun _fmt (struct_field, struct_field_type) ->
             Format.fprintf fmt "%a:@ %a;" format_struct_field_name struct_field format_typ
               struct_field_type))
        struct_fields;
    if !Cli.trace_flag then format_struct_embedding fmt (struct_name, struct_fields)
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if List.length enum_cons = 0 then
      Format.fprintf fmt "type %a = unit@\n@\n" format_enum_name enum_name
    else
      Format.fprintf fmt "type %a =@\n@[<hov 2>  %a@]@\n@\n" format_enum_name enum_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           (fun _fmt (enum_cons, enum_cons_type) ->
             Format.fprintf fmt "| %a@ of@ %a" format_enum_cons_name enum_cons format_typ
               enum_cons_type))
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
      (Dcalc.Ast.StructMap.bindings
         (Dcalc.Ast.StructMap.filter (fun s _ -> not (is_in_type_ordering s)) ctx.ctx_structs))
  in
  List.iter
    (fun struct_or_enum ->
      match struct_or_enum with
      | Scopelang.Dependency.TVertex.Struct s ->
          Format.fprintf fmt "%a@\n@\n" format_struct_decl
            (s, Dcalc.Ast.StructMap.find s ctx.Dcalc.Ast.ctx_structs)
      | Scopelang.Dependency.TVertex.Enum e ->
          Format.fprintf fmt "%a@\n@\n" format_enum_decl
            (e, Dcalc.Ast.EnumMap.find e ctx.Dcalc.Ast.ctx_enums))
    (type_ordering @ scope_structs)

let format_program (fmt : Format.formatter) (p : Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) : unit =
  Cli.style_flag := false;
  Format.fprintf fmt
    "(** This file has been generated by the Catala compiler, do not edit! *)@\n\
     @\n\
     open Runtime@\n\
     @\n\
     [@@@@@@ocaml.warning \"-4-26-27-32-41-42\"]@\n\
     @\n\
     %a@\n\
     @\n\
     %a@?"
    (format_ctx type_ordering) p.decl_ctx
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n@\n")
       (fun fmt (name, e) ->
         Format.fprintf fmt "@[<hov 2>let@ %a@ =@ %a@]" format_var name (format_expr p.decl_ctx) e))
    p.scopes
