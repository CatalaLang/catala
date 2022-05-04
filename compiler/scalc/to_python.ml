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
[@@@warning "-32-27"]

open Utils
open Ast
open Lcalc.Backends
module D = Dcalc.Ast
module L = Lcalc.Ast

let format_lit (fmt : Format.formatter) (l : L.lit Pos.marked) : unit =
  match Pos.unmark l with
  | LBool true -> Format.fprintf fmt "True"
  | LBool false -> Format.fprintf fmt "False"
  | LInt i ->
    Format.fprintf fmt "integer_of_string(\"%s\")" (Runtime.integer_to_string i)
  | LUnit -> Format.fprintf fmt "Unit()"
  | LRat i ->
    Format.fprintf fmt "decimal_of_string(\"%a\")" Dcalc.Print.format_lit
      (Pos.same_pos_as (Dcalc.Ast.LRat i) l)
  | LMoney e ->
    Format.fprintf fmt "money_of_cents_string(\"%s\")"
      (Runtime.integer_to_string (Runtime.money_to_cents e))
  | LDate d ->
    Format.fprintf fmt "date_of_numbers(%d,%d,%d)"
      (Runtime.integer_to_int (Runtime.year_of_date d))
      (Runtime.integer_to_int (Runtime.month_number_of_date d))
      (Runtime.integer_to_int (Runtime.day_of_month_of_date d))
  | LDuration d ->
    let years, months, days = Runtime.duration_to_years_months_days d in
    Format.fprintf fmt "duration_of_numbers(%d,%d,%d)" years months days

let format_log_entry (fmt : Format.formatter) (entry : Dcalc.Ast.log_entry) :
    unit =
  match entry with
  | VarDef _ -> Format.fprintf fmt ":="
  | BeginCall -> Format.fprintf fmt "→ "
  | EndCall -> Format.fprintf fmt "%s" "← "
  | PosRecordIfTrueBool -> Format.fprintf fmt "☛ "

let format_binop (fmt : Format.formatter) (op : Dcalc.Ast.binop Pos.marked) :
    unit =
  match Pos.unmark op with
  | Add _ | Concat -> Format.fprintf fmt "+"
  | Sub _ -> Format.fprintf fmt "-"
  | Mult _ -> Format.fprintf fmt "*"
  | Div D.KInt -> Format.fprintf fmt "//"
  | Div _ -> Format.fprintf fmt "/"
  | And -> Format.fprintf fmt "and"
  | Or -> Format.fprintf fmt "or"
  | Eq -> Format.fprintf fmt "=="
  | Neq | Xor -> Format.fprintf fmt "!="
  | Lt _ -> Format.fprintf fmt "<"
  | Lte _ -> Format.fprintf fmt "<="
  | Gt _ -> Format.fprintf fmt ">"
  | Gte _ -> Format.fprintf fmt ">="
  | Map -> Format.fprintf fmt "list_map"
  | Filter -> Format.fprintf fmt "list_filter"

let format_ternop (fmt : Format.formatter) (op : Dcalc.Ast.ternop Pos.marked) :
    unit =
  match Pos.unmark op with Fold -> Format.fprintf fmt "list_fold_left"

let format_uid_list (fmt : Format.formatter) (uids : Uid.MarkedString.info list)
    : unit =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       (fun fmt info ->
         Format.fprintf fmt "\"%a\"" Utils.Uid.MarkedString.format_info info))
    uids

let format_string_list (fmt : Format.formatter) (uids : string list) : unit =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       (fun fmt info -> Format.fprintf fmt "\"%s\"" info))
    uids

let format_unop (fmt : Format.formatter) (op : Dcalc.Ast.unop Pos.marked) : unit
    =
  match Pos.unmark op with
  | Minus _ -> Format.fprintf fmt "-"
  | Not -> Format.fprintf fmt "not"
  | Log (entry, infos) -> assert false (* should not happen *)
  | Length -> Format.fprintf fmt "%s" "list_length"
  | IntToRat -> Format.fprintf fmt "%s" "decimal_of_integer"
  | GetDay -> Format.fprintf fmt "%s" "day_of_month_of_date"
  | GetMonth -> Format.fprintf fmt "%s" "month_number_of_date"
  | GetYear -> Format.fprintf fmt "%s" "year_of_date"
  | RoundMoney -> Format.fprintf fmt "%s" "money_round"
  | RoundDecimal -> Format.fprintf fmt "%s" "decimal_round"

let avoid_keywords (s : string) : string =
  if
    match s with
    (* list taken from
       https://www.programiz.com/python-programming/keyword-list *)
    | "False" | "None" | "True" | "and" | "as" | "assert" | "async" | "await"
    | "break" | "class" | "continue" | "def" | "del" | "elif" | "else"
    | "except" | "finally" | "for" | "from" | "global" | "if" | "import" | "in"
    | "is" | "lambda" | "nonlocal" | "not" | "or" | "pass" | "raise" | "return"
    | "try" | "while" | "with" | "yield" ->
      true
    | _ -> false
  then s ^ "_"
  else s

let format_struct_name (fmt : Format.formatter) (v : Dcalc.Ast.StructName.t) :
    unit =
  Format.fprintf fmt "%s"
    (avoid_keywords
       (to_uppercase
          (to_ascii (Format.asprintf "%a" Dcalc.Ast.StructName.format_t v))))

let format_struct_field_name
    (fmt : Format.formatter)
    (v : Dcalc.Ast.StructFieldName.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords
       (to_ascii (Format.asprintf "%a" Dcalc.Ast.StructFieldName.format_t v)))

let format_enum_name (fmt : Format.formatter) (v : Dcalc.Ast.EnumName.t) : unit
    =
  Format.fprintf fmt "%s"
    (avoid_keywords
       (to_uppercase
          (to_ascii (Format.asprintf "%a" Dcalc.Ast.EnumName.format_t v))))

let format_enum_cons_name
    (fmt : Format.formatter)
    (v : Dcalc.Ast.EnumConstructor.t) : unit =
  Format.fprintf fmt "%s"
    (avoid_keywords
       (to_ascii (Format.asprintf "%a" Dcalc.Ast.EnumConstructor.format_t v)))

let typ_needs_parens (e : Dcalc.Ast.typ Pos.marked) : bool =
  match Pos.unmark e with TArrow _ | TArray _ -> true | _ -> false

let rec format_typ (fmt : Format.formatter) (typ : Dcalc.Ast.typ Pos.marked) :
    unit =
  let format_typ = format_typ in
  let format_typ_with_parens
      (fmt : Format.formatter)
      (t : Dcalc.Ast.typ Pos.marked) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  match Pos.unmark typ with
  | TLit TUnit -> Format.fprintf fmt "Unit"
  | TLit TMoney -> Format.fprintf fmt "Money"
  | TLit TInt -> Format.fprintf fmt "Integer"
  | TLit TRat -> Format.fprintf fmt "Decimal"
  | TLit TDate -> Format.fprintf fmt "Date"
  | TLit TDuration -> Format.fprintf fmt "Duration"
  | TLit TBool -> Format.fprintf fmt "bool"
  | TTuple (ts, None) ->
    Format.fprintf fmt "Tuple[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt t -> Format.fprintf fmt "%a" format_typ_with_parens t))
      ts
  | TTuple (_, Some s) -> Format.fprintf fmt "%a" format_struct_name s
  | TEnum ([ _; some_typ ], e) when D.EnumName.compare e L.option_enum = 0 ->
    (* We translate the option type with an overloading by Python's [None] *)
    Format.fprintf fmt "Optional[%a]" format_typ some_typ
  | TEnum (_, e) -> Format.fprintf fmt "%a" format_enum_name e
  | TArrow (t1, t2) ->
    Format.fprintf fmt "Callable[[%a], %a]" format_typ_with_parens t1
      format_typ_with_parens t2
  | TArray t1 -> Format.fprintf fmt "List[%a]" format_typ_with_parens t1
  | TAny -> Format.fprintf fmt "Any"

let format_name_cleaned (fmt : Format.formatter) (s : string) : unit =
  let lowercase_name = to_lowercase (to_ascii s) in
  let lowercase_name =
    Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\\.")
      ~subst:(fun _ -> "_dot_")
      lowercase_name
  in
  let lowercase_name = avoid_keywords (to_ascii lowercase_name) in
  Format.fprintf fmt "%s" lowercase_name

module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

(** For each `LocalName.t` defined by its string and then by its hash, we keep
    track of which local integer id we've given it. This is used to keep
    variable naming with low indices rather than one global counter for all
    variables. TODO: should be removed when
    https://github.com/CatalaLang/catala/issues/240 is fixed. *)
let string_counter_map : int IntMap.t StringMap.t ref = ref StringMap.empty

let format_var (fmt : Format.formatter) (v : LocalName.t) : unit =
  let v_str = Pos.unmark (LocalName.get_info v) in
  let hash = LocalName.hash v in
  let local_id =
    match StringMap.find_opt v_str !string_counter_map with
    | Some ids -> (
      match IntMap.find_opt hash ids with
      | None ->
        let max_id =
          snd
            (List.hd
               (List.fast_sort
                  (fun (_, x) (_, y) -> Int.compare y x)
                  (IntMap.bindings ids)))
        in
        string_counter_map :=
          StringMap.add v_str
            (IntMap.add hash (max_id + 1) ids)
            !string_counter_map;
        max_id + 1
      | Some local_id -> local_id)
    | None ->
      string_counter_map :=
        StringMap.add v_str (IntMap.singleton hash 0) !string_counter_map;
      0
  in
  if v_str = "_" then Format.fprintf fmt "_"
    (* special case for the unit pattern *)
  else if local_id = 0 then format_name_cleaned fmt v_str
  else Format.fprintf fmt "%a_%d" format_name_cleaned v_str local_id

let format_toplevel_name (fmt : Format.formatter) (v : TopLevelName.t) : unit =
  let v_str = Pos.unmark (TopLevelName.get_info v) in
  format_name_cleaned fmt v_str

let needs_parens (e : expr Pos.marked) : bool =
  match Pos.unmark e with
  | ELit (LBool _ | LUnit) | EVar _ | EOp _ -> false
  | _ -> true

let format_exception (fmt : Format.formatter) (exc : L.except Pos.marked) : unit
    =
  match Pos.unmark exc with
  | ConflictError -> Format.fprintf fmt "ConflictError"
  | EmptyError -> Format.fprintf fmt "EmptyError"
  | Crash -> Format.fprintf fmt "Crash"
  | NoValueProvided ->
    let pos = Pos.get_position exc in
    Format.fprintf fmt
      "NoValueProvided(@[<hov 0>SourcePosition(@[<hov 0>filename=\"%s\",@ \
       start_line=%d,@ start_column=%d,@ end_line=%d,@ end_column=%d,@ \
       law_headings=%a)@])@]"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos)

let rec format_expression
    (ctx : Dcalc.Ast.decl_ctx)
    (fmt : Format.formatter)
    (e : expr Pos.marked) : unit =
  match Pos.unmark e with
  | EVar v -> format_var fmt v
  | EFunc f -> format_toplevel_name fmt f
  | EStruct (es, s) ->
    Format.fprintf fmt "%a(%a)" format_struct_name s
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (e, struct_field) ->
           Format.fprintf fmt "%a = %a" format_struct_field_name struct_field
             (format_expression ctx) e))
      (List.combine es
         (List.map fst (Dcalc.Ast.StructMap.find s ctx.ctx_structs)))
  | EStructFieldAccess (e1, field, _) ->
    Format.fprintf fmt "%a.%a" (format_expression ctx) e1
      format_struct_field_name field
  | EInj (_, cons, e_name)
    when D.EnumName.compare e_name L.option_enum = 0
         && D.EnumConstructor.compare cons L.none_constr = 0 ->
    (* We translate the option type with an overloading by Python's [None] *)
    Format.fprintf fmt "None"
  | EInj (e, cons, e_name)
    when D.EnumName.compare e_name L.option_enum = 0
         && D.EnumConstructor.compare cons L.some_constr = 0 ->
    (* We translate the option type with an overloading by Python's [None] *)
    format_expression ctx fmt e
  | EInj (e, cons, enum_name) ->
    Format.fprintf fmt "%a(%a_Code.%a,@ %a)" format_enum_name enum_name
      format_enum_name enum_name format_enum_cons_name cons
      (format_expression ctx) e
  | EArray es ->
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt e -> Format.fprintf fmt "%a" (format_expression ctx) e))
      es
  | ELit l -> Format.fprintf fmt "%a" format_lit (Pos.same_pos_as l e)
  | EApp
      ( (EOp (Binop ((Dcalc.Ast.Map | Dcalc.Ast.Filter) as op)), _),
        [ arg1; arg2 ] ) ->
    Format.fprintf fmt "%a(%a,@ %a)" format_binop (op, Pos.no_pos)
      (format_expression ctx) arg1 (format_expression ctx) arg2
  | EApp ((EOp (Binop op), _), [ arg1; arg2 ]) ->
    Format.fprintf fmt "(%a %a@ %a)" (format_expression ctx) arg1 format_binop
      (op, Pos.no_pos) (format_expression ctx) arg2
  | EApp
      ((EApp ((EOp (Unop (D.Log (D.BeginCall, info))), _), [ f ]), _), [ arg ])
    when !Cli.trace_flag ->
    Format.fprintf fmt "log_begin_call(%a,@ %a,@ %a)" format_uid_list info
      (format_expression ctx) f (format_expression ctx) arg
  | EApp ((EOp (Unop (D.Log (D.VarDef tau, info))), _), [ arg1 ])
    when !Cli.trace_flag ->
    Format.fprintf fmt "log_variable_definition(%a,@ %a)" format_uid_list info
      (format_expression ctx) arg1
  | EApp ((EOp (Unop (D.Log (D.PosRecordIfTrueBool, _))), pos), [ arg1 ])
    when !Cli.trace_flag ->
    Format.fprintf fmt
      "log_decision_taken(SourcePosition(filename=\"%s\",@ start_line=%d,@ \
       start_column=%d,@ end_line=%d, end_column=%d,@ law_headings=%a), %a)"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos) (format_expression ctx) arg1
  | EApp ((EOp (Unop (D.Log (D.EndCall, info))), _), [ arg1 ])
    when !Cli.trace_flag ->
    Format.fprintf fmt "log_end_call(%a,@ %a)" format_uid_list info
      (format_expression ctx) arg1
  | EApp ((EOp (Unop (D.Log _)), _), [ arg1 ]) ->
    Format.fprintf fmt "%a" (format_expression ctx) arg1
  | EApp ((EOp (Unop ((Minus _ | Not) as op)), _), [ arg1 ]) ->
    Format.fprintf fmt "%a %a" format_unop (op, Pos.no_pos)
      (format_expression ctx) arg1
  | EApp ((EOp (Unop op), _), [ arg1 ]) ->
    Format.fprintf fmt "%a(%a)" format_unop (op, Pos.no_pos)
      (format_expression ctx) arg1
  | EApp (f, args) ->
    Format.fprintf fmt "%a(@[<hov 0>%a)@]" (format_expression ctx) f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | EOp (Ternop op) -> Format.fprintf fmt "%a" format_ternop (op, Pos.no_pos)
  | EOp (Binop op) -> Format.fprintf fmt "%a" format_binop (op, Pos.no_pos)
  | EOp (Unop op) -> Format.fprintf fmt "%a" format_unop (op, Pos.no_pos)

let rec format_statement
    (ctx : Dcalc.Ast.decl_ctx)
    (fmt : Format.formatter)
    (s : stmt Pos.marked) : unit =
  match Pos.unmark s with
  | SInnerFuncDef (name, { func_params; func_body }) ->
    Format.fprintf fmt "@[<hov 4>def %a(%a):@\n%a@]" format_var
      (Pos.unmark name)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt (var, typ) ->
           Format.fprintf fmt "%a:%a" format_var (Pos.unmark var) format_typ typ))
      func_params (format_block ctx) func_body
  | SLocalDecl _ ->
    assert false (* We don't need to declare variables in Python *)
  | SLocalDef (v, e) ->
    Format.fprintf fmt "@[<hov 4>%a = %a@]" format_var (Pos.unmark v)
      (format_expression ctx) e
  | STryExcept (try_b, except, catch_b) ->
    Format.fprintf fmt "@[<hov 4>try:@\n%a@]@\n@[<hov 4>except %a:@\n%a@]"
      (format_block ctx) try_b format_exception (except, Pos.no_pos)
      (format_block ctx) catch_b
  | SRaise except ->
    Format.fprintf fmt "@[<hov 4>raise %a@]" format_exception
      (except, Pos.get_position s)
  | SIfThenElse (cond, b1, b2) ->
    Format.fprintf fmt "@[<hov 4>if %a:@\n%a@]@\n@[<hov 4>else:@\n%a@]"
      (format_expression ctx) cond (format_block ctx) b1 (format_block ctx) b2
  | SSwitch (e1, e_name, [ (case_none, _); (case_some, case_some_var) ])
    when D.EnumName.compare e_name L.option_enum = 0 ->
    (* We translate the option type with an overloading by Python's [None] *)
    let tmp_var = LocalName.fresh ("perhaps_none_arg", Pos.no_pos) in
    Format.fprintf fmt
      "%a = %a@\n\
       @[<hov 4>if %a is None:@\n\
       %a@]@\n\
       @[<hov 4>else:@\n\
       %a = %a@\n\
       %a@]"
      format_var tmp_var (format_expression ctx) e1 format_var tmp_var
      (format_block ctx) case_none format_var case_some_var format_var tmp_var
      (format_block ctx) case_some
  | SSwitch (e1, e_name, cases) ->
    let cases =
      List.map2
        (fun (x, y) (cons, _) -> (x, y, cons))
        cases
        (D.EnumMap.find e_name ctx.ctx_enums)
    in
    let tmp_var = LocalName.fresh ("match_arg", Pos.no_pos) in
    Format.fprintf fmt "%a = %a@\n@[<hov 4>if %a@]" format_var tmp_var
      (format_expression ctx) e1
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@]@\n@[<hov 4>elif ")
         (fun fmt (case_block, payload_var, cons_name) ->
           Format.fprintf fmt "%a.code == %a_Code.%a:@\n%a = %a.value@\n%a"
             format_var tmp_var format_enum_name e_name format_enum_cons_name
             cons_name format_var payload_var format_var tmp_var
             (format_block ctx) case_block))
      cases
  | SReturn e1 ->
    Format.fprintf fmt "@[<hov 4>return %a@]" (format_expression ctx)
      (e1, Pos.get_position s)
  | SAssert e1 ->
    Format.fprintf fmt "@[<hov 4>assert %a@]" (format_expression ctx)
      (e1, Pos.get_position s)

and format_block (ctx : Dcalc.Ast.decl_ctx) (fmt : Format.formatter) (b : block)
    : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    (format_statement ctx) fmt
    (List.filter
       (fun s -> match Pos.unmark s with SLocalDecl _ -> false | _ -> true)
       b)

let format_ctx
    (type_ordering : Scopelang.Dependency.TVertex.t list)
    (fmt : Format.formatter)
    (ctx : D.decl_ctx) : unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    Format.fprintf fmt
      "class %a:@\n\
       \tdef __init__(self, %a) -> None:@\n\
       %a@\n\
       @\n\
       \tdef __eq__(self, other: object) -> bool:@\n\
       \t\tif isinstance(other, %a):@\n\
       \t\t\treturn @[<hov>(%a)@]@\n\
       \t\telse:@\n\
       \t\t\treturn False@\n\
       @\n\
       \tdef __ne__(self, other: object) -> bool:@\n\
       \t\treturn not (self == other)@\n\
       @\n\
       \tdef __str__(self) -> str:@\n\
       \t\t@[<hov 4>return \"%a(%a)\".format(%a)@]" format_struct_name
      struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun _fmt (struct_field, struct_field_type) ->
           Format.fprintf fmt "%a: %a" format_struct_field_name struct_field
             format_typ struct_field_type))
      struct_fields
      (if List.length struct_fields = 0 then fun fmt _ ->
       Format.fprintf fmt "\t\tpass"
      else
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
          (fun _fmt (struct_field, _) ->
            Format.fprintf fmt "\t\tself.%a = %a" format_struct_field_name
              struct_field format_struct_field_name struct_field))
      struct_fields format_struct_name struct_name
      (if List.length struct_fields > 0 then
       Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " and@ ")
         (fun _fmt (struct_field, _) ->
           Format.fprintf fmt "self.%a == other.%a" format_struct_field_name
             struct_field format_struct_field_name struct_field)
      else fun fmt _ -> Format.fprintf fmt "True")
      struct_fields format_struct_name struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         (fun _fmt (struct_field, _) ->
           Format.fprintf fmt "%a={}" format_struct_field_name struct_field))
      struct_fields
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun _fmt (struct_field, _) ->
           Format.fprintf fmt "self.%a" format_struct_field_name struct_field))
      struct_fields
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if List.length enum_cons = 0 then failwith "no constructors in the enum"
    else
      Format.fprintf fmt
        "@[<hov 4>class %a_Code(Enum):@\n\
         %a@]@\n\
         @\n\
         class %a:@\n\
         \tdef __init__(self, code: %a_Code, value: Any) -> None:@\n\
         \t\tself.code = code@\n\
         \t\tself.value = value@\n\
         @\n\
         @\n\
         \tdef __eq__(self, other: object) -> bool:@\n\
         \t\tif isinstance(other, %a):@\n\
         \t\t\treturn self.code == other.code and self.value == other.value@\n\
         \t\telse:@\n\
         \t\t\treturn False@\n\
         @\n\
         @\n\
         \tdef __ne__(self, other: object) -> bool:@\n\
         \t\treturn not (self == other)@\n\
         @\n\
         \tdef __str__(self) -> str:@\n\
         \t\t@[<hov 4>return \"{}({})\".format(self.code, self.value)@]"
        format_enum_name enum_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
           (fun _fmt (i, enum_cons, enum_cons_type) ->
             Format.fprintf fmt "%a = %d" format_enum_cons_name enum_cons i))
        (List.mapi (fun i (x, y) -> (i, x, y)) enum_cons)
        format_enum_name enum_name format_enum_name enum_name format_enum_name
        enum_name
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
         (Dcalc.Ast.StructMap.filter
            (fun s _ -> not (is_in_type_ordering s))
            ctx.ctx_structs))
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

let format_program
    (fmt : Format.formatter)
    (p : Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) : unit =
  (* We disable the style flag in order to enjoy formatting from the
     pretty-printers of Dcalc and Lcalc but without the color terminal
     markers. *)
  Cli.style_flag := false;
  Format.fprintf fmt
    "# This file has been generated by the Catala compiler, do not edit!\n\
     @\n\
     from catala.runtime import *@\n\
     from typing import Any, List, Callable, Tuple\n\
     from enum import Enum\n\
     @\n\
     %a@\n\
     @\n\
     %a@?"
    (format_ctx type_ordering) p.decl_ctx
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n@\n")
       (fun fmt body ->
         let { Ast.func_params; Ast.func_body } = body.scope_body_func in
         Format.fprintf fmt "@[<hov 4>def %a(%a):@\n%a@]" format_toplevel_name
           body.scope_body_var
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
              (fun fmt (var, typ) ->
                Format.fprintf fmt "%a:%a" format_var (Pos.unmark var)
                  format_typ typ))
           func_params (format_block p.decl_ctx) func_body))
    p.scopes
