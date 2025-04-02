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
open Shared_ast
open Ast
module Runtime = Runtime_ocaml.Runtime
module D = Dcalc.Ast
module L = Lcalc.Ast
open Format

let pp_comma ppf () = fprintf ppf ",@ "
let ext = "java"

(* let format_lit (fmt : formatter) (l : lit Mark.pos) : unit = *)
(*   match Mark.remove l with *)
(*   | LBool true -> pp_print_string fmt "True" *)
(*   | LBool false -> pp_print_string fmt "False" *)
(*   | LInt i -> *)
(* fprintf fmt "integer_of_string(\"%s\")" (Runtime.integer_to_string i) *)
(* | LUnit -> pp_print_string fmt "Unit()" *)
(* | LRat i -> fprintf fmt "decimal_of_string(\"%s\")" (Q.to_string i) *)
(*   | LMoney e -> *)
(*     fprintf fmt "money_of_cents_string(\"%s\")" *)
(*       (Runtime.integer_to_string (Runtime.money_to_cents e)) *)
(*   | LDate d -> *)
(*     fprintf fmt "date_of_numbers(%d,%d,%d)" *)
(*       (Runtime.integer_to_int (Runtime.year_of_date d)) *)
(*       (Runtime.integer_to_int (Runtime.month_number_of_date d)) *)
(*       (Runtime.integer_to_int (Runtime.day_of_month_of_date d)) *)
(*   | LDuration d -> *)
(*     let years, months, days = Runtime.duration_to_years_months_days d in *)
(*     fprintf fmt "duration_of_numbers(%d,%d,%d)" years months days *)

let format_uid_list (fmt : formatter) (uids : Uid.MarkedString.info list) : unit
    =
  fprintf fmt "[%a]"
    (pp_print_list ~pp_sep:pp_comma (fun fmt info ->
         fprintf fmt "\"%a\"" Uid.MarkedString.format info))
    uids

let format_string_list (fmt : formatter) (uids : string list) : unit =
  let sanitize_quotes = Re.compile (Re.char '"') in
  fprintf fmt "[%a]"
    (pp_print_list ~pp_sep:pp_comma (fun fmt info ->
         fprintf fmt "\"%s\""
           (Re.replace sanitize_quotes ~f:(fun _ -> "\\\"") info)))
    uids

let java_keywords =
  (* list taken from
     https://docs.oracle.com/javase/tutorial/java/nutsandbolts/_keywords.html *)
  [
    "abstract";
    "continue";
    "for";
    "new";
    "switch";
    "assert";
    "default";
    "goto";
    "package";
    "synchronized";
    "boolean";
    "do";
    "if";
    "private";
    "this";
    "break";
    "double";
    "implements";
    "protected";
    "throw";
    "byte";
    "else";
    "import";
    "public";
    "throws";
    "case";
    "enum";
    "instanceof";
    "return";
    "transient";
    "catch";
    "extends";
    "int";
    "short";
    "try";
    "char";
    "final";
    "interface";
    "static";
    "void";
    "class";
    "finally";
    "long";
    "strictfp";
    "volatile";
    "const";
    "float";
    "native";
    "super";
    "while";
  ]
(* todo: reserved names should also include built-in types and everything
   exposed by the runtime. *)

let renaming =
  Renaming.program ()
    ~reserved:java_keywords
      (* TODO: add catala runtime built-ins as reserved as well ? *)
    ~skip_constant_binders:false ~constant_binder_name:None
    ~namespaced_fields:true ~namespaced_constrs:true ~prefix_module:false
    ~f_var:(String.to_camel_case ~capitalize:false)
    ~f_struct:String.to_camel_case ~f_enum:String.to_camel_case

let format_qualified
    (type id)
    (module Id : Uid.Qualified with type t = id)
    ctx
    ppf
    (s : id) =
  match List.rev (Id.path s) with
  | [] -> pp_print_string ppf (Id.base s)
  | m :: _ ->
    fprintf ppf "%a.%s" VarName.format
      (ModuleName.Map.find m ctx.modules)
      (Id.base s)

let format_struct = format_qualified (module StructName)
let format_enum = format_qualified (module EnumName)

let typ_needs_parens (e : typ) : bool =
  match Mark.remove e with TArrow _ | TArray _ -> true | _ -> false

let rec format_typ ctx (fmt : formatter) (typ : typ) : unit =
  let format_typ = format_typ ctx in
  let format_typ_with_parens (fmt : formatter) (t : typ) =
    if typ_needs_parens t then fprintf fmt "(%a)" format_typ t
    else fprintf fmt "%a" format_typ t
  in
  match Mark.remove typ with
  | TLit TUnit -> fprintf fmt "Unit"
  | TLit TMoney -> fprintf fmt "Money"
  | TLit TInt -> fprintf fmt "Integer"
  | TLit TRat -> fprintf fmt "Decimal"
  | TLit TDate -> fprintf fmt "Date"
  | TLit TDuration -> fprintf fmt "Duration"
  | TLit TBool -> fprintf fmt "bool"
  | TLit TPos -> fprintf fmt "SourcePosition"
  | TTuple ts ->
    fprintf fmt "Tuple[%a]"
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt ", ")
         (fun fmt t -> fprintf fmt "%a" format_typ_with_parens t))
      ts
  | TStruct s -> format_struct ctx fmt s
  | TOption some_typ ->
    (* We translate the option type with an overloading by Python's [None] *)
    fprintf fmt "Optional[%a]" format_typ some_typ
  | TDefault t -> format_typ fmt t
  | TEnum e -> format_enum ctx fmt e
  | TArrow (t1, t2) ->
    fprintf fmt "Callable[[%a], %a]"
      (pp_print_list ~pp_sep:pp_comma format_typ_with_parens)
      t1 format_typ_with_parens t2
  | TArray t1 -> fprintf fmt "List[%a]" format_typ_with_parens t1
  | TAny -> fprintf fmt "Any"
  | TClosureEnv -> failwith "unimplemented!"

let format_func_name (fmt : formatter) (v : FuncName.t) : unit =
  FuncName.format fmt v

let format_op (fmt : formatter) (op : operator Mark.pos) : unit =
  match Mark.remove op with
  | Log (_entry, _infos) -> assert false
  | Minus_int | Minus_rat | Minus_mon | Minus_dur -> pp_print_string fmt "-"
  (* Todo: use the names from [Operator.name] *)
  | Not -> pp_print_string fmt "not"
  | Length -> pp_print_string fmt "list_length"
  | ToInt_rat -> pp_print_string fmt "integer_of_decimal"
  | ToRat_int -> pp_print_string fmt "decimal_of_integer"
  | ToRat_mon -> pp_print_string fmt "decimal_of_money"
  | ToMoney_rat -> pp_print_string fmt "money_of_decimal"
  | GetDay -> pp_print_string fmt "day_of_month_of_date"
  | GetMonth -> pp_print_string fmt "month_number_of_date"
  | GetYear -> pp_print_string fmt "year_of_date"
  | FirstDayOfMonth -> pp_print_string fmt "first_day_of_month"
  | LastDayOfMonth -> pp_print_string fmt "last_day_of_month"
  | Round_mon -> pp_print_string fmt "money_round"
  | Round_rat -> pp_print_string fmt "decimal_round"
  | Add_int_int -> pp_print_string fmt "add"
  | Add_rat_rat | Add_mon_mon | Add_dur_dur | Concat -> pp_print_string fmt "+"
  | Add_dat_dur rounding ->
    fprintf fmt "add_date_duration(%s)"
      (match rounding with
      | RoundUp -> "DateRounding.RoundUp"
      | RoundDown -> "DateRounding.RoundDown"
      | AbortOnRound -> "DateRounding.AbortOnRound")
  | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dur_dur ->
    pp_print_string fmt "sub"
  | Sub_dat_dur rounding ->
    fprintf fmt "sub_date_duration(%s)"
      (match rounding with
      | RoundUp -> "DateRounding.RoundUp"
      | RoundDown -> "DateRounding.RoundDown"
      | AbortOnRound -> "DateRounding.AbortOnRound")
  | Mult_int_int | Mult_rat_rat | Mult_mon_int | Mult_mon_rat | Mult_dur_int ->
    pp_print_string fmt "*"
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int | Div_mon_rat
  | Div_dur_dur ->
    pp_print_string fmt "div"
  | And -> pp_print_string fmt "and"
  | Or -> pp_print_string fmt "or"
  | Eq -> pp_print_string fmt "=="
  | Xor -> pp_print_string fmt "!="
  | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat -> pp_print_string fmt "<"
  | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat ->
    pp_print_string fmt "<="
  | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat -> pp_print_string fmt ">"
  | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat ->
    pp_print_string fmt ">="
  | Lt_dur_dur -> pp_print_string fmt "lt_duration"
  | Lte_dur_dur -> pp_print_string fmt "leq_duration"
  | Gt_dur_dur -> pp_print_string fmt "gt_duration"
  | Gte_dur_dur -> pp_print_string fmt "geq_duration"
  | Eq_boo_boo | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dat_dat | Eq_dur_dur
    ->
    pp_print_string fmt "=="
  | Map -> pp_print_string fmt "list_map"
  | Map2 -> pp_print_string fmt "list_map2"
  | Reduce -> pp_print_string fmt "list_reduce"
  | Filter -> pp_print_string fmt "list_filter"
  | Fold -> pp_print_string fmt "list_fold_left"
  | HandleExceptions -> pp_print_string fmt "handle_exceptions"
  | FromClosureEnv | ToClosureEnv -> failwith "unimplemented"

(* let rec format_expression ctx (fmt : formatter) (e : expr) : unit = *)
(*   match Mark.remove e with *)
(*   | EVar v -> VarName.format fmt v *)
(*   | EFunc f -> FuncName.format fmt f *)
(*   | EStruct { fields = es; name = s } -> *)
(*     fprintf fmt "%a(%a)" (format_struct ctx) s *)
(*       (pp_print_list *)
(*          ~pp_sep:pp_comma *)
(*          (fun fmt (struct_field, e) -> *)
(*            fprintf fmt "%a = %a" StructField.format struct_field *)
(*              (format_expression ctx) e)) *)
(*       (StructField.Map.bindings es) *)
(*   | EStructFieldAccess { e1; field; _ } -> *)
(* fprintf fmt "%a.%a" (format_expression ctx) e1 StructField.format *)
(*       field *)
(*   | EInj { cons; name = e_name; _ } *)
(*     when EnumName.equal e_name Expr.option_enum *)
(*          && EnumConstructor.equal cons Expr.none_constr -> *)
(* (\* We translate the option type with an overloading by Python's [None]
   *\) *)
(*     fprintf fmt "None" *)
(*   | EInj { e1 = e; cons; name = e_name; _ } *)
(*     when EnumName.equal e_name Expr.option_enum *)
(*          && EnumConstructor.equal cons Expr.some_constr -> *)
(* (\* We translate the option type with an overloading by Python's [None]
   *\) *)
(*     format_expression ctx fmt e *)
(*   | EInj { e1 = e; cons; name = enum_name; _ } -> *)
(*     fprintf fmt "%a(%a_Code.%a,@ %a)" (format_enum ctx) enum_name *)
(*       (format_enum ctx) enum_name EnumConstructor.format cons *)
(*       (format_expression ctx) e *)
(*   | EArray es -> *)
(*     fprintf fmt "[%a]" *)
(*       (pp_print_list *)
(*          ~pp_sep:pp_comma *)
(*          (fun fmt e -> fprintf fmt "%a" (format_expression ctx) e)) *)
(*       es *)
(*   | ELit l -> fprintf fmt "%a" format_lit (Mark.copy e l) *)
(*   | EPosLit -> *)
(*     let pos = Mark.get e in *)
(*     fprintf fmt *)
(* "@[<hov 4>SourcePosition(@,\ *) (* filename=\"%s\",@ start_line=%d,
   start_column=%d,@ end_line=%d, \ *) (* end_column=%d,@ law_headings=%a@;\ *)
   (* <0 -4>)@]" (Pos.get_file pos) (Pos.get_start_line pos) *)
(* (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos) *)
(*       format_string_list (Pos.get_law_info pos) *)
(*   | EAppOp { op; args = [arg1; arg2]; _ } -> *)
(* fprintf fmt "%a(%a,@ %a)" format_op op (format_expression ctx) arg1 *)
(*       (format_expression ctx) arg2 *)
(*   | EApp *)
(*       { *)
(*         f = EAppOp { op = Log (BeginCall, info), _; args = [f]; _ }, _; *)
(*         args = [arg]; *)
(*       } *)
(*     when Global.options.trace <> None -> *)
(*     fprintf fmt "log_begin_call(%a,@ %a,@ %a)" format_uid_list info *)
(*       (format_expression ctx) f (format_expression ctx) arg *)
(*   | EAppOp { op = Log (VarDef var_def_info, info), _; args = [arg1]; _ } *)
(*     when Global.options.trace <> None -> *)
(*     fprintf fmt *)
(* "log_variable_definition(%a,@ LogIO(input_io=InputIO.%s,@ \ *) (*
   output_io=%s),@ %a)" *)
(*       format_uid_list info *)
(*       (match var_def_info.log_io_input with *)
(*       | Runtime.NoInput -> "NoInput" *)
(*       | Runtime.OnlyInput -> "OnlyInput" *)
(*       | Runtime.Reentrant -> "Reentrant") *)
(*       (if var_def_info.log_io_output then "True" else "False") *)
(*       (format_expression ctx) arg1 *)
(*   | EAppOp { op = Log (PosRecordIfTrueBool, _), _; args = [arg1]; _ } *)
(*     when Global.options.trace <> None -> *)
(*     let pos = Mark.get e in *)
(*     fprintf fmt *)
(* "log_decision_taken(SourcePosition(filename=\"%s\",@ start_line=%d,@ \ *) (*
   start_column=%d,@ end_line=%d, end_column=%d,@ law_headings=%a), %a)" *)
(* (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos) *)
(*       (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list *)
(*       (Pos.get_law_info pos) (format_expression ctx) arg1 *)
(*   | EAppOp { op = Log (EndCall, info), _; args = [arg1]; _ } *)
(*     when Global.options.trace <> None -> *)
(*     fprintf fmt "log_end_call(%a,@ %a)" format_uid_list info *)
(*       (format_expression ctx) arg1 *)
(*   | EAppOp { op = Log _, _; args = [arg1]; _ } -> *)
(*     fprintf fmt "%a" (format_expression ctx) arg1 *)
(*   | EAppOp { op = (Not, _) as op; args = [arg1]; _ } -> *)
(*     fprintf fmt "%a %a" format_op op (format_expression ctx) arg1 *)
(*   | EAppOp *)
(*       { *)
(*         op = ((Minus_int | Minus_rat | Minus_mon | Minus_dur), _) as op; *)
(*         args = [arg1]; *)
(*         _; *)
(*       } -> *)
(*     fprintf fmt "%a %a" format_op op (format_expression ctx) arg1 *)
(*   | EAppOp { op; args = [arg1]; _ } -> *)
(*     fprintf fmt "%a(%a)" format_op op (format_expression ctx) arg1 *)
(*   | EApp { f; args } -> *)
(*     fprintf fmt "%a(@[<hv 0>%a)@]" (format_expression ctx) f *)
(*       (pp_print_list *)
(*          ~pp_sep:pp_comma *)
(*          (format_expression ctx)) *)
(*       args *)
(*   | EAppOp { op; args; _ } -> *)
(*     fprintf fmt "%a(@[<hv 0>%a)@]" format_op op *)
(*       (pp_print_list *)
(*          ~pp_sep:pp_comma *)
(*          (format_expression ctx)) *)
(*       args *)
(*   | ETuple es -> *)
(*     fprintf fmt "(%a)" *)
(*       (pp_print_list *)
(*          ~pp_sep:pp_comma *)
(*          (fun fmt e -> fprintf fmt "%a" (format_expression ctx) e)) *)
(*       es *)
(*   | ETupleAccess { e1; index; _ } -> *)
(*     fprintf fmt "%a[%d]" (format_expression ctx) e1 index *)
(*   | EExternal { modname; name } -> *)
(*     fprintf fmt "%a.%s" VarName.format (Mark.remove modname) *)
(*       (Mark.remove name) *)

(* let rec format_statement ctx (fmt : formatter) (s : stmt Mark.pos) : unit *)
(*     = *)
(*   match Mark.remove s with *)
(*   | SInnerFuncDef { name; func = { func_params; func_body; _ } } -> *)
(*     fprintf fmt "@[<v 4>def %a(@[<hov>%a@]):@ %a@]" VarName.format *)
(*       (Mark.remove name) *)
(*       (pp_print_list *)
(*          ~pp_sep:pp_comma *)
(*          (fun fmt (var, typ) -> *)
(*            fprintf fmt "%a:%a" VarName.format (Mark.remove var) *)
(*              (format_typ ctx) typ)) *)
(*       func_params (format_block ctx) func_body *)
(*   | SLocalDecl _ -> *)
(*     assert false (\* We don't need to declare variables in Python *\) *)
(* | SLocalDef { name = v; expr = e; _ } | SLocalInit { name = v; expr = e; _
   } *)
(*     -> *)
(*     fprintf fmt "@[<hv 4>%a = (%a)@]" VarName.format (Mark.remove v) *)
(*       (format_expression ctx) e *)
(*   | SFatalError { pos_expr; error } -> *)
(*     fprintf fmt "@[<hov 4>raise %s(%a)@]" *)
(*       (Runtime.error_to_string error) *)
(*       (format_expression ctx) pos_expr *)
(*   | SIfThenElse { if_expr = cond; then_block = b1; else_block = b2 } -> *)
(*     fprintf fmt "@[<v 4>if %a:@ %a@]@,@[<v 4>else:@ %a@]" *)
(* (format_expression ctx) cond (format_block ctx) b1 (format_block ctx) b2 *)
(*   | SSwitch *)
(*       { *)
(*         switch_var; *)
(*         enum_name = e_name; *)
(*         switch_cases = *)
(*           [ *)
(*             { case_block = case_none; _ }; *)
(*             { *)
(*               case_block = case_some; *)
(*               payload_var_name = case_some_var; *)
(*               payload_var_typ; *)
(*             }; *)
(*           ]; *)
(*         _; *)
(*       } *)
(*     when EnumName.equal e_name Expr.option_enum -> *)
(* (\* We translate the option type with an overloading by Python's [None]
   *\) *)
(* let pos = Mark.get s in *)
(* fprintf fmt "@[<v 4>if %a is None:@ %a@]@," VarName.format switch_var *)
(*       (format_block ctx) case_none; *)
(*     fprintf fmt "@[<v 4>else:@ %a@]" (format_block ctx) *)
(* (Utils.subst_block case_some_var (EVar switch_var, pos) payload_var_typ *)
(*          pos case_some) *)
(*   | SSwitch { switch_var; enum_name = e_name; switch_cases = cases; _ } -> *)
(*     let cons_map = EnumName.Map.find e_name ctx.decl_ctx.ctx_enums in *)
(*     let pos = Mark.get s in *)
(*     let cases = *)
(*       List.map2 *)
(*         (fun x (cons, _) -> x, cons) *)
(*         cases *)
(*         (EnumConstructor.Map.bindings cons_map) *)
(*     in *)
(*     fprintf fmt "@[<hov 4>if %a@]" *)
(*       (pp_print_list *)
(*          ~pp_sep:(fun fmt () -> fprintf fmt "@]@\n@[<hov 4>elif ") *)
(*          (fun fmt (case, cons_name) -> *)
(*            fprintf fmt "%a.code == %a_Code.%a:@," VarName.format *)
(*              switch_var (format_enum ctx) e_name EnumConstructor.format *)
(*              cons_name; *)
(*            format_block ctx fmt *)
(*              (Utils.subst_block case.payload_var_name *)
(* (\* Not a real catala struct, but will print as <var>.value *\) *)
(*                 ( EStructFieldAccess *)
(*                     { *)
(*                       e1 = EVar switch_var, pos; *)
(*                       field = StructField.fresh ("value", pos); *)
(*                       name = StructName.fresh [] ("Dummy", pos); *)
(*                     }, *)
(*                   pos ) *)
(*                 case.payload_var_typ pos case.case_block))) *)
(*       cases *)
(*   | SReturn e1 -> *)
(*     fprintf fmt "@[<hov 4>return %a@]" (format_expression ctx) e1 *)
(*   | SAssert { pos_expr; expr = e1 } -> *)
(*     fprintf fmt "@[<hv 4>if not (%a):@,raise AssertionFailed(%a)@]" *)
(*       (format_expression ctx) e1 (format_expression ctx) pos_expr *)
(*   | SSpecialOp _ -> . *)

(* and format_block ctx (fmt : formatter) (b : block) : unit = *)
(*   pp_open_vbox fmt 0; *)
(*   pp_print_list (format_statement ctx) fmt *)
(*     (List.filter *)
(* (fun s -> match Mark.remove s with SLocalDecl _ -> false | _ -> true) *)
(*        b); *)
(*   pp_close_box fmt () *)

let format_ctx (type_ordering : TypeIdent.t list) (fmt : formatter) ctx : unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    let fields = StructField.Map.bindings struct_fields in
    fprintf fmt
      "class %a:@,\
      \    def __init__(self, %a) -> None:@,\
       %a@,\
       @,\
      \    def __eq__(self, other: object) -> bool:@,\
      \        if isinstance(other, %a):@,\
      \            return @[<hov>(%a)@]@,\
      \        else:@,\
      \            return False@,\
       @,\
      \    def __ne__(self, other: object) -> bool:@,\
      \        return not (self == other)@,\
       @,\
      \    def __str__(self) -> str:@,\
      \        @[<hov 4>return \"%a(%a)\".format(%a)@]" StructName.format
      struct_name
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt ", ")
         (fun fmt (struct_field, struct_field_type) ->
           fprintf fmt "%a: %a" StructField.format struct_field (format_typ ctx)
             struct_field_type))
      fields
      (if StructField.Map.is_empty struct_fields then fun fmt _ ->
         fprintf fmt "        pass"
       else
         pp_print_list (fun fmt (struct_field, _) ->
             fprintf fmt "        self.%a = %a" StructField.format struct_field
               StructField.format struct_field))
      fields StructName.format struct_name
      (if not (StructField.Map.is_empty struct_fields) then
         pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt " and@ ")
           (fun fmt (struct_field, _) ->
             fprintf fmt "self.%a == other.%a" StructField.format struct_field
               StructField.format struct_field)
       else fun fmt _ -> fprintf fmt "True")
      fields StructName.format struct_name
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt ",")
         (fun fmt (struct_field, _) ->
           fprintf fmt "%a={}" StructField.format struct_field))
      fields
      (pp_print_list ~pp_sep:pp_comma (fun fmt (struct_field, _) ->
           fprintf fmt "self.%a" StructField.format struct_field))
      fields
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if EnumConstructor.Map.is_empty enum_cons then
      failwith "no constructors in the enum"
    else
      fprintf fmt
        "@[<v 4>class %a_Code(Enum):@,\
         %a@]@,\
         @,\
         class %a:@,\
        \    def __init__(self, code: %a_Code, value: Any) -> None:@,\
        \        self.code = code@,\
        \        self.value = value@,\
         @,\
         @,\
        \    def __eq__(self, other: object) -> bool:@,\
        \        if isinstance(other, %a):@,\
        \            return self.code == other.code and self.value == \
         other.value@,\
        \        else:@,\
        \            return False@,\
         @,\
         @,\
        \    def __ne__(self, other: object) -> bool:@,\
        \        return not (self == other)@,\
         @,\
        \    def __str__(self) -> str:@,\
        \        @[<hov 4>return \"{}({})\".format(self.code, self.value)@]"
        EnumName.format enum_name
        (pp_print_list (fun fmt (i, enum_cons, _enum_cons_type) ->
             fprintf fmt "%a = %d" EnumConstructor.format enum_cons i))
        (List.mapi
           (fun i (x, y) -> i, x, y)
           (EnumConstructor.Map.bindings enum_cons))
        EnumName.format enum_name EnumName.format enum_name EnumName.format
        enum_name
  in

  let is_in_type_ordering s =
    List.exists
      (fun struct_or_enum ->
        match struct_or_enum with
        | TypeIdent.Enum _ -> false
        | TypeIdent.Struct s' -> s = s')
      type_ordering
  in
  let scope_structs =
    List.map
      (fun (s, _) -> TypeIdent.Struct s)
      (StructName.Map.bindings
         (StructName.Map.filter
            (fun s _ -> not (is_in_type_ordering s))
            ctx.decl_ctx.ctx_structs))
  in
  List.iter
    (fun struct_or_enum ->
      match struct_or_enum with
      | TypeIdent.Struct s ->
        if StructName.path s = [] then
          fprintf fmt "%a@,@," format_struct_decl
            (s, StructName.Map.find s ctx.decl_ctx.ctx_structs)
      | TypeIdent.Enum e ->
        if EnumName.path e = [] then
          fprintf fmt "%a@,@," format_enum_decl
            (e, EnumName.Map.find e ctx.decl_ctx.ctx_enums))
    (type_ordering @ scope_structs)

(* let format_code_item ctx fmt = function *)
(*   | SVar { var; expr; typ = _; visibility = _ } -> *)
(*     fprintf fmt "@[<hv 4>%a = (@,%a@;<0 -4>)@]@," VarName.format var *)
(*       (format_expression ctx) expr *)
(*   | SFunc { var; func; visibility = _ } *)
(*   | SScope { scope_body_var = var; scope_body_func = func; _ } -> *)
(*     let { Ast.func_params; Ast.func_body; _ } = func in *)
(*     fprintf fmt "@[<v 4>@[<hov 2>def %a(@,%a@;<0 -2>):@]@ %a@]@," *)
(*       format_func_name var *)
(*       (pp_print_list *)
(*          ~pp_sep:pp_comma *)
(*          (fun fmt (var, typ) -> *)
(*            fprintf fmt "%a:%a" VarName.format (Mark.remove var) *)
(*              (format_typ ctx) typ)) *)
(*       func_params (format_block ctx) func_body *)

let format_scope_calls ppf (p : Ast.program) =
  let scopes_with_no_input =
    List.fold_left
      (fun acc -> function
        | SScope
            {
              scope_body_func = { func_params = [(_, (TStruct ts, _))]; _ };
              scope_body_var = var;
              scope_body_name = name;
              scope_body_visibility = _;
            } ->
          let input_struct =
            StructName.Map.find ts p.ctx.decl_ctx.ctx_structs
          in
          if StructField.Map.is_empty input_struct then (var, name, ts) :: acc
          else acc
        | SVar _ | SFunc _ | SScope _ -> acc)
      [] p.code_items
    |> List.rev
  in
  if scopes_with_no_input = [] then ()
  else
    let () =
      Message.debug "Generating entry points for scopes:@ %a"
        (pp_print_list ~pp_sep:pp_print_space (fun ppf (_, s, _) ->
             ScopeName.format ppf s))
        scopes_with_no_input
    in
    fprintf ppf "@,# Automatic Catala tests@,";
    fprintf ppf "@[<v 2>if __name__ == \"__main__\":";
    List.iter
      (fun (var, name, ts) ->
        fprintf ppf "@,print(\"Executing scope %a...\")" ScopeName.format name;
        fprintf ppf "@,%a (%a());" FuncName.format var StructName.format ts;
        fprintf ppf
          "@,\
           print(\"\\x1b[32m[RESULT]\\x1b[m Scope %a executed successfully.\")"
          ScopeName.format name)
      scopes_with_no_input;
    fprintf ppf "@]@,"

(* let format_program *)
(*     (fmt : formatter) *)
(*     (p : Ast.program) *)
(*     (type_ordering : TypeIdent.t list) : unit = *)
(*   pp_open_vbox fmt 0; *)
(*   let header = *)
(*     [ *)
(* "# This file has been generated by the Catala compiler, do not edit!"; *)
(*       ""; *)
(*       "from catala.runtime import *"; *)
(*       "from typing import Any, List, Callable, Tuple"; *)
(*       "from enum import Enum"; *)
(*       ""; *)
(*     ] *)
(*   in *)
(*   pp_print_list pp_print_string fmt header; *)
(*   ModuleName.Map.iter *)
(*     (fun m v -> *)
(*       fprintf fmt "from . import %a as %a@," ModuleName.format m *)
(*         VarName.format v) *)
(*     p.ctx.modules; *)
(*   pp_print_cut fmt (); *)
(*   format_ctx type_ordering fmt p.ctx; *)
(*   pp_print_cut fmt (); *)
(*   pp_print_list (format_code_item p.ctx) fmt p.code_items; *)
(*   format_scope_calls fmt p; *)
(*   pp_print_flush fmt () *)

let format_params fmt func_params =
  fprintf fmt "blabla";
  ignore func_params

let format_visibility ppf = function
  | Private -> () (* TODO? *)
  | Public -> fprintf ppf "public "

let format_typ_lit ppf = function
  | TBool -> fprintf ppf "boolean"
  | TUnit -> fprintf ppf "CatalaUnit"
  | TInt -> fprintf ppf "CatalaInteger"
  | TRat -> fprintf ppf "CatalaDecimal"
  | TMoney -> fprintf ppf "CatalaMoney"
  | TDate -> fprintf ppf "CatalaDate"
  | TDuration -> fprintf ppf "CatalaDuration"
  | TPos -> fprintf ppf "CatalaPosition"

let rec format_typ ppf typ =
  match Mark.remove typ with
  | TLit typ_lit -> format_typ_lit ppf typ_lit
  | TArrow _ -> assert false
  | TTuple _ -> fprintf ppf "CatalaValue[]"
  | TStruct sname -> StructName.format ppf sname (* TODO: resolve imports *)
  | TEnum ename -> EnumName.format ppf ename (* TODO: resolve imports *)
  | TOption typ -> fprintf ppf "CatalaOption<%a>" format_typ typ
  | TArray typ -> fprintf ppf "%a[]" format_typ typ
  | TDefault typ -> format_typ ppf typ
  | TAny -> assert false
  | TClosureEnv -> assert false

let format_struct_params (fields : typ StructField.Map.t) ppf =
  let fields = StructField.Map.bindings fields in
  pp_print_list ~pp_sep:pp_comma
    (fun ppf (sfield, typ) ->
      fprintf ppf "%a %a" format_typ typ StructField.format sfield)
    ppf fields

let fields_of_struct ctx sname =
  StructName.Map.find_opt sname ctx.decl_ctx.ctx_structs
  |> function
  | None -> StructField.Set.empty
  | Some m -> StructField.Map.keys m |> StructField.Set.of_list

let format_lit (fmt : formatter) (l : lit Mark.pos) : unit =
  match Mark.remove l with
  | LBool true -> pp_print_string fmt "CatalaBool.true"
  | LBool false -> pp_print_string fmt "CatalaBool.false"
  | LInt i ->
    fprintf fmt "new CatalaInteger(\"%s\")" (Runtime.integer_to_string i)
  | LUnit -> pp_print_string fmt "null"
  | LRat i -> fprintf fmt "new CatalaDecimal(\"%s\")" (Q.to_string i)
  | LMoney e ->
    fprintf fmt "CatalaMoney.from_cents(\"%s\")"
      (Runtime.integer_to_string (Runtime.money_to_cents e))
  | LDate d ->
    fprintf fmt "new CatalaDate(%d,%d,%d)"
      (Runtime.integer_to_int (Runtime.year_of_date d))
      (Runtime.integer_to_int (Runtime.month_number_of_date d))
      (Runtime.integer_to_int (Runtime.day_of_month_of_date d))
  | LDuration d ->
    let years, months, days = Runtime.duration_to_years_months_days d in
    fprintf fmt "new CatalaDuration(%d,%d,%d)" years months days

(* TODO: adapt *)
let rec format_expression (in_struct as jctx) ctx (fmt : formatter) (e : expr) :
    unit =
  match Mark.remove e with
  | EVar v -> VarName.format fmt v
  | EFunc f -> FuncName.format fmt f
  | EStruct { fields = es; name = s } ->
    fprintf fmt "%a(%a)" (format_struct ctx) s
      (pp_print_list ~pp_sep:pp_comma (fun fmt (struct_field, e) ->
           fprintf fmt "%a = %a" StructField.format struct_field
             (format_expression jctx ctx)
             e))
      (StructField.Map.bindings es)
  | EStructFieldAccess { name; field; _ } when StructName.equal name in_struct
    ->
    fprintf fmt "%a" StructField.format field
  | EStructFieldAccess { e1; field; _ } ->
    fprintf fmt "%a.%a" (format_expression jctx ctx) e1 StructField.format field
  | EInj { cons; name = e_name; _ }
    when EnumName.equal e_name Expr.option_enum
         && EnumConstructor.equal cons Expr.none_constr ->
    (* We translate the option type with an overloading by Python's [None] *)
    fprintf fmt "None"
  | EInj { e1 = e; cons; name = e_name; _ }
    when EnumName.equal e_name Expr.option_enum
         && EnumConstructor.equal cons Expr.some_constr ->
    (* We translate the option type with an overloading by Python's [None] *)
    format_expression jctx ctx fmt e
  | EInj { e1 = e; cons; name = enum_name; _ } ->
    fprintf fmt "%a(%a_Code.%a,@ %a)" (format_enum ctx) enum_name
      (format_enum ctx) enum_name EnumConstructor.format cons
      (format_expression jctx ctx)
      e
  | EArray es ->
    fprintf fmt "[%a]"
      (pp_print_list ~pp_sep:pp_comma (fun fmt e ->
           fprintf fmt "%a" (format_expression jctx ctx) e))
      es
  | ELit l -> fprintf fmt "%a" format_lit (Mark.copy e l)
  | EPosLit ->
    let pos = Mark.get e in
    fprintf fmt
      "@[<hov 4>SourcePosition(@,\
       filename=\"%s\",@ start_line=%d, start_column=%d,@ end_line=%d, \
       end_column=%d,@ law_headings=%a@;\
       <0 -4>)@]" (Pos.get_file pos) (Pos.get_start_line pos)
      (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos)
      format_string_list (Pos.get_law_info pos)
  | EAppOp { op; args = [arg1; arg2]; _ } ->
    fprintf fmt "%a.%a(%a)"
      (format_expression jctx ctx)
      arg1 format_op op
      (format_expression jctx ctx)
      arg2
  | EApp
      {
        f = EAppOp { op = Log (BeginCall, info), _; args = [f]; _ }, _;
        args = [arg];
      }
    when Global.options.trace <> None ->
    fprintf fmt "log_begin_call(%a,@ %a,@ %a)" format_uid_list info
      (format_expression jctx ctx)
      f
      (format_expression jctx ctx)
      arg
  | EAppOp { op = Log (VarDef var_def_info, info), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    fprintf fmt
      "log_variable_definition(%a,@ LogIO(input_io=InputIO.%s,@ \
       output_io=%s),@ %a)"
      format_uid_list info
      (match var_def_info.log_io_input with
      | Runtime.NoInput -> "NoInput"
      | Runtime.OnlyInput -> "OnlyInput"
      | Runtime.Reentrant -> "Reentrant")
      (if var_def_info.log_io_output then "True" else "False")
      (format_expression jctx ctx)
      arg1
  | EAppOp { op = Log (PosRecordIfTrueBool, _), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    let pos = Mark.get e in
    fprintf fmt
      "log_decision_taken(SourcePosition(filename=\"%s\",@ start_line=%d,@ \
       start_column=%d,@ end_line=%d, end_column=%d,@ law_headings=%a), %a)"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos)
      (format_expression jctx ctx)
      arg1
  | EAppOp { op = Log (EndCall, info), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    fprintf fmt "log_end_call(%a,@ %a)" format_uid_list info
      (format_expression jctx ctx)
      arg1
  | EAppOp { op = Log _, _; args = [arg1]; _ } ->
    fprintf fmt "%a" (format_expression jctx ctx) arg1
  | EAppOp { op = (Not, _) as op; args = [arg1]; _ } ->
    fprintf fmt "%a %a" format_op op (format_expression jctx ctx) arg1
  | EAppOp
      {
        op = ((Minus_int | Minus_rat | Minus_mon | Minus_dur), _) as op;
        args = [arg1];
        _;
      } ->
    fprintf fmt "%a %a" format_op op (format_expression jctx ctx) arg1
  | EAppOp { op; args = [arg1]; _ } ->
    fprintf fmt "%a(%a)" format_op op (format_expression jctx ctx) arg1
  | EApp { f; args } ->
    fprintf fmt "%a(@[<hv 0>%a)@]"
      (format_expression jctx ctx)
      f
      (pp_print_list ~pp_sep:pp_comma (format_expression jctx ctx))
      args
  | EAppOp { op; args; _ } ->
    fprintf fmt "%a(@[<hv 0>%a)@]" format_op op
      (pp_print_list ~pp_sep:pp_comma (format_expression jctx ctx))
      args
  | ETuple es ->
    fprintf fmt "(%a)"
      (pp_print_list ~pp_sep:pp_comma (fun fmt e ->
           fprintf fmt "%a" (format_expression jctx ctx) e))
      es
  | ETupleAccess { e1; index; _ } ->
    fprintf fmt "%a[%d]" (format_expression jctx ctx) e1 index
  | EExternal { modname; name } ->
    fprintf fmt "%a.%s" VarName.format (Mark.remove modname) (Mark.remove name)

let rec format_stmt ?scope jctx (ctx : Ast.ctx) ppf (stmt : Ast.stmt Mark.pos) =
  match Mark.remove stmt with
  | SLocalDecl { name; typ } ->
    fprintf ppf "@[<hov 2>%a@ %a;@]" format_typ typ VarName.format
      (Mark.remove name)
  | SLocalDef { name; typ = _; expr } ->
    fprintf ppf "@[<hov 2>%a = %a;@]" VarName.format (Mark.remove name)
      (format_expression jctx ctx)
      expr
  | SReturn expr -> (
    match scope, Mark.remove expr with
    | Some sbody, EStruct { name; fields; _ } ->
      let out_struct_name =
        match sbody.scope_body_func.func_return_typ with
        | TStruct sn, _ -> sn
        | _ -> assert false
      in
      assert (StructName.equal name out_struct_name);
      (* we are in a constructor: assign outputs *)
      let pp_self_assign ppf (field, expr) =
        fprintf ppf "@[<hov 2>this.%a = %a;@]" StructField.format field
          (format_expression jctx ctx)
          expr
      in
      fprintf ppf "@[<v>%a@]"
        (pp_print_list ~pp_sep:pp_print_space pp_self_assign)
        (StructField.Map.bindings fields)
    | _ -> fprintf ppf "@[<hov 2>return %a;@]" (format_expression jctx ctx) expr
    )
  | SInnerFuncDef _ -> fprintf ppf "<todo: SInnerFuncDef>"
  | SLocalInit _ -> fprintf ppf "<todo: SLocalInit>"
  | SFatalError _ -> fprintf ppf "<todo: SFatalError>"
  | SIfThenElse _ -> fprintf ppf "<todo: SIfThenElse>"
  | SSwitch { switch_var; switch_var_typ = _; enum_name; switch_cases } ->
    let format_init_case ppf (var, (typ : typ)) =
      match Mark.remove typ with
      | TLit TUnit -> ()
      | TTuple _ ->
        fprintf ppf "%a %a = %a.getContents();@ " format_typ typ VarName.format
          var VarName.format switch_var
      | _ ->
        fprintf ppf "%a %a = (%a)%a.getContents();@ " format_typ typ
          VarName.format var format_typ typ VarName.format switch_var
    in
    let format_switch_case
        ppf
        (enum_cstr, { case_block; payload_var_name; payload_var_typ }) =
      let case_block =
        match case_block with (SLocalInit _, _) :: r -> r | x -> x
      in
      fprintf ppf "@[<v 2>case %a:@ %a%a@ break;@]" EnumConstructor.format
        enum_cstr format_init_case
        (payload_var_name, payload_var_typ)
        (format_block ?scope jctx ctx)
        case_block
    in
    let enum_cstrs =
      EnumName.Map.find enum_name ctx.decl_ctx.ctx_enums
      |> EnumConstructor.Map.keys
    in
    fprintf ppf "@[<v 2>switch (%a.getKind()) {@ %a@ }@]" VarName.format
      switch_var
      (pp_print_list ~pp_sep:pp_print_space format_switch_case)
      (List.combine enum_cstrs switch_cases)
  | SAssert _ -> fprintf ppf "<todo: SAssert>"
  | SSpecialOp _ -> .

and format_block ?scope jctx ctx ppf (block : Ast.block) =
  fprintf ppf "@[<v>%a@]"
    (pp_print_list ~pp_sep:pp_print_space (format_stmt ?scope jctx ctx))
    block

let format_constructor_body (ctx : Ast.ctx) sbody ppf =
  let in_struct_name =
    match sbody.scope_body_func.func_params with
    | [(_vname, (TStruct sn, _))] -> sn
    | _ -> assert false
  in
  format_block ~scope:sbody in_struct_name ctx ppf
    sbody.scope_body_func.func_body

let format_constructor (ctx : Ast.ctx) (sbody : scope_body) ppf =
  let in_struct_name =
    match sbody.scope_body_func.func_params with
    | [(_vname, (TStruct sn, _))] -> sn
    | _ -> assert false
  in
  StructName.Map.find_opt in_struct_name ctx.decl_ctx.ctx_structs
  |> function
  | None -> ()
  | Some in_fields ->
    fprintf ppf "@[<hov 2>%a%a (@[<hov>%t@]) {@\n%t@]@\n}" format_visibility
      sbody.scope_body_visibility ScopeName.format sbody.scope_body_name
      (format_struct_params in_fields)
      (format_constructor_body ctx sbody)

let format_scope_output_parameters (ctx : Ast.ctx) (sbody : scope_body) ppf =
  let out_struct_name =
    match sbody.scope_body_func.func_return_typ with
    | TStruct sn, _ -> sn
    | _ -> assert false
  in
  StructName.Map.find_opt out_struct_name ctx.decl_ctx.ctx_structs
  |> function
  | None -> ()
  | Some out_fields ->
    let format_output_parameter ppf (field_name, typ) =
      fprintf ppf "@[<h>%afinal@ %a@ %a;@]" format_visibility
        sbody.scope_body_visibility format_typ typ StructField.format field_name
    in
    fprintf ppf "@[<v>%a@]@\n"
      (pp_print_list ~pp_sep:pp_print_space format_output_parameter)
      (StructField.Map.bindings out_fields)

let generate_scope ~package ~dir (p : Ast.program) (sbody : Ast.scope_body) =
  let open File in
  with_formatter_of_file
    ((dir / ScopeName.to_string sbody.scope_body_name) -.- ext)
  @@ fun ppf ->
  fprintf ppf
    "package %s;@\n\
     import catala.runtime.*;@\n\
     @\n\
     @[<v 4>@[<hov 2>%aclass %a {@]@ @ %t@ %t@]@\n\
     }"
    package format_visibility sbody.scope_body_visibility ScopeName.format
    sbody.scope_body_name
    (format_scope_output_parameters p.ctx sbody)
    (format_constructor p.ctx sbody)

let generate_ctx ~package ~dir p =
  let ctx = p.ctx in
  let generate_enum ppf (ename, cstrs) =
    let format_enum_kind ppf =
      fprintf ppf "@[<hov 2>public enum Kind {@ %a@] }"
        (pp_print_list ~pp_sep:pp_comma EnumConstructor.format)
        (EnumConstructor.Map.keys cstrs)
    in
    let format_enum_params ppf =
      fprintf ppf
        "private final CatalaValue contents;@\npublic final Kind kind;"
    in
    let format_enum_constrs ppf =
      let format_enum_make ppf (cstr, typ) =
        let is_unit =
          match Mark.remove typ with TLit TUnit -> true | _ -> false
        in
        let format_arg ppf =
          if is_unit then () else fprintf ppf "%a v" format_typ typ
        in
        fprintf ppf
          "@[<v 2>public static %a make%a(%t) {@ return new %a(Kind.%a, %s);@]@\n\
           }"
          EnumName.format ename EnumConstructor.format cstr format_arg
          EnumName.format ename EnumConstructor.format cstr
          (if is_unit then "CatalaUnit.INSTANCE" else "v")
      in
      fprintf ppf
        "@[<v 2>private %a(Kind k, CatalaValue contents) {@ this.kind = k;@ \
         this.contents = contents;@ @]}@ @ @[<v>%a@]"
        EnumName.format ename
        (pp_print_list ~pp_sep:pp_print_space format_enum_make)
        (EnumConstructor.Map.bindings cstrs)
    in
    let format_enum_accessors ppf =
      let format_default_accessor ppf =
        fprintf ppf
          "@[<v 2>public <T> T getContentsAs(%a.Kind k, Class<T> clazz) {@ \
           @[<v 2>if (this.kind != k) {@ throw new CatalaException(\"Invalid \
           enum contents access: expected \" + k + \", got \" + this.kind);@]@ \
           }@ return (T) this.contents;@]@ }"
          EnumName.format ename
      in
      let format_enum_accessor ppf (cstr, typ) =
        let is_unit =
          match Mark.remove typ with TLit TUnit -> true | _ -> false
        in
        if is_unit then ()
        else
          fprintf ppf
            "@[<v 2>public %a get%aContents() {@ return \
             this.getContentsAs(Kind.%a, %a.class);@]@\n\
             }"
            format_typ typ EnumConstructor.format cstr EnumConstructor.format
            cstr format_typ typ
      in
      fprintf ppf "@[<v>%t@\n@\n%a@]" format_default_accessor
        (pp_print_list ~pp_sep:pp_print_space format_enum_accessor)
        (EnumConstructor.Map.bindings cstrs)
    in
    fprintf ppf
      "package %s;@\n\
       import catala.runtime.*;@\n\
       import catala.runtime.exception.*;@\n\
       @\n\
       @[<v 4>@[<hov 2>public class %a {@]@ @ %t@ @ %t@ @ %t@ @ %t@]@\n\
       }"
      package EnumName.format ename format_enum_kind format_enum_params
      format_enum_constrs format_enum_accessors
  in
  let enums_to_generate =
    EnumName.Map.filter
      (fun ename _ ->
        EnumName.path ename = [] && EnumName.to_string ename <> "Eoption")
      ctx.decl_ctx.ctx_enums
  in
  EnumName.Map.iter
    (fun ename cstrs ->
      File.(with_formatter_of_file ((dir / EnumName.to_string ename) -.- ext))
      @@ fun ppf -> generate_enum ppf (ename, cstrs))
    enums_to_generate;
  let generate_struct ppf (sname, fields) =
    let format_params ppf =
      let format_output_parameter ppf (field_name, typ) =
        fprintf ppf "@[<h>public final@ %a@ %a;@]" format_typ typ
          StructField.format field_name
      in
      fprintf ppf "@[<v>%a@]"
        (pp_print_list ~pp_sep:pp_print_space format_output_parameter)
        (StructField.Map.bindings fields)
    in
    let format_struct_constr_body ppf =
      let fields = StructField.Map.bindings fields in
      fprintf ppf "@[<v>%a@]"
        (pp_print_list ~pp_sep:pp_print_space (fun ppf (sfield, _typ) ->
             fprintf ppf "this.%a = %a;" StructField.format sfield
               StructField.format sfield))
        fields
    in
    let format_struct_constr ppf =
      fprintf ppf "@[<hov 2>public %a (@[<hov>%t@]) {@\n%t@]@\n}"
        StructName.format sname
        (format_struct_params fields)
        format_struct_constr_body
    in
    fprintf ppf
      "package %s;@\n\
       import catala.runtime.*;@\n\
       @\n\
       @[<v 4>@[<hov 2>public class %a {@]@ @ %t@ @ %t@]@\n\
       }"
      package StructName.format sname format_params format_struct_constr
  in
  let structs_to_generate =
    let scope_structs =
      ScopeName.Map.fold
        (fun _sname scope_info acc ->
          StructName.Set.(
            add scope_info.in_struct_name acc |> add scope_info.out_struct_name))
        ctx.decl_ctx.ctx_scopes StructName.Set.empty
    in
    StructName.Map.filter
      (fun sname _ -> not (StructName.Set.mem sname scope_structs))
      ctx.decl_ctx.ctx_structs
  in
  StructName.Map.iter
    (fun sname cstrs ->
      File.(with_formatter_of_file ((dir / StructName.to_string sname) -.- ext))
      @@ fun ppf -> generate_struct ppf (sname, cstrs))
    structs_to_generate

let generate_program
    ~output_dir
    ~input_file
    (p : Ast.program)
    (_type_ordering : TypeIdent.t list) : unit =
  let open File in
  let package_name =
    match p.module_name with
    | None -> Filename.chop_extension input_file
    | Some (mname, _) -> ModuleName.to_string mname
  in
  let prog_dir = output_dir / "src" / "main" / "java" / package_name in
  (* Creates the output directory *)
  Catala_utils.File.ensure_dir prog_dir;
  let scopes =
    List.filter_map
      (function SScope body -> Some body | _ -> None)
      p.code_items
  in
  (* Copy over the runtime *)
  (* TODO: add --no-runtime option ? *)
  ignore
  @@ File.process_out "cp"
       [
         "-r";
         "/home/vincent/catala/runtimes/java/pom.xml";
         "/home/vincent/catala/runtimes/java/src/";
         output_dir;
       ];
  generate_ctx ~package:package_name ~dir:prog_dir p;
  List.iter (generate_scope ~package:package_name ~dir:prog_dir p) scopes
