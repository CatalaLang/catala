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
module Runtime = Catala_runtime
module D = Dcalc.Ast
module L = Lcalc.Ast

let format_lit (fmt : Format.formatter) (l : lit Mark.pos) : unit =
  match Mark.remove l with
  | LBool true -> Format.pp_print_string fmt "True"
  | LBool false -> Format.pp_print_string fmt "False"
  | LInt i -> Format.fprintf fmt "Integer(%s)" (Runtime.integer_to_string i)
  | LUnit -> Format.pp_print_string fmt "Unit()"
  | LRat i -> Format.fprintf fmt "decimal_of_string(\"%s\")" (Q.to_string i)
  | LMoney e ->
    Format.fprintf fmt "Money(Integer(%s))"
      (Runtime.integer_to_string (Runtime.money_to_cents e))
  | LDate d ->
    Format.fprintf fmt "Date((%d,%d,%d))"
      (Runtime.integer_to_int (Runtime.year_of_date d))
      (Runtime.integer_to_int (Runtime.month_number_of_date d))
      (Runtime.integer_to_int (Runtime.day_of_month_of_date d))
  | LDuration d ->
    let years, months, days = Runtime.duration_to_years_months_days d in
    Format.fprintf fmt "Duration((%d,%d,%d))" years months days

let format_op (fmt : Format.formatter) (op : operator Mark.pos) : unit =
  match Mark.remove op with
  | Log (_entry, _infos) -> assert false
  | Minus_int | Minus_rat | Minus_mon | Minus_dur ->
    Format.pp_print_string fmt "-"
  (* Todo: use the names from [Operator.name] *)
  | Not -> Format.pp_print_string fmt "not"
  | Length -> Format.pp_print_string fmt "list_length"
  | ToInt_rat -> Format.pp_print_string fmt "integer_of_decimal"
  | ToInt_mon -> Format.pp_print_string fmt "integer_of_money"
  | ToRat_int -> Format.pp_print_string fmt "decimal_of_integer"
  | ToRat_mon -> Format.pp_print_string fmt "decimal_of_money"
  | ToMoney_rat -> Format.pp_print_string fmt "money_of_decimal"
  | ToMoney_int -> Format.pp_print_string fmt "money_of_integer"
  | Round_mon -> Format.pp_print_string fmt "money_round"
  | Round_rat -> Format.pp_print_string fmt "decimal_round"
  | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dur_dur | Concat ->
    Format.pp_print_string fmt "+"
  | Add_dat_dur rounding ->
    Format.fprintf fmt "add_date_duration(%s)"
      (match rounding with
      | RoundUp -> "dates.DateRounding.RoundUp"
      | RoundDown -> "dates.DateRounding.RoundDown"
      | AbortOnRound -> "dates.DateRounding.AbortOnRound")
  | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dur_dur ->
    Format.pp_print_string fmt "-"
  | Sub_dat_dur rounding ->
    Format.fprintf fmt "sub_date_duration(%s)"
      (match rounding with
      | RoundUp -> "dates.DateRounding.RoundUp"
      | RoundDown -> "dates.DateRounding.RoundDown"
      | AbortOnRound -> "dates.DateRounding.AbortOnRound")
  | Mult_int_int | Mult_rat_rat | Mult_mon_int | Mult_mon_rat | Mult_dur_int ->
    Format.pp_print_string fmt "*"
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int | Div_mon_rat
  | Div_dur_dur ->
    Format.pp_print_string fmt "div"
  | And -> Format.pp_print_string fmt "and"
  | Or -> Format.pp_print_string fmt "or"
  | Eq -> Format.pp_print_string fmt "=="
  | Xor -> Format.pp_print_string fmt "!="
  | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat ->
    Format.pp_print_string fmt "<"
  | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat ->
    Format.pp_print_string fmt "<="
  | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat ->
    Format.pp_print_string fmt ">"
  | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat ->
    Format.pp_print_string fmt ">="
  | Eq_boo_boo | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dat_dat ->
    Format.pp_print_string fmt "=="
  | Lt_dur_dur -> Format.pp_print_string fmt "lt_duration"
  | Lte_dur_dur -> Format.pp_print_string fmt "le_duration"
  | Gt_dur_dur -> Format.pp_print_string fmt "gt_duration"
  | Gte_dur_dur -> Format.pp_print_string fmt "ge_duration"
  | Eq_dur_dur -> Format.pp_print_string fmt "eq_duration"
  | Map -> Format.pp_print_string fmt "list_map"
  | Map2 -> Format.pp_print_string fmt "list_map2"
  | Reduce -> Format.pp_print_string fmt "list_reduce"
  | Filter -> Format.pp_print_string fmt "list_filter"
  | Fold -> Format.pp_print_string fmt "list_fold_left"
  | HandleExceptions -> Format.pp_print_string fmt "handle_exceptions"
  | FromClosureEnv | ToClosureEnv -> failwith "unimplemented"

let format_uid_list (fmt : Format.formatter) (uids : Uid.MarkedString.info list)
    : unit =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       (fun fmt info ->
         Format.fprintf fmt "\"%a\"" Uid.MarkedString.format info))
    uids

let format_string_list (fmt : Format.formatter) (uids : string list) : unit =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       Format.pp_print_string)
    (List.map String.quote uids)

let python_keywords =
  (* list taken from
     https://www.programiz.com/python-programming/keyword-list *)
  [
    "False";
    "None";
    "True";
    "and";
    "as";
    "assert";
    "async";
    "await";
    "break";
    "class";
    "continue";
    "def";
    "del";
    "elif";
    "else";
    "except";
    "finally";
    "for";
    "from";
    "global";
    "if";
    "import";
    "in";
    "is";
    "lambda";
    "nonlocal";
    "not";
    "or";
    "pass";
    "raise";
    "return";
    "try";
    "while";
    "with";
    "yield";
  ]
(* todo: reserved names should also include built-in types and everything
   exposed by the runtime. *)

let renaming =
  Renaming.program ()
    ~reserved:python_keywords
      (* TODO: add catala runtime built-ins as reserved as well ? *)
    ~skip_constant_binders:false ~constant_binder_name:None
    ~namespaced_fields:true ~namespaced_constrs:true ~prefix_module:false
    ~modnames_conflict:false ~f_var:String.to_snake_case
    ~f_struct:String.to_camel_case ~f_enum:String.to_camel_case

let format_qualified
    (type id)
    (module Id : Uid.Qualified with type t = id)
    ctx
    ppf
    (s : id) =
  let rec shorten_rpath = function
    | m :: _ when ModuleName.Map.mem m ctx.modules -> [m]
    | m :: r -> m :: shorten_rpath r
    | [] -> []
  in
  let path = List.rev (shorten_rpath (List.rev (Id.path s))) in
  List.iter
    (fun m ->
      VarName.format ppf (ModuleName.Map.find m ctx.modules);
      Format.pp_print_char ppf '.')
    path;
  Format.pp_print_string ppf (Id.base s)

let format_struct = format_qualified (module StructName)
let format_enum = format_qualified (module EnumName)

let typ_needs_parens (e : typ) : bool =
  match Mark.remove e with TArrow _ | TArray _ -> true | _ -> false

let rec format_typ ctx (fmt : Format.formatter) (typ : typ) : unit =
  let format_typ = format_typ ctx in
  let format_typ_with_parens (fmt : Format.formatter) (t : typ) =
    if typ_needs_parens t then Format.fprintf fmt "(%a)" format_typ t
    else Format.fprintf fmt "%a" format_typ t
  in
  match Mark.remove typ with
  | TLit TUnit -> Format.fprintf fmt "Unit"
  | TLit TMoney -> Format.fprintf fmt "Money"
  | TLit TInt -> Format.fprintf fmt "Integer"
  | TLit TRat -> Format.fprintf fmt "Decimal"
  | TLit TDate -> Format.fprintf fmt "Date"
  | TLit TDuration -> Format.fprintf fmt "Duration"
  | TLit TBool -> Format.fprintf fmt "bool"
  | TLit TPos -> Format.fprintf fmt "SourcePosition"
  | TTuple ts ->
    Format.fprintf fmt "Tuple[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt t -> Format.fprintf fmt "%a" format_typ_with_parens t))
      ts
  | TStruct s -> format_struct ctx fmt s
  | TOption some_typ -> Format.fprintf fmt "Option[%a]" format_typ some_typ
  | TDefault t -> format_typ fmt t
  | TEnum e -> format_enum ctx fmt e
  | TAbstract t -> format_qualified (module AbstractType) ctx fmt t
  | TArrow (t1, t2) ->
    Format.fprintf fmt "Callable[[%a], %a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         format_typ_with_parens)
      t1 format_typ_with_parens t2
  | TArray t1 -> Format.fprintf fmt "List[%a]" format_typ_with_parens t1
  | TVar _ -> Format.fprintf fmt "Any"
  | TForAll tb ->
    let _v, typ = Bindlib.unmbind tb in
    format_typ fmt typ
  | TClosureEnv -> failwith "unimplemented!"
  | TError -> assert false

let format_func_name (fmt : Format.formatter) (v : FuncName.t) : unit =
  FuncName.format fmt v

let rec format_expression ctx (fmt : Format.formatter) (e : expr) : unit =
  match Mark.remove e with
  | EVar v -> VarName.format fmt v
  | EFunc f -> FuncName.format fmt f
  | EStruct { fields = es; name = s } ->
    Format.fprintf fmt "%a(%a)" (format_struct ctx) s
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (struct_field, e) ->
           Format.fprintf fmt "%a = %a" StructField.format struct_field
             (format_expression ctx) e))
      (StructField.Map.bindings es)
  | EStructFieldAccess { e1; field; _ } ->
    Format.fprintf fmt "%a.%a" (format_expression ctx) e1 StructField.format
      field
  | EInj { cons; name = e_name; _ }
    when EnumName.equal e_name Expr.option_enum
         && EnumConstructor.equal cons Expr.none_constr ->
    Format.fprintf fmt "Option(None)"
  | EInj { e1 = e; cons; name = e_name; _ }
    when EnumName.equal e_name Expr.option_enum
         && EnumConstructor.equal cons Expr.some_constr ->
    Format.fprintf fmt "Option(%a)" (format_expression ctx) e
  | EInj { e1 = e; cons; name = enum_name; _ } ->
    Format.fprintf fmt "%a(%a_Code.%a,@ %a)" (format_enum ctx) enum_name
      (format_enum ctx) enum_name EnumConstructor.format cons
      (format_expression ctx) e
  | EArray es ->
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt e -> Format.fprintf fmt "%a" (format_expression ctx) e))
      es
  | ELit l -> Format.fprintf fmt "%a" format_lit (Mark.copy e l)
  | EPosLit ->
    let pos = Mark.get e in
    Format.fprintf fmt
      "@[<hov 4>SourcePosition(@,\
       filename=\"%s\",@ start_line=%d, start_column=%d,@ end_line=%d, \
       end_column=%d,@ law_headings=%a@;\
       <0 -4>)@]" (Pos.get_file pos) (Pos.get_start_line pos)
      (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos)
      format_string_list (Pos.get_law_info pos)
  | EAppOp
      {
        op = ((HandleExceptions | Map | Filter), _) as op;
        args = [arg1; arg2];
        _;
      } ->
    Format.fprintf fmt "%a(%a,@ %a)" format_op op (format_expression ctx) arg1
      (format_expression ctx) arg2
  | EAppOp { op; args = [arg1; arg2]; _ } ->
    Format.fprintf fmt "(%a %a@ %a)" (format_expression ctx) arg1 format_op op
      (format_expression ctx) arg2
  | EApp
      {
        f = EAppOp { op = Log (BeginCall, info), _; args = [f]; _ }, _;
        args = [arg];
        _;
      }
    when Global.options.trace <> None ->
    Format.fprintf fmt "log_begin_call(%a,@ %a,@ %a)" format_uid_list info
      (format_expression ctx) f (format_expression ctx) arg
  | EAppOp { op = Log (VarDef var_def_info, info), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    Format.fprintf fmt
      "log_variable_definition(%a,@ LogIO(input_io=InputIO.%s,@ \
       output_io=%s),@ %a)"
      format_uid_list info
      (match var_def_info.log_io_input with
      | Runtime.NoInput -> "NoInput"
      | Runtime.OnlyInput -> "OnlyInput"
      | Runtime.Reentrant -> "Reentrant")
      (if var_def_info.log_io_output then "True" else "False")
      (format_expression ctx) arg1
  | EAppOp { op = Log (PosRecordIfTrueBool, _), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    let pos = Mark.get e in
    Format.fprintf fmt
      "log_decision_taken(SourcePosition(filename=\"%s\",@ start_line=%d,@ \
       start_column=%d,@ end_line=%d, end_column=%d,@ law_headings=%a), %a)"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos) (format_expression ctx) arg1
  | EAppOp { op = Log (EndCall, info), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    Format.fprintf fmt "log_end_call(%a,@ %a)" format_uid_list info
      (format_expression ctx) arg1
  | EAppOp { op = Log _, _; args = [arg1]; _ } ->
    Format.fprintf fmt "%a" (format_expression ctx) arg1
  | EAppOp { op = (Not, _) as op; args = [arg1]; _ } ->
    Format.fprintf fmt "%a %a" format_op op (format_expression ctx) arg1
  | EAppOp
      {
        op = ((Minus_int | Minus_rat | Minus_mon | Minus_dur), _) as op;
        args = [arg1];
        _;
      } ->
    Format.fprintf fmt "%a %a" format_op op (format_expression ctx) arg1
  | EAppOp { op; args = [arg1]; _ } ->
    Format.fprintf fmt "%a(%a)" format_op op (format_expression ctx) arg1
  | EApp { f; args; _ } ->
    Format.fprintf fmt "%a(@[<hv 0>%a)@]" (format_expression ctx) f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | EAppOp { op; args; _ } ->
    Format.fprintf fmt "%a(@[<hv 0>%a)@]" format_op op
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | ETuple es ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt e -> Format.fprintf fmt "%a" (format_expression ctx) e))
      es
  | ETupleAccess { e1; index; _ } ->
    Format.fprintf fmt "%a[%d]" (format_expression ctx) e1 index
  | EExternal { modname; name } ->
    Format.fprintf fmt "%a.%s" VarName.format (Mark.remove modname)
      (Mark.remove name)

let rec format_statement ctx (fmt : Format.formatter) (s : stmt Mark.pos) : unit
    =
  match Mark.remove s with
  | SInnerFuncDef { name; func = { func_params; func_body; _ } } ->
    Format.fprintf fmt "@[<v 4>def %a(@[<hov>%a@]):@ %a@]" VarName.format
      (Mark.remove name)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (var, typ) ->
           Format.fprintf fmt "%a:%a" VarName.format (Mark.remove var)
             (format_typ ctx) typ))
      func_params (format_block ctx) func_body
  | SLocalDecl _ ->
    assert false (* We don't need to declare variables in Python *)
  | SLocalDef { name = v; expr = e; _ } | SLocalInit { name = v; expr = e; _ }
    ->
    Format.fprintf fmt "@[<hv 4>%a = (%a)@]" VarName.format (Mark.remove v)
      (format_expression ctx) e
  | SFatalError { pos_expr; error } ->
    Format.fprintf fmt "@[<hov 4>raise %s(%a)@]"
      (Runtime.error_to_string error)
      (format_expression ctx) pos_expr
  | SIfThenElse { if_expr; then_block; else_block } ->
    let rec pr_else = function
      | [(SIfThenElse { if_expr; then_block; else_block }, _)] ->
        Format.fprintf fmt "@[<v 4>elif @[<hov>%a@]:@ %a@]@,"
          (format_expression ctx) if_expr (format_block ctx) then_block;
        pr_else else_block
      | else_block ->
        Format.fprintf fmt "@[<v 4>else:@ %a@]" (format_block ctx) else_block
    in
    Format.fprintf fmt "@[<v 4>if @[<hov>%a@]:@ %a@]@," (format_expression ctx)
      if_expr (format_block ctx) then_block;
    pr_else else_block
  | SSwitch
      {
        switch_var;
        enum_name = e_name;
        switch_cases =
          [
            { case_block = case_none; _ };
            {
              case_block = case_some;
              payload_var_name = case_some_var;
              payload_var_typ;
            };
          ];
        _;
      }
    when EnumName.equal e_name Expr.option_enum ->
    let pos = Mark.get s in
    Format.fprintf fmt "@[<v 4>if %a.value is not None:@ %a@]@," VarName.format
      switch_var (format_block ctx)
      (Utils.subst_block case_some_var
         (* Not a real catala struct, but will print as <var>.value *)
         ( EStructFieldAccess
             {
               e1 = EVar switch_var, pos;
               field = StructField.fresh ("value", pos);
               name = StructName.fresh [] ("Dummy", pos);
             },
           pos )
         payload_var_typ pos case_some);
    Format.fprintf fmt "@[<v 4>else:@ %a@]" (format_block ctx) case_none
  | SSwitch { switch_var; enum_name = e_name; switch_cases = cases; _ } ->
    let cons_map = EnumName.Map.find e_name ctx.decl_ctx.ctx_enums in
    let pos = Mark.get s in
    let cases =
      List.map2
        (fun x (cons, _) -> x, cons)
        cases
        (EnumConstructor.Map.bindings cons_map)
    in
    Format.fprintf fmt "@[<v 4>if %a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@]@\n@[<v 4>elif ")
         (fun fmt (case, cons_name) ->
           Format.fprintf fmt "%a.code == %a_Code.%a:@," VarName.format
             switch_var (format_enum ctx) e_name EnumConstructor.format
             cons_name;
           format_block ctx fmt
             (Utils.subst_block case.payload_var_name
                (* Not a real catala struct, but will print as <var>.value *)
                ( EStructFieldAccess
                    {
                      e1 = EVar switch_var, pos;
                      field = StructField.fresh ("value", pos);
                      name = StructName.fresh [] ("Dummy", pos);
                    },
                  pos )
                case.payload_var_typ pos case.case_block)))
      cases
  | SReturn e1 ->
    Format.fprintf fmt "@[<hov 4>return %a@]" (format_expression ctx) e1
  | SAssert { pos_expr; expr = e1 } ->
    Format.fprintf fmt "@[<hv 4>if not (%a):@ raise AssertionFailed(%a)@]"
      (format_expression ctx) e1 (format_expression ctx) pos_expr
  | SSpecialOp _ -> .

and format_block ctx (fmt : Format.formatter) (b : block) : unit =
  Format.pp_open_vbox fmt 0;
  Format.pp_print_list (format_statement ctx) fmt
    (List.filter
       (fun s -> match Mark.remove s with SLocalDecl _ -> false | _ -> true)
       b);
  Format.pp_close_box fmt ()

let format_ctx (type_ordering : TypeIdent.t list) (fmt : Format.formatter) ctx :
    unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    let fields = StructField.Map.bindings struct_fields in
    Format.fprintf fmt
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
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt (struct_field, struct_field_type) ->
           Format.fprintf fmt "%a: %a" StructField.format struct_field
             (format_typ ctx) struct_field_type))
      fields
      (if StructField.Map.is_empty struct_fields then fun fmt _ ->
         Format.fprintf fmt "        pass"
       else
         Format.pp_print_list (fun fmt (struct_field, _) ->
             Format.fprintf fmt "        self.%a = %a" StructField.format
               struct_field StructField.format struct_field))
      fields StructName.format struct_name
      (if not (StructField.Map.is_empty struct_fields) then
         Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " and@ ")
           (fun fmt (struct_field, _) ->
             Format.fprintf fmt "self.%a == other.%a" StructField.format
               struct_field StructField.format struct_field)
       else fun fmt _ -> Format.fprintf fmt "True")
      fields StructName.format struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
         (fun fmt (struct_field, _) ->
           Format.fprintf fmt "%a={}" StructField.format struct_field))
      fields
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (struct_field, _) ->
           Format.fprintf fmt "self.%a" StructField.format struct_field))
      fields
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if EnumConstructor.Map.is_empty enum_cons then
      failwith "no constructors in the enum"
    else
      Format.fprintf fmt
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
        (Format.pp_print_list (fun fmt (i, enum_cons, _enum_cons_type) ->
             Format.fprintf fmt "%a = %d" EnumConstructor.format enum_cons i))
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
        | TypeIdent.Enum _ | TypeIdent.Abstract _ -> false
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
          Format.fprintf fmt "%a@,@," format_struct_decl
            (s, StructName.Map.find s ctx.decl_ctx.ctx_structs)
      | TypeIdent.Enum e ->
        if EnumName.path e = [] && not (EnumName.equal e Expr.option_enum) then
          Format.fprintf fmt "%a@,@," format_enum_decl
            (e, EnumName.Map.find e ctx.decl_ctx.ctx_enums)
      | TypeIdent.Abstract t ->
        if AbstractType.path t = [] then
          Format.fprintf fmt "class %a:@,@," AbstractType.format t)
    (type_ordering @ scope_structs)

let format_code_item ctx fmt = function
  | SVar { var; expr; typ = _; visibility = _ } ->
    Format.fprintf fmt "@[<hv 4>%a = (@,%a@;<0 -4>)@]@," VarName.format var
      (format_expression ctx) expr
  | SFunc { var; func; visibility = _ }
  | SScope { scope_body_var = var; scope_body_func = func; _ } ->
    let { Ast.func_params; Ast.func_body; _ } = func in
    Format.fprintf fmt "@[<v 4>@[<hov 2>def %a(@,%a@;<0 -2>):@]@ %a@]@,"
      format_func_name var
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (var, typ) ->
           Format.fprintf fmt "%a:%a" VarName.format (Mark.remove var)
             (format_typ ctx) typ))
      func_params (format_block ctx) func_body

let format_tests ctx ppf (p : Ast.program) =
  let closures, tests = p.tests in
  assert (closures = []);
  if tests = [] then
    Message.warning
      "%a@{<magenta>#[test]@}@ attribute@ above@ their@ declaration."
      Format.pp_print_text
      "No test scope were found: the generated executable won't test any \
       computation. To mark scopes as tests, ensure they don't require inputs, \
       and add the "
  else
    let () =
      Message.debug "@[<hov 2>Generating entry points for scopes:@ %a@]@."
        (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (s, _) ->
             ScopeName.format ppf s))
        tests
    in
    Format.fprintf ppf "@,# Automatic Catala tests@,";
    Format.fprintf ppf "@[<v 2>if __name__ == \"__main__\":";
    List.iter
      (fun (name, block) ->
        Format.pp_print_cut ppf ();
        (* Format.fprintf ppf "@,print(\"Executing scope %a...\")@," ScopeName.format
         *   name; *)
        format_block ctx ppf block;
        Format.fprintf ppf
          "@,\
           print(\"\\x1b[32m[RESULT]\\x1b[m Scope %a executed successfully.\")"
          ScopeName.format name)
      tests;
    Format.fprintf ppf "@]@,"

let format_program
    (output_file : File.t option)
    (fmt : Format.formatter)
    (p : Ast.program)
    (type_ordering : TypeIdent.t list) : unit =
  (* Disable the formatting line length limit which may lead to broken
     indentation (we need to disable all line breaking for this, since Format
     doesn't accept max_indent >= margin) *)
  Format.pp_set_geometry fmt ~max_indent:999_990 ~margin:1_000_000;
  Format.pp_open_vbox fmt 0;
  let header =
    (if Global.options.gen_external then
       [
         "# This is a template file following the expected interface and \
          declarations to";
         "# implement the corresponding Catala module.";
         "#";
         "# You should replace all `raise Impossible` place-holders with your";
         "# implementation and rename it to remove the \".template\" suffix.";
       ]
     else
       ["# This file has been generated by the Catala compiler, do not edit!"])
    @ [
        "";
        "from catala_runtime import *";
        "from typing import Any, List, Callable, Tuple";
        "from enum import Enum";
        "";
      ]
  in
  Format.pp_print_list Format.pp_print_string fmt header;
  List.iter
    (fun (m, _) ->
      Format.fprintf fmt "from . import %a as %a@," ModuleName.format
        (ModuleName.normalise m) VarName.format
        (ModuleName.Map.find m p.ctx.modules))
    (Program.modules_to_list ~trim_stdlib:true p.ctx.decl_ctx.ctx_modules);
  Format.pp_print_cut fmt ();
  format_ctx type_ordering fmt p.ctx;
  Format.pp_print_cut fmt ();
  Format.pp_print_list (format_code_item p.ctx) fmt p.code_items;
  if snd p.tests <> [] then format_tests p.ctx fmt p;
  Format.pp_print_flush fmt ();
  if Global.options.gen_external then
    output_file
    |> Option.iter
         (Message.result "Generated template external implementations:@ %a"
            File.format)
