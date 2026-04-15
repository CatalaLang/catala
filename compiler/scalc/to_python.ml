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
  | LBool true -> Format.pp_print_string fmt "Bool(True)"
  | LBool false -> Format.pp_print_string fmt "Bool(False)"
  | LInt i -> Format.fprintf fmt "Integer(%s)" (Runtime.integer_to_string i)
  | LUnit -> Format.pp_print_string fmt "Unit()"
  | LRat i -> Format.fprintf fmt "Decimal('%s')" (Q.to_string i)
  | LMoney e -> Format.fprintf fmt "Money('%s')" (Runtime.money_to_string e)
  | LDate d ->
    Format.fprintf fmt "Date(%d,%d,%d)"
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
  | Not -> Format.pp_print_string fmt ".not_()"
  | Length -> Format.pp_print_string fmt "length"
  | ToInt_rat -> Format.pp_print_string fmt "Integer"
  | ToInt_mon -> Format.pp_print_string fmt "Integer"
  | ToRat_int -> Format.pp_print_string fmt "Decimal"
  | ToRat_mon -> Format.pp_print_string fmt "Decimal"
  | ToMoney_rat -> Format.pp_print_string fmt "Money"
  | ToMoney_int -> Format.pp_print_string fmt "Money"
  | Round_mon -> Format.pp_print_string fmt ".round()"
  | Round_rat -> Format.pp_print_string fmt ".round()"
  | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dur_dur | Concat ->
    Format.pp_print_string fmt "+"
  | Add_dat_dur _ -> Format.fprintf fmt ".__add__"
  | Sub_dat_dur _ -> Format.fprintf fmt ".__sub__"
  | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dur_dur ->
    Format.pp_print_string fmt "-"
  | Mult_int_int | Mult_rat_rat | Mult_mon_int | Mult_mon_rat | Mult_dur_int ->
    Format.pp_print_string fmt "*"
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int | Div_mon_rat
  | Div_dur_dur ->
    Format.pp_print_string fmt ".__truediv__"
  | And -> Format.pp_print_string fmt "and"
  | Or -> Format.pp_print_string fmt "or"
  | Eq -> Format.pp_print_string fmt "=="
  | Xor -> Format.pp_print_string fmt "!="
  | Lt ->
    (* FIXME: position argument and errors *)
    Format.pp_print_string fmt "<"
  | Lte -> Format.pp_print_string fmt "<="
  | Gt -> Format.pp_print_string fmt ">"
  | Gte -> Format.pp_print_string fmt ">="
  | Map -> Format.pp_print_string fmt "map"
  | Map2 -> Format.pp_print_string fmt "map2"
  | Reduce -> Format.pp_print_string fmt "reduce"
  | Filter -> Format.pp_print_string fmt "filter"
  | Fold -> Format.pp_print_string fmt "fold_left"
  | HandleExceptions -> Format.pp_print_string fmt "handle_exceptions"
  | ValueFromJson (_ty, str) ->
    Format.fprintf fmt ".from_json(%s" (String.quote str)
  | ArrayAccess _ -> assert false
  | ConstructorCheck _ -> failwith "TODO"
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
    (* todo: reserved names should also include built-in types and everything
       exposed by the runtime. *)
    "Code";
  ]

let op_needs_pos (type a) (op : a Op.t) ty =
  match op with
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int | Div_mon_rat
  | Div_dur_dur | Add_dat_dur _ | Sub_dat_dur _ | Map2 | ValueFromJson _ ->
    true
  | Op.Eq | Lt | Lte | Gt | Gte -> (
    match ty with
    | TLit (TUnit | TBool | TInt | TMoney | TRat | TDate) -> false
    | _ -> true)
  | _ -> false

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
  | TLit TBool -> Format.fprintf fmt "Bool"
  | TLit TPos -> Format.fprintf fmt "SourcePosition"
  | TTuple ts ->
    Format.fprintf fmt "CatalaTuple[%a]"
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
    Format.fprintf fmt "Function [[%a], %a]"
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
    Format.fprintf fmt "%a(%a.Code.%a,@ %a)" (format_enum ctx) enum_name
      (format_enum ctx) enum_name EnumConstructor.format cons
      (format_expression ctx) e
  | EArray es ->
    Format.fprintf fmt "Array([@[<v>%a@]])"
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
       <0 -4>)@]"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos)
  | EAppOp
      {
        op = ((Length | Map | Map2 | Reduce | Filter | Fold), _) as op;
        args;
        _;
      } ->
    let base, args =
      match Mark.remove op, args with
      | Length, [l] -> l, []
      | Map, [f; l] -> l, [f]
      | Map2, [pos; f; l1; l2] -> l1, [pos; f; l2]
      | Reduce, [f; dft; l] -> l, [f; dft]
      | Filter, [f; l] -> l, [f]
      | Fold, [f; init; l] -> l, [f; init]
      | _ -> assert false
    in
    Format.fprintf fmt "%a.%a(%a)" (format_expression ctx) base format_op op
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         (format_expression ctx))
      args
  | EAppOp { op = (HandleExceptions, _) as op; args = [arg1; arg2]; _ } ->
    Format.fprintf fmt "%a(%a,@ %a)" format_op op (format_expression ctx) arg1
      (format_expression ctx) arg2
  | EAppOp
      {
        op = (ValueFromJson (ty, _), _) as op;
        args = [pos_expr; (ELit LUnit, _)];
        _;
      } ->
    Format.fprintf fmt "%a%a, %a)" (format_typ ctx) ty format_op op
      (format_expression ctx) pos_expr
  | EAppOp { op; args = [arg1; arg2]; _ } ->
    let args =
      match Mark.remove op with
      | And | Or ->
        (* right-associative *)
        let rec aux = function
          | EAppOp { op = op1; args = [arg2; arg3]; _ }, _
            when Mark.equal Operator.equal op op1 ->
            arg2 :: aux arg3
          | a -> [a]
        in
        arg1 :: aux arg2
      | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dur_dur | Sub_int_int
      | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dur_dur | Mult_int_int
      | Mult_rat_rat | Div_rat_rat ->
        (* left-associative *)
        let rec aux = function
          | EAppOp { op = op1; args = [arg2; arg3]; _ }, _
            when Mark.equal Operator.equal op op1 ->
            arg2 :: aux arg3
          | a -> [a]
        in
        aux arg1 @ [arg2]
      | _ -> [arg1; arg2]
    in
    Format.fprintf fmt "(@[<hov>%a@])"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " %a@ " format_op op)
         (format_expression ctx))
      args
  | EAppOp
      {
        op = ((Add_dat_dur rounding | Sub_dat_dur rounding), _) as op;
        args = [pos; t; dt];
        _;
      } ->
    Format.fprintf fmt "%a%a(%a, %s, %a)" (format_expression ctx) t format_op op
      (format_expression ctx) dt
      (match rounding with
      | RoundUp -> "dates.DateRounding.RoundUp"
      | RoundDown -> "dates.DateRounding.RoundDown"
      | AbortOnRound -> "dates.DateRounding.AbortOnRound")
      (format_expression ctx) pos
  | EAppOp { op = ArrayAccess n, _; args = [a]; _ } ->
    Format.fprintf fmt "%a[%d]" (format_expression ctx) a n
  | EAppOp
      { op = ((Eq | Lt | Lte | Gt | Gte), _) as op; args = [pos; a1; a2]; _ } ->
    Format.fprintf fmt "%a.%a(@[<hv>%a,@ %a)@]" (format_expression ctx) a1
      (fun ppf -> function
        | Operator.Eq, _ -> Format.pp_print_string ppf "__eq__"
        | Lt, _ -> Format.pp_print_string ppf "__lt__"
        | Lte, _ -> Format.pp_print_string ppf "__le__"
        | Gt, _ -> Format.pp_print_string ppf "__gt__"
        | Gte, _ -> Format.pp_print_string ppf "__ge__"
        | _ -> assert false)
      op (format_expression ctx) a2 (format_expression ctx) pos
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
    Format.fprintf fmt "%a%a" (format_expression ctx) arg1 format_op op
  | EAppOp
      {
        op = ((Minus_int | Minus_rat | Minus_mon | Minus_dur), _) as op;
        args = [arg1];
        _;
      } ->
    Format.fprintf fmt "%a %a" format_op op (format_expression ctx) arg1
  | EAppOp { op = ((Round_mon | Round_rat), _) as op; args = [arg1]; _ } ->
    Format.fprintf fmt "(%a)%a" (format_expression ctx) arg1 format_op op
  | EAppOp { op; args = [arg1]; _ } ->
    Format.fprintf fmt "%a(%a)" format_op op (format_expression ctx) arg1
  | EApp { f = EFunc f, _; args; _ } ->
    Format.fprintf fmt "%a(@[<hv 0>%a)@]" FuncName.format f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | EApp { f; args; _ } ->
    (* can this happen ? *)
    Format.fprintf fmt "%a(@[<hv 0>%a)@]" (format_expression ctx) f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | EFunc f -> Format.fprintf fmt "Function(%a)" FuncName.format f
  | EAppOp
      {
        op =
          ( ( Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int
            | Div_mon_rat | Div_dur_dur ),
            _ ) as op;
        args = [pos; arg1; arg2];
        _;
      } ->
    Format.fprintf fmt "(%a)%a(%a, pos=%a)" (format_expression ctx) arg1
      format_op op (format_expression ctx) arg2 (format_expression ctx) pos
  | EAppOp { op; args; _ } ->
    Format.fprintf fmt "%a(@[<hv 0>%a)@]" format_op op
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | ETuple es ->
    Format.fprintf fmt "CatalaTuple(%a)"
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
    Format.fprintf fmt "@[<v 4>def _%a(@[<hov>%a@]):@ %a@]@,%a = Function(_%a)"
      VarName.format (Mark.remove name)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (var, typ) ->
           Format.fprintf fmt "%a:%a" VarName.format (Mark.remove var)
             (format_typ ctx) typ))
      func_params (format_block ctx) func_body VarName.format (Mark.remove name)
      VarName.format (Mark.remove name)
  | SLocalDecl _ ->
    assert false (* We don't need to declare variables in Python *)
  | SLocalDef { name = v; expr = e; _ } | SLocalInit { name = v; expr = e; _ }
    ->
    Format.fprintf fmt "@[<hv 4>%a = (%a)@]" VarName.format (Mark.remove v)
      (format_expression ctx) e
  | SFatalError { pos_expr; error } ->
    Format.fprintf fmt "@[<hov 4>raise %s(%a%s)@]"
      (Runtime.error_to_string error)
      (format_expression ctx) pos_expr
      (match
         Pos.get_attr (Mark.get s) (function
           | ErrorMessage m -> Some m
           | _ -> None)
       with
      | None -> ""
      | Some m -> ", " ^ String.quote m)
  | SIfThenElse { if_expr; then_block; else_block } ->
    let rec pr_else = function
      | [(SIfThenElse { if_expr; then_block; else_block }, _)] ->
        Format.fprintf fmt "@,@[<v 4>elif @[<hov>%a@]:@ %a@]"
          (format_expression ctx) if_expr (format_block ctx) then_block;
        pr_else else_block
      | [(SLocalDef { expr = ELit LUnit, _; _ }, _)]
      | [(SReturn (ELit LUnit, _), _)] ->
        ()
      | else_block ->
        Format.fprintf fmt "@,@[<v 4>else:@ %a@]" (format_block ctx) else_block
    in
    Format.fprintf fmt "@[<v 4>if @[<hov>%a@]:@ %a@]" (format_expression ctx)
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
           Format.fprintf fmt "%a.code == %a.Code.%a:@," VarName.format
             switch_var (format_enum ctx) e_name EnumConstructor.format
             cons_name;
           format_block ctx fmt
             (Utils.subst_block case.payload_var_name
                (* Not a real catala struct, but will print as <var>.payload *)
                ( EStructFieldAccess
                    {
                      e1 = EVar switch_var, pos;
                      field = StructField.fresh ("payload", pos);
                      name = StructName.fresh [] ("Dummy", pos);
                    },
                  pos )
                case.payload_var_typ pos case.case_block)))
      cases
  | SReturn e1 ->
    Format.fprintf fmt "@[<hov 4>return %a@]" (format_expression ctx) e1
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
    Format.fprintf fmt "@[<v 4>class %a(CatalaStruct):" StructName.format
      struct_name;
    Format.fprintf fmt "@,__slots__ = (@[<hov>%a@])"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
         (fun ppf (fld, _ty) ->
           Format.fprintf ppf "'%a'" StructField.format fld))
      fields;
    List.iter
      (fun (struct_field, struct_field_type) ->
        Format.fprintf fmt "@,%a: %a" StructField.format struct_field
          (format_typ ctx) struct_field_type)
      fields;
    Format.fprintf fmt "@,name = '%a'" StructName.format_original struct_name;
    Format.fprintf fmt "@,@[<v 4>fields = {";
    List.iter
      (fun (struct_field, struct_field_type) ->
        Format.fprintf fmt "@,'%a': '%a', # content @[<h>%a@]"
          StructField.format struct_field StructField.format_original
          struct_field (format_typ ctx) struct_field_type)
      fields;
    Format.fprintf fmt "@]@,}@]"
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if EnumConstructor.Map.is_empty enum_cons then
      failwith "no constructors in the enum";
    Format.fprintf fmt "@[<v 4>class %a(CatalaEnum):@,name = '%a'@,"
      EnumName.format enum_name EnumName.format_original enum_name;
    Format.fprintf fmt "@[<v 4>class Code(CatalaEnum.Code):@,%a@]"
      (Format.pp_print_list (fun fmt (enum_cons, enum_cons_type) ->
           Format.fprintf fmt "%a = '%a'" EnumConstructor.format enum_cons
             EnumConstructor.format_original enum_cons;
           match enum_cons_type with
           | TLit TUnit, _ -> ()
           | ty -> Format.fprintf fmt " # content @[<h>%a@]" (format_typ ctx) ty))
      (EnumConstructor.Map.bindings enum_cons);
    Format.fprintf fmt "@]"
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
          Format.fprintf fmt
            "class %a(Value):@,pass # Define your external type here@,"
            AbstractType.format t)
    (type_ordering @ scope_structs)

let format_code_item ctx fmt = function
  | SVar { var; expr; typ = _; visibility = _ } ->
    Format.fprintf fmt "@[<hv 4>%a = (@,%a@;<0 -4>)@]@," VarName.format var
      (format_expression ctx) expr
  | SFunc { var; func; visibility = _ }
  | SScope { scope_body_var = var; scope_body_func = func; _ } ->
    Format.fprintf fmt "@[<v 4>@[<hov 2>def %a(@,%a@;<0 -2>) -> %a:@]@ %a@]@,"
      format_func_name var
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (var, typ) ->
           Format.fprintf fmt "%a:%a" VarName.format (Mark.remove var)
             (format_typ ctx) typ))
      func.func_params (format_typ ctx) func.func_return_typ (format_block ctx)
      func.func_body

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
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           (fun ppf (s, _, _) -> ScopeName.format ppf s))
        tests
    in
    Format.fprintf ppf "@,# Automatic Catala tests@,";
    Format.fprintf ppf "@[<v 2>if __name__ == \"__main__\":";
    List.iter
      (fun (name, var, block) ->
        Format.pp_print_cut ppf ();
        (* Format.fprintf ppf "@,print(\"Executing scope %a...\")@," ScopeName.format
         *   name; *)
        format_block ctx ppf block;
        Format.fprintf ppf
          "@,\
           print(\"\\x1b[32m[RESULT]\\x1b[m Scope %a executed successfully.\", \
           file=stderr)"
          ScopeName.format_original name;
        Format.fprintf ppf "@,print(%a)" VarName.format var)
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
        "from sys import stderr";
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
