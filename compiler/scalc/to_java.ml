(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
   Vincent Botbol <vincent.botbol@inria.fr>

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
open Format

let pp_comma ppf () = fprintf ppf ",@ "
let pp_print_double_space ppf () = fprintf ppf "@ @ "

let pp_print_list_padded ?pp_sep pp ppf l =
  if l = [] then ()
  else (
    pp_print_double_space ppf ();
    (pp_print_list ?pp_sep pp) ppf l)

type context = {
  decl_ctx : decl_ctx;
  in_scope_structs : StructName.Set.t;
  out_scope_structs : StructName.Set.t;
  scope_func_names : ScopeName.t FuncName.Map.t;
  in_globals : bool;
  global_funcs : FuncName.Set.t;
  global_vars : VarName.Set.t;
  external_global_funcs : String.Set.t;
  external_global_vars : String.Set.t;
  external_scopes : string String.Map.t;
}

let format_string_list (ppf : formatter) (uids : string list) : unit =
  fprintf ppf "new String[]{%a}"
    (pp_print_list ~pp_sep:pp_comma pp_print_string)
    (List.map String.quote uids)

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
    (* Reserved for generation *)
    "Globals";
  ]
(* todo: reserved names should also include built-in types and everything
   exposed by the runtime. *)

let renaming =
  Renaming.program () ~reserved:java_keywords ~skip_constant_binders:false
    ~constant_binder_name:None ~namespaced_fields:true ~namespaced_constrs:true
    ~prefix_module:false
    ~f_var:(String.to_camel_case ~capitalize:false)
    ~f_struct:String.to_camel_case ~f_enum:String.to_camel_case

let format_qualified
    (type id)
    (module Id : Uid.Qualified with type t = id)
    ppf
    (s : id) =
  match List.rev (Id.path s) with
  | [] -> pp_print_string ppf (Id.base s)
  | m :: _ -> fprintf ppf "%a.%s" ModuleName.format m (Id.base s)

let format_struct = format_qualified (module StructName)
let format_enum = format_qualified (module EnumName)
let format_scope = format_qualified (module ScopeName)

let format_op (ppf : formatter) (op : operator Mark.pos) : unit =
  match Mark.remove op with
  | Log (_entry, _infos) -> assert false
  | Minus_int | Minus_rat | Minus_mon | Minus_dur ->
    pp_print_string ppf "subtract"
  | Not -> pp_print_string ppf "not"
  | Length -> pp_print_string ppf "length"
  | ToRat_int | ToRat_mon -> pp_print_string ppf "asDecimal"
  | ToInt_rat -> pp_print_string ppf "asInteger"
  | ToMoney_rat -> pp_print_string ppf "asMoney"
  | GetDay -> pp_print_string ppf "getDay"
  | GetMonth -> pp_print_string ppf "getMonth"
  | GetYear -> pp_print_string ppf "getYear"
  | FirstDayOfMonth -> pp_print_string ppf "getFirstDayOfMonth"
  | LastDayOfMonth -> pp_print_string ppf "getLastDayOfMonth"
  | Round_mon -> pp_print_string ppf "round"
  | Round_rat -> pp_print_string ppf "round"
  | Concat -> pp_print_string ppf "append"
  | Add_rat_rat | Add_mon_mon | Add_dur_dur | Add_int_int ->
    pp_print_string ppf "add"
  | Add_dat_dur RoundUp -> fprintf ppf "addDurationRoundUp"
  | Add_dat_dur RoundDown -> fprintf ppf "addDurationRoundDown"
  | Add_dat_dur AbortOnRound -> fprintf ppf "addDurationAbortOnRound"
  | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dur_dur ->
    pp_print_string ppf "subtract"
  | Sub_dat_dur RoundUp -> fprintf ppf "subDurationRoundUp"
  | Sub_dat_dur RoundDown -> fprintf ppf "subDurationRoundDown"
  | Sub_dat_dur AbortOnRound -> fprintf ppf "subDurationAbortOnRound"
  | Mult_int_int | Mult_rat_rat | Mult_mon_int | Mult_mon_rat | Mult_dur_int ->
    pp_print_string ppf "multiply"
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int | Div_mon_rat
  | Div_dur_dur ->
    pp_print_string ppf "divide"
  | And -> pp_print_string ppf "and"
  | Or -> pp_print_string ppf "or"
  | Eq | Eq_boo_boo | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dat_dat
  | Eq_dur_dur ->
    pp_print_string ppf "equalsTo"
  | Xor -> pp_print_string ppf "xor"
  | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur ->
    pp_print_string ppf "lessThan"
  | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat | Lte_dur_dur ->
    pp_print_string ppf "lessEqThan"
  | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur ->
    pp_print_string ppf "greaterThan"
  | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat | Gte_dur_dur ->
    pp_print_string ppf "greaterEqThan"
  | Map -> pp_print_string ppf "map"
  | Map2 -> pp_print_string ppf "map2"
  | Reduce -> pp_print_string ppf "reduce"
  | Filter -> pp_print_string ppf "filter"
  | Fold -> pp_print_string ppf "foldLeft"
  | HandleExceptions -> pp_print_string ppf "CatalaConflict.handleExceptions"
  | FromClosureEnv | ToClosureEnv -> failwith "unimplemented"

let format_visibility ppf = function
  | Private -> () (* nothing => package visibility *)
  | Public -> fprintf ppf "public "

let rec format_typ ctx ppf (typ : typ) =
  let typ = Type.unquantify typ in
  match Mark.remove typ with
  | TLit TBool -> fprintf ppf "CatalaBool"
  | TLit TUnit -> fprintf ppf "CatalaUnit"
  | TLit TInt -> fprintf ppf "CatalaInteger"
  | TLit TRat -> fprintf ppf "CatalaDecimal"
  | TLit TMoney -> fprintf ppf "CatalaMoney"
  | TLit TDate -> fprintf ppf "CatalaDate"
  | TLit TDuration -> fprintf ppf "CatalaDuration"
  | TLit TPos -> fprintf ppf "CatalaPosition"
  | TArrow ([ty], ret_ty) ->
    fprintf ppf "CatalaFunction<%a,%a>" (format_typ ctx) ty (format_typ ctx)
      ret_ty
  | TArrow (_args_ty, ret_ty) ->
    fprintf ppf "CatalaFunction<CatalaTuple,%a>" (format_typ ctx) ret_ty
  | TTuple _ -> fprintf ppf "CatalaTuple"
  | TStruct sname when sname == Expr.source_pos_struct ->
    pp_print_string ppf "CatalaPosition"
  | TStruct sname -> format_struct ppf sname
  | TEnum ename -> format_enum ppf ename
  | TOption typ -> fprintf ppf "CatalaOption<%a>" (format_typ ctx) typ
  | TArray typ -> fprintf ppf "CatalaArray<%a>" (format_typ ctx) typ
  | TDefault typ -> (format_typ ctx) ppf typ
  | TVar _ -> fprintf ppf "CatalaValue"
  | TForAll _ -> assert false
  | TClosureEnv -> assert false

let format_struct_params ctx ppf (fields : typ StructField.Map.t) =
  let fields = StructField.Map.bindings fields in
  pp_print_list ~pp_sep:pp_comma
    (fun ppf (sfield, typ) ->
      fprintf ppf "final %a %a" (format_typ ctx) typ StructField.format sfield)
    ppf fields

let rec format_lit (ppf : formatter) (l : lit Mark.pos) : unit =
  match Mark.remove l with
  | LBool true -> pp_print_string ppf "CatalaBool.TRUE"
  | LBool false -> pp_print_string ppf "CatalaBool.FALSE"
  | LInt i ->
    fprintf ppf "new CatalaInteger(\"%s\")" (Runtime.integer_to_string i)
  | LUnit -> pp_print_string ppf "null"
  | LRat i ->
    if Q.den i = Z.one then
      fprintf ppf "CatalaDecimal.ofInteger(%a)" format_lit
        (Mark.copy l (LInt (Q.num i)))
    else
      fprintf ppf "new CatalaDecimal(%a, %a)" format_lit
        (Mark.copy l (LInt (Q.num i)))
        format_lit
        (Mark.copy l (LInt (Q.den i)))
  | LMoney e ->
    fprintf ppf "CatalaMoney.ofCents(\"%s\")"
      (Runtime.integer_to_string (Runtime.money_to_cents e))
  | LDate d ->
    fprintf ppf "CatalaDate.of(%d,%d,%d)"
      (Runtime.integer_to_int (Runtime.year_of_date d))
      (Runtime.integer_to_int (Runtime.month_number_of_date d))
      (Runtime.integer_to_int (Runtime.day_of_month_of_date d))
  | LDuration d ->
    let years, months, days = Runtime.duration_to_years_months_days d in
    fprintf ppf "CatalaDuration.of(%d,%d,%d)" years months days

let get_list_and_args_expr (op : Ast.operator Mark.pos) args =
  match Mark.remove op, args with
  | (Filter | Map), [f; l] -> l, [f]
  | (Fold | Reduce), [f; f_dft; l] -> l, [f; f_dft]
  | Map2, [pos; f; l1; l2] -> l1, [pos; f; l2]
  | Concat, [l1; l2] -> l1, [l2]
  | _ -> assert false

let fill_struct_bindings
    (ctx : context)
    struct_name
    (given : expr StructField.Map.t) =
  let expected : expr StructField.Map.t =
    StructName.Map.find struct_name ctx.decl_ctx.ctx_structs
    |> StructField.Map.map (fun _ ->
           ( EInj
               {
                 name = Expr.option_enum;
                 cons = Expr.none_constr;
                 e1 = ELit LUnit, Pos.void;
                 expr_typ = TOption (Type.any Pos.void), Pos.void;
               },
             Pos.void ))
  in
  StructField.Map.(
    merge
      (fun _f e g ->
        match e, g with
        | None, None -> assert false
        | Some l, None | _, Some l -> Some l)
      expected given
    |> bindings)

let poly_cast ctx ppf e fmt =
  match Mark.remove e with
  | EApp { poly = true; typ; _ } ->
    fprintf ppf
      ("@[<hv 2>CatalaValue.<%a>cast@;<0 -1>(" ^^ fmt ^^ ")@]")
      (format_typ ctx) typ
  | _ -> fprintf ppf fmt

let rec format_expression ctx (ppf : formatter) (e : expr) : unit =
  let {
    in_scope_structs;
    out_scope_structs;
    scope_func_names;
    global_vars;
    global_funcs;
    in_globals;
    _;
  } =
    ctx
  in
  match Mark.remove e with
  | EVar v ->
    if VarName.Set.mem v global_vars && not in_globals then
      fprintf ppf "Globals.";
    VarName.format ppf v
  | EFunc f ->
    if FuncName.Set.mem f global_funcs && not in_globals then
      fprintf ppf "Globals.";
    FuncName.format ppf f
  | EStruct { name = s; fields } when s == Expr.source_pos_struct ->
    fprintf ppf "new CatalaPosition(%a)"
      (pp_print_list ~pp_sep:pp_comma (fun ppf (_struct_field, e) ->
           fprintf ppf "%a" (format_expression ctx) e))
      (StructField.Map.bindings fields)
  | EStruct { fields = es; name = s } ->
    if StructName.Set.mem s in_scope_structs then begin
      (pp_print_list ~pp_sep:pp_comma (fun ppf (_struct_field, e) ->
           format_expression ctx ppf e))
        ppf
        (fill_struct_bindings ctx s es)
    end
    else if StructName.Set.mem s out_scope_structs then begin
      fprintf ppf "new %a(new %a.%sOut(%a))" format_struct s format_struct s
        (StructName.base s)
        (pp_print_list ~pp_sep:pp_comma (fun ppf (_struct_field, e) ->
             fprintf ppf "%a" (format_expression ctx) e))
        (StructField.Map.bindings es)
    end
    else
      fprintf ppf "new %a(%a)" format_struct s
        (pp_print_list ~pp_sep:pp_comma (fun ppf (_struct_field, e) ->
             fprintf ppf "%a" (format_expression ctx) e))
        (fill_struct_bindings ctx s es)
  | EStructFieldAccess { name; field; _ }
    when StructName.Set.mem name in_scope_structs ->
    StructField.format ppf field
  | EStructFieldAccess { e1 = (EVar _, _) as e1; field; _ } ->
    fprintf ppf "%a.%a" (format_expression ctx) e1 StructField.format field
  | EStructFieldAccess { e1; field; _ } ->
    fprintf ppf "(%a).%a" (format_expression ctx) e1 StructField.format field
  | EInj { cons; name = e_name; _ }
    when EnumName.equal e_name Expr.option_enum
         && EnumConstructor.equal cons Expr.none_constr ->
    fprintf ppf "CatalaOption.none()"
  | EInj { e1 = e; cons; name = e_name; _ }
    when EnumName.equal e_name Expr.option_enum
         && EnumConstructor.equal cons Expr.some_constr ->
    fprintf ppf "@[<hv 2>CatalaOption.some@;<0 -1>(%a)@]"
      (format_expression ctx) e
  | EInj { e1 = ELit LUnit, _; cons; name = enum_name; _ } ->
    fprintf ppf "%a.make%a()" format_enum enum_name EnumConstructor.format cons
  | EInj { e1 = e; cons; name = enum_name; _ } ->
    fprintf ppf "%a.make%a(%a)" format_enum enum_name EnumConstructor.format
      cons (format_expression ctx) e
  | EArray es ->
    fprintf ppf "@[<hv 2>new CatalaArray<>@;<0 -1>(%a)@]"
      (pp_print_list ~pp_sep:pp_comma (fun ppf e ->
           fprintf ppf "%a" (format_expression ctx) e))
      es
  | ELit l -> fprintf ppf "%a" format_lit (Mark.copy e l)
  | EPosLit ->
    let pos = Mark.get e in
    fprintf ppf
      "@[<hv 2>new CatalaPosition@;\
       <0 -1>(@[<hov>\"%s\",@ %d, %d,@ %d, %d,@ %a@])@]" (Pos.get_file pos)
      (Pos.get_start_line pos) (Pos.get_start_column pos) (Pos.get_end_line pos)
      (Pos.get_end_column pos) format_string_list (Pos.get_law_info pos)
  | EAppOp { op = (HandleExceptions, _) as op; args = [(EArray exprs, _)]; _ }
    ->
    fprintf ppf "@[<hv 2>%a@;<0 -1>(new CatalaArray<>(@ %a@ )@])" format_op op
      (pp_print_list ~pp_sep:pp_comma (fun ppf e -> format_expression ctx ppf e))
      exprs
  | EAppOp { op = Concat, _; args = [(EArray [], _); e2]; _ } ->
    (* Do not append to empty list *)
    format_expression ctx ppf e2
  | EAppOp
      {
        op = ((Map | Filter | Reduce | Fold | Map2 | Concat), _) as op;
        args;
        _;
      } ->
    let l, args = get_list_and_args_expr op args in
    fprintf ppf "@[<hv 2>%a.%a@;<0 -1>(%a@])"
      (format_expression_with_paren ctx)
      l format_op op
      (pp_print_list ~pp_sep:pp_comma (fun ppf e -> format_expression ctx ppf e))
      args
  | EAppOp { op; args = [arg1; arg2]; _ } ->
    fprintf ppf "@[<hv 2>%a.%a@;<0 -1>(%a@])"
      (format_expression_with_paren ctx)
      arg1 format_op op (format_expression ctx) arg2
  | EAppOp { op = Log _, _; _ } when Global.options.trace <> None ->
    Message.error "tracing is not yet supported in Java"
  | EAppOp { op = Log _, _; args = [arg1]; _ } ->
    fprintf ppf "%a" (format_expression ctx) arg1
  | EAppOp { op = (Not, _) as op; args = [arg1]; _ } ->
    fprintf ppf "%a.%a()" (format_expression ctx) arg1 format_op op
  | EAppOp
      {
        op = (Minus_int | Minus_rat | Minus_mon | Minus_dur), _;
        args = [arg1];
        _;
      } ->
    fprintf ppf "%a.negate()" (format_expression_with_paren ctx) arg1
  | EAppOp { op; args = [arg1]; _ } ->
    fprintf ppf "%a.%a()" (format_expression_with_paren ctx) arg1 format_op op
  | EApp { f = EFunc fname, _; args; _ }
    when FuncName.Set.mem fname global_funcs ->
    poly_cast ctx ppf e "@[<hv 2>%s%a.apply@;<0 -1>(%a)@]"
      (if in_globals then "" else "Globals.")
      FuncName.format fname
      (format_currified_args ctx)
      args
  | EApp { f = EExternal { modname; name }, _; args; _ }
    when String.Set.mem (Mark.remove name) ctx.external_global_funcs ->
    poly_cast ctx ppf e "@[<hv 2>%a.Globals.%s.apply@;<0 -1>(%a)@]"
      VarName.format (Mark.remove modname) (Mark.remove name)
      (format_currified_args ctx)
      args
  | EApp { f = EFunc fname, _; args; _ }
    when FuncName.Map.mem fname scope_func_names ->
    fprintf ppf "@[<hv 2>new %a@;<0 -1>(%a)@]" format_scope
      (FuncName.Map.find fname scope_func_names)
      (pp_print_list ~pp_sep:pp_comma (format_expression ctx))
      args
  | EApp { f = EExternal { modname; name }, _; args; _ }
    when String.Map.mem (Mark.remove name) ctx.external_scopes ->
    let scope_name = String.Map.find (Mark.remove name) ctx.external_scopes in
    fprintf ppf "@[<hv 0>new %a.%s@;<0 -1>(%a)@]" VarName.format
      (Mark.remove modname) scope_name
      (pp_print_list ~pp_sep:pp_comma (format_expression ctx))
      args
  | EApp { f; args; _ } ->
    poly_cast ctx ppf e "@[<hv 0>%a.apply(%a)@]" (format_expression ctx) f
      (format_currified_args ctx)
      args
  | EAppOp { args = []; _ } -> assert false
  | EAppOp { op; args = arg_pos :: arg1 :: args; tys = (TLit TPos, _) :: _ } ->
    fprintf ppf "@[<hv 2>%a.%a@;<0 -1>(%a)@]"
      (format_expression_with_paren ctx)
      arg1 format_op op
      (pp_print_list ~pp_sep:pp_comma (format_expression ctx))
      (arg_pos :: args)
  | EAppOp { op; args = arg1 :: args; _ } ->
    fprintf ppf "@[<hv 2>%a.%a@;<0 -1>(%a)@]" (format_expression ctx) arg1
      format_op op
      (pp_print_list ~pp_sep:pp_comma (format_expression ctx))
      args
  | ETuple es ->
    fprintf ppf "@[<hv 2>new CatalaTuple@;<0 -1>(%a)@]"
      (pp_print_list ~pp_sep:pp_comma (fun ppf e ->
           fprintf ppf "%a" (format_expression ctx) e))
      es
  | ETupleAccess { e1; index; typ } ->
    fprintf ppf "CatalaValue.<%a>cast@;<0 -1>(%a.get(%d))" (format_typ ctx) typ
      (format_expression_with_paren ctx)
      e1 index
  | EExternal { modname; name }
    when String.Set.mem (Mark.remove name) ctx.external_global_vars ->
    fprintf ppf "%a.Globals.%s" VarName.format (Mark.remove modname)
      (Mark.remove name)
  | EExternal { modname; name } ->
    fprintf ppf "%a.Globals.%s" VarName.format (Mark.remove modname)
      (Mark.remove name)

and format_expression_with_paren ctx (ppf : formatter) (e : expr) : unit =
  match Mark.remove e with
  | EAppOp _ | EInj _ | ETupleAccess _ | EStructFieldAccess _ | EFunc _ | EVar _
    ->
    format_expression ctx ppf e
  | EExternal _ | EPosLit | EApp _ | ELit _ | EArray _ | ETuple _ | EStruct _ ->
    fprintf ppf "(%a)" (format_expression ctx) e

and format_currified_args ctx ppf = function
  | [] -> fprintf ppf "CatalaUnit.INSTANCE"
  | [arg] -> (format_expression ctx) ppf arg
  | args ->
    fprintf ppf "@[<hov 4>new CatalaTuple@;<0 -1>(%a)@]"
      (pp_print_list ~pp_sep:pp_comma (format_expression ctx))
      args

let rec format_stmt ~toplevel (ctx : context) ppf (stmt : Ast.stmt Mark.pos) =
  match Mark.remove stmt with
  | SLocalDecl { name; typ } ->
    fprintf ppf "@[<hov 4>final %a@ %a;@]" (format_typ ctx) typ VarName.format
      (Mark.remove name)
  | SLocalDef { name; expr; _ } ->
    fprintf ppf "@[<hov 4>%a = %a;@]" VarName.format (Mark.remove name)
      (format_expression ctx) expr
  | SReturn (EStruct { fields; _ }, _) when toplevel ->
    (* we are in a constructor: assign outputs *)
    let pp_self_assign ppf (field, expr) =
      fprintf ppf "@[<hov 4>this.%a = %a;@]" StructField.format field
        (format_expression ctx) expr
    in
    if not (StructField.Map.is_empty fields) then
      fprintf ppf "@[<v>%a@]"
        (pp_print_list ~pp_sep:pp_print_space pp_self_assign)
        (StructField.Map.bindings fields)
  | SReturn expr ->
    fprintf ppf "@[<hov 4>return %a;@]" (format_expression ctx) expr
  | SInnerFuncDef { name; func } ->
    fprintf ppf "%a;" (format_inner_func_def ctx) (name, func)
  | SLocalInit { name; typ; expr } ->
    fprintf ppf "@[<hov 4>%a %a =@ %a;@]" (format_typ ctx) typ VarName.format
      (Mark.remove name) (format_expression ctx) expr
  | SFatalError { pos_expr; error } ->
    fprintf ppf "throw new CatalaError(CatalaError.Error.%s, %a);"
      (Runtime.error_to_string error)
      (format_expression ctx) pos_expr
  | SIfThenElse { if_expr; then_block; else_block } ->
    format_if ppf
      ~cond_format:(fun ppf ->
        fprintf ppf "%a.asBoolean()" (format_expression_with_paren ctx) if_expr)
      ~cons_format:(fun ppf -> format_block ctx ppf then_block)
      ~alt_format:(fun ppf -> format_block ctx ppf else_block)
  | SSwitch
      {
        switch_var;
        switch_var_typ = TOption _, _;
        enum_name = _;
        switch_cases = [none; some];
      } ->
    let cond_format ppf = fprintf ppf "%a.isNone()" VarName.format switch_var in
    let cons_format ppf = format_block ctx ppf none.case_block in
    let alt_format ppf =
      fprintf ppf "%a %a = %a.get();@\n" (format_typ ctx) some.payload_var_typ
        VarName.format some.payload_var_name VarName.format switch_var;
      format_block ctx ppf some.case_block
    in
    format_if ppf ~cond_format ~cons_format ~alt_format
  | SSwitch { switch_var; switch_var_typ = _; enum_name; switch_cases } ->
    let format_init_case ppf (enum_cstr, var, (typ : typ)) =
      match Mark.remove typ with
      | TLit TUnit -> ()
      | _ ->
        let s = VarName.to_string var in
        if s = "" || (s.[0] >= '0' && s.[0] <= '9') then
          (* Do not generate invalid initializers: it might be the case when
             considering a wildcard var *)
          ()
        else
          fprintf ppf "%a %a = %a.get%aContents();@ " (format_typ ctx) typ
            VarName.format var VarName.format switch_var EnumConstructor.format
            enum_cstr
    in
    let format_switch_case
        ppf
        (enum_cstr, { case_block; payload_var_name; payload_var_typ }) =
      let format_break ppf =
        let has_return =
          Utils.find_block
            (function SReturn _, _ | SFatalError _, _ -> true | _ -> false)
            case_block
          |> Option.is_some
        in
        if not has_return then (
          pp_print_space ppf ();
          fprintf ppf "break;")
      in
      fprintf ppf "@[<v 4>case %a: {@ %a%a%t@;<1 -4>}@]" EnumConstructor.format
        enum_cstr format_init_case
        (enum_cstr, payload_var_name, payload_var_typ)
        (format_block ctx) case_block format_break
    in
    let enum_cstrs =
      EnumName.Map.find enum_name ctx.decl_ctx.ctx_enums
      |> EnumConstructor.Map.keys
    in
    let pp_default_initializer ppf =
      fprintf ppf
        "@\n\
         @[<v 4>default: {@ throw new RuntimeException(\"Unreachable case\");@;\
         <1 -4>}@]"
    in
    fprintf ppf "@[<v 4>switch (%a.kind) {@ %a%t@;<1 -4>}@]" VarName.format
      switch_var
      (pp_print_list ~pp_sep:pp_print_space format_switch_case)
      (List.combine enum_cstrs switch_cases)
      pp_default_initializer
  | SAssert { expr; pos_expr } ->
    fprintf ppf "@[<hov 4>CatalaAssertion.check(%a, %a);@]"
      (format_expression_with_paren ctx)
      pos_expr
      (format_expression_with_paren ctx)
      expr
  | SSpecialOp _ -> .

and format_inner_func_def ctx ppf (name, func) =
  fprintf ppf "@[<hov 4>%a = %a@]" VarName.format (Mark.remove name)
    (format_inner_func_body ctx)
    func

and format_inner_func_body ctx ppf =
  let format_var ppf v =
    if VarName.to_string v = "" then (* wildcard *) fprintf ppf "unit"
    else VarName.format ppf v
  in
  function
  | { func_params = []; func_body; _ }
  | { func_params = [(_, (TLit TUnit, _))]; func_body; _ } ->
    fprintf ppf "unit -> {@ %a }" (format_block ctx) func_body
  | { func_params = [(pname, _)]; func_body; _ } ->
    fprintf ppf "%a -> {@ %a }" format_var (Mark.remove pname)
      (format_block ctx) func_body
  | { func_params = _ :: _ :: _ as params; func_body; _ } ->
    let args_name =
      VarName.fresh ("tup_arg", Pos.void)
      |> fun x ->
      VarName.map_info (fun (s, p) -> sprintf "%s_%d" s (VarName.id x), p) x
    in
    let init_params =
      List.mapi
        (fun index (name, typ) ->
          let expr =
            ETupleAccess { e1 = EVar args_name, Pos.void; index; typ }, Pos.void
          in
          SLocalInit { name; typ; expr }, Pos.void)
        params
    in
    fprintf ppf "%a -> {@ %a@\n%a }" format_var args_name (format_block ctx)
      init_params (format_block ctx) func_body

and format_block ?(toplevel = false) ctx ppf (block : Ast.block) =
  let rec format_stmts ~toplevel = function
    | [] -> ()
    | [stmt] -> format_stmt ~toplevel ctx ppf stmt
    | (SLocalDecl { name = n, _; typ }, _)
      :: (SLocalDef { name = n2, _; expr; _ }, _)
      :: r
      when VarName.equal n n2 ->
      fprintf ppf "@[<hov 4>final %a@ %a = %a;@]" (format_typ ctx) typ
        VarName.format n (format_expression ctx) expr;
      pp_print_space ppf ();
      format_stmts ~toplevel r
    | (SLocalDecl { name = (n, _) as name; typ }, _)
      :: (SInnerFuncDef { name = n2, _; func }, _)
      :: r
      when VarName.equal n n2 ->
      fprintf ppf "@[<hov 4>final %a@ %a;@]" (format_typ ctx) typ
        (format_inner_func_def ctx)
        (name, func);
      pp_print_space ppf ();
      format_stmts ~toplevel r
    | ((SFatalError _, _) as stmt) :: _ -> format_stmt ~toplevel ctx ppf stmt
    | stmt :: r ->
      format_stmt ~toplevel ctx ppf stmt;
      pp_print_space ppf ();
      format_stmts ~toplevel r
  in
  pp_open_vbox ppf 0;
  format_stmts ~toplevel block;
  pp_close_box ppf ()

and format_if
    ~(cond_format : formatter -> unit)
    ~(cons_format : formatter -> unit)
    ~(alt_format : formatter -> unit)
    ppf =
  fprintf ppf "@[<v 4>if (%t) {@ %t@;<1 -4>} else {@ %t@;<1 -4>}@]" cond_format
    cons_format alt_format

let format_constructor_body (ctx : context) sbody ppf =
  format_block ~toplevel:true ctx ppf sbody.scope_body_func.func_body

let format_constructor (ctx : context) (sbody : scope_body) ppf =
  let in_struct_name =
    match sbody.scope_body_func.func_params with
    | [(_vname, (TStruct sn, _))] -> sn
    | _ -> assert false
  in
  StructName.Map.find_opt in_struct_name ctx.decl_ctx.ctx_structs
  |> function
  | None -> ()
  | Some in_fields ->
    fprintf ppf "@[<v 4>@[<hov 4>%a%a@ (@[<hov>%a@])@;<1 -4>{@]@,%t@;<1 -4>}@]"
      format_visibility sbody.scope_body_visibility format_scope
      sbody.scope_body_name (format_struct_params ctx) in_fields
      (format_constructor_body ctx sbody)

let format_output_parameter ?(vis = Public) ctx ppf (field_name, typ) =
  fprintf ppf "@[<h>%afinal@ %a@ %a;@]" format_visibility vis (format_typ ctx)
    typ StructField.format field_name

let format_scope_output_parameters
    (ctx : context)
    (sbody : scope_body)
    ppf
    fields =
  fprintf ppf "@[<v>%a@]@\n"
    (pp_print_list ~pp_sep:pp_print_space
       (format_output_parameter ~vis:sbody.scope_body_visibility ctx))
    fields

let format_comparison class_name pp_fields_comparison ppf =
  fprintf ppf
    "%@Override@\n\
     @[<hov2>public CatalaBool equalsTo(CatalaValue other) {@\n\
     @[<v 4>if (other instanceof %s v) {@\n\
     %t@]@\n\
     } else { return CatalaBool.FALSE; } @]@\n\
     }"
    class_name pp_fields_comparison

let format_struct_constructor_body ppf fields =
  let fields = StructField.Map.bindings fields in
  fprintf ppf "@[<v>%a@]"
    (pp_print_list ~pp_sep:pp_print_space (fun ppf (sfield, _typ) ->
         fprintf ppf "this.%a = %a;" StructField.format sfield
           StructField.format sfield))
    fields

let format_struct_constructor ?(vis = Public) ctx ppf (sname, fields) =
  if StructField.Map.is_empty fields then ()
  else
    fprintf ppf "@[<hov 4>%a%a (@[<hov>%a@]) {@\n%a@]@\n}" format_visibility vis
      format_struct sname (format_struct_params ctx) fields
      format_struct_constructor_body fields

let format_scope_out_struct_constructor
    ?(vis = Public)
    ctx
    ppf
    (scope_name, fields) =
  if StructField.Map.is_empty fields then ()
  else
    let format_constr ppf fields =
      fprintf ppf "@[<hov 4>%a%aOut (@[<hov>%a@]) {@\n%a@]@\n}"
        format_visibility vis format_scope scope_name (format_struct_params ctx)
        fields format_struct_constructor_body fields
    in
    fprintf ppf "@[<hov 4>%astatic class %aOut {@\n%a@\n%a@]@\n}@\n@\n"
      format_visibility vis format_scope scope_name
      (pp_print_list ~pp_sep:pp_print_space (format_output_parameter ~vis ctx))
      (StructField.Map.bindings fields)
      format_constr fields;
    let format_scope_out_constructor_body ppf fields =
      let fields = StructField.Map.bindings fields in
      fprintf ppf "@[<v>%a@]"
        (pp_print_list ~pp_sep:pp_print_space (fun ppf (sfield, _typ) ->
             fprintf ppf "this.%a = result.%a;" StructField.format sfield
               StructField.format sfield))
        fields
    in
    fprintf ppf "@[<hov 4>%a%a (%aOut result) {@\n%a@]@\n}" format_visibility
      vis format_scope scope_name format_scope scope_name
      format_scope_out_constructor_body fields

let format_fields_comparison ppf (fields : string list) =
  let format_field_comparison ppf field =
    fprintf ppf "this.%s.equalsTo(v.%s)" field field
  in
  let rec pp_conjunction ppf = function
    | [] -> fprintf ppf "CatalaBool.TRUE"
    | [h] -> format_field_comparison ppf h
    | h :: t ->
      fprintf ppf "%a.and@;<0 -1>(%a)" format_field_comparison h pp_conjunction
        t
  in
  fprintf ppf "return @[<hov 4>%a;@]" pp_conjunction fields

let format_struct_to_string ppf fields =
  fprintf ppf
    "%@Override@\n@[<v 4>public String toString() {@\nreturn %a;@]@\n}"
    (fun ppf -> function
      | [] -> fprintf ppf "\"\""
      | fields ->
        (pp_print_list
           ~pp_sep:(fun ppf () -> fprintf ppf " + \", \"@ + ")
           (fun ppf field_s ->
             fprintf ppf "\"%s = \" + this.%s.toString()" field_s field_s))
          ppf fields)
    fields

let format_enum_to_string ppf =
  fprintf ppf
    "%@Override@\n\
     @[<v 4>public String toString() {@\n\
     return this.kind.toString() + \" \" + this.contents.toString();@]@\n\
     }"

let format_tests ctx ppf (closures, tests) =
  assert (closures = []);
  pp_print_double_space ppf ();
  fprintf ppf "// Automatic Catala tests@\n";
  fprintf ppf "@[<v 4>public static void main(String[] args) {@\n";
  (if tests = [] then
     Message.warning
       "%a@{<magenta>#[test]@}@ attribute@ above@ their@ declaration."
       Format.pp_print_text
       "No test scope were found: the generated executable won't test any \
        computation. To mark scopes as tests, ensure they don't require \
        inputs, and add the "
   else
     let () =
       Message.debug "@[<hov 2>Generating entry points for scopes:@ %a@]@."
         (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (s, _) ->
              ScopeName.format ppf s))
         tests
     in
     let format_test ppf (scope_name, block) =
       fprintf ppf "{ /* Test for scope %a */@\n" ScopeName.format scope_name;
       fprintf ppf
         "%a@\n\
          System.out.println(\"\\u001B[32m[RESULT]\\u001B[0m Scope %a executed \
          successfully.\"); }"
         (format_block ~toplevel:true ctx)
         block ScopeName.format scope_name
     in
     pp_print_list ~pp_sep:pp_print_space format_test ppf tests);
  fprintf ppf "@]@\n}"

let format_scope ctx ppf (sbody : Ast.scope_body) =
  let out_struct =
    match sbody.scope_body_func.func_return_typ with
    | TStruct name, _ -> StructName.Map.find name ctx.decl_ctx.ctx_structs
    | _ -> assert false
  in
  let pp_out_struct ppf =
    format_scope_out_struct_constructor ctx ~vis:sbody.scope_body_visibility ppf
      (sbody.scope_body_name, out_struct)
  in
  let out_struct_name =
    match sbody.scope_body_func.func_return_typ with
    | TStruct sn, _ -> sn
    | _ -> assert false
  in
  let out_fields =
    StructName.Map.find_opt out_struct_name ctx.decl_ctx.ctx_structs
    |> function
    | None -> [] | Some out_fields -> StructField.Map.bindings out_fields
  in
  let fields_l = List.map fst out_fields |> List.map StructField.to_string in
  let format_fields_comparison ppf = format_fields_comparison ppf fields_l in
  fprintf ppf
    "@[<v 4>@[<hov 4>public static class %a@ implements CatalaValue {@]@ @ %a@ \
     %t@ @ %t@ @ %t@ @ %a@]@\n\
     }"
    format_scope sbody.scope_body_name
    (format_scope_output_parameters ctx sbody)
    out_fields
    (format_constructor ctx sbody)
    pp_out_struct
    (format_comparison
       (ScopeName.to_string sbody.scope_body_name)
       format_fields_comparison)
    format_struct_to_string fields_l

let gather_externals ctx =
  let external_global_funcs, external_global_vars =
    TopdefName.Map.fold
      (fun topdef_name (typ, vis) ((efuncs, evars) as acc) ->
        if TopdefName.path topdef_name = [] || vis <> Public then acc
        else
          let v = TopdefName.base topdef_name in
          match typ with
          | TArrow _, _ -> String.Set.add v efuncs, evars
          | _ -> efuncs, String.Set.add v evars)
      ctx.decl_ctx.ctx_topdefs
      (String.Set.empty, String.Set.empty)
  in
  let external_scopes, external_scopes_in, external_scopes_out =
    ScopeName.Map.fold
      (fun sname v ((s_acc, in_acc, out_acc) as acc) ->
        if ScopeName.path sname = [] then acc
        else
          ( String.Map.add (ScopeName.base sname)
              (StructName.base v.out_struct_name)
              s_acc,
            StructName.Set.add v.in_struct_name in_acc,
            StructName.Set.add v.out_struct_name out_acc ))
      ctx.decl_ctx.ctx_scopes
      (String.Map.empty, StructName.Set.empty, StructName.Set.empty)
  in
  let in_scope_structs =
    StructName.Set.union external_scopes_in ctx.in_scope_structs
  in
  let out_scope_structs =
    StructName.Set.union external_scopes_out ctx.out_scope_structs
  in
  {
    ctx with
    external_scopes;
    in_scope_structs;
    out_scope_structs;
    external_global_vars;
    external_global_funcs;
  }

let populate_context (p : Ast.program) : context =
  let ctx =
    {
      decl_ctx = p.ctx.decl_ctx;
      in_scope_structs = StructName.Set.empty;
      out_scope_structs = StructName.Set.empty;
      scope_func_names = FuncName.Map.empty;
      in_globals = false;
      global_funcs = FuncName.Set.empty;
      global_vars = VarName.Set.empty;
      external_global_funcs = String.Set.empty;
      external_global_vars = String.Set.empty;
      external_scopes = String.Map.empty;
    }
  in
  let ctx = gather_externals ctx in
  let in_scope_structs, scope_structs =
    List.fold_left
      (fun ((in_s, out_s) as acc) -> function
        | SScope
            {
              scope_body_func =
                {
                  func_params = [(_, (TStruct in_sname, _))];
                  func_return_typ = TStruct out_sname, _;
                  _;
                };
              _;
            } ->
          StructName.Set.add in_sname in_s, StructName.Set.add out_sname out_s
        | _ -> acc)
      (StructName.Set.empty, StructName.Set.empty)
      p.code_items
  in
  {
    ctx with
    in_scope_structs =
      StructName.Set.union ctx.in_scope_structs in_scope_structs;
    out_scope_structs = StructName.Set.union ctx.out_scope_structs scope_structs;
  }

let format_structs ctx ppf =
  let format_struct ppf (sname, fields) =
    let fields_l = StructField.Map.bindings fields in
    let format_params ppf =
      let format_output_parameter ppf (field_name, typ) =
        fprintf ppf "@[<h>public final@ %a@ %a;@]" (format_typ ctx) typ
          StructField.format field_name
      in
      fprintf ppf "@[<v>%a@]"
        (pp_print_list ~pp_sep:pp_print_space format_output_parameter)
        fields_l
    in
    let fields_l = List.map fst fields_l |> List.map StructField.to_string in
    let format_fields_comparison ppf = format_fields_comparison ppf fields_l in
    fprintf ppf
      "@[<v 4>public static class %a@ implements CatalaValue {@ @ %t@ @ %a@ @ \
       %t@ @ %a@]@\n\
       }"
      format_struct sname format_params
      (format_struct_constructor ctx ~vis:Public)
      (sname, fields)
      (format_comparison (StructName.to_string sname) format_fields_comparison)
      format_struct_to_string fields_l
  in
  let structs_to_generate =
    StructName.Map.filter
      (fun sname _ ->
        StructName.path sname = []
        && (not (StructName.Set.mem sname ctx.in_scope_structs))
        && not (StructName.Set.mem sname ctx.out_scope_structs))
      ctx.decl_ctx.ctx_structs
    |> StructName.Map.bindings
  in
  pp_print_list_padded ~pp_sep:pp_print_double_space format_struct ppf
    structs_to_generate

let format_enums ctx ppf =
  let format_enum ppf (ename, cstrs) =
    let format_enum_kind ppf =
      fprintf ppf "@[<hov 4>public enum Kind {@ %a@] }"
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
          if is_unit then () else fprintf ppf "%a v" (format_typ ctx) typ
        in
        fprintf ppf
          "@[<v 4>public static %a make%a(%t) {@ return new %a(Kind.%a, %s);@;\
           <1 -4>}@]" format_enum ename EnumConstructor.format cstr format_arg
          format_enum ename EnumConstructor.format cstr
          (if is_unit then "CatalaUnit.INSTANCE" else "v")
      in
      fprintf ppf
        "@[<v 4>private %a(Kind k, CatalaValue contents) {@ this.kind = k;@ \
         this.contents = contents;@;\
         <1 -4>}@]%a" format_enum ename
        (pp_print_list_padded ~pp_sep:pp_print_space format_enum_make)
        (EnumConstructor.Map.bindings cstrs)
    in
    let format_enum_accessors ppf =
      let format_default_accessor ppf =
        fprintf ppf
          "@@SuppressWarnings(\"unchecked\")@,\
           @[<v 4>public <T> T getContentsAs(Kind k, Class<T> clazz) {@ @[<v \
           2>if (this.kind != k) {@ throw new \
           IllegalArgumentException(\"Invalid enum contents access: expected \
           \" + k + \", got \" + this.kind);@;\
           <1 -2>}@]@ return (T) this.contents;@;\
           <1 -4>}@]"
      in
      let format_enum_accessor ppf (cstr, typ) =
        fprintf ppf
          "@[<v 4>public %a get%aContents() {@ return \
           this.getContentsAs(Kind.%a, %a.class);@]@\n\
           }"
          (format_typ ctx) typ EnumConstructor.format cstr
          EnumConstructor.format cstr (format_typ ctx) typ
      in
      fprintf ppf "@[<v>%t%a@]" format_default_accessor
        (pp_print_list_padded ~pp_sep:pp_print_space format_enum_accessor)
        (List.filter
           (fun (_, typ) ->
             match Mark.remove typ with TLit TUnit -> false | _ -> true)
           (EnumConstructor.Map.bindings cstrs))
    in
    let format_fields_comparison ppf =
      fprintf ppf
        "@[<v 4>if (this.kind == v.kind) {@,\
         return @[<v 4>this.getContentsAs(this.kind, \
         CatalaValue.class).equalsTo(@\n\
         v.getContentsAs(v.kind,CatalaValue.class));@]@;\
         <1 -4>} else {@,\
         return CatalaBool.FALSE;@;\
         <1 -4>}@]"
    in
    fprintf ppf
      "@[<v 4>public static class %a@ implements CatalaValue {@ @ %t@ @ %t@ @ \
       %t@ @ %t@ @ %t@ @ %t@]@\n\
       }"
      format_enum ename format_enum_kind format_enum_params format_enum_constrs
      format_enum_accessors
      (format_comparison (EnumName.to_string ename) format_fields_comparison)
      format_enum_to_string
  in
  let enums_to_generate =
    EnumName.Map.filter
      (fun ename _ ->
        EnumName.path ename = [] && not (EnumName.equal ename Expr.option_enum))
      ctx.decl_ctx.ctx_enums
    |> EnumName.Map.bindings
  in
  pp_print_list_padded ~pp_sep:pp_print_double_space format_enum ppf
    enums_to_generate

let format_external_parameter ctx ppf (name, ty, vis) =
  fprintf ppf
    "// TO IMPLEMENT@\n@[<hov 4>%astatic final %a %a =@ null; //TO IMPLEMENT@]"
    format_visibility vis (format_typ ctx) ty TopdefName.format name

let format_external_method ctx ppf (name, (ty_l, ret_ty), vis) =
  let format_input_types ppf = function
    | [] -> fprintf ppf "CatalaUnit"
    | [t] -> (format_typ ctx) ppf t
    | l -> (format_typ ctx) ppf (TTuple l, Pos.void)
  in
  fprintf ppf
    "// EXTERNAL TO IMPLEMENT@\n\
     @[<hov 4>%astatic final CatalaFunction<%a,%a> %a =@ x -> {@\n\
     throw new RuntimeException(\"External function %a not implemented\");@]@\n\
     };"
    format_visibility vis format_input_types ty_l (format_typ ctx) ret_ty
    TopdefName.format name TopdefName.format name

let format_global_parameter ctx ppf (name, e, ty, vis) =
  fprintf ppf "@[<hov 4>%astatic final %a %a =@ %a;@]" format_visibility vis
    (format_typ ctx) ty VarName.format name (format_expression ctx) e

let format_global_method ctx ppf (name, f, vis) =
  let format_input_types ppf = function
    | [] -> fprintf ppf "CatalaUnit"
    | [t] -> (format_typ ctx) ppf t
    | l -> (format_typ ctx) ppf (TTuple l, Pos.void)
  in
  fprintf ppf "@[<hov 4>%astatic final CatalaFunction<%a,%a> %a =@ %a;@]"
    format_visibility vis format_input_types
    (List.map snd f.func_params)
    (format_typ ctx) f.func_return_typ FuncName.format name
    (format_inner_func_body ctx)
    f

let format_globals ctx ppf globals =
  let externals_vars, externals_funcs =
    Shared_ast.TopdefName.Map.fold
      (fun topdef_name (typ, vis) ((vars, funcs) as acc) ->
        if TopdefName.path topdef_name <> [] then acc
        else
          match typ with
          | TArrow (ty_l, ret_ty), _ ->
            vars, (topdef_name, (ty_l, ret_ty), vis) :: funcs
          | _ -> (topdef_name, typ, vis) :: vars, funcs)
      ctx.decl_ctx.ctx_topdefs ([], [])
    |> fun (l, r) -> List.rev l, List.rev r
  in
  if globals = [] && externals_vars = [] && externals_funcs = [] then (
    Message.debug "No globals definition to generate";
    ctx)
  else
    let globals, externals_vars, externals_funcs =
      if globals <> [] then
        (* Don't generate anything if there are (real) globals *)
        globals, [], []
      else [], externals_vars, externals_funcs
    in
    let ctx' = { ctx with in_globals = true } in
    let pp_item ppf = function
      | SVar { var; expr; typ; visibility } ->
        format_global_parameter ctx' ppf (var, expr, typ, visibility)
      | SFunc { var; func; visibility } ->
        format_global_method ctx' ppf (var, func, visibility)
      | _ -> assert false
    in
    pp_print_double_space ppf ();
    fprintf ppf "@[<v 4>@[<hov 4>public static class Globals@ {@]%a%a%a@]@\n}"
      (pp_print_list_padded ~pp_sep:pp_print_double_space pp_item)
      globals
      (pp_print_list_padded ~pp_sep:pp_print_double_space
         (format_external_parameter ctx'))
      externals_vars
      (pp_print_list_padded ~pp_sep:pp_print_double_space
         (format_external_method ctx'))
      externals_funcs;
    let vars, funcs =
      List.partition_map
        (let open Either in
         function
         | SVar { var; _ } -> Left var
         | SFunc { var; _ } -> Right var
         | SScope _ -> assert false)
        globals
    in
    {
      ctx with
      global_vars = VarName.Set.of_list vars;
      global_funcs = FuncName.Set.of_list funcs;
    }

let format_program ctx ppf { code_items; tests; _ } =
  let scopes, globals =
    List.partition_map
      (let open Either in
       function
       | SScope body ->
         let out_struct_name =
           match body.scope_body_func.func_return_typ with
           | TStruct name, _ -> name
           | _ -> assert false
         in
         let body =
           {
             body with
             scope_body_name =
               ScopeName.map_info
                 (fun (ml, (_, p)) ->
                   ml, (StructName.get_info out_struct_name |> fst, p))
                 body.scope_body_name;
           }
         in
         Left body
       | x -> Right x)
      code_items
  in
  let ctx = format_globals ctx ppf globals in
  format_structs ctx ppf;
  format_enums ctx ppf;
  let ctx =
    List.fold_left
      (fun ctx { scope_body_var; scope_body_name; _ } ->
        {
          ctx with
          scope_func_names =
            FuncName.Map.add scope_body_var scope_body_name ctx.scope_func_names;
        })
      ctx scopes
  in
  pp_print_list_padded ~pp_sep:pp_print_double_space
    (fun ppf s -> format_scope ctx ppf s)
    ppf scopes;
  if snd tests <> [] then format_tests ctx ppf tests

let format_program ~class_name output_file ppf (p : Ast.program) : unit =
  Format.pp_open_vbox ppf 0;
  let header =
    (if Global.options.gen_external then
       [
         "/* This is a template file following the expected interface and \
          declarations to";
         " * implement the corresponding Catala module.";
         " *";
         " * You should replace all `Error.Impossible` place-holders with your";
         " * implementation and rename it to remove the \".template\" suffix. \
          */";
       ]
     else
       [
         "/* This file has been generated by the Catala compiler, do not edit! \
          */";
       ])
    @ [""; "import catala.runtime.*;"; "import catala.runtime.exception.*;"; ""]
  in
  let ctx = populate_context p in
  pp_print_list pp_print_string ppf header;
  pp_print_newline ppf ();
  fprintf ppf "@[<v 4>public class %s {%a@ @]@\n}@\n" class_name
    (format_program ctx) p;
  if Global.options.gen_external then
    output_file
    |> Option.iter
         (Message.result "Generated template external implementations:@ %a"
            File.format)
