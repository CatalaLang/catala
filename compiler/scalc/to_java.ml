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

module SPrint = Print
open Catala_utils
open Shared_ast
open Ast
module Runtime = Runtime_ocaml.Runtime
module D = Dcalc.Ast
module L = Lcalc.Ast
open Format

let pp_comma ppf () = fprintf ppf ",@ "
let pp_print_double_space ppf () = fprintf ppf "@ @ "
let ext = "java"

type context = {
  ctx : Ast.ctx;
  in_scope_structs : StructName.Set.t;
  out_scope_structs : ScopeName.t StructName.Map.t;
  scope_func_names : FuncName.Set.t;
  in_globals : bool;
  global_funcs : FuncName.Set.t;
  global_vars : VarName.Set.t;
}

let format_uid_list (ppf : formatter) (uids : Uid.MarkedString.info list) : unit
    =
  fprintf ppf "[%a]"
    (pp_print_list ~pp_sep:pp_comma (fun ppf info ->
         fprintf ppf "\"%a\"" Uid.MarkedString.format info))
    uids

let format_string_list (ppf : formatter) (uids : string list) : unit =
  let sanitize_quotes = Re.compile (Re.char '"') in
  fprintf ppf "new String[]{%a}"
    (pp_print_list ~pp_sep:pp_comma (fun ppf info ->
         fprintf ppf "\"%s\""
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
    (* Reserved for generation *)
    "Globals";
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
      (ModuleName.Map.find m ctx.ctx.modules)
      (Id.base s)

let format_struct = format_qualified (module StructName)
let format_enum = format_qualified (module EnumName)

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
  | HandleExceptions -> pp_print_string ppf "ConflictException.handleExceptions"
  | FromClosureEnv | ToClosureEnv -> failwith "unimplemented"

let _format_scope_calls ppf (p : Ast.program) =
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

let format_visibility ppf = function
  | Private -> () (* nothing => package visibility *)
  | Public -> fprintf ppf "public "

let rec format_typ ppf typ =
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
    fprintf ppf "CatalaFunction<%a,%a>" format_typ ty format_typ ret_ty
  | TArrow (_args_ty, ret_ty) ->
    fprintf ppf "CatalaFunction<CatalaTuple,%a>" format_typ ret_ty
  | TTuple _ -> fprintf ppf "CatalaTuple"
  | TStruct sname -> StructName.format ppf sname (* TODO: resolve imports *)
  | TEnum ename -> EnumName.format ppf ename (* TODO: resolve imports *)
  | TOption typ -> fprintf ppf "CatalaOption<%a>" format_typ typ
  | TArray typ -> fprintf ppf "CatalaArray<%a>" format_typ typ
  | TDefault typ -> format_typ ppf typ
  | TAny -> fprintf ppf "CatalaValue"
  | TClosureEnv -> assert false

let format_struct_params ppf (fields : typ StructField.Map.t) =
  let fields = StructField.Map.bindings fields in
  pp_print_list ~pp_sep:pp_comma
    (fun ppf (sfield, typ) ->
      fprintf ppf "final %a %a" format_typ typ StructField.format sfield)
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
    StructName.Map.find struct_name ctx.ctx.decl_ctx.ctx_structs
    |> StructField.Map.map (fun _ ->
           ( EInj
               {
                 name = Expr.option_enum;
                 cons = Expr.none_constr;
                 e1 = ELit LUnit, Pos.void;
                 expr_typ = TOption (TAny, Pos.void), Pos.void;
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
  | EStruct { fields = es; name = s } ->
    if StructName.Set.mem s in_scope_structs then begin
      (pp_print_list ~pp_sep:pp_comma (fun ppf (_struct_field, e) ->
           format_expression ctx ppf e))
        ppf
        (fill_struct_bindings ctx s es)
    end
    else if StructName.Map.mem s out_scope_structs then begin
      let sname = StructName.Map.find s out_scope_structs in
      fprintf ppf "new %a(new %a.%aOut(%a))" ScopeName.format sname
        ScopeName.format sname ScopeName.format sname
        (pp_print_list ~pp_sep:pp_comma (fun ppf (_struct_field, e) ->
             fprintf ppf "%a" (format_expression ctx) e))
        (StructField.Map.bindings es)
    end
    else
      fprintf ppf "new %a (%a)" (format_struct ctx) s
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
    fprintf ppf "CatalaOption.NONE"
  | EInj { e1 = e; cons; name = e_name; _ }
    when EnumName.equal e_name Expr.option_enum
         && EnumConstructor.equal cons Expr.some_constr ->
    fprintf ppf "CatalaOption.some(%a)" (format_expression ctx) e
  | EInj { e1 = ELit LUnit, _; cons; name = enum_name; _ } ->
    fprintf ppf "%a.make%a()" (format_enum ctx) enum_name EnumConstructor.format
      cons
  | EInj { e1 = e; cons; name = enum_name; _ } ->
    fprintf ppf "%a.make%a(%a)" (format_enum ctx) enum_name
      EnumConstructor.format cons (format_expression ctx) e
  | EArray es ->
    fprintf ppf "new CatalaArray<>(%a)"
      (pp_print_list ~pp_sep:pp_comma (fun ppf e ->
           fprintf ppf "%a" (format_expression ctx) e))
      es
  | ELit l -> fprintf ppf "%a" format_lit (Mark.copy e l)
  | EPosLit ->
    let pos = Mark.get e in
    fprintf ppf
      "@[<hov 4>new SourcePosition(@,\"%s\",@ %d, %d,@ %d, %d,@ %a@;<0 -4>)@]"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos)
  | EAppOp { op = (HandleExceptions, _) as op; args = [(EArray exprs, _)]; _ }
    ->
    fprintf ppf
      "@[<hv 2>%a(@;<0 -1>new CatalaArray<CatalaOption<CatalaTuple>>(@ %a@ )@])"
      format_op op
      (pp_print_list ~pp_sep:pp_comma (fun ppf e -> format_expression ctx ppf e))
      exprs
  | EAppOp
      {
        op = ((Map | Filter | Reduce | Fold | Map2 | Concat), _) as op;
        args;
        _;
      } ->
    let l, args = get_list_and_args_expr op args in
    fprintf ppf "@[<hv 2>%a.%a(@;<0 -1>%a@])"
      (format_expression_with_paren ctx)
      l format_op op
      (pp_print_list ~pp_sep:pp_comma (fun ppf e -> format_expression ctx ppf e))
      args
  | EAppOp { op; args = [arg1; arg2]; _ } ->
    fprintf ppf "@[<hv 2>%a.%a(@;<0 -1>%a@])"
      (format_expression_with_paren ctx)
      arg1 format_op op (format_expression ctx) arg2
  | EApp
      {
        f = EAppOp { op = Log (BeginCall, info), _; args = [f]; _ }, _;
        args = [arg];
      }
    when Global.options.trace <> None ->
    fprintf ppf "log_begin_call(%a,@ %a,@ %a)" format_uid_list info
      (format_expression ctx) f (format_expression ctx) arg
  | EAppOp { op = Log (VarDef var_def_info, info), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    fprintf ppf
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
    fprintf ppf
      "log_decision_taken(SourcePosition(filename=\"%s\",@ start_line=%d,@ \
       start_column=%d,@ end_line=%d, end_column=%d,@ law_headings=%a), %a)"
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos) format_string_list
      (Pos.get_law_info pos) (format_expression ctx) arg1
  | EAppOp { op = Log (EndCall, info), _; args = [arg1]; _ }
    when Global.options.trace <> None ->
    fprintf ppf "log_end_call(%a,@ %a)" format_uid_list info
      (format_expression ctx) arg1
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
  | EApp { f = EFunc fname, _; args } when FuncName.Set.mem fname global_funcs
    ->
    fprintf ppf "@[<hv 0>%s%a.apply(@;<0 -1>%a)@]"
      (if in_globals then "" else "Globals.")
      FuncName.format fname
      (format_currified_args ctx)
      args
  | EApp { f = (EFunc fname, _) as f; args }
    when FuncName.Set.mem fname scope_func_names ->
    fprintf ppf "@[<hv 0>new %a(@;<0 -1>%a)@]" (format_expression ctx) f
      (pp_print_list ~pp_sep:pp_comma (format_expression ctx))
      args
  | EApp { f; args } ->
    fprintf ppf "@[<hv 0>%a.apply(%a)@]" (format_expression ctx) f
      (format_currified_args ctx)
      args
  | EAppOp { args = []; _ } -> assert false
  | EAppOp { op; args = arg_pos :: arg1 :: args; tys = (TLit TPos, _) :: _ } ->
    fprintf ppf "@[<hv 2>%a.%a(@;<0 -1>%a)@]"
      (format_expression_with_paren ctx)
      arg1 format_op op
      (pp_print_list ~pp_sep:pp_comma (format_expression ctx))
      (arg_pos :: args)
  | EAppOp { op; args = arg1 :: args; _ } ->
    fprintf ppf "@[<hv 2>%a.%a(@;<0 -1>%a)@]" (format_expression ctx) arg1
      format_op op
      (pp_print_list ~pp_sep:pp_comma (format_expression ctx))
      args
  | ETuple es ->
    fprintf ppf "new CatalaTuple(@[<hv 0>%a)@]"
      (pp_print_list ~pp_sep:pp_comma (fun ppf e ->
           fprintf ppf "%a" (format_expression ctx) e))
      es
  | ETupleAccess { e1; index; typ } ->
    fprintf ppf "((%a)%a.get(%d))" format_typ typ
      (format_expression_with_paren ctx)
      e1 index
  | EExternal { modname; name } ->
    fprintf ppf "%a.%s" VarName.format (Mark.remove modname) (Mark.remove name)

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
    fprintf ppf "new CatalaTuple(%a)"
      (pp_print_list ~pp_sep:pp_comma (format_expression ctx))
      args

let rec format_stmt
    ?scope
    ?switch_return_var
    (ctx : context)
    ppf
    (stmt : Ast.stmt Mark.pos) =
  match Mark.remove stmt with
  | SLocalDecl { name; typ } ->
    fprintf ppf "@[<hov 2>final %a@ %a;@]" format_typ typ VarName.format
      (Mark.remove name)
  | SLocalDef { name; expr; _ } ->
    fprintf ppf "@[<hov 2>%a = %a;@]" VarName.format (Mark.remove name)
      (format_expression ctx) expr
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
          (format_expression ctx) expr
      in
      fprintf ppf "@[<v>%a@]"
        (pp_print_list ~pp_sep:pp_print_space pp_self_assign)
        (StructField.Map.bindings fields)
    | _ -> fprintf ppf "@[<hov 2>return %a;@]" (format_expression ctx) expr)
  | SInnerFuncDef { name; func } ->
    fprintf ppf "%a;" (format_inner_func_def ?scope ctx) (name, func)
  | SLocalInit { name; typ; expr } ->
    fprintf ppf "@[<hov 2>%a %a =@ %a;@]" format_typ typ VarName.format
      (Mark.remove name) (format_expression ctx) expr
  | SFatalError { pos_expr; error } ->
    fprintf ppf
      "throw new CatalaException(\"TODO pos(%a) + runtime errors %s specific \
       construction\");"
      (format_expression ctx) pos_expr
      (Runtime.error_to_string error)
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
    let cons_format ppf = format_block ?scope ctx ppf none.case_block in
    let alt_format ppf =
      fprintf ppf "%a %a = %a.get();@\n" format_typ some.payload_var_typ
        VarName.format some.payload_var_name VarName.format switch_var;
      format_block ?scope ctx ppf some.case_block
    in
    format_if ppf ~cond_format ~cons_format ~alt_format
  | SSwitch { switch_var; switch_var_typ = _; enum_name; switch_cases } ->
    let format_init_case ppf (enum_cstr, var, (typ : typ)) =
      match Mark.remove typ with
      | TLit TUnit -> ()
      | _ ->
        let s = VarName.to_string var in
        if s.[0] >= '0' && s.[0] <= '9' then
          (* Do not generate invalid initializers *)
          (* FIXME: wildcard pattern yields weird variable names *)
          ()
        else
          fprintf ppf "%a %a = %a.get%aContents();@ " format_typ typ
            VarName.format var VarName.format switch_var EnumConstructor.format
            enum_cstr
    in
    let format_switch_case
        ppf
        (enum_cstr, { case_block; payload_var_name; payload_var_typ }) =
      fprintf ppf "@[<v 2>case %a: {@ %a%a@ break;@\n}@]" EnumConstructor.format
        enum_cstr format_init_case
        (enum_cstr, payload_var_name, payload_var_typ)
        (format_block ?scope ctx) case_block
    in
    let enum_cstrs =
      EnumName.Map.find enum_name ctx.ctx.decl_ctx.ctx_enums
      |> EnumConstructor.Map.keys
    in
    let pp_default_initializer ppf =
      match switch_return_var with
      | None -> ()
      | Some v ->
        fprintf ppf "@\n@[<v 2>default: {@ %a = null;@ break;@\n}@]"
          VarName.format (Mark.remove v)
    in
    fprintf ppf "@[<v 2>switch (%a.kind) {@ %a%t@ }@]" VarName.format switch_var
      (pp_print_list ~pp_sep:pp_print_space format_switch_case)
      (List.combine enum_cstrs switch_cases)
      pp_default_initializer
  | SAssert { expr; _ } ->
    fprintf ppf "@[<hov 2>assert@ %a.asBoolean();@]"
      (format_expression_with_paren ctx)
      expr
  | SSpecialOp _ -> .

and format_inner_func_def ?scope ctx ppf (name, func) =
  fprintf ppf "@[<hov 2>%a = %a@];" VarName.format (Mark.remove name)
    (format_inner_func_body ?scope ctx)
    func

and format_inner_func_body ?scope ctx ppf = function
  | { func_params = []; func_body; _ }
  | { func_params = [(_, (TLit TUnit, _))]; func_body; _ } ->
    fprintf ppf "unit -> {@ %a }" (format_block ?scope ctx) func_body
  | { func_params = [(pname, _)]; func_body; _ } ->
    fprintf ppf "%a -> {@ %a }" VarName.format (Mark.remove pname)
      (format_block ?scope ctx) func_body
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
    fprintf ppf "%a -> {@ %a@\n%a }" VarName.format args_name
      (format_block ?scope ctx) init_params (format_block ?scope ctx) func_body

and format_block ?scope ctx ppf (block : Ast.block) =
  let rec format_stmts = function
    | [] -> ()
    | [stmt] -> format_stmt ?scope ctx ppf stmt
    | ((SLocalDecl { name; _ }, _) as s1) :: ((SSwitch _, _) as switch) :: r ->
      format_stmt ctx ppf s1;
      pp_print_space ppf ();
      format_stmt ctx ~switch_return_var:name ppf switch;
      pp_print_space ppf ();
      format_stmts r
    | (SLocalDecl { name = n, _; typ }, _)
      :: (SLocalDef { name = n2, _; expr; _ }, _)
      :: r
      when VarName.equal n n2 ->
      fprintf ppf "@[<hov 2>final %a@ %a = %a;@]" format_typ typ VarName.format
        n (format_expression ctx) expr;
      pp_print_space ppf ();
      format_stmts r
    | (SLocalDecl { name = (n, _) as name; typ }, _)
      :: (SInnerFuncDef { name = n2, _; func }, _)
      :: r
      when VarName.equal n n2 ->
      fprintf ppf "@[<hov 2>final %a@ %a;@]" format_typ typ
        (format_inner_func_def ?scope ctx)
        (name, func);
      pp_print_space ppf ();
      format_stmts r
    | ((SFatalError _, _) as stmt) :: _ -> format_stmt ?scope ctx ppf stmt
    | stmt :: r ->
      format_stmt ?scope ctx ppf stmt;
      pp_print_space ppf ();
      format_stmts r
  in
  pp_open_vbox ppf 0;
  format_stmts block;
  pp_close_box ppf ()

and format_if
    ~(cond_format : formatter -> unit)
    ~(cons_format : formatter -> unit)
    ~(alt_format : formatter -> unit)
    ppf =
  fprintf ppf "@[<v 2>if (%t) {@ %t@]@\n@[<v 2>} else {@ %t@]@\n}" cond_format
    cons_format alt_format

let format_constructor_body (ctx : context) sbody ppf =
  format_block ~scope:sbody ctx ppf sbody.scope_body_func.func_body

let format_constructor (ctx : context) (sbody : scope_body) ppf =
  let in_struct_name =
    match sbody.scope_body_func.func_params with
    | [(_vname, (TStruct sn, _))] -> sn
    | _ -> assert false
  in
  StructName.Map.find_opt in_struct_name ctx.ctx.decl_ctx.ctx_structs
  |> function
  | None -> ()
  | Some in_fields ->
    fprintf ppf "@[<hov 2>%a%a (@[<hov>%a@]) {@\n%t@]@\n}" format_visibility
      sbody.scope_body_visibility ScopeName.format sbody.scope_body_name
      format_struct_params in_fields
      (format_constructor_body ctx sbody)

let format_output_parameter ?(vis = Public) ppf (field_name, typ) =
  fprintf ppf "@[<h>%afinal@ %a@ %a;@]" format_visibility vis format_typ typ
    StructField.format field_name

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
    fprintf ppf "@[<v>%a@]@\n"
      (pp_print_list ~pp_sep:pp_print_space
         (format_output_parameter ~vis:sbody.scope_body_visibility))
      (StructField.Map.bindings out_fields)

let format_comparison ppf =
  fprintf ppf
    "@Override@\n\
     @[<hov2>public CatalaBool equalsTo(CatalaValue other) {@\n\
     //TODO@\n\
     return CatalaBool.FALSE;@]@\n\
     }"

let format_struct_constructor_body ppf fields =
  let fields = StructField.Map.bindings fields in
  fprintf ppf "@[<v>%a@]"
    (pp_print_list ~pp_sep:pp_print_space (fun ppf (sfield, _typ) ->
         fprintf ppf "this.%a = %a;" StructField.format sfield
           StructField.format sfield))
    fields

let format_struct_constructor ?(vis = Public) ppf (sname, fields) =
  if StructField.Map.is_empty fields then ()
  else
    fprintf ppf "@[<hov 2>%a%a (@[<hov>%a@]) {@\n%a@]@\n}" format_visibility vis
      StructName.format sname format_struct_params fields
      format_struct_constructor_body fields

let format_scope_out_struct_constructor ?(vis = Public) ppf (scope_name, fields)
    =
  if StructField.Map.is_empty fields then ()
  else
    let format_constr ppf fields =
      fprintf ppf "@[<hov 2>%a%aOut (@[<hov>%a@]) {@\n%a@]@\n}"
        format_visibility vis ScopeName.format scope_name format_struct_params
        fields format_struct_constructor_body fields
    in
    fprintf ppf "@[<hov 2>%astatic class %aOut {@\n%a@\n%a@]@\n}@\n@\n"
      format_visibility vis ScopeName.format scope_name
      (pp_print_list ~pp_sep:pp_print_space (format_output_parameter ~vis))
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
    fprintf ppf "@[<hov 2>%a%a (%aOut result) {@\n%a@]@\n}" format_visibility
      vis ScopeName.format scope_name ScopeName.format scope_name
      format_scope_out_constructor_body fields

let generate_scope ~package ~dir ctx (p : Ast.program) (sbody : Ast.scope_body)
    =
  let open File in
  with_formatter_of_file
    ((dir / ScopeName.to_string sbody.scope_body_name) -.- ext)
  @@ fun ppf ->
  let pp_out_struct ppf =
    match
      ScopeName.Map.find_opt sbody.scope_body_name ctx.ctx.decl_ctx.ctx_scopes
    with
    | None -> ()
    | Some sname ->
      let out_sname = sname.out_struct_name in
      let out_struct =
        StructName.Map.find out_sname ctx.ctx.decl_ctx.ctx_structs
      in
      format_scope_out_struct_constructor ~vis:sbody.scope_body_visibility ppf
        (sbody.scope_body_name, out_struct)
  in
  fprintf ppf
    "package %s;@\n\
     import catala.runtime.*;@\n\
     import catala.runtime.exception.*;@\n\
     @\n\
     @[<v 4>@[<hov 2>%aclass %a@ implements CatalaValue {@]@ @ %t@ %t@ @ %t@ @ \
     %t@]@\n\
     }"
    package format_visibility sbody.scope_body_visibility ScopeName.format
    sbody.scope_body_name
    (format_scope_output_parameters p.ctx sbody)
    (format_constructor ctx sbody)
    pp_out_struct format_comparison

let generate_ctx ~package ~dir (p : Ast.program) =
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
       @[<v 4>@[<hov 2>public class %a@ implements CatalaValue {@]@ @ %t@ @ \
       %t@ @ %t@ @ %t@ @ %t@]@\n\
       }"
      package EnumName.format ename format_enum_kind format_enum_params
      format_enum_constrs format_enum_accessors format_comparison
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
    fprintf ppf
      "package %s;@\n\
       import catala.runtime.*;@\n\
       @\n\
       @[<v 4>@[<hov 2>public class %a@ implements CatalaValue {@]@ @ %t@ @ \
       %a@ @ %t@]@\n\
       }"
      package StructName.format sname format_params
      (format_struct_constructor ~vis:Public)
      (sname, fields) format_comparison
  in
  let in_scope_structs, scope_structs =
    ScopeName.Map.fold
      (fun sname scope_info (in_s, out_s) ->
        ( StructName.Set.(add scope_info.in_struct_name in_s),
          StructName.Map.(add scope_info.out_struct_name sname out_s) ))
      ctx.decl_ctx.ctx_scopes
      (StructName.Set.empty, StructName.Map.empty)
  in
  let structs_to_generate =
    StructName.Map.filter
      (fun sname _ -> not (StructName.Set.mem sname in_scope_structs))
      ctx.decl_ctx.ctx_structs
  in
  StructName.Map.iter
    (fun sname cstrs ->
      File.(with_formatter_of_file ((dir / StructName.to_string sname) -.- ext))
      @@ fun ppf -> generate_struct ppf (sname, cstrs))
    structs_to_generate;
  let context =
    {
      ctx;
      in_scope_structs;
      out_scope_structs = scope_structs;
      scope_func_names = FuncName.Set.empty;
      in_globals = false;
      global_funcs = FuncName.Set.empty;
      global_vars = VarName.Set.empty;
    }
  in
  context

let format_global_parameter ctx ppf (name, e, ty, vis) =
  fprintf ppf "@[<hov 2>%astatic final %a %a =@ %a;@]" format_visibility vis
    format_typ ty VarName.format name (format_expression ctx) e

let format_global_method ctx ppf (name, f, vis) =
  let format_input_types ppf = function
    | [] -> fprintf ppf "CatalaUnit"
    | [t] -> format_typ ppf t
    | l -> format_typ ppf (TTuple l, Pos.void)
  in

  fprintf ppf "@[<hov 2>%astatic final CatalaFunction<%a,%a> %a =@ %a;@]"
    format_visibility vis format_input_types
    (List.map snd f.func_params)
    format_typ f.func_return_typ FuncName.format name
    (format_inner_func_body ctx)
    f

let generate_globals ~package ~dir ctx globals =
  let open File in
  if globals = [] then (
    Message.debug "No globals definition to generate";
    ctx)
  else
    with_formatter_of_file ((dir / "Globals") -.- ext)
    @@ fun ppf ->
    let ctx' = { ctx with in_globals = true } in
    let pp_item ppf = function
      | SVar { var; expr; typ; visibility } ->
        format_global_parameter ctx' ppf (var, expr, typ, visibility)
      | SFunc { var; func; visibility } ->
        format_global_method ctx' ppf (var, func, visibility)
      | _ -> assert false
    in
    fprintf ppf
      "package %s;@\n\
       import catala.runtime.*;@\n\
       import catala.runtime.exception.*;@\n\
       @\n\
       @[<v 4>@[<hov 2>public class Globals@ {@]@ @ %a@]@\n\
       }"
      package
      (pp_print_list ~pp_sep:pp_print_double_space pp_item)
      globals;
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

let generate_items ~package:package_name ~dir:prog_dir ctx p =
  let scopes, globals =
    List.partition_map
      (let open Either in
       function SScope body -> Left body | x -> Right x)
      p.code_items
  in
  let ctx =
    List.fold_left
      (fun ctx { scope_body_var; _ } ->
        {
          ctx with
          scope_func_names =
            FuncName.Set.add scope_body_var ctx.scope_func_names;
        })
      ctx scopes
  in
  let ctx = generate_globals ~package:package_name ~dir:prog_dir ctx globals in
  List.iter (generate_scope ~package:package_name ~dir:prog_dir ctx p) scopes

let generate_program
    ~output_dir
    ~input_file
    (p : Ast.program)
    (_type_ordering : TypeIdent.t list) : unit =
  let open File in
  let to_valid_id s =
    match s.[0] with 'A' .. 'Z' | 'a' .. 'z' -> s | _ -> "_" ^ s
  in
  let package_name =
    let mname =
      match p.module_name with
      | None -> Filename.chop_extension input_file
      | Some (mname, _) -> ModuleName.to_string mname
    in
    (* FIXME: put that in renaming *)
    String.to_snake_case mname |> to_valid_id
  in
  let prog_dir = output_dir / "src" / "main" / "java" / package_name in
  (* Creates the output directory *)
  Catala_utils.File.ensure_dir prog_dir;
  (* Copy over the runtime *)
  (* TODO: add --no-runtime option ? *)
  let catala_bin = Sys.argv.(0) in
  (* assuming '_build/install/default/bin/catala' *)
  let ( / ) = Filename.concat in
  let path =
    Filename.(
      dirname catala_bin / ".." / ".." / ".." / "default" / "runtimes" / "java")
  in
  ignore
  @@ File.process_out "cp" ["-r"; path / "pom.xml"; path / "src"; output_dir];
  ignore @@ File.process_out "chmod" ["a+w"; "-R"; output_dir];
  let ctx = generate_ctx ~package:package_name ~dir:prog_dir p in
  generate_items ~package:package_name ~dir:prog_dir ctx p
