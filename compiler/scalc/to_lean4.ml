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

(* Context for pretty-printing *)
type ctx = { decl_ctx : decl_ctx }

(* Lean4 keywords to avoid *)
let lean4_keywords =
  [
    "def";
    "theorem";
    "axiom";
    "inductive";
    "structure";
    "class";
    "instance";
    "where";
    "let";
    "in";
    "fun";
    "match";
    "with";
    "do";
    "if";
    "then";
    "else";
    "for";
    "return";
    "try";
    "catch";
    "finally";
    "import";
    "open";
    "namespace";
    "section";
    "variable";
    "universe";
    "mutual";
    "private";
    "protected";
    "partial";
    "unsafe";
    "noncomputable";
    "opaque";
    "abbrev";
    "deriving";
  ]

(* Renaming configuration for Lean4 *)
let renaming =
  Renaming.program ()
    ~reserved:lean4_keywords
    ~skip_constant_binders:false
    ~constant_binder_name:None
    ~namespaced_fields:true
    ~namespaced_constrs:true
    ~prefix_module:false
    ~modnames_conflict:false
    ~f_var:String.to_snake_case
    ~f_struct:String.to_camel_case
    ~f_enum:String.to_camel_case

(* Helper to check if type needs parentheses *)
let typ_needs_parens (t : typ) : bool =
  match Mark.remove t with
  | TArrow _ | TArray _ -> true
  | _ -> false

(* Format Catala types to Lean4 types *)
let rec format_typ (ctx : ctx) (fmt : Format.formatter) (typ : typ) : unit =
  let format_typ = format_typ ctx in
  let format_typ_with_parens (fmt : Format.formatter) (t : typ) =
    if typ_needs_parens t then
      Format.fprintf fmt "(%a)" format_typ t
    else
      Format.fprintf fmt "%a" format_typ t
  in
  match Mark.remove typ with
  | TLit TUnit -> Format.pp_print_string fmt "Unit"
  | TLit TBool -> Format.pp_print_string fmt "Bool"
  | TLit TInt -> Format.pp_print_string fmt "Int"
  | TLit TRat -> Format.pp_print_string fmt "Rat" (* Rational numbers *)
  | TLit TMoney -> Format.pp_print_string fmt "Money"
  | TLit TDate -> Format.pp_print_string fmt "Date"
  | TLit TDuration -> Format.pp_print_string fmt "Duration"
  | TLit TPos -> Format.pp_print_string fmt "SourcePosition"
  | TTuple ts ->
    (* Lean4 uses × for product types *)
    Format.fprintf fmt "@[<hov>%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " × ")
         format_typ_with_parens)
      ts
  | TStruct s -> StructName.format fmt s
  | TEnum e -> EnumName.format fmt e
  | TOption some_typ ->
    (* Lean4's Option type *)
    Format.fprintf fmt "Option %a" format_typ_with_parens some_typ
  | TArrow (t1, t2) ->
    (* Function types *)
    Format.fprintf fmt "%a → %a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " → ")
         format_typ_with_parens)
      t1 format_typ_with_parens t2
  | TArray t1 ->
    (* Lists in Lean4 *)
    Format.fprintf fmt "List %a" format_typ_with_parens t1
  | TDefault t -> format_typ fmt t
  | TVar _ -> Format.pp_print_string fmt "α" (* Type variable *)
  | TForAll tb ->
    let _v, t = Bindlib.unmbind tb in
    format_typ fmt t
  | TClosureEnv -> Format.pp_print_string fmt "ClosureEnv" (* Placeholder *)

(* Format struct declaration *)
let format_struct_decl (ctx : ctx) (fmt : Format.formatter)
    (struct_name, struct_fields) : unit =
  let fields = StructField.Map.bindings struct_fields in
  if StructField.Map.is_empty struct_fields then
    (* Empty struct - use a single unit field *)
    Format.fprintf fmt "@[<v 2>structure %a where@,  dummy : Unit@]"
      StructName.format struct_name
  else
    Format.fprintf fmt "@[<v 2>structure %a where@,%a@]"
      StructName.format struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (field_name, field_type) ->
           Format.fprintf fmt "  %a : %a"
             StructField.format field_name
             (format_typ ctx) field_type))
      fields

(* Format enum declaration *)
let format_enum_decl (ctx : ctx) (fmt : Format.formatter)
    (enum_name, enum_cons) : unit =
  if EnumConstructor.Map.is_empty enum_cons then
    failwith "Cannot translate empty enum"
  else
    Format.fprintf fmt "@[<v 2>inductive %a where@,%a@]"
      EnumName.format enum_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         (fun fmt (cons_name, cons_type) ->
           Format.fprintf fmt "  | %a : %a → %a"
             EnumConstructor.format cons_name
             (format_typ ctx) cons_type
             EnumName.format enum_name))
      (EnumConstructor.Map.bindings enum_cons)

(* Format literals *)
let format_lit (fmt : Format.formatter) (l : lit Mark.pos) : unit =
  match Mark.remove l with
  | LBool true -> Format.pp_print_string fmt "true"
  | LBool false -> Format.pp_print_string fmt "false"
  | LInt i ->
    Format.fprintf fmt "(%s : Int)"
      (Runtime.integer_to_string i)
  | LUnit -> Format.pp_print_string fmt "()"
  | LRat i ->
    Format.fprintf fmt "(Rat.mk %s 1)" (* TODO: proper rational representation *)
      (Q.to_string i)
  | LMoney e ->
    Format.fprintf fmt "(Money.ofCents %s)"
      (Runtime.integer_to_string (Runtime.money_to_cents e))
  | LDate d ->
    Format.fprintf fmt "(Date.mk %d %d %d)"
      (Runtime.integer_to_int (Runtime.year_of_date d))
      (Runtime.integer_to_int (Runtime.month_number_of_date d))
      (Runtime.integer_to_int (Runtime.day_of_month_of_date d))
  | LDuration d ->
    let years, months, days = Runtime.duration_to_years_months_days d in
    Format.fprintf fmt "(Duration.mk %d %d %d)" years months days

(* Format operators *)
let format_op (fmt : Format.formatter) (op : operator Mark.pos) : unit =
  match Mark.remove op with
  | Log _ -> failwith "Log operator should not appear in code generation"
  (* Unary operators *)
  | Minus_int | Minus_rat | Minus_mon | Minus_dur ->
    Format.pp_print_string fmt "-"
  | Not -> Format.pp_print_string fmt "!"
  (* Arithmetic operators *)
  | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dur_dur ->
    Format.pp_print_string fmt "+"
  | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dur_dur ->
    Format.pp_print_string fmt "-"
  | Mult_int_int | Mult_rat_rat | Mult_mon_int | Mult_mon_rat | Mult_dur_int ->
    Format.pp_print_string fmt "*"
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int | Div_mon_rat | Div_dur_dur ->
    Format.pp_print_string fmt "/"
  (* Logical operators *)
  | And -> Format.pp_print_string fmt "&&"
  | Or -> Format.pp_print_string fmt "||"
  (* Comparison operators *)
  | Eq | Eq_boo_boo | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dat_dat | Eq_dur_dur ->
    Format.pp_print_string fmt "="
  | Xor -> Format.pp_print_string fmt "!="
  | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur ->
    Format.pp_print_string fmt "<"
  | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat | Lte_dur_dur ->
    Format.pp_print_string fmt "<="
  | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur ->
    Format.pp_print_string fmt ">"
  | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat | Gte_dur_dur ->
    Format.pp_print_string fmt ">="
  (* Type conversions - will need runtime functions *)
  | ToInt_rat -> Format.pp_print_string fmt "Rat.toInt"
  | ToInt_mon -> Format.pp_print_string fmt "Money.toInt"
  | ToRat_int -> Format.pp_print_string fmt "Rat.ofInt"
  | ToRat_mon -> Format.pp_print_string fmt "Money.toRat"
  | ToMoney_rat -> Format.pp_print_string fmt "Money.ofRat"
  | ToMoney_int -> Format.pp_print_string fmt "Money.ofInt"
  | Round_mon -> Format.pp_print_string fmt "Money.round"
  | Round_rat -> Format.pp_print_string fmt "Rat.round"
  (* Array/List operations *)
  | Length -> Format.pp_print_string fmt "List.length"
  | Concat -> Format.pp_print_string fmt "++"
  | Map -> Format.pp_print_string fmt "List.map"
  | Map2 -> Format.pp_print_string fmt "List.zipWith"
  | Filter -> Format.pp_print_string fmt "List.filter"
  | Fold -> Format.pp_print_string fmt "List.foldl"
  | Reduce -> Format.pp_print_string fmt "List.foldl1"
  (* Date operations *)
  | Add_dat_dur _ -> Format.pp_print_string fmt "Date.addDuration"
  | Sub_dat_dur _ -> Format.pp_print_string fmt "Date.subDuration"
  (* Special operations *)
  | HandleExceptions -> Format.pp_print_string fmt "handleExceptions"
  | FromClosureEnv | ToClosureEnv ->
    failwith "Closure environment operations not implemented"

(* Check if expression needs parentheses *)
let needs_parens (e : expr) : bool =
  match Mark.remove e with
  | EAppOp _ | EApp _ -> true
  | _ -> false

(* Format expressions *)
let rec format_expression (ctx : ctx) (fmt : Format.formatter) (e : expr) : unit =
  let format_expr = format_expression ctx in
  let format_expr_with_parens fmt e =
    if needs_parens e then
      Format.fprintf fmt "(%a)" format_expr e
    else
      format_expr fmt e
  in
  match Mark.remove e with
  | EVar v -> VarName.format fmt v
  | EFunc f -> FuncName.format fmt f
  | EStruct { fields; name = _ } ->
    Format.fprintf fmt "{ %a }"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt (field, e) ->
           Format.fprintf fmt "%a := %a"
             StructField.format field format_expr e))
      (StructField.Map.bindings fields)
  | EStructFieldAccess { e1; field; _ } ->
    Format.fprintf fmt "%a.%a" format_expr_with_parens e1 StructField.format field
  | ETuple es ->
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         format_expr)
      es
  | ETupleAccess { e1; index; _ } ->
    Format.fprintf fmt "%a.%d" format_expr_with_parens e1 (index + 1)
  | EInj { cons; name = e_name; _ }
    when EnumName.equal e_name Expr.option_enum
         && EnumConstructor.equal cons Expr.none_constr ->
    Format.pp_print_string fmt "none"
  | EInj { e1 = e; cons; name = e_name; _ }
    when EnumName.equal e_name Expr.option_enum
         && EnumConstructor.equal cons Expr.some_constr ->
    Format.fprintf fmt "(some %a)" format_expr e
  | EInj { e1 = e; cons; name = enum_name; _ } ->
    Format.fprintf fmt "%a.%a %a"
      EnumName.format enum_name
      EnumConstructor.format cons
      format_expr_with_parens e
  | EArray es ->
    Format.fprintf fmt "[%a]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         format_expr)
      es
  | ELit l -> format_lit fmt (Mark.copy e l)
  | EPosLit ->
    let pos = Mark.get e in
    Format.fprintf fmt
      "(SourcePosition.mk \"%s\" %d %d %d %d)"
      (Pos.get_file pos)
      (Pos.get_start_line pos)
      (Pos.get_start_column pos)
      (Pos.get_end_line pos)
      (Pos.get_end_column pos)
  | EApp { f; args; _ } ->
    Format.fprintf fmt "%a %a"
      format_expr_with_parens f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
         format_expr_with_parens)
      args
  | EAppOp { op; args = [arg1; arg2]; _ } ->
    (* Binary operator *)
    Format.fprintf fmt "(%a %a %a)"
      format_expr_with_parens arg1
      format_op op
      format_expr_with_parens arg2
  | EAppOp { op; args = [arg]; _ } ->
    (* Unary operator *)
    Format.fprintf fmt "(%a%a)"
      format_op op
      format_expr_with_parens arg
  | EAppOp { op; args; _ } ->
    (* Function-style operator application *)
    Format.fprintf fmt "%a %a"
      format_op op
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
         format_expr_with_parens)
      args
  | EExternal { modname; name } ->
    Format.fprintf fmt "%a.%s"
      VarName.format (Mark.remove modname)
      (Mark.remove name)

(* Format type declarations (structs and enums) *)
let format_ctx (type_ordering : TypeIdent.t list) (fmt : Format.formatter)
    (ctx : ctx) : unit =
  let is_in_type_ordering s =
    List.exists
      (fun struct_or_enum ->
        match struct_or_enum with
        | TypeIdent.Enum _ -> false
        | TypeIdent.Struct s' -> StructName.equal s s')
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
          Format.fprintf fmt "%a@\n@\n"
            (format_struct_decl ctx)
            (s, StructName.Map.find s ctx.decl_ctx.ctx_structs)
      | TypeIdent.Enum e ->
        if EnumName.path e = [] && not (EnumName.equal e Expr.option_enum) then
          Format.fprintf fmt "%a@\n@\n"
            (format_enum_decl ctx)
            (e, EnumName.Map.find e ctx.decl_ctx.ctx_enums))
    (type_ordering @ scope_structs)

(* Format a block of statements - simplified for now *)
let rec format_block (ctx : ctx) (fmt : Format.formatter) (block : block) : unit =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
    (format_statement ctx)
    fmt block

(* Format statements - basic implementation *)
and format_statement (ctx : ctx) (fmt : Format.formatter) (stmt : stmt Mark.pos) : unit =
  match Mark.remove stmt with
  | SLocalDecl { name; typ } ->
    Format.fprintf fmt "  -- Declaration: %a : %a"
      VarName.format (Mark.remove name)
      (format_typ ctx) typ
  | SLocalInit { name; typ = _; expr } | SLocalDef { name; typ = _; expr } ->
    Format.fprintf fmt "  let %a := %a"
      VarName.format (Mark.remove name)
      (format_expression ctx) expr
  | SReturn expr ->
    Format.fprintf fmt "  return %a"
      (format_expression ctx) expr
  | SIfThenElse { if_expr; then_block; else_block } ->
    Format.fprintf fmt "  @[<v 2>if %a then@\n%a@]@\n  @[<v 2>else@\n%a@]"
      (format_expression ctx) if_expr
      (format_block ctx) then_block
      (format_block ctx) else_block
  | SSwitch { switch_var; enum_name; switch_cases; _ } 
    when EnumName.equal enum_name Expr.option_enum ->
    (* Special case for Option type - use Lean4's native none/some *)
    (match switch_cases with
     | [case_none; case_some] ->
       Format.fprintf fmt "  @[<v 2>match %a with@\n  | none =>@\n%a@\n  | some %a =>@\n%a@]"
         VarName.format switch_var
         (format_block ctx) case_none.case_block
         VarName.format case_some.payload_var_name
         (format_block ctx) case_some.case_block
     | _ -> 
       Format.fprintf fmt "  -- Invalid Option switch"
    )
  | SSwitch { switch_var; enum_name; switch_cases; _ } ->
    let cons_map = EnumName.Map.find enum_name ctx.decl_ctx.ctx_enums in
    let cases_with_cons =
      List.map2
        (fun case (cons_name, _) -> case, cons_name)
        switch_cases
        (EnumConstructor.Map.bindings cons_map)
    in
    Format.fprintf fmt "  @[<v 2>match %a with@\n%a@]"
      VarName.format switch_var
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         (fun fmt ({ case_block; payload_var_name; _ }, cons_name) ->
           Format.fprintf fmt "  | %a.%a %a =>@\n%a"
             EnumName.format enum_name
             EnumConstructor.format cons_name
             VarName.format payload_var_name
             (format_block ctx) case_block))
      cases_with_cons
  | SAssert { expr; _ } ->
    Format.fprintf fmt "  assert! %a"
      (format_expression ctx) expr
  | SFatalError { error; _ } ->
    Format.fprintf fmt "  panic! \"%s\""
      (Runtime.error_to_string error)
  | SInnerFuncDef { name; func = _ } ->
    Format.fprintf fmt "  -- Inner function %a (not implemented)"
      VarName.format (Mark.remove name)
  | SSpecialOp _ ->
    Format.fprintf fmt "  -- Special operation (not implemented)"

(* Format code items *)
let format_code_item (ctx : ctx) (fmt : Format.formatter) = function
  | SVar { var; expr; _ } ->
    Format.fprintf fmt "def %a : Unit :=@\n  let _ := %a@\n  ()@\n@\n"
      VarName.format var
      (format_expression ctx) expr
  | SFunc { var; func; _ } | SScope { scope_body_var = var; scope_body_func = func; _ } ->
    let { func_params; func_body; _ } = func in
    Format.fprintf fmt "@[<v>def %a%a : Unit :=@\n%a@\n  ()@]@\n@\n"
      FuncName.format var
      (fun fmt params ->
        if params = [] then ()
        else
          Format.fprintf fmt " @[<hov>(%a)@]"
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
               (fun fmt (param_name, param_typ) ->
                 Format.fprintf fmt "(%a : %a)"
                   VarName.format (Mark.remove param_name)
                   (format_typ ctx) param_typ))
            params)
      func_params
      (format_block ctx) func_body

(* Format test scopes *)
let format_tests (fmt : Format.formatter) (p : Ast.program) : unit =
  let _, tests = p.tests in
  if tests <> [] then begin
    Format.fprintf fmt "-- Test scopes@\n";
    List.iter (fun (scope_name, _block) ->
      Format.fprintf fmt "-- Test for scope %a@\n" ScopeName.format scope_name;
      Format.fprintf fmt "def test_%a := sorry -- test not implemented@\n@\n"
        ScopeName.format scope_name
    ) tests
  end

(* Main program formatter *)
let format_program
    (output_file : File.t option)
    (fmt : Format.formatter)
    (p : Ast.program)
    (type_ordering : TypeIdent.t list) : unit =
  (* Suppress unused variable warning *)
  let _ = output_file in
  
  (* Create context *)
  let ctx = { decl_ctx = p.ctx.decl_ctx } in
  
  Format.pp_open_vbox fmt 0;
  
  (* File header *)
  Format.fprintf fmt "-- This file has been generated by the Catala compiler, do not edit!@\n@\n";
  Format.fprintf fmt "-- Lean4 runtime imports@\n";
  Format.fprintf fmt "-- TODO: Add runtime library imports@\n@\n";
  
  (* Type declarations (structs and enums) *)
  Format.fprintf fmt "-- Type declarations@\n";
  format_ctx type_ordering fmt ctx;
  Format.fprintf fmt "@\n";
  
  (* Program definitions *)
  Format.fprintf fmt "-- Program definitions@\n";
  List.iter (format_code_item ctx fmt) p.code_items;
  Format.fprintf fmt "@\n";
  
  (* Test scopes *)
  format_tests fmt p;
  
  Format.pp_close_box fmt ();
  Format.pp_print_flush fmt ()

