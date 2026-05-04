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
module Runtime = Catala_runtime
open Ast

type ctx = { decl_ctx : decl_ctx; module_name : ModuleName.t option }

type env = {
  locs : VarName.t option;
  global_vars : VarName.Set.t;
  local_vars : VarName.Set.t;
}

let c_keywords =
  [
    "auto";
    "break";
    "case";
    "char";
    "const";
    "continue";
    "default";
    "do";
    "double";
    "else";
    "enum";
    "extern";
    "float";
    "for";
    "goto";
    "if";
    "inline";
    "int";
    "long";
    "register";
    "restrict";
    "return";
    "short";
    "signed";
    "sizeof";
    "static";
    "struct";
    "switch";
    "typedef";
    "union";
    "unsigned";
    "void";
    "volatile";
    "while";
  ]

let is_dummy_var v = VarName.to_string v = "_"
(* this is the marker of a variable that's not expected to be used TODO: mark
   and/or detect such variables in a better way *)

let op_needs_pos (type a) (op : a Op.t) _ty =
  match op with
  | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_int | Div_mon_rat
  | Div_dur_dur | Add_dat_dur _ | Sub_dat_dur _ | Map2 | Eq | Lt | Lte | Gt
  | Gte | Sort _ | ValueFromJson _ ->
    true
  | _ -> false

let renaming =
  (* We reserve the `__` separator for use in this backend; it's stripped from
     idents coming from the user, separates modules from idents, and is also
     used for special ids (eg enum codes) later on *)
  let module_sep_re = Re.(compile (str "__+")) in
  let cap s = String.to_id s |> String.capitalize_ascii in
  let uncap s = String.to_id s |> String.uncapitalize_ascii in
  let upper s = String.to_id s |> String.uppercase_ascii in
  let ren_qualified f s =
    let pfx, id =
      match String.split_on_char '.' s with
      | [id] -> [], id
      | [modname; id] -> [String.to_camel_case modname], id
      | [modname; enum_name; id] ->
        [String.to_camel_case modname; cap enum_name], id
      | _ -> assert false
    in
    let id = f id |> Re.replace_string module_sep_re ~by:"_" in
    String.concat "__" (pfx @ [id])
  in
  Renaming.program ()
    ~reserved:c_keywords
      (* TODO: add catala runtime built-ins as reserved as well ? *)
    ~skip_constant_binders:false ~constant_binder_name:None
    ~namespaced_fields:true ~namespaced_constrs:false ~prefix_module:true
    ~modnames_conflict:false
    ~f_var:(ren_qualified String.to_snake_case)
    ~f_struct:(ren_qualified cap) ~f_field:(ren_qualified uncap)
    ~f_enum:(ren_qualified cap) ~f_constr:(ren_qualified upper)

module TypMap = Map.Make (struct
  type t = naked_typ

  let compare x y = Type.compare (x, Pos.void) (y, Pos.void)
  let format fmt x = Print.typ fmt (x, Pos.void)
end)

(** Formatting to a list of formatters *)
let pp dest fmt = Format.kdprintf (fun k -> List.iter k dest) fmt

(* Here, [element_name] is the struct field, union member or function parameter
   of which you're printing the type. *)
let rec format_typ
    (decl_ctx : decl_ctx)
    ?(const = false)
    (element_name : Format.formatter -> unit)
    (fmt : Format.formatter)
    (typ : typ) : unit =
  let sconst = if const then "const " else "" in
  match Mark.remove (Type.unquantify typ) with
  | TLit TUnit -> Format.fprintf fmt "CATALA_UNIT%t" element_name
  | TLit TMoney -> Format.fprintf fmt "CATALA_MONEY%t" element_name
  | TLit TInt -> Format.fprintf fmt "CATALA_INT%t" element_name
  | TLit TRat -> Format.fprintf fmt "CATALA_DEC%t" element_name
  | TLit TDate -> Format.fprintf fmt "CATALA_DATE%t" element_name
  | TLit TDuration -> Format.fprintf fmt "CATALA_DURATION%t" element_name
  | TLit TBool -> Format.fprintf fmt "CATALA_BOOL%t" element_name
  | TLit TPos -> Format.fprintf fmt "CATALA_POSITION%t" element_name
  | TTuple [_; (TClosureEnv, _)] ->
    Format.fprintf fmt "%scatala_closure*%t" sconst element_name
  | TTuple ts ->
    Format.fprintf fmt "%sCATALA_TUPLE(%a)%t" sconst
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
         (format_typ decl_ctx ~const:false ignore))
      ts element_name
  | TStruct s ->
    Format.fprintf fmt "%s%s*%t" sconst (StructName.base s) element_name
  | TOption t ->
    Format.fprintf fmt "%sCATALA_OPTION(%a)%t" sconst
      (format_typ decl_ctx ~const:false ignore)
      t element_name
  | TDefault t -> format_typ decl_ctx ~const element_name fmt t
  | TEnum e ->
    Format.fprintf fmt "%s%s*%t" sconst (EnumName.base e) element_name
  | TAbstract t ->
    Format.fprintf fmt "%s%s*%t" sconst (AbstractType.base t) element_name
  | TArrow (t1, t2) ->
    Format.fprintf fmt "@[<hv 4>@[<hov 4>%a@]@,@[<hov 1>(%a)@]@]"
      (format_typ decl_ctx ~const (fun fmt ->
           Format.fprintf fmt "(*%t)" element_name))
      t2
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_typ decl_ctx ~const ignore))
      t1
  | TArray t ->
    Format.fprintf fmt "%sCATALA_ARRAY(%a)%t" sconst
      (format_typ decl_ctx ~const:false ignore)
      t element_name
  | TVar v ->
    Format.fprintf fmt "%svoid * /* any %s */%t" sconst (Bindlib.name_of v)
      element_name
  | TForAll _ | TError -> assert false
  | TClosureEnv -> Format.fprintf fmt "%sCLOSURE_ENV%t" sconst element_name

let is_closure_typ = function
  | TTuple [tf; (TClosureEnv, _)], _ -> Type.is_arrow tf
  | _ -> false

let rec format_rtyp ppf ty =
  match Mark.remove ty with
  | TLit TUnit -> Format.pp_print_string ppf "catala_type_unit"
  | TLit TBool -> Format.pp_print_string ppf "catala_type_bool"
  | TLit TInt -> Format.pp_print_string ppf "catala_type_integer"
  | TLit TMoney -> Format.pp_print_string ppf "catala_type_money"
  | TLit TRat -> Format.pp_print_string ppf "catala_type_decimal"
  | TLit TDate -> Format.pp_print_string ppf "catala_type_date"
  | TLit TDuration -> Format.pp_print_string ppf "catala_type_duration"
  | TLit TPos -> Format.pp_print_string ppf "catala_type_position"
  | TArray ty -> Format.fprintf ppf "catala_type_array(%a)" format_rtyp ty
  | TTuple _ when is_closure_typ ty -> Format.fprintf ppf "catala_type_function"
  | TTuple tl ->
    Format.fprintf ppf "@[<hov 2>catala_type_tuple(%d,@ %a)@]" (List.length tl)
      (Format.pp_print_list format_rtyp ~pp_sep:(fun ppf () ->
           Format.fprintf ppf ",@ "))
      tl
  | TStruct name ->
    Format.fprintf ppf "catala_type__%s()" (StructName.base name)
  | TEnum name -> Format.fprintf ppf "catala_type__%s()" (EnumName.base name)
  | TOption ty -> Format.fprintf ppf "catala_type_optional(%a)" format_rtyp ty
  | TAbstract name ->
    Format.fprintf ppf "catala_type__%s()" (AbstractType.base name)
  | TArrow _ -> Format.fprintf ppf "catala_type_function"
  | TDefault ((_, pos) as ty) ->
    format_rtyp ppf (TOption (TTuple [ty; TLit TPos, pos], pos), pos)
  | TError | TVar _ | TForAll _ -> Format.fprintf ppf "catala_type_poly"
  | TClosureEnv -> assert false (* Should only appear in closure types *)

let format_ctx (type_ordering : TypeIdent.t list) ~ppc ~pph (ctx : ctx) : unit =
  let format_struct_decl ppfs (struct_name, struct_fields) =
    let fields = StructField.Map.bindings struct_fields in
    if fields = [] then
      pp ppfs "@,@[<v 2>typedef void %s;@]" (StructName.base struct_name)
    else
      pp ppfs "@,@[<v 2>typedef struct %s {@ %a@;<1 -2>}@] %s;"
        (StructName.base struct_name)
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           (fun fmt (struct_field, struct_field_type) ->
             Format.fprintf fmt "@[<hov>%a;@]"
               (format_typ ~const:true ctx.decl_ctx (fun fmt ->
                    Format.pp_print_space fmt ();
                    StructField.format fmt struct_field))
               struct_field_type))
        fields
        (StructName.base struct_name);
    let size = List.length fields in
    pp ppfs "@,const catala_type catala_type__%s()"
      (StructName.base struct_name);
    pp (List.tl ppfs) ";";
    Format.fprintf ppc "@,@[<v 2>{@,";
    Format.fprintf ppc "static catala_type ty = {UNINITIALIZED};@,";
    if size > 0 then
      Format.fprintf ppc "static struct catala_label_type fields[%d];@," size;
    Format.fprintf ppc "if (ty.kind != UNINITIALIZED) return ty;@,";
    if size <= 16 then (
      (* More readable version using the runtime function. Could be limited by
         the max number of arguments accepted (at least 127 by the standard) *)
      Format.fprintf ppc "@[<hv 2>return catala_type_struct(&ty, %s, \"%s\", %d"
        (if size > 0 then "fields" else "NULL")
        (StructName.canonical_str ctx.module_name struct_name)
        size;
      List.iter
        (fun (name, ty) ->
          Format.fprintf ppc ",@,\"%a\", %a" StructField.format_original name
            format_rtyp ty)
        fields;
      Format.fprintf ppc ");@]@;<1 -2>}@]")
    else (
      (* Manual array init for unlimited number of fields *)
      Format.fprintf ppc
        "ty.contents.tstruct.name = \"%s\";@,ty.contents.tstruct.size = %d;"
        (StructName.canonical_str ctx.module_name struct_name)
        size;
      if size > 0 then
        Format.fprintf ppc "@,ty.contents.tstruct.fields = fields;";
      List.iteri
        (fun i (name, ty) ->
          Format.fprintf ppc "@,fields[%d].name = \"%a\";" i
            StructField.format_original name;
          Format.fprintf ppc "@,fields[%d].ty = %a;" i format_rtyp ty)
        fields;
      Format.fprintf ppc "@,ty.kind = STRUCT;@,return ty;@;<1 -2>}@]")
  in
  let format_enum_decl ppfs (enum_name, enum_cons) =
    let cases = EnumConstructor.Map.bindings enum_cons in
    if cases = [] then failwith "no constructors in the enum";
    pp ppfs "@,@[<v 2>enum %s__code {@,%a@;<0 -2>}@];@,"
      (EnumName.base enum_name)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (enum_cons, _) -> EnumConstructor.format fmt enum_cons))
      cases;
    pp ppfs
      "@,\
       @[<v 2>typedef struct %s {@ enum %s__code code;@ @[<v 2>union {@ %a@]@,\
       } payload;@]@,\
       } %s;"
      (EnumName.base enum_name) (EnumName.base enum_name)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (enum_cons, typ) ->
           Format.fprintf fmt "@[<hov 2>%a;@]"
             (format_typ ~const:true ctx.decl_ctx (fun fmt ->
                  Format.pp_print_space fmt ();
                  EnumConstructor.format fmt enum_cons))
             typ))
      cases (EnumName.base enum_name);
    let size = List.length cases in
    pp ppfs "@,const catala_type catala_type__%s()" (EnumName.base enum_name);
    pp (List.tl ppfs) ";";
    Format.fprintf ppc "@,@[<v 2>{@,";
    Format.fprintf ppc "static catala_type ty = {UNINITIALIZED};@,";
    Format.fprintf ppc "static struct catala_label_type cases[%d];@," size;
    Format.fprintf ppc "if (ty.kind != UNINITIALIZED) return ty;@,";
    if size <= 16 then (
      (* More readable version using the runtime function. Could be limited by
         the max number of arguments accepted (at least 127 by the standard) *)
      Format.fprintf ppc
        "@[<hv 2>return catala_type_enum(&ty, cases, \"%s\", %d"
        (EnumName.canonical_str ctx.module_name enum_name)
        size;
      List.iter
        (fun (name, ty) ->
          Format.fprintf ppc ",@,\"%a\", %a" EnumConstructor.format_original
            name format_rtyp ty)
        cases;
      Format.fprintf ppc ");@]@;<1 -2>}@]")
    else (
      (* Manual array init for unlimited number of cases *)
      Format.fprintf ppc
        "ty.contents.tenum.name = \"%s\";@,ty.contents.tenum.size = %d;@,"
        (EnumName.canonical_str ctx.module_name enum_name)
        size;
      if size > 0 then Format.fprintf ppc "ty.contents.tenum.cases = cases;";
      List.iteri
        (fun i (name, ty) ->
          Format.fprintf ppc "@,cases[%d].name = \"%a\";" i
            EnumConstructor.format name;
          Format.fprintf ppc "@,cases[%d].ty = %a;" i format_rtyp ty)
        cases;
      Format.fprintf ppc "@,ty.kind = ENUM;@,return ty;@;<1 -2>}@]")
  in
  let scope_structs =
    List.fold_left
      (fun acc -> function
        | TypeIdent.Struct s -> StructName.Map.remove s acc | _ -> acc)
      ctx.decl_ctx.ctx_structs type_ordering
    |> StructName.Map.keys
    |> List.map (fun s -> TypeIdent.Struct s)
  in
  let format_abstract_decl ppfs tid =
    let id = AbstractType.base tid in
    pp ppfs
      "@[<v 2>typedef const void* %s; /* The type must be a pointer to const \
       */@]@,"
      id;
    Format.fprintf ppc
      "@,\
       @[<v 2>int catala_type__%s_equal(@[<hv>const catala_code_position* \
       pos,@ const %s t1,@ const %s t2@]) {@,\
       /* ... */@;\
       <1 -2>}@]"
      id id id;
    Format.fprintf ppc
      "@,\
       @[<v 2>int catala_type__%s_compare(@[<hv>const catala_code_position* \
       pos,@ const %s t1,@ const %s t2@]) {@,\
       /* ... */@;\
       <1 -2>}@]"
      id id id;
    Format.fprintf ppc
      "@,\
       @[<v 2>void catala_type__%s_print(@[<hv>struct catala_buf buf,@ const \
       %s t@]) {@,\
       /* ... */@;\
       <1 -2>}@]"
      id id;
    Format.fprintf ppc
      "@,\
       @[<v 2>void catala_type__%s_to_json(@[<hv>struct catala_buf buf,@ const \
       %s t@]) {@,\
       /* ... */@;\
       <1 -2>}@]"
      id id;
    Format.fprintf ppc
      "@,\
       @[<v 2>const %s catala_type__%s_from_json(const char *) {@,\
       /* ... */@;\
       <1 -2>}@]"
      id id;

    pp ppfs "@,@,/* This should be left unchanged */";
    pp ppfs "@,const catala_type catala_type__%s()" (AbstractType.base tid);
    pp (List.tl ppfs) ";";
    Format.fprintf ppc "@,@[<v 2>{";
    Format.fprintf ppc "@,static catala_type ty = {UNINITIALIZED};";
    Format.fprintf ppc "@,if (ty.kind != UNINITIALIZED) return ty;";
    Format.fprintf ppc "@,@[<hv 2>ty.contents.texternal.name =@ %S;@]" id;
    Format.fprintf ppc
      "@,\
       @[<hv 2>ty.contents.texternal.equal =@ (int (*)(const \
       catala_code_position *, const void *, const void *))@,\
       &catala_type__%s_equal;@]"
      id;
    Format.fprintf ppc
      "@,\
       @[<hv 2>ty.contents.texternal.compare =@ (int (*)(const \
       catala_code_position *, const void *, const void *))@,\
       &catala_type__%s_compare;@]"
      id;
    Format.fprintf ppc
      "@,\
       @[<hv 2>ty.contents.texternal.print =@ (void (*)(struct catala_buf, \
       const void *))@,\
       &catala_type__%s_print;@]"
      id;
    Format.fprintf ppc
      "@,\
       @[<hv 2>ty.contents.texternal.to_json =@ (void (*)(struct catala_buf, \
       const void *))@,\
       &catala_type__%s_to_json;@]"
      id;
    Format.fprintf ppc
      "@,\
       @[<hv 2>ty.contents.texternal.from_json =@ (void * (*)(const \
       catala_code_position *, const char *))@,\
       &catala_type__%s_from_json;@]"
      id;
    Format.fprintf ppc "@,ty.kind = EXTERNAL;@,return ty;@;<1 -2>}@]"
  in
  List.iter
    (fun struct_or_enum ->
      let ppfs =
        ppc
        ::
        (if TypeIdent.Set.mem struct_or_enum ctx.decl_ctx.ctx_public_types then
           [pph]
         else [])
      in
      match struct_or_enum with
      | TypeIdent.Struct s ->
        if StructName.path s = [] then (
          let def = StructName.Map.find s ctx.decl_ctx.ctx_structs in
          pp ppfs "@,";
          format_struct_decl ppfs (s, def))
      | TypeIdent.Enum e ->
        if EnumName.path e = [] && not (EnumName.equal e Expr.option_enum) then (
          let def = EnumName.Map.find e ctx.decl_ctx.ctx_enums in
          pp ppfs "@,";
          format_enum_decl ppfs (e, def))
      | TypeIdent.Abstract tid ->
        if AbstractType.path tid = [] then (
          pp ppfs "@,";
          format_abstract_decl ppfs tid))
    (type_ordering @ scope_structs)

(* Be safe and assume 32bit integers for literal constants *)
let in_bounds i =
  let min_int = Z.of_int (-0x80000000) in
  let max_int = Z.of_int 0x7fffffff in
  Z.leq min_int i && Z.leq i max_int

let format_lit (fmt : Format.formatter) (l : lit Mark.pos) : unit =
  match Mark.remove l with
  | LBool true -> Format.pp_print_string fmt "CATALA_TRUE"
  | LBool false -> Format.pp_print_string fmt "CATALA_FALSE"
  | LInt i ->
    if in_bounds i then
      Format.fprintf fmt "catala_new_int(%s)" (Runtime.integer_to_string i)
    else
      Format.fprintf fmt "catala_new_int_str(\"%s\")"
        (Runtime.integer_to_string i)
  | LUnit -> Format.pp_print_string fmt "CATALA_UNITVAL"
  | LRat q ->
    if in_bounds (Q.num q) && in_bounds (Q.den q) then
      Format.fprintf fmt "catala_new_frac(%s, %s)"
        (Z.to_string (Q.num q))
        (Z.to_string (Q.den q))
    else Format.fprintf fmt "catala_new_dec_str(\"%s\")" (Q.to_string q)
  | LMoney e ->
    if in_bounds e then
      Format.fprintf fmt "catala_new_money(%s)"
        (Runtime.integer_to_string (Runtime.money_to_cents e))
    else
      Format.fprintf fmt "catala_new_money_str(\"%s\")"
        (Runtime.integer_to_string (Runtime.money_to_cents e))
  | LDate d ->
    let y, m, d = Runtime.date_to_years_months_days d in
    Format.fprintf fmt "catala_new_date(%d,%d,%d)" y m d
  | LDuration dt ->
    let y, m, d = Runtime.duration_to_years_months_days dt in
    Format.fprintf fmt "catala_new_duration(%d,%d,%d)" y m d

let format_op (fmt : Format.formatter) (op : operator Mark.pos) : unit =
  match Mark.remove op with
  | Log (_entry, _infos) -> assert false
  | FromClosureEnv | ToClosureEnv -> assert false
  | Add_dat_dur _ -> assert false (* needs specific printing *)
  | op -> Format.fprintf fmt "@{<blue;bold>%s@}" (Operator.name op)

let rec format_expression
    (ctx : ctx)
    (env : env)
    (fmt : Format.formatter)
    (e : expr) : unit =
  let format_expression = format_expression ctx env in
  match Mark.remove e with
  | EVar v ->
    if VarName.Set.mem v env.global_vars then
      Format.fprintf fmt "%a()" VarName.format v
    else VarName.format fmt v
  | EFunc f -> FuncName.format fmt f
  | EStructFieldAccess { e1; field; _ } ->
    let lpar, rpar =
      match e1 with
      | EVar _, _ | EStructFieldAccess _, _ -> "", ""
      | _ -> "(", ")"
    in
    Format.fprintf fmt "%s%a%s->%s" lpar format_expression e1 rpar
      (StructField.to_string field)
  | EInj { e1; cons; name = enum_name; _ }
    when EnumName.equal enum_name Expr.option_enum ->
    if EnumConstructor.equal cons Expr.none_constr then
      Format.fprintf fmt "CATALA_NONE"
    else Format.fprintf fmt "catala_some(%a)" format_expression e1
  | EStruct _ | EInj _ | EArray _ ->
    Message.error ~internal:true "Unlifted construct found: %a"
      (Scalc__Print.format_expr ctx.decl_ctx ?debug:None)
      e
    (* Should always be handled at the root of a statement *)
  | ELit l -> Format.fprintf fmt "%a" format_lit (Mark.copy e l)
  | EPosLit ->
    (* Note: this is not an expression, but an initialisation string. It should
       only appear in the global [loc] table *)
    let pos = Mark.get e in
    Format.fprintf fmt "@[<hv 2>{%S,@ %d, %d, %d, %d}@]" (Pos.get_file pos)
      (Pos.get_start_line pos) (Pos.get_start_column pos) (Pos.get_end_line pos)
      (Pos.get_end_column pos)
  | EAppOp { op = ToClosureEnv, _; args = [arg]; _ } ->
    Format.fprintf fmt "((catala_closure *)%a)" format_expression arg
  | EAppOp { op = FromClosureEnv, _; args = [arg]; _ } ->
    Format.fprintf fmt "((CATALA_TUPLE(_))%a)" format_expression arg
  | EAppOp { op = ConstructorCheck (enum, constr), _; args = [arg]; _ } ->
    if EnumName.equal enum Expr.option_enum then
      Format.fprintf fmt "catala_new_bool((%a)->code == %s)" format_expression
        arg
        (if EnumConstructor.equal constr Expr.none_constr then
           "catala_option_none"
         else "catala_option_some")
    else
      Format.fprintf fmt "catala_new_bool((%a)->code == %s)" format_expression
        arg
        (EnumConstructor.to_string constr)
  | EAppOp { op = ((Map | Filter), _) as op; args = [arg1; arg2]; _ } ->
    Format.fprintf fmt "%a(%a,@ %a)" format_op op format_expression arg1
      format_expression arg2
  | EAppOp
      {
        op = (Fold, _) as op;
        args = [fct; base; arr];
        tys = [(TArrow (_, rty), _); _; _];
      } ->
    (* Operators with a polymorphic return type need a cast *)
    Format.fprintf fmt "((%a)%a(%a,@ %a,@ %a))"
      (format_typ ~const:true ctx.decl_ctx ignore)
      rty format_op op format_expression fct format_expression base
      format_expression arr
  | EAppOp
      {
        op = (Find, _) as op;
        args = [fct; arr];
        tys = [(TArrow ([ty], _), _); _];
      } ->
    (* Operators with a polymorphic return type need a cast *)
    Format.fprintf fmt "((%a)%a(%a,@ %a))"
      (format_typ ~const:true ctx.decl_ctx ignore)
      (TOption ty, Pos.void) format_op op format_expression fct
      format_expression arr
  | EAppOp
      {
        op = (Reduce, _) as op;
        args = [fct; arr];
        tys = [(TArrow (_, rty), _); _];
      } ->
    (* Operators with a polymorphic return type need a cast *)
    Format.fprintf fmt "((%a)%a(%a,@ %a))"
      (format_typ ~const:true ctx.decl_ctx ignore)
      (TOption rty, Pos.void) format_op op format_expression fct
      format_expression arr
  | EAppOp
      {
        op = (Sort _, _) as op;
        args = [pos; fct; arr];
        tys = [_; (TArrow (_, rty), _); _];
      } ->
    Format.fprintf fmt "%a(%a,@ %a,@ %a,@ %a)" format_op op format_rtyp rty
      format_expression pos format_expression fct format_expression arr
  | EAppOp
      { op = ((Add_dat_dur rounding | Sub_dat_dur rounding) as op), _; args; _ }
    ->
    Format.fprintf fmt "%s(%s,@ %a)"
      (match op with
      | Add_dat_dur _ -> "o_add_dat_dur"
      | Sub_dat_dur _ -> "o_sub_dat_dur"
      | _ -> assert false)
      (match rounding with
      | RoundUp -> "dc_date_round_up"
      | RoundDown -> "dc_date_round_down"
      | AbortOnRound -> "dc_date_round_abort")
      (Format.pp_print_list format_expression ~pp_sep:(fun ppf () ->
           Format.fprintf ppf ",@ "))
      args
  | EApp { f; args; _ } ->
    let format_fun fmt = function
      | EExternal { name; _ }, _ ->
        Format.pp_print_string fmt (Mark.remove name)
      | EFunc f, _ -> FuncName.format fmt f
      | ETupleAccess { e1; index = 0; typ }, _ when Type.is_arrow typ ->
        let typ = Type.unquantify typ in
        Format.fprintf fmt "@[<hov 1>((%a)@,%a->funcp)@]"
          (format_typ ~const:true ctx.decl_ctx ignore)
          typ format_expression e1
      | (_, pos) as e ->
        Message.error ~internal:true ~pos "Cannot apply %a"
          (Scalc__Print.format_expr ctx.decl_ctx ?debug:None)
          e
    in
    Format.fprintf fmt "@[<hov 2>%a@,(@[<hov 0>%a)@]@]" format_fun f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         format_expression)
      args
  | EAppOp
      { op = ((Eq | Lt | Lte | Gt | Gte), _) as op; args; tys = [_pos; ty; _] }
    ->
    Format.fprintf fmt "%a(@[<hov 0>%a,@ %a)@]" format_op op format_rtyp ty
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         format_expression)
      args
  | EAppOp { op = ((And | Or) as op), _; args; _ } ->
    Format.fprintf fmt "catala_new_bool(@[<hov 0>%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () ->
           Format.fprintf fmt " %s@ "
             (match op with And -> "&&" | Or -> "||" | _ -> assert false))
         (fun fmt e -> Format.fprintf fmt "*(%a)" format_expression e))
      args
  | EAppOp { op = ArrayAccess index, _; args = [(EVar v, _)]; _ }
    when Option.equal VarName.equal env.locs (Some v) ->
    Format.fprintf fmt "%a[%d]" VarName.format v index
  | EAppOp { op = ArrayAccess index, _; args = [e]; _ } ->
    Format.fprintf fmt "%a->elements[%d]" format_expression e index
  | EAppOp
      {
        op = ValueFromJson (ty, json), _;
        args = [pos_expr; (ELit LUnit, _)];
        _;
      } ->
    Format.fprintf fmt "catala_fromjson(%a, %a, %s)" format_rtyp ty
      format_expression pos_expr (String.quote json)
  | EAppOp { op; args; _ } ->
    Format.fprintf fmt "%a(@[<hov 0>%a)@]" format_op op
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         format_expression)
      args
  | ETuple _ -> assert false (* Must be a statement *)
  | ETupleAccess { e1; index = 1; typ = TClosureEnv, _ } ->
    Format.fprintf fmt "%a->env" format_expression e1
  | ETupleAccess { e1; index; typ } ->
    Format.fprintf fmt "((%a)(%a[%d].content))"
      (format_typ ctx.decl_ctx ignore)
      typ
      (fun ppf -> function
        | (EStructFieldAccess { name; _ }, _) as e
          when name = Expr.option_struct ->
          Format.fprintf ppf "((CATALA_TUPLE(_))%a)" format_expression e
        | e -> format_expression ppf e)
      e1 index
  | EExternal { name; _ } ->
    (* The name has already been properly qualified in [Renaming] *)
    Format.fprintf fmt "%s()" (Mark.remove name)

let rec format_statement
    (ctx : ctx)
    (env : env)
    (fmt : Format.formatter)
    (s : stmt Mark.pos) : unit =
  match Mark.remove s with
  | SInnerFuncDef _ ->
    Message.error ~pos:(Mark.get s) ~internal:true
      "These inner functions should have been hoisted in Scalc: %a"
      (fun ppf e -> Scalc__Print.format_statement ctx.decl_ctx ppf e)
      s
  | SLocalInit { name = v, _; typ; expr = EStruct { fields; _ }, _ }
    when StructField.Map.is_empty fields && not (is_dummy_var v) ->
    Format.fprintf fmt "@,@[<hov 2>%a =@ NULL@];"
      (format_typ ~const:true ctx.decl_ctx (fun fmt -> VarName.format fmt v))
      typ
  | SLocalInit { name = v, _; typ; expr = ETuple [], _ }
    when not (is_dummy_var v) ->
    Format.fprintf fmt "@,@[<hov 2>%a =@ NULL@];"
      (format_typ ~const:true ctx.decl_ctx (fun fmt ->
           Format.pp_print_space fmt ();
           VarName.format fmt v))
      typ
  | SLocalDef
      { name = v, _; expr = EArray locs, _; typ = TArray (TLit TPos, _), _ } ->
    let len = List.length locs in
    (* Nicer printing for the locations table *)
    Format.fprintf fmt
      "@,@[<hv 2>static const catala_code_position locs[][1] = {";
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ',')
      (fun ppf -> Format.fprintf ppf "@ {%a}" (format_expression ctx env))
      fmt locs;
    Format.fprintf fmt
      "@;<1 -2>};@]@,%a->size = %d;@,%a->elements = (const void **)locs;"
      VarName.format v len VarName.format v
  | SLocalDecl { name = v, _; typ = ty } ->
    if is_dummy_var v then ()
    else
      Format.fprintf fmt "@,@[<hov 2>%a@];"
        (format_typ ctx.decl_ctx ~const:true (fun fmt ->
             Format.pp_print_space fmt ();
             VarName.format fmt v))
        ty
  | SLocalDef { name = v, _; expr = EArray elts, _; _ } ->
    (* We detect array initializations which have special treatment. *)
    let size = List.length elts in
    if size <= 16 then
      (* The C standard allows up to 127 arguments, but let's not be greedy *)
      Format.fprintf fmt "@,@[<hv 2>catala_set_array(%a, %d%a);@]"
        VarName.format v size
        (Format.pp_print_list
           ~pp_sep:(fun _ () -> ())
           (fun ppf -> Format.fprintf ppf ",@ %a" (format_expression ctx env)))
        elts
    else (
      Format.fprintf fmt "@,@[<hov 2>%a->size =@ %d;@]" VarName.format v size;
      if size > 0 then
        Format.fprintf fmt
          "@,@[<hov 2>%a->elements = catala_malloc(%d * sizeof(void*));@]"
          VarName.format v size;
      List.iteri
        (fun i arg ->
          Format.fprintf fmt "@,@[<hov 2>%a->elements[%d] =@ %a;@]"
            VarName.format v i
            (format_expression ctx env)
            arg)
        elts)
  | SLocalDef { name = v, _; expr = EStruct { fields; _ }, _; _ } ->
    StructField.Map.iter
      (fun field expr ->
        Format.fprintf fmt "@,@[<hov 2>%a->%s =@ %a;@]" VarName.format v
          (StructField.to_string field)
          (format_expression ctx env)
          expr)
      fields
  | SLocalDef { name = v, _; expr = EInj { e1; cons; name; _ }, _; _ }
    when not (EnumName.equal name Expr.option_enum) ->
    Format.fprintf fmt "@,@[<hov 2>%a->code = %s;@]" VarName.format v
      (EnumConstructor.to_string cons);
    Format.fprintf fmt "@,@[<hov 2>%a->payload.%s = %a;@]" VarName.format v
      (EnumConstructor.to_string cons)
      (format_expression ctx env)
      e1
  | SLocalDef
      {
        name = v, _;
        expr = ETuple [fct; cls_env], _;
        typ = TTuple [tfun; (TClosureEnv, _)], _;
      }
    when Type.is_arrow tfun ->
    (* We detect closure initializations which have special treatment. *)
    Format.fprintf fmt "@,@[<hov 2>%a->funcp =@ (void (*)(void))%a;@]"
      VarName.format v
      (format_expression ctx env)
      fct;
    Format.fprintf fmt "@,@[<hov 2>%a->env =@ (void*)%a;@]" VarName.format v
      (format_expression ctx env)
      cls_env
  | SLocalDef { name = v, _; expr = ETuple elts, _; _ } ->
    (* We detect tuple initializations which have special treatment. *)
    List.iteri
      (fun i arg ->
        Format.fprintf fmt "@,@[<hov 2>%a[%d].content =@ %a;@]" VarName.format v
          i
          (format_expression ctx env)
          arg)
      elts
  | SLocalInit
      {
        name = v;
        typ;
        expr =
          ( EAppOp
              {
                op = ((FromClosureEnv | ToClosureEnv) as op), _;
                args = [(EVar _, _)];
                _;
              },
            _ ) as e;
      } ->
    let cast =
      match op with
      | FromClosureEnv -> "CATALA_TUPLE(_)"
      | ToClosureEnv -> "catala_closure *"
      | _ -> assert false
    in
    Format.fprintf fmt "@,@[<hov 2>%a =@ (const %s)(%a);@]"
      (format_typ ~const:true ctx.decl_ctx (fun fmt ->
           Format.pp_print_space fmt ();
           VarName.format fmt (Mark.remove v)))
      typ cast
      (format_expression ctx env)
      e
  | SLocalInit { name = v; typ; expr = e } ->
    (* Handling at the block level guarantees that [e] is supported as initial
       value *)
    Format.fprintf fmt "@,@[<hov 2>%a =@ %a;@]"
      (format_typ ~const:true ctx.decl_ctx (fun fmt ->
           Format.pp_print_space fmt ();
           VarName.format fmt (Mark.remove v)))
      typ
      (format_expression ctx env)
      e
  | SLocalDef { name = v; expr = e; _ } ->
    Format.fprintf fmt "@,@[<hov 2>%a = %a;@]" VarName.format (Mark.remove v)
      (format_expression ctx env)
      e
  | SFatalError { pos_expr; error } ->
    Format.fprintf fmt
      "@,@[<hov 2>catala_error(catala_%s,@ %a,@ 1,@ %s);@]@,abort();"
      (String.to_snake_case (Runtime.error_to_string error))
      (format_expression ctx env)
      pos_expr
      (match
         Pos.get_attr (Mark.get s) (function
           | ErrorMessage m -> Some m
           | _ -> None)
       with
      | None -> "NULL"
      | Some m -> String.quote m)
  | SIfThenElse _ ->
    Format.fprintf fmt "@,@[<hv 2>%a@]" (format_ite ctx env) [s]
  | SSwitch { switch_var; enum_name = e_name; switch_cases = cases; _ } ->
    if EnumName.equal e_name Expr.option_enum then
      Format.fprintf fmt "@,@[<hv 2>%a@]" (format_ite ctx env) [s]
    else
      let () =
        Format.fprintf fmt "@,@[<v 2>@[<hov 4>switch (%a->code) {@]"
          VarName.format switch_var
      in
      List.iter2
        (fun { case_block; payload_var_name; payload_var_typ } (cons_name, _) ->
          Format.fprintf fmt "@,@[<v 2>case %a: {" EnumConstructor.format
            cons_name;
          if
            (not (Type.equal payload_var_typ (TLit TUnit, Pos.void)))
            && not (is_dummy_var payload_var_name)
          then
            Format.fprintf fmt "@ @[<hov 2>%a = %a->payload.%a;@]"
              (format_typ ctx.decl_ctx ~const:true (fun fmt ->
                   Format.pp_print_space fmt ();
                   VarName.format fmt payload_var_name))
              payload_var_typ VarName.format switch_var EnumConstructor.format
              cons_name;
          Format.fprintf fmt "%a@ break;@;<1 -2>}@]" (format_block ctx env)
            case_block)
        cases
        (EnumConstructor.Map.bindings
           (EnumName.Map.find e_name ctx.decl_ctx.ctx_enums));
      Format.fprintf fmt "@,@[<v 2>default:@,abort();@]";
      Format.fprintf fmt "@;<0 -2>}";
      Format.pp_close_box fmt ()
  | SReturn e1 ->
    Format.fprintf fmt "@,@[<hov 2>return %a;@]" (format_expression ctx env) e1
  | _ -> .

and format_ite (ctx : ctx) (env : env) (fmt : Format.formatter) (b : block) :
    unit =
  let format_else fmt = function
    | [(SLocalDef { expr = ELit LUnit, _; _ }, _)]
    | [(SReturn (ELit LUnit, _), _)] ->
      ()
    | else_block ->
      Format.fprintf fmt " else %a" (format_ite ctx env) else_block
  in
  match b with
  | [(SIfThenElse { if_expr = ELit (LBool true), _; then_block = b; _ }, _)]
  | [(SIfThenElse { if_expr = ELit (LBool false), _; else_block = b; _ }, _)] ->
    format_ite ctx env fmt b
  | [
   ( SIfThenElse
       ({ if_expr = EAppOp { op = Not, _; args = [a]; _ }, _; _ } as ite),
     _ );
  ] ->
    Format.fprintf fmt "@[<hov 4>if (CATALA_FALSE ==@ %a) {@]"
      (format_expression ctx env)
      a;
    format_block ctx env fmt ite.then_block;
    Format.fprintf fmt "@;<1 -2>}%a" format_else ite.else_block
  | [(SIfThenElse ite, _)] ->
    Format.fprintf fmt "@[<hov 4>if (CATALA_TRUE ==@ %a) {@]"
      (format_expression ctx env)
      ite.if_expr;
    format_block ctx env fmt ite.then_block;
    Format.fprintf fmt "@;<1 -2>}%a" format_else ite.else_block
  | [(SSwitch { switch_var; enum_name = e_name; switch_cases = cases; _ }, _)]
    when EnumName.equal e_name Expr.option_enum ->
    let cases =
      List.map2
        (fun x (cons, _) -> x, cons)
        cases
        (EnumConstructor.Map.bindings
           (EnumName.Map.find e_name ctx.decl_ctx.ctx_enums))
    in
    let some_case, none_case =
      match
        List.partition
          (fun (_, cons) -> EnumConstructor.equal cons Expr.some_constr)
          cases
      with
      | [(some, _)], [(none, _)] -> some, none
      | _ -> assert false
    in
    Format.fprintf fmt "if (%a->code == catala_option_some) {" VarName.format
      switch_var;
    Format.fprintf fmt "@ @[<hov 2>%a = %a->payload;@]"
      (format_typ ctx.decl_ctx ~const:true (fun fmt ->
           Format.pp_print_space fmt ();
           VarName.format fmt some_case.payload_var_name))
      some_case.payload_var_typ VarName.format switch_var;
    format_block ctx env fmt some_case.case_block;
    Format.fprintf fmt "@;<1 -2>} else ";
    format_ite ctx env fmt none_case.case_block
  | _ -> Format.fprintf fmt "{%a@;<1 -2>}" (format_block ctx env) b

and format_block (ctx : ctx) (env : env) (fmt : Format.formatter) (b : block) :
    unit =
  (* C89 doesn't accept initialisations of constructions from non-constants: -
     for known structures needing malloc, provision the malloc here (turn Decl
     into Init (that will only do the malloc) + def) - for literal constants
     keep init - otherwise split Init into decl + def *)
  let requires_malloc = function
    | EInj { name; _ }, _ when EnumName.equal name Expr.option_enum -> false
    | (EArray _ | EStruct _ | EInj _ | ETuple _), _ -> true
    | _ -> false
  in
  let print_init_malloc fmt const_pointer v typ =
    let const, pp_size =
      match Mark.remove (Type.unquantify typ) with
      | TArray _ ->
        false, fun fmt -> Format.pp_print_string fmt "sizeof(catala_array)"
      | TStruct name ->
        if
          StructField.Map.is_empty
            (StructName.Map.find name ctx.decl_ctx.ctx_structs)
        then false, fun fmt -> Format.fprintf fmt "0"
        else
          ( false,
            fun fmt -> Format.fprintf fmt "sizeof(%s)" (StructName.base name) )
      | TEnum name ->
        false, fun fmt -> Format.fprintf fmt "sizeof(%s)" (EnumName.base name)
      | TTuple _ when is_closure_typ typ ->
        false, fun fmt -> Format.pp_print_string fmt "sizeof(catala_closure)"
      | TTuple ts ->
        ( false,
          fun fmt ->
            Format.fprintf fmt "%d * sizeof(tuple_element*)" (List.length ts) )
      | _ ->
        Message.error ~internal:true
          "Invalid type for malloc: variable %a, type %a" VarName.format v
          Print.typ typ
    in
    (* Postfix [const] declares that the pointer is const, but not its
       contents *)
    Format.fprintf fmt "@,@[<hov 2>%a =@ catala_malloc(%t)@];"
      (format_typ ~const ctx.decl_ctx (fun fmt ->
           if const_pointer then Format.pp_print_string fmt " const";
           Format.pp_print_space fmt ();
           VarName.format fmt v))
      typ pp_size
  in
  (* C89 requires declarations to be on top of the block *)
  let rec format_decls defined_vars remaining = function
    | (SLocalDecl { name; _ }, _)
      :: (SLocalDef { name = n1; typ; expr }, m)
      :: r
      when Mark.equal VarName.equal name n1 ->
      format_decls defined_vars remaining
        ((SLocalInit { name; typ; expr }, m) :: r)
    | ((SLocalDecl _, _) as decl) :: r ->
      format_statement ctx env fmt decl;
      format_decls defined_vars remaining r
    | ((SLocalInit { name; typ; expr }, m) as init) :: r ->
      if requires_malloc expr then (
        print_init_malloc fmt true (Mark.remove name) typ;
        format_decls defined_vars
          ((SLocalDef { name; typ; expr }, m) :: remaining)
          r)
      else if VarName.Set.subset (Utils.get_vars expr) defined_vars then (
        format_statement ctx env fmt init;
        format_decls
          (VarName.Set.add (Mark.remove name) defined_vars)
          remaining r)
      else (
        (* The init depends on undefined variables, it can't be moved to the
           top, so we split it into decl + def *)
        format_statement ctx env fmt (SLocalDecl { name; typ }, m);
        format_decls defined_vars
          ((SLocalDef { name; typ; expr }, m) :: remaining)
          r)
    | stmt :: r -> format_decls defined_vars (stmt :: remaining) r
    | [] -> List.rev remaining
  in
  match List.find_opt (function SFatalError _, _ -> true | _ -> false) b with
  | Some ((SFatalError { pos_expr = EVar vpos, _; _ }, _) as fatal) ->
    (* avoid printing dead code: only print the fatal error (this also avoids
       warnings about unused or undefined variables) *)
    let pos_def =
      List.find_opt
        (function
          | SLocalInit { name = v, _; _ }, _ -> VarName.equal v vpos
          | _ -> false)
        b
    in
    Option.iter (format_statement ctx env fmt) pos_def;
    format_statement ctx env fmt fatal;
    Format.fprintf fmt "@,abort();" (* unreachable, but avoids a warning *)
  | _ ->
    let remaining =
      format_decls (VarName.Set.union env.global_vars env.local_vars) [] b
    in
    List.iter (format_statement ctx env fmt) remaining

let format_code_item ctx ~ppc ~pph env = function
  | SVar
      {
        var;
        expr = EArray locs, _;
        typ = TArray (TLit TPos, _), _;
        visibility = _;
      } ->
    (* Special case to print the locations table in a simpler way *)
    pp [pph] "@,@[<h>extern const catala_code_position %a[][1];@]@,"
      VarName.format var;
    pp [ppc] "@,@[<v 2>@[<h>const catala_code_position %a" VarName.format var;
    pp [ppc] "[][1] = {@]";
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ',')
      (fun ppf loc ->
        Format.fprintf ppf "@,{%a}" (format_expression ctx env) loc)
      ppc locs;
    pp [ppc] "@;<1 -2>};@]@,";
    {
      env with
      locs = Some var;
      global_vars = VarName.Set.add var env.global_vars;
    }
  | SVar { var; expr; typ; visibility } ->
    (* Global variables are turned into inline functions without parameters that
       perform lazy evaluation: {[ inline foo_type foo() { static foo_type foo =
       NULL; return (foo ? foo : foo = foo_init()); } ]} NOTE: "inline" is not
       defined in C89 *)
    let pp_intf = if visibility = Public then [pph] else [] in
    pp (ppc :: pp_intf) "@,@[<v 2>@[<hov 4>%s%a"
      (if visibility = Public then "" else "static ")
      (format_typ ~const:true ctx.decl_ctx (fun fmt ->
           Format.pp_print_space fmt ();
           VarName.format fmt var))
      typ;
    pp pp_intf " ();@]@]@,";
    pp [ppc] " () {@]@,";
    pp [ppc] "@[<hov 2>static %a = NULL;@]@,"
      (format_typ ~const:true ctx.decl_ctx (fun fmt ->
           Format.pp_print_space fmt ();
           VarName.format fmt var))
      typ;
    pp [ppc] "@[<hov 2>return CATALA_GET_LAZY(%a, %a);@]"
      (* This does (foo ? foo : foo = foo_init()), but enabling persistent
         allocation around the init *)
      (* FIXME: the proper solution would be to do a deep copy of the allocated
         object from the Catala heap to the persistent heap instead of switching
         allocation mode (which could persist intermediate values) *)
      VarName.format var
      (format_expression ctx env)
      expr;
    pp [ppc] "@;<1 -2>}@]@,";
    { env with global_vars = VarName.Set.add var env.global_vars }
  | SFunc { var; func; visibility }
  | SScope
      {
        scope_body_var = var;
        scope_body_func = func;
        scope_body_visibility = visibility;
        _;
      } ->
    let { func_params; func_body; func_return_typ } = func in
    let local_vars =
      VarName.Set.of_list (List.map (fun (v, _) -> Mark.remove v) func_params)
    in
    let pp_intf = if visibility = Public then [pph] else [] in
    pp (ppc :: pp_intf) "@,@[<v 2>@[<hov 4>%s%a@ @[<hv 1>(%a)@]@]"
      (if visibility = Public then "" else "static ")
      (format_typ ~const:true ctx.decl_ctx (fun fmt ->
           Format.pp_print_space fmt ();
           FuncName.format fmt var))
      func_return_typ
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (var, typ) ->
           Format.pp_open_hovbox fmt 2;
           (format_typ ~const:true ctx.decl_ctx (fun fmt ->
                Format.pp_print_space fmt ();
                VarName.format fmt (Mark.remove var)))
             fmt typ;
           Format.pp_close_box fmt ()))
      func_params;
    pp pp_intf "@];@,";
    pp [ppc] "@;<1 -2>{%a@]@,}@,"
      (format_block ctx { env with local_vars })
      func_body;
    env

let format_main ctx env (fmt : Format.formatter) (p : Ast.program) =
  Format.pp_open_vbox fmt 0;
  let t_defs, tests = p.tests in
  let env =
    List.fold_left
      (format_code_item ctx ~ppc:fmt ~pph:(Message.ignore_ppf ()))
      env t_defs
  in
  Format.fprintf fmt "@,@[<v 2>void* run_tests ()@;<0 -2>{";
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
         (Format.pp_print_list ~pp_sep:Format.pp_print_space
            (fun ppf (s, _, _) -> ScopeName.format ppf s))
         tests
     in
     List.iter
       (fun (name, var, block) ->
         Format.fprintf fmt "@,@[<v 2>{ /* Test for scope %a */"
           ScopeName.format name;
         format_block ctx env fmt block;
         Format.fprintf fmt
           "@,\
            fprintf(stderr,\"\\x1b[32m[RESULT]\\x1b[m Scope %a executed \
            successfully.\\n\");"
           ScopeName.format_original name;
         let ty =
           ( TStruct
               (ScopeName.Map.find name ctx.decl_ctx.ctx_scopes).out_struct_name,
             Pos.void )
         in
         Format.fprintf fmt
           "@,\
            catala_print(catala_stdbuf, embed(%a, %a));@,\
            catala_stdbuf.flush();@;\
            <1 -2>}@]"
           format_rtyp ty VarName.format var)
       tests);
  Format.fprintf fmt "@,return (void*)1;@;<1 -2>}@]@,";
  Format.fprintf fmt "@,@[<v 2>int main (int argc, char** argv)@;<0 -2>{";
  Format.fprintf fmt "@,void* result = catala_do(&run_tests);";

  Format.fprintf fmt "@,return !result;@;<1 -2>}@]"

let format_program
    output_file
    ppc
    (p : Ast.program)
    (type_ordering : TypeIdent.t list) : unit =
  File.with_secondary_out_channel ~output_file ~ext:"h"
  @@ fun h_file pph ->
  File.with_secondary_out_channel
    ~output_file:(if snd p.tests = [] then None else output_file)
    ~ext:"+main.c"
  @@ fun _ ppmain ->
  let ppall = pp [ppc; pph; ppmain] in
  Fun.protect ~finally:(fun () -> ppall "@.")
  @@ fun () ->
  ppall "@[<v>";
  if Global.options.gen_external then
    ppall
      "/* This is a template file following the expected interface and \
       declarations to\n\
      \ * implement the corresponding Catala module.\n\
      \ *\n\
      \ * You should replace all `catala_error(catala_impossible)` \
       place-holders with\n\
      \ * your implementation and rename it to remove the \".template\" \
       suffix. */@,"
  else
    ppall
      "/* This file has been generated by the Catala compiler, do not edit! */@,";
  pp [ppc; ppmain]
    "@,#include <stdio.h>@,#include <stdlib.h>@,#include <catala_runtime.h>@,";

  let module_id =
    match p.module_name, output_file with
    | None, None -> "MAIN"
    | None, Some f ->
      String.uppercase_ascii
        (String.to_id (File.basename (File.remove_extension f)))
    | Some (m, _), _ ->
      String.uppercase_ascii (String.to_ascii (ModuleName.to_string m))
  in
  Format.fprintf pph "@,#ifndef __%s_H__@,#define __%s_H__@," module_id
    module_id;
  List.iter
    (fun (m, _intf_id) ->
      pp [ppc; pph] "@,#include <%s.h>" ModuleName.(to_string (normalise m)))
    (List.map
       (fun (m, intf) -> m, intf.intf_id)
       (ModuleName.Map.bindings p.ctx.decl_ctx.ctx_modules));
  Option.iter
    (pp [ppmain] "@,#include \"%s\"")
    (Option.map File.(fun f -> basename f -.- "h") output_file);

  (* TODO: check the module hash ? *)
  let ctx =
    { decl_ctx = p.ctx.decl_ctx; module_name = Option.map fst p.module_name }
  in
  format_ctx type_ordering ~ppc ~pph ctx;
  ppall "@,";
  let env =
    {
      locs = None;
      global_vars = VarName.Set.empty;
      local_vars = VarName.Set.empty;
    }
  in
  let env = List.fold_left (format_code_item ctx ~ppc ~pph) env p.code_items in
  pp [pph] "@,#endif /* __%s_H__ */" module_id;
  if snd p.tests <> [] then format_main ctx env ppmain p;
  ppall "@]";
  if Global.options.gen_external then
    let files = List.filter_map Fun.id [output_file; h_file] in
    if files <> [] then
      Message.result "Generated template external implementations:@ %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space File.format)
        files
