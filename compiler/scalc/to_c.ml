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
module Runtime = Runtime_ocaml.Runtime
module D = Dcalc.Ast
module L = Lcalc.Ast
open Ast

type ctx = { decl_ctx : decl_ctx }

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

let is_dummy_var v = Mark.remove (VarName.get_info v) = "_"
(* this is the marker of a variable that's not expected to be used TODO: mark
   and/or detect such variables in a better way *)

let renaming =
  Renaming.program ()
    ~reserved:c_keywords
      (* TODO: add catala runtime built-ins as reserved as well ? *)
    ~skip_constant_binders:false ~constant_binder_name:None
    ~namespaced_fields_constrs:false

module TypMap = Map.Make (struct
  type t = naked_typ

  let compare x y = Type.compare (x, Pos.no_pos) (y, Pos.no_pos)
  let format fmt x = Print.typ_debug fmt (x, Pos.no_pos)
end)

(* Here, [element_name] is the struct field, union member or function parameter
   of which you're printing the type. *)
let rec format_typ
    (decl_ctx : decl_ctx)
    ?(const = false)
    (element_name : Format.formatter -> unit)
    (fmt : Format.formatter)
    (typ : typ) : unit =
  let sconst = if const then "const " else "" in
  match Mark.remove typ with
  | TLit TUnit -> Format.fprintf fmt "CATALA_UNIT%t" element_name
  | TLit TMoney -> Format.fprintf fmt "CATALA_MONEY%t" element_name
  | TLit TInt -> Format.fprintf fmt "CATALA_INT%t" element_name
  | TLit TRat -> Format.fprintf fmt "CATALA_DEC%t" element_name
  | TLit TDate -> Format.fprintf fmt "CATALA_DATE%t" element_name
  | TLit TDuration -> Format.fprintf fmt "CATALA_DURATION%t" element_name
  | TLit TBool -> Format.fprintf fmt "CATALA_BOOL%t" element_name
  | TTuple [_; (TClosureEnv, _)] ->
    Format.fprintf fmt "%scatala_closure*%t" sconst element_name
  | TTuple _ -> Format.fprintf fmt "%sCATALA_TUPLE%t" sconst element_name
  | TStruct s ->
    Format.fprintf fmt "%s%a*%t" sconst StructName.format s element_name
  | TOption t ->
    Format.fprintf fmt "%sCATALA_OPTION(%a)%t" sconst
      (format_typ decl_ctx ~const:false ignore)
      t element_name
  | TDefault t -> format_typ decl_ctx ~const element_name fmt t
  | TEnum e ->
    Format.fprintf fmt "%s%a*%t" sconst EnumName.format e element_name
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
  | TAny -> Format.fprintf fmt "%svoid * /* any */%t" sconst element_name
  | TClosureEnv -> Format.fprintf fmt "%sCLOSURE_ENV%t" sconst element_name

let format_ctx
    (type_ordering : TypeIdent.t list)
    (fmt : Format.formatter)
    (ctx : decl_ctx) : unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    let fields = StructField.Map.bindings struct_fields in
    if fields = [] then
      Format.fprintf fmt "@,@[<v 2>typedef void %a;@]" StructName.format
        struct_name
    else
      Format.fprintf fmt "@,@[<v 2>typedef struct %a {@ %a@;<1 -2>}@] %a;"
        StructName.format struct_name
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
           (fun fmt (struct_field, struct_field_type) ->
             Format.fprintf fmt "@[<hov>%a;@]"
               (format_typ ~const:true ctx (fun fmt ->
                    Format.pp_print_space fmt ();
                    StructField.format fmt struct_field))
               struct_field_type))
        fields StructName.format struct_name
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if EnumConstructor.Map.is_empty enum_cons then
      failwith "no constructors in the enum"
    else
      Format.fprintf fmt "@,@[<v 2>enum %a_code {@,%a@;<0 -2>}@] %a_code;@,"
        EnumName.format enum_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (enum_cons, _) ->
             Format.fprintf fmt "%a_%a" EnumName.format enum_name
               EnumConstructor.format enum_cons))
        (EnumConstructor.Map.bindings enum_cons)
        EnumName.format enum_name;
    Format.fprintf fmt
      "@,\
       @[<v 2>typedef struct %a {@ enum %a_code code;@ @[<v 2>union {@ %a@]@,\
       } payload;@]@,\
       } %a;" EnumName.format enum_name EnumName.format enum_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (enum_cons, typ) ->
           Format.fprintf fmt "@[<hov 2>%a;@]"
             (format_typ ~const:true ctx (fun fmt ->
                  Format.pp_print_space fmt ();
                  EnumConstructor.format fmt enum_cons))
             typ))
      (EnumConstructor.Map.bindings enum_cons)
      EnumName.format enum_name
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
            ctx.ctx_structs))
  in
  Format.pp_print_list
    (fun fmt struct_or_enum ->
      match struct_or_enum with
      | TypeIdent.Struct s ->
        Format.fprintf fmt "%a" format_struct_decl
          (s, StructName.Map.find s ctx.ctx_structs)
      | TypeIdent.Enum e ->
        Format.fprintf fmt "%a" format_enum_decl
          (e, EnumName.Map.find e ctx.ctx_enums))
    fmt
    (type_ordering @ scope_structs)

let format_lit (fmt : Format.formatter) (l : lit Mark.pos) : unit =
  match Mark.remove l with
  | LBool true -> Format.pp_print_string fmt "CATALA_TRUE"
  | LBool false -> Format.pp_print_string fmt "CATALA_FALSE"
  | LInt i ->
    Format.fprintf fmt "catala_new_int_str(\"%s\")"
      (Runtime.integer_to_string i)
  | LUnit -> Format.pp_print_string fmt "CATALA_UNITVAL"
  | LRat q ->
    Format.fprintf fmt "catala_new_dec_str(\"%s\")" (Q.to_string q)
    (* TODO: expose function from [Runtime] *)
  | LMoney e ->
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

let _format_string_list (fmt : Format.formatter) (uids : string list) : unit =
  let sanitize_quotes = Re.compile (Re.char '"') in
  Format.fprintf fmt "c(%a)"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       (fun fmt info ->
         Format.fprintf fmt "\"%s\""
           (Re.replace sanitize_quotes ~f:(fun _ -> "\\\"") info)))
    uids

let rec format_expression (ctx : ctx) (global_vars : VarName.Set.t) (fmt : Format.formatter) (e : expr) : unit
    =
  let format_expression = format_expression ctx global_vars in
  match Mark.remove e with
  | EVar v ->
    if VarName.Set.mem v global_vars then Format.fprintf fmt "%a()" VarName.format v
    else VarName.format fmt v
  | EFunc f -> FuncName.format fmt f
  | EStructFieldAccess { e1; field; _ } ->
    Format.fprintf fmt "%a->%a" format_expression e1 StructField.format
      field
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
  | EPosLit -> assert false (* Handled only as toplevel definitions *)
  | EAppOp { op = (ToClosureEnv | FromClosureEnv), _; args = [arg]; _ } ->
    format_expression fmt arg
  | EAppOp { op = ((Map | Filter), _) as op; args = [arg1; arg2]; _ } ->
    Format.fprintf fmt "%a(%a,@ %a)" format_op op format_expression arg1
      format_expression arg2
  | EAppOp
      {
        op = ((Reduce | Fold), _) as op;
        args = [fct; base; arr];
        tys = [_; aty; _];
      } ->
    (* Operators with a polymorphic return type need a cast *)
    Format.fprintf fmt "((%a)%a(%a,@ %a,@ %a))"
      (format_typ ~const:true ctx.decl_ctx ignore)
      aty format_op op format_expression fct format_expression base
      format_expression arr
  | EAppOp { op = Add_dat_dur rounding, _; args; _ } ->
    Format.fprintf fmt "o_add_dat_dur(%s,@ %a)"
      (match rounding with
      | RoundUp -> "dc_date_round_up"
      | RoundDown -> "dc_date_round_down"
      | AbortOnRound -> "dc_date_round_abort")
      (Format.pp_print_list format_expression ~pp_sep:(fun ppf () ->
           Format.fprintf ppf ",@ "))
      args
  | EApp { f; args } ->
    Format.fprintf fmt "@[<hov 2>%a@,(@[<hov 0>%a)@]@]" format_expression
      f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         format_expression)
      args
  | EAppOp { op; args; _ } ->
    Format.fprintf fmt "%a(@[<hov 0>%a)@]" format_op op
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         format_expression)
      args
  | ETuple _ -> assert false (* Must be a statement *)
  | ETupleAccess { e1; index = 0; typ = (TArrow _, _) as typ } ->
    (* Closure function *)
    Format.fprintf fmt "@[<hov 1>((%a)@,%a->funcp)@]"
      (format_typ ~const:true ctx.decl_ctx ignore)
      typ format_expression e1
  | ETupleAccess { e1; index = 1; typ = TClosureEnv, _ } ->
    Format.fprintf fmt "%a->env" format_expression e1
  | ETupleAccess { e1; index; typ } ->
    Format.fprintf fmt "(%a)%a[%d]"
      (format_typ ctx.decl_ctx ignore)
      typ format_expression e1 index
  | EExternal _ -> failwith "TODO"

let is_closure_typ = function
  | TTuple [(TArrow _, _); (TClosureEnv, _)], _ -> true
  | _ -> false

let rec format_statement
    (ctx : ctx)
    (global_vars : VarName.Set.t)
    (fmt : Format.formatter)
    (s : stmt Mark.pos) : unit =
  match Mark.remove s with
  | SInnerFuncDef _ ->
    Message.error ~pos:(Mark.get s) ~internal:true
      "This inner functions should have been hoisted in Scalc"
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
  | SLocalInit { name = v, _; expr = EPosLit, pos; _ } ->
    Format.fprintf fmt
      "@,\
       @[<hov 2>static const catala_code_position %a[1] =@ {{%S,@ %d, %d, %d, \
       %d}};@]"
      VarName.format v (Pos.get_file pos) (Pos.get_start_line pos)
      (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos)
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
    Format.fprintf fmt "@,@[<hov 2>%a->size =@ %d;@]" VarName.format v size;
    if size > 0 then
      Format.fprintf fmt
        "@,@[<hov 2>%a->elements = catala_malloc(%d * sizeof(void*));@]"
        VarName.format v size;
    List.iteri
      (fun i arg ->
        Format.fprintf fmt "@,@[<hov 2>%a->elements[%d] =@ %a;@]" VarName.format
          v i (format_expression ctx global_vars) arg)
      elts
  | SLocalDef { name = v, _; expr = EStruct { fields; _ }, _; _ } ->
    StructField.Map.iter
      (fun field expr ->
        Format.fprintf fmt "@,@[<hov 2>%a->%a =@ %a;@]" VarName.format v
          StructField.format field (format_expression ctx global_vars) expr)
      fields
  | SLocalDef { name = v, _; expr = EInj { e1; cons; name; _ }, _; _ }
    when not (EnumName.equal name Expr.option_enum) ->
    Format.fprintf fmt "@,@[<hov 2>%a->code = %a_%a;@]" VarName.format v
      EnumName.format name EnumConstructor.format cons;
    Format.fprintf fmt "@,@[<hov 2>%a->payload.%a = %a;@]" VarName.format v
      EnumConstructor.format cons (format_expression ctx global_vars) e1
  | SLocalDef
      {
        name = v, _;
        expr = ETuple [fct; env], _;
        typ = TTuple [(TArrow _, _); (TClosureEnv, _)], _;
      } ->
    (* We detect closure initializations which have special treatment. *)
    Format.fprintf fmt "@,@[<hov 2>%a->funcp =@ (void (*)(void))%a;@]"
      VarName.format v (format_expression ctx global_vars) fct;
    Format.fprintf fmt "@,@[<hov 2>%a->env =@ %a;@]" VarName.format v
      (format_expression ctx global_vars) env
  | SLocalDef { name = v, _; expr = ETuple elts, _; _ } ->
    (* We detect tuple initializations which have special treatment. *)
    List.iteri
      (fun i arg ->
        Format.fprintf fmt "@,@[<hov 2>%a[%d] =@ %a;@]" VarName.format v i
          (format_expression ctx global_vars) arg)
      elts
  | SLocalInit
      {
        name = v;
        typ;
        expr =
          ( EVar _, _
          | ( EAppOp
                {
                  op = (FromClosureEnv | ToClosureEnv), _;
                  args = [(EVar _, _)];
                  _;
                },
              _ ) ) as e;
      } ->
    Format.fprintf fmt "@,@[<hov 2>%a = %a;@]"
      (format_typ ~const:true ctx.decl_ctx (fun fmt ->
           Format.pp_print_space fmt ();
           VarName.format fmt (Mark.remove v)))
      typ (format_expression ctx global_vars) e
  | SLocalInit { name = v; typ; expr = e } ->
    (* Handling at the block level guarantees that [e] is supported as initial value *)
    Format.fprintf fmt "@,@[<hov 2>%a = %a;@]"
      (format_typ ctx.decl_ctx (fun fmt ->
           Format.pp_print_space fmt ();
           VarName.format fmt (Mark.remove v)))
      typ (format_expression ctx global_vars) e
  | SLocalDef { name = v; expr = e; _ } ->
    Format.fprintf fmt "@,@[<hov 2>%a = %a;@]" VarName.format (Mark.remove v)
      (format_expression ctx global_vars) e
  | SFatalError { pos_expr; error } ->
    Format.fprintf fmt "@,@[<hov 2>catala_error(catala_%s,@ %a);@]"
      (String.to_snake_case (Runtime.error_to_string error))
      (format_expression ctx global_vars) pos_expr
  | SIfThenElse { if_expr = ELit (LBool true), _; then_block; _ } ->
    format_block ctx global_vars fmt then_block
  | SIfThenElse { if_expr = ELit (LBool false), _; else_block; _ } ->
    format_block ctx global_vars fmt else_block
  | SIfThenElse { if_expr = cond; then_block = b1; else_block = b2 } ->
    Format.fprintf fmt
      "@,\
       @[<hv 2>@[<hov 2>if (%a == CATALA_TRUE) {@]%a@;\
       <1 -2>} else {%a@;\
       <1 -2>}@]" (format_expression ctx global_vars) cond (format_block ctx global_vars) b1
      (format_block ctx global_vars) b2
  | SSwitch { switch_var; enum_name = e_name; switch_cases = cases; _ }
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
    Format.fprintf fmt "@,@[<v 2>if (%a->code == catala_option_some) {"
      VarName.format switch_var;
    let pos = Mark.get s in
    format_block ctx global_vars fmt
      (Utils.subst_block some_case.payload_var_name
         (* Not a real catala struct, but will print as <var>->payload *)
         ( EStructFieldAccess
             {
               e1 = EVar switch_var, pos;
               field = StructField.fresh ("payload", pos);
               name = StructName.fresh [] ("Dummy", pos);
             },
           pos )
         some_case.payload_var_typ pos some_case.case_block);
    Format.fprintf fmt "@;<1 -2>} else {";
    format_block ctx global_vars fmt none_case.case_block;
    Format.fprintf fmt "@;<1 -2>}@]"
  | SSwitch { switch_var; enum_name = e_name; switch_cases = cases; _ } ->
    Format.fprintf fmt "@,@[<v 2>@[<hov 4>switch (%a->code) {@]" VarName.format
      switch_var;
    List.iter2
      (fun { case_block; payload_var_name; payload_var_typ } (cons_name, _) ->
        Format.fprintf fmt "@,@[<v 2>case %a_%a: {" EnumName.format e_name
          EnumConstructor.format cons_name;
        if
          (not (Type.equal payload_var_typ (TLit TUnit, Pos.no_pos)))
          && not (is_dummy_var payload_var_name)
        then
          Format.fprintf fmt "@ @[<hov 2>%a = %a->payload.%a;@]"
            (format_typ ctx.decl_ctx (fun fmt ->
                 Format.pp_print_space fmt ();
                 VarName.format fmt payload_var_name))
            payload_var_typ VarName.format switch_var
            (* EnumName.format e_name *)
            EnumConstructor.format cons_name;
        Format.fprintf fmt "%a@ break;@;<1 -2>}@]" (format_block ctx global_vars) case_block)
      cases
      (EnumConstructor.Map.bindings
         (EnumName.Map.find e_name ctx.decl_ctx.ctx_enums));
    (* Do we want to add 'default' case with a failure ? *)
    Format.fprintf fmt "@;<0 -2>}";
    Format.pp_close_box fmt ()
  | SReturn e1 ->
    Format.fprintf fmt "@,@[<hov 2>return %a;@]" (format_expression ctx global_vars) e1
  | SAssert { pos_expr; expr } ->
    Format.fprintf fmt
      "@,\
       @[<v 2>@[<hov 2>if (%a != CATALA_TRUE) {@]@,\
       @[<hov 2>catala_error(catala_assertion_failed,@ %a);@]@;\
       <1 -2>}@]" (format_expression ctx global_vars) expr (format_expression ctx global_vars) pos_expr
  | _ -> .

and format_block (ctx : ctx) (global_vars : VarName.Set.t) (fmt : Format.formatter) (b : block) : unit =
  (* C89 doesn't accept initialisations of constructions from non-constants:
     - for known structures needing malloc, provision the malloc here (turn Decl
     into Init (that will only do the malloc) + def) - for literal constants
     keep init - otherwise split Init into decl + def *)
  let requires_malloc = function
    | (EArray _ | EStruct _ | EInj _ | ETuple _ | ELit _), _ -> true
    | _ -> false
  in
  let print_init_malloc fmt v typ =
    let const, pp_size =
      match Mark.remove typ with
      | TArray _ ->
        false, fun fmt -> Format.pp_print_string fmt "sizeof(catala_array)"
      | TStruct name ->
        false, fun fmt -> Format.fprintf fmt "sizeof(%a)" StructName.format name
      | TEnum name ->
        true, fun fmt -> Format.fprintf fmt "sizeof(%a)" EnumName.format name
      | TTuple _ when is_closure_typ typ ->
        false, fun fmt -> Format.pp_print_string fmt "sizeof(catala_closure)"
      | TTuple ts ->
        true, fun fmt -> Format.fprintf fmt "%d * sizeof(void*)" (List.length ts)
      | _ -> assert false
    in
    (* Postfix [const] declares that the pointer is const, but not its contents *)
    Format.fprintf fmt "@,@[<hov 2>%a =@ catala_malloc(%t)@];"
      (format_typ ~const ctx.decl_ctx (fun fmt ->
           Format.fprintf fmt " const@ %a" VarName.format v))
      typ pp_size
  in
  (* C89 requires declarations to be on top of the block *)
  let rec format_decls defined_vars remaining = function
    | (SLocalDecl { name; typ }, _) as decl :: r ->
      let requires_malloc =
        match typ with
        | (TArray _ | TStruct _ | TEnum _ | TTuple _), _ ->
          None <>
          Utils.find_block (function
              | SLocalDef { name = n1; expr; _ }, _ | SLocalInit { name = n1; expr; _ }, _ ->
                Mark.equal VarName.equal name n1 && requires_malloc expr
              | _ -> false)
            r
        | _ -> false
      in
      if requires_malloc then print_init_malloc fmt (Mark.remove name) typ
      else format_statement ctx global_vars fmt decl;
      format_decls defined_vars remaining r
    | (SLocalInit { name; typ; expr }, m) as init :: r ->
      if requires_malloc expr then
        (print_init_malloc fmt (Mark.remove name) typ;
         format_decls defined_vars ((SLocalDef { name; typ; expr }, m) :: remaining) r)
      else if VarName.Set.subset (Utils.get_vars expr) defined_vars then
        (format_statement ctx global_vars fmt init;
         format_decls (VarName.Set.add (Mark.remove name) defined_vars) remaining r)
      else
        (* The init depends on undefined variables, it can't be moved to the top, so we split it into decl + def *)
        (format_statement ctx global_vars fmt (SLocalDecl { name; typ }, m);
         format_decls defined_vars ((SLocalDef { name; typ; expr }, m) :: remaining) r)
    | stmt :: r -> format_decls defined_vars (stmt :: remaining) r
    | [] -> List.rev remaining
  in
  let remaining = format_decls global_vars [] b in
  List.iter (format_statement ctx global_vars fmt) remaining

let format_main (fmt : Format.formatter) (p : Ast.program) =
  Format.fprintf fmt "@,@[<v 2>int main (int argc, char** argv)@;<0 -2>{";
  Format.fprintf fmt "@,catala_init();";
  let scopes_with_no_input =
    List.fold_left
      (fun acc -> function
        | SScope
            {
              scope_body_func = { func_params = [(_, (TStruct ts, _))]; _ };
              scope_body_var = var;
              scope_body_name = name;
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
  if scopes_with_no_input <> [] then
    Message.debug "Generating entry points for scopes:@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (_, s, _) ->
           ScopeName.format ppf s))
      scopes_with_no_input;
  List.iter
    (fun (var, name, _) ->
      Format.fprintf fmt "@,printf(\"Executing scope %a...\\n\");"
        ScopeName.format name;
      Format.fprintf fmt "@,%a (NULL);" FuncName.format var;
      Format.fprintf fmt
        "@,\
         printf(\"\\x1b[32m[RESULT]\\x1b[m Scope %a executed \
         successfully.\\n\");"
        ScopeName.format name)
    scopes_with_no_input;
  Format.fprintf fmt "@,return 0;@;<1 -2>}@]"

let format_program
    (fmt : Format.formatter)
    (p : Ast.program)
    (type_ordering : TypeIdent.t list) : unit =
  Fun.protect ~finally:(Format.pp_print_newline fmt)
  @@ fun () ->
  Format.pp_open_vbox fmt 0;
  Format.fprintf fmt
    "/* This file has been generated by the Catala compiler, do not edit! */@,\
     @,\
     #include <stdio.h>@,\
     #include <stdlib.h>@,\
     #include <catala_runtime.h>@,\
     @,";
  format_ctx type_ordering fmt p.ctx.decl_ctx;
  Format.pp_print_cut fmt ();
  let ctx = { decl_ctx = p.ctx.decl_ctx } in
  let _global_vars =
    List.fold_left (fun global_vars code_item ->
        match code_item with
        | SVar { var; expr; typ } ->
          (* Global variables are turned into inline functions without parameters that perform lazy evaluation: {[
               inline foo_type foo() {
                 static foo_type foo = NULL;
                 return (foo ? foo : foo = foo_init());
               }
             ]}
          *)
          Format.fprintf fmt "@[<v 2>@[<hov 4>inline %a() {@]@,"
            (format_typ ~const:true p.ctx.decl_ctx (fun fmt ->
                 Format.pp_print_space fmt ();
                 VarName.format fmt var))
            typ;
          Format.fprintf fmt "@[<hov 2>static %a = NULL;@]@,"
            (format_typ ~const:true p.ctx.decl_ctx (fun fmt ->
                 Format.pp_print_space fmt ();
                 VarName.format fmt var))
            typ;
          Format.fprintf fmt "@[<hov 2>return (%a ? %a : (%a = %a));"
            VarName.format var VarName.format var VarName.format var
            (format_expression ctx global_vars) expr;
          Format.fprintf fmt "@;<1 -2>}@]@,@,";
          VarName.Set.add var global_vars
      | SFunc { var; func }
      | SScope { scope_body_var = var; scope_body_func = func; _ } ->
        let { func_params; func_body; func_return_typ } = func in
        Format.fprintf fmt
          "@,@[<v 2>@[<hov 4>%a@ @[<hv 1>(%a)@]@]@;<1 -2>{%a@]@,}@,"
          (format_typ ~const:true ctx.decl_ctx (fun fmt ->
               Format.pp_print_space fmt ();
               FuncName.format fmt var))
          func_return_typ
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
             (fun fmt (var, typ) ->
               Format.pp_open_hovbox fmt 2;
               (format_typ ~const:true p.ctx.decl_ctx (fun fmt ->
                    Format.pp_print_space fmt ();
                    VarName.format fmt (Mark.remove var)))
                 fmt typ;
               Format.pp_close_box fmt ()))
          func_params (format_block ctx global_vars) func_body;
        global_vars)
      VarName.Set.empty p.code_items
  in
  Format.pp_print_cut fmt ();
  format_main fmt p;
  Format.pp_close_box fmt ()
