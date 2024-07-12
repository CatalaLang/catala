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

let avoid_keywords (s : string) : string =
  if
    match s with
    (* list taken from
       https://learn.microsoft.com/en-us/cpp/c-language/c-keywords *)
    | "auto" | "break" | "case" | "char" | "const" | "continue" | "default"
    | "do" | "double" | "else" | "enum" | "extern" | "float" | "for" | "goto"
    | "if" | "inline" | "int" | "long" | "register" | "restrict" | "return"
    | "short" | "signed" | "sizeof" | "static" | "struct" | "switch" | "typedef"
    | "union" | "unsigned" | "void" | "volatile" | "while" ->
      true
    | _ -> false
  then s ^ "_"
  else s

let format_struct_name (fmt : Format.formatter) (v : StructName.t) : unit =
  Format.pp_print_string fmt
    (Format.asprintf "%a_struct" StructName.format v
    |> String.to_snake_case
    |> avoid_keywords)

let format_struct_field_name (fmt : Format.formatter) (v : StructField.t) : unit
    =
  Format.pp_print_string fmt
    (Format.asprintf "%a_field" StructField.format v
    |> String.to_snake_case
    |> avoid_keywords)

let format_enum_name (fmt : Format.formatter) (v : EnumName.t) : unit =
  Format.fprintf fmt "%s_enum"
    (Format.asprintf "%a" EnumName.format v
    |> String.to_snake_case
    |> avoid_keywords)

let format_enum_cons_name (fmt : Format.formatter) (v : EnumConstructor.t) :
    unit =
  Format.fprintf fmt "%s_cons"
    (Format.asprintf "%a" EnumConstructor.format v
    |> String.to_snake_case
    |> avoid_keywords)

let format_name_cleaned (fmt : Format.formatter) (s : string) : unit =
  s
  |> String.to_snake_case
  |> Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\\.") ~subst:(fun _ -> "_dot_")
  |> avoid_keywords
  |> Format.pp_print_string fmt

let format_func_name (fmt : Format.formatter) (v : FuncName.t) : unit =
  let v_str = Mark.remove (FuncName.get_info v) in
  Format.fprintf fmt "%a_func" format_name_cleaned v_str

module StringMap = String.Map

module IntMap = Map.Make (struct
  include Int

  let format ppf i = Format.pp_print_int ppf i
end)

(** For each `VarName.t` defined by its string and then by its hash, we keep
    track of which local integer id we've given it. This is used to keep
    variable naming with low indices rather than one global counter for all
    variables. TODO: should be removed when
    https://github.com/CatalaLang/catala/issues/240 is fixed. *)
let string_counter_map : int IntMap.t StringMap.t ref = ref StringMap.empty

let format_var (fmt : Format.formatter) (v : VarName.t) : unit =
  let v_str = Mark.remove (VarName.get_info v) in
  let id = VarName.id v in
  let local_id =
    match StringMap.find_opt v_str !string_counter_map with
    | Some ids -> (
      match IntMap.find_opt id ids with
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
            (IntMap.add id (max_id + 1) ids)
            !string_counter_map;
        max_id + 1
      | Some local_id -> local_id)
    | None ->
      string_counter_map :=
        StringMap.add v_str (IntMap.singleton id 0) !string_counter_map;
      0
  in
  if v_str = "_" then Format.fprintf fmt "dummy_var"
    (* special case for the unit pattern TODO escape dummy_var *)
  else if local_id = 0 then format_name_cleaned fmt v_str
  else Format.fprintf fmt "%a_%d" format_name_cleaned v_str local_id

module TypMap = Map.Make (struct
  type t = naked_typ

  let compare x y = Type.compare (x, Pos.no_pos) (y, Pos.no_pos)
  let format fmt x = Print.typ_debug fmt (x, Pos.no_pos)
end)

let tname_to_string = function
  | TLit TUnit -> "CATALA_UNIT"
  | TLit TMoney -> "CATALA_MONEY"
  | TLit TInt -> "CATALA_INT"
  | TLit TRat -> "CATALA_DEC"
  | TLit TDate -> "CATALA_DATE"
  | TLit TDuration -> "CATALA_DURATION"
  | TLit TBool -> "CATALA_BOOL"
  | TTuple tl -> Printf.sprintf "void*()[%d]" (List.length tl)
  | TStruct s -> Format.asprintf "%a" format_struct_name s
  | TOption _ -> "CATALA_OPTION"
  | TEnum e -> Format.asprintf "%a" format_enum_name e
  | TDefault _ -> assert false
  | TArrow _ -> "void*"
  | TArray _ -> "CATALA_ARRAY %t"
  | TAny -> "void*"
  | TClosureEnv -> "void *"

(* Here, [element_name] is the struct field, union member or function parameter
   of which you're printing the type. *)
let rec format_typ
    (decl_ctx : decl_ctx)
    (element_name : Format.formatter -> unit)
    (fmt : Format.formatter)
    (typ : typ) : unit =
  match Mark.remove typ with
  | TLit TUnit -> Format.fprintf fmt "CATALA_UNIT %t" element_name
  | TLit TMoney -> Format.fprintf fmt "CATALA_MONEY %t" element_name
  | TLit TInt -> Format.fprintf fmt "CATALA_INT %t" element_name
  | TLit TRat -> Format.fprintf fmt "CATALA_DEC %t" element_name
  | TLit TDate -> Format.fprintf fmt "CATALA_DATE %t" element_name
  | TLit TDuration -> Format.fprintf fmt "CATALA_DURATION %t" element_name
  | TLit TBool -> Format.fprintf fmt "CATALA_BOOL %t" element_name
  | TTuple ts ->
    Format.fprintf fmt "@[<v 2>struct {@,%a @]@,}"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (t, i) ->
           Format.fprintf fmt "%a;"
             (format_typ decl_ctx (fun fmt -> Format.fprintf fmt "arg_%d" i))
             t))
      (List.mapi (fun x y -> y, x) ts)
  | TStruct s -> Format.fprintf fmt "%a %t" format_struct_name s element_name
  | TOption _ -> Format.fprintf fmt "CATALA_OPTION %t" element_name
  | TDefault t -> format_typ decl_ctx element_name fmt t
  | TEnum e -> Format.fprintf fmt "%a %t" format_enum_name e element_name
  | TArrow (t1, t2) ->
    Format.fprintf fmt "%a(%a)"
      (format_typ decl_ctx (fun fmt -> Format.fprintf fmt "(*%t)" element_name))
      t2
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         (fun fmt (i, t1_arg) ->
           (format_typ decl_ctx (fun fmt -> Format.fprintf fmt "arg_%d_typ" i))
             fmt t1_arg))
      (List.mapi (fun x y -> x, y) t1)
  | TArray _ ->
    Format.fprintf fmt "CATALA_ARRAY %t" element_name
  | TAny -> Format.fprintf fmt "void * /* any */ %t" element_name
  | TClosureEnv -> Format.fprintf fmt "void * /* closure_env */ %t" element_name

let format_ctx
    (type_ordering : Scopelang.Dependency.TVertex.t list)
    (fmt : Format.formatter)
    (ctx : decl_ctx) : unit =
  let format_struct_decl fmt (struct_name, struct_fields) =
    let fields = StructField.Map.bindings struct_fields in
    Format.fprintf fmt "@[<v 2>typedef struct %a {@ %a@;<1 -2>}@] %a;"
      format_struct_name struct_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (struct_field, struct_field_type) ->
           Format.fprintf fmt "@[<v>%a;@]"
             (format_typ ctx (fun fmt ->
                  format_struct_field_name fmt struct_field))
             struct_field_type))
      fields format_struct_name struct_name
  in
  let format_enum_decl fmt (enum_name, enum_cons) =
    if EnumConstructor.Map.is_empty enum_cons then
      failwith "no constructors in the enum"
    else
      Format.fprintf fmt "@[<v 2>enum %a_code {@,%a@;<0 -2>}@] %a_code;@\n@\n"
        format_enum_name enum_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (enum_cons, _) ->
             Format.fprintf fmt "%a_%a" format_enum_name enum_name
               format_enum_cons_name enum_cons))
        (EnumConstructor.Map.bindings enum_cons)
        format_enum_name enum_name;
    Format.fprintf fmt
      "@[<v 2>typedef struct %a {@ enum %a_code code;@ @[<v 2>union {@ %a@]@,\
       } payload;@]@,\
       } %a;" format_enum_name enum_name format_enum_name enum_name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ ")
         (fun fmt (enum_cons, typ) ->
           Format.fprintf fmt "%a;"
             (format_typ ctx (fun fmt -> format_enum_cons_name fmt enum_cons))
             typ))
      (EnumConstructor.Map.bindings enum_cons)
      format_enum_name enum_name
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
      (StructName.Map.bindings
         (StructName.Map.filter
            (fun s _ -> not (is_in_type_ordering s))
            ctx.ctx_structs))
  in
  Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_cut fmt (); Format.pp_print_cut fmt ())
    (fun fmt struct_or_enum ->
      match struct_or_enum with
      | Scopelang.Dependency.TVertex.Struct s ->
        Format.fprintf fmt "%a" format_struct_decl
          (s, StructName.Map.find s ctx.ctx_structs)
      | Scopelang.Dependency.TVertex.Enum e ->
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

let rec format_expression (ctx : decl_ctx) (fmt : Format.formatter) (e : expr) :
    unit =
  match Mark.remove e with
  | EVar v -> format_var fmt v
  | EFunc f -> format_func_name fmt f
  | EStruct { fields = es; _ } ->
    (* These should only appear when initializing a variable definition *)
    Format.fprintf fmt "{ %a }"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt (_, e) -> Format.fprintf fmt "%a" (format_expression ctx) e))
      (StructField.Map.bindings es)
  | EStructFieldAccess { e1; field; _ } ->
    Format.fprintf fmt "%a.%a" (format_expression ctx) e1
      format_struct_field_name field
  | EInj { e1; cons; name = enum_name; _ }
    when EnumName.equal enum_name Expr.option_enum ->
    if EnumConstructor.equal cons Expr.none_constr then
      Format.fprintf fmt "CATALA_NONE"
    else
      Format.fprintf fmt "catala_some(%a)"
        (format_expression ctx) e1
  | EInj { e1; cons; name = enum_name; _ } ->
    Format.fprintf fmt "{%a_%a,@ {%a: %a}}" format_enum_name enum_name
      format_enum_cons_name cons format_enum_cons_name cons
      (format_expression ctx) e1
  | EArray _ ->
    failwith
      "should not happen, array initialization is caught at the statement level"
  | ELit l -> Format.fprintf fmt "%a" format_lit (Mark.copy e l)
  | EAppOp { op = (ToClosureEnv | FromClosureEnv), _; args = [arg]; _} ->
    format_expression ctx fmt arg
  | EAppOp { op = ((Map | Filter), _) as op; args = [arg1; arg2]; _ } ->
    Format.fprintf fmt "%a(%a,@ %a)" format_op op (format_expression ctx) arg1
      (format_expression ctx) arg2
  (* | EAppOp {
   *     op = HandleExceptions, _ as op;
   *     args;
   *     _ } ->
   *   Format.fprintf fmt "*%a(%s,@ %a)" format_op op
   *     "&catala_empty_position"
   *     (\* TODO: actual position must be lifted in a (static const) declaration *\)
   *     (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
   *        (format_expression ctx))
   *     args *)
  | EAppOp {
      op = (HandleExceptions | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_rat | Div_dur_dur), _ as op;
      args;
      _ } ->
    Format.fprintf fmt "%a(%s,@ %a)" format_op op
      "&catala_empty_position"
      (* TODO: actual position must be lifted in a (static const) declaration *)
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | EAppOp { op; args = [arg1; arg2]; _ } ->
    Format.fprintf fmt "%a(%a,@ %a)"
      format_op op
      (format_expression ctx) arg1
      (format_expression ctx) arg2
  | EAppOp { op = (Not, _) as op; args = [arg1]; _ } ->
    Format.fprintf fmt "%a %a" format_op op (format_expression ctx) arg1
  | EAppOp
      {
        op = ((Minus_int | Minus_rat | Minus_mon | Minus_dur), _) as op;
        args = [arg1];
        _
      } ->
    Format.fprintf fmt "%a(%a)" format_op op (format_expression ctx) arg1
  | EAppOp { op; args = [arg1]; _ } ->
    Format.fprintf fmt "%a(%a)" format_op op (format_expression ctx) arg1
  | EApp { f; args } ->
    Format.fprintf fmt "%a(@[<hov 0>%a)@]" (format_expression ctx) f
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | EAppOp { op; args; _ } ->
    Format.fprintf fmt "%a(@[<hov 0>%a)@]" format_op op
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (format_expression ctx))
      args
  | ETuple _ | ETupleAccess _ ->
    Message.error ~internal:true "Tuple compilation to C unimplemented!"
  | EExternal _ -> failwith "TODO"

let format_raise_error fmt err pos =
  Format.fprintf fmt
    "@[<hov 2>catala_raise(catala_%s,@ %S,@ %d,@ %d, %d,@ %d);@]"
    (String.to_snake_case (Runtime.error_to_string err))
    (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
    (Pos.get_end_line pos) (Pos.get_end_column pos)

let rec format_statement
    (ctx : decl_ctx)
    (fmt : Format.formatter)
    (s : stmt Mark.pos) : unit =
  match Mark.remove s with
  | SInnerFuncDef _ ->
    Message.error ~pos:(Mark.get s) ~internal:true
      "This inner functions should have been hoisted in Scalc"
  | SLocalDecl { name = v; typ = ty } ->
    Format.fprintf fmt "@[<hov 2>%a@];"
      (format_typ ctx (fun fmt -> format_var fmt (Mark.remove v)))
      ty
    (* Below we detect array initializations which have special treatment. *)
  | SLocalDef { name = v, _; expr = EArray elts, _; _ } ->
    let size = List.length elts in
    Format.fprintf fmt
      "@[<hov 2>%a->size =@ %d;@]@," format_var v size;
    Format.fprintf fmt
      "@[<hov 2>%a->elements = catala_malloc(%d * sizeof(void*));@]@,"
      format_var v size;
    Format.pp_print_list
      (fun fmt (i, arg) ->
         Format.fprintf fmt "@[<hov 2>%a->elements[%d] =@ %a;@]"
           format_var v i (format_expression ctx) arg)
      fmt
      (List.mapi (fun i a -> i, a) elts)
  | SLocalInit { name = v; expr = e; typ } -> assert false
    (* Format.fprintf fmt "@[<hov 2>%a = %a;@]"
     *   (format_typ ctx (fun fmt -> format_var fmt (Mark.remove v)))
     *   typ (format_expression ctx) e *)
  | SLocalDef { name = v; expr = e; _ } ->
    Format.fprintf fmt "@[<hov 2>%a = %a;@]" format_var (Mark.remove v)
      (format_expression ctx) e
  | SFatalError err ->
    let pos = Mark.get s in
    format_raise_error fmt err pos;
  | SIfThenElse { if_expr = cond; then_block = b1; else_block = b2 } ->
    Format.fprintf fmt
      "@[<hv 2>@[<hov 2>if (%a) {@]@,%a@;<1 -2>} else {@,%a@;<1 -2>}@]"
      (format_expression ctx) cond (format_block ctx) b1 (format_block ctx) b2
  | SSwitch { switch_expr = EVar match_var, _; enum_name = e_name; switch_cases = cases; _ }
    when EnumName.equal e_name Expr.option_enum ->
    let cases =
      List.map2
        (fun x (cons, _) -> x, cons)
        cases
        (EnumConstructor.Map.bindings (EnumName.Map.find e_name ctx.ctx_enums))
    in
    let some_case, none_case =
      match
        List.partition (fun (_, cons) -> EnumConstructor.equal cons Expr.some_constr)
          cases
      with
      | [some, _], [none, _] -> some, none
      | _ -> assert false
    in
    Format.fprintf fmt "@[<v 2>if (%a->code == catala_option_some) {@,"
      format_var match_var;
    let () =
      let pvar fmt = format_var fmt some_case.payload_var_name in
      Format.fprintf fmt "%a;@," (format_typ ctx pvar) some_case.payload_var_typ;
      let decls, others =
        List.partition (function
            | (SLocalDecl _| SLocalInit _), _ -> true
            | _ -> false)
          some_case.case_block
      in
      format_block ctx fmt decls;
      Format.fprintf fmt "%t = (%a)(%a->payload);@,"
        pvar
        (format_typ ctx ignore)
        some_case.payload_var_typ
        format_var match_var;
      format_block ctx fmt others
    in
    Format.fprintf fmt "@;<1 -2>} else {@,";
    format_block ctx fmt none_case.case_block;
    Format.fprintf fmt "@;<1 -2>}@]"
  | SSwitch { switch_expr = EVar match_var, _; enum_name = e_name; switch_cases = cases; _ } ->
    let cases =
      List.map2
        (fun x (cons, _) -> x, cons)
        cases
        (EnumConstructor.Map.bindings (EnumName.Map.find e_name ctx.ctx_enums))
    in
    Format.pp_open_vbox fmt 2;
    Format.fprintf fmt "@[<hov 4>switch (%a.code) {@]@," format_var match_var;
    Format.pp_print_list
      (fun fmt ({ case_block; payload_var_name; payload_var_typ }, cons_name) ->
        Format.fprintf fmt "@[<hv 2>case %a_%a:@ " format_enum_name e_name
          format_enum_cons_name cons_name;
        if not (Type.equal payload_var_typ (TLit TUnit, Pos.no_pos)) then
          Format.fprintf fmt "%a = %a.payload.%a;@ "
            (format_typ ctx (fun fmt -> format_var fmt payload_var_name))
            payload_var_typ format_var match_var format_enum_cons_name cons_name;
        Format.fprintf fmt "%a@ break;@]" (format_block ctx) case_block)
      fmt cases;
    (* Do we want to add 'default' case with a failure ? *)
    Format.fprintf fmt "@;<0 -2>}";
    Format.pp_close_box fmt ()
  | SSwitch _ -> assert false (* switches should have been rewritten to only match on variables *)

  | SReturn e1 ->
    Format.fprintf fmt "@[<hov 2>return %a;@]" (format_expression ctx)
      (e1, Mark.get s)
  | SAssert e1 ->
    let pos = Mark.get s in
    Format.fprintf fmt
      "@[<v 2>@[<hov 2>if (!(%a)) {@]@,\
       @[<hov 2>catala_raise(catala_assertion_failed,@ \"%s\",@ \
       %d, %d, %d, %d);@]@;\
       <1 -2>}@]" (format_expression ctx)
      (e1, Mark.get s)
      (Pos.get_file pos) (Pos.get_start_line pos) (Pos.get_start_column pos)
      (Pos.get_end_line pos) (Pos.get_end_column pos)
  | _ -> .
(*   | SSpecialOp (OHandleExceptions { exceptions }) ->
 * 
 *     Format.fprintf fmt "catala_handle_exceptions(%a, "
 *     let excs_arr = match excs with
 *       | EStruct {fields; name}, _ ->
 *         let arr_field, elt_t =
 *           match Mark.remove etyp with
 *           | TStruct arr_struct ->
 *         arr_struct,
 *         StructField.Map.choose @@
 *         StructField.Map.filter_map (fun _ -> function
 *             | TArray telt, _ -> Some telt
 *             | _ -> None)
 *           (StructName.Map.find arr_struct ctx.ctx_structs)
 *       | _ -> assert false
 *     in
 *         
 *         (match StructField.Map.find arr_field fields with
 *          | EArray es, _ -> es
 *          | _ -> assert false)
 *       | _ -> assert false
 *     in
 *     (* T acc = None;
 *        if (e0 != None) { if (acc != None) catala_error; else acc = e0 }
 *        if (e1 != None) { if (acc != None) catala_error; else acc = e1 }
 *        etc. *)
 *     (* e0 != None
 *        ? (e1..en == None..None ? e0 : conflict)
 *        : e1 != None
 *        ? (e2..en == None..None ? e1 : conflict)
 *        etc.
 *        : None *)
 *     (* T[] e = [excs];
 *        int i, j;
 *        for(i=0; i<n && e[i] == None; i++);
 *        if (i>=n) return None;
 *        for(j=i; j<n && e[j] == None; j++);
 *        if (j < n) conflict;
 *        else return e[i];
 * *)
 *     let none_cons, some_cons =
 *       match EnumConstructor.Map.bindings (EnumName.Map.find e_name ctx.ctx_enums) with
 *       | [none_cons, (TLit TUnit, _); some_cons, _] -> none_cons, some_cons
 *       | _ -> failwith "should not happen"
 *     in
 *     let pos = Mark.get s in
 *     let exception_acc_var = VarName.fresh ("exception_acc", Mark.get s) in
 *     let exception_current = VarName.fresh ("exception_current", Mark.get s) in
 *     let exception_conflict = VarName.fresh ("exception_conflict", Mark.get s) in
 *     let variable_defined_in_cons =
 *       match List.hd (List.rev cons) with
 *       | SReturn (EVar v), _ -> v
 *       | SLocalDef { name; _ }, _ | SLocalInit { name; _ }, _ -> Mark.remove name
 *       | _ -> failwith "should not happen"
 *     in
 *     if exceptions <> [] then begin
 *       Format.fprintf fmt "@[<hov 2>%a = {%a_%a,@ {%a: NULL}};@]@,"
 *         (format_typ ctx (fun fmt -> format_var fmt exception_acc_var))
 *         return_typ format_enum_name e_name format_enum_cons_name none_cons
 *         format_enum_cons_name none_cons;
 *       Format.fprintf fmt "%a;@,"
 *         (format_typ ctx (fun fmt -> format_var fmt exception_current))
 *         return_typ;
 *       Format.fprintf fmt "char %a = 0;@," format_var exception_conflict;
 *       List.iter
 *         (fun except ->
 *           Format.fprintf fmt
 *             "%a = %a;@,\
 *              @[<v 2>if (%a.code == %a_%a) {@,\
 *              @[<v 2>if (%a.code == %a_%a) {@,\
 *              %a = 1;@]@,\
 *              @[<v 2>} else {@,\
 *              %a = %a;@]@,\
 *              }@]@,\
 *              }@,"
 *             format_var exception_current (format_expression ctx) except
 *             format_var exception_current format_enum_name e_name
 *             format_enum_cons_name some_cons format_var exception_acc_var
 *             format_enum_name e_name format_enum_cons_name some_cons format_var
 *             exception_conflict format_var exception_acc_var format_var
 *             exception_current)
 *         exceptions;
 *       Format.fprintf fmt
 *         "@[<v 2>if (%a) {@,\
 *          @[<hov 2>catala_raise_fatal_error(catala_conflict,@ \"%s\",@ %d, %d, \
 *          %d, %d);@]@;\
 *          <1 -2>}@]@,"
 *         format_var exception_conflict (Pos.get_file pos)
 *         (Pos.get_start_line pos) (Pos.get_start_column pos)
 *         (Pos.get_end_line pos) (Pos.get_end_column pos);
 *       Format.fprintf fmt
 *         "@[<v 2>if (%a.code == %a_%a) {@,%a = %a;@]@,@[<v 2>} else {@,"
 *         format_var exception_acc_var format_enum_name e_name
 *         format_enum_cons_name some_cons format_var variable_defined_in_cons
 *         format_var exception_acc_var
 *     end;
 *     Format.fprintf fmt
 *       "@[<v 2>if (%a) {@,\
 *        %a@]@,\
 *        @[<v 2>} else {@,\
 *        %a.code = %a_%a;@,\
 *        %a.payload.%a = NULL;@]@,\
 *        }"
 *       (format_expression ctx) just (format_block ctx) cons format_var
 *       variable_defined_in_cons format_enum_name e_name format_enum_cons_name
 *       none_cons format_var variable_defined_in_cons format_enum_cons_name
 *       none_cons;
 *     if exceptions <> [] then Format.fprintf fmt "@]@,}" *)
  (* | SSpecialOp (OHandleDefaultOpt { exceptions; just; cons; return_typ }) ->
   *   let e_name =
   *     match Mark.remove return_typ with
   *     | TEnum t -> t
   *     | _ -> failwith "should not happen"
   *   in
   *   let option_config =
   *     List.map fst
   *       (EnumConstructor.Map.bindings (EnumName.Map.find e_name ctx.ctx_enums))
   *   in
   *   let none_cons, some_cons =
   *     match option_config with
   *     | [none_cons; some_cons] -> none_cons, some_cons
   *     | _ -> failwith "should not happen"
   *   in
   *   let pos = Mark.get s in
   *   let exception_acc_var = VarName.fresh ("exception_acc", Mark.get s) in
   *   let exception_current = VarName.fresh ("exception_current", Mark.get s) in
   *   let exception_conflict = VarName.fresh ("exception_conflict", Mark.get s) in
   *   let variable_defined_in_cons =
   *     match List.hd (List.rev cons) with
   *     | SReturn (EVar v), _ -> v
   *     | SLocalDef { name; _ }, _ | SLocalInit { name; _ }, _ -> Mark.remove name
   *     | _ -> failwith "should not happen"
   *   in
   *   if exceptions <> [] then begin
   *     Format.fprintf fmt "@[<hov 2>%a = {%a_%a,@ {%a: NULL}};@]@,"
   *       (format_typ ctx (fun fmt -> format_var fmt exception_acc_var))
   *       return_typ format_enum_name e_name format_enum_cons_name none_cons
   *       format_enum_cons_name none_cons;
   *     Format.fprintf fmt "%a;@,"
   *       (format_typ ctx (fun fmt -> format_var fmt exception_current))
   *       return_typ;
   *     Format.fprintf fmt "char %a = 0;@," format_var exception_conflict;
   *     List.iter
   *       (fun except ->
   *         Format.fprintf fmt
   *           "%a = %a;@,\
   *            @[<v 2>if (%a.code == %a_%a) {@,\
   *            @[<v 2>if (%a.code == %a_%a) {@,\
   *            %a = 1;@]@,\
   *            @[<v 2>} else {@,\
   *            %a = %a;@]@,\
   *            }@]@,\
   *            }@,"
   *           format_var exception_current (format_expression ctx) except
   *           format_var exception_current format_enum_name e_name
   *           format_enum_cons_name some_cons format_var exception_acc_var
   *           format_enum_name e_name format_enum_cons_name some_cons format_var
   *           exception_conflict format_var exception_acc_var format_var
   *           exception_current)
   *       exceptions;
   *     Format.fprintf fmt
   *       "@[<v 2>if (%a) {@,\
   *        @[<hov 2>catala_raise_fatal_error(catala_conflict,@ \"%s\",@ %d, %d, \
   *        %d, %d);@]@;\
   *        <1 -2>}@]@,"
   *       format_var exception_conflict (Pos.get_file pos)
   *       (Pos.get_start_line pos) (Pos.get_start_column pos)
   *       (Pos.get_end_line pos) (Pos.get_end_column pos);
   *     Format.fprintf fmt
   *       "@[<v 2>if (%a.code == %a_%a) {@,%a = %a;@]@,@[<v 2>} else {@,"
   *       format_var exception_acc_var format_enum_name e_name
   *       format_enum_cons_name some_cons format_var variable_defined_in_cons
   *       format_var exception_acc_var
   *   end;
   *   Format.fprintf fmt
   *     "@[<v 2>if (%a) {@,\
   *      %a@]@,\
   *      @[<v 2>} else {@,\
   *      %a.code = %a_%a;@,\
   *      %a.payload.%a = NULL;@]@,\
   *      }"
   *     (format_expression ctx) just (format_block ctx) cons format_var
   *     variable_defined_in_cons format_enum_name e_name format_enum_cons_name
   *     none_cons format_var variable_defined_in_cons format_enum_cons_name
   *     none_cons;
   *   if exceptions <> [] then Format.fprintf fmt "@]@,}" *)

and format_block (ctx : decl_ctx) (fmt : Format.formatter) (b : block) : unit =
  let b =
    (* C89 doesn't accept initialisations from non-constants: split all init into decl + def *)
    let revb =
      List.fold_left (fun acc -> function
        | SLocalInit {expr = ELit _, _; _}, _ as st -> st :: acc
        | SLocalInit {name; typ; expr}, m ->
          (SLocalDecl {name; typ}, m) :: (SLocalDef {name; typ; expr}, m) :: acc
        | st -> st :: acc)
        [] b
    in
    (* C89 requires declarations to be on top of the block *)
    let decls, others =
      List.partition (function
          | (SLocalDecl _| SLocalInit _), _ -> true
          | _ -> false)
        revb
    in
    List.rev_append decls (List.rev others)
  in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
    (format_statement ctx) fmt b

let format_main
    (fmt : Format.formatter)
    (p: Ast.program) =
  Format.fprintf fmt "@,@[<v 2>int main (int argc, char** argv) {";
  Format.fprintf fmt "@,catala_init();";
  let scopes_with_no_input =
    List.fold_left
      (fun acc -> function
        | SScope { scope_body_func =
                     { func_params = [_, (TStruct ts, _)]; _};
                   scope_body_var = var;
                   scope_body_name = name; } ->
          let input_struct =
            StructName.Map.find ts p.ctx.decl_ctx.ctx_structs
          in
          if StructField.Map.is_empty input_struct then (var, name, ts) :: acc
          else acc
        | SVar _ | SFunc _ | SScope _ -> acc)
      []
      p.code_items
    |> List.rev
  in
  if scopes_with_no_input <> [] then
    Message.debug "Generating entry points for scopes:@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf (_, s, _) ->
           ScopeName.format ppf s))
      scopes_with_no_input;
  List.iter
    (fun (var, name, ts) ->
       Format.fprintf fmt "@,printf(\"Executing scope %a...\\n\");"
         ScopeName.format name;
       Format.fprintf fmt "@,%a ((%a){});" format_func_name var format_struct_name ts;
       Format.fprintf fmt "@,printf(\"Scope %a executed successfully.\\n\");"
         ScopeName.format name)
    scopes_with_no_input;
  Format.fprintf fmt "@;<1 -2>}@]@,"

let format_program
    (fmt : Format.formatter)
    (p : Ast.program)
    (type_ordering : Scopelang.Dependency.TVertex.t list) : unit =
  Format.pp_open_vbox fmt 0;
  Format.fprintf fmt
    "/* This file has been generated by the Catala compiler, do not edit! \
     */@,\
     @,\
     #include <stdio.h>@,\
     #include <stdlib.h>@,\
     #include <runtime.c>@,@,";
  format_ctx type_ordering fmt p.ctx.decl_ctx;
  Format.pp_print_cut fmt ();
  Format.pp_print_cut fmt ();
  Format.pp_print_list (fun fmt code_item ->
      match code_item with
      | SVar { var; expr; typ } ->
        Format.fprintf fmt "@[<v 2>%a = %a;@]"
          (format_typ p.ctx.decl_ctx (fun fmt -> format_var fmt var))
          typ
          (format_expression p.ctx.decl_ctx)
          expr
      | SFunc { var; func }
      | SScope { scope_body_var = var; scope_body_func = func; _ } ->
        let { func_params; func_body; func_return_typ } = func in
        Format.fprintf fmt "@[<v 2>%a(%a) {@,%a@]@,}"
          (format_typ p.ctx.decl_ctx (fun fmt -> format_func_name fmt var))
          func_return_typ
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
             (fun fmt (var, typ) ->
                (format_typ p.ctx.decl_ctx (fun fmt ->
                     format_var fmt (Mark.remove var)))
                  fmt typ))
          func_params
          (format_block p.ctx.decl_ctx)
          func_body)
    fmt
    p.code_items;
  Format.pp_print_cut fmt ();
  format_main fmt p;
  Format.pp_close_box fmt ()
