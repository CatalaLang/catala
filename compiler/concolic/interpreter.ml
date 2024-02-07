(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Alain Delaët <alain.delaet--tixeuil@inria.Fr>, Louis
   Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Concolic interpreter for the default calculus *)

open Catala_utils
open Shared_ast
open Op
module Concrete = Shared_ast.Interpreter

module SymbExpr = struct
  type z3_expr = Z3.Expr.expr
  type reentrant = { name : StructField.t; symbol : z3_expr }

  module RuntimeError = struct
    type span_list = (string option * Pos.t) list

    type runtime_error =
      | EmptyError
      | ConflictError of { spans : span_list }
      | DivisionByZeroError of { spans : span_list }  (** TODO factorize? *)

    type message = string

    type t = {
      except : runtime_error; (* TODO use actual exceptions from [Runtime]? *)
      message : message; (* TODO use formatted stuff instead *)
    }

    let make (except : runtime_error) (message : message) = { except; message }

    (* TODO use formatter *)
    let string_of_spans (spans : span_list) : string =
      let _, pos = List.split spans in
      let pos_strings = List.map Pos.to_string_short pos in
      List.fold_left (fun acc a -> a ^ "," ^ acc) "" pos_strings

    let to_string { except; _ } =
      let except_string =
        match except with
        | EmptyError -> "Empty"
        | ConflictError { spans } -> "Conflict(" ^ string_of_spans spans ^ ")"
        | DivisionByZeroError { spans } ->
          "DivisionByZero(" ^ string_of_spans spans ^ ")"
      in
      "↯" ^ except_string ^ "↯"
  end

  type t =
    | Symb_z3 of z3_expr
    | Symb_reentrant of reentrant
      (* only for the lambda expression corresponding to a reentrant variable *)
    | Symb_none
    | Symb_error of RuntimeError.t
        (** TODO make sure that this can only be used on errors? *)

  let mk_z3 s = Symb_z3 s
  let mk_reentrant name symbol = Symb_reentrant { name; symbol }
  let none = Symb_none

  let mk_emptyerror message =
    let err = RuntimeError.(make EmptyError message) in
    Symb_error err

  let mk_conflicterror message spans =
    let open RuntimeError in
    let conflict = ConflictError { spans } in
    let err = make conflict message in
    Symb_error err

  let mk_divisionbyzeroerror message spans =
    let open RuntimeError in
    let conflict = DivisionByZeroError { spans } in
    let err = make conflict message in
    Symb_error err

  let map_z3 (f : z3_expr -> z3_expr) = function
    | Symb_z3 e -> Symb_z3 (f e)
    | x -> x

  let app_z3 (f : z3_expr -> z3_expr) = function
    | Symb_z3 e -> Symb_z3 (f e)
    | _ -> invalid_arg "[SymbExpr.app_z3] expected two z3 expressions"

  let app2_z3 (f : z3_expr -> z3_expr -> z3_expr) e1 e2 =
    match e1, e2 with
    | Symb_z3 e1, Symb_z3 e2 -> Symb_z3 (f e1 e2)
    | _ -> invalid_arg "[SymbExpr.app2_z3] expected two z3 expressions"

  let applist_z3 (f : z3_expr list -> z3_expr) (l : t list) =
    let extract_z3 = function
      | Symb_z3 e -> e
      | _ -> invalid_arg "[SymbExpr.applist_z3] expected z3 expressions"
    in
    let l_z3 = List.map extract_z3 l in
    Symb_z3 (f l_z3)

  let map_none ~none = function Symb_none -> none | e -> e
  let simplify = map_z3 (fun e -> Z3.Expr.simplify e None)

  let string_of_reentrant { name; _ } =
    "<" ^ Mark.remove (StructField.get_info @@ name) ^ ">"

  (* TODO formatter *)
  let to_string ?(typed : bool = false) e =
    match e with
    | Symb_z3 s ->
      let str = Z3.Expr.to_string s in
      if typed then
        "(" ^ str ^ ":" ^ (Z3.Sort.to_string @@ Z3.Expr.get_sort s) ^ ")"
      else str
    | Symb_reentrant r -> string_of_reentrant r
    | Symb_none -> "None"
    | Symb_error err -> RuntimeError.to_string err

  let formatter (fmt : Format.formatter) (symb_expr : t) : unit =
    Format.pp_print_string fmt (to_string symb_expr)

  let formatter_typed (fmt : Format.formatter) (symb_expr : t) : unit =
    Format.pp_print_string fmt (to_string ~typed:true symb_expr)
end

type s_expr = SymbExpr.z3_expr
type reentrant = { name : StructField.t; is_empty : bool }
type pc_expr = Pc_z3 of s_expr | Pc_reentrant of reentrant

(* path constraint cannot be empty (this looks like a GADT but it would be
   overkill I think) *)
type path_constraint = { expr : pc_expr; pos : Pos.t; branch : bool }

type annotated_path_constraint =
  | Negated of path_constraint
      (** the path constraint that has been negated to generate a new input *)
  | Done of path_constraint
      (** a path node that has been explored should, and whose constraint should
          not be negated *)
  | Normal of path_constraint  (** all other constraints *)

type _conc_info = {
  symb_expr : SymbExpr.t;
  constraints : path_constraint list;
  ty : typ option;
}

type conc_info = _conc_info custom

(* This is DCalc with possibly genericErrors and customs *)
type ('c, 'e) conc_interpr_kind =
  < monomorphic : yes
  ; polymorphic : yes
  ; overloaded : no
  ; resolved : yes
  ; syntacticNames : no
  ; scopeVarStates : no
  ; scopeVarSimpl : no
  ; explicitScopes : no
  ; assertions : yes
  ; defaultTerms : yes
  ; genericErrors : 'e
  ; exceptions : no
  ; custom : 'c >

type conc_src_kind = (yes, no) conc_interpr_kind
type conc_dest_kind = (yes, yes) conc_interpr_kind

type conc_expr = (conc_src_kind, conc_info) gexpr
(** A concolic expression is a concrete DCalc expression that carries its
    symbolic representation and the constraints necessary to compute it. Upon
    initialization, [symb_expr] is [None], except for inputs whose [symb_expr]
    is a symbol. Then [symb_expr] is set by evaluation, except for inputs which
    stay the same. The expression can have [EGenericError] and [ECustom]. *)

type conc_result = (conc_dest_kind, conc_info) gexpr
(** A concolic result expression is the same as a concolic expression but it can
    be an error *)

type conc_naked_expr = (conc_src_kind, conc_info) naked_gexpr
type conc_naked_result = (conc_dest_kind, conc_info) naked_gexpr
type conc_boxed_expr = (conc_src_kind, conc_info) boxed_gexpr

let set_conc_info
    (type m)
    (symb_expr : SymbExpr.t)
    (constraints : path_constraint list)
    (mk : m mark) : conc_info mark =
  let symb_expr = SymbExpr.simplify symb_expr in
  let custom = { symb_expr; constraints; ty = None } in
  match mk with
  | Untyped { pos } -> Custom { pos; custom }
  | Typed { pos; ty } ->
    let custom = { custom with ty = Some ty } in
    Custom { pos; custom }
    (* NOTE we keep type information. This information is used for instance in
       generating concolic input values, eg in [inputs_of_model] *)
  | Custom m -> Custom { m with custom }

(** Maybe replace constraints, and safely replace the symbolic expression from
    former mark *)
let add_conc_info_m
    (former_mark : conc_info mark)
    (symb_expr : SymbExpr.t)
    ?(constraints : path_constraint list option)
    (x : 'a) : ('a, conc_info) marked =
  let (Custom { pos; custom }) = former_mark in
  let symb_expr = SymbExpr.simplify symb_expr in
  let symb_expr = SymbExpr.map_none custom.symb_expr ~none:symb_expr in
  (* only update symb_expr if it does not exist already *)
  let constraints = Option.value ~default:custom.constraints constraints in
  (* only change constraints if new ones are provided *)
  let ty = custom.ty in
  (* we don't change types *)
  Mark.add (Custom { pos; custom = { symb_expr; constraints; ty } }) x

(** Maybe replace the constraints, and safely replace the symbolic expression
    from former expression *)
let add_conc_info_e
    (symb_expr : SymbExpr.t)
    ?(constraints : path_constraint list option)
    (x : ('a, conc_info) marked) : ('a, conc_info) marked =
  match constraints with
  | None -> add_conc_info_m (Mark.get x) symb_expr (Mark.remove x)
  | Some constraints ->
    add_conc_info_m (Mark.get x) symb_expr ~constraints (Mark.remove x)

let map_conc_mark
    ?(symb_expr_f = fun x -> x)
    ?(constraints_f = fun x -> x)
    ?(ty_f = fun x -> x)
    ?(pos_f = fun x -> x)
    (m : conc_info mark) : conc_info mark =
  let (Custom { custom = { symb_expr; constraints; ty }; pos }) = m in
  let symb_expr = symb_expr_f symb_expr in
  let constraints = constraints_f constraints in
  let ty = ty_f ty in
  let pos = pos_f pos in
  Custom { custom = { symb_expr; constraints; ty }; pos }

(* Typing shenanigan to [EGenericError] terms to the AST type. Inspired by
   [Concrete.addcustom]. *)
let add_genericerror e =
  let rec f :
      type c e.
      ((c, e) conc_interpr_kind, 't) gexpr ->
      ((c, yes) conc_interpr_kind, 't) gexpr boxed = function
    | (ECustom _, _) as e -> Expr.map ~f e
    | (EGenericError, _) as e -> Expr.map ~f e
    | EAppOp { op; tys; args }, m ->
      Expr.eappop ~tys ~args:(List.map f args) ~op:(Operator.translate op) m
    | (EDefault _, _) as e -> Expr.map ~f e
    | (EPureDefault _, _) as e -> Expr.map ~f e
    | (EEmptyError, _) as e -> Expr.map ~f e
    | (EErrorOnEmpty _, _) as e -> Expr.map ~f e
    | ( ( EAssert _ | ELit _ | EApp _ | EArray _ | EVar _ | EExternal _ | EAbs _
        | EIfThenElse _ | ETuple _ | ETupleAccess _ | EInj _ | EStruct _
        | EStructAccess _ | EMatch _ ),
        _ ) as e ->
      Expr.map ~f e
    | _ -> .
  in
  let open struct
    external id :
      (('c, 'e) conc_interpr_kind, 't) gexpr ->
      (('c, yes) conc_interpr_kind, 't) gexpr = "%identity"
  end in
  if false then Expr.unbox (f e)
    (* We keep the implementation as a typing proof, but bypass the AST
       traversal for performance. Note that it's not completely 1-1 since the
       traversal would do a reboxing of all bound variables *)
  else id e

(* Coerce a non-error expression into the result type *)
let make_ok : conc_expr -> conc_result = add_genericerror

let make_error mk symb_expr constraints : conc_result =
  let mark =
    set_conc_info symb_expr constraints
      mk (* FIXME this loses type information: is it ok? *)
  in
  Expr.unbox (Expr.egenericerror mark)

let make_error_emptyerror mk constraints message : conc_result =
  let symb_expr = SymbExpr.mk_emptyerror message in
  make_error mk symb_expr constraints

let make_error_conflicterror mk constraints spans message : conc_result =
  let symb_expr = SymbExpr.mk_conflicterror message spans in
  make_error mk symb_expr constraints

let make_error_divisionbyzeroerror mk constraints spans message : conc_result =
  let symb_expr = SymbExpr.mk_divisionbyzeroerror message spans in
  make_error mk symb_expr constraints

(* Inspired by [Concrete.delcustom] *)
let del_genericerror e =
  let rec f : (conc_dest_kind, 'm) gexpr -> (conc_src_kind, 'm) gexpr boxed =
    function
    | EGenericError, _ ->
      invalid_arg "Generic error remaining after propagation"
    | EAppOp { op; args; tys }, m ->
      Expr.eappop ~tys ~args:(List.map f args) ~op:(Operator.translate op) m
    | ( ( EAssert _ | ELit _ | EApp _ | EArray _ | EVar _ | EExternal _ | EAbs _
        | EIfThenElse _ | ETuple _ | ETupleAccess _ | EInj _ | EStruct _
        | EStructAccess _ | EMatch _ | EDefault _ | EPureDefault _ | EEmptyError
        | EErrorOnEmpty _ | ECustom _ ),
        _ ) as e ->
      Expr.map ~f e
    | _ -> .
  in
  (* /!\ don't be tempted to use the same trick here, the function does one
     thing: validate at runtime that the term does not contain [ECustom]
     nodes. *)
  Expr.unbox (f e)

(** Transform any DCalc expression into a concolic expression with no symbolic
    expression and no constraints *)
let init_conc_expr (e : (('c, 'e) conc_interpr_kind, 'm) gexpr) : conc_expr =
  let e = Concrete.addcustom e in
  let f = set_conc_info SymbExpr.none [] in
  Expr.unbox (Expr.map_marks ~f e)

(* taken from z3backend but with the right types *)
(* TODO check if some should be used or removed *)
type context = {
  ctx_z3 : Z3.context;
  (* The Z3 context, used to create symbols and expressions *)
  ctx_decl : decl_ctx;
  (* The declaration context from the Catala program, containing information to
     precisely pretty print Catala expressions *)
  ctx_dummy_sort : Z3.Sort.sort;
  (* A dummy sort for lambda abstractions *)
  ctx_dummy_const : s_expr;
  (* A dummy expression for lambda abstractions *)
  ctx_z3enums : Z3.Sort.sort EnumName.Map.t;
  (* A map from Catala enumeration names to the corresponding Z3 datatype sort,
     from which we can retrieve constructors and accessors *)
  ctx_z3structs : Z3.Sort.sort StructName.Map.t;
  (* A map from Catala struct names to the corresponding Z3 sort, from which we
     can retrieve the constructor and the accessors *)
  ctx_z3unit : Z3.Sort.sort * s_expr;
      (* A pair containing the Z3 encodings of the unit type, encoded as a tuple
         of 0 elements, and the unit value *)
}

(** adds the mapping between the Catala struct [s] and the corresponding Z3
    datatype [sort] to the context **)
let add_z3struct (s : StructName.t) (sort : Z3.Sort.sort) (ctx : context) :
    context =
  { ctx with ctx_z3structs = StructName.Map.add s sort ctx.ctx_z3structs }

(** adds the mapping between the Catala enum [enum] and the corresponding Z3
    datatype [sort] to the context **)
let add_z3enum (enum : EnumName.t) (sort : Z3.Sort.sort) (ctx : context) :
    context =
  { ctx with ctx_z3enums = EnumName.Map.add enum sort ctx.ctx_z3enums }

(** Z3 encoding/decoding helpers *)

let z3_int_of_bigint ctx (n : Z.t) : s_expr =
  (* NOTE CONC I use string instead of int to translate without overflows, as
     both [Runtime.integer] and Z3 integers are big *)
  Z3.Arithmetic.Integer.mk_numeral_s ctx.ctx_z3 (Runtime.integer_to_string n)

let make_z3_path_constraint (expr : SymbExpr.t) (pos : Pos.t) (branch : bool) :
    path_constraint =
  let expr =
    match expr with
    | Symb_z3 e -> Pc_z3 e
    | _ ->
      invalid_arg "[make_z3_path_constraint] expects a z3 symbolic expression"
  in
  { expr; pos; branch }

let make_reentrant_path_constraint
    (ctx : context)
    (expr : SymbExpr.t)
    (pos : Pos.t)
    (branch : bool) : path_constraint option =
  let expr =
    match expr with
    | Symb_reentrant { name; _ } ->
      Some (Pc_reentrant { name; is_empty = branch })
    | Symb_z3 s when Z3.Expr.equal s ctx.ctx_dummy_const ->
      (* If the symbolic expression is the dummy, it means that the default
         being evaluated is in a scope called by the scope under analysis. Thus
         the context variable is not an input variable of the concolic engine,
         and its evaluation should not generate a path constraint. *)
      None
    | _ ->
      Message.raise_spanned_error pos
        "[make_reentrant_path_constraint] expects reentrant symbolic \
         expression or dummy const but got %a"
        SymbExpr.formatter expr
  in
  Option.bind expr (fun expr -> Some { expr; pos; branch })

let integer_of_symb_expr (e : s_expr) : Runtime.integer =
  match Z3.Sort.get_sort_kind (Z3.Expr.get_sort e) with
  | Z3enums.INT_SORT -> Z3.Arithmetic.Integer.get_big_int e
  | _ -> invalid_arg "[integer_of_symb_expr] expected a Z3 Integer"

let bool_of_symb_expr (e : s_expr) : Runtime.bool =
  match Z3.Sort.get_sort_kind (Z3.Expr.get_sort e) with
  | Z3enums.BOOL_SORT -> begin
    match Z3.Boolean.get_bool_value e with
    | L_FALSE -> false
    | L_TRUE -> true
    | L_UNDEF -> failwith "boolean value undefined"
  end
  | _ -> invalid_arg "[bool_of_symb_expr] expected a Z3 Boolean"

let decimal_of_symb_expr (e : s_expr) : Runtime.decimal =
  match Z3.Sort.get_sort_kind (Z3.Expr.get_sort e) with
  | Z3enums.REAL_SORT -> Z3.Arithmetic.Real.get_ratio e
  | _ -> invalid_arg "[decimal_of_symb_expr] expected a Z3 Real"

module DateEncoding = struct
  (* For now a date is represented by the number of days since UNIX epoch
     (1970-01-01) *)
  let mk_sort_date ctx = Z3.Arithmetic.Integer.mk_sort ctx.ctx_z3

  (* A duration is a number of days *)
  let mk_sort_duration ctx = Z3.Arithmetic.Integer.mk_sort ctx.ctx_z3

  let encode_duration ctx (dur : Runtime.duration) : s_expr =
    let y, m, d = Runtime.duration_to_years_months_days dur in
    if y <> 0 || m <> 0 then
      failwith
        "[DateEncoding] Duration literals containing years or months not \
         supported";
    z3_int_of_bigint ctx (Z.of_int d)

  let decode_duration (e : s_expr) : Runtime.duration =
    let days_bigint = integer_of_symb_expr e in
    let days_int =
      Z.to_int
        days_bigint (* TODO catch overflow? durations are ints anyways... *)
    in
    Runtime.duration_of_numbers 0 0 days_int

  let default_duration : Runtime.duration = Runtime.duration_of_numbers 0 0 10
  let base_day : Runtime.date = Runtime.date_of_numbers 1970 1 1

  (** [date_to_bigint] translates [d] to an integer corresponding to the number
      of days since the base date. Adapted from z3backend *)
  let date_to_bigint (d : Runtime.date) : Z.t =
    let period = Runtime.Oper.o_sub_dat_dat d base_day in
    let y, m, d = Runtime.duration_to_years_months_days period in
    assert (y = 0 && m = 0);
    Z.of_int d

  let encode_date (ctx : context) (date : Runtime.date) : s_expr =
    let days = date_to_bigint date in
    z3_int_of_bigint ctx days

  let decode_date
      ?(round : Runtime.date_rounding = Dates_calc.Dates.AbortOnRound)
      (* NOTE: no rounding for now *)
        (e : s_expr) : Runtime.date =
    let days = decode_duration e in
    Runtime.o_add_dat_dur round base_day days

  (* Default date is epoch *)
  let default_date : Runtime.date = base_day

  (* operations *)

  (* Date operations are simply integer operations for now *)
  let minus_dur = Z3.Arithmetic.mk_unary_minus
  let add_dat_dur = Z3.Arithmetic.mk_add
  (* For now, the Z3 encoding does not care about rounding because both dates
     and durations are just numbers of days *)

  let add_dur_dur = Z3.Arithmetic.mk_add

  let sub_dat_dat =
    Z3.Arithmetic.mk_sub (* NOTE: always a number of days in [Dates_calc] *)

  let sub_dat_dur = Z3.Arithmetic.mk_sub
  let sub_dur_dur = Z3.Arithmetic.mk_sub
  let mult_dur_int = Z3.Arithmetic.mk_mul
  let div_dur_dur = Z3.Arithmetic.mk_div

  (* TODO CONC REU incompleteness warning for comparisons? eg on day < month *)
  let lt_dat_dat = Z3.Arithmetic.mk_lt
  let lt_dur_dur = Z3.Arithmetic.mk_lt
  let lte_dat_dat = Z3.Arithmetic.mk_le
  let lte_dur_dur = Z3.Arithmetic.mk_le
  let gt_dat_dat = Z3.Arithmetic.mk_gt
  let gt_dur_dur = Z3.Arithmetic.mk_gt
  let gte_dat_dat = Z3.Arithmetic.mk_ge
  let gte_dur_dur = Z3.Arithmetic.mk_ge
  let eq_dat_dat = Z3.Boolean.mk_eq
  let eq_dur_dur = Z3.Boolean.mk_eq
end

(** [translate_typ_lit] returns the Z3 sort corresponding to the Catala literal
    type [t] **)
let translate_typ_lit (ctx : context) (t : typ_lit) : Z3.Sort.sort =
  match t with
  | TBool -> Z3.Boolean.mk_sort ctx.ctx_z3
  | TUnit -> fst ctx.ctx_z3unit
  | TInt -> Z3.Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TRat -> Z3.Arithmetic.Real.mk_sort ctx.ctx_z3
  | TMoney -> Z3.Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TDate -> DateEncoding.mk_sort_date ctx
  | TDuration -> DateEncoding.mk_sort_duration ctx

(** [translate_typ] returns the Z3 sort correponding to the Catala type [t] **)
let rec translate_typ (ctx : context) (t : naked_typ) : context * Z3.Sort.sort =
  match t with
  | TLit t -> ctx, translate_typ_lit ctx t
  | TStruct name ->
    find_or_create_struct ctx name
    (* DONE CONC are declarations sorted in topological order? "Yes" -Denis *)
    (* use [type_ordering] from Driver to make sure ? =>> actually it does not
       work because the input struct for scope [A], called [A_in], is not a part
       of this order *)
  | TTuple _ -> failwith "[translate_typ] TTuple not implemented"
  | TEnum name -> find_or_create_enum ctx name
  | TOption _ -> failwith "[translate_typ] TOption not implemented"
  | TArrow ([(TLit TUnit, _)], (TDefault _, _)) ->
    (* context variable *)
    ctx, ctx.ctx_dummy_sort
  | TArrow _ -> ctx, ctx.ctx_dummy_sort (* other functions *)
  | TArray _ -> failwith "[translate_typ] TArray not implemented"
  | TAny -> failwith "[translate_typ] TAny not implemented"
  | TClosureEnv -> failwith "[translate_typ] TClosureEnv not implemented"
  | TDefault _ -> failwith "[translate_typ] TDefault not implemented"

(* taken from z3backend's find_or_create_struct *)
and find_or_create_struct (ctx : context) (s : StructName.t) :
    context * Z3.Sort.sort =
  Message.emit_debug "[Struct] Find or create struct %s"
    (Mark.remove (StructName.get_info s));
  match StructName.Map.find_opt s ctx.ctx_z3structs with
  | Some s ->
    Message.emit_debug "[Struct] . found!";
    ctx, s
  | None ->
    let s_name = Mark.remove (StructName.get_info s) in
    let fields = StructName.Map.find s ctx.ctx_decl.ctx_structs in

    let mk_struct_s = "mk!" ^ s_name (* struct constructor *) in
    let is_struct_s = "is!" ^ s_name (* recognizer *) in
    let z3_fieldnames =
      List.map
        (fun f ->
          let raw_field_name = Mark.remove (StructField.get_info f) in
          let field_name = "fd!" ^ raw_field_name in
          Z3.Symbol.mk_string ctx.ctx_z3 field_name)
        (StructField.Map.keys fields)
    in
    let ctx, z3_fieldtypes_rev =
      StructField.Map.fold
        (fun f ty (ctx, ftypes) ->
          Message.emit_debug "[Struct] . %s : %a"
            (Mark.remove (StructField.get_info f))
            Print.typ_debug ty;
          let ctx, ftype = translate_typ ctx (Mark.remove ty) in
          ctx, ftype :: ftypes)
        fields (ctx, [])
    in
    let z3_fieldtypes = List.rev z3_fieldtypes_rev in
    let z3_sortrefs =
      List.map (fun _ -> 0) z3_fieldtypes (* will not be used *)
    in

    let z3_mk_struct =
      Z3.Datatype.mk_constructor_s ctx.ctx_z3 mk_struct_s
        (Z3.Symbol.mk_string ctx.ctx_z3 is_struct_s)
        z3_fieldnames
        (List.map Option.some z3_fieldtypes)
        z3_sortrefs
    in
    let z3_struct = Z3.Datatype.mk_sort_s ctx.ctx_z3 s_name [z3_mk_struct] in
    add_z3struct s z3_struct ctx, z3_struct

(* inspired by z3backend *)
and find_or_create_enum (ctx : context) (enum : EnumName.t) :
    context * Z3.Sort.sort =
  Message.emit_debug "[Enum] Find or create enum %s"
    (Mark.remove (EnumName.get_info enum));

  let create_constructor (name : EnumConstructor.t) (ty : typ) (ctx : context) :
      context * Z3.Datatype.Constructor.constructor =
    let cstr_name = Mark.remove (EnumConstructor.get_info name) in
    let mk_cstr_s = "mk!" ^ cstr_name (* case constructor *) in
    let is_cstr_s = "is!" ^ cstr_name (* recognizer *) in
    let fieldname_s = cstr_name ^ "!0" (* name of the argument *) in
    let ctx, z3_arg_ty = translate_typ ctx (Mark.remove ty) in
    let z3_sortrefs = [0] (* will not be used *) in
    Message.emit_debug "[Enum] . %s : %a" cstr_name Print.typ_debug ty;
    ( ctx,
      Z3.Datatype.mk_constructor_s ctx.ctx_z3 mk_cstr_s
        (Z3.Symbol.mk_string ctx.ctx_z3 is_cstr_s)
        [Z3.Symbol.mk_string ctx.ctx_z3 fieldname_s]
        [Some z3_arg_ty] z3_sortrefs )
  in

  match EnumName.Map.find_opt enum ctx.ctx_z3enums with
  | Some e ->
    Message.emit_debug "[Enum] . found!";
    ctx, e
  | None ->
    let ctrs = EnumName.Map.find enum ctx.ctx_decl.ctx_enums in
    let ctx, z3_ctrs =
      EnumConstructor.Map.fold
        (fun ctr ty (ctx, ctrs) ->
          let ctx, ctr = create_constructor ctr ty ctx in
          ctx, ctr :: ctrs)
        ctrs (ctx, [])
    in
    let z3_enum =
      Z3.Datatype.mk_sort_s ctx.ctx_z3
        (Mark.remove (EnumName.get_info enum))
        (List.rev z3_ctrs)
    in
    add_z3enum enum z3_enum ctx, z3_enum

(** [create_z3unit] creates a Z3 sort and expression corresponding to the unit
    type and value respectively. Concretely, we represent unit as a tuple with 0
    elements. Taken from z3backend. **)
let create_z3unit (ctx : Z3.context) : Z3.Sort.sort * Z3.Expr.expr =
  let unit_sort = Z3.Tuple.mk_sort ctx (Z3.Symbol.mk_string ctx "unit") [] [] in
  let mk_unit = Z3.Tuple.get_mk_decl unit_sort in
  let unit_val = Z3.Expr.mk_app ctx mk_unit [] in
  unit_sort, unit_val

(* taken from z3backend, but without the option check *)
let make_empty_context (decl_ctx : decl_ctx) : context =
  let z3_cfg = ["model", "true"; "proof", "false"] in
  let z3_ctx = Z3.mk_context z3_cfg in
  let z3_dummy_sort = Z3.Sort.mk_uninterpreted_s z3_ctx "!dummy_sort!" in
  let z3_dummy_const =
    Z3.Expr.mk_const_s z3_ctx "!dummy_const!" z3_dummy_sort
  in
  {
    ctx_z3 = z3_ctx;
    ctx_decl = decl_ctx;
    (*     ctx_funcdecl = Var.Map.empty; *)
    (*     ctx_z3vars = StringMap.empty; *)
    ctx_z3enums = EnumName.Map.empty;
    (* ctx_z3matchsubsts = Var.Map.empty; *)
    ctx_z3structs = StructName.Map.empty;
    ctx_z3unit = create_z3unit z3_ctx;
    (* ctx_z3constraints = []; *)
    ctx_dummy_sort = z3_dummy_sort;
    ctx_dummy_const = z3_dummy_const;
  }

let init_context (ctx : context) : context =
  (* create all struct sorts *)
  let ctx =
    StructName.Map.fold
      (fun s _ ctx -> fst (find_or_create_struct ctx s))
      ctx.ctx_decl.ctx_structs ctx
  in
  let ctx =
    EnumName.Map.fold
      (fun enum _ ctx -> fst (find_or_create_enum ctx enum))
      ctx.ctx_decl.ctx_enums ctx
  in
  ctx

(* loosely taken from z3backend, could be exposed instead? not necessarily,
   especially if they become plugins *)
let z3_of_lit ctx (l : lit) : s_expr =
  match l with
  | LBool b -> Z3.Boolean.mk_val ctx.ctx_z3 b
  | LInt n -> z3_int_of_bigint ctx n
  | LRat r ->
    Z3.Arithmetic.Real.mk_numeral_nd ctx.ctx_z3 (Z.to_int r.num)
      (Z.to_int r.den)
    (* assumption: numerator and denominator are integers *)
  | LMoney m ->
    let cents = Runtime.money_to_cents m in
    z3_int_of_bigint ctx cents
  | LUnit -> snd ctx.ctx_z3unit
  | LDate date -> DateEncoding.encode_date ctx date
  | LDuration dur -> DateEncoding.encode_duration ctx dur

let symb_of_lit ctx (l : lit) : SymbExpr.t = SymbExpr.mk_z3 (z3_of_lit ctx l)

let _get_symb_expr_unsafe (e : ((yes, 'e) conc_interpr_kind, conc_info) gexpr) :
    SymbExpr.t =
  let (Custom { custom; _ }) = Mark.get e in
  custom.symb_expr

(** Get the symbolic expression corresponding to concolic expression [e]. This
    function makes sure that an expression that can't be [EGenericError] does
    not have a [Symb_error] symbolic expression. *)
let get_symb_expr (e : conc_expr) : SymbExpr.t =
  match _get_symb_expr_unsafe e with
  | Symb_error _ ->
    invalid_arg
      "[get_symb_expr] an expression that can't be an error cannot have an \
       error symbolic expression"
  | _ as s -> s

(** Get the symbolic expression corresponding to concolic result [e]. The
    symbolic expression can be a [Symb_error] here *)
let get_symb_expr_r (e : conc_result) : SymbExpr.t = _get_symb_expr_unsafe e

let _get_constraints_unsafe (e : ((yes, 'e) conc_interpr_kind, conc_info) gexpr)
    : path_constraint list =
  let (Custom { custom; _ }) = Mark.get e in
  custom.constraints

(* Get constraints from concolic expression that cannot be an error. The
   signature of this function helps making sure that errors are always properly
   propagated during execution. By forcing [get_constraints_r] to be called
   explicitely if the expression is a result. *)
let get_constraints (e : conc_expr) : path_constraint list =
  _get_constraints_unsafe e

let get_constraints_r (e : conc_result) : path_constraint list =
  _get_constraints_unsafe e

let gather_constraints (es : conc_expr list) =
  let constraints = List.map get_constraints es in
  Message.emit_debug "gather_constraints is concatenating constraints";
  List.flatten constraints

let get_type (e : conc_expr) : typ option =
  let (Custom { custom; _ }) = Mark.get e in
  custom.ty

let make_z3_struct ctx (name : StructName.t) (es : conc_expr list) : s_expr =
  let sort = StructName.Map.find name ctx.ctx_z3structs in
  let constructor = List.hd (Z3.Datatype.get_constructors sort) in
  let z3_of_expr (e : conc_expr) : s_expr =
    (* To build a Z3 struct, all of the fields of the concolic struct must have
     * a z3 symbolic expression.
     * - Normal fields will have a z3 symbolic expression computed during their
     *   evaluation (just before this function is called)
     * - Context variables in input structures will have their reentrant
     *   symbolic expression: we can put a dummy z3 constant (of the designated
     *   dummy z3 sort), because this constant will not be accessed by a
     *   StructAccess. Indeed, the only access to a reentrant field is during a
     *   Default case that is handled in a specific way.
     * - Functions do not have a symbolic value for now, so they can also have
     *   the dummy z3 constant. They won't be accessed in a symbolic way because
     *   the only expression in which they can be used is an application: in such a
     *   case, the symbolic expression of the function is ignored and a new
     *   symbolic expression is computed concolically (following the symbolic
     *   expressoin of the body of function).
     * - If a field is not a function but has no symbolic value, an error is
     *   raised because its value should have been computed before.
     *)
    let e_symb = get_symb_expr e in
    match e_symb with
    | Symb_z3 s -> s
    | Symb_reentrant _ -> ctx.ctx_dummy_const
    | Symb_none -> (
      match Mark.remove e with
      | EAbs _ -> ctx.ctx_dummy_const
      | _ ->
        Message.raise_spanned_error (Expr.pos e)
          "Fields of structs that are not functions or context variables must \
           have a symbolic expression. This should not happen if the \
           evaluation of fields worked.")
    | Symb_error _ ->
      Message.raise_spanned_error (Expr.pos e)
        "Fields of structs cannot be errors when making the symbolic \
         expression. This should not happen if errors were handled properly."
  in
  let es_symb = List.map z3_of_expr es in
  Z3.Expr.mk_app ctx.ctx_z3 constructor es_symb

(* taken loosely from z3backend *)
let make_z3_struct_access
    ctx
    (name : StructName.t)
    (field : StructField.t)
    (struct_expr : SymbExpr.t)
    (field_expr : SymbExpr.t) : SymbExpr.t =
  match field_expr with
  | Symb_reentrant _ ->
    (* If the field is for a reentrant variable, we want the symbolic expression
       of the access to be the actual symbolic expression for the reentrant
       variable and not a Z3 struct access (which would return a dummy) *)
    field_expr
  | _ ->
    let sort = StructName.Map.find name ctx.ctx_z3structs in
    let fields = StructName.Map.find name ctx.ctx_decl.ctx_structs in
    let z3_accessors = List.hd (Z3.Datatype.get_accessors sort) in
    Message.emit_debug "struct accessors %s"
      (List.fold_left
         (fun acc a -> Z3.FuncDecl.to_string a ^ "," ^ acc)
         "" z3_accessors);
    let idx_mappings =
      List.combine (StructField.Map.keys fields) z3_accessors
    in
    let _, z3_accessor =
      List.find (fun (field1, _) -> StructField.equal field field1) idx_mappings
    in
    SymbExpr.app_z3
      (fun s -> Z3.Expr.mk_app ctx.ctx_z3 z3_accessor [s])
      struct_expr

let make_z3_enum_inj
    ctx
    (name : EnumName.t)
    (cons : EnumConstructor.t)
    (s : s_expr) =
  let sort = EnumName.Map.find name ctx.ctx_z3enums in
  let constructors = EnumName.Map.find name ctx.ctx_decl.ctx_enums in
  let z3_constructors = Z3.Datatype.get_constructors sort in
  Message.emit_debug "constructors %s"
    (List.fold_left
       (fun acc a -> Z3.FuncDecl.to_string a ^ "," ^ acc)
       "" z3_constructors);
  (* NOTE assumption: they are in the right order *)
  (* TODO for all instances of this "mappings" pattern, maybe have more
     information in the context to avoid it *)
  let idx_mappings =
    List.combine (EnumConstructor.Map.keys constructors) z3_constructors
  in
  let _, z3_constructor =
    List.find (fun (cons1, _) -> EnumConstructor.equal cons cons1) idx_mappings
  in
  Z3.Expr.mk_app ctx.ctx_z3 z3_constructor [s]

let make_z3_enum_access
    ctx
    (name : EnumName.t)
    (cons : EnumConstructor.t)
    (s : s_expr) =
  let sort = EnumName.Map.find name ctx.ctx_z3enums in
  let constructors = EnumName.Map.find name ctx.ctx_decl.ctx_enums in
  (* [get_accessors] returns a list containing the list of accessors for each
     constructor. In a Catala enum, each constructor has exactly (possibly
     [unit]) accessor, so we can safely [List.hd]. *)
  let z3_accessors = List.map List.hd (Z3.Datatype.get_accessors sort) in
  Message.emit_debug "enum accessors %s"
    (List.fold_left
       (fun acc a -> Z3.FuncDecl.to_string a ^ "," ^ acc)
       "" z3_accessors);
  let idx_mappings =
    List.combine (EnumConstructor.Map.keys constructors) z3_accessors
  in
  let _, z3_accessor =
    List.find (fun (cons1, _) -> EnumConstructor.equal cons cons1) idx_mappings
  in
  Z3.Expr.mk_app ctx.ctx_z3 z3_accessor [s]

(** Return the list of conditions corresponding to a pattern match on
    enumeration [name], where the arm that matches [s] is [cons]. The condition
    is [is!C s] when [C] is the [cons] arm, and [not (is!C s)] for any other
    arm. Additionaly, return along the condition whether it corresponds to
    [cons]. *)
let make_z3_arm_conditions
    ctx
    (name : EnumName.t)
    (cons : EnumConstructor.t)
    (s : SymbExpr.t) : (SymbExpr.t * bool) list =
  let sort = EnumName.Map.find name ctx.ctx_z3enums in
  let constructors = EnumName.Map.find name ctx.ctx_decl.ctx_enums in
  let z3_recognizers = Z3.Datatype.get_recognizers sort in
  let make_case_condition cstr_name z3_recognizer =
    let app =
      SymbExpr.applist_z3 (Z3.Expr.mk_app ctx.ctx_z3 z3_recognizer) [s]
    in
    let is_cons = EnumConstructor.equal cons cstr_name in
    let prefix = if is_cons then fun x -> x else Z3.Boolean.mk_not ctx.ctx_z3 in
    SymbExpr.app_z3 prefix app, is_cons
  in
  (* NOTE assumption: the lists are in the right order *)
  List.map2 make_case_condition
    (EnumConstructor.Map.keys constructors)
    z3_recognizers

let make_vars_args_map
    (vars : conc_naked_expr Bindlib.var array)
    (args : conc_expr list) : (conc_expr, conc_expr) Var.Map.t =
  let zipped = Array.combine vars (Array.of_list args) in
  Array.fold_left (fun acc (v, a) -> Var.Map.add v a acc) Var.Map.empty zipped

let replace_EVar_mark
    (vars_args : (conc_expr, conc_expr) Var.Map.t)
    (e : conc_expr) : conc_expr =
  match Mark.remove e with
  | EVar v -> (
    match Var.Map.find_opt v vars_args with
    | Some arg ->
      let symb_expr = get_symb_expr arg in
      Message.emit_debug "EApp>binder put mark %a on var %a" SymbExpr.formatter
        symb_expr (Print.expr ()) e;
      add_conc_info_e symb_expr ~constraints:[] e
    (* NOTE CONC we keep the position from the var, as in concrete
       interpreter *)
    | None -> e)
  | _ -> e

(* TODO CONC REU *)
let propagate_generic_error
    (e : conc_result)
    (other_constraints : path_constraint list)
    (f : conc_expr -> conc_result) : conc_result =
  let e_symb = get_symb_expr_r e in
  match Mark.remove e, e_symb with
  | EGenericError, Symb_error _ ->
    let e_constraints = get_constraints_r e in
    Message.emit_debug
      "Propagating error %a with %d contained constraints and %d other \
       constraints"
      SymbExpr.formatter e_symb
      (List.length e_constraints)
      (List.length other_constraints);
    let constraints = e_constraints @ other_constraints in
    (* Add the new constraints but don't change the symbolic expression *)
    add_conc_info_e Symb_none ~constraints e
  | _, Symb_error _ ->
    Message.raise_internal_error
      "A non-error case cannot have an error symbolic expression"
  | _, _ ->
    let e_noerror = del_genericerror e in
    (* NOTE LUNDI [del_genericerror] should not be too costly because [e] is
       supposed to be a value *)
    f e_noerror

(* TODO check order NOTE this evaluates [v1;v2;err;v4] to err(new_constraints @
   pc1 @ pc2) and ignores pc4 *)
let propagate_generic_error_list l other_constraints f =
  let rec aux acc constraints = function
    | [] -> f (List.rev acc)
    | e :: r ->
      propagate_generic_error e constraints
        (* FIXME this seems very slow *)
        (fun e -> aux (e :: acc) (get_constraints e @ constraints) r)
  in
  aux [] other_constraints l

(* TODO CONC REU Rewrite EmptyError propagation functions from [Concrete]
   because they don't allow for [f] have a different input and output type *)
let propagate_empty_error (e : conc_expr) (f : conc_expr -> conc_result) :
    conc_result =
  match e with (EEmptyError, _) as e -> e | _ -> f e

let propagate_empty_error_list
    (elist : conc_expr list)
    (f : conc_expr list -> conc_result) : conc_result =
  let rec aux acc = function
    | [] -> f (List.rev acc)
    | e :: r -> propagate_empty_error e (fun e -> aux (e :: acc) r)
  in
  aux [] elist

let handle_eq evaluate_operator pos lang e1 e2 =
  let open Runtime.Oper in
  match e1, e2 with
  | ELit LUnit, ELit LUnit -> true
  | ELit (LBool b1), ELit (LBool b2) -> not (o_xor b1 b2)
  | ELit (LInt x1), ELit (LInt x2) -> o_eq_int_int x1 x2
  | ELit (LRat x1), ELit (LRat x2) -> o_eq_rat_rat x1 x2
  | ELit (LMoney x1), ELit (LMoney x2) -> o_eq_mon_mon x1 x2
  | ELit (LDuration x1), ELit (LDuration x2) -> o_eq_dur_dur x1 x2
  | ELit (LDate x1), ELit (LDate x2) -> o_eq_dat_dat x1 x2
  | EArray _, EArray _ -> failwith "EOp Eq EArray not implemented"
  | EStruct { fields = es1; name = s1 }, EStruct { fields = es2; name = s2 } ->
    StructName.equal s1 s2
    && StructField.Map.equal
         (fun e1 e2 ->
           match Mark.remove (evaluate_operator Eq pos lang [e1; e2]) with
           | ELit (LBool b) -> b
           | _ -> assert false
           (* should not happen *))
         es1 es2
  | ( EInj { e = e1; cons = i1; name = en1 },
      EInj { e = e2; cons = i2; name = en2 } ) -> (
    try
      EnumName.equal en1 en2
      && EnumConstructor.equal i1 i2
      &&
      match Mark.remove (evaluate_operator Eq pos lang [e1; e2]) with
      | ELit (LBool b) -> b
      | _ -> assert false
      (* should not happen *)
    with Invalid_argument _ -> false)
  | _, _ -> false (* comparing anything else return false *)

let op1
    ctx
    m
    (concrete_f : 'x -> conc_naked_result)
    (symbolic_f : Z3.context -> s_expr -> s_expr)
    x
    e : conc_result =
  let concrete = concrete_f x in
  let e = get_symb_expr e in
  let symb_expr = SymbExpr.app_z3 (symbolic_f ctx.ctx_z3) e in
  (* TODO handle errors *)
  add_conc_info_m m symb_expr ~constraints:[] concrete

let op2
    ctx
    m
    (concrete_f : 'x -> 'y -> conc_naked_result)
    (symbolic_f : Z3.context -> s_expr -> s_expr -> s_expr)
    ?(constraints : path_constraint list = [])
    x
    y
    e1
    e2 : conc_result =
  let concrete = concrete_f x y in
  let e1 = get_symb_expr e1 in
  let e2 = get_symb_expr e2 in
  Message.emit_debug "[op2] args %a, %a" SymbExpr.formatter_typed e1
    SymbExpr.formatter_typed e2;
  let symb_expr = SymbExpr.app2_z3 (symbolic_f ctx.ctx_z3) e1 e2 in
  (* TODO handle errors *)
  add_conc_info_m m symb_expr ~constraints concrete

let op2list
    ctx
    m
    (concrete_f : 'x -> 'y -> conc_naked_result)
    (symbolic_f : Z3.context -> s_expr list -> s_expr)
    ?(constraints : path_constraint list = [])
    x
    y
    e1
    e2 : conc_result =
  let symbolic_f_curry ctx e1 e2 = symbolic_f ctx [e1; e2] in
  op2 ctx m concrete_f symbolic_f_curry ~constraints x y e1 e2

(** Round Z3 [Real] to Z3 [Integer] using the same strategy as [Runtime.round]:
    round to nearest, half away from zero. *)
let z3_round ctx (q : s_expr) : s_expr =
  (* The mathematical formula is [round(q) = sgn(q) * floor(abs(q) + 0.5)].
     However, Z3 does not have [sgn] or [abs] functions. Instead, we encode it
     as [round(q) = if q>=0 then floor(q + 1/2) else -floor(-q + 1/2)], where
     [Z3.Arithmetic.Real.mk_real2int] floors. *)
  (* NOTE why not define Z3 function? =>> they are the same as declaration +
     forall, and the gain in legibility is marginal, so I don't think it is
     necessary *)
  let zero = Z3.Arithmetic.Real.mk_numeral_i ctx 0 in
  let is_positive = Z3.Arithmetic.mk_ge ctx q zero in

  let half = Z3.Arithmetic.Real.mk_numeral_nd ctx 1 2 in

  let shift_pos = Z3.Arithmetic.mk_add ctx [q; half] in
  let round_pos = Z3.Arithmetic.Real.mk_real2int ctx shift_pos in

  let shift_neg =
    Z3.Arithmetic.mk_add ctx [Z3.Arithmetic.mk_unary_minus ctx q; half]
  in
  let round_neg =
    Z3.Arithmetic.mk_unary_minus ctx
      (Z3.Arithmetic.Real.mk_real2int ctx shift_neg)
  in

  Z3.Boolean.mk_ite ctx is_positive round_pos round_neg

(* Call-by-value: the arguments are expected to be already evaluated here *)
let rec evaluate_operator
    evaluate_expr
    ctx
    (op : < overloaded : no ; .. > operator)
    m
    lang
    (args : conc_expr list) : conc_result =
  let pos = Expr.mark_pos m in
  let protect f x y =
    let get_binop_args_pos = function
      | (arg0 :: arg1 :: _ : ('t, 'm) gexpr list) ->
        [None, Expr.pos arg0; None, Expr.pos arg1]
      | _ -> assert false
    in
    try f x y with
    | Division_by_zero ->
      Message.raise_multispanned_error
        [
          Some "The division operator:", pos;
          Some "The null denominator:", Expr.pos (List.nth args 1);
        ]
        "division by zero at runtime"
    | Runtime.UncomparableDurations ->
      Message.raise_multispanned_error (get_binop_args_pos args)
        "Cannot compare together durations that cannot be converted to a \
         precise number of days"
  in
  let err () =
    Message.raise_multispanned_error
      ([
         ( Some
             (Format.asprintf "Operator (value %a):"
                (Print.operator ~debug:true)
                op),
           pos );
       ]
      @ List.mapi
          (fun i arg ->
            ( Some
                (Format.asprintf "Argument n°%d, value %a" (i + 1)
                   (Print.UserFacing.expr lang)
                   arg),
              Expr.pos arg ))
          args)
      "Operator %a applied to the wrong arguments\n\
       (should not happen if the term was well-typed)%a"
      (Print.operator ~debug:true)
      op (Print.expr ())
      (EAppOp { op; tys = []; args }, m)
  in
  propagate_empty_error_list args
  @@ fun args ->
  let open Runtime.Oper in
  (* Mark.add m @@ *)
  match op, args with
  | Length, _ -> failwith "EOp Length not implemented"
  | Log _, _ -> failwith "Eop Log not implemented"
  | (FromClosureEnv | ToClosureEnv), _ ->
    (* NOTE CONC used for typing only *)
    failwith "Eop From/ToClosureEnv should not appear in concolic evaluation"
  | Eq, [e1; e2] ->
    let e1' = Mark.remove e1 in
    let e2' = Mark.remove e2 in
    let concrete =
      ELit
        (LBool (handle_eq (evaluate_operator evaluate_expr ctx) m lang e1' e2'))
    in
    let s_e1 = get_symb_expr e1 in
    let s_e2 = get_symb_expr e2 in
    let symb_expr = SymbExpr.app2_z3 (Z3.Boolean.mk_eq ctx.ctx_z3) s_e1 s_e2 in
    (* TODO catch errors here, or maybe propagate [None]? *)
    add_conc_info_m m symb_expr ~constraints:[] concrete
  | Map, _ -> failwith "Eop Map not implemented"
  | Reduce, _ -> failwith "Eop Reduce not implemented"
  (* | Reduce, _ -> failwith "Eop Reduce not implemented" *)
  | Concat, _ -> failwith "Eop Concat not implemented"
  | Filter, _ -> failwith "Eop Filter not implemented"
  | Fold, _ -> failwith "Eop Fold not implemented"
  (* Length | Log _ *)
  | Eq (* | Map | Concat | Filter | Fold | Reduce *), _ -> err ()
  | Not, [((ELit (LBool b), _) as e)] ->
    op1 ctx m (fun x -> ELit (LBool (o_not x))) Z3.Boolean.mk_not b e
  | GetDay, _ -> failwith "Eop GetDay not implemented"
  | GetMonth, _ -> failwith "Eop GetMonth not implemented"
  | GetYear, _ -> failwith "Eop GetYear not implemented"
  | FirstDayOfMonth, _ -> failwith "Eop FirstDayOfMonth not implemented"
  | LastDayOfMonth, _ -> failwith "Eop LastDayOfMonth not implemented"
  | And, [((ELit (LBool b1), _) as e1); ((ELit (LBool b2), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LBool (o_and x y)))
      Z3.Boolean.mk_and b1 b2 e1 e2
  | Or, [((ELit (LBool b1), _) as e1); ((ELit (LBool b2), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LBool (o_or x y)))
      Z3.Boolean.mk_or b1 b2 e1 e2
  | Xor, [((ELit (LBool b1), _) as e1); ((ELit (LBool b2), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_xor x y)))
      Z3.Boolean.mk_xor b1 b2 e1 e2
  | ( ( Not
        (* | GetDay | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth *)
      | And | Or | Xor ),
      _ ) ->
    err ()
  | Minus_int, [((ELit (LInt x), _) as e)] ->
    op1 ctx m
      (fun x -> ELit (LInt (o_minus_int x)))
      Z3.Arithmetic.mk_unary_minus x e
  | Minus_rat, [((ELit (LRat x), _) as e)] ->
    op1 ctx m
      (fun x -> ELit (LRat (o_minus_rat x)))
      Z3.Arithmetic.mk_unary_minus x e
  | Minus_mon, [((ELit (LMoney x), _) as e)] ->
    op1 ctx m
      (fun x -> ELit (LMoney (o_minus_mon x)))
      Z3.Arithmetic.mk_unary_minus x e
    (* TODO CONC maybe abstract this symbolic operation? like s_minus_mon and
       s_minus_int... *)
  | Minus_dur, [((ELit (LDuration x), _) as e)] ->
    op1 ctx m
      (fun x -> ELit (LDuration (o_minus_dur x)))
      DateEncoding.minus_dur x e
  | ToRat_int, [((ELit (LInt i), _) as e)] ->
    (* TODO maybe write specific tests for this and other similar cases? *)
    op1 ctx m
      (fun x -> ELit (LRat (o_torat_int x)))
      (fun ctx e ->
        let one = Z3.Arithmetic.Integer.mk_numeral_i ctx 1 in
        Z3.Arithmetic.mk_div ctx e one)
      i e
  | ToRat_mon, [((ELit (LMoney i), _) as e)] ->
    op1 ctx m
      (fun x -> ELit (LRat (o_torat_mon x)))
      (fun ctx e ->
        let hundred = Z3.Arithmetic.Integer.mk_numeral_i ctx 100 in
        Z3.Arithmetic.mk_div ctx e hundred)
      i e
    (* TODO test *)
  | ToMoney_rat, [((ELit (LRat i), _) as e)] ->
    (* TODO be careful with this, [Round_mon], [Round_rat], [Mult_mon_rat],
       [Div_mon_rat] because of rounding *)
    (* DONE test *)
    op1 ctx m
      (fun x -> ELit (LMoney (o_tomoney_rat x)))
      (fun ctx e ->
        let cents =
          Z3.Arithmetic.mk_mul ctx [e; Z3.Arithmetic.Real.mk_numeral_i ctx 100]
        in
        z3_round ctx cents)
      i e
  | Round_mon, [((ELit (LMoney mon), _) as e)] ->
    op1 ctx m
      (fun mon -> ELit (LMoney (o_round_mon mon)))
      (fun ctx e ->
        (* careful here: use multiplication by 1/100 instead of division by 100
           to prevent [units] from being an integer (rounded down automatically
           by Z3) *)
        let units =
          Z3.Arithmetic.mk_mul ctx
            [e; Z3.Arithmetic.Real.mk_numeral_nd ctx 1 100]
        in
        let units_round = z3_round ctx units in
        Z3.Arithmetic.mk_mul ctx
          [units_round; Z3.Arithmetic.Integer.mk_numeral_i ctx 100])
      mon e
  (* DONE with careful rounding *)
  | Round_rat, [((ELit (LRat q), _) as e)] ->
    op1 ctx m
      (fun q -> ELit (LRat (o_round_rat q)))
      (fun ctx e -> z3_round ctx e)
      q e
  (* DONE with careful rounding *)
  | Add_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LInt (o_add_int_int x y)))
      Z3.Arithmetic.mk_add x y e1 e2
  | Add_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LRat (o_add_rat_rat x y)))
      Z3.Arithmetic.mk_add x y e1 e2
  | Add_mon_mon, [((ELit (LMoney x), _) as e1); ((ELit (LMoney y), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LMoney (o_add_mon_mon x y)))
      Z3.Arithmetic.mk_add x y e1 e2
  | ( Add_dat_dur r,
      [((ELit (LDate x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2list ctx m
      (fun x y -> ELit (LDate (o_add_dat_dur r x y)))
      DateEncoding.add_dat_dur x y e1 e2
  | ( Add_dur_dur,
      [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2list ctx m
      (fun x y -> ELit (LDuration (o_add_dur_dur x y)))
      DateEncoding.add_dur_dur x y e1 e2
  | Sub_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LInt (o_sub_int_int x y)))
      Z3.Arithmetic.mk_sub x y e1 e2
  | Sub_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LRat (o_sub_rat_rat x y)))
      Z3.Arithmetic.mk_sub x y e1 e2
  | Sub_mon_mon, [((ELit (LMoney x), _) as e1); ((ELit (LMoney y), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LMoney (o_sub_mon_mon x y)))
      Z3.Arithmetic.mk_sub x y e1 e2
  | Sub_dat_dat, [((ELit (LDate x), _) as e1); ((ELit (LDate y), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LDuration (o_sub_dat_dat x y)))
      DateEncoding.sub_dat_dat x y e1 e2
  | Sub_dat_dur, [((ELit (LDate x), _) as e1); ((ELit (LDuration y), _) as e2)]
    ->
    op2list ctx m
      (fun x y -> ELit (LDate (o_sub_dat_dur x y)))
      DateEncoding.sub_dat_dur x y e1 e2
  | ( Sub_dur_dur,
      [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2list ctx m
      (fun x y -> ELit (LDuration (o_sub_dur_dur x y)))
      DateEncoding.sub_dur_dur x y e1 e2
  | Mult_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LInt (o_mult_int_int x y)))
      Z3.Arithmetic.mk_mul x y e1 e2
  | Mult_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2list ctx m
      (fun x y -> ELit (LRat (o_mult_rat_rat x y)))
      Z3.Arithmetic.mk_mul x y e1 e2
  | Mult_mon_rat, [((ELit (LMoney x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LMoney (o_mult_mon_rat x y)))
      (fun ctx cents r ->
        let product = Z3.Arithmetic.mk_mul ctx [cents; r] in
        z3_round ctx product)
      x y e1 e2
  (* DONE with careful rounding *)
  | Mult_dur_int, [((ELit (LDuration x), _) as e1); ((ELit (LInt y), _) as e2)]
    ->
    op2list ctx m
      (fun x y -> ELit (LDuration (o_mult_dur_int x y)))
      DateEncoding.mult_dur_int x y e1 e2
  | Div_int_int, _ -> failwith "EOp Div_int_int not implemented (division)"
  (* | Div_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
     op2 ctx m (fun x y -> ELit (LRat (protect o_div_int_int x y)))
     Z3.Arithmetic.mk_div x y e1 e2 (* TODO test this specifically? *) *)
  | Div_rat_rat, _ -> failwith "EOp Div_rat_rat not implemented (division)"
  (* | Div_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
     op2 ctx m (fun x y -> ELit (LRat (protect o_div_rat_rat x y)))
     Z3.Arithmetic.mk_div x y e1 e2 *)
  | Div_mon_mon, _ -> failwith "Eop Div_mon_mon not implemented (division)"
  | Div_mon_rat, _ -> failwith "Eop Div_mon_rat not implemented (division)"
  (* TODO with careful rounding *)
  | Div_dur_dur, _ -> failwith "EOp Div_dur_dur not implemented (division)"
  (* | ( Div_dur_dur, [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _)
     as e2)] ) -> op2 ctx m (fun x y -> ELit (LRat (protect o_div_dur_dur x y)))
     DateEncoding.div_dur_dur x y e1 e2 *)
  | Lt_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_lt_int_int x y)))
      Z3.Arithmetic.mk_lt x y e1 e2
  | Lt_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_lt_rat_rat x y)))
      Z3.Arithmetic.mk_lt x y e1 e2
  | Lt_mon_mon, [((ELit (LMoney x), _) as e1); ((ELit (LMoney y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_lt_mon_mon x y)))
      Z3.Arithmetic.mk_lt x y e1 e2
  | Lt_dat_dat, [((ELit (LDate x), _) as e1); ((ELit (LDate y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_lt_dat_dat x y)))
      DateEncoding.lt_dat_dat x y e1 e2
  | ( Lt_dur_dur,
      [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2 ctx m
      (fun x y -> ELit (LBool (protect o_lt_dur_dur x y)))
      DateEncoding.lt_dur_dur x y e1 e2
  | Lte_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_lte_int_int x y)))
      Z3.Arithmetic.mk_le x y e1 e2
  | Lte_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_lte_rat_rat x y)))
      Z3.Arithmetic.mk_le x y e1 e2
  | Lte_mon_mon, [((ELit (LMoney x), _) as e1); ((ELit (LMoney y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_lte_mon_mon x y)))
      Z3.Arithmetic.mk_le x y e1 e2
  | Lte_dat_dat, [((ELit (LDate x), _) as e1); ((ELit (LDate y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_lte_dat_dat x y)))
      DateEncoding.lte_dat_dat x y e1 e2
  | ( Lte_dur_dur,
      [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2 ctx m
      (fun x y -> ELit (LBool (protect o_lte_dur_dur x y)))
      DateEncoding.lte_dur_dur x y e1 e2
  | Gt_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_gt_int_int x y)))
      Z3.Arithmetic.mk_gt x y e1 e2
  | Gt_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_gt_rat_rat x y)))
      Z3.Arithmetic.mk_gt x y e1 e2
  | Gt_mon_mon, [((ELit (LMoney x), _) as e1); ((ELit (LMoney y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_gt_mon_mon x y)))
      Z3.Arithmetic.mk_gt x y e1 e2
  | Gt_dat_dat, [((ELit (LDate x), _) as e1); ((ELit (LDate y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_gt_dat_dat x y)))
      DateEncoding.gt_dat_dat x y e1 e2
  | ( Gt_dur_dur,
      [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2 ctx m
      (fun x y -> ELit (LBool (protect o_gt_dur_dur x y)))
      DateEncoding.gt_dur_dur x y e1 e2
  | Gte_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_gte_int_int x y)))
      Z3.Arithmetic.mk_ge x y e1 e2
  | Gte_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_gte_rat_rat x y)))
      Z3.Arithmetic.mk_ge x y e1 e2
  | Gte_mon_mon, [((ELit (LMoney x), _) as e1); ((ELit (LMoney y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_gte_mon_mon x y)))
      Z3.Arithmetic.mk_ge x y e1 e2
  | Gte_dat_dat, [((ELit (LDate x), _) as e1); ((ELit (LDate y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_gte_dat_dat x y)))
      DateEncoding.gte_dat_dat x y e1 e2
  | ( Gte_dur_dur,
      [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2 ctx m
      (fun x y -> ELit (LBool (protect o_gte_dur_dur x y)))
      DateEncoding.gte_dur_dur x y e1 e2
  | Eq_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_eq_int_int x y)))
      Z3.Boolean.mk_eq x y e1 e2
  | Eq_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_eq_rat_rat x y)))
      Z3.Boolean.mk_eq x y e1 e2
  | Eq_mon_mon, [((ELit (LMoney x), _) as e1); ((ELit (LMoney y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_eq_mon_mon x y)))
      Z3.Boolean.mk_eq x y e1 e2
  | Eq_dat_dat, [((ELit (LDate x), _) as e1); ((ELit (LDate y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LBool (o_eq_dat_dat x y)))
      DateEncoding.eq_dat_dat x y e1 e2
  | ( Eq_dur_dur,
      [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2 ctx m
      (fun x y -> ELit (LBool (protect o_eq_dur_dur x y)))
      DateEncoding.eq_dur_dur x y e1 e2
  | HandleDefault, _ ->
    Message.raise_internal_error
      "The concolic interpreter is trying to evaluate the \"handle_default\" \
       operator, which should not happen with a DCalc AST"
  | HandleDefaultOpt, _ ->
    Message.raise_internal_error
      "The concolic interpreter is trying to evaluate the \
       \"handle_default_opt\" operator, which should not happen with a DCalc \
       AST"
  | ( ( Minus_int | Minus_rat | Minus_mon | Minus_dur | ToRat_int | ToRat_mon
      | ToMoney_rat | Round_rat | Round_mon | Add_int_int | Add_rat_rat
      | Add_mon_mon | Add_dat_dur _ | Add_dur_dur | Sub_int_int | Sub_rat_rat
      | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur | Sub_dur_dur | Mult_int_int
      | Mult_rat_rat | Mult_mon_rat
      | Mult_dur_int
        (* | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_rat |
           Div_dur_dur *)
      | Lt_int_int | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur
      | Lte_int_int | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat | Lte_dur_dur
      | Gt_int_int | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur
      | Gte_int_int | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat | Gte_dur_dur
      | Eq_int_int | Eq_rat_rat | Eq_mon_mon | Eq_dat_dat | Eq_dur_dur ),
      _ ) ->
    err ()

let rec evaluate_expr : context -> Cli.backend_lang -> conc_expr -> conc_result
    =
 fun ctx lang e ->
  (* Message.emit_debug "eval %a\nsymbolic: %a" (Print.expr ()) e
     SymbExpr.formatter (get_symb_expr e); *)
  Message.emit_debug "eval symbolic: %a" SymbExpr.formatter (get_symb_expr e);
  let m = Mark.get e in
  let pos = Expr.mark_pos m in
  let ret =
    match Mark.remove e with
    | EVar _ ->
      Message.raise_spanned_error pos
        "free variable found at evaluation (should not happen if term was \
         well-typed)"
    | EExternal _ -> failwith "EExternal not implemented"
    | EApp { f = e1; args; _ } -> (
      Message.emit_debug "... it's an EApp";
      let e1 = evaluate_expr ctx lang e1 in
      Message.emit_debug "EApp f evaluated";
      propagate_generic_error e1 []
      @@ fun e1 ->
      let f_constraints = get_constraints e1 in
      let args = List.map (evaluate_expr ctx lang) args in
      Message.emit_debug "EApp args evaluated";
      propagate_generic_error_list args f_constraints
      @@ fun args ->
      let args_constraints = gather_constraints args in
      propagate_empty_error e1
      @@ fun e1 ->
      match Mark.remove e1 with
      | EAbs { binder; _ } ->
        (* TODO make this discussion into a doc? should constraints from the
           args be added, or should we trust the call to return them? We could
           take both for safety but there will be duplication... I think we
           should trust the recursive call, and we'll see if it's better not to
           =>> actually it's better not to : it can lead to duplication, and the
           subexpression does not need the constraints anyway =>> see the big
           concatenation below *)
        (* The arguments passed to [Bindlib.msubst] are unmarked. To circumvent
           this, I change the corresponding marks in the receiving expression,
           ie the expression in which substitution happens: 1/ unbind 2/ change
           marks 3/ rebind 4/ substitute concrete expressions normally *)
        if Bindlib.mbinder_arity binder = List.length args then (
          let vars, eb = Bindlib.unmbind binder in
          let vars_args_map = make_vars_args_map vars args in
          Message.emit_debug "EApp>EAbs vars are %a"
            (Format.pp_print_list Print.var_debug)
            (Array.to_list vars);
          (* Message.emit_debug "EApp>EAbs args are %a" (Format.pp_print_list
             (Print.expr ())) args; *)
          Message.emit_debug "EApp>EAbs args are";
          List.iter
            (fun arg ->
              Message.emit_debug "EApp>EAbs arg %a | %a | %i" (Print.expr ())
                arg SymbExpr.formatter (get_symb_expr arg)
                (List.length (get_constraints arg)))
            args;
          let marked_eb =
            Expr.map_top_down ~f:(replace_EVar_mark vars_args_map) eb
          in
          Message.emit_debug "EApp>EAbs vars replaced in box";
          let marked_binder = Bindlib.unbox (Expr.bind vars marked_eb) in
          Message.emit_debug "EApp>EAbs binder reconstructed";
          let result =
            evaluate_expr ctx lang
              (Bindlib.msubst marked_binder
                 (Array.of_list (List.map Mark.remove args)))
          in
          Message.emit_debug "EApp>EAbs substituted binder evaluated";
          (* TODO [Expr.subst]? *)
          propagate_generic_error result (args_constraints @ f_constraints)
          @@ fun result ->
          let r_symb = get_symb_expr result in
          Message.emit_debug
            "EApp>EAbs extracted symbolic expression from result: %a"
            SymbExpr.formatter r_symb;
          let r_constraints = get_constraints result in
          (* the constraints generated by the evaluation of the application are:
           * - those generated by the evaluation of the function
           * - those generated by the evaluation of the arguments
           * - the NEW ones generated by the evaluation of the subexpression
           *   (where the ones of the arguments are neither passed down, nor
           *   re-generated as this is cbv)
           *)
          let constraints = r_constraints @ args_constraints @ f_constraints in
          add_conc_info_e r_symb ~constraints result |> make_ok
          (* NOTE that here the position comes from [result], while in other
             cases of this function the position comes from the input
             expression. This is the behaviour of the concrete interpreter *))
        else
          Message.raise_spanned_error pos
            "wrong function call, expected %d arguments, got %d"
            (Bindlib.mbinder_arity binder)
            (List.length args)
      | ECustom _ -> failwith "EApp of ECustom not implemented"
      | _ ->
        Message.raise_spanned_error pos
          "function has not been reduced to a lambda at evaluation (should not \
           happen if the term was well-typed")
    | EAppOp { op; args; _ } ->
      Message.emit_debug "... it's an EAppOp";
      let args = List.map (evaluate_expr ctx lang) args in
      Message.emit_debug "EAppOp args evaluated";
      propagate_generic_error_list args []
      @@ fun args ->
      let args_constraints = gather_constraints args in
      let result =
        evaluate_operator (evaluate_expr ctx lang) ctx op m lang args
      in
      propagate_generic_error result args_constraints
      @@ fun result ->
      let r_symb = get_symb_expr result in
      let r_constraints = get_constraints result in
      (* the constraints generated by the evaluation of the evaluation of the
       * operation are:
       * - those generated by the evaluation of the arguments
       * - those possibly generated by the application of the operator to the
       *   arguments *)
      let constraints = r_constraints @ args_constraints in
      add_conc_info_e r_symb ~constraints result |> make_ok
    | EAbs { binder; tys } ->
      Message.emit_debug "... it's an EAbs";
      Expr.unbox (Expr.eabs (Bindlib.box binder) tys m) |> make_ok
      (* TODO simplify this once issue #540 is resolved *)
    | ELit l as e ->
      Message.emit_debug "... it's an ELit";
      let symb_expr = symb_of_lit ctx l in
      (* no constraints generated *)
      add_conc_info_m m symb_expr ~constraints:[] e
    (* | EAbs _ as e -> Marked.mark m e (* these are values *) *)
    | EStruct { fields = es; name } ->
      Message.emit_debug "... it's an EStruct";
      let fields, es = List.split (StructField.Map.bindings es) in

      (* compute all subexpressions *)
      let es = List.map (evaluate_expr ctx lang) es in
      propagate_generic_error_list es []
      @@ fun es ->
      (* make symbolic expression using the symbolic sub-expressions *)
      let symb_expr = SymbExpr.mk_z3 (make_z3_struct ctx name es) in

      (* TODO catch error... should not happen *)

      (* gather all constraints from sub-expressions *)
      let constraints = gather_constraints es in

      propagate_empty_error_list es
      @@ fun es ->
      add_conc_info_m m symb_expr ~constraints
        (EStruct
           {
             fields =
               StructField.Map.of_seq
                 (Seq.zip (List.to_seq fields) (List.to_seq es));
             name;
           })
      |> make_ok
    | EStructAccess { e; name = s; field } -> (
      Message.emit_debug "... it's an EStructAccess";
      propagate_generic_error (evaluate_expr ctx lang e) []
      @@ fun e ->
      propagate_empty_error e
      @@ fun e ->
      match Mark.remove e with
      | EStruct { fields = es; name } ->
        if not (StructName.equal s name) then
          Message.raise_multispanned_error
            [None, pos; None, Expr.pos e]
            "Error during struct access: not the same structs (should not \
             happen if the term was well-typed)";
        let field_expr =
          match StructField.Map.find_opt field es with
          | Some e' -> e'
          | None ->
            Message.raise_spanned_error (Expr.pos e)
              "Invalid field access %a in struct %a (should not happen if the \
               term was well-typed)"
              StructField.format field StructName.format s
        in
        let e_symb = get_symb_expr e in
        let fd_symb = get_symb_expr field_expr in
        let symb_expr =
          make_z3_struct_access ctx s field e_symb fd_symb
          (* TODO catch error... should not happen *)
        in
        Message.emit_debug "EStructAccess symbolic struct access created";
        (* the constraints generated by struct access are only those generated
           by the subcall, as the field expression is already a value *)
        let constraints = get_constraints e in
        add_conc_info_m m symb_expr ~constraints (Mark.remove field_expr)
        |> make_ok
      | _ ->
        Message.raise_spanned_error (Expr.pos e)
          "The expression %a should be a struct %a but is not (should not \
           happen if the term was well-typed)"
          (Print.UserFacing.expr lang)
          e StructName.format s)
    | ETuple _ -> failwith "ETuple not implemented"
    | ETupleAccess _ -> failwith "ETupleAccess not implemented"
    | EInj { name; e; cons } ->
      Message.emit_debug "... it's an EInj";
      propagate_generic_error (evaluate_expr ctx lang e) []
      @@ fun e ->
      propagate_empty_error e
      @@ fun e ->
      let concrete = EInj { name; e; cons } in

      let e_symb = get_symb_expr e in
      let symb_expr = SymbExpr.app_z3 (make_z3_enum_inj ctx name cons) e_symb in
      let constraints = get_constraints e in

      add_conc_info_m m symb_expr ~constraints concrete |> make_ok
    | EMatch { e; cases; name } -> (
      Message.emit_debug "... it's an EMatch";
      (* NOTE: The surface keyword [anything] is expanded during desugaring, so
         it makes me generate many cases. See the [enum_wildcard] test for an
         example. TODO issue #130 asks for this feature ; use it once it is
         added. *)
      propagate_generic_error (evaluate_expr ctx lang e) []
      @@ fun e ->
      propagate_empty_error e
      @@ fun e ->
      match Mark.remove e with
      | EInj { e = e1; cons; name = name' } ->
        if not (EnumName.equal name name') then
          Message.raise_multispanned_error
            [None, Expr.pos e; None, Expr.pos e1]
            "Error during match: two different enums found (should not happen \
             if the term was well-typed)";
        let es_n =
          match EnumConstructor.Map.find_opt cons cases with
          | Some es_n -> es_n
          | None ->
            Message.raise_spanned_error (Expr.pos e)
              "sum type index error (should not happen if the term was \
               well-typed)"
        in

        (* Here we make sure that the symbolic expression of [e1] (that will be
           sent "down" in a binder) is the accessor of [cons] applied to [e].
           Otherwise it would be the symbolic expression built from the bottom
           up during the evaluation of [e], which may not take into account the
           symbolic value of [e]. *)
        let e_symb = get_symb_expr e in
        let e1_symb =
          SymbExpr.app_z3 (make_z3_enum_access ctx name cons) e_symb
          (* TODO catch error *)
        in
        let e1_constraints = get_constraints e1 in
        (* Here we have to explicitely "force" the new symbolic value, keep the
           constraints from [e1], and keep the position and type from [e1] *)
        let new_mark = set_conc_info e1_symb e1_constraints (Mark.get e1) in
        let e1 = Mark.set new_mark e1 in

        (* To encode the fact that we are in the [cons] arm of the pattern
           matching, we add a constraint per arm. For the [cons] arm, it encodes
           the fact that [e] is a [cons], and thus is a "true" path branch. For
           every other arm [A], it encodes the fact that [e] is not an [A], and
           thus is a "false" path branch. *)
        let e_constraints = get_constraints e in
        let arm_conditions = make_z3_arm_conditions ctx name cons e_symb in
        let arm_path_constraints =
          List.map
            (fun (s, b) -> make_z3_path_constraint s (Expr.pos e) b)
            arm_conditions
        in

        (* then we can evaluate the branch that was taken *)
        let ty =
          EnumConstructor.Map.find cons
            (EnumName.Map.find name ctx.ctx_decl.ctx_enums)
        in
        let new_e = Mark.add m (EApp { f = es_n; args = [e1]; tys = [ty] }) in
        let result = evaluate_expr ctx lang new_e in
        propagate_generic_error result (arm_path_constraints @ e_constraints)
        @@ fun result ->
        let r_concrete = Mark.remove result in
        let r_symb = get_symb_expr result in
        let r_constraints = get_constraints result in

        (* the constraints generated by the match when in case [cons] are :
         * - those generated by the evaluation of [e]
         * - the new constraints corresponding to the fact that we are in the [cons] case
         * - those generated by the evaluation of [es_n e1]
         *)
        let constraints =
          r_constraints @ arm_path_constraints @ e_constraints
        in

        add_conc_info_m m r_symb ~constraints r_concrete |> make_ok
      | _ ->
        Message.raise_spanned_error (Expr.pos e)
          "Expected a term having a sum type as an argument to a match (should \
           not happen if the term was well-typed")
    | EIfThenElse { cond; etrue; efalse } -> (
      Message.emit_debug "... it's an EIfThenElse";
      propagate_generic_error (evaluate_expr ctx lang cond) []
      @@ fun cond ->
      propagate_empty_error cond
      @@ fun cond ->
      let c_symb = get_symb_expr cond in
      let c_constraints = get_constraints cond in
      match Mark.remove cond with
      | ELit (LBool true) ->
        Message.emit_debug "EIfThenElse>true adding %a to constraints"
          SymbExpr.formatter c_symb;
        let c_symb = SymbExpr.simplify c_symb in
        let c_path_constraint =
          make_z3_path_constraint c_symb (Expr.pos cond) true
        in
        let etrue = evaluate_expr ctx lang etrue in
        propagate_generic_error etrue (c_path_constraint :: c_constraints)
        @@ fun etrue ->
        let e_symb = get_symb_expr etrue in
        let e_constraints = get_constraints etrue in
        let e_mark = Mark.get etrue in
        let e_concr = Mark.remove etrue in
        (* the constraints generated by the ifthenelse when [cond] is true are :
         * - those generated by the evaluation of [cond]
         * - a new constraint corresponding to [cond] itself
         * - those generated by the evaluation of [etrue]
         *)
        let constraints =
          e_constraints @ (c_path_constraint :: c_constraints)
        in
        add_conc_info_m e_mark e_symb ~constraints e_concr |> make_ok
      | ELit (LBool false) ->
        Message.emit_debug "EIfThenElse>false adding %a to constraints"
          SymbExpr.formatter c_symb;
        let not_c_symb =
          SymbExpr.app_z3 (Z3.Boolean.mk_not ctx.ctx_z3) c_symb
        in
        let not_c_symb = SymbExpr.simplify not_c_symb in
        (* TODO catch error... should not happen *)
        let not_c_path_constraint =
          make_z3_path_constraint not_c_symb (Expr.pos cond) false
        in
        let efalse = evaluate_expr ctx lang efalse in
        propagate_generic_error efalse (not_c_path_constraint :: c_constraints)
        @@ fun efalse ->
        let e_symb = get_symb_expr efalse in
        let e_constraints = get_constraints efalse in
        let e_mark = Mark.get efalse in
        let e_concr = Mark.remove efalse in
        (* the constraints generated by the ifthenelse when [cond] is false are :
         * - those generated by the evaluation of [cond]
         * - a new constraint corresponding to [cond] itself
         * - those generated by the evaluation of [efalse]
         *)
        let constraints =
          e_constraints @ (not_c_path_constraint :: c_constraints)
        in
        add_conc_info_m e_mark e_symb ~constraints e_concr |> make_ok
      | _ ->
        Message.raise_spanned_error (Expr.pos cond)
          "Expected a boolean literal for the result of this condition (should \
           not happen if the term was well-typed)")
    | EArray _ -> failwith "EArray not implemented"
    | EAssert _ -> failwith "EAssert not implemented"
    | ECustom _ -> failwith "ECustom not implemented"
    | EEmptyError ->
      Message.emit_debug "... it's an EEmptyError";
      make_ok e
      (* TODO check that it's ok to pass along the symbolic values and
         constraints? *)
    | EErrorOnEmpty e' -> (
      Message.emit_debug "... it's an EErrorOnEmpty";
      propagate_generic_error (evaluate_expr ctx lang e') []
      @@ fun e' ->
      match e' with
      | EEmptyError, m ->
        make_error_emptyerror m (get_constraints e')
          "This variable evaluated to an empty term (no rule that defined it \
           applied in this situation)"
      | e -> make_ok e
      (* just pass along the concrete and symbolic values, and the
         constraints *))
    | EDefault
        {
          excepts =
            [
              ((EApp { f = (EAbs _, _) as abs; args = [(ELit LUnit, _)]; _ }, _)
               as except);
            ];
          just = ELit (LBool true), _;
          cons;
        } -> (
      (* FIXME add metadata to find this case instead of this big match *)
      Message.emit_debug "... it's a context variable definition %a"
        (Print.expr ()) except;

      let app = evaluate_expr ctx lang except in
      propagate_generic_error app []
      @@ fun app ->
      let pos = Expr.pos except in
      let abs_symb = get_symb_expr abs in
      let app_constraints =
        get_constraints app (* TODO check that this is always []? *)
      in
      match Mark.remove app with
      | EEmptyError ->
        Message.emit_debug "Context>empty";
        let is_empty : path_constraint list =
          make_reentrant_path_constraint ctx abs_symb pos true |> Option.to_list
        in
        let result = evaluate_expr ctx lang cons in
        propagate_generic_error result (is_empty @ app_constraints)
        @@ fun result ->
        let r_symb = get_symb_expr result in
        let r_constraints = get_constraints result in
        (* TODO check that constraints from app should stay as well, just in
           case *)
        let constraints = r_constraints @ is_empty @ app_constraints in
        add_conc_info_e r_symb ~constraints result |> make_ok
      | _ ->
        Message.emit_debug "Context>non-empty";
        let not_is_empty : path_constraint list =
          make_reentrant_path_constraint ctx abs_symb pos false
          |> Option.to_list
        in
        (* the only constraint is the new one encoding the fact that there is a
           reentrant value, and the symbolic expression is that of the
           reentering value *)
        (* TODO check that constraints from app should stay as well, just in
           case *)
        let constraints = not_is_empty @ app_constraints in
        add_conc_info_e SymbExpr.none ~constraints app |> make_ok)
    | EDefault { excepts; just; cons } -> (
      Message.emit_debug "... it's an EDefault";
      let excepts = List.map (evaluate_expr ctx lang) excepts in
      propagate_generic_error_list excepts []
      @@ fun excepts ->
      let exc_constraints = gather_constraints excepts in
      let empty_count =
        List.length (List.filter Concrete.is_empty_error excepts)
      in
      match List.length excepts - empty_count with
      | 0 -> (
        Message.emit_debug "EDefault>no except";
        let just = evaluate_expr ctx lang just in
        propagate_generic_error just exc_constraints
        @@ fun just ->
        let j_symb = get_symb_expr just in
        let j_constraints = get_constraints just in
        match Mark.remove just with
        | EEmptyError ->
          Message.emit_debug "EDefault>empty";
          (* TODO test this case *)
          (* the constraints generated by the default when [just] is empty are :
           * - those generated by the evaluation of the excepts
           * - those generated by the evaluation of [just]
           *)
          let constraints = j_constraints @ exc_constraints in
          add_conc_info_m m SymbExpr.none ~constraints EEmptyError
        | ELit (LBool true) ->
          Message.emit_debug "EDefault>true adding %a to constraints"
            SymbExpr.formatter j_symb;
          let j_symb = SymbExpr.simplify j_symb in
          (* TODO catch error... should not happen *)
          (* TODO factorize the simplifications? *)
          let j_path_constraint =
            make_z3_path_constraint j_symb (Expr.pos just) true
          in
          let cons = evaluate_expr ctx lang cons in
          propagate_generic_error cons
            ((j_path_constraint :: j_constraints) @ exc_constraints)
          @@ fun cons ->
          let c_symb = get_symb_expr cons in
          let c_constraints = get_constraints cons in
          let c_mark = Mark.get cons in
          let c_concr = Mark.remove cons in
          (* the constraints generated by the default when [just] is true are :
           * - those generated by the evaluation of the excepts
           * - those generated by the evaluation of [just]
           * - a new constraint corresponding to [just] itself
           * - those generated by the evaluation of [cons]
           *)
          let constraints =
            c_constraints
            @ (j_path_constraint :: j_constraints)
            @ exc_constraints
          in
          add_conc_info_m c_mark c_symb ~constraints c_concr |> make_ok
        | ELit (LBool false) ->
          let not_j_symb =
            SymbExpr.app_z3 (Z3.Boolean.mk_not ctx.ctx_z3) j_symb
          in
          let not_j_symb = SymbExpr.simplify not_j_symb in
          let not_j_path_constraint =
            make_z3_path_constraint not_j_symb (Expr.pos just) false
          in
          Message.emit_debug "EDefault>false adding %a to constraints"
            SymbExpr.formatter not_j_symb;
          (* the constraints generated by the default when [just] is false are :
           * - those generated by the evaluation of the excepts
           * - those generated by the evaluation of [just]
           * - a new constraint corresponding to [just] itself
           *)
          let constraints =
            (not_j_path_constraint :: j_constraints) @ exc_constraints
          in
          add_conc_info_m m SymbExpr.none ~constraints EEmptyError
        | _ ->
          Message.raise_spanned_error (Expr.pos e)
            "Default justification has not been reduced to a boolean at \
             evaluation (should not happen if the term was well-typed)")
      | 1 ->
        Message.emit_debug "EDefault>except";
        let r =
          List.find (fun sub -> not (Concrete.is_empty_error sub)) excepts
        in
        (* the constraints generated by the default when exactly one except is raised are :
         * - those generated by the evaluation of the excepts
         *)
        let r_symb = get_symb_expr r in
        let constraints = exc_constraints in
        add_conc_info_e r_symb ~constraints r |> make_ok
      | _ ->
        make_error_conflicterror m exc_constraints
          (List.map
             (fun except ->
               ( Some "This consequence has a valid justification:",
                 Expr.pos except ))
             (List.filter
                (fun sub -> not (Concrete.is_empty_error sub))
                excepts))
          "There is a conflict between multiple valid consequences for \
           assigning the same variable.")
    | EPureDefault e ->
      Message.emit_debug "... it's an EPureDefault";
      evaluate_expr ctx lang e
    | _ -> .
  in
  (* Message.emit_debug "\teval returns %a | %a" (Print.expr ()) ret
     SymbExpr.formatter (get_symb_expr_r ret); *)
  Message.emit_debug "\teval returns %a" SymbExpr.formatter
    (get_symb_expr_r ret);
  ret

(** The following functions gather methods to generate input values for concolic
    execution, be it from a model or from hardcoded default values. *)

(** Create the mark of an input field from its name [field] and its type [ty].
    Note that this function guarantees that the type information will be present
    when calling [inputs_of_model] *)
let make_input_mark ctx m field (ty : typ) : conc_info mark =
  let name = Mark.remove (StructField.get_info field) in
  let _, sort = translate_typ ctx (Mark.remove ty) in
  let symb_expr =
    match Mark.remove ty with
    | TArrow ([(TLit TUnit, _)], (TDefault inner_ty, _)) ->
      (* Context variables carry the name of the actual input variable (that is
         the name of the field in the input struct), as well as a symbol used to
         mark the inner expression of the thunk, that can then be used in Z3
         when the thunk is non-empty. See [make_reentrant_input]. *)
      Message.emit_debug "[make_input_mark] reentrant variable <%s> : %a" name
        Print.typ_debug ty;
      let _, inner_sort = translate_typ ctx (Mark.remove inner_ty) in
      let symbol = Z3.Expr.mk_const_s ctx.ctx_z3 name inner_sort in
      SymbExpr.mk_reentrant field symbol
    | TArrow _ ->
      (* proper functions are not allowed as input *)
      Message.raise_spanned_error (Mark.get ty)
        "This input of the scope is a function. This case is not handled by \
         the concolic interpreter for now. You may want to call this scope \
         from an other scope and provide a specific function as argument."
    | _ ->
      (* Other variales simply carry a symbol corresponding their name. *)
      let symbol = Z3.Expr.mk_const_s ctx.ctx_z3 name sort in
      SymbExpr.mk_z3 symbol
  in
  let pos = Expr.mark_pos m in
  Custom { pos; custom = { symb_expr; constraints = []; ty = Some ty } }

(** Evaluation *)

(** Do a "pre-evaluation" of the program. It compiles the target scope to a
    function that takes the input struct of the scope and returns its output
    struct *)
let simplify_program (type m) ctx (p : (dcalc, m) gexpr program) s : conc_expr =
  Message.emit_debug "[CONC] Make program expression concolic";
  let e = Expr.unbox (Program.to_expr p s) in
  let e_conc = init_conc_expr e in
  Message.emit_debug "[CONC] Pre-compute program";
  let result = evaluate_expr ctx p.lang e_conc in
  try del_genericerror result
  with Invalid_argument _ ->
    failwith
      "Program pre-evaluation returned an error!" (* TODO catch this properly *)

(** Evaluate pre-compiled scope [e] with its input struct [name] populated with
    [fields] *)
let eval_conc_with_input
    ctx
    lang
    (name : StructName.t)
    (e : conc_expr)
    (mark : conc_info mark)
    (fields : conc_boxed_expr StructField.Map.t) : conc_result =
  let to_interpret =
    Expr.eapp
      ~f:(Expr.box e)
        (* box instead of rebox because the term is supposed to be closed *)
      ~args:[Expr.estruct ~name ~fields mark]
      ~tys:[Option.get (get_type e)] (* these are supposed to be typed?? *)
      (set_conc_info SymbExpr.none [] (Mark.get e))
  in
  Message.emit_debug "...inputs applied...";
  evaluate_expr ctx lang (Expr.unbox to_interpret)

(** Constraint solving *)
module Solver = struct
  module Z3Solver = struct
    type solver_result = Sat of Z3.Model.model option | Unsat | Unknown

    let solve (z3ctx : Z3.context) (constraints : s_expr list) : solver_result =
      let solver = Z3.Solver.mk_solver z3ctx None in
      Z3.Solver.add solver constraints;
      match Z3.Solver.check solver [] with
      | SATISFIABLE -> Sat (Z3.Solver.get_model solver)
      | UNSATISFIABLE -> Unsat
      | UNKNOWN -> Unknown
  end

  type input = pc_expr list

  type model = {
    model_z3 : Z3.Model.model;
    model_empty_reentrants : StructField.Set.t;
  }

  (* TODO make formatter *)
  let string_of_model m =
    let z3_string = Z3.Model.to_string m.model_z3 in
    let empty_reentrants_list =
      List.of_seq @@ StructField.Set.to_seq m.model_empty_reentrants
    in
    let empty_reentrants_strings =
      List.map
        (fun f -> Mark.remove (StructField.get_info f))
        empty_reentrants_list
    in
    let empty_reentrants_string =
      List.fold_left (fun x acc -> x ^ ", " ^ acc) "" empty_reentrants_strings
    in
    "z3: " ^ z3_string ^ "\nreentrant: " ^ empty_reentrants_string

  type solver_result = Sat of model option | Unsat | Unknown

  let split_input (l : input) : s_expr list * StructField.Set.t =
    let rec aux l (acc_z3 : s_expr list) (acc_reentrant : StructField.Set.t) =
      match l with
      | [] -> acc_z3, acc_reentrant
      | Pc_z3 e :: l' -> aux l' (e :: acc_z3) acc_reentrant
      | Pc_reentrant e :: l' ->
        aux l' acc_z3
          (if e.is_empty then StructField.Set.add e.name acc_reentrant
           else acc_reentrant)
    in
    aux l [] StructField.Set.empty

  let solve (ctx : context) (constraints : input) =
    let z3_constraints, model_empty_reentrants = split_input constraints in
    match Z3Solver.solve ctx.ctx_z3 z3_constraints with
    | Z3Solver.Sat (Some model_z3) ->
      Sat (Some { model_z3; model_empty_reentrants })
    | Z3Solver.Sat None -> Sat None
    | Z3Solver.Unsat -> Unsat
    | Z3Solver.Unknown -> Unknown

  (** Create a dummy concolic mark with a position and a type. It has no
      symbolic expression or constraints, and is used for subexpressions inside
      of input structs whose mark is a single symbol. *)
  let dummy_mark pos ty : conc_info mark =
    Custom
      {
        pos;
        custom = { symb_expr = SymbExpr.none; constraints = []; ty = Some ty };
      }

  (** Get a default literal value from a literal type *)
  let default_lit_of_tlit t : lit =
    match t with
    | TBool -> LBool true
    | TInt -> LInt (Z.of_int 42)
    | TMoney -> LMoney (Runtime.money_of_units_int 42)
    | TUnit -> LUnit
    | TRat -> LRat (Runtime.decimal_of_string "42")
    | TDate -> LDate DateEncoding.default_date
    | TDuration -> LDuration DateEncoding.default_duration

  (** Get a default concolic expression from a type. The concolic [mark] is
      given by the caller, and this function gives a default concrete value.
      This function is expected to be called from [inputs_of_model] when Z3 has
      not given a value for an input field. *)
  let rec default_expr_of_typ ctx mark ty : conc_boxed_expr =
    match Mark.remove ty with
    | TLit t -> Expr.elit (default_lit_of_tlit t) mark
    | TArrow ([(TLit TUnit, _)], (TDefault _, _)) ->
      (* context variable *)
      (* TODO CONC REU *)
      Message.raise_spanned_error (Expr.mark_pos mark)
        "[default_expr_of_typ] should not be called on a context variable. \
         This should not happen if context variables were handled properly."
    | TArrow _ ->
      (* functions *)
      (* TODO CONC REU *)
      Message.raise_spanned_error (Expr.mark_pos mark)
        "[default_expr_of_typ] should not be called on a function. This should \
         not happen if functions were handled properly."
    | TTuple _ -> failwith "TTuple not implemented"
    | TStruct name ->
      (* When a field of the input structure is a struct itself, its fields will
         only be evaluated for their concrete values, as their symbolic value
         will be accessed symbolically from the symbol of the input structure
         field. Thus we can safely give a dummy mark, one with position and type
         for printing, but no symbolic expression or constraint *)
      let pos = Expr.mark_pos mark in
      let fields_typ = StructName.Map.find name ctx.ctx_decl.ctx_structs in
      let fields_expr =
        StructField.Map.map
          (fun ty -> default_expr_of_typ ctx (dummy_mark pos ty) ty)
          fields_typ
      in
      Expr.estruct ~name ~fields:fields_expr mark
    | TEnum name ->
      (* Catala enums always have at least one case, so we can take the first
         one we find and use it as the default case *)
      let pos = Expr.mark_pos mark in
      let constructors = EnumName.Map.find name ctx.ctx_decl.ctx_enums in
      let cstr_name, cstr_ty =
        List.hd (EnumConstructor.Map.bindings constructors)
      in
      let cstr_e = default_expr_of_typ ctx (dummy_mark pos cstr_ty) cstr_ty in
      Expr.einj ~name ~cons:cstr_name ~e:cstr_e mark
    | TOption _ -> failwith "[default_expr_of_typ] TOption not implemented"
    | TArray _ -> failwith "[default_expr_of_typ] TArray not implemented"
    | TDefault _ -> failwith "[default_expr_of_typ] TDefault not implemented"
    | TAny -> failwith "[default_expr_of_typ] TAny not implemented"
    | TClosureEnv ->
      failwith "[default_expr_of_typ] TClosureEnv not implemented"

  (** Get the Z3 expression corresponding to the value of Z3 symbol constant [v]
      in Z3 model [m]. [None] if [m] has not given a value. TODO CONC check that
      the following hypothesis is correct : "get_const_interp_e only returns
      None if the symbol constant can take any value" *)
  let interp_in_model (m : Z3.Model.model) (v : s_expr) : s_expr option =
    Z3.Model.get_const_interp_e m v

  let value_of_symb_expr_lit tl e =
    match tl with
    | TInt -> LInt (integer_of_symb_expr e)
    | TBool -> LBool (bool_of_symb_expr e)
    | TMoney ->
      let cents = integer_of_symb_expr e in
      let money = Runtime.money_of_cents_integer cents in
      LMoney money
    | TRat -> LRat (decimal_of_symb_expr e)
    | TUnit -> LUnit (* TODO maybe check that the Z3 value is indeed unit? *)
    | TDate -> LDate (DateEncoding.decode_date e)
    | TDuration -> LDuration (DateEncoding.decode_duration e)

  (** Make a Catala value from a Z3 expression. The concolic [mark] is given by
      the caller, and this function gives a concrete value corresponding to
      symbolic value [e]. This function is expected to be called from
      [inputs_of_model] when Z3 has given a value for an input field. *)
  let rec value_of_symb_expr ctx model mark ty (e : s_expr) =
    match Mark.remove ty with
    | TLit tl ->
      let lit = value_of_symb_expr_lit tl e in
      Expr.elit lit mark
    | TAny -> failwith "[value_of_symb_expr] TAny not implemented"
    | TClosureEnv -> failwith "[value_of_symb_expr] TClosureEnv not implemented"
    | TTuple _ -> failwith "[value_of_symb_expr] TTuple not implemented"
    | TStruct name ->
      (* To get the values of fields inside a Z3 struct and reconstruct a Catala
         struct out of those, evaluate a Z3 "accessor" to the corresponding
         field in the model *)
      let pos = Expr.mark_pos mark in
      let fields_typ = StructName.Map.find name ctx.ctx_decl.ctx_structs in
      let expr_of_fd fd ty =
        let access =
          make_z3_struct_access ctx name fd (SymbExpr.mk_z3 e) SymbExpr.none
        in
        match access with
        | Symb_z3 access ->
          (* TODO check that there is no reentrant here *)
          let ev = Option.get (Z3.Model.eval model access true) in
          (* TODO catch error *)
          value_of_symb_expr ctx model (dummy_mark pos ty) ty ev
        (* See [default_expr_of_typ] for an explanation on the dummy mark *)
        | _ ->
          failwith
            "[value_of_symb_expr] access expression is not Z3, this should not \
             happen"
        (* TODO make better error handling here *)
      in
      let fields_expr = StructField.Map.mapi expr_of_fd fields_typ in
      Expr.estruct ~name ~fields:fields_expr mark
    | TEnum name ->
      let pos = Expr.mark_pos mark in
      (* create a mapping of Z3 constructors to the Catala constructors of this
         enum and their types *)
      let sort = EnumName.Map.find name ctx.ctx_z3enums in
      let z3_constructors = Z3.Datatype.get_constructors sort in
      let constructors = EnumName.Map.find name ctx.ctx_decl.ctx_enums in
      let mapping =
        List.combine z3_constructors (EnumConstructor.Map.bindings constructors)
      in
      (* get the Z3 constructor used in [e] *)
      let e_constructor = Z3.Expr.get_func_decl e in
      (* recover the constructor corresponding to the Z3 constructor *)
      let _, (cstr_name, cstr_ty) =
        try
          List.find
            (fun (cons1, _) -> Z3.FuncDecl.equal e_constructor cons1)
            mapping
        with Not_found ->
          failwith
            "value_of_symb_expr could not find what case of an enum was used. \
             This should not happen."
        (* TODO make better error *)
      in
      let e_arg = List.hd (Z3.Expr.get_args e) in
      let arg =
        value_of_symb_expr ctx model (dummy_mark pos cstr_ty) cstr_ty e_arg
      in
      Expr.einj ~name ~cons:cstr_name ~e:arg mark
    | TArrow _ ->
      Message.raise_spanned_error (Expr.mark_pos mark)
        "[value_of_symb_expr] should not be called on a context variable or a \
         function. This should not happen if they were handled properly"
    | TOption _ -> failwith "[value_of_symb_expr] TOption not implemented"
    | TArray _ -> failwith "[value_of_symb_expr] TArray not implemented"
    | TDefault _ -> failwith "[value_of_symb_expr] TDefault not implemented"

  let make_term ctx z3_model mk ty symb_expr =
    let symb_expr_opt = interp_in_model z3_model symb_expr in
    Option.fold
      ~none:(default_expr_of_typ ctx mk ty)
      ~some:(value_of_symb_expr ctx z3_model mk ty)
      symb_expr_opt

  let make_reentrant_input ctx name z3_model empty_reentrants mk ty symb_expr :
      conc_boxed_expr =
    (* See [make_input_mark] for a general description of the Symb_reentrant
       symbolic expression. *)
    if StructField.Set.mem name empty_reentrants then (
      (* If the context variable must evaluate to its default value (as defined
         in the scope), then we make an empty thunked term. During evaluation,
         the [name] of the variable will be used to generate a constraint
         encoding whether it is empty, but the symbolic expression on the
         (empty) innner term will not be used. *)
      Message.emit_debug "[make_reentrant_input] empty";
      Expr.empty_thunked_term mk)
    else (
      (* If the context variable must evaluate to a specific value computed by
         the Z3 model, then we make this inner term and thunk it. The mark on
         the inner term (inside the thunk) is the symbol in the Symb_reentrant
         structure, and will be be used during evaluation. The mark on the outer
         term (the thunk itself) will be used only for its [name] field and will
         be used to generate a constraint encoding whether it is empty. *)
      Message.emit_debug "[make_reentrant_input] non empty";
      match Mark.remove ty with
      | TArrow ([(TLit TUnit, _)], (TDefault inner_ty, _)) ->
        let inner_mk =
          map_conc_mark ~symb_expr_f:(fun _ -> Symb_z3 symb_expr) mk
        in
        let term = make_term ctx z3_model inner_mk inner_ty symb_expr in
        let (Custom { custom; _ }) = Mark.get term in
        Message.emit_debug "[make_reentrant_input] non empty inner: %a"
          SymbExpr.formatter_typed custom.symb_expr;
        let term = Expr.thunk_term term mk in
        let term = Mark.add mk (Mark.remove term) in
        let (Custom { custom; _ }) = Mark.get term in
        Message.emit_debug "[make_reentrant_input] non empty thunked: %a"
          SymbExpr.formatter_typed custom.symb_expr;
        term
      | _ -> failwith "[make_reentrant_input] did not get an arrow type")

  (** Get Catala values from a Z3 model, possibly using default values *)
  let inputs_of_model
      ctx
      (m : model)
      (input_marks : conc_info mark StructField.Map.t) :
      conc_boxed_expr StructField.Map.t =
    let f _ (mk : conc_info mark) : conc_boxed_expr =
      let (Custom { custom; _ }) = mk in
      let ty =
        Option.get custom.ty
        (* should not fail because [make_input_mark] always adds a ty *)
      in
      let symb_expr = custom.symb_expr in
      let t =
        match symb_expr with
        | Symb_reentrant { name; symbol } ->
          (* Context variable *)
          make_reentrant_input ctx name m.model_z3 m.model_empty_reentrants mk
            ty symbol
        | Symb_z3 s ->
          (* Input variable *)
          make_term ctx m.model_z3 mk ty s
        | Symb_none ->
          failwith "[inputs_of_model] input mark should not be none"
        | Symb_error _ ->
          failwith "[inputs_of_model] input mark should not be an error"
      in
      let (Custom { custom; _ }) = Mark.get t in
      Message.emit_debug "[inputs_of_model] input has symb? %a"
        SymbExpr.formatter custom.symb_expr;
      t
    in
    StructField.Map.mapi f input_marks
end

(** Computation path logic *)

(* Two path constraint expressions are equal if they are of the same kind, and
   if either their Z3 expressions are equal or their variable name and
   "emptyness" are equal depending on that kind. *)
let pc_expr_equal e e' : bool =
  match e, e' with
  | Pc_z3 e1, Pc_z3 e2 -> Z3.Expr.equal e1 e2
  | Pc_reentrant e1, Pc_reentrant e2 ->
    StructField.equal e1.name e2.name && e1.is_empty = e2.is_empty
  | _, _ -> false

(** Two path constraints are equal if their expressions are equal, and they are
    marked with the same branch information. Position is not taken into account
    as it is used only in printing and not in computations *)
let path_constraint_equal c c' : bool =
  pc_expr_equal c.expr c'.expr && c.branch = c'.branch

(** Compare the path of the previous evaluation and the path of the current
    evaluation. If a constraint was previously marked as Done or Normal, then
    check that it stayed the same. If it was previously marked as Negated, thus
    if it was negated before the two evaluations, then check that the concrete
    value was indeed negated and mark it Done. If there are new constraints
    after the last one, add them as Normal. Crash in other cases. *)
let rec compare_paths
    (path_prev : annotated_path_constraint list)
    (path_new : path_constraint list) : annotated_path_constraint list =
  match path_prev, path_new with
  | [], [] -> []
  | [], c' :: p' ->
    Normal c' :: compare_paths [] p' (* the new path can be longer *)
  | _ :: _, [] -> failwith "[compare_paths] old path is longer than new path"
  | Normal c :: p, c' :: p' ->
    if path_constraint_equal c c' then Normal c :: compare_paths p p'
    else
      failwith "[compare_paths] a constraint that should not change has changed"
  | Negated c :: p, c' :: p' ->
    if c.branch <> c'.branch then
      (* the branch has been successfully negated and is now done *)
      (* TODO we should have a way to know if c and c' are the same except for
         their [branch] *)
      Done c' :: compare_paths p p'
    else failwith "[compare_paths] the negated condition lead to the same path"
  | Done c :: p, c' :: p' ->
    if c = c' then Done c :: compare_paths p p'
    else
      failwith
        "[compare_paths] a done constraint that should not change has changed"

(** Remove Done paths until a Normal (not yet negated) constraint is found, then
    mark this branch as Negated. This function shall be called on an output of
    [compare_paths], and thus no Negated constraint should appear in its input. *)
let rec make_expected_path (path : annotated_path_constraint list) :
    annotated_path_constraint list =
  match path with
  | [] -> []
  | Normal c :: p -> Negated c :: p
  | Done _ :: p -> make_expected_path p
  | Negated _ :: _ ->
    failwith
      "[make_expected_path] found a negated constraint, which should not happen"

(** Remove marks from an annotated path, to get a list of path constraints to
    feed in the solver. In doing so, actually negate constraints marked as
    Negated. This function shall be called on an output of [make_expected_path]. *)
let constraint_list_of_path ctx (path : annotated_path_constraint list) :
    Solver.input =
  let f = function
    | Normal c -> c.expr
    | Done c -> c.expr
    | Negated c -> begin
      match c.expr with
      | Pc_z3 e -> Pc_z3 (Z3.Boolean.mk_not ctx.ctx_z3 e)
      | Pc_reentrant e -> Pc_reentrant { e with is_empty = not e.is_empty }
    end
  in
  List.map f path

(* TODO use formatter for those *)
let string_of_pc_expr (e : pc_expr) : string =
  match e with
  | Pc_z3 e -> Z3.Expr.to_string e
  | Pc_reentrant { name; is_empty } ->
    (if is_empty then "Empty(" else "NotEmpty(")
    ^ Mark.remove (StructField.get_info name)
    ^ ")"

let string_of_path_constraint (pc : path_constraint) : string =
  string_of_pc_expr pc.expr
  ^
  if Cli.globals.debug then
    "@" ^ Pos.to_string_short pc.pos ^ " {" ^ string_of_bool pc.branch ^ "}"
  else ""

let print_path_constraints (pcs : path_constraint list) : unit =
  let pp_sep fmt () = Format.fprintf fmt "\n" in
  Message.emit_debug "Path constraints after evaluation:\n%a"
    (Format.pp_print_list ~pp_sep Format.pp_print_string)
    (List.map string_of_path_constraint pcs)

let print_annotated_path_constraints constraints : unit =
  if constraints = [] then Message.emit_debug "No constraints"
  else begin
    let pp_sep fmt () = Format.fprintf fmt "\n" in
    let aux apc =
      match apc with
      | Normal pc -> "       " ^ string_of_path_constraint pc
      | Done pc -> "DONE   " ^ string_of_path_constraint pc
      | Negated pc -> "NEGATE " ^ string_of_path_constraint pc
    in
    Message.emit_debug "Trying new path constraints:\n%a"
      (Format.pp_print_list ~pp_sep Format.pp_print_string)
      (List.map aux constraints)
  end

let print_fields language (prefix : string) fields =
  let ordered_fields =
    List.sort (fun ((v1, _), _) ((v2, _), _) -> String.compare v1 v2) fields
  in
  List.iter
    (fun ((var, _), value) ->
      Message.emit_result "%s@[<hov 2>%s@ =@ %a@]%s" prefix var
        (if Cli.globals.debug then Print.expr ()
         else Print.UserFacing.value language)
        value
        (if Cli.globals.debug then
           " | " ^ SymbExpr.to_string (_get_symb_expr_unsafe value)
         else ""))
    ordered_fields

(** Main function *)
let interpret_program_concolic (type m) (p : (dcalc, m) gexpr program) s :
    (Uid.MarkedString.info * conc_expr) list =
  Message.emit_debug "=== Start concolic interpretation... ===";
  let decl_ctx = p.decl_ctx in
  Message.emit_debug "[CONC] Create empty context";
  let ctx = make_empty_context decl_ctx in
  Message.emit_debug "[CONC] Initialize context";
  let ctx = init_context ctx in

  let scope_e = simplify_program ctx p s in
  match scope_e with
  | EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e -> begin
    (* [taus] contain the types of the scope arguments. For [context] arguments,
       we can provide an empty thunked term. For [input] arguments of another
       type, we provide an empty value. *)

    (* first set of inputs *)
    let taus = StructName.Map.find s_in ctx.ctx_decl.ctx_structs in

    (* TODO CONC should it be [mark_e] or something else? *)
    let input_marks = StructField.Map.mapi (make_input_mark ctx mark_e) taus in

    let rec concolic_loop (previous_path : annotated_path_constraint list) :
        unit =
      Message.emit_debug "";
      print_annotated_path_constraints previous_path;
      let solver_constraints = constraint_list_of_path ctx previous_path in

      match Solver.solve ctx solver_constraints with
      | Solver.Sat (Some m) ->
        Message.emit_debug "Solver returned a model";
        Message.emit_debug "model:\n%s" (Solver.string_of_model m);
        let inputs = Solver.inputs_of_model ctx m input_marks in

        if not Cli.globals.debug then Message.emit_result "";
        Message.emit_result "Evaluating with inputs:";
        let inputs_list =
          List.map
            (fun (fld, e) -> StructField.get_info fld, Expr.unbox e)
            (StructField.Map.bindings inputs)
        in
        print_fields p.lang ". " inputs_list;

        let res = eval_conc_with_input ctx p.lang s_in scope_e mark_e inputs in

        let res_path_constraints = get_constraints_r res in

        print_path_constraints res_path_constraints;

        Message.emit_result "Output of scope after evaluation:";
        begin
          match Mark.remove res with
          | EStruct { fields; _ } ->
            let outputs_list =
              List.map
                (fun (fld, e) -> StructField.get_info fld, e)
                (StructField.Map.bindings fields)
            in
            print_fields p.lang ". " outputs_list
          | EGenericError ->
            (* TODO better error messages *)
            (* TODO test the different cases *)
            Message.emit_result "Found error %a" SymbExpr.formatter
              (get_symb_expr_r res)
          | _ ->
            Message.raise_spanned_error (Expr.pos scope_e)
              "The concolic interpretation of a program should always yield a \
               struct corresponding to the scope variables"
        end;

        (* TODO find a better way *)
        let new_path_constraints =
          compare_paths (List.rev previous_path) (List.rev res_path_constraints)
          |> List.rev
          |> make_expected_path
        in
        if new_path_constraints = [] then ()
        else concolic_loop new_path_constraints
      | Solver.Unsat -> begin
        Message.emit_debug "Solver returned Unsat";
        match previous_path with
        | [] -> failwith "[CONC] Failed to solve without constraints"
        | _ :: new_path_constraints ->
          let new_expected_path = make_expected_path new_path_constraints in
          if new_expected_path = [] then () else concolic_loop new_expected_path
      end
      | Solver.Sat None ->
        failwith "[CONC] Constraints satisfiable but no model was produced"
      | Solver.Unknown -> failwith "[CONC] Unknown solver result"
    in

    let _ = concolic_loop [] in
    Message.emit_result "";
    Message.emit_result "Concolic interpreter done";

    (* XXX BROKEN output *)
    []
  end
  | _ ->
    Message.raise_spanned_error (Expr.pos scope_e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"
