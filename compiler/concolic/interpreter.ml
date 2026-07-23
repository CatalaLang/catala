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
open Z3_utils
open Symb_expr
open Path_constraint
module Optimizations = Concolic_optimizations
module Runtime = Catala_runtime
open Conc_types

let set_conc_info
    (type m)
    (symb_expr : SymbExpr.t)
    (constraints : PathConstraint.naked_path)
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
    ?(constraints : PathConstraint.naked_path option)
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
    ?(constraints : PathConstraint.naked_path option)
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
  let rec f : type c e.
      ((c, e) conc_interpr_kind, 't) gexpr ->
      ((c, yes) conc_interpr_kind, 't) gexpr boxed = function
    | (ECustom _, _) as e -> Expr.map ~f e
    | (EGenericError, _) as e -> Expr.map ~f e
    | EAppOp { op; tys; args }, m ->
      Expr.eappop ~tys ~args:(List.map f args) ~op:(Operator.translate op) m
    | (EDefault _, _) as e -> Expr.map ~f e
    | (EPureDefault _, _) as e -> Expr.map ~f e
    | (EEmpty, _) as e -> Expr.map ~f e
    | (EErrorOnEmpty _, _) as e -> Expr.map ~f e
    | (EFatalError _, _) as e -> Expr.map ~f e
    | ( ( EAssert _ | ELit _ | EApp _ | EArray _ | EVar _ | EExternal _ | EAbs _
        | EIfThenElse _ | ETuple _ | ETupleAccess _ | EInj _ | EStruct _
        | EStructAccess _ | EMatch _ | EBad | EPos _ ),
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

let make_error_assertionerror mk constraints message : conc_result =
  let symb_expr = SymbExpr.mk_assertionerror message in
  make_error mk symb_expr constraints

(* Inspired by [Concrete.delcustom] *)
let del_genericerror e =
  if false then
    let rec f : (conc_dest_kind, 'm) gexpr -> (conc_src_kind, 'm) gexpr boxed =
      function
      | EGenericError, _ ->
        invalid_arg "Generic error remaining after propagation"
      | EAppOp { op; args; tys }, m ->
        Expr.eappop ~tys ~args:(List.map f args) ~op:(Operator.translate op) m
      | ( ( EAssert _ | ELit _ | EApp _ | EArray _ | EVar _ | EExternal _
          | EAbs _ | EIfThenElse _ | ETuple _ | ETupleAccess _ | EInj _
          | EStruct _ | EStructAccess _ | EMatch _ | EDefault _ | EPureDefault _
          | EEmpty | EFatalError _ | EErrorOnEmpty _ | ECustom _ | EBad | EPos _
            ),
          _ ) as e ->
        Expr.map ~f e
      | _ -> .
    in
    (* /!\ don't be tempted to use the same trick here, the function does one
       thing: validate at runtime that the term does not contain [ECustom]
       nodes. *)
    Expr.unbox (f e)
  else
    (* TODO QU RAPHAEL: turns out del_genericerror is pretty costly (~2x), so I
       decided to remove it because I think it is a safety/debugging check now
       =>> OUI c'est un runtime check qui sert à vérifier le type d'expression
       (erreur ou pas), a priori tout doit passer sans ! Peut-être à garder
       comme option pour le debugging/les unit tests *)
    let open struct
      external id : (conc_dest_kind, 'm) gexpr -> (conc_src_kind, 'm) gexpr
        = "%identity"
    end in
    id e

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
  ctx_reentrant_sort : Z3.Sort.sort;
  (* A dummy sort for reentrant variables *)
  ctx_reentrant_const : s_expr;
  (* A dummy expression for reentrant variables *)
  ctx_z3enums : Z3.Sort.sort EnumName.Map.t;
  (* A map from Catala enumeration names to the corresponding Z3 datatype sort,
     from which we can retrieve constructors and accessors *)
  ctx_z3structs : Z3.Sort.sort StructName.Map.t;
  (* A map from Catala struct names to the corresponding Z3 sort, from which we
     can retrieve the constructor and the accessors *)
  ctx_z3unit : Z3.Sort.sort * s_expr;
      (* A pair containing the Z3 encodings of the unit type, encoded as a tuple
         of 0 elements, and the unit value *)
  ctx_z3round : Z3.FuncDecl.func_decl;
  ctx_optims : Optimizations.flag list; (* A list of optimizations to apply *)
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
      Message.error
        "[DateEncoding] Duration literals containing years or months not \
         supported";
    z3_int_of_bigint ctx.ctx_z3 (Z.of_int d)

  let decode_duration (e : s_expr) : Runtime.duration =
    let days_bigint = integer_of_symb_expr e in
    let days_int =
      Z.to_int
        days_bigint (* TODO catch overflow? durations are ints anyways... *)
    in
    Runtime.duration_of_numbers 0 0 days_int

  let default_duration : Runtime.duration = Runtime.duration_of_numbers 0 0 10
  let base_day : Runtime.date = Runtime.date_of_numbers 2010 1 1

  (** [date_to_bigint] translates [d] to an integer corresponding to the number
      of days since the base date. Adapted from z3backend *)
  let date_to_bigint (d : Runtime.date) : Z.t =
    let period = Runtime.Oper.o_sub_dat_dat d base_day in
    let y, m, d = Runtime.duration_to_years_months_days period in
    assert (y = 0 && m = 0);
    Z.of_int d

  let encode_date (ctx : context) (date : Runtime.date) : s_expr =
    let days = date_to_bigint date in
    z3_int_of_bigint ctx.ctx_z3 days

  let decode_date
      ?(round : Runtime.date_rounding = Dates_calc.AbortOnRound)
      (* NOTE: no rounding for now *)
        (e : s_expr) : Runtime.date =
    let days = decode_duration e in
    Runtime.o_add_dat_dur round (Expr.pos_to_runtime Pos.void) base_day days

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

  let div_dur_dur ctx dur1 dur2 =
    (* TODO factorize with [Div_int_int]? *)
    (* convert e1 to a [Real] explicitely to avoid using integer division *)
    let dur1_rat = z3_force_real ctx dur1 in
    Z3.Arithmetic.mk_div ctx dur1_rat dur2
end

(** [translate_typ_lit] returns the Z3 sort corresponding to the Catala literal
    type [t] **)
let translate_typ_lit (ctx : context) (t : typ_lit) : Z3.Sort.sort =
  match t with
  | TBool -> Z3.Boolean.mk_sort ctx.ctx_z3
  | TPos | TUnit -> fst ctx.ctx_z3unit
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
    failwith "[translate_typ] no more thunk"
    (* FIXME CONTEXT *)
    (* context variable *)
    (* ctx, ctx.ctx_dummy_sort *)
  | TArrow _ -> ctx, ctx.ctx_dummy_sort (* other functions *)
  | TArray _ ->
    ( ctx,
      ctx.ctx_dummy_sort
      (* TODO maybe put a better sort here? this should not be read anyway... *)
    )
  | TForAll b ->
    let vars, _ = Bindlib.unmbind b in
    begin match vars with
    | [| v |] ->
      ( ctx,
        Z3.Sort.mk_uninterpreted_s ctx.ctx_z3
          (Bindlib.name_of v ^ (Bindlib.uid_of v |> string_of_int)) )
    | _ -> failwith "[translate_typ] TForAll(tuple) not implemented"
    end
  | TClosureEnv -> failwith "[translate_typ] TClosureEnv not implemented"
  | TDefault _ ->
    (* context variable *)
    ctx, ctx.ctx_reentrant_sort
  | TVar v ->
    (* (\* A type variable being an unresolved type, it can't be deconstructed, so *)
    (*    we can let it pass through. *\) *)
    (* failwith "[translate_typ] TVar not implemented" *)
    (* ?? *)
    ( ctx,
      Z3.Sort.mk_uninterpreted_s ctx.ctx_z3
        (Bindlib.name_of v ^ (Bindlib.uid_of v |> string_of_int)) )
  | TAbstract _ -> failwith "[translate_typ] TAbstract not implemented"
  | TError -> assert false

(* taken from z3backend's find_or_create_struct *)
and find_or_create_struct (ctx : context) (s : StructName.t) :
    context * Z3.Sort.sort =
  if Global.options.debug then
    Message.debug "[Struct] Find or create struct %s"
      (Mark.remove (StructName.get_info s));
  match StructName.Map.find_opt s ctx.ctx_z3structs with
  | Some s ->
    if Global.options.debug then Message.debug "[Struct] . found!";
    ctx, s
  | None ->
    let s_name = Mark.remove (StructName.get_info s) in
    let fields = StructName.Map.find s ctx.ctx_decl.ctx_structs in

    let mk_struct_s =
      "mk!" ^ s_name
      (* struct constructor *)
    in
    let is_struct_s =
      "is!" ^ s_name
      (* recognizer *)
    in
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
          if Global.options.debug then
            Message.debug "[Struct] . %s : %a"
              (Mark.remove (StructField.get_info f))
              Print.typ ty;
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
  if Global.options.debug then
    Message.debug "[Enum] Find or create enum %s"
      (Mark.remove (EnumName.get_info enum));

  let create_constructor (name : EnumConstructor.t) (ty : typ) (ctx : context) :
      context * Z3.Datatype.Constructor.constructor =
    let cstr_name = Mark.remove (EnumConstructor.get_info name) in
    let mk_cstr_s =
      "mk!" ^ cstr_name
      (* case constructor *)
    in
    let is_cstr_s =
      "is!" ^ cstr_name
      (* recognizer *)
    in
    let fieldname_s =
      cstr_name ^ "!0"
      (* name of the argument *)
    in
    let ctx, z3_arg_ty = translate_typ ctx (Mark.remove ty) in
    let z3_sortrefs =
      [0]
      (* will not be used *)
    in
    if Global.options.debug then
      Message.debug "[Enum] . %s : %a" cstr_name Print.typ ty;
    ( ctx,
      Z3.Datatype.mk_constructor_s ctx.ctx_z3 mk_cstr_s
        (Z3.Symbol.mk_string ctx.ctx_z3 is_cstr_s)
        [Z3.Symbol.mk_string ctx.ctx_z3 fieldname_s]
        [Some z3_arg_ty] z3_sortrefs )
  in

  match EnumName.Map.find_opt enum ctx.ctx_z3enums with
  | Some e ->
    if Global.options.debug then Message.debug "[Enum] . found!";
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

let create_z3round (ctx : Z3.context) : Z3.FuncDecl.func_decl =
  let real_sort = Z3.Arithmetic.Real.mk_sort ctx in
  let int_sort = Z3.Arithmetic.Integer.mk_sort ctx in
  let func_decl =
    Z3.FuncDecl.mk_rec_func_decl_s ctx "!round!" [real_sort] int_sort
  in
  let var = Z3.Arithmetic.Real.mk_const_s ctx "!q!" in
  Z3.FuncDecl.add_rec_def ctx func_decl [var] (z3_round_expr ctx var);
  func_decl

(* TODO move to utils? *)
let z3_round ctx = Z3_utils.z3_round_func ctx.ctx_z3round

(* taken from z3backend, but without the option check *)
let make_empty_context (decl_ctx : decl_ctx) (optims : Optimizations.flag list)
    : context =
  let z3_cfg = ["model", "true"; "proof", "false"] in
  let z3_ctx = Z3.mk_context z3_cfg in
  let z3_dummy_sort = Z3.Sort.mk_uninterpreted_s z3_ctx "!dummy_sort!" in
  let z3_dummy_const =
    Z3.Expr.mk_const_s z3_ctx "!dummy_const!" z3_dummy_sort
  in
  let z3_reentrant_sort =
    Z3.Sort.mk_uninterpreted_s z3_ctx "!reentrant_sort!"
  in
  let z3_reentrant_const =
    Z3.Expr.mk_const_s z3_ctx "!reentrant_const!" z3_reentrant_sort
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
    ctx_z3round = create_z3round z3_ctx;
    (* ctx_z3constraints = []; *)
    ctx_dummy_sort = z3_dummy_sort;
    ctx_dummy_const = z3_dummy_const;
    ctx_reentrant_sort = z3_reentrant_sort;
    ctx_reentrant_const = z3_reentrant_const;
    ctx_optims = optims;
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
  | LInt n -> z3_int_of_bigint ctx.ctx_z3 n
  | LRat r ->
    Z3.Arithmetic.Real.mk_numeral_nd ctx.ctx_z3 (Z.to_int r.num)
      (Z.to_int r.den)
    (* assumption: numerator and denominator are integers *)
  | LMoney m ->
    let cents = Runtime.money_to_cents m in
    z3_int_of_bigint ctx.ctx_z3 cents
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
    : PathConstraint.naked_path =
  let (Custom { custom; _ }) = Mark.get e in
  custom.constraints

(* Get constraints from concolic expression that cannot be an error. The
   signature of this function helps making sure that errors are always properly
   propagated during execution. By forcing [get_constraints_r] to be called
   explicitely if the expression is a result. *)
let get_constraints (e : conc_expr) : PathConstraint.naked_path =
  _get_constraints_unsafe e

let get_constraints_r (e : conc_result) : PathConstraint.naked_path =
  _get_constraints_unsafe e

(** Concatenate the constraints from a list of evaluated expressions [es]. [es]
    is expected to be in the order of evaluation of the expressions. *)
let gather_constraints (es : conc_expr list) =
  (* NOTE The expression evaluated last has its constraint on top, hence the
     list reversal. The constraints inside each expression are expected to be in
     the right order, with the "most recent" constraint first. Thus, the most
     recent constraint of the last evaluated (most recent) expression is on the
     top of the output list. *)
  let es_rev = List.rev es in
  let constraints = List.map get_constraints es_rev in
  List.flatten constraints

let get_type (e : conc_expr) : typ option =
  let (Custom { custom; _ }) = Mark.get e in
  custom.ty

let make_z3_struct ctx (name : StructName.t) (es : conc_expr list) : s_expr =
  let sort = StructName.Map.find name ctx.ctx_z3structs in
  let constructor = List.hd (Z3.Datatype.get_constructors sort) in
  let z3_of_expr (e : conc_expr) (d : Z3.Sort.sort) : s_expr =
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
    (* FIXME CONTEXT is this right? *)
    (* don't if the field is of sort reentrant, we don't need the underlying
     * symbolic expression because it would be of the wrong sort *)
    if d = ctx.ctx_reentrant_sort then ctx.ctx_reentrant_const
    else
      let e_symb = get_symb_expr e in
      match e_symb with
      | Symb_z3 s -> s
      | Symb_reentrant _ -> ctx.ctx_reentrant_const
      | Symb_abs -> ctx.ctx_dummy_const
      | Symb_none ->
        Message.error ~pos:(Expr.pos e)
          "Fields of structs that are not functions or context variables must \
           have a symbolic expression. This should not happen if the \
           evaluation of fields worked."
      | Symb_incomplete ->
        Message.error ~pos:(Expr.pos e)
          "Fields of structs cannot be incomplete" (* TODO INC *)
      | Symb_error _ ->
        Message.error ~pos:(Expr.pos e)
          "Fields of structs cannot be errors when making the symbolic \
           expression. This should not happen if errors were handled properly."
  in
  let domain = Z3.FuncDecl.get_domain constructor in
  let es_symb = List.map2 z3_of_expr es domain in
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
    (* if Global.options.debug then Message.debug "struct accessors %s"
       (List.fold_left (fun acc a -> Z3.FuncDecl.to_string a ^ "," ^ acc) ""
       z3_accessors); *)
    let idx_mappings =
      List.combine (StructField.Map.keys fields) z3_accessors
    in
    let _, z3_accessor =
      List.find (fun (field1, _) -> StructField.equal field field1) idx_mappings
    in
    let range = Z3.FuncDecl.get_range z3_accessor in
    (* FIXME CONTEXT: is this ok? *)
    (* Same as the Symb_reentrant _ case of the match *)
    if range = ctx.ctx_reentrant_sort then field_expr
    else
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
  if Global.options.debug then
    Message.debug "enum constructors: @[<hov>%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt c -> Format.pp_print_string fmt (Z3.FuncDecl.to_string c)))
      z3_constructors;
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
  if Global.options.debug then
    Message.debug "enum accessors: @[<hov>%a@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (fun fmt c -> Format.pp_print_string fmt (Z3.FuncDecl.to_string c)))
      z3_accessors;
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
      (*if Global.options.debug then Message.debug "EApp>binder put mark %a on
        var " SymbExpr.formatter symb_expr (* (Print.expr ()) e *);*)
      add_conc_info_e symb_expr ~constraints:[] e
    (* NOTE CONC we keep the position from the var, as in concrete
       interpreter *)
    | None -> e)
  | _ -> e

let propagate_generic_error
    (e : conc_result)
    (other_constraints : PathConstraint.naked_path)
    (f : conc_expr -> conc_result) : conc_result =
  let e_symb = get_symb_expr_r e in
  match Mark.remove e, e_symb with
  | EGenericError, Symb_error _ ->
    let e_constraints = get_constraints_r e in
    if Global.options.debug then
      Message.debug "Propagating error %a" SymbExpr.formatter e_symb;
    let constraints = e_constraints @ other_constraints in
    (* Add the new constraints but don't change the symbolic expression *)
    add_conc_info_e SymbExpr.none ~constraints e
  | _, Symb_error _ ->
    Message.error ~internal:true
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

(* TODO QU RAPHAEL: these empty errors have been removed from the standard
   interpreter =>> OK *)
(* (\* NOTE We have to rewrite EmptyError propagation functions from
   [Concrete] *)
(* because they don't allow for [f] have a different input and output type
   *\) *)
(* let propagate_empty_error (e : conc_expr) (f : conc_expr -> conc_result) : *)
(*     conc_result = *)
(*   match e with (EEmptyError, _) as e -> e | _ -> f e *)

(* let propagate_empty_error_list *)
(*     (elist : conc_expr list) *)
(*     (f : conc_expr list -> conc_result) : conc_result = *)
(*   let rec aux acc = function *)
(*     | [] -> f (List.rev acc) *)
(*     | e :: r -> propagate_empty_error e (fun e -> aux (e :: acc) r) *)
(*   in *)
(*   aux [] elist *)

let handle_eq ctx pos e1 e2 =
  Runtime.Value.equal (Expr.pos_to_runtime pos) (Expr.embed_value ctx e1)
    (Expr.embed_value ctx e2)

let handle_compare ctx pos e1 e2 =
  Runtime.Value.compare (Expr.pos_to_runtime pos) (Expr.embed_value ctx e1)
    (Expr.embed_value ctx e2)

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
    (concrete : conc_naked_result)
    (symbolic_f : Z3.context -> s_expr -> s_expr -> s_expr)
    e1
    e2 : conc_result =
  let e1 = get_symb_expr e1 in
  let e2 = get_symb_expr e2 in
  if Global.options.debug then
    Message.debug "[op2] args %a, %a" SymbExpr.formatter_typed e1
      SymbExpr.formatter_typed e2;
  let symb_expr = SymbExpr.app2_z3 (symbolic_f ctx.ctx_z3) e1 e2 in
  (* TODO handle errors *)
  add_conc_info_m m symb_expr ~constraints:[] concrete

let op2list
    ctx
    m
    (concrete : conc_naked_result)
    (symbolic_f : Z3.context -> s_expr list -> s_expr)
    e1
    e2 : conc_result =
  let symbolic_f_curry ctx e1 e2 = symbolic_f ctx [e1; e2] in
  op2 ctx m concrete symbolic_f_curry e1 e2

let handle_division
    ctx
    m
    (concrete_f : unit -> conc_naked_result)
    (symbolic_f : Z3.context -> s_expr -> s_expr -> s_expr)
    e1
    e2 : conc_result =
  let e1_symb = get_symb_expr e1 in
  let e2_symb = get_symb_expr e2 in
  if Global.options.debug then
    Message.debug "[handle_div] args %a, %a" SymbExpr.formatter_typed e1_symb
      SymbExpr.formatter_typed e2_symb;

  let zero = SymbExpr.mk_z3 (Z3.Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 0) in
  let den_zero = SymbExpr.app2_z3 (Z3.Boolean.mk_eq ctx.ctx_z3) e2_symb zero in
  try
    let concrete = concrete_f () in
    let den_not_zero =
      SymbExpr.app_z3 (Z3.Boolean.mk_not ctx.ctx_z3) den_zero
    in
    let den_not_zero_pc =
      PathConstraint.mk_z3 den_not_zero (Expr.pos e2) false
    in
    let symb_expr = SymbExpr.app2_z3 (symbolic_f ctx.ctx_z3) e1_symb e2_symb in
    (* TODO handle errors *)
    (* A successful division adds one constraint : the denominator is not zero.
       The constraints from e1 and e2 are ignored here because those are fully
       evaluated expressions, and their constraints are handled by [EAppOp]. *)
    let constraints = [den_not_zero_pc] in
    add_conc_info_m m symb_expr ~constraints concrete
  with Runtime.(Error (DivisionByZero, _, _)) ->
    let den_zero_pc = PathConstraint.mk_z3 den_zero (Expr.pos e2) true in
    make_error_divisionbyzeroerror m [den_zero_pc]
      [
        Some "The division operator:", Expr.mark_pos m;
        Some "The null denominator:", Expr.pos e2;
      ]
      "division by zero at runtime"

(* Call-by-value: the arguments are expected to be already evaluated here *)
let evaluate_operator
    evaluate_expr
    ctx
    ((op, opos) : < overloaded : no ; .. > operator Mark.pos)
    (m : conc_info mark)
    (args : conc_expr list) : conc_result =
  let pos = Expr.mark_pos m in
  let rpos () = Expr.pos_to_runtime opos in
  let div_pos () =
    (* Division by 0 errors point to their 2nd operand *)
    Expr.pos_to_runtime
    @@ match args with _ :: denom :: _ -> Expr.pos denom | _ -> opos
  in
  (* let protect f x y = *)
  (*   (\* TODO CONC For now, I crash on date ambiguities, because they should not *)
  (*      happen: any duration expressed with months or years is rejected early *)
  (*      on. *\) *)
  (*   let get_binop_args_pos = function *)
  (*     | (arg0 :: arg1 :: _ : ('t, 'm) gexpr list) -> *)
  (*       ["", Expr.pos arg0; "", Expr.pos arg1] *)
  (*     | _ -> assert false *)
  (*   in *)
  (*   try f (rpos ()) x y *)
  (*   with Runtime.(Error (UncomparableValues, _, _)) -> *)
  (*     Message.error ~extra_pos:(get_binop_args_pos args) *)
  (*       "Cannot compare together durations that cannot be converted to a \ *)
  (*        precise number of days" *)
  (* in *)
  let err () =
    Message.error
      ~extra_pos:
        ([
           ( Format.asprintf "Operator (value %a):"
               (Print.operator ~debug:true)
               op,
             pos );
         ]
        @ List.mapi
            (fun i arg ->
              ( Format.asprintf "Argument n°%d, value %a" (i + 1)
                  (Print.expr ()) arg,
                Expr.pos arg ))
            args)
      "Operator %a applied to the wrong@ arguments@ (should not happen if the \
       term was well-typed)"
      (Print.operator ~debug:true)
      op
  in
  let open Runtime.Oper in
  (* trick to have this function type correctly *)
  let z3_round _ = z3_round ctx in
  (* Mark.add m @@ *)
  match op, args with
  | Length, [(EArray es, _)] ->
    let l = Runtime.integer_of_int (List.length es) in
    let symb_expr = SymbExpr.incomplete in
    (* no constraints generated *)
    add_conc_info_m m symb_expr ~constraints:[] (ELit (LInt l))
    (* TODO INC *)
  | Tag _, _ -> failwith "Tag evaluation not implemented"
  | (FromClosureEnv | ToClosureEnv), _ ->
    (* NOTE CONC used for typing only *)
    failwith "Eop From/ToClosureEnv should not appear in concolic evaluation"
  | Eq, [e1; e2] ->
    let concrete = ELit (LBool (handle_eq ctx.ctx_decl opos e1 e2)) in
    let s_e1 = get_symb_expr e1 in
    let s_e2 = get_symb_expr e2 in
    let symb_expr = SymbExpr.app2_z3 (Z3.Boolean.mk_eq ctx.ctx_z3) s_e1 s_e2 in
    (* TODO catch errors here, or maybe propagate [None]? *)
    add_conc_info_m m symb_expr ~constraints:[] concrete
  | Lt, [e1; e2] ->
    (* TODO CONC incompleteness warning for comparisons? eg on day < month *)
    let concrete = ELit (LBool (handle_compare ctx.ctx_decl opos e1 e2 < 0)) in
    op2 ctx m concrete Z3.Arithmetic.mk_lt e1 e2
  | Lte, [e1; e2] ->
    let concrete = ELit (LBool (handle_compare ctx.ctx_decl opos e1 e2 <= 0)) in
    op2 ctx m concrete Z3.Arithmetic.mk_le e1 e2
  | Gt, [e1; e2] ->
    let concrete = ELit (LBool (handle_compare ctx.ctx_decl opos e1 e2 > 0)) in
    op2 ctx m concrete Z3.Arithmetic.mk_gt e1 e2
  | Gte, [e1; e2] ->
    let concrete = ELit (LBool (handle_compare ctx.ctx_decl opos e1 e2 >= 0)) in
    op2 ctx m concrete Z3.Arithmetic.mk_ge e1 e2
  | Map, [f; (EArray es, _)] ->
    let concrete =
      EArray
        (List.map
           (fun e' ->
             evaluate_expr
               (Mark.copy e'
                  (EApp { f; args = [e']; tys = [Expr.maybe_ty (Mark.get e')] })))
           es)
    in
    add_conc_info_m m SymbExpr.incomplete ~constraints:[] concrete
  | Map2, [f; (EArray es1, _); (EArray es2, _)] ->
    let concrete =
      EArray
        (List.map2
           (fun e1 e2 ->
             evaluate_expr
               (Mark.add m
                  (EApp
                     {
                       f;
                       args = [e1; e2];
                       tys =
                         [
                           Expr.maybe_ty (Mark.get e1);
                           Expr.maybe_ty (Mark.get e2);
                         ];
                     })))
           es1 es2)
    in
    add_conc_info_m m SymbExpr.incomplete ~constraints:[] concrete
  | Reduce, [_; default; (EArray [], _)] ->
    let concrete = Mark.remove default in
    add_conc_info_m m SymbExpr.incomplete ~constraints:[] concrete |> make_ok
  | Reduce, [f; _; (EArray (x0 :: xn), _)] ->
    let concrete =
      Mark.remove
        (List.fold_left
           (fun acc x ->
             propagate_generic_error acc []
             @@ fun acc ->
             evaluate_expr
               (Mark.copy f
                  (EApp
                     {
                       f;
                       args = [acc; x];
                       tys =
                         [
                           Expr.maybe_ty (Mark.get acc);
                           Expr.maybe_ty (Mark.get x);
                         ];
                     })))
           (make_ok x0) xn)
    in
    add_conc_info_m m SymbExpr.incomplete ~constraints:[] concrete
  | Concat, [(EArray es1, _); (EArray es2, _)] ->
    let concrete = EArray (es1 @ es2) in
    add_conc_info_m m SymbExpr.incomplete ~constraints:[] concrete |> make_ok
  | Filter, [f; (EArray es, _)] ->
    let concrete =
      EArray
        (List.filter
           (fun e' ->
             match
               evaluate_expr
                 (Mark.copy e'
                    (EApp
                       { f; args = [e']; tys = [Expr.maybe_ty (Mark.get e')] }))
             with
             | ELit (LBool b), _ -> b
             | _ ->
               Message.error
                 ~pos:(Expr.pos (List.nth args 0))
                 "%a" Format.pp_print_text
                 "This predicate evaluated to something else than a boolean \
                  (should not happen if the term was well-typed)")
           es)
    in
    add_conc_info_m m SymbExpr.incomplete ~constraints:[] concrete |> make_ok
  | Fold, [f; init; (EArray es, _)] ->
    let concrete =
      Mark.remove
        (List.fold_left
           (fun acc e' ->
             propagate_generic_error acc []
             @@ fun acc ->
             evaluate_expr
               (Mark.copy e'
                  (EApp
                     {
                       f;
                       args = [acc; e'];
                       tys =
                         [
                           Expr.maybe_ty (Mark.get acc);
                           Expr.maybe_ty (Mark.get e');
                         ];
                     })))
           (make_ok init) es)
    in
    add_conc_info_m m SymbExpr.incomplete ~constraints:[] concrete
  | Find, [_f; (EArray _es, _)] ->
    failwith "Find evaluation not implemented yet"
  | Sort _, _ :: _ -> failwith "sort evaluation not implemented yet"
  | ArrayAccess _i, [(EArray _es, _)] ->
    failwith "ArrayAccess evaluation not implemented yet"
  | ( ( Length | Eq | Map | Map2 | Concat | Filter | Fold | Reduce | Find
      | Sort _ | ArrayAccess _ ),
      _ ) ->
    err ()
  | Not, [((ELit (LBool b), _) as e)] ->
    op1 ctx m (fun x -> ELit (LBool (o_not x))) Z3.Boolean.mk_not b e
  | And, [((ELit (LBool b1), _) as e1); ((ELit (LBool b2), _) as e2)] ->
    op2list ctx m (ELit (LBool (o_and b1 b2))) Z3.Boolean.mk_and e1 e2
  | Or, [((ELit (LBool b1), _) as e1); ((ELit (LBool b2), _) as e2)] ->
    op2list ctx m (ELit (LBool (o_or b1 b2))) Z3.Boolean.mk_or e1 e2
  | Xor, [((ELit (LBool b1), _) as e1); ((ELit (LBool b2), _) as e2)] ->
    op2 ctx m (ELit (LBool (o_xor b1 b2))) Z3.Boolean.mk_xor e1 e2
  | (Not | And | Or | Xor), _ -> err ()
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
  | ToInt_mon, [(ELit (LMoney _x), _)] ->
    failwith "ToInt_mon not implemented yet"
  | ToInt_rat, [(ELit (LRat _x), _)] ->
    failwith "ToInt_rat not implemented yet" (* ELit (LInt (o_toint_rat x)) *)
  (* is this used? should it be [round] or floor?*)
  | ToRat_int, [((ELit (LInt i), _) as e)] ->
    (* TODO maybe write specific tests for this and other similar cases? *)
    op1 ctx m (fun x -> ELit (LRat (o_torat_int x))) z3_force_real i e
  | ToRat_mon, [((ELit (LMoney i), _) as e)] ->
    op1 ctx m
      (fun x -> ELit (LRat (o_torat_mon x)))
      (fun ctx e ->
        let hundred = Z3.Arithmetic.Integer.mk_numeral_i ctx 100 in
        Z3.Arithmetic.mk_div ctx e hundred)
      i e
  | ToMoney_rat, [((ELit (LRat i), _) as e)] ->
    (* TODO be careful with this, [Round_mon], [Round_rat], [Mult_mon_rat],
       [Div_mon_rat] because of rounding *)
    op1 ctx m
      (fun x -> ELit (LMoney (o_tomoney_rat x)))
      (fun ctx e ->
        let cents =
          Z3.Arithmetic.mk_mul ctx [e; Z3.Arithmetic.Real.mk_numeral_i ctx 100]
        in
        z3_round ctx cents)
      i e
  | ToMoney_int, [((ELit (LInt i), _) as e)] ->
    op1 ctx m
      (fun x -> ELit (LMoney (o_tomoney_int x)))
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
  | Round_rat, [((ELit (LRat q), _) as e)] ->
    op1 ctx m
      (fun q -> ELit (LRat (o_round_rat q)))
      (fun ctx e -> z3_round ctx e)
      q e
  | Add_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2list ctx m (ELit (LInt (o_add_int_int x y))) Z3.Arithmetic.mk_add e1 e2
  | Add_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2list ctx m (ELit (LRat (o_add_rat_rat x y))) Z3.Arithmetic.mk_add e1 e2
  | Add_mon_mon, [((ELit (LMoney x), _) as e1); ((ELit (LMoney y), _) as e2)] ->
    op2list ctx m (ELit (LMoney (o_add_mon_mon x y))) Z3.Arithmetic.mk_add e1 e2
  | ( Add_dat_dur r,
      [((ELit (LDate x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2list ctx m
      (ELit (LDate (o_add_dat_dur r (rpos ()) x y)))
      DateEncoding.add_dat_dur e1 e2
  | ( Add_dur_dur,
      [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2list ctx m
      (ELit (LDuration (o_add_dur_dur x y)))
      DateEncoding.add_dur_dur e1 e2
  | Sub_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2list ctx m (ELit (LInt (o_sub_int_int x y))) Z3.Arithmetic.mk_sub e1 e2
  | Sub_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2list ctx m (ELit (LRat (o_sub_rat_rat x y))) Z3.Arithmetic.mk_sub e1 e2
  | Sub_mon_mon, [((ELit (LMoney x), _) as e1); ((ELit (LMoney y), _) as e2)] ->
    op2list ctx m (ELit (LMoney (o_sub_mon_mon x y))) Z3.Arithmetic.mk_sub e1 e2
  | Sub_dat_dat, [((ELit (LDate x), _) as e1); ((ELit (LDate y), _) as e2)] ->
    op2list ctx m
      (ELit (LDuration (o_sub_dat_dat x y)))
      DateEncoding.sub_dat_dat e1 e2
  | ( Sub_dat_dur r,
      [((ELit (LDate x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2list ctx m
      (ELit (LDate (o_sub_dat_dur r (rpos ()) x y)))
      DateEncoding.sub_dat_dur e1 e2
  | ( Sub_dur_dur,
      [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    op2list ctx m
      (ELit (LDuration (o_sub_dur_dur x y)))
      DateEncoding.sub_dur_dur e1 e2
  | Mult_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2list ctx m (ELit (LInt (o_mult_int_int x y))) Z3.Arithmetic.mk_mul e1 e2
  | Mult_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2list ctx m (ELit (LRat (o_mult_rat_rat x y))) Z3.Arithmetic.mk_mul e1 e2
  | Mult_mon_rat, [((ELit (LMoney x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2 ctx m
      (ELit (LMoney (o_mult_mon_rat x y)))
      (fun ctx cents r ->
        let product = Z3.Arithmetic.mk_mul ctx [cents; r] in
        z3_round ctx product)
      e1 e2
  | Mult_mon_int, [((ELit (LMoney x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2 ctx m
      (ELit (LMoney (o_mult_mon_int x y)))
      (fun ctx cents r ->
        let product = Z3.Arithmetic.mk_mul ctx [cents; r] in
        z3_round ctx product)
      e1 e2
  | Mult_dur_int, [((ELit (LDuration x), _) as e1); ((ELit (LInt y), _) as e2)]
    ->
    op2list ctx m
      (ELit (LDuration (o_mult_dur_int x y)))
      DateEncoding.mult_dur_int e1 e2
  | Div_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    handle_division ctx m
      (fun () -> ELit (LRat (o_div_int_int (div_pos ()) x y)))
      (fun ctx e1 e2 ->
        (* convert e1 to a [Real] explicitely to avoid using integer division *)
        let e1_rat = z3_force_real ctx e1 in
        Z3.Arithmetic.mk_div ctx e1_rat e2)
      e1 e2
  | Div_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    handle_division ctx m
      (fun () -> ELit (LRat (o_div_rat_rat (div_pos ()) x y)))
      (* Z3.Arithmetic.mk_div x y e1 e2 *)
      (fun ctx e1 e2 ->
        (* convert e1 to a [Real] explicitely to avoid using integer division *)
        let e1_rat = z3_force_real ctx e1 in
        Z3.Arithmetic.mk_div ctx e1_rat e2)
      e1 e2
  | Div_mon_mon, [((ELit (LMoney x), _) as e1); ((ELit (LMoney y), _) as e2)] ->
    handle_division ctx m
      (fun () -> ELit (LRat (o_div_mon_mon (div_pos ()) x y)))
      (fun ctx e1 e2 ->
        (* TODO factorize with [Div_int_int]? *)
        (* convert e1 to a [Real] explicitely to avoid using integer division *)
        let e1_rat = z3_force_real ctx e1 in
        Z3.Arithmetic.mk_div ctx e1_rat e2)
      e1 e2
  | Div_mon_rat, [((ELit (LMoney x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    handle_division ctx m
      (fun () -> ELit (LMoney (o_div_mon_rat (div_pos ()) x y)))
      (fun ctx cents r ->
        (* TODO maybe factorize with [Mult_mon_rat] and [ToRat_int]? *)
        let cents_rat = z3_force_real ctx cents in
        let div = Z3.Arithmetic.mk_div ctx cents_rat r in
        z3_round ctx div)
      e1 e2
  | Div_mon_int, [((ELit (LMoney x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    handle_division ctx m
      (fun () -> ELit (LMoney (o_div_mon_int (div_pos ()) x y)))
      (fun ctx cents r ->
        (* TODO maybe factorize with [Mult_mon_rat] and [ToRat_int]? *)
        let cents_rat = z3_force_real ctx cents in
        let div = Z3.Arithmetic.mk_div ctx cents_rat r in
        z3_round ctx div)
      e1 e2
  (* TODO with careful rounding *)
  | ( Div_dur_dur,
      [((ELit (LDuration x), _) as e1); ((ELit (LDuration y), _) as e2)] ) ->
    handle_division ctx m
      (fun () -> ELit (LRat (o_div_dur_dur (div_pos ()) x y)))
      DateEncoding.div_dur_dur e1 e2
  | HandleExceptions, [(EArray _exps, _)] ->
    failwith "HandleExceptions not implemented yet"
  | ConstructorCheck _, _ -> failwith "ConstructorCheck not implemented yet"
  | DebugPrint _, _ -> failwith "DebugPrint not implemented yet"
  | ValueFromJson _, _ -> failwith "ValueFromJson not implemented yet"
  (* ( let valid_exceptions = ListLabels.filter exps ~f:(function | EInj { name;
     cons; _ }, _ when EnumName.equal name Expr.option_enum ->
     EnumConstructor.equal cons Expr.some_constr | _ -> err ()) in match
     valid_exceptions with | [] -> EInj { name = Expr.option_enum; cons =
     Expr.none_constr; e = ELit LUnit, m } | [((EInj { cons; name; _ } as e),
     _)] when EnumName.equal name Expr.option_enum && EnumConstructor.equal cons
     Expr.some_constr -> e | [_] -> err () | excs -> raise Runtime.( Error
     (Conflict, List.map Expr.(fun e -> pos_to_runtime (pos e)) excs)) ) *)
  | ( ( Minus_int | Minus_rat | Minus_mon | Minus_dur | ToInt_mon | ToInt_rat
      | ToRat_int | ToRat_mon | ToMoney_int | ToMoney_rat | Round_rat
      | Round_mon | Add_int_int | Add_rat_rat | Add_mon_mon | Add_dat_dur _
      | Add_dur_dur | Sub_int_int | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat
      | Sub_dat_dur _ | Sub_dur_dur | Mult_int_int | Mult_rat_rat | Mult_mon_rat
      | Mult_mon_int | Mult_dur_int | Div_int_int | Div_rat_rat | Div_mon_mon
      | Div_mon_rat | Div_mon_int | Div_dur_dur | HandleExceptions | Lt | Gt
      | Lte | Gte ),
      _ ) ->
    err ()

let rec evaluate_expr :
    context -> Global.backend_lang -> conc_expr -> conc_result =
 fun ctx lang e ->
  if Global.options.debug then
    Message.debug "@[<v 0>eval %a@,symbolic: %a@]" (Print.expr ()) e
      SymbExpr.formatter (get_symb_expr e);
  (* if Global.options.debug then Message.debug "eval symbolic: %a"
     SymbExpr.formatter (get_symb_expr e); *)
  let m = Mark.get e in
  let pos = Expr.mark_pos m in
  let ret =
    match Mark.remove e with
    | EVar _ ->
      Message.error ~pos
        "free variable found at evaluation (should not happen if term was \
         well-typed)"
    | EExternal _ ->
      (* failwith "EExternal not implemented" *)
      let e = Concrete.evaluate_expr ctx.ctx_decl lang e in
      add_conc_info_e Symb_incomplete e |> make_ok
    | EApp { f = e1; args; _ } -> (
      if Global.options.debug then Message.debug "... it's an EApp";
      let e1 = evaluate_expr ctx lang e1 in
      if Global.options.debug then Message.debug "EApp f evaluated";
      propagate_generic_error e1 []
      @@ fun e1 ->
      let f_constraints = get_constraints e1 in
      let args = List.map (evaluate_expr ctx lang) args in
      if Global.options.debug then Message.debug "EApp args evaluated";
      propagate_generic_error_list args f_constraints
      @@ fun args ->
      let args_constraints = gather_constraints args in
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
          if Global.options.debug then
            Message.debug "EApp>EAbs vars are %a"
              (Format.pp_print_list Print.var_debug)
              (Array.to_list vars);
          if Global.options.debug then Message.debug "EApp>EAbs args are";
          List.iter
            (fun arg ->
              if Global.options.debug then
                Message.debug "EApp>EAbs arg | %a | %i"
                  (* (Print.expr ()) arg *) SymbExpr.formatter
                  (get_symb_expr arg)
                  (List.length (get_constraints arg)))
            args;
          let marked_eb =
            Expr.map_top_down ~f:(replace_EVar_mark vars_args_map) eb
          in
          if Global.options.debug then
            Message.debug "EApp>EAbs vars replaced in box";
          let marked_binder = Bindlib.unbox (Expr.bind vars marked_eb) in
          if Global.options.debug then
            Message.debug "EApp>EAbs binder reconstructed";
          let result =
            evaluate_expr ctx lang
              (Bindlib.msubst marked_binder
                 (Array.of_list (List.map Mark.remove args)))
          in
          if Global.options.debug then
            Message.debug "EApp>EAbs substituted binder evaluated";
          (* TODO [Expr.subst]? *)
          propagate_generic_error result (args_constraints @ f_constraints)
          @@ fun result ->
          let r_symb = get_symb_expr result in
          if Global.options.debug then
            Message.debug
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
          Message.error ~pos
            "wrong function call, expected %d arguments, got %d"
            (Bindlib.mbinder_arity binder)
            (List.length args)
      | ECustom _ ->
        (* failwith "EExternal not implemented" *)
        let e = Concrete.evaluate_expr ctx.ctx_decl lang e in
        add_conc_info_e Symb_incomplete e |> make_ok
        (* failwith "EApp of ECustom not implemented" *)
      | _ ->
        Message.error ~pos
          "function has not been reduced to a lambda at evaluation (should not \
           happen if the term was well-typed")
    | EAppOp { op; args; _ } ->
      if Global.options.debug then Message.debug "... it's an EAppOp";
      let args = List.map (evaluate_expr ctx lang) args in
      if Global.options.debug then Message.debug "EAppOp args evaluated";
      propagate_generic_error_list args []
      @@ fun args ->
      let args_constraints = gather_constraints args in
      let result = evaluate_operator (evaluate_expr ctx lang) ctx op m args in
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
    | EAbs _ ->
      if Global.options.debug then Message.debug "... it's an EAbs";
      (* Give Symb_abs symbolic expression if it is not already something else.
         This is mainly used in [make_z3_struct] *)
      add_conc_info_e SymbExpr.abs e |> make_ok
    | ELit l as e ->
      if Global.options.debug then Message.debug "... it's an ELit";
      let symb_expr = symb_of_lit ctx l in
      (* no constraints generated *)
      add_conc_info_m m symb_expr ~constraints:[] e
    (* | EAbs _ as e -> Marked.mark m e (* these are values *) *)
    | EStruct { fields = es; name } ->
      if Global.options.debug then Message.debug "... it's an EStruct";
      let fields, es = List.split (StructField.Map.bindings es) in
      (* compute all subexpressions *)
      let es = List.map (evaluate_expr ctx lang) es in
      propagate_generic_error_list es []
      @@ fun es ->
      (* make symbolic expression using the symbolic sub-expressions *)
      (* TODO INC *)
      let symb_expr =
        if List.exists (fun x -> get_symb_expr x = SymbExpr.incomplete) es then
          SymbExpr.incomplete
        else SymbExpr.mk_z3 (make_z3_struct ctx name es)
      in
      (* TODO catch error... should not happen *)
      (* gather all constraints from sub-expressions *)
      let constraints = gather_constraints es in
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
      if Global.options.debug then Message.debug "... it's an EStructAccess";
      propagate_generic_error (evaluate_expr ctx lang e) []
      @@ fun e ->
      match Mark.remove e with
      | EStruct { fields = es; name } ->
        if not (StructName.equal s name) then
          Message.error
            ~extra_pos:["", pos; "", Expr.pos e]
            "Error during struct access: not the same structs (should not \
             happen if the term was well-typed)";
        let field_expr =
          match StructField.Map.find_opt field es with
          | Some e' -> e'
          | None ->
            Message.error ~pos:(Expr.pos e)
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
        if Global.options.debug then
          Message.debug "EStructAccess symbolic struct access created";
        (* the constraints generated by struct access are only those generated
           by the subcall, as the field expression is already a value *)
        let constraints = get_constraints e in
        add_conc_info_m m symb_expr ~constraints (Mark.remove field_expr)
        |> make_ok
      | _ ->
        Message.error ~pos:(Expr.pos e)
          "The expression %a should be a struct %a but is not (should not \
           happen if the term was well-typed)"
          (Print.expr ()) e StructName.format s)
    | ETuple _ -> failwith "ETuple not implemented"
    | ETupleAccess _ -> failwith "ETupleAccess not implemented"
    | EBad -> assert false
    | EPos _ -> failwith "EPos not implemented"
    | EInj { name; e; cons } ->
      if Global.options.debug then Message.debug "... it's an EInj";
      propagate_generic_error (evaluate_expr ctx lang e) []
      @@ fun e ->
      let concrete = EInj { name; e; cons } in

      let e_symb = get_symb_expr e in
      let symb_expr = SymbExpr.app_z3 (make_z3_enum_inj ctx name cons) e_symb in
      let constraints = get_constraints e in

      add_conc_info_m m symb_expr ~constraints concrete |> make_ok
    | EMatch { e; cases; name } -> (
      if Global.options.debug then Message.debug "... it's an EMatch";
      (* NOTE: The surface keyword [anything] is expanded during desugaring, so
         it makes me generate many cases. See the [enum_wildcard] test for an
         example. TODO issue #130 asks for this feature ; use it once it is
         added. *)
      propagate_generic_error (evaluate_expr ctx lang e) []
      @@ fun e ->
      match Mark.remove e with
      | EInj { e = e1; cons; name = name' } ->
        if not (EnumName.equal name name') then
          Message.error
            ~extra_pos:["", Expr.pos e; "", Expr.pos e1]
            "Error during match: two different enums found (should not happen \
             if the term was well-typed)";
        let es_n =
          match EnumConstructor.Map.find_opt cons cases with
          | Some es_n -> es_n
          | None ->
            Message.error ~pos:(Expr.pos e)
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
            (fun (s, b) ->
              PathConstraint.mk_z3 (SymbExpr.simplify s) (Expr.pos e) b)
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
        Message.error ~pos:(Expr.pos e)
          "Expected a term having a sum type as an argument to a match (should \
           not happen if the term was well-typed")
    | EIfThenElse { cond; etrue; efalse } -> (
      if Global.options.debug then Message.debug "... it's an EIfThenElse";
      propagate_generic_error (evaluate_expr ctx lang cond) []
      @@ fun cond ->
      let c_symb = get_symb_expr cond in
      let c_constraints = get_constraints cond in
      match Mark.remove cond with
      | ELit (LBool true) ->
        if Global.options.debug then
          Message.debug "EIfThenElse>true adding %a to constraints"
            SymbExpr.formatter c_symb;
        let c_symb = SymbExpr.simplify c_symb in
        let c_path_constraint =
          PathConstraint.mk_z3 c_symb (Expr.pos cond) true
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
        if Global.options.debug then
          Message.debug "EIfThenElse>false adding %a to constraints"
            SymbExpr.formatter c_symb;
        let not_c_symb =
          SymbExpr.app_z3 (Z3.Boolean.mk_not ctx.ctx_z3) c_symb
        in
        let not_c_symb = SymbExpr.simplify not_c_symb in
        (* TODO catch error... should not happen *)
        let not_c_path_constraint =
          PathConstraint.mk_z3 not_c_symb (Expr.pos cond) false
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
        Message.error ~pos:(Expr.pos cond)
          "Expected a boolean literal for the result of this condition (should \
           not happen if the term was well-typed)")
    | EArray es ->
      let es = List.map (evaluate_expr ctx lang) es in
      propagate_generic_error_list es []
      @@ fun es ->
      let constraints = gather_constraints es in
      let es_concr = EArray es in
      add_conc_info_m m SymbExpr.incomplete ~constraints es_concr |> make_ok
      (* TODO INC *)
    | EAssert e' ->
      (* TODO CONC REU *)
      propagate_generic_error (evaluate_expr ctx lang e') []
      @@ fun e ->
      begin
        let e_symb = get_symb_expr e in
        let e_constraints = get_constraints e in
        match Mark.remove e with
        | ELit (LBool true) ->
          let concrete = ELit LUnit in
          let e_symb_pc = PathConstraint.mk_z3 e_symb (Expr.pos e') true in
          (* the constraints generated by an assertion when [e] is true are :
           * - those generated by the evaluation of [e]
           * - a new constraint corresponding to [e]
           *)
          (* NOTE that there is no symbolic expression on asserts *)
          let constraints = e_symb_pc :: e_constraints in
          add_conc_info_m m SymbExpr.none ~constraints concrete |> make_ok
        | ELit (LBool false) ->
          (* FIXME use [partially_evaluate_expr_for_assertion_failure_message]
             in error message like in concrete interpreter *)
          let not_e_symb =
            SymbExpr.app_z3 (Z3.Boolean.mk_not ctx.ctx_z3) e_symb
          in
          let not_e_symb_pc =
            PathConstraint.mk_z3 not_e_symb (Expr.pos e') false
          in
          let constraints = not_e_symb_pc :: e_constraints in
          make_error_assertionerror m constraints "Assertion failed"
          (* "Assertion failed:@\n%a" (Print.UserFacing.expr lang) e'
             (partially_evaluate_expr_for_assertion_failure_message ctx lang
             (Expr.skip_wrappers e')) *)
        | _ ->
          Message.error ~pos:(Expr.pos e')
            "Expected a boolean literal for the result of this assertion \
             (should not happen if the term was well-typed)"
      end
    | ECustom _ -> failwith "ECustom not implemented"
    | EEmpty ->
      if Global.options.debug then Message.debug "... it's an EEmptyError";
      make_ok e (* it is a value *)
    | EFatalError _err ->
      failwith "EFatalError not implemented"
      (* raise (Runtime.Error (err, [Expr.pos_to_runtime pos])) *)
    | EErrorOnEmpty e' -> (
      if Global.options.debug then Message.debug "... it's an EErrorOnEmpty";
      propagate_generic_error (evaluate_expr ctx lang e') []
      @@ fun e' ->
      match e' with
      | EEmpty, m ->
        make_error_emptyerror m (get_constraints e')
          "This variable evaluated to an empty term (no rule that defined it \
           applied in this situation)"
      | e -> make_ok e
      (* just pass along the concrete and symbolic values, and the
         constraints *)
      )
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
      (* failwith "[evaluate_expr] no more thunk" (* FIXME CONTEXT *) *)
      (* FIXME add metadata to find this case instead of this big match *)
      if Global.options.debug then
        Message.debug "... it's a context variable definition";

      let app = evaluate_expr ctx lang except in
      propagate_generic_error app []
      @@ fun app ->
      let pos = Expr.pos except in
      let abs_symb = get_symb_expr abs in
      let app_constraints =
        get_constraints app (* TODO check that this is always []? *)
      in
      match Mark.remove app with
      | EEmpty ->
        if Global.options.debug then Message.debug "Context>empty";
        let is_empty : PathConstraint.naked_path =
          PathConstraint.mk_reentrant abs_symb ctx.ctx_dummy_const pos true
          |> Option.to_list
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
        if Global.options.debug then Message.debug "Context>non-empty";
        let not_is_empty : PathConstraint.naked_path =
          PathConstraint.mk_reentrant abs_symb ctx.ctx_dummy_const pos false
          |> Option.to_list
        in
        (* the only constraint is the new one encoding the fact that there is a
           reentrant value, and the symbolic expression is that of the
           reentering value *)
        (* TODO check that constraints from app should stay as well, just in
           case *)
        let constraints = not_is_empty @ app_constraints in
        add_conc_info_e SymbExpr.none ~constraints app |> make_ok)
    | EDefault { excepts = [outer]; just = ELit (LBool true), _; cons }
      when SymbExpr.is_reentrant (get_symb_expr outer) -> (
      (* FIXME add metadata to find this case instead of this match? *)
      if Global.options.debug then
        Message.debug "... it's a context variable definition";

      let outer_symb = get_symb_expr outer in
      if Global.options.debug then
        Message.debug "context symb %a" SymbExpr.formatter outer_symb;
      let inner =
        match Mark.remove outer with
        | EEmpty -> outer
        | EPureDefault inner -> inner
        | _ -> failwith "no"
      in
      let eval_inner = evaluate_expr ctx lang inner in
      propagate_generic_error eval_inner []
      @@ fun eval_inner ->
      let pos = Expr.pos eval_inner in
      let eval_inner_constraints =
        get_constraints eval_inner (* TODO check that this is always []? *)
      in
      match Mark.remove eval_inner with
      | EEmpty ->
        if Global.options.debug then Message.debug "Context>empty";
        let is_empty : PathConstraint.naked_path =
          PathConstraint.mk_reentrant outer_symb ctx.ctx_reentrant_const pos
            true
          |> Option.to_list
        in
        let result = evaluate_expr ctx lang cons in
        propagate_generic_error result (is_empty @ eval_inner_constraints)
        @@ fun result ->
        let r_symb = get_symb_expr result in
        let r_constraints = get_constraints result in
        (* TODO check that constraints from app should stay as well, just in
           case *)
        let constraints = r_constraints @ is_empty @ eval_inner_constraints in
        add_conc_info_e r_symb ~constraints result |> make_ok
      | _ ->
        if Global.options.debug then Message.debug "Context>non-empty";
        let not_is_empty : PathConstraint.naked_path =
          PathConstraint.mk_reentrant outer_symb ctx.ctx_reentrant_const pos
            false
          |> Option.to_list
        in
        (* the only constraint is the new one encoding the fact that there is a
           reentrant value, and the symbolic expression is that of the
           reentering value *)
        (* TODO check that constraints from app should stay as well, just in
           case *)
        let constraints = not_is_empty @ eval_inner_constraints in
        add_conc_info_e SymbExpr.none ~constraints eval_inner |> make_ok)
    | EDefault { excepts; just; cons } ->
      if Global.options.debug then Message.debug "... it's an EDefault";

      let count_nonempty_greedy l =
        let l = List.map (evaluate_expr ctx lang) l in
        if Global.options.debug then
          Message.debug "EDefault using greedy conflict finder";
        let empty_count = List.length (List.filter Concrete.is_empty_error l) in
        let nonempty_count = List.length l - empty_count in
        nonempty_count, l
      in

      let count_nonempty_lazy l =
        if Global.options.debug then
          Message.debug "EDefault using lazy conflict finder";
        let l = List.map (fun e -> lazy (evaluate_expr ctx lang e)) l in
        let rec aux l seen_nonempty acc =
          match l with
          | [] -> Bool.to_int seen_nonempty, List.rev acc
          | (lazy ex) :: exs ->
            if not (Concrete.is_empty_error ex) then
              if seen_nonempty then 2, List.rev (ex :: acc)
              else aux exs true (ex :: acc)
            else aux exs seen_nonempty (ex :: acc)
        in
        aux l false []
      in

      let count_nonempty =
        if Optimizations.lazy_default ctx.ctx_optims then count_nonempty_lazy
        else count_nonempty_greedy
      in

      let nonempty_count, excepts = count_nonempty excepts in
      if Global.options.debug then
        Message.debug "EDefault found %n non-empty exceptions!" nonempty_count;
      handle_default ctx lang m (Expr.pos e) nonempty_count excepts just cons
    | EPureDefault _ when SymbExpr.is_reentrant (get_symb_expr e) ->
      if Global.options.debug then
        Message.debug "... it's an EPureDefault for reentrant";
      e |> make_ok
    | EPureDefault e -> evaluate_expr ctx lang e
    | _ -> .
  in
  (* if Global.options.debug then Message.debug "\teval returns %a | %a"
     (Print.expr ()) ret SymbExpr.formatter (get_symb_expr_r ret); *)
  if Global.options.debug then
    Message.debug "\teval returns %a" SymbExpr.formatter (get_symb_expr_r ret);
  ret

and handle_default ctx lang m pos nonempty_count excepts just cons =
  propagate_generic_error_list excepts []
  @@ fun excepts ->
  let exc_constraints = gather_constraints excepts in
  match nonempty_count with
  | 0 -> (
    if Global.options.debug then Message.debug "EDefault>no except";
    let just = evaluate_expr ctx lang just in
    propagate_generic_error just exc_constraints
    @@ fun just ->
    let j_symb = get_symb_expr just in
    let j_constraints = get_constraints just in
    match Mark.remove just with
    | EEmpty ->
      (* TODO should be a runtime error *)
      if Global.options.debug then Message.debug "EDefault>empty";
      (* TODO test this case *)
      (* the constraints generated by the default when [just] is empty are :
       * - those generated by the evaluation of the excepts
       * - those generated by the evaluation of [just]
       *)
      let constraints = j_constraints @ exc_constraints in
      add_conc_info_m m SymbExpr.none ~constraints EEmpty
    | ELit (LBool true) ->
      if Global.options.debug then
        Message.debug "EDefault>true adding %a to constraints"
          SymbExpr.formatter j_symb;
      let j_symb = SymbExpr.simplify j_symb in
      (* TODO catch error... should not happen *)
      (* TODO factorize the simplifications? *)
      let j_path_constraint =
        PathConstraint.mk_z3 j_symb (Expr.pos just) true
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
        c_constraints @ (j_path_constraint :: j_constraints) @ exc_constraints
      in
      add_conc_info_m c_mark c_symb ~constraints c_concr |> make_ok
    | ELit (LBool false) ->
      let not_j_symb = SymbExpr.app_z3 (Z3.Boolean.mk_not ctx.ctx_z3) j_symb in
      let not_j_symb = SymbExpr.simplify not_j_symb in
      let not_j_path_constraint =
        PathConstraint.mk_z3 not_j_symb (Expr.pos just) false
      in
      if Global.options.debug then
        Message.debug "EDefault>false adding %a to constraints"
          SymbExpr.formatter not_j_symb;
      (* the constraints generated by the default when [just] is false are :
       * - those generated by the evaluation of the excepts
       * - those generated by the evaluation of [just]
       * - a new constraint corresponding to [just] itself
       *)
      let constraints =
        (not_j_path_constraint :: j_constraints) @ exc_constraints
      in
      add_conc_info_m m SymbExpr.none ~constraints EEmpty
    | _ ->
      Message.error ~pos
        "Default justification has not been reduced to a boolean at evaluation \
         (should not happen if the term was well-typed)")
  | 1 ->
    if Global.options.debug then Message.debug "EDefault>except";
    let r = List.find (fun sub -> not (Concrete.is_empty_error sub)) excepts in
    (* the constraints generated by the default when exactly one except is raised are :
     * - those generated by the evaluation of the excepts
     *)
    let r_symb = get_symb_expr r in
    let constraints = exc_constraints in
    add_conc_info_e r_symb ~constraints r |> make_ok
  | _ ->
    (* TODO QU Raphaël: discrepancy with standard interpreter? =>> NON à mon
       avis *)
    make_error_conflicterror m exc_constraints
      (List.map
         (fun except ->
           Some "This consequence has a valid justification:", Expr.pos except)
         (List.filter (fun sub -> not (Concrete.is_empty_error sub)) excepts))
      "There is a conflict between multiple valid consequences for assigning \
       the same variable."

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
    | TArrow ([(TLit TUnit, _)], (TDefault _inner_ty, _)) ->
      failwith "[make_input_mark] no more thunks" (* FIXME CONTEXT *)
    | TDefault inner_ty ->
      (* Context variables carry the name of the actual input variable (that is
         the name of the field in the input struct), as well as a symbol used to
         mark the default expression, that can then be used in Z3 when the given
         value is non-empty. See [make_reentrant_input]. *)
      if Global.options.debug then
        Message.debug "[make_input_mark] reentrant variable <%s> : %a" name
          Print.typ ty;
      let _, inner_sort = translate_typ ctx (Mark.remove inner_ty) in
      let symbol = Z3.Expr.mk_const_s ctx.ctx_z3 name inner_sort in
      SymbExpr.mk_reentrant field symbol
    | TArrow _ ->
      (* proper functions are not allowed as input *)
      Message.error ~pos:(Mark.get ty)
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

let soft_constraints_of_input_mark ctx (m : conc_info mark) :
    PathConstraint.pc_expr list =
  let (Custom { custom = { ty; symb_expr; _ }; _ }) = m in
  let ty = Option.get ty in
  (* by construction ty is not None *)
  let naked_ty = Mark.remove ty in
  let pos = Mark.get ty in
  let ctx = ctx.ctx_z3 in
  match naked_ty, symb_expr with
  | TLit TMoney, Symb_z3 var ->
    let zero = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 in
    let money_unit = Z3.Arithmetic.Integer.mk_numeral_i ctx 1_00 in
    let money_ten = Z3.Arithmetic.Integer.mk_numeral_i ctx 10_00 in
    let money_hundred = Z3.Arithmetic.Integer.mk_numeral_i ctx 100_00 in
    let non_negative = Z3.Arithmetic.mk_ge ctx var zero in
    let round_unit =
      Z3.Boolean.mk_eq ctx
        (Z3.Arithmetic.Integer.mk_mod ctx var money_unit)
        zero
    in
    let round_ten =
      Z3.Boolean.mk_eq ctx (Z3.Arithmetic.Integer.mk_mod ctx var money_ten) zero
    in
    let round_hundred =
      Z3.Boolean.mk_eq ctx
        (Z3.Arithmetic.Integer.mk_mod ctx var money_hundred)
        zero
    in
    List.map
      (fun (x, id) ->
        let x = SymbExpr.mk_z3 x in
        (PathConstraint.mk_soft x 1 (Some id) pos false).expr)
      [
        non_negative, "3non-negative";
        round_unit, "2unit";
        round_ten, "1ten";
        round_hundred, "0hundred";
      ]
  | _ -> []

let make_soft_constraints ctx (input_marks : conc_info mark StructField.Map.t) :
    PathConstraint.pc_expr list =
  StructField.Map.fold
    (fun _ m acc -> soft_constraints_of_input_mark ctx m @ acc)
    input_marks []

(** Evaluation *)

(** Do a "pre-evaluation" of the program. It compiles the target scope to a
    function that takes the input struct of the scope and returns its output
    struct *)
let simplify_program ctx (p : (dcalc, 'm) gexpr program) s : conc_expr =
  if Global.options.debug then
    Message.debug "[CONC] Make program expression concolic";
  let e = Expr.unbox (Program.to_expr p s) in
  if Global.options.debug then
    Message.debug "[CONC] Pre-compute program concretely";
  let result = Concrete.evaluate_expr ctx.ctx_decl p.lang e in
  init_conc_expr result

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
  if Global.options.debug then Message.debug "...inputs applied...";
  evaluate_expr ctx lang (Expr.unbox to_interpret)

(** Constraint solving *)
module Solver (Settings : sig
  val optims : Optimizations.flag list
end) =
struct
  type unknown_info = {
    z3reason : string;
    z3stats : Z3.Statistics.statistics;
    z3solver_string : string;
    z3assertions : s_expr list;
  }

  type z3_solver_result =
    | Z3Sat of Z3.Model.model option
    | Z3Unsat
    | Z3Unknown of unknown_info

  (* FIXME this is ugly *)
  let num_unsat = ref 0
  let num_soft_unsat = ref 0
  let num_soft_sat = ref 0

  module StringMap = Map.Make (String)

  let soft_sats = ref StringMap.empty
  let soft_unsats = ref StringMap.empty

  let incr_sat group =
    soft_sats :=
      StringMap.update group
        (fun n -> Some (Option.value ~default:0 n + 1))
        !soft_sats

  let incr_unsat group =
    soft_unsats :=
      StringMap.update group
        (fun n -> Some (Option.value ~default:0 n + 1))
        !soft_unsats

  let print_soft_sats () =
    let open Format in
    let seq = StringMap.to_seq !soft_sats in
    asprintf "sat groups:@[<v>@,%a@]"
      (pp_print_seq ~pp_sep:pp_print_cut (fun fmt (name, n) ->
           fprintf fmt "%n %s sat group" n name))
      seq

  let print_soft_unsats () =
    let open Format in
    let seq = StringMap.to_seq !soft_unsats in
    asprintf "unsat groups:@[<v>@,%a@]"
      (pp_print_seq ~pp_sep:pp_print_cut (fun fmt (name, n) ->
           fprintf fmt "%n %s unsat group" n name))
      seq

  module type Z3SolverModuleType = sig
    type t

    val make : Z3.context -> t
    val add : t -> s_expr list -> unit
    val push : t -> unit
    val pop : t -> unit
    val check : t -> s_expr list -> Z3.Solver.status
    val get_model : t -> Z3.Model.model option
    val get_reason_unknown : t -> string
    val get_statistics : t -> Z3.Statistics.statistics
    val to_string : t -> string
    val get_assertions : t -> s_expr list
  end

  module Z3SolverModule_Solver : Z3SolverModuleType = struct
    type t = Z3.Solver.solver

    let make ctx = Z3.Solver.mk_solver ctx None
    let add s l = Z3.Solver.add s l

    (* let add_soft _ _ l = if l <> [] then Message.error ~internal:true "Tried
       to add a soft constraint on an incompatible solver. Try activating the
       soft constraint option." *)
    let push = Z3.Solver.push
    let pop s = Z3.Solver.pop s 1
    let check s cs = Z3.Solver.check s cs
    let get_model = Z3.Solver.get_model
    let get_reason_unknown = Z3.Solver.get_reason_unknown
    let get_statistics = Z3.Solver.get_statistics
    let to_string = Z3.Solver.to_string
    let get_assertions = Z3.Solver.get_assertions
  end

  module type Z3SolverType = sig
    val solve :
      Z3.context -> s_expr list -> PathConstraint.soft list -> z3_solver_result

    val push : Z3.context -> s_expr -> unit
    val pop : Z3.context -> unit -> unit
  end

  module SimpleZ3Solver (S : Z3SolverModuleType) : Z3SolverType = struct
    let solve ctx (constraints : s_expr list) (_ : PathConstraint.soft list) :
        z3_solver_result =
      let solver = S.make ctx in
      S.add solver constraints;
      if Global.options.debug then
        Message.debug "Solver is\n%s" (S.to_string solver);
      match S.check solver [] with
      | SATISFIABLE -> Z3Sat (S.get_model solver)
      | UNSATISFIABLE -> Z3Unsat
      | UNKNOWN ->
        Z3Unknown
          {
            z3reason = S.get_reason_unknown solver;
            z3stats = S.get_statistics solver;
            z3solver_string = S.to_string solver;
            z3assertions = S.get_assertions solver;
          }

    let push _ _ = ()
    let pop _ () = ()
  end

  module IncrementalZ3Solver (S : Z3SolverModuleType) : Z3SolverType = struct
    let solver = ref None

    let get_solver (ctx : Z3.context) =
      match !solver with
      | None ->
        let s = S.make ctx in
        solver := Some s;
        s
      | Some s -> s

    let _solve ctx (local_constraints : s_expr list) : z3_solver_result =
      let solver = get_solver ctx in
      if Global.options.debug then Message.debug "Using incremental Z3 solver";
      S.push solver;
      S.add solver local_constraints;
      let status = S.check solver local_constraints in
      if Global.options.debug then
        Message.debug "Solver is\n%s" (S.to_string solver);
      let result =
        match status with
        | SATISFIABLE -> Z3Sat (S.get_model solver)
        | UNSATISFIABLE -> Z3Unsat
        | UNKNOWN ->
          Z3Unknown
            {
              z3reason = S.get_reason_unknown solver;
              z3stats = S.get_statistics solver;
              z3solver_string = S.to_string solver;
              z3assertions = S.get_assertions solver;
            }
      in
      S.pop solver;
      result

    module StringMap = String.Map

    let group_softs (softs : PathConstraint.soft list) :
        PathConstraint.soft list StringMap.t =
      List.fold_left
        (fun acc (s : PathConstraint.soft) ->
          StringMap.update s.id
            (function None -> Some [s] | Some l -> Some (s :: l))
            acc)
        StringMap.empty softs

    let fold_softs ctx name group acc =
      match acc with
      | Some _ -> acc
      | None -> begin
        if Global.options.debug then Message.debug "Trying soft group %s" name;
        let soft_exprs =
          List.map (fun (s : PathConstraint.soft) -> s.symb) group
        in
        let result_soft = _solve ctx soft_exprs in
        match result_soft with
        | Z3Sat _ ->
          incr num_soft_sat;
          incr_sat name;
          Some result_soft
        | Z3Unsat ->
          incr num_soft_unsat;
          incr_unsat name;
          None
        | Z3Unknown _ as r -> Some r
      end

    let solve ctx _ (softs : PathConstraint.soft list) : z3_solver_result =
      if Global.options.debug then
        Message.debug "Trying solver without softs...";
      let result = _solve ctx [] in
      match result with
      | Z3Sat _ ->
        if softs = [] then result
        else begin
          if Global.options.debug then
            Message.debug "Sat without softs, so trying solver with softs";
          let groups = group_softs softs in
          (* Message.result "%s" (StringMap.to_seq groups |> List.of_seq |>
             List.map fst |> List.hd); *)
          let try_softs = StringMap.fold (fold_softs ctx) groups None in
          Option.value try_softs ~default:result
        end
      | _ ->
        incr num_unsat;
        result

    let push ctx e =
      let t = Sys.time () in
      let solver = get_solver ctx in
      S.push solver;
      S.add solver [e];
      if Global.options.debug then
        Message.debug "after_push %f" (Sys.time () -. t)

    let pop ctx () =
      let t = Sys.time () in
      let solver = get_solver ctx in
      S.pop solver;
      if Global.options.debug then
        Message.debug "after_pop %f" (Sys.time () -. t)
  end

  let z3Solver =
    let module SM = Z3SolverModule_Solver in
    if Optimizations.incremental_solver Settings.optims then
      (module IncrementalZ3Solver (SM) : Z3SolverType)
    else (module SimpleZ3Solver (SM) : Z3SolverType)

  module Z3Solver = (val z3Solver)

  type input = PathConstraint.pc_expr list

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

  let fmt_unknown_info (fmt : Format.formatter) (info : unknown_info) =
    let open Format in
    fprintf fmt "Reason: %s" info.z3reason;
    pp_print_newline fmt ();
    fprintf fmt "Statistics:@.%s" (Z3.Statistics.to_string info.z3stats);
    pp_print_newline fmt ();
    fprintf fmt "Solver:@.%s" info.z3solver_string;
    pp_print_newline fmt ();
    fprintf fmt "Assertions:@.  @[<v>%a@]"
      (Format.pp_print_list Format.pp_print_string)
      (List.map Z3.Expr.to_string info.z3assertions)

  type solver_result = Sat of model option | Unsat | Unknown of unknown_info

  let split_input (l : input) :
      s_expr list * PathConstraint.soft list * StructField.Set.t =
    let rec aux
        l
        (acc_z3 : s_expr list)
        (acc_soft : PathConstraint.soft list)
        (acc_reentrant : StructField.Set.t) =
      let open PathConstraint in
      match l with
      | [] -> acc_z3, acc_soft, acc_reentrant
      | Pc_z3 e :: l' -> aux l' (e :: acc_z3) acc_soft acc_reentrant
      | Pc_soft s :: l' -> aux l' acc_z3 (s :: acc_soft) acc_reentrant
      | Pc_reentrant e :: l' ->
        aux l' acc_z3 acc_soft
          (if e.is_empty then StructField.Set.add e.symb.name acc_reentrant
           else acc_reentrant)
      | Pc_incomplete :: _ ->
        Message.error ~internal:true
          "[Solver.split_input] should not get Pc_incomplete as argument"
    in
    aux l [] [] StructField.Set.empty

  (* https://discuss.ocaml.org/t/computation-with-time-constraint/5548/9 *)
  exception Timeout

  let delayed_fun f timeout =
    if not @@ Optimizations.timeout Settings.optims then f ()
    else
      let _ =
        Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout))
      in
      ignore (Unix.alarm timeout);
      try
        let r = f () in
        ignore (Unix.alarm 0);
        r
      with e ->
        ignore (Unix.alarm 0);
        raise e

  let solve (ctx : context) (constraints : input) =
    if Optimizations.check_easy_unsat Settings.optims ctx.ctx_z3 constraints
    then Unsat
    else
      let rec aux retry ctx constraints =
        let z3_constraints, z3_soft_constraints, model_empty_reentrants =
          split_input constraints
        in
        try
          let status =
            delayed_fun
              (fun () ->
                Z3Solver.solve ctx.ctx_z3 z3_constraints z3_soft_constraints)
              1
          in
          begin match status with
          | Z3Sat (Some model_z3) ->
            Sat (Some { model_z3; model_empty_reentrants })
          | Z3Sat None -> Sat None
          | Z3Unsat -> Unsat
          | Z3Unknown info -> Unknown info
          end
        with Timeout ->
          if retry then begin
            Message.warning "Concolic execution solver timed out once";
            Message.warning "Trying to solve again...";
            aux false ctx constraints
          end
          else Message.error ~internal:true "[CONC] Solver timed out"
      in
      aux (Optimizations.timeout_retry Settings.optims) ctx constraints

  let push ctx (pc : PathConstraint.pc_expr) =
    match pc with
    | Pc_z3 pc -> Z3Solver.push ctx.ctx_z3 pc
    | Pc_soft _ -> () (* Z3Solver.push_soft ctx.ctx_z3 sc *)
    | Pc_reentrant _ -> ()
    | Pc_incomplete ->
      Message.error ~internal:true
        "[Solver.push] should not get Pc_incomplete as argument"

  let pop ctx (pc : PathConstraint.pc_expr) =
    match pc with
    | Pc_z3 _ | Pc_soft _ -> Z3Solver.pop ctx.ctx_z3 ()
    | Pc_reentrant _ -> ()
    | Pc_incomplete ->
      Message.error ~internal:true
        "[Solver.pop] should not get Pc_incomplete as argument"

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
    | TInt -> LInt (Z.of_int 0)
    | TMoney -> LMoney (Runtime.money_of_units_int 0)
    | TUnit | TPos -> LUnit
    | TRat -> LRat (Runtime.decimal_of_string "0")
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
      Message.error ~pos:(Expr.mark_pos mark)
        "[default_expr_of_typ] should not be called on a context variable. \
         This should not happen if context variables were handled properly."
    | TArrow _ ->
      (* functions *)
      Message.error ~pos:(Expr.mark_pos mark)
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
    | TForAll _ -> failwith "[default_expr_of_typ] TForAll not implemented"
    | TVar _ -> failwith "[default_expr_of_typ] TVar not implemented"
    | TAbstract _ -> failwith "[default_expr_of_typ] TAbstract not implemented"
    | TError -> failwith "[default_expr_of_typ] TError not implemented"
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
    | TPos | TUnit ->
      LUnit (* TODO maybe check that the Z3 value is indeed unit? *)
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
    | TForAll _ -> failwith "[value_of_symb_expr] TForAll not implemented"
    | TVar _ -> failwith "[value_of_symb_expr] TVar not implemented"
    | TAbstract _ -> failwith "[value_of_symb_expr] TAbstract not implemented"
    | TError -> failwith "[value_of_symb_expr] TError not implemented"
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
      Message.error ~pos:(Expr.mark_pos mark)
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
         in the scope), then we make an empty term. During evaluation, the
         [name] of the variable will be used to generate a constraint encoding
         whether it is empty, but the symbolic expression on the (empty) innner
         term will not be used. *)
      if Global.options.debug then Message.debug "[make_reentrant_input] empty";
      Expr.eempty mk)
    else (
      (* If the context variable must evaluate to a specific value computed by
         the Z3 model, then we make this inner term and encapsulate it. The mark
         on the inner term (inside the default) is the symbol in the
         Symb_reentrant structure, and will be be used during evaluation. The
         mark on the outer term (the default term itself) will be used only for
         its [name] field and will be used to generate a constraint encoding
         whether it is empty. *)
      if Global.options.debug then
        Message.debug "[make_reentrant_input] non empty";
      match Mark.remove ty with
      | TArrow ([(TLit TUnit, _)], (TDefault _inner_ty, _)) ->
        failwith "[make_reentrant_input] no more thunk" (* FIXME CONTEXT *)
      | TDefault inner_ty ->
        let inner_mk =
          map_conc_mark ~symb_expr_f:(fun _ -> Symb_z3 symb_expr) mk
        in
        let term = make_term ctx z3_model inner_mk inner_ty symb_expr in
        let (Custom { custom; _ }) = Mark.get term in
        if Global.options.debug then
          Message.debug "[make_reentrant_input] non empty inner: %a"
            SymbExpr.formatter_typed custom.symb_expr;
        let term = Expr.epuredefault term mk in
        let (Custom { custom; _ }) = Mark.get term in
        if Global.options.debug then
          Message.debug "[make_reentrant_input] non empty thunked: %a"
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
        | Symb_incomplete ->
          failwith "[inputs_of_model] input mark should not be incomplete"
          (* TODO INC *)
        | Symb_abs ->
          failwith
            "[inputs_of_model] input mark should not be abs" (* TODO INC *)
        | Symb_error _ ->
          failwith "[inputs_of_model] input mark should not be an error"
      in
      let (Custom { custom; _ }) = Mark.get t in
      if Global.options.debug then
        Message.debug "[inputs_of_model] input has symb? %a" SymbExpr.formatter
          custom.symb_expr;
      t
    in
    StructField.Map.mapi f input_marks
end

(** Remove marks from an annotated path, to get a list of path constraints to
    feed in the solver. In doing so, actually negate constraints marked as
    Negated. This function shall be called on an output of
    [PathConstraint.make_expected_path]. *)
let pc_expr_of_apc ctx (apc : PathConstraint.annotated_pc) :
    PathConstraint.pc_expr (* FIXME Solver.input *) =
  let open PathConstraint in
  match apc with
  | Normal c -> c.expr
  | Done c -> c.expr
  | Negated c -> begin
    match c.expr with
    | Pc_z3 e -> Pc_z3 (Z3.Boolean.mk_not ctx.ctx_z3 e)
    | Pc_soft _ ->
      failwith "[pc_expr_of_apc] negation of soft constraint should not happen"
    | Pc_reentrant e -> Pc_reentrant { e with is_empty = not e.is_empty }
    | Pc_incomplete ->
      Message.error ~internal:true
        "[pc_expr_of_apc] should not get Pc_incomplete as argument"
  end

let constraint_list_of_path ctx (path : PathConstraint.annotated_path) :
    PathConstraint.pc_expr list (* FIXME Solver.input? *) =
  List.map (pc_expr_of_apc ctx) path

let apply_diff
    ctx
    f_push
    f_pop
    (diff : PathConstraint.incremental_annotated_pc list) : unit =
  if Global.options.debug then Message.debug "apply_diff";
  let f = function
    | PathConstraint.IncrPush apc ->
      let expr = pc_expr_of_apc ctx apc in
      f_push ctx expr
    | PathConstraint.IncrPop apc ->
      let expr = pc_expr_of_apc ctx apc in
      f_pop ctx expr
  in
  List.iter f diff

let print_fields (prefix : string) fields =
  let ordered_fields =
    List.sort (fun ((v1, _), _) ((v2, _), _) -> String.compare v1 v2) fields
  in
  List.iter
    (fun ((var, _), value) ->
      Message.result "%s@[<hov 2>%s@ =@ %a@]%s" prefix var (Print.expr ()) value
        (if Global.options.debug then
           " | " ^ SymbExpr.to_string (_get_symb_expr_unsafe value)
         else ""))
    ordered_fields

module Stats = struct
  (* TODO: quel temps manque ? compter le nombre d'evals *)
  (* GC: pic mémoire alloué ? Z3 incrémental le dit ? *)
  type time = float
  type period = { start : time; stop : time }
  type step = string * period

  type execution = {
    steps : step list;
    total_time : period;
    num_constraints : int;
  }

  type t = {
    total_time : period;
    steps : step list;
    executions : execution list;
  }

  let start_period () : period =
    let start = Sys.time () in
    { start; stop = nan }

  let stop_period p : period =
    let stop = Sys.time () in
    { p with stop }

  let init () : t =
    let total_time = start_period () in
    { total_time; steps = []; executions = [] }

  let start_step message : step = message, start_period ()
  let stop_step ((msg, p) : step) : step = msg, stop_period p

  let start_exec num_constraints : execution =
    let total_time = start_period () in
    { steps = []; total_time; num_constraints }

  let add_exec_step (e : execution) step : execution =
    { e with steps = step :: e.steps }

  let stop_exec (e : execution) : execution =
    let total_time = stop_period e.total_time in
    { e with total_time }

  let add_stat_step (stats : t) (step : step) : t =
    { stats with steps = step :: stats.steps }

  let add_stat_exec (stats : t) (e : execution) : t =
    { stats with executions = e :: stats.executions }

  let stop stats : t =
    let total_time = stop_period stats.total_time in
    { stats with total_time }

  let running_period stats : period = stop_period stats.total_time

  let fold_execs (execs : execution list) : step list =
    let f (steps : step list) (exec : execution) =
      List.map2
        (fun (s, p) (s', p') ->
          assert (String.equal s s');
          s, { p with stop = p.stop +. p'.stop -. p'.start })
        steps exec.steps
    in
    match execs with
    | [] -> []
    | { steps; _ } :: execs -> List.fold_left f steps execs

  module Print = struct
    open Format

    (* let ms (fmt : formatter) (t : time) = *)
    (*   let milli : int = int_of_float (t *. 1000.) in *)
    (*   pp_print_int fmt milli; *)
    (*   pp_print_string fmt " ms" *)

    let sec (fmt : formatter) (t : time) =
      fprintf fmt "%.3f" t;
      pp_print_string fmt " s"

    let period (fmt : formatter) (p : period) = sec fmt (p.stop -. p.start)

    let step (fmt : formatter) ((msg, p) : step) =
      fprintf fmt "%s: %a" msg period p

    (* let itemize (bullet : string) (ppf : formatter -> 'a -> unit) (fmt :
       formatter) (x : 'a) = fprintf fmt "%s %a" bullet ppf x *)

    let steps (fmt : formatter) (l : step list) =
      let l = List.rev l in
      pp_print_list ~pp_sep:pp_print_cut step fmt l

    let executions (fmt : formatter) (execs : execution list) =
      let folded = fold_execs execs |> List.rev in
      let max_constraints =
        List.fold_left (fun acc exe -> max acc exe.num_constraints) 0 execs
      in
      pp_print_list ~pp_sep:pp_print_cut step fmt folded;
      pp_print_cut fmt ();
      fprintf fmt "max constraints: %n" max_constraints
  end

  let print (fmt : Format.formatter) (stats : t) =
    let open Format in
    fprintf fmt "General steps:@\n@[<v 2>  %a@]@\n" Print.steps stats.steps;
    fprintf fmt "After %n execution steps:@\n@[<v 2>  %a@]@\n"
      (List.length stats.executions)
      Print.executions stats.executions;
    fprintf fmt "Total concolic time: %a" Print.period stats.total_time
end

(** Main function *)
let interpret_program_concolic
    (type m)
    (print_stats : bool)
    (optims : Optimizations.flag list)
    (mutation_seed : int option)
    (p : (dcalc, m) gexpr program)
    s : (Uid.MarkedString.info * conc_expr) list =
  if Global.options.debug then
    Message.debug "=== Start concolic interpretation... ===";
  Optimizations.check_optims_coherent optims;

  (* output_name, out_fmt : string * Format.formatter) *)
  let stats = Stats.init () in

  let s_context_creation = Stats.start_step "create context" in
  let decl_ctx = p.decl_ctx in
  if Global.options.debug then Message.debug "[CONC] Create empty context";
  let ctx = make_empty_context decl_ctx optims in
  if Global.options.debug then Message.debug "[CONC] Initialize context";
  let ctx = init_context ctx in
  let stats = Stats.stop_step s_context_creation |> Stats.add_stat_step stats in

  let s_simplify = Stats.start_step "simplify" in
  let scope_e = simplify_program ctx p s in

  let ast_stats = Mutation.get_stats scope_e in

  if Optimizations.mutation optims then Mutation.init mutation_seed;

  let scope_e =
    if Optimizations.random_mutations optims then begin
      let mutations =
        List.filter_map
          (fun (o, f, p) -> if o optims then Some (f, p) else None)
          [
            Optimizations.mutation_remove, Mutation.remove_excepts 0.3, 0.3;
            Optimizations.mutation_duplicate, Mutation.duplicate_excepts, 0.3;
            Optimizations.mutation_negate_justs, Mutation.negate_justs, 0.1;
          ]
      in
      if Global.options.debug then
        Message.debug "Before random mutations:\n%a" (Print.expr ()) scope_e;
      let mutated_scope_e =
        Mutation.apply_mutations mutations scope_e |> Expr.unbox
      in
      if Global.options.debug then
        Message.debug "\nAfter random mutations:\n%a" (Print.expr ())
          mutated_scope_e;
      mutated_scope_e
    end
    else if Optimizations.mutation_one_conflict optims then begin
      if Global.options.debug then
        Message.debug "Before one mutation:\n%a" (Print.expr ()) scope_e;
      let mutated_scope_e =
        Mutation.create_one_conflict scope_e |> Expr.unbox
      in
      if Global.options.debug then
        Message.debug "\nAfter one mutation:\n%a" (Print.expr ())
          mutated_scope_e;
      mutated_scope_e
    end
    else scope_e
  in

  let scope_e = Optimizations.optimize_expr optims scope_e in

  if Optimizations.ast_stats optims then begin
    Message.result "%a" Mutation.pprint_ast_stats (Mutation.get_stats scope_e);
    exit 0
  end;

  let stats = Stats.stop_step s_simplify |> Stats.add_stat_step stats in
  match scope_e with
  | EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e -> begin
    (* [taus] contain the types of the scope arguments. For [context] arguments,
       we can provide an empty thunked term. For [input] arguments of another
       type, we provide an empty value. *)
    (* first set of inputs *)
    let taus = StructName.Map.find s_in ctx.ctx_decl.ctx_structs in

    (* TODO CONC should it be [mark_e] or something else? *)
    let input_marks = StructField.Map.mapi (make_input_mark ctx mark_e) taus in
    let soft_constraints =
      if Optimizations.soft_constraints optims then
        make_soft_constraints ctx input_marks
      else []
    in
    if Global.options.debug then
      Message.debug "Initial soft constraints: %a\n"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           PathConstraint.Print.pc_expr)
        soft_constraints;

    let total_tests = ref 0 in

    let module Solver = Solver (struct
      let optims = optims
    end) in
    (* add soft constraints to solver if it is incremental *)
    List.iter (Solver.push ctx) soft_constraints;

    let found_incomplete = ref false in

    let rec concolic_loop (previous_path : PathConstraint.annotated_path) stats
        : Stats.t =
      if Optimizations.tests_vs_time optims then
        Message.result "time of step: %a" Stats.Print.period
          (Stats.running_period stats);
      let exec = Stats.start_exec (List.length previous_path) in
      let s_print_pc = Stats.start_step "print path constraints" in
      if Global.options.debug then Message.debug "";
      if Global.options.debug then
        Message.debug "Trying new path constraints:@ @[<v>%a@]"
          PathConstraint.Print.annotated_path previous_path;
      let exec = Stats.stop_step s_print_pc |> Stats.add_exec_step exec in
      let s_extract_constraints =
        Stats.start_step "extract solver constraints"
      in
      let solver_constraints = constraint_list_of_path ctx previous_path in
      let solver_constraints = soft_constraints @ solver_constraints in
      let exec =
        Stats.stop_step s_extract_constraints |> Stats.add_exec_step exec
      in

      let s_solve = Stats.start_step "solve" in
      let solver_result = Solver.solve ctx solver_constraints in
      let exec = Stats.stop_step s_solve |> Stats.add_exec_step exec in

      match solver_result with
      | Solver.Sat (Some m) ->
        if Global.options.debug then Message.debug "Solver returned a model";
        if Global.options.debug then
          Message.debug "model:\n%s" (Solver.string_of_model m);

        let s_inputs = Stats.start_step "get inputs from model" in
        let inputs = Solver.inputs_of_model ctx m input_marks in

        if not Global.options.debug then Message.result "";
        if Optimizations.tests_vs_time optims then
          Message.result "time of test: %a" Stats.Print.period
            (Stats.running_period stats);
        Message.result "Evaluating with inputs:";
        let inputs_list =
          List.map
            (fun (fld, e) -> StructField.get_info fld, Expr.unbox e)
            (StructField.Map.bindings inputs)
        in
        print_fields ". " inputs_list;

        let exec = Stats.stop_step s_inputs |> Stats.add_exec_step exec in

        let s_eval = Stats.start_step "eval" in
        let res = eval_conc_with_input ctx p.lang s_in scope_e mark_e inputs in
        let exec = Stats.stop_step s_eval |> Stats.add_exec_step exec in

        Message.result "Output of scope after evaluation:";

        begin match Mark.remove res with
        | EStruct { fields; _ } ->
          let outputs_list =
            List.map
              (fun (fld, e) -> StructField.get_info fld, e)
              (StructField.Map.bindings fields)
          in
          print_fields ". " outputs_list
        | EGenericError ->
          (* TODO better error messages *)
          (* TODO test the different cases *)
          Message.result "Found error %a at %s" SymbExpr.formatter
            (get_symb_expr_r res)
            (Pos.to_string_short (Expr.pos res))
        | _ ->
          Message.error ~pos:(Expr.pos scope_e)
            "The concolic interpretation of a program should always yield a \
             struct corresponding to the scope variables"
        end;
        incr total_tests;

        let incomplete =
          List.exists PathConstraint.is_incomplete (get_constraints_r res)
        in

        if incomplete then begin
          found_incomplete := true;
          Message.warning
            "Concolic evaluation found an expression that cannot be encoded (a \
             list or a date). The current\n\
            \                           path will be dropped.";
          match previous_path with
          | [] ->
            Message.result
              "Incomplete execution finished with no more constraints.";
            (* add empty steps for stats *)
            let exec =
              Stats.start_step "choose new path constraints"
              |> Stats.stop_step
              |> Stats.add_exec_step exec
            in
            let exec =
              Stats.start_step "apply diff"
              |> Stats.stop_step
              |> Stats.add_exec_step exec
            in
            let stats = Stats.stop_exec exec |> Stats.add_stat_exec stats in
            stats
          | apc :: new_path_constraints ->
            let s_new_pc = Stats.start_step "choose new path constraints" in
            let new_expected_path, diff =
              PathConstraint.make_expected_path new_path_constraints
            in

            let exec = Stats.stop_step s_new_pc |> Stats.add_exec_step exec in

            let s_diff = Stats.start_step "apply diff" in
            let diff = PathConstraint.IncrPop apc :: diff in
            apply_diff ctx Solver.push Solver.pop diff;
            let exec = Stats.stop_step s_diff |> Stats.add_exec_step exec in

            let stats = Stats.stop_exec exec |> Stats.add_stat_exec stats in
            if new_expected_path = [] then stats
            else concolic_loop new_expected_path stats
        end
        else
          let s_new_pc = Stats.start_step "choose new path constraints" in
          let res_path_constraints = get_constraints_r res in

          let res_path_constraints =
            Optimizations.remove_trivial_constraints optims res_path_constraints
          in

          if Global.options.debug then
            Message.debug "Path constraints after evaluation:@.@[<v>%a@]"
              PathConstraint.Print.naked_path res_path_constraints;

          (* TODO find a better way than all those revs *)
          let new_path_constraints, diff_compare =
            PathConstraint.compare_paths (List.rev previous_path)
              (List.rev res_path_constraints)
          in
          let new_path_constraints, diff_expected =
            PathConstraint.make_expected_path (List.rev new_path_constraints)
          in

          let exec = Stats.stop_step s_new_pc |> Stats.add_exec_step exec in
          let s_diff = Stats.start_step "apply diff" in

          let diff = diff_compare @ diff_expected in
          apply_diff ctx Solver.push Solver.pop diff;

          let exec = Stats.stop_step s_diff |> Stats.add_exec_step exec in
          let stats = Stats.stop_exec exec |> Stats.add_stat_exec stats in
          if new_path_constraints = [] then stats
          else concolic_loop new_path_constraints stats
      | Solver.Unsat -> begin
        if Global.options.debug then Message.debug "Solver returned Unsat";
        match previous_path with
        | [] -> failwith "[CONC] Failed to solve without constraints"
        | apc :: new_path_constraints ->
          (* add empty steps for stats *)
          let exec =
            Stats.start_step "get inputs from model"
            |> Stats.stop_step
            |> Stats.add_exec_step exec
          in
          let exec =
            Stats.start_step "eval"
            |> Stats.stop_step
            |> Stats.add_exec_step exec
          in
          let s_new_pc = Stats.start_step "choose new path constraints" in
          let new_expected_path, diff =
            PathConstraint.make_expected_path new_path_constraints
          in

          let exec = Stats.stop_step s_new_pc |> Stats.add_exec_step exec in
          let s_diff = Stats.start_step "apply diff" in
          let diff = PathConstraint.IncrPop apc :: diff in
          apply_diff ctx Solver.push Solver.pop diff;
          let exec = Stats.stop_step s_diff |> Stats.add_exec_step exec in
          let stats = Stats.stop_exec exec |> Stats.add_stat_exec stats in
          if new_expected_path = [] then stats
          else concolic_loop new_expected_path stats
      end
      | Solver.Sat None ->
        failwith "[CONC] Constraints satisfiable but no model was produced"
      | Solver.Unknown info ->
        Message.error ~internal:true
          "[CONC] Unknown solver result, debug info:@.%a@."
          Solver.fmt_unknown_info info
    in

    let s_loop = Stats.start_step "total loop time" in
    let stats = concolic_loop [] stats in
    let stats = Stats.stop_step s_loop |> Stats.add_stat_step stats in
    Message.result "";

    Message.result "Concolic interpreter done";
    if !found_incomplete then
      Message.warning
        "Please note that the concolic execution may be incomplete: it could \
         systematically explore the whole program.";

    let stats = Stats.stop stats in
    if print_stats then
      Message.result
        "=== Concolic execution statistics ===\n%a\n%d tests\n%s%s======"
        Stats.print stats !total_tests
        (* FIXME ugly *)
        (if Optimizations.soft_constraints optims then
           "Soft constraints:\n"
           ^ "  "
           ^ string_of_int !Solver.num_unsat
           ^ " hard unsat\n"
           ^ "  "
           ^ string_of_int !Solver.num_soft_unsat
           ^ " soft unsat\n"
           ^ "  "
           ^ string_of_int !Solver.num_soft_sat
           ^ " soft sat\n"
           ^ "  "
           ^ Solver.print_soft_sats ()
           ^ "\n"
           ^ "  "
           ^ Solver.print_soft_unsats ()
           ^ "\n"
         else "")
        (if Optimizations.mutation optims then
           "Mutations:\n"
           ^ "  out of "
           ^ string_of_int ast_stats.defaults
           ^ " defaults"
           ^ " ("
           ^ string_of_int ast_stats.defaults_with_excepts
           ^ " non-empty)"
           ^ " and "
           ^ string_of_int (List.fold_left ( + ) 0 ast_stats.excepts_sizes)
           ^ " excepts...\n"
           ^ "  "
           ^ string_of_int !Mutation.remove_excepts_n
           ^ " excepts removed\n"
           ^ "  "
           ^ string_of_int !Mutation.duplicate_excepts_n
           ^ " excepts duplicated\n"
           ^ "  "
           ^ string_of_int !Mutation.negate_justs_n
           ^ " justs negated\n"
         else "");
    (* XXX BROKEN output *)
    []
  end
  | _ ->
    Message.error ~pos:(Expr.pos scope_e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"
