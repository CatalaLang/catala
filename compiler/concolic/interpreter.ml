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

type s_expr = Z3.Expr.expr
type path_constraint = { expr : s_expr; pos : Pos.t; branch : bool }

type annotated_path_constraint =
  | Negated of path_constraint
      (** the path constraint that has been negated to generate a new input *)
  | Done of path_constraint
      (** a path node that has been explored should, and whose constraint should
          not be negated *)
  | Normal of path_constraint  (** all other constraints *)

type _conc_info = {
  symb_expr : s_expr option;
  constraints : path_constraint list;
  ty : typ option;
}

type conc_info = _conc_info custom

type 'c conc_expr = ((yes, no, 'c) interpr_kind, conc_info) gexpr
(** A concolic expression is a concrete DCalc expression that carries its
    symbolic representation and the constraints necessary to compute it. Upon
    initialization, [symb_expr] is [None], except for inputs whose [symb_expr]
    is a Z3 symbol constant. Then [symb_expr] is set by evaluation, except for
    inputs which stay the same *)

type 'c conc_naked_expr = ((yes, no, 'c) interpr_kind, conc_info) naked_gexpr
type 'c conc_boxed_expr = ((yes, no, 'c) interpr_kind, conc_info) boxed_gexpr

let make_path_constraint (expr : s_expr) (pos : Pos.t) (branch : bool) :
    path_constraint =
  { expr; pos; branch }

let set_conc_info
    (type m)
    (symb_expr : s_expr option)
    (constraints : path_constraint list)
    (mk : m mark) : conc_info mark =
  let symb_expr = Option.map (fun z -> Z3.Expr.simplify z None) symb_expr in
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
    (symb_expr : s_expr option)
    ?(constraints : path_constraint list option)
    ?(ty : typ option)
    (x : 'a) : ('a, conc_info) marked =
  let (Custom { pos; custom }) = former_mark in
  let symb_expr = Option.map (fun z -> Z3.Expr.simplify z None) symb_expr in
  let symb_expr =
    Option.fold ~none:symb_expr ~some:Option.some custom.symb_expr
  in
  (* only update symb_expr if it does not exist already *)
  let constraints = Option.value ~default:custom.constraints constraints in
  (* only change constraints if new ones are provided *)
  let ty = Option.fold ~none:custom.ty ~some:Option.some ty in
  Mark.add (Custom { pos; custom = { symb_expr; constraints; ty } }) x

(** Maybe replace the constraints, and safely replace the symbolic expression
    from former expression *)
let add_conc_info_e
    (symb_expr : s_expr option)
    ?(constraints : path_constraint list option)
    (x : ('a, conc_info) marked) : ('a, conc_info) marked =
  match constraints with
  | None -> add_conc_info_m (Mark.get x) symb_expr (Mark.remove x)
  | Some constraints ->
    add_conc_info_m (Mark.get x) symb_expr ~constraints (Mark.remove x)

(** Transform any DCalc expression into a concolic expression with no symbolic
    expression and no constraints *)
let init_conc_expr (e : ((yes, no, 'c) interpr_kind, 'm) gexpr) : 'c conc_expr =
  let f = set_conc_info None [] in
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
  (* XXX ctx_funcdecl : (typed expr, FuncDecl.func_decl) Var.Map.t; *)
  (* A map from Catala function names (represented as variables) to Z3 function
     declarations, used to only define once functions in Z3 queries *)
  (* XXX ctx_z3vars : (typed expr Var.t * typ) StringMap.t; *)
  (* A map from strings, corresponding to Z3 symbol names, to the Catala
     variable they represent. Used when to pretty-print Z3 models when a
     counterexample is generated *)
  ctx_z3enums : Z3.Sort.sort EnumName.Map.t;
  (* A map from Catala enumeration names to the corresponding Z3 datatype sort,
     from which we can retrieve constructors and accessors *)
  (* XXX ctx_z3matchsubsts : (typed expr, Expr.expr) Var.Map.t; *)
  (* A map from Catala temporary variables, generated when translating a match,
     to the corresponding enum accessor call as a Z3 expression *)
  ctx_z3structs : Z3.Sort.sort StructName.Map.t;
  (* A map from Catala struct names to the corresponding Z3 sort, from which we
     can retrieve the constructor and the accessors *)
  ctx_z3unit : Z3.Sort.sort * Z3.Expr.expr;
      (* A pair containing the Z3 encodings of the unit type, encoded as a tuple
         of 0 elements, and the unit value *)

      (* XXX ctx_z3constraints : Expr.expr list; *)
      (* A list of constraints about the created Z3 expressions accumulated
         during their initialization, for instance, that the length of an array
         is an integer which always is greater than 0 *)
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

(** [translate_typ_lit] returns the Z3 sort corresponding to the Catala literal
    type [t] **)
let translate_typ_lit (ctx : context) (t : typ_lit) : Z3.Sort.sort =
  match t with
  | TBool -> Z3.Boolean.mk_sort ctx.ctx_z3
  | TUnit -> fst ctx.ctx_z3unit
  | TInt -> Z3.Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TRat -> Z3.Arithmetic.Real.mk_sort ctx.ctx_z3
  | TMoney -> Z3.Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TDate -> failwith "TDate not implemented"
  | TDuration -> failwith "TDuration not implemented"

(** [translate_typ] returns the Z3 sort correponding to the Catala type [t] **)
let rec translate_typ (ctx : context) (t : naked_typ) : context * Z3.Sort.sort =
  match t with
  | TLit t -> ctx, translate_typ_lit ctx t
  | TStruct name ->
    find_or_create_struct ctx name
    (* DONE CONC REU are declarations sorted in topological order? "Yes" *)
    (* TODO CONC use [type_ordering] from Driver to make sure =>> actually it
       does not work because the input struct for scope [A], called [A_in], is
       not a part of this order *)
  | TTuple _ -> failwith "[translate_typ] TTuple not implemented"
  | TEnum name -> find_or_create_enum ctx name
  | TOption _ -> failwith "[translate_typ] TOption not implemented"
  | TArrow _ ->
    ctx, ctx.ctx_dummy_sort (* TODO CONC check whether this is for an input *)
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
  (* TODO add things when needed *)
  ctx

(* loosely taken from z3backend, could be exposed instead? not necessarily,
   especially if they become plugins *)
let symb_of_lit ctx (l : lit) : s_expr =
  let z3_int_of_bigint (n : Z.t) : s_expr =
    (* NOTE CONC I use string instead of int to translate without overflows, as
       both [Runtime.integer] and Z3 integers are big *)
    Z3.Arithmetic.Integer.mk_numeral_s ctx.ctx_z3 (Runtime.integer_to_string n)
  in
  match l with
  | LBool b -> Z3.Boolean.mk_val ctx.ctx_z3 b
  | LInt n -> z3_int_of_bigint n
  | LRat r ->
    Z3.Arithmetic.Real.mk_numeral_nd ctx.ctx_z3 (Z.to_int r.num)
      (Z.to_int r.den)
    (* assumption: numerator and denominator are integers *)
  | LMoney m ->
    let cents = Runtime.money_to_cents m in
    z3_int_of_bigint cents
  | LUnit -> snd ctx.ctx_z3unit
  | LDate _ -> failwith "LDate not implemented"
  | LDuration _ -> failwith "LDuration not implemented"

(** Get the symbolic expression corresponding to concolic expression [e] *)
let get_symb_expr ?(dummy : s_expr option) (e : 'c conc_expr) : s_expr option =
  let (Custom { custom; _ }) = Mark.get e in
  (* If the expression is a lambda abstraction, then give it a dummy value
   * TODO CONC this is very ugly and is just a botched fix for lambda-abs
   * appearing in structs *)
  (* TODO test *)
  match custom.ty with Some (TArrow _, _) -> dummy | _ -> custom.symb_expr

let get_constraints (e : 'c conc_expr) : path_constraint list =
  let (Custom { custom; _ }) = Mark.get e in
  custom.constraints

let gather_constraints (es : 'c conc_expr list) =
  let constraints = List.map get_constraints es in
  Message.emit_debug "gather_constraints is concatenating constraints";
  List.flatten constraints

let make_z3_struct ctx (name : StructName.t) (es : s_expr list) : s_expr =
  let sort = StructName.Map.find name ctx.ctx_z3structs in
  let constructor = List.hd (Z3.Datatype.get_constructors sort) in
  Z3.Expr.mk_app ctx.ctx_z3 constructor es

(* taken from z3backend *)
let make_z3_struct_access
    ctx
    (name : StructName.t)
    (field : StructField.t)
    (s : s_expr) : s_expr =
  let sort = StructName.Map.find name ctx.ctx_z3structs in
  let fields = StructName.Map.find name ctx.ctx_decl.ctx_structs in
  let z3_accessors = List.hd (Z3.Datatype.get_accessors sort) in
  Message.emit_debug "accessors %s"
    (List.fold_left
       (fun acc a -> Z3.FuncDecl.to_string a ^ "," ^ acc)
       "" z3_accessors);
  let idx_mappings = List.combine (StructField.Map.keys fields) z3_accessors in
  let _, z3_accessor =
    List.find (fun (field1, _) -> StructField.equal field field1) idx_mappings
  in
  Z3.Expr.mk_app ctx.ctx_z3 z3_accessor [s]

let make_z3_enum_inj
    ctx
    (name : EnumName.t)
    (s : s_expr)
    (cons : EnumConstructor.t) =
  let sort = EnumName.Map.find name ctx.ctx_z3enums in
  let constructors = EnumName.Map.find name ctx.ctx_decl.ctx_enums in
  let z3_constructors = Z3.Datatype.get_constructors sort in
  Message.emit_debug "constructors %s"
    (List.fold_left
       (fun acc a -> Z3.FuncDecl.to_string a ^ "," ^ acc)
       "" z3_constructors);
  let idx_mappings =
    List.combine (EnumConstructor.Map.keys constructors) z3_constructors
  in
  let _, z3_constructor =
    List.find (fun (cons1, _) -> EnumConstructor.equal cons cons1) idx_mappings
  in
  Z3.Expr.mk_app ctx.ctx_z3 z3_constructor [s]

let make_vars_args_map
    (vars : 'c conc_naked_expr Bindlib.var array)
    (args : 'c conc_expr list) : ('c conc_expr, 'c conc_expr) Var.Map.t =
  let zipped = Array.combine vars (Array.of_list args) in
  Array.fold_left (fun acc (v, a) -> Var.Map.add v a acc) Var.Map.empty zipped

let replace_EVar_mark
    (vars_args : ('c conc_expr, 'c conc_expr) Var.Map.t)
    (e : 'c conc_expr) : 'c conc_expr =
  match Mark.remove e with
  | EVar v -> (
    match Var.Map.find_opt v vars_args with
    | Some arg ->
      let symb_expr = get_symb_expr arg in
      Message.emit_debug "EApp>binder put mark %s on var %a"
        (Option.fold ~none:"None" ~some:Z3.Expr.to_string symb_expr)
        (Print.expr ()) e;
      add_conc_info_e symb_expr ~constraints:[] e
    (* NOTE CONC we keep the position from the var, as in concrete
       interpreter *)
    | None -> e)
  | _ -> e

let handle_eq evaluate_operator pos lang e1 e2 =
  let open Runtime.Oper in
  match e1, e2 with
  | ELit LUnit, ELit LUnit -> true
  | ELit (LBool b1), ELit (LBool b2) -> not (o_xor b1 b2)
  | ELit (LInt x1), ELit (LInt x2) -> o_eq_int_int x1 x2
  | ELit (LRat x1), ELit (LRat x2) -> o_eq_rat_rat x1 x2
  | ELit (LMoney x1), ELit (LMoney x2) -> o_eq_mon_mon x1 x2
  | ELit (LDuration _), ELit (LDuration _) ->
    failwith "EOp Eq LDuration not implemented"
  | ELit (LDate _), ELit (LDate _) -> failwith "EOp Eq LDate not implemented"
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
    (concrete_f : 'x -> 'c conc_naked_expr)
    (symbolic_f : Z3.context -> s_expr -> s_expr)
    x
    e : 'c conc_expr =
  let concrete = concrete_f x in
  let e = Option.get (get_symb_expr e) in
  (* TODO handle errors *)
  let symb_expr = symbolic_f ctx.ctx_z3 e in
  add_conc_info_m m (Some symb_expr) ~constraints:[] concrete

let op2list
    ctx
    m
    (concrete_f : 'x -> 'y -> 'c conc_naked_expr)
    (symbolic_f : Z3.context -> s_expr list -> s_expr)
    x
    y
    e1
    e2 : 'c conc_expr =
  let concrete = concrete_f x y in
  let e1 = Option.get (get_symb_expr e1) in
  let e2 = Option.get (get_symb_expr e2) in
  (* TODO handle errors *)
  let symb_expr = symbolic_f ctx.ctx_z3 [e1; e2] in
  add_conc_info_m m (Some symb_expr) ~constraints:[] concrete

let op2
    ctx
    m
    (concrete_f : 'x -> 'y -> 'c conc_naked_expr)
    (symbolic_f : Z3.context -> s_expr -> s_expr -> s_expr)
    x
    y
    e1
    e2 : 'c conc_expr =
  let concrete = concrete_f x y in
  let e1 = Option.get (get_symb_expr e1) in
  let e2 = Option.get (get_symb_expr e2) in
  (* TODO handle errors *)
  let symb_expr = symbolic_f ctx.ctx_z3 e1 e2 in
  add_conc_info_m m (Some symb_expr) ~constraints:[] concrete

(* Reproduce the behaviour of [Q.to_bigint] rounding rational [q] to an integer
   towards 0 (ie 0.8 and -0.8 are rounded to 0)
 * TODO CONC rounding will change once PR #557 is merged *)
(* TODO add proper test like for [z3_real2int_nearest] *)
(* TODO CONC why not define Z3 function? =>> they are the same as declaration +
   forall, and the gain in legibility is marginal, so I don't think it is
   necessary *)
let z3_real2int_towards_zero ctx (q : s_expr) : s_expr =
  let zero = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 in
  let is_positive = Z3.Arithmetic.mk_ge ctx q zero in
  let round_pos = Z3.Arithmetic.Real.mk_real2int ctx q in
  let round_neg =
    Z3.Arithmetic.mk_unary_minus ctx
      (Z3.Arithmetic.Real.mk_real2int ctx (Z3.Arithmetic.mk_unary_minus ctx q))
  in
  Z3.Boolean.mk_ite ctx is_positive round_pos round_neg

(* Reproduce the behaviour of [o_mult_mon_rat] rational [q] to the nearest
   integer (away from 0). 1/2 rounds to 1, and -1/2 rounds to -1.
 * NOTE CONC see above *)
let z3_real2int_nearest ctx (q : s_expr) : s_expr =
  let zero = Z3.Arithmetic.Integer.mk_numeral_i ctx 0 in
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
    args =
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
      (EApp { f = EOp { op; tys = [] }, m; args }, m)
  in
  Concrete.propagate_empty_error_list args
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
    (* TODO catch errors here, or maybe propagate [None]? *)
    let s_e1 = Option.get (get_symb_expr e1) in
    let s_e2 = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Boolean.mk_eq ctx.ctx_z3 s_e1 s_e2 in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
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
  | Minus_dur, _ -> failwith "Eop Minus_dur not implemented"
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
  | ToMoney_rat, [((ELit (LRat i), _) as e)] ->
    (* TODO CONC warning here: [o_tomoney_rat] rounds to the cent
     *   closest to 0, while [mk_real2int] rounds to the lower cent.
     * =>> thus I implement OCaml's rounding in Z3 in function [z3_real2int_towards_zero]
     * TODO be careful as well with [Round_mon], [Round_rat], [Mult_mon_rat],
     * [Div_mon_rat], because there are several different rounding methods *)
    op1 ctx m
      (fun x -> ELit (LMoney (o_tomoney_rat x)))
      (fun ctx e ->
        let cents =
          Z3.Arithmetic.mk_mul ctx [e; Z3.Arithmetic.Real.mk_numeral_i ctx 100]
        in
        z3_real2int_towards_zero ctx cents)
      i e
  | Round_mon, _ -> failwith "Eop Round_mon not implemented"
  (* TODO with careful rounding *)
  | Round_rat, _ -> failwith "Eop Round_rat not implemented"
  (* TODO with careful rounding *)
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
  | Add_dat_dur _, _ -> failwith "Eop Add_dat_dur not implemented"
  | Add_dur_dur, _ -> failwith "Eop Add_dur_dur not implemented"
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
  | Sub_dat_dat, _ -> failwith "Eop Sub_dat_dat not implemented"
  | Sub_dat_dur, _ -> failwith "Eop Sub_dat_dur not implemented"
  | Sub_dur_dur, _ -> failwith "Eop Sub_dur_dur not implemented"
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
        z3_real2int_nearest ctx product)
      x y e1 e2
  (* TODO with careful rounding *)
  | Mult_dur_int, _ -> failwith "Eop Mult_dur_int not implemented"
  | Div_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LRat (protect o_div_int_int x y)))
      Z3.Arithmetic.mk_div x y e1 e2
  (* TODO test this specifically? *)
  | Div_rat_rat, [((ELit (LRat x), _) as e1); ((ELit (LRat y), _) as e2)] ->
    op2 ctx m
      (fun x y -> ELit (LRat (protect o_div_rat_rat x y)))
      Z3.Arithmetic.mk_div x y e1 e2
  | Div_mon_mon, _ -> failwith "Eop Div_mon_mon not implemented"
  | Div_mon_rat, _ -> failwith "Eop Div_mon_rat not implemented"
  (* TODO with careful rounding *)
  | Div_dur_dur, _ -> failwith "Eop Div_dur_dur not implemented"
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
  | Lt_dat_dat, _ -> failwith "Eop Lt_dat_dat not implemented"
  | Lt_dur_dur, _ -> failwith "Eop Lt_dur_dur not implemented"
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
  | Lte_dat_dat, _ -> failwith "Eop Lte_dat_dat not implemented"
  | Lte_dur_dur, _ -> failwith "Eop Lte_dur_dur not implemented"
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
  | Gt_dat_dat, _ -> failwith "Eop Gt_dat_dat not implemented"
  | Gt_dur_dur, _ -> failwith "Eop Gt_dur_dur not implemented"
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
  | Gte_dat_dat, _ -> failwith "Eop Gte_dat_dat not implemented"
  | Gte_dur_dur, _ -> failwith "Eop Gte_dur_dur not implemented"
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
  | Eq_dat_dat, _ -> failwith "Eop Eq_dat_dat not implemented"
  | Eq_dur_dur, _ -> failwith "Eop Eq_dur_dur not implemented"
  | HandleDefault, _ ->
    (* TODO change error message *)
    Message.raise_internal_error
      "The concolic interpreter is trying to evaluate the \"handle_default\" \
       operator, which should not happen with a DCalc AST"
  | HandleDefaultOpt, _ ->
    Message.raise_internal_error
      "The concolic interpreter is trying to evaluate the \
       \"handle_default_opt\" operator, which should not happen with a DCalc \
       AST"
  | ( ( Minus_int | Minus_rat | Minus_mon (*| Minus_dur *)
      | ToRat_int | ToRat_mon | ToMoney_rat (* | Round_rat | Round_mon *)
      | Add_int_int | Add_rat_rat
      | Add_mon_mon (* | Add_dat_dur _ | Add_dur_dur *)
      | Sub_int_int | Sub_rat_rat
      | Sub_mon_mon (* | Sub_dat_dat | Sub_dat_dur | Sub_dur_dur *)
      | Mult_int_int | Mult_rat_rat | Mult_mon_rat (* | Mult_dur_int *)
      | Div_int_int
      | Div_rat_rat (* | Div_mon_mon | Div_mon_rat | Div_dur_dur *)
      | Lt_int_int | Lt_rat_rat | Lt_mon_mon (* | Lt_dat_dat | Lt_dur_dur *)
      | Lte_int_int | Lte_rat_rat
      | Lte_mon_mon (* | Lte_dat_dat | Lte_dur_dur *)
      | Gt_int_int | Gt_rat_rat | Gt_mon_mon (* | Gt_dat_dat | Gt_dur_dur *)
      | Gte_int_int | Gte_rat_rat
      | Gte_mon_mon (* | Gte_dat_dat | Gte_dur_dur *)
      | Eq_int_int | Eq_rat_rat | Eq_mon_mon (* | Eq_dat_dat | Eq_dur_dur *) ),
      _ ) ->
    err ()

let rec evaluate_expr :
    context -> Cli.backend_lang -> yes conc_expr -> yes conc_expr =
 fun ctx lang e ->
  (* Message.emit_debug "eval %a" (Print.expr ()) e; *)
  let m = Mark.get e in
  let pos = Expr.mark_pos m in
  match Mark.remove e with
  | EVar _ ->
    Message.raise_spanned_error pos
      "free variable found at evaluation (should not happen if term was \
       well-typed)"
  | EExternal _ -> failwith "EExternal not implemented"
  | EApp { f = e1; args } -> (
    Message.emit_debug "... it's an EApp";
    let e1 = evaluate_expr ctx lang e1 in
    Message.emit_debug "EApp f evaluated";
    let f_constraints = get_constraints e1 in
    let args = List.map (evaluate_expr ctx lang) args in
    let args_constraints = gather_constraints args in
    Message.emit_debug "EApp args evaluated";
    Concrete.propagate_empty_error e1
    @@ fun e1 ->
    match Mark.remove e1 with
    | EAbs { binder; _ } ->
      (* TODO make this discussion into a doc? should constraints from the args
         be added, or should we trust the call to return them? We could take
         both for safety but there will be duplication... I think we should
         trust the recursive call, and we'll see if it's better not to =>>
         actually it's better not to : it can lead to duplication, and the
         subexpression does not need the constraints anyway =>> see the big
         concatenation below *)
      (* The arguments passed to [Bindlib.msubst] are unmarked. To circumvent
         this, I change the corresponding marks in the receiving expression, ie
         the expression in which substitution happens: 1/ unbind 2/ change marks
         3/ rebind 4/ substitute concrete expressions normally *)
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
            Message.emit_debug "EApp>EAbs arg %a | %s | %i" (Print.expr ()) arg
              (Option.fold ~none:"None" ~some:Z3.Expr.to_string
                 (get_symb_expr arg))
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
        let r_symb = get_symb_expr result in
        Message.emit_debug "EApp>EAbs extracted symbolic expressions";
        let r_constraints = get_constraints result in
        (* the constraints generated by the evaluation of the application are:
         * - those generated by the evaluation of the function
         * - those generated by the evaluation of the arguments
         * - the NEW ones generated by the evaluation of the subexpression
         *   (where the ones of the arguments are neither passed down, nor
         *   re-generated as this is cbv)
         *)
        let constraints = r_constraints @ args_constraints @ f_constraints in
        add_conc_info_e r_symb ~constraints result
        (* NOTE that here the position comes from [result], while in other cases
           of this function the position comes from the input expression. This
           is the behaviour of the concrete interpreter *))
      else
        Message.raise_spanned_error pos
          "wrong function call, expected %d arguments, got %d"
          (Bindlib.mbinder_arity binder)
          (List.length args)
    | EOp { op; _ } ->
      let result =
        evaluate_operator (evaluate_expr ctx lang) ctx op m lang args
      in
      let r_symb = get_symb_expr result in
      (* the constraints generated by the evaluation of the application are:
       * - those generated by the evaluation of the operator
       * - those generated by the evaluation of the arguments
       * NB: [evaluate_operator] is cbv so we don't care what constraints it returns *)
      let constraints = args_constraints @ f_constraints in
      add_conc_info_e r_symb ~constraints result
    | ECustom _ -> failwith "EApp of ECustom not implemented"
    | _ ->
      Message.raise_spanned_error pos
        "function has not been reduced to a lambda at evaluation (should not \
         happen if the term was well-typed")
  | EAbs { binder; tys } ->
    Message.emit_debug "... it's an EAbs";
    Expr.unbox (Expr.eabs (Bindlib.box binder) tys m)
    (* TODO simplify this once issue #540 is resolved *)
  | ELit l as e ->
    Message.emit_debug "... it's an ELit";
    let symb_expr = symb_of_lit ctx l in
    (* no constraints generated *)
    add_conc_info_m m (Some symb_expr) ~constraints:[] e
  | EOp { op; tys } -> Expr.unbox (Expr.eop (Operator.translate op) tys m)
  (* | EAbs _ as e -> Marked.mark m e (* these are values *) *)
  | EStruct { fields = es; name } ->
    Message.emit_debug "... it's an EStruct";
    let fields, es = List.split (StructField.Map.bindings es) in

    (* compute all subexpressions *)
    let es = List.map (evaluate_expr ctx lang) es in

    (* make symbolic expression using the symbolic sub-expressions *)
    (* TODO CONC here we activate the dummy value for lambda abstractions, it is
       not used but at least the [Option.get] does not crash *)
    let symb_exprs = List.map (get_symb_expr ~dummy:ctx.ctx_dummy_const) es in
    Message.emit_debug "EStruct extracted symbolic expressions";
    let symb_exprs = List.map Option.get symb_exprs in
    Message.emit_debug "EStruct got symbolic expressions";
    (* TODO catch error... should not happen *)
    let symb_expr = make_z3_struct ctx name symb_exprs in

    (* gather all constraints from sub-expressions *)
    let constraints = gather_constraints es in

    Concrete.propagate_empty_error_list es
    @@ fun es ->
    add_conc_info_m m (Some symb_expr) ~constraints
      (EStruct
         {
           fields =
             StructField.Map.of_seq
               (Seq.zip (List.to_seq fields) (List.to_seq es));
           name;
         })
  | EStructAccess { e; name = s; field } -> (
    Message.emit_debug "... it's an EStructAccess";
    Concrete.propagate_empty_error (evaluate_expr ctx lang e)
    @@ fun e ->
    match Mark.remove e with
    | EStruct { fields = es; name } ->
      if not (StructName.equal s name) then
        Message.raise_multispanned_error
          [None, pos; None, Expr.pos e]
          "Error during struct access: not the same structs (should not happen \
           if the term was well-typed)";
      let field_expr =
        match StructField.Map.find_opt field es with
        | Some e' -> e'
        | None ->
          Message.raise_spanned_error (Expr.pos e)
            "Invalid field access %a in struct %a (should not happen if the \
             term was well-typed)"
            StructField.format field StructName.format s
      in
      let symb_expr =
        make_z3_struct_access ctx s field (Option.get (get_symb_expr e))
        (* TODO catch error... should not happen *)
      in
      Message.emit_debug "EStructAccess symbolic struct access created";
      (* the constraints generated by struct access are only those generated by
         the subcall, as the field expression is already a value *)
      let constraints = get_constraints e in
      add_conc_info_m m (Some symb_expr) ~constraints (Mark.remove field_expr)
    | _ ->
      Message.raise_spanned_error (Expr.pos e)
        "The expression %a should be a struct %a but is not (should not happen \
         if the term was well-typed)"
        (Print.UserFacing.expr lang)
        e StructName.format s)
  | ETuple _ -> failwith "ETuple not implemented"
  | ETupleAccess _ -> failwith "ETupleAccess not implemented"
  | EInj { name; e; cons } ->
    Message.emit_debug "... it's an EInj";
    Concrete.propagate_empty_error (evaluate_expr ctx lang e)
    @@ fun e ->
    let concrete = EInj { name; e; cons } in

    let e_symb =
      Option.get (get_symb_expr e)
      (* TODO catch error... should not happen *)
    in
    let symb_expr = make_z3_enum_inj ctx name e_symb cons in
    let constraints = get_constraints e in

    add_conc_info_m m (Some symb_expr) ~constraints concrete
  | EMatch _ -> failwith "EMatch not implemented"
  | EIfThenElse { cond; etrue; efalse } -> (
    Message.emit_debug "... it's an EIfThenElse";
    Concrete.propagate_empty_error (evaluate_expr ctx lang cond)
    @@ fun cond ->
    let c_symb_opt = get_symb_expr cond in
    let c_symb = Option.get c_symb_opt in
    (* TODO catch error... should not happen *)
    let c_constraints = get_constraints cond in
    match Mark.remove cond with
    | ELit (LBool true) ->
      Message.emit_debug "EIfThenElse>true adding %s to constraints"
        (Z3.Expr.to_string c_symb);
      let etrue = evaluate_expr ctx lang etrue in
      let e_symb = get_symb_expr etrue in
      let e_constraints = get_constraints etrue in
      let e_mark = Mark.get etrue in
      let e_concr = Mark.remove etrue in
      let c_symb = Z3.Expr.simplify c_symb None in
      (* TODO factorize the simplifications? *)
      let c_path_constraint =
        make_path_constraint c_symb (Expr.pos cond) true
      in
      (* the constraints generated by the ifthenelse when [cond] is true are :
       * - those generated by the evaluation of [cond]
       * - a new constraint corresponding to [cond] itself
       * - those generated by the evaluation of [etrue]
       *)
      let constraints = e_constraints @ (c_path_constraint :: c_constraints) in
      add_conc_info_m e_mark e_symb ~constraints e_concr
    | ELit (LBool false) ->
      Message.emit_debug "EIfThenElse>false adding %s to constraints"
        (Z3.Expr.to_string c_symb);
      let efalse = evaluate_expr ctx lang efalse in
      let e_symb = get_symb_expr efalse in
      let e_constraints = get_constraints efalse in
      let e_mark = Mark.get efalse in
      let e_concr = Mark.remove efalse in
      let not_c_symb = Z3.Boolean.mk_not ctx.ctx_z3 c_symb in
      let not_c_symb = Z3.Expr.simplify not_c_symb None in
      (* TODO factorize the simplifications? *)
      let not_c_path_constraint =
        make_path_constraint not_c_symb (Expr.pos cond) false
      in
      (* the constraints generated by the ifthenelse when [cond] is false are :
       * - those generated by the evaluation of [cond]
       * - a new constraint corresponding to [cond] itself
       * - those generated by the evaluation of [efalse]
       *)
      let constraints =
        e_constraints @ (not_c_path_constraint :: c_constraints)
      in
      add_conc_info_m e_mark e_symb ~constraints e_concr
    | _ ->
      Message.raise_spanned_error (Expr.pos cond)
        "Expected a boolean literal for the result of this condition (should \
         not happen if the term was well-typed)")
  | EArray _ -> failwith "EArray not implemented"
  | EAssert _ -> failwith "EAssert not implemented"
  | ECustom _ -> failwith "ECustom not implemented"
  | EEmptyError ->
    Message.emit_debug "... it's an EEmptyError";
    e
    (* TODO check that it's ok to pass along the symbolic values and
       constraints? *)
  | EErrorOnEmpty e' -> (
    Message.emit_debug "... it's an EErrorOnEmpty";
    match evaluate_expr ctx lang e' with
    | EEmptyError, _ ->
      Message.raise_spanned_error (Expr.pos e')
        "This variable evaluated to an empty term (no rule that defined it \
         applied in this situation)"
    | e -> e
    (* just pass along the concrete and symbolic values, and the constraints *))
  | EDefault { excepts; just; cons } -> (
    Message.emit_debug "... it's an EDefault";
    let excepts = List.map (evaluate_expr ctx lang) excepts in
    let exc_constraints = gather_constraints excepts in
    let empty_count =
      List.length (List.filter Concrete.is_empty_error excepts)
    in
    match List.length excepts - empty_count with
    | 0 -> (
      let just = evaluate_expr ctx lang just in
      let j_symb_opt = get_symb_expr just in
      let j_symb = Option.get j_symb_opt in
      (* TODO catch error... should not happen *)
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
        add_conc_info_m m None ~constraints EEmptyError
      | ELit (LBool true) ->
        Message.emit_debug "EDefault>true adding %s to constraints"
          (Z3.Expr.to_string j_symb);
        let cons = evaluate_expr ctx lang cons in
        let c_symb = get_symb_expr cons in
        let c_constraints = get_constraints cons in
        let c_mark = Mark.get cons in
        let c_concr = Mark.remove cons in
        let j_symb = Z3.Expr.simplify j_symb None in
        (* TODO factorize the simplifications? *)
        let j_path_constraint =
          make_path_constraint j_symb (Expr.pos just) true
        in
        (* the constraints generated by the default when [just] is true are :
         * - those generated by the evaluation of the excepts
         * - those generated by the evaluation of [just]
         * - a new constraint corresponding to [just] itself
         * - those generated by the evaluation of [cons]
         *)
        let constraints =
          c_constraints @ (j_path_constraint :: j_constraints) @ exc_constraints
        in
        add_conc_info_m c_mark c_symb ~constraints c_concr
      | ELit (LBool false) ->
        let not_j_symb = Z3.Boolean.mk_not ctx.ctx_z3 j_symb in
        let not_j_symb = Z3.Expr.simplify not_j_symb None in
        let not_j_path_constraint =
          make_path_constraint not_j_symb (Expr.pos just) false
        in
        Message.emit_debug "EDefault>false adding %s to constraints"
          (Z3.Expr.to_string not_j_symb);
        (* the constraints generated by the default when [just] is false are :
         * - those generated by the evaluation of the excepts
         * - those generated by the evaluation of [just]
         * - a new constraint corresponding to [just] itself
         *)
        let constraints =
          (not_j_path_constraint :: j_constraints) @ exc_constraints
        in
        add_conc_info_m m None ~constraints EEmptyError
      | _ ->
        Message.raise_spanned_error (Expr.pos e)
          "Default justification has not been reduced to a boolean at \
           evaluation (should not happen if the term was well-typed")
    | 1 ->
      let r =
        List.find (fun sub -> not (Concrete.is_empty_error sub)) excepts
      in
      (* the constraints generated by the default when exactly one except is raised are :
       * - those generated by the evaluation of the excepts
       *)
      let r_symb = get_symb_expr r in
      let constraints = exc_constraints in
      add_conc_info_e r_symb ~constraints r
    | _ ->
      Message.raise_multispanned_error
        (List.map
           (fun except ->
             Some "This consequence has a valid justification:", Expr.pos except)
           (List.filter (fun sub -> not (Concrete.is_empty_error sub)) excepts))
        "There is a conflict between multiple valid consequences for assigning \
         the same variable.")
  | EPureDefault e ->
    Message.emit_debug "... it's an EPureDefault";
    evaluate_expr ctx lang e
  | _ -> .

(** The following functions gather methods to generate input values for concolic
    execution, be it from a model or from hardcoded default values. *)

(** Create the mark of an input field from its name [field] and its type [ty].
    Note that this function guarantees that the type information will be present
    when calling [inputs_of_model] *)
let make_input_mark ctx m field ty : conc_info mark =
  let name = Mark.remove (StructField.get_info field) in
  let _, sort = translate_typ ctx (Mark.remove ty) in
  let symb_expr = Z3.Expr.mk_const_s ctx.ctx_z3 name sort in
  Custom
    {
      pos = Expr.mark_pos m;
      custom = { symb_expr = Some symb_expr; constraints = []; ty = Some ty };
    }

(** Create a dummy concolic mark with a position and a type. It has no symbolic
    expression or constraints, and is used for subexpressions inside of input
    structs whose mark is a single symbol. *)
let dummy_mark pos ty : conc_info mark =
  Custom { pos; custom = { symb_expr = None; constraints = []; ty = Some ty } }

(** Get a default literal value from a literal type *)
let default_lit_of_tlit t : lit =
  match t with
  | TBool -> LBool true
  | TInt -> LInt (Z.of_int 42)
  | TMoney -> LMoney (Runtime.money_of_units_int 42)
  | TUnit -> LUnit
  | TRat -> LRat (Runtime.decimal_of_string "42")
  | TDate -> failwith "TDate not implemented"
  | TDuration -> failwith "TDuration not implemented"

(** Get a default concolic expression from a type. The concolic [mark] is given
    by the caller, and this function gives a default concrete value. This
    function is expected to be called from [inputs_of_model] when Z3 has not
    given a value for an input field. *)
let rec default_expr_of_typ ctx mark ty : 'c conc_boxed_expr =
  match Mark.remove ty with
  | TLit t -> Expr.elit (default_lit_of_tlit t) mark
  | TArrow (ty_in, ty_out) ->
    (* TODO CONC REU this handling of functional inputs does not discriminate
       between context variables (for which a value can be given, and whose
       default case should be handled with care) and proper functions. For now,
       we emit a warning that incompleteness may occur, and sometimes runtime
       crashes caused by this default encoding of functions will happen as
       well. *)
    Message.emit_warning
      "An input of the scope is a context variable or a function. In that \
       case, the concolic exploration may be INCOMPLETE, and therefore miss \
       errors.";
    Expr.make_abs
      (Array.of_list @@ List.map (fun _ -> Var.make "_") ty_in)
      (Bindlib.box EEmptyError, Expr.with_ty mark ty_out)
      ty_in (Expr.mark_pos mark)
  | TTuple _ -> failwith "TTuple not implemented"
  | TStruct name ->
    (* When a field of the input structure is a struct itself, its fields will
       only be evaluated for their concrete values, as their symbolic value will
       be accessed symbolically from the symbol of the input structure field.
       Thus we can safely give a dummy mark, one with position and type for
       printing, but no symbolic expression or constraint *)
    let pos = Expr.mark_pos mark in
    let fields_typ = StructName.Map.find name ctx.ctx_decl.ctx_structs in
    let fields_expr =
      StructField.Map.map
        (fun ty -> default_expr_of_typ ctx (dummy_mark pos ty) ty)
        fields_typ
    in
    Expr.estruct ~name ~fields:fields_expr mark
  | TEnum _ -> failwith "[default_expr_of_typ] TEnum not implemented"
  | TOption _ -> failwith "[default_expr_of_typ] TOption not implemented"
  | TArray _ -> failwith "[default_expr_of_typ] TArray not implemented"
  | TDefault _ -> failwith "[default_expr_of_typ] TDefault not implemented"
  | TAny -> failwith "[default_expr_of_typ] TAny not implemented"
  | TClosureEnv -> failwith "[default_expr_of_typ] TClosureEnv not implemented"

(** Get the Z3 expression corresponding to the value of Z3 symbol constant [v]
    in Z3 model [m]. [None] if [m] has not given a value. TODO CONC check that
    the following hypothesis is correct : "get_const_interp_e only returns None
    if the symbol constant can take any value" *)
let interp_in_model (m : Z3.Model.model) (v : Z3.Expr.expr) : s_expr option =
  Z3.Model.get_const_interp_e m v

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
  | TDate -> failwith "TDate not implemented"
  | TDuration -> failwith "TDuration not implemented"

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
       struct out of those, evaluate a Z3 "accessor" to the corresponding field
       in the model *)
    let pos = Expr.mark_pos mark in
    let fields_typ = StructName.Map.find name ctx.ctx_decl.ctx_structs in
    let expr_of_fd fd ty =
      let access = make_z3_struct_access ctx name fd e in
      let ev = Option.get (Z3.Model.eval model access true) in
      value_of_symb_expr ctx model (dummy_mark pos ty) ty ev
      (* See [default_expr_of_typ] for an explanation on the dummy mark *)
    in
    let fields_expr = StructField.Map.mapi expr_of_fd fields_typ in
    Expr.estruct ~name ~fields:fields_expr mark
  | TEnum _ -> failwith "[value_of_symb_expr] TEnum not implemented"
  | TOption _ -> failwith "[value_of_symb_expr] TOption not implemented"
  | TArrow _ -> failwith "[value_of_symb_expr] TArrow not implemented"
  | TArray _ -> failwith "[value_of_symb_expr] TArray not implemented"
  | TDefault _ -> failwith "[value_of_symb_expr] TDefault not implemented"

(** Get Catala values from a Z3 model, possibly using default values *)
let inputs_of_model
    ctx
    (m : Z3.Model.model)
    (input_marks : conc_info mark StructField.Map.t) :
    'c conc_boxed_expr StructField.Map.t =
  let f _ (mk : conc_info mark) =
    let (Custom { custom; _ }) = mk in
    let ty = Option.get custom.ty in
    (* this get should not fail because [make_input_mark] always adds the type
       information *)
    let symb_expr_opt = interp_in_model m (Option.get custom.symb_expr) in
    Option.fold
      ~none:(default_expr_of_typ ctx mk ty)
      ~some:(value_of_symb_expr ctx m mk ty)
      symb_expr_opt
  in
  StructField.Map.mapi f input_marks

(** Evaluation *)

(** Do a "pre-evaluation" of the program. It compiles the target scope to a
    function that takes the input struct of the scope and returns its output
    struct *)
let simplify_program (type m) ctx (p : (dcalc, m) gexpr program) s :
    yes conc_expr =
  Message.emit_debug "[CONC] Make program expression concolic";
  let e = Expr.unbox (Program.to_expr p s) in
  let e_conc = init_conc_expr e in
  Message.emit_debug "[CONC] Pre-compute program";
  evaluate_expr ctx p.lang (Concrete.addcustom e_conc)

(** Evaluate pre-compiled scope [e] with its input struct [name] populated with
    [fields] *)
let eval_conc_with_input
    ctx
    lang
    (name : StructName.t)
    (e : yes conc_expr)
    (mark : conc_info mark)
    (fields : yes conc_boxed_expr StructField.Map.t) =
  let to_interpret =
    Expr.eapp (Expr.box e)
      (* box instead of rebox because the term is supposed to be closed *)
      [Expr.estruct ~name ~fields mark]
      (set_conc_info None [] (Mark.get e))
  in
  Message.emit_debug "...inputs applied...";
  evaluate_expr ctx lang (Expr.unbox to_interpret)

(** Computation path logic *)

(** Two path constraints are equal if their Z3 expressions are equal, and they
    are marked with the same branch information. Position is not taken into
    account as it is used only in printing and not in computations *)
let path_constraint_equal c c' : bool =
  Z3.Expr.equal c.expr c'.expr && c.branch = c'.branch

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

(** Remove marks from an annotated path, to get a list of Z3 expressions to feed
    in the solver. In doing so, actually negate constraints marked as Negated.
    This function shall be called on an output of [make_expected_path]. *)
let constraint_list_of_path ctx (path : annotated_path_constraint list) :
    s_expr list =
  let f = function
    | Normal c -> c.expr
    | Done c -> c.expr
    | Negated c -> Z3.Boolean.mk_not ctx.ctx_z3 c.expr
  in
  List.map f path

let string_of_path_constraint (pc : path_constraint) : string =
  Z3.Expr.to_string pc.expr
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

(** Z3 model solving *)

type solver_result = Sat of Z3.Model.model option | Unsat | Unknown

let solve_constraints ctx constraints : solver_result =
  let solver = Z3.Solver.mk_solver ctx.ctx_z3 None in
  Z3.Solver.add solver constraints;
  match Z3.Solver.check solver [] with
  | SATISFIABLE -> Sat (Z3.Solver.get_model solver)
  | UNSATISFIABLE -> Unsat
  | UNKNOWN -> Unknown

let print_fields language (prefix : string) fields =
  let ordered_fields =
    List.sort (fun ((v1, _), _) ((v2, _), _) -> String.compare v1 v2) fields
  in
  List.iter
    (fun ((var, _), value) ->
      Message.emit_result "%s@[<hov 2>%s@ =@ %a@]" prefix var
        (* TODO CONC UserFacing does not print the minus =>> opened #551. For
           now, I will use [Print.expr] *)
        (* (if Cli.globals.debug then Print.expr ~debug:false () else
           Print.UserFacing.value language) *)
        (Print.expr ())
        value)
    ordered_fields

(** Main function *)
let interpret_program_concolic (type m) (p : (dcalc, m) gexpr program) s :
    (Uid.MarkedString.info * yes conc_expr) list =
  Message.emit_debug "=== Start concolic interpretation... ===";
  let ctx = p.decl_ctx in
  Message.emit_debug "[CONC] Create empty context";
  let ctx = make_empty_context ctx in
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

    let rec concolic_loop (path_constraints : annotated_path_constraint list) :
        unit =
      Message.emit_debug "";
      print_annotated_path_constraints path_constraints;
      let solver_constraints = constraint_list_of_path ctx path_constraints in

      match solve_constraints ctx solver_constraints with
      | Sat (Some m) ->
        Message.emit_debug "Solver returned a model";
        Message.emit_debug "model: %s" (Z3.Model.to_string m);
        let inputs = inputs_of_model ctx m input_marks in

        if not Cli.globals.debug then Message.emit_result "";
        Message.emit_result "Evaluating with inputs:";
        let inputs_list =
          List.map
            (fun (fld, e) -> StructField.get_info fld, Expr.unbox e)
            (StructField.Map.bindings inputs)
        in
        print_fields p.lang ". " inputs_list;

        let res = eval_conc_with_input ctx p.lang s_in scope_e mark_e inputs in

        let res_path_constraints = get_constraints res in

        print_path_constraints res_path_constraints;

        Message.emit_result "Output of scope after evaluation:";
        let outputs_list =
          match Mark.remove res with
          | EStruct { fields; _ } ->
            List.map
              (fun (fld, e) -> StructField.get_info fld, e)
              (StructField.Map.bindings fields)
          | _ ->
            Message.raise_spanned_error (Expr.pos scope_e)
              "The concolic interpretation of a program should always yield a \
               struct corresponding to the scope variables"
        in
        print_fields p.lang ". " outputs_list;

        (* TODO find a better way *)
        let new_path_constraints =
          compare_paths
            (List.rev path_constraints)
            (List.rev res_path_constraints)
          |> List.rev
          |> make_expected_path
        in
        if new_path_constraints = [] then ()
        else concolic_loop new_path_constraints
      | Unsat -> begin
        Message.emit_debug "Solver returned Unsat";
        match path_constraints with
        | [] -> failwith "[CONC] Failed to solve without constraints"
        | _ :: new_path_constraints ->
          let new_expected_path = make_expected_path new_path_constraints in
          if new_expected_path = [] then () else concolic_loop new_expected_path
      end
      | Sat None ->
        failwith "[CONC] Constraints satisfiable but no model was produced"
      | Unknown -> failwith "[CONC] Unknown solver result"
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
