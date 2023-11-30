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
type _conc_info = { symb_expr : s_expr option; constraints : s_expr list }
type conc_info = _conc_info custom

type 'c conc_expr = ((yes, no, 'c) interpr_kind, conc_info) gexpr
(** A concolic expression is a concrete DCalc expression that carries its
    symbolic representation and the constraints necessary to compute it. Upon
    initialization, [symb_expr] is [None], except for inputs whose [symb_expr]
    is a Z3 symbol constant. Then [symb_expr] is set by evaluation, except for
    inputs which stay the same *)

type 'c conc_naked_expr = ((yes, no, 'c) interpr_kind, conc_info) naked_gexpr
type 'c conc_boxed_expr = ((yes, no, 'c) interpr_kind, conc_info) boxed_gexpr

let set_conc_info
    (type m)
    (symb_expr : s_expr option)
    (constraints : s_expr list)
    (mk : m mark) : conc_info mark =
  let custom = { symb_expr; constraints } in
  match mk with
  | Untyped { pos } -> Custom { pos; custom }
  | Typed { pos; _ } ->
    Custom { pos; custom }
    (* TODO CONC should we keep the type information? maybe for the symb
       exprs *)
  | Custom m -> Custom { m with custom }

(** Maby replace constraints, and safely replace the symbolic expression from
    former mark *)
let add_conc_info_m
    (former_mark : conc_info mark)
    (symb_expr : s_expr option)
    ?(constraints : s_expr list option)
    (x : 'a) : ('a, conc_info) marked =
  let (Custom { pos; custom = { symb_expr = s; constraints = c } }) =
    former_mark
  in
  let symb_expr = Option.fold ~none:symb_expr ~some:Option.some s in
  (* only update symb_expr if it does not exist already *)
  let constraints = Option.value ~default:c constraints in
  (* only change constraints if new ones are provided *)
  Mark.add (Custom { pos; custom = { symb_expr; constraints } }) x

(** Maybe replace the constraints, and safely replace the symbolic expression
    from former expression *)
let add_conc_info_e
    (symb_expr : s_expr option)
    ?(constraints : s_expr list option)
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
  (* XXX ctx_funcdecl : (typed expr, FuncDecl.func_decl) Var.Map.t; *)
  (* A map from Catala function names (represented as variables) to Z3 function
     declarations, used to only define once functions in Z3 queries *)
  (* XXX ctx_z3vars : (typed expr Var.t * typ) StringMap.t; *)
  (* A map from strings, corresponding to Z3 symbol names, to the Catala
     variable they represent. Used when to pretty-print Z3 models when a
     counterexample is generated *)
  (* XXX ctx_z3datatypes : Sort.sort EnumName.Map.t; *)
  (* A map from Catala enumeration names to the corresponding Z3 sort, from
     which we can retrieve constructors and accessors *)
  (* XXX ctx_z3matchsubsts : (typed expr, Expr.expr) Var.Map.t; *)
  (* A map from Catala temporary variables, generated when translating a match,
     to the corresponding enum accessor call as a Z3 expression *)
  ctx_z3structs : Z3.Sort.sort StructName.Map.t;
      (* A map from Catala struct names to the corresponding Z3 sort, from which
         we can retrieve the constructor and the accessors *)
      (* XXX ctx_z3unit : Sort.sort * Expr.expr; *)
      (* A pair containing the Z3 encodings of the unit type, encoded as a tuple
         of 0 elements, and the unit value *)
      (* XXX ctx_z3constraints : Expr.expr list; *)
      (* A list of constraints about the created Z3 expressions accumulated
         during their initialization, for instance, that the length of an array
         is an integer which always is greater than 0 *)
}

(** [add_z3struct] adds the mapping between the Catala struct [s] and the
    corresponding Z3 datatype [sort] to the context **)
let add_z3struct (s : StructName.t) (sort : Z3.Sort.sort) (ctx : context) :
    context =
  { ctx with ctx_z3structs = StructName.Map.add s sort ctx.ctx_z3structs }

(* let create_z3unit (z3_ctx : Z3.context) : Z3.Sort.sort * Z3.Expr.expr = let
   unit_sort = Z3.Tuple.mk_sort z3_ctx (Z3.Symbol.mk_string z3_ctx "unit") [] []
   in let mk_unit = Z3.Tuple.get_mk_decl unit_sort in let unit_val =
   Z3.Expr.mk_app z3_ctx mk_unit [] in unit_sort, unit_val *)

(** [translate_typ_lit] returns the Z3 sort corresponding to the Catala literal
    type [t] **)
let translate_typ_lit (ctx : context) (t : typ_lit) : Z3.Sort.sort =
  match t with
  | TBool -> Z3.Boolean.mk_sort ctx.ctx_z3
  | TUnit -> failwith "TUnit not implemented"
  | TInt -> Z3.Arithmetic.Integer.mk_sort ctx.ctx_z3
  | TRat -> failwith "TRat not implemented"
  | TMoney -> failwith "TMoney not implemented"
  | TDate -> failwith "TDate not implemented"
  | TDuration -> failwith "TDuration not implemented"

(** [translate_typ] returns the Z3 sort correponding to the Catala type [t] **)
let rec translate_typ (ctx : context) (t : naked_typ) : context * Z3.Sort.sort =
  match t with
  | TLit t -> ctx, translate_typ_lit ctx t
  | TStruct name ->
    find_or_create_struct ctx
      name (* TODO CONC are declarations sorted in topological order? *)
  | TTuple _ -> failwith "TTuple not implemented"
  | TEnum _ -> failwith "TEnum not implemented"
  | TOption _ -> failwith "TOption not implemented"
  | TArrow _ -> failwith "TArrow not implemented"
  | TArray _ -> failwith "TArray not implemented"
  | TAny -> failwith "TAny not implemented"
  | TClosureEnv -> failwith "TClosureEnv not implemented"

(* taken from z3backend's find_or_create_struct *)
and find_or_create_struct (ctx : context) (s : StructName.t) :
    context * Z3.Sort.sort =
  match StructName.Map.find_opt s ctx.ctx_z3structs with
  | Some s -> ctx, s
  | None ->
    let s_name = Mark.remove (StructName.get_info s) in
    let fields = StructName.Map.find s ctx.ctx_decl.ctx_structs in
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
        (fun _ ty (ctx, ftypes) ->
          let ctx, ftype = translate_typ ctx (Mark.remove ty) in
          ctx, ftype :: ftypes)
        fields (ctx, [])
    in
    let z3_fieldtypes = List.rev z3_fieldtypes_rev in
    let z3_sortrefs = List.map (fun _ -> 0) z3_fieldtypes in
    (* will not be used *)
    let mk_struct_s = "mk!" ^ s_name (* struct constructor *) in
    let is_struct_s = "is!" ^ s_name (* recognizer *) in
    let z3_mk_struct =
      Z3.Datatype.mk_constructor_s ctx.ctx_z3 mk_struct_s
        (Z3.Symbol.mk_string ctx.ctx_z3 is_struct_s)
        z3_fieldnames
        (List.map Option.some z3_fieldtypes)
        z3_sortrefs
    in

    let z3_struct = Z3.Datatype.mk_sort_s ctx.ctx_z3 s_name [z3_mk_struct] in
    add_z3struct s z3_struct ctx, z3_struct

(* taken from z3backend, but without the option check *)
let make_empty_context (decl_ctx : decl_ctx) : context =
  let z3_cfg = ["model", "true"; "proof", "false"] in
  let z3_ctx = Z3.mk_context z3_cfg in
  {
    ctx_z3 = z3_ctx;
    ctx_decl = decl_ctx;
    (*     ctx_funcdecl = Var.Map.empty; *)
    (*     ctx_z3vars = StringMap.empty; *)
    (*     ctx_z3datatypes = EnumName.Map.empty; *)
    (*     ctx_z3matchsubsts = Var.Map.empty; *)
    ctx_z3structs = StructName.Map.empty;
    (*     ctx_z3unit = create_z3unit z3_ctx; *)
    (*     ctx_z3constraints = []; *)
  }

let init_context (ctx : context) : context =
  (* create all struct sorts *)
  let ctx =
    StructName.Map.fold
      (fun s _ ctx -> fst (find_or_create_struct ctx s))
      ctx.ctx_decl.ctx_structs ctx
  in
  (* TODO add things when needed *)
  ctx

(* loosely taken from z3backend, could be exposed instead? not necessarily,
   especially if they become plugins *)
let symb_of_lit ctx (l : lit) : s_expr =
  match l with
  | LBool b -> Z3.Boolean.mk_val ctx.ctx_z3 b
  | LInt n ->
    Z3.Arithmetic.Integer.mk_numeral_i ctx.ctx_z3 (Runtime.integer_to_int n)
    (* TODO CONC this can technically overflow even though neither [Z] nor Z3
       integers do, but does the case happen in real use? *)
  | LRat _ -> failwith "LRat not implemented"
  | LMoney _ -> failwith "LMoney not implemented"
  | LUnit -> failwith "LUnit not implemented"
  | LDate _ -> failwith "LDate not implemented"
  | LDuration _ -> failwith "LDuration not implemented"

(** Get the symbolic expression corresponding to concolic expression [e] *)
let get_symb_expr (e : 'c conc_expr) : s_expr option =
  let (Custom { custom; _ }) = Mark.get e in
  custom.symb_expr

let get_constraints (e : 'c conc_expr) : s_expr list =
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
  let accessors = List.hd (Z3.Datatype.get_accessors sort) in
  Message.emit_debug "accessors %s"
    (List.fold_left
       (fun acc a -> Z3.FuncDecl.to_string a ^ "," ^ acc)
       "" accessors);
  let idx_mappings = List.combine (StructField.Map.keys fields) accessors in
  let _, accessor =
    List.find (fun (field1, _) -> StructField.equal field field1) idx_mappings
  in
  Z3.Expr.mk_app ctx.ctx_z3 accessor [s]

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
        Expr.format e;
      add_conc_info_e symb_expr ~constraints:[] e
    (* TODO CONC REU copy down symb_expr, reset constraints *)
    (* TODO CONC note that we keep the position from the var *)
    | None -> e)
  | _ -> e

let handle_eq evaluate_operator pos lang e1 e2 =
  let open Runtime.Oper in
  match e1, e2 with
  | ELit LUnit, ELit LUnit -> failwith "EOp Eq LUnit not implemented"
  | ELit (LBool b1), ELit (LBool b2) -> not (o_xor b1 b2)
  | ELit (LInt x1), ELit (LInt x2) -> o_eq_int_int x1 x2
  | ELit (LRat _), ELit (LRat _) -> failwith "EOp Eq LRat not implemented"
  | ELit (LMoney _), ELit (LMoney _) -> failwith "EOp Eq LMoney not implemented"
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
  | EInj _, EInj _ -> failwith "EOp Eq EInj not implemented"
  | _, _ -> false (* comparing anything else return false *)

(* Call-by-value: the arguments are expected to be already evaluated here *)
let rec evaluate_operator
    evaluate_expr
    ctx
    (op : < overloaded : no ; .. > operator)
    m
    lang
    args =
  let pos = Expr.mark_pos m in
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
      op Expr.format
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
    failwith
      "Eop From/ToClosureEnv not implemented" (* TODO CONC will they ever be? *)
  (* | (ToClosureEnv | FromClosureEnv), _ -> err () *)
  | Eq, [e1; e2] ->
    let e1' = Mark.remove e1 in
    let e2' = Mark.remove e2 in
    let concrete =
      ELit
        (LBool (handle_eq (evaluate_operator evaluate_expr ctx) m lang e1' e2'))
    in
    (* TODO CONC catch errors here, or maybe propagate [None]? *)
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
    let concrete = ELit (LBool (o_not b)) in
    let e_symb = Option.get (get_symb_expr e) in
    (* TODO handle error *)
    let symb_expr = Z3.Boolean.mk_not ctx.ctx_z3 e_symb in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | GetDay, _ -> failwith "Eop GetDay not implemented"
  | GetMonth, _ -> failwith "Eop GetMonth not implemented"
  | GetYear, _ -> failwith "Eop GetYear not implemented"
  | FirstDayOfMonth, _ -> failwith "Eop FirstDayOfMonth not implemented"
  | LastDayOfMonth, _ -> failwith "Eop LastDayOfMonth not implemented"
  | And, [((ELit (LBool b1), _) as e1); ((ELit (LBool b2), _) as e2)] ->
    let concrete = ELit (LBool (o_and b1 b2)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Boolean.mk_and ctx.ctx_z3 [e1_symb; e2_symb] in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Or, [((ELit (LBool b1), _) as e1); ((ELit (LBool b2), _) as e2)] ->
    let concrete = ELit (LBool (o_or b1 b2)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Boolean.mk_or ctx.ctx_z3 [e1_symb; e2_symb] in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Xor, [((ELit (LBool b1), _) as e1); ((ELit (LBool b2), _) as e2)] ->
    let concrete = ELit (LBool (o_xor b1 b2)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Boolean.mk_xor ctx.ctx_z3 e1_symb e2_symb in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | ( ( Not
        (* | GetDay | GetMonth | GetYear | FirstDayOfMonth | LastDayOfMonth *)
      | And | Or | Xor ),
      _ ) ->
    err ()
  | Minus_int, [((ELit (LInt x), _) as e)] ->
    let concrete = ELit (LInt (o_minus_int x)) in
    (* TODO handle error *)
    let e_symb = Option.get (get_symb_expr e) in
    let symb_expr = Z3.Arithmetic.mk_unary_minus ctx.ctx_z3 e_symb in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Minus_rat, _ -> failwith "Eop Minus_rat not implemented"
  | Minus_mon, _ -> failwith "Eop Minus_mon not implemented"
  | Minus_dur, _ -> failwith "Eop Minus_dur not implemented"
  | ToRat_int, _ -> failwith "Eop ToRat_int not implemented"
  | ToRat_mon, _ -> failwith "Eop ToRat_mon not implemented"
  | ToMoney_rat, _ -> failwith "Eop ToMoney_rat not implemented"
  | Round_mon, _ -> failwith "Eop Round_mon not implemented"
  | Round_rat, _ -> failwith "Eop Round_rat not implemented"
  | Add_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    let concrete = ELit (LInt (o_add_int_int x y)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Arithmetic.mk_add ctx.ctx_z3 [e1_symb; e2_symb] in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Add_rat_rat, _ -> failwith "Eop Add_rat_rat not implemented"
  | Add_mon_mon, _ -> failwith "Eop Add_mon_mon not implemented"
  | Add_dat_dur _, _ -> failwith "Eop Add_dat_dur not implemented"
  | Add_dur_dur, _ -> failwith "Eop Add_dur_dur not implemented"
  | Sub_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    let concrete = ELit (LInt (o_sub_int_int x y)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Arithmetic.mk_sub ctx.ctx_z3 [e1_symb; e2_symb] in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Sub_rat_rat, _ -> failwith "Eop Sub_rat_rat not implemented"
  | Sub_mon_mon, _ -> failwith "Eop Sub_mon_mon not implemented"
  | Sub_dat_dat, _ -> failwith "Eop Sub_dat_dat not implemented"
  | Sub_dat_dur, _ -> failwith "Eop Sub_dat_dur not implemented"
  | Sub_dur_dur, _ -> failwith "Eop Sub_dur_dur not implemented"
  | Mult_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    let concrete = ELit (LInt (o_mult_int_int x y)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Arithmetic.mk_mul ctx.ctx_z3 [e1_symb; e2_symb] in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Mult_rat_rat, _ -> failwith "Eop Mult_rat_rat not implemented"
  | Mult_mon_rat, _ -> failwith "Eop Mult_mon_rat not implemented"
  | Mult_dur_int, _ -> failwith "Eop Mult_dur_int not implemented"
  | Div_int_int, _ -> failwith "Eop Div_int_int not implemented"
  | Div_rat_rat, _ -> failwith "Eop Div_rat_rat not implemented"
  | Div_mon_mon, _ -> failwith "Eop Div_mon_mon not implemented"
  | Div_mon_rat, _ -> failwith "Eop Div_mon_rat not implemented"
  | Div_dur_dur, _ -> failwith "Eop Div_dur_dur not implemented"
  | Lt_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    let concrete = ELit (LBool (o_lt_int_int x y)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Arithmetic.mk_lt ctx.ctx_z3 e1_symb e2_symb in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Lt_rat_rat, _ -> failwith "Eop Lt_rat_rat not implemented"
  | Lt_mon_mon, _ -> failwith "Eop Lt_mon_mon not implemented"
  | Lt_dat_dat, _ -> failwith "Eop Lt_dat_dat not implemented"
  | Lt_dur_dur, _ -> failwith "Eop Lt_dur_dur not implemented"
  | Lte_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    let concrete = ELit (LBool (o_lte_int_int x y)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Arithmetic.mk_le ctx.ctx_z3 e1_symb e2_symb in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Lte_rat_rat, _ -> failwith "Eop Lte_rat_rat not implemented"
  | Lte_mon_mon, _ -> failwith "Eop Lte_mon_mon not implemented"
  | Lte_dat_dat, _ -> failwith "Eop Lte_dat_dat not implemented"
  | Lte_dur_dur, _ -> failwith "Eop Lte_dur_dur not implemented"
  | Gt_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    let concrete = ELit (LBool (o_gt_int_int x y)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Arithmetic.mk_gt ctx.ctx_z3 e1_symb e2_symb in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Gt_rat_rat, _ -> failwith "Eop Gt_rat_rat not implemented"
  | Gt_mon_mon, _ -> failwith "Eop Gt_mon_mon not implemented"
  | Gt_dat_dat, _ -> failwith "Eop Gt_dat_dat not implemented"
  | Gt_dur_dur, _ -> failwith "Eop Gt_dur_dur not implemented"
  | Gte_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    let concrete = ELit (LBool (o_gte_int_int x y)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Arithmetic.mk_ge ctx.ctx_z3 e1_symb e2_symb in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Gte_rat_rat, _ -> failwith "Eop Gte_rat_rat not implemented"
  | Gte_mon_mon, _ -> failwith "Eop Gte_mon_mon not implemented"
  | Gte_dat_dat, _ -> failwith "Eop Gte_dat_dat not implemented"
  | Gte_dur_dur, _ -> failwith "Eop Gte_dur_dur not implemented"
  | Eq_int_int, [((ELit (LInt x), _) as e1); ((ELit (LInt y), _) as e2)] ->
    let concrete = ELit (LBool (o_eq_int_int x y)) in
    (* TODO handle errors *)
    let e1_symb = Option.get (get_symb_expr e1) in
    let e2_symb = Option.get (get_symb_expr e2) in
    let symb_expr = Z3.Boolean.mk_eq ctx.ctx_z3 e1_symb e2_symb in
    add_conc_info_m m (Some symb_expr) ~constraints:[] concrete
  | Eq_rat_rat, _ -> failwith "Eop Eq_rat_rat not implemented"
  | Eq_mon_mon, _ -> failwith "Eop Eq_mon_mon not implemented"
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
  | ( ( Minus_int
        (* | Minus_rat | Minus_mon | Minus_dur | ToRat_int | ToRat_mon |
           ToMoney_rat | Round_rat | Round_mon *)
      | Add_int_int
        (* | Add_rat_rat | Add_mon_mon | Add_dat_dur _ | Add_dur_dur *)
      | Sub_int_int
        (* | Sub_rat_rat | Sub_mon_mon | Sub_dat_dat | Sub_dat_dur |
           Sub_dur_dur *)
      | Mult_int_int
        (* | Mult_rat_rat | Mult_mon_rat | Mult_dur_int | Div_int_int |
           Div_rat_rat | Div_mon_mon | Div_mon_rat | Div_dur_dur *)
      | Lt_int_int (* | Lt_rat_rat | Lt_mon_mon | Lt_dat_dat | Lt_dur_dur *)
      | Lte_int_int
        (* | Lte_rat_rat | Lte_mon_mon | Lte_dat_dat | Lte_dur_dur *)
      | Gt_int_int (* | Gt_rat_rat | Gt_mon_mon | Gt_dat_dat | Gt_dur_dur *)
      | Gte_int_int
        (* | Gte_rat_rat | Gte_mon_mon | Gte_dat_dat | Gte_dur_dur *)
      | Eq_int_int (* | Eq_rat_rat | Eq_mon_mon | Eq_dat_dat | Eq_dur_dur *) ),
      _ ) ->
    err ()

let rec evaluate_expr :
    context -> Cli.backend_lang -> yes conc_expr -> yes conc_expr =
 fun ctx lang e ->
  Message.emit_debug "eval %a" Expr.format e;
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
    (* TODO I don't like that "Concrete" has to appear here because this
       function is not specific to concrete evaluation. maybe let p... =
       Concrete.p...? *)
    @@ fun e1 ->
    match Mark.remove e1 with
    | EAbs { binder; _ } ->
      (* DONE CONC should constraints from the args be added, or should we trust
         the call to return them? We could take both for safety but there will
         be duplication... I think we should trust the recursive call, and we'll
         see if it's better not to TODO CONC REU actually it's better not to :
         it can lead to duplication, and the subespression does not need the
         constraints anyway =>> see the big concatenation below *)
      (* DONE CONC REU How to pass down the symbolic expressions and the
         constraints anyway? The arguments as passed to [Bindlib.msubst] are
         unmarked. Maybe a way is to change the corresponding marks in the
         receiving expression in which substitution happens. 1/ change marks 2/
         substitute concrete expressions *)
      if Bindlib.mbinder_arity binder = List.length args then (
        let vars, eb = Bindlib.unmbind binder in
        let vars_args_map = make_vars_args_map vars args in
        Message.emit_debug "EApp>EAbs vars are %s"
          (Array.fold_left
             (fun acc s -> s ^ "," ^ acc)
             "" (Bindlib.names_of vars));
        Message.emit_debug "EApp>EAbs args are";
        List.iter
          (fun arg ->
            Message.emit_debug "EApp>EAbs arg %a | %s | %i" Expr.format arg
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
        (* TODO [Expr.subst]? *)
        let r_symb = get_symb_expr result in
        let r_constraints = get_constraints result in
        (* TODO CONC REU the constraints generated by the evaluation of the application are:
         * - those generated by the evaluation of the function
         * - those generated by the evaluation of the arguments
         * - the NEW ones generated by the evaluation of the subexpression
         *   (where the ones of the arguments are neither passed down, nor
         *   re-generated as this is cbv)
         *)
        let constraints = r_constraints @ args_constraints @ f_constraints in
        Message.emit_debug
          "EApp>EAbs concatenated r_constraints(%i), args_constraints(%i), and \
           f_constraints(%i)"
          (List.length r_constraints)
          (List.length args_constraints)
          (List.length f_constraints);
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
      (* TODO CONC the constraints generated by the evaluation of the application are:
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
    (* TODO check if this typing shenanigan is really necessary... *)
  | ELit l as e ->
    Message.emit_debug "... it's an ELit";
    let symb_expr = symb_of_lit ctx l in
    (* TODO CONC REU no constraints generated *)
    add_conc_info_m m (Some symb_expr) ~constraints:[] e
  | EOp { op; tys } -> Expr.unbox (Expr.eop (Operator.translate op) tys m)
  (* TODO CONC is this "typing shenanigan" needed? I removed it from the
     concrete interpreter and nothing broke... *)
  (* | EAbs _ as e -> Marked.mark m e (* these are values *) *)
  | EStruct { fields = es; name } ->
    Message.emit_debug "... it's an EStruct";
    let fields, es = List.split (StructField.Map.bindings es) in

    (* compute all subexpressions *)
    let es = List.map (evaluate_expr ctx lang) es in

    (* make symbolic expression using the symbolic sub-expressions *)
    let symb_exprs = List.map get_symb_expr es in
    let symb_exprs = List.map Option.get symb_exprs in
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
      (* TODO CONC REU the constraints generated by struct access are only those
         generated by the subcall, as the field expression is already a value *)
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
  | EInj _ -> failwith "EInj not implemented"
  | EMatch _ -> failwith "EMatch not implemented"
  | EIfThenElse _ -> failwith "EIfThenElse not implemented"
  | EArray _ -> failwith "EArray not implemented"
  | EAssert _ -> failwith "EAssert not implemented"
  | ECustom _ -> failwith "ECustom not implemented"
  | EEmptyError ->
    Message.emit_debug "... it's an EEmptyError";
    e (* TODO CONC REU just pass along the symbolic values and constraints? *)
  | EErrorOnEmpty e' -> (
    Message.emit_debug "... it's an EErrorOnEmpty";
    match evaluate_expr ctx lang e' with
    | EEmptyError, _ ->
      Message.raise_spanned_error (Expr.pos e')
        "This variable evaluated to an empty term (no rule that defined it \
         applied in this situation)"
    | e -> e
    (* TODO CONC REU just pass along the concrete and symbolic values, and the
       constraints *))
  | EDefault { excepts; just; cons } -> (
    Message.emit_debug "... it's an EDefault";
    (* TODO CONC REU how did Rohan do? maybe contact him? => `*)
    (* TODO CONC look into litterature for exceptions in concolic exec (Java?)
       or symb exec (symbolic pathfinder) *)
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
        Message.emit_debug "EDefault>empty adding %s to constraints"
          (Z3.Expr.to_string j_symb);
        (* TODO CONC REU the constraints generated by the default when [just] is empty are :
         * - those generated by the evaluation of the excepts
         * - those generated by the evaluation of [just]
         * - a new constraint corresponding to [just] itself
         *)
        let constraints = (j_symb :: j_constraints) @ exc_constraints in
        add_conc_info_m m None ~constraints EEmptyError
      | ELit (LBool true) ->
        Message.emit_debug "EDefault>true adding %s to constraints"
          (Z3.Expr.to_string j_symb);
        let cons = evaluate_expr ctx lang cons in
        let c_symb = get_symb_expr cons in
        let c_constraints = get_constraints cons in
        let c_mark = Mark.get cons in
        let c_concr = Mark.remove cons in
        (* TODO CONC REU the constraints generated by the default when [just] is true are :
         * - those generated by the evaluation of the excepts
         * - those generated by the evaluation of [just]
         * - a new constraint corresponding to [just] itself
         * - those generated by the evaluation of [cons]
         *)
        let constraints =
          c_constraints @ (j_symb :: j_constraints) @ exc_constraints
        in
        Message.emit_debug
          "EDefault>true concatenated j_symb::j_constraints(%i) to \
           c_constraints(%i)"
          (1 + List.length j_constraints)
          (List.length c_constraints);
        add_conc_info_m c_mark c_symb ~constraints c_concr
      | ELit (LBool false) ->
        let not_j_symb = Z3.Boolean.mk_not ctx.ctx_z3 j_symb in
        Message.emit_debug "EDefault>false adding %s to constraints"
          (Z3.Expr.to_string not_j_symb);
        (* TODO CONC REU the constraints generated by the default when [just] is false are :
         * - those generated by the evaluation of the excepts
         * - those generated by the evaluation of [just]
         * - a new constraint corresponding to [just] itself
         *)
        let constraints = (not_j_symb :: j_constraints) @ exc_constraints in
        add_conc_info_m m None ~constraints EEmptyError
      | _ ->
        Message.raise_spanned_error (Expr.pos e)
          "Default justification has not been reduced to a boolean at \
           evaluation (should not happen if the term was well-typed")
    | 1 ->
      let r =
        List.find (fun sub -> not (Concrete.is_empty_error sub)) excepts
      in
      (* TODO CONC the constraints generated by the default when exactly one except is raised are :
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
  | _ -> .

let lit_of_tlit t =
  match t with
  | TBool -> LBool true
  | TInt -> LInt (Z.of_int 42)
  | _ -> failwith "not implemented"

let expr_of_typ mark ty =
  match Mark.remove ty with
  | TLit t -> Expr.elit (lit_of_tlit t) mark
  | TArrow (ty_in, ty_out) ->
    Expr.make_abs
      (Array.of_list @@ List.map (fun _ -> Var.make "_") ty_in)
      (Bindlib.box EEmptyError, Expr.with_ty mark ty_out)
      ty_in (Expr.mark_pos mark)
  | _ -> failwith "not implemented"

let make_default_inputs (input_typs : typ StructField.Map.t) mark =
  StructField.Map.map (expr_of_typ mark) input_typs

let conc_expr_of_typ ctx mark field ty : 'c conc_boxed_expr =
  let concrete = expr_of_typ mark ty in
  let name = Mark.remove (StructField.get_info field) in
  let _, sort = translate_typ ctx (Mark.remove ty) in
  let symbol = Z3.Expr.mk_const_s ctx.ctx_z3 name sort in
  add_conc_info_e (Some symbol) concrete

(** for now, just like default inputs, but concolic *)
let make_conc_inputs ctx (input_typs : typ StructField.Map.t) mark :
    'c conc_boxed_expr StructField.Map.t =
  StructField.Map.mapi (conc_expr_of_typ ctx mark) input_typs

let input_of_list = StructField.Map.of_list

(* Currently interprets with defaults values when available *)
let interpret_concrete_with_inputs :
    ((yes, no, 'c) interpr_kind, 't) gexpr program ->
    ScopeName.t ->
    ((yes, no, 'c) interpr_kind, 't) boxed_gexpr StructField.Map.t ->
    ((yes, no, 'c) interpr_kind, 't) gexpr StructField.Map.t =
 fun p s i ->
  let ctx = p.decl_ctx in
  let e = Expr.unbox (Program.to_expr p s) in
  match Concrete.evaluate_expr p.decl_ctx p.lang (Concrete.addcustom e) with
  | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e -> begin
    let taus = StructName.Map.find s_in ctx.ctx_structs in
    let all_fields_in_input =
      (* sanity check to catch a missing input before interpretation
       * TODO CONC should there be a typecheck as well?
       * TODO CONC better error message to show which field is missing *)
      StructField.Map.for_all (fun field _ -> StructField.Map.mem field i) taus
    in
    if all_fields_in_input then begin
      let to_interpret =
        Expr.make_app (Expr.box e)
          [Expr.estruct ~name:s_in ~fields:i mark_e]
          (Expr.pos e)
      in
      match
        Mark.remove
          (Concrete.evaluate_expr ctx p.lang (Expr.unbox to_interpret))
      with
      | EStruct { fields; _ } -> fields
      | _ ->
        Message.raise_spanned_error (Expr.pos e)
          "The interpretation of a program should always yield a struct \
           corresponding to the scope variables"
    end
    else
      Message.raise_spanned_error (Expr.pos e)
        "Concolic concrete execution expects values in all input fields"
  end
  | _ ->
    Message.raise_spanned_error (Expr.pos e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"

let interpret_program_concolic (type m) (p : (dcalc, m) gexpr program) s :
    (Uid.MarkedString.info * yes conc_expr) list =
  Message.emit_debug "Start concolic interpretation...";
  let ctx = p.decl_ctx in
  let ctx = make_empty_context ctx in
  let ctx = init_context ctx in

  (* let s = StructName.Map.fold (fun struct_name sort acc -> let struct_s =
     Mark.remove (StructName.get_info struct_name) in let sort_s =
     Z3.Sort.to_string sort in struct_s ^ " : " ^ sort_s ^ "\n" ^ acc)
     ctx.ctx_z3structs "" in print_endline s; *)
  let e = Expr.unbox (Program.to_expr p s) in
  let e_conc = init_conc_expr e in
  Message.emit_debug "...program expression made concolic...";
  match evaluate_expr ctx p.lang (Concrete.addcustom e_conc) with
  | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e -> begin
    (* At this point, the interpreter seeks to execute the scope but does not
       have a way to retrieve input values from the command line. [taus] contain
       the types of the scope arguments. For [context] arguments, we can provide
       an empty thunked term. But for [input] arguments of another type, we
       cannot provide anything so we have to fail. *)
    Message.emit_debug "...pre-evaluation done...";
    let taus = StructName.Map.find s_in ctx.ctx_decl.ctx_structs in
    let application_term =
      make_conc_inputs ctx taus mark_e (* TODO CONC should this mark change? *)
    in
    Message.emit_debug "...inputs generated...";
    let to_interpret =
      Expr.eapp (Expr.box e)
        (* TODO CONC REU originally [make_app] but it folds marks... *)
        [Expr.estruct ~name:s_in ~fields:application_term mark_e]
        (set_conc_info None [] (Mark.get e))
    in
    Message.emit_debug "...inputs applied...";
    let res = evaluate_expr ctx p.lang (Expr.unbox to_interpret) in
    Message.emit_debug "...scope interepred with inputs...";
    Message.emit_result "Constraints: %s"
      (List.fold_left
         (fun acc z -> Z3.Expr.to_string (Z3.Expr.simplify z None) ^ "," ^ acc)
           (* TODO CONC REU should I always simplify? Rohan does it *)
         "" (get_constraints res));
    match Mark.remove res with
    | EStruct { fields; _ } ->
      List.map
        (fun (fld, e) -> StructField.get_info fld, e)
        (StructField.Map.bindings fields)
    | _ ->
      Message.raise_spanned_error (Expr.pos e)
        "The interpretation of a program should always yield a struct \
         corresponding to the scope variables"
  end
  | _ ->
    Message.raise_spanned_error (Expr.pos e)
      "The interpreter can only interpret terms starting with functions having \
       thunked arguments"
