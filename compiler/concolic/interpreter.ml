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
module Concrete = Shared_ast.Interpreter

type s_expr = Z3.Expr.expr (* TODO use real Z3 expression type here *)
type _conc_info = { symb_expr : s_expr option; constraints : s_expr list }
type conc_info = _conc_info custom

type 'c conc_expr = ((yes, no, 'c) interpr_kind, conc_info) gexpr
(** A concolic expression is a concrete DCalc expression that carries its
    symbolic representation and the constraints necessary to compute it. Upon
    initialization, [symb_expr] is [None], except for inputs whose [symb_expr]
    is a Z3 symbol constant. Then [symb_expr] is set by evaluation, except for
    inputs which stay the same *)

type 'c conc_naked_expr = ((yes, no, 'c) interpr_kind, conc_info) naked_gexpr

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

(* TODO CONC taken from z3backend *)

(** [add_z3struct] adds the mapping between the Catala struct [s] and the
    corresponding Z3 datatype [sort] to the context **)
let add_z3struct (s : StructName.t) (sort : Z3.Sort.sort) (ctx : context) :
    context =
  { ctx with ctx_z3structs = StructName.Map.add s sort ctx.ctx_z3structs }

(* TODO CONC taken from z3backend: should I expose it there instead? *)
let create_z3unit (z3_ctx : Z3.context) : Z3.Sort.sort * Z3.Expr.expr =
  let unit_sort =
    Z3.Tuple.mk_sort z3_ctx (Z3.Symbol.mk_string z3_ctx "unit") [] []
  in
  let mk_unit = Z3.Tuple.get_mk_decl unit_sort in
  let unit_val = Z3.Expr.mk_app z3_ctx mk_unit [] in
  unit_sort, unit_val

(* TODO CONC taken from z3backend *)

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

(* TODO CONC taken from z3backend *)

(** [translate_typ] returns the Z3 sort correponding to the Catala type [t] **)
let rec translate_typ (ctx : context) (t : naked_typ) : context * Z3.Sort.sort =
  match t with
  | TLit t -> ctx, translate_typ_lit ctx t
  | TStruct name ->
    find_or_create_struct ctx
      name (* TODO CONC are declaration sorted in topological order? *)
  | TTuple _ -> failwith "TTuple not implemented"
  | TEnum _ -> failwith "TEnum not implemented"
  | TOption _ -> failwith "TOption not implemented"
  | TArrow _ -> failwith "TArrow not implemented"
  | TArray _ -> failwith "TArray not implemented"
  | TAny -> failwith "TAny not implemented"
  | TClosureEnv -> failwith "TClosureEnv not implemented"

(* TODO CONC taken from z3backend's find_or_create_struct *)
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
          Mark.remove (StructField.get_info f) |> Z3.Symbol.mk_string ctx.ctx_z3)
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
    let mk_struct_s = "mk!" ^ s_name in
    let is_struct_s = "is!" ^ s_name in
    (* recognizer *)
    let z3_mk_struct =
      Z3.Datatype.mk_constructor_s ctx.ctx_z3 mk_struct_s
        (Z3.Symbol.mk_string ctx.ctx_z3 mk_struct_s)
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

let hard_set_conc_info
    (type m)
    (symb_expr : s_expr option)
    (constraints : s_expr list)
    (x : ('a, m) marked) : ('a, conc_info) marked =
  let custom = { symb_expr; constraints } in
  Mark.add
    (match Mark.get x with
    | Untyped { pos } -> Custom { pos; custom }
    | Typed { pos; ty } ->
      Custom { pos; custom }
      (* TODO CONC should we keep the type information? maybe for the symb
         exprs *)
    | Custom m -> Custom { m with custom })
    (Mark.remove x)

(** Replace the constraints, and safely replace the symbolic expression from
    former mark *)
let add_conc_info
    (former_mark : conc_info mark)
    (symb_expr : s_expr)
    (constraints : s_expr list) : 'a -> ('a, conc_info) marked =
  let (Custom { pos; custom = { symb_expr = s; _ } }) = former_mark in
  let symb_expr = Some (Option.value s ~default:symb_expr) in
  Mark.add (Custom { pos; custom = { symb_expr; constraints } })

(** Replace the constraints, and safely replace the symbolic expression *)
let replace_conc_info
    (symb_expr : s_expr)
    (constraints : s_expr list)
    (x : ('a, conc_info) marked) : ('a, conc_info) marked =
  let m = Mark.get x in
  add_conc_info m symb_expr constraints (Mark.remove x)

(* TODO CONC loosely taken from z3backend, could be exposed instead? *)
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

let get_constraints (e : 'c conc_expr) : s_expr list =
  let (Custom { custom; _ }) = Mark.get e in
  custom.constraints

(** Get the symbolic expression corresponding to concolic expression [e] This
    should only be called if the symbolic expression has already been computed,
    and fails otherwise *)
let get_symb_expr (e : 'c conc_expr) : s_expr =
  let (Custom { custom; _ }) = Mark.get e in
  Option.get custom.symb_expr
(* TODO use a more explicit exception? this should not happen anyway *)

let gather_constraints (es : 'c conc_expr list) =
  let constraints = List.map get_constraints es in
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
  let idx_mappings = List.combine (StructField.Map.keys fields) accessors in
  let _, accessor =
    List.find (fun (field1, _) -> StructField.equal field field1) idx_mappings
  in
  Z3.Expr.mk_app ctx.ctx_z3 accessor [s]

let make_vars_args_map
    (vars : 'c conc_naked_expr Bindlib.var array)
    (args : 'c conc_expr list) : ('c conc_expr, conc_info mark) Var.Map.t =
  let marks = List.map Mark.get args in
  let zipped = Array.combine vars (Array.of_list marks) in
  Array.fold_left (fun acc (v, m) -> Var.Map.add v m acc) Var.Map.empty zipped

let replace_EVar_mark
    (vars_args : ('c conc_expr, conc_info mark) Var.Map.t)
    (e : 'c conc_expr) : 'c conc_expr =
  match Mark.remove e with
  | EVar v as e ->
    let m = Var.Map.find v vars_args in
    (* TODO raise exception when find fails *)
    Mark.add m e
    (* TODO CONC be more subtle and keep the original position from var? *)
  | _ -> e

let rec evaluate_expr :
    context -> Cli.backend_lang -> yes conc_expr -> yes conc_expr =
 fun ctx lang e ->
  let m = Mark.get e in
  let pos = Expr.mark_pos m in
  match Mark.remove e with
  | EVar _ ->
    (* TODO eval *)
    Message.raise_spanned_error pos
      "free variable found at evaluation (should not happen if term was \
       well-typed)"
  | EExternal _ -> failwith "EExternal not implemented"
  | EApp { f = e1; args } -> (
    (* DONE eval *)
    let e1 = evaluate_expr ctx lang e1 in
    let c1 = get_constraints e1 in
    let args = List.map (evaluate_expr ctx lang) args in
    let constraints = gather_constraints args in
    Concrete.propagate_empty_error e1
    (* TODO I don't like that "Concrete" has to appear here because this
       function is not specific to concrete evaluation. maybe let p... =
       Concrete.p...? *)
    @@ fun e1 ->
    match Mark.remove e1 with
    | EAbs { binder; _ } ->
      (* TODO CONC should constraints from the args be added, or should we trust
         the call to return them? We could take both for safety but there will
         be duplication... I think we should trust the recursive call, and we'll
         see if it's better not to *)
      (* TODO CONC How to pass down the symbolic expressions and the constraints
         anyway? The arguments as passed to [Bindlib.msubst] are unmarked. Maybe
         a way is to change the corresponding marks in the receiving expression
         in which substitution happens. 1/ change marks 2/ substitute concrete
         expressions *)
      if Bindlib.mbinder_arity binder = List.length args then
        let vars, eb = Bindlib.unmbind binder in
        let vars_args_map = make_vars_args_map vars args in
        let marked_eb =
          Expr.map_top_down ~f:(replace_EVar_mark vars_args_map) eb
        in
        let marked_binder = Bindlib.unbox (Expr.bind vars marked_eb) in
        let result =
          evaluate_expr ctx lang
            (Bindlib.msubst marked_binder
               (Array.of_list (List.map Mark.remove args)))
        in
        (* TODO [Expr.subst]? *)
        let r_symb = get_symb_expr result in
        let r_constraints = get_constraints result in
        replace_conc_info r_symb (c1 @ r_constraints) result
        (* NOTE that here the position comes from [result], while in other cases
           of this function the position comes from the input expression. This
           is the behaviour of the concrete interpreter *)
      else
        Message.raise_spanned_error pos
          "wrong function call, expected %d arguments, got %d"
          (Bindlib.mbinder_arity binder)
          (List.length args)
    | EOp _ -> failwith "EApp of EOp not implemented"
    | ECustom _ -> failwith "EApp of ECustom not implemented"
    | _ ->
      Message.raise_spanned_error pos
        "function has not been reduced to a lambda at evaluation (should not \
         happen if the term was well-typed")
  | EAbs { binder; tys } ->
    Expr.unbox (Expr.eabs (Bindlib.box binder) tys m)
    (* DONE eval *)
    (* TODO check if this typing shenanigan is really necessary... *)
  | ELit l as e ->
    (* DONE eval *)
    let symb_expr = symb_of_lit ctx l in
    add_conc_info m symb_expr [] e
  | EOp _ -> failwith "EOp not implemented"
  (* | EAbs _ as e -> Marked.mark m e (* these are values *) *)
  | EStruct { fields = es; name } ->
    (* DONE eval *)
    let fields, es = List.split (StructField.Map.bindings es) in

    (* compute all subexpressions *)
    let es = List.map (evaluate_expr ctx lang) es in

    (* make symbolic expression using the symbolic sub-expressions *)
    let symb_exprs = List.map get_symb_expr es in
    let symb_expr = make_z3_struct ctx name symb_exprs in

    (* gather all constraints from sub-expressions *)
    let constraints = gather_constraints es in

    Concrete.propagate_empty_error_list es
    @@ fun es ->
    add_conc_info m symb_expr constraints
      (EStruct
         {
           fields =
             StructField.Map.of_seq
               (Seq.zip (List.to_seq fields) (List.to_seq es));
           name;
         })
  | EStructAccess { e; name = s; field } -> (
    (* DONE eval *)
    Concrete.propagate_empty_error (evaluate_expr ctx lang e)
    @@ fun e ->
    match Mark.remove e with
    | EStruct { fields = es; name } ->
      if not (StructName.equal s name) then
        Message.raise_multispanned_error
          [None, pos; None, Expr.pos e]
          "Error during struct access: not the same structs (should not happen \
           if the term was well-typed)";
      let concrete_expr =
        match StructField.Map.find_opt field es with
        | Some e' -> e'
        | None ->
          Message.raise_spanned_error (Expr.pos e)
            "Invalid field access %a in struct %a (should not happen if the \
             term was well-typed)"
            StructField.format field StructName.format s
      in
      let symb_expr = make_z3_struct_access ctx s field (get_symb_expr e) in
      let constraints = get_constraints e in
      (* TODO CONC is it e or concrete_expr ? *)
      add_conc_info m symb_expr constraints (Mark.remove concrete_expr)
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
  | EEmptyError -> Mark.copy e EEmptyError (* TODO eval *)
  | EErrorOnEmpty e' -> (
    (* TODO eval *)
    match evaluate_expr ctx lang e' with
    | EEmptyError, _ ->
      Message.raise_spanned_error (Expr.pos e')
        "This variable evaluated to an empty term (no rule that defined it \
         applied in this situation)"
    | e -> e)
  | EDefault { excepts; just; cons } -> (
    (* TODO eval *)
    (* TODO CONC how did Rohan do? maybe contact him? *)
    (* TODO CONC look into litterature for exceptions in concolic exec (Java?)
       or symb exec (symbolic pathfinder) *)
    let excepts = List.map (evaluate_expr ctx lang) excepts in
    let empty_count =
      List.length (List.filter Concrete.is_empty_error excepts)
    in
    match List.length excepts - empty_count with
    | 0 -> (
      let just = evaluate_expr ctx lang just in
      match Mark.remove just with
      | EEmptyError -> Mark.add m EEmptyError
      | ELit (LBool true) -> evaluate_expr ctx lang cons
      | ELit (LBool false) -> Mark.copy e EEmptyError
      | _ ->
        Message.raise_spanned_error (Expr.pos e)
          "Default justification has not been reduced to a boolean at \
           evaluation (should not happen if the term was well-typed")
    | 1 -> List.find (fun sub -> not (Concrete.is_empty_error sub)) excepts
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

let interpret_program_concolic p s :
    (Uid.MarkedString.info * ('a, 'm) gexpr) list =
  let ctx = p.decl_ctx in
  let e = Expr.unbox (Program.to_expr p s) in
  match Concrete.evaluate_expr p.decl_ctx p.lang (Concrete.addcustom e) with
  | (EAbs { tys = [((TStruct s_in, _) as _targs)]; _ }, mark_e) as e -> begin
    (* At this point, the interpreter seeks to execute the scope but does not
       have a way to retrieve input values from the command line. [taus] contain
       the types of the scope arguments. For [context] arguments, we can provide
       an empty thunked term. But for [input] arguments of another type, we
       cannot provide anything so we have to fail. *)
    let taus = StructName.Map.find s_in ctx.ctx_structs in
    let application_term =
      make_default_inputs taus mark_e (* TODO CONC should this mark change? *)
    in
    let to_interpret =
      Expr.make_app (Expr.box e)
        [Expr.estruct ~name:s_in ~fields:application_term mark_e]
        (Expr.pos e)
    in
    match
      Mark.remove (Concrete.evaluate_expr ctx p.lang (Expr.unbox to_interpret))
    with
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
