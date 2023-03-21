(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Alain Delaët-Tixeuil <alain.delaet--tixeuil@inria.fr>

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
module D = Dcalc.Ast
module A = Ast

(** The main idea around this pass is to compile Dcalc to Lcalc without using
    [raise EmptyError] nor [try _ with EmptyError -> _]. To do so, we use the
    same technique as in rust or erlang to handle this kind of exceptions. Each
    [raise EmptyError] will be translated as [None] and each
    [try e1 with EmtpyError -> e2] as
    [match e1 with | None -> e2 | Some x -> x].

    When doing this naively, this requires to add matches and Some constructor
    everywhere. We apply here an other technique where we generate what we call
    `hoists`. Hoists are expression whom could minimally [raise EmptyError]. For
    instance in [let x = <e1, e2, ..., en| e_just :- e_cons> * 3 in x + 1], the
    sub-expression [<e1, e2, ..., en| e_just :- e_cons>] can produce an empty
    error. So we make a hoist with a new variable [y] linked to the Dcalc
    expression [<e1, e2, ..., en| e_just :- e_cons>], and we return as the
    translated expression [let x = y * 3 in x + 1].

    The compilation of expressions is found in the functions
    [translate_and_hoist ctx e] and [translate_expr ctx e]. Every
    option-generating expression when calling [translate_and_hoist] will be
    hoisted and later handled by the [translate_expr] function. Every other
    cases is found in the translate_and_hoist function. *)

open Shared_ast

type 'm hoists = ('m A.expr, 'm D.expr) Var.Map.t
(** Hoists definition. It represent bindings between [A.Var.t] and [D.expr]. *)

type 'm info = { expr : 'm A.expr boxed; var : 'm A.expr Var.t; is_pure : bool }
(** Information about each encontered Dcalc variable is stored inside a context
    : what is the corresponding LCalc variable; an expression corresponding to
    the variable build correctly using Bindlib, and a boolean `is_pure`
    indicating whenever the variable can be an EmptyError and hence should be
    matched (false) or if it never can be EmptyError (true). *)

let pp_info (fmt : Format.formatter) (info : 'm info) =
  Format.fprintf fmt "{var: %a; is_pure: %b}" Print.var info.var info.is_pure

type 'm ctx = {
  decl_ctx : decl_ctx;
  vars : ('m D.expr, 'm info) Var.Map.t;
      (** information context about variables in the current scope *)
}

let _pp_ctx (fmt : Format.formatter) (ctx : 'm ctx) =
  let pp_binding
      (fmt : Format.formatter)
      ((v, info) : 'm D.expr Var.t * 'm info) =
    Format.fprintf fmt "%a: %a" Print.var v pp_info info
  in

  let pp_bindings =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
      pp_binding
  in

  Format.fprintf fmt "@[<2>[%a]@]" pp_bindings (Var.Map.bindings ctx.vars)

(** [find ~info n ctx] is a warpper to ocaml's Map.find that handle errors in a
    slightly better way. *)
let find ?(info : string = "none") (n : 'm D.expr Var.t) (ctx : 'm ctx) :
    'm info =
  (* let _ = Format.asprintf "Searching for variable %a inside context %a"
     Print.var n pp_ctx ctx |> Cli.debug_print in *)
  try Var.Map.find n ctx.vars
  with Not_found ->
    Errors.raise_spanned_error Pos.no_pos
      "Internal Error: Variable %a was not found in the current environment. \
       Additional informations : %s."
      Print.var n info

(** [add_var pos var is_pure ctx] add to the context [ctx] the Dcalc variable
    var, creating a unique corresponding variable in Lcalc, with the
    corresponding expression, and the boolean is_pure. It is usefull for
    debuging purposes as it printing each of the Dcalc/Lcalc variable pairs. *)
let add_var
    (mark : 'm mark)
    (var : 'm D.expr Var.t)
    (is_pure : bool)
    (ctx : 'm ctx) : 'm ctx =
  let new_var = Var.make (Bindlib.name_of var) in
  let expr = Expr.make_var new_var mark in

  (* Cli.debug_print @@ Format.asprintf "D.%a |-> A.%a" Print.var var Print.var
     new_var; *)
  {
    ctx with
    vars =
      Var.Map.update var
        (fun _ -> Some { expr; var = new_var; is_pure })
        ctx.vars;
  }

(** [tau' = translate_typ tau] translate the a dcalc type into a lcalc type.

    Since positions where there is thunked expressions is exactly where we will
    put option expressions. Hence, the transformation simply reduce [unit -> 'a]
    into ['a option] recursivly. There is no polymorphism inside catala. *)
let rec translate_typ (tau : typ) : typ =
  (Fun.flip Marked.same_mark_as)
    tau
    begin
      match Marked.unmark tau with
      | TLit l -> TLit l
      | TTuple ts -> TTuple (List.map translate_typ ts)
      | TStruct s -> TStruct s
      | TEnum en -> TEnum en
      | TOption t -> TOption t
      | TAny -> TAny
      | TArray ts -> TArray (translate_typ ts)
      (* catala is not polymorphic *)
      | TArrow ([(TLit TUnit, _)], t2) -> TOption (translate_typ t2)
      | TArrow (t1, t2) -> TArrow (List.map translate_typ t1, translate_typ t2)
    end

(** [c = disjoint_union_maps cs] Compute the disjoint union of multiple maps.
    Raises an internal error if there is two identicals keys in differnts parts. *)
let disjoint_union_maps (pos : Pos.t) (cs : ('e, 'a) Var.Map.t list) :
    ('e, 'a) Var.Map.t =
  let disjoint_union =
    Var.Map.union (fun _ _ _ ->
        Errors.raise_spanned_error pos
          "Internal Error: Two supposed to be disjoints maps have one shared \
           key.")
  in

  List.fold_left disjoint_union Var.Map.empty cs

(** [e' = translate_and_hoist ctx e ] Translate the Dcalc expression e into an
    expression in Lcalc, given we translate each hoists correctly. It ensures
    the equivalence between the execution of e and the execution of e' are
    equivalent in an environement where each variable v, where (v, e_v) is in
    hoists, has the non-empty value in e_v. *)
let rec translate_and_hoist (ctx : 'm ctx) (e : 'm D.expr) :
    'm A.expr boxed * 'm hoists =
  let mark = Marked.get_mark e in
  let pos = Expr.mark_pos mark in
  match Marked.unmark e with
  (* empty-producing/using terms. We hoist those. (D.EVar in some cases,
     EApp(D.EVar _, [ELit LUnit]), EDefault _, ELit LEmptyDefault) I'm unsure
     about assert. *)
  | EVar v ->
    (* todo: for now, every unpure (such that [is_pure] is [false] in the
       current context) is thunked, hence matched in the next case. This
       assumption can change in the future, and this case is here for this
       reason. *)
    if not (find ~info:"search for a variable" v ctx).is_pure then
      let v' = Var.make (Bindlib.name_of v) in
      (* Cli.debug_print @@ Format.asprintf "Found an unpure variable %a,
         created a variable %a to replace it" Print.var v Print.var v'; *)
      Expr.make_var v' mark, Var.Map.singleton v' e
    else (find ~info:"should never happen" v ctx).expr, Var.Map.empty
  | EApp { f = EVar v, p; args = [(ELit LUnit, _)] } ->
    if not (find ~info:"search for a variable" v ctx).is_pure then
      let v' = Var.make (Bindlib.name_of v) in
      (* Cli.debug_print @@ Format.asprintf "Found an unpure variable %a,
         created a variable %a to replace it" Print.var v Print.var v'; *)
      Expr.make_var v' mark, Var.Map.singleton v' (EVar v, p)
    else
      Errors.raise_spanned_error (Expr.pos e)
        "Internal error: an pure variable was found in an unpure environment."
  | EDefault _ ->
    let v' = Var.make "default_term" in
    Expr.make_var v' mark, Var.Map.singleton v' e
  | ELit LEmptyError ->
    let v' = Var.make "empty_litteral" in
    Expr.make_var v' mark, Var.Map.singleton v' e
  (* This one is a very special case. It transform an unpure expression
     environement to a pure expression. *)
  | EErrorOnEmpty arg ->
    (* [ match arg with | None -> raise NoValueProvided | Some v -> {{ v }} ] *)
    let silent_var = Var.make "_" in
    let x = Var.make "non_empty_argument" in

    let arg' = translate_expr ctx arg in
    let rty = Expr.maybe_ty mark in

    ( A.make_matchopt_with_abs_arms arg'
        (Expr.make_abs [| silent_var |]
           (Expr.eraise NoValueProvided (Expr.with_ty mark rty))
           [rty] pos)
        (Expr.make_abs [| x |] (Expr.make_var x mark) [rty] pos),
      Var.Map.empty )
  (* pure terms *)
  | ELit
      ((LBool _ | LInt _ | LRat _ | LMoney _ | LUnit | LDate _ | LDuration _) as
      l) ->
    Expr.elit l mark, Var.Map.empty
  | EIfThenElse { cond; etrue; efalse } ->
    let cond', h1 = translate_and_hoist ctx cond in
    let etrue', h2 = translate_and_hoist ctx etrue in
    let efalse', h3 = translate_and_hoist ctx efalse in

    let e' = Expr.eifthenelse cond' etrue' efalse' mark in

    (*(* equivalent code : *) let e' = let+ cond' = cond' and+ etrue' = etrue'
      and+ efalse' = efalse' in (A.EIfThenElse (cond', etrue', efalse'), pos)
      in *)
    e', disjoint_union_maps (Expr.pos e) [h1; h2; h3]
  | EAssert e1 ->
    (* same behavior as in the ICFP paper: if e1 is empty, then no error is
       raised. *)
    let e1', h1 = translate_and_hoist ctx e1 in
    Expr.eassert e1' mark, h1
  | EAbs { binder; tys } ->
    let vars, body = Bindlib.unmbind binder in
    let ctx, lc_vars =
      ArrayLabels.fold_right vars ~init:(ctx, []) ~f:(fun var (ctx, lc_vars) ->
          (* we suppose the invariant that when applying a function, its
             arguments cannot be of the type "option".

             The code should behave correctly in the without this assumption if
             we put here an is_pure=false, but the types are more compilcated.
             (unimplemented for now) *)
          let ctx = add_var mark var true ctx in
          let lc_var = (find var ctx).var in
          ctx, lc_var :: lc_vars)
    in
    let lc_vars = Array.of_list lc_vars in

    (* here we take the guess that if we cannot build the closure because one of
       the variable is empty, then we cannot build the function. *)
    let new_body, hoists = translate_and_hoist ctx body in
    let new_binder = Expr.bind lc_vars new_body in

    Expr.eabs new_binder (List.map translate_typ tys) mark, hoists
  | EApp { f = e1; args } ->
    let e1', h1 = translate_and_hoist ctx e1 in
    let args', h_args =
      args |> List.map (translate_and_hoist ctx) |> List.split
    in

    let hoists = disjoint_union_maps (Expr.pos e) (h1 :: h_args) in
    let e' = Expr.eapp e1' args' mark in
    e', hoists
  | EStruct { name; fields } ->
    let fields', h_fields =
      StructField.Map.fold
        (fun field e (fields, hoists) ->
          let e, h = translate_and_hoist ctx e in
          StructField.Map.add field e fields, h :: hoists)
        fields
        (StructField.Map.empty, [])
    in
    let hoists = disjoint_union_maps (Expr.pos e) h_fields in
    Expr.estruct name fields' mark, hoists
  | EStructAccess { name; e = e1; field } ->
    let e1', hoists = translate_and_hoist ctx e1 in
    let e1' = Expr.estructaccess e1' field name mark in
    e1', hoists
  | ETuple es ->
    let hoists, es' =
      List.fold_left_map
        (fun hoists e ->
          let e, h = translate_and_hoist ctx e in
          h :: hoists, e)
        [] es
    in
    Expr.etuple es' mark, disjoint_union_maps (Expr.pos e) hoists
  | ETupleAccess { e = e1; index; size } ->
    let e1', hoists = translate_and_hoist ctx e1 in
    let e1' = Expr.etupleaccess e1' index size mark in
    e1', hoists
  | EInj { name; e = e1; cons } ->
    let e1', hoists = translate_and_hoist ctx e1 in
    let e1' = Expr.einj e1' cons name mark in
    e1', hoists
  | EMatch { name; e = e1; cases } ->
    let e1', h1 = translate_and_hoist ctx e1 in
    let cases', h_cases =
      EnumConstructor.Map.fold
        (fun cons e (cases, hoists) ->
          let e', h = translate_and_hoist ctx e in
          EnumConstructor.Map.add cons e' cases, h :: hoists)
        cases
        (EnumConstructor.Map.empty, [])
    in
    let hoists = disjoint_union_maps (Expr.pos e) (h1 :: h_cases) in
    let e' = Expr.ematch e1' name cases' mark in
    e', hoists
  | EArray es ->
    let es', hoists = es |> List.map (translate_and_hoist ctx) |> List.split in

    Expr.earray es' mark, disjoint_union_maps (Expr.pos e) hoists
  | EOp { op; tys } ->
    Expr.eop (Operator.translate None op) tys mark, Var.Map.empty

and translate_expr ?(append_esome = true) (ctx : 'm ctx) (e : 'm D.expr) :
    'm A.expr boxed =
  let e', hoists = translate_and_hoist ctx e in
  let hoists = Var.Map.bindings hoists in

  let _pos = Marked.get_mark e in

  (* build the hoists *)
  (* Cli.debug_print @@ Format.asprintf "hoist for the expression: [%a]"
     (Format.pp_print_list Print.var) (List.map fst hoists); *)
  ListLabels.fold_left hoists
    ~init:(if append_esome then A.make_some e' else e')
    ~f:(fun acc (v, (hoist, mark_hoist)) ->
      (* Cli.debug_print @@ Format.asprintf "hoist using A.%a" Print.var v; *)
      let pos = Expr.mark_pos mark_hoist in
      let c' : 'm A.expr boxed =
        match hoist with
        (* Here we have to handle only the cases appearing in hoists, as defined
           the [translate_and_hoist] function. *)
        | EVar v -> (find ~info:"should never happen" v ctx).expr
        | EDefault { excepts; just; cons } ->
          let excepts' = List.map (translate_expr ctx) excepts in
          let just' = translate_expr ctx just in
          let cons' = translate_expr ctx cons in
          (* calls handle_option. *)
          Expr.make_app
            (Expr.make_var (Var.translate A.handle_default_opt) mark_hoist)
            [Expr.earray excepts' mark_hoist; just'; cons']
            pos
        | ELit LEmptyError -> A.make_none mark_hoist
        | EAssert arg ->
          let arg' = translate_expr ctx arg in

          (* [ match arg with | None -> raise NoValueProvided | Some v -> assert
             {{ v }} ] *)
          let silent_var = Var.make "_" in
          let x = Var.make "assertion_argument" in

          A.make_matchopt_with_abs_arms arg'
            (Expr.make_abs [| silent_var |]
               (Expr.eraise NoValueProvided mark_hoist)
               [TAny, Expr.mark_pos mark_hoist]
               pos)
            (Expr.make_abs [| x |]
               (Expr.eassert (Expr.make_var x mark_hoist) mark_hoist)
               [TAny, Expr.mark_pos mark_hoist]
               pos)
        | _ ->
          Errors.raise_spanned_error (Expr.mark_pos mark_hoist)
            "Internal Error: An term was found in a position where it should \
             not be"
      in

      (* [ match {{ c' }} with | None -> None | Some {{ v }} -> {{ acc }} end
         ] *)
      (* Cli.debug_print @@ Format.asprintf "build matchopt using %a" Print.var
         v; *)
      A.make_matchopt pos v
        (TAny, Expr.mark_pos mark_hoist)
        c' (A.make_none mark_hoist) acc)

let rec translate_scope_let (ctx : 'm ctx) (lets : 'm D.expr scope_body_expr) :
    'm A.expr scope_body_expr Bindlib.box =
  match lets with
  | Result e ->
    Bindlib.box_apply
      (fun e -> Result e)
      (Expr.Box.lift (translate_expr ~append_esome:false ctx e))
  | ScopeLet
      {
        scope_let_kind = SubScopeVarDefinition;
        scope_let_typ = typ;
        scope_let_expr = EAbs { binder; _ }, emark;
        scope_let_next = next;
        scope_let_pos = pos;
      } ->
    (* special case : the subscope variable is thunked (context i/o). We remove
       this thunking. *)
    let _, expr = Bindlib.unmbind binder in

    let var_is_pure = true in
    let var, next = Bindlib.unbind next in
    (* Cli.debug_print @@ Format.asprintf "unbinding %a" Print.var var; *)
    let vmark = Expr.with_ty emark ~pos typ in
    let ctx' = add_var vmark var var_is_pure ctx in
    let new_var = (find ~info:"variable that was just created" var ctx').var in
    let new_next = translate_scope_let ctx' next in
    Bindlib.box_apply2
      (fun new_expr new_next ->
        ScopeLet
          {
            scope_let_kind = SubScopeVarDefinition;
            scope_let_typ = translate_typ typ;
            scope_let_expr = new_expr;
            scope_let_next = new_next;
            scope_let_pos = pos;
          })
      (Expr.Box.lift (translate_expr ctx ~append_esome:false expr))
      (Bindlib.bind_var new_var new_next)
  | ScopeLet
      {
        scope_let_kind = SubScopeVarDefinition;
        scope_let_typ = typ;
        scope_let_expr = (EErrorOnEmpty _, emark) as expr;
        scope_let_next = next;
        scope_let_pos = pos;
      } ->
    (* special case: regular input to the subscope *)
    let var_is_pure = true in
    let var, next = Bindlib.unbind next in
    (* Cli.debug_print @@ Format.asprintf "unbinding %a" Print.var var; *)
    let vmark = Expr.with_ty emark ~pos typ in
    let ctx' = add_var vmark var var_is_pure ctx in
    let new_var = (find ~info:"variable that was just created" var ctx').var in
    Bindlib.box_apply2
      (fun new_expr new_next ->
        ScopeLet
          {
            scope_let_kind = SubScopeVarDefinition;
            scope_let_typ = translate_typ typ;
            scope_let_expr = new_expr;
            scope_let_next = new_next;
            scope_let_pos = pos;
          })
      (Expr.Box.lift (translate_expr ctx ~append_esome:false expr))
      (Bindlib.bind_var new_var (translate_scope_let ctx' next))
  | ScopeLet
      {
        scope_let_kind = SubScopeVarDefinition;
        scope_let_pos = pos;
        scope_let_expr = expr;
        _;
      } ->
    Errors.raise_spanned_error pos
      "Internal Error: found an SubScopeVarDefinition that does not satisfy \
       the invariants when translating Dcalc to Lcalc without exceptions: \
       @[<hov 2>%a@]"
      (Expr.format ctx.decl_ctx) expr
  | ScopeLet
      {
        scope_let_kind = kind;
        scope_let_typ = typ;
        scope_let_expr = expr;
        scope_let_next = next;
        scope_let_pos = pos;
      } ->
    let var_is_pure =
      match kind with
      | DestructuringInputStruct -> (
        (* Here, we have to distinguish between context and input variables. We
           can do so by looking at the typ of the destructuring: if it's
           thunked, then the variable is context. If it's not thunked, it's a
           regular input. *)
        match Marked.unmark typ with
        | TArrow ([(TLit TUnit, _)], _) -> false
        | _ -> true)
      | ScopeVarDefinition | SubScopeVarDefinition | CallingSubScope
      | DestructuringSubScopeResults | Assertion ->
        true
    in
    let var, next = Bindlib.unbind next in
    (* Cli.debug_print @@ Format.asprintf "unbinding %a" Print.var var; *)
    let vmark = Expr.with_ty (Marked.get_mark expr) ~pos typ in
    let ctx' = add_var vmark var var_is_pure ctx in
    let new_var = (find ~info:"variable that was just created" var ctx').var in
    Bindlib.box_apply2
      (fun new_expr new_next ->
        ScopeLet
          {
            scope_let_kind = kind;
            scope_let_typ = translate_typ typ;
            scope_let_expr = new_expr;
            scope_let_next = new_next;
            scope_let_pos = pos;
          })
      (Expr.Box.lift (translate_expr ctx ~append_esome:false expr))
      (Bindlib.bind_var new_var (translate_scope_let ctx' next))

let translate_scope_body
    (scope_pos : Pos.t)
    (ctx : 'm ctx)
    (body : 'm D.expr scope_body) : 'm A.expr scope_body Bindlib.box =
  match body with
  | {
   scope_body_expr = result;
   scope_body_input_struct = input_struct;
   scope_body_output_struct = output_struct;
  } ->
    let v, lets = Bindlib.unbind result in
    let vmark =
      let m =
        match lets with
        | Result e | ScopeLet { scope_let_expr = e; _ } -> Marked.get_mark e
      in
      Expr.map_mark (fun _ -> scope_pos) (fun ty -> ty) m
    in
    let ctx' = add_var vmark v true ctx in
    let v' = (find ~info:"variable that was just created" v ctx').var in
    Bindlib.box_apply
      (fun new_expr ->
        {
          scope_body_expr = new_expr;
          scope_body_input_struct = input_struct;
          scope_body_output_struct = output_struct;
        })
      (Bindlib.bind_var v' (translate_scope_let ctx' lets))

let translate_code_items (ctx : 'm ctx) (scopes : 'm D.expr code_item_list) :
    'm A.expr code_item_list Bindlib.box =
  let _ctx, scopes =
    Scope.fold_map
      ~f:
        (fun ctx var -> function
          | Topdef (name, ty, e) ->
            ( add_var (Marked.get_mark e) var true ctx,
              Bindlib.box_apply
                (fun e -> Topdef (name, ty, e))
                (Expr.Box.lift (translate_expr ~append_esome:false ctx e)) )
          | ScopeDef (scope_name, scope_body) ->
            ( ctx,
              let scope_pos = Marked.get_mark (ScopeName.get_info scope_name) in
              Bindlib.box_apply
                (fun body -> ScopeDef (scope_name, body))
                (translate_scope_body scope_pos ctx scope_body) ))
      ~varf:Var.translate ctx scopes
  in
  scopes

let translate_program (prgm : 'm D.program) : 'm A.program =
  let inputs_structs =
    Scope.fold_left prgm.code_items ~init:[] ~f:(fun acc def _ ->
        match def with
        | ScopeDef (_, body) -> body.scope_body_input_struct :: acc
        | Topdef _ -> acc)
  in
  (* Cli.debug_print @@ Format.asprintf "List of structs to modify: [%a]"
     (Format.pp_print_list D.StructName.format_t) inputs_structs; *)
  let decl_ctx =
    {
      prgm.decl_ctx with
      ctx_enums =
        prgm.decl_ctx.ctx_enums
        |> EnumName.Map.add A.option_enum A.option_enum_config;
    }
  in
  let decl_ctx =
    {
      decl_ctx with
      ctx_structs =
        prgm.decl_ctx.ctx_structs
        |> StructName.Map.mapi (fun n str ->
               if List.mem n inputs_structs then
                 StructField.Map.map translate_typ str
                 (* Cli.debug_print @@ Format.asprintf "Input type: %a"
                    (Print.typ decl_ctx) tau; Cli.debug_print @@ Format.asprintf
                    "Output type: %a" (Print.typ decl_ctx) (translate_typ
                    tau); *)
               else str);
    }
  in

  let code_items =
    Bindlib.unbox
      (translate_code_items { decl_ctx; vars = Var.Map.empty } prgm.code_items)
  in

  { code_items; decl_ctx }
