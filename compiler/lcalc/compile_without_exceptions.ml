(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
module D = Dcalc.Ast
module A = Ast

type info = {expr: A.expr Pos.marked Bindlib.box; var: A.expr Bindlib.var; is_pure: bool}
type ctx = info D.VarMap.t 

let translate_lit (l : D.lit) : A.expr =
  let build lit =
    fst @@ Bindlib.unbox @@ A.make_some (Bindlib.box (Pos.mark Pos.no_pos (A.ELit lit)))
  in
  match l with
  | D.LBool l -> build (A.LBool l)
  | D.LInt i -> build (A.LInt i)
  | D.LRat r -> build (A.LRat r)
  | D.LMoney m -> build (A.LMoney m)
  | D.LUnit -> build A.LUnit
  | D.LDate d -> build (A.LDate d)
  | D.LDuration d -> build (A.LDuration d)
  | D.LEmptyError -> fst @@ Bindlib.unbox @@ A.make_none Pos.no_pos

let ( let+ ) x f = Bindlib.box_apply f x

let ( and+ ) x y = Bindlib.box_pair x y

let thunk_expr (e : A.expr Pos.marked Bindlib.box) (pos : Pos.t) : A.expr Pos.marked Bindlib.box =
  let dummy_var = A.Var.make ("_", pos) in
  A.make_abs [| dummy_var |] e pos [ (D.TAny, pos) ] pos


let add_var pos var is_pure ctx =
  let new_var = A.Var.make (Bindlib.name_of var, pos) in
  let expr = A.make_var (new_var, pos) in
  D.VarMap.add var {expr; var=new_var; is_pure} ctx

let rec translate_default (ctx : ctx) (exceptions : D.expr Pos.marked list)
    (just : D.expr Pos.marked) (cons : D.expr Pos.marked) (pos_default : Pos.t) :
    A.expr Pos.marked Bindlib.box =
  let exceptions = List.map (fun except -> translate_expr ctx except) exceptions in
  let exceptions =
    A.make_app
      (A.make_var (A.handle_default, pos_default))
      [
        Bindlib.box_apply
          (fun exceptions -> (A.EArray exceptions, pos_default))
          (Bindlib.box_list exceptions);
        thunk_expr (translate_expr ctx just) pos_default;
        thunk_expr (translate_expr ctx cons) pos_default;
      ]
      pos_default
  in
  exceptions

and translate_typ (t : D.typ Pos.marked) : D.typ Pos.marked =
  (* Hack: If the type is D.TAny, it means for the compiler to not put any type annotation.*)
  Pos.same_pos_as D.TAny t

and translate_binder (ctx: ctx) ((binder, pos_binder): (D.expr, D.expr Pos.marked) Bindlib.mbinder Pos.marked): (A.expr, A.expr Pos.marked) Bindlib.mbinder Pos.marked Bindlib.box =

  let vars, body = Bindlib.unmbind binder in
  let ctx, lc_vars =
    Array.fold_right
      begin fun var (ctx, lc_vars) ->
        let lc_var = A.Var.make (Bindlib.name_of var, pos_binder) in
        let lc_var_expr = A.make_var (lc_var, pos_binder) in
        let new_ctx = D.VarMap.add var {expr=lc_var_expr; is_pure= false; var= lc_var} ctx in
        (new_ctx, lc_var :: lc_vars) end
      vars (ctx, [])
  in
  let lc_vars = Array.of_list lc_vars in
  let new_body = translate_expr ctx body in
  let+ binder = Bindlib.bind_mvar lc_vars new_body in
  (binder, pos_binder)

and translate_expr (ctx : ctx) (e : D.expr Pos.marked) : A.expr Pos.marked Bindlib.box =
  let same_pos e' = Pos.same_pos_as e' e in
  match Pos.unmark e with
  | D.EVar v ->

    let info = D.VarMap.find (Pos.unmark v) ctx in
    if info.is_pure then
      A.make_some info.expr
    else
      info.expr
    
  | D.ETuple (args, s) ->
      let+ args = Bindlib.box_list (List.map (translate_expr ctx) args) in
      Pos.same_pos_as (A.ETuple (args, s)) e
  | D.ETupleAccess (e1, i, s, ts) ->
      let e1 = translate_expr ctx e1 in

      let pos = Pos.get_position (Bindlib.unbox e1) in
      let tau = (D.TAny, pos) in

      let new_e new_e1 =
        let+ new_e1 = new_e1 in
        same_pos @@ A.ETupleAccess (new_e1, i, s, ts)
      in

      A.make_bindopt pos tau e1 new_e

  | D.EInj (e1, i, en, ts) ->
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let tau = (D.TAny, pos) in

      let new_e new_e1 =
        let+ new_e1 = new_e1 in
        same_pos @@ A.EInj (new_e1, i, en, ts)
      in

      A.make_bindopt pos tau e1 new_e

  | D.EMatch (e1, cases, en) ->
    let e1 = translate_expr ctx e1 in
    let pos = Pos.get_position (Bindlib.unbox e1) in
    let tau = (D.TAny, pos) in

    let new_e new_e1 =
      let+ new_e1 = new_e1
      and+ cases =
        cases
        |> List.map (fun (e', _pos) ->
            match e' with
            | D.EAbs (binder, ts) ->
              let+ new_binder = translate_binder ctx binder in
                same_pos @@ A.EAbs (new_binder, List.map translate_typ ts)
            | _ -> Errors.raise_spanned_error "Internal error: an error occured during the translation of a amtch." (Pos.get_position e))
        |> Bindlib.box_list
      in
        if (List.for_all (fun (x, _) -> match x with A.EAbs _ -> true | _ -> false) cases) then
          Errors.raise_spanned_error "Internal error: an error occured during the translation of a match." (Pos.get_position e);

        same_pos @@ A.EMatch (new_e1, cases, en)
      in

      A.make_bindopt pos tau e1 new_e

  | D.EArray es ->
      let+ es = es |> List.map (translate_expr ctx) |> Bindlib.box_list in
      same_pos @@ A.make_some' (same_pos @@ A.EArray es)

  | D.ELit l -> Bindlib.box @@ same_pos @@ translate_lit l
  | D.EOp _op ->
    Errors.raise_spanned_error "Internal error: partial application of generic operator are not yet supported when using --avoid_exception." (Pos.get_position e)
  | D.EApp((D.EOp op, pos), args) ->
    begin
      let xs = List.mapi (fun i arg -> A.Var.make (Printf.sprintf "x_%d" i, Pos.get_position arg)) args in

      let dummy = A.Var.make ("unit", pos) in

      let e' final = args
      |> List.map (translate_expr ctx)
      |> List.combine xs
      |> List.fold_left (fun acc (x, arg) ->
        A.make_matchopt
          arg
          (A.make_abs (Array.of_list [dummy]) (A.make_none pos) (pos) [D.TLit D.TUnit, pos] pos)
          (A.make_abs (Array.of_list [x]) acc pos [D.TAny, pos] pos)
        ) final
      in

      let new_e =
        let+ args_var = xs
          |> List.map (fun x -> Bindlib.box_var x)
          |> Bindlib.box_list
        in

        let args_var = args_var
          |> List.combine args
          |> List.map (fun (arg, x) -> Pos.same_pos_as x arg)
        in
        same_pos @@ A.make_some' @@ same_pos @@ A.EApp ((A.EOp op, pos), args_var)
      in

      e' new_e
    end

  | D.EIfThenElse (e1, e2, e3) ->
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let tau = (D.TAny, pos) in



      let new_e new_e1 =
        let+ e1_new = new_e1
        and+ e2 = translate_expr ctx e2
        and+ e3 = translate_expr ctx e3 in
        same_pos @@ A.EIfThenElse (e1_new, e2, e3)
      in

      A.make_bindopt pos tau e1 new_e

  | D.EAssert e1 ->
      (* don't know the semantic of EAssert. *)
      (* Bindlib.box_apply (fun e1 -> Pos.same_pos_as (A.EAssert e1) e) (translate_expr ctx e1) *)
      let e1 = translate_expr ctx e1 in
      let pos = Pos.get_position (Bindlib.unbox e1) in
      let tau = (D.TAny, pos) in

      let new_e new_e1 =
        let+ e1_new = new_e1 in
        same_pos @@ A.EAssert e1_new
      in

      A.make_bindopt pos tau e1 new_e

      | D.EApp (e1, args) ->
    let e1 = translate_expr ctx e1 in
    let pos = Pos.get_position (Bindlib.unbox e1) in
    let tau = (D.TAny, pos) in

    let new_e new_e1 =
      let+ new_e1 = new_e1
      and+ args = args
        |> List.map (translate_expr ctx)
        |> Bindlib.box_list
      in
      same_pos @@ A.EApp (new_e1, args)
    in

    A.make_bindopt pos tau e1 new_e

  | D.EAbs (binder, ts) ->
      let+ new_binder = translate_binder ctx binder in
      same_pos
      @@ A.make_some' (same_pos @@ A.EAbs (new_binder, List.map translate_typ ts))
  | D.EDefault (exceptions, just, cons) ->
      translate_default ctx exceptions just cons (Pos.get_position e)

  | D.ErrorOnEmpty _ ->

    Errors.raise_spanned_error "Internal error: Error on empty found in incorrect place when compiling using the --avoid_exception option." (Pos.get_position e)


let rec translate_scope_vardefinition ctx expr: A.expr Pos.marked Bindlib.box =
  match expr with

  | D.ErrorOnEmpty arg, pos_expr ->
  begin
    (* ~> match [| arg |] with None -> raise NoValueProvided | Some x -> x *)
    let pos_arg = Pos.get_position arg in
    let x = A.Var.make ("result", pos_arg) in
    let arg = translate_expr ctx arg in

    let tau = (D.TAny, pos_arg) in

    let e3 =
      A.make_abs
        (Array.of_list [ x ])
        (let+ v = Bindlib.box_var x in (v, pos_arg))
        pos_arg [ tau ] pos_arg
    and e1 = arg
    and e2 =
      A.make_abs
        (Array.of_list [ x ])
        (Bindlib.box @@ (A.ERaise A.NoValueProvided, pos_expr))
        pos_arg [ tau ] pos_arg
    in

    A.make_matchopt e1 e2 e3
  end

  | D.EApp((D.EOp (D.Unop (D.Log (le, l))), pos_log), [e']), pos ->

    let+ e' = translate_scope_vardefinition ctx e' in
    A.EApp((A.EOp (D.Unop (D.Log (le, l))), pos_log), [e']), pos

  | (expr, pos) ->

    Errors.raise_spanned_error (Printf.sprintf "Internal error: Found unexpected expression when compiling an expression using the --avoid_exception option. ''Full'' term: %s" (D.show_expr expr)) pos


let translate_scope_let (ctx: ctx) (s: D.scope_let) : ctx * A.expr Pos.marked Bindlib.box =

  match s with {
    D.scope_let_var = var;
    D.scope_let_kind = kind;
    D.scope_let_typ = typ;
    D.scope_let_expr = expr;
  } -> begin

  (* I need to match on the expression. *)
  let expr' : A.expr Pos.marked Bindlib.box =
    let expr = Bindlib.unbox expr in

    let same_pos e' = Pos.same_pos_as e' expr in
    match kind, typ, expr with
    | ScopeVarDefinition, _typ, expr -> 
      translate_scope_vardefinition ctx expr
    | Assertion, _typ, expr -> begin
      let pos = Pos.get_position expr in
      let x = A.Var.make ("result", pos) in
      let arg = translate_expr ctx expr in

      let tau = (D.TAny, pos) in

      let e3 =
        A.make_abs
          (Array.of_list [ x ])
          (let+ v = Bindlib.box_var x in (v, pos))
          pos [ tau ] pos
      and e1 = arg
      and e2 =
        A.make_abs
          (Array.of_list [ x ])
          (Bindlib.box @@ same_pos @@ A.ERaise A.NoValueProvided)
          pos [ tau ] pos
      in

      A.make_matchopt e1 e2 e3
    end
    | SubScopeVarDefinition, _typ, (D.EAbs ((binder, pos_binder), _tau), pos) ->
      begin
      let vs, body = Bindlib.unmbind binder in

      let vs' = Array.map (fun v -> (D.VarMap.find v ctx).var) vs in

      let body' = translate_expr ctx body in

      (* there is no need to add the binded var to the context since we know it is thunked *)
      A.make_abs vs' body' pos_binder [D.TAny, pos_binder] pos
    end

    | DestructuringInputStruct, _typ, expr ->
      translate_expr ctx expr

    | DestructuringSubScopeResults, _typ, expr ->
      translate_expr ctx expr

    | CallingSubScope, _typ, expr ->
      translate_expr ctx expr

    


    | kind, _typ, (expr, pos) ->

      let kind_s = match kind with
      | ScopeVarDefinition -> "ScopeVarDefinition"
      | Assertion -> "Assertion"
      | SubScopeVarDefinition -> "SubScopeVarDefinition"
      | DestructuringInputStruct -> "DestructuringInputStruct"
      | DestructuringSubScopeResults -> "DestructuringSubScopeResults"
      | CallingSubScope -> "CallingSubScope" in

      let expr_s =  match expr with
      | EVar _ -> "EVar"
      | ETuple _ -> "ETuple"
      | ETupleAccess _ -> "ETupleAccess"
      | EInj _ -> "EInj"
      | EMatch _ -> "EMatch"
      | EArray _ -> "EArray"
      | ELit _ -> "ELit"
      | EAbs _ -> "EAbs"
      | EApp _ -> "EApp"
      | EAssert _ -> "EAssert"
      | EOp _ -> "EOp"
      | EDefault _ -> "EDefault"
      | EIfThenElse _ -> "EIfThenElse"
      | ErrorOnEmpty _ -> "ErrorOnEmpty"
      in
 
      Errors.raise_spanned_error (Printf.sprintf "Internal error: Found unexpected %s when compiling an expression containing %s using the --avoid_exception option. ''Full'' term: %s" kind_s expr_s (D.show_expr expr)) pos
  in

  let is_pure = match kind with
  | ScopeVarDefinition -> true
  | Assertion -> true
  | SubScopeVarDefinition -> true
  | DestructuringInputStruct -> true
  | DestructuringSubScopeResults -> true
  | CallingSubScope -> false
  in

  let ctx' = add_var (snd var) (fst var) is_pure ctx in

  (ctx', expr')

  end

let translate_scope_body (ctx: ctx) (s: D.scope_body): A.expr Pos.marked Bindlib.box =
match s with {
  D.scope_body_lets=lets;
  D.scope_body_result=result;
  D.scope_body_arg=arg;
  _
} -> begin

  (* first we add to the input the ctx *)
  let ctx1 = add_var Pos.no_pos arg true ctx in

  (* then, we compute the lets bindings and modification to the ctx *)
  (* todo: once we update to ocaml 4.11, use fold_left_map instead of fold_left + List.rev *)
  let ctx2, acc = ListLabels.fold_left lets
    ~init:(ctx1, [])
    ~f:begin fun (ctx, acc) (s: D.scope_let) ->
      let ctx, e = translate_scope_let ctx s in
      (ctx, (s.scope_let_var, D.TAny, e)::acc)
    end
  in
  let acc = List.rev acc in

  (* we now have the context for the final transformation: the result *)
  (* todo: alaid, result is boxed and hence incompatible with translate_expr... *)
  let result = translate_expr ctx2 (Bindlib.unbox result) in

  (* finally, we can recombine everything using nested let ... = ... in *)
  let body =
    ListLabels.fold_left acc
    ~init:result
    ~f:(fun (body: (A.expr * Pos.t) Bindlib.box) ((v, pos), tau, e) ->
      A.make_let_in (D.VarMap.find v ctx2).var (tau, pos) e body
    )
  in
  
  
  (* we finnally rebuild the binder *)

  A.make_abs (Array.of_list [(D.VarMap.find arg ctx1).var]) body Pos.no_pos [D.TAny, Pos.no_pos] Pos.no_pos
end


let translate_program (prgm : D.program) : A.program =
  let new_scopes = (prgm.scopes : (D.ScopeName.t * D.expr Bindlib.var * D.scope_body) list)
    |> ListLabels.fold_left
      ~init:([], D.VarMap.empty)
      ~f:begin fun (acc, ctx) (_, n, e) ->

        let env: ctx = D.VarMap.map (fun v ->
          let new_var = A.Var.make (Bindlib.name_of v, Pos.no_pos) in
          let expr = A.make_var (new_var, Pos.no_pos) in
          {expr; var=new_var; is_pure=true}
        ) ctx in

        let new_n = A.Var.make (Bindlib.name_of n, Pos.no_pos) in
        let new_e = translate_scope_body env e in

        let new_acc = (new_n, Bindlib.unbox new_e) :: acc in
        let new_ctx = D.VarMap.add n new_n ctx in

        (new_acc, new_ctx)
      end
    |> fst
    |> List.rev
  in
  {
    scopes = new_scopes;
    decl_ctx =
      {
        ctx_enums = prgm.decl_ctx.ctx_enums |> D.EnumMap.add A.option_enum A.option_enum_config;
        ctx_structs = prgm.decl_ctx.ctx_structs;
      };
  }


