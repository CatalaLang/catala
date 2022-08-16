(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Shared_ast

type lit = dcalc glit

type 'm expr = (dcalc, 'm mark) gexpr
and 'm marked_expr = (dcalc, 'm mark) marked_gexpr

type 'm program = 'm expr Shared_ast.program
type 'e box_expr_sig = 'e marked -> 'e marked Bindlib.box
type 'm var = 'm expr Var.t
type 'm vars = 'm expr Var.vars

let rec free_vars_expr (e : 'm marked_expr) : 'm expr Var.Set.t =
  match Marked.unmark e with
  | EVar v -> Var.Set.singleton v
  | ETuple (es, _) | EArray es ->
    es |> List.map free_vars_expr |> List.fold_left Var.Set.union Var.Set.empty
  | ETupleAccess (e1, _, _, _)
  | EAssert e1
  | ErrorOnEmpty e1
  | EInj (e1, _, _, _) ->
    free_vars_expr e1
  | EApp (e1, es) | EMatch (e1, es, _) ->
    e1 :: es
    |> List.map free_vars_expr
    |> List.fold_left Var.Set.union Var.Set.empty
  | EDefault (es, ejust, econs) ->
    ejust :: econs :: es
    |> List.map free_vars_expr
    |> List.fold_left Var.Set.union Var.Set.empty
  | EOp _ | ELit _ -> Var.Set.empty
  | EIfThenElse (e1, e2, e3) ->
    [e1; e2; e3]
    |> List.map free_vars_expr
    |> List.fold_left Var.Set.union Var.Set.empty
  | EAbs (binder, _) ->
    let vs, body = Bindlib.unmbind binder in
    Array.fold_right Var.Set.remove vs (free_vars_expr body)

let rec free_vars_scope_body_expr (scope_lets : 'm expr scope_body_expr) :
    'm expr Var.Set.t =
  match scope_lets with
  | Result e -> free_vars_expr e
  | ScopeLet { scope_let_expr = e; scope_let_next = next; _ } ->
    let v, body = Bindlib.unbind next in
    Var.Set.union (free_vars_expr e)
      (Var.Set.remove v (free_vars_scope_body_expr body))

let free_vars_scope_body (scope_body : 'm expr scope_body) : 'm expr Var.Set.t =
  let { scope_body_expr = binder; _ } = scope_body in
  let v, body = Bindlib.unbind binder in
  Var.Set.remove v (free_vars_scope_body_expr body)

let rec free_vars_scopes (scopes : 'm expr scopes) : 'm expr Var.Set.t =
  match scopes with
  | Nil -> Var.Set.empty
  | ScopeDef { scope_body = body; scope_next = next; _ } ->
    let v, next = Bindlib.unbind next in
    Var.Set.union
      (Var.Set.remove v (free_vars_scopes next))
      (free_vars_scope_body body)

let make_var ((x, mark) : ('m expr Bindlib.var, 'm mark) Marked.t) :
    'm marked_expr Bindlib.box =
  Bindlib.box_apply (fun x -> x, mark) (Bindlib.box_var x)

type 'e make_abs_sig =
  'e Bindlib.mvar ->
  'e marked Bindlib.box ->
  typ Marked.pos list ->
  'm mark ->
  'e marked Bindlib.box
  constraint 'e = ('a, 'm mark) gexpr

let (make_abs : 'm expr make_abs_sig) =
 fun xs e taus mark ->
  Bindlib.box_apply (fun b -> EAbs (b, taus), mark) (Bindlib.bind_mvar xs e)

let make_app :
    'm marked_expr Bindlib.box ->
    'm marked_expr Bindlib.box list ->
    'm mark ->
    'm marked_expr Bindlib.box =
 fun e u mark ->
  Bindlib.box_apply2 (fun e u -> EApp (e, u), mark) e (Bindlib.box_list u)

type 'e make_let_in_sig =
  'e Bindlib.var ->
  marked_typ ->
  'e marked Bindlib.box ->
  'e marked Bindlib.box ->
  Pos.t ->
  'e marked Bindlib.box

let empty_thunked_term mark : 'm marked_expr =
  let silent = Var.make "_" in
  let pos = Expr.mark_pos mark in
  Bindlib.unbox
    (make_abs [| silent |]
       (Bindlib.box (ELit LEmptyError, mark))
       [TLit TUnit, pos]
       (Expr.map_mark
          (fun pos -> pos)
          (fun ty ->
            Marked.mark pos (TArrow (Marked.mark pos (TLit TUnit), ty)))
          mark))

let (make_let_in : 'm expr make_let_in_sig) =
 fun x tau e1 e2 pos ->
  let m_e1 = Marked.get_mark (Bindlib.unbox e1) in
  let m_e2 = Marked.get_mark (Bindlib.unbox e2) in
  let m_abs =
    Expr.map_mark2
      (fun _ _ -> pos)
      (fun m1 m2 -> Marked.mark pos (TArrow (m1.ty, m2.ty)))
      m_e1 m_e2
  in
  make_app (make_abs [| x |] e2 [tau] m_abs) [e1] m_e2

let is_value (e : 'e marked_expr) : bool =
  match Marked.unmark e with ELit _ | EAbs _ | EOp _ -> true | _ -> false

let rec equal_typs (ty1 : typ Marked.pos) (ty2 : typ Marked.pos) : bool =
  match Marked.unmark ty1, Marked.unmark ty2 with
  | TLit l1, TLit l2 -> l1 = l2
  | TTuple (tys1, n1), TTuple (tys2, n2) -> n1 = n2 && equal_typs_list tys1 tys2
  | TEnum (tys1, n1), TEnum (tys2, n2) -> n1 = n2 && equal_typs_list tys1 tys2
  | TArrow (t1, t1'), TArrow (t2, t2') -> equal_typs t1 t2 && equal_typs t1' t2'
  | TArray t1, TArray t2 -> equal_typs t1 t2
  | TAny, TAny -> true
  | _, _ -> false

and equal_typs_list (tys1 : typ Marked.pos list) (tys2 : typ Marked.pos list) :
    bool =
  List.length tys1 = List.length tys2
  && (* OCaml && operator short-circuits when a clause is false, we can safely
        assume here that both lists have equal length *)
  List.for_all (fun (x, y) -> equal_typs x y) (List.combine tys1 tys2)

let equal_log_entries (l1 : log_entry) (l2 : log_entry) : bool =
  match l1, l2 with
  | VarDef t1, VarDef t2 -> equal_typs (t1, Pos.no_pos) (t2, Pos.no_pos)
  | x, y -> x = y

let equal_unops (op1 : unop) (op2 : unop) : bool =
  match op1, op2 with
  (* Log entries contain a typ which contain position information, we thus need
     to descend into them *)
  | Log (l1, info1), Log (l2, info2) -> equal_log_entries l1 l2 && info1 = info2
  (* All the other cases can be discharged through equality *)
  | _ -> op1 = op2

let equal_ops (op1 : operator) (op2 : operator) : bool =
  match op1, op2 with
  | Ternop op1, Ternop op2 -> op1 = op2
  | Binop op1, Binop op2 -> op1 = op2
  | Unop op1, Unop op2 -> equal_unops op1 op2
  | _, _ -> false

let rec equal_exprs (e1 : 'm marked_expr) (e2 : 'm marked_expr) : bool =
  match Marked.unmark e1, Marked.unmark e2 with
  | EVar v1, EVar v2 -> Bindlib.eq_vars v1 v2
  | ETuple (es1, n1), ETuple (es2, n2) -> n1 = n2 && equal_exprs_list es1 es2
  | ETupleAccess (e1, id1, n1, tys1), ETupleAccess (e2, id2, n2, tys2) ->
    equal_exprs e1 e2 && id1 = id2 && n1 = n2 && equal_typs_list tys1 tys2
  | EInj (e1, id1, n1, tys1), EInj (e2, id2, n2, tys2) ->
    equal_exprs e1 e2 && id1 = id2 && n1 = n2 && equal_typs_list tys1 tys2
  | EMatch (e1, cases1, n1), EMatch (e2, cases2, n2) ->
    n1 = n2 && equal_exprs e1 e2 && equal_exprs_list cases1 cases2
  | EArray es1, EArray es2 -> equal_exprs_list es1 es2
  | ELit l1, ELit l2 -> l1 = l2
  | EAbs (b1, tys1), EAbs (b2, tys2) ->
    equal_typs_list tys1 tys2
    &&
    let vars1, body1 = Bindlib.unmbind b1 in
    let body2 = Bindlib.msubst b2 (Array.map (fun x -> EVar x) vars1) in
    equal_exprs body1 body2
  | EAssert e1, EAssert e2 -> equal_exprs e1 e2
  | EOp op1, EOp op2 -> equal_ops op1 op2
  | EDefault (exc1, def1, cons1), EDefault (exc2, def2, cons2) ->
    equal_exprs def1 def2
    && equal_exprs cons1 cons2
    && equal_exprs_list exc1 exc2
  | EIfThenElse (if1, then1, else1), EIfThenElse (if2, then2, else2) ->
    equal_exprs if1 if2 && equal_exprs then1 then2 && equal_exprs else1 else2
  | ErrorOnEmpty e1, ErrorOnEmpty e2 -> equal_exprs e1 e2
  | _, _ -> false

and equal_exprs_list (es1 : 'e marked_expr list) (es2 : 'm marked_expr list) :
    bool =
  List.length es1 = List.length es2
  && (* OCaml && operator short-circuits when a clause is false, we can safely
        assume here that both lists have equal length *)
  List.for_all (fun (x, y) -> equal_exprs x y) (List.combine es1 es2)

let rec unfold_scope_body_expr
    ~(box_expr : 'e box_expr_sig)
    ~(make_let_in : 'e make_let_in_sig)
    (ctx : decl_ctx)
    (scope_let : 'e scope_body_expr) : 'e marked Bindlib.box =
  match scope_let with
  | Result e -> box_expr e
  | ScopeLet
      {
        scope_let_kind = _;
        scope_let_typ;
        scope_let_expr;
        scope_let_next;
        scope_let_pos;
      } ->
    let var, next = Bindlib.unbind scope_let_next in
    make_let_in var scope_let_typ (box_expr scope_let_expr)
      (unfold_scope_body_expr ~box_expr ~make_let_in ctx next)
      scope_let_pos

let build_whole_scope_expr
    ~(box_expr : 'e box_expr_sig)
    ~(make_abs : 'e make_abs_sig)
    ~(make_let_in : 'e make_let_in_sig)
    (ctx : decl_ctx)
    (body : 'e scope_body)
    (mark_scope : 'm mark) : 'e marked Bindlib.box =
  let var, body_expr = Bindlib.unbind body.scope_body_expr in
  let body_expr = unfold_scope_body_expr ~box_expr ~make_let_in ctx body_expr in
  make_abs (Array.of_list [var]) body_expr
    [
      ( TTuple
          ( List.map snd
              (StructMap.find body.scope_body_input_struct ctx.ctx_structs),
            Some body.scope_body_input_struct ),
        Expr.mark_pos mark_scope );
    ]
    mark_scope

let build_scope_typ_from_sig
    (ctx : decl_ctx)
    (scope_input_struct_name : StructName.t)
    (scope_return_struct_name : StructName.t)
    (pos : Pos.t) : typ Marked.pos =
  let scope_sig = StructMap.find scope_input_struct_name ctx.ctx_structs in
  let scope_return_typ =
    StructMap.find scope_return_struct_name ctx.ctx_structs
  in
  let result_typ =
    TTuple (List.map snd scope_return_typ, Some scope_return_struct_name), pos
  in
  let input_typ =
    TTuple (List.map snd scope_sig, Some scope_input_struct_name), pos
  in
  TArrow (input_typ, result_typ), pos

type 'expr scope_name_or_var =
  | ScopeName of ScopeName.t
  | ScopeVar of 'expr Bindlib.var

let rec unfold_scopes
    ~(box_expr : 'e box_expr_sig)
    ~(make_abs : 'e make_abs_sig)
    ~(make_let_in : 'e make_let_in_sig)
    (ctx : decl_ctx)
    (s : 'e scopes)
    (mark : 'm mark)
    (main_scope : 'expr scope_name_or_var) : 'e marked Bindlib.box =
  match s with
  | Nil -> (
    match main_scope with
    | ScopeVar v -> Bindlib.box_apply (fun v -> v, mark) (Bindlib.box_var v)
    | ScopeName _ -> failwith "should not happen")
  | ScopeDef { scope_name; scope_body; scope_next } ->
    let scope_var, scope_next = Bindlib.unbind scope_next in
    let scope_pos = Marked.get_mark (ScopeName.get_info scope_name) in
    let scope_body_mark = Expr.get_scope_body_mark scope_body in
    let main_scope =
      match main_scope with
      | ScopeVar v -> ScopeVar v
      | ScopeName n ->
        if ScopeName.compare n scope_name = 0 then ScopeVar scope_var
        else ScopeName n
    in
    make_let_in scope_var
      (build_scope_typ_from_sig ctx scope_body.scope_body_input_struct
         scope_body.scope_body_output_struct scope_pos)
      (build_whole_scope_expr ~box_expr ~make_abs ~make_let_in ctx scope_body
         scope_body_mark)
      (unfold_scopes ~box_expr ~make_abs ~make_let_in ctx scope_next mark
         main_scope)
      scope_pos

let rec find_scope name vars = function
  | Nil -> raise Not_found
  | ScopeDef { scope_name; scope_body; _ } when scope_name = name ->
    List.rev vars, scope_body
  | ScopeDef { scope_next; _ } ->
    let var, next = Bindlib.unbind scope_next in
    find_scope name (var :: vars) next

let build_whole_program_expr
    ~(box_expr : 'e box_expr_sig)
    ~(make_abs : 'e make_abs_sig)
    ~(make_let_in : 'e make_let_in_sig)
    (p : 'e Shared_ast.program)
    (main_scope : ScopeName.t) : 'e marked Bindlib.box =
  let _, main_scope_body = find_scope main_scope [] p.scopes in
  unfold_scopes ~box_expr ~make_abs ~make_let_in p.decl_ctx p.scopes
    (Expr.get_scope_body_mark main_scope_body)
    (ScopeName main_scope)

let rec expr_size (e : 'm marked_expr) : int =
  match Marked.unmark e with
  | EVar _ | ELit _ | EOp _ -> 1
  | ETuple (args, _) | EArray args ->
    List.fold_left (fun acc arg -> acc + expr_size arg) 1 args
  | ETupleAccess (e1, _, _, _)
  | EInj (e1, _, _, _)
  | EAssert e1
  | ErrorOnEmpty e1 ->
    expr_size e1 + 1
  | EMatch (arg, args, _) | EApp (arg, args) ->
    List.fold_left (fun acc arg -> acc + expr_size arg) (1 + expr_size arg) args
  | EAbs (binder, _) ->
    let _, body = Bindlib.unmbind binder in
    1 + expr_size body
  | EIfThenElse (e1, e2, e3) -> 1 + expr_size e1 + expr_size e2 + expr_size e3
  | EDefault (exceptions, just, cons) ->
    List.fold_left
      (fun acc except -> acc + expr_size except)
      (1 + expr_size just + expr_size cons)
      exceptions

let remove_logging_calls (e : 'm marked_expr) : 'm marked_expr Bindlib.box =
  let rec f () e =
    match Marked.unmark e with
    | EApp ((EOp (Unop (Log _)), _), [arg]) -> Expr.map () ~f arg
    | _ -> Expr.map () ~f e
  in
  f () e
