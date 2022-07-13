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

[@@@ocaml.warning "-7-34"]

open Utils

module ScopeName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module StructName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module StructFieldName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module StructMap : Map.S with type key = StructName.t = Map.Make (StructName)

module EnumName : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module EnumConstructor : Uid.Id with type info = Uid.MarkedString.info =
  Uid.Make (Uid.MarkedString) ()

module EnumMap : Map.S with type key = EnumName.t = Map.Make (EnumName)

type typ_lit = TBool | TUnit | TInt | TRat | TMoney | TDate | TDuration

type marked_typ = typ Marked.pos

and typ =
  | TLit of typ_lit
  | TTuple of marked_typ list * StructName.t option
  | TEnum of marked_typ list * EnumName.t
  | TArrow of marked_typ * marked_typ
  | TArray of marked_typ
  | TAny

type date = Runtime.date
type duration = Runtime.duration
type integer = Runtime.integer
type decimal = Runtime.decimal
type money = Runtime.money

type lit =
  | LBool of bool
  | LEmptyError
  | LInt of integer
  | LRat of decimal
  | LMoney of money
  | LUnit
  | LDate of date
  | LDuration of duration

type op_kind = KInt | KRat | KMoney | KDate | KDuration
type ternop = Fold

type binop =
  | And
  | Or
  | Xor
  | Add of op_kind
  | Sub of op_kind
  | Mult of op_kind
  | Div of op_kind
  | Lt of op_kind
  | Lte of op_kind
  | Gt of op_kind
  | Gte of op_kind
  | Eq
  | Neq
  | Map
  | Concat
  | Filter

type log_entry = VarDef of typ | BeginCall | EndCall | PosRecordIfTrueBool

type unop =
  | Not
  | Minus of op_kind
  | Log of log_entry * Utils.Uid.MarkedString.info list
  | Length
  | IntToRat
  | GetDay
  | GetMonth
  | GetYear
  | RoundMoney
  | RoundDecimal

type operator = Ternop of ternop | Binop of binop | Unop of unop

(** Some structures used for type inference *)
module Infer = struct
  module Any =
    Utils.Uid.Make
      (struct
        type info = unit

        let format_info fmt () = Format.fprintf fmt "any"
      end)
      ()

  type unionfind_typ = typ Marked.pos UnionFind.elem
  (** We do not reuse {!type: Dcalc.Ast.typ} because we have to include a new
      [TAny] variant. Indeed, error terms can have any type and this has to be
      captured by the type sytem. *)

  and typ =
    | TLit of typ_lit
    | TArrow of unionfind_typ * unionfind_typ
    | TTuple of unionfind_typ list * StructName.t option
    | TEnum of unionfind_typ list * EnumName.t
    | TArray of unionfind_typ
    | TAny of Any.t

  let rec typ_to_ast (ty : unionfind_typ) : marked_typ =
    let ty, pos = UnionFind.get (UnionFind.find ty) in
    match ty with
    | TLit l -> TLit l, pos
    | TTuple (ts, s) -> TTuple (List.map typ_to_ast ts, s), pos
    | TEnum (ts, e) -> TEnum (List.map typ_to_ast ts, e), pos
    | TArrow (t1, t2) -> TArrow (typ_to_ast t1, typ_to_ast t2), pos
    | TAny _ -> TAny, pos
    | TArray t1 -> TArray (typ_to_ast t1), pos

  let rec ast_to_typ (ty : marked_typ) : unionfind_typ =
    let ty' =
      match Marked.unmark ty with
      | TLit l -> TLit l
      | TArrow (t1, t2) -> TArrow (ast_to_typ t1, ast_to_typ t2)
      | TTuple (ts, s) -> TTuple (List.map (fun t -> ast_to_typ t) ts, s)
      | TEnum (ts, e) -> TEnum (List.map (fun t -> ast_to_typ t) ts, e)
      | TArray t -> TArray (ast_to_typ t)
      | TAny -> TAny (Any.fresh ())
    in
    UnionFind.make (Marked.same_mark_as ty' ty)
end

type untyped = { pos : Pos.t } [@@ocaml.unboxed]
type typed = { pos : Pos.t; ty : Infer.unionfind_typ }

(** The generic type of AST markings. Using a GADT allows functions to be
    polymorphic in the marking, but still do transformations on types when
    appropriate *)
type _ mark = Untyped : untyped -> untyped mark | Typed : typed -> typed mark

type ('a, 'm) marked = ('a, 'm mark) Marked.t

type 'm marked_expr = ('m expr, 'm) marked

and 'm expr =
  | EVar of 'm expr Bindlib.var
  | ETuple of 'm marked_expr list * StructName.t option
  | ETupleAccess of
      'm marked_expr * int * StructName.t option * typ Marked.pos list
  | EInj of 'm marked_expr * int * EnumName.t * typ Marked.pos list
  | EMatch of 'm marked_expr * 'm marked_expr list * EnumName.t
  | EArray of 'm marked_expr list
  | ELit of lit
  | EAbs of
      (('m expr, 'm marked_expr) Bindlib.mbinder[@opaque]) * typ Marked.pos list
  | EApp of 'm marked_expr * 'm marked_expr list
  | EAssert of 'm marked_expr
  | EOp of operator
  | EDefault of 'm marked_expr list * 'm marked_expr * 'm marked_expr
  | EIfThenElse of 'm marked_expr * 'm marked_expr * 'm marked_expr
  | ErrorOnEmpty of 'm marked_expr

type typed_expr = typed marked_expr
type struct_ctx = (StructFieldName.t * typ Marked.pos) list StructMap.t
type enum_ctx = (EnumConstructor.t * typ Marked.pos) list EnumMap.t
type decl_ctx = { ctx_enums : enum_ctx; ctx_structs : struct_ctx }
type 'm binder = ('m expr, 'm marked_expr) Bindlib.binder

type scope_let_kind =
  | DestructuringInputStruct
  | ScopeVarDefinition
  | SubScopeVarDefinition
  | CallingSubScope
  | DestructuringSubScopeResults
  | Assertion

type ('expr, 'm) scope_let = {
  scope_let_kind : scope_let_kind;
  scope_let_typ : typ Marked.pos;
  scope_let_expr : ('expr, 'm) marked;
  scope_let_next : ('expr, ('expr, 'm) scope_body_expr) Bindlib.binder;
  scope_let_pos : Pos.t;
}

and ('expr, 'm) scope_body_expr =
  | Result of ('expr, 'm) marked
  | ScopeLet of ('expr, 'm) scope_let

type ('expr, 'm) scope_body = {
  scope_body_input_struct : StructName.t;
  scope_body_output_struct : StructName.t;
  scope_body_expr : ('expr, ('expr, 'm) scope_body_expr) Bindlib.binder;
}

type ('expr, 'm) scope_def = {
  scope_name : ScopeName.t;
  scope_body : ('expr, 'm) scope_body;
  scope_next : ('expr, ('expr, 'm) scopes) Bindlib.binder;
}

and ('expr, 'm) scopes = Nil | ScopeDef of ('expr, 'm) scope_def

type 'm program = { decl_ctx : decl_ctx; scopes : ('m expr, 'm) scopes }

let no_mark (type m) : m mark -> m mark = function
  | Untyped _ -> Untyped { pos = Pos.no_pos }
  | Typed _ ->
    Typed
      {
        pos = Pos.no_pos;
        ty = UnionFind.make Infer.(TAny (Any.fresh ()), Pos.no_pos);
      }

let mark_pos (type m) (m : m mark) : Pos.t =
  match m with Untyped { pos } | Typed { pos; _ } -> pos

let pos (type m) (x : ('a, m) marked) : Pos.t = mark_pos (Marked.get_mark x)
let ty (_, Typed { ty; _ }) : typ = Marked.unmark (Infer.typ_to_ast ty)

let with_ty (type m) (ty : Infer.unionfind_typ) (x : ('a, m) marked) :
    ('a, typed) marked =
  Marked.mark
    (match Marked.get_mark x with
    | Untyped { pos } -> Typed { pos; ty }
    | Typed m -> Typed { m with ty })
    (Marked.unmark x)

let evar v mark = Bindlib.box_apply (Marked.mark mark) (Bindlib.box_var v)

let etuple args s mark =
  Bindlib.box_apply (fun args -> ETuple (args, s), mark) (Bindlib.box_list args)

let etupleaccess e1 i s typs mark =
  Bindlib.box_apply (fun e1 -> ETupleAccess (e1, i, s, typs), mark) e1

let einj e1 i e_name typs mark =
  Bindlib.box_apply (fun e1 -> EInj (e1, i, e_name, typs), mark) e1

let ematch arg arms e_name mark =
  Bindlib.box_apply2
    (fun arg arms -> EMatch (arg, arms, e_name), mark)
    arg (Bindlib.box_list arms)

let earray args mark =
  Bindlib.box_apply (fun args -> EArray args, mark) (Bindlib.box_list args)

let elit l mark = Bindlib.box (ELit l, mark)

let eabs binder typs mark =
  Bindlib.box_apply (fun binder -> EAbs (binder, typs), mark) binder

let eapp e1 args mark =
  Bindlib.box_apply2
    (fun e1 args -> EApp (e1, args), mark)
    e1 (Bindlib.box_list args)

let eassert e1 mark = Bindlib.box_apply (fun e1 -> EAssert e1, mark) e1
let eop op mark = Bindlib.box (EOp op, mark)

let edefault excepts just cons mark =
  Bindlib.box_apply3
    (fun excepts just cons -> EDefault (excepts, just, cons), mark)
    (Bindlib.box_list excepts) just cons

let eifthenelse e1 e2 e3 mark =
  Bindlib.box_apply3 (fun e1 e2 e3 -> EIfThenElse (e1, e2, e3), mark) e1 e2 e3

let eerroronempty e1 mark =
  Bindlib.box_apply (fun e1 -> ErrorOnEmpty e1, mark) e1

let translate_var v = Bindlib.copy_var v (fun x -> EVar x) (Bindlib.name_of v)

let map_expr ctx ~f e =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EVar v -> evar (translate_var v) m
  | EApp (e1, args) -> eapp (f ctx e1) (List.map (f ctx) args) m
  | EAbs (binder, typs) ->
    let vars, body = Bindlib.unmbind binder in
    eabs (Bindlib.bind_mvar (Array.map translate_var vars) (f ctx body)) typs m
  | ETuple (args, s) -> etuple (List.map (f ctx) args) s m
  | ETupleAccess (e1, n, s_name, typs) ->
    etupleaccess ((f ctx) e1) n s_name typs m
  | EInj (e1, i, e_name, typs) -> einj ((f ctx) e1) i e_name typs m
  | EMatch (arg, arms, e_name) ->
    ematch ((f ctx) arg) (List.map (f ctx) arms) e_name m
  | EArray args -> earray (List.map (f ctx) args) m
  | ELit l -> elit l m
  | EAssert e1 -> eassert ((f ctx) e1) m
  | EOp op -> Bindlib.box (EOp op, m)
  | EDefault (excepts, just, cons) ->
    edefault (List.map (f ctx) excepts) ((f ctx) just) ((f ctx) cons) m
  | EIfThenElse (e1, e2, e3) ->
    eifthenelse ((f ctx) e1) ((f ctx) e2) ((f ctx) e3) m
  | ErrorOnEmpty e1 -> eerroronempty ((f ctx) e1) m

let rec map_expr_top_down ~f e =
  map_expr () ~f:(fun () -> map_expr_top_down ~f) (f e)

let map_expr_marks ~f e =
  Bindlib.unbox
  @@ map_expr_top_down ~f:(fun e -> Marked.(mark (f (get_mark e)) (unmark e))) e

let untype_expr e = map_expr_marks ~f:(fun m -> Untyped { pos = mark_pos m }) e

type ('expr, 'm) box_expr_sig =
  ('expr, 'm) marked -> ('expr, 'm) marked Bindlib.box

(** See [Bindlib.box_term] documentation for why we are doing that. *)
let box_expr : ('m expr, 'm) box_expr_sig =
 fun e ->
  let rec id_t () e = map_expr () ~f:id_t e in
  id_t () e

let rec fold_left_scope_lets ~f ~init scope_body_expr =
  match scope_body_expr with
  | Result _ -> init
  | ScopeLet scope_let ->
    let var, next = Bindlib.unbind scope_let.scope_let_next in
    fold_left_scope_lets ~f ~init:(f init scope_let var) next

let rec fold_right_scope_lets ~f ~init scope_body_expr =
  match scope_body_expr with
  | Result result -> init result
  | ScopeLet scope_let ->
    let var, next = Bindlib.unbind scope_let.scope_let_next in
    let next_result = fold_right_scope_lets ~f ~init next in
    f scope_let var next_result

let map_exprs_in_scope_lets ~f ~varf scope_body_expr =
  fold_right_scope_lets
    ~f:(fun scope_let var_next acc ->
      Bindlib.box_apply2
        (fun scope_let_next scope_let_expr ->
          ScopeLet { scope_let with scope_let_next; scope_let_expr })
        (Bindlib.bind_var (varf var_next) acc)
        (f scope_let.scope_let_expr))
    ~init:(fun res -> Bindlib.box_apply (fun res -> Result res) (f res))
    scope_body_expr

let rec fold_left_scope_defs ~f ~init scopes =
  match scopes with
  | Nil -> init
  | ScopeDef scope_def ->
    let var, next = Bindlib.unbind scope_def.scope_next in
    fold_left_scope_defs ~f ~init:(f init scope_def var) next

let rec fold_right_scope_defs ~f ~init scopes =
  match scopes with
  | Nil -> init
  | ScopeDef scope_def ->
    let var_next, next = Bindlib.unbind scope_def.scope_next in
    let result_next = fold_right_scope_defs ~f ~init next in
    f scope_def var_next result_next

let map_scope_defs ~f scopes =
  fold_right_scope_defs
    ~f:(fun scope_def var_next acc ->
      let new_scope_def = f scope_def in
      let new_next = Bindlib.bind_var var_next acc in
      Bindlib.box_apply2
        (fun new_scope_def new_next ->
          ScopeDef { new_scope_def with scope_next = new_next })
        new_scope_def new_next)
    ~init:(Bindlib.box Nil) scopes

let map_exprs_in_scopes ~f ~varf scopes =
  fold_right_scope_defs
    ~f:(fun scope_def var_next acc ->
      let scope_input_var, scope_lets =
        Bindlib.unbind scope_def.scope_body.scope_body_expr
      in
      let new_scope_body_expr = map_exprs_in_scope_lets ~f ~varf scope_lets in
      let new_scope_body_expr =
        Bindlib.bind_var (varf scope_input_var) new_scope_body_expr
      in
      let new_next = Bindlib.bind_var (varf var_next) acc in
      Bindlib.box_apply2
        (fun scope_body_expr scope_next ->
          ScopeDef
            {
              scope_def with
              scope_body = { scope_def.scope_body with scope_body_expr };
              scope_next;
            })
        new_scope_body_expr new_next)
    ~init:(Bindlib.box Nil) scopes

let untype_program prg =
  {
    prg with
    scopes =
      Bindlib.unbox
        (map_exprs_in_scopes
           ~f:(fun e -> Bindlib.box (untype_expr e))
           ~varf:translate_var prg.scopes);
  }

type 'm var = 'm expr Bindlib.var
type 'm vars = 'm expr Bindlib.mvar

let new_var s = Bindlib.new_var (fun x -> EVar x) s

module Var = struct
  type t = V : 'a expr Bindlib.var -> t
  (* We use this trivial GADT to make the 'm parameter disappear under an
     existential. It's fine for a use as keys only. (bindlib defines [any_var]
     similarly but it's not exported) todo: add [@@ocaml.unboxed] once it's
     possible through abstract types *)

  let t v = V v
  let get (V v) = Bindlib.copy_var v (fun x -> EVar x) (Bindlib.name_of v)
  let compare (V x) (V y) = Bindlib.compare_vars x y
  let eq (V x) (V y) = Bindlib.eq_vars x y
end

module VarSet = Set.Make (Var)
module VarMap = Map.Make (Var)

(** {[
      let rec free_vars_expr (e : untyped marked_expr) : VarSet.t =
        match Marked.unmark e with
        | EVar v -> VarSet.singleton v
        | ETuple (es, _) | EArray es ->
          es |> List.map free_vars_expr
          |> List.fold_left VarSet.union VarSet.empty
        | ETupleAccess (e1, _, _, _)
        | EAssert e1
        | ErrorOnEmpty e1
        | EInj (e1, _, _, _) ->
          free_vars_expr e1
        | EApp (e1, es) | EMatch (e1, es, _) ->
          e1 :: es |> List.map free_vars_expr
          |> List.fold_left VarSet.union VarSet.empty
        | EDefault (es, ejust, econs) ->
          ejust :: econs :: es |> List.map free_vars_expr
          |> List.fold_left VarSet.union VarSet.empty
        | EOp _ | ELit _ -> VarSet.empty
        | EIfThenElse (e1, e2, e3) ->
          [e1; e2; e3] |> List.map free_vars_expr
          |> List.fold_left VarSet.union VarSet.empty
        | EAbs (binder, _) ->
          let vs, body = Bindlib.unmbind binder in
          Array.fold_right VarSet.remove vs (free_vars_expr body)

      module VarMap = Map.Make (Var (struct
        type t = untyped
      end))

      module VarSet = Set.Make (Var (struct
        type t = untyped
      end))

      let rec free_vars_expr (e : expr) : VarSet.t =
        match Marked.unmark e with
        | EVar (v, _) -> VarSet.singleton v
        | ETuple (es, _) | EArray es ->
          es |> List.map free_vars_expr
          |> List.fold_left VarSet.union VarSet.empty
        | ETupleAccess (e1, _, _, _)
        | EAssert e1
        | ErrorOnEmpty e1
        | EInj (e1, _, _, _) ->
          free_vars_expr e1
        | EApp (e1, es) | EMatch (e1, es, _) ->
          e1 :: es |> List.map free_vars_expr
          |> List.fold_left VarSet.union VarSet.empty
        | EDefault (es, ejust, econs) ->
          ejust :: econs :: es |> List.map free_vars_expr
          |> List.fold_left VarSet.union VarSet.empty
        | EOp _ | ELit _ -> VarSet.empty
        | EIfThenElse (e1, e2, e3) ->
          [e1; e2; e3] |> List.map free_vars_expr
          |> List.fold_left VarSet.union VarSet.empty
        | EAbs ((binder, _), _) ->
          let vs, body = Bindlib.unmbind binder in
          Array.fold_right VarSet.remove vs (free_vars_expr body)

      let rec free_vars_scope_body_expr (scope_lets : expr scope_body_expr) :
          VarSet.t =
        match scope_lets with
        | Result e -> free_vars_expr e
        | ScopeLet { scope_let_expr = e; scope_let_next = next; _ } ->
          let v, body = Bindlib.unbind next in
          VarSet.union (free_vars_expr e)
            (VarSet.remove v (free_vars_scope_body_expr body))

      let free_vars_scope_body (scope_body : expr scope_body) : VarSet.t =
        let { scope_body_expr = binder; _ } = scope_body in
        let v, body = Bindlib.unbind binder in
        VarSet.remove v (free_vars_scope_body_expr body)

      let rec free_vars_scopes (scopes : expr scopes) : VarSet.t =
        match scopes with
        | Nil -> VarSet.empty
        | ScopeDef { scope_body = body; scope_next = next; _ } ->
          let v, next = Bindlib.unbind next in
          VarSet.union
            (VarSet.remove v (free_vars_scopes next))
            (free_vars_scope_body body)
      (* type vars = expr Bindlib.mvar *)
    ]}*)

let make_var ((x, mark) : ('m expr Bindlib.var, 'm) marked) :
    'm marked_expr Bindlib.box =
  Bindlib.box_apply (fun x -> x, mark) (Bindlib.box_var x)

(* 'm expr → 'e 'm marked_expr = ('m expr, 'm) marked → ('e, 'm) marked

   ('e, 'x) marked = ('e, 'x mark) Marked.t = ('e * 'x mark)

   'e Bindlib.mvar -> ('e, 'm) Marked.t Bindlib.box -> typ Marked.pos list -> 'm
   -> ('e, 'm) Marked.t Bindlib.box *)
(* 'e = 'm expr ('e, 'm) marked = ('e, 'm mark) Marked.t = ('m expr, 'm mark)
   Marked.t *)

type ('e, 'm) make_abs_sig =
  'e Bindlib.mvar ->
  ('e, 'm) marked Bindlib.box ->
  typ Marked.pos list ->
  'm mark ->
  ('e, 'm) marked Bindlib.box

let (make_abs : ('m expr, 'm) make_abs_sig) =
 fun xs e taus mark ->
  Bindlib.box_apply (fun b -> EAbs (b, taus), mark) (Bindlib.bind_mvar xs e)

let make_app :
    'm marked_expr Bindlib.box ->
    'm marked_expr Bindlib.box list ->
    'm mark ->
    'm marked_expr Bindlib.box =
 fun e u mark ->
  Bindlib.box_apply2 (fun e u -> EApp (e, u), mark) e (Bindlib.box_list u)

type ('expr, 'm) make_let_in_sig =
  'expr Bindlib.var ->
  typ Marked.pos ->
  ('expr, 'm) marked Bindlib.box ->
  ('expr, 'm) marked Bindlib.box ->
  Pos.t ->
  ('expr, 'm) marked Bindlib.box

let map_mark
    (type m)
    (pos_f : Pos.t -> Pos.t)
    (ty_f : Infer.unionfind_typ -> Infer.unionfind_typ)
    (m : m mark) : m mark =
  match m with
  | Untyped { pos } -> Untyped { pos = pos_f pos }
  | Typed { pos; ty } -> Typed { pos = pos_f pos; ty = ty_f ty }

let map_mark2
    (type m)
    (pos_f : Pos.t -> Pos.t -> Pos.t)
    (ty_f : typed -> typed -> Infer.unionfind_typ)
    (m1 : m mark)
    (m2 : m mark) : m mark =
  match m1, m2 with
  | Untyped m1, Untyped m2 -> Untyped { pos = pos_f m1.pos m2.pos }
  | Typed m1, Typed m2 -> Typed { pos = pos_f m1.pos m2.pos; ty = ty_f m1 m2 }

let fold_marks
    (type m)
    (pos_f : Pos.t list -> Pos.t)
    (ty_f : typed list -> Infer.unionfind_typ)
    (ms : m mark list) : m mark =
  match ms with
  | [] -> invalid_arg "Dcalc.Ast.fold_mark"
  | Untyped _ :: _ as ms ->
    Untyped { pos = pos_f (List.map (function Untyped { pos } -> pos) ms) }
  | Typed _ :: _ ->
    Typed
      {
        pos = pos_f (List.map (function Typed { pos; _ } -> pos) ms);
        ty = ty_f (List.map (function Typed m -> m) ms);
      }

let empty_thunked_term mark : 'm marked_expr =
  let silent = new_var "_" in
  let pos = mark_pos mark in
  Bindlib.unbox
    (make_abs [| silent |]
       (Bindlib.box (ELit LEmptyError, mark))
       [TLit TUnit, mark_pos mark]
       (map_mark
          (fun pos -> pos)
          (fun ty ->
            UnionFind.make
              Infer.(
                TArrow (UnionFind.make (TLit TUnit, pos), ty), mark_pos mark))
          mark))

let (make_let_in : ('m expr, 'm) make_let_in_sig) =
 fun x tau e1 e2 pos ->
  let m_e1 = Marked.get_mark (Bindlib.unbox e1) in
  let m_e2 = Marked.get_mark (Bindlib.unbox e2) in
  let m_abs =
    map_mark2
      (fun _ _ -> pos)
      (fun m1 m2 -> UnionFind.make (Infer.TArrow (m1.ty, m2.ty), m1.pos))
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
    equal_exprs def1 def2 && equal_exprs cons1 cons2
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
    ~(box_expr : ('expr, 'm) box_expr_sig)
    ~(make_let_in : ('expr, 'm) make_let_in_sig)
    (ctx : decl_ctx)
    (scope_let : ('expr, 'm) scope_body_expr) : ('expr, 'm) marked Bindlib.box =
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
    ~(box_expr : ('expr, 'm) box_expr_sig)
    ~(make_abs : ('expr, 'm) make_abs_sig)
    ~(make_let_in : ('expr, 'm) make_let_in_sig)
    (ctx : decl_ctx)
    (body : ('expr, 'm) scope_body)
    (mark_scope : 'm mark) : ('expr, 'm) marked Bindlib.box =
  let var, body_expr = Bindlib.unbind body.scope_body_expr in
  let body_expr = unfold_scope_body_expr ~box_expr ~make_let_in ctx body_expr in
  make_abs (Array.of_list [var]) body_expr
    [
      ( TTuple
          ( List.map snd
              (StructMap.find body.scope_body_input_struct ctx.ctx_structs),
            Some body.scope_body_input_struct ),
        mark_pos mark_scope );
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

let get_scope_body_mark scope_body =
  match snd (Bindlib.unbind scope_body.scope_body_expr) with
  | Result e | ScopeLet { scope_let_expr = e; _ } -> Marked.get_mark e

let rec unfold_scopes
    ~(box_expr : ('expr, 'm) box_expr_sig)
    ~(make_abs : ('expr, 'm) make_abs_sig)
    ~(make_let_in : ('expr, 'm) make_let_in_sig)
    (ctx : decl_ctx)
    (s : ('expr, 'm) scopes)
    (mark : 'm mark)
    (main_scope : 'expr scope_name_or_var) : ('expr, 'm) marked Bindlib.box =
  match s with
  | Nil -> (
    match main_scope with
    | ScopeVar v -> Bindlib.box_apply (fun v -> v, mark) (Bindlib.box_var v)
    | ScopeName _ -> failwith "should not happen")
  | ScopeDef { scope_name; scope_body; scope_next } ->
    let scope_var, scope_next = Bindlib.unbind scope_next in
    let scope_pos = Marked.get_mark (ScopeName.get_info scope_name) in
    let scope_body_mark = get_scope_body_mark scope_body in
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

let build_whole_program_expr (p : 'm program) (main_scope : ScopeName.t) =
  let _, main_scope_body = find_scope main_scope [] p.scopes in
  unfold_scopes ~box_expr ~make_abs ~make_let_in p.decl_ctx p.scopes
    (get_scope_body_mark main_scope_body)
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
    | EApp ((EOp (Unop (Log _)), _), [arg]) -> map_expr () ~f arg
    | _ -> map_expr () ~f e
  in
  f () e
