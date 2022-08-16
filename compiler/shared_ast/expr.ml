(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>, Louis Gesbert <louis.gesbert@inria.fr>

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
open Types

(** Functions handling the types of [shared_ast] *)

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

let eraise e1 pos = Bindlib.box (ERaise e1, pos)

let ecatch e1 exn e2 pos =
  Bindlib.box_apply2 (fun e1 e2 -> ECatch (e1, exn, e2), pos) e1 e2

(* - Manipulation of marks - *)

let no_mark (type m) : m mark -> m mark = function
  | Untyped _ -> Untyped { pos = Pos.no_pos }
  | Typed _ -> Typed { pos = Pos.no_pos; ty = Marked.mark Pos.no_pos TAny }

let mark_pos (type m) (m : m mark) : Pos.t =
  match m with Untyped { pos } | Typed { pos; _ } -> pos

let pos (type m) (x : ('a, m mark) Marked.t) : Pos.t =
  mark_pos (Marked.get_mark x)

let ty (_, m) : marked_typ = match m with Typed { ty; _ } -> ty

let with_ty (type m) (ty : marked_typ) (x : ('a, m mark) Marked.t) :
    ('a, typed mark) Marked.t =
  Marked.mark
    (match Marked.get_mark x with
    | Untyped { pos } -> Typed { pos; ty }
    | Typed m -> Typed { m with ty })
    (Marked.unmark x)

let map_mark
    (type m)
    (pos_f : Pos.t -> Pos.t)
    (ty_f : marked_typ -> marked_typ)
    (m : m mark) : m mark =
  match m with
  | Untyped { pos } -> Untyped { pos = pos_f pos }
  | Typed { pos; ty } -> Typed { pos = pos_f pos; ty = ty_f ty }

let map_mark2
    (type m)
    (pos_f : Pos.t -> Pos.t -> Pos.t)
    (ty_f : typed -> typed -> marked_typ)
    (m1 : m mark)
    (m2 : m mark) : m mark =
  match m1, m2 with
  | Untyped m1, Untyped m2 -> Untyped { pos = pos_f m1.pos m2.pos }
  | Typed m1, Typed m2 -> Typed { pos = pos_f m1.pos m2.pos; ty = ty_f m1 m2 }

let fold_marks
    (type m)
    (pos_f : Pos.t list -> Pos.t)
    (ty_f : typed list -> marked_typ)
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

let get_scope_body_mark scope_body =
  match snd (Bindlib.unbind scope_body.scope_body_expr) with
  | Result e | ScopeLet { scope_let_expr = e; _ } -> Marked.get_mark e

(* - Traversal functions - *)

let map
    (type a)
    (ctx : 'ctx)
    ~(f : 'ctx -> (a, 'm1) marked_gexpr -> (a, 'm2) marked_gexpr Bindlib.box)
    (e : ((a, 'm1) gexpr, 'm2) Marked.t) : (a, 'm2) marked_gexpr Bindlib.box =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | ELit l -> elit l m
  | EApp (e1, args) -> eapp (f ctx e1) (List.map (f ctx) args) m
  | EOp op -> Bindlib.box (EOp op, m)
  | EArray args -> earray (List.map (f ctx) args) m
  | EVar v -> evar (Var.translate v) m
  | EAbs (binder, typs) ->
    let vars, body = Bindlib.unmbind binder in
    eabs (Bindlib.bind_mvar (Array.map Var.translate vars) (f ctx body)) typs m
  | EIfThenElse (e1, e2, e3) ->
    eifthenelse ((f ctx) e1) ((f ctx) e2) ((f ctx) e3) m
  | ETuple (args, s) -> etuple (List.map (f ctx) args) s m
  | ETupleAccess (e1, n, s_name, typs) ->
    etupleaccess ((f ctx) e1) n s_name typs m
  | EInj (e1, i, e_name, typs) -> einj ((f ctx) e1) i e_name typs m
  | EMatch (arg, arms, e_name) ->
    ematch ((f ctx) arg) (List.map (f ctx) arms) e_name m
  | EAssert e1 -> eassert ((f ctx) e1) m
  | EDefault (excepts, just, cons) ->
    edefault (List.map (f ctx) excepts) ((f ctx) just) ((f ctx) cons) m
  | ErrorOnEmpty e1 -> eerroronempty ((f ctx) e1) m
  | ECatch (e1, exn, e2) -> ecatch (f ctx e1) exn (f ctx e2) (Marked.get_mark e)
  | ERaise exn -> eraise exn (Marked.get_mark e)

let rec map_top_down ~f e = map () ~f:(fun () -> map_top_down ~f) (f e)

let map_marks ~f e =
  map_top_down ~f:(fun e -> Marked.(mark (f (get_mark e)) (unmark e))) e

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

(* - *)

(** See [Bindlib.box_term] documentation for why we are doing that. *)
let box e =
  let rec id_t () e = map () ~f:id_t e in
  id_t () e

let untype e = map_marks ~f:(fun m -> Untyped { pos = mark_pos m }) e

let untype_program (prg : ('a, 'm mark) gexpr program) :
    ('a, untyped mark) gexpr program =
  {
    prg with
    scopes =
      Bindlib.unbox
        (map_exprs_in_scopes ~f:untype ~varf:Var.translate prg.scopes);
  }
