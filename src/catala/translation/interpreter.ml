(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module G = Graph.Pack.Digraph
open Lambda

type uid = int

type scope_uid = int

module UidMap = Uid.UidMap
module UidSet = Uid.UidSet

type exec_context = Lambda.untyped_term UidMap.t

let rec substitute (var : uid) (value : Lambda.untyped_term) (term : Lambda.term) : Lambda.term =
  let (term', pos), typ = term in
  let subst = substitute var value in
  let subst_term =
    match term' with
    | EVar uid -> if var = uid then value else term'
    | EFun (bindings, body) -> EFun (bindings, subst body)
    | EApp (f, args) -> EApp (subst f, List.map subst args)
    | EIfThenElse (t_if, t_then, t_else) -> EIfThenElse (subst t_if, subst t_then, subst t_else)
    | EInt _ | EBool _ | EDec _ | EOp _ -> term'
  in
  ((subst_term, pos), typ)

let rec eval_term (exec_ctxt : exec_context) (term : Lambda.term) : Lambda.term =
  let (term, pos), typ = term in
  let evaled_term =
    match term with
    | EFun _ | EInt _ | EDec _ | EBool _ | EOp _ -> term (* already a value *)
    | EVar uid -> ( match UidMap.find_opt uid exec_ctxt with Some t -> t | None -> assert false )
    | EApp (f, args) -> (
        (* First evaluate and match the function body *)
        let f = f |> eval_term exec_ctxt |> Lambda.untype in
        match f with
        | EFun (bindings, body) ->
            let body =
              List.fold_left2
                (fun body arg (uid, _) ->
                  substitute uid (arg |> eval_term exec_ctxt |> Lambda.untype) body)
                body args bindings
            in
            eval_term exec_ctxt body |> Lambda.untype
        | EOp op -> (
            let args = List.map (fun arg -> arg |> eval_term exec_ctxt |> Lambda.untype) args in
            match op with
            | Binop binop -> (
                match binop with
                | And | Or ->
                    let b1, b2 =
                      match args with [ EBool b1; EBool b2 ] -> (b1, b2) | _ -> assert false
                    in
                    EBool (if binop = And then b1 && b2 else b1 || b2)
                | _ -> (
                    let i1, i2 =
                      match args with [ EInt i1; EInt i2 ] -> (i1, i2) | _ -> assert false
                    in
                    let op_arith =
                      match binop with
                      | Add -> ( + )
                      | Sub -> ( - )
                      | Mult -> ( * )
                      | Div -> ( / )
                      | _ -> fun _ _ -> 0
                    in
                    let op_comp =
                      match binop with
                      | Lt -> ( < )
                      | Lte -> ( <= )
                      | Gt -> ( > )
                      | Gte -> ( >= )
                      | Eq -> ( = )
                      | Neq -> ( <> )
                      | _ -> fun _ _ -> false
                    in
                    match binop with
                    | Add | Sub | Mult | Div -> EInt (op_arith i1 i2)
                    | _ -> EBool (op_comp i1 i2) ) )
            | Unop Minus -> ( match args with [ EInt i ] -> EInt (-i) | _ -> assert false )
            | Unop Not -> ( match args with [ EBool b ] -> EBool (not b) | _ -> assert false ) )
        | _ -> assert false )
    | EIfThenElse (t_if, t_then, t_else) ->
        ( match eval_term exec_ctxt t_if |> Lambda.untype with
        | EBool b -> if b then eval_term exec_ctxt t_then else eval_term exec_ctxt t_else
        | _ -> assert false )
        |> Lambda.untype
  in
  ((evaled_term, pos), typ)

(* Evaluates a default term : see the formalization for an insight about this operation *)
let eval_default_term (exec_ctxt : exec_context) (term : Lambda.default_term) : Lambda.term option =
  (* First filter out the term which justification are false *)
  let candidates =
    IntMap.filter
      (fun _ (cond, _) ->
        match eval_term exec_ctxt cond |> Lambda.untype with EBool b -> b | _ -> assert false)
      term.defaults
  in
  (* Now filter out the terms that have a predecessor *)
  let module ISet = Set.Make (Int) in
  let key_candidates = IntMap.fold (fun x _ -> ISet.add x) candidates ISet.empty in
  let chosen_one =
    List.fold_left
      (fun set (lo, hi) -> if ISet.mem lo set && ISet.mem hi set then ISet.remove hi set else set)
      key_candidates term.ordering
  in
  match ISet.elements chosen_one with
  | [ x ] ->
      let _, cons = IntMap.find x term.defaults in
      Some (eval_term exec_ctxt cons)
  | _ -> (* TODO : error reporting for cause of conflicts *) None

(** Returns the scheduling of the scope variables, if y is a subscope and x a variable of y, then we
    have two different variable y.x(internal) and y.x(result) and the ordering y.x(internal) -> y ->
    y.x(result) *)
let build_scope_schedule (ctxt : Context.context) (prgm : Scope.program) (scope_uid : scope_uid) :
    G.t =
  let g = G.create ~size:100 () in
  let scope = UidMap.find scope_uid prgm in
  (* Add all the vertices to the graph *)
  let vertices =
    UidSet.fold
      (fun uid verts ->
        match (UidMap.find uid ctxt.data).uid_sort with
        | IdScopeVar | IdSubScope _ -> UidMap.add uid (G.V.create uid) verts
        | _ -> verts)
      (UidMap.find scope_uid ctxt.scopes).uid_set UidMap.empty
  in
  UidMap.iter (fun _ v -> G.add_vertex g v) vertices;
  (* Process definitions dependencies. There are two types of dependencies : var -> var; sub_scope
     -> var *)
  UidMap.iter
    (fun var_uid def ->
      let fv = Lambda.default_term_fv def in
      UidSet.iter
        (fun uid ->
          let data = UidMap.find uid ctxt.data in
          let from_uid =
            match data.uid_sort with
            | IdScopeVar -> uid
            | IdSubScopeVar (_, sub_scope_uid) -> sub_scope_uid
            | _ -> assert false
          in
          Printf.printf "%d -> %d" from_uid var_uid;
          G.add_edge g (UidMap.find from_uid vertices) (UidMap.find var_uid vertices);
          Printf.printf ".\n")
        fv)
    scope.scope_defs;
  (* Process sub-definitions dependencies. Only one kind of dependencies : var -> sub_scopes*)
  UidMap.iter
    (fun sub_scope_uid defs ->
      UidMap.iter
        (fun _ def ->
          let fv = Lambda.default_term_fv def in
          UidSet.iter
            (fun var_uid ->
              (* Process only uid from the current scope (not the subscope) *)
              if Context.belongs_to ctxt var_uid scope_uid then (
                Printf.printf "%d -> %d" var_uid sub_scope_uid;
                G.add_edge g (UidMap.find var_uid vertices) (UidMap.find sub_scope_uid vertices);
                Printf.printf ".\n" )
              else ())
            fv)
        defs)
    scope.scope_sub_defs;
  g

let merge_var_redefs (_subscope : uid) (_caller_scope : scope_uid) (_prgm : Scope.program) :
    Scope.scope =
  assert false

let execute_scope (ctxt : Context.context) (prgm : Scope.program) (scope_uid : scope_uid) :
    exec_context =
  let _scope = UidMap.find scope_uid prgm in
  let schedule = build_scope_schedule ctxt prgm scope_uid in
  let exec_context : exec_context = UidMap.empty in
  G.Topological.fold
    (fun v_uid exec_context ->
      Printf.printf "%d\n" (G.V.label v_uid);
      exec_context)
    schedule exec_context
