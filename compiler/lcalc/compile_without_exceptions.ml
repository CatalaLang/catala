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
    cases is found in the translate_and_hoist function.

    Problem arise when there is a function application. *)

open Shared_ast

module Ren = struct
  module Set = Set.Make (String)

  type ctxt = Set.t

  let skip_constant_binders = true
  let reset_context_for_closed_terms = true
  let constant_binder_name = None
  let empty_ctxt = Set.empty
  let reserve_name n s = Set.add n s
  let new_name n s = n, Set.add n s
end

module Ctx = Bindlib.Ctxt (Ren)

type 'a info_pure = {
  info_pure : bool;
  is_scope : bool;
  var : 'a Ast.expr Var.t;
}

(** Information about each encontered Dcalc variable is stored inside a context
    : what is the corresponding LCalc variable; an expression corresponding to
    the variable build correctly using Bindlib, and a boolean `is_pure`
    indicating whenever the variable can be an EmptyError and hence should be
    matched (false) or if it never can be EmptyError (true). *)

(** [tau' = translate_typ tau] translate the a dcalc type into a lcalc type.

    Since positions where there is thunked expressions is exactly where we will
    put option expressions. Hence, the transformation simply reduce [unit -> 'a]
    into ['a option] recursivly. There is no polymorphism inside catala. *)

let trans_typ (tau : typ) : typ = Marked.same_mark_as TAny tau

let monad_return e ~(mark : 'a mark) =
  Expr.einj e Ast.some_constr Ast.option_enum mark

let monad_empty ~(mark : 'a mark) =
  Expr.einj (Expr.elit LUnit mark) Ast.none_constr Ast.option_enum mark

let monad_bind_var f x arg ~(mark : 'a mark) =
  let cases =
    EnumConstructor.Map.of_seq
      (List.to_seq
         [
           ( Ast.none_constr,
             let x = Var.make "_" in
             Expr.eabs
               (Expr.bind [| x |]
                  (Expr.einj (Expr.evar x mark) Ast.none_constr Ast.option_enum
                     mark))
               [TLit TUnit, Expr.mark_pos mark]
               mark );
           (* | None x -> None x *)
           ( Ast.some_constr,
             Expr.eabs (Expr.bind [| x |] f) [TAny, Expr.mark_pos mark] mark )
           (*| Some x -> f (where f contains x as a free variable) *);
         ])
  in
  Expr.ematch arg Ast.option_enum cases mark

let monad_bind f arg ~(mark : 'a mark) =
  let x = Var.make "x" in
  (* todo modify*)
  monad_bind_var f x arg ~mark

let monad_bind_cont f arg ~(mark : 'a mark) =
  let x = Var.make "x" in
  monad_bind_var (f x) x arg ~mark

let monad_mbind_mvar f xs args ~(mark : 'a mark) =
  (* match e1, ..., en with | Some e1', ..., Some en' -> f (e1, ..., en) | _ ->
     None *)
  ListLabels.fold_left2 xs args ~f:(monad_bind_var ~mark)
    ~init:(Expr.eapp f (List.map (fun v -> Expr.evar v mark) xs) mark)

let monad_mbind f args ~(mark : 'a mark) =
  (* match e1, ..., en with | Some e1', ..., Some en' -> f (e1, ..., en) | _ ->
     None *)
  let vars =
    ListLabels.mapi args ~f:(fun i _ -> Var.make (Format.sprintf "e_%i" i))
  in
  monad_mbind_mvar f vars args ~mark

let monad_mbind_cont f args ~(mark : 'a mark) =
  let vars =
    ListLabels.mapi args ~f:(fun i _ -> Var.make (Format.sprintf "e_%i" i))
  in
  ListLabels.fold_left2 vars args ~f:(monad_bind_var ~mark) ~init:(f vars)
(* monad_mbind_mvar (f vars) vars args ~mark *)

let monad_mmap_mvar f xs args ~(mark : 'a mark) =
  (* match e1, ..., en with | Some e1', ..., Some en' -> f (e1, ..., en) | _ ->
     None *)
  ListLabels.fold_left2 xs args ~f:(monad_bind_var ~mark)
    ~init:
      (Expr.einj
         (Expr.eapp f (List.map (fun v -> Expr.evar v mark) xs) mark)
         Ast.some_constr Ast.option_enum mark)

let monad_map_var f x arg ~(mark : 'a mark) = monad_mmap_mvar f [x] [arg] ~mark

let monad_map f arg ~(mark : 'a mark) =
  let x = Var.make "x" in
  monad_map_var f x arg ~mark

let monad_mmap f args ~(mark : 'a mark) =
  let vars =
    ListLabels.mapi args ~f:(fun i _ -> Var.make (Format.sprintf "e_%i" i))
  in
  monad_mmap_mvar f vars args ~mark

let monad_eoe ?(toplevel = false) arg ~(mark : 'a mark) =
  let cases =
    EnumConstructor.Map.of_seq
      (List.to_seq
         [
           ( Ast.none_constr,
             let x = Var.make "x" in
             Expr.eabs
               (Expr.bind [| x |] (Expr.eraise NoValueProvided mark))
               [TAny, Expr.mark_pos mark]
               mark );
           (* | None x -> raise NoValueProvided *)
           Ast.some_constr, Expr.eid mark (* | Some x -> x*);
         ])
  in
  if toplevel then Expr.ematch arg Ast.option_enum cases mark
  else monad_return ~mark (Expr.ematch arg Ast.option_enum cases mark)

let monad_handle_default ~(_mark : 'a mark) _except _cond _just =
  (* let handle_default_opt (exceptions : 'a eoption array) (just : bool
     eoption) (cons : 'a eoption) : 'a eoption = let except = Array.fold_left
     (fun acc except -> match acc, except with | ENone _, _ -> except | ESome _,
     ENone _ -> acc | ESome _, ESome _ -> raise (ConflictError pos)) (ENone ())
     exceptions in match except with | ESome _ -> except | ENone _ -> ( match
     just with | ESome b -> if b then cons else ENone () | ENone _ -> ENone
     ()) *)
  assert false

let _ = monad_return
let _ = monad_empty
let _ = monad_bind_var
let _ = monad_bind
let _ = monad_bind_cont
let _ = monad_mbind_mvar
let _ = monad_mbind
let _ = monad_mbind_cont
let _ = monad_eoe
let _ = monad_map
let _ = monad_mmap_mvar
let _ = monad_mmap
let _ = monad_handle_default

(** Start of the translation *)

let trans_var ctx (x : 'm D.expr Var.t) : 'm Ast.expr Var.t =
  let new_ = (Var.Map.find x ctx).var in
  Cli.debug_format "before: %a after: %a" Print.var_debug x Print.var_debug new_;

  new_

let trans_op : (dcalc, 'a) Op.t -> (lcalc, 'a) Op.t = Operator.translate

let rec trans ctx (e : 'm D.expr) : (lcalc, 'm mark) boxed_gexpr =
  let m = Marked.get_mark e in
  let mark = m in
  let pos = Expr.pos e in
  (* Cli.debug_format "%a" (Print.expr_debug ~debug:true) e; *)
  match Marked.unmark e with
  | EVar x ->
    if (Var.Map.find x ctx).info_pure then
      monad_return (Expr.evar (trans_var ctx x) m) ~mark
    else Expr.evar (trans_var ctx x) m
  | EApp { f = EVar v, _; args = [(ELit LUnit, _)] } ->
    (* lazy *)
    assert (not (Var.Map.find v ctx).info_pure);
    Expr.evar (trans_var ctx v) m
  | EAbs { binder; tys = [(TLit TUnit, _)] } ->
    (* this is to be used with monad_bind. *)
    let _, body = Bindlib.unmbind binder in
    trans ctx body
  | EAbs { binder; tys } ->
    (* this is to be used with monad_bind. *)
    let vars, body = Bindlib.unmbind binder in
    let ctx' =
      ArrayLabels.fold_right vars ~init:ctx ~f:(fun v ->
          Var.Map.add v
            { info_pure = true; is_scope = false; var = Var.translate v })
    in

    let body' = trans ctx' body in
    let binder' = Expr.bind (Array.map Var.translate vars) body' in
    monad_return ~mark (Expr.eabs binder' tys m)
  | EDefault { excepts; just; cons } ->
    let excepts' = List.map (trans ctx) excepts in
    let just' = trans ctx just in
    let cons' = trans ctx cons in

    (* for each e in excepts, e: 'a option. just: bool. cons': 'a option.
       Result: 'a option*)
    let m' = match m with Typed m -> Typed { m with ty = TAny, pos } in
    (* App Handle_default excepts just cons *)
    Expr.make_app
      (Expr.eop Op.HandleDefaultOpt [TAny, pos; TAny, pos; TAny, pos] m')
      [Expr.earray excepts' m; just'; cons']
      pos
  | ELit l -> begin
    match l with
    | LEmptyError -> monad_empty ~mark
    (* gadts cannot infer l is in fact lcalc glit. Hence, we explicit it. *)
    | (LBool _ | LInt _ | LRat _ | LMoney _ | LUnit | LDate _ | LDuration _) as
      l ->
      monad_return ~mark (Expr.elit l m)
  end
  | EErrorOnEmpty arg ->
    let arg' = trans ctx arg in
    monad_eoe arg' ~mark ~toplevel:false
  | EApp { f = (EVar ff, _) as f; args } ->
    (* INVARIANT: functions are always encoded using this function.info_pure
       Invariant failed : scope calls are encoded using an other technique. *)
    (* Cli.debug_format "%s %b" (Bindlib.name_of ff) (Var.Map.find ff
       ctx).info_pure; *)

    (* todo: is_scope test *)
    if (Var.Map.find ff ctx).is_scope then
      let f_var = Var.make "fff" in
      monad_bind_var ~mark
        (monad_mmap (Expr.evar f_var mark) (List.map (trans ctx) args) ~mark)
        f_var (trans ctx f)
    else
      let f_var = Var.make "fff" in
      monad_bind_var ~mark
        (monad_mbind (Expr.evar f_var mark) (List.map (trans ctx) args) ~mark)
        f_var (trans ctx f)
  | EApp { f = (EStructAccess _, _) as f; args } ->
    let f_var = Var.make "fff" in
    monad_bind_var ~mark
      (monad_mbind (Expr.evar f_var mark) (List.map (trans ctx) args) ~mark)
      f_var (trans ctx f)
    (* correspond to what? *)
  | EApp { f = EAbs { binder; _ }, _; args } ->
    (* Invariant: every let have only one argument. (checked by
       invariant_let) *)
    let var, body = Bindlib.unmbind binder in
    let[@warning "-8"] [| var |] = var in
    let var' = Var.translate var in
    let[@warning "-8"] [arg] = args in
    let ctx' =
      Var.Map.add var { info_pure = true; is_scope = false; var = var' } ctx
    in
    monad_bind_var (trans ctx' body) var' (trans ctx arg) ~mark
  | EApp { f = EApp { f = EOp { op = Op.Log _; _ }, _; args = _ }, _; _ } ->
    assert false
  (* | EApp { f = EOp { op = Op.Fold; tys }, opmark; args = [f; init; l] } -> (*
     *) let x1 = Var.make "x1" in let x2 = var.make "x2" in let body = monad let
     f' = assert false in monad_mbind (Expr.eop (trans_op Op.Fold) tys opmark)
     [f'; trans ctx init; trans ctx l] ~mark | EApp { f = EOp { op = Op.Fold; _
     }, _; _ } -> (* Cannot happend: folds must be fully determined *) assert
     false *)
  | EApp { f = EOp { op; tys }, opmark; args } ->
    let res =
      monad_mmap
        (Expr.eop (trans_op op) tys opmark)
        (List.map (trans ctx) args)
        ~mark
    in

    (* Cli.debug_format "before:\n%a" (Print.expr_debug ~debug:true) e;
       Cli.debug_format "after:\n%a" (Print.expr_debug ~debug:true) (Expr.unbox
       res); *)
    res
  | EMatch { name; e; cases } ->
    let cases =
      EnumConstructor.MapLabels.map cases ~f:(fun case ->
          match Marked.unmark case with
          | EAbs { binder; tys } ->
            let vars, body = Bindlib.unmbind binder in
            let ctx' =
              ArrayLabels.fold_right vars ~init:ctx ~f:(fun var ->
                  Var.Map.add var
                    {
                      info_pure = true;
                      is_scope = false;
                      var = Var.translate var;
                    })
            in
            let binder =
              Expr.bind (Array.map Var.translate vars) (trans ctx' body)
            in
            Expr.eabs binder tys m
          | _ -> assert false)
    in
    monad_bind_cont
      (fun e -> Expr.ematch (Expr.evar e m) name cases m)
      (trans ctx e) ~mark
  | EArray args ->
    monad_mbind_cont ~mark
      (fun vars ->
        monad_return ~mark
          (Expr.earray
             (List.map (fun v -> monad_return ~mark (Expr.evar v m)) vars)
             mark))
      (List.map (trans ctx) args)
  | EStruct { name; fields } ->
    (* TODO: since ocaml is determinisitc, the fields will always be enumerated
       in the same order on the fields. *)
    let fields_name, fields = List.split (StructField.Map.bindings fields) in
    monad_mbind_cont
      (fun xs ->
        let fields =
          ListLabels.fold_right2 fields_name
            (List.map (fun x -> monad_return ~mark (Expr.evar x mark)) xs)
            ~f:StructField.Map.add ~init:StructField.Map.empty
        in
        monad_return ~mark (Expr.estruct name fields mark))
      (List.map (trans ctx) fields)
      ~mark
  | EIfThenElse { cond; etrue; efalse } ->
    (* semantic one : ife is not rendondant with defaults. *)
    (* monad_bind_cont ~mark (fun cond -> monad_bind_cont (fun etrue ->
       monad_bind_cont ~mark (fun efalse -> Expr.eifthenelse (Expr.evar cond m)
       (Expr.evar etrue m) (Expr.evar efalse m) m) (trans ctx efalse)) (trans
       ctx etrue) ~mark) (trans ctx cond) *)

    (* semantic two: ife is redondant with defaults. *)
    monad_bind_cont ~mark
      (fun cond ->
        Expr.eifthenelse (Expr.evar cond mark) (trans ctx etrue)
          (trans ctx efalse) mark)
      (trans ctx cond)
  | EInj { name; cons; e } ->
    monad_bind_cont
      (fun e ->
        monad_return ~mark (Expr.einj (Expr.evar e mark) cons name mark))
      (trans ctx e) ~mark
  | EStructAccess { name; e; field } ->
    monad_bind_cont
      (fun e -> Expr.estructaccess (Expr.evar e mark) field name mark)
      (trans ctx e) ~mark
  | ETuple args ->
    monad_mbind_cont
      (fun xs ->
        monad_return ~mark
          (Expr.etuple (List.map (fun x -> Expr.evar x mark) xs) mark))
      (List.map (trans ctx) args)
      ~mark
  | ETupleAccess { e; index; size } ->
    monad_bind_cont
      (fun e -> Expr.etupleaccess (Expr.evar e mark) index size mark)
      (trans ctx e) ~mark
  | EAssert e ->
    monad_bind_cont
      (fun e -> monad_return ~mark (Expr.eassert (Expr.evar e mark) mark))
      (trans ctx e) ~mark
  | EApp _ ->
    Errors.raise_spanned_error (Expr.pos e)
      "Internal Error: found an EApp that does not satisfy the invariants when \
       translating Dcalc to Lcalc without exceptions."
  (* invalid invariant *)
  | EOp _ ->
    Errors.raise_spanned_error (Expr.pos e)
      "Internal Error: found an EOp that does not satisfy the invariants when \
       translating Dcalc to Lcalc without exceptions."

let rec trans_scope_let ctx s =
  match s with
  | {
   scope_let_kind = SubScopeVarDefinition;
   scope_let_typ = (TArrow ([(TLit TUnit, _)], _), _) as scope_let_typ;
   scope_let_expr = EAbs { binder; _ }, _;
   scope_let_next;
   scope_let_pos;
  } ->
    (* special case : the subscope variable is thunked (context i/o). We remove
       this thunking. *)
    let _, scope_let_expr = Bindlib.unmbind binder in
    let next_var, next_body = Bindlib.unbind scope_let_next in

    let next_var' = Var.translate next_var in
    let ctx' =
      Var.Map.add next_var
        { info_pure = false; is_scope = false; var = next_var' }
        ctx
    in

    let next_var = next_var' in
    let next_body = trans_scope_body_expr ctx' next_body in

    let scope_let_next = Bindlib.bind_var next_var next_body in

    let scope_let_expr = Expr.Box.lift @@ trans ctx scope_let_expr in

    Bindlib.box_apply2
      (fun scope_let_expr scope_let_next ->
        {
          scope_let_kind = SubScopeVarDefinition;
          scope_let_typ = trans_typ scope_let_typ;
          scope_let_expr;
          scope_let_next;
          scope_let_pos;
        })
      scope_let_expr scope_let_next
  | {
   scope_let_kind = SubScopeVarDefinition;
   scope_let_typ;
   scope_let_expr = (EAbs _, _) as scope_let_expr;
   scope_let_next;
   scope_let_pos;
  } ->
    (* special case : the subscope variable is thunked (context i/o). We remove
       this thunking. *)
    let next_var, next_body = Bindlib.unbind scope_let_next in
    let next_var' = Var.translate next_var in
    let ctx' =
      Var.Map.add next_var
        { info_pure = false; is_scope = false; var = next_var' }
        ctx
    in
    let next_var = next_var' in
    let next_body = trans_scope_body_expr ctx' next_body in

    let scope_let_next = Bindlib.bind_var next_var next_body in

    let scope_let_expr = Expr.Box.lift @@ trans ctx scope_let_expr in

    Bindlib.box_apply2
      (fun scope_let_expr scope_let_next ->
        {
          scope_let_kind = SubScopeVarDefinition;
          scope_let_typ = trans_typ scope_let_typ;
          scope_let_expr;
          scope_let_next;
          scope_let_pos;
        })
      scope_let_expr scope_let_next
  | {
   scope_let_kind = SubScopeVarDefinition;
   scope_let_typ;
   scope_let_expr = EErrorOnEmpty e, mark;
   scope_let_next;
   scope_let_pos;
  } ->
    (* special case: regular input to the subscope *)
    let next_var, next_body = Bindlib.unbind scope_let_next in

    let next_var' = Var.translate next_var in
    let ctx' =
      Var.Map.add next_var
        { info_pure = false; is_scope = false; var = next_var' }
        ctx
    in

    let next_var = next_var' in
    let next_body = trans_scope_body_expr ctx' next_body in

    let scope_let_next = Bindlib.bind_var next_var next_body in

    let scope_let_expr =
      Expr.Box.lift @@ monad_eoe ~mark ~toplevel:true (trans ctx e)
    in

    Bindlib.box_apply2
      (fun scope_let_expr scope_let_next ->
        {
          scope_let_kind = SubScopeVarDefinition;
          scope_let_typ = trans_typ scope_let_typ;
          scope_let_expr;
          scope_let_next;
          scope_let_pos;
        })
      scope_let_expr scope_let_next
  | { scope_let_kind = SubScopeVarDefinition; scope_let_pos = pos; _ } ->
    Errors.raise_spanned_error pos
      "Internal Error: found an SubScopeVarDefinition that does not satisfy \
       the invariants when translating Dcalc to Lcalc without exceptions."
  | {
   scope_let_kind;
   scope_let_typ;
   scope_let_expr;
   scope_let_next;
   scope_let_pos;
  } ->
    (* base case *)
    let next_var, next_body = Bindlib.unbind scope_let_next in

    let next_var' = Var.translate next_var in
    let ctx' =
      Var.Map.add next_var
        (match scope_let_kind with
        | DestructuringInputStruct -> (
          (* Here, we have to distinguish between context and input variables.
             We can do so by looking at the typ of the destructuring: if it's
             thunked, then the variable is context. If it's not thunked, it's a
             regular input. *)
          match Marked.unmark scope_let_typ with
          | TArrow ([(TLit TUnit, _)], _) ->
            { info_pure = false; is_scope = false; var = next_var' }
          | _ -> { info_pure = false; is_scope = false; var = next_var' })
        | ScopeVarDefinition | SubScopeVarDefinition | CallingSubScope
        | DestructuringSubScopeResults | Assertion ->
          { info_pure = false; is_scope = false; var = next_var' })
        ctx
    in

    let next_var = next_var' in
    let next_body = trans_scope_body_expr ctx' next_body in

    let scope_let_next = Bindlib.bind_var next_var next_body in

    let scope_let_expr = Expr.Box.lift @@ trans ctx scope_let_expr in

    Bindlib.box_apply2
      (fun scope_let_expr scope_let_next ->
        {
          scope_let_kind;
          scope_let_typ = trans_typ scope_let_typ;
          scope_let_expr;
          scope_let_next;
          scope_let_pos;
        })
      scope_let_expr scope_let_next

and trans_scope_body_expr ctx s :
    (lcalc, typed mark) gexpr scope_body_expr Bindlib.box =
  match s with
  | Result e -> begin
    (* invariant : result is always in the form of a record. *)
    match Marked.unmark e with
    | EStruct { name; fields } ->
      Bindlib.box_apply
        (fun e -> Result e)
        (Expr.Box.lift
        @@ Expr.estruct name
             (StructField.Map.map (trans ctx) fields)
             (Marked.get_mark e))
    | _ -> assert false
  end
  | ScopeLet s ->
    Bindlib.box_apply (fun s -> ScopeLet s) (trans_scope_let ctx s)

let trans_scope_body
    ctx
    { scope_body_input_struct; scope_body_output_struct; scope_body_expr } =
  let var, body = Bindlib.unbind scope_body_expr in
  let body =
    trans_scope_body_expr
      (Var.Map.add var
         { info_pure = true; is_scope = false; var = Var.translate var }
         ctx)
      body
  in
  let binder = Bindlib.bind_var (Var.translate var) body in
  Bindlib.box_apply
    (fun scope_body_expr ->
      { scope_body_input_struct; scope_body_output_struct; scope_body_expr })
    binder

let rec trans_code_items ctx c :
    (lcalc, typed mark) gexpr code_item_list Bindlib.box =
  match c with
  | Nil -> Bindlib.box Nil
  | Cons (c, next) -> (
    let var, next = Bindlib.unbind next in
    match c with
    | Topdef (name, typ, e) ->
      let next =
        Bindlib.bind_var (Var.translate var)
          (trans_code_items
             (Var.Map.add var
                { info_pure = false; is_scope = false; var = Var.translate var }
                ctx)
             next)
      in
      let e = Expr.Box.lift @@ trans ctx e in
      (* TODO: need to add an error_on_empty *)
      Bindlib.box_apply2
        (fun next e -> Cons (Topdef (name, trans_typ typ, e), next))
        next e
    | ScopeDef (name, body) ->
      let next =
        Bindlib.bind_var (Var.translate var)
          (trans_code_items
             (Var.Map.add var
                { info_pure = true; is_scope = true; var = Var.translate var }
                ctx)
             next)
      in
      let body = trans_scope_body ctx body in
      Bindlib.box_apply2
        (fun next body -> Cons (ScopeDef (name, body), next))
        next body)

let rec translate_typ (tau : typ) : typ =
  let m = Marked.get_mark tau in
  (Fun.flip Marked.same_mark_as)
    tau
    begin
      match Marked.unmark tau with
      | TLit l -> TLit l
      | TTuple ts -> TTuple (List.map translate_typ ts)
      | TStruct s -> TStruct s
      | TEnum en -> TEnum en
      | TOption _ -> assert false
      | TAny -> TAny
      | TArray ts ->
        TArray (TOption (translate_typ ts), m) (* catala is not polymorphic *)
      | TArrow ([(TLit TUnit, _)], t2) -> Marked.unmark (translate_typ t2)
      | TArrow (t1, t2) ->
        TArrow (List.map translate_typ t1, (TOption (translate_typ t2), m))
    end

let translate_typ (tau : typ) : typ =
  Marked.same_mark_as (TOption (translate_typ tau)) tau

let translate_program (prgm : typed D.program) : untyped A.program =
  (* let inputs_structs = Scope.fold_left prgm.code_items ~init:[] ~f:(fun acc
     def _ -> match def with | ScopeDef (_, body) ->
     body.scope_body_input_struct :: acc | Topdef _ -> acc) in *)
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
        |> StructName.Map.mapi (fun _n str ->
               StructField.Map.map translate_typ str
               (* Cli.debug_print @@ Format.asprintf "Input type: %a" (Print.typ
                  decl_ctx) tau; Cli.debug_print @@ Format.asprintf "Output
                  type: %a" (Print.typ decl_ctx) (translate_typ tau); *));
    }
  in

  let code_items = trans_code_items Var.Map.empty prgm.code_items in

  let fv = Ren.Set.elements (Ctx.free_vars code_items) in

  ListLabels.iter fv ~f:(fun s -> Cli.debug_format "freevar: %s" s);

  (* assert (Bindlib.free_vars code_items = Bindlib.empty_ctxt); *)
  (* assert (Bindlib.is_closed code_items); *)
  let code_items = Bindlib.unbox code_items in

  Program.untype { decl_ctx; code_items }
