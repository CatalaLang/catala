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
    same technique as in Rust or Erlang to handle this kind of exceptions. Each
    [raise EmptyError] will be translated as [None] and each
    [try e1 with EmtpyError -> e2] as
    [match e1 with | None -> e2 | Some x -> x].

    When doing this naively, this requires to add matches and [Some] constructor
    everywhere. We apply optimizations to remove the additional constructors.

    The compilation process is handled by the [trans_expr] function. *)

open Shared_ast

(** Start of the translation *)

(** {2 Type translating functions}

    Since positions where there is thunked expressions is exactly where we will
    put option expressions. Hence, the transformation simply reduce [unit -> 'a]
    into ['a option] recursivly. There is no polymorphism inside catala. *)

(** In the general case, we use the [trans_typ_to_any] function to put [TAny]
    and then ask the typing algorithm to reinfer all the types. However, this is
    not sufficient as the typing inference need at least input and output types.
    Those a generated using the [trans_typ_keep] function, that build [TOption]s
    where needed. *)
let trans_typ_to_any (tau : typ) : typ = Mark.copy tau TAny

let rec trans_typ_keep (tau : typ) : typ =
  let m = Mark.get tau in
  Mark.copy tau
    begin
      match Mark.remove tau with
      | TLit l -> TLit l
      | TTuple ts -> TTuple (List.map trans_typ_keep ts)
      | TStruct s -> TStruct s
      | TEnum en -> TEnum en
      | TOption _ | TClosureEnv ->
        Message.raise_internal_error
          "The types option and closure_env should not appear before the dcalc \
           -> lcalc translation step."
      | TAny -> TAny
      | TArray ts ->
        TArray (TOption (trans_typ_keep ts), m) (* catala is not polymorphic *)
      | TArrow ([(TLit TUnit, _)], t2) -> Mark.remove (trans_typ_keep t2)
      | TArrow (t1, t2) ->
        TArrow (List.map trans_typ_keep t1, (TOption (trans_typ_keep t2), m))
    end

let trans_typ_keep (tau : typ) : typ =
  Mark.copy tau (TOption (trans_typ_keep tau))

let trans_op : dcalc Op.t -> lcalc Op.t = Operator.translate

type 'm var_ctx = { info_pure : bool; is_scope : bool; var : 'm Ast.expr Var.t }

type 'm ctx = {
  ctx_vars : ((dcalc, 'm) gexpr, 'm var_ctx) Var.Map.t;
  ctx_context_name : string;
}

let trans_var (ctx : 'm ctx) (x : 'm D.expr Var.t) : 'm Ast.expr Var.t =
  (Var.Map.find x ctx.ctx_vars).var

(** TODO: give better names to variables *)

(** The function [e' = trans ctx e] actually does the translation between
    [D.expr] and [L.expr]. The context links every free variables of the [e]
    expression to a new lcalc variable [var], and some information [info_pure]
    on whenever the variable can be reduced to an [EmptyError], and hence should
    be matched. We also keep [is_scope] to indicate if a variable comes from a
    top-level scope definition. This is used when applying functions as
    described below. Finally, the following invariant is upheld: if [e] is of
    type [a], then the result should be of type [trans_typ_keep a]. For
    literals, this mean that a expression of type [money] will be of type
    [money option]. We rely on later optimization to shorten the size of the
    generated code. *)
let rec trans (ctx : typed ctx) (e : typed D.expr) : (lcalc, typed) boxed_gexpr
    =
  let m = Mark.get e in
  let mark = m in
  let pos = Expr.pos e in
  (* Message.emit_debug "%a" (Print.expr ~debug:true ()) e; *)
  let context_or_same_var (ctx : typed ctx) (e : typed D.expr) : string =
    match Mark.remove e with
    | EInj { e = EVar v, _; _ } | EVar v -> Bindlib.name_of v
    | EInj { e = ELit _, _; _ } | ELit _ -> "lit"
    | _ -> ctx.ctx_context_name
  in
  match Mark.remove e with
  | EVar x ->
    if (Var.Map.find x ctx.ctx_vars).info_pure then
      Ast.OptionMonad.return (Expr.evar (trans_var ctx x) m) ~mark
    else Expr.evar (trans_var ctx x) m
  | EExternal _ as e -> Expr.map ~f:(trans ctx) (e, m)
  | EApp { f = EVar v, _; args = [(ELit LUnit, _)] } ->
    (* Invariant: as users cannot write thunks, it can only come from prior
       compilation passes. Hence we can safely remove those. *)
    assert (not (Var.Map.find v ctx.ctx_vars).info_pure);
    Expr.evar (trans_var ctx v) m
  | EAbs { binder; tys = [(TLit TUnit, _)] } ->
    (* this is to be used with Ast.OptionMonad.bind. *)
    let _, body = Bindlib.unmbind binder in
    trans ctx body
  | EAbs { binder; tys } ->
    (* Every function of type [a -> b] is translated to a function of type [a ->
       option b] *)
    let vars, body = Bindlib.unmbind binder in
    let ctx' =
      ArrayLabels.fold_right vars ~init:ctx ~f:(fun v ctx ->
          {
            ctx with
            ctx_vars =
              Var.Map.add v
                { info_pure = true; is_scope = false; var = Var.translate v }
                ctx.ctx_vars;
          })
    in

    let body' = trans ctx' body in
    let binder' = Expr.bind (Array.map Var.translate vars) body' in
    Ast.OptionMonad.return ~mark (Expr.eabs binder' tys m)
  | EDefault { excepts; just; cons } ->
    let excepts' = List.map (trans ctx) excepts in
    let just' = trans ctx just in
    let cons' = trans ctx cons in

    (* If the default term has the following type [<es: a list | just: bool |-
       cons: a>] then the resulting term will have type [handledefaultOpt (es':
       a option list) (just': bool option) (cons': a option)] *)
    let m' = match m with Typed m -> Typed { m with ty = TAny, pos } in
    Expr.make_app
      (Expr.eop Op.HandleDefaultOpt [TAny, pos; TAny, pos; TAny, pos] m')
      [Expr.earray excepts' m; Expr.thunk_term just' m; Expr.thunk_term cons' m]
      pos
  | ELit l -> Ast.OptionMonad.return ~mark (Expr.elit l m)
  | EEmptyError -> Ast.OptionMonad.empty ~mark
  | EErrorOnEmpty arg ->
    let arg' = trans ctx arg in
    Ast.OptionMonad.error_on_empty arg' ~mark ~toplevel:false
      ~var_name:(context_or_same_var ctx arg)
  | EApp { f = EVar scope, _; args = [(EStruct { fields; name }, _)] }
    when (Var.Map.find scope ctx.ctx_vars).is_scope ->
    (* Scopes are encoded as functions that can take option arguments, and
       always return (or raise panic exceptions like AssertionFailed,
       NoValueProvided or Conflict) a structure that can contain optionnal
       elements. Hence, to call a scope, we don't need to use the monad bind. *)
    Ast.OptionMonad.return ~mark
      (Expr.eapp
         (Expr.evar (trans_var ctx scope) mark)
         [
           Expr.estruct ~name
             ~fields:(StructField.Map.map (trans ctx) fields)
             mark;
         ]
         mark)
  | EApp { f = (EVar ff, _) as f; args }
    when not (Var.Map.find ff ctx.ctx_vars).is_scope ->
    (* INVARIANT: functions are always encoded using this function.

       As every function of type [a -> b] but top-level scopes is built using
       this function, returning a function of type [a -> b option], we should
       use [Ast.OptionMonad.mbind]. *)
    let f_var = Var.make (Bindlib.name_of ff) in
    Ast.OptionMonad.bind_var ~mark
      (Ast.OptionMonad.mbind
         ~var_name:(context_or_same_var ctx (List.hd args))
         (Expr.evar f_var mark)
         (List.map (trans ctx) args)
         ~mark)
      f_var (trans ctx f)
  | EApp { f = (EStructAccess { e = es; _ }, _) as f; args } ->
    (* This occurs when calling a subscope function. The same encoding as the
       one for [EApp (Var _) _] if the variable is not a scope works. *)
    let f_var = Var.make (context_or_same_var ctx es) in
    Ast.OptionMonad.bind_var ~mark
      (Ast.OptionMonad.mbind
         ~var_name:(context_or_same_var ctx es)
         (Expr.evar f_var mark)
         (List.map (trans ctx) args)
         ~mark)
      f_var (trans ctx f)
  | EApp { f = EAbs { binder; _ }, _; args } ->
    (* INVARIANTS: every let have only one argument. (checked by
       invariant_let) *)
    let var, body = Bindlib.unmbind binder in
    let[@warning "-8"] [| var |] = var in
    let var' = Var.translate var in
    let[@warning "-8"] [arg] = args in
    let ctx' =
      {
        ctx_vars =
          Var.Map.add var
            { info_pure = true; is_scope = false; var = var' }
            ctx.ctx_vars;
        ctx_context_name = Bindlib.name_of var;
      }
    in
    Ast.OptionMonad.bind_var (trans ctx' body) var' (trans ctx arg) ~mark
  | EApp { f = EApp { f = EOp { op = Op.Log _; _ }, _; args = _ }, _; _ } ->
    Message.raise_internal_error
      "Parameter trace is incompatible with parameter avoid_exceptions: some \
       tracing logs were added while they are not supported."
  (* Encoding of Fold, Filter, Map and Reduce is non trivial because we don't
     define new monadic operators for every one of those. *)
  | EApp { f = EOp { op = Op.Fold; tys }, opmark; args = [f; init; l] } ->
    (* The function f should have type b -> a -> a. Hence, its translation has
       type [b] -> [a] -> option [a]. But we need a function of type option [b]
       -> option [a] -> option [a] for the type checking of fold. Hence, we
       "iota-expand" the function as follows: [λ x y. bindm x y. [f] x y] *)
    let x1 = Var.make "f" in
    let x2 = Var.make "init" in
    let f' =
      Ast.OptionMonad.bind_cont ~mark
        ~var_name:(context_or_same_var ctx f)
        (fun f' ->
          Ast.OptionMonad.return ~mark
            (Expr.eabs
               (Expr.bind [| x1; x2 |]
                  (Ast.OptionMonad.mbind_cont
                     ~var_name:(context_or_same_var ctx f)
                     ~mark
                     (fun vars ->
                       Expr.eapp (Expr.evar f' m)
                         (ListLabels.map vars ~f:(fun v -> Expr.evar v m))
                         m)
                     [Expr.evar x1 m; Expr.evar x2 m]))
               [TAny, pos; TAny, pos]
               m))
        (trans ctx f)
    in
    Ast.OptionMonad.mbind
      ~var_name:(context_or_same_var ctx f)
      (Expr.eop (trans_op Op.Fold) tys opmark)
      [f'; Ast.OptionMonad.return ~mark (trans ctx init); trans ctx l]
      ~mark
  | EApp { f = EOp { op = Op.Reduce; tys }, opmark; args = [f; init; l] } ->
    let x1 = Var.make "f" in
    let x2 = Var.make "init" in
    let f' =
      Ast.OptionMonad.bind_cont ~mark
        ~var_name:(context_or_same_var ctx f)
        (fun f' ->
          Ast.OptionMonad.return ~mark
            (Expr.eabs
               (Expr.bind [| x1; x2 |]
                  (Ast.OptionMonad.mbind_cont
                     ~var_name:(context_or_same_var ctx f)
                     ~mark
                     (fun vars ->
                       Expr.eapp (Expr.evar f' m)
                         (ListLabels.map vars ~f:(fun v -> Expr.evar v m))
                         m)
                     [Expr.evar x1 m; Expr.evar x2 m]))
               [TAny, pos; TAny, pos]
               m))
        (trans ctx f)
    in
    Ast.OptionMonad.mbind
      ~var_name:(context_or_same_var ctx f)
      (Expr.eop (trans_op Op.Reduce) tys opmark)
      [f'; Ast.OptionMonad.return ~mark (trans ctx init); trans ctx l]
      ~mark
  | EApp { f = EOp { op = Op.Map; tys }, opmark; args = [f; l] } ->
    (* The funtion [f] has type [a -> option b], but Map needs a function of
       type [a -> b], hence we need to transform [f] into a function of type [a
       option -> option b]. *)
    let x1 = Var.make "f" in
    let f' =
      Ast.OptionMonad.bind_cont ~mark ~var_name:ctx.ctx_context_name
        (fun f ->
          Ast.OptionMonad.return ~mark
            (Expr.eabs
               (Expr.bind [| x1 |]
                  (Ast.OptionMonad.mbind_cont ~mark
                     ~var_name:ctx.ctx_context_name
                     (fun vars ->
                       Expr.eapp (Expr.evar f m)
                         (ListLabels.map vars ~f:(fun v -> Expr.evar v m))
                         m)
                     [Expr.evar x1 m]))
               [TAny, pos]
               m))
        (trans ctx f)
    in
    Ast.OptionMonad.mbind_cont
      ~var_name:(context_or_same_var ctx f)
      (fun vars ->
        Ast.OptionMonad.return ~mark
          (Expr.eapp
             (Expr.eop (trans_op Op.Map) tys opmark)
             (ListLabels.map vars ~f:(fun v -> Expr.evar v m))
             mark))
      [f'; trans ctx l]
      ~mark
  | EApp { f = EOp { op = Op.Filter; tys }, opmark; args = [f; l] } ->
    (* The function [f] has type [a -> bool option] while the filter operator
       requires an function of type [a option -> bool]. Hence we need to modify
       [f] by first matching the input, and second using the error_on_empty on
       the result. *)
    let x1 = Var.make (context_or_same_var ctx f) in
    let f' =
      Ast.OptionMonad.bind_cont ~mark
        ~var_name:(context_or_same_var ctx f)
        (fun f' ->
          Ast.OptionMonad.return ~mark
            (Expr.eabs
               (Expr.bind [| x1 |]
                  (Ast.OptionMonad.error_on_empty ~toplevel:true ~mark
                     ~var_name:(context_or_same_var ctx f)
                     (Ast.OptionMonad.mbind_cont ~mark
                        ~var_name:(context_or_same_var ctx f)
                        (fun vars ->
                          Expr.eapp (Expr.evar f' m)
                            (ListLabels.map vars ~f:(fun v -> Expr.evar v m))
                            m)
                        [Expr.evar x1 m])))
               [TAny, pos]
               m))
        (trans ctx f)
    in
    Ast.OptionMonad.mbind_cont
      ~var_name:(context_or_same_var ctx f)
      (fun vars ->
        Ast.OptionMonad.return ~mark
          (Expr.eapp
             (Expr.eop (trans_op Op.Filter) tys opmark)
             (ListLabels.map vars ~f:(fun v -> Expr.evar v m))
             mark))
      [f'; trans ctx l]
      ~mark
  | EApp { f = EOp { op = Op.Filter as op; _ }, _; _ }
  | EApp { f = EOp { op = Op.Map as op; _ }, _; _ }
  | EApp { f = EOp { op = Op.Fold as op; _ }, _; _ }
  | EApp { f = EOp { op = Op.Reduce as op; _ }, _; _ } ->
    (* Cannot happend: list operator must be fully determined *)
    Message.raise_internal_error
      "List operator %a was not fully determined: some partial evaluation was \
       found while compiling."
      (Print.operator ~debug:false)
      op
  | EApp { f = EOp { op; tys }, opmark; args } ->
    let res =
      Ast.OptionMonad.mmap
        ~var_name:(context_or_same_var ctx (List.hd args))
        (Expr.eop (trans_op op) tys opmark)
        (List.map (trans ctx) args)
        ~mark
    in
    res
  | EMatch { name; e; cases } ->
    let cases =
      EnumConstructor.Map.map
        (fun case ->
          match Mark.remove case with
          | EAbs { binder; tys } ->
            let vars, body = Bindlib.unmbind binder in
            let ctx' =
              ArrayLabels.fold_right vars ~init:ctx ~f:(fun var ctx ->
                  {
                    ctx with
                    ctx_vars =
                      Var.Map.add var
                        {
                          info_pure = true;
                          is_scope = false;
                          var = Var.translate var;
                        }
                        ctx.ctx_vars;
                  })
            in
            let binder =
              Expr.bind (Array.map Var.translate vars) (trans ctx' body)
            in
            Expr.eabs binder tys m
          | _ -> assert false)
        cases
    in
    Ast.OptionMonad.bind_cont
      ~var_name:(context_or_same_var ctx e)
      (fun e -> Expr.ematch ~e:(Expr.evar e m) ~name ~cases m)
      (trans ctx e) ~mark
  | EArray args ->
    Ast.OptionMonad.mbind_cont ~mark ~var_name:ctx.ctx_context_name
      (fun vars ->
        Ast.OptionMonad.return ~mark
          (Expr.earray
             (List.map
                (fun v -> Ast.OptionMonad.return ~mark (Expr.evar v m))
                vars)
             mark))
      (List.map (trans ctx) args)
  | EStruct { name; fields } ->
    let fields_name, fields = List.split (StructField.Map.bindings fields) in
    Ast.OptionMonad.mbind_cont ~var_name:ctx.ctx_context_name
      (fun xs ->
        let fields =
          ListLabels.fold_right2 fields_name
            (List.map
               (fun x -> Ast.OptionMonad.return ~mark (Expr.evar x mark))
               xs)
            ~f:StructField.Map.add ~init:StructField.Map.empty
        in
        Ast.OptionMonad.return ~mark (Expr.estruct ~name ~fields mark))
      (List.map (trans ctx) fields)
      ~mark
  | EIfThenElse { cond; etrue; efalse } ->
    Ast.OptionMonad.bind_cont ~mark
      ~var_name:(context_or_same_var ctx cond)
      (fun cond ->
        Expr.eifthenelse (Expr.evar cond mark) (trans ctx etrue)
          (trans ctx efalse) mark)
      (trans ctx cond)
  | EInj { name; cons; e } ->
    Ast.OptionMonad.bind_cont
      ~var_name:(context_or_same_var ctx e)
      (fun e ->
        Ast.OptionMonad.return ~mark
          (Expr.einj ~e:(Expr.evar e mark) ~cons ~name mark))
      (trans ctx e) ~mark
  | EStructAccess { name; e; field } ->
    Ast.OptionMonad.bind_cont
      ~var_name:(context_or_same_var ctx e)
      (fun e -> Expr.estructaccess ~e:(Expr.evar e mark) ~field ~name mark)
      (trans ctx e) ~mark
  | ETuple args ->
    Ast.OptionMonad.mbind_cont ~var_name:ctx.ctx_context_name
      (fun xs ->
        Ast.OptionMonad.return ~mark
          (Expr.etuple (List.map (fun x -> Expr.evar x mark) xs) mark))
      (List.map (trans ctx) args)
      ~mark
  | ETupleAccess { e; index; size } ->
    Ast.OptionMonad.bind_cont
      ~var_name:(context_or_same_var ctx e)
      (fun e -> Expr.etupleaccess (Expr.evar e mark) index size mark)
      (trans ctx e) ~mark
  | EAssert e ->
    Ast.OptionMonad.bind_cont
      ~var_name:(context_or_same_var ctx e)
      (fun e ->
        Ast.OptionMonad.return ~mark (Expr.eassert (Expr.evar e mark) mark))
      (trans ctx e) ~mark
  | EApp _ ->
    Message.raise_spanned_error (Expr.pos e)
      "Internal Error: found an EApp that does not satisfy the invariants when \
       translating Dcalc to Lcalc without exceptions."
  (* invalid invariant *)
  | EOp _ ->
    Message.raise_spanned_error (Expr.pos e)
      "Internal Error: found an EOp that does not satisfy the invariants when \
       translating Dcalc to Lcalc without exceptions."
  | ELocation _ -> .

(** Now we have translated expression, we still need to translate the statements
    (scope_let_list) and then scopes. This is pretty much straightforward. *)
let rec trans_scope_let (ctx : typed ctx) (s : typed D.expr scope_let) =
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
      {
        ctx with
        ctx_vars =
          Var.Map.add next_var
            { info_pure = false; is_scope = false; var = next_var' }
            ctx.ctx_vars;
      }
    in
    let next_var = next_var' in
    let next_body = trans_scope_body_expr ctx' next_body in
    let scope_let_next = Bindlib.bind_var next_var next_body in
    let scope_let_expr =
      Expr.Box.lift
      @@ trans
           { ctx with ctx_context_name = Bindlib.name_of next_var }
           scope_let_expr
    in
    Bindlib.box_apply2
      (fun scope_let_expr scope_let_next ->
        {
          scope_let_kind = SubScopeVarDefinition;
          scope_let_typ = trans_typ_to_any scope_let_typ;
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
      {
        ctx with
        ctx_vars =
          Var.Map.add next_var
            { info_pure = false; is_scope = false; var = next_var' }
            ctx.ctx_vars;
      }
    in
    let next_var = next_var' in
    let next_body = trans_scope_body_expr ctx' next_body in
    let scope_let_next = Bindlib.bind_var next_var next_body in
    let scope_let_expr =
      Expr.Box.lift
      @@ trans
           { ctx with ctx_context_name = Bindlib.name_of next_var }
           scope_let_expr
    in
    Bindlib.box_apply2
      (fun scope_let_expr scope_let_next ->
        {
          scope_let_kind = SubScopeVarDefinition;
          scope_let_typ = trans_typ_to_any scope_let_typ;
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
      {
        ctx with
        ctx_vars =
          Var.Map.add next_var
            { info_pure = true; is_scope = false; var = next_var' }
            ctx.ctx_vars;
      }
    in
    let next_var = next_var' in
    let next_body = trans_scope_body_expr ctx' next_body in
    let scope_let_next = Bindlib.bind_var next_var next_body in
    let scope_let_expr =
      Expr.Box.lift
      @@ Ast.OptionMonad.error_on_empty ~mark ~toplevel:true
           ~var_name:ctx.ctx_context_name
           (trans { ctx with ctx_context_name = Bindlib.name_of next_var } e)
    in
    Bindlib.box_apply2
      (fun scope_let_expr scope_let_next ->
        {
          scope_let_kind = SubScopeVarDefinition;
          scope_let_typ = trans_typ_to_any scope_let_typ;
          scope_let_expr;
          scope_let_next;
          scope_let_pos;
        })
      scope_let_expr scope_let_next
  | { scope_let_kind = SubScopeVarDefinition; scope_let_pos = pos; _ } ->
    Message.raise_spanned_error pos
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
      {
        ctx with
        ctx_vars =
          Var.Map.add next_var
            (match scope_let_kind with
            | DestructuringInputStruct -> (
              (* note for future: we keep this useless match for distinguishing
                 further optimization while building the terms. *)
              match Mark.remove scope_let_typ with
              | TArrow ([(TLit TUnit, _)], _) ->
                { info_pure = false; is_scope = false; var = next_var' }
              | _ -> { info_pure = false; is_scope = false; var = next_var' })
            | ScopeVarDefinition | SubScopeVarDefinition | CallingSubScope
            | DestructuringSubScopeResults | Assertion ->
              { info_pure = false; is_scope = false; var = next_var' })
            ctx.ctx_vars;
      }
    in
    let next_var = next_var' in
    let next_body = trans_scope_body_expr ctx' next_body in
    let scope_let_next = Bindlib.bind_var next_var next_body in
    let scope_let_expr =
      Expr.Box.lift
      @@ trans
           { ctx with ctx_context_name = Bindlib.name_of next_var }
           scope_let_expr
    in
    Bindlib.box_apply2
      (fun scope_let_expr scope_let_next ->
        {
          scope_let_kind;
          scope_let_typ = trans_typ_to_any scope_let_typ;
          scope_let_expr;
          scope_let_next;
          scope_let_pos;
        })
      scope_let_expr scope_let_next

and trans_scope_body_expr ctx s :
    (lcalc, typed) gexpr scope_body_expr Bindlib.box =
  match s with
  | Result e -> begin
    (* invariant : result is always in the form of a record. *)
    match Mark.remove e with
    | EStruct { name; fields } ->
      Bindlib.box_apply
        (fun e -> Result e)
        (Expr.Box.lift
        @@ Expr.estruct ~name
             ~fields:(StructField.Map.map (trans ctx) fields)
             (Mark.get e))
    | _ -> assert false
  end
  | ScopeLet s ->
    Bindlib.box_apply (fun s -> ScopeLet s) (trans_scope_let ctx s)

let trans_scope_body
    (ctx : typed ctx)
    ({ scope_body_input_struct; scope_body_output_struct; scope_body_expr } :
      typed D.expr scope_body) =
  let var, body = Bindlib.unbind scope_body_expr in
  let body =
    trans_scope_body_expr
      {
        ctx_vars =
          Var.Map.add var
            { info_pure = true; is_scope = false; var = Var.translate var }
            ctx.ctx_vars;
        ctx_context_name = Bindlib.name_of var;
      }
      body
  in
  let binder = Bindlib.bind_var (Var.translate var) body in
  Bindlib.box_apply
    (fun scope_body_expr ->
      { scope_body_input_struct; scope_body_output_struct; scope_body_expr })
    binder

let rec trans_code_items (ctx : typed ctx) (c : typed D.expr code_item_list) :
    (lcalc, typed) gexpr code_item_list Bindlib.box =
  match c with
  | Nil -> Bindlib.box Nil
  | Cons (c, next) -> (
    let var, next = Bindlib.unbind next in
    match c with
    | Topdef (name, typ, e) ->
      let next =
        Bindlib.bind_var (Var.translate var)
          (trans_code_items
             {
               ctx_vars =
                 Var.Map.add var
                   {
                     info_pure = false;
                     is_scope = false;
                     var = Var.translate var;
                   }
                   ctx.ctx_vars;
               ctx_context_name = fst (TopdefName.get_info name);
             }
             next)
      in
      let e = Expr.Box.lift @@ trans ctx e in
      (* Invariant: We suppose there are no defaults in toplevel definitions,
         hence we don't need to add an error_on_empty *)
      Bindlib.box_apply2
        (fun next e -> Cons (Topdef (name, trans_typ_to_any typ, e), next))
        next e
    | ScopeDef (name, body) ->
      let next =
        Bindlib.bind_var (Var.translate var)
          (trans_code_items
             {
               ctx_vars =
                 Var.Map.add var
                   {
                     info_pure = true;
                     is_scope = true;
                     var = Var.translate var;
                   }
                   ctx.ctx_vars;
               ctx_context_name = fst (ScopeName.get_info name);
             }
             next)
      in
      let body = trans_scope_body ctx body in
      Bindlib.box_apply2
        (fun next body -> Cons (ScopeDef (name, body), next))
        next body)

let translate_program (prgm : typed D.program) : untyped A.program =
  let decl_ctx =
    {
      prgm.decl_ctx with
      ctx_enums =
        prgm.decl_ctx.ctx_enums
        |> EnumName.Map.add Expr.option_enum Expr.option_enum_config;
    }
  in
  let decl_ctx =
    {
      decl_ctx with
      ctx_structs =
        prgm.decl_ctx.ctx_structs
        |> StructName.Map.mapi (fun _n str ->
               StructField.Map.map trans_typ_keep str);
    }
  in

  let code_items =
    trans_code_items
      { ctx_vars = Var.Map.empty; ctx_context_name = "" }
      prgm.code_items
  in

  Expr.Box.assert_closed code_items;

  (* program is closed here. *)
  let code_items = Bindlib.unbox code_items in

  Program.untype { prgm with decl_ctx; code_items }
