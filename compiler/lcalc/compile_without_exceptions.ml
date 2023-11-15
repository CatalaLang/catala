(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Alain Delaët-Tixeuil <alain.delaet--tixeuil@inria.fr>

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
open Shared_ast
module D = Dcalc.Ast
module A = Ast

(** We make use of the strong invriants on the structure of programs:
    Defaultable values can only appear in certin positions. This information is
    given by the type structure of expressions. In particular this mean we don't
    need to use the monadic bind while computing arithmetic opertions or
    function calls. The resulting function is not more difficult than what we
    had when translating without exceptions.

    The typing translation is to simply trnsform defult type into option types. *)

let rec translate_typ (tau : typ) : typ =
  Mark.copy tau
    begin
      match Mark.remove tau with
      | TDefault t -> TOption (translate_typ t)
      | TLit l -> TLit l
      | TTuple ts -> TTuple (List.map translate_typ ts)
      | TStruct s -> TStruct s
      | TEnum en -> TEnum en
      | TOption _ ->
        Message.raise_internal_error
          "The types option should not appear before the dcalc -> lcalc \
           translation step."
      | TClosureEnv ->
        Message.raise_internal_error
          "The types closure_env should not appear before the dcalc -> lcalc \
           translation step."
      | TAny -> TAny
      | TArray ts -> TArray (translate_typ ts)
      | TArrow (t1, t2) -> TArrow (List.map translate_typ t1, translate_typ t2)
    end

let rec translate_default
    (exceptions : 'm D.expr list)
    (just : 'm D.expr)
    (cons : 'm D.expr)
    (mark_default : 'm mark) : 'm A.expr boxed =
  (* Since the program is well typed, all exceptions have as type [option 't] *)
  let exceptions = List.map translate_expr exceptions in
  let pos = Expr.mark_pos mark_default in
  let exceptions =
    Expr.make_app
      (Expr.eop Op.HandleDefaultOpt
         [TAny, pos; TAny, pos; TAny, pos]
         (Expr.no_mark mark_default))
      [
        Expr.earray exceptions mark_default;
        (* In call-by-value programming languages, as lcalc, arguments are
           evalulated before calling the function. Since we don't want to
           execute the justification and conclusion while before checking every
           exceptions, we need to thunk them. *)
        Expr.thunk_term (translate_expr just) (Mark.get just);
        Expr.thunk_term (translate_expr cons) (Mark.get cons);
      ]
      pos
  in
  exceptions

and translate_expr (e : 'm D.expr) : 'm A.expr boxed =
  let mark = Mark.get e in
  match Mark.remove e with
  | EEmptyError -> Ast.OptionMonad.empty ~mark
  | EErrorOnEmpty arg ->
    Ast.OptionMonad.error_on_empty ~mark ~var_name:"arg" (translate_expr arg)
  | EDefault { excepts; just; cons } ->
    translate_default excepts just cons (Mark.get e)
  | EPureDefault e -> Ast.OptionMonad.return ~mark (translate_expr e)
  (* As we need to translate types as well as terms, we cannot simply use
     [Expr.map] for terms that contains types. *)
  | EOp { op; tys } ->
    Expr.eop (Operator.translate op) (List.map translate_typ tys) mark
  | EAbs { binder; tys } ->
    let vars, body = Bindlib.unmbind binder in
    let body = translate_expr body in
    let binder = Expr.bind (Array.map Var.translate vars) body in
    let tys = List.map translate_typ tys in
    Expr.eabs binder tys mark
  | ( ELit _ | EApp _ | EArray _ | EVar _ | EExternal _ | EIfThenElse _
    | ETuple _ | ETupleAccess _ | EInj _ | EAssert _ | EStruct _
    | EStructAccess _ | EMatch _ ) as e ->
    Expr.map ~f:translate_expr (Mark.add mark e)
  | _ -> .

let translate_scope_body_expr (scope_body_expr : 'expr1 scope_body_expr) :
    'expr2 scope_body_expr Bindlib.box =
  Scope.fold_right_lets
    ~f:(fun scope_let var_next acc ->
      Bindlib.box_apply2
        (fun scope_let_next scope_let_expr ->
          ScopeLet
            {
              scope_let with
              scope_let_next;
              scope_let_expr;
              scope_let_typ = translate_typ scope_let.scope_let_typ;
            })
        (Bindlib.bind_var (Var.translate var_next) acc)
        (Expr.Box.lift (translate_expr scope_let.scope_let_expr)))
    ~init:(fun res ->
      Bindlib.box_apply
        (fun res -> Result res)
        (Expr.Box.lift (translate_expr res)))
    scope_body_expr

let translate_code_items scopes =
  let f = function
    | ScopeDef (name, body) ->
      let scope_input_var, scope_lets = Bindlib.unbind body.scope_body_expr in
      let new_body_expr = translate_scope_body_expr scope_lets in
      let new_body_expr =
        Bindlib.bind_var (Var.translate scope_input_var) new_body_expr
      in
      Bindlib.box_apply
        (fun scope_body_expr -> ScopeDef (name, { body with scope_body_expr }))
        new_body_expr
    | Topdef (name, typ, expr) ->
      Bindlib.box_apply
        (fun e -> Topdef (name, typ, e))
        (Expr.Box.lift (translate_expr expr))
  in
  Scope.map ~f ~varf:Var.translate scopes

let translate_program (prg : typed D.program) : untyped A.program =
  Program.untype
  @@ Bindlib.unbox
  @@ Bindlib.box_apply
       (fun code_items ->
         let ctx_enums =
           EnumName.Map.map
             (EnumConstructor.Map.map translate_typ)
             prg.decl_ctx.ctx_enums
         in
         let ctx_structs =
           StructName.Map.map
             (StructField.Map.map translate_typ)
             prg.decl_ctx.ctx_structs
         in
         {
           prg with
           code_items;
           decl_ctx = { prg.decl_ctx with ctx_enums; ctx_structs };
         })
       (translate_code_items prg.code_items)
