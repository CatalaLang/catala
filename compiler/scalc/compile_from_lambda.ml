(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2021 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

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
module A = Ast
module L = Lcalc.Ast
module D = Dcalc.Ast

type 'm ctxt = {
  func_dict : ('m L.expr, A.TopLevelName.t) Var.Map.t;
  decl_ctx : decl_ctx;
  var_dict : ('m L.expr, A.LocalName.t) Var.Map.t;
  inside_definition_of : A.LocalName.t option;
  context_name : string;
}

(* Expressions can spill out side effect, hence this function also returns a
   list of statements to be prepended before the expression is evaluated *)
let rec translate_expr (ctxt : 'm ctxt) (expr : 'm L.marked_expr) :
    A.block * A.expr Marked.pos =
  match Marked.unmark expr with
  | EVar v ->
    let local_var =
      try A.EVar (Var.Map.find v ctxt.var_dict)
      with Not_found -> A.EFunc (Var.Map.find v ctxt.func_dict)
    in
    [], (local_var, Expr.pos expr)
  | ETuple (args, Some s_name) ->
    let args_stmts, new_args =
      List.fold_left
        (fun (args_stmts, new_args) arg ->
          let arg_stmts, new_arg = translate_expr ctxt arg in
          arg_stmts @ args_stmts, new_arg :: new_args)
        ([], []) args
    in
    let new_args = List.rev new_args in
    let args_stmts = List.rev args_stmts in
    args_stmts, (A.EStruct (new_args, s_name), Expr.pos expr)
  | ETuple (_, None) -> failwith "Non-struct tuples cannot be compiled to scalc"
  | ETupleAccess (e1, num_field, Some s_name, _) ->
    let e1_stmts, new_e1 = translate_expr ctxt e1 in
    let field_name =
      fst (List.nth (StructMap.find s_name ctxt.decl_ctx.ctx_structs) num_field)
    in
    e1_stmts, (A.EStructFieldAccess (new_e1, field_name, s_name), Expr.pos expr)
  | ETupleAccess (_, _, None, _) ->
    failwith "Non-struct tuples cannot be compiled to scalc"
  | EInj (e1, num_cons, e_name, _) ->
    let e1_stmts, new_e1 = translate_expr ctxt e1 in
    let cons_name =
      fst (List.nth (EnumMap.find e_name ctxt.decl_ctx.ctx_enums) num_cons)
    in
    e1_stmts, (A.EInj (new_e1, cons_name, e_name), Expr.pos expr)
  | EApp (f, args) ->
    let f_stmts, new_f = translate_expr ctxt f in
    let args_stmts, new_args =
      List.fold_left
        (fun (args_stmts, new_args) arg ->
          let arg_stmts, new_arg = translate_expr ctxt arg in
          arg_stmts @ args_stmts, new_arg :: new_args)
        ([], []) args
    in
    let new_args = List.rev new_args in
    f_stmts @ args_stmts, (A.EApp (new_f, new_args), Expr.pos expr)
  | EArray args ->
    let args_stmts, new_args =
      List.fold_left
        (fun (args_stmts, new_args) arg ->
          let arg_stmts, new_arg = translate_expr ctxt arg in
          arg_stmts @ args_stmts, new_arg :: new_args)
        ([], []) args
    in
    let new_args = List.rev new_args in
    args_stmts, (A.EArray new_args, Expr.pos expr)
  | EOp op -> [], (A.EOp op, Expr.pos expr)
  | ELit l -> [], (A.ELit l, Expr.pos expr)
  | _ ->
    let tmp_var =
      A.LocalName.fresh
        ( (*This piece of logic is used to make the code more readable. TODO:
            should be removed when
            https://github.com/CatalaLang/catala/issues/240 is fixed. *)
          (match ctxt.inside_definition_of with
          | None -> ctxt.context_name
          | Some v ->
            let v = Marked.unmark (A.LocalName.get_info v) in
            let tmp_rex = Re.Pcre.regexp "^temp_" in
            if Re.Pcre.pmatch ~rex:tmp_rex v then v else "temp_" ^ v),
          Expr.pos expr )
    in
    let ctxt =
      {
        ctxt with
        inside_definition_of = Some tmp_var;
        context_name = Marked.unmark (A.LocalName.get_info tmp_var);
      }
    in
    let tmp_stmts = translate_statements ctxt expr in
    ( ( A.SLocalDecl ((tmp_var, Expr.pos expr), (TAny, Expr.pos expr)),
        Expr.pos expr )
      :: tmp_stmts,
      (A.EVar tmp_var, Expr.pos expr) )

and translate_statements (ctxt : 'm ctxt) (block_expr : 'm L.marked_expr) :
    A.block =
  match Marked.unmark block_expr with
  | EAssert e ->
    (* Assertions are always encapsulated in a unit-typed let binding *)
    let e_stmts, new_e = translate_expr ctxt e in
    e_stmts @ [A.SAssert (Marked.unmark new_e), Expr.pos block_expr]
  | EApp ((EAbs (binder, taus), binder_mark), args) ->
    (* This defines multiple local variables at the time *)
    let binder_pos = Expr.mark_pos binder_mark in
    let vars, body = Bindlib.unmbind binder in
    let vars_tau = List.map2 (fun x tau -> x, tau) (Array.to_list vars) taus in
    let ctxt =
      {
        ctxt with
        var_dict =
          List.fold_left
            (fun var_dict (x, _) ->
              Var.Map.add x
                (A.LocalName.fresh (Bindlib.name_of x, binder_pos))
                var_dict)
            ctxt.var_dict vars_tau;
      }
    in
    let local_decls =
      List.map
        (fun (x, tau) ->
          ( A.SLocalDecl ((Var.Map.find x ctxt.var_dict, binder_pos), tau),
            binder_pos ))
        vars_tau
    in
    let vars_args =
      List.map2
        (fun (x, tau) arg ->
          (Var.Map.find x ctxt.var_dict, binder_pos), tau, arg)
        vars_tau args
    in
    let def_blocks =
      List.map
        (fun (x, _tau, arg) ->
          let ctxt =
            {
              ctxt with
              inside_definition_of = Some (Marked.unmark x);
              context_name =
                Marked.unmark (A.LocalName.get_info (Marked.unmark x));
            }
          in
          let arg_stmts, new_arg = translate_expr ctxt arg in
          arg_stmts @ [A.SLocalDef (x, new_arg), binder_pos])
        vars_args
    in
    let rest_of_block = translate_statements ctxt body in
    local_decls @ List.flatten def_blocks @ rest_of_block
  | EAbs (binder, taus) ->
    let vars, body = Bindlib.unmbind binder in
    let binder_pos = Expr.pos block_expr in
    let vars_tau = List.map2 (fun x tau -> x, tau) (Array.to_list vars) taus in
    let closure_name =
      match ctxt.inside_definition_of with
      | None -> A.LocalName.fresh (ctxt.context_name, Expr.pos block_expr)
      | Some x -> x
    in
    let ctxt =
      {
        ctxt with
        var_dict =
          List.fold_left
            (fun var_dict (x, _) ->
              Var.Map.add x
                (A.LocalName.fresh (Bindlib.name_of x, binder_pos))
                var_dict)
            ctxt.var_dict vars_tau;
        inside_definition_of = None;
      }
    in
    let new_body = translate_statements ctxt body in
    [
      ( A.SInnerFuncDef
          ( (closure_name, binder_pos),
            {
              func_params =
                List.map
                  (fun (var, tau) ->
                    (Var.Map.find var ctxt.var_dict, binder_pos), tau)
                  vars_tau;
              func_body = new_body;
            } ),
        binder_pos );
    ]
  | EMatch (e1, args, e_name) ->
    let e1_stmts, new_e1 = translate_expr ctxt e1 in
    let new_args =
      List.fold_left
        (fun new_args arg ->
          match Marked.unmark arg with
          | EAbs (binder, _) ->
            let vars, body = Bindlib.unmbind binder in
            assert (Array.length vars = 1);
            let var = vars.(0) in
            let scalc_var =
              A.LocalName.fresh (Bindlib.name_of var, Expr.pos arg)
            in
            let ctxt =
              { ctxt with var_dict = Var.Map.add var scalc_var ctxt.var_dict }
            in
            let new_arg = translate_statements ctxt body in
            (new_arg, scalc_var) :: new_args
          | _ -> assert false
          (* should not happen *))
        [] args
    in
    let new_args = List.rev new_args in
    e1_stmts @ [A.SSwitch (new_e1, e_name, new_args), Expr.pos block_expr]
  | EIfThenElse (cond, e_true, e_false) ->
    let cond_stmts, s_cond = translate_expr ctxt cond in
    let s_e_true = translate_statements ctxt e_true in
    let s_e_false = translate_statements ctxt e_false in
    cond_stmts
    @ [A.SIfThenElse (s_cond, s_e_true, s_e_false), Expr.pos block_expr]
  | ECatch (e_try, except, e_catch) ->
    let s_e_try = translate_statements ctxt e_try in
    let s_e_catch = translate_statements ctxt e_catch in
    [A.STryExcept (s_e_try, except, s_e_catch), Expr.pos block_expr]
  | ERaise except ->
    (* Before raising the exception, we still give a dummy definition to the
       current variable so that tools like mypy don't complain. *)
    (match ctxt.inside_definition_of with
    | None -> []
    | Some x ->
      [
        ( A.SLocalDef
            ( (x, Expr.pos block_expr),
              (Ast.EVar Ast.dead_value, Expr.pos block_expr) ),
          Expr.pos block_expr );
      ])
    @ [A.SRaise except, Expr.pos block_expr]
  | _ -> (
    let e_stmts, new_e = translate_expr ctxt block_expr in
    e_stmts
    @
    match e_stmts with
    | (A.SRaise _, _) :: _ ->
      (* if the last statement raises an exception, then we don't need to return
         or to define the current variable since this code will be
         unreachable *)
      []
    | _ ->
      [
        ( (match ctxt.inside_definition_of with
          | None -> A.SReturn (Marked.unmark new_e)
          | Some x -> A.SLocalDef (Marked.same_mark_as x new_e, new_e)),
          Expr.pos block_expr );
      ])

let rec translate_scope_body_expr
    (scope_name : ScopeName.t)
    (decl_ctx : decl_ctx)
    (var_dict : ('m L.expr, A.LocalName.t) Var.Map.t)
    (func_dict : ('m L.expr, A.TopLevelName.t) Var.Map.t)
    (scope_expr : 'm L.expr scope_body_expr) : A.block =
  match scope_expr with
  | Result e ->
    let block, new_e =
      translate_expr
        {
          decl_ctx;
          func_dict;
          var_dict;
          inside_definition_of = None;
          context_name = Marked.unmark (ScopeName.get_info scope_name);
        }
        e
    in
    block @ [A.SReturn (Marked.unmark new_e), Marked.get_mark new_e]
  | ScopeLet scope_let ->
    let let_var, scope_let_next = Bindlib.unbind scope_let.scope_let_next in
    let let_var_id =
      A.LocalName.fresh (Bindlib.name_of let_var, scope_let.scope_let_pos)
    in
    let new_var_dict = Var.Map.add let_var let_var_id var_dict in
    (match scope_let.scope_let_kind with
    | Assertion ->
      translate_statements
        {
          decl_ctx;
          func_dict;
          var_dict;
          inside_definition_of = Some let_var_id;
          context_name = Marked.unmark (ScopeName.get_info scope_name);
        }
        scope_let.scope_let_expr
    | _ ->
      let let_expr_stmts, new_let_expr =
        translate_expr
          {
            decl_ctx;
            func_dict;
            var_dict;
            inside_definition_of = Some let_var_id;
            context_name = Marked.unmark (ScopeName.get_info scope_name);
          }
          scope_let.scope_let_expr
      in
      let_expr_stmts
      @ [
          ( A.SLocalDecl
              ((let_var_id, scope_let.scope_let_pos), scope_let.scope_let_typ),
            scope_let.scope_let_pos );
          ( A.SLocalDef ((let_var_id, scope_let.scope_let_pos), new_let_expr),
            scope_let.scope_let_pos );
        ])
    @ translate_scope_body_expr scope_name decl_ctx new_var_dict func_dict
        scope_let_next

let translate_program (p : 'm L.program) : A.program =
  {
    decl_ctx = p.decl_ctx;
    scopes =
      (let _, new_scopes =
         Scope.fold_left
           ~f:(fun (func_dict, new_scopes) scope_def scope_var ->
             let scope_input_var, scope_body_expr =
               Bindlib.unbind scope_def.scope_body.scope_body_expr
             in
             let input_pos =
               Marked.get_mark (ScopeName.get_info scope_def.scope_name)
             in
             let scope_input_var_id =
               A.LocalName.fresh (Bindlib.name_of scope_input_var, input_pos)
             in
             let var_dict =
               Var.Map.singleton scope_input_var scope_input_var_id
             in
             let new_scope_body =
               translate_scope_body_expr scope_def.scope_name p.decl_ctx
                 var_dict func_dict scope_body_expr
             in
             let func_id =
               A.TopLevelName.fresh (Bindlib.name_of scope_var, Pos.no_pos)
             in
             let func_dict = Var.Map.add scope_var func_id func_dict in
             ( func_dict,
               {
                 Ast.scope_body_name = scope_def.scope_name;
                 Ast.scope_body_var = func_id;
                 scope_body_func =
                   {
                     A.func_params =
                       [
                         ( (scope_input_var_id, input_pos),
                           ( TTuple
                               ( List.map snd
                                   (StructMap.find
                                      scope_def.scope_body
                                        .scope_body_input_struct
                                      p.decl_ctx.ctx_structs),
                                 Some
                                   scope_def.scope_body.scope_body_input_struct
                               ),
                             input_pos ) );
                       ];
                     A.func_body = new_scope_body;
                   };
               }
               :: new_scopes ))
           ~init:
             ( (if !Cli.avoid_exceptions_flag then
                Var.Map.singleton L.handle_default_opt A.handle_default_opt
               else Var.Map.singleton L.handle_default A.handle_default),
               [] )
           p.scopes
       in
       List.rev new_scopes);
  }
