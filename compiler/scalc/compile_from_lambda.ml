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
module A = Ast
module L = Lcalc.Ast
module D = Dcalc.Ast

type ctxt = {
  func_dict : A.TopLevelName.t L.VarMap.t;
  decl_ctx : D.decl_ctx;
  var_dict : A.LocalName.t L.VarMap.t;
  inside_definition_of : A.LocalName.t option;
  context_name : string;
}

(* Expressions can spill out side effect, hence this function also returns a
   list of statements to be prepended before the expression is evaluated *)
let rec translate_expr (ctxt : ctxt) (expr : L.expr Pos.marked) :
    A.block * A.expr Pos.marked =
  match Pos.unmark expr with
  | L.EVar v ->
      let local_var =
        try A.EVar (L.VarMap.find (Pos.unmark v) ctxt.var_dict)
        with Not_found ->
          A.EFunc (L.VarMap.find (Pos.unmark v) ctxt.func_dict)
      in
      ([], (local_var, Pos.get_position v))
  | L.ETuple (args, Some s_name) ->
      let args_stmts, new_args =
        List.fold_left
          (fun (args_stmts, new_args) arg ->
            let arg_stmts, new_arg = translate_expr ctxt arg in
            (arg_stmts @ args_stmts, new_arg :: new_args))
          ([], []) args
      in
      let new_args = List.rev new_args in
      let args_stmts = List.rev args_stmts in
      (args_stmts, (A.EStruct (new_args, s_name), Pos.get_position expr))
  | L.ETuple (_, None) ->
      failwith "Non-struct tuples cannot be compiled to scalc"
  | L.ETupleAccess (e1, num_field, Some s_name, _) ->
      let e1_stmts, new_e1 = translate_expr ctxt e1 in
      let field_name =
        fst
          (List.nth
             (D.StructMap.find s_name ctxt.decl_ctx.ctx_structs)
             num_field)
      in
      ( e1_stmts,
        ( A.EStructFieldAccess (new_e1, field_name, s_name),
          Pos.get_position expr ) )
  | L.ETupleAccess (_, _, None, _) ->
      failwith "Non-struct tuples cannot be compiled to scalc"
  | L.EInj (e1, num_cons, e_name, _) ->
      let e1_stmts, new_e1 = translate_expr ctxt e1 in
      let cons_name =
        fst (List.nth (D.EnumMap.find e_name ctxt.decl_ctx.ctx_enums) num_cons)
      in
      (e1_stmts, (A.EInj (new_e1, cons_name, e_name), Pos.get_position expr))
  | L.EApp (f, args) ->
      let f_stmts, new_f = translate_expr ctxt f in
      let args_stmts, new_args =
        List.fold_left
          (fun (args_stmts, new_args) arg ->
            let arg_stmts, new_arg = translate_expr ctxt arg in
            (arg_stmts @ args_stmts, new_arg :: new_args))
          ([], []) args
      in
      let new_args = List.rev new_args in
      (f_stmts @ args_stmts, (A.EApp (new_f, new_args), Pos.get_position expr))
  | L.EArray args ->
      let args_stmts, new_args =
        List.fold_left
          (fun (args_stmts, new_args) arg ->
            let arg_stmts, new_arg = translate_expr ctxt arg in
            (arg_stmts @ args_stmts, new_arg :: new_args))
          ([], []) args
      in
      let new_args = List.rev new_args in
      (args_stmts, (A.EArray new_args, Pos.get_position expr))
  | L.EOp op -> ([], (A.EOp op, Pos.get_position expr))
  | L.ELit l -> ([], (A.ELit l, Pos.get_position expr))
  | _ ->
      let tmp_var =
        A.LocalName.fresh
          ( (*This piece of logic is used to make the code more readable. TODO:
              should be removed when
              https://github.com/CatalaLang/catala/issues/240 is fixed. *)
            (match ctxt.inside_definition_of with
            | None -> ctxt.context_name
            | Some v ->
                let v = Pos.unmark (A.LocalName.get_info v) in
                let tmp_rex = Re.Pcre.regexp "^temp_" in
                if Re.Pcre.pmatch ~rex:tmp_rex v then v else "temp_" ^ v),
            Pos.get_position expr )
      in
      let ctxt =
        {
          ctxt with
          inside_definition_of = Some tmp_var;
          context_name = Pos.unmark (A.LocalName.get_info tmp_var);
        }
      in
      let tmp_stmts = translate_statements ctxt expr in
      ( ( A.SLocalDecl
            ((tmp_var, Pos.get_position expr), (D.TAny, Pos.get_position expr)),
          Pos.get_position expr )
        :: tmp_stmts,
        (A.EVar tmp_var, Pos.get_position expr) )

and translate_statements (ctxt : ctxt) (block_expr : L.expr Pos.marked) :
    A.block =
  match Pos.unmark block_expr with
  | L.EApp
      ((L.EAbs ((binder, _), [ (D.TLit D.TUnit, _) ]), _), [ (L.EAssert e, _) ])
    ->
      (* Assertions are always encapsulated in a unit-typed let binding *)
      let _, body = Bindlib.unmbind binder in
      let e_stmts, new_e = translate_expr ctxt e in
      e_stmts
      @ (A.SAssert (Pos.unmark new_e), Pos.get_position block_expr)
        :: translate_statements ctxt body
  | L.EApp ((L.EAbs ((binder, binder_pos), taus), eabs_pos), args) ->
      (* This defines multiple local variables at the time *)
      let vars, body = Bindlib.unmbind binder in
      let vars_tau =
        List.map2 (fun x tau -> (x, tau)) (Array.to_list vars) taus
      in
      let ctxt =
        {
          ctxt with
          var_dict =
            List.fold_left
              (fun var_dict (x, _) ->
                L.VarMap.add x
                  (A.LocalName.fresh (Bindlib.name_of x, binder_pos))
                  var_dict)
              ctxt.var_dict vars_tau;
        }
      in
      let local_decls =
        List.map
          (fun (x, tau) ->
            ( A.SLocalDecl ((L.VarMap.find x ctxt.var_dict, binder_pos), tau),
              eabs_pos ))
          vars_tau
      in
      let vars_args =
        List.map2
          (fun (x, tau) arg ->
            ((L.VarMap.find x ctxt.var_dict, binder_pos), tau, arg))
          vars_tau args
      in
      let def_blocks =
        List.map
          (fun (x, _tau, arg) ->
            let ctxt =
              {
                ctxt with
                inside_definition_of = Some (Pos.unmark x);
                context_name = Pos.unmark (A.LocalName.get_info (Pos.unmark x));
              }
            in
            let arg_stmts, new_arg = translate_expr ctxt arg in
            arg_stmts @ [ (A.SLocalDef (x, new_arg), binder_pos) ])
          vars_args
      in
      let rest_of_block = translate_statements ctxt body in
      local_decls @ List.flatten def_blocks @ rest_of_block
  | L.EAbs ((binder, binder_pos), taus) ->
      let vars, body = Bindlib.unmbind binder in
      let vars_tau =
        List.map2 (fun x tau -> (x, tau)) (Array.to_list vars) taus
      in
      let closure_name =
        match ctxt.inside_definition_of with
        | None ->
            A.LocalName.fresh (ctxt.context_name, Pos.get_position block_expr)
        | Some x -> x
      in
      let ctxt =
        {
          ctxt with
          var_dict =
            List.fold_left
              (fun var_dict (x, _) ->
                L.VarMap.add x
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
                      ((L.VarMap.find var ctxt.var_dict, binder_pos), tau))
                    vars_tau;
                func_body = new_body;
              } ),
          binder_pos );
      ]
  | L.EMatch (e1, args, e_name) ->
      let e1_stmts, new_e1 = translate_expr ctxt e1 in
      let new_args =
        List.fold_left
          (fun new_args arg ->
            match Pos.unmark arg with
            | L.EAbs ((binder, pos_binder), _) ->
                let vars, body = Bindlib.unmbind binder in
                assert (Array.length vars = 1);
                let var = vars.(0) in
                let scalc_var =
                  A.LocalName.fresh (Bindlib.name_of var, pos_binder)
                in
                let ctxt =
                  {
                    ctxt with
                    var_dict = L.VarMap.add var scalc_var ctxt.var_dict;
                  }
                in
                let new_arg = translate_statements ctxt body in
                (new_arg, scalc_var) :: new_args
            | _ -> assert false
            (* should not happen *))
          [] args
      in
      let new_args = List.rev new_args in
      e1_stmts
      @ [ (A.SSwitch (new_e1, e_name, new_args), Pos.get_position block_expr) ]
  | L.EIfThenElse (cond, e_true, e_false) ->
      let cond_stmts, s_cond = translate_expr ctxt cond in
      let s_e_true = translate_statements ctxt e_true in
      let s_e_false = translate_statements ctxt e_false in
      cond_stmts
      @ [
          ( A.SIfThenElse (s_cond, s_e_true, s_e_false),
            Pos.get_position block_expr );
        ]
  | L.ECatch (e_try, except, e_catch) ->
      let s_e_try = translate_statements ctxt e_try in
      let s_e_catch = translate_statements ctxt e_catch in
      [
        (A.STryExcept (s_e_try, except, s_e_catch), Pos.get_position block_expr);
      ]
  | L.ERaise except -> [ (A.SRaise except, Pos.get_position block_expr) ]
  | _ -> (
      let e_stmts, new_e = translate_expr ctxt block_expr in
      e_stmts
      @
      match e_stmts with
      | (A.SRaise _, _) :: _ ->
          (* if the last statement raises an exception, then we don't need to
             return or to define the current variable since this code will be
             unreachable *)
          []
      | _ ->
          [
            ( (match ctxt.inside_definition_of with
              | None -> A.SReturn (Pos.unmark new_e)
              | Some x -> A.SLocalDef (Pos.same_pos_as x new_e, new_e)),
              Pos.get_position block_expr );
          ])

let translate_scope
    (scope_name : D.ScopeName.t)
    (decl_ctx : D.decl_ctx)
    (func_dict : A.TopLevelName.t L.VarMap.t)
    (scope_expr : L.expr Pos.marked) :
    (A.LocalName.t Pos.marked * D.typ Pos.marked) list * A.block =
  match Pos.unmark scope_expr with
  | L.EAbs ((binder, binder_pos), typs) ->
      let vars, body = Bindlib.unmbind binder in
      let var_dict =
        Array.fold_left
          (fun var_dict var ->
            L.VarMap.add var
              (A.LocalName.fresh (Bindlib.name_of var, binder_pos))
              var_dict)
          L.VarMap.empty vars
      in
      let param_list =
        List.map2
          (fun var typ -> ((L.VarMap.find var var_dict, binder_pos), typ))
          (Array.to_list vars) typs
      in
      let new_body =
        translate_statements
          {
            decl_ctx;
            func_dict;
            var_dict;
            inside_definition_of = None;
            context_name = Pos.unmark (D.ScopeName.get_info scope_name);
          }
          body
      in
      (param_list, new_body)
  | _ -> assert false
(* should not happen *)

let translate_program (p : L.program) : A.program =
  {
    decl_ctx = p.L.decl_ctx;
    scopes =
      (let _, new_scopes =
         List.fold_left
           (fun (func_dict, new_scopes) body ->
             let new_scope_params, new_scope_body =
               translate_scope body.L.scope_body_name p.decl_ctx func_dict
                 body.L.scope_body_expr
             in
             let func_id =
               A.TopLevelName.fresh
                 (Bindlib.name_of body.Lcalc.Ast.scope_body_var, Pos.no_pos)
             in
             let func_dict =
               L.VarMap.add body.Lcalc.Ast.scope_body_var func_id func_dict
             in
             ( func_dict,
               {
                 Ast.scope_body_name = body.Lcalc.Ast.scope_body_name;
                 Ast.scope_body_var = func_id;
                 scope_body_func =
                   {
                     A.func_params = new_scope_params;
                     A.func_body = new_scope_body;
                   };
               }
               :: new_scopes ))
           ( (if !Cli.avoid_exceptions_flag then
              L.VarMap.singleton L.handle_default_opt
                (A.TopLevelName.fresh ("handle_default_opt", Pos.no_pos))
             else
               L.VarMap.singleton L.handle_default
                 (A.TopLevelName.fresh ("handle_default", Pos.no_pos))),
             [] )
           p.L.scopes
       in
       List.rev new_scopes);
  }
