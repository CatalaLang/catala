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

open Catala_utils
open Shared_ast
module A = Ast
module L = Lcalc.Ast
module D = Dcalc.Ast

type 'm ctxt = {
  func_dict : ('m L.expr, A.FuncName.t) Var.Map.t;
  decl_ctx : decl_ctx;
  var_dict : ('m L.expr, A.VarName.t) Var.Map.t;
  inside_definition_of : A.VarName.t option;
  context_name : string;
}

(* Expressions can spill out side effect, hence this function also returns a
   list of statements to be prepended before the expression is evaluated *)
let rec translate_expr (ctxt : 'm ctxt) (expr : 'm L.expr) : A.block * A.expr =
  match Mark.remove expr with
  | EVar v ->
    let local_var =
      try A.EVar (Var.Map.find v ctxt.var_dict)
      with Var.Map.Not_found _ -> (
        try A.EFunc (Var.Map.find v ctxt.func_dict)
        with Var.Map.Not_found _ ->
          Message.raise_spanned_error (Expr.pos expr)
            "Var not found in lambda→scalc: %a@\nknown: @[<hov>%a@]@\n"
            Print.var_debug v
            (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf v ->
                 Print.var_debug ppf v))
            (Var.Map.keys ctxt.var_dict))
    in
    [], (local_var, Expr.pos expr)
  | EStruct { fields; name } ->
    let args_stmts, new_args =
      StructField.Map.fold
        (fun _ arg (args_stmts, new_args) ->
          let arg_stmts, new_arg = translate_expr ctxt arg in
          arg_stmts @ args_stmts, new_arg :: new_args)
        fields ([], [])
    in
    let new_args = List.rev new_args in
    let args_stmts = List.rev args_stmts in
    args_stmts, (A.EStruct (new_args, name), Expr.pos expr)
  | ETuple _ -> failwith "Tuples cannot be compiled to scalc"
  | EStructAccess { e = e1; field; name } ->
    let e1_stmts, new_e1 = translate_expr ctxt e1 in
    e1_stmts, (A.EStructFieldAccess (new_e1, field, name), Expr.pos expr)
  | ETupleAccess _ -> failwith "Non-struct tuples cannot be compiled to scalc"
  | EInj { e = e1; cons; name } ->
    let e1_stmts, new_e1 = translate_expr ctxt e1 in
    e1_stmts, (A.EInj (new_e1, cons, name), Expr.pos expr)
  | EApp { f; args } ->
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
  | EOp { op; _ } -> [], (A.EOp (Operator.translate op), Expr.pos expr)
  | ELit l -> [], (A.ELit l, Expr.pos expr)
  | _ ->
    let tmp_var =
      A.VarName.fresh
        ( (*This piece of logic is used to make the code more readable. TODO:
            should be removed when
            https://github.com/CatalaLang/catala/issues/240 is fixed. *)
          (match ctxt.inside_definition_of with
          | None -> ctxt.context_name
          | Some v ->
            let v = Mark.remove (A.VarName.get_info v) in
            let tmp_rex = Re.Pcre.regexp "^temp_" in
            if Re.Pcre.pmatch ~rex:tmp_rex v then v else "temp_" ^ v),
          Expr.pos expr )
    in
    let ctxt =
      {
        ctxt with
        inside_definition_of = Some tmp_var;
        context_name = Mark.remove (A.VarName.get_info tmp_var);
      }
    in
    let tmp_stmts = translate_statements ctxt expr in
    ( ( A.SLocalDecl ((tmp_var, Expr.pos expr), (TAny, Expr.pos expr)),
        Expr.pos expr )
      :: tmp_stmts,
      (A.EVar tmp_var, Expr.pos expr) )

and translate_statements (ctxt : 'm ctxt) (block_expr : 'm L.expr) : A.block =
  match Mark.remove block_expr with
  | EAssert e ->
    (* Assertions are always encapsulated in a unit-typed let binding *)
    let e_stmts, new_e = translate_expr ctxt e in
    e_stmts @ [A.SAssert (Mark.remove new_e), Expr.pos block_expr]
  | EApp { f = EAbs { binder; tys }, binder_mark; args } ->
    (* This defines multiple local variables at the time *)
    let binder_pos = Expr.mark_pos binder_mark in
    let vars, body = Bindlib.unmbind binder in
    let vars_tau = List.map2 (fun x tau -> x, tau) (Array.to_list vars) tys in
    let ctxt =
      {
        ctxt with
        var_dict =
          List.fold_left
            (fun var_dict (x, _) ->
              Var.Map.add x
                (A.VarName.fresh (Bindlib.name_of x, binder_pos))
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
              inside_definition_of = Some (Mark.remove x);
              context_name = Mark.remove (A.VarName.get_info (Mark.remove x));
            }
          in
          let arg_stmts, new_arg = translate_expr ctxt arg in
          arg_stmts @ [A.SLocalDef (x, new_arg), binder_pos])
        vars_args
    in
    let rest_of_block = translate_statements ctxt body in
    local_decls @ List.flatten def_blocks @ rest_of_block
  | EAbs { binder; tys } ->
    let vars, body = Bindlib.unmbind binder in
    let binder_pos = Expr.pos block_expr in
    let vars_tau = List.map2 (fun x tau -> x, tau) (Array.to_list vars) tys in
    let closure_name =
      match ctxt.inside_definition_of with
      | None -> A.VarName.fresh (ctxt.context_name, Expr.pos block_expr)
      | Some x -> x
    in
    let ctxt =
      {
        ctxt with
        var_dict =
          List.fold_left
            (fun var_dict (x, _) ->
              Var.Map.add x
                (A.VarName.fresh (Bindlib.name_of x, binder_pos))
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
  | EMatch { e = e1; cases; name } ->
    let e1_stmts, new_e1 = translate_expr ctxt e1 in
    let new_cases =
      EnumConstructor.Map.fold
        (fun _ arg new_args ->
          match Mark.remove arg with
          | EAbs { binder; _ } ->
            let vars, body = Bindlib.unmbind binder in
            assert (Array.length vars = 1);
            let var = vars.(0) in
            let scalc_var =
              A.VarName.fresh (Bindlib.name_of var, Expr.pos arg)
            in
            let ctxt =
              { ctxt with var_dict = Var.Map.add var scalc_var ctxt.var_dict }
            in
            let new_arg = translate_statements ctxt body in
            (new_arg, scalc_var) :: new_args
          | _ -> assert false
          (* should not happen *))
        cases []
    in
    let new_args = List.rev new_cases in
    e1_stmts @ [A.SSwitch (new_e1, name, new_args), Expr.pos block_expr]
  | EIfThenElse { cond; etrue; efalse } ->
    let cond_stmts, s_cond = translate_expr ctxt cond in
    let s_e_true = translate_statements ctxt etrue in
    let s_e_false = translate_statements ctxt efalse in
    cond_stmts
    @ [A.SIfThenElse (s_cond, s_e_true, s_e_false), Expr.pos block_expr]
  | ECatch { body; exn; handler } ->
    let s_e_try = translate_statements ctxt body in
    let s_e_catch = translate_statements ctxt handler in
    [A.STryExcept (s_e_try, exn, s_e_catch), Expr.pos block_expr]
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
          | None -> A.SReturn (Mark.remove new_e)
          | Some x -> A.SLocalDef (Mark.copy new_e x, new_e)),
          Expr.pos block_expr );
      ])

let rec translate_scope_body_expr
    (scope_name : ScopeName.t)
    (decl_ctx : decl_ctx)
    (var_dict : ('m L.expr, A.VarName.t) Var.Map.t)
    (func_dict : ('m L.expr, A.FuncName.t) Var.Map.t)
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
          context_name = Mark.remove (ScopeName.get_info scope_name);
        }
        e
    in
    block @ [A.SReturn (Mark.remove new_e), Mark.get new_e]
  | ScopeLet scope_let ->
    let let_var, scope_let_next = Bindlib.unbind scope_let.scope_let_next in
    let let_var_id =
      A.VarName.fresh (Bindlib.name_of let_var, scope_let.scope_let_pos)
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
          context_name = Mark.remove (ScopeName.get_info scope_name);
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
            context_name = Mark.remove (ScopeName.get_info scope_name);
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
  let _, _, rev_items =
    Scope.fold_left
      ~f:(fun (func_dict, var_dict, rev_items) code_item var ->
        match code_item with
        | ScopeDef (name, body) ->
          let scope_input_var, scope_body_expr =
            Bindlib.unbind body.scope_body_expr
          in
          let input_pos = Mark.get (ScopeName.get_info name) in
          let scope_input_var_id =
            A.VarName.fresh (Bindlib.name_of scope_input_var, input_pos)
          in
          let var_dict_local =
            Var.Map.add scope_input_var scope_input_var_id var_dict
          in
          let new_scope_body =
            translate_scope_body_expr name p.decl_ctx var_dict_local func_dict
              scope_body_expr
          in
          let func_id = A.FuncName.fresh (Bindlib.name_of var, Pos.no_pos) in
          ( Var.Map.add var func_id func_dict,
            var_dict,
            A.SScope
              {
                Ast.scope_body_name = name;
                Ast.scope_body_var = func_id;
                scope_body_func =
                  {
                    A.func_params =
                      [
                        ( (scope_input_var_id, input_pos),
                          (TStruct body.scope_body_input_struct, input_pos) );
                      ];
                    A.func_body = new_scope_body;
                  };
              }
            :: rev_items )
        | Topdef (name, _, (EAbs abs, _)) ->
          (* Toplevel function def *)
          let func_id = A.FuncName.fresh (Bindlib.name_of var, Pos.no_pos) in
          let args_a, expr = Bindlib.unmbind abs.binder in
          let args = Array.to_list args_a in
          let args_id =
            List.map2
              (fun v ty ->
                let pos = Mark.get ty in
                (A.VarName.fresh (Bindlib.name_of v, pos), pos), ty)
              args abs.tys
          in
          let block, expr =
            let ctxt =
              {
                func_dict;
                decl_ctx = p.decl_ctx;
                var_dict =
                  List.fold_left2
                    (fun map arg ((id, _), _) -> Var.Map.add arg id map)
                    var_dict args args_id;
                inside_definition_of = None;
                context_name = Mark.remove (TopdefName.get_info name);
              }
            in
            translate_expr ctxt expr
          in
          let body_block =
            block @ [A.SReturn (Mark.remove expr), Mark.get expr]
          in
          ( Var.Map.add var func_id func_dict,
            var_dict,
            A.SFunc
              {
                var = func_id;
                func = { A.func_params = args_id; A.func_body = body_block };
              }
            :: rev_items )
        | Topdef (name, _ty, expr) ->
          (* Toplevel constant def *)
          let var_id = A.VarName.fresh (Bindlib.name_of var, Pos.no_pos) in
          let block, expr =
            let ctxt =
              {
                func_dict;
                decl_ctx = p.decl_ctx;
                var_dict;
                inside_definition_of = None;
                context_name = Mark.remove (TopdefName.get_info name);
              }
            in
            translate_expr ctxt expr
          in
          (* If the evaluation of the toplevel expr requires preliminary
             statements, we lift its computation into an auxiliary function *)
          let rev_items =
            match block with
            | [] -> A.SVar { var = var_id; expr } :: rev_items
            | block ->
              let pos = Mark.get expr in
              let func_id =
                A.FuncName.fresh (Bindlib.name_of var ^ "_aux", pos)
              in
              (* The list is being built in reverse order *)
              A.SVar
                { var = var_id; expr = A.EApp ((EFunc func_id, pos), []), pos }
              :: A.SFunc
                   {
                     var = func_id;
                     func =
                       {
                         A.func_params = [];
                         A.func_body =
                           block @ [A.SReturn (Mark.remove expr), Mark.get expr];
                       };
                   }
              :: rev_items
          in
          ( func_dict,
            (* No need to add func_id since the function will only be called
               right here *)
            Var.Map.add var var_id var_dict,
            rev_items ))
      ~init:(Var.Map.empty, Var.Map.empty, [])
      p.code_items
  in
  { decl_ctx = p.decl_ctx; code_items = List.rev rev_items }
