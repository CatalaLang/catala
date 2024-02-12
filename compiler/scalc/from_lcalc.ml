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

type translation_config = {
  keep_special_ops : bool;
  dead_value_assignment : bool;
  no_struct_literals : bool;
}

type 'm ctxt = {
  func_dict : ('m L.expr, A.FuncName.t) Var.Map.t;
  var_dict : ('m L.expr, A.VarName.t) Var.Map.t;
  inside_definition_of : A.VarName.t option;
  context_name : string;
  config : translation_config;
}

let unthunk e =
  match Mark.remove e with
  | EAbs { binder; tys = [(TLit TUnit, _)] } ->
    let _, e = Bindlib.unmbind binder in
    e
  | _ -> failwith "should not happen"

(* Expressions can spill out side effect, hence this function also returns a
   list of statements to be prepended before the expression is evaluated *)

exception NotAnExpr of { needs_a_local_decl : bool }
(** Contains the LocalDecl of the temporary variable that will be defined by the
    next block is it's here *)

let rec translate_expr (ctxt : 'm ctxt) (expr : 'm L.expr) : A.block * A.expr =
  try
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
    | EStruct { fields; name } when not ctxt.config.no_struct_literals ->
      let args_stmts, new_args =
        StructField.Map.fold
          (fun field arg (args_stmts, new_args) ->
            let arg_stmts, new_arg = translate_expr ctxt arg in
            arg_stmts @ args_stmts, StructField.Map.add field new_arg new_args)
          fields
          ([], StructField.Map.empty)
      in
      let args_stmts = List.rev args_stmts in
      args_stmts, (A.EStruct { fields = new_args; name }, Expr.pos expr)
    | EStruct _ when ctxt.config.no_struct_literals ->
      (* In C89, struct literates have to be initialized at variable
         definition... *)
      raise (NotAnExpr { needs_a_local_decl = false })
    | EInj { e = e1; cons; name } when not ctxt.config.no_struct_literals ->
      let e1_stmts, new_e1 = translate_expr ctxt e1 in
      ( e1_stmts,
        ( A.EInj
            {
              e1 = new_e1;
              cons;
              name;
              expr_typ = Expr.maybe_ty (Mark.get expr);
            },
          Expr.pos expr ) )
    | EInj _ when ctxt.config.no_struct_literals ->
      (* In C89, struct literates have to be initialized at variable
         definition... *)
      raise (NotAnExpr { needs_a_local_decl = false })
    | ETuple args ->
      let args_stmts, new_args =
        List.fold_left
          (fun (args_stmts, new_args) arg ->
            let arg_stmts, new_arg = translate_expr ctxt arg in
            arg_stmts @ args_stmts, new_arg :: new_args)
          ([], []) args
      in
      let new_args = List.rev new_args in
      args_stmts, (A.ETuple new_args, Expr.pos expr)
    | EStructAccess { e = e1; field; name } ->
      let e1_stmts, new_e1 = translate_expr ctxt e1 in
      ( e1_stmts,
        (A.EStructFieldAccess { e1 = new_e1; field; name }, Expr.pos expr) )
    | ETupleAccess { e = e1; index; _ } ->
      let e1_stmts, new_e1 = translate_expr ctxt e1 in
      e1_stmts, (A.ETupleAccess { e1 = new_e1; index }, Expr.pos expr)
    | EAppOp
        {
          op = Op.HandleDefaultOpt;
          args = [_exceptions; _just; _cons];
          tys = _;
        }
      when ctxt.config.keep_special_ops ->
      (* This should be translated as a statement *)
      raise (NotAnExpr { needs_a_local_decl = true })
    | EAppOp { op; args; tys = _ } ->
      let args_stmts, new_args =
        List.fold_left
          (fun (args_stmts, new_args) arg ->
            let arg_stmts, new_arg = translate_expr ctxt arg in
            arg_stmts @ args_stmts, new_arg :: new_args)
          ([], []) args
      in
      (* FIXME: what happens if [arg] is not a tuple but reduces to one ? *)
      let new_args = List.rev new_args in
      args_stmts, (A.EAppOp { op; args = new_args }, Expr.pos expr)
    | EApp { f = EAbs { binder; tys }, binder_mark; args; tys = _ } ->
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
            ( A.SLocalDecl
                { name = Var.Map.find x ctxt.var_dict, binder_pos; typ = tau },
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
            arg_stmts
            @ [
                ( A.SLocalDef
                    {
                      name = x;
                      expr = new_arg;
                      typ = Expr.maybe_ty (Mark.get arg);
                    },
                  binder_pos );
              ])
          vars_args
      in
      let rest_of_expr_stmts, rest_of_expr = translate_expr ctxt body in
      local_decls @ List.flatten def_blocks @ rest_of_expr_stmts, rest_of_expr
    | EApp { f; args; tys = _ } ->
      let f_stmts, new_f = translate_expr ctxt f in
      let args_stmts, new_args =
        List.fold_left
          (fun (args_stmts, new_args) arg ->
            let arg_stmts, new_arg = translate_expr ctxt arg in
            arg_stmts @ args_stmts, new_arg :: new_args)
          ([], []) args
      in
      (* FIXME: what happens if [arg] is not a tuple but reduces to one ? *)
      let new_args = List.rev new_args in
      ( f_stmts @ args_stmts,
        (A.EApp { f = new_f; args = new_args }, Expr.pos expr) )
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
    | ELit l -> [], (A.ELit l, Expr.pos expr)
    | _ -> raise (NotAnExpr { needs_a_local_decl = true })
  with NotAnExpr { needs_a_local_decl } ->
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
    ( (if needs_a_local_decl then
         [
           ( A.SLocalDecl
               {
                 name = tmp_var, Expr.pos expr;
                 typ = Expr.maybe_ty (Mark.get expr);
               },
             Expr.pos expr );
         ]
       else [])
      @ tmp_stmts,
      (A.EVar tmp_var, Expr.pos expr) )

and translate_statements (ctxt : 'm ctxt) (block_expr : 'm L.expr) : A.block =
  match Mark.remove block_expr with
  | EAssert e ->
    (* Assertions are always encapsulated in a unit-typed let binding *)
    let e_stmts, new_e = translate_expr ctxt e in
    e_stmts @ [A.SAssert (Mark.remove new_e), Expr.pos block_expr]
  | EAppOp
      { op = Op.HandleDefaultOpt; tys = _; args = [exceptions; just; cons] }
    when ctxt.config.keep_special_ops ->
    let exceptions =
      match Mark.remove exceptions with
      | EStruct { fields; _ } -> (
        let _, exceptions =
          List.find
            (fun (field, _) ->
              String.equal (Mark.remove (StructField.get_info field)) "content")
            (StructField.Map.bindings fields)
        in
        match Mark.remove exceptions with
        | EArray exceptions -> exceptions
        | _ -> failwith "should not happen")
      | _ -> failwith "should not happen"
    in
    let just = unthunk just in
    let cons = unthunk cons in
    let exceptions_stmts, new_exceptions =
      List.fold_left
        (fun (exceptions_stmts, new_exceptions) except ->
          let except_stmts, new_except = translate_expr ctxt except in
          except_stmts @ exceptions_stmts, new_except :: new_exceptions)
        ([], []) exceptions
    in
    let just_stmts, new_just = translate_expr ctxt just in
    let cons_stmts, new_cons = translate_expr ctxt cons in
    exceptions_stmts
    @ just_stmts
    @ [
        ( A.SSpecialOp
            (OHandleDefaultOpt
               {
                 exceptions = new_exceptions;
                 just = new_just;
                 cons =
                   cons_stmts
                   @ [
                       ( (match ctxt.inside_definition_of with
                         | None -> A.SReturn (Mark.remove new_cons)
                         | Some x ->
                           A.SLocalDef
                             {
                               name = Mark.copy new_cons x;
                               expr = new_cons;
                               typ = Expr.maybe_ty (Mark.get block_expr);
                             }),
                         Expr.pos block_expr );
                     ];
                 return_typ = Expr.maybe_ty (Mark.get block_expr);
               }),
          Expr.pos block_expr );
      ]
  | EApp { f = EAbs { binder; tys }, binder_mark; args; _ } ->
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
          ( A.SLocalDecl
              { name = Var.Map.find x ctxt.var_dict, binder_pos; typ = tau },
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
          arg_stmts
          @ [
              ( A.SLocalDef
                  {
                    name = x;
                    expr = new_arg;
                    typ = Expr.maybe_ty (Mark.get arg);
                  },
                binder_pos );
            ])
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
          {
            name = closure_name, binder_pos;
            func =
              {
                func_params =
                  List.map
                    (fun (var, tau) ->
                      (Var.Map.find var ctxt.var_dict, binder_pos), tau)
                    vars_tau;
                func_body = new_body;
                func_return_typ =
                  (match Expr.maybe_ty (Mark.get block_expr) with
                  | TArrow (_, t2), _ -> t2
                  | TAny, pos_any -> TAny, pos_any
                  | _ -> failwith "should not happen");
              };
          },
        binder_pos );
    ]
  | EMatch { e = e1; cases; name } ->
    let e1_stmts, new_e1 = translate_expr ctxt e1 in
    let new_cases =
      EnumConstructor.Map.fold
        (fun _ arg new_args ->
          match Mark.remove arg with
          | EAbs { binder; tys } ->
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
            {
              A.case_block = new_arg;
              payload_var_name = scalc_var;
              payload_var_typ = List.hd tys;
            }
            :: new_args
          | _ -> assert false
          (* should not happen *))
        cases []
    in
    let new_args = List.rev new_cases in
    e1_stmts
    @ [
        ( A.SSwitch
            {
              switch_expr = new_e1;
              switch_expr_typ = Expr.maybe_ty (Mark.get e1);
              enum_name = name;
              switch_cases = new_args;
            },
          Expr.pos block_expr );
      ]
  | EIfThenElse { cond; etrue; efalse } ->
    let cond_stmts, s_cond = translate_expr ctxt cond in
    let s_e_true = translate_statements ctxt etrue in
    let s_e_false = translate_statements ctxt efalse in
    cond_stmts
    @ [
        ( A.SIfThenElse
            { if_expr = s_cond; then_block = s_e_true; else_block = s_e_false },
          Expr.pos block_expr );
      ]
  | ECatch { body; exn; handler } ->
    let s_e_try = translate_statements ctxt body in
    let s_e_catch = translate_statements ctxt handler in
    [
      ( A.STryExcept
          { try_block = s_e_try; except = exn; with_block = s_e_catch },
        Expr.pos block_expr );
    ]
  | ERaise except ->
    (* Before raising the exception, we still give a dummy definition to the
       current variable so that tools like mypy don't complain. *)
    (match ctxt.inside_definition_of with
    | Some x when ctxt.config.dead_value_assignment ->
      [
        ( A.SLocalDef
            {
              name = x, Expr.pos block_expr;
              expr = Ast.EVar Ast.dead_value, Expr.pos block_expr;
              typ = Expr.maybe_ty (Mark.get block_expr);
            },
          Expr.pos block_expr );
      ]
    | _ -> [])
    @ [A.SRaise except, Expr.pos block_expr]
  | EInj { e = e1; cons; name } when ctxt.config.no_struct_literals ->
    let e1_stmts, new_e1 = translate_expr ctxt e1 in
    let tmp_struct_var_name =
      match ctxt.inside_definition_of with
      | None ->
        failwith "should not happen"
        (* [translate_expr] should create this [inside_definition_of]*)
      | Some x -> x, Expr.pos block_expr
    in
    let inj_expr =
      ( A.EInj
          {
            e1 = new_e1;
            cons;
            name;
            expr_typ = Expr.maybe_ty (Mark.get block_expr);
          },
        Expr.pos block_expr )
    in
    e1_stmts
    @ [
        ( A.SLocalInit
            {
              name = tmp_struct_var_name;
              expr = inj_expr;
              typ =
                ( Mark.remove (Expr.maybe_ty (Mark.get block_expr)),
                  Expr.pos block_expr );
            },
          Expr.pos block_expr );
      ]
  | EStruct { fields; name } when ctxt.config.no_struct_literals ->
    let args_stmts, new_args =
      StructField.Map.fold
        (fun field arg (args_stmts, new_args) ->
          let arg_stmts, new_arg = translate_expr ctxt arg in
          arg_stmts @ args_stmts, StructField.Map.add field new_arg new_args)
        fields
        ([], StructField.Map.empty)
    in
    let args_stmts = List.rev args_stmts in
    let struct_expr =
      A.EStruct { fields = new_args; name }, Expr.pos block_expr
    in
    let tmp_struct_var_name =
      match ctxt.inside_definition_of with
      | None ->
        failwith "should not happen"
        (* [translate_expr] should create this [inside_definition_of]*)
      | Some x -> x, Expr.pos block_expr
    in
    args_stmts
    @ [
        ( A.SLocalInit
            {
              name = tmp_struct_var_name;
              expr = struct_expr;
              typ = TStruct name, Expr.pos block_expr;
            },
          Expr.pos block_expr );
      ]
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
          | Some x ->
            A.SLocalDef
              {
                name = Mark.copy new_e x;
                expr = new_e;
                typ = Expr.maybe_ty (Mark.get block_expr);
              }),
          Expr.pos block_expr );
      ])

let rec translate_scope_body_expr
    ~(config : translation_config)
    (scope_name : ScopeName.t)
    (decl_ctx : decl_ctx)
    (var_dict : ('m L.expr, A.VarName.t) Var.Map.t)
    (func_dict : ('m L.expr, A.FuncName.t) Var.Map.t)
    (scope_expr : 'm L.expr scope_body_expr) : A.block =
  match scope_expr with
  | Last e ->
    let block, new_e =
      translate_expr
        {
          func_dict;
          var_dict;
          inside_definition_of = None;
          context_name = Mark.remove (ScopeName.get_info scope_name);
          config;
        }
        e
    in
    block @ [A.SReturn (Mark.remove new_e), Mark.get new_e]
  | Cons (scope_let, next_bnd) ->
    let let_var, scope_let_next = Bindlib.unbind next_bnd in
    let let_var_id =
      A.VarName.fresh (Bindlib.name_of let_var, scope_let.scope_let_pos)
    in
    let new_var_dict = Var.Map.add let_var let_var_id var_dict in
    (match scope_let.scope_let_kind with
    | Assertion ->
      translate_statements
        {
          func_dict;
          var_dict;
          inside_definition_of = Some let_var_id;
          context_name = Mark.remove (ScopeName.get_info scope_name);
          config;
        }
        scope_let.scope_let_expr
    | _ ->
      let let_expr_stmts, new_let_expr =
        translate_expr
          {
            func_dict;
            var_dict;
            inside_definition_of = Some let_var_id;
            context_name = Mark.remove (ScopeName.get_info scope_name);
            config;
          }
          scope_let.scope_let_expr
      in
      let_expr_stmts
      @ [
          ( A.SLocalDecl
              {
                name = let_var_id, scope_let.scope_let_pos;
                typ = scope_let.scope_let_typ;
              },
            scope_let.scope_let_pos );
          ( A.SLocalDef
              {
                name = let_var_id, scope_let.scope_let_pos;
                expr = new_let_expr;
                typ = scope_let.scope_let_typ;
              },
            scope_let.scope_let_pos );
        ])
    @ translate_scope_body_expr ~config scope_name decl_ctx new_var_dict
        func_dict scope_let_next

let translate_program ~(config : translation_config) (p : 'm L.program) :
    A.program =
  let (_, _, rev_items), () =
    BoundList.fold_left
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
            translate_scope_body_expr ~config name p.decl_ctx var_dict_local
              func_dict scope_body_expr
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
                    func_return_typ =
                      TStruct body.scope_body_output_struct, input_pos;
                  };
              }
            :: rev_items )
        | Topdef (name, topdef_ty, (EAbs abs, _)) ->
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
                var_dict =
                  List.fold_left2
                    (fun map arg ((id, _), _) -> Var.Map.add arg id map)
                    var_dict args args_id;
                inside_definition_of = None;
                context_name = Mark.remove (TopdefName.get_info name);
                config;
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
                func =
                  {
                    A.func_params = args_id;
                    A.func_body = body_block;
                    A.func_return_typ =
                      (match topdef_ty with
                      | TArrow (_, t2), _ -> t2
                      | TAny, pos_any -> TAny, pos_any
                      | _ -> failwith "should not happen");
                  };
              }
            :: rev_items )
        | Topdef (name, topdef_ty, expr) ->
          (* Toplevel constant def *)
          let var_id = A.VarName.fresh (Bindlib.name_of var, Pos.no_pos) in
          let block, expr =
            let ctxt =
              {
                func_dict;
                var_dict;
                inside_definition_of = None;
                context_name = Mark.remove (TopdefName.get_info name);
                config;
              }
            in
            translate_expr ctxt expr
          in
          (* If the evaluation of the toplevel expr requires preliminary
             statements, we lift its computation into an auxiliary function *)
          let rev_items =
            match block with
            | [] -> A.SVar { var = var_id; expr; typ = topdef_ty } :: rev_items
            | block ->
              let pos = Mark.get expr in
              let func_id =
                A.FuncName.fresh (Bindlib.name_of var ^ "_aux", pos)
              in
              (* The list is being built in reverse order *)
              (* FIXME: find a better way than a function with no parameters... *)
              A.SVar
                {
                  var = var_id;
                  expr = A.EApp { f = EFunc func_id, pos; args = [] }, pos;
                  typ = topdef_ty;
                }
              :: A.SFunc
                   {
                     var = func_id;
                     func =
                       {
                         A.func_params = [];
                         A.func_body =
                           block @ [A.SReturn (Mark.remove expr), Mark.get expr];
                         A.func_return_typ = topdef_ty;
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
