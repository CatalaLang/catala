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
  program_ctx : A.ctx;
}

(* Expressions can spill out side effect, hence this function also returns a
   list of statements to be prepended before the expression is evaluated *)

exception NotAnExpr of { needs_a_local_decl : bool }
(** Contains the LocalDecl of the temporary variable that will be defined by the
    next block is it's here *)

(** Blocks are constructed as reverse ordered lists. This module abstracts this
    and avoids confusion in ordering of statements (also opening the opportunity
    for more optimisations) *)
module RevBlock : sig
  type t = private A.block

  val empty : t
  val append : t -> A.stmt Mark.pos -> t
  val make : A.block -> t
  val seq : t -> t -> t
  val rebuild : t -> tail:A.block -> A.block
end = struct
  type t = A.block

  let empty = []
  let append t st = st :: t
  let make st = List.rev st
  let seq t1 t2 = t2 @ t1
  let rebuild t ~tail = List.rev_append t tail
end

let ( ++ ) = RevBlock.seq

let rec translate_expr_list ctxt args =
  let stmts, args =
    List.fold_left
      (fun (args_stmts, new_args) arg ->
        let arg_stmts, new_arg = translate_expr ctxt arg in
        args_stmts ++ arg_stmts, new_arg :: new_args)
      (RevBlock.empty, []) args
  in
  stmts, List.rev args

and translate_expr (ctxt : 'm ctxt) (expr : 'm L.expr) : RevBlock.t * A.expr =
  try
    match Mark.remove expr with
    | EVar v ->
      let local_var =
        try A.EVar (Var.Map.find v ctxt.var_dict)
        with Var.Map.Not_found _ -> (
          try A.EFunc (Var.Map.find v ctxt.func_dict)
          with Var.Map.Not_found _ ->
            Message.error ~pos:(Expr.pos expr)
              "Var not found in lambda→scalc: %a@\nknown: @[<hov>%a@]@\n"
              Print.var_debug v
              (Format.pp_print_list ~pp_sep:Format.pp_print_space (fun ppf v ->
                   Print.var_debug ppf v))
              (Var.Map.keys ctxt.var_dict))
      in
      RevBlock.empty, (local_var, Expr.pos expr)
    | EStruct { fields; name } ->
      if ctxt.config.no_struct_literals then
        (* In C89, struct literates have to be initialized at variable
           definition... *)
        raise (NotAnExpr { needs_a_local_decl = false });
      let args_stmts, new_args =
        StructField.Map.fold
          (fun field arg (args_stmts, new_args) ->
            let arg_stmts, new_arg = translate_expr ctxt arg in
            args_stmts ++ arg_stmts, StructField.Map.add field new_arg new_args)
          fields
          (RevBlock.empty, StructField.Map.empty)
      in
      args_stmts, (A.EStruct { fields = new_args; name }, Expr.pos expr)
    | EInj { e = e1; cons; name } ->
      if ctxt.config.no_struct_literals then
        (* In C89, struct literates have to be initialized at variable
           definition... *)
        raise (NotAnExpr { needs_a_local_decl = false });
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
    | ETuple args ->
      let args_stmts, new_args = translate_expr_list ctxt args in
      args_stmts, (A.ETuple new_args, Expr.pos expr)
    | EStructAccess { e = e1; field; name } ->
      let e1_stmts, new_e1 = translate_expr ctxt e1 in
      ( e1_stmts,
        (A.EStructFieldAccess { e1 = new_e1; field; name }, Expr.pos expr) )
    | ETupleAccess { e = e1; index; _ } ->
      let e1_stmts, new_e1 = translate_expr ctxt e1 in
      e1_stmts, (A.ETupleAccess { e1 = new_e1; index }, Expr.pos expr)
    | EAppOp { op = Op.HandleExceptions, pos; tys = [t_arr]; args = [exceptions] }
      when ctxt.config.keep_special_ops ->
      let arr_struct, field_contents, field_size =
        match Mark.remove t_arr with
        | TStruct arr_struct ->
          let contents, sizes =
            StructField.Map.partition (fun _ -> function
                | TArray _, _ -> true
                | _ -> false)
              (StructName.Map.find arr_struct ctxt.program_ctx.decl_ctx.ctx_structs)
          in
          arr_struct,
          fst (StructField.Map.choose contents),
          fst (StructField.Map.choose sizes)
        | _ -> assert false
      in
      let exceptions =
        match Mark.remove exceptions with
        | EStruct { fields; _ } -> (
            match StructField.Map.find field_contents fields with
            | EArray exceptions, _ -> exceptions
            | _ -> assert false)
        | _ -> assert false
      in
      let exceptions_stmts, new_exceptions =
        translate_expr_list ctxt exceptions
      in
      let exn_array =
        A.EStruct {
          name = arr_struct;
          fields =
            StructField.Map.of_list [
              field_size, (A.ELit (LInt (Runtime.integer_of_int (List.length exceptions))), pos);
              field_contents, (A.EArray new_exceptions, pos);
            ];
        }
      in
      let arr_var_name =
        A.VarName.fresh (ctxt.context_name, pos)
      in
      let stmts =
        RevBlock.make
          [A.SLocalDecl
             {
               name = arr_var_name, pos;
               typ = t_arr;
             },
           pos]
        ++ exceptions_stmts
        ++ RevBlock.make
          [A.SLocalDef
             {
               name = arr_var_name, pos;
               typ = t_arr;
               expr =  exn_array, pos;
             },
           pos]
      in
      stmts,
      ( A.EAppOp
          { op = Op.HandleExceptions, pos;
            args = [A.EVar arr_var_name, pos];
            tys = [t_arr];
          },
        pos )
    | EAppOp { op; args; tys } ->
      let args_stmts, new_args = translate_expr_list ctxt args in
      (* FIXME: what happens if [arg] is not a tuple but reduces to one ? *)
      args_stmts, (A.EAppOp { op; args = new_args; tys }, Expr.pos expr)
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
        List.fold_left
          (fun acc (x, tau) ->
            RevBlock.append acc
              ( A.SLocalDecl
                  { name = Var.Map.find x ctxt.var_dict, binder_pos; typ = tau },
                binder_pos ))
          RevBlock.empty vars_tau
      in
      let vars_args =
        List.map2
          (fun (x, tau) arg ->
            (Var.Map.find x ctxt.var_dict, binder_pos), tau, arg)
          vars_tau args
      in
      let def_blocks =
        List.fold_left
          (fun acc (x, _tau, arg) ->
            let ctxt =
              {
                ctxt with
                inside_definition_of = Some (Mark.remove x);
                context_name = Mark.remove (A.VarName.get_info (Mark.remove x));
              }
            in
            let arg_stmts, new_arg = translate_expr ctxt arg in
            RevBlock.append (acc ++ arg_stmts)
              ( A.SLocalDef
                  {
                    name = x;
                    expr = new_arg;
                    typ = Expr.maybe_ty (Mark.get arg);
                  },
                binder_pos ))
          RevBlock.empty vars_args
      in
      let rest_of_expr_stmts, rest_of_expr = translate_expr ctxt body in
      local_decls ++ def_blocks ++ rest_of_expr_stmts, rest_of_expr
    | EApp { f; args; tys = _ } ->
      let f_stmts, new_f = translate_expr ctxt f in
      let args_stmts, new_args = translate_expr_list ctxt args in
      (* FIXME: what happens if [arg] is not a tuple but reduces to one ? *)
      ( f_stmts ++ args_stmts,
        (A.EApp { f = new_f; args = new_args }, Expr.pos expr) )
    | EArray args ->
      let args_stmts, new_args = translate_expr_list ctxt args in
      args_stmts, (A.EArray new_args, Expr.pos expr)
    | ELit l -> RevBlock.empty, (A.ELit l, Expr.pos expr)
    | EExternal { name } ->
      let path, name =
        match Mark.remove name with
        | External_value name -> TopdefName.(path name, get_info name)
        | External_scope name -> ScopeName.(path name, get_info name)
      in
      let modname =
        ( ModuleName.Map.find (List.hd (List.rev path)) ctxt.program_ctx.modules,
          Expr.pos expr )
      in
      RevBlock.empty, (EExternal { modname; name }, Expr.pos expr)
    | EAbs _ | EIfThenElse _ | EMatch _ | EAssert _ | EFatalError _ ->
      raise (NotAnExpr { needs_a_local_decl = true })
    | _ -> .
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
         RevBlock.make
           (( A.SLocalDecl
                {
                  name = tmp_var, Expr.pos expr;
                  typ = Expr.maybe_ty (Mark.get expr);
                },
              Expr.pos expr )
           :: tmp_stmts)
       else RevBlock.make tmp_stmts),
      (A.EVar tmp_var, Expr.pos expr) )

and translate_statements (ctxt : 'm ctxt) (block_expr : 'm L.expr) : A.block =
  match Mark.remove block_expr with
  | EAssert e ->
    (* Assertions are always encapsulated in a unit-typed let binding *)
    let e_stmts, new_e = translate_expr ctxt e in
    RevBlock.rebuild
      ~tail:[A.SAssert (Mark.remove new_e), Expr.pos block_expr]
      e_stmts
  | EFatalError err -> [SFatalError err, Expr.pos block_expr]
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
          RevBlock.rebuild arg_stmts
            ~tail:
              [
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
                  | _ -> assert false);
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
          | _ -> assert false)
        cases []
    in
    let new_args = List.rev new_cases in
    RevBlock.rebuild e1_stmts
      ~tail:
        [
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
    RevBlock.rebuild cond_stmts
      ~tail:
        [
          ( A.SIfThenElse
              {
                if_expr = s_cond;
                then_block = s_e_true;
                else_block = s_e_false;
              },
            Expr.pos block_expr );
        ]
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
    RevBlock.rebuild e1_stmts
      ~tail:
        [
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
          args_stmts ++ arg_stmts, StructField.Map.add field new_arg new_args)
        fields
        (RevBlock.empty, StructField.Map.empty)
    in
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
    RevBlock.rebuild args_stmts
      ~tail:
        [
          ( A.SLocalInit
              {
                name = tmp_struct_var_name;
                expr = struct_expr;
                typ = TStruct name, Expr.pos block_expr;
              },
            Expr.pos block_expr );
        ]
  | ELit _ | EAppOp _ | EArray _ | EVar _ | EStruct _ | EInj _ | ETuple _
  | ETupleAccess _ | EStructAccess _ | EExternal _ | EApp _ ->
    let e_stmts, new_e = translate_expr ctxt block_expr in
    let tail =
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
      ]
    in
    RevBlock.rebuild e_stmts ~tail
  | _ -> .

let rec translate_scope_body_expr
    ~(config : translation_config)
    (scope_name : ScopeName.t)
    (program_ctx : A.ctx)
    (var_dict : ('m L.expr, A.VarName.t) Var.Map.t)
    (func_dict : ('m L.expr, A.FuncName.t) Var.Map.t)
    (scope_expr : 'm L.expr scope_body_expr) : A.block =
  let ctx =
    {
      func_dict;
      var_dict;
      inside_definition_of = None;
      context_name = Mark.remove (ScopeName.get_info scope_name);
      config;
      program_ctx;
    }
  in
  match scope_expr with
  | Last e ->
    let block, new_e = translate_expr ctx e in
    RevBlock.rebuild block ~tail:[A.SReturn (Mark.remove new_e), Mark.get new_e]
  | Cons (scope_let, next_bnd) -> (
    let let_var, scope_let_next = Bindlib.unbind next_bnd in
    let let_var_id =
      A.VarName.fresh (Bindlib.name_of let_var, scope_let.scope_let_pos)
    in
    let new_var_dict = Var.Map.add let_var let_var_id var_dict in
    let next =
      translate_scope_body_expr ~config scope_name program_ctx new_var_dict
        func_dict scope_let_next
    in
    match scope_let.scope_let_kind with
    | Assertion ->
      translate_statements
        { ctx with inside_definition_of = Some let_var_id }
        scope_let.scope_let_expr
      @ next
    | _ ->
      let let_expr_stmts, new_let_expr =
        translate_expr
          { ctx with inside_definition_of = Some let_var_id }
          scope_let.scope_let_expr
      in
      RevBlock.rebuild let_expr_stmts
        ~tail:
          (( A.SLocalDecl
               {
                 name = let_var_id, scope_let.scope_let_pos;
                 typ = scope_let.scope_let_typ;
               },
             scope_let.scope_let_pos )
          :: ( A.SLocalDef
                 {
                   name = let_var_id, scope_let.scope_let_pos;
                   expr = new_let_expr;
                   typ = scope_let.scope_let_typ;
                 },
               scope_let.scope_let_pos )
          :: next))

let translate_program ~(config : translation_config) (p : 'm L.program) :
    A.program =
  let modules =
    List.fold_left
      (fun acc (m, _) ->
        let vname = Mark.map (( ^ ) "Module_") (ModuleName.get_info m) in
        (* The "Module_" prefix is a workaround name clashes for same-name
           structs and modules, Python in particular mixes everything in one
           namespaces. It can be removed once we have full clash-free variable
           renaming in the Python backend (requiring all idents to go through
           one stage of being bindlib vars) *)
        ModuleName.Map.add m (A.VarName.fresh vname) acc)
      ModuleName.Map.empty
      (Program.modules_to_list p.decl_ctx.ctx_modules)
  in
  let ctx = { A.decl_ctx = p.decl_ctx; A.modules } in
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
            translate_scope_body_expr ~config name ctx var_dict_local func_dict
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
                program_ctx = ctx;
              }
            in
            translate_expr ctxt expr
          in
          let body_block =
            RevBlock.rebuild block
              ~tail:[A.SReturn (Mark.remove expr), Mark.get expr]
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
                program_ctx = ctx;
              }
            in
            translate_expr ctxt expr
          in
          (* If the evaluation of the toplevel expr requires preliminary
             statements, we lift its computation into an auxiliary function *)
          let rev_items =
            if (block :> (A.stmt * Pos.t) list) = [] then
              A.SVar { var = var_id; expr; typ = topdef_ty } :: rev_items
            else
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
                           RevBlock.rebuild block
                             ~tail:[A.SReturn (Mark.remove expr), Mark.get expr];
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
  { ctx; code_items = List.rev rev_items; module_name = p.module_name }
