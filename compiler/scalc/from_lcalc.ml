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
  renaming_context : Renaming.context;
}

type 'm ctxt = {
  func_dict : ('m L.expr, A.FuncName.t) Var.Map.t;
  var_dict : ('m L.expr, A.VarName.t) Var.Map.t;
  context_name : string;
  config : translation_config;
  program_ctx : A.ctx;
  ren_ctx : Renaming.context;
}

(** Blocks are constructed as reverse ordered lists. This module abstracts this
    and avoids confusion in ordering of statements (also opening the opportunity
    for more optimisations) *)
module RevBlock : sig
  type t = private A.block

  val empty : t
  val append : t -> A.stmt Mark.pos -> t
  val make : A.block -> t
  val seq : t -> t -> t
  val rebuild : ?tail:A.block -> t -> A.block
end = struct
  type t = A.block

  let empty = []
  let append t st = st :: t
  let make st = List.rev st
  let seq t1 t2 = t2 @ t1
  let rebuild ?(tail = []) t = List.rev_append t tail
end

let ( ++ ) = RevBlock.seq
let ( +> ) = RevBlock.append

let unbind ctxt bnd =
  let v, body, ren_ctx = Renaming.unbind_in ctxt.ren_ctx bnd in
  v, body, { ctxt with ren_ctx }

let unmbind ctxt bnd =
  let vs, body, ren_ctx = Renaming.unmbind_in ctxt.ren_ctx bnd in
  vs, body, { ctxt with ren_ctx }

let get_name ctxt s =
  let name, ren_ctx = Renaming.new_var_id ctxt.ren_ctx s in
  name, { ctxt with ren_ctx }

let fresh_var ~pos ctxt name =
  let v, ctxt = get_name ctxt name in
  A.VarName.fresh (v, pos), ctxt

let register_fresh_var ~pos ctxt x =
  let v = A.VarName.fresh (Bindlib.name_of x, pos) in
  let var_dict = Var.Map.add x v ctxt.var_dict in
  v, { ctxt with var_dict }

let register_fresh_func ~pos ctxt x =
  let f = A.FuncName.fresh (Bindlib.name_of x, pos) in
  let func_dict = Var.Map.add x f ctxt.func_dict in
  f, { ctxt with func_dict }

let register_fresh_arg ~pos ctxt (x, _) =
  let _, ctxt = register_fresh_var ~pos ctxt x in
  ctxt

(* These operators, since they can raise, have an added first argument giving
   the position of the error if it happens, so they need special treatment *)
let op_can_raise op =
  let open Op in
  match Mark.remove op with
  | HandleExceptions | Div_int_int | Div_rat_rat | Div_mon_mon | Div_mon_rat
  | Div_dur_dur | Add_dat_dur _ | Gte_dur_dur | Gt_dur_dur | Lte_dur_dur
  | Lt_dur_dur | Map2 ->
    true
  | _ -> false

let lift_pos ctxt pos =
  let v, ctxt = fresh_var ~pos ctxt "pos" in
  ( (A.EVar v, pos),
    ( A.SLocalInit
        {
          name = v, pos;
          typ = TStruct Expr.source_pos_struct, pos;
          expr = A.EPosLit, pos;
        },
      pos ),
    ctxt )

let rec translate_expr_list ctxt args =
  let stmts, args, ren_ctx =
    List.fold_left
      (fun (args_stmts, new_args, ren_ctx) arg ->
        let arg_stmts, new_arg, ren_ctx =
          translate_expr { ctxt with ren_ctx } arg
        in
        args_stmts ++ arg_stmts, new_arg :: new_args, ren_ctx)
      (RevBlock.empty, [], ctxt.ren_ctx)
      args
  in
  stmts, List.rev args, ren_ctx

and translate_struct_literal ctxt expr =
  match Mark.remove expr with
  | EStruct { fields; name } ->
    let args_stmts, new_args, ren_ctx =
      StructField.Map.fold
        (fun field arg (args_stmts, new_args, ren_ctx) ->
          let arg_stmts, new_arg, ren_ctx =
            translate_expr { ctxt with ren_ctx } arg
          in
          ( args_stmts ++ arg_stmts,
            StructField.Map.add field new_arg new_args,
            ren_ctx ))
        fields
        (RevBlock.empty, StructField.Map.empty, ctxt.ren_ctx)
    in
    args_stmts, (A.EStruct { fields = new_args; name }, Expr.pos expr), ren_ctx
  | EInj { e = e1; cons; name } ->
    let e1_stmts, new_e1, ren_ctx = translate_expr ctxt e1 in
    ( e1_stmts,
      ( A.EInj
          { e1 = new_e1; cons; name; expr_typ = Expr.maybe_ty (Mark.get expr) },
        Expr.pos expr ),
      ren_ctx )
  | ETuple args ->
    let args_stmts, new_args, ren_ctx = translate_expr_list ctxt args in
    args_stmts, (A.ETuple new_args, Expr.pos expr), ren_ctx
  | EArray args ->
    let args_stmts, new_args, ren_ctx = translate_expr_list ctxt args in
    args_stmts, (A.EArray new_args, Expr.pos expr), ren_ctx
  | _ -> invalid_arg "translate_struct_literal"

and translate_expr (ctxt : 'm ctxt) (expr : 'm L.expr) :
    RevBlock.t * A.expr * Renaming.context =
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
    RevBlock.empty, (local_var, Expr.pos expr), ctxt.ren_ctx
  | EStruct _ | EInj _ | ETuple _ | EArray _ ->
    (* In C89, struct literals have to be initialized at variable
       definition... *)
    if ctxt.config.no_struct_literals then spill_expr ctxt expr
    else translate_struct_literal ctxt expr
  | EStructAccess { e = e1; field; name } ->
    let e1_stmts, new_e1, ren_ctx = translate_expr ctxt e1 in
    ( e1_stmts,
      (A.EStructFieldAccess { e1 = new_e1; field; name }, Expr.pos expr),
      ren_ctx )
  | ETupleAccess { e = e1; index; _ } ->
    let e1_stmts, new_e1, ren_ctx = translate_expr ctxt e1 in
    let typ = Expr.maybe_ty (Mark.get expr) in
    ( e1_stmts,
      (A.ETupleAccess { e1 = new_e1; index; typ }, Expr.pos expr),
      ren_ctx )
  | EAppOp
      {
        op = Op.HandleExceptions, pos;
        tys = [t_arr];
        args = [(EArray exceptions, _)];
      } ->
    let exceptions_stmts, new_exceptions, ren_ctx =
      translate_expr_list ctxt exceptions
    in
    let ctxt = { ctxt with ren_ctx } in
    let eposl, vposdefs, ctxt =
      List.fold_left
        (fun (eposl, vposdefs, ctxt) exc ->
          let epos, vposdef, ctxt = lift_pos ctxt (Expr.pos exc) in
          epos :: eposl, RevBlock.append vposdefs vposdef, ctxt)
        ([], RevBlock.empty, ctxt) exceptions
    in
    let stmts = exceptions_stmts ++ vposdefs in
    let stmts, epos, excs, ctxt =
      if not ctxt.config.no_struct_literals then
        ( stmts,
          (A.EArray (List.rev eposl), pos),
          (A.EArray new_exceptions, pos),
          ctxt )
      else
        let arr_var_name, ctxt =
          fresh_var ~pos ctxt ("exc_" ^ ctxt.context_name)
        in
        let pos_arr_var_name, ctxt = fresh_var ~pos ctxt "pos_list" in
        let stmts =
          stmts
          ++ RevBlock.make
               [
                 ( A.SLocalInit
                     {
                       name = arr_var_name, pos;
                       typ = t_arr;
                       expr = A.EArray new_exceptions, pos;
                     },
                   pos );
                 ( A.SLocalInit
                     {
                       name = pos_arr_var_name, pos;
                       typ = TArray (TLit TUnit, pos), pos;
                       expr = A.EArray (List.rev eposl), pos;
                     },
                   pos );
               ]
        in
        stmts, (A.EVar pos_arr_var_name, pos), (A.EVar arr_var_name, pos), ctxt
    in
    ( stmts,
      ( A.EAppOp
          {
            op = Op.HandleExceptions, pos;
            args = [epos; excs];
            tys = [TLit TUnit, pos; t_arr];
          },
        pos ),
      ctxt.ren_ctx )
  | EAppOp { op; args; tys } ->
    let pos = Mark.get op in
    let stmts, args, ren_ctx = translate_expr_list ctxt args in
    let ctxt = { ctxt with ren_ctx } in
    let stmts, args, tys, ctxt =
      if op_can_raise op then
        let epos, vposdef, ctxt = lift_pos ctxt pos in
        ( RevBlock.append stmts vposdef,
          epos :: args,
          (TLit TUnit, pos) :: tys,
          ctxt )
      else stmts, args, tys, ctxt
    in
    (* FIXME: what happens if [arg] is not a tuple but reduces to one ? *)
    stmts, (A.EAppOp { op; args; tys }, Expr.pos expr), ctxt.ren_ctx
  | EApp { f = EAbs { binder; tys }, binder_mark; args; tys = _ } ->
    (* This defines multiple local variables at the time *)
    let binder_pos = Expr.mark_pos binder_mark in
    let vars, body, ctxt = unmbind ctxt binder in
    let vars_tau = List.map2 (fun x tau -> x, tau) (Array.to_list vars) tys in
    let ctxt =
      List.fold_left (register_fresh_arg ~pos:binder_pos) ctxt vars_tau
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
    let def_blocks, ren_ctx =
      List.fold_left
        (fun (rblock, ren_ctx) (x, _tau, arg) ->
          let ctxt =
            {
              ctxt with
              context_name = Mark.remove (A.VarName.get_info (Mark.remove x));
              ren_ctx;
            }
          in
          let arg_stmts, new_arg, ren_ctx = translate_expr ctxt arg in
          ( RevBlock.append (rblock ++ arg_stmts)
              ( A.SLocalDef
                  {
                    name = x;
                    expr = new_arg;
                    typ = Expr.maybe_ty (Mark.get arg);
                  },
                binder_pos ),
            ren_ctx ))
        (RevBlock.empty, ctxt.ren_ctx)
        vars_args
    in
    let rest_of_expr_stmts, rest_of_expr, ren_ctx =
      translate_expr { ctxt with ren_ctx } body
    in
    local_decls ++ def_blocks ++ rest_of_expr_stmts, rest_of_expr, ren_ctx
  | EApp { f; args; tys = _ } ->
    let f_stmts, new_f, ren_ctx = translate_expr ctxt f in
    let args_stmts, new_args, ren_ctx =
      translate_expr_list { ctxt with ren_ctx } args
    in
    (* FIXME: what happens if [arg] is not a tuple but reduces to one ? *)
    ( f_stmts ++ args_stmts,
      (A.EApp { f = new_f; args = new_args }, Expr.pos expr),
      ren_ctx )
  | ELit l -> RevBlock.empty, (A.ELit l, Expr.pos expr), ctxt.ren_ctx
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
    RevBlock.empty, (EExternal { modname; name }, Expr.pos expr), ctxt.ren_ctx
  | EAbs _ | EIfThenElse _ | EMatch _ | EAssert _ | EFatalError _ ->
    spill_expr ctxt expr
  | _ -> .

(** Used when an lcalc expression needs to be translated, but doesn't fit in a
    single statement: creates a temporary variable that gets assigned to by a
    block of statements, and an expression containing the variable holding the
    result *)
and spill_expr ctxt expr =
  let tmp_var, ctxt = fresh_var ctxt ctxt.context_name ~pos:(Expr.pos expr) in
  let ctxt =
    { ctxt with context_name = Mark.remove (A.VarName.get_info tmp_var) }
  in
  let tmp_stmts, ren_ctx =
    translate_assignment ctxt (Some (tmp_var, Expr.pos expr)) expr
  in
  let stmts =
    RevBlock.make
      [
        ( A.SLocalDecl
            {
              name = tmp_var, Expr.pos expr;
              typ = Expr.maybe_ty (Mark.get expr);
            },
          Expr.pos expr );
      ]
    ++ tmp_stmts
  in
  stmts, (A.EVar tmp_var, Expr.pos expr), ren_ctx

(** This translates an expression [block_expr] to a series of statements that
    compute its value, and either assign to the given variable, or return it. *)
and translate_assignment
    (ctxt : 'm ctxt)
    (assign_to : A.VarName.t Mark.pos option)
    (block_expr : 'm L.expr) : RevBlock.t * Renaming.context =
  let pos = Expr.pos block_expr in
  let ctxt =
    match assign_to with
    | Some (v, _) ->
      { ctxt with context_name = Mark.remove (A.VarName.get_info v) }
    | None -> ctxt
  in
  match Mark.remove block_expr with
  | EAssert e ->
    let e_stmts, expr, ren_ctx = translate_expr ctxt e in
    let pos_expr, vposdef, ctxt = lift_pos { ctxt with ren_ctx } pos in
    e_stmts +> vposdef +> (A.SAssert { pos_expr; expr }, pos), ctxt.ren_ctx
  | EFatalError error ->
    let pos_expr, vposdef, ctxt = lift_pos ctxt pos in
    RevBlock.make [vposdef; SFatalError { pos_expr; error }, pos], ctxt.ren_ctx
  | EApp { f = EAbs { binder; tys }, binder_mark; args; _ } ->
    (* This defines multiple local variables at the time *)
    let binder_pos = Expr.mark_pos binder_mark in
    let vars, body, ctxt = unmbind ctxt binder in
    let vars_tau = List.map2 (fun x tau -> x, tau) (Array.to_list vars) tys in
    let ctxt =
      List.fold_left (register_fresh_arg ~pos:binder_pos) ctxt vars_tau
    in
    let local_decls =
      List.map
        (fun (x, tau) ->
          ( A.SLocalDecl
              { name = Var.Map.find x ctxt.var_dict, binder_pos; typ = tau },
            binder_pos ))
        vars_tau
      |> RevBlock.make
    in
    let vars_args =
      List.map2
        (fun (x, tau) arg ->
          (Var.Map.find x ctxt.var_dict, binder_pos), tau, arg)
        vars_tau args
    in
    let def_blocks, ren_ctx =
      List.fold_left
        (fun (def_blocks, ren_ctx) (x, _tau, arg) ->
          let ctxt = { ctxt with ren_ctx } in
          let stmts, ren_ctx = translate_assignment ctxt (Some x) arg in
          def_blocks ++ stmts, ren_ctx)
        (RevBlock.empty, ctxt.ren_ctx)
        vars_args
    in
    let rest_of_block, ren_ctx =
      translate_assignment { ctxt with ren_ctx } assign_to body
    in
    local_decls ++ def_blocks ++ rest_of_block, ren_ctx
  | EAbs { binder; tys } ->
    let vars, body, ctxt = unmbind ctxt binder in
    let binder_pos = Expr.pos block_expr in
    let vars_tau = List.combine (Array.to_list vars) tys in
    let ctxt =
      List.fold_left (register_fresh_arg ~pos:binder_pos) ctxt vars_tau
    in
    let stmts_body, ren_ctx = translate_assignment ctxt None body in
    let name =
      match assign_to with
      | Some v -> v
      | None -> assert false (* it's not allowed to [return] a function *)
    in
    ( RevBlock.make
        [
          ( A.SInnerFuncDef
              {
                name;
                func =
                  {
                    func_params =
                      List.map
                        (fun (var, tau) ->
                          (Var.Map.find var ctxt.var_dict, binder_pos), tau)
                        vars_tau;
                    func_body = RevBlock.rebuild stmts_body ~tail:[];
                    func_return_typ =
                      (match Expr.maybe_ty (Mark.get block_expr) with
                      | TArrow (_, t2), _ -> t2
                      | TAny, pos_any -> TAny, pos_any
                      | _ -> assert false);
                  };
              },
            binder_pos );
        ],
      ren_ctx )
  | EMatch { e = e1; cases; name } ->
    let typ = Expr.maybe_ty (Mark.get e1) in
    let e1_stmts, new_e1, ren_ctx = translate_expr ctxt e1 in
    let ctxt = { ctxt with ren_ctx } in
    let e1_stmts, switch_var, ctxt =
      match new_e1 with
      | A.EVar v, _ -> e1_stmts, v, ctxt
      | _ ->
        let v, ctxt = fresh_var ctxt ctxt.context_name ~pos:(Expr.pos e1) in
        ( RevBlock.append e1_stmts
            ( A.SLocalInit { name = v, Expr.pos e1; expr = new_e1; typ },
              Expr.pos e1 ),
          v,
          ctxt )
    in
    let new_cases =
      EnumConstructor.Map.fold
        (fun _ arg new_args ->
          match Mark.remove arg with
          | EAbs { binder; tys = typ :: _ } ->
            let vars, body, ctxt = unmbind { ctxt with ren_ctx } binder in
            assert (Array.length vars = 1);
            let var = vars.(0) in
            let scalc_var, ctxt =
              register_fresh_var ctxt var ~pos:(Expr.pos arg)
            in
            let new_arg, _ren_ctx = translate_assignment ctxt assign_to body in
            {
              A.case_block = RevBlock.rebuild new_arg;
              payload_var_name = scalc_var;
              payload_var_typ = typ;
            }
            :: new_args
          | _ -> assert false)
        cases []
    in
    ( e1_stmts
      +> ( A.SSwitch
             {
               switch_var;
               switch_var_typ = typ;
               enum_name = name;
               switch_cases = List.rev new_cases;
             },
           Expr.pos block_expr ),
      ren_ctx )
  | EIfThenElse { cond; etrue; efalse } ->
    let cond_stmts, s_cond, ren_ctx = translate_expr ctxt cond in
    let ctxt = { ctxt with ren_ctx } in
    let s_e_true, _ = translate_assignment ctxt assign_to etrue in
    let s_e_false, _ = translate_assignment ctxt assign_to efalse in
    ( cond_stmts
      +> ( A.SIfThenElse
             {
               if_expr = s_cond;
               then_block = RevBlock.rebuild s_e_true;
               else_block = RevBlock.rebuild s_e_false;
             },
           Expr.pos block_expr ),
      ren_ctx )
  | ELit _ | EAppOp _ | EArray _ | EVar _ | EStruct _ | EInj _ | ETuple _
  | ETupleAccess _ | EStructAccess _ | EExternal _ | EApp _ ->
    let stmts, expr, ren_ctx =
      match block_expr with
      | ((EStruct _ | EInj _ | ETuple _ | EArray _), _) as e ->
        translate_struct_literal ctxt e
      | e -> translate_expr ctxt e
    in
    ( (stmts
      +>
      match assign_to with
      | None -> A.SReturn expr, pos
      | Some name ->
        ( A.SLocalDef
            {
              name;
              expr;
              typ = Mark.remove (Expr.maybe_ty (Mark.get block_expr)), pos;
            },
          pos )),
      ren_ctx )
  | _ -> .

let rec translate_scope_body_expr ctx (scope_expr : 'm L.expr scope_body_expr) :
    A.block =
  match scope_expr with
  | Last e ->
    let block, new_e, _ren_ctx = translate_expr ctx e in
    RevBlock.rebuild block ~tail:[A.SReturn new_e, Mark.get new_e]
  | Cons (scope_let, next_bnd) ->
    let let_var, scope_let_next, ctx = unbind ctx next_bnd in
    let let_var_id, ctx =
      register_fresh_var ctx let_var ~pos:scope_let.scope_let_pos
    in
    let pos = scope_let.scope_let_pos in
    let decl, assign_to =
      if scope_let.scope_let_kind = Assertion then RevBlock.empty, None
      else
        ( RevBlock.make
            [
              ( A.SLocalDecl
                  { name = let_var_id, pos; typ = scope_let.scope_let_typ },
                pos );
            ],
          Some (let_var_id, pos) )
    in
    let statements, ren_ctx =
      translate_assignment ctx assign_to scope_let.scope_let_expr
    in
    RevBlock.rebuild (decl ++ statements)
      ~tail:(translate_scope_body_expr { ctx with ren_ctx } scope_let_next)

let translate_program ~(config : translation_config) (p : 'm L.program) :
    A.program =
  let ctxt =
    {
      func_dict = Var.Map.empty;
      var_dict = Var.Map.empty;
      context_name = "top";
      config;
      program_ctx = { A.decl_ctx = p.decl_ctx; modules = ModuleName.Map.empty };
      ren_ctx = config.renaming_context;
    }
  in
  let modules, ctxt =
    List.fold_left
      (fun (modules, ctxt) (m, _) ->
        let name, pos = ModuleName.get_info m in
        let vname, ctxt = get_name ctxt name in
        ModuleName.Map.add m (A.VarName.fresh (vname, pos)) modules, ctxt)
      (ModuleName.Map.empty, ctxt)
      (Program.modules_to_list p.decl_ctx.ctx_modules)
  in
  let program_ctx = { ctxt.program_ctx with A.modules } in
  let ctxt = { ctxt with program_ctx } in
  let (_, rev_items), _vlist =
    BoundList.fold_left ~init:(ctxt, [])
      ~f:(fun (ctxt, rev_items) code_item var ->
        match code_item with
        | ScopeDef (name, body) ->
          let scope_input_var, scope_body_expr, outer_ctx =
            unbind ctxt body.scope_body_expr
          in
          let input_pos = Mark.get (ScopeName.get_info name) in
          let scope_input_var_id, inner_ctx =
            register_fresh_var ctxt scope_input_var ~pos:input_pos
          in
          let new_scope_body =
            translate_scope_body_expr
              {
                inner_ctx with
                context_name = Mark.remove (ScopeName.get_info name);
              }
              scope_body_expr
          in
          let func_id, outer_ctx =
            register_fresh_func outer_ctx var ~pos:input_pos
          in
          ( outer_ctx,
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
        | Topdef (name, topdef_ty, _vis, (EAbs abs, m)) ->
          (* Toplevel function def *)
          let (block, expr, _ren_ctx_inner), args_id =
            let args_a, expr, ctxt_inner = unmbind ctxt abs.binder in
            let args = Array.to_list args_a in
            let rargs_id, ctxt_inner =
              List.fold_left2
                (fun (rargs_id, ctxt_inner) v ty ->
                  let pos = Mark.get ty in
                  let id, ctxt_inner = register_fresh_var ctxt_inner v ~pos in
                  ((id, pos), ty) :: rargs_id, ctxt_inner)
                ([], ctxt_inner) args abs.tys
            in
            let ctxt_inner =
              {
                ctxt_inner with
                context_name = Mark.remove (TopdefName.get_info name);
              }
            in
            translate_expr ctxt_inner expr, List.rev rargs_id
          in
          let body_block =
            RevBlock.rebuild block ~tail:[A.SReturn expr, Mark.get expr]
          in
          let func_id, ctxt_outer =
            register_fresh_func ctxt var ~pos:(Expr.mark_pos m)
          in
          ( ctxt_outer,
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
        | Topdef (name, topdef_ty, _vis, expr) ->
          (* Toplevel constant def *)
          let block, expr, _ren_ctx_inner =
            let ctxt =
              {
                ctxt with
                context_name = Mark.remove (TopdefName.get_info name);
              }
            in
            translate_expr ctxt expr
          in
          let var_id, ctxt =
            register_fresh_var ctxt var
              ~pos:(Mark.get (TopdefName.get_info name))
          in
          (* If the evaluation of the toplevel expr requires preliminary
             statements, we lift its computation into an auxiliary function *)
          let rev_items, ctxt =
            if (block :> (A.stmt * Pos.t) list) = [] then
              A.SVar { var = var_id; expr; typ = topdef_ty } :: rev_items, ctxt
            else
              let pos = Mark.get expr in
              let func_name, ctxt =
                get_name ctxt (A.VarName.to_string var_id ^ "_init")
              in
              let func_id = A.FuncName.fresh (func_name, pos) in
              (* The list is being built in reverse order *)
              (* FIXME: find a better way than a function with no parameters... *)
              ( A.SVar
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
                               ~tail:[A.SReturn expr, Mark.get expr];
                           A.func_return_typ = topdef_ty;
                         };
                     }
                :: rev_items,
                ctxt )
          in
          ( ctxt,
            (* No need to add func_id since the function will only be called
               right here *)
            rev_items ))
      p.code_items
  in
  {
    ctx = program_ctx;
    code_items = List.rev rev_items;
    module_name = p.module_name;
  }
