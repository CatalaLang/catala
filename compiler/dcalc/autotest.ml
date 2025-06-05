(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

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

let rec make_assertions ctx scope_name acc path ty e =
  match e with
  | EStruct { name; fields }, m ->
    let tys = StructName.Map.find name ctx.ctx_structs in
    StructField.Map.fold
      (fun field value acc ->
        let ty = StructField.Map.find field tys in
        let path =
          Expr.estructaccess ~name ~field ~e:path (Expr.with_ty m ty)
        in
        make_assertions ctx scope_name acc path ty value)
      fields acc
  | (_, m) as e ->
    if Type.has_arrow ctx ty then acc
    else
      let pos = Expr.mark_pos m in
      Message.debug "[autotest] Adding assertion %a.%a = %a" ScopeName.format
        scope_name Expr.format (Expr.unbox path) Expr.format e;
      Expr.eassert
        (Expr.eappop ~op:(Op.Eq, pos)
           ~args:[path; Expr.box (Interpreter.delcustom e)]
           ~tys:[ty; ty]
           (Expr.with_ty m (TLit TBool, pos)))
        (Expr.with_ty m (TLit TUnit, pos))
      :: acc

let test_scope_outs ctx lang acc env name scope =
  let info = ScopeName.Map.find name ctx.ctx_scopes in
  let acc =
    match
      begin
        if not (Pos.has_attr (Mark.get (ScopeName.get_info name)) Test) then
          raise Exit;
        Message.debug "Interpreting scope %a for autotest instrumentation..."
          ScopeName.format name;
        let mark =
          Expr.with_pos
            (Mark.get (ScopeName.get_info name))
            (Option.get (Scope.get_mark_witness scope))
        in
        let body_expr =
          Bindlib.subst scope.scope_body_expr
            (Mark.remove
               (Expr.unbox
                  (Scope.empty_input_struct_dcalc ctx info.in_struct_name mark)))
        in
        let expr = Scope.unfold_body_expr ctx body_expr in
        Interpreter.evaluate_expr ctx lang (Expr.unbox_closed (env expr))
      end
    with
    | exception
        (( Sys.Break | Assert_failure _ | Match_failure _ | Out_of_memory
         | Stack_overflow ) as e) ->
      raise e
    | exception e ->
      if e <> Exit then
        Message.warning
          "Failed to interpret scope %a: cannot add autotests, will generate \
           an always-failing test program."
          ScopeName.format name;
      acc
    | output_expr -> ScopeName.Map.add name output_expr acc
  in
  let var, body = Bindlib.unbind scope.scope_body_expr in
  let body = Scope.map_exprs_in_lets ~f:Expr.rebox ~varf:Fun.id body in
  ( acc,
    Bindlib.bind_var var body
    |> Bindlib.box_apply (fun scope_body_expr -> { scope with scope_body_expr })
  )

let program prg =
  let ctx = prg.decl_ctx in
  Program.map_scopes_env prg ~f:(test_scope_outs prg.decl_ctx prg.lang)
    ~init:ScopeName.Map.empty ~last:(fun scope_asserts _env exports ->
      List.map
        (function
          | ((KTopdef _ | KScope _) as export), e ->
            Bindlib.box_apply
              (fun e -> export, e)
              (Expr.Box.lift (Expr.rebox e))
          | KTest name, scope_apply ->
            let pos = Mark.get (ScopeName.get_info name) in
            let struc =
              (ScopeName.Map.find name ctx.ctx_scopes).out_struct_name
            in
            let ty = TStruct struc, pos in
            let v_result = Var.make (ScopeName.to_string name) in
            let asserts =
              match ScopeName.Map.find_opt name scope_asserts with
              | None ->
                let m = Mark.get scope_apply in
                [
                  Expr.eassert
                    (Expr.elit (LBool false) (Expr.with_ty m (TLit TBool, pos)))
                    (Expr.with_ty m (TLit TUnit, pos));
                ]
              | Some expected_result ->
                make_assertions ctx name []
                  (Expr.make_var v_result (Mark.get expected_result))
                  ty expected_result
            in
            let asserts_expr =
              Expr.make_multiple_let_in
                (List.map
                   (fun _ ->
                     Var.make ("_test_" ^ ScopeName.to_string name), pos)
                   asserts)
                (List.map (fun _ -> TLit TUnit, pos) asserts)
                asserts
                (Expr.make_var v_result (Mark.get scope_apply))
                pos
            in
            Expr.make_let_in
              (v_result, Expr.pos scope_apply)
              (TStruct struc, pos) (Expr.rebox scope_apply) asserts_expr pos
            |> Expr.Box.lift
            |> Bindlib.box_apply (fun e -> KTest name, e))
        exports
      |> Bindlib.box_list)
