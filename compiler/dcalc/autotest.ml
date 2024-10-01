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

let scope ctx lang env name scope =
  let info = ScopeName.Map.find name ctx.ctx_scopes in
  let input_struct = StructName.Map.find info.in_struct_name ctx.ctx_structs in
  let output_struct =
    StructName.Map.find info.out_struct_name ctx.ctx_structs
  in
  match
    begin
      if not (StructField.Map.is_empty input_struct) then raise Exit;
      Message.debug "Interpreting scope %a for autotest instrumentation..."
        ScopeName.format name;
      let body_expr =
        Bindlib.subst scope.scope_body_expr
          (EStruct
             {
               name = scope.scope_body_input_struct;
               fields = StructField.Map.empty;
             })
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
        "Failed to interpret scope %a, will not add automatic tests."
        ScopeName.format name;
    (* Just rebox the original scope without changes *)
    let var, body = Bindlib.unbind scope.scope_body_expr in
    let body = Scope.map_exprs_in_lets ~f:Expr.rebox ~varf:Fun.id body in
    Bindlib.bind_var var body
    |> Bindlib.box_apply (fun scope_body_expr -> { scope with scope_body_expr })
  | EStruct { fields = output_fields; _ }, _ ->
    let rec make_assertions acc path ty e =
      match e with
      | EStruct { name; fields }, m ->
        let tys = StructName.Map.find name ctx.ctx_structs in
        StructField.Map.fold
          (fun field value acc ->
            let ty = StructField.Map.find field tys in
            let path =
              Expr.estructaccess ~name ~field ~e:path (Expr.with_ty m ty)
            in
            make_assertions acc path ty value)
          fields acc
      | (_, m) as e ->
        if Type.has_arrow ctx ty then acc
        else
          let pos = Expr.mark_pos m in
          Message.debug "[autotest] Adding assertion %a.%a = %a"
            ScopeName.format name Expr.format (Expr.unbox path) Expr.format e;
          Expr.eassert
            (Expr.eappop ~op:(Op.Eq, pos)
               ~args:[path; Expr.box (Interpreter.delcustom e)]
               ~tys:[ty; ty]
               (Expr.with_ty m (TLit TBool, pos)))
            (Expr.with_ty m (TLit TUnit, pos))
          :: acc
    in
    let var, body = Bindlib.unbind scope.scope_body_expr in
    let body =
      BoundList.map_last
        ~f:(fun v scope_let ->
          ( v,
            Bindlib.box_apply
              (fun e -> { scope_let with scope_let_expr = e })
              Expr.(Box.lift (rebox scope_let.scope_let_expr)) ))
        ~last:(fun struct_expr ->
          match struct_expr with
          | EStruct { fields; _ }, _ ->
            let assertions =
              StructField.Map.fold
                (fun fld def acc ->
                  make_assertions acc (Expr.rebox def)
                    (StructField.Map.find fld output_struct)
                    (StructField.Map.find fld output_fields))
                fields []
            in
            let assertions_let =
              List.rev_map
                (fun e ->
                  let slet =
                    Bindlib.box_apply
                      (fun e ->
                        {
                          scope_let_kind = Assertion;
                          scope_let_typ = TLit TUnit, Expr.pos e;
                          scope_let_expr = e;
                          scope_let_pos = Expr.pos e;
                        })
                      (Expr.Box.lift e)
                  in
                  Var.make "_autotest", slet)
                assertions
            in
            BoundList.of_list assertions_let
              ~last:Expr.(Box.lift (rebox struct_expr))
          | e ->
            Message.error ~pos:(Expr.pos e) ~internal:true
              "Scope body not ending with result structure initialisation")
        body
    in
    Bindlib.bind_var var body
    |> Bindlib.box_apply (fun scope_body_expr -> { scope with scope_body_expr })
  | _ ->
    assert false (* Evaluation is expected to return the scope output struct *)

let program prg = Program.map_scopes_env ~f:(scope prg.decl_ctx prg.lang) prg
