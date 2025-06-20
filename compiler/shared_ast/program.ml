(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Denis Merigoux <denis.merigoux@inria.fr>, Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>, Louis Gesbert <louis.gesbert@inria.fr>

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
open Definitions

let map_decl_ctx ~f ctx =
  {
    ctx with
    ctx_enums = EnumName.Map.map (EnumConstructor.Map.map f) ctx.ctx_enums;
    ctx_structs = StructName.Map.map (StructField.Map.map f) ctx.ctx_structs;
    ctx_topdefs =
      TopdefName.Map.map (fun (ty, vis) -> f ty, vis) ctx.ctx_topdefs;
  }

let map_scopes ~f prg =
  let code_items =
    let f var = function
      | ScopeDef (name, body) ->
        var, Bindlib.box_apply (fun body -> ScopeDef (name, body)) (f name body)
      | Topdef (name, ty, vis, expr) ->
        ( var,
          Bindlib.box_apply
            (fun e -> Topdef (name, ty, vis, e))
            (Expr.Box.lift (Expr.rebox expr)) )
    in
    BoundList.map ~f ~last:(Scope.map_exports Expr.rebox) prg.code_items
    |> Bindlib.unbox
  in
  { prg with code_items }

let map_scopes_env
    ~f
    ?(last = fun _ _ -> Scope.map_exports Expr.rebox)
    ~init
    prg =
  let code_items =
    let f (acc, env) var = function
      | ScopeDef (name, body) ->
        let pos = Mark.get (ScopeName.get_info name) in
        let acc, body1 = f acc env name body in
        let env child =
          env
          @@ Expr.make_let_in (Mark.add pos var)
               ( TArrow
                   ( [TStruct body.scope_body_input_struct, pos],
                     (TStruct body.scope_body_output_struct, pos) ),
                 pos )
               (Scope.to_expr prg.decl_ctx body)
               child pos
        in
        let def = Bindlib.box_apply (fun body -> ScopeDef (name, body)) body1 in
        (acc, env), var, def
      | Topdef (name, ty, vis, expr) ->
        let pos = Mark.get (TopdefName.get_info name) in
        let env child =
          env
          @@ Expr.make_let_in (Mark.add pos var) ty (Expr.rebox expr) child pos
        in
        let def =
          Bindlib.box_apply
            (fun e -> Topdef (name, ty, vis, e))
            (Expr.Box.lift (Expr.rebox expr))
        in
        (acc, env), var, def
    in
    BoundList.fold_map
      ~init:(init, fun e -> e)
      ~f
      ~last:(fun (acc, env) elast -> (), last acc env elast)
      prg.code_items
    |> snd
    |> Bindlib.unbox
  in
  { prg with code_items }

let map_exprs ?typ ~f ~varf { code_items; decl_ctx; lang; module_name } =
  let boxed_prg =
    Bindlib.box_apply
      (fun code_items ->
        let decl_ctx =
          match typ with None -> decl_ctx | Some f -> map_decl_ctx ~f decl_ctx
        in
        { code_items; decl_ctx; lang; module_name })
      (Scope.map_exprs ?typ ~f ~varf code_items)
  in
  Expr.Box.assert_closed boxed_prg;
  Bindlib.unbox boxed_prg

let fold_left ~f ~init { code_items; _ } =
  fst @@ BoundList.fold_left ~f:(fun acc e _ -> f acc e) ~init code_items

let fold_exprs ~f ~init prg = Scope.fold_exprs ~f ~init prg.code_items

let fold_right ~f ~init { code_items; _ } =
  BoundList.fold_right
    ~f:(fun e _ acc -> f e acc)
    ~init:(fun _vlist -> init)
    code_items

let empty_ctx =
  {
    ctx_enums = EnumName.Map.empty;
    ctx_structs = StructName.Map.empty;
    ctx_scopes = ScopeName.Map.empty;
    ctx_topdefs = TopdefName.Map.empty;
    ctx_public_types = TypeIdent.Set.empty;
    ctx_struct_fields = Ident.Map.empty;
    ctx_enum_constrs = Ident.Map.empty;
    ctx_scope_index = Ident.Map.empty;
    ctx_modules = ModuleName.Map.empty;
  }

let get_scope_body { code_items; _ } scope =
  match
    BoundList.fold_left ~init:None
      ~f:(fun acc item _ ->
        match item with
        | ScopeDef (name, body) when ScopeName.equal scope name -> Some body
        | _ -> acc)
      code_items
  with
  | None, _ -> raise Not_found
  | Some body, _ -> body

let get_mark_witness { code_items; _ } =
  BoundList.find code_items ~f:(function
    | Topdef (_, _, _, e) -> Some (Mark.get e)
    | ScopeDef (_, body) -> Scope.get_mark_witness body)

let untype : 'm. ('a, 'm) gexpr program -> ('a, untyped) gexpr program =
 fun prg -> map_exprs ~f:Expr.untype ~varf:Var.translate prg

let find_scope name =
  BoundList.find ~f:(function
    | ScopeDef (n, body) when ScopeName.equal name n -> Some body
    | _ -> None)

let to_expr p main_scope =
  let res = Scope.unfold p.decl_ctx p.code_items main_scope in
  Expr.Box.assert_closed (Expr.Box.lift res);
  res

let modules_to_list (mt : module_tree) =
  let rec aux acc mtree =
    ModuleName.Map.fold
      (fun mname mnode acc ->
        if List.exists (fun (m, _) -> ModuleName.equal m mname) acc then acc
        else (mname, mnode.intf_id) :: aux acc mnode.deps)
      mtree acc
  in
  List.rev (aux [] mt)
