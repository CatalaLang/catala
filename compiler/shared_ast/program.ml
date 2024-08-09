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

open Definitions

let map_decl_ctx ~f ctx =
  {
    ctx with
    ctx_enums = EnumName.Map.map (EnumConstructor.Map.map f) ctx.ctx_enums;
    ctx_structs = StructName.Map.map (StructField.Map.map f) ctx.ctx_structs;
    ctx_topdefs = TopdefName.Map.map f ctx.ctx_topdefs;
  }

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
