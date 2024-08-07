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

let cap s = String.to_ascii s |> String.capitalize_ascii
let uncap s = String.to_ascii s |> String.uncapitalize_ascii

(* Todo? - handle separate namespaces ? (e.g. allow a field and var to have the
   same name for backends that support it) - register module names as reserved
   names *)
let rename_ids
    ~reserved
    ~reset_context_for_closed_terms
    ~skip_constant_binders
    ~constant_binder_name
    ~namespaced_fields_constrs
    ?(f_var = String.to_snake_case)
    ?(f_struct = cap)
    ?(f_field = uncap)
    ?(f_enum = cap)
    ?(f_constr = cap)
    p =
  let cfg =
    {
      Expr.Renaming.reserved;
      sanitize_varname = f_var;
      reset_context_for_closed_terms;
      skip_constant_binders;
      constant_binder_name;
    }
  in
  let ctx = Expr.Renaming.get_ctx cfg in
  (* Each module needs its separate ctx since resolution is qualified ; and name
     resolution in a given module must be processed consistently independently
     on the current context. *)
  let ctx0 = ctx in
  let module PathMap = Map.Make (Uid.Path) in
  let pctxmap = PathMap.singleton [] ctx in
  let pctxmap, structs_map, fields_map, ctx_structs =
    (* Warning: the folding order matters here, if a module contains e.g. two
       fields with the same name. This fold relies on UIDs, and is thus
       dependent on the definition order. Another possibility would be to fold
       lexicographically, but the result would be "less intuitive" *)
    StructName.Map.fold
      (fun name fields (pctxmap, structs_map, fields_map, ctx_structs) ->
        let path = StructName.path name in
        let str, pos = StructName.get_info name in
        let pctxmap, ctx =
          try pctxmap, PathMap.find path pctxmap
          with PathMap.Not_found _ -> PathMap.add path ctx pctxmap, ctx
        in
        let id, ctx = Expr.Renaming.new_id ctx (f_struct str) in
        let new_name = StructName.fresh path (id, pos) in
        let ctx1, fields_map, ctx_fields =
          StructField.Map.fold
            (fun name ty (ctx, fields_map, ctx_fields) ->
              let str, pos = StructField.get_info name in
              let id, ctx = Expr.Renaming.new_id ctx (f_field str) in
              let new_name = StructField.fresh (id, pos) in
              ( ctx,
                StructField.Map.add name new_name fields_map,
                StructField.Map.add new_name ty ctx_fields ))
            fields
            ( (if namespaced_fields_constrs then ctx0 else ctx),
              fields_map,
              StructField.Map.empty )
        in
        let ctx = if namespaced_fields_constrs then ctx else ctx1 in
        ( PathMap.add path ctx pctxmap,
          StructName.Map.add name new_name structs_map,
          fields_map,
          StructName.Map.add new_name ctx_fields ctx_structs ))
      p.decl_ctx.ctx_structs
      ( pctxmap,
        StructName.Map.empty,
        StructField.Map.empty,
        StructName.Map.empty )
  in
  let pctxmap, enums_map, constrs_map, ctx_enums =
    EnumName.Map.fold
      (fun name constrs (pctxmap, enums_map, constrs_map, ctx_enums) ->
        let path = EnumName.path name in
        let str, pos = EnumName.get_info name in
        let pctxmap, ctx =
          try pctxmap, PathMap.find path pctxmap
          with Not_found -> PathMap.add path ctx pctxmap, ctx
        in
        let id, ctx = Expr.Renaming.new_id ctx (f_enum str) in
        let new_name = EnumName.fresh path (id, pos) in
        let ctx1, constrs_map, ctx_constrs =
          EnumConstructor.Map.fold
            (fun name ty (ctx, constrs_map, ctx_constrs) ->
              let str, pos = EnumConstructor.get_info name in
              let id, ctx = Expr.Renaming.new_id ctx (f_constr str) in
              let new_name = EnumConstructor.fresh (id, pos) in
              ( ctx,
                EnumConstructor.Map.add name new_name constrs_map,
                EnumConstructor.Map.add new_name ty ctx_constrs ))
            constrs
            ( (if namespaced_fields_constrs then ctx0 else ctx),
              constrs_map,
              EnumConstructor.Map.empty )
        in
        let ctx = if namespaced_fields_constrs then ctx else ctx1 in
        ( PathMap.add path ctx pctxmap,
          EnumName.Map.add name new_name enums_map,
          constrs_map,
          EnumName.Map.add new_name ctx_constrs ctx_enums ))
      p.decl_ctx.ctx_enums
      ( pctxmap,
        EnumName.Map.empty,
        EnumConstructor.Map.empty,
        EnumName.Map.empty )
  in
  let pctxmap, scopes_map, ctx_scopes =
    ScopeName.Map.fold
      (fun name info (pctxmap, scopes_map, ctx_scopes) ->
        let info =
          {
            in_struct_name = StructName.Map.find info.in_struct_name structs_map;
            out_struct_name =
              StructName.Map.find info.out_struct_name structs_map;
            out_struct_fields =
              ScopeVar.Map.map
                (fun fld -> StructField.Map.find fld fields_map)
                info.out_struct_fields;
          }
        in
        let path = ScopeName.path name in
        if path = [] then
          (* Scopes / topdefs in the root module will be renamed through the
             variables binding them in the code_items *)
          ( pctxmap,
            ScopeName.Map.add name name scopes_map,
            ScopeName.Map.add name info ctx_scopes )
        else
          let str, pos = ScopeName.get_info name in
          let pctxmap, ctx =
            try pctxmap, PathMap.find path pctxmap
            with Not_found -> PathMap.add path ctx pctxmap, ctx
          in
          let id, ctx = Expr.Renaming.new_id ctx (f_var str) in
          let new_name = ScopeName.fresh path (id, pos) in
          ( PathMap.add path ctx pctxmap,
            ScopeName.Map.add name new_name scopes_map,
            ScopeName.Map.add new_name info ctx_scopes ))
      p.decl_ctx.ctx_scopes
      (pctxmap, ScopeName.Map.empty, ScopeName.Map.empty)
  in
  let pctxmap, topdefs_map, ctx_topdefs =
    TopdefName.Map.fold
      (fun name typ (pctxmap, topdefs_map, ctx_topdefs) ->
        let path = TopdefName.path name in
        if path = [] then
          (* Topdefs / topdefs in the root module will be renamed through the
             variables binding them in the code_items *)
          ( pctxmap,
            TopdefName.Map.add name name topdefs_map,
            TopdefName.Map.add name typ ctx_topdefs )
          (* [typ] is rewritten later on *)
        else
          let str, pos = TopdefName.get_info name in
          let pctxmap, ctx =
            try pctxmap, PathMap.find path pctxmap
            with Not_found -> PathMap.add path ctx pctxmap, ctx
          in
          let id, ctx = Expr.Renaming.new_id ctx (f_var str) in
          let new_name = TopdefName.fresh path (id, pos) in
          ( PathMap.add path ctx pctxmap,
            TopdefName.Map.add name new_name topdefs_map,
            TopdefName.Map.add new_name typ ctx_topdefs ))
      p.decl_ctx.ctx_topdefs
      (pctxmap, TopdefName.Map.empty, TopdefName.Map.empty)
  in
  let ctx = PathMap.find [] pctxmap in
  let ctx =
    Expr.Renaming.set_rewriters ctx
      ~scopes:(fun n -> ScopeName.Map.find n scopes_map)
      ~topdefs:(fun n -> TopdefName.Map.find n topdefs_map)
      ~structs:(fun n -> StructName.Map.find n structs_map)
      ~fields:(fun n -> StructField.Map.find n fields_map)
      ~enums:(fun n -> EnumName.Map.find n enums_map)
      ~constrs:(fun n -> EnumConstructor.Map.find n constrs_map)
  in
  let decl_ctx =
    { p.decl_ctx with ctx_enums; ctx_structs; ctx_scopes; ctx_topdefs }
  in
  let decl_ctx = map_decl_ctx ~f:(Expr.Renaming.typ ctx) decl_ctx in
  let code_items = Scope.rename_ids ctx p.code_items in
  { p with decl_ctx; code_items }, ctx

(* This first-class module wrapping is here to allow a polymorphic renaming
   function to be passed around *)

module type Renaming = sig
  val apply : 'e program -> 'e program * Expr.Renaming.context
end

type renaming = (module Renaming)

let apply (module R : Renaming) = R.apply

let renaming
    ~reserved
    ~reset_context_for_closed_terms
    ~skip_constant_binders
    ~constant_binder_name
    ~namespaced_fields_constrs
    ?f_var
    ?f_struct
    ?f_field
    ?f_enum
    ?f_constr
    () =
  let module M = struct
    let apply p =
      rename_ids ~reserved ~reset_context_for_closed_terms
        ~skip_constant_binders ~constant_binder_name ~namespaced_fields_constrs
        ?f_var ?f_struct ?f_field ?f_enum ?f_constr p
  end in
  (module M : Renaming)
