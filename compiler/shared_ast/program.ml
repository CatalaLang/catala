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

let map_exprs ~f ~varf { code_items; decl_ctx } =
  Bindlib.box_apply
    (fun code_items -> { code_items; decl_ctx })
    (Scope.map_exprs ~f ~varf code_items)

let fold_left_exprs ~f ~init { code_items; decl_ctx = _ } =
  Scope.fold_left ~f:(fun acc e _ -> f acc e) ~init code_items

let fold_right_exprs ~f ~init { code_items; decl_ctx = _ } =
  Scope.fold_right ~f:(fun e _ acc -> f e acc) ~init code_items

let empty_ctx =
  {
    ctx_enums = EnumName.Map.empty;
    ctx_structs = StructName.Map.empty;
    ctx_struct_fields = Ident.Map.empty;
    ctx_scopes = ScopeName.Map.empty;
    ctx_topdefs = TopdefName.Map.empty;
    ctx_modules = ModuleName.Map.empty;
  }

let rec module_ctx ctx = function
  | [] -> ctx
  | (modname, mpos) :: path ->
    match ModuleName.Map.find_opt modname ctx.ctx_modules with
    | None ->
      Message.raise_spanned_error mpos "Module %a not found" ModuleName.format modname
    | Some ctx -> module_ctx ctx path

let get_scope_body { code_items; _ } scope =
  match
    Scope.fold_left ~init:None
      ~f:(fun acc item _ ->
        match item with
        | ScopeDef (name, body) when ScopeName.equal scope name -> Some body
        | _ -> acc)
      code_items
  with
  | None -> raise Not_found
  | Some body -> body

let untype : 'm. ('a, 'm) gexpr program -> ('a, untyped) gexpr program =
 fun prg -> Bindlib.unbox (map_exprs ~f:Expr.untype ~varf:Var.translate prg)

let rec find_scope name vars = function
  | Nil -> raise Not_found
  | Cons (ScopeDef (n, body), _) when ScopeName.equal name n ->
    List.rev vars, body
  | Cons (_, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    find_scope name (var :: vars) next

let rec all_scopes code_item_list =
  match code_item_list with
  | Nil -> []
  | Cons (ScopeDef (n, _), next_bind) ->
    let _var, next = Bindlib.unbind next_bind in
    n :: all_scopes next
  | Cons (_, next_bind) ->
    let _var, next = Bindlib.unbind next_bind in
    all_scopes next

let to_expr p main_scope =
  let _, main_scope_body = find_scope main_scope [] p.code_items in

  let res =
    Scope.unfold p.decl_ctx p.code_items
      (Scope.get_body_mark main_scope_body)
      (ScopeName main_scope)
  in
  Expr.Box.assert_closed (Expr.Box.lift res);
  res

let equal p p' =
  (* TODO: include toplevel definitions in this program comparison. *)
  let ss = all_scopes p.code_items in
  let ss' = all_scopes p'.code_items in

  List.length ss = List.length ss'
  && ListLabels.for_all2 ss ss' ~f:(fun s s' ->
         ScopeName.equal s s'
         &&
         let e1 = Expr.unbox @@ to_expr p s in
         let e2 = Expr.unbox @@ to_expr p' s in
         Expr.equal e1 e2)
