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

let map_exprs ~f ~varf { scopes; decl_ctx } =
  Bindlib.box_apply
    (fun scopes -> { scopes; decl_ctx })
    (Scope.map_exprs ~f ~varf scopes)

let get_scope_body { scopes; _ } scope =
  match
    Scope.fold_left ~init:None
      ~f:(fun acc item _ ->
          match item with
          | ScopeDef (name, body) when ScopeName.equal scope name -> Some body
          | _ -> acc)
      scopes
  with
  | None -> raise Not_found
  | Some body -> body

let untype : 'm. ('a, 'm mark) gexpr program -> ('a, untyped mark) gexpr program
    =
 fun prg -> Bindlib.unbox (map_exprs ~f:Expr.untype ~varf:Var.translate prg)

let rec find_scope name vars = function
  | Nil -> raise Not_found
  | Cons (ScopeDef (n, body), _) when ScopeName.equal name n -> List.rev vars, body
  | Cons (_, next_bind) ->
    let var, next = Bindlib.unbind next_bind in
    find_scope name (var :: vars) next

let to_expr p main_scope =
  let _, main_scope_body = find_scope main_scope [] p.scopes in
  Scope.unfold p.decl_ctx p.scopes
    (Scope.get_body_mark main_scope_body)
    (ScopeName main_scope)
