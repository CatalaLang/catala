(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Print a scope program *)
let print_scope (scope : Scope_ast.scope) : string =
  let print_defs (defs : Scope_ast.definition Uid.ScopeDefMap.t) : string =
    defs |> Uid.ScopeDefMap.bindings
    |> List.map (fun (uid, term) ->
           Printf.sprintf "%s:\n%s" (Uid.ScopeDef.format_t uid) (Format_lambda.print_term term))
    |> String.concat ""
  in
  "___Variables Definition___\n" ^ print_defs scope.scope_defs ^ "___Subscope (Re)definition___\n"
  ^ "\n"

(** Print the whole program *)
let print_program (prgm : Scope_ast.program) : string =
  prgm |> Uid.ScopeMap.bindings
  |> List.map (fun (uid, scope) ->
         Printf.sprintf "Scope %s:\n%s" (Uid.Scope.format_t uid) (print_scope scope))
  |> String.concat "\n"
  |> Printf.sprintf "Scope program\n%s"
