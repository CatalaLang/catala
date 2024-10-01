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

(** {2 Program declaration context helpers} *)

val empty_ctx : decl_ctx

(** {2 Transformations} *)

val map_decl_ctx : f:(typ -> typ) -> decl_ctx -> decl_ctx

val map_scopes :
  f:(ScopeName.t -> 'e scope_body -> 'e scope_body Bindlib.box) ->
  'e program ->
  'e program

val map_scopes_env :
  f:
    (('e boxed -> 'e boxed) ->
    ScopeName.t ->
    'e scope_body ->
    'e scope_body Bindlib.box) ->
  'e program ->
  'e program
(** Maps on the scopes in the program, passing along an "environment" in the
    form of a function binding all toplevel and scope definitions to their
    variables in the argument expression *)

val map_exprs :
  ?typ:(typ -> typ) ->
  f:('expr1 -> 'expr2 boxed) ->
  varf:('expr1 Var.t -> 'expr2 Var.t) ->
  'expr1 program ->
  'expr2 program
(** If [typ] is specified, definitions in [decl_ctx] are also processed *)

val fold_left :
  f:('a -> 'expr code_item -> 'a) -> init:'a -> 'expr program -> 'a

val fold_exprs : f:('a -> 'expr -> typ -> 'a) -> init:'a -> 'expr program -> 'a

val fold_right :
  f:('expr code_item -> 'a -> 'a) -> init:'a -> 'expr program -> 'a

val get_scope_body :
  ((_ any, 't) gexpr as 'e) program -> ScopeName.t -> 'e scope_body

val untype : ('a any, _) gexpr program -> ('a, untyped) gexpr program

val to_expr : ((_ any, _) gexpr as 'e) program -> ScopeName.t -> 'e boxed
(** Usage: [build_whole_program_expr program main_scope] builds an expression
    corresponding to the main program and returning the main scope as a
    function. *)

val find_scope : ScopeName.t -> 'e code_item_list -> 'e scope_body

val modules_to_list : module_tree -> (ModuleName.t * module_intf_id) list
(** Returns a list of used modules, in topological order ; the boolean indicates
    if the module is external *)
