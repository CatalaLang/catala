(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020-2022 Inria, contributor: Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module D = Ast

(** Alternative representation of the Dcalc Ast. It is currently used in the transformation without
    exceptions. We make heavy use of bindlib, binding each scope-let-variable and each scope
    explicitly. *)

(** In [Ast], [Ast.scope_lets] is defined as a list of kind, var, and boxed expression. This
    representation binds using bindlib the tail of the list with the variable defined in the let. *)
type scope_lets =
  | Result of D.expr Utils.Pos.marked
  | ScopeLet of {
      scope_let_kind : D.scope_let_kind;
      scope_let_typ : D.typ Utils.Pos.marked;
      scope_let_expr : D.expr Utils.Pos.marked;
      scope_let_next : (D.expr, scope_lets) Bindlib.binder;
      scope_let_pos : Utils.Pos.t;
    }

type scope_body = {
  scope_body_input_struct : D.StructName.t;
  scope_body_output_struct : D.StructName.t;
  scope_body_result : (D.expr, scope_lets) Bindlib.binder;
}
(** As a consequence, the scope_body contains only a result and input/output signature, as the other
    elements are stored inside the scope_let. The binder present is the argument of type
    [scope_body_input_struct]. *)

(** Finally, we do the same transformation for the whole program for the kinded lets. This permit us
    to use bindlib variables for scopes names. *)
type scopes =
  | Nil
  | ScopeDef of {
      scope_name : D.ScopeName.t;
      scope_body : scope_body;
      scope_next : (D.expr, scopes) Bindlib.binder;
    }

val free_vars_list_scope_lets : scope_lets -> D.Var.t list
(** List of variables not binded inside a scope_lets *)

val free_vars_list_scope_body : scope_body -> D.Var.t list
(** List of variables not binded inside a scope_body. *)

val free_vars_list_scopes : scopes -> D.Var.t list
(** List of variables not binded inside scopes*)

val bind_scopes : (D.ScopeName.t * D.expr Bindlib.var * D.scope_body) list -> scopes Bindlib.box
(** Transform a list of scopes into our representation of scopes. It requires that scopes are
    topologically-well-ordered, and ensure there is no free variables in the returned [scopes] *)
