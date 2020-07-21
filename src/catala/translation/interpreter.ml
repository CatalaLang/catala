(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(* We interpret the result of a computation as an integer *)
type result = int

type uid = int

type scope_uid = int

module UidMap = Uid.UidMap

type exec_context = result UidMap.t

let substitute (_var : uid) (_value : result) (_term : Lambda.term) : Lambda.term = assert false

let eval_term ((term, _) : Lambda.term) : result = match Pos.unmark term with _ -> assert false

let eval_default_term (_term : Lambda.default_term) : result option = assert false

(** Returns the scheduling of the scope variables, if y is a subscope and x a variable of y, then we
    have two different variable y.x(internal) and y.x(result) and the ordering y.x(internal) -> y ->
    y.x(result) *)
let schedule_scope (_scope : Scope.scope) : uid list = assert false

let merge_var_redefs (_subscope : uid) (_caller_scope : scope_uid) (_prgm : Scope.program) :
    Scope.scope =
  assert false

let execute_scope (_scope : scope_uid) (_prgm : Scope.program) : exec_context = assert false
