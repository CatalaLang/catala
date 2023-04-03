(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Alain Delaët <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

module Ren = struct
  module Set = Set.Make (String)

  type ctxt = Set.t

  let skip_constant_binders = true
  let reset_context_for_closed_terms = true
  let constant_binder_name = None
  let empty_ctxt = Set.empty
  let reserve_name n s = Set.add n s
  let new_name n s = n, Set.add n s
end

module Ctx = Bindlib.Ctxt (Ren)

let fv b = Ren.Set.elements (Ctx.free_vars b)
