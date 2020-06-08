(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the license *)

module UidMap = Context.UidMap
module UidSet = Context.UidSet

(** Processes scope use to :

    - regroup rules or definitions (aka defaults) of the same variable under the same hood
    - push scope use conditions into the justification of the defaults *)

let desugar (_context : Context.context) (_prgm : Ir.program_use) : Ir.program = assert false

(** Inlines subscope definitions, rules and assertions into the parent scope *)
let inline (prgm : Ir.program) : Ir.program = prgm
