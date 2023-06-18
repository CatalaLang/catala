(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** This module performs environment-passing style closure conversion, relying
    on the existential [TClosureEnv] type and tuples for closure environments.
    The implementation is based on François Pottier's
    {{:http://gallium.inria.fr/~fpottier/mpri/cours04.pdf#page=10} MPRI lesson}.
    After closure conversion, closure hoisting is perform and all closures end
    up as toplevel definitions. *)

val closure_conversion : 'm Ast.program -> 'm Ast.program Bindlib.box
