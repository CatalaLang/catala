(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** {1 Catala operator utilities} *)

(** Resolving operators from the surface syntax proceeds in three steps:

    - During desugaring, the operators may remain untyped (with [TAny]) or, if
      they have an explicit type suffix (e.g. the [$] for "money" in [+$]),
      their operands types are already explicited in the [EOp] expression node.
    - {!modules:Shared_ast.Typing} will then enforce these constraints in
      addition to the known built-in type for each operator (e.g.
      [Eq: 'a -> 'a -> 'a] isn't encoded in the first-order AST types).
    - Finally, during {!modules:Scopelang.From_desugared}, these types are
      leveraged to resolve the overloaded operators to their concrete,
      monomorphic counterparts
*)

open Catala_utils
open Definitions
include module type of Definitions.Op

val equal : 'a1 t -> 'a2 t -> bool
val compare : 'a1 t -> 'a2 t -> int

val name : 'a t -> string
(** Returns the operator name as a valid ident starting with a lowercase
    character. This is different from Print.operator which returns operator
    symbols, e.g. [+$]. *)

val kind_dispatch :
  polymorphic:([> polymorphic ] t -> 'b) ->
  monomorphic:([> monomorphic ] t -> 'b) ->
  ?overloaded:([> overloaded ] t -> 'b) ->
  ?resolved:([> resolved ] t -> 'b) ->
  'a t ->
  'b
(** Calls one of the supplied functions depending on the kind of the operator *)

type 'a no_overloads = [< all_ast_features | `Monomorphic | `Polymorphic | `Resolved ] as 'a

val translate :
  [> `Monomorphic | `Polymorphic | `Resolved ] no_overloads t ->
  [> `Monomorphic | `Polymorphic | `Resolved ] t
(** An identity function that allows translating an operator between different
    passes that don't change operator types *)

(** {2 Getting the types of operators} *)

val monomorphic_type : monomorphic t Marked.pos -> typ
val resolved_type : resolved t Marked.pos -> typ

val overload_type : decl_ctx -> overloaded t Marked.pos -> typ list -> typ
(** The type for typing overloads is different since the types of the operands
    are required in advance.

    @raise a detailed user error if no matching operator can be found *)

(** Polymorphic operators are typed directly within [Typing], since their types
    may contain type variables that can't be expressed outside of it*)

(** {2 Overload handling} *)

val resolve_overload :
  decl_ctx ->
  overloaded t Marked.pos ->
  typ list ->
  [> resolved ] t * [ `Straight | `Reversed ]
(** Some overloads are sugar for an operation with reversed operands, e.g.
    [TRat * TMoney] is using [mult_mon_rat]. [`Reversed] is returned to signify
    this case. *)
