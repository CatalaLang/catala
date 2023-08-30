(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
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

(** Graph representation of the dependencies between scopes in the Catala
    program. Vertices are functions, x -> y if x is used in the definition of y. *)

open Catala_utils
open Shared_ast

(** {1 Scope dependencies} *)

type vertex = Scope of ScopeName.t | Topdef of TopdefName.t

(** On the edges, the label is the expression responsible for the use of the
    function *)
module SDependencies :
  Graph.Sig.P with type V.t = vertex and type E.label = Pos.t

val build_program_dep_graph : 'm Ast.program -> SDependencies.t
val check_for_cycle_in_defs : SDependencies.t -> unit
val get_defs_ordering : SDependencies.t -> vertex list

(** {1 Type dependencies} *)

module TVertex : sig
  type t = Struct of StructName.t | Enum of EnumName.t

  val format : Format.formatter -> t -> unit
  val get_info : t -> Uid.MarkedString.info

  include Graph.Sig.COMPARABLE with type t := t
end

module TVertexSet : Set.S with type elt = TVertex.t

(** On the edges, the label is the expression responsible for the use of the
    function *)
module TDependencies :
  Graph.Sig.P with type V.t = TVertex.t and type E.label = Pos.t

val get_structs_or_enums_in_type : typ -> TVertexSet.t
val build_type_graph : struct_ctx -> enum_ctx -> TDependencies.t
val check_type_cycles : struct_ctx -> enum_ctx -> TVertex.t list
