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

(** Graph representation of the dependencies between scopes in the Catala program. Vertices are
    functions, x -> y if x is used in the definition of y. *)

open Utils

(** On the edges, the label is the expression responsible for the use of the function *)
module SDependencies : Graph.Sig.P with type V.t = Ast.ScopeName.t and type E.label = Pos.t

module STopologicalTraversal : sig
  val fold : (Ast.ScopeName.t -> 'a -> 'a) -> SDependencies.t -> 'a -> 'a

  val iter : (Ast.ScopeName.t -> unit) -> SDependencies.t -> unit
end

(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)
module SSCC : sig
  val scc : SDependencies.t -> int * (Ast.ScopeName.t -> int)

  val scc_array : SDependencies.t -> Ast.ScopeName.t list array

  val scc_list : SDependencies.t -> Ast.ScopeName.t list list
end

val build_program_dep_graph : Ast.program -> SDependencies.t

val check_for_cycle_in_scope : SDependencies.t -> unit

val get_scope_ordering : SDependencies.t -> Ast.ScopeName.t list

module TVertex : sig
  type t = Struct of Ast.StructName.t | Enum of Ast.EnumName.t

  val format_t : Format.formatter -> t -> unit

  val get_info : t -> Ast.StructName.info

  include Graph.Sig.COMPARABLE with type t := t
end

module TVertexSet : Set.S with type elt = TVertex.t

(** On the edges, the label is the expression responsible for the use of the function *)
module TDependencies : Graph.Sig.P with type V.t = TVertex.t and type E.label = Pos.t

module TTopologicalTraversal : sig
  val fold : (TVertex.t -> 'a -> 'a) -> TDependencies.t -> 'a -> 'a

  val iter : (TVertex.t -> unit) -> TDependencies.t -> unit
end

(** Tarjan's stongly connected components algorithm, provided by OCamlGraph *)
module TSCC : sig
  val scc : TDependencies.t -> int * (TVertex.t -> int)

  val scc_array : TDependencies.t -> TVertex.t list array

  val scc_list : TDependencies.t -> TVertex.t list list
end

val get_structs_or_enums_in_type : Ast.typ Pos.marked -> TVertexSet.t

val build_type_graph : Ast.struct_ctx -> Ast.enum_ctx -> TDependencies.t

val check_type_cycles : Ast.struct_ctx -> Ast.enum_ctx -> TVertex.t list
