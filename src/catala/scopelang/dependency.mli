module SVertex :
  sig
    type t = Ast.ScopeName.t
    val hash : Ast.ScopeName.t -> int
    val compare :
      Ast.ScopeName.t -> Ast.ScopeName.t -> int
    val equal :
      Ast.ScopeName.t -> Ast.ScopeName.t -> bool
    val format_t : Format.formatter -> t -> unit
  end
module SEdge :
  sig
    type t = Utils.Pos.t
    val compare : 'a -> 'a -> int
    val default : Utils.Pos.t
  end
module SDependencies :
  sig
    type t =
        Graph__Persistent.Digraph.ConcreteBidirectionalLabeled(SVertex)(SEdge).t
    module V :
      sig
        type t = SVertex.t
        val compare : t -> t -> int
        val hash : t -> int
        val equal : t -> t -> bool
        type label = SVertex.t
        val create : label -> t
        val label : t -> label
      end
    type vertex = V.t
    module E :
      sig
        type t = SVertex.t * SEdge.t * SVertex.t
        val compare : t -> t -> int
        type nonrec vertex = vertex
        val src : t -> vertex
        val dst : t -> vertex
        type label = SEdge.t
        val create : vertex -> label -> vertex -> t
        val label : t -> label
      end
    type edge = E.t
    val is_directed : bool
    val is_empty : t -> bool
    val nb_vertex : t -> int
    val nb_edges : t -> int
    val out_degree : t -> vertex -> int
    val in_degree : t -> vertex -> int
    val mem_vertex : t -> vertex -> bool
    val mem_edge : t -> vertex -> vertex -> bool
    val mem_edge_e : t -> edge -> bool
    val find_edge : t -> vertex -> vertex -> edge
    val find_all_edges : t -> vertex -> vertex -> edge list
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list
    val iter_vertex : (vertex -> unit) -> t -> unit
    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges : (vertex -> vertex -> unit) -> t -> unit
    val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges_e : (edge -> unit) -> t -> unit
    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    val map_vertex : (vertex -> vertex) -> t -> t
    val iter_succ : (vertex -> unit) -> t -> vertex -> unit
    val iter_pred : (vertex -> unit) -> t -> vertex -> unit
    val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
    val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
    val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val empty : t
    val add_vertex : t -> vertex -> t
    val remove_vertex : t -> vertex -> t
    val add_edge : t -> vertex -> vertex -> t
    val add_edge_e : t -> edge -> t
    val remove_edge : t -> vertex -> vertex -> t
    val remove_edge_e : t -> edge -> t
  end
module STopologicalTraversal :
  sig
    val fold : (SDependencies.V.t -> 'a -> 'a) -> SDependencies.t -> 'a -> 'a
    val iter : (SDependencies.V.t -> unit) -> SDependencies.t -> unit
  end
module SSCC :
  sig
    val scc : SDependencies.t -> int * (SDependencies.V.t -> int)
    val scc_array : SDependencies.t -> SDependencies.V.t list array
    val scc_list : SDependencies.t -> SDependencies.V.t list list
  end
val build_program_dep_graph : Ast.program -> SDependencies.t
val check_for_cycle_in_scope : SDependencies.t -> unit
val get_scope_ordering : SDependencies.t -> Ast.ScopeName.t list
module TVertex :
  sig
    type t =
        Struct of Ast.StructName.t
      | Enum of Ast.EnumName.t
    val hash : t -> int
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val format_t : Format.formatter -> t -> unit
    val get_info : t -> Ast.StructName.info
  end
module TVertexSet :
  sig
    type elt = TVertex.t
    type t = Stdlib__set.Make(TVertex).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
module TEdge :
  sig
    type t = Utils.Pos.t
    val compare : 'a -> 'a -> int
    val default : Utils.Pos.t
  end
module TDependencies :
  sig
    type t =
        Graph__Persistent.Digraph.ConcreteBidirectionalLabeled(TVertex)(TEdge).t
    module V :
      sig
        type t = TVertex.t
        val compare : t -> t -> int
        val hash : t -> int
        val equal : t -> t -> bool
        type label = TVertex.t
        val create : label -> t
        val label : t -> label
      end
    type vertex = V.t
    module E :
      sig
        type t = TVertex.t * TEdge.t * TVertex.t
        val compare : t -> t -> int
        type nonrec vertex = vertex
        val src : t -> vertex
        val dst : t -> vertex
        type label = TEdge.t
        val create : vertex -> label -> vertex -> t
        val label : t -> label
      end
    type edge = E.t
    val is_directed : bool
    val is_empty : t -> bool
    val nb_vertex : t -> int
    val nb_edges : t -> int
    val out_degree : t -> vertex -> int
    val in_degree : t -> vertex -> int
    val mem_vertex : t -> vertex -> bool
    val mem_edge : t -> vertex -> vertex -> bool
    val mem_edge_e : t -> edge -> bool
    val find_edge : t -> vertex -> vertex -> edge
    val find_all_edges : t -> vertex -> vertex -> edge list
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list
    val iter_vertex : (vertex -> unit) -> t -> unit
    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges : (vertex -> vertex -> unit) -> t -> unit
    val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges_e : (edge -> unit) -> t -> unit
    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    val map_vertex : (vertex -> vertex) -> t -> t
    val iter_succ : (vertex -> unit) -> t -> vertex -> unit
    val iter_pred : (vertex -> unit) -> t -> vertex -> unit
    val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
    val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
    val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val empty : t
    val add_vertex : t -> vertex -> t
    val remove_vertex : t -> vertex -> t
    val add_edge : t -> vertex -> vertex -> t
    val add_edge_e : t -> edge -> t
    val remove_edge : t -> vertex -> vertex -> t
    val remove_edge_e : t -> edge -> t
  end
module TTopologicalTraversal :
  sig
    val fold : (TDependencies.V.t -> 'a -> 'a) -> TDependencies.t -> 'a -> 'a
    val iter : (TDependencies.V.t -> unit) -> TDependencies.t -> unit
  end
module TSCC :
  sig
    val scc : TDependencies.t -> int * (TDependencies.V.t -> int)
    val scc_array : TDependencies.t -> TDependencies.V.t list array
    val scc_list : TDependencies.t -> TDependencies.V.t list list
  end
val get_structs_or_enums_in_type :
  Ast.typ Utils.Pos.marked -> TVertexSet.t
val build_type_graph :
  Ast.struct_ctx -> Ast.enum_ctx -> TDependencies.t
val check_type_cycles :
  Ast.struct_ctx -> Ast.enum_ctx -> TVertex.t list
