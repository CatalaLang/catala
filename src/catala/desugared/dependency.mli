module Vertex :
  sig
    type t =
        Var of Scopelang.Ast.ScopeVar.t
      | SubScope of Scopelang.Ast.SubScopeName.t
    val hash : t -> int
    val compare : 'a -> 'a -> int
    val equal : t -> t -> bool
    val format_t : Format.formatter -> t -> unit
  end
module Edge :
  sig
    type t = Utils.Pos.t
    val compare : 'a -> 'a -> int
    val default : Utils.Pos.t
  end
module ScopeDependencies :
  sig
    type t =
        Graph__Persistent.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge).t
    module V :
      sig
        type t = Vertex.t
        val compare : t -> t -> int
        val hash : t -> int
        val equal : t -> t -> bool
        type label = Vertex.t
        val create : label -> t
        val label : t -> label
      end
    type vertex = V.t
    module E :
      sig
        type t = Vertex.t * Edge.t * Vertex.t
        val compare : t -> t -> int
        type nonrec vertex = vertex
        val src : t -> vertex
        val dst : t -> vertex
        type label = Edge.t
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
module TopologicalTraversal :
  sig
    val fold :
      (ScopeDependencies.V.t -> 'a -> 'a) -> ScopeDependencies.t -> 'a -> 'a
    val iter : (ScopeDependencies.V.t -> unit) -> ScopeDependencies.t -> unit
  end
module SCC :
  sig
    val scc : ScopeDependencies.t -> int * (ScopeDependencies.V.t -> int)
    val scc_array : ScopeDependencies.t -> ScopeDependencies.V.t list array
    val scc_list : ScopeDependencies.t -> ScopeDependencies.V.t list list
  end
val correct_computation_ordering : ScopeDependencies.t -> Vertex.t list
val check_for_cycle : Ast.scope -> ScopeDependencies.t -> unit
val build_scope_dependencies : Ast.scope -> ScopeDependencies.t
module ExceptionVertex :
  sig
    type t = Desugared__Ast.RuleName.t
    type info = Utils.Uid.MarkedString.info
    val fresh : info -> t
    val get_info : t -> info
    val compare : t -> t -> int
    val format_t : Format.formatter -> t -> unit
    val hash : t -> int
    val equal : t -> t -> bool
  end
module ExceptionsDependencies :
  sig
    type t =
        Graph__Persistent.Digraph.ConcreteBidirectionalLabeled(ExceptionVertex)(Edge).t
    module V :
      sig
        type t = ExceptionVertex.t
        val compare : t -> t -> int
        val hash : t -> int
        val equal : t -> t -> bool
        type label = ExceptionVertex.t
        val create : label -> t
        val label : t -> label
      end
    type vertex = V.t
    module E :
      sig
        type t = ExceptionVertex.t * Edge.t * ExceptionVertex.t
        val compare : t -> t -> int
        type nonrec vertex = vertex
        val src : t -> vertex
        val dst : t -> vertex
        type label = Edge.t
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
module ExceptionsSCC :
  sig
    val scc :
      ExceptionsDependencies.t -> int * (ExceptionsDependencies.V.t -> int)
    val scc_array :
      ExceptionsDependencies.t -> ExceptionsDependencies.V.t list array
    val scc_list :
      ExceptionsDependencies.t -> ExceptionsDependencies.V.t list list
  end
val build_exceptions_graph :
  Ast.rule Ast.RuleMap.t ->
  Ast.ScopeDef.t -> ExceptionsDependencies.t
val check_for_exception_cycle : ExceptionsDependencies.t -> unit
