module JSON_Graph
    (V : sig
      include Graph.Sig.COMPARABLE

      val format : Format.formatter -> t -> unit
    end)
    (G : Graph.Sig.G with type V.t = V.t) =
struct
  let graph_to_json g =
    let nodes =
      G.fold_vertex
        (fun v acc ->
          (string_of_int (G.V.hash v), `String (Format.asprintf "%a" V.format v))
          :: acc)
        g []
    in
    let nodes = `Assoc nodes in
    let edges =
      G.fold_edges
        (fun v1 v2 acc ->
          `Assoc ["from", `Int (V.hash v1); "to", `Int (V.hash v2)] :: acc)
        g []
    in
    let edges = `List edges in
    `Assoc ["nodes", nodes; "edges", edges]
end
