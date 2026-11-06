module JSON_Graph
    (V : sig
      include Graph.Sig.COMPARABLE

      val format : Format.formatter -> t -> unit
    end)
    (G : Graph.Sig.G with type V.t = V.t) =
struct
  module H = Hashtbl.Make (V)

  module Edges = Set.Make (struct
    type t = int * int

    let compare = compare
  end)

  let graph_to_json g =
    let ids = H.create (G.nb_vertex g) in
    let nodes =
      G.fold_vertex
        (fun v acc ->
          let id = H.length ids in
          H.add ids v id;
          (string_of_int id, `String (Format.asprintf "%a" V.format v)) :: acc)
        g []
    in
    let edges =
      G.fold_edges
        (fun v1 v2 acc -> Edges.add (H.find ids v1, H.find ids v2) acc)
        g Edges.empty
      |> Edges.elements
      |> List.map (fun (from, to_) ->
          `Assoc ["from", `Int from; "to", `Int to_])
    in
    `Assoc ["nodes", `Assoc nodes; "edges", `List edges]
end
