type origin = String.Set.t
type 'a coverage = Reachable | Positive of 'a | Negative | Fulfilled of 'a
type small_pos = { line : int; col : int }
type loc_interval = { start : small_pos; stop : small_pos }

let from_pos p =
  {
    start = { line = Pos.get_start_line p; col = Pos.get_start_column p };
    stop = { line = Pos.get_end_line p; col = Pos.get_end_column p };
  }

module Interval_map = Stdlib.Map.Make (struct
  type t = loc_interval

  let compare x y = compare x.start y.start
end)

type 'a gen = 'a coverage Interval_map.t File.Map.t
type simple = unit gen
type t = origin gen

let empty = File.Map.empty

let with_name name fm =
  let add_name o = function
    | (Reachable | Negative) as x -> x
    | Positive () -> Positive (String.Set.singleton o)
    | Fulfilled () -> Fulfilled (String.Set.singleton o)
  in
  File.Map.map (Interval_map.map (add_name name)) fm


type 'a origin_ops = {
  merge: 'a -> 'a -> 'a;
  eq: 'a -> 'a -> bool;
}

let c_equal ops x y = match x, y with
    | Fulfilled x, Fulfilled y -> ops.eq x y
    | Positive x, Positive y -> ops.eq x y
    | Negative, Negative | Reachable, Reachable -> true
    | _, _ -> false

let merge ~ops ~inside x y =
  match x, y with
  | Fulfilled set, (Fulfilled set' | Positive set') ->
    Fulfilled (ops.merge set set')
  | Fulfilled set, (Negative | Reachable) -> Fulfilled set
  | Positive set, Positive set' -> Positive (ops.merge set set')
  | Positive _, Negative -> Negative
  | Positive _, Fulfilled set -> (* should not happen *) Fulfilled set
  | x, Reachable | Reachable, x -> x
  | Negative, (Positive set | Fulfilled set) ->
    if inside then Fulfilled set else Negative
  | Negative, Negative -> Negative

let pp_pos ppf p =
  if p.start.line = p.stop.line then
    Format.fprintf ppf "%d:%d-%d" p.start.line p.start.col p.stop.col
  else
    Format.fprintf ppf "%d:%d-%d:%d" p.start.line p.start.col p.stop.line
      p.stop.col

let pp_interval ppf (pos, x) =
  match x with
  | Reachable -> Format.fprintf ppf "@{<grey>%a@}" pp_pos pos
  | Positive _ -> Format.fprintf ppf "@{<green>%a@}" pp_pos pos
  | Negative -> Format.fprintf ppf "@{<red>%a@}" pp_pos pos
  | Fulfilled _ -> Format.fprintf ppf "@{<blue>%a@}" pp_pos pos

let pp_map ppf m =
  let pp_sep = Format.pp_print_cut in
  Format.pp_print_seq ~pp_sep pp_interval ppf (Interval_map.to_seq m)

let pp_file ppf f m =
  Format.fprintf ppf "@[<v>--------------@,%s:@,--------------@,%a@]@." f pp_map
    m



let rec add_interval ~ops ~inside i v pos_map =
  let add_interval = add_interval ~ops in
  let merge = merge ~ops in
  match Interval_map.find_first_opt (fun mi -> mi.stop > i.start) pos_map with
  | None -> Interval_map.add i v pos_map
  | Some (mi, prev) ->
    if i = mi then Interval_map.add i (merge ~inside prev v) pos_map
    else if i.start <= mi.start then begin
      (*xxxxx|------------| |--???? *)
      if i.stop = mi.start && c_equal ops prev v then
        pos_map
        |> Interval_map.remove mi
        |> Interval_map.add { i with stop = mi.stop } v
      else if i.stop <= mi.start then Interval_map.add i v pos_map
      else if i.stop < mi.stop then
        (* xxx|------------| |------| *)
        pos_map
        |> Interval_map.remove mi
        |> Interval_map.add { mi with stop = i.stop }
             (merge ~inside:false prev v)
        |> Interval_map.add { start = i.stop; stop = mi.stop } prev
        |> add_interval ~inside:false { i with stop = mi.start } v
      else
        (* xxxxx|------------| |------------------| *)
        pos_map
        |> Interval_map.add mi (merge ~inside:false prev v)
        |> (if mi.start > i.start then
              add_interval ~inside:false { i with stop = mi.start } v
            else Fun.id)
        |> add_interval ~inside:false { start = mi.stop; stop = i.stop } v
        (* |------------| |----------| *)
    end
    else if mi.stop = i.start && c_equal ops prev v then
      pos_map
      |> Interval_map.remove mi
      |> Interval_map.add { mi with stop = i.stop } v
      (* |------------------| |---| *)
    else if i.stop <= mi.stop && merge ~inside prev v = prev then pos_map
    else
      (* |------------| |---???? *)
      let pos_map =
        pos_map
        |> Interval_map.remove mi
        |> Interval_map.add { mi with stop = i.start } prev
      in
      (* |------------| |----------| *)
      if i.stop > mi.stop then
        pos_map
        |> Interval_map.add { i with stop = mi.stop }
             (merge ~inside:false prev v)
        |> add_interval ~inside:false { start = mi.stop; stop = i.stop } v
        (* |------------------| |---| *)
      else
        let pos_map = Interval_map.add i (merge ~inside prev v) pos_map in
        if i.stop = mi.stop then pos_map
        else Interval_map.add { start = i.stop; stop = mi.stop } prev pos_map

let pp ppf f = File.Map.iter (pp_file ppf) f

let set_ops = {
  merge = String.Set.union;
  eq = String.Set.equal;
}

let unit_ops = {
  merge = (fun () () -> ());
  eq = (fun () () -> true);
}

let fusion x y =
  File.Map.union
    (fun _s l r ->
      Some
        (Interval_map.fold
           (add_interval ~ops:set_ops ~inside:true)
           r l))
    x y

let add pos v map =
  if pos = Pos.void then map
  else
    let loc = from_pos pos in
    let name = Pos.get_file pos in
    match File.Map.find_opt name map with
    | None -> File.Map.add name (Interval_map.singleton loc v) map
    | Some f ->
      let f' = add_interval ~ops:unit_ops ~inside:true loc v f in
      File.Map.add name f' map

let pos pos map = add pos (Positive ()) map
let neg pos map = add pos Negative map
let reachable pos map = add pos Reachable map

let flatten_pol loc c map =
  match c with
  | Positive set | Fulfilled set ->
    add_interval ~ops:set_ops ~inside:true loc (Positive set) map
  | Negative | Reachable ->
    add_interval ~ops:set_ops ~inside:true loc Negative map


let simplify_map m =
  let reached_set (loc,c) = loc, match c with
    | Positive set | Fulfilled set ->  set
    | Negative | Reachable -> String.Set.empty in
  List.of_seq
  @@ Seq.map reached_set
  @@ Interval_map.to_seq
  @@ Interval_map.fold flatten_pol m Interval_map.empty

let export_reached fm = File.Map.map simplify_map fm

let flatten_reachable loc _ map =
  add_interval ~ops:set_ops ~inside:true loc Reachable map

let reachable_map m =
  List.of_seq
  @@ Seq.map fst
  @@ Interval_map.to_seq
  @@ Interval_map.fold flatten_reachable m Interval_map.empty

let export_reachable fm = File.Map.map reachable_map fm

let report_coverage ppf map =
  Hex.pp ppf (Hex.of_string (Marshal.to_string map []))
