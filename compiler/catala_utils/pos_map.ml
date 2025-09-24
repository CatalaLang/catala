type origin = String.Set.t

type 'a coverage =
  | Reachable
  | Positive of 'a
  | Negative
  | Fulfilled of 'a
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
    | Reachable | Negative as x -> x
    | Positive () -> Positive (String.Set.singleton o)
    | Fulfilled () -> Fulfilled (String.Set.singleton o)
  in
  File.Map.map (Interval_map.map (add_name name)) fm


let export_reachable m =
  let only_reachable (x,c) = match c with
    | Reachable -> Some x
    | _ -> None
  in
  File.Map.map (fun x -> List.of_seq @@ Seq.filter_map only_reachable @@ Interval_map.to_seq x) m


let merge ~omerge ~inside x y =
  match x, y with
  | Fulfilled set, (Fulfilled set'|Positive set') -> Fulfilled (omerge set set')
  | Fulfilled set, (Negative|Reachable) -> Fulfilled set
  | Positive set, Positive set' -> Positive (omerge set set')
  | Positive _ , Negative -> Negative
  | Positive _, Fulfilled set -> (* should not happen *) Fulfilled set
  | x, Reachable | Reachable, x -> x
  | Negative, (Positive set|Fulfilled set) -> if inside then Fulfilled set else Negative
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

let rec add_interval ~omerge ~inside i v pos_map =
  let add_interval = add_interval ~omerge in
  let merge = merge ~omerge in
  match Interval_map.find_first_opt (fun mi -> mi.stop > i.start) pos_map with
  | None -> Interval_map.add i v pos_map
  | Some (mi, prev) ->
    if i = mi then Interval_map.add i (merge ~inside prev v) pos_map
    else if i.start <= mi.start then begin
      (*xxxxx|------------| |--???? *)
      if i.stop = mi.start && prev = v then
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
    else if mi.stop = i.start && prev = v then
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



let fusion x y =
  File.Map.union
    (fun _s l r -> Some (Interval_map.fold (add_interval ~omerge:String.Set.union ~inside:true) r l))
    x y

let add pos v map =
  let omerge () () = () in
  if pos = Pos.void then map
  else
    let loc = from_pos pos in
    let name = Pos.get_file pos in
    match File.Map.find_opt name map with
    | None -> File.Map.add name (Interval_map.singleton loc v) map
    | Some f ->
      let f' = add_interval ~omerge ~inside:true loc v f in
      File.Map.add name f' map

let pos pos map = add pos (Positive ()) map
let neg pos map = add pos Negative map
let reachable pos map = add pos Reachable map


let flatten_pos loc c map =
  match c with
  | Positive set | Fulfilled set ->
    add_interval ~omerge:String.Set.union ~inside:true loc (Positive set) map
  | Negative | Reachable -> map

let simplify_map m =
  let only_pos = function
    | (loc,Positive set) -> Some (loc,set)
    | _ -> None
  in
  List.of_seq
  @@ Seq.filter_map only_pos
  @@ Interval_map.to_seq
  @@ Interval_map.fold flatten_pos m Interval_map.empty

let export_reached fm =
  File.Map.map simplify_map fm

let report_coverage ppf map =
  Hex.pp ppf (Hex.of_string (Marshal.to_string map []))
