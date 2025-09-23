type coverage = Positive | Negative | Fulfilled
type small_pos = { line : int; col : int }
type loc_interval = { start : small_pos; stop : small_pos }

let from_pos p =
  {
    start = { line = Pos.get_start_line p; col = Pos.get_start_column p };
    stop = { line = Pos.get_end_line p; col = Pos.get_end_column p };
  }

module Filemap = Stdlib.Map.Make (String)

module Interval_map = Stdlib.Map.Make (struct
  type t = loc_interval

  let compare x y = compare x.start y.start
end)

type t = coverage Interval_map.t Filemap.t

let empty = Filemap.empty
let export m = Filemap.map Interval_map.bindings m

let merge ~inside x y =
  match x, y with
  | Fulfilled, _ -> Fulfilled
  | Positive, x -> x
  | Negative, (Positive | Fulfilled) -> if inside then Fulfilled else Negative
  | Negative, Negative -> Negative

let pp_pos ppf p =
  if p.start.line = p.stop.line then
    Format.fprintf ppf "%d:%d-%d" p.start.line p.start.col p.stop.col
  else
    Format.fprintf ppf "%d:%d-%d:%d" p.start.line p.start.col p.stop.line
      p.stop.col

let pp_interval ppf (pos, x) =
  match x with
  | Positive -> Format.fprintf ppf "@{<green>%a@}" pp_pos pos
  | Negative -> Format.fprintf ppf "@{<red>%a@}" pp_pos pos
  | Fulfilled -> Format.fprintf ppf "@{<blue>%a@}" pp_pos pos

let pp_map ppf m =
  let pp_sep = Format.pp_print_cut in
  Format.pp_print_seq ~pp_sep pp_interval ppf (Interval_map.to_seq m)

let pp_file ppf f m =
  Format.fprintf ppf "@[<v>--------------@,%s:@,--------------@,%a@]@." f pp_map
    m

let rec add_interval ~inside i v pos_map =
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

let _pp_filemap ppf f = Filemap.iter (pp_file ppf) f

let fusion x y =
  Filemap.union
    (fun _s l r -> Some (Interval_map.fold (add_interval ~inside:true) r l))
    x y

let add pos v map =
  if pos = Pos.void then map
  else
    let loc = from_pos pos in
    let name = Pos.get_file pos in
    match Filemap.find_opt name map with
    | None -> Filemap.add name (Interval_map.singleton loc v) map
    | Some f ->
      let f' = add_interval ~inside:true loc v f in
      Filemap.add name f' map

let report_coverage ppf map =
  Hex.pp ppf (Hex.of_string (Marshal.to_string map []))
