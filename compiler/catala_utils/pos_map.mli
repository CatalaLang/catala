type small_pos = { line : int; col : int }
type loc_interval = { start : small_pos; stop : small_pos }
type 'a gen
type simple = unit gen
type t = String.Set.t gen

val empty : 'a gen
val with_name : string -> simple -> t
val export_reached : t -> (loc_interval * String.Set.t) list File.Map.t
val export_reachable : t -> loc_interval list File.Map.t
val pp : Format.formatter -> 'a gen -> unit
val reachable : Pos.t -> simple -> simple
val pos : Pos.t -> simple -> simple
val neg : Pos.t -> simple -> simple
val fusion : t -> t -> t
val report_coverage : Format.formatter -> simple -> unit

type simple_coverage = Reach | Pos | Neg | Fulf

val add : Pos.t -> simple_coverage -> simple -> simple
val get_value_simple : simple -> Pos.t -> simple_coverage option
