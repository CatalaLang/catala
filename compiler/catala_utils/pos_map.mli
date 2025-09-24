type coverage =
  | Reachable | Positive | Negative | Fulfilled
type small_pos = { line : int; col : int }
type loc_interval = { start : small_pos; stop : small_pos }

type t

val empty : t
val export : t -> (loc_interval * coverage) list File.Map.t
val pp: Format.formatter -> t -> unit

val add : Pos.t -> coverage -> t -> t
val fusion : t -> t -> t
val report_coverage : Format.formatter -> t -> unit
