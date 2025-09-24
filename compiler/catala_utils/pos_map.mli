type small_pos = { line : int; col : int }
type loc_interval = { start : small_pos; stop : small_pos }
type t

val empty : t

val export_reached : t -> loc_interval list File.Map.t
val export_reachable : t -> loc_interval list File.Map.t

val pp: Format.formatter -> t -> unit

val reachable : Pos.t -> t -> t
val pos : Pos.t -> t -> t
val neg : Pos.t -> t -> t

val fusion : t -> t -> t
val report_coverage: Format.formatter -> t -> unit
