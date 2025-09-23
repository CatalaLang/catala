type coverage =
  | Positive
  | Negative
  | Fulfilled

type t
val empty:t

val add: Pos.t -> coverage -> t -> t

val report_coverage: Format.formatter -> t -> unit
