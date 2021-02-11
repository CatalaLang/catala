exception StructuredError of (string * (string option * Pos.t) list)
val print_structured_error :
  string -> (string option * Pos.t) list -> string
val raise_spanned_error : string -> ?span_msg:string -> Pos.t -> 'a
val raise_multispanned_error :
  string -> (string option * Pos.t) list -> 'a
val raise_error : string -> 'a
val print_multispanned_warning :
  string -> (string option * Pos.t) list -> unit
val print_spanned_warning : string -> ?span_msg:string -> Pos.t -> unit
val print_warning : string -> unit
