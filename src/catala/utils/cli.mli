type frontend_lang = [ `En | `Fr | `NonVerbose ]
type backend_lang = [ `En | `Fr ]
val to_backend_lang : frontend_lang -> backend_lang
val source_files : string list ref
val locale_lang : backend_lang ref
val contents : string ref
val debug_flag : bool ref
val style_flag : bool ref
val max_prec_digits : int ref
val trace_flag : bool ref
val file : string Cmdliner.Term.t
val debug : bool Cmdliner.Term.t
val unstyled : bool Cmdliner.Term.t
val trace_opt : bool Cmdliner.Term.t
val wrap_weaved_output : bool Cmdliner.Term.t
val backend : string Cmdliner.Term.t
type backend_option = Latex | Makefile | Html | Run | OCaml
val language : string option Cmdliner.Term.t
val max_prec_digits_opt : int option Cmdliner.Term.t
val ex_scope : string option Cmdliner.Term.t
val output : string option Cmdliner.Term.t
val pygmentize_loc : string option Cmdliner.Term.t
val catala_t :
  (string ->
   bool ->
   bool ->
   bool ->
   string option ->
   string ->
   string option ->
   int option -> bool -> string option -> string option -> 'a) ->
  'a Cmdliner.Term.t
val version : string
val info : Cmdliner.Term.info
val print_with_style :
  ANSITerminal.style list -> ('a, unit, string) format -> 'a
val debug_marker : unit -> string
val error_marker : unit -> string
val warning_marker : unit -> string
val result_marker : unit -> string
val log_marker : unit -> string
val concat_with_line_depending_prefix_and_suffix :
  (int -> string) -> (int -> string) -> string list -> string
val add_prefix_to_each_line : string -> (int -> string) -> string
val debug_print : string -> unit
val error_print : string -> unit
val warning_print : string -> unit
val result_print : string -> unit
val log_print : string -> unit
