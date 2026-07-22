open Shared_ast

val init : int option -> unit

type ('e, 'c, 't) mutation_type =
  ((yes, 'c, yes) interpr_kind, 't) boxed_gexpr ->
  ((yes, 'c, yes) interpr_kind, 't) boxed_gexpr

val remove_excepts_n : int ref
val remove_excepts : float -> ('e, 'c, 't) mutation_type
val duplicate_excepts_n : int ref
val duplicate_excepts : ('e, 'c, 't) mutation_type
val negate_justs_n : int ref
val negate_justs : ('e, 'c, 't) mutation_type

val apply_mutations :
  (('e, 'c, 't) mutation_type * float) list ->
  ((yes, 'c, yes) interpr_kind, 't) gexpr ->
  ((yes, 'c, yes) interpr_kind, 't) gexpr boxed

type ast_stats_t = {
  mutable defaults : int;
  mutable defaults_with_excepts : int;
  mutable excepts_sizes : int list;
  mutable ifs : int;
  mutable asserts : int;
  mutable matches : int;
  mutable match_max_arms : int;
}

val get_stats : ((yes, no, yes) interpr_kind, 't) gexpr -> ast_stats_t
val pprint_ast_stats : Format.formatter -> ast_stats_t -> unit

val create_one_conflict :
  ((yes, no, yes) interpr_kind, 't) gexpr ->
  ((yes, no, yes) interpr_kind, 't) gexpr boxed
