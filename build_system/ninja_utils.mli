(** Expression containing variable references. *)
module Expr : sig
  type t =
    (* Sequence of expressions. *)
    | Seq of t list
    (* Literal string. *)
    | Lit of string
    (* Variable reference. *)
    | Var of string

  val to_string : t -> string
end

module Rule : sig
  type t = { name : string; command : Expr.t; description : Expr.t option }

  val make : string -> command:Expr.t -> description:Expr.t -> t
end

module RuleMap : Map.S with type key = String.t

type ninja = { rules : Rule.t RuleMap.t (* ; builds : Build.t BuildMap.t *) }

val empty : ninja

val write : out_channel -> ninja -> unit
