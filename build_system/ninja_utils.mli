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

  val list_to_string : ?sep:string -> t list -> string
end

module VarMap : Map.S with type key = String.t

module Rule : sig
  type t = { name : string; command : Expr.t; description : Expr.t option }

  val make : string -> command:Expr.t -> description:Expr.t -> t
end

module Build : sig
  type t = {
    outputs : Expr.t list;
    rule : string;
    inputs : Expr.t list option;
    vars : (string * Expr.t) list;
  }
  (** @note Currently, nothing nothing forces to build valid {!: Expr.t} for the variables. *)

  val make : outputs:Expr.t list -> rule:string -> t

  val make_with_vars : outputs:Expr.t list -> rule:string -> vars:(string * Expr.t) list -> t

  val make_with_inputs : outputs:Expr.t list -> rule:string -> inputs:Expr.t list -> t

  val unpath : string -> string
  (** [unpath path] replaces all '/' occurences with '-' in [path] to avoid ninja writing the
      corresponding file. *)

  val to_string : t -> string
end

module RuleMap : Map.S with type key = String.t

module BuildMap : Map.S with type key = String.t

type ninja = { rules : Rule.t RuleMap.t; builds : Build.t BuildMap.t }

val empty : ninja

val write : out_channel -> ninja -> unit
