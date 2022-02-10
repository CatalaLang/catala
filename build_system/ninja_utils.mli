(* This file is part of the Catala build system, a specification language for tax and social
   benefits computation rules. Copyright (C) 2020 Inria, contributor: Emile Rolley
   <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** {1 Ninja expressions} *)

(** Helper module to build ninja expressions. *)
module Expr : sig
  (** Represents a ninja expression. Which could be either a literal, a variable references ($_) or
      a sequence of sub-expressions. *)
  type t =
    (* Sequence of sub-expressions. *)
    | Seq of t list
    (* Literal string. *)
    | Lit of string
    (* Variable reference. *)
    | Var of string

  val to_string : t -> string
  (** [to_string exp] returns the string representation of an ninja expression [exp]. *)

  val list_to_string : ?sep:string -> t list -> string
  (** [list_to_string ?sep ls] returns the string representation of a list [ls] of ninja expressions
      concatenated with the separator [sep] -- by default ' '. *)
end

(** {1 Ninja rules} *)

(** Helper module to build ninja rules. *)
module Rule : sig
  (* NOTE: is name is really needed despite the use of Map? *)
  type t = {
    (* Name of the rule: `rule <name>`. *)
    name : string;
    (* Command of the rule: ` command = <command>. *)
    command : Expr.t;
    (* Description of the rule: ` description = <description>. *)
    description : Expr.t option;
  }
  (** Represents the minimal ninja rule representation for clerk. *)

  val make : string -> command:Expr.t -> description:Expr.t -> t
  (** [make name ~command ~description] returns the corresponding ninja rule [Rule.t]. *)

  val to_string : t -> string
  (** [to_string rule] returns the string representation of the [rule]. *)
end

(** {1 Ninja builds} *)

(** Helper module to build ninja build declarations. *)
module Build : sig
  type t = {
    outputs : Expr.t list;
    (* NOTE: what's the difference between [Expr.t list] and [Expr.Seq]? => [Expr.Seq] is a unique
       expression with possible variable references => no space in its string representation. *)
    rule : string;
    inputs : Expr.t list option;
    vars : (string * Expr.t) list;
  }
  (** @note Currently, nothing nothing forces to build valid {!: Expr.t} for the variables. *)

  val make : outputs:Expr.t list -> rule:string -> t

  val make_with_vars : outputs:Expr.t list -> rule:string -> vars:(string * Expr.t) list -> t

  val make_with_inputs : outputs:Expr.t list -> rule:string -> inputs:Expr.t list -> t

  val empty : t
  (** [empty] is the minimal ninja build. *)

  val unpath : string -> string
  (** [unpath path] replaces all '/' occurences with '-' in [path] to avoid ninja writing the
      corresponding file. *)

  val to_string : t -> string
end

(** {1 Maps} *)

module RuleMap : Map.S with type key = String.t

module BuildMap : Map.S with type key = String.t

(** {1 Ninja} *)

type ninja = { rules : Rule.t RuleMap.t; builds : Build.t BuildMap.t }
(** Represents the minimal ninja architectures (list of rule and build declarations) needed for
    clerk. *)

val empty : ninja
(** [empty] returns the empty empty ninja structure. *)

val write : out_channel -> ninja -> unit
(** [write out ninja] writes in [out] the string representation of all [ninja.rules] and
    [ninja.builds]. *)
