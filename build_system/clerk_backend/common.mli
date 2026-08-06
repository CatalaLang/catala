(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

module type S = Sig.S

type t = (module S)

(** Functions useful for the backend rules definitions. [~name] is for the
    backend name here, for convenience *)

val static_base_rules : Ninja_utils.def list

val extern_src :
  filename:string ->
  name:string ->
  ext:string ->
  missing:string list ->
  string * string list
(** The [missing] argument is simply an accumulator, returned as snd with any
    missing files added *)

(** We use 3 types of pseudo-targets for compiled objets. The <modname> below is
    the normalised module name (using String.to_id)
    - @catala-obj/<modname> ([catala_obj_target]) is what is needed for Catala
      evaluation (an OCaml cmxs)
    - @<backend>/interface/<modname> ([module_target]) is the possibly compiled
      interface that dependent modules will need to be compiled (e,g, .h, .cmi)
    - @<backend>/obj/<modname> or @<backend>/obj/<filename> ([obj_target]) is
      the compiled object for linking, including all its dependencies. This
      allows transitive compilation of required objects before linking *)

val catala_obj_target : string -> Ninja_utils.Expr.elt
(** From a module name, gives the pseudo-target that builds the Catala object
    required for interpretation (i.e. actually the OCaml dynlink object, .cmxs
    or .cmo) *)

val target : ?name:string -> string -> Ninja_utils.Expr.elt

val interface_dep : name:string -> string -> Ninja_utils.Expr.elt
(** backend name, module name -> target name *)

val src_dep : name:string -> string -> Ninja_utils.Expr.elt
(** backend name, module name -> target name *)

val obj_dep : name:string -> Clerk_utils.Scan.item -> Ninja_utils.Expr.elt
(** backend name, source item -> target name *)

module Make_backend : functor (A : Sig.Spec) -> Sig.S

val register : t -> unit
val get : Clerk_config.backend -> t
val all : unit -> t list
val name : t -> string
val id : t -> Clerk_config.backend
