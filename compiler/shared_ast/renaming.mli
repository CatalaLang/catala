(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
open Definitions

type config = {
  reserved : string list;  (** Use for keywords and built-ins *)
  sanitize_varname : string -> string;  (** Typically String.to_snake_case *)
  reset_context_for_closed_terms : bool;  (** See [Bindlib.Renaming] *)
  skip_constant_binders : bool;  (** See [Bindlib.Renaming] *)
  constant_binder_name : string option;  (** See [Bindlib.Renaming] *)
}

type context

val default_config: config

val get_ctx : config -> context

val unbind_in :
  context ->
  ?fname:(string -> string) ->
  ('e, 'b) Bindlib.binder ->
  ('e, _) Mark.ed Var.t * 'b * context
(* [fname] applies a transformation on the variable name (typically something
   like [String.to_snake_case]). The result is advisory and a numerical suffix
   may be appended or modified *)

val unmbind_in :
  context ->
  ?fname:(string -> string) ->
  ('e, 'b) Bindlib.mbinder ->
  ('e, _) Mark.ed Var.t Array.t * 'b * context

val new_id : context -> string -> string * context

val reserve_name : context -> string -> context

val set_rewriters :
  ?scopes:(ScopeName.t -> ScopeName.t) ->
  ?topdefs:(TopdefName.t -> TopdefName.t) ->
  ?structs:(StructName.t -> StructName.t) ->
  ?fields:(StructField.t -> StructField.t) ->
  ?enums:(EnumName.t -> EnumName.t) ->
  ?constrs:(EnumConstructor.t -> EnumConstructor.t) ->
  context ->
  context

val typ : context -> typ -> typ

val expr : context -> ('a any, 'm) gexpr -> ('a, 'm) boxed_gexpr
(** Disambiguates all variable names in [e], and renames structs, fields, enums
    and constrs according to the given context configuration *)

val scope_name : context -> ScopeName.t -> ScopeName.t
val topdef_name : context -> TopdefName.t -> TopdefName.t
val struct_name : context -> StructName.t -> StructName.t
val enum_name : context -> EnumName.t -> EnumName.t

val code_items :
  context -> ((_ any, 'm) gexpr as 'e) code_item_list -> 'e code_item_list

type t
(** Enclosing of a polymorphic renaming function, to be used by [apply] *)

val apply : t -> 'e program -> 'e program * context

val program :
  reserved:string list ->
  reset_context_for_closed_terms:bool ->
  skip_constant_binders:bool ->
  constant_binder_name:string option ->
  namespaced_fields_constrs:bool ->
  ?f_var:(string -> string) ->
  ?f_struct:(string -> string) ->
  ?f_field:(string -> string) ->
  ?f_enum:(string -> string) ->
  ?f_constr:(string -> string) ->
  unit ->
  t
(** Renames all idents (variables, types, struct and enum names, fields and
    constructors) to dispel ambiguities in the target language. Names in
    [reserved], typically keywords and built-ins, will be avoided ; the meaning
    of the following three flags is described in [Bindlib.Renaming].

    if [namespaced_fields_constrs] is true, then struct fields and enum
    constructors can reuse names from other fields/constructors or other idents.

    The [f_*] optional arguments sanitize the different kinds of ids. The
    default is what is used for OCaml: project to ASCII, capitalise structs,
    enums (both modules in the backend) and constructors, lowercase fields, and
    rewrite variables to snake case.

    In the returned program, it is safe to directly use `Bindlib.name_of` on
    variables for printing. The same is true for `StructName.get_info` etc. *)
