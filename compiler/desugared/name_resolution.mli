(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Nicolas Chataing <nicolas.chataing@ens.fr> Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Builds a context that allows for mapping each name to a precise uid, taking
    lexical scopes into account *)

open Catala_utils
open Shared_ast

(** {1 Name resolution context} *)

type unique_rulename = Ambiguous of Pos.t list | Unique of RuleName.t Mark.pos

type scope_def_context = {
  default_exception_rulename : unique_rulename option;
  label_idmap : LabelName.t Ident.Map.t;
}

type scope_var_or_subscope =
  | ScopeVar of ScopeVar.t
  | SubScope of SubScopeName.t * ScopeName.t

type scope_context = {
  var_idmap : scope_var_or_subscope Ident.Map.t;
      (** All variables, including scope variables and subscopes *)
  scope_defs_contexts : scope_def_context Ast.ScopeDef.Map.t;
      (** What is the default rule to refer to for unnamed exceptions, if any *)
  sub_scopes : ScopeName.Set.t;
      (** Other scopes referred to by this scope. Used for dependency analysis *)
}
(** Inside a scope, we distinguish between the variables and the subscopes. *)

type struct_context = typ StructField.Map.t
(** Types of the fields of a struct *)

type enum_context = typ EnumConstructor.Map.t
(** Types of the payloads of the cases of an enum *)

type var_sig = {
  var_sig_typ : typ;
  var_sig_is_condition : bool;
  var_sig_parameters :
    (Uid.MarkedString.info * Shared_ast.typ) list Mark.pos option;
  var_sig_io : Surface.Ast.scope_decl_context_io;
  var_sig_states_idmap : StateName.t Ident.Map.t;
  var_sig_states_list : StateName.t list;
}

(** Capitalised type names share a namespace on the user side, but may
    correspond to only one of the following *)
type typedef =
  | TStruct of StructName.t
  | TEnum of EnumName.t
  | TScope of ScopeName.t * scope_info  (** Implicitly defined output struct *)

type context = {
  path : ModuleName.t list;
      (** The current path being processed. Used for generating the Uids. *)
  typedefs : typedef Ident.Map.t;
      (** Gathers the names of the scopes, structs and enums *)
  field_idmap : StructField.t StructName.Map.t Ident.Map.t;
      (** The names of the struct fields. Names of fields can be shared between
          different structs *)
  constructor_idmap : EnumConstructor.t EnumName.Map.t Ident.Map.t;
      (** The names of the enum constructors. Constructor names can be shared
          between different enums *)
  scopes : scope_context ScopeName.Map.t;  (** For each scope, its context *)
  topdefs : TopdefName.t Ident.Map.t;  (** Global definitions *)
  topdef_types : typ TopdefName.Map.t;
      (** Types associated with the global definitions *)
  structs : struct_context StructName.Map.t;
      (** For each struct, its context *)
  enums : enum_context EnumName.Map.t;  (** For each enum, its context *)
  var_typs : var_sig ScopeVar.Map.t;
      (** The signatures of each scope variable declared *)
  modules : context ModuleName.Map.t;
}
(** Main context used throughout {!module: Desugared.From_surface} *)

(** {1 Helpers} *)

val raise_unsupported_feature : string -> Pos.t -> 'a
(** Temporary function raising an error message saying that a feature is not
    supported yet *)

val raise_unknown_identifier : string -> Ident.t Mark.pos -> 'a
(** Function to call whenever an identifier used somewhere has not been declared
    in the program previously *)

val get_var_typ : context -> ScopeVar.t -> typ
(** Gets the type associated to an uid *)

val is_var_cond : context -> ScopeVar.t -> bool
val get_var_io : context -> ScopeVar.t -> Surface.Ast.scope_decl_context_io

val get_scope_context : context -> ScopeName.t -> scope_context
(** Get the corresponding scope context from the context, looking up into nested
    submodules as necessary, following the path information in the scope name *)

val get_var_uid : ScopeName.t -> context -> Ident.t Mark.pos -> ScopeVar.t
(** Get the variable uid inside the scope given in argument *)

val get_subscope_uid :
  ScopeName.t -> context -> Ident.t Mark.pos -> SubScopeName.t
(** Get the subscope uid inside the scope given in argument *)

val is_subscope_uid : ScopeName.t -> context -> Ident.t -> bool
(** [is_subscope_uid scope_uid ctxt y] returns true if [y] belongs to the
    subscopes of [scope_uid]. *)

val belongs_to : context -> ScopeVar.t -> ScopeName.t -> bool
(** Checks if the var_uid belongs to the scope scope_uid *)

val get_def_typ : context -> Ast.ScopeDef.t -> typ
(** Retrieves the type of a scope definition from the context *)

val get_params :
  context ->
  Ast.ScopeDef.t ->
  (Uid.MarkedString.info * typ) list Mark.pos option

val is_def_cond : context -> Ast.ScopeDef.t -> bool
val is_type_cond : Surface.Ast.typ -> bool

val get_def_key :
  Surface.Ast.scope_var ->
  Surface.Ast.lident Mark.pos option ->
  ScopeName.t ->
  context ->
  Pos.t ->
  Ast.ScopeDef.t
(** Usage: [get_def_key var_name var_state scope_uid ctxt pos]*)

val get_enum : context -> Ident.t Mark.pos -> EnumName.t
(** Find an enum definition from the typedefs, failing if there is none or it
    has a different kind *)

val get_struct : context -> Ident.t Mark.pos -> StructName.t
(** Find a struct definition from the typedefs (possibly an implicit output
    struct from a scope), failing if there is none or it has a different kind *)

val get_scope : context -> Ident.t Mark.pos -> ScopeName.t
(** Find a scope definition from the typedefs, failing if there is none or it
    has a different kind *)

val module_ctx : context -> Surface.Ast.path -> context
(** Returns the context corresponding to the given module path; raises a user
    error if the module is not found *)

val process_type : context -> Surface.Ast.typ -> typ
(** Convert a surface base type to an AST type *)

(** {1 API} *)

val form_context : Surface.Ast.program -> context
(** Derive the context from metadata, in one pass over the declarations *)
