(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria,
   contributors: Arthur Carcano <arthur.carcano@ocamlpro.com>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Extraction of the public interface of a module, with its docstrings.

    The interface is read from the [catala-metadata] blocks of the surface
    program, in source order. Types are kept as the author wrote them, but each
    named type is resolved to the declaration it refers to, so that it can be
    linked.

    {2 What is documented}

    - The module: its name, whether it is external, and the modules it uses.
    - The declarations of [catala-metadata] blocks: structures with their
      fields, enumerations with their cases, scopes with their [input],
      [context] and [output] variables and their [output] subscopes, constants
      and functions with their arguments, and external types.
    - The docstrings ([##] lines or [#[doc]] attributes) placed on these
      declarations and members, and on the arguments of functions, including
      function-typed fields and scope variables.

    {2 What is ignored}

    - Law text and headings.
    - Everything in code blocks other than [catala-metadata] blocks, docstrings
      included: private types and scopes, and the definitions of constants and
      functions declared by their signature in a metadata block. A warning is
      emitted when the public interface uses a private type or scope.
    - Scope implementations ([scope S:] blocks): their rules, definitions,
      assertions, and the docstrings on them.
    - Test scopes, [internal] scope variables, subscopes that are not [output],
      and the states of scope variables.
    - Docstrings anywhere else, such as on expressions, which the compiler
      reports.

    A constant or function declared twice in metadata blocks, by its signature
    and its definition, is documented by its first declaration there: docstrings
    on the second one are errors. This is specific to [catala doc], see the
    implementation. *)

open Shared_ast

type target = {
  target_module : ModuleName.t option;
      (** [None] when the type is declared in the documented module *)
  target_anchor : string;
}
(** The declaration a named type refers to *)

type typ =
  | Builtin of Surface.Ast.primitive_typ
      (** Never [Named]; [Var] is a type variable ([anything]) *)
  | Named of { name : string; target : target option }
      (** [name] is the type as written, including its module path *)
  | List of typ
  | Option of typ
  | Tuple of typ list

type base_typ = Condition | Data of typ
type arg = { arg_name : string; arg_typ : base_typ; arg_doc : string option }

type signature = {
  sig_typ : base_typ;  (** The return type for functions *)
  sig_args : arg list;  (** Empty if not a function *)
}

type 'a member = {
  member_name : string;
  member_decl : 'a;  (** What it declares, which depends on the kind of item *)
  member_doc : string option;
  member_anchor : string;  (** Under the anchor of its item *)
}
(** A member of an item: a field, a case, a scope variable or an argument *)

type scope_member =
  | Var of { input : Surface.Ast.io_input; output : bool; var_sig : signature }
      (** Scope variable *)
  | Subscope of { output : bool; scope : typ }

type item_kind =
  | Struct of signature member list  (** Fields *)
  | Enum of base_typ option member list
      (** Cases, with the type they carry, if any *)
  | Scope of scope_member member list
      (** Variables and subscopes, except internal ones *)
  | Constant of base_typ
  | Function of { return_typ : base_typ; args : base_typ member list }
  | Abstract  (** External type *)

type item = {
  item_name : string;
  item_kind : item_kind;
  item_doc : string option;
  item_anchor : string;
}

type module_use = { use_module : ModuleName.t; use_alias : string }

type module_doc = {
  module_name : string;
  module_is_external : bool;
  module_uses : module_use list;
  module_items : item list;
}

val extract :
  Surface.Ast.program -> Desugared.Name_resolution.context -> module_doc
(** See the module documentation for what is kept. Raises a user error if the
    program is not a module, or is not written in English, and reports misplaced
    docstrings as errors. *)
