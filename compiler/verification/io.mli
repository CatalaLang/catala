(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2022 Inria, contributor: Aymeric Fromherz
   <aymeric.fromherz@inria.fr>, Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

(** Common code for handling the IO of all proof backends supported *)

module type Backend = sig
  type backend_context

  type vc_encoding

  val print_encoding : vc_encoding -> string

  type model

  type solver_result = ProvenTrue | ProvenFalse of model option | Unknown

  val solve_vc_encoding : backend_context -> vc_encoding -> solver_result

  val print_model : backend_context -> model -> string

  val is_model_empty : model -> bool
end

module SolverIO : functor (B : Backend) -> sig
  type vc_encoding_result = Success of B.vc_encoding * B.backend_context | Fail of string

  val print_positive_result : Conditions.verification_condition -> string

  val print_negative_result :
    Conditions.verification_condition -> B.backend_context -> B.model option -> string

  val encode_and_check_vc :
    Dcalc.Ast.decl_ctx -> Conditions.verification_condition * vc_encoding_result -> unit
end
