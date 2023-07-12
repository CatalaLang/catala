(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
   Aymeric Fromherz <aymeric.fromherz@inria.fr>, Denis Merigoux
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

open Catala_utils
open Shared_ast

module type Backend = sig
  val init_backend : unit -> unit

  type backend_context

  val make_context : decl_ctx -> backend_context

  type vc_encoding

  val print_encoding : vc_encoding -> string

  type model
  type solver_result = ProvenTrue | ProvenFalse of model option | Unknown

  val solve_vc_encoding : backend_context -> vc_encoding -> solver_result
  val print_model : backend_context -> model -> string
  val is_model_empty : model -> bool

  val translate_expr :
    backend_context -> typed Dcalc.Ast.expr -> backend_context * vc_encoding

  val encode_asserts :
    backend_context -> typed Dcalc.Ast.expr -> backend_context
end

module type BackendIO = sig
  val init_backend : unit -> unit

  type backend_context

  val make_context : decl_ctx -> backend_context

  type vc_encoding

  val translate_expr :
    backend_context -> typed Dcalc.Ast.expr -> backend_context * vc_encoding

  val encode_asserts :
    backend_context -> typed Dcalc.Ast.expr -> backend_context

  type model

  type vc_encoding_result =
    | Success of vc_encoding * backend_context
    | Fail of string

  val print_negative_result :
    Conditions.verification_condition ->
    backend_context ->
    model option ->
    string

  val encode_and_check_vc :
    decl_ctx -> Conditions.verification_condition * vc_encoding_result -> bool
end

module MakeBackendIO (B : Backend) = struct
  let init_backend = B.init_backend

  type backend_context = B.backend_context

  let make_context = B.make_context

  type vc_encoding = B.vc_encoding

  let translate_expr = B.translate_expr
  let encode_asserts = B.encode_asserts

  type model = B.model

  type vc_encoding_result =
    | Success of B.vc_encoding * B.backend_context
    | Fail of string

  let print_negative_result
      (vc : Conditions.verification_condition)
      (ctx : B.backend_context)
      (model : B.model option) : string =
    let var_and_pos =
      match vc.Conditions.vc_kind with
      | Conditions.NoEmptyError ->
        Format.asprintf
          "@[<v>@{<yellow>[%a.%s]@} This variable might return an empty error:@,\
           %a@]"
          ScopeName.format vc.vc_scope
          (Bindlib.name_of (Mark.remove vc.vc_variable))
          Pos.format_loc_text (Mark.get vc.vc_variable)
      | Conditions.NoOverlappingExceptions ->
        Format.asprintf
          "@[<v>@{<yellow>[%a.%s]@} At least two exceptions overlap for this \
           variable:@,\
           %a@]"
          ScopeName.format vc.vc_scope
          (Bindlib.name_of (Mark.remove vc.vc_variable))
          Pos.format_loc_text (Mark.get vc.vc_variable)
    in
    let counterexample : string option =
      if Globals.disable_counterexamples () then
        Some "Counterexample generation is disabled so none was generated."
      else
        match model with
        | None ->
          Some
            "The solver did not manage to generate a counterexample to explain \
             the faulty behavior."
        | Some model ->
          if B.is_model_empty model then None
          else
            Some
              (Format.asprintf
                 "The solver generated the following counterexample to explain \
                  the faulty behavior:\n\
                  %s"
                 (B.print_model ctx model))
    in
    var_and_pos
    ^
    match counterexample with
    | None -> ""
    | Some counterexample -> "\n" ^ counterexample

  let encode_and_check_vc
      (_decl_ctx : decl_ctx)
      (vc : Conditions.verification_condition * vc_encoding_result) : bool =
    let vc, z3_vc = vc in

    Message.emit_debug "@[<v>For this variable:@,%a@,@]" Pos.format_loc_text
      (Expr.pos vc.Conditions.vc_guard);
    Message.emit_debug
      "@[<v>This verification condition was generated for @{<yellow>%s@}:@,\
       %a@,\
       with assertions:@,\
       %a@]"
      (match vc.vc_kind with
      | Conditions.NoEmptyError ->
        "the variable definition never to return an empty error"
      | NoOverlappingExceptions -> "no two exceptions to ever overlap")
      (Print.expr ()) vc.vc_guard (Print.expr ()) vc.vc_asserts;

    match z3_vc with
    | Success (encoding, backend_ctx) -> (
      Message.emit_debug "@[<v>The translation to Z3 is the following:@,%s@]"
        (B.print_encoding encoding);
      match B.solve_vc_encoding backend_ctx encoding with
      | ProvenTrue -> true
      | ProvenFalse model ->
        Message.emit_warning "%s" (print_negative_result vc backend_ctx model);
        false
      | Unknown -> failwith "The solver failed at proving or disproving the VC")
    | Fail msg ->
      Message.emit_warning
        "@[<v>@{<yellow>[%a.%s]@} The translation to Z3 failed:@,%s@]"
        ScopeName.format vc.vc_scope
        (Bindlib.name_of (Mark.remove vc.vc_variable))
        msg;
      false
end
