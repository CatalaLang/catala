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

open Utils
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
end

module type BackendIO = sig
  val init_backend : unit -> unit

  type backend_context

  val make_context : decl_ctx -> backend_context

  type vc_encoding

  val translate_expr :
    backend_context -> typed Dcalc.Ast.expr -> backend_context * vc_encoding

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
        Format.asprintf "%s This variable might return an empty error:\n%s"
          (Cli.with_style [ANSITerminal.yellow] "[%s.%s]"
             (Format.asprintf "%a" ScopeName.format_t vc.vc_scope)
             (Bindlib.name_of (Marked.unmark vc.vc_variable)))
          (Pos.retrieve_loc_text (Marked.get_mark vc.vc_variable))
      | Conditions.NoOverlappingExceptions ->
        Format.asprintf
          "%s At least two exceptions overlap for this variable:\n%s"
          (Cli.with_style [ANSITerminal.yellow] "[%s.%s]"
             (Format.asprintf "%a" ScopeName.format_t vc.vc_scope)
             (Bindlib.name_of (Marked.unmark vc.vc_variable)))
          (Pos.retrieve_loc_text (Marked.get_mark vc.vc_variable))
    in
    let counterexample : string option =
      if !Cli.disable_counterexamples then
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
      (decl_ctx : decl_ctx)
      (vc : Conditions.verification_condition * vc_encoding_result) : bool =
    let vc, z3_vc = vc in

    Cli.debug_print "For this variable:\n%s\n"
      (Pos.retrieve_loc_text (Expr.pos vc.Conditions.vc_guard));
    Cli.debug_format "This verification condition was generated for %a:@\n%a"
      (Cli.format_with_style [ANSITerminal.yellow])
      (match vc.vc_kind with
      | Conditions.NoEmptyError ->
        "the variable definition never to return an empty error"
      | NoOverlappingExceptions -> "no two exceptions to ever overlap")
      (Expr.format decl_ctx) vc.vc_guard;

    match z3_vc with
    | Success (encoding, backend_ctx) -> (
      Cli.debug_print "The translation to Z3 is the following:\n%s"
        (B.print_encoding encoding);
      match B.solve_vc_encoding backend_ctx encoding with
      | ProvenTrue -> true
      | ProvenFalse model ->
        Cli.error_print "%s" (print_negative_result vc backend_ctx model);
        false
      | Unknown -> failwith "The solver failed at proving or disproving the VC")
    | Fail msg ->
      Cli.error_print "%s The translation to Z3 failed:\n%s"
        (Cli.with_style [ANSITerminal.yellow] "[%s.%s]"
           (Format.asprintf "%a" ScopeName.format_t vc.vc_scope)
           (Bindlib.name_of (Marked.unmark vc.vc_variable)))
        msg;
      false
end
