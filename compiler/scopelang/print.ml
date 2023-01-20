(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

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
open Ast

let struc
    ctx
    (fmt : Format.formatter)
    (name : StructName.t)
    (fields : typ StructField.Map.t) : unit =
  Format.fprintf fmt "%a %a %a %a@\n@[<hov 2>  %a@]@\n%a" Print.keyword "struct"
    StructName.format_t name Print.punctuation "=" Print.punctuation "{"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (field_name, typ) ->
         Format.fprintf fmt "%a%a %a" StructField.format_t field_name
           Print.punctuation ":" (Print.typ ctx) typ))
    (StructField.Map.bindings fields)
    Print.punctuation "}"

let enum
    ctx
    (fmt : Format.formatter)
    (name : EnumName.t)
    (cases : typ EnumConstructor.Map.t) : unit =
  Format.fprintf fmt "%a %a %a @\n@[<hov 2>  %a@]" Print.keyword "enum"
    EnumName.format_t name Print.punctuation "="
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (field_name, typ) ->
         Format.fprintf fmt "%a %a%a %a" Print.punctuation "|"
           EnumConstructor.format_t field_name Print.punctuation ":"
           (Print.typ ctx) typ))
    (EnumConstructor.Map.bindings cases)

let scope ?(debug = false) ctx fmt (name, decl) =
  Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@]@\n@[<v 2>  %a@]"
    Print.keyword "let" Print.keyword "scope" ScopeName.format_t name
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       (fun fmt (scope_var, (typ, vis)) ->
         Format.fprintf fmt "%a%a%a %a%a%a%a%a" Print.punctuation "("
           ScopeVar.format_t scope_var Print.punctuation ":" (Print.typ ctx) typ
           Print.punctuation "|" Print.keyword
           (match Marked.unmark vis.Desugared.Ast.io_input with
           | NoInput -> "internal"
           | OnlyInput -> "input"
           | Reentrant -> "context")
           (if Marked.unmark vis.Desugared.Ast.io_output then fun fmt () ->
            Format.fprintf fmt "%a@,%a" Print.punctuation "|" Print.keyword
              "output"
           else fun fmt () -> Format.fprintf fmt "@<0>")
           () Print.punctuation ")"))
    (ScopeVar.Map.bindings decl.scope_sig)
    Print.punctuation "="
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " Print.punctuation ";")
       (fun fmt rule ->
         match rule with
         | Definition (loc, typ, _, e) ->
           Format.fprintf fmt "@[<hov 2>%a %a %a %a %a@ %a@]" Print.keyword
             "let" Print.location (Marked.unmark loc) Print.punctuation ":"
             (Print.typ ctx) typ Print.punctuation "="
             (fun fmt e ->
               match Marked.unmark loc with
               | SubScopeVar _ -> Print.expr ctx fmt e
               | ScopelangScopeVar v -> (
                 match
                   Marked.unmark
                     (snd (ScopeVar.Map.find (Marked.unmark v) decl.scope_sig))
                       .io_input
                 with
                 | Reentrant ->
                   Format.fprintf fmt "%a@ %a" Print.op_style
                     "reentrant or by default" (Print.expr ~debug ctx) e
                 | _ -> Format.fprintf fmt "%a" (Print.expr ~debug ctx) e))
             e
         | Assertion e ->
           Format.fprintf fmt "%a %a" Print.keyword "assert"
             (Print.expr ~debug ctx) e
         | Call (scope_name, subscope_name, _) ->
           Format.fprintf fmt "%a %a%a%a%a" Print.keyword "call"
             ScopeName.format_t scope_name Print.punctuation "["
             SubScopeName.format_t subscope_name Print.punctuation "]"))
    decl.scope_decl_rules

let program ?(debug : bool = false) (fmt : Format.formatter) (p : 'm program) :
    unit =
  let ctx = p.program_ctx in
  let pp_sep fmt () =
    Format.pp_print_cut fmt ();
    Format.pp_print_cut fmt ()
  in
  Format.pp_open_vbox fmt 0;
  StructName.Map.iter
    (fun n s ->
      struc ctx fmt n s;
      pp_sep fmt ())
    ctx.ctx_structs;
  EnumName.Map.iter
    (fun n e ->
      enum ctx fmt n e;
      pp_sep fmt ())
    ctx.ctx_enums;
  Format.pp_print_list ~pp_sep (scope ~debug ctx) fmt
    (ScopeName.Map.bindings p.program_scopes);
  Format.pp_close_box fmt ()
