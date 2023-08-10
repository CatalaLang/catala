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
    (path, fields : path * typ StructField.Map.t) : unit =
  Format.fprintf fmt "%a %a%a %a %a@\n@[<hov 2>  %a@]@\n%a" Print.keyword "struct"
    Print.path path
    StructName.format name Print.punctuation "=" Print.punctuation "{"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (field_name, typ) ->
         Format.fprintf fmt "%a%a %a" StructField.format field_name
           Print.punctuation ":" (Print.typ ctx) typ))
    (StructField.Map.bindings fields)
    Print.punctuation "}"

let enum
    ctx
    (fmt : Format.formatter)
    (name : EnumName.t)
    (path, cases : path * typ EnumConstructor.Map.t) : unit =
  Format.fprintf fmt "%a %a%a %a @\n@[<hov 2>  %a@]" Print.keyword "enum"
    Print.path path
    EnumName.format name Print.punctuation "="
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
       (fun fmt (field_name, typ) ->
         Format.fprintf fmt "%a %a%a %a" Print.punctuation "|"
           EnumConstructor.format field_name Print.punctuation ":"
           (Print.typ ctx) typ))
    (EnumConstructor.Map.bindings cases)

let scope ?debug ctx fmt (name, (decl, _pos)) =
  Format.fprintf fmt "@[<hov 2>%a@ %a@ %a@ %a@ %a@]@\n@[<v 2>  %a@]"
    Print.keyword "let" Print.keyword "scope" ScopeName.format name
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       (fun fmt (scope_var, (typ, vis)) ->
         Format.fprintf fmt "%a%a%a %a%a%a%a%a" Print.punctuation "("
           ScopeVar.format scope_var Print.punctuation ":" (Print.typ ctx) typ
           Print.punctuation "|" Print.keyword
           (match Mark.remove vis.Desugared.Ast.io_input with
           | NoInput -> "internal"
           | OnlyInput -> "input"
           | Reentrant -> "context")
           (if Mark.remove vis.Desugared.Ast.io_output then fun fmt () ->
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
             "let" Print.location (Mark.remove loc) Print.punctuation ":"
             (Print.typ ctx) typ Print.punctuation "="
             (fun fmt e ->
               match Mark.remove loc with
               | SubScopeVar _ | ToplevelVar _ -> Print.expr () fmt e
               | ScopelangScopeVar { name = v } -> (
                 match
                   Mark.remove
                     (snd (ScopeVar.Map.find (Mark.remove v) decl.scope_sig))
                       .io_input
                 with
                 | Reentrant ->
                   Format.fprintf fmt "%a@ %a" Print.op_style
                     "reentrant or by default" (Print.expr ?debug ()) e
                 | _ -> Format.fprintf fmt "%a" (Print.expr ?debug ()) e))
             e
         | Assertion e ->
           Format.fprintf fmt "%a %a" Print.keyword "assert"
             (Print.expr ?debug ()) e
         | Call ((scope_path, scope_name), subscope_name, _) ->
           Format.fprintf fmt "%a %a%a%a%a%a" Print.keyword "call"
             Print.path scope_path
             ScopeName.format scope_name Print.punctuation "["
             SubScopeName.format subscope_name Print.punctuation "]"))
    decl.scope_decl_rules

let print_topdef ctx ppf name (e, ty) =
  Format.pp_open_vbox ppf 2;
  let () =
    Format.pp_open_hovbox ppf 2;
    Print.keyword ppf "let";
    Format.pp_print_space ppf ();
    TopdefName.format ppf name;
    Print.punctuation ppf ":";
    Format.pp_print_space ppf ();
    Print.typ ctx ppf ty;
    Format.pp_print_space ppf ();
    Print.punctuation ppf "=";
    Format.pp_close_box ppf ()
  in
  Format.pp_print_cut ppf ();
  Print.expr () ppf e;
  Format.pp_close_box ppf ()

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
  TopdefName.Map.iter
    (fun name def ->
      print_topdef ctx fmt name def;
      pp_sep fmt ())
    p.program_topdefs;
  Format.pp_print_list ~pp_sep (scope ~debug ctx) fmt
    (ScopeName.Map.bindings p.program_scopes);
  Format.pp_close_box fmt ()
