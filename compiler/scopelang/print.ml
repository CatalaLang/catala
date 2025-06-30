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

let scope ?debug fmt (name, (decl, _pos)) =
  Print.attrs fmt (Mark.get (ScopeName.get_info name));
  Format.pp_open_vbox fmt 2;
  Format.pp_open_hvbox fmt 4;
  Print.keyword fmt "let scope ";
  ScopeName.format fmt name;
  Format.pp_print_space fmt ();
  Format.pp_print_list ~pp_sep:Format.pp_print_space
    (fun fmt (scope_var, svar) ->
      Format.pp_open_hovbox fmt 1;
      Print.punctuation fmt "(";
      ScopeVar.format fmt scope_var;
      Print.punctuation fmt ":";
      Format.pp_print_space fmt ();
      Print.typ fmt svar.svar_in_ty;
      Format.pp_print_cut fmt ();
      Print.punctuation fmt "|";
      Print.keyword fmt
        (match Mark.remove svar.svar_io.Desugared.Ast.io_input with
        | NoInput -> "internal"
        | OnlyInput -> "input"
        | Reentrant -> "context");
      if Mark.remove svar.svar_io.Desugared.Ast.io_output then (
        Print.punctuation fmt "|";
        Print.keyword fmt "output");
      Print.punctuation fmt ")";
      Format.pp_close_box fmt ())
    fmt
    (ScopeVar.Map.bindings decl.scope_sig);
  Format.pp_print_break fmt 1 (-2);
  Print.punctuation fmt "=";
  Format.pp_close_box fmt ();
  Format.pp_print_space fmt ();
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%a@ " Print.punctuation ";")
    (fun fmt rule ->
      match rule with
      | ScopeVarDefinition { var; typ; io; e } ->
        Print.attrs fmt (Mark.get (ScopeVar.get_info (fst var)));
        Format.fprintf fmt "@[<hov 2>%a %a %a %a %a@ %t%a@]" Print.keyword "let"
          ScopeVar.format (Mark.remove var) Print.punctuation ":" Print.typ typ
          Print.punctuation "="
          (fun fmt ->
            match Mark.remove io.io_input with
            | Reentrant ->
              Print.op_style fmt "reentrant or by default";
              Format.pp_print_space fmt ()
            | _ -> ())
          (Print.expr ?debug ()) e
      | SubScopeVarDefinition { var; typ; e; _ } ->
        Print.attrs fmt (Mark.get (ScopeVar.get_info (fst var)));
        Format.fprintf fmt "@[<hov 2>%a %a %a %a %a@ %a@]" Print.keyword "let"
          ScopeVar.format (Mark.remove var) Print.punctuation ":" Print.typ typ
          Print.punctuation "=" (Print.expr ?debug ()) e
      | Assertion e ->
        Format.fprintf fmt "%a %a" Print.keyword "assert" (Print.expr ?debug ())
          e)
    fmt decl.scope_decl_rules;
  Format.pp_close_box fmt ()

let print_topdef ppf name (e, ty, _vis, _is_external) =
  Print.attrs ppf (Mark.get (TopdefName.get_info name));
  Format.pp_open_vbox ppf 2;
  let () =
    Format.pp_open_hovbox ppf 2;
    Print.keyword ppf "let";
    Format.pp_print_space ppf ();
    TopdefName.format ppf name;
    Print.punctuation ppf ":";
    Format.pp_print_space ppf ();
    Print.typ ppf ty;
    Format.pp_print_space ppf ();
    Print.punctuation ppf "=";
    Format.pp_close_box ppf ()
  in
  Format.pp_print_cut ppf ();
  Print.expr () ppf e;
  Format.pp_close_box ppf ()

let program ?(debug : bool = false) (fmt : Format.formatter) (p : 'm program) :
    unit =
  let pp_sep fmt () =
    Format.pp_print_cut fmt ();
    Format.pp_print_cut fmt ()
  in
  Format.pp_open_vbox fmt 0;
  Print.decl_ctx ~debug fmt p.program_ctx;
  TopdefName.Map.iter
    (fun name def ->
      print_topdef fmt name def;
      pp_sep fmt ())
    p.program_topdefs;
  ScopeName.Map.format_bindings_i
    (fun fmt _ name scope_decl ->
      Format.pp_print_cut fmt ();
      scope ~debug fmt (name, scope_decl))
    fmt p.program_scopes;
  Format.pp_close_box fmt ()
