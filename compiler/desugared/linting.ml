(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
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

open Shared_ast
open Ast
open Catala_utils

(** If the variable is not an input, then it should be defined somewhere. *)
let detect_empty_definitions
    (scope : ScopeName.t)
    (def_info : ScopeDef.t)
    (def : scope_def) : unit =
  if
    RuleName.Map.is_empty def.scope_def_rules
    && (not def.scope_def_is_condition)
    &&
    match Marked.unmark def.scope_def_io.io_input with
    | Ast.NoInput -> true
    | _ -> false
  then
    Errors.format_spanned_warning
      (ScopeDef.get_position def_info)
      "The variable %a is declared but never defined in scope %a"
      (Cli.format_with_style [ANSITerminal.yellow])
      (Format.asprintf "\"%a\"" Ast.ScopeDef.format_t def_info)
      (Cli.format_with_style [ANSITerminal.yellow])
      (Format.asprintf "\"%a\"" ScopeName.format_t scope)

let lint_program (p : program) : unit =
  ScopeName.Map.iter
    (fun scope_name scope ->
      ScopeDefMap.iter
        (fun scope_def_key scope_def ->
          detect_empty_definitions scope_name scope_def_key scope_def)
        scope.scope_defs)
    p.program_scopes
