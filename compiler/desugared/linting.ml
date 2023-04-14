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
let detect_empty_definitions (p : program) : unit =
  ScopeName.Map.iter
    (fun (scope_name : ScopeName.t) scope ->
      ScopeDef.Map.iter
        (fun scope_def_key scope_def ->
          if
            (match scope_def_key with ScopeDef.Var _ -> true | _ -> false)
            && RuleName.Map.is_empty scope_def.scope_def_rules
            && (not scope_def.scope_def_is_condition)
            &&
            match Marked.unmark scope_def.scope_def_io.io_input with
            | Ast.NoInput -> true
            | _ -> false
          then
            Errors.format_spanned_warning
              (ScopeDef.get_position scope_def_key)
              "In scope %a, the variable %a is declared but never defined; did \
               you forget something?"
              (Cli.format_with_style [ANSITerminal.yellow])
              (Format.asprintf "\"%a\"" ScopeName.format_t scope_name)
              (Cli.format_with_style [ANSITerminal.yellow])
              (Format.asprintf "\"%a\"" Ast.ScopeDef.format_t scope_def_key))
        scope.scope_defs)
    p.program_scopes

(* To detect rules that have the same justification and conclusion, we create a
   set data structure with an appropriate comparison function *)
module RuleExpressionsMap = Map.Make (struct
  type t = rule

  let compare x y =
    let xj, xj_mark = x.rule_just in
    let yj, yj_mark = y.rule_just in
    let just =
      Bindlib.unbox
        (Bindlib.box_apply2
           (fun xj yj -> Expr.compare (xj, xj_mark) (yj, yj_mark))
           xj yj)
    in
    if just = 0 then
      let xc, xc_mark = x.rule_cons in
      let yc, yc_mark = y.rule_cons in
      Bindlib.unbox
        (Bindlib.box_apply2
           (fun xc yc -> Expr.compare (xc, xc_mark) (yc, yc_mark))
           xc yc)
    else just
end)

let detect_identical_rules (p : program) : unit =
  ScopeName.Map.iter
    (fun _ scope ->
      ScopeDefMap.iter
        (fun _ scope_def ->
          let rules_seen =
            RuleName.Map.fold
              (fun _ rule rules_seen ->
                RuleExpressionsMap.update rule
                  (fun l ->
                    let x =
                      ( None,
                        Pos.overwrite_law_info
                          (snd (RuleName.get_info rule.rule_id))
                          (Pos.get_law_info (Expr.pos rule.rule_just)) )
                    in
                    match l with None -> Some [x] | Some l -> Some (x :: l))
                  rules_seen)
              scope_def.scope_def_rules RuleExpressionsMap.empty
          in
          RuleExpressionsMap.iter
            (fun _ pos ->
              if List.length pos > 1 then
                Errors.format_multispanned_warning pos
                  "These %s have identical justifications and consequences; is \
                   it a mistake?"
                  (if scope_def.scope_def_is_condition then "rules"
                  else "definitions"))
            rules_seen)
        scope.scope_defs)
    p.program_scopes

let detect_unused_scope_vars (p : program) : unit =
  let used_scope_vars =
    Ast.fold_exprs
      ~f:(fun used_scope_vars e ->
        let rec used_scope_vars_expr e used_scope_vars =
          match Marked.unmark e with
          | ELocation (DesugaredScopeVar (v, _)) ->
            ScopeVar.Set.add (Marked.unmark v) used_scope_vars
          | _ -> Expr.shallow_fold used_scope_vars_expr e used_scope_vars
        in
        used_scope_vars_expr e used_scope_vars)
      ~init:ScopeVar.Set.empty p
  in
  ScopeName.Map.iter
    (fun (scope_name : ScopeName.t) scope ->
      ScopeDef.Map.iter
        (fun scope_def_key scope_def ->
          match scope_def_key with
          | ScopeDef.Var (v, _)
            when (not (ScopeVar.Set.mem v used_scope_vars))
                 && not (Marked.unmark scope_def.scope_def_io.io_output) ->
            Errors.format_spanned_warning
              (ScopeDef.get_position scope_def_key)
              "In scope %a, the variable %a is never used anywhere; maybe it's \
               unnecessary?"
              (Cli.format_with_style [ANSITerminal.yellow])
              (Format.asprintf "\"%a\"" ScopeName.format_t scope_name)
              (Cli.format_with_style [ANSITerminal.yellow])
              (Format.asprintf "\"%a\"" Ast.ScopeDef.format_t scope_def_key)
          | _ -> ())
        scope.scope_defs)
    p.program_scopes

let detect_unused_struct_fields (p : program) : unit =
  let struct_fields_used =
    Ast.fold_exprs
      ~f:(fun struct_fields_used e ->
        let rec structs_fields_used_expr e struct_fields_used =
          match Marked.unmark e with
          | EDStructAccess { name_opt = Some name; e = e_struct; field } ->
            let field =
              StructName.Map.find name
                (IdentName.Map.find field p.program_ctx.ctx_struct_fields)
            in
            StructField.Set.add field
              (structs_fields_used_expr e_struct struct_fields_used)
          | EStruct { name = _; fields } ->
            StructField.Map.fold
              (fun field e_field struct_fields_used ->
                StructField.Set.add field
                  (structs_fields_used_expr e_field struct_fields_used))
              fields struct_fields_used
          | _ -> Expr.shallow_fold structs_fields_used_expr e struct_fields_used
        in
        structs_fields_used_expr e struct_fields_used)
      ~init:StructField.Set.empty p
  in
  let scope_out_structs_fields =
    ScopeName.Map.fold
      (fun _ out_struct acc ->
        ScopeVar.Map.fold
          (fun _ field acc -> StructField.Set.add field acc)
          out_struct.out_struct_fields acc)
      p.program_ctx.ctx_scopes StructField.Set.empty
  in
  StructName.Map.iter
    (fun s_name fields ->
      if
        (not (StructField.Map.is_empty fields))
        && StructField.Map.for_all
             (fun field _ ->
               (not (StructField.Set.mem field struct_fields_used))
               && not (StructField.Set.mem field scope_out_structs_fields))
             fields
      then
        Errors.format_spanned_warning
          (snd (StructName.get_info s_name))
          "The structure %a is never used; maybe it's unnecessary?"
          (Cli.format_with_style [ANSITerminal.yellow])
          (Format.asprintf "\"%a\"" StructName.format_t s_name)
      else
        StructField.Map.iter
          (fun field _ ->
            if
              (not (StructField.Set.mem field struct_fields_used))
              && not (StructField.Set.mem field scope_out_structs_fields)
            then
              Errors.format_spanned_warning
                (snd (StructField.get_info field))
                "The field %a of struct %a is never used; maybe it's \
                 unnecessary?"
                (Cli.format_with_style [ANSITerminal.yellow])
                (Format.asprintf "\"%a\"" StructField.format_t field)
                (Cli.format_with_style [ANSITerminal.yellow])
                (Format.asprintf "\"%a\"" StructName.format_t s_name))
          fields)
    p.program_ctx.ctx_structs

let detect_unused_enum_constructors (p : program) : unit =
  let enum_constructors_used =
    Ast.fold_exprs
      ~f:(fun enum_constructors_used e ->
        let rec enum_constructors_used_expr e enum_constructors_used =
          match Marked.unmark e with
          | EInj { name = _; e = e_enum; cons } ->
            EnumConstructor.Set.add cons
              (enum_constructors_used_expr e_enum enum_constructors_used)
          | EMatch { e = e_match; name = _; cases } ->
            let enum_constructors_used =
              enum_constructors_used_expr e_match enum_constructors_used
            in
            EnumConstructor.Map.fold
              (fun cons e_cons enum_constructors_used ->
                EnumConstructor.Set.add cons
                  (enum_constructors_used_expr e_cons enum_constructors_used))
              cases enum_constructors_used
          | _ ->
            Expr.shallow_fold enum_constructors_used_expr e
              enum_constructors_used
        in
        enum_constructors_used_expr e enum_constructors_used)
      ~init:EnumConstructor.Set.empty p
  in
  EnumName.Map.iter
    (fun e_name constructors ->
      if
        EnumConstructor.Map.for_all
          (fun cons _ ->
            not (EnumConstructor.Set.mem cons enum_constructors_used))
          constructors
      then
        Errors.format_spanned_warning
          (snd (EnumName.get_info e_name))
          "The enumeration %a is never used; maybe it's unnecessary?"
          (Cli.format_with_style [ANSITerminal.yellow])
          (Format.asprintf "\"%a\"" EnumName.format_t e_name)
      else
        EnumConstructor.Map.iter
          (fun constructor _ ->
            if not (EnumConstructor.Set.mem constructor enum_constructors_used)
            then
              Errors.format_spanned_warning
                (snd (EnumConstructor.get_info constructor))
                "The constructor %a of enumeration %a is never used; maybe \
                 it's unnecessary?"
                (Cli.format_with_style [ANSITerminal.yellow])
                (Format.asprintf "\"%a\"" EnumConstructor.format_t constructor)
                (Cli.format_with_style [ANSITerminal.yellow])
                (Format.asprintf "\"%a\"" EnumName.format_t e_name))
          constructors)
    p.program_ctx.ctx_enums

let lint_program (p : program) : unit =
  detect_empty_definitions p;
  detect_unused_scope_vars p;
  detect_unused_struct_fields p;
  detect_unused_enum_constructors p;
  detect_identical_rules p
