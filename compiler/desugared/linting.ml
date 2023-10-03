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
            match Mark.remove scope_def.scope_def_io.io_input with
            | NoInput -> true
            | _ -> false
          then
            Message.emit_spanned_warning
              (ScopeDef.get_position scope_def_key)
              "In scope \"%a\", the variable \"%a\" is declared but never \
               defined; did you forget something?"
              ScopeName.format scope_name Ast.ScopeDef.format scope_def_key)
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

  let format ppf r = RuleName.format ppf r.rule_id
end)

let detect_identical_rules (p : program) : unit =
  ScopeName.Map.iter
    (fun _ scope ->
      ScopeDef.Map.iter
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
                Message.emit_multispanned_warning pos
                  "These %s have identical justifications and consequences; is \
                   it a mistake?"
                  (if scope_def.scope_def_is_condition then "rules"
                   else "definitions"))
            rules_seen)
        scope.scope_defs)
    p.program_scopes

let detect_unused_struct_fields (p : program) : unit =
  (* TODO: this analysis should be finer grained: a false negative is if the
     field is used to define itself, for passing data around but that never gets
     really used or defined. *)
  if p.program_module_name <> None then ()
  else
    (* Disabled on modules *)
    let struct_fields_used =
      Ast.fold_exprs
        ~f:(fun struct_fields_used e ->
          let rec structs_fields_used_expr e struct_fields_used =
            match Mark.remove e with
            | EDStructAccess { name_opt = Some name; e = e_struct; field } ->
              let ctx = Program.module_ctx p.program_ctx (StructName.path name) in
              let field =
                StructName.Map.find name
                  (Ident.Map.find field ctx.ctx_struct_fields)
              in
              StructField.Set.add field
                (structs_fields_used_expr e_struct struct_fields_used)
            | EStruct { name = _; fields } ->
              StructField.Map.fold
                (fun field e_field struct_fields_used ->
                  StructField.Set.add field
                    (structs_fields_used_expr e_field struct_fields_used))
                fields struct_fields_used
            | _ ->
              Expr.shallow_fold structs_fields_used_expr e struct_fields_used
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
        if StructName.path s_name <> [] then
          (* Only check structs from the current module *)
          ()
        else if
          (not (StructField.Map.is_empty fields))
          && StructField.Map.for_all
               (fun field _ ->
                 (not (StructField.Set.mem field struct_fields_used))
                 && not (StructField.Set.mem field scope_out_structs_fields))
               fields
        then
          Message.emit_spanned_warning
            (snd (StructName.get_info s_name))
            "The structure \"%a\" is never used; maybe it's unnecessary?"
            StructName.format s_name
        else
          StructField.Map.iter
            (fun field _ ->
              if
                (not (StructField.Set.mem field struct_fields_used))
                && not (StructField.Set.mem field scope_out_structs_fields)
              then
                Message.emit_spanned_warning
                  (snd (StructField.get_info field))
                  "The field \"%a\" of struct @{<yellow>\"%a\"@} is never \
                   used; maybe it's unnecessary?"
                  StructField.format field StructName.format s_name)
            fields)
      p.program_ctx.ctx_structs

let detect_unused_enum_constructors (p : program) : unit =
  if p.program_module_name <> None then ()
  else
    (* Disabled on modules *)
    let enum_constructors_used =
      Ast.fold_exprs
        ~f:(fun enum_constructors_used e ->
          let rec enum_constructors_used_expr e enum_constructors_used =
            match Mark.remove e with
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
        if EnumName.path e_name <> [] then
          (* Only check enums from the current module *)
          ()
        else if
          EnumConstructor.Map.for_all
            (fun cons _ ->
              not (EnumConstructor.Set.mem cons enum_constructors_used))
            constructors
        then
          Message.emit_spanned_warning
            (snd (EnumName.get_info e_name))
            "The enumeration \"%a\" is never used; maybe it's unnecessary?"
            EnumName.format e_name
        else
          EnumConstructor.Map.iter
            (fun constructor _ ->
              if
                not (EnumConstructor.Set.mem constructor enum_constructors_used)
              then
                Message.emit_spanned_warning
                  (snd (EnumConstructor.get_info constructor))
                  "The constructor \"%a\" of enumeration \"%a\" is never used; \
                   maybe it's unnecessary?"
                  EnumConstructor.format constructor EnumName.format e_name)
            constructors)
      p.program_ctx.ctx_enums

(* Reachability in a graph can be implemented as a simple fixpoint analysis with
   backwards propagation. *)
module Reachability =
  Graph.Fixpoint.Make
    (Dependency.ScopeDependencies)
    (struct
      type vertex = Dependency.ScopeDependencies.vertex
      type edge = Dependency.ScopeDependencies.E.t
      type g = Dependency.ScopeDependencies.t
      type data = bool

      let direction = Graph.Fixpoint.Backward
      let equal = ( = )
      let join = ( || )
      let analyze _ x = x
    end)

let detect_dead_code (p : program) : unit =
  (* Dead code detection for scope variables based on an intra-scope dependency
     analysis. *)
  ScopeName.Map.iter
    (fun scope_name scope ->
      let scope_dependencies = Dependency.build_scope_dependencies scope in
      let is_alive (v : Dependency.ScopeDependencies.vertex) =
        match v with
        | Assertion _ -> true
        | SubScope _ -> true
        | Var (var, state) ->
          let scope_def =
            ScopeDef.Map.find (Var (var, state)) scope.scope_defs
          in
          Mark.remove scope_def.scope_def_io.io_output
        (* A variable is initially alive if it is an output*)
      in
      let is_alive = Reachability.analyze is_alive scope_dependencies in
      ScopeVar.Map.iter
        (fun var states ->
          let emit_unused_warning () =
            Message.emit_spanned_warning
              (Mark.get (ScopeVar.get_info var))
              "This variable is dead code; it does not contribute to computing \
               any of scope \"%a\" outputs. Did you forget something?"
              ScopeName.format scope_name
          in
          match states with
          | WholeVar ->
            if not (is_alive (Var (var, None))) then emit_unused_warning ()
          | States states ->
            List.iter
              (fun state ->
                if not (is_alive (Var (var, Some state))) then
                  emit_unused_warning ())
              states)
        scope.scope_vars)
    p.program_scopes

let lint_program (p : program) : unit =
  detect_empty_definitions p;
  detect_dead_code p;
  detect_unused_struct_fields p;
  detect_unused_enum_constructors p;
  detect_identical_rules p
