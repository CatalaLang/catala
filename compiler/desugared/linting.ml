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
            (match scope_def_key with _, ScopeDef.Var _ -> true | _ -> false)
            && RuleName.Map.is_empty scope_def.scope_def_rules
            && (not scope_def.scope_def_is_condition)
            && (not
                  (ScopeVar.Map.mem
                     (Mark.remove (fst scope_def_key))
                     scope.scope_sub_scopes))
            &&
            match Mark.remove scope_def.scope_def_io.io_input with
            | NoInput -> true
            | _ -> false
          then
            Message.warning
              ~pos:(ScopeDef.get_position scope_def_key)
              "In scope \"%a\",@ the@ variable@ \"%a\"@ is@ declared@ but@ \
               never@ defined;@ did you forget something?"
              ScopeName.format scope_name Ast.ScopeDef.format scope_def_key)
        scope.scope_defs)
    p.program_root.module_scopes

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
                      ( "",
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
                Message.warning ~extra_pos:pos
                  "These %s have identical justifications@ and@ consequences;@ \
                   is it a mistake?"
                  (if scope_def.scope_def_is_condition then "rules"
                   else "definitions"))
            rules_seen)
        scope.scope_defs)
    p.program_root.module_scopes

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
            | EDStructAccess _ -> assert false
            (* linting must be performed after disambiguation *)
            | EStructAccess { e = e_struct; field; _ } ->
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
          Message.warning
            ~pos:(snd (StructName.get_info s_name))
            "The structure@ \"%a\"@ is@ never@ used;@ maybe it's unnecessary?"
            StructName.format s_name
        else
          StructField.Map.iter
            (fun field _ ->
              if
                (not (StructField.Set.mem field struct_fields_used))
                && not (StructField.Set.mem field scope_out_structs_fields)
              then
                Message.warning
                  ~pos:(snd (StructField.get_info field))
                  "The field@ \"%a\"@ of@ struct@ @{<yellow>\"%a\"@}@ is@ \
                   never@ used;@ maybe it's unnecessary?"
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
        ~init:
          (EnumConstructor.Set.of_list
             [ConstantNames.some_constr; ConstantNames.none_constr])
        p
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
          Message.warning
            ~pos:(snd (EnumName.get_info e_name))
            "The enumeration@ \"%a\"@ is@ never@ used;@ maybe it's unnecessary?"
            EnumName.format e_name
        else
          EnumConstructor.Map.iter
            (fun constructor _ ->
              if
                not (EnumConstructor.Set.mem constructor enum_constructors_used)
              then
                Message.warning
                  ~pos:(snd (EnumConstructor.get_info constructor))
                  "The constructor@ \"%a\"@ of@ enumeration@ \"%a\"@ is@ \
                   never@ used;@ maybe it's unnecessary?"
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
        | Var (var, state) ->
          let scope_def =
            ScopeDef.Map.find
              ((var, Pos.void), ScopeDef.Var state)
              scope.scope_defs
          in
          Mark.remove scope_def.scope_def_io.io_output
        (* A variable is initially alive if it is an output*)
      in
      let is_alive = Reachability.analyze is_alive scope_dependencies in
      let emit_unused_warning vx =
        Message.warning
          ~pos:(Mark.get (Dependency.Vertex.info vx))
          "@[<hov>Unused variable:@ %a@ does@ not@ contribute@ to@ computing@ \
           any@ of@ scope@ %a@ outputs.@]@ Did you forget something?"
          Dependency.Vertex.format vx ScopeName.format scope_name
      in
      Dependency.ScopeDependencies.iter_vertex
        (fun vx ->
          if
            (not (is_alive vx))
            && Dependency.ScopeDependencies.succ scope_dependencies vx = []
          then emit_unused_warning vx)
        scope_dependencies)
    p.program_root.module_scopes

(** Local variables bound by [let ... in] that are never used afterwards.
    Desugaring turns let bindings into immediately-applied lambdas; bindings
    synthesized by the compiler itself are recognized (and skipped) by their
    ghost position or ["_"] name. *)
let detect_unused_local_variables (p : program) : unit =
  Ast.fold_exprs
    ~f:(fun () e ->
      let rec aux e =
        (match Mark.remove e with
        | EApp { f = EAbs { binder; pos; _ }, _; _ } ->
          let effect_only =
            (* [let x equals e1 in impossible] evaluates [e1] only for the
               error it may raise, so the unused binding is intentional
               there. *)
            match Bindlib.unmbind binder with
            | _, (EFatalError _, _) -> true
            | _ -> false
          in
          if not effect_only then begin
            let occurs = Bindlib.mbinder_occurs binder in
            let names = Bindlib.mbinder_names binder in
            List.iteri
              (fun i vpos ->
                if
                  (not occurs.(i))
                  && (not (Pos.equal vpos Pos.void))
                  && names.(i).[0] <> '_'
                then
                  Message.warning ~pos:vpos
                    "The local variable@ \"@{<cyan>%s@}\"@ is@ never@ used;@ \
                     maybe it's unnecessary?"
                    names.(i))
              pos
          end
        | _ -> ());
        Expr.shallow_fold (fun e () -> aux e) e ()
      in
      aux e)
    ~init:() p

(** Names that should be in snake_case but contain a CamelCase word boundary.
    Isolated uppercase letters are allowed since they are used in legal
    references (e.g. [section_121_b_2_A], [art93quaterI]). Types, scopes and
    constructors are not checked: underscores are commonly used there for
    article numbers. *)
let detect_non_snake_case_names (p : program) : unit =
  let add kind (name, pos) names =
    if
      Pos.equal pos Pos.void
      || (not (String.has_camel_case_boundary name))
      || Pos.Map.mem pos names
    then names
    else Pos.Map.add pos (kind, name) names
  in
  let names =
    ScopeName.Map.fold
      (fun _ scope names ->
        let names =
          ScopeVar.Map.fold
            (fun var states names ->
              let names = add "variable" (ScopeVar.get_info var) names in
              match states with
              | WholeVar -> names
              | States states ->
                List.fold_left
                  (fun names state ->
                    add "state" (StateName.get_info state) names)
                  names states)
            scope.scope_vars names
        in
        let names =
          ScopeVar.Map.fold
            (fun var _ names -> add "sub-scope" (ScopeVar.get_info var) names)
            scope.scope_sub_scopes names
        in
        ScopeDef.Map.fold
          (fun def scope_def names ->
            match def, scope_def.scope_def_parameters with
            | (_, ScopeDef.Var _), Some (params, _) ->
              List.fold_left
                (fun names (param, _) -> add "parameter" param names)
                names params
            | _ -> names)
          scope.scope_defs names)
      p.program_root.module_scopes Pos.Map.empty
  in
  let names =
    TopdefName.Map.fold
      (fun name topdef names ->
        List.fold_left
          (fun names arg -> add "parameter" arg names)
          (add "toplevel declaration" (TopdefName.get_info name) names)
          topdef.topdef_arg_names)
      p.program_root.module_topdefs names
  in
  let names =
    let scope_structs =
      ScopeName.Map.fold
        (fun _ info acc ->
          StructName.Set.add info.in_struct_name
            (StructName.Set.add info.out_struct_name acc))
        p.program_ctx.ctx_scopes StructName.Set.empty
    in
    StructName.Map.fold
      (fun s_name fields names ->
        if
          StructName.path s_name <> []
          || StructName.Set.mem s_name scope_structs
        then names
        else
          StructField.Map.fold
            (fun field _ names ->
              add "field" (StructField.get_info field) names)
            fields names)
      p.program_ctx.ctx_structs names
  in
  let names =
    (* Local variables, including the ones synthesized by desugaring: those
       have no CamelCase boundary so they are never reported. Toplevel
       parameters, already added above, are skipped by position. *)
    Ast.fold_exprs
      ~f:(fun names e ->
        let rec aux e names =
          let names =
            match Mark.remove e with
            | EAbs { binder; pos; _ } ->
              let vars = Bindlib.mbinder_names binder in
              List.mapi (fun i vpos -> vars.(i), vpos) pos
              |> List.fold_left
                   (fun names v -> add "local variable" v names)
                   names
            | _ -> names
          in
          Expr.shallow_fold aux e names
        in
        aux e names)
      ~init:names p
  in
  Pos.Map.iter
    (fun pos (kind, name) ->
      Message.warning ~pos
        "The %s@ \"@{<cyan>%s@}\"@ is@ not@ written@ in@ snake_case;@ \
         consider@ renaming@ it@ to@ \"@{<cyan>%s@}\"."
        kind name
        (String.camel_to_snake_case name))
    names

let lint_program (p : program) : unit =
  detect_empty_definitions p;
  detect_dead_code p;
  detect_unused_struct_fields p;
  detect_unused_enum_constructors p;
  detect_identical_rules p;
  detect_unused_local_variables p;
  detect_non_snake_case_names p
