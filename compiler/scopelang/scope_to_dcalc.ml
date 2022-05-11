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

open Utils

type scope_var_ctx = {
  scope_var_name : Ast.ScopeVar.t;
  scope_var_typ : Dcalc.Ast.typ;
  scope_var_io : Ast.io;
}

type scope_sig_ctx = {
  scope_sig_local_vars : scope_var_ctx list;  (** List of scope variables *)
  scope_sig_scope_var : Dcalc.Ast.Var.t;  (** Var representing the scope *)
  scope_sig_input_var : Dcalc.Ast.Var.t;
      (** Var representing the scope input inside the scope func *)
  scope_sig_input_struct : Ast.StructName.t;  (** Scope input *)
  scope_sig_output_struct : Ast.StructName.t;  (** Scope output *)
}

type scope_sigs_ctx = scope_sig_ctx Ast.ScopeMap.t

type ctx = {
  structs : Ast.struct_ctx;
  enums : Ast.enum_ctx;
  scope_name : Ast.ScopeName.t;
  scopes_parameters : scope_sigs_ctx;
  scope_vars : (Dcalc.Ast.Var.t * Dcalc.Ast.typ * Ast.io) Ast.ScopeVarMap.t;
  subscope_vars :
    (Dcalc.Ast.Var.t * Dcalc.Ast.typ * Ast.io) Ast.ScopeVarMap.t
    Ast.SubScopeMap.t;
  local_vars : Dcalc.Ast.Var.t Ast.VarMap.t;
}

let empty_ctx
    (struct_ctx : Ast.struct_ctx)
    (enum_ctx : Ast.enum_ctx)
    (scopes_ctx : scope_sigs_ctx)
    (scope_name : Ast.ScopeName.t) =
  {
    structs = struct_ctx;
    enums = enum_ctx;
    scope_name;
    scopes_parameters = scopes_ctx;
    scope_vars = Ast.ScopeVarMap.empty;
    subscope_vars = Ast.SubScopeMap.empty;
    local_vars = Ast.VarMap.empty;
  }

let rec translate_typ (ctx : ctx) (t : Ast.typ Pos.marked) :
    Dcalc.Ast.typ Pos.marked =
  Pos.same_pos_as
    (match Pos.unmark t with
    | Ast.TLit l -> Dcalc.Ast.TLit l
    | Ast.TArrow (t1, t2) ->
      Dcalc.Ast.TArrow (translate_typ ctx t1, translate_typ ctx t2)
    | Ast.TStruct s_uid ->
      let s_fields = Ast.StructMap.find s_uid ctx.structs in
      Dcalc.Ast.TTuple
        (List.map (fun (_, t) -> translate_typ ctx t) s_fields, Some s_uid)
    | Ast.TEnum e_uid ->
      let e_cases = Ast.EnumMap.find e_uid ctx.enums in
      Dcalc.Ast.TEnum
        (List.map (fun (_, t) -> translate_typ ctx t) e_cases, e_uid)
    | Ast.TArray t1 ->
      Dcalc.Ast.TArray (translate_typ ctx (Pos.same_pos_as t1 t))
    | Ast.TAny -> Dcalc.Ast.TAny)
    t

let merge_defaults
    (caller : Dcalc.Ast.expr Pos.marked Bindlib.box)
    (callee : Dcalc.Ast.expr Pos.marked Bindlib.box) :
    Dcalc.Ast.expr Pos.marked Bindlib.box =
  let caller =
    Dcalc.Ast.make_app caller
      [Bindlib.box (Dcalc.Ast.ELit Dcalc.Ast.LUnit, Pos.no_pos)]
      Pos.no_pos
  in
  let body =
    Bindlib.box_apply2
      (fun caller callee ->
        ( Dcalc.Ast.EDefault
            ( [caller],
              (Dcalc.Ast.ELit (Dcalc.Ast.LBool true), Pos.no_pos),
              callee ),
          Pos.no_pos ))
      caller callee
  in
  body

let tag_with_log_entry
    (e : Dcalc.Ast.expr Pos.marked Bindlib.box)
    (l : Dcalc.Ast.log_entry)
    (markings : Utils.Uid.MarkedString.info list) :
    Dcalc.Ast.expr Pos.marked Bindlib.box =
  Bindlib.box_apply
    (fun e ->
      ( Dcalc.Ast.EApp
          ( ( Dcalc.Ast.EOp (Dcalc.Ast.Unop (Dcalc.Ast.Log (l, markings))),
              Pos.get_position e ),
            [e] ),
        Pos.get_position e ))
    e

let rec translate_expr (ctx : ctx) (e : Ast.expr Pos.marked) :
    Dcalc.Ast.expr Pos.marked Bindlib.box =
  Bindlib.box_apply
    (fun (x : Dcalc.Ast.expr) -> Pos.same_pos_as x e)
    (match Pos.unmark e with
    | EVar v -> Bindlib.box_var (Ast.VarMap.find (Pos.unmark v) ctx.local_vars)
    | ELit l -> Bindlib.box (Dcalc.Ast.ELit l)
    | EStruct (struct_name, e_fields) ->
      let struct_sig = Ast.StructMap.find struct_name ctx.structs in
      let d_fields, remaining_e_fields =
        List.fold_right
          (fun (field_name, _) (d_fields, e_fields) ->
            let field_e = Ast.StructFieldMap.find field_name e_fields in
            let field_d = translate_expr ctx field_e in
            field_d :: d_fields, Ast.StructFieldMap.remove field_name e_fields)
          struct_sig ([], e_fields)
      in
      if Ast.StructFieldMap.cardinal remaining_e_fields > 0 then
        Errors.raise_spanned_error (Pos.get_position e)
          "The fields \"%a\" do not belong to the structure %a"
          Ast.StructName.format_t struct_name
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt (field_name, _) ->
               Format.fprintf fmt "%a" Ast.StructFieldName.format_t field_name))
          (Ast.StructFieldMap.bindings remaining_e_fields)
      else
        Bindlib.box_apply
          (fun d_fields -> Dcalc.Ast.ETuple (d_fields, Some struct_name))
          (Bindlib.box_list d_fields)
    | EStructAccess (e1, field_name, struct_name) ->
      let struct_sig = Ast.StructMap.find struct_name ctx.structs in
      let _, field_index =
        try
          List.assoc field_name
            (List.mapi (fun i (x, y) -> x, (y, i)) struct_sig)
        with Not_found ->
          Errors.raise_spanned_error (Pos.get_position e)
            "The field \"%a\" does not belong to the structure %a"
            Ast.StructFieldName.format_t field_name Ast.StructName.format_t
            struct_name
      in
      let e1 = translate_expr ctx e1 in
      Bindlib.box_apply
        (fun e1 ->
          Dcalc.Ast.ETupleAccess
            ( e1,
              field_index,
              Some struct_name,
              List.map (fun (_, t) -> translate_typ ctx t) struct_sig ))
        e1
    | EEnumInj (e1, constructor, enum_name) ->
      let enum_sig = Ast.EnumMap.find enum_name ctx.enums in
      let _, constructor_index =
        try
          List.assoc constructor
            (List.mapi (fun i (x, y) -> x, (y, i)) enum_sig)
        with Not_found ->
          Errors.raise_spanned_error (Pos.get_position e)
            "The constructor \"%a\" does not belong to the enum %a"
            Ast.EnumConstructor.format_t constructor Ast.EnumName.format_t
            enum_name
      in
      let e1 = translate_expr ctx e1 in
      Bindlib.box_apply
        (fun e1 ->
          Dcalc.Ast.EInj
            ( e1,
              constructor_index,
              enum_name,
              List.map (fun (_, t) -> translate_typ ctx t) enum_sig ))
        e1
    | EMatch (e1, enum_name, cases) ->
      let enum_sig = Ast.EnumMap.find enum_name ctx.enums in
      let d_cases, remaining_e_cases =
        List.fold_right
          (fun (constructor, _) (d_cases, e_cases) ->
            let case_e =
              try Ast.EnumConstructorMap.find constructor e_cases
              with Not_found ->
                Errors.raise_spanned_error (Pos.get_position e)
                  "The constructor %a of enum %a is missing from this pattern \
                   matching"
                  Ast.EnumConstructor.format_t constructor Ast.EnumName.format_t
                  enum_name
            in
            let case_d = translate_expr ctx case_e in
            case_d :: d_cases, Ast.EnumConstructorMap.remove constructor e_cases)
          enum_sig ([], cases)
      in
      if Ast.EnumConstructorMap.cardinal remaining_e_cases > 0 then
        Errors.raise_spanned_error (Pos.get_position e)
          "Patter matching is incomplete for enum %a: missing cases %a"
          Ast.EnumName.format_t enum_name
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             (fun fmt (case_name, _) ->
               Format.fprintf fmt "%a" Ast.EnumConstructor.format_t case_name))
          (Ast.EnumConstructorMap.bindings remaining_e_cases)
      else
        let e1 = translate_expr ctx e1 in
        Bindlib.box_apply2
          (fun d_fields e1 -> Dcalc.Ast.EMatch (e1, d_fields, enum_name))
          (Bindlib.box_list d_cases) e1
    | EApp (e1, args) ->
      (* We insert various log calls to record arguments and outputs of
         user-defined functions belonging to scopes *)
      let e1_func = translate_expr ctx e1 in
      let markings l =
        match l with
        | Ast.ScopeVar (v, _) ->
          [Ast.ScopeName.get_info ctx.scope_name; Ast.ScopeVar.get_info v]
        | Ast.SubScopeVar (s, _, (v, _)) ->
          [Ast.ScopeName.get_info s; Ast.ScopeVar.get_info v]
      in
      let e1_func =
        match Pos.unmark e1 with
        | ELocation l ->
          tag_with_log_entry e1_func Dcalc.Ast.BeginCall (markings l)
        | _ -> e1_func
      in
      let new_args = List.map (translate_expr ctx) args in
      let new_args =
        match Pos.unmark e1, new_args with
        | ELocation l, [new_arg] ->
          [
            tag_with_log_entry new_arg (Dcalc.Ast.VarDef Dcalc.Ast.TAny)
              (markings l @ [Pos.same_pos_as "input" e]);
          ]
        | _ -> new_args
      in
      let new_e =
        Bindlib.box_apply2
          (fun e' u -> Dcalc.Ast.EApp (e', u), Pos.get_position e)
          e1_func
          (Bindlib.box_list new_args)
      in
      let new_e =
        match Pos.unmark e1 with
        | ELocation l ->
          tag_with_log_entry
            (tag_with_log_entry new_e (Dcalc.Ast.VarDef Dcalc.Ast.TAny)
               (markings l @ [Pos.same_pos_as "output" e]))
            Dcalc.Ast.EndCall (markings l)
        | _ -> new_e
      in
      Bindlib.box_apply Pos.unmark new_e
    | EAbs ((binder, pos_binder), typ) ->
      let xs, body = Bindlib.unmbind binder in
      let new_xs =
        Array.map
          (fun x -> Dcalc.Ast.Var.make (Bindlib.name_of x, Pos.no_pos))
          xs
      in
      let both_xs = Array.map2 (fun x new_x -> x, new_x) xs new_xs in
      let body =
        translate_expr
          {
            ctx with
            local_vars =
              Array.fold_left
                (fun local_vars (x, new_x) -> Ast.VarMap.add x new_x local_vars)
                ctx.local_vars both_xs;
          }
          body
      in
      let binder = Bindlib.bind_mvar new_xs body in
      Bindlib.box_apply
        (fun b ->
          Dcalc.Ast.EAbs ((b, pos_binder), List.map (translate_typ ctx) typ))
        binder
    | EDefault (excepts, just, cons) ->
      Bindlib.box_apply3
        (fun e j c -> Dcalc.Ast.EDefault (e, j, c))
        (Bindlib.box_list (List.map (translate_expr ctx) excepts))
        (translate_expr ctx just) (translate_expr ctx cons)
    | ELocation (ScopeVar a) ->
      let v, _, _ = Ast.ScopeVarMap.find (Pos.unmark a) ctx.scope_vars in
      Bindlib.box_var v
    | ELocation (SubScopeVar (_, s, a)) -> (
      try
        let v, _, _ =
          Ast.ScopeVarMap.find (Pos.unmark a)
            (Ast.SubScopeMap.find (Pos.unmark s) ctx.subscope_vars)
        in
        Bindlib.box_var v
      with Not_found ->
        Errors.raise_multispanned_error
          [
            Some "Incriminated variable usage:", Pos.get_position e;
            ( Some "Incriminated subscope variable declaration:",
              Pos.get_position (Ast.ScopeVar.get_info (Pos.unmark a)) );
            ( Some "Incriminated subscope declaration:",
              Pos.get_position (Ast.SubScopeName.get_info (Pos.unmark s)) );
          ]
          "The variable %a.%a cannot be used here, as it is not part subscope \
           %a's results. Maybe you forgot to qualify it as an output?"
          Ast.SubScopeName.format_t (Pos.unmark s) Ast.ScopeVar.format_t
          (Pos.unmark a) Ast.SubScopeName.format_t (Pos.unmark s))
    | EIfThenElse (cond, et, ef) ->
      Bindlib.box_apply3
        (fun c t f -> Dcalc.Ast.EIfThenElse (c, t, f))
        (translate_expr ctx cond) (translate_expr ctx et)
        (translate_expr ctx ef)
    | EOp op -> Bindlib.box (Dcalc.Ast.EOp op)
    | ErrorOnEmpty e' ->
      Bindlib.box_apply
        (fun e' -> Dcalc.Ast.ErrorOnEmpty e')
        (translate_expr ctx e')
    | EArray es ->
      Bindlib.box_apply
        (fun es -> Dcalc.Ast.EArray es)
        (Bindlib.box_list (List.map (translate_expr ctx) es)))

(** The result of a rule translation is a list of assignment, with variables and
    expressions. We also return the new translation context available after the
    assignment to use in later rule translations. The list is actually a
    continuation yielding a [Dcalc.scope_body_expr] by giving it what should
    come later in the chain of let-bindings. *)
let translate_rule
    (ctx : ctx)
    (rule : Ast.rule)
    ((sigma_name, pos_sigma) : Utils.Uid.MarkedString.info) :
    (Dcalc.Ast.expr Dcalc.Ast.scope_body_expr Bindlib.box ->
    Dcalc.Ast.expr Dcalc.Ast.scope_body_expr Bindlib.box)
    * ctx =
  match rule with
  | Definition ((ScopeVar a, var_def_pos), tau, a_io, e) ->
    let a_name = Ast.ScopeVar.get_info (Pos.unmark a) in
    let a_var = Dcalc.Ast.Var.make a_name in
    let tau = translate_typ ctx tau in
    let new_e = translate_expr ctx e in
    let a_expr = Dcalc.Ast.make_var (a_var, var_def_pos) in
    let merged_expr =
      Bindlib.box_apply
        (fun merged_expr ->
          Dcalc.Ast.ErrorOnEmpty merged_expr, Pos.get_position a_name)
        (match Pos.unmark a_io.io_input with
        | OnlyInput ->
          failwith "should not happen"
          (* scopelang should not contain any definitions of input only
             variables *)
        | Reentrant -> merge_defaults a_expr new_e
        | NoInput -> new_e)
    in
    let merged_expr =
      tag_with_log_entry merged_expr
        (Dcalc.Ast.VarDef (Pos.unmark tau))
        [sigma_name, pos_sigma; a_name]
    in
    ( (fun next ->
        Bindlib.box_apply2
          (fun next merged_expr ->
            Dcalc.Ast.ScopeLet
              {
                Dcalc.Ast.scope_let_next = next;
                Dcalc.Ast.scope_let_typ = tau;
                Dcalc.Ast.scope_let_expr = merged_expr;
                Dcalc.Ast.scope_let_kind = Dcalc.Ast.ScopeVarDefinition;
                Dcalc.Ast.scope_let_pos = Pos.get_position a;
              })
          (Bindlib.bind_var a_var next)
          merged_expr),
      {
        ctx with
        scope_vars =
          Ast.ScopeVarMap.add (Pos.unmark a)
            (a_var, Pos.unmark tau, a_io)
            ctx.scope_vars;
      } )
  | Definition
      ( (SubScopeVar (_subs_name, subs_index, subs_var), var_def_pos),
        tau,
        a_io,
        e ) ->
    let a_name =
      Pos.map_under_mark
        (fun str ->
          str ^ "." ^ Pos.unmark (Ast.ScopeVar.get_info (Pos.unmark subs_var)))
        (Ast.SubScopeName.get_info (Pos.unmark subs_index))
    in
    let a_var = Dcalc.Ast.Var.make a_name in
    let tau = translate_typ ctx tau in
    let new_e =
      tag_with_log_entry (translate_expr ctx e)
        (Dcalc.Ast.VarDef (Pos.unmark tau))
        [sigma_name, pos_sigma; a_name]
    in
    let silent_var = Dcalc.Ast.Var.make ("_", Pos.no_pos) in
    let thunked_or_nonempty_new_e =
      match Pos.unmark a_io.io_input with
      | NoInput -> failwith "should not happen"
      | OnlyInput ->
        Bindlib.box_apply
          (fun new_e -> Dcalc.Ast.ErrorOnEmpty new_e, Pos.get_position subs_var)
          new_e
      | Reentrant ->
        Dcalc.Ast.make_abs
          (Array.of_list [silent_var])
          new_e var_def_pos
          [Dcalc.Ast.TLit TUnit, var_def_pos]
          var_def_pos
    in
    ( (fun next ->
        Bindlib.box_apply2
          (fun next thunked_or_nonempty_new_e ->
            Dcalc.Ast.ScopeLet
              {
                Dcalc.Ast.scope_let_next = next;
                Dcalc.Ast.scope_let_pos = Pos.get_position a_name;
                Dcalc.Ast.scope_let_typ =
                  (match Pos.unmark a_io.io_input with
                  | NoInput -> failwith "should not happen"
                  | OnlyInput -> tau
                  | Reentrant ->
                    ( Dcalc.Ast.TArrow ((TLit TUnit, var_def_pos), tau),
                      var_def_pos ));
                Dcalc.Ast.scope_let_expr = thunked_or_nonempty_new_e;
                Dcalc.Ast.scope_let_kind = Dcalc.Ast.SubScopeVarDefinition;
              })
          (Bindlib.bind_var a_var next)
          thunked_or_nonempty_new_e),
      {
        ctx with
        subscope_vars =
          Ast.SubScopeMap.update (Pos.unmark subs_index)
            (fun map ->
              match map with
              | Some map ->
                Some
                  (Ast.ScopeVarMap.add (Pos.unmark subs_var)
                     (a_var, Pos.unmark tau, a_io)
                     map)
              | None ->
                Some
                  (Ast.ScopeVarMap.singleton (Pos.unmark subs_var)
                     (a_var, Pos.unmark tau, a_io)))
            ctx.subscope_vars;
      } )
  | Call (subname, subindex) ->
    let subscope_sig = Ast.ScopeMap.find subname ctx.scopes_parameters in
    let all_subscope_vars = subscope_sig.scope_sig_local_vars in
    let all_subscope_input_vars =
      List.filter
        (fun var_ctx ->
          match Pos.unmark var_ctx.scope_var_io.Ast.io_input with
          | NoInput -> false
          | _ -> true)
        all_subscope_vars
    in
    let all_subscope_output_vars =
      List.filter
        (fun var_ctx -> Pos.unmark var_ctx.scope_var_io.Ast.io_output)
        all_subscope_vars
    in
    let scope_dcalc_var = subscope_sig.scope_sig_scope_var in
    let called_scope_input_struct = subscope_sig.scope_sig_input_struct in
    let called_scope_return_struct = subscope_sig.scope_sig_output_struct in
    let subscope_vars_defined =
      try Ast.SubScopeMap.find subindex ctx.subscope_vars
      with Not_found -> Ast.ScopeVarMap.empty
    in
    let subscope_var_not_yet_defined subvar =
      not (Ast.ScopeVarMap.mem subvar subscope_vars_defined)
    in
    let pos_call = Pos.get_position (Ast.SubScopeName.get_info subindex) in
    let subscope_args =
      List.map
        (fun (subvar : scope_var_ctx) ->
          if subscope_var_not_yet_defined subvar.scope_var_name then
            (* This is a redundant check. Normally, all subscope varaibles
               should have been defined (even an empty definition, if they're
               not defined by any rule in the source code) by the translation
               from desugared to the scope language. *)
            Bindlib.box Dcalc.Ast.empty_thunked_term
          else
            let a_var, _, _ =
              Ast.ScopeVarMap.find subvar.scope_var_name subscope_vars_defined
            in
            Dcalc.Ast.make_var (a_var, pos_call))
        all_subscope_input_vars
    in
    let subscope_struct_arg =
      Bindlib.box_apply
        (fun subscope_args ->
          ( Dcalc.Ast.ETuple (subscope_args, Some called_scope_input_struct),
            pos_call ))
        (Bindlib.box_list subscope_args)
    in
    let all_subscope_output_vars_dcalc =
      List.map
        (fun (subvar : scope_var_ctx) ->
          let sub_dcalc_var =
            Dcalc.Ast.Var.make
              (Pos.map_under_mark
                 (fun s ->
                   Pos.unmark (Ast.SubScopeName.get_info subindex) ^ "." ^ s)
                 (Ast.ScopeVar.get_info subvar.scope_var_name))
          in
          subvar, sub_dcalc_var)
        all_subscope_output_vars
    in
    let subscope_func =
      tag_with_log_entry
        (Dcalc.Ast.make_var
           ( scope_dcalc_var,
             Pos.get_position (Ast.SubScopeName.get_info subindex) ))
        Dcalc.Ast.BeginCall
        [
          sigma_name, pos_sigma;
          Ast.SubScopeName.get_info subindex;
          Ast.ScopeName.get_info subname;
        ]
    in
    let call_expr =
      tag_with_log_entry
        (Bindlib.box_apply2
           (fun e u -> Dcalc.Ast.EApp (e, [u]), Pos.no_pos)
           subscope_func subscope_struct_arg)
        Dcalc.Ast.EndCall
        [
          sigma_name, pos_sigma;
          Ast.SubScopeName.get_info subindex;
          Ast.ScopeName.get_info subname;
        ]
    in
    let result_tuple_var = Dcalc.Ast.Var.make ("result", pos_sigma) in
    let result_tuple_typ =
      ( Dcalc.Ast.TTuple
          ( List.map
              (fun (subvar, _) -> subvar.scope_var_typ, pos_sigma)
              all_subscope_output_vars_dcalc,
            Some called_scope_return_struct ),
        pos_sigma )
    in
    let call_scope_let
        (next : Dcalc.Ast.expr Dcalc.Ast.scope_body_expr Bindlib.box) =
      Bindlib.box_apply2
        (fun next call_expr ->
          Dcalc.Ast.ScopeLet
            {
              Dcalc.Ast.scope_let_next = next;
              Dcalc.Ast.scope_let_pos = pos_sigma;
              Dcalc.Ast.scope_let_kind = Dcalc.Ast.CallingSubScope;
              Dcalc.Ast.scope_let_typ = result_tuple_typ;
              Dcalc.Ast.scope_let_expr = call_expr;
            })
        (Bindlib.bind_var result_tuple_var next)
        call_expr
    in
    let result_bindings_lets
        (next : Dcalc.Ast.expr Dcalc.Ast.scope_body_expr Bindlib.box) =
      List.fold_right
        (fun (var_ctx, v) (next, i) ->
          ( Bindlib.box_apply2
              (fun next r ->
                Dcalc.Ast.ScopeLet
                  {
                    Dcalc.Ast.scope_let_next = next;
                    Dcalc.Ast.scope_let_pos = pos_sigma;
                    Dcalc.Ast.scope_let_typ = var_ctx.scope_var_typ, pos_sigma;
                    Dcalc.Ast.scope_let_kind =
                      Dcalc.Ast.DestructuringSubScopeResults;
                    Dcalc.Ast.scope_let_expr =
                      ( Dcalc.Ast.ETupleAccess
                          ( r,
                            i,
                            Some called_scope_return_struct,
                            List.map
                              (fun (var_ctx, _) ->
                                var_ctx.scope_var_typ, pos_sigma)
                              all_subscope_output_vars_dcalc ),
                        pos_sigma );
                  })
              (Bindlib.bind_var v next)
              (Dcalc.Ast.make_var (result_tuple_var, pos_sigma)),
            i - 1 ))
        all_subscope_output_vars_dcalc
        (next, List.length all_subscope_output_vars_dcalc - 1)
    in
    ( (fun next -> call_scope_let (fst (result_bindings_lets next))),
      {
        ctx with
        subscope_vars =
          Ast.SubScopeMap.add subindex
            (List.fold_left
               (fun acc (var_ctx, dvar) ->
                 Ast.ScopeVarMap.add var_ctx.scope_var_name
                   (dvar, var_ctx.scope_var_typ, var_ctx.scope_var_io)
                   acc)
               Ast.ScopeVarMap.empty all_subscope_output_vars_dcalc)
            ctx.subscope_vars;
      } )
  | Assertion e ->
    let new_e = translate_expr ctx e in
    ( (fun next ->
        Bindlib.box_apply2
          (fun next new_e ->
            Dcalc.Ast.ScopeLet
              {
                Dcalc.Ast.scope_let_next = next;
                Dcalc.Ast.scope_let_pos = Pos.get_position e;
                Dcalc.Ast.scope_let_typ =
                  Dcalc.Ast.TLit TUnit, Pos.get_position e;
                Dcalc.Ast.scope_let_expr =
                  (* To ensure that we throw an error if the value is not
                     defined, we add an check "ErrorOnEmpty" here. *)
                  Pos.same_pos_as
                    (Dcalc.Ast.EAssert
                       (Dcalc.Ast.ErrorOnEmpty new_e, Pos.get_position e))
                    new_e;
                Dcalc.Ast.scope_let_kind = Dcalc.Ast.Assertion;
              })
          (Bindlib.bind_var (Dcalc.Ast.Var.make ("_", Pos.get_position e)) next)
          new_e),
      ctx )

let translate_rules
    (ctx : ctx)
    (rules : Ast.rule list)
    ((sigma_name, pos_sigma) : Utils.Uid.MarkedString.info)
    (sigma_return_struct_name : Ast.StructName.t) :
    Dcalc.Ast.expr Dcalc.Ast.scope_body_expr Bindlib.box * ctx =
  let scope_lets, new_ctx =
    List.fold_left
      (fun (scope_lets, ctx) rule ->
        let new_scope_lets, new_ctx =
          translate_rule ctx rule (sigma_name, pos_sigma)
        in
        (fun next -> scope_lets (new_scope_lets next)), new_ctx)
      ((fun next -> next), ctx)
      rules
  in
  let scope_variables = Ast.ScopeVarMap.bindings new_ctx.scope_vars in
  let scope_output_variables =
    List.filter
      (fun (_, (_, _, io)) -> Pos.unmark io.Ast.io_output)
      scope_variables
  in
  let return_exp =
    Bindlib.box_apply
      (fun args ->
        Dcalc.Ast.ETuple (args, Some sigma_return_struct_name), pos_sigma)
      (Bindlib.box_list
         (List.map
            (fun (_, (dcalc_var, _, _)) ->
              Dcalc.Ast.make_var (dcalc_var, pos_sigma))
            scope_output_variables))
  in
  ( scope_lets
      (Bindlib.box_apply
         (fun return_exp -> Dcalc.Ast.Result return_exp)
         return_exp),
    new_ctx )

let translate_scope_decl
    (struct_ctx : Ast.struct_ctx)
    (enum_ctx : Ast.enum_ctx)
    (sctx : scope_sigs_ctx)
    (scope_name : Ast.ScopeName.t)
    (sigma : Ast.scope_decl) :
    Dcalc.Ast.expr Dcalc.Ast.scope_body Bindlib.box * Dcalc.Ast.struct_ctx =
  let sigma_info = Ast.ScopeName.get_info sigma.scope_decl_name in
  let scope_sig = Ast.ScopeMap.find sigma.scope_decl_name sctx in
  let scope_variables = scope_sig.scope_sig_local_vars in
  let ctx =
    (* the context must be initialized for fresh variables for all only-input
       scope variables *)
    List.fold_left
      (fun ctx scope_var ->
        match Pos.unmark scope_var.scope_var_io.io_input with
        | OnlyInput ->
          let scope_var_name = Ast.ScopeVar.get_info scope_var.scope_var_name in
          let scope_var_dcalc = Dcalc.Ast.Var.make scope_var_name in
          {
            ctx with
            scope_vars =
              Ast.ScopeVarMap.add scope_var.scope_var_name
                ( scope_var_dcalc,
                  scope_var.scope_var_typ,
                  scope_var.scope_var_io )
                ctx.scope_vars;
          }
        | _ -> ctx)
      (empty_ctx struct_ctx enum_ctx sctx scope_name)
      scope_variables
  in
  let scope_input_var = scope_sig.scope_sig_input_var in
  let scope_input_struct_name = scope_sig.scope_sig_input_struct in
  let scope_return_struct_name = scope_sig.scope_sig_output_struct in
  let pos_sigma = Pos.get_position sigma_info in
  let rules_with_return_expr, ctx =
    translate_rules ctx sigma.scope_decl_rules sigma_info
      scope_return_struct_name
  in
  let scope_variables =
    List.map
      (fun var_ctx ->
        let dcalc_x, _, _ =
          Ast.ScopeVarMap.find var_ctx.scope_var_name ctx.scope_vars
        in
        var_ctx, dcalc_x)
      scope_variables
  in
  (* first we create variables from the fields of the input struct *)
  let scope_input_variables =
    List.filter
      (fun (var_ctx, _) ->
        match Pos.unmark var_ctx.scope_var_io.io_input with
        | NoInput -> false
        | _ -> true)
      scope_variables
  in
  let scope_output_variables =
    List.filter
      (fun (var_ctx, _) -> Pos.unmark var_ctx.scope_var_io.io_output)
      scope_variables
  in
  let input_var_typ (var_ctx : scope_var_ctx) =
    match Pos.unmark var_ctx.scope_var_io.io_input with
    | OnlyInput -> var_ctx.scope_var_typ, pos_sigma
    | Reentrant ->
      ( Dcalc.Ast.TArrow
          ((Dcalc.Ast.TLit TUnit, pos_sigma), (var_ctx.scope_var_typ, pos_sigma)),
        pos_sigma )
    | NoInput -> failwith "should not happen"
  in
  let input_destructurings
      (next : Dcalc.Ast.expr Dcalc.Ast.scope_body_expr Bindlib.box) =
    fst
      (List.fold_right
         (fun (var_ctx, v) (next, i) ->
           ( Bindlib.box_apply2
               (fun next r ->
                 Dcalc.Ast.ScopeLet
                   {
                     Dcalc.Ast.scope_let_kind =
                       Dcalc.Ast.DestructuringInputStruct;
                     Dcalc.Ast.scope_let_next = next;
                     Dcalc.Ast.scope_let_pos = pos_sigma;
                     Dcalc.Ast.scope_let_typ = input_var_typ var_ctx;
                     Dcalc.Ast.scope_let_expr =
                       ( Dcalc.Ast.ETupleAccess
                           ( r,
                             i,
                             Some scope_input_struct_name,
                             List.map
                               (fun (var_ctx, _) -> input_var_typ var_ctx)
                               scope_input_variables ),
                         pos_sigma );
                   })
               (Bindlib.bind_var v next)
               (Dcalc.Ast.make_var (scope_input_var, pos_sigma)),
             i - 1 ))
         scope_input_variables
         (next, List.length scope_input_variables - 1))
  in
  let scope_return_struct_fields =
    List.map
      (fun (var_ctx, dvar) ->
        let struct_field_name =
          Ast.StructFieldName.fresh (Bindlib.name_of dvar ^ "_out", pos_sigma)
        in
        struct_field_name, (var_ctx.scope_var_typ, pos_sigma))
      scope_output_variables
  in
  let scope_input_struct_fields =
    List.map
      (fun (var_ctx, dvar) ->
        let struct_field_name =
          Ast.StructFieldName.fresh (Bindlib.name_of dvar ^ "_in", pos_sigma)
        in
        struct_field_name, input_var_typ var_ctx)
      scope_input_variables
  in
  let new_struct_ctx =
    Ast.StructMap.add scope_input_struct_name scope_input_struct_fields
      (Ast.StructMap.singleton scope_return_struct_name
         scope_return_struct_fields)
  in
  ( Bindlib.box_apply
      (fun scope_body_expr ->
        {
          Dcalc.Ast.scope_body_expr;
          Dcalc.Ast.scope_body_input_struct = scope_input_struct_name;
          Dcalc.Ast.scope_body_output_struct = scope_return_struct_name;
        })
      (Bindlib.bind_var scope_input_var
         (input_destructurings rules_with_return_expr)),
    new_struct_ctx )

let translate_program (prgm : Ast.program) :
    Dcalc.Ast.program * Dependency.TVertex.t list =
  let scope_dependencies = Dependency.build_program_dep_graph prgm in
  Dependency.check_for_cycle_in_scope scope_dependencies;
  let types_ordering =
    Dependency.check_type_cycles prgm.program_structs prgm.program_enums
  in
  let scope_ordering = Dependency.get_scope_ordering scope_dependencies in
  let struct_ctx = prgm.program_structs in
  let enum_ctx = prgm.program_enums in
  let ctx_for_typ_translation scope_name =
    empty_ctx struct_ctx enum_ctx Ast.ScopeMap.empty scope_name
  in
  let dummy_scope = Ast.ScopeName.fresh ("dummy", Pos.no_pos) in
  let decl_ctx =
    {
      Dcalc.Ast.ctx_structs =
        Ast.StructMap.map
          (List.map (fun (x, y) ->
               x, translate_typ (ctx_for_typ_translation dummy_scope) y))
          struct_ctx;
      Dcalc.Ast.ctx_enums =
        Ast.EnumMap.map
          (List.map (fun (x, y) ->
               x, (translate_typ (ctx_for_typ_translation dummy_scope)) y))
          enum_ctx;
    }
  in
  let sctx : scope_sigs_ctx =
    Ast.ScopeMap.mapi
      (fun scope_name scope ->
        let scope_dvar =
          Dcalc.Ast.Var.make (Ast.ScopeName.get_info scope.Ast.scope_decl_name)
        in
        let scope_return_struct_name =
          Ast.StructName.fresh
            (Pos.map_under_mark
               (fun s -> s ^ "_out")
               (Ast.ScopeName.get_info scope_name))
        in
        let scope_input_var =
          Dcalc.Ast.Var.make
            (Pos.map_under_mark
               (fun s -> s ^ "_in")
               (Ast.ScopeName.get_info scope_name))
        in
        let scope_input_struct_name =
          Ast.StructName.fresh
            (Pos.map_under_mark
               (fun s -> s ^ "_in")
               (Ast.ScopeName.get_info scope_name))
        in
        {
          scope_sig_local_vars =
            List.map
              (fun (scope_var, (tau, vis)) ->
                let tau =
                  translate_typ (ctx_for_typ_translation scope_name) tau
                in
                {
                  scope_var_name = scope_var;
                  scope_var_typ = Pos.unmark tau;
                  scope_var_io = vis;
                })
              (Ast.ScopeVarMap.bindings scope.scope_sig);
          scope_sig_scope_var = scope_dvar;
          scope_sig_input_var = scope_input_var;
          scope_sig_input_struct = scope_input_struct_name;
          scope_sig_output_struct = scope_return_struct_name;
        })
      prgm.program_scopes
  in
  (* the resulting expression is the list of definitions of all the scopes,
     ending with the top-level scope. *)
  let (scopes, decl_ctx) : Dcalc.Ast.expr Dcalc.Ast.scopes Bindlib.box * _ =
    List.fold_right
      (fun scope_name (scopes, decl_ctx) ->
        let scope = Ast.ScopeMap.find scope_name prgm.program_scopes in
        let scope_body, scope_out_struct =
          translate_scope_decl struct_ctx enum_ctx sctx scope_name scope
        in
        let dvar = (Ast.ScopeMap.find scope_name sctx).scope_sig_scope_var in
        let decl_ctx =
          {
            decl_ctx with
            Dcalc.Ast.ctx_structs =
              Ast.StructMap.union
                (fun _ _ -> assert false (* should not happen *))
                decl_ctx.Dcalc.Ast.ctx_structs scope_out_struct;
          }
        in
        let scope_next = Bindlib.bind_var dvar scopes in
        let new_scopes =
          Bindlib.box_apply2
            (fun scope_body scope_next ->
              Dcalc.Ast.ScopeDef { scope_name; scope_body; scope_next })
            scope_body scope_next
        in
        new_scopes, decl_ctx)
      scope_ordering
      (Bindlib.box Dcalc.Ast.Nil, decl_ctx)
  in
  { scopes = Bindlib.unbox scopes; decl_ctx }, types_ordering
