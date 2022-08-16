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
open Shared_ast

type scope_var_ctx = {
  scope_var_name : Ast.ScopeVar.t;
  scope_var_typ : typ;
  scope_var_io : Ast.io;
}

type scope_sig_ctx = {
  scope_sig_local_vars : scope_var_ctx list;  (** List of scope variables *)
  scope_sig_scope_var : untyped Dcalc.Ast.var;
      (** Var representing the scope *)
  scope_sig_input_var : untyped Dcalc.Ast.var;
      (** Var representing the scope input inside the scope func *)
  scope_sig_input_struct : StructName.t;  (** Scope input *)
  scope_sig_output_struct : StructName.t;  (** Scope output *)
}

type scope_sigs_ctx = scope_sig_ctx Ast.ScopeMap.t

type ctx = {
  structs : Ast.struct_ctx;
  enums : Ast.enum_ctx;
  scope_name : ScopeName.t;
  scopes_parameters : scope_sigs_ctx;
  scope_vars : (untyped Dcalc.Ast.var * typ * Ast.io) Ast.ScopeVarMap.t;
  subscope_vars :
    (untyped Dcalc.Ast.var * typ * Ast.io) Ast.ScopeVarMap.t Ast.SubScopeMap.t;
  local_vars : untyped Dcalc.Ast.var Ast.VarMap.t;
}

let empty_ctx
    (struct_ctx : Ast.struct_ctx)
    (enum_ctx : Ast.enum_ctx)
    (scopes_ctx : scope_sigs_ctx)
    (scope_name : ScopeName.t) =
  {
    structs = struct_ctx;
    enums = enum_ctx;
    scope_name;
    scopes_parameters = scopes_ctx;
    scope_vars = Ast.ScopeVarMap.empty;
    subscope_vars = Ast.SubScopeMap.empty;
    local_vars = Ast.VarMap.empty;
  }

let rec translate_typ (ctx : ctx) (t : Ast.typ Marked.pos) : typ Marked.pos =
  Marked.same_mark_as
    (match Marked.unmark t with
    | Ast.TLit l -> TLit l
    | Ast.TArrow (t1, t2) -> TArrow (translate_typ ctx t1, translate_typ ctx t2)
    | Ast.TStruct s_uid ->
      let s_fields = StructMap.find s_uid ctx.structs in
      TTuple (List.map (fun (_, t) -> translate_typ ctx t) s_fields, Some s_uid)
    | Ast.TEnum e_uid ->
      let e_cases = EnumMap.find e_uid ctx.enums in
      TEnum (List.map (fun (_, t) -> translate_typ ctx t) e_cases, e_uid)
    | Ast.TArray t1 -> TArray (translate_typ ctx (Marked.same_mark_as t1 t))
    | Ast.TAny -> TAny)
    t

let pos_mark (pos : Pos.t) : untyped mark = Untyped { pos }
let pos_mark_as e = pos_mark (Marked.get_mark e)

let merge_defaults
    (caller : untyped Dcalc.Ast.marked_expr Bindlib.box)
    (callee : untyped Dcalc.Ast.marked_expr Bindlib.box) :
    untyped Dcalc.Ast.marked_expr Bindlib.box =
  let caller =
    let m = Marked.get_mark (Bindlib.unbox caller) in
    Dcalc.Ast.make_app caller [Bindlib.box (ELit LUnit, m)] m
  in
  let body =
    Bindlib.box_apply2
      (fun caller callee ->
        let m = Marked.get_mark callee in
        EDefault ([caller], (ELit (LBool true), m), callee), m)
      caller callee
  in
  body

let tag_with_log_entry
    (e : untyped Dcalc.Ast.marked_expr Bindlib.box)
    (l : log_entry)
    (markings : Utils.Uid.MarkedString.info list) :
    untyped Dcalc.Ast.marked_expr Bindlib.box =
  Bindlib.box_apply
    (fun e ->
      Marked.same_mark_as
        (EApp (Marked.same_mark_as (EOp (Unop (Log (l, markings)))) e, [e]))
        e)
    e

(* In a list of exceptions, it is normally an error if more than a single one
   apply at the same time. This relaxes this constraint slightly, allowing a
   conflict if all the triggered conflicting exception yield syntactically equal
   results (and as long as none of these exceptions have exceptions themselves)

   NOTE: the choice of the exception that will be triggered and show in the
   trace is arbitrary (but deterministic). *)
let collapse_similar_outcomes (excepts : Ast.expr Marked.pos list) :
    Ast.expr Marked.pos list =
  let cons_map =
    List.fold_left
      (fun map -> function
        | (Ast.EDefault ([], _, (cons, _)), _) as e ->
          Ast.ExprMap.update cons
            (fun prev -> Some (e :: Option.value ~default:[] prev))
            map
        | _ -> map)
      Ast.ExprMap.empty excepts
  in
  let _, excepts =
    List.fold_right
      (fun e (cons_map, excepts) ->
        match e with
        | Ast.EDefault ([], _, (cons, _)), _ ->
          let collapsed_exc =
            List.fold_left
              (fun acc -> function
                | Ast.EDefault ([], just, cons), pos ->
                  [Ast.EDefault (acc, just, cons), pos]
                | _ -> assert false)
              []
              (Ast.ExprMap.find cons cons_map)
          in
          Ast.ExprMap.add cons [] cons_map, collapsed_exc @ excepts
        | e -> cons_map, e :: excepts)
      excepts (cons_map, [])
  in
  excepts

let rec translate_expr (ctx : ctx) (e : Ast.expr Marked.pos) :
    untyped Dcalc.Ast.marked_expr Bindlib.box =
  Bindlib.box_apply (fun (x : untyped Dcalc.Ast.expr) ->
      Marked.mark (pos_mark_as e) x)
  @@
  match Marked.unmark e with
  | EVar v -> Bindlib.box_var (Ast.VarMap.find v ctx.local_vars)
  | ELit l -> Bindlib.box (ELit l)
  | EStruct (struct_name, e_fields) ->
    let struct_sig = StructMap.find struct_name ctx.structs in
    let d_fields, remaining_e_fields =
      List.fold_right
        (fun (field_name, _) (d_fields, e_fields) ->
          let field_e = Ast.StructFieldMap.find field_name e_fields in
          let field_d = translate_expr ctx field_e in
          field_d :: d_fields, Ast.StructFieldMap.remove field_name e_fields)
        struct_sig ([], e_fields)
    in
    if Ast.StructFieldMap.cardinal remaining_e_fields > 0 then
      Errors.raise_spanned_error (Marked.get_mark e)
        "The fields \"%a\" do not belong to the structure %a"
        StructName.format_t struct_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (field_name, _) ->
             Format.fprintf fmt "%a" StructFieldName.format_t field_name))
        (Ast.StructFieldMap.bindings remaining_e_fields)
    else
      Bindlib.box_apply
        (fun d_fields -> ETuple (d_fields, Some struct_name))
        (Bindlib.box_list d_fields)
  | EStructAccess (e1, field_name, struct_name) ->
    let struct_sig = StructMap.find struct_name ctx.structs in
    let _, field_index =
      try
        List.assoc field_name (List.mapi (fun i (x, y) -> x, (y, i)) struct_sig)
      with Not_found ->
        Errors.raise_spanned_error (Marked.get_mark e)
          "The field \"%a\" does not belong to the structure %a"
          StructFieldName.format_t field_name StructName.format_t struct_name
    in
    let e1 = translate_expr ctx e1 in
    Bindlib.box_apply
      (fun e1 ->
        ETupleAccess
          ( e1,
            field_index,
            Some struct_name,
            List.map (fun (_, t) -> translate_typ ctx t) struct_sig ))
      e1
  | EEnumInj (e1, constructor, enum_name) ->
    let enum_sig = EnumMap.find enum_name ctx.enums in
    let _, constructor_index =
      try
        List.assoc constructor (List.mapi (fun i (x, y) -> x, (y, i)) enum_sig)
      with Not_found ->
        Errors.raise_spanned_error (Marked.get_mark e)
          "The constructor \"%a\" does not belong to the enum %a"
          EnumConstructor.format_t constructor EnumName.format_t enum_name
    in
    let e1 = translate_expr ctx e1 in
    Bindlib.box_apply
      (fun e1 ->
        EInj
          ( e1,
            constructor_index,
            enum_name,
            List.map (fun (_, t) -> translate_typ ctx t) enum_sig ))
      e1
  | EMatch (e1, enum_name, cases) ->
    let enum_sig = EnumMap.find enum_name ctx.enums in
    let d_cases, remaining_e_cases =
      List.fold_right
        (fun (constructor, _) (d_cases, e_cases) ->
          let case_e =
            try Ast.EnumConstructorMap.find constructor e_cases
            with Not_found ->
              Errors.raise_spanned_error (Marked.get_mark e)
                "The constructor %a of enum %a is missing from this pattern \
                 matching"
                EnumConstructor.format_t constructor EnumName.format_t enum_name
          in
          let case_d = translate_expr ctx case_e in
          case_d :: d_cases, Ast.EnumConstructorMap.remove constructor e_cases)
        enum_sig ([], cases)
    in
    if Ast.EnumConstructorMap.cardinal remaining_e_cases > 0 then
      Errors.raise_spanned_error (Marked.get_mark e)
        "Patter matching is incomplete for enum %a: missing cases %a"
        EnumName.format_t enum_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (case_name, _) ->
             Format.fprintf fmt "%a" EnumConstructor.format_t case_name))
        (Ast.EnumConstructorMap.bindings remaining_e_cases)
    else
      let e1 = translate_expr ctx e1 in
      Bindlib.box_apply2
        (fun d_fields e1 -> EMatch (e1, d_fields, enum_name))
        (Bindlib.box_list d_cases) e1
  | EApp (e1, args) ->
    (* We insert various log calls to record arguments and outputs of
       user-defined functions belonging to scopes *)
    let e1_func = translate_expr ctx e1 in
    let markings l =
      match l with
      | Ast.ScopeVar (v, _) ->
        [ScopeName.get_info ctx.scope_name; Ast.ScopeVar.get_info v]
      | Ast.SubScopeVar (s, _, (v, _)) ->
        [ScopeName.get_info s; Ast.ScopeVar.get_info v]
    in
    let e1_func =
      match Marked.unmark e1 with
      | ELocation l -> tag_with_log_entry e1_func BeginCall (markings l)
      | _ -> e1_func
    in
    let new_args = List.map (translate_expr ctx) args in
    let input_typ, output_typ =
      (* NOTE: this is a temporary solution, it works because it's assume that
         all function calls are from scope variable. However, this will change
         -- for more information see
         https://github.com/CatalaLang/catala/pull/280#discussion_r898851693. *)
      let retrieve_in_and_out_typ_or_any var vars =
        let _, typ, _ = Ast.ScopeVarMap.find (Marked.unmark var) vars in
        match typ with
        | TArrow (marked_input_typ, marked_output_typ) ->
          Marked.unmark marked_input_typ, Marked.unmark marked_output_typ
        | _ -> TAny, TAny
      in
      match Marked.unmark e1 with
      | ELocation (ScopeVar var) ->
        retrieve_in_and_out_typ_or_any var ctx.scope_vars
      | ELocation (SubScopeVar (_, sname, var)) ->
        ctx.subscope_vars
        |> Ast.SubScopeMap.find (Marked.unmark sname)
        |> retrieve_in_and_out_typ_or_any var
      | _ -> TAny, TAny
    in
    let new_args =
      match Marked.unmark e1, new_args with
      | ELocation l, [new_arg] ->
        [
          tag_with_log_entry new_arg (VarDef input_typ)
            (markings l @ [Marked.same_mark_as "input" e]);
        ]
      | _ -> new_args
    in
    let new_e =
      Bindlib.box_apply2
        (fun e' u -> EApp (e', u), pos_mark_as e)
        e1_func
        (Bindlib.box_list new_args)
    in
    let new_e =
      match Marked.unmark e1 with
      | ELocation l ->
        tag_with_log_entry
          (tag_with_log_entry new_e (VarDef output_typ)
             (markings l @ [Marked.same_mark_as "output" e]))
          EndCall (markings l)
      | _ -> new_e
    in
    Bindlib.box_apply Marked.unmark new_e
  | EAbs (binder, typ) ->
    let xs, body = Bindlib.unmbind binder in
    let new_xs = Array.map (fun x -> Var.make (Bindlib.name_of x)) xs in
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
      (fun b -> EAbs (b, List.map (translate_typ ctx) typ))
      binder
  | EDefault (excepts, just, cons) ->
    let excepts = collapse_similar_outcomes excepts in
    Bindlib.box_apply3
      (fun e j c -> EDefault (e, j, c))
      (Bindlib.box_list (List.map (translate_expr ctx) excepts))
      (translate_expr ctx just) (translate_expr ctx cons)
  | ELocation (ScopeVar a) ->
    let v, _, _ = Ast.ScopeVarMap.find (Marked.unmark a) ctx.scope_vars in
    Bindlib.box_var v
  | ELocation (SubScopeVar (_, s, a)) -> (
    try
      let v, _, _ =
        Ast.ScopeVarMap.find (Marked.unmark a)
          (Ast.SubScopeMap.find (Marked.unmark s) ctx.subscope_vars)
      in
      Bindlib.box_var v
    with Not_found ->
      Errors.raise_multispanned_error
        [
          Some "Incriminated variable usage:", Marked.get_mark e;
          ( Some "Incriminated subscope variable declaration:",
            Marked.get_mark (Ast.ScopeVar.get_info (Marked.unmark a)) );
          ( Some "Incriminated subscope declaration:",
            Marked.get_mark (Ast.SubScopeName.get_info (Marked.unmark s)) );
        ]
        "The variable %a.%a cannot be used here, as it is not part subscope \
         %a's results. Maybe you forgot to qualify it as an output?"
        Ast.SubScopeName.format_t (Marked.unmark s) Ast.ScopeVar.format_t
        (Marked.unmark a) Ast.SubScopeName.format_t (Marked.unmark s))
  | EIfThenElse (cond, et, ef) ->
    Bindlib.box_apply3
      (fun c t f -> EIfThenElse (c, t, f))
      (translate_expr ctx cond) (translate_expr ctx et) (translate_expr ctx ef)
  | EOp op -> Bindlib.box (EOp op)
  | ErrorOnEmpty e' ->
    Bindlib.box_apply (fun e' -> ErrorOnEmpty e') (translate_expr ctx e')
  | EArray es ->
    Bindlib.box_apply
      (fun es -> EArray es)
      (Bindlib.box_list (List.map (translate_expr ctx) es))

(** The result of a rule translation is a list of assignment, with variables and
    expressions. We also return the new translation context available after the
    assignment to use in later rule translations. The list is actually a
    continuation yielding a [Dcalc.scope_body_expr] by giving it what should
    come later in the chain of let-bindings. *)
let translate_rule
    (ctx : ctx)
    (rule : Ast.rule)
    ((sigma_name, pos_sigma) : Utils.Uid.MarkedString.info) :
    ((untyped Dcalc.Ast.expr, untyped) scope_body_expr Bindlib.box ->
    (untyped Dcalc.Ast.expr, untyped) scope_body_expr Bindlib.box)
    * ctx =
  match rule with
  | Definition ((ScopeVar a, var_def_pos), tau, a_io, e) ->
    let a_name = Ast.ScopeVar.get_info (Marked.unmark a) in
    let a_var = Var.make (Marked.unmark a_name) in
    let tau = translate_typ ctx tau in
    let new_e = translate_expr ctx e in
    let a_expr = Dcalc.Ast.make_var (a_var, pos_mark var_def_pos) in
    let merged_expr =
      Bindlib.box_apply
        (fun merged_expr -> ErrorOnEmpty merged_expr, pos_mark_as a_name)
        (match Marked.unmark a_io.io_input with
        | OnlyInput ->
          failwith "should not happen"
          (* scopelang should not contain any definitions of input only
             variables *)
        | Reentrant -> merge_defaults a_expr new_e
        | NoInput -> new_e)
    in
    let merged_expr =
      tag_with_log_entry merged_expr
        (VarDef (Marked.unmark tau))
        [sigma_name, pos_sigma; a_name]
    in
    ( (fun next ->
        Bindlib.box_apply2
          (fun next merged_expr ->
            ScopeLet
              {
                scope_let_next = next;
                scope_let_typ = tau;
                scope_let_expr = merged_expr;
                scope_let_kind = ScopeVarDefinition;
                scope_let_pos = Marked.get_mark a;
              })
          (Bindlib.bind_var a_var next)
          merged_expr),
      {
        ctx with
        scope_vars =
          Ast.ScopeVarMap.add (Marked.unmark a)
            (a_var, Marked.unmark tau, a_io)
            ctx.scope_vars;
      } )
  | Definition
      ( (SubScopeVar (_subs_name, subs_index, subs_var), var_def_pos),
        tau,
        a_io,
        e ) ->
    let a_name =
      Marked.map_under_mark
        (fun str ->
          str
          ^ "."
          ^ Marked.unmark (Ast.ScopeVar.get_info (Marked.unmark subs_var)))
        (Ast.SubScopeName.get_info (Marked.unmark subs_index))
    in
    let a_var = Var.make (Marked.unmark a_name) in
    let tau = translate_typ ctx tau in
    let new_e =
      tag_with_log_entry (translate_expr ctx e)
        (VarDef (Marked.unmark tau))
        [sigma_name, pos_sigma; a_name]
    in
    let silent_var = Var.make "_" in
    let thunked_or_nonempty_new_e =
      match Marked.unmark a_io.io_input with
      | NoInput -> failwith "should not happen"
      | OnlyInput ->
        Bindlib.box_apply
          (fun new_e -> ErrorOnEmpty new_e, pos_mark_as subs_var)
          new_e
      | Reentrant ->
        Dcalc.Ast.make_abs
          (Array.of_list [silent_var])
          new_e
          [TLit TUnit, var_def_pos]
          (pos_mark var_def_pos)
    in
    ( (fun next ->
        Bindlib.box_apply2
          (fun next thunked_or_nonempty_new_e ->
            ScopeLet
              {
                scope_let_next = next;
                scope_let_pos = Marked.get_mark a_name;
                scope_let_typ =
                  (match Marked.unmark a_io.io_input with
                  | NoInput -> failwith "should not happen"
                  | OnlyInput -> tau
                  | Reentrant ->
                    TArrow ((TLit TUnit, var_def_pos), tau), var_def_pos);
                scope_let_expr = thunked_or_nonempty_new_e;
                scope_let_kind = SubScopeVarDefinition;
              })
          (Bindlib.bind_var a_var next)
          thunked_or_nonempty_new_e),
      {
        ctx with
        subscope_vars =
          Ast.SubScopeMap.update (Marked.unmark subs_index)
            (fun map ->
              match map with
              | Some map ->
                Some
                  (Ast.ScopeVarMap.add (Marked.unmark subs_var)
                     (a_var, Marked.unmark tau, a_io)
                     map)
              | None ->
                Some
                  (Ast.ScopeVarMap.singleton (Marked.unmark subs_var)
                     (a_var, Marked.unmark tau, a_io)))
            ctx.subscope_vars;
      } )
  | Call (subname, subindex) ->
    let subscope_sig = Ast.ScopeMap.find subname ctx.scopes_parameters in
    let all_subscope_vars = subscope_sig.scope_sig_local_vars in
    let all_subscope_input_vars =
      List.filter
        (fun var_ctx ->
          match Marked.unmark var_ctx.scope_var_io.Ast.io_input with
          | NoInput -> false
          | _ -> true)
        all_subscope_vars
    in
    let all_subscope_output_vars =
      List.filter
        (fun var_ctx -> Marked.unmark var_ctx.scope_var_io.Ast.io_output)
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
    let pos_call = Marked.get_mark (Ast.SubScopeName.get_info subindex) in
    let subscope_args =
      List.map
        (fun (subvar : scope_var_ctx) ->
          if subscope_var_not_yet_defined subvar.scope_var_name then
            (* This is a redundant check. Normally, all subscope variables
               should have been defined (even an empty definition, if they're
               not defined by any rule in the source code) by the translation
               from desugared to the scope language. *)
            Bindlib.box
              (Dcalc.Ast.empty_thunked_term (Untyped { pos = pos_call }))
          else
            let a_var, _, _ =
              Ast.ScopeVarMap.find subvar.scope_var_name subscope_vars_defined
            in
            Dcalc.Ast.make_var (a_var, pos_mark pos_call))
        all_subscope_input_vars
    in
    let subscope_struct_arg =
      Bindlib.box_apply
        (fun subscope_args ->
          ( ETuple (subscope_args, Some called_scope_input_struct),
            pos_mark pos_call ))
        (Bindlib.box_list subscope_args)
    in
    let all_subscope_output_vars_dcalc =
      List.map
        (fun (subvar : scope_var_ctx) ->
          let sub_dcalc_var =
            Var.make
              (Marked.unmark (Ast.SubScopeName.get_info subindex)
              ^ "."
              ^ Marked.unmark (Ast.ScopeVar.get_info subvar.scope_var_name))
          in
          subvar, sub_dcalc_var)
        all_subscope_output_vars
    in
    let subscope_func =
      tag_with_log_entry
        (Dcalc.Ast.make_var
           (scope_dcalc_var, pos_mark_as (Ast.SubScopeName.get_info subindex)))
        BeginCall
        [
          sigma_name, pos_sigma;
          Ast.SubScopeName.get_info subindex;
          ScopeName.get_info subname;
        ]
    in
    let call_expr =
      tag_with_log_entry
        (Bindlib.box_apply2
           (fun e u -> EApp (e, [u]), pos_mark Pos.no_pos)
           subscope_func subscope_struct_arg)
        EndCall
        [
          sigma_name, pos_sigma;
          Ast.SubScopeName.get_info subindex;
          ScopeName.get_info subname;
        ]
    in
    let result_tuple_var = Var.make "result" in
    let result_tuple_typ =
      ( TTuple
          ( List.map
              (fun (subvar, _) -> subvar.scope_var_typ, pos_sigma)
              all_subscope_output_vars_dcalc,
            Some called_scope_return_struct ),
        pos_sigma )
    in
    let call_scope_let next =
      Bindlib.box_apply2
        (fun next call_expr ->
          ScopeLet
            {
              scope_let_next = next;
              scope_let_pos = pos_sigma;
              scope_let_kind = CallingSubScope;
              scope_let_typ = result_tuple_typ;
              scope_let_expr = call_expr;
            })
        (Bindlib.bind_var result_tuple_var next)
        call_expr
    in
    let result_bindings_lets next =
      List.fold_right
        (fun (var_ctx, v) (next, i) ->
          ( Bindlib.box_apply2
              (fun next r ->
                ScopeLet
                  {
                    scope_let_next = next;
                    scope_let_pos = pos_sigma;
                    scope_let_typ = var_ctx.scope_var_typ, pos_sigma;
                    scope_let_kind = DestructuringSubScopeResults;
                    scope_let_expr =
                      ( ETupleAccess
                          ( r,
                            i,
                            Some called_scope_return_struct,
                            List.map
                              (fun (var_ctx, _) ->
                                var_ctx.scope_var_typ, pos_sigma)
                              all_subscope_output_vars_dcalc ),
                        pos_mark pos_sigma );
                  })
              (Bindlib.bind_var v next)
              (Dcalc.Ast.make_var (result_tuple_var, pos_mark pos_sigma)),
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
            ScopeLet
              {
                scope_let_next = next;
                scope_let_pos = Marked.get_mark e;
                scope_let_typ = TLit TUnit, Marked.get_mark e;
                scope_let_expr =
                  (* To ensure that we throw an error if the value is not
                     defined, we add an check "ErrorOnEmpty" here. *)
                  Marked.same_mark_as
                    (EAssert (ErrorOnEmpty new_e, pos_mark_as e))
                    new_e;
                scope_let_kind = Assertion;
              })
          (Bindlib.bind_var (Var.make "_") next)
          new_e),
      ctx )

let translate_rules
    (ctx : ctx)
    (rules : Ast.rule list)
    ((sigma_name, pos_sigma) : Utils.Uid.MarkedString.info)
    (sigma_return_struct_name : StructName.t) :
    (untyped Dcalc.Ast.expr, untyped) scope_body_expr Bindlib.box * ctx =
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
      (fun (_, (_, _, io)) -> Marked.unmark io.Ast.io_output)
      scope_variables
  in
  let return_exp =
    Bindlib.box_apply
      (fun args ->
        ETuple (args, Some sigma_return_struct_name), pos_mark pos_sigma)
      (Bindlib.box_list
         (List.map
            (fun (_, (dcalc_var, _, _)) ->
              Dcalc.Ast.make_var (dcalc_var, pos_mark pos_sigma))
            scope_output_variables))
  in
  ( scope_lets
      (Bindlib.box_apply (fun return_exp -> Result return_exp) return_exp),
    new_ctx )

let translate_scope_decl
    (struct_ctx : Ast.struct_ctx)
    (enum_ctx : Ast.enum_ctx)
    (sctx : scope_sigs_ctx)
    (scope_name : ScopeName.t)
    (sigma : Ast.scope_decl) :
    (untyped Dcalc.Ast.expr, untyped) scope_body Bindlib.box * struct_ctx =
  let sigma_info = ScopeName.get_info sigma.scope_decl_name in
  let scope_sig = Ast.ScopeMap.find sigma.scope_decl_name sctx in
  let scope_variables = scope_sig.scope_sig_local_vars in
  let ctx =
    (* the context must be initialized for fresh variables for all only-input
       scope variables *)
    List.fold_left
      (fun ctx scope_var ->
        match Marked.unmark scope_var.scope_var_io.io_input with
        | OnlyInput ->
          let scope_var_name = Ast.ScopeVar.get_info scope_var.scope_var_name in
          let scope_var_dcalc = Var.make (Marked.unmark scope_var_name) in
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
  let pos_sigma = Marked.get_mark sigma_info in
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
        match Marked.unmark var_ctx.scope_var_io.io_input with
        | NoInput -> false
        | _ -> true)
      scope_variables
  in
  let scope_output_variables =
    List.filter
      (fun (var_ctx, _) -> Marked.unmark var_ctx.scope_var_io.io_output)
      scope_variables
  in
  let input_var_typ (var_ctx : scope_var_ctx) =
    match Marked.unmark var_ctx.scope_var_io.io_input with
    | OnlyInput -> var_ctx.scope_var_typ, pos_sigma
    | Reentrant ->
      ( TArrow ((TLit TUnit, pos_sigma), (var_ctx.scope_var_typ, pos_sigma)),
        pos_sigma )
    | NoInput -> failwith "should not happen"
  in
  let input_destructurings next =
    fst
      (List.fold_right
         (fun (var_ctx, v) (next, i) ->
           ( Bindlib.box_apply2
               (fun next r ->
                 ScopeLet
                   {
                     scope_let_kind = DestructuringInputStruct;
                     scope_let_next = next;
                     scope_let_pos = pos_sigma;
                     scope_let_typ = input_var_typ var_ctx;
                     scope_let_expr =
                       ( ETupleAccess
                           ( r,
                             i,
                             Some scope_input_struct_name,
                             List.map
                               (fun (var_ctx, _) -> input_var_typ var_ctx)
                               scope_input_variables ),
                         pos_mark pos_sigma );
                   })
               (Bindlib.bind_var v next)
               (Dcalc.Ast.make_var (scope_input_var, pos_mark pos_sigma)),
             i - 1 ))
         scope_input_variables
         (next, List.length scope_input_variables - 1))
  in
  let scope_return_struct_fields =
    List.map
      (fun (var_ctx, dvar) ->
        let struct_field_name =
          StructFieldName.fresh (Bindlib.name_of dvar ^ "_out", pos_sigma)
        in
        struct_field_name, (var_ctx.scope_var_typ, pos_sigma))
      scope_output_variables
  in
  let scope_input_struct_fields =
    List.map
      (fun (var_ctx, dvar) ->
        let struct_field_name =
          StructFieldName.fresh (Bindlib.name_of dvar ^ "_in", pos_sigma)
        in
        struct_field_name, input_var_typ var_ctx)
      scope_input_variables
  in
  let new_struct_ctx =
    StructMap.add scope_input_struct_name scope_input_struct_fields
      (StructMap.singleton scope_return_struct_name scope_return_struct_fields)
  in
  ( Bindlib.box_apply
      (fun scope_body_expr ->
        {
          scope_body_expr;
          scope_body_input_struct = scope_input_struct_name;
          scope_body_output_struct = scope_return_struct_name;
        })
      (Bindlib.bind_var scope_input_var
         (input_destructurings rules_with_return_expr)),
    new_struct_ctx )

let translate_program (prgm : Ast.program) :
    untyped Dcalc.Ast.program * Dependency.TVertex.t list =
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
  let dummy_scope = ScopeName.fresh ("dummy", Pos.no_pos) in
  let decl_ctx =
    {
      ctx_structs =
        StructMap.map
          (List.map (fun (x, y) ->
               x, translate_typ (ctx_for_typ_translation dummy_scope) y))
          struct_ctx;
      ctx_enums =
        EnumMap.map
          (List.map (fun (x, y) ->
               x, (translate_typ (ctx_for_typ_translation dummy_scope)) y))
          enum_ctx;
    }
  in
  let sctx : scope_sigs_ctx =
    Ast.ScopeMap.mapi
      (fun scope_name scope ->
        let scope_dvar =
          Var.make
            (Marked.unmark (ScopeName.get_info scope.Ast.scope_decl_name))
        in
        let scope_return_struct_name =
          StructName.fresh
            (Marked.map_under_mark
               (fun s -> s ^ "_out")
               (ScopeName.get_info scope_name))
        in
        let scope_input_var =
          Var.make (Marked.unmark (ScopeName.get_info scope_name) ^ "_in")
        in
        let scope_input_struct_name =
          StructName.fresh
            (Marked.map_under_mark
               (fun s -> s ^ "_in")
               (ScopeName.get_info scope_name))
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
                  scope_var_typ = Marked.unmark tau;
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
  let (scopes, decl_ctx)
        : (untyped Dcalc.Ast.expr, untyped) scopes Bindlib.box * _ =
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
            ctx_structs =
              StructMap.union
                (fun _ _ -> assert false (* should not happen *))
                decl_ctx.ctx_structs scope_out_struct;
          }
        in
        let scope_next = Bindlib.bind_var dvar scopes in
        let new_scopes =
          Bindlib.box_apply2
            (fun scope_body scope_next ->
              ScopeDef { scope_name; scope_body; scope_next })
            scope_body scope_next
        in
        new_scopes, decl_ctx)
      scope_ordering
      (Bindlib.box Nil, decl_ctx)
  in
  { scopes = Bindlib.unbox scopes; decl_ctx }, types_ordering
