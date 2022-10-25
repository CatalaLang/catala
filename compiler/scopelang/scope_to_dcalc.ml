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
  scope_var_name : ScopeVar.t;
  scope_var_typ : naked_typ;
  scope_var_io : Ast.io;
}

type 'm scope_sig_ctx = {
  scope_sig_local_vars : scope_var_ctx list;  (** List of scope variables *)
  scope_sig_scope_var : 'm Dcalc.Ast.expr Var.t;
      (** Var representing the scope *)
  scope_sig_input_var : 'm Dcalc.Ast.expr Var.t;
      (** Var representing the scope input inside the scope func *)
  scope_sig_input_struct : StructName.t;  (** Scope input *)
  scope_sig_output_struct : StructName.t;  (** Scope output *)
  scope_sig_in_fields :
    (StructFieldName.t * Ast.io_input Marked.pos) ScopeVarMap.t;
      (** Mapping between the input scope variables and the input struct fields.
          The boolean is true for 'context' variables which need to be thunked. *)
}

type 'm scope_sigs_ctx = 'm scope_sig_ctx ScopeMap.t

type 'm ctx = {
  structs : struct_ctx;
  enums : enum_ctx;
  scope_name : ScopeName.t;
  scopes_parameters : 'm scope_sigs_ctx;
  scope_vars : ('m Dcalc.Ast.expr Var.t * naked_typ * Ast.io) ScopeVarMap.t;
  subscope_vars :
    ('m Dcalc.Ast.expr Var.t * naked_typ * Ast.io) ScopeVarMap.t SubScopeMap.t;
  local_vars : ('m Ast.expr, 'm Dcalc.Ast.expr Var.t) Var.Map.t;
}

let empty_ctx
    (struct_ctx : struct_ctx)
    (enum_ctx : enum_ctx)
    (scopes_ctx : 'm scope_sigs_ctx)
    (scope_name : ScopeName.t) =
  {
    structs = struct_ctx;
    enums = enum_ctx;
    scope_name;
    scopes_parameters = scopes_ctx;
    scope_vars = ScopeVarMap.empty;
    subscope_vars = SubScopeMap.empty;
    local_vars = Var.Map.empty;
  }

let mark_tany m pos = Expr.with_ty m (Marked.mark pos TAny) ~pos

(* Expression argument is used as a type witness, its type and positions aren't
   used *)
let pos_mark_mk (type a m) (e : (a, m mark) gexpr) :
    (Pos.t -> m mark) * ((_, Pos.t) Marked.t -> m mark) =
  let pos_mark pos =
    Expr.map_mark (fun _ -> pos) (fun _ -> TAny, pos) (Marked.get_mark e)
  in
  let pos_mark_as e = pos_mark (Marked.get_mark e) in
  pos_mark, pos_mark_as

let merge_defaults caller callee =
  let caller =
    let m = Marked.get_mark caller in
    let pos = Expr.mark_pos m in
    Expr.make_app caller
      [Expr.elit LUnit (Expr.with_ty m (Marked.mark pos (TLit TUnit)))]
      pos
  in
  let body =
    let m = Marked.get_mark callee in
    let ltrue =
      Expr.elit (LBool true)
        (Expr.with_ty m (Marked.mark (Expr.mark_pos m) (TLit TBool)))
    in
    Expr.edefault [caller] ltrue callee m
  in
  body

let tag_with_log_entry
    (e : 'm Dcalc.Ast.expr boxed)
    (l : log_entry)
    (markings : Utils.Uid.MarkedString.info list) : 'm Dcalc.Ast.expr boxed =
  let m = mark_tany (Marked.get_mark e) (Expr.pos e) in
  Expr.eapp (Expr.eop (Unop (Log (l, markings))) m) [e] m

(* In a list of exceptions, it is normally an error if more than a single one
   apply at the same time. This relaxes this constraint slightly, allowing a
   conflict if all the triggered conflicting exception yield syntactically equal
   results (and as long as none of these exceptions have exceptions themselves)

   NOTE: the choice of the exception that will be triggered and show in the
   trace is arbitrary (but deterministic). *)
let collapse_similar_outcomes (type m) (excepts : m Ast.expr list) :
    m Ast.expr list =
  let module ExprMap = Map.Make (struct
    type t = m Ast.expr

    let compare = Expr.compare
  end) in
  let cons_map =
    List.fold_left
      (fun map -> function
        | (EDefault ([], _, cons), _) as e ->
          ExprMap.update cons
            (fun prev -> Some (e :: Option.value ~default:[] prev))
            map
        | _ -> map)
      ExprMap.empty excepts
  in
  let _, excepts =
    List.fold_right
      (fun e (cons_map, excepts) ->
        match e with
        | EDefault ([], _, cons), _ ->
          let collapsed_exc =
            List.fold_left
              (fun acc -> function
                | EDefault ([], just, cons), pos ->
                  [EDefault (acc, just, cons), pos]
                | _ -> assert false)
              []
              (ExprMap.find cons cons_map)
          in
          ExprMap.add cons [] cons_map, collapsed_exc @ excepts
        | e -> cons_map, e :: excepts)
      excepts (cons_map, [])
  in
  excepts

let thunk_scope_arg io_in e =
  let silent_var = Var.make "_" in
  let pos = Marked.get_mark io_in in
  match Marked.unmark io_in with
  | Ast.NoInput -> invalid_arg "thunk_scope_arg"
  | Ast.OnlyInput -> Expr.eerroronempty e (Marked.get_mark e)
  | Ast.Reentrant -> Expr.make_abs [| silent_var |] e [TLit TUnit, pos] pos

let rec translate_expr (ctx : 'm ctx) (e : 'm Ast.expr) :
    'm Dcalc.Ast.expr boxed =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EVar v -> Expr.evar (Var.Map.find v ctx.local_vars) m
  | ELit
      (( LBool _ | LEmptyError | LInt _ | LRat _ | LMoney _ | LUnit | LDate _
       | LDuration _ ) as l) ->
    Expr.elit l m
  | EStruct (struct_name, e_fields) ->
    let struct_sig = StructMap.find struct_name ctx.structs in
    let d_fields, remaining_e_fields =
      List.fold_right
        (fun (field_name, _) (d_fields, e_fields) ->
          let field_e = StructFieldMap.find field_name e_fields in
          let field_d = translate_expr ctx field_e in
          field_d :: d_fields, StructFieldMap.remove field_name e_fields)
        struct_sig ([], e_fields)
    in
    if StructFieldMap.cardinal remaining_e_fields > 0 then
      Errors.raise_spanned_error (Expr.pos e)
        "The fields \"%a\" do not belong to the structure %a"
        StructName.format_t struct_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (field_name, _) ->
             Format.fprintf fmt "%a" StructFieldName.format_t field_name))
        (StructFieldMap.bindings remaining_e_fields)
    else Expr.etuple d_fields (Some struct_name) m
  | EStructAccess (e1, field_name, struct_name) ->
    let struct_sig = StructMap.find struct_name ctx.structs in
    let _, field_index =
      try
        List.assoc field_name (List.mapi (fun i (x, y) -> x, (y, i)) struct_sig)
      with Not_found ->
        Errors.raise_spanned_error (Expr.pos e)
          "The field \"%a\" does not belong to the structure %a"
          StructFieldName.format_t field_name StructName.format_t struct_name
    in
    let e1 = translate_expr ctx e1 in
    Expr.etupleaccess e1 field_index (Some struct_name)
      (List.map snd struct_sig) m
  | EEnumInj (e1, constructor, enum_name) ->
    let enum_sig = EnumMap.find enum_name ctx.enums in
    let _, constructor_index =
      try
        List.assoc constructor (List.mapi (fun i (x, y) -> x, (y, i)) enum_sig)
      with Not_found ->
        Errors.raise_spanned_error (Expr.pos e)
          "The constructor \"%a\" does not belong to the enum %a"
          EnumConstructor.format_t constructor EnumName.format_t enum_name
    in
    let e1 = translate_expr ctx e1 in
    Expr.einj e1 constructor_index enum_name (List.map snd enum_sig) m
  | EMatchS (e1, enum_name, cases) ->
    let enum_sig = EnumMap.find enum_name ctx.enums in
    let d_cases, remaining_e_cases =
      List.fold_right
        (fun (constructor, _) (d_cases, e_cases) ->
          let case_e =
            try EnumConstructorMap.find constructor e_cases
            with Not_found ->
              Errors.raise_spanned_error (Expr.pos e)
                "The constructor %a of enum %a is missing from this pattern \
                 matching"
                EnumConstructor.format_t constructor EnumName.format_t enum_name
          in
          let case_d = translate_expr ctx case_e in
          case_d :: d_cases, EnumConstructorMap.remove constructor e_cases)
        enum_sig ([], cases)
    in
    if EnumConstructorMap.cardinal remaining_e_cases > 0 then
      Errors.raise_spanned_error (Expr.pos e)
        "Pattern matching is incomplete for enum %a: missing cases %a"
        EnumName.format_t enum_name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (case_name, _) ->
             Format.fprintf fmt "%a" EnumConstructor.format_t case_name))
        (EnumConstructorMap.bindings remaining_e_cases)
    else
      let e1 = translate_expr ctx e1 in
      Expr.ematch e1 d_cases enum_name m
  | EScopeCall (sc_name, fields) ->
    let pos = Expr.mark_pos m in
    let sc_sig = ScopeMap.find sc_name ctx.scopes_parameters in
    let in_var_map =
      ScopeVarMap.merge
        (fun var_name str_field expr ->
          let expr =
            match str_field, expr with
            | Some (_, (Ast.Reentrant, _)), None ->
              Some (Expr.unbox (Expr.elit LEmptyError (mark_tany m pos)))
            | _ -> expr
          in
          match str_field, expr with
          | None, None -> None
          | Some (fld, io_in), Some e ->
            Some (fld, thunk_scope_arg io_in (translate_expr ctx e))
          | Some (fld, _), None ->
            Errors.raise_multispanned_error
              [
                None, pos;
                ( Some "Declaration of the missing input variable",
                  Marked.get_mark (StructFieldName.get_info fld) );
              ]
              "Definition of input variable '%a' missing in this scope call"
              ScopeVar.format_t var_name
          | None, Some _ ->
            Errors.raise_multispanned_error
              [
                None, pos;
                ( Some "Declaration of scope '%a'",
                  Marked.get_mark (ScopeName.get_info sc_name) );
              ]
              "Unknown input variable '%a' in scope call of '%a'"
              ScopeVar.format_t var_name ScopeName.format_t sc_name)
        sc_sig.scope_sig_in_fields fields
    in
    let field_map =
      ScopeVarMap.fold
        (fun _ (fld, e) acc -> StructFieldMap.add fld e acc)
        in_var_map StructFieldMap.empty
    in
    let arg_struct =
      Expr.make_struct field_map sc_sig.scope_sig_input_struct (mark_tany m pos)
    in
    Expr.eapp
      (Expr.evar sc_sig.scope_sig_scope_var (mark_tany m pos))
      [arg_struct] m
  | EApp (e1, args) ->
    (* We insert various log calls to record arguments and outputs of
       user-defined functions belonging to scopes *)
    let e1_func = translate_expr ctx e1 in
    let markings l =
      match l with
      | ScopelangScopeVar (v, _) ->
        [ScopeName.get_info ctx.scope_name; ScopeVar.get_info v]
      | SubScopeVar (s, _, (v, _)) ->
        [ScopeName.get_info s; ScopeVar.get_info v]
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
        let _, typ, _ = ScopeVarMap.find (Marked.unmark var) vars in
        match typ with
        | TArrow (marked_input_typ, marked_output_typ) ->
          Marked.unmark marked_input_typ, Marked.unmark marked_output_typ
        | _ -> TAny, TAny
      in
      match Marked.unmark e1 with
      | ELocation (ScopelangScopeVar var) ->
        retrieve_in_and_out_typ_or_any var ctx.scope_vars
      | ELocation (SubScopeVar (_, sname, var)) ->
        ctx.subscope_vars
        |> SubScopeMap.find (Marked.unmark sname)
        |> retrieve_in_and_out_typ_or_any var
      | _ -> TAny, TAny
    in
    let new_args =
      match Marked.unmark e1, new_args with
      | ELocation l, [new_arg] ->
        [
          tag_with_log_entry new_arg (VarDef input_typ)
            (markings l @ [Marked.mark (Expr.pos e) "input"]);
        ]
      | _ -> new_args
    in
    let new_e = Expr.eapp e1_func new_args m in
    let new_e =
      match Marked.unmark e1 with
      | ELocation l ->
        tag_with_log_entry
          (tag_with_log_entry new_e (VarDef output_typ)
             (markings l @ [Marked.mark (Expr.pos e) "output"]))
          EndCall (markings l)
      | _ -> new_e
    in
    new_e
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
              (fun local_vars (x, new_x) -> Var.Map.add x new_x local_vars)
              ctx.local_vars both_xs;
        }
        body
    in
    let binder = Expr.bind new_xs body in
    Expr.eabs binder typ m
  | EDefault (excepts, just, cons) ->
    let excepts = collapse_similar_outcomes excepts in
    Expr.edefault
      (List.map (translate_expr ctx) excepts)
      (translate_expr ctx just) (translate_expr ctx cons) m
  | ELocation (ScopelangScopeVar a) ->
    let v, _, _ = ScopeVarMap.find (Marked.unmark a) ctx.scope_vars in
    Expr.evar v m
  | ELocation (SubScopeVar (_, s, a)) -> (
    try
      let v, _, _ =
        ScopeVarMap.find (Marked.unmark a)
          (SubScopeMap.find (Marked.unmark s) ctx.subscope_vars)
      in
      Expr.evar v m
    with Not_found ->
      Errors.raise_multispanned_error
        [
          Some "Incriminated variable usage:", Expr.pos e;
          ( Some "Incriminated subscope variable declaration:",
            Marked.get_mark (ScopeVar.get_info (Marked.unmark a)) );
          ( Some "Incriminated subscope declaration:",
            Marked.get_mark (SubScopeName.get_info (Marked.unmark s)) );
        ]
        "The variable %a.%a cannot be used here, as it is not part subscope \
         %a's results. Maybe you forgot to qualify it as an output?"
        SubScopeName.format_t (Marked.unmark s) ScopeVar.format_t
        (Marked.unmark a) SubScopeName.format_t (Marked.unmark s))
  | EIfThenElse (cond, et, ef) ->
    Expr.eifthenelse (translate_expr ctx cond) (translate_expr ctx et)
      (translate_expr ctx ef) m
  | EOp op -> Expr.eop op m
  | ErrorOnEmpty e' -> Expr.eerroronempty (translate_expr ctx e') m
  | EArray es -> Expr.earray (List.map (translate_expr ctx) es) m

(** The result of a rule translation is a list of assignment, with variables and
    expressions. We also return the new translation context available after the
    assignment to use in later rule translations. The list is actually a
    continuation yielding a [Dcalc.scope_body_expr] by giving it what should
    come later in the chain of let-bindings. *)
let translate_rule
    (ctx : 'm ctx)
    (rule : 'm Ast.rule)
    ((sigma_name, pos_sigma) : Utils.Uid.MarkedString.info) :
    ('m Dcalc.Ast.expr scope_body_expr Bindlib.box ->
    'm Dcalc.Ast.expr scope_body_expr Bindlib.box)
    * 'm ctx =
  match rule with
  | Definition ((ScopelangScopeVar a, var_def_pos), tau, a_io, e) ->
    let pos_mark, pos_mark_as = pos_mark_mk e in
    let a_name = ScopeVar.get_info (Marked.unmark a) in
    let a_var = Var.make (Marked.unmark a_name) in
    let new_e = translate_expr ctx e in
    let a_expr = Expr.make_var a_var (pos_mark var_def_pos) in
    let merged_expr =
      Expr.eerroronempty
        (match Marked.unmark a_io.io_input with
        | OnlyInput -> failwith "should not happen"
        (* scopelang should not contain any definitions of input only
           variables *)
        | Reentrant -> merge_defaults a_expr new_e
        | NoInput -> new_e)
        (pos_mark_as a_name)
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
          (Expr.Box.lift merged_expr)),
      {
        ctx with
        scope_vars =
          ScopeVarMap.add (Marked.unmark a)
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
          str ^ "." ^ Marked.unmark (ScopeVar.get_info (Marked.unmark subs_var)))
        (SubScopeName.get_info (Marked.unmark subs_index))
    in
    let a_var = Var.make (Marked.unmark a_name) in
    let new_e =
      tag_with_log_entry (translate_expr ctx e)
        (VarDef (Marked.unmark tau))
        [sigma_name, pos_sigma; a_name]
    in
    let thunked_or_nonempty_new_e = thunk_scope_arg a_io.Ast.io_input new_e in
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
          (Expr.Box.lift thunked_or_nonempty_new_e)),
      {
        ctx with
        subscope_vars =
          SubScopeMap.update (Marked.unmark subs_index)
            (fun map ->
              match map with
              | Some map ->
                Some
                  (ScopeVarMap.add (Marked.unmark subs_var)
                     (a_var, Marked.unmark tau, a_io)
                     map)
              | None ->
                Some
                  (ScopeVarMap.singleton (Marked.unmark subs_var)
                     (a_var, Marked.unmark tau, a_io)))
            ctx.subscope_vars;
      } )
  | Call (subname, subindex, m) ->
    let subscope_sig = ScopeMap.find subname ctx.scopes_parameters in
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
      try SubScopeMap.find subindex ctx.subscope_vars
      with Not_found -> ScopeVarMap.empty
    in
    let subscope_var_not_yet_defined subvar =
      not (ScopeVarMap.mem subvar subscope_vars_defined)
    in
    let pos_call = Marked.get_mark (SubScopeName.get_info subindex) in
    let subscope_args =
      List.map
        (fun (subvar : scope_var_ctx) ->
          if subscope_var_not_yet_defined subvar.scope_var_name then
            (* This is a redundant check. Normally, all subscope variables
               should have been defined (even an empty definition, if they're
               not defined by any rule in the source code) by the translation
               from desugared to the scope language. *)
            Expr.empty_thunked_term m
          else
            let a_var, _, _ =
              ScopeVarMap.find subvar.scope_var_name subscope_vars_defined
            in
            Expr.make_var a_var (mark_tany m pos_call))
        all_subscope_input_vars
    in
    let subscope_struct_arg =
      (* FIXME: this is very fragile: we assume that the ordering of the scope
         variables is the same as the ordering of the struct fields. *)
      Expr.etuple subscope_args (Some called_scope_input_struct)
        (mark_tany m pos_call)
    in
    let all_subscope_output_vars_dcalc =
      List.map
        (fun (subvar : scope_var_ctx) ->
          let sub_dcalc_var =
            Var.make
              (Marked.unmark (SubScopeName.get_info subindex)
              ^ "."
              ^ Marked.unmark (ScopeVar.get_info subvar.scope_var_name))
          in
          subvar, sub_dcalc_var)
        all_subscope_output_vars
    in
    let subscope_func =
      tag_with_log_entry
        (Expr.make_var scope_dcalc_var (mark_tany m pos_call))
        BeginCall
        [
          sigma_name, pos_sigma;
          SubScopeName.get_info subindex;
          ScopeName.get_info subname;
        ]
    in
    let call_expr =
      tag_with_log_entry
        (Expr.eapp subscope_func [subscope_struct_arg] (mark_tany m pos_call))
        EndCall
        [
          sigma_name, pos_sigma;
          SubScopeName.get_info subindex;
          ScopeName.get_info subname;
        ]
    in
    let result_tuple_var = Var.make "result" in
    let result_tuple_typ = TStruct called_scope_return_struct, pos_sigma in
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
        (Expr.Box.lift call_expr)
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
                        mark_tany m pos_sigma );
                  })
              (Bindlib.bind_var v next)
              (Expr.Box.lift
                 (Expr.make_var result_tuple_var (mark_tany m pos_sigma))),
            i - 1 ))
        all_subscope_output_vars_dcalc
        (next, List.length all_subscope_output_vars_dcalc - 1)
    in
    ( (fun next -> call_scope_let (fst (result_bindings_lets next))),
      {
        ctx with
        subscope_vars =
          SubScopeMap.add subindex
            (List.fold_left
               (fun acc (var_ctx, dvar) ->
                 ScopeVarMap.add var_ctx.scope_var_name
                   (dvar, var_ctx.scope_var_typ, var_ctx.scope_var_io)
                   acc)
               ScopeVarMap.empty all_subscope_output_vars_dcalc)
            ctx.subscope_vars;
      } )
  | Assertion e ->
    let new_e = translate_expr ctx e in
    let scope_let_pos = Expr.pos e in
    let scope_let_typ = TLit TUnit, scope_let_pos in
    ( (fun next ->
        Bindlib.box_apply2
          (fun next new_e ->
            ScopeLet
              {
                scope_let_next = next;
                scope_let_pos;
                scope_let_typ;
                scope_let_expr =
                  (* To ensure that we throw an error if the value is not
                     defined, we add an check "ErrorOnEmpty" here. *)
                  Marked.mark
                    (Expr.map_ty (fun _ -> scope_let_typ) (Marked.get_mark e))
                    (EAssert (Marked.same_mark_as (ErrorOnEmpty new_e) e));
                scope_let_kind = Assertion;
              })
          (Bindlib.bind_var (Var.make "_") next)
          (Expr.Box.lift new_e)),
      ctx )

let translate_rules
    (ctx : 'm ctx)
    (rules : 'm Ast.rule list)
    ((sigma_name, pos_sigma) : Utils.Uid.MarkedString.info)
    (mark : 'm mark)
    (sigma_return_struct_name : StructName.t) :
    'm Dcalc.Ast.expr scope_body_expr Bindlib.box * 'm ctx =
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
  let scope_variables = ScopeVarMap.bindings new_ctx.scope_vars in
  let scope_output_variables =
    List.filter
      (fun (_, (_, _, io)) -> Marked.unmark io.Ast.io_output)
      scope_variables
  in
  let return_exp =
    Expr.etuple
      (List.map
         (fun (_, (dcalc_var, _, _)) ->
           Expr.make_var dcalc_var (mark_tany mark pos_sigma))
         scope_output_variables)
      (Some sigma_return_struct_name) (mark_tany mark pos_sigma)
  in
  ( scope_lets
      (Bindlib.box_apply
         (fun return_exp -> Result return_exp)
         (Expr.Box.lift return_exp)),
    new_ctx )

let translate_scope_decl
    (struct_ctx : struct_ctx)
    (enum_ctx : enum_ctx)
    (sctx : 'm scope_sigs_ctx)
    (scope_name : ScopeName.t)
    (sigma : 'm Ast.scope_decl) :
    'm Dcalc.Ast.expr scope_body Bindlib.box * struct_ctx =
  let sigma_info = ScopeName.get_info sigma.scope_decl_name in
  let scope_sig = ScopeMap.find sigma.scope_decl_name sctx in
  let scope_variables = scope_sig.scope_sig_local_vars in
  let ctx =
    (* the context must be initialized for fresh variables for all only-input
       scope variables *)
    List.fold_left
      (fun ctx scope_var ->
        match Marked.unmark scope_var.scope_var_io.io_input with
        | OnlyInput ->
          let scope_var_name = ScopeVar.get_info scope_var.scope_var_name in
          let scope_var_dcalc = Var.make (Marked.unmark scope_var_name) in
          {
            ctx with
            scope_vars =
              ScopeVarMap.add scope_var.scope_var_name
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
    translate_rules ctx sigma.scope_decl_rules sigma_info sigma.scope_mark
      scope_return_struct_name
  in
  let scope_variables =
    List.map
      (fun var_ctx ->
        let dcalc_x, _, _ =
          ScopeVarMap.find var_ctx.scope_var_name ctx.scope_vars
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
                         mark_tany sigma.scope_mark pos_sigma );
                   })
               (Bindlib.bind_var v next)
               (Expr.Box.lift
                  (Expr.make_var scope_input_var
                     (mark_tany sigma.scope_mark pos_sigma))),
             i - 1 ))
         scope_input_variables
         (next, List.length scope_input_variables - 1))
  in
  let field_map =
    List.map
      (fun (var_ctx, _) ->
        let var = var_ctx.scope_var_name in
        let field, _ = ScopeVarMap.find var scope_sig.scope_sig_in_fields in
        field, input_var_typ var_ctx)
      scope_input_variables
  in
  let new_struct_ctx = StructMap.singleton scope_input_struct_name field_map in
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

let translate_program (prgm : 'm Ast.program) : 'm Dcalc.Ast.program =
  let scope_dependencies = Dependency.build_program_dep_graph prgm in
  Dependency.check_for_cycle_in_scope scope_dependencies;
  let scope_ordering = Dependency.get_scope_ordering scope_dependencies in
  let decl_ctx = prgm.program_ctx in
  let sctx : 'm scope_sigs_ctx =
    ScopeMap.mapi
      (fun scope_name scope ->
        let scope_dvar =
          Var.make
            (Marked.unmark (ScopeName.get_info scope.Ast.scope_decl_name))
        in
        let scope_return_struct_name =
          ScopeMap.find scope_name decl_ctx.ctx_scopes
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
        let scope_sig_in_fields =
          ScopeVarMap.filter_map
            (fun dvar (_, vis) ->
              match Marked.unmark vis.Ast.io_input with
              | NoInput -> None
              | OnlyInput | Reentrant ->
                let info = ScopeVar.get_info dvar in
                let s = Marked.unmark info ^ "_in" in
                Some
                  ( StructFieldName.fresh (s, Marked.get_mark info),
                    vis.Ast.io_input ))
            scope.scope_sig
        in
        {
          scope_sig_local_vars =
            List.map
              (fun (scope_var, (tau, vis)) ->
                {
                  scope_var_name = scope_var;
                  scope_var_typ = Marked.unmark tau;
                  scope_var_io = vis;
                })
              (ScopeVarMap.bindings scope.scope_sig);
          scope_sig_scope_var = scope_dvar;
          scope_sig_input_var = scope_input_var;
          scope_sig_input_struct = scope_input_struct_name;
          scope_sig_output_struct = scope_return_struct_name;
          scope_sig_in_fields;
        })
      prgm.program_scopes
  in
  (* the resulting expression is the list of definitions of all the scopes,
     ending with the top-level scope. The decl_ctx is filled in left-to-right
     order, then the chained scopes aggregated from the right. *)
  let rec translate_scopes decl_ctx = function
    | scope_name :: next_scopes ->
      let scope = ScopeMap.find scope_name prgm.program_scopes in
      let scope_body, scope_in_struct =
        translate_scope_decl decl_ctx.ctx_structs decl_ctx.ctx_enums sctx
          scope_name scope
      in
      let dvar = (ScopeMap.find scope_name sctx).scope_sig_scope_var in
      let decl_ctx =
        {
          decl_ctx with
          ctx_structs =
            StructMap.union
              (fun _ _ -> assert false (* should not happen *))
              decl_ctx.ctx_structs scope_in_struct;
        }
      in
      let scope_next, decl_ctx = translate_scopes decl_ctx next_scopes in
      ( Bindlib.box_apply2
          (fun scope_body scope_next ->
            ScopeDef { scope_name; scope_body; scope_next })
          scope_body
          (Bindlib.bind_var dvar scope_next),
        decl_ctx )
    | [] -> Bindlib.box Nil, decl_ctx
  in
  let scopes, decl_ctx = translate_scopes decl_ctx scope_ordering in
  { scopes = Bindlib.unbox scopes; decl_ctx }
