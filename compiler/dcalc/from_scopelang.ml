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

type scope_var_ctx = {
  scope_var_name : ScopeVar.t;
  scope_var_typ : naked_typ;
  scope_var_io : Desugared.Ast.io;
}

type scope_input_var_ctx = {
  scope_input_name : StructField.t;
  scope_input_io : Runtime.io_input Mark.pos;
  scope_input_typ : naked_typ;
}

type 'm scope_ref =
  | Local_scope_ref of 'm Ast.expr Var.t
  | External_scope_ref of ScopeName.t Mark.pos

type 'm scope_sig_ctx = {
  scope_sig_local_vars : scope_var_ctx list;  (** List of scope variables *)
  scope_sig_scope_ref : 'm scope_ref;
      (** Var or external representing the scope *)
  scope_sig_input_struct : StructName.t;  (** Scope input *)
  scope_sig_output_struct : StructName.t;  (** Scope output *)
  scope_sig_in_fields : scope_input_var_ctx ScopeVar.Map.t;
      (** Mapping between the input scope variables and the input struct fields. *)
}

type 'm scope_sigs_ctx = {
  scope_sigs : 'm scope_sig_ctx ScopeName.Map.t;
  scope_sigs_modules : 'm scope_sigs_ctx ModuleName.Map.t;
}

type 'm ctx = {
  decl_ctx : decl_ctx;
  scope_name : ScopeName.t option;
  scopes_parameters : 'm scope_sigs_ctx;
  toplevel_vars : ('m Ast.expr Var.t * naked_typ) TopdefName.Map.t;
  scope_vars :
    ('m Ast.expr Var.t * naked_typ * Desugared.Ast.io) ScopeVar.Map.t;
  subscope_vars :
    ('m Ast.expr Var.t * naked_typ * Desugared.Ast.io) ScopeVar.Map.t
    SubScopeName.Map.t;
  date_rounding : date_rounding;
}

let mark_tany m pos = Expr.with_ty m (Mark.add pos TAny) ~pos

(* Expression argument is used as a type witness, its type and positions aren't
   used *)
let pos_mark_mk (type a m) (e : (a, m) gexpr) :
    (Pos.t -> m mark) * ((_, Pos.t) Mark.ed -> m mark) =
  let pos_mark pos =
    Expr.map_mark (fun _ -> pos) (fun _ -> TAny, pos) (Mark.get e)
  in
  let pos_mark_as e = pos_mark (Mark.get e) in
  pos_mark, pos_mark_as

let module_scope_sig scope_sig_ctx scope =
  let ssctx =
    List.fold_left
      (fun ssctx m -> ModuleName.Map.find m ssctx.scope_sigs_modules)
      scope_sig_ctx (ScopeName.path scope)
  in
  ScopeName.Map.find scope ssctx.scope_sigs

let merge_defaults
    ~(is_func : bool)
    (caller : (dcalc, 'm) boxed_gexpr)
    (callee : (dcalc, 'm) boxed_gexpr) : (dcalc, 'm) boxed_gexpr =
  (* the merging of the two defaults, from the reentrant caller and the callee,
     is straightfoward in the general case and a little subtler when the
     variable being defined is a function. *)
  if is_func then
    let m_callee = Mark.get callee in
    let unboxed_callee = Expr.unbox callee in
    match Mark.remove unboxed_callee with
    | EAbs { binder; tys } ->
      let vars, body = Bindlib.unmbind binder in
      let m_body = Mark.get body in
      let caller =
        let m = Mark.get caller in
        let pos = Expr.mark_pos m in
        Expr.make_app caller
          (List.map2
             (fun (var : (dcalc, 'm) naked_gexpr Bindlib.var) ty ->
               Expr.evar var
                 (* we have to correctly propagate types when doing this
                    rewriting *)
                 (Expr.with_ty m_body ~pos:(Expr.mark_pos m_body) ty))
             (Array.to_list vars) tys)
          pos
      in
      let ltrue =
        Expr.elit (LBool true)
          (Expr.with_ty m_callee
             (Mark.add (Expr.mark_pos m_callee) (TLit TBool)))
      in
      let d = Expr.edefault [caller] ltrue (Expr.rebox body) m_body in
      Expr.make_abs vars
        (Expr.eerroronempty d m_body)
        tys (Expr.mark_pos m_callee)
    | _ -> assert false
    (* should not happen because there should always be a lambda at the
       beginning of a default with a function type *)
  else
    let caller =
      let m = Mark.get caller in
      let pos = Expr.mark_pos m in
      Expr.make_app caller
        [Expr.elit LUnit (Expr.with_ty m (Mark.add pos (TLit TUnit)))]
        pos
    in
    let body =
      let m = Mark.get callee in
      let ltrue =
        Expr.elit (LBool true)
          (Expr.with_ty m (Mark.add (Expr.mark_pos m) (TLit TBool)))
      in
      Expr.eerroronempty (Expr.edefault [caller] ltrue callee m) m
    in
    body

let tag_with_log_entry
    (e : 'm Ast.expr boxed)
    (l : log_entry)
    (markings : Uid.MarkedString.info list) : 'm Ast.expr boxed =
  let m = mark_tany (Mark.get e) (Expr.pos e) in

  if Cli.globals.trace then
    Expr.eapp (Expr.eop (Log (l, markings)) [TAny, Expr.pos e] m) [e] m
  else e

(* In a list of exceptions, it is normally an error if more than a single one
   apply at the same time. This relaxes this constraint slightly, allowing a
   conflict if all the triggered conflicting exception yield syntactically equal
   results (and as long as none of these exceptions have exceptions themselves)

   NOTE: the choice of the exception that will be triggered and show in the
   trace is arbitrary (but deterministic). *)
let collapse_similar_outcomes (type m) (excepts : m Scopelang.Ast.expr list) :
    m Scopelang.Ast.expr list =
  let module ExprMap = Map.Make (struct
    type t = m Scopelang.Ast.expr

    let compare = Expr.compare
    let format = Expr.format
  end) in
  let cons_map =
    List.fold_left
      (fun map -> function
        | (EDefault { excepts = []; cons; _ }, _) as e ->
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
        | EDefault { excepts = []; cons; _ }, _ ->
          let collapsed_exc =
            List.fold_left
              (fun acc -> function
                | EDefault { excepts = []; just; cons }, pos ->
                  [EDefault { excepts = acc; just; cons }, pos]
                | _ -> assert false)
              []
              (ExprMap.find cons cons_map)
          in
          ExprMap.add cons [] cons_map, collapsed_exc @ excepts
        | e -> cons_map, e :: excepts)
      excepts (cons_map, [])
  in
  excepts

let thunk_scope_arg ~is_func io_in e =
  (* For "context" (or reentrant) variables, we thunk them as [(fun () -> e)] so
     that we can put them in default terms at the initialisation of the function
     body, allowing an empty error to recover the default value. *)
  let silent_var = Var.make "_" in
  let pos = Mark.get io_in in
  match Mark.remove io_in with
  | Runtime.NoInput -> invalid_arg "thunk_scope_arg"
  | Runtime.OnlyInput -> Expr.eerroronempty e (Mark.get e)
  | Runtime.Reentrant ->
    (* we don't need to thunk expressions that are already functions *)
    if is_func then e
    else Expr.make_abs [| silent_var |] e [TLit TUnit, pos] pos

let rec translate_expr (ctx : 'm ctx) (e : 'm Scopelang.Ast.expr) :
    'm Ast.expr boxed =
  let m = Mark.get e in
  match Mark.remove e with
  | EMatch { e = e1; name; cases = e_cases } ->
    let enum_sig = EnumName.Map.find name ctx.decl_ctx.ctx_enums in
    let d_cases, remaining_e_cases =
      (* FIXME: these checks should probably be moved to a better place *)
      EnumConstructor.Map.fold
        (fun constructor _ (d_cases, e_cases) ->
          let case_e =
            try EnumConstructor.Map.find constructor e_cases
            with EnumConstructor.Map.Not_found _ ->
              Message.raise_spanned_error (Expr.pos e)
                "The constructor %a of enum %a is missing from this pattern \
                 matching"
                EnumConstructor.format constructor EnumName.format name
          in
          let case_d = translate_expr ctx case_e in
          ( EnumConstructor.Map.add constructor case_d d_cases,
            EnumConstructor.Map.remove constructor e_cases ))
        enum_sig
        (EnumConstructor.Map.empty, e_cases)
    in
    if not (EnumConstructor.Map.is_empty remaining_e_cases) then
      Message.raise_spanned_error (Expr.pos e)
        "Pattern matching is incomplete for enum %a: missing cases %a"
        EnumName.format name
        (EnumConstructor.Map.format_keys ~pp_sep:(fun fmt () ->
             Format.fprintf fmt ", "))
        remaining_e_cases;
    let e1 = translate_expr ctx e1 in
    Expr.ematch ~e:e1 ~name ~cases:d_cases m
  | EScopeCall { scope; args } ->
    let pos = Expr.mark_pos m in
    let sc_sig = module_scope_sig ctx.scopes_parameters scope in
    let in_var_map =
      ScopeVar.Map.merge
        (fun var_name (str_field : scope_input_var_ctx option) expr ->
          let expr =
            match str_field, expr with
            | Some { scope_input_io = Reentrant, _; _ }, None ->
              Some (Expr.unbox (Expr.eemptyerror (mark_tany m pos)))
            | _ -> expr
          in
          match str_field, expr with
          | None, None -> None
          | Some var_ctx, Some e ->
            Some
              ( var_ctx.scope_input_name,
                thunk_scope_arg
                  ~is_func:
                    (match var_ctx.scope_input_typ with
                    | TArrow _ -> true
                    | _ -> false)
                  var_ctx.scope_input_io (translate_expr ctx e) )
          | Some var_ctx, None ->
            Message.raise_multispanned_error
              [
                None, pos;
                ( Some "Declaration of the missing input variable",
                  Mark.get (StructField.get_info var_ctx.scope_input_name) );
              ]
              "Definition of input variable '%a' missing in this scope call"
              ScopeVar.format var_name
          | None, Some _ ->
            Message.raise_multispanned_error
              [
                None, pos;
                ( Some "Declaration of scope '%a'",
                  Mark.get (ScopeName.get_info scope) );
              ]
              "Unknown input variable '%a' in scope call of '%a'"
              ScopeVar.format var_name ScopeName.format scope)
        sc_sig.scope_sig_in_fields args
    in
    let field_map =
      ScopeVar.Map.fold
        (fun _ (fld, e) acc -> StructField.Map.add fld e acc)
        in_var_map StructField.Map.empty
    in
    let arg_struct =
      Expr.estruct ~name:sc_sig.scope_sig_input_struct ~fields:field_map
        (mark_tany m pos)
    in
    let called_func =
      let m = mark_tany m pos in
      let e =
        match sc_sig.scope_sig_scope_ref with
        | Local_scope_ref v -> Expr.evar v m
        | External_scope_ref name ->
          Expr.eexternal ~name:(Mark.map (fun s -> External_scope s) name) m
      in
      tag_with_log_entry e BeginCall
        [ScopeName.get_info scope; Mark.add (Expr.pos e) "direct"]
    in
    let single_arg =
      tag_with_log_entry arg_struct
        (VarDef
           {
             log_typ = TStruct sc_sig.scope_sig_input_struct;
             log_io_output = false;
             log_io_input = OnlyInput;
           })
        [
          ScopeName.get_info scope;
          Mark.add (Expr.pos e) "direct";
          Mark.add (Expr.pos e) "input";
        ]
    in
    let direct_output_info =
      [
        ScopeName.get_info scope;
        Mark.add (Expr.pos e) "direct";
        Mark.add (Expr.pos e) "output";
      ]
    in
    (* calling_expr = scope_function scope_input_struct *)
    let calling_expr = Expr.eapp called_func [single_arg] m in
    (* For the purposes of log parsing explained in Runtime.EventParser, we need
       to wrap this function call in a flurry of log tags. Specifically, we are
       mascarading this scope call as a function call. In a normal function
       call, the log parser expects the output of the function to be defined as
       a default, hence the production of the output should yield a
       PosRecordIfTrueBool (which is not the case here). To remedy this absence
       we fabricate a fake PosRecordIfTrueBool attached to a silent let binding
       to "true" before returning the output value.

       But this is not sufficient. Indeed for the tricky case of
       [tests/test_scope/scope_call3.catala_en], when a scope returns a
       function, because we insert loggins calls at the call site of the
       function and not during its definition, then we're missing the call log
       instructions of the function returned. To avoid this trap, we need to
       rebind the resulting scope output struct by eta-expanding the functions
       to insert logging instructions. *)
    let result_var = Var.make "result" in
    let result_eta_expanded_var = Var.make "result" in
    (* result_eta_expanded = { struct_output_function_field = lambda x -> log
       (struct_output.struct_output_function_field x) ... } *)
    let result_eta_expanded =
      Expr.estruct ~name:sc_sig.scope_sig_output_struct
        ~fields:
          (StructField.Map.mapi
             (fun field typ ->
               let original_field_expr =
                 Expr.estructaccess
                   ~e:
                     (Expr.make_var result_var
                        (Expr.with_ty m
                           (TStruct sc_sig.scope_sig_output_struct, Expr.pos e)))
                   ~field ~name:sc_sig.scope_sig_output_struct
                   (Expr.with_ty m typ)
               in
               match Mark.remove typ with
               | TArrow (ts_in, t_out) ->
                 (* Here the output scope struct field is a function so we
                    eta-expand it and insert logging instructions. Invariant:
                    works because there is no partial evaluation. *)
                 let params_vars =
                   ListLabels.mapi ts_in ~f:(fun i _ ->
                       Var.make ("param" ^ string_of_int i))
                 in
                 let f_markings =
                   [ScopeName.get_info scope; StructField.get_info field]
                 in
                 Expr.make_abs
                   (Array.of_list params_vars)
                   (tag_with_log_entry
                      (tag_with_log_entry
                         (Expr.eapp
                            (tag_with_log_entry original_field_expr BeginCall
                               f_markings)
                            (ListLabels.mapi (List.combine params_vars ts_in)
                               ~f:(fun i (param_var, t_in) ->
                                 tag_with_log_entry
                                   (Expr.make_var param_var
                                      (Expr.with_ty m t_in))
                                   (VarDef
                                      {
                                        log_typ = Mark.remove t_in;
                                        log_io_output = false;
                                        log_io_input = OnlyInput;
                                      })
                                   (f_markings
                                   @ [
                                       Mark.add (Expr.pos e)
                                         ("input" ^ string_of_int i);
                                     ])))
                            (Expr.with_ty m t_out))
                         (VarDef
                            {
                              log_typ = Mark.remove t_out;
                              log_io_output = true;
                              log_io_input = NoInput;
                            })
                         (f_markings @ [Mark.add (Expr.pos e) "output"]))
                      EndCall f_markings)
                   ts_in (Expr.pos e)
               | _ -> original_field_expr)
             (StructName.Map.find sc_sig.scope_sig_output_struct
                ctx.decl_ctx.ctx_structs))
        (Expr.with_ty m (TStruct sc_sig.scope_sig_output_struct, Expr.pos e))
    in
    (* Here we have to go through an if statement that records a decision being
       taken with a log. We can't just do a let-in with the true boolean value
       enclosed in the log because it might get optimized by a compiler later
       down the chain. *)
    (* if_then_else_returned = if log true then result_eta_expanded else
       result_eta_expanded *)
    let if_then_else_returned =
      Expr.eifthenelse
        (tag_with_log_entry
           (Expr.box
              (Mark.add
                 (Expr.with_ty m (TLit TBool, Expr.pos e))
                 (ELit (LBool true))))
           PosRecordIfTrueBool direct_output_info)
        (Expr.make_var result_eta_expanded_var
           (Expr.with_ty m (TStruct sc_sig.scope_sig_output_struct, Expr.pos e)))
        (Expr.make_var result_eta_expanded_var
           (Expr.with_ty m (TStruct sc_sig.scope_sig_output_struct, Expr.pos e)))
        (Expr.with_ty m (TStruct sc_sig.scope_sig_output_struct, Expr.pos e))
    in
    (* let result_var = calling_expr in let result_eta_expanded_var =
       result_eta_expaneded in log (if_then_else_returned ) *)
    Expr.make_let_in result_var
      (TStruct sc_sig.scope_sig_output_struct, Expr.pos e)
      calling_expr
      (Expr.make_let_in result_eta_expanded_var
         (TStruct sc_sig.scope_sig_output_struct, Expr.pos e)
         result_eta_expanded
         (tag_with_log_entry
            (tag_with_log_entry if_then_else_returned
               (VarDef
                  {
                    log_typ = TStruct sc_sig.scope_sig_output_struct;
                    log_io_output = true;
                    log_io_input = NoInput;
                  })
               direct_output_info)
            EndCall
            [ScopeName.get_info scope; Mark.add (Expr.pos e) "direct"])
         (Expr.pos e))
      (Expr.pos e)
  | EApp { f; args } ->
    (* We insert various log calls to record arguments and outputs of
       user-defined functions belonging to scopes *)
    let e1_func = translate_expr ctx f in
    let markings =
      match ctx.scope_name, Mark.remove f with
      | Some sname, ELocation loc -> (
        match loc with
        | ScopelangScopeVar { name = v, _; _ } ->
          [ScopeName.get_info sname; ScopeVar.get_info v]
        | SubScopeVar { scope; var = v, _; _ } ->
          [ScopeName.get_info scope; ScopeVar.get_info v]
        | ToplevelVar _ -> [])
      | _ -> []
    in
    let e1_func =
      match markings with
      | [] -> e1_func
      | m -> tag_with_log_entry e1_func BeginCall m
    in
    let new_args = List.map (translate_expr ctx) args in
    let input_typs, output_typ =
      (* NOTE: this is a temporary solution, it works because it's assumed that
         all function calls are from scope variables. However, this will change
         -- for more information see
         https://github.com/CatalaLang/catala/pull/280#discussion_r898851693. *)
      let retrieve_in_and_out_typ_or_any var vars =
        let _, typ, _ = ScopeVar.Map.find (Mark.remove var) vars in
        match typ with
        | TArrow (marked_input_typ, marked_output_typ) ->
          List.map Mark.remove marked_input_typ, Mark.remove marked_output_typ
        | _ -> ListLabels.map new_args ~f:(fun _ -> TAny), TAny
      in
      match Mark.remove f with
      | ELocation (ScopelangScopeVar { name = var }) ->
        retrieve_in_and_out_typ_or_any var ctx.scope_vars
      | ELocation (SubScopeVar { alias; var; _ }) ->
        ctx.subscope_vars
        |> SubScopeName.Map.find (Mark.remove alias)
        |> retrieve_in_and_out_typ_or_any var
      | ELocation (ToplevelVar { name }) -> (
        let decl_ctx =
          Program.module_ctx ctx.decl_ctx (TopdefName.path (Mark.remove name))
        in
        let typ = TopdefName.Map.find (Mark.remove name) decl_ctx.ctx_topdefs in
        match Mark.remove typ with
        | TArrow (tin, (tout, _)) -> List.map Mark.remove tin, tout
        | _ ->
          Message.raise_spanned_error (Expr.pos e)
            "Application of non-function toplevel variable")
      | _ -> ListLabels.map new_args ~f:(fun _ -> TAny), TAny
    in

    (* Message.emit_debug "new_args %d, input_typs: %d, input_typs %a"
       (List.length new_args) (List.length input_typs) (Format.pp_print_list
       Print.typ_debug) (List.map (Mark.add Pos.no_pos) input_typs); *)
    let new_args =
      ListLabels.mapi (List.combine new_args input_typs)
        ~f:(fun i (new_arg, input_typ) ->
          match markings with
          | _ :: _ as m ->
            tag_with_log_entry new_arg
              (VarDef
                 {
                   log_typ = input_typ;
                   log_io_output = false;
                   log_io_input = OnlyInput;
                 })
              (m @ [Mark.add (Expr.pos e) ("input" ^ string_of_int i)])
          | _ -> new_arg)
    in

    let new_e = Expr.eapp e1_func new_args m in
    let new_e =
      match markings with
      | [] -> new_e
      | m ->
        tag_with_log_entry
          (tag_with_log_entry new_e
             (VarDef
                {
                  log_typ = output_typ;
                  log_io_output = true;
                  log_io_input = NoInput;
                })
             (m @ [Mark.add (Expr.pos e) "output"]))
          EndCall m
    in
    new_e
  | EDefault { excepts; just; cons } ->
    let excepts = collapse_similar_outcomes excepts in
    Expr.edefault
      (List.map (translate_expr ctx) excepts)
      (translate_expr ctx just) (translate_expr ctx cons) m
  | ELocation (ScopelangScopeVar { name = a }) ->
    let v, _, _ = ScopeVar.Map.find (Mark.remove a) ctx.scope_vars in
    Expr.evar v m
  | ELocation (SubScopeVar { alias = s; var = a; _ }) -> (
    try
      let v, _, _ =
        ScopeVar.Map.find (Mark.remove a)
          (SubScopeName.Map.find (Mark.remove s) ctx.subscope_vars)
      in
      Expr.evar v m
    with ScopeVar.Map.Not_found _ | SubScopeName.Map.Not_found _ ->
      Message.raise_multispanned_error
        [
          Some "Incriminated variable usage:", Expr.pos e;
          ( Some "Incriminated subscope variable declaration:",
            Mark.get (ScopeVar.get_info (Mark.remove a)) );
          ( Some "Incriminated subscope declaration:",
            Mark.get (SubScopeName.get_info (Mark.remove s)) );
        ]
        "The variable %a.%a cannot be used here, as it is not part of subscope \
         %a's results. Maybe you forgot to qualify it as an output?"
        SubScopeName.format (Mark.remove s) ScopeVar.format (Mark.remove a)
        SubScopeName.format (Mark.remove s))
  | ELocation (ToplevelVar { name }) ->
    let path = TopdefName.path (Mark.remove name) in
    if path = [] then
      let v, _ = TopdefName.Map.find (Mark.remove name) ctx.toplevel_vars in
      Expr.evar v m
    else Expr.eexternal ~name:(Mark.map (fun n -> External_value n) name) m
  | EOp { op = Add_dat_dur _; tys } ->
    Expr.eop (Add_dat_dur ctx.date_rounding) tys m
  | EOp { op; tys } -> Expr.eop (Operator.translate op) tys m
  | ( EVar _ | EAbs _ | ELit _ | EStruct _ | EStructAccess _ | ETuple _
    | ETupleAccess _ | EInj _ | EEmptyError | EErrorOnEmpty _ | EArray _
    | EIfThenElse _ ) as e ->
    Expr.map ~f:(translate_expr ctx) (e, m)

(** The result of a rule translation is a list of assignment, with variables and
    expressions. We also return the new translation context available after the
    assignment to use in later rule translations. The list is actually a
    continuation yielding a [Dcalc.scope_body_expr] by giving it what should
    come later in the chain of let-bindings. *)
let translate_rule
    (ctx : 'm ctx)
    (rule : 'm Scopelang.Ast.rule)
    ((sigma_name, pos_sigma) : Uid.MarkedString.info) :
    ('m Ast.expr scope_body_expr Bindlib.box ->
    'm Ast.expr scope_body_expr Bindlib.box)
    * 'm ctx =
  match rule with
  | Definition ((ScopelangScopeVar { name = a }, var_def_pos), tau, a_io, e) ->
    let pos_mark, pos_mark_as = pos_mark_mk e in
    let a_name = ScopeVar.get_info (Mark.remove a) in
    let a_var = Var.make (Mark.remove a_name) in
    let new_e = translate_expr ctx e in
    let a_expr = Expr.make_var a_var (pos_mark var_def_pos) in
    let is_func = match Mark.remove tau with TArrow _ -> true | _ -> false in
    let merged_expr =
      match Mark.remove a_io.io_input with
      | OnlyInput -> failwith "should not happen"
      (* scopelang should not contain any definitions of input only variables *)
      | Reentrant -> merge_defaults ~is_func a_expr new_e
      | NoInput ->
        if is_func then new_e else Expr.eerroronempty new_e (pos_mark_as a_name)
    in
    let merged_expr =
      tag_with_log_entry merged_expr
        (VarDef
           {
             log_typ = Mark.remove tau;
             log_io_output = Mark.remove a_io.io_output;
             log_io_input = Mark.remove a_io.io_input;
           })
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
                scope_let_pos = Mark.get a;
              })
          (Bindlib.bind_var a_var next)
          (Expr.Box.lift merged_expr)),
      {
        ctx with
        scope_vars =
          ScopeVar.Map.add (Mark.remove a)
            (a_var, Mark.remove tau, a_io)
            ctx.scope_vars;
      } )
  | Definition
      ( (SubScopeVar { alias = subs_index; var = subs_var; _ }, var_def_pos),
        tau,
        a_io,
        e ) ->
    let a_name =
      Mark.map
        (fun str ->
          str ^ "." ^ Mark.remove (ScopeVar.get_info (Mark.remove subs_var)))
        (SubScopeName.get_info (Mark.remove subs_index))
    in
    let a_var = Var.make (Mark.remove a_name) in
    let new_e =
      tag_with_log_entry (translate_expr ctx e)
        (VarDef
           {
             log_typ = Mark.remove tau;
             log_io_output = false;
             log_io_input = Mark.remove a_io.Desugared.Ast.io_input;
           })
        [sigma_name, pos_sigma; a_name]
    in
    let is_func = match Mark.remove tau with TArrow _ -> true | _ -> false in
    let thunked_or_nonempty_new_e =
      thunk_scope_arg ~is_func a_io.Desugared.Ast.io_input new_e
    in
    ( (fun next ->
        Bindlib.box_apply2
          (fun next thunked_or_nonempty_new_e ->
            ScopeLet
              {
                scope_let_next = next;
                scope_let_pos = Mark.get a_name;
                scope_let_typ =
                  (match Mark.remove a_io.io_input with
                  | NoInput -> failwith "should not happen"
                  | OnlyInput -> tau
                  | Reentrant ->
                    if is_func then tau
                    else TArrow ([TLit TUnit, var_def_pos], tau), var_def_pos);
                scope_let_expr = thunked_or_nonempty_new_e;
                scope_let_kind = SubScopeVarDefinition;
              })
          (Bindlib.bind_var a_var next)
          (Expr.Box.lift thunked_or_nonempty_new_e)),
      {
        ctx with
        subscope_vars =
          SubScopeName.Map.update (Mark.remove subs_index)
            (fun map ->
              match map with
              | Some map ->
                Some
                  (ScopeVar.Map.add (Mark.remove subs_var)
                     (a_var, Mark.remove tau, a_io)
                     map)
              | None ->
                Some
                  (ScopeVar.Map.singleton (Mark.remove subs_var)
                     (a_var, Mark.remove tau, a_io)))
            ctx.subscope_vars;
      } )
  | Definition ((ToplevelVar _, _), _, _, _) ->
    assert false
    (* A global variable can't be defined locally. The [Definition] constructor
       could be made more specific to avoid this case, but the added complexity
       didn't seem worth it *)
  | Call (subname, subindex, m) ->
    let subscope_sig = module_scope_sig ctx.scopes_parameters subname in
    let scope_sig_decl =
      ScopeName.Map.find subname
        (Program.module_ctx ctx.decl_ctx (ScopeName.path subname)).ctx_scopes
    in
    let all_subscope_vars = subscope_sig.scope_sig_local_vars in
    let all_subscope_input_vars =
      List.filter
        (fun var_ctx ->
          match Mark.remove var_ctx.scope_var_io.Desugared.Ast.io_input with
          | NoInput -> false
          | _ -> true)
        all_subscope_vars
    in
    let all_subscope_output_vars =
      List.filter
        (fun var_ctx ->
          Mark.remove var_ctx.scope_var_io.Desugared.Ast.io_output)
        all_subscope_vars
    in
    let pos_call = Mark.get (SubScopeName.get_info subindex) in
    let scope_dcalc_ref =
      let m = mark_tany m pos_call in
      match subscope_sig.scope_sig_scope_ref with
      | Local_scope_ref var -> Expr.make_var var m
      | External_scope_ref name ->
        Expr.eexternal ~name:(Mark.map (fun n -> External_scope n) name) m
    in
    let called_scope_input_struct = subscope_sig.scope_sig_input_struct in
    let called_scope_return_struct = subscope_sig.scope_sig_output_struct in
    let subscope_vars_defined =
      try SubScopeName.Map.find subindex ctx.subscope_vars
      with SubScopeName.Map.Not_found _ -> ScopeVar.Map.empty
    in
    let subscope_var_not_yet_defined subvar =
      not (ScopeVar.Map.mem subvar subscope_vars_defined)
    in
    let subscope_args =
      List.fold_left
        (fun acc (subvar : scope_var_ctx) ->
          let e =
            if subscope_var_not_yet_defined subvar.scope_var_name then
              (* This is a redundant check. Normally, all subscope variables
                 should have been defined (even an empty definition, if they're
                 not defined by any rule in the source code) by the translation
                 from desugared to the scope language. *)
              Expr.empty_thunked_term m
            else
              let a_var, _, _ =
                ScopeVar.Map.find subvar.scope_var_name subscope_vars_defined
              in
              Expr.make_var a_var (mark_tany m pos_call)
          in
          let field =
            (ScopeVar.Map.find subvar.scope_var_name
               subscope_sig.scope_sig_in_fields)
              .scope_input_name
          in
          StructField.Map.add field e acc)
        StructField.Map.empty all_subscope_input_vars
    in
    let subscope_struct_arg =
      Expr.estruct ~name:called_scope_input_struct ~fields:subscope_args
        (mark_tany m pos_call)
    in
    let all_subscope_output_vars_dcalc =
      List.map
        (fun (subvar : scope_var_ctx) ->
          let sub_dcalc_var =
            Var.make
              (Mark.remove (SubScopeName.get_info subindex)
              ^ "."
              ^ Mark.remove (ScopeVar.get_info subvar.scope_var_name))
          in
          subvar, sub_dcalc_var)
        all_subscope_output_vars
    in
    let subscope_func =
      tag_with_log_entry scope_dcalc_ref BeginCall
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
        (fun (var_ctx, v) next ->
          let field =
            ScopeVar.Map.find var_ctx.scope_var_name
              scope_sig_decl.out_struct_fields
          in
          Bindlib.box_apply2
            (fun next r ->
              ScopeLet
                {
                  scope_let_next = next;
                  scope_let_pos = pos_sigma;
                  scope_let_typ = var_ctx.scope_var_typ, pos_sigma;
                  scope_let_kind = DestructuringSubScopeResults;
                  scope_let_expr =
                    ( EStructAccess
                        { name = called_scope_return_struct; e = r; field },
                      mark_tany m pos_sigma );
                })
            (Bindlib.bind_var v next)
            (Expr.Box.lift
               (Expr.make_var result_tuple_var (mark_tany m pos_sigma))))
        all_subscope_output_vars_dcalc next
    in
    ( (fun next -> call_scope_let (result_bindings_lets next)),
      {
        ctx with
        subscope_vars =
          SubScopeName.Map.add subindex
            (List.fold_left
               (fun acc (var_ctx, dvar) ->
                 ScopeVar.Map.add var_ctx.scope_var_name
                   (dvar, var_ctx.scope_var_typ, var_ctx.scope_var_io)
                   acc)
               ScopeVar.Map.empty all_subscope_output_vars_dcalc)
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
                  Mark.add
                    (Expr.map_ty (fun _ -> scope_let_typ) (Mark.get e))
                    (EAssert (Mark.copy e (EErrorOnEmpty new_e)));
                scope_let_kind = Assertion;
              })
          (Bindlib.bind_var (Var.make "_") next)
          (Expr.Box.lift new_e)),
      ctx )

let translate_rules
    (ctx : 'm ctx)
    (scope_name : ScopeName.t)
    (rules : 'm Scopelang.Ast.rule list)
    ((sigma_name, pos_sigma) : Uid.MarkedString.info)
    (mark : 'm mark)
    (scope_sig : 'm scope_sig_ctx) :
    'm Ast.expr scope_body_expr Bindlib.box * 'm ctx =
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
  let scope_sig_decl = ScopeName.Map.find scope_name ctx.decl_ctx.ctx_scopes in
  let return_exp =
    Expr.estruct ~name:scope_sig.scope_sig_output_struct
      ~fields:
        (ScopeVar.Map.fold
           (fun var (dcalc_var, _, io) acc ->
             if Mark.remove io.Desugared.Ast.io_output then
               let field =
                 ScopeVar.Map.find var scope_sig_decl.out_struct_fields
               in
               StructField.Map.add field
                 (Expr.make_var dcalc_var (mark_tany mark pos_sigma))
                 acc
             else acc)
           new_ctx.scope_vars StructField.Map.empty)
      (mark_tany mark pos_sigma)
  in
  ( scope_lets
      (Bindlib.box_apply
         (fun return_exp -> Result return_exp)
         (Expr.Box.lift return_exp)),
    new_ctx )

(* From a scope declaration and definitions, create the corresponding scope body
   wrapped in the appropriate call convention. *)
let translate_scope_decl
    (ctx : 'm ctx)
    (scope_name : ScopeName.t)
    (sigma : 'm Scopelang.Ast.scope_decl) :
    'm Ast.expr scope_body Bindlib.box * struct_ctx =
  let sigma_info = ScopeName.get_info sigma.scope_decl_name in
  let scope_sig =
    ScopeName.Map.find sigma.scope_decl_name ctx.scopes_parameters.scope_sigs
  in
  let scope_variables = scope_sig.scope_sig_local_vars in
  let ctx = { ctx with scope_name = Some scope_name } in
  let ctx =
    (* the context must be initialized for fresh variables for all only-input
       scope variables *)
    List.fold_left
      (fun ctx scope_var ->
        match Mark.remove scope_var.scope_var_io.io_input with
        | OnlyInput ->
          let scope_var_name = ScopeVar.get_info scope_var.scope_var_name in
          let scope_var_dcalc = Var.make (Mark.remove scope_var_name) in
          {
            ctx with
            scope_vars =
              ScopeVar.Map.add scope_var.scope_var_name
                ( scope_var_dcalc,
                  scope_var.scope_var_typ,
                  scope_var.scope_var_io )
                ctx.scope_vars;
          }
        | _ -> ctx)
      ctx scope_variables
  in
  let date_rounding : date_rounding =
    match
      List.find_opt
        (function Desugared.Ast.DateRounding _, _ -> true)
        sigma.scope_options
    with
    | Some (Desugared.Ast.DateRounding Desugared.Ast.Increasing, _) -> RoundUp
    | Some (DateRounding Decreasing, _) -> RoundDown
    | None -> AbortOnRound
  in
  let ctx = { ctx with date_rounding } in
  let scope_input_var =
    Var.make (Mark.remove (ScopeName.get_info scope_name) ^ "_in")
  in
  let scope_input_struct_name = scope_sig.scope_sig_input_struct in
  let scope_return_struct_name = scope_sig.scope_sig_output_struct in
  let pos_sigma = Mark.get sigma_info in
  let scope_mark =
    (* Find a witness of a mark in the definitions *)
    match sigma.scope_decl_rules with
    | [] ->
      (* Todo: are we sure this can't happen in normal code ? E.g. is calling a
         scope which only defines input variables already an error at this stage
         or not ? *)
      Message.raise_spanned_error pos_sigma "Scope %a has no content"
        ScopeName.format scope_name
    | (Definition (_, _, _, (_, m)) | Assertion (_, m) | Call (_, _, m)) :: _ ->
      m
  in
  let rules_with_return_expr, ctx =
    translate_rules ctx scope_name sigma.scope_decl_rules sigma_info scope_mark
      scope_sig
  in
  let scope_variables =
    List.map
      (fun var_ctx ->
        let dcalc_x, _, _ =
          ScopeVar.Map.find var_ctx.scope_var_name ctx.scope_vars
        in
        var_ctx, dcalc_x)
      scope_variables
  in
  (* first we create variables from the fields of the input struct *)
  let scope_input_variables =
    List.filter
      (fun (var_ctx, _) ->
        match Mark.remove var_ctx.scope_var_io.io_input with
        | NoInput -> false
        | _ -> true)
      scope_variables
  in
  let input_var_typ (var_ctx : scope_var_ctx) =
    match Mark.remove var_ctx.scope_var_io.io_input with
    | OnlyInput -> var_ctx.scope_var_typ, pos_sigma
    | Reentrant -> (
      match var_ctx.scope_var_typ with
      | TArrow _ -> var_ctx.scope_var_typ, pos_sigma
      | _ ->
        ( TArrow ([TLit TUnit, pos_sigma], (var_ctx.scope_var_typ, pos_sigma)),
          pos_sigma ))
    | NoInput -> failwith "should not happen"
  in
  let input_destructurings next =
    List.fold_right
      (fun (var_ctx, v) next ->
        let field =
          (ScopeVar.Map.find var_ctx.scope_var_name
             scope_sig.scope_sig_in_fields)
            .scope_input_name
        in
        Bindlib.box_apply2
          (fun next r ->
            ScopeLet
              {
                scope_let_kind = DestructuringInputStruct;
                scope_let_next = next;
                scope_let_pos = pos_sigma;
                scope_let_typ = input_var_typ var_ctx;
                scope_let_expr =
                  ( EStructAccess
                      { name = scope_input_struct_name; e = r; field },
                    mark_tany scope_mark pos_sigma );
              })
          (Bindlib.bind_var v next)
          (Expr.Box.lift
             (Expr.make_var scope_input_var (mark_tany scope_mark pos_sigma))))
      scope_input_variables next
  in
  let scope_body =
    Bindlib.box_apply
      (fun scope_body_expr ->
        {
          scope_body_expr;
          scope_body_input_struct = scope_input_struct_name;
          scope_body_output_struct = scope_return_struct_name;
        })
      (Bindlib.bind_var scope_input_var
         (input_destructurings rules_with_return_expr))
  in
  let field_map =
    List.fold_left
      (fun acc (var_ctx, _) ->
        let var = var_ctx.scope_var_name in
        let field =
          (ScopeVar.Map.find var scope_sig.scope_sig_in_fields).scope_input_name
        in
        StructField.Map.add field (input_var_typ var_ctx) acc)
      StructField.Map.empty scope_input_variables
  in
  let new_struct_ctx =
    StructName.Map.singleton scope_input_struct_name field_map
  in
  scope_body, new_struct_ctx

let translate_program (prgm : 'm Scopelang.Ast.program) : 'm Ast.program =
  let defs_dependencies = Scopelang.Dependency.build_program_dep_graph prgm in
  Scopelang.Dependency.check_for_cycle_in_defs defs_dependencies;
  let defs_ordering =
    Scopelang.Dependency.get_defs_ordering defs_dependencies
  in
  let decl_ctx = prgm.program_ctx in
  let sctx : 'm scope_sigs_ctx =
    let process_scope_sig scope_name scope =
      let scope_path = ScopeName.path scope_name in
      let scope_ref =
        if scope_path = [] then
          let v = Var.make (Mark.remove (ScopeName.get_info scope_name)) in
          Local_scope_ref v
        else
          External_scope_ref
            (Mark.copy (ScopeName.get_info scope_name) scope_name)
      in
      let scope_info =
        try
          ScopeName.Map.find scope_name
            (Program.module_ctx decl_ctx scope_path).ctx_scopes
        with ScopeName.Map.Not_found _ ->
          Message.raise_spanned_error
            (Mark.get (ScopeName.get_info scope_name))
            "Could not find scope %a" ScopeName.format scope_name
      in
      let scope_sig_in_fields =
        (* Output fields have already been generated and added to the program
           ctx at this point, because they are visible to the user (manipulated
           as the return type of ScopeCalls) ; but input fields are used purely
           internally and need to be created here to implement the call
           convention for scopes. *)
        ScopeVar.Map.filter_map
          (fun dvar (typ, vis) ->
            match Mark.remove vis.Desugared.Ast.io_input with
            | NoInput -> None
            | OnlyInput | Reentrant ->
              let info = ScopeVar.get_info dvar in
              let s = Mark.remove info ^ "_in" in
              Some
                {
                  scope_input_name = StructField.fresh (s, Mark.get info);
                  scope_input_io = vis.Desugared.Ast.io_input;
                  scope_input_typ = Mark.remove typ;
                })
          scope.Scopelang.Ast.scope_sig
      in
      {
        scope_sig_local_vars =
          List.map
            (fun (scope_var, (tau, vis)) ->
              {
                scope_var_name = scope_var;
                scope_var_typ = Mark.remove tau;
                scope_var_io = vis;
              })
            (ScopeVar.Map.bindings scope.scope_sig);
        scope_sig_scope_ref = scope_ref;
        scope_sig_input_struct = scope_info.in_struct_name;
        scope_sig_output_struct = scope_info.out_struct_name;
        scope_sig_in_fields;
      }
    in
    let rec process_modules prg =
      {
        scope_sigs =
          ScopeName.Map.mapi
            (fun scope_name (scope_decl, _) ->
              process_scope_sig scope_name scope_decl)
            prg.Scopelang.Ast.program_scopes;
        scope_sigs_modules =
          ModuleName.Map.map process_modules prg.Scopelang.Ast.program_modules;
      }
    in
    {
      scope_sigs =
        ScopeName.Map.mapi
          (fun scope_name (scope_decl, _) ->
            process_scope_sig scope_name scope_decl)
          prgm.Scopelang.Ast.program_scopes;
      scope_sigs_modules =
        ModuleName.Map.map process_modules prgm.Scopelang.Ast.program_modules;
    }
  in
  let rec gather_module_in_structs acc sctx =
    (* Expose all added in_structs from submodules at toplevel *)
    ModuleName.Map.fold
      (fun _ scope_sigs acc ->
        let acc = gather_module_in_structs acc scope_sigs.scope_sigs_modules in
        ScopeName.Map.fold
          (fun _ scope_sig_ctx acc ->
            let fields =
              ScopeVar.Map.fold
                (fun _ sivc acc ->
                  let pos =
                    Mark.get (StructField.get_info sivc.scope_input_name)
                  in
                  StructField.Map.add sivc.scope_input_name
                    (sivc.scope_input_typ, pos)
                    acc)
                scope_sig_ctx.scope_sig_in_fields StructField.Map.empty
            in
            StructName.Map.add scope_sig_ctx.scope_sig_input_struct fields acc)
          scope_sigs.scope_sigs acc)
      sctx acc
  in
  let decl_ctx =
    {
      decl_ctx with
      ctx_structs =
        gather_module_in_structs decl_ctx.ctx_structs sctx.scope_sigs_modules;
    }
  in
  let top_ctx =
    let toplevel_vars =
      TopdefName.Map.mapi
        (fun name (_, ty) ->
          Var.make (Mark.remove (TopdefName.get_info name)), Mark.remove ty)
        prgm.Scopelang.Ast.program_topdefs
    in
    {
      decl_ctx;
      scope_name = None;
      scopes_parameters = sctx;
      scope_vars = ScopeVar.Map.empty;
      subscope_vars = SubScopeName.Map.empty;
      toplevel_vars;
      date_rounding = AbortOnRound;
    }
  in
  (* the resulting expression is the list of definitions of all the scopes,
     ending with the top-level scope. The decl_ctx is filled in left-to-right
     order, then the chained scopes aggregated from the right. *)
  let rec translate_defs ctx = function
    | [] -> Bindlib.box Nil, ctx
    | def :: next ->
      let ctx, dvar, def =
        match def with
        | Scopelang.Dependency.Topdef gname ->
          let expr, ty = TopdefName.Map.find gname prgm.program_topdefs in
          let expr = translate_expr ctx expr in
          ( ctx,
            fst (TopdefName.Map.find gname ctx.toplevel_vars),
            Bindlib.box_apply
              (fun e -> Topdef (gname, ty, e))
              (Expr.Box.lift expr) )
        | Scopelang.Dependency.Scope scope_name ->
          let scope = ScopeName.Map.find scope_name prgm.program_scopes in
          let scope_body, scope_in_struct =
            translate_scope_decl ctx scope_name (Mark.remove scope)
          in
          let scope_var =
            match
              (ScopeName.Map.find scope_name sctx.scope_sigs)
                .scope_sig_scope_ref
            with
            | Local_scope_ref v -> v
            | External_scope_ref _ -> assert false
          in
          ( {
              ctx with
              decl_ctx =
                {
                  ctx.decl_ctx with
                  ctx_structs =
                    StructName.Map.union
                      (fun _ _ -> assert false)
                      ctx.decl_ctx.ctx_structs scope_in_struct;
                };
            },
            scope_var,
            Bindlib.box_apply
              (fun body -> ScopeDef (scope_name, body))
              scope_body )
      in
      let scope_next, ctx = translate_defs ctx next in
      let next_bind = Bindlib.bind_var dvar scope_next in
      ( Bindlib.box_apply2
          (fun item next_bind -> Cons (item, next_bind))
          def next_bind,
        ctx )
  in
  let items, ctx = translate_defs top_ctx defs_ordering in
  {
    code_items = Bindlib.unbox items;
    decl_ctx = ctx.decl_ctx;
    module_name = prgm.Scopelang.Ast.program_module_name;
    lang = prgm.program_lang;
  }
