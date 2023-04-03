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
  scope_input_io : Desugared.Ast.io_input Marked.pos;
  scope_input_typ : naked_typ;
}

type 'm scope_sig_ctx = {
  scope_sig_local_vars : scope_var_ctx list;  (** List of scope variables *)
  scope_sig_scope_var : 'm Ast.expr Var.t;  (** Var representing the scope *)
  scope_sig_input_var : 'm Ast.expr Var.t;
      (** Var representing the scope input inside the scope func *)
  scope_sig_input_struct : StructName.t;  (** Scope input *)
  scope_sig_output_struct : StructName.t;  (** Scope output *)
  scope_sig_in_fields : scope_input_var_ctx ScopeVar.Map.t;
      (** Mapping between the input scope variables and the input struct fields. *)
  scope_sig_out_fields : StructField.t ScopeVar.Map.t;
      (** Mapping between the output scope variables and the output struct
          fields. TODO: could likely be removed now that we have it in the
          program ctx *)
}

type 'm scope_sigs_ctx = 'm scope_sig_ctx ScopeName.Map.t

type 'm ctx = {
  structs : struct_ctx;
  enums : enum_ctx;
  scope_name : ScopeName.t option;
  scopes_parameters : 'm scope_sigs_ctx;
  toplevel_vars : ('m Ast.expr Var.t * naked_typ) TopdefName.Map.t;
  scope_vars :
    ('m Ast.expr Var.t * naked_typ * Desugared.Ast.io) ScopeVar.Map.t;
  subscope_vars :
    ('m Ast.expr Var.t * naked_typ * Desugared.Ast.io) ScopeVar.Map.t
    SubScopeName.Map.t;
  local_vars : ('m Scopelang.Ast.expr, 'm Ast.expr Var.t) Var.Map.t;
  date_rounding : date_rounding;
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

let merge_defaults
    ~(is_func : bool)
    (caller : (dcalc, 'm mark) boxed_gexpr)
    (callee : (dcalc, 'm mark) boxed_gexpr) : (dcalc, 'm mark) boxed_gexpr =
  (* the merging of the two defaults, from the reentrant caller and the callee,
     is straightfoward in the general case and a little subtler when the
     variable being defined is a function. *)
  if is_func then
    let m_callee = Marked.get_mark callee in
    let unboxed_callee = Expr.unbox callee in
    match Marked.unmark unboxed_callee with
    | EAbs { binder; tys } ->
      let vars, body = Bindlib.unmbind binder in
      let m_body = Marked.get_mark body in
      let caller =
        let m = Marked.get_mark caller in
        let pos = Expr.mark_pos m in
        Expr.make_app caller
          (List.map2
             (fun (var : (dcalc, 'm mark) naked_gexpr Bindlib.var) ty ->
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
             (Marked.mark (Expr.mark_pos m_callee) (TLit TBool)))
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
      Expr.eerroronempty (Expr.edefault [caller] ltrue callee m) m
    in
    body

let tag_with_log_entry
    (e : 'm Ast.expr boxed)
    (l : log_entry)
    (markings : Uid.MarkedString.info list) : 'm Ast.expr boxed =
  let m = mark_tany (Marked.get_mark e) (Expr.pos e) in
  Expr.eapp (Expr.eop (Log (l, markings)) [TAny, Expr.pos e] m) [e] m

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
  let pos = Marked.get_mark io_in in
  match Marked.unmark io_in with
  | Desugared.Ast.NoInput -> invalid_arg "thunk_scope_arg"
  | Desugared.Ast.OnlyInput -> Expr.eerroronempty e (Marked.get_mark e)
  | Desugared.Ast.Reentrant ->
    (* we don't need to thunk expressions that are already functions *)
    if is_func then e
    else Expr.make_abs [| silent_var |] e [TLit TUnit, pos] pos

let rec translate_expr (ctx : 'm ctx) (e : 'm Scopelang.Ast.expr) :
    'm Ast.expr boxed =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EVar v -> Expr.evar (Var.Map.find v ctx.local_vars) m
  | ELit
      ((LBool _ | LInt _ | LRat _ | LMoney _ | LUnit | LDate _ | LDuration _) as
      l) ->
    Expr.elit l m
  | EStruct { name; fields } ->
    let fields = StructField.Map.map (translate_expr ctx) fields in
    Expr.estruct name fields m
  | EStructAccess { e; field; name } ->
    Expr.estructaccess (translate_expr ctx e) field name m
  | ETuple es -> Expr.etuple (List.map (translate_expr ctx) es) m
  | ETupleAccess { e; index; size } ->
    Expr.etupleaccess (translate_expr ctx e) index size m
  | EInj { e; cons; name } ->
    let e' = translate_expr ctx e in
    Expr.einj e' cons name m
  | EMatch { e = e1; name; cases = e_cases } ->
    let enum_sig = EnumName.Map.find name ctx.enums in
    let d_cases, remaining_e_cases =
      (* FIXME: these checks should probably be moved to a better place *)
      EnumConstructor.Map.fold
        (fun constructor _ (d_cases, e_cases) ->
          let case_e =
            try EnumConstructor.Map.find constructor e_cases
            with Not_found ->
              Errors.raise_spanned_error (Expr.pos e)
                "The constructor %a of enum %a is missing from this pattern \
                 matching"
                EnumConstructor.format_t constructor EnumName.format_t name
          in
          let case_d = translate_expr ctx case_e in
          ( EnumConstructor.Map.add constructor case_d d_cases,
            EnumConstructor.Map.remove constructor e_cases ))
        enum_sig
        (EnumConstructor.Map.empty, e_cases)
    in
    if not (EnumConstructor.Map.is_empty remaining_e_cases) then
      Errors.raise_spanned_error (Expr.pos e)
        "Pattern matching is incomplete for enum %a: missing cases %a"
        EnumName.format_t name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (fun fmt (case_name, _) -> EnumConstructor.format_t fmt case_name))
        (EnumConstructor.Map.bindings remaining_e_cases);
    let e1 = translate_expr ctx e1 in
    Expr.ematch e1 name d_cases m
  | EScopeCall { scope; args } ->
    let pos = Expr.mark_pos m in
    let sc_sig = ScopeName.Map.find scope ctx.scopes_parameters in
    let in_var_map =
      ScopeVar.Map.merge
        (fun var_name (str_field : scope_input_var_ctx option) expr ->
          let expr =
            match str_field, expr with
            | Some { scope_input_io = Desugared.Ast.Reentrant, _; _ }, None ->
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
            Errors.raise_multispanned_error
              [
                None, pos;
                ( Some "Declaration of the missing input variable",
                  Marked.get_mark
                    (StructField.get_info var_ctx.scope_input_name) );
              ]
              "Definition of input variable '%a' missing in this scope call"
              ScopeVar.format_t var_name
          | None, Some _ ->
            Errors.raise_multispanned_error
              [
                None, pos;
                ( Some "Declaration of scope '%a'",
                  Marked.get_mark (ScopeName.get_info scope) );
              ]
              "Unknown input variable '%a' in scope call of '%a'"
              ScopeVar.format_t var_name ScopeName.format_t scope)
        sc_sig.scope_sig_in_fields args
    in
    let field_map =
      ScopeVar.Map.fold
        (fun _ (fld, e) acc -> StructField.Map.add fld e acc)
        in_var_map StructField.Map.empty
    in
    let arg_struct =
      Expr.estruct sc_sig.scope_sig_input_struct field_map (mark_tany m pos)
    in
    let called_func =
      tag_with_log_entry
        (Expr.evar sc_sig.scope_sig_scope_var (mark_tany m pos))
        BeginCall
        [ScopeName.get_info scope; Marked.mark (Expr.pos e) "direct"]
    in
    let single_arg =
      tag_with_log_entry arg_struct
        (VarDef (TStruct sc_sig.scope_sig_input_struct))
        [
          ScopeName.get_info scope;
          Marked.mark (Expr.pos e) "direct";
          Marked.mark (Expr.pos e) "input";
        ]
    in
    let direct_output_info =
      [
        ScopeName.get_info scope;
        Marked.mark (Expr.pos e) "direct";
        Marked.mark (Expr.pos e) "output";
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
       to insert logging instructions*)
    let result_var = Var.make "result" in
    let result_eta_expanded_var = Var.make "result" in
    (* result_eta_expanded = { struct_output_function_field = lambda x -> log
       (struct_output.struct_output_function_field x) ... } *)
    let result_eta_expanded =
      Expr.estruct sc_sig.scope_sig_output_struct
        (StructField.Map.mapi
           (fun field typ ->
             let original_field_expr =
               Expr.estructaccess
                 (Expr.make_var result_var
                    (Expr.with_ty m
                       (TStruct sc_sig.scope_sig_output_struct, Expr.pos e)))
                 field sc_sig.scope_sig_output_struct (Expr.with_ty m typ)
             in
             match Marked.unmark typ with
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
                                 (Expr.make_var param_var (Expr.with_ty m t_in))
                                 (VarDef (Marked.unmark t_in))
                                 (f_markings
                                 @ [
                                     Marked.mark (Expr.pos e)
                                       ("input" ^ string_of_int i);
                                   ])))
                          (Expr.with_ty m t_out))
                       (VarDef (Marked.unmark t_out))
                       (f_markings @ [Marked.mark (Expr.pos e) "output"]))
                    EndCall f_markings)
                 ts_in (Expr.pos e)
             | _ -> original_field_expr)
           (StructName.Map.find sc_sig.scope_sig_output_struct ctx.structs))
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
              (Marked.mark
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
               (VarDef (TStruct sc_sig.scope_sig_output_struct))
               direct_output_info)
            EndCall
            [ScopeName.get_info scope; Marked.mark (Expr.pos e) "direct"])
         (Expr.pos e))
      (Expr.pos e)
  | EApp { f; args } ->
    (* We insert various log calls to record arguments and outputs of
       user-defined functions belonging to scopes *)
    let e1_func = translate_expr ctx f in
    let markings =
      match ctx.scope_name, Marked.unmark f with
      | Some sname, ELocation loc -> (
        match loc with
        | ScopelangScopeVar (v, _) ->
          [ScopeName.get_info sname; ScopeVar.get_info v]
        | SubScopeVar (s, _, (v, _)) ->
          [ScopeName.get_info s; ScopeVar.get_info v]
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
      (* NOTE: this is a temporary solution, it works because it's assume that
         all function calls are from scope variable. However, this will change
         -- for more information see
         https://github.com/CatalaLang/catala/pull/280#discussion_r898851693. *)
      let retrieve_in_and_out_typ_or_any var vars =
        let _, typ, _ = ScopeVar.Map.find (Marked.unmark var) vars in
        match typ with
        | TArrow (marked_input_typ, marked_output_typ) ->
          ( List.map Marked.unmark marked_input_typ,
            Marked.unmark marked_output_typ )
        | _ -> ListLabels.map new_args ~f:(fun _ -> TAny), TAny
      in
      match Marked.unmark f with
      | ELocation (ScopelangScopeVar var) ->
        retrieve_in_and_out_typ_or_any var ctx.scope_vars
      | ELocation (SubScopeVar (_, sname, var)) ->
        ctx.subscope_vars
        |> SubScopeName.Map.find (Marked.unmark sname)
        |> retrieve_in_and_out_typ_or_any var
      | ELocation (ToplevelVar tvar) -> (
        let _, typ =
          TopdefName.Map.find (Marked.unmark tvar) ctx.toplevel_vars
        in
        match typ with
        | TArrow (tin, (tout, _)) -> List.map Marked.unmark tin, tout
        | _ ->
          Errors.raise_spanned_error (Expr.pos e)
            "Application of non-function toplevel variable")
      | _ -> ListLabels.map new_args ~f:(fun _ -> TAny), TAny
    in

    (* Cli.debug_format "new_args %d, input_typs: %d, input_typs %a"
       (List.length new_args) (List.length input_typs) (Format.pp_print_list
       Print.typ_debug) (List.map (Marked.mark Pos.no_pos) input_typs); *)
    let new_args =
      ListLabels.mapi (List.combine new_args input_typs)
        ~f:(fun i (new_arg, input_typ) ->
          match markings with
          | _ :: _ as m ->
            tag_with_log_entry new_arg (VarDef input_typ)
              (m @ [Marked.mark (Expr.pos e) ("input" ^ string_of_int i)])
          | _ -> new_arg)
    in

    let new_e = Expr.eapp e1_func new_args m in
    let new_e =
      match markings with
      | [] -> new_e
      | m ->
        tag_with_log_entry
          (tag_with_log_entry new_e (VarDef output_typ)
             (m @ [Marked.mark (Expr.pos e) "output"]))
          EndCall m
    in
    new_e
  | EAbs { binder; tys } ->
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
    Expr.eabs binder tys m
  | EDefault { excepts; just; cons } ->
    let excepts = collapse_similar_outcomes excepts in
    Expr.edefault
      (List.map (translate_expr ctx) excepts)
      (translate_expr ctx just) (translate_expr ctx cons) m
  | ELocation (ScopelangScopeVar a) ->
    let v, _, _ = ScopeVar.Map.find (Marked.unmark a) ctx.scope_vars in
    Expr.evar v m
  | ELocation (SubScopeVar (_, s, a)) -> (
    try
      let v, _, _ =
        ScopeVar.Map.find (Marked.unmark a)
          (SubScopeName.Map.find (Marked.unmark s) ctx.subscope_vars)
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
        "The variable %a.%a cannot be used here, as it is not part of subscope \
         %a's results. Maybe you forgot to qualify it as an output?"
        SubScopeName.format_t (Marked.unmark s) ScopeVar.format_t
        (Marked.unmark a) SubScopeName.format_t (Marked.unmark s))
  | ELocation (ToplevelVar v) ->
    let v, _ = TopdefName.Map.find (Marked.unmark v) ctx.toplevel_vars in
    Expr.evar v m
  | EIfThenElse { cond; etrue; efalse } ->
    Expr.eifthenelse (translate_expr ctx cond) (translate_expr ctx etrue)
      (translate_expr ctx efalse)
      m
  | EOp { op = Add_dat_dur _; tys } ->
    Expr.eop (Add_dat_dur ctx.date_rounding) tys m
  | EOp { op; tys } -> Expr.eop (Operator.translate op) tys m
  | EEmptyError -> Expr.eemptyerror m
  | EErrorOnEmpty e' -> Expr.eerroronempty (translate_expr ctx e') m
  | EArray es -> Expr.earray (List.map (translate_expr ctx) es) m

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
  | Definition ((ScopelangScopeVar a, var_def_pos), tau, a_io, e) ->
    let pos_mark, pos_mark_as = pos_mark_mk e in
    let a_name = ScopeVar.get_info (Marked.unmark a) in
    let a_var = Var.make (Marked.unmark a_name) in
    let new_e = translate_expr ctx e in
    let a_expr = Expr.make_var a_var (pos_mark var_def_pos) in
    let merged_expr =
      match Marked.unmark a_io.io_input with
      | OnlyInput -> failwith "should not happen"
      (* scopelang should not contain any definitions of input only variables *)
      | Reentrant ->
        merge_defaults
          ~is_func:
            (match Marked.unmark tau with TArrow _ -> true | _ -> false)
          a_expr new_e
      | NoInput -> Expr.eerroronempty new_e (pos_mark_as a_name)
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
          ScopeVar.Map.add (Marked.unmark a)
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
    let is_func =
      match Marked.unmark tau with TArrow _ -> true | _ -> false
    in
    let thunked_or_nonempty_new_e =
      thunk_scope_arg ~is_func a_io.Desugared.Ast.io_input new_e
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
          SubScopeName.Map.update (Marked.unmark subs_index)
            (fun map ->
              match map with
              | Some map ->
                Some
                  (ScopeVar.Map.add (Marked.unmark subs_var)
                     (a_var, Marked.unmark tau, a_io)
                     map)
              | None ->
                Some
                  (ScopeVar.Map.singleton (Marked.unmark subs_var)
                     (a_var, Marked.unmark tau, a_io)))
            ctx.subscope_vars;
      } )
  | Definition ((ToplevelVar _, _), _, _, _) ->
    assert false
    (* A global variable can't be defined locally. The [Definition] constructor
       could be made more specific to avoid this case, but the added complexity
       didn't seem worth it *)
  | Call (subname, subindex, m) ->
    let subscope_sig = ScopeName.Map.find subname ctx.scopes_parameters in
    let all_subscope_vars = subscope_sig.scope_sig_local_vars in
    let all_subscope_input_vars =
      List.filter
        (fun var_ctx ->
          match Marked.unmark var_ctx.scope_var_io.Desugared.Ast.io_input with
          | NoInput -> false
          | _ -> true)
        all_subscope_vars
    in
    let all_subscope_output_vars =
      List.filter
        (fun var_ctx ->
          Marked.unmark var_ctx.scope_var_io.Desugared.Ast.io_output)
        all_subscope_vars
    in
    let scope_dcalc_var = subscope_sig.scope_sig_scope_var in
    let called_scope_input_struct = subscope_sig.scope_sig_input_struct in
    let called_scope_return_struct = subscope_sig.scope_sig_output_struct in
    let subscope_vars_defined =
      try SubScopeName.Map.find subindex ctx.subscope_vars
      with Not_found -> ScopeVar.Map.empty
    in
    let subscope_var_not_yet_defined subvar =
      not (ScopeVar.Map.mem subvar subscope_vars_defined)
    in
    let pos_call = Marked.get_mark (SubScopeName.get_info subindex) in
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
      Expr.estruct called_scope_input_struct subscope_args
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
        (fun (var_ctx, v) next ->
          let field =
            ScopeVar.Map.find var_ctx.scope_var_name
              subscope_sig.scope_sig_out_fields
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
                  Marked.mark
                    (Expr.map_ty (fun _ -> scope_let_typ) (Marked.get_mark e))
                    (EAssert (Marked.same_mark_as (EErrorOnEmpty new_e) e));
                scope_let_kind = Assertion;
              })
          (Bindlib.bind_var (Var.make "_") next)
          (Expr.Box.lift new_e)),
      ctx )

let translate_rules
    (ctx : 'm ctx)
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
  let return_exp =
    Expr.estruct scope_sig.scope_sig_output_struct
      (ScopeVar.Map.fold
         (fun var (dcalc_var, _, io) acc ->
           if Marked.unmark io.Desugared.Ast.io_output then
             let field = ScopeVar.Map.find var scope_sig.scope_sig_out_fields in
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

let translate_scope_decl
    (ctx : 'm ctx)
    (scope_name : ScopeName.t)
    (sigma : 'm Scopelang.Ast.scope_decl) :
    'm Ast.expr scope_body Bindlib.box * struct_ctx =
  let sigma_info = ScopeName.get_info sigma.scope_decl_name in
  let scope_sig =
    ScopeName.Map.find sigma.scope_decl_name ctx.scopes_parameters
  in
  let scope_variables = scope_sig.scope_sig_local_vars in
  let ctx = { ctx with scope_name = Some scope_name } in
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
  let scope_input_var = scope_sig.scope_sig_input_var in
  let scope_input_struct_name = scope_sig.scope_sig_input_struct in
  let scope_return_struct_name = scope_sig.scope_sig_output_struct in
  let pos_sigma = Marked.get_mark sigma_info in
  let rules_with_return_expr, ctx =
    translate_rules ctx sigma.scope_decl_rules sigma_info sigma.scope_mark
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
        match Marked.unmark var_ctx.scope_var_io.io_input with
        | NoInput -> false
        | _ -> true)
      scope_variables
  in
  let input_var_typ (var_ctx : scope_var_ctx) =
    match Marked.unmark var_ctx.scope_var_io.io_input with
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
                    mark_tany sigma.scope_mark pos_sigma );
              })
          (Bindlib.bind_var v next)
          (Expr.Box.lift
             (Expr.make_var scope_input_var
                (mark_tany sigma.scope_mark pos_sigma))))
      scope_input_variables next
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

let translate_program (prgm : 'm Scopelang.Ast.program) : 'm Ast.program =
  let defs_dependencies = Scopelang.Dependency.build_program_dep_graph prgm in
  Scopelang.Dependency.check_for_cycle_in_defs defs_dependencies;
  let defs_ordering =
    Scopelang.Dependency.get_defs_ordering defs_dependencies
  in
  let decl_ctx = prgm.program_ctx in
  let sctx : 'm scope_sigs_ctx =
    ScopeName.Map.mapi
      (fun scope_name scope ->
        let scope_dvar =
          Var.make
            (Marked.unmark
               (ScopeName.get_info scope.Scopelang.Ast.scope_decl_name))
        in
        let scope_return = ScopeName.Map.find scope_name decl_ctx.ctx_scopes in
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
          ScopeVar.Map.filter_map
            (fun dvar (typ, vis) ->
              match Marked.unmark vis.Desugared.Ast.io_input with
              | NoInput -> None
              | OnlyInput | Reentrant ->
                let info = ScopeVar.get_info dvar in
                let s = Marked.unmark info ^ "_in" in
                Some
                  {
                    scope_input_name =
                      StructField.fresh (s, Marked.get_mark info);
                    scope_input_io = vis.Desugared.Ast.io_input;
                    scope_input_typ = Marked.unmark typ;
                  })
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
              (ScopeVar.Map.bindings scope.scope_sig);
          scope_sig_scope_var = scope_dvar;
          scope_sig_input_var = scope_input_var;
          scope_sig_input_struct = scope_input_struct_name;
          scope_sig_output_struct = scope_return.out_struct_name;
          scope_sig_in_fields;
          scope_sig_out_fields = scope_return.out_struct_fields;
        })
      prgm.Scopelang.Ast.program_scopes
  in
  let top_ctx =
    let toplevel_vars =
      TopdefName.Map.mapi
        (fun name (_, ty) ->
          Var.make (Marked.unmark (TopdefName.get_info name)), Marked.unmark ty)
        prgm.Scopelang.Ast.program_topdefs
    in
    {
      structs = decl_ctx.ctx_structs;
      enums = decl_ctx.ctx_enums;
      scope_name = None;
      scopes_parameters = sctx;
      scope_vars = ScopeVar.Map.empty;
      subscope_vars = SubScopeName.Map.empty;
      local_vars = Var.Map.empty;
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
            translate_scope_decl ctx scope_name scope
          in
          ( {
              ctx with
              structs =
                StructName.Map.union
                  (fun _ _ -> assert false)
                  ctx.structs scope_in_struct;
            },
            (ScopeName.Map.find scope_name sctx).scope_sig_scope_var,
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
    decl_ctx = { decl_ctx with ctx_structs = ctx.structs };
  }
