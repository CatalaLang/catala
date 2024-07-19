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
module S = Scopelang.Ast

type scope_var_ctx = {
  scope_var_name : ScopeVar.t;
  scope_var_typ : naked_typ;
  scope_var_io : Desugared.Ast.io;
}

type scope_input_var_ctx = {
  scope_input_name : StructField.t;
  scope_input_io : Runtime.io_input Mark.pos;
  scope_input_typ : naked_typ;
  scope_input_thunked : bool;
      (* For reentrant variables: if true, the type t of the field has been
         changed to (unit -> t). Otherwise, the type was already a function and
         wasn't changed so no additional wrapping will be needed *)
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

type 'm ctx = {
  decl_ctx : decl_ctx;
  scope_name : ScopeName.t option;
  scopes_parameters : 'm scope_sig_ctx ScopeName.Map.t;
  toplevel_vars : ('m Ast.expr Var.t * naked_typ) TopdefName.Map.t;
  scope_vars :
    ('m Ast.expr Var.t * naked_typ * Desugared.Ast.io) ScopeVar.Map.t;
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
          tys pos
      in
      let ltrue =
        Expr.elit (LBool true)
          (Expr.with_ty m_callee
             (Mark.add (Expr.mark_pos m_callee) (TLit TBool)))
      in

      let cons = Expr.make_puredefault (Expr.rebox body) in
      let d =
        Expr.edefault ~excepts:[caller] ~just:ltrue ~cons (Mark.get cons)
      in
      Expr.make_abs vars (Expr.make_erroronempty d) tys (Expr.mark_pos m_callee)
    | _ -> assert false
    (* should not happen because there should always be a lambda at the
       beginning of a default with a function type *)
  else
    let caller =
      let m = Mark.get caller in
      let pos = Expr.mark_pos m in
      Expr.make_app caller
        [Expr.elit LUnit (Expr.with_ty m (Mark.add pos (TLit TUnit)))]
        [TLit TUnit, pos]
        pos
    in
    let body =
      let m = Mark.get callee in
      let ltrue =
        Expr.elit (LBool true)
          (Expr.with_ty m (Mark.add (Expr.mark_pos m) (TLit TBool)))
      in
      let cons = Expr.make_puredefault callee in
      Expr.make_erroronempty
        (Expr.edefault ~excepts:[caller] ~just:ltrue ~cons (Mark.get cons))
    in
    body

let tag_with_log_entry
    (e : 'm Ast.expr boxed)
    (l : log_entry)
    (markings : Uid.MarkedString.info list) : 'm Ast.expr boxed =
  let m = mark_tany (Mark.get e) (Expr.pos e) in

  if Global.options.trace then
    let pos = Expr.pos e in
    Expr.eappop ~op:(Log (l, markings), pos) ~tys:[TAny, pos] ~args:[e] m
  else e

(* In a list of exceptions, it is normally an error if more than a single one
   apply at the same time. This relaxes this constraint slightly, allowing a
   conflict if all the triggered conflicting exception yield syntactically equal
   results (and as long as none of these exceptions have exceptions themselves)

   NOTE: the choice of the exception that will be triggered and show in the
   trace is arbitrary (but deterministic). *)
let collapse_similar_outcomes (type m) (excepts : m S.expr list) : m S.expr list
    =
  let module ExprMap = Map.Make (struct
    type t = m S.expr

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

let input_var_needs_thunking typ io_in =
  (* For "context" (or reentrant) variables, we thunk them as [(fun () -> e)] so
     that we can put them in default terms at the initialisation of the function
     body, allowing an empty error to recover the default value. *)
  match Mark.remove io_in.Desugared.Ast.io_input, typ with
  | Runtime.Reentrant, TArrow _ ->
    false (* we don't need to thunk expressions that are already functions *)
  | Runtime.Reentrant, _ -> true
  | _ -> false

let input_var_typ typ io_in =
  let pos = Mark.get io_in.Desugared.Ast.io_input in
  if input_var_needs_thunking typ io_in then
    TArrow ([TLit TUnit, pos], (typ, pos)), pos
  else typ, pos

let thunk_scope_arg var_ctx e =
  match var_ctx.scope_input_io, var_ctx.scope_input_thunked with
  | (Runtime.NoInput, _), _ -> invalid_arg "thunk_scope_arg"
  | (Runtime.OnlyInput, _), false -> e
  | (Runtime.Reentrant, _), false -> e
  | (Runtime.Reentrant, pos), true ->
    Expr.make_abs [| Var.make "_" |] e [TLit TUnit, pos] pos
  | _ -> assert false

let rec translate_expr (ctx : 'm ctx) (e : 'm S.expr) : 'm Ast.expr boxed =
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
              Message.error ~pos:(Expr.pos e)
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
      Message.error ~pos:(Expr.pos e)
        "Pattern matching is incomplete for enum %a: missing cases %a"
        EnumName.format name
        (EnumConstructor.Map.format_keys ~pp_sep:(fun fmt () ->
             Format.fprintf fmt ", "))
        remaining_e_cases;
    let e1 = translate_expr ctx e1 in
    Expr.ematch ~e:e1 ~name ~cases:d_cases m
  | EScopeCall { scope; args } ->
    let pos = Expr.mark_pos m in
    let sc_sig = ScopeName.Map.find scope ctx.scopes_parameters in
    let in_var_map =
      ScopeVar.Map.merge
        (fun var_name (str_field : scope_input_var_ctx option)
             (arg : _ scope_call_arg option) ->
          match str_field, arg with
          | None, None -> assert false
          | Some ({ scope_input_io = Reentrant, iopos; _ } as var_ctx), None ->
            let ty0 =
              match var_ctx.scope_input_typ with
              | TArrow ([_], ty) -> ty
              | _ -> assert false
              (* reentrant field must be thunked with correct function type at
                 this point *)
            in
            Some
              ( var_ctx.scope_input_name,
                Expr.make_abs
                  [| Var.make "_" |]
                  (Expr.eempty (Expr.with_ty m ty0))
                  [TAny, iopos]
                  pos )
          | Some var_ctx, Some (ScopeVarArg e) ->
            Some
              ( var_ctx.scope_input_name,
                thunk_scope_arg var_ctx (translate_expr ctx e) )
          | Some _var_ctx, Some (SubScopeVarArg _) -> failwith "unimplemented !"
          | Some var_ctx, None ->
            Message.error ~pos
              ~extra_pos:
                [
                  ( "Declaration of the missing input variable",
                    Mark.get (StructField.get_info var_ctx.scope_input_name) );
                ]
              "Definition of input variable '%a' missing in this scope call"
              ScopeVar.format var_name
          | None, Some (ScopeVarArg e) ->
            Message.error
              ~suggestion:
                (List.map
                   (fun v -> Mark.remove (ScopeVar.get_info v))
                   (ScopeVar.Map.keys sc_sig.scope_sig_in_fields))
              ~fmt_pos:
                [
                  ignore, Expr.pos e;
                  ( (fun ppf ->
                      Format.fprintf ppf "Declaration of scope %a"
                        ScopeName.format scope),
                    Mark.get (ScopeName.get_info scope) );
                ]
              "Unknown input variable '%a' in scope call of '%a'"
              ScopeVar.format var_name ScopeName.format scope
          | None, Some (SubScopeVarArg _) -> failwith "unimplemented !")
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
    let calling_expr =
      Expr.eapp ~f:called_func ~args:[single_arg]
        ~tys:[TStruct sc_sig.scope_sig_input_struct, pos]
        m
    in
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
                 let args =
                   List.mapi
                     (fun i (param_var, t_in) ->
                       tag_with_log_entry
                         (Expr.make_var param_var (Expr.with_ty m t_in))
                         (VarDef
                            {
                              log_typ = Mark.remove t_in;
                              log_io_output = false;
                              log_io_input = OnlyInput;
                            })
                         (f_markings
                         @ [Mark.add (Expr.pos e) ("input" ^ string_of_int i)]))
                     (List.combine params_vars ts_in)
                 in
                 Expr.make_abs
                   (Array.of_list params_vars)
                   (tag_with_log_entry
                      (tag_with_log_entry
                         (Expr.eapp
                            ~f:
                              (tag_with_log_entry original_field_expr BeginCall
                                 f_markings)
                            ~args ~tys:ts_in (Expr.with_ty m t_out))
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
  | EApp { f; args; tys } ->
    (* We insert various log calls to record arguments and outputs of
       user-defined functions belonging to scopes *)
    let e1_func = translate_expr ctx f in
    let markings =
      match ctx.scope_name, Mark.remove f with
      | Some sname, ELocation loc -> (
        match loc with
        | ScopelangScopeVar { name = v, _; _ } ->
          [ScopeName.get_info sname; ScopeVar.get_info v]
        | ToplevelVar _ -> [])
      | _ -> []
    in
    let e1_func =
      match markings with
      | [] -> e1_func
      | m -> tag_with_log_entry e1_func BeginCall m
    in
    let new_args = List.map (translate_expr ctx) args in
    let input_typs = List.map Mark.remove tys in
    let output_typ =
      (* NOTE: this is a temporary solution, it works because it's assumed that
         all function have explicit types. However, this will change -- for more
         information see
         https://github.com/CatalaLang/catala/pull/280#discussion_r898851693. *)
      let retrieve_out_typ_or_any var vars =
        let _, typ, _ = ScopeVar.Map.find (Mark.remove var) vars in
        match typ with
        | TArrow (_, marked_output_typ) -> Mark.remove marked_output_typ
        | _ -> TAny
      in
      match Mark.remove f with
      | ELocation (ScopelangScopeVar { name = var }) ->
        retrieve_out_typ_or_any var ctx.scope_vars
      | ELocation (ToplevelVar { name }) -> (
        let typ =
          TopdefName.Map.find (Mark.remove name) ctx.decl_ctx.ctx_topdefs
        in
        match Mark.remove typ with
        | TArrow (_, (tout, _)) -> tout
        | _ ->
          Message.error ~pos:(Expr.pos e)
            "Application of non-function toplevel variable")
      | _ -> TAny
    in
    (* Message.debug "new_args %d, input_typs: %d, input_typs %a" (List.length
       new_args) (List.length input_typs) (Format.pp_print_list Print.typ_debug)
       (List.map (Mark.add Pos.no_pos) input_typs); *)
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
    let new_e = Expr.eapp ~f:e1_func ~args:new_args ~tys m in
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
      ~excepts:(List.map (translate_expr ctx) excepts)
      ~just:(translate_expr ctx just) ~cons:(translate_expr ctx cons) m
  | EPureDefault e -> Expr.epuredefault (translate_expr ctx e) m
  | ELocation (ScopelangScopeVar { name = a }) ->
    let v, _, _ = ScopeVar.Map.find (Mark.remove a) ctx.scope_vars in
    Expr.evar v m
  | ELocation (ToplevelVar { name }) ->
    let path = TopdefName.path (Mark.remove name) in
    if path = [] then
      let v, _ = TopdefName.Map.find (Mark.remove name) ctx.toplevel_vars in
      Expr.evar v m
    else Expr.eexternal ~name:(Mark.map (fun n -> External_value n) name) m
  | EAppOp { op = Add_dat_dur _, opos; args; tys } ->
    let args = List.map (translate_expr ctx) args in
    Expr.eappop ~op:(Add_dat_dur ctx.date_rounding, opos) ~args ~tys m
  | ( EVar _ | EAbs _ | ELit _ | EStruct _ | EStructAccess _ | ETuple _
    | ETupleAccess _ | EInj _ | EFatalError _ | EEmpty | EErrorOnEmpty _
    | EArray _ | EIfThenElse _ | EAppOp _ ) as e ->
    Expr.map ~f:(translate_expr ctx) ~op:Operator.translate (e, m)

(** The result of a rule translation is a list of assignments, with variables
    and expressions. We also return the new translation context available after
    the assignment to use in later rule translations. The list is actually a
    continuation yielding a [Dcalc.scope_body_expr] by giving it what should
    come later in the chain of let-bindings. *)
let translate_rule
    (ctx : 'm ctx)
    (rule : 'm S.rule)
    ((sigma_name, pos_sigma) : Uid.MarkedString.info) :
    ('m Ast.expr scope_body_expr Bindlib.box ->
    'm Ast.expr scope_body_expr Bindlib.box)
    * 'm ctx =
  match rule with
  | S.ScopeVarDefinition { var; typ; e; _ }
  | S.SubScopeVarDefinition { var; typ; e; _ } ->
    let scope_var = Mark.remove var in
    let decl_pos = Mark.get (ScopeVar.get_info scope_var) in
    let pos_mark, _ = pos_mark_mk e in
    let scope_let_kind, io =
      match rule with
      | S.ScopeVarDefinition { io; _ } -> ScopeVarDefinition, io
      | S.SubScopeVarDefinition _ ->
        ( SubScopeVarDefinition,
          { io_input = NoInput, decl_pos; io_output = false, decl_pos } )
      | S.Assertion _ -> assert false
    in
    let a_name = ScopeVar.get_info (Mark.remove var) in
    let a_var = Var.make (Mark.remove a_name) in
    let new_e = translate_expr ctx e in
    let a_expr = Expr.make_var a_var (pos_mark decl_pos) in
    let is_func = match Mark.remove typ with TArrow _ -> true | _ -> false in
    let merged_expr =
      match Mark.remove io.io_input with
      | OnlyInput -> assert false
      (* scopelang should not contain any definitions of input only variables *)
      | Reentrant -> merge_defaults ~is_func a_expr new_e
      | NoInput -> new_e
    in
    let merged_expr =
      tag_with_log_entry merged_expr
        (VarDef
           {
             log_typ = Mark.remove typ;
             log_io_output = Mark.remove io.io_output;
             log_io_input = Mark.remove io.io_input;
           })
        [sigma_name, pos_sigma; a_name]
    in
    ( (fun next ->
        Bindlib.box_apply2
          (fun next merged_expr ->
            Cons
              ( {
                  scope_let_typ = typ;
                  scope_let_expr = merged_expr;
                  scope_let_kind;
                  scope_let_pos = decl_pos;
                },
                next ))
          (Bindlib.bind_var a_var next)
          (Expr.Box.lift merged_expr)),
      {
        ctx with
        scope_vars =
          ScopeVar.Map.add (Mark.remove var)
            (a_var, Mark.remove typ, io)
            ctx.scope_vars;
      } )
  | Assertion e ->
    let new_e = translate_expr ctx e in
    let scope_let_pos = Expr.pos e in
    let scope_let_typ = TLit TUnit, scope_let_pos in
    ( (fun next ->
        Bindlib.box_apply2
          (fun next new_e ->
            Cons
              ( {
                  scope_let_pos;
                  scope_let_typ;
                  scope_let_expr =
                    Mark.add
                      (Expr.map_ty (fun _ -> scope_let_typ) (Mark.get e))
                      (EAssert new_e);
                  scope_let_kind = Assertion;
                },
                next ))
          (Bindlib.bind_var (Var.make "_") next)
          (Expr.Box.lift new_e)),
      ctx )

let translate_rules
    (ctx : 'm ctx)
    (scope_name : ScopeName.t)
    (rules : 'm S.rule list)
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
         (fun return_exp -> Last return_exp)
         (Expr.Box.lift return_exp)),
    new_ctx )

(* From a scope declaration and definitions, create the corresponding scope body
   wrapped in the appropriate call convention. *)
let translate_scope_decl
    (ctx : 'm ctx)
    (scope_name : ScopeName.t)
    (sigma : 'm S.scope_decl) =
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
      Message.error ~pos:pos_sigma "Scope %a has no content" ScopeName.format
        scope_name
    | ( S.ScopeVarDefinition { e; _ }
      | S.SubScopeVarDefinition { e; _ }
      | S.Assertion e )
      :: _ ->
      Mark.get e
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
            Cons
              ( {
                  scope_let_kind = DestructuringInputStruct;
                  scope_let_pos = pos_sigma;
                  scope_let_typ =
                    input_var_typ var_ctx.scope_var_typ var_ctx.scope_var_io;
                  scope_let_expr =
                    ( EStructAccess
                        { name = scope_input_struct_name; e = r; field },
                      mark_tany scope_mark pos_sigma );
                },
                next ))
          (Bindlib.bind_var v next)
          (Expr.Box.lift
             (Expr.make_var scope_input_var (mark_tany scope_mark pos_sigma))))
      scope_input_variables next
  in
  Bindlib.box_apply
    (fun scope_body_expr ->
      {
        scope_body_expr;
        scope_body_input_struct = scope_input_struct_name;
        scope_body_output_struct = scope_return_struct_name;
      })
    (Bindlib.bind_var scope_input_var
       (input_destructurings rules_with_return_expr))

let translate_program (prgm : 'm S.program) : 'm Ast.program =
  let defs_dependencies = Scopelang.Dependency.build_program_dep_graph prgm in
  Scopelang.Dependency.check_for_cycle_in_defs defs_dependencies;
  let defs_ordering =
    Scopelang.Dependency.get_defs_ordering defs_dependencies
  in
  let decl_ctx = prgm.program_ctx in
  let scopes_parameters : 'm scope_sig_ctx ScopeName.Map.t =
    let process_scope_sig decl_ctx scope_name scope =
      let scope_path = ScopeName.path scope_name in
      let scope_ref =
        if scope_path = [] then
          let v = Var.make (Mark.remove (ScopeName.get_info scope_name)) in
          Local_scope_ref v
        else
          External_scope_ref
            (Mark.copy (ScopeName.get_info scope_name) scope_name)
      in
      let scope_info = ScopeName.Map.find scope_name decl_ctx.ctx_scopes in
      let scope_sig_in_fields =
        (* Output fields have already been generated and added to the program
           ctx at this point, because they are visible to the user (manipulated
           as the return type of ScopeCalls) ; but input fields are used purely
           internally and need to be created here to implement the call
           convention for scopes. *)
        let module S = S in
        ScopeVar.Map.filter_map
          (fun dvar svar ->
            match Mark.remove svar.S.svar_io.Desugared.Ast.io_input with
            | NoInput -> None
            | OnlyInput | Reentrant ->
              let info = ScopeVar.get_info dvar in
              let s = Mark.remove info ^ "_in" in
              Some
                {
                  scope_input_name = StructField.fresh (s, Mark.get info);
                  scope_input_io = svar.S.svar_io.Desugared.Ast.io_input;
                  scope_input_typ =
                    Mark.remove
                      (input_var_typ
                         (Mark.remove svar.S.svar_in_ty)
                         svar.S.svar_io);
                  scope_input_thunked =
                    input_var_needs_thunking
                      (Mark.remove svar.S.svar_in_ty)
                      svar.S.svar_io;
                })
          scope.S.scope_sig
      in
      {
        scope_sig_local_vars =
          List.map
            (fun (scope_var, svar) ->
              {
                scope_var_name = scope_var;
                scope_var_typ = Mark.remove svar.S.svar_in_ty;
                scope_var_io = svar.S.svar_io;
              })
            (ScopeVar.Map.bindings scope.scope_sig);
        scope_sig_scope_ref = scope_ref;
        scope_sig_input_struct = scope_info.in_struct_name;
        scope_sig_output_struct = scope_info.out_struct_name;
        scope_sig_in_fields;
      }
    in
    let process_scopes scopes =
      ScopeName.Map.mapi
        (fun scope_name (scope_decl, _) ->
          process_scope_sig decl_ctx scope_name scope_decl)
        scopes
    in
    ModuleName.Map.fold
      (fun _ s -> ScopeName.Map.disjoint_union (process_scopes s))
      prgm.S.program_modules
      (process_scopes prgm.S.program_scopes)
  in
  let ctx_structs =
    ScopeName.Map.fold
      (fun _ scope_sig_ctx acc ->
        let fields =
          ScopeVar.Map.fold
            (fun _ sivc acc ->
              let pos = Mark.get (StructField.get_info sivc.scope_input_name) in
              StructField.Map.add sivc.scope_input_name
                (sivc.scope_input_typ, pos)
                acc)
            scope_sig_ctx.scope_sig_in_fields StructField.Map.empty
        in
        StructName.Map.add scope_sig_ctx.scope_sig_input_struct fields acc)
      scopes_parameters decl_ctx.ctx_structs
  in
  let decl_ctx = { decl_ctx with ctx_structs } in
  let toplevel_vars =
    TopdefName.Map.mapi
      (fun name (_, ty) ->
        Var.make (Mark.remove (TopdefName.get_info name)), Mark.remove ty)
      prgm.S.program_topdefs
  in
  let ctx =
    {
      decl_ctx;
      scope_name = None;
      scopes_parameters;
      scope_vars = ScopeVar.Map.empty;
      (* subscope_vars = ScopeVar.Map.empty; *)
      toplevel_vars;
      date_rounding = AbortOnRound;
    }
  in
  (* the resulting expression is the list of definitions of all the scopes,
     ending with the top-level scope. The decl_ctx is filled in left-to-right
     order, then the chained scopes aggregated from the right. *)
  let rec translate_defs = function
    | [] -> Bindlib.box (Last ())
    | def :: next ->
      let dvar, def =
        match def with
        | Scopelang.Dependency.Topdef gname ->
          let expr, ty = TopdefName.Map.find gname prgm.program_topdefs in
          let expr = translate_expr ctx expr in
          ( fst (TopdefName.Map.find gname ctx.toplevel_vars),
            Bindlib.box_apply
              (fun e -> Topdef (gname, ty, e))
              (Expr.Box.lift expr) )
        | Scopelang.Dependency.Scope scope_name ->
          let scope = ScopeName.Map.find scope_name prgm.program_scopes in
          let scope_body =
            translate_scope_decl ctx scope_name (Mark.remove scope)
          in
          let scope_var =
            match
              (ScopeName.Map.find scope_name scopes_parameters)
                .scope_sig_scope_ref
            with
            | Local_scope_ref v -> v
            | External_scope_ref _ -> assert false
          in
          ( scope_var,
            Bindlib.box_apply
              (fun body -> ScopeDef (scope_name, body))
              scope_body )
      in
      let scope_next = translate_defs next in
      let next_bind = Bindlib.bind_var dvar scope_next in
      Bindlib.box_apply2
        (fun item next_bind -> Cons (item, next_bind))
        def next_bind
  in
  let items = translate_defs defs_ordering in
  Expr.Box.assert_closed items;
  {
    code_items = Bindlib.unbox items;
    decl_ctx;
    module_name = prgm.S.program_module_name;
    lang = prgm.program_lang;
  }
