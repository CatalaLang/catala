(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020-2022 Inria, contributor: Alain Delaët-Tixeuil
   <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
module D = Dcalc.Ast
module A = Ast
open Dcalc.Binded_representation

(*** hoisting *)

(** The main idea around this pass is to compile Dcalc to Lcalc without using [raise EmptyError] nor
    [try _ with EmptyError -> _]. To do so, we use the same technique as in rust or erlang to handle
    this kind of exceptions. Each [raise EmptyError] will be translated as [None] and each
    [try e1 with EmtpyError -> e2] as [match e1 with | None -> e2 | Some x -> x].

    When doing this naively, this requires to add matches and Some constructor everywhere. We apply
    here an other technique where we generate what we call `hoists`. Hoists are expression whom
    could minimally [raise EmptyError]. For instance in
    [let x = <e1, e2, ..., en| e_just :- e_cons> * 3 in x + 1], the sub-expression
    [<e1, e2, ..., en| e_just :- e_cons>] can produce an empty error. So we make a hoist with a new
    variable [y] linked to the Dcalc expression [<e1, e2, ..., en| e_just :- e_cons>], and we return
    as the translated expression [let x = y * 3 in x + 1].

    The compilation of expressions is found in the functions [translate_and_hoist ctx e] and
    [translate_expr ctx e]. Every option-generating expression when calling [translate_and_hoist]
    will be hoisted and later handled by the [translate_expr] function. Every other cases is found
    in the translate_and_hoist function. *)

type hoists = D.expr Pos.marked A.VarMap.t
(** Hoists definition. It represent bindings between [A.Var.t] and [D.expr]. *)

type info = { expr : A.expr Pos.marked Bindlib.box; var : A.expr Bindlib.var; is_pure : bool }
(** Information about each encontered Dcalc variable is stored inside a context : what is the
    corresponding LCalc variable; an expression corresponding to the variable build correctly using
    Bindlib, and a boolean `is_pure` indicating whenever the variable can be an EmptyError and hence
    should be matched (false) or if it never can be EmptyError (true). *)

let pp_info (fmt : Format.formatter) (info : info) =
  Format.fprintf fmt "{var: %a; is_pure: %b}" Print.format_var info.var info.is_pure

type ctx = info D.VarMap.t
(** information context about variables in the current scope *)

let pp_ctx (fmt : Format.formatter) (ctx : ctx) =
  let pp_binding (fmt : Format.formatter) ((v, info) : D.Var.t * info) =
    Format.fprintf fmt "%a: %a" Dcalc.Print.format_var v pp_info info
  in

  let pp_bindings =
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ") pp_binding
  in

  Format.fprintf fmt "@[<2>[%a]@]" pp_bindings (D.VarMap.bindings ctx)

(** [find ~info n ctx] is a warpper to ocaml's Map.find that handle errors in a slightly better way. *)
let find ?(info : string = "none") (n : D.Var.t) (ctx : ctx) : info =
  let _ =
    Format.asprintf "Searching for variable %a inside context %a" Dcalc.Print.format_var n pp_ctx
      ctx
    |> Cli.debug_print
  in
  try D.VarMap.find n ctx
  with Not_found ->
    Errors.raise_spanned_error
      (Format.asprintf
         "Internal Error: Variable %a was not found in the current environment. Additional \
          informations : %s."
         Dcalc.Print.format_var n info)
      Pos.no_pos

(** [add_var pos var is_pure ctx] add to the context [ctx] the Dcalc variable var, creating a unique
    corresponding variable in Lcalc, with the corresponding expression, and the boolean is_pure. It
    is usefull for debuging purposes as it printing each of the Dcalc/Lcalc variable pairs. *)
let add_var (pos : Pos.t) (var : D.Var.t) (is_pure : bool) (ctx : ctx) : ctx =
  let new_var = A.Var.make (Bindlib.name_of var, pos) in
  let expr = A.make_var (new_var, pos) in

  Cli.debug_print
  @@ Format.asprintf "D.%a |-> A.%a" Dcalc.Print.format_var var Print.format_var new_var;

  D.VarMap.update var (fun _ -> Some { expr; var = new_var; is_pure }) ctx

(** [tau' = translate_typ tau] translate the a dcalc type into a lcalc type.

    Since positions where there is thunked expressions is exactly where we will put option
    expressions. Hence, the transformation simply reduce [unit -> 'a] into ['a option] recursivly.
    There is no polymorphism inside catala. *)
let rec translate_typ (tau : D.typ Pos.marked) : D.typ Pos.marked =
  (Fun.flip Pos.same_pos_as) tau
    begin
      match Pos.unmark tau with
      | D.TLit l -> D.TLit l
      | D.TTuple (ts, s) -> D.TTuple (List.map translate_typ ts, s)
      | D.TEnum (ts, en) -> D.TEnum (List.map translate_typ ts, en)
      | D.TAny -> D.TAny
      | D.TArray ts -> D.TArray (translate_typ ts)
      (* catala is not polymorphic*)
      | D.TArrow ((D.TLit D.TUnit, _), t2) ->
          D.TEnum ([ translate_typ t2 ], A.option_enum) (* D.TAny *)
      | D.TArrow (t1, t2) -> D.TArrow (translate_typ t1, translate_typ t2)
    end

let translate_lit (l : D.lit) (pos : Pos.t) : A.lit =
  match l with
  | D.LBool l -> A.LBool l
  | D.LInt i -> A.LInt i
  | D.LRat r -> A.LRat r
  | D.LMoney m -> A.LMoney m
  | D.LUnit -> A.LUnit
  | D.LDate d -> A.LDate d
  | D.LDuration d -> A.LDuration d
  | D.LEmptyError ->
      Errors.raise_spanned_error
        "Internal Error: An empty error was found in a place that shouldn't be possible." pos

(** [c = disjoint_union_maps cs] Compute the disjoint union of multiple maps. Raises an internal
    error if there is two identicals keys in differnts parts. *)
let disjoint_union_maps (pos : Pos.t) (cs : 'a A.VarMap.t list) : 'a A.VarMap.t =
  let disjoint_union =
    A.VarMap.union (fun _ _ _ ->
        Errors.raise_spanned_error
          "Internal Error: Two supposed to be disjoints maps have one shared key." pos)
  in

  List.fold_left disjoint_union A.VarMap.empty cs

(** [e' = translate_and_hoist ctx e ] Translate the Dcalc expression e into an expression in Lcalc,
    given we translate each hoists correctly. It ensures the equivalence between the execution of e
    and the execution of e' are equivalent in an environement where each variable v, where (v, e_v)
    is in hoists, has the non-empty value in e_v. *)
let rec translate_and_hoist (ctx : ctx) (e : D.expr Pos.marked) :
    A.expr Pos.marked Bindlib.box * hoists =
  let pos = Pos.get_position e in
  match Pos.unmark e with
  (* empty-producing/using terms. We hoist those. (D.EVar in some cases, EApp(D.EVar _, [ELit
     LUnit]), EDefault _, ELit LEmptyDefault) I'm unsure about assert. *)
  | D.EVar v ->
      (* todo: for now, every unpure (such that [is_pure] is [false] in the current context) is
         thunked, hence matched in the next case. This assumption can change in the future, and this
         case is here for this reason. *)
      let v, pos_v = v in
      if not (find ~info:"search for a variable" v ctx).is_pure then begin
        let v' = A.Var.make (Bindlib.name_of v, pos_v) in
        Cli.debug_print
        @@ Format.asprintf "Found an unpure variable %a, created a variable %a to replace it"
             Dcalc.Print.format_var v Print.format_var v';
        (A.make_var (v', pos), A.VarMap.singleton v' e)
      end
      else ((find ~info:"should never happend" v ctx).expr, A.VarMap.empty)
  | D.EApp ((D.EVar (v, pos_v), p), [ (D.ELit D.LUnit, _) ]) ->
      if not (find ~info:"search for a variable" v ctx).is_pure then begin
        let v' = A.Var.make (Bindlib.name_of v, pos_v) in
        Cli.debug_print
        @@ Format.asprintf "Found an unpure variable %a, created a variable %a to replace it"
             Dcalc.Print.format_var v Print.format_var v';
        (A.make_var (v', pos), A.VarMap.singleton v' (D.EVar (v, pos_v), p))
      end
      else
        Errors.raise_spanned_error
          "Internal error: an pure variable was found in an unpure environment." pos
  | D.EDefault (_exceptions, _just, _cons) ->
      let v' = A.Var.make ("default_term", pos) in
      (A.make_var (v', pos), A.VarMap.singleton v' e)
  | D.ELit D.LEmptyError ->
      let v' = A.Var.make ("empty_litteral", pos) in
      (A.make_var (v', pos), A.VarMap.singleton v' e)
  (* This one is a very special case. It transform an unpure expression environement to a pure
     expression. *)
  | ErrorOnEmpty arg ->
      (* [ match arg with | None -> raise NoValueProvided | Some v -> {{ v }} ] *)
      let silent_var = A.Var.make ("_", pos) in
      let x = A.Var.make ("non_empty_argument", pos) in

      let arg' = translate_expr ctx arg in

      ( A.make_matchopt_with_abs_arms arg'
          (A.make_abs [| silent_var |]
             (Bindlib.box (A.ERaise A.NoValueProvided, pos))
             pos [ (D.TAny, pos) ] pos)
          (A.make_abs [| x |] (A.make_var (x, pos)) pos [ (D.TAny, pos) ] pos),
        A.VarMap.empty )
  (* pure terms *)
  | D.ELit l -> (Bindlib.box (A.ELit (translate_lit l pos), pos), A.VarMap.empty)
  | D.EIfThenElse (e1, e2, e3) ->
      let e1', h1 = translate_and_hoist ctx e1 in
      let e2', h2 = translate_and_hoist ctx e2 in
      let e3', h3 = translate_and_hoist ctx e3 in

      let e' =
        Bindlib.box_apply3 (fun e1' e2' e3' -> (A.EIfThenElse (e1', e2', e3'), pos)) e1' e2' e3'
      in

      (*(* equivalent code : *) let e' = let+ e1' = e1' and+ e2' = e2' and+ e3' = e3' in
        (A.EIfThenElse (e1', e2', e3'), pos) in *)
      (e', disjoint_union_maps pos [ h1; h2; h3 ])
  | D.EAssert e1 ->
      (* same behavior as in the ICFP paper: if e1 is empty, then no error is raised. *)
      let e1', h1 = translate_and_hoist ctx e1 in
      (Bindlib.box_apply (fun e1' -> (A.EAssert e1', pos)) e1', h1)
  | D.EAbs ((binder, pos_binder), ts) ->
      let vars, body = Bindlib.unmbind binder in
      let ctx, lc_vars =
        ArrayLabels.fold_right vars ~init:(ctx, []) ~f:(fun var (ctx, lc_vars) ->
            (* we suppose the invariant that when applying a function, its arguments cannot be of
               the type "option".

               The code should behave correctly in the without this assumption if we put here an
               is_pure=false, but the types are more compilcated. (unimplemented for now) *)
            let ctx = add_var pos var true ctx in
            let lc_var = (find var ctx).var in
            (ctx, lc_var :: lc_vars))
      in
      let lc_vars = Array.of_list lc_vars in

      (* here we take the guess that if we cannot build the closure because one of the variable is
         empty, then we cannot build the function. *)
      let new_body, hoists = translate_and_hoist ctx body in
      let new_binder = Bindlib.bind_mvar lc_vars new_body in

      ( Bindlib.box_apply
          (fun new_binder -> (A.EAbs ((new_binder, pos_binder), List.map translate_typ ts), pos))
          new_binder,
        hoists )
  | EApp (e1, args) ->
      let e1', h1 = translate_and_hoist ctx e1 in
      let args', h_args = args |> List.map (translate_and_hoist ctx) |> List.split in

      let hoists = disjoint_union_maps pos (h1 :: h_args) in
      let e' =
        Bindlib.box_apply2
          (fun e1' args' -> (A.EApp (e1', args'), pos))
          e1' (Bindlib.box_list args')
      in
      (e', hoists)
  | ETuple (args, s) ->
      let args', h_args = args |> List.map (translate_and_hoist ctx) |> List.split in

      let hoists = disjoint_union_maps pos h_args in
      (Bindlib.box_apply (fun args' -> (A.ETuple (args', s), pos)) (Bindlib.box_list args'), hoists)
  | ETupleAccess (e1, i, s, ts) ->
      let e1', hoists = translate_and_hoist ctx e1 in
      let e1' = Bindlib.box_apply (fun e1' -> (A.ETupleAccess (e1', i, s, ts), pos)) e1' in
      (e1', hoists)
  | EInj (e1, i, en, ts) ->
      let e1', hoists = translate_and_hoist ctx e1 in
      let e1' = Bindlib.box_apply (fun e1' -> (A.EInj (e1', i, en, ts), pos)) e1' in
      (e1', hoists)
  | EMatch (e1, cases, en) ->
      let e1', h1 = translate_and_hoist ctx e1 in
      let cases', h_cases = cases |> List.map (translate_and_hoist ctx) |> List.split in

      let hoists = disjoint_union_maps pos (h1 :: h_cases) in
      let e' =
        Bindlib.box_apply2
          (fun e1' cases' -> (A.EMatch (e1', cases', en), pos))
          e1' (Bindlib.box_list cases')
      in
      (e', hoists)
  | EArray es ->
      let es', hoists = es |> List.map (translate_and_hoist ctx) |> List.split in

      ( Bindlib.box_apply (fun es' -> (A.EArray es', pos)) (Bindlib.box_list es'),
        disjoint_union_maps pos hoists )
  | EOp op -> (Bindlib.box (A.EOp op, pos), A.VarMap.empty)

and translate_expr ?(append_esome = true) (ctx : ctx) (e : D.expr Pos.marked) :
    A.expr Pos.marked Bindlib.box =
  let e', hoists = translate_and_hoist ctx e in
  let hoists = A.VarMap.bindings hoists in

  let _pos = Pos.get_position e in

  (* build the hoists *)
  Cli.debug_print
  @@ Format.asprintf "hoist for the expression: [%a]"
       (Format.pp_print_list Print.format_var)
       (List.map fst hoists);

  ListLabels.fold_left hoists
    ~init:(if append_esome then A.make_some e' else e')
    ~f:(fun acc (v, (hoist, pos_hoist)) ->
      Cli.debug_print @@ Format.asprintf "hoist using A.%a" Print.format_var v;

      let c' : A.expr Pos.marked Bindlib.box =
        match hoist with
        (* Here we have to handle only the cases appearing in hoists, as defined the
           [translate_and_hoist] function. *)
        | D.EVar v -> (find ~info:"should never happend" (Pos.unmark v) ctx).expr
        | D.EDefault (excep, just, cons) ->
            let excep' = List.map (translate_expr ctx) excep in
            let just' = translate_expr ctx just in
            let cons' = translate_expr ctx cons in
            (* calls handle_option. *)
            A.make_app
              (A.make_var (A.handle_default_opt, pos_hoist))
              [
                Bindlib.box_apply
                  (fun excep' -> (A.EArray excep', pos_hoist))
                  (Bindlib.box_list excep');
                just';
                cons';
              ]
              pos_hoist
        | D.ELit D.LEmptyError -> A.make_none pos_hoist
        | D.EAssert arg ->
            let arg' = translate_expr ctx arg in

            (* [ match arg with | None -> raise NoValueProvided | Some v -> assert {{ v }} ] *)
            let silent_var = A.Var.make ("_", pos_hoist) in
            let x = A.Var.make ("assertion_argument", pos_hoist) in

            A.make_matchopt_with_abs_arms arg'
              (A.make_abs [| silent_var |]
                 (Bindlib.box (A.ERaise A.NoValueProvided, pos_hoist))
                 pos_hoist [ (D.TAny, pos_hoist) ] pos_hoist)
              (A.make_abs [| x |]
                 (Bindlib.box_apply
                    (fun arg -> (A.EAssert arg, pos_hoist))
                    (A.make_var (x, pos_hoist)))
                 pos_hoist [ (D.TAny, pos_hoist) ] pos_hoist)
        | _ ->
            Errors.raise_spanned_error
              "Internal Error: An term was found in a position where it should not be" pos_hoist
      in

      (* [ match {{ c' }} with | None -> None | Some {{ v }} -> {{ acc }} end ] *)
      Cli.debug_print @@ Format.asprintf "build matchopt using %a" Print.format_var v;
      A.make_matchopt pos_hoist v (D.TAny, pos_hoist) c' (A.make_none pos_hoist) acc)

let rec translate_scope_let (ctx : ctx) (lets : scope_lets) =
  match lets with
  | Result e -> translate_expr ~append_esome:false ctx e
  | ScopeLet
      {
        scope_let_kind = SubScopeVarDefinition;
        scope_let_typ = typ;
        scope_let_expr = D.EAbs ((binder, _), _), _pos;
        scope_let_next = next;
        scope_let_pos = pos;
      } ->
      (* special case : the subscope variable is always thunked. We remove this thunking. *)
      let _, expr = Bindlib.unmbind binder in

      let var_is_pure = true in
      let var, next = Bindlib.unbind next in
      Cli.debug_print @@ Format.asprintf "unbinding %a" Dcalc.Print.format_var var;
      let ctx' = add_var pos var var_is_pure ctx in
      let new_var = (find ~info:"variable that was just created" var ctx').var in
      A.make_let_in new_var (translate_typ typ)
        (translate_expr ctx ~append_esome:true expr)
        (translate_scope_let ctx' next)
  | ScopeLet { scope_let_kind = SubScopeVarDefinition; scope_let_pos = pos; _ } ->
      Errors.raise_spanned_error
        "Internal Error: found an SubScopeVarDefinition that does not satisfy the thunked \
         invariant when translating Dcalc to Lcalc without exceptions."
        pos
  | ScopeLet
      {
        scope_let_kind = kind;
        scope_let_typ = typ;
        scope_let_expr = expr;
        scope_let_next = next;
        scope_let_pos = pos;
      } ->
      let var_is_pure =
        match kind with
        | DestructuringInputStruct -> false
        | ScopeVarDefinition | SubScopeVarDefinition | CallingSubScope
        | DestructuringSubScopeResults | Assertion ->
            true
      in
      let var, next = Bindlib.unbind next in
      Cli.debug_print @@ Format.asprintf "unbinding %a" Dcalc.Print.format_var var;
      let ctx' = add_var pos var var_is_pure ctx in
      let new_var = (find ~info:"variable that was just created" var ctx').var in
      A.make_let_in new_var (translate_typ typ)
        (translate_expr ctx ~append_esome:false expr)
        (translate_scope_let ctx' next)

let translate_scope_body (scope_pos : Pos.t) (_decl_ctx : D.decl_ctx) (ctx : ctx)
    (body : scope_body) : A.expr Pos.marked Bindlib.box =
  match body with
  | {
   scope_body_result = result;
   scope_body_input_struct = input_struct;
   scope_body_output_struct = _output_struct;
  } ->
      let v, lets = Bindlib.unbind result in
      let ctx' = add_var scope_pos v true ctx in
      let v' = (find ~info:"variable that was just created" v ctx').var in

      A.make_abs [| v' |] (translate_scope_let ctx' lets) Pos.no_pos
        [ (D.TTuple ([], Some input_struct), Pos.no_pos) ]
        Pos.no_pos

let rec translate_scopes (decl_ctx : D.decl_ctx) (ctx : ctx) (scopes : scopes) :
    (A.expr Bindlib.var * (A.expr * Pos.t)) list Bindlib.box =
  match scopes with
  | Nil -> Bindlib.box []
  | ScopeDef { scope_name; scope_body; scope_next } ->
      let scope_var, next = Bindlib.unbind scope_next in
      let new_ctx = add_var Pos.no_pos scope_var true ctx in
      let new_scope_name = (find ~info:"variable that was just created" scope_var new_ctx).var in

      let scope_pos = Pos.get_position (D.ScopeName.get_info scope_name) in

      let new_body = translate_scope_body scope_pos decl_ctx ctx scope_body in
      let tail = translate_scopes decl_ctx new_ctx next in

      Bindlib.box_apply2 (fun body tail -> (new_scope_name, body) :: tail) new_body tail

let translate_scopes (decl_ctx : D.decl_ctx) (ctx : ctx) (scopes : scopes) :
    (A.expr Bindlib.var * (A.expr * Pos.t)) list =
  Bindlib.unbox (translate_scopes decl_ctx ctx scopes)

let translate_program (prgm : D.program) : A.program =
  (* modify the *)
  let inputs_structs =
    ListLabels.fold_left prgm.scopes ~init:[] ~f:(fun acc (_, _, body) ->
        body.D.scope_body_input_struct :: acc)
  in

  Cli.debug_print
  @@ Format.asprintf "List of structs to modify: [%a]"
       (Format.pp_print_list D.StructName.format_t)
       inputs_structs;

  let decl_ctx =
    {
      D.ctx_enums = prgm.decl_ctx.ctx_enums |> D.EnumMap.add A.option_enum A.option_enum_config;
      D.ctx_structs =
        prgm.decl_ctx.ctx_structs
        |> D.StructMap.mapi (fun n l ->
               if List.mem n inputs_structs then
                 ListLabels.map l ~f:(fun (n, tau) ->
                     Cli.debug_print
                     @@ Format.asprintf "Input type: %a" (Dcalc.Print.format_typ prgm.decl_ctx) tau;
                     Cli.debug_print
                     @@ Format.asprintf "Output type: %a"
                          (Dcalc.Print.format_typ prgm.decl_ctx)
                          (translate_typ tau);
                     (n, translate_typ tau))
               else l);
    }
  in

  let scopes =
    prgm.scopes |> bind_scopes |> Bindlib.unbox |> translate_scopes decl_ctx D.VarMap.empty
  in

  { scopes; decl_ctx }
