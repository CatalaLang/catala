
open Utils
module D = Dcalc.Ast
module A = Ast


(**
  The main idea around this pass is to compile Dcalc to Lcalc without using [raise EmptyError] nor [try _ with EmptyError -> _]. To do so, we use the same technique as in rust or erlang to handle this kind of exceptions. Each [raise EmptyError] will be translated as [None] and each [try e1 with EmtpyError -> e2] as [match e1 with | None -> e2 | Some x -> x].

  When doing this naively, this requires to add matches and Some constructor everywhere. We apply here an other technique where we generate what we call `cuts`. Cuts are expression whom could minimally [raise EmptyError]. 
*)


(** information about the variables *)
type info = {
  expr: A.expr Pos.marked Bindlib.box;
  var: A.expr Bindlib.var;
  is_pure: bool
}

(** information context about variables in the current scope *)
type ctx = info D.VarMap.t  

type cuts = D.expr Pos.marked A.VarMap.t


let translate_lit (l : D.lit) (pos: Pos.t): A.lit =
  match l with
  | D.LBool l -> (A.LBool l)
  | D.LInt i -> (A.LInt i)
  | D.LRat r -> (A.LRat r)
  | D.LMoney m -> (A.LMoney m)
  | D.LUnit -> A.LUnit
  | D.LDate d -> (A.LDate d)
  | D.LDuration d -> (A.LDuration d)
  | D.LEmptyError -> Errors.raise_spanned_error "Internal Error: An empty error was found in a place that shouldn't be possible." pos


(** [c = merge_maps cs]
   Compute the disjoint union of multiple maps. Raises an internal error if there is two identicals keys in differnts parts. *)
let disjoint_union_maps (pos: Pos.t) (cs: 'a A.VarMap.t list): 'a A.VarMap.t =
  let disjoint_union = A.VarMap.union (fun _ _ _ -> Errors.raise_spanned_error "Internal Error: Two supposed to be disjoints maps have one shared key." pos) in

  List.fold_left disjoint_union A.VarMap.empty cs

(** [e' = translate_and_cut ctx e ]
  Translate the Dcalc expression e into an expression in Lcalc, given we translate each cuts correctly. It ensures the equivalence between the execution of e and the execution of e' are equivalent in an environement where each variable v, where (v, e_v) is in cuts, has the non-empty value in e_v. *)
let rec translate_and_cut (ctx: ctx) (e: D.expr Pos.marked)
  : A.expr Pos.marked Bindlib.box * cuts =
  let pos = Pos.get_position e in
  match Pos.unmark e with

  (* empty-producing/using terms *)
  | D.EVar v ->

    (* todo: for now, we requires there is unpure variables. This can change if the said variable are always present in the tree as thunked. *)
    let v, pos_v = v in
    if not (D.VarMap.find v ctx).is_pure then
      let v' = A.Var.make ((Bindlib.name_of v), pos_v) in
      (A.make_var (v', pos), A.VarMap.singleton v' e)
    else
      (D.VarMap.find v ctx).expr, A.VarMap.empty
  | D.EDefault (_exceptions, _just, _cons) ->
    let v' = A.Var.make ("default_term", pos) in
    (A.make_var (v', pos), A.VarMap.singleton v' e)
  | D.ELit D.LEmptyError ->
    let v' = A.Var.make ("empty_litteral", pos) in
    (A.make_var (v', pos), A.VarMap.singleton v' e)
  | D.EAssert _ ->
    (* as discuted, if the value in an assertion is empty, an error should the raised. This beavior is different from the ICFP paper. *)
    let v' = A.Var.make ("assertion_value", pos) in
    (A.make_var (v', pos), A.VarMap.singleton v' e)
  
  | ErrorOnEmpty _ ->
    let v' = A.Var.make ("error_on_empty_value", pos) in
    (A.make_var (v', pos), A.VarMap.singleton v' e)

  
  (* pure terms *)
  | D.ELit l ->
    (Bindlib.box (A.ELit (translate_lit l pos), pos), A.VarMap.empty)

  | D.EIfThenElse (e1, e2, e3) ->
    let e1', c1 = translate_and_cut ctx e1 in
    let e2', c2 = translate_and_cut ctx e2 in
    let e3', c3 = translate_and_cut ctx e3 in    

    let e' = Bindlib.box_apply3 (
      fun e1' e2' e3' -> (A.EIfThenElse(e1', e2', e3'), pos)) e1' e2' e3'
    in
    
    (*(* equivalent code : *)
    let e' =
      let+ e1' = e1' and+ e2' = e2' and+ e3' = e3' in
      (A.EIfThenElse (e1', e2', e3'), pos)
    in
    *)

    (e', disjoint_union_maps pos [c1; c2; c3])

  | EAbs ((binder, pos_binder), ts) ->
    let vars, body = Bindlib.unmbind binder in
    let ctx, lc_vars = ArrayLabels.fold_right vars ~init:(ctx, [])
      ~f:(fun var (ctx, lc_vars) ->
        let lc_var = A.Var.make (Bindlib.name_of var, pos_binder) in
        let lc_var_expr = A.make_var (lc_var, pos_binder) in
        (* we suppose the invariant that when applying a function, its arguments cannot be of the type "option".
        
          The code should behave correctly in the opposite direction if we put here an is_pure=false. *)
        (D.VarMap.add var {var=lc_var; expr=lc_var_expr; is_pure=true} ctx, lc_var :: lc_vars))
    in
    let lc_vars = Array.of_list lc_vars in

    (* here we take the guess that if we cannot build the closure because one of the variable is empty, then we cannot build the function. *)
    let new_body, cuts = translate_and_cut ctx body in
    let new_binder = Bindlib.bind_mvar lc_vars new_body in


    (Bindlib.box_apply
    (fun new_binder -> A.EAbs ((new_binder, pos_binder), ts), pos)
    new_binder, cuts)
  
  | EApp (e1, args) ->
      (* general case is simple *)
      let e1', c1 = translate_and_cut ctx e1 in
      let args', c_args = args
        |> List.map (translate_and_cut ctx)
        |> List.split
      in

      let cuts = disjoint_union_maps pos (c1::c_args) in
      let e' = Bindlib.box_apply2 (fun e1' args' -> A.EApp (e1', args'), pos)
        e1' (Bindlib.box_list args')
      in
      (e', cuts)

  | ETuple (args, s) ->
    let args', c_args = args
      |> List.map (translate_and_cut ctx)
      |> List.split
    in

    let cuts = disjoint_union_maps pos c_args in
    (Bindlib.box_apply (fun args' -> A.ETuple (args', s), pos) (Bindlib.box_list args'), cuts)
  | ETupleAccess (e1, i, s, ts) ->
    let e1', cuts = translate_and_cut ctx e1 in
    let e1' = Bindlib.box_apply
      (fun e1' -> A.ETupleAccess (e1', i, s, ts), pos)
      e1'
    in
    (e1', cuts)
  | EInj (e1, i, en, ts) ->
    let e1', cuts = translate_and_cut ctx e1 in
    let e1' = Bindlib.box_apply
      (fun e1' -> A.EInj (e1', i, en, ts), pos)
      e1'
    in
    (e1', cuts)
  | EMatch (e1, cases, en) ->
    let e1', c1 = translate_and_cut ctx e1 in
      let cases', c_cases = cases
        |> List.map (translate_and_cut ctx)
        |> List.split
      in

      let cuts = disjoint_union_maps pos (c1::c_cases) in
      let e' = Bindlib.box_apply2 (fun e1' cases' -> A.EMatch (e1', cases', en), pos)
        e1' (Bindlib.box_list cases')
      in
      (e', cuts)
  | EArray es ->
    let es', cuts = es 
      |> List.map (translate_and_cut ctx)
      |> List.split
    in

    (Bindlib.box_apply (fun es' -> (A.EArray es', pos)) (Bindlib.box_list es'), disjoint_union_maps pos cuts)

  | EOp op -> (Bindlib.box (A.EOp op, pos), A.VarMap.empty)
    

let rec translate_expr (ctx: ctx) (e: D.expr Pos.marked)
    : (A.expr Pos.marked Bindlib.box) =
    let e', cs = translate_and_cut ctx e in
    let cs = A.VarMap.bindings cs in
    
    let _pos = Pos.get_position e in
    (* build the cuts *)
    ListLabels.fold_left cs ~init:e'
      ~f:(fun acc (v, (c, pos_c)) ->

        let c': A.expr Pos.marked Bindlib.box = match c with
        (* Here we have to handle only the cases appearing in cuts, as defined the [translate_and_cut] function. *)
        | D.EVar v -> (D.VarMap.find (Pos.unmark v) ctx).expr
        | D.EDefault (excep, just, cons) ->
          let excep' = List.map (translate_expr ctx) excep in
          let just' = translate_expr ctx just in
          let cons' = translate_expr ctx cons in
          (* calls handle_option. *)
          A.make_app
            (A.make_var (A.handle_default_opt, pos_c))
            [(Bindlib.box_apply (fun excep' -> (A.EArray excep', pos_c)) (Bindlib.box_list excep'));
            just';
            cons'
            ]
            pos_c
          
        | D.ELit D.LEmptyError ->
          A.make_none pos_c
        
        | ErrorOnEmpty arg ->
          (* [
            match arg with
            | None -> raise NoValueProvided
            | Some v -> {{ v }}
          ] *)

          let unit = A.Var.make ("unit", pos_c) in
          let x = A.Var.make ("assertion_argument", pos_c) in

          let arg' = translate_expr ctx arg in

          A.make_matchopt arg'
            (A.make_abs [| unit |] (Bindlib.box (A.ERaise A.NoValueProvided, pos_c)) pos_c [D.TAny, pos_c] pos_c)
            (A.make_abs [| x |] ((A.make_var (x, pos_c))) pos_c [D.TAny, pos_c] pos_c)


        | D.EAssert arg ->
          let arg' = translate_expr ctx arg in

          (* [
            match arg with
            | None -> raise NoValueProvided
            | Some v -> assert {{ v }}
          ] *)

          let unit = A.Var.make ("unit", pos_c) in
          let x = A.Var.make ("assertion_argument", pos_c) in

          A.make_matchopt arg'
            (A.make_abs [| unit |] (Bindlib.box (A.ERaise A.NoValueProvided, pos_c)) pos_c [D.TAny, pos_c] pos_c)
            (A.make_abs [| x |] (Bindlib.box_apply (fun arg -> A.EAssert arg, pos_c) (A.make_var (x, pos_c))) pos_c [D.TAny, pos_c] pos_c)

        | _ -> Errors.raise_spanned_error "Internal Error: An term was found in a position where it should not be" pos_c
        in

        (* [
            match {{ c' }} with
            | None -> None
            | Some {{ v }} -> {{ acc }}
            end
          ] *)
        A.make_matchopt'' pos_c v (D.TAny, pos_c) c' (A.make_none pos_c) acc
      )

