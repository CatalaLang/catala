
open Utils
module D = Dcalc.Ast
module A = Ast

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
  | D.EAssert e ->
    (* as discuted, if the value in an assertion is empty, an error should the raised. This beavior is different from the ICFP paper. *)
      let v' = A.Var.make ("assertion_value", pos) in
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

  


  | _ -> assert false

let rec translate_expr (ctx: ctx) (e: D.expr Pos.marked)
    : (A.expr Pos.marked Bindlib.box) =
    let e', cs = translate_and_cut ctx e in
    let cs = A.VarMap.bindings cs in
    
    let pos = Pos.get_position e in
    (* build the cuts *)
    ListLabels.fold_left cs ~init:e'
      ~f:(fun acc (v, (c, pos_c)) ->

        let c': A.expr Pos.marked Bindlib.box = match c with
        (* Here we have to handle only the cases appearing in cuts, as defined the [translate_and_cut] function. *)
        | D.EVar v -> Bindlib.box_var v
        | D.EDefault (excep, just, cons) ->
          let excep' = List.map (translate_expr ctx) excep in
          let just' = translate_expr ctx just in
          let cons' = translate_expr ctx cons in
          (* TODO: [
            match process_expr {{ except' }} with
            | Some -> fun v -> v
            | None -> (fun () -> match {{ just' }} with
              | Some -> (fun v -> if v then {{ cons' }} else None)
              | None -> (fun  () -> None)
            )
          ] *)
          assert false
        | D.ELit D.LEmptyError ->
          A.make_none pos_c
        | _ -> Errors.raise_spanned_error "Internal Error: An term was found in a position where it should not be" pos_c
        in

        (* [
            match {{ c' }} with
            | None -> None
            | Some {{ v }} -> {{ acc }}
            end
          ] *)
        A.make_matchopt'' pos v (D.TAny, pos) c' (A.make_none pos) acc
      )

