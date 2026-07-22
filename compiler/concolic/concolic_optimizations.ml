open Catala_utils
open Shared_ast
open Path_constraint
open PathConstraint
open Conc_types

type flag =
  | OTrivial
  | OLazyDefault
  | OLinearizeMatch
  | OPacking
  | OIncrementalSolver
  | OSoftConstraints
  | OTestsVTime
  | OTimeout
  | OTimeoutRetry
  | OMutationRemove
  | OMutationDuplicate
  | OMutationNegateJusts
  | OMutationOneConflict
  | OASTStats
  | OAll

let optim_list =
  [
    "trivial", OTrivial;
    "lazy-default", OLazyDefault;
    "linearize-match", OLinearizeMatch;
    "packing", OPacking;
    "incremental", OIncrementalSolver;
    "soft", OSoftConstraints;
    "tests-vs-time", OTestsVTime;
    "timeout", OTimeout;
    "no-timeout-retry", OTimeoutRetry;
    "mutation-remove", OMutationRemove;
    "mutation-duplicate", OMutationDuplicate;
    "mutation-negate-justs", OMutationNegateJusts;
    "mutation-one-conflict", OMutationOneConflict;
    "ast-stats", OASTStats;
    "all", OAll;
  ]

let all_or (f : flag) (opt : flag list) : bool =
  List.mem OAll opt || List.mem f opt

let trivial : flag list -> bool = all_or OTrivial
let lazy_default : flag list -> bool = all_or OLazyDefault
let linearize_match : flag list -> bool = all_or OLinearizeMatch
let packing : flag list -> bool = all_or OPacking
let incremental_solver : flag list -> bool = all_or OIncrementalSolver
let soft_constraints : flag list -> bool = List.mem OSoftConstraints
let tests_vs_time : flag list -> bool = List.mem OTestsVTime
let timeout (l : flag list) : bool = List.mem OTimeout l
let timeout_retry (l : flag list) : bool = not (List.mem OTimeoutRetry l)

(* This option is active by default *)
let mutation_remove : flag list -> bool = List.mem OMutationRemove
let mutation_duplicate : flag list -> bool = List.mem OMutationDuplicate
let mutation_negate_justs : flag list -> bool = List.mem OMutationNegateJusts
let mutation_one_conflict : flag list -> bool = List.mem OMutationOneConflict
let ast_stats : flag list -> bool = List.mem OASTStats

let random_mutations flags =
  mutation_remove flags
  || mutation_duplicate flags
  || mutation_negate_justs flags

let one_mutation flags = mutation_one_conflict flags
let mutation flags = random_mutations flags || one_mutation flags

let check_optims_coherent optims =
  if soft_constraints optims && (not @@ incremental_solver optims) then
    Catala_utils.Message.warning
      "[CONC] Soft constraints are enabled without incremental solver. This \
       won't do anything."

let check_easy_unsat opt ctx (pcs : PathConstraint.pc_expr list) : bool =
  if not @@ trivial opt then false
  else begin
    if Global.options.debug then Message.debug "[check_easy_unsat]";
    match pcs with
    | Pc_z3 s0 :: l -> begin
      if Global.options.debug then
        Message.debug "[check_easy_unsat] first constraint is z3";
      let check = function
        | Pc_z3 s -> Z3.Expr.equal (Z3.Boolean.mk_not ctx s) s0
        | _ -> false
      in
      match List.find_opt check l with Some _ -> true | None -> false
    end
    | _ -> false
  end

let remove_trivial_constraints opt (pcs : naked_path) : naked_path =
  if not (trivial opt) then pcs
  else begin
    let f pc =
      match pc.expr with Pc_z3 s -> not (Z3.Boolean.is_true s) | _ -> true
    in
    List.filter f pcs
  end

let all_match_cases_take_unit_and_map_to_boolean_literals cases =
  EnumConstructor.Map.for_all
    (fun _ case ->
      match Mark.remove case with
      | EAbs { binder; tys; _ } -> (
        let _, body = Bindlib.unmbind binder in
        match Mark.remove body with
        | ELit (LBool _) ->
          let ty = List.hd tys in
          (* because of invariant [invariant_match], the arity is always one. *)
          Mark.remove ty = TLit TUnit
        | _ -> false)
      | _ -> assert false)
    cases

module ConcVarSet = struct
  type t = conc_expr Var.Set.t * int

  let _n = ref 0

  let create s =
    incr _n;
    s, !_n

  let compare (s1, n1) (s2, n2) =
    match Var.Set.compare s1 s2 with 0 -> compare n1 n2 | c -> c

  let format fmt (s, n) = Format.fprintf fmt "%a_%n" Var.Set.format s n
end

module VarSetMap = Map.Make (ConcVarSet)

let rec optimize_rec : flag list -> conc_expr -> conc_boxed_expr =
 fun optims e ->
  (* We proceed bottom-up, first apply on the subterms *)
  let e = Expr.map ~f:(optimize_rec optims) ~op:Fun.id e in
  let mark = Mark.get e in
  (* Fixme: when removing enclosing expressions, it would be better if we were
     able to keep the inner position (see the division_by_zero test) *)
  (* Then reduce the parent node (this is applied through Box.apply, therefore
     delayed to unbinding time: no need to be concerned about reboxing) *)
  let reduce (e : conc_expr) : conc_naked_expr =
    (* Todo: improve the handling of eapp(log,elit) cases here, it obfuscates
       the matches and the log calls are not preserved, which would be a good
       property *)
    match Mark.remove e with
    | EMatch { e = e'; cases; name = n }
      when linearize_match optims
           && all_match_cases_take_unit_and_map_to_boolean_literals cases ->
      (* transform matches whose arms are all of the form [Constructor () ->
         true/false] to a big [or] of all those cases *)
      let cases_true =
        EnumConstructor.Map.filter
          (fun _ case ->
            match Mark.remove case with
            | EAbs { binder; _ } -> (
              let _, body = Bindlib.unmbind binder in
              match Mark.remove body with
              | ELit (LBool b) -> b
              | _ -> failwith "should not happen")
            | _ -> assert false)
          cases
      in
      let boxed_e' = Expr.rebox e' in
      let m = Mark.get e' |> Expr.no_mark in
      let lunit = Expr.elit LUnit m in
      let enum_typ = Mark.add Pos.no_pos (TEnum n) in
      let bool_typ = Mark.add Pos.no_pos (TLit TBool) in
      let make_eq (cons : EnumConstructor.t) =
        let inj = Expr.einj ~name:n ~cons ~e:lunit m in
        Expr.eappop
          ~op:(Eq, Expr.mark_pos m)
          ~args:[boxed_e'; inj] ~tys:[enum_typ; enum_typ] m
      in
      let make_or e1 e2 =
        Expr.eappop
          ~op:(Or, Expr.mark_pos m)
          ~args:[e1; e2] ~tys:[bool_typ; bool_typ] m
      in
      let f cons _ acc =
        let eq = make_eq cons in
        make_or acc eq
      in
      let lfalse = Expr.elit (LBool false) m in
      EnumConstructor.Map.fold f cases_true lfalse
      |> Expr.unbox
      |> optimize_rec optims
      |> Expr.unbox
      |> Mark.remove
    | EDefault { excepts; just; cons } as e'
      when packing optims && List.length excepts > 2 -> begin
      if Global.options.debug then
        Message.debug "Before packing %a" (Shared_ast.Print.expr ()) e;
      let f acc exc =
        let set = Expr.free_vars exc in
        let set_unique = ConcVarSet.create set in
        VarSetMap.add set_unique exc acc
      in
      let sets = List.fold_left f VarSetMap.empty excepts in
      if Global.options.debug then
        Message.debug "map: %a"
          (fun fmt s -> VarSetMap.format (Shared_ast.Print.expr ()) fmt s)
          sets;
      let new_excepts =
        VarSetMap.to_seq sets
        |> List.of_seq
        |> List.map (fun (_, e) -> Expr.rebox e)
      in
      assert (List.length new_excepts = List.length excepts);
      let just = Expr.rebox just in
      let cons = Expr.rebox cons in
      let new_default = Expr.edefault ~excepts:new_excepts ~just ~cons mark in
      if Global.options.debug then
        Message.debug "After packing %a" (Shared_ast.Print.expr ())
          (Expr.unbox new_default);
      e'
    end
    | e -> e
  in
  Expr.Box.app1 e reduce mark

let optimize_expr (optims : flag list) : conc_expr -> conc_expr =
 fun e ->
  if linearize_match optims && Global.options.debug then
    Message.debug "[CONC] Applying match linearization optim";
  if packing optims && Global.options.debug then
    Message.debug "[CONC] Applying packing";
  if packing optims || linearize_match optims then
    Expr.unbox (optimize_rec optims e)
  else e
