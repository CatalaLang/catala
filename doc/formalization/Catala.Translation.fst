module Catala.Translation

module L = Catala.LambdaCalculus
module D = Catala.DefaultCalculus
open Catala.Translation.Helpers

(*** Translation definitions *)

let rec translate_ty (ty: D.ty) : Tot L.ty = match ty with
  | D.TBool -> L.TBool
  | D.TUnit -> L.TUnit
  | D.TArrow t1 t2 -> L.TArrow (translate_ty t1) (translate_ty t2)

let translate_lit (l: D.lit) : Tot L.lit = match l with
  | D.LTrue -> L.LTrue
  | D.LFalse -> L.LFalse
  | D.LUnit -> L.LUnit
  | D.LEmptyError -> L.LError L.EmptyError
  | D.LConflictError -> L.LError L.ConflictError

let rec translate_exp (e: D.exp) : Tot L.exp = match e with
  | D.EVar x -> L.EVar x
  | D.EApp e1 e2 tau_arg ->
    L.EApp (translate_exp e1) (translate_exp e2) (translate_ty tau_arg)
  | D.EAbs ty body -> L.EAbs (translate_ty ty) (translate_exp body)
  | D.ELit l -> L.ELit (translate_lit l)
  | D.EIf e1 e2 e3 -> L.EIf
    (translate_exp e1)
    (translate_exp e2)
    (translate_exp e3)
  | D.EDefault exceptions just cons tau ->
    build_default_translation
      (translate_exp_list exceptions)
      L.ENone
      (translate_exp just)
      (translate_exp cons)
      (translate_ty tau)
and translate_exp_list (l: list D.exp) : Tot (list L.exp) =
  match l with
  | [] -> []
  | hd::tl -> (L.EThunk (translate_exp hd))::(translate_exp_list tl)

let translate_env (g: D.env) : Tot L.env =
 FunctionalExtensionality.on_dom L.var
   (fun v -> match g v with None -> None | Some t -> Some (translate_ty t))

(*** Typing preservation *)

(**** Helpers and lemmas *)

let extend_translate_commute (g: D.env) (tau: D.ty)
    : Lemma (L.extend (translate_env g) (translate_ty tau) == translate_env (D.extend g tau))
  =
  FunctionalExtensionality.extensionality L.var (fun _ -> option L.ty)
    (L.extend (translate_env g) (translate_ty tau))
    (translate_env (D.extend g tau))

let translate_empty_is_empty () : Lemma (translate_env D.empty == L.empty) =
  FunctionalExtensionality.extensionality L.var (fun _ -> option L.ty)
    (translate_env D.empty)
    L.empty

(**** Typing preservation theorem *)

#push-options "--fuel 1 --ifuel 1 --z3rlimit 30"
let rec translation_preserves_typ (g: D.env) (e: D.exp) (tau: D.ty) : Lemma
    (requires (D.typing g e tau))
    (ensures (L.typing (translate_env g) (translate_exp e) (translate_ty tau)))
    (decreases %[e; 1])
  =
  match e with
  | D.EVar _ -> ()
  | D.EApp e1 e2 tau_arg ->
    translation_preserves_typ g e1 (D.TArrow tau_arg tau);
    translation_preserves_typ g e2 tau_arg
  | D.EAbs tau_arg body -> begin
    match tau with
    | D.TArrow tau_in tau_out ->
      if tau_in = tau_arg then begin
        translation_preserves_typ (D.extend g tau_in) body tau_out;
        extend_translate_commute g tau_in
      end else ()
    | _ -> ()
  end
  | D.ELit _ -> ()
  | D.EIf e1 e2 e3 ->
    translation_preserves_typ g e1 D.TBool;
    translation_preserves_typ g e2 tau;
    translation_preserves_typ g e3 tau
  | D.EDefault exceptions just cons tau_out ->
    if tau = tau_out then begin
      let tau' = translate_ty tau in
      translation_preserves_typ_exceptions g e exceptions tau;
      typ_process_exceptions_f (translate_env g) tau';
      translation_preserves_typ g just D.TBool;
      translation_preserves_typ g cons tau;
      let result_exp = L.EMatchOption
        (L.EFoldLeft
          (process_exceptions_f tau')
          L.ENone (L.TOption tau')
          (L.EList (translate_exp_list exceptions)) (L.TArrow L.TUnit tau'))
        tau'
        (L.EIf
          (translate_exp just)
          (translate_exp cons)
          (L.ELit (L.LError L.EmptyError)))
        (L.EAbs tau' (L.EVar 0))
      in
      let open FStar.Tactics in
      assert(L.typing (translate_env g) result_exp tau') by begin
        compute ();
        smt ()
      end
    end else ()
and translation_preserves_typ_exceptions
  (g: D.env)
  (e: D.exp)
  (exceptions: list D.exp{exceptions << e})
  (tau: D.ty)
    : Lemma
      (requires (D.typing_list g exceptions tau))
      (ensures (L.typing_list
        (translate_env g)
        (translate_exp_list exceptions)
        (L.TArrow L.TUnit (translate_ty tau))))
      (decreases %[e; 0; exceptions])
  =
  match exceptions with
  | [] -> ()
  | hd::tl ->
    translation_preserves_typ g hd tau;
    translation_preserves_typ_exceptions g e tl tau;
    let g' = translate_env g in
    let hd' = translate_exp hd in
    let tl' = translate_exp_list tl in
    let tau' = translate_ty tau in
    let thunked_tau' = L.TArrow L.TUnit tau' in
    assert(L.typing_list g' tl' thunked_tau');
    assert(L.typing g' hd' tau');
    assert(L.typing g' (L.EThunk hd') thunked_tau')
#pop-options

let translation_preserves_empty_typ (e: D.exp) (tau: D.ty) : Lemma
    (requires (D.typing D.empty e tau))
    (ensures (L.typing L.empty (translate_exp e) (translate_ty tau)))
  =
  translate_empty_is_empty ();
  translation_preserves_typ D.empty e tau

(*** Translation correctness *)

(**** Helpers *)

let translate_var_to_exp (s: D.var_to_exp) : Tot L.var_to_exp = fun x -> translate_exp (s x)

#push-options "--fuel 3 --ifuel 1 --z3rlimit 50"
let rec substitution_correctness (s: D.var_to_exp) (e: D.exp)
    : Lemma (ensures (
      translate_exp (D.subst s e) ==
        L.subst (translate_var_to_exp s) (translate_exp e)))
      (decreases %[D.is_var_size e; D.is_renaming_size s; 1; e])
  =
  match e with
  | D.EVar y -> ()
  | D.ELit _ -> ()
  | D.EIf e1 e2 e3 ->
    substitution_correctness s e1;
    substitution_correctness s e2;
    substitution_correctness s e3
  | D.EAbs _ body ->
    substitution_correctness (D.subst_abs s) body;
    translate_var_to_exp_abs_commute s
  | D.EApp e1 e2 _ ->
    substitution_correctness s e1;
    substitution_correctness s e2
  | D.EDefault exceptions just cons tau ->
    substitution_correctness s just;
    substitution_correctness s cons;
    substitution_correctness_list s exceptions;
    process_exceptions_untouched_by_subst (translate_var_to_exp s) (translate_ty tau)
and substitution_correctness_list (s: D.var_to_exp) (l: list D.exp)
    : Lemma (ensures (
      translate_exp_list (D.subst_list s l) ==
      L.subst_list (translate_var_to_exp s) (translate_exp_list l)))
      (decreases %[1; D.is_renaming_size s; 1; l])
 =
 match l with
 | [] -> ()
 | hd::tl ->
   let s' = translate_var_to_exp s in
   substitution_correctness_list s tl;
   substitution_correctness s hd
and translate_var_to_exp_abs_commute (s: D.var_to_exp)
    : Lemma
      (ensures (
        FunctionalExtensionality.feq
          (translate_var_to_exp (D.subst_abs s))
          (L.subst_abs (translate_var_to_exp s))))
      (decreases %[1; D.is_renaming_size s; 0])
  =
  let s1 = translate_var_to_exp (D.subst_abs s) in
  let s2 = L.subst_abs (translate_var_to_exp s) in
  let aux (x: L.var) : Lemma (s1 x == s2 x) =
    if x = 0 then () else
      substitution_correctness D.increment (s (x - 1))
  in
  Classical.forall_intro aux
#pop-options


let exceptions_smaller'
  (e: D.exp{match e with D.EDefault _ _ _ _ -> True | _ -> False})
    : Lemma(let D.EDefault exc just cons tau = e in
     exc << e /\ just << e /\ cons << e /\ tau << e)
  =
  ()

let exceptions_smaller
  (exceptions: list D.exp)
  (just: D.exp)
  (cons: D.exp)
  (tau: D.ty)
    : Lemma(
      exceptions <<  (D.EDefault exceptions just cons tau) /\
      just << (D.EDefault exceptions just cons tau) /\
      cons << (D.EDefault exceptions just cons tau) /\
      tau << (D.EDefault exceptions just cons tau)
    )
  =
  exceptions_smaller' (D.EDefault exceptions just cons tau)

let build_default_translation_typing_source
  (exceptions: list D.exp)
  (acc: L.exp)
  (just: D.exp)
  (cons: D.exp)
  (tau: D.ty)
  (g: D.env)
    : Lemma
      (requires (
        D.typing_list g exceptions tau /\
        D.typing g just D.TBool /\
        D.typing g cons tau) /\
        L.typing (translate_env g) acc (L.TOption (translate_ty tau)))
      (ensures (
        L.typing (translate_env g) (build_default_translation
          (translate_exp_list exceptions)
          acc
          (translate_exp just)
          (translate_exp cons)
          (translate_ty tau)) (translate_ty tau) /\
        L.typing_list
          (translate_env g)
          (translate_exp_list exceptions)
          (L.TArrow L.TUnit (translate_ty tau))
      ))
  =
  let e = D.EDefault exceptions just cons tau in
  exceptions_smaller exceptions just cons tau;
  translation_preserves_typ_exceptions g e exceptions tau;
  translation_preserves_typ g just D.TBool;
  translation_preserves_typ g cons tau;
  build_default_translation_typing
    (translate_exp_list exceptions)
    acc
    (translate_exp just)
    (translate_exp cons)
    (translate_ty tau)
    (translate_env g)

let rec translate_list_is_value_list (l: list D.exp)
    : Lemma(L.is_value_list (translate_exp_list l))
  =
  match l with
  | [] -> ()
  | _::tl -> translate_list_is_value_list tl

(**** Main theorems *)

let translation_correctness_value (e: D.exp) : Lemma
    ((D.is_value e) <==> (L.is_value (translate_exp e)))
  = ()

let rec_correctness_step_type (de: D.exp) : Type =
  (df: D.exp{df << de}) -> (dtau_f:D.ty) ->
    Pure (nat & typed_l_exp (translate_ty dtau_f) & nat)
      (requires (Some? (D.step df) /\ D.typing D.empty df dtau_f))
      (ensures (fun (n1, target_f, n2) ->
        translation_preserves_empty_typ df dtau_f;
        let df' = Some?.v (D.step df) in
        D.preservation df dtau_f;
        translation_preserves_empty_typ df' dtau_f;
        take_l_steps (translate_ty dtau_f) (translate_exp df) n1 == Some target_f /\
        take_l_steps (translate_ty dtau_f) (translate_exp df') n2 == Some target_f
      ))
      (decreases df)

#push-options "--fuel 2 --ifuel 1 --z3rlimit 70"
let translation_correctness_exceptions_left_to_right_step_head_not_value
  (de: D.exp)
  (dexceptions: list D.exp {dexceptions << de})
  (djust: D.exp{djust << de})
  (dcons: D.exp{dcons << de})
  (dtau: D.ty)
  (acc: typed_l_exp (L.TOption (translate_ty dtau)))
  (rec_lemma: rec_correctness_step_type de)
    : Pure (nat & typed_l_exp (translate_ty dtau) & nat)
      (requires (
        D.typing_list D.empty dexceptions dtau /\
        D.typing D.empty djust D.TBool /\
        D.typing D.empty dcons dtau /\
        L.is_value acc /\ not (L.is_error acc) /\
        Some? (D.step_exceptions_left_to_right de dexceptions djust dcons dtau) /\
        (match dexceptions with hd::tl -> not (D.is_value hd) | _ -> False)
      ))
      (ensures (fun (n1, target_e, n2) ->
        translate_empty_is_empty ();
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        translation_preserves_empty_typ djust D.TBool;
        translation_preserves_empty_typ dcons dtau;
        let lexceptions = translate_exp_list dexceptions in
        let ljust = translate_exp djust in
        let lcons = translate_exp dcons in
        let Some de' = D.step_exceptions_left_to_right de dexceptions djust dcons dtau in
        let ltau = translate_ty dtau in
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        build_default_translation_typing lexceptions acc ljust lcons ltau L.empty;
        take_l_steps ltau (build_default_translation lexceptions acc ljust lcons ltau)
          n1 == Some target_e /\
        begin
          D.preservation_exceptions_left_to_right de dexceptions djust dcons dtau;
          translation_preserves_empty_typ de' dtau;
          let le' = translate_exp de' in
          match de' with
          | D.ELit D.LConflictError -> take_l_steps ltau le' n2 == Some target_e
          | D.EDefault dexceptions' djust' dcons' dtau' ->
            assert(djust' == djust /\ dcons' == dcons /\ dtau' == dtau);
            let lexceptions' = translate_exp_list dexceptions' in
            take_l_steps ltau (build_default_translation lexceptions' acc ljust lcons ltau)
              n2 == Some target_e
        end
      ))
      (decreases dexceptions)
  =
  let Some de' = D.step_exceptions_left_to_right de dexceptions djust dcons dtau in
  let le = translate_exp de in
  D.preservation_exceptions_left_to_right de dexceptions djust dcons dtau;
  translate_empty_is_empty ();
  translation_preserves_typ_exceptions D.empty de dexceptions dtau;
  translation_preserves_empty_typ djust D.TBool;
  translation_preserves_empty_typ dcons dtau;
  translation_preserves_empty_typ de' dtau;
  let ltau = translate_ty dtau in
  let le' : typed_l_exp ltau = translate_exp de' in
  let ljust = translate_exp djust in
  let lcons = translate_exp dcons in
  let lexceptions = translate_exp_list dexceptions in
  match dexceptions with
  | [] ->  0, le, 0
  | dhd::dtl ->
    let ltl = translate_exp_list dtl in
    let lhd = translate_exp dhd in
    begin
    match D.step dhd with
      | Some (D.ELit D.LConflictError) ->
         D.preservation dhd dtau;
         translation_preserves_empty_typ dhd dtau;
         let n1_hd, target_hd, n2_hd = rec_lemma dhd dtau in
         translation_preserves_empty_typ dhd dtau;
         translation_preserves_empty_typ djust D.TBool;
         translation_preserves_empty_typ dcons dtau;
         let l_err : typed_l_exp ltau = L.ELit (L.LError L.ConflictError) in
         assert(n2_hd == 0 /\ target_hd  == l_err);
         assert(take_l_steps ltau lhd n1_hd == Some l_err);
         translate_list_is_value_list dexceptions;
         build_default_translation_typing_source dexceptions acc djust dcons dtau D.empty;
         lift_multiple_l_steps_exceptions_head ltau ltl acc ljust lcons n1_hd lhd l_err;
         assert(take_l_steps ltau
           (build_default_translation ((L.EThunk lhd)::ltl) acc ljust lcons ltau) (n1_hd + 4) ==
             Some (exceptions_head_lift ltau ltl acc ljust lcons l_err));
         exceptions_head_lift_steps_to_error ltau ltl acc ljust lcons;
         assert(take_l_steps ltau (exceptions_head_lift ltau ltl acc ljust lcons l_err) 5 ==
           Some l_err);
         assert(le' == l_err);
         take_l_steps_transitive ltau
           (build_default_translation ((L.EThunk lhd)::ltl) acc ljust lcons ltau)
           (exceptions_head_lift ltau ltl acc ljust lcons l_err)
           (n1_hd + 4)
           5;
         let lexceptions = translate_exp_list dexceptions in
         assert(take_l_steps ltau (build_default_translation lexceptions acc ljust lcons ltau)
            (n1_hd + 4 + 5) == Some le');
         (n1_hd + 4 + 5, l_err, 0)
      | Some dhd' ->
         D.preservation dhd dtau;
         translation_preserves_empty_typ dhd dtau;
         translation_preserves_empty_typ dhd' dtau;
         let lhd' : typed_l_exp ltau = translate_exp dhd' in
         let n1_hd, target_hd, n2_hd = rec_lemma dhd dtau in
         translation_preserves_empty_typ djust D.TBool;
         translation_preserves_empty_typ dcons dtau;
         assert(take_l_steps ltau lhd n1_hd == Some target_hd);
         assert(take_l_steps ltau lhd' n2_hd == Some target_hd);
         translate_list_is_value_list dexceptions;
         translate_empty_is_empty ();
         build_default_translation_typing_source dexceptions acc djust dcons dtau D.empty;
         assert(L.is_value_list ltl);
         assert(L.typing_list L.empty ltl (L.TArrow L.TUnit ltau));
         lift_multiple_l_steps_exceptions_head ltau ltl acc ljust lcons n1_hd lhd target_hd;
         lift_multiple_l_steps_exceptions_head ltau ltl acc ljust lcons n2_hd lhd' target_hd;
         let target_lexp : typed_l_exp ltau =
           exceptions_head_lift ltau ltl acc ljust lcons target_hd
         in
         assert(take_l_steps ltau (build_default_translation ((L.EThunk lhd)::ltl) acc
          ljust lcons ltau) (n1_hd + 4) == Some target_lexp);
         lift_multiple_l_steps_exceptions_head ltau ltl acc ljust lcons n2_hd lhd' target_hd;
         build_default_translation_typing ((L.EThunk lhd')::ltl) acc ljust lcons ltau
           L.empty;
         assert(take_l_steps ltau (build_default_translation ((L.EThunk lhd')::ltl) acc
          ljust lcons ltau) (n2_hd + 4) == Some target_lexp);
         let lexceptions = translate_exp_list dexceptions in
         assert((L.EThunk lhd)::ltl == lexceptions);
         assert(le' == translate_exp (D.EDefault (dhd'::dtl) djust dcons dtau));
         (n1_hd + 4, target_lexp, n2_hd + 4)
    end
#pop-options

#push-options "--fuel 2 --ifuel 1 --z3rlimit 150"
let step_exceptions_left_to_right_result_shape
  (de: D.exp)
  (dexceptions: list D.exp {dexceptions << de})
  (djust: D.exp{djust << de})
  (dcons: D.exp{dcons << de})
  (dtau: D.ty)
    : Lemma
      (requires (
        Some? (D.step_exceptions_left_to_right de dexceptions djust dcons dtau) /\
        (match dexceptions with dhd::dtl -> D.is_value dhd | _ -> False)
      ))
      (ensures (
        let dhd::dtl = dexceptions in
        match D.step_exceptions_left_to_right de dtl djust dcons dtau with
        | Some (D.ELit D.LConflictError) -> True
        | Some (D.EDefault dtl' djust' dcons' dtau') ->
          djust' == djust /\ dcons' == dcons /\ dtau' == dtau
        | _ -> False
      ))
  =
  ()
#pop-options

#push-options "--fuel 2 --ifuel 1 --z3rlimit 150"
let translation_correctness_exceptions_left_to_right_step_error
  (de: D.exp)
  (dexceptions: list D.exp {dexceptions << de})
  (djust: D.exp{djust << de})
  (dcons: D.exp{dcons << de})
  (dtau: D.ty)
  (acc: typed_l_exp (L.TOption (translate_ty dtau)))
    : Pure (nat & typed_l_exp (translate_ty dtau) & nat)
      (requires (
        D.typing_list D.empty dexceptions dtau /\
        D.typing D.empty djust D.TBool /\
        D.typing D.empty dcons dtau /\
        L.is_value acc /\ L.is_error acc /\  acc <> L.ELit (L.LError L.EmptyError) /\
        Some? (D.step_exceptions_left_to_right de dexceptions djust dcons dtau)
      ))
      (ensures (fun (n1, target_e, n2) ->
        translate_empty_is_empty ();
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        translation_preserves_empty_typ djust D.TBool;
        translation_preserves_empty_typ dcons dtau;
        let lexceptions = translate_exp_list dexceptions in
        let ljust = translate_exp djust in
        let lcons = translate_exp dcons in
        let Some de' = D.step_exceptions_left_to_right de dexceptions djust dcons dtau in
        let ltau = translate_ty dtau in
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        build_default_translation_typing lexceptions acc ljust lcons ltau L.empty;
        take_l_steps ltau (build_default_translation lexceptions acc ljust lcons ltau)
          n1 == Some target_e /\
        begin
          D.preservation_exceptions_left_to_right de dexceptions djust dcons dtau;
          translation_preserves_empty_typ de' dtau;
          let le' = translate_exp de' in
          match de' with
          | D.ELit D.LConflictError -> take_l_steps ltau le' n2 == Some target_e
          | D.EDefault dexceptions' djust' dcons' dtau' ->
            assert(djust' == djust /\ dcons' == dcons /\ dtau' == dtau);
            let lexceptions' = translate_exp_list dexceptions' in
            take_l_steps ltau (build_default_translation lexceptions' acc ljust lcons ltau)
              n2 == Some target_e
        end
      ))
      (decreases dexceptions)
  =
  let Some de' = D.step_exceptions_left_to_right de dexceptions djust dcons dtau in
  let le = translate_exp de in
  D.preservation_exceptions_left_to_right de dexceptions djust dcons dtau;
  translate_empty_is_empty ();
  translation_preserves_typ_exceptions D.empty de dexceptions dtau;
  translation_preserves_empty_typ djust D.TBool;
  translation_preserves_empty_typ dcons dtau;
  translation_preserves_empty_typ de' dtau;
  let ltau = translate_ty dtau in
  let le' : typed_l_exp ltau = translate_exp de' in
  let ljust = translate_exp djust in
  let lcons = translate_exp dcons in
  let lexceptions = translate_exp_list dexceptions in
  let c_err : typed_l_exp ltau = L.ELit (L.LError L.ConflictError) in
  let aux () : Lemma (acc == c_err) =
    match acc with
    | L.ELit (L.LError _) -> ()
    | _ -> ()
  in
  aux ();
   translate_list_is_value_list dexceptions;
  let n1 = step_exceptions_general_conflict_error ltau ljust lcons lexceptions in
  build_default_translation_typing lexceptions c_err ljust lcons ltau L.empty;
  assert(take_l_steps ltau (build_default_translation lexceptions acc ljust lcons ltau)
          n1 == Some c_err);
  D.preservation_exceptions_left_to_right de dexceptions djust dcons dtau;
  translation_preserves_empty_typ de' dtau;
  match de' with
  | D.ELit D.LConflictError -> n1, c_err, 0
  | D.EDefault dexceptions' djust' dcons' dtau' ->
     let lexceptions' = translate_exp_list dexceptions' in
     translate_list_is_value_list dexceptions';
     assert(djust' == djust /\ dcons' == dcons /\ dtau' == dtau);
     translation_preserves_typ_exceptions D.empty de' dexceptions' dtau';
     build_default_translation_typing lexceptions' acc ljust lcons ltau L.empty;
     let n2 = step_exceptions_general_conflict_error ltau ljust lcons lexceptions' in
     n1, c_err, n2
#pop-options

#push-options "--fuel 2 --ifuel 1 --z3rlimit 1500"
let rec translation_correctness_exceptions_left_to_right_step
  (de: D.exp)
  (dexceptions: list D.exp {dexceptions << de})
  (djust: D.exp{djust << de})
  (dcons: D.exp{dcons << de})
  (dtau: D.ty)
  (acc: typed_l_exp (L.TOption (translate_ty dtau)))
  (rec_lemma: rec_correctness_step_type de)
    : Pure (nat & typed_l_exp (translate_ty dtau) & nat)
      (requires (
        D.typing_list D.empty dexceptions dtau /\
        D.typing D.empty djust D.TBool /\
        D.typing D.empty dcons dtau /\
        L.is_value acc /\  acc <> L.ELit (L.LError L.EmptyError) /\
        Some? (D.step_exceptions_left_to_right de dexceptions djust dcons dtau)
      ))
      (ensures (fun (n1, target_e, n2) ->
        translate_empty_is_empty ();
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        translation_preserves_empty_typ djust D.TBool;
        translation_preserves_empty_typ dcons dtau;
        let lexceptions = translate_exp_list dexceptions in
        let ljust = translate_exp djust in
        let lcons = translate_exp dcons in
        let Some de' = D.step_exceptions_left_to_right de dexceptions djust dcons dtau in
        let ltau = translate_ty dtau in
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        build_default_translation_typing lexceptions acc ljust lcons ltau L.empty;
        take_l_steps ltau (build_default_translation lexceptions acc ljust lcons ltau)
          n1 == Some target_e /\
        begin
          D.preservation_exceptions_left_to_right de dexceptions djust dcons dtau;
          translation_preserves_empty_typ de' dtau;
          let le' = translate_exp de' in
          match de' with
          | D.ELit D.LConflictError -> take_l_steps ltau le' n2 == Some target_e
          | D.EDefault dexceptions' djust' dcons' dtau' ->
            assert(djust' == djust /\ dcons' == dcons /\ dtau' == dtau);
            let lexceptions' = translate_exp_list dexceptions' in
            take_l_steps ltau (build_default_translation lexceptions' acc ljust lcons ltau)
              n2 == Some target_e
        end
      ))
      (decreases dexceptions)
  =
  let Some de' = D.step_exceptions_left_to_right de dexceptions djust dcons dtau in
  let le = translate_exp de in
  D.preservation_exceptions_left_to_right de dexceptions djust dcons dtau;
  translate_empty_is_empty ();
  translation_preserves_typ_exceptions D.empty de dexceptions dtau;
  translation_preserves_empty_typ djust D.TBool;
  translation_preserves_empty_typ dcons dtau;
  translation_preserves_empty_typ de' dtau;
  let ltau = translate_ty dtau in
  let le' : typed_l_exp ltau = translate_exp de' in
  let ljust = translate_exp djust in
  let lcons = translate_exp dcons in
  let lexceptions = translate_exp_list dexceptions in
  if L.is_error acc then
    translation_correctness_exceptions_left_to_right_step_error
      de dexceptions djust dcons dtau acc
  else match dexceptions with
  | [] ->  0, le, 0
  | dhd::dtl ->
    let ltl = translate_exp_list dtl in
    let lhd = translate_exp dhd in
    if D.is_value dhd then begin
      step_exceptions_left_to_right_result_shape de dexceptions djust dcons dtau;
      match D.step_exceptions_left_to_right de dtl djust dcons dtau with
      | Some (D.ELit D.LConflictError) ->
        assert(de' == D.ELit (D.LConflictError));
        assert(le' == L.ELit (L.LError (L.ConflictError)));
        let l_err : typed_l_exp ltau = L.ELit (L.LError (L.ConflictError)) in
        translate_list_is_value_list dexceptions;
        build_default_translation_typing_source dexceptions acc djust dcons dtau D.empty;
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        assert(L.typing_list L.empty ltl (L.TArrow L.TUnit ltau));
        assert(L.is_value_list ltl);
        translation_preserves_empty_typ dhd dtau;
        lift_multiple_l_steps_exceptions_head ltau ltl acc ljust lcons 0 lhd lhd;
        assert(take_l_steps ltau (build_default_translation lexceptions acc ljust lcons ltau) 4 ==
          Some (exceptions_head_lift ltau ltl acc ljust lcons lhd));
        let new_acc, n_to_tl = step_exceptions_head_value ltau ltl acc ljust lcons lhd in
        assert(take_l_steps ltau (exceptions_head_lift ltau ltl acc ljust lcons lhd) n_to_tl ==
          Some (exceptions_init_lift ltau ltl ljust lcons new_acc));
        take_l_steps_transitive ltau
          (build_default_translation lexceptions acc ljust lcons ltau)
          (exceptions_head_lift ltau ltl acc ljust lcons lhd)
          4 n_to_tl;
        assert(take_l_steps ltau (build_default_translation lexceptions acc ljust lcons ltau)
          (4 + n_to_tl) == Some (exceptions_init_lift ltau ltl ljust lcons new_acc));
        assert(exceptions_init_lift ltau ltl ljust lcons new_acc ==
          build_default_translation ltl new_acc ljust lcons ltau);
        let n1_tl, target_tl, n2_tl = translation_correctness_exceptions_left_to_right_step
          de dtl djust dcons dtau new_acc rec_lemma
        in
        assert(take_l_steps ltau (build_default_translation ltl new_acc ljust lcons ltau)
          n1_tl == Some target_tl);
        take_l_steps_transitive ltau
          (build_default_translation lexceptions acc ljust lcons ltau)
          (exceptions_init_lift ltau ltl ljust lcons new_acc)
          (4 + n_to_tl)
          n1_tl;
        4 + n_to_tl + n1_tl, l_err, 0
      | Some (D.EDefault dtl' djust' dcons' dtau') ->
        // Left side
        assert(djust' == djust /\ dcons' == dcons /\ dtau' == dtau);
        translate_list_is_value_list dexceptions;
        build_default_translation_typing_source dexceptions acc djust dcons dtau D.empty;
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        assert(L.typing_list L.empty ltl (L.TArrow L.TUnit ltau));
        assert(L.is_value_list ltl);
        translation_preserves_empty_typ dhd dtau;
        lift_multiple_l_steps_exceptions_head ltau ltl acc ljust lcons 0 lhd lhd;
        let stepped_le_1 : typed_l_exp ltau =
          exceptions_head_lift ltau ltl acc ljust lcons lhd
        in
        assert(take_l_steps ltau (build_default_translation lexceptions acc ljust lcons ltau)
          4 == Some stepped_le_1);
        let new_acc, n_to_tl = step_exceptions_head_value ltau ltl acc ljust lcons lhd in
        take_l_steps_transitive ltau
          (build_default_translation lexceptions acc ljust lcons ltau)
          stepped_le_1
          4
          n_to_tl;
        let stepped_le_2 : typed_l_exp ltau =
          exceptions_init_lift ltau ltl ljust lcons new_acc
        in
        assert(take_l_steps ltau (build_default_translation lexceptions acc ljust lcons ltau)
          (4 + n_to_tl) == Some stepped_le_2);
        // Right side
        let dexceptions' = dhd::dtl' in
        let lexceptions' = translate_exp_list dexceptions' in
        let ltl' = translate_exp_list dtl' in
        build_default_translation_typing_source dexceptions' acc djust dcons dtau D.empty;
        exceptions_smaller dexceptions' djust dcons dtau;
        translation_preserves_typ_exceptions D.empty
          (D.EDefault dexceptions' djust dcons dtau)
          dexceptions' dtau;
        translate_list_is_value_list dexceptions';
        assert(L.typing_list L.empty ltl' (L.TArrow L.TUnit ltau));
        assert(L.is_value_list ltl');
        lift_multiple_l_steps_exceptions_head ltau ltl' acc ljust lcons 0 lhd lhd;
        let stepped_le_1' : typed_l_exp ltau =
          exceptions_head_lift ltau ltl' acc ljust lcons lhd
        in
        assert(take_l_steps ltau (build_default_translation lexceptions' acc ljust lcons ltau)
          4 == Some stepped_le_1');
        let new_acc', n_to_tl' = step_exceptions_head_value ltau ltl' acc ljust lcons lhd in
        take_l_steps_transitive ltau
          (build_default_translation lexceptions' acc ljust lcons ltau)
          stepped_le_1'
          4
          n_to_tl';
        let stepped_le_2' : typed_l_exp ltau =
          exceptions_init_lift ltau ltl' ljust lcons new_acc'
        in
        assert(take_l_steps ltau (build_default_translation lexceptions' acc ljust lcons ltau)
          (4 + n_to_tl') == Some stepped_le_2');
        // Both
        step_exceptions_head_value_same_acc_result ltau ltl ltl' acc ljust lcons lhd;
        let n1_tl, target_tl, n2_tl =
          translation_correctness_exceptions_left_to_right_step
            de dtl djust dcons dtau new_acc rec_lemma
        in
        take_l_steps_transitive ltau
          (build_default_translation lexceptions acc ljust lcons ltau)
          stepped_le_2
          (4 + n_to_tl)
          n1_tl;
        take_l_steps_transitive ltau
          (build_default_translation lexceptions' acc ljust lcons ltau)
          stepped_le_2'
          (4 + n_to_tl')
          n2_tl;
        4 + n_to_tl + n1_tl, target_tl, 4 + n_to_tl' + n2_tl
    end else begin
      translation_correctness_exceptions_left_to_right_step_head_not_value
        de dexceptions djust dcons dtau acc rec_lemma
    end
#pop-options

let dacc_lacc_sync
  (ltau: L.ty)
  (dacc: D.empty_count_result)
  (lacc: typed_l_exp (L.TOption ltau))
    : Tot prop
  =
  match dacc, lacc with
  | D.AllEmpty, L.ENone -> True
  | D.OneNonEmpty de', L.ESome le' -> le' == translate_exp de' /\ L.is_value le'
  | D.Conflict, L.ELit (L.LError L.ConflictError) -> True
  | _ -> False

#push-options "--fuel 3 --ifuel 2 --z3rlimit 40"
let step_exceptions_head_value_source_acc_synced_dacc
  (dtau: D.ty)
  (ltl: list L.exp{
    L.is_value_list ltl /\ L.typing_list L.empty ltl (L.TArrow L.TUnit (translate_ty dtau))
  })
  (dacc: D.empty_count_result)
  (lacc: (typed_l_exp (L.TOption (translate_ty dtau))))
  (ljust: typed_l_exp L.TBool)
  (lcons: typed_l_exp (translate_ty dtau))
  (dhd: D.exp)
    : Pure (D.empty_count_result)
      (requires (
        dacc_lacc_sync (translate_ty dtau) dacc lacc /\
        D.typing D.empty dhd dtau /\ D.is_value dhd /\
        L.is_value lacc /\
        dacc <> D.Conflict
      ))
      (ensures (fun new_dacc ->
        translate_empty_is_empty ();
        translation_preserves_empty_typ dhd dtau;
        let ltau = translate_ty dtau in
        let new_lacc, _ =
          step_exceptions_head_value ltau ltl lacc ljust lcons (translate_exp dhd)
        in
        dacc_lacc_sync ltau new_dacc new_lacc
      ))
  =
  let new_dacc =
    match dhd, dacc with
    | D.ELit D.LEmptyError, D.AllEmpty -> D.AllEmpty
    | D.ELit D.LEmptyError, D.OneNonEmpty e -> D.OneNonEmpty e
    | D.ELit D.LConflictError, _ -> D.Conflict
    | _, D.AllEmpty -> D.OneNonEmpty dhd
    | _, D.OneNonEmpty _ -> D.Conflict
  in
  translate_empty_is_empty ();
  translation_preserves_empty_typ dhd dtau;
  let ltau = translate_ty dtau in
  let new_lacc, _ =
  step_exceptions_head_value ltau ltl lacc ljust lcons (translate_exp dhd)
  in
  let lhd = translate_exp dhd in
  let aux () : Lemma (dacc_lacc_sync ltau new_dacc new_lacc) =
    match dhd, dacc with
    | D.ELit D.LEmptyError, D.AllEmpty -> ()
    | D.ELit D.LEmptyError, D.OneNonEmpty e -> ()
    | D.ELit D.LConflictError, _ -> ()
    | _, D.AllEmpty -> ()
    | _, D.OneNonEmpty _ -> ()
  in
  aux ();
  new_dacc
#pop-options

#push-options "--fuel 2 --ifuel 1 --z3rlimit 70"
let rec translation_correctness_exceptions_empty_count_exception_triggered
  (de: D.exp)
  (dexceptions: list D.exp {dexceptions << de})
  (djust: D.exp{djust << de})
  (dcons: D.exp{dcons << de})
  (dtau: D.ty)
  (dacc: D.empty_count_result)
  (lacc: typed_l_exp (L.TOption (translate_ty dtau)))
  (rec_lemma: rec_correctness_step_type de)
    : Pure (nat & typed_l_exp (translate_ty dtau))
      (requires (
        D.typing D.empty de dtau /\
        D.typing_list D.empty dexceptions dtau /\
        D.typing D.empty djust D.TBool /\
        D.typing D.empty dcons dtau /\
        dacc_lacc_sync (translate_ty dtau) dacc lacc /\
        Some? (D.step de) /\
        L.is_value lacc /\
        List.Tot.for_all D.is_value dexceptions /\
        (D.step de == (match D.empty_count dacc dexceptions with
          | D.AllEmpty -> None
          | D.OneNonEmpty e' -> Some e'
          | D.Conflict -> Some (D.ELit D.LConflictError)))
      ))
      (ensures (fun (n, target_e) ->
        translate_empty_is_empty ();
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        translation_preserves_empty_typ djust D.TBool;
        translation_preserves_empty_typ dcons dtau;
        let Some de' = D.step de in
        D.preservation de dtau;
        translation_preserves_empty_typ de' dtau;
        let le' = translate_exp de' in
        let lexceptions = translate_exp_list dexceptions in
        let ljust = translate_exp djust in
        let lcons = translate_exp dcons in
        let ltau = translate_ty dtau in
        let l_err : typed_l_exp ltau = L.ELit (L.LError L.ConflictError) in
        build_default_translation_typing lexceptions lacc ljust lcons ltau L.empty;
        take_l_steps ltau (build_default_translation lexceptions lacc ljust lcons ltau) n
          == Some target_e /\
        (match D.empty_count dacc dexceptions with
          | D.AllEmpty -> False
          | D.OneNonEmpty e' -> translate_exp e' == target_e
          | D.Conflict -> l_err == target_e)
      ))
      (decreases dexceptions)
  =
  translation_preserves_typ_exceptions D.empty de dexceptions dtau;
  translation_preserves_empty_typ djust D.TBool;
  translation_preserves_empty_typ dcons dtau;
  let ljust = translate_exp djust in
  let lcons = translate_exp dcons in
  let ltau = translate_ty dtau in
  let l_err : typed_l_exp ltau = L.ELit (L.LError L.ConflictError) in
  match dexceptions with
  | [] -> begin
    match lacc with
    | L.ELit (L.LError L.ConflictError) ->
      let n_err = step_exceptions_empty_conflict_error ltau ljust lcons in
      n_err, l_err
    | L.ESome lacc_inner ->
      assert(L.is_value lacc_inner);
      assert(not (L.is_error lacc_inner));
      let n = step_exceptions_empty_some_acc ltau ljust lcons lacc_inner in
      n, lacc_inner
  end
  | dhd::dtl -> begin
    FStar.List.Tot.Base.for_all_mem D.is_value dexceptions;
    translate_empty_is_empty ();
    translation_preserves_typ_exceptions D.empty de dexceptions dtau;
    translate_list_is_value_list dtl;
    assert(D.is_value dhd);
    translation_correctness_value dhd;
    let ltl = translate_exp_list dtl in
    let lhd : typed_l_exp ltau = translate_exp dhd in
    match dacc with
    | D.Conflict ->
      let n_err = step_exceptions_cons_conflict_error ltau ljust lcons lhd ltl in
      n_err, l_err
    | _ ->
      lift_multiple_l_steps_exceptions_head ltau ltl lacc ljust lcons 0 lhd lhd;
      let new_lacc, n_acc = step_exceptions_head_value ltau ltl lacc ljust lcons lhd in
      let new_dacc = step_exceptions_head_value_source_acc_synced_dacc
        dtau ltl dacc lacc ljust lcons dhd
      in
      let n_final, target_e =
        translation_correctness_exceptions_empty_count_exception_triggered
          de dtl djust dcons dtau new_dacc new_lacc rec_lemma
      in
      let lexceptions = translate_exp_list dexceptions in
      build_default_translation_typing lexceptions lacc ljust lcons ltau L.empty;
      take_l_steps_transitive ltau
        (build_default_translation lexceptions lacc ljust lcons ltau)
        (exceptions_head_lift ltau ltl lacc ljust lcons lhd)
        4 n_acc;
      let intermediate_e : typed_l_exp ltau =
        exceptions_init_lift ltau ltl ljust lcons new_lacc
      in
      assert(take_l_steps ltau (build_default_translation lexceptions lacc ljust lcons ltau)
        (n_acc + 4) == Some intermediate_e);
      assert(intermediate_e == (build_default_translation ltl new_lacc ljust lcons ltau));
      take_l_steps_transitive ltau
        (build_default_translation lexceptions lacc ljust lcons ltau)
        intermediate_e
        (4 + n_acc)
        n_final;
      4 + n_acc + n_final, target_e
  end
#pop-options

#push-options "--fuel 2 --ifuel 1 --z3rlimit 50"
let translation_correctness_exceptions_step
  (de: D.exp)
  (dexceptions: list D.exp {dexceptions << de})
  (djust: D.exp{djust << de})
  (dcons: D.exp{dcons << de})
  (dtau: D.ty)
  (rec_lemma: rec_correctness_step_type de)
    : Pure (nat & typed_l_exp (translate_ty dtau) & nat)
      (requires (
        Some? (D.step de) /\
        de == D.EDefault dexceptions djust dcons dtau /\
        D.typing D.empty de dtau /\
        (match D.step de, D.step_exceptions de dexceptions djust dcons dtau with
         | Some de', D.SomeStep de'' -> de' == de''
         | _ -> False)
      ))
      (ensures (fun (n1, target_e, n2) ->
      translation_preserves_empty_typ de dtau;
      let lexceptions = translate_exp_list dexceptions in
      let ljust = translate_exp djust in
      let lcons = translate_exp dcons in
      let D.SomeStep de' = D.step_exceptions de dexceptions djust dcons dtau in
      let le' = translate_exp de' in
      D.preservation de dtau;
      let ltau = translate_ty dtau in
      translation_preserves_empty_typ de' dtau;
      take_l_steps ltau (build_default_translation lexceptions L.ENone ljust lcons ltau) n1
        == Some target_e /\
      take_l_steps ltau le' n2 == Some target_e
      ))
  =
  if List.Tot.for_all D.is_value dexceptions then begin
    let n1, target_e = translation_correctness_exceptions_empty_count_exception_triggered
      de dexceptions djust dcons dtau D.AllEmpty L.ENone rec_lemma
    in
    n1, target_e, 0
  end else
    translation_correctness_exceptions_left_to_right_step
      de dexceptions djust dcons dtau L.ENone rec_lemma
#pop-options

let final_default_subexp
  (tau: L.ty)
  (just: typed_l_exp L.TBool) (cons: typed_l_exp tau)
    : Tot (typed_l_exp tau)
  =
  L.EIf just cons (L.ELit (L.LError L.EmptyError))

let rec empty_count_non_all_empty_if_one
  (e: D.exp)
  (l: list D.exp)
    : Lemma (D.empty_count (D.OneNonEmpty e) l <> D.AllEmpty)
  =
  match l with
  | [] -> ()
  | hd::tl -> begin
    match hd with
    | D.ELit D.LEmptyError -> empty_count_non_all_empty_if_one e tl
    | _ -> ()
  end

#push-options "--fuel 2 --ifuel 1 --z3rlimit 150"
let rec translation_correctness_exceptions_no_exceptions_triggered
  (de: D.exp)
  (dexceptions: list D.exp {dexceptions << de})
  (djust: D.exp{djust << de})
  (dcons: D.exp{dcons << de})
  (dtau: D.ty)
    : Pure nat
      (requires (
        D.typing D.empty de dtau /\
        D.typing_list D.empty dexceptions dtau /\
        D.typing D.empty djust D.TBool /\
        D.typing D.empty dcons dtau /\
        List.Tot.for_all D.is_value dexceptions /\
        (D.step_exceptions de dexceptions djust dcons dtau == D.NoStep)
      ))
      (ensures (fun n ->
        translate_empty_is_empty ();
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        translation_preserves_empty_typ djust D.TBool;
        translation_preserves_empty_typ dcons dtau;
        let lexceptions = translate_exp_list dexceptions in
        let ljust = translate_exp djust in
        let lcons = translate_exp dcons in
        let ltau = translate_ty dtau in
        let l_err : typed_l_exp ltau = L.ELit (L.LError L.ConflictError) in
        build_default_translation_typing lexceptions L.ENone ljust lcons ltau L.empty;
        take_l_steps ltau (build_default_translation lexceptions L.ENone ljust lcons ltau) n
          == Some (final_default_subexp ltau ljust lcons)
      ))
      (decreases dexceptions)
  =
  translate_empty_is_empty ();
  translation_preserves_typ_exceptions D.empty de dexceptions dtau;
  translation_preserves_empty_typ djust D.TBool;
  translation_preserves_empty_typ dcons dtau;
  let lexceptions = translate_exp_list dexceptions in
  let ljust = translate_exp djust in
  let lcons = translate_exp dcons in
  let ltau = translate_ty dtau in
  match dexceptions with
  | [] ->
    let n1 = step_exceptions_empty_none ltau ljust lcons in n1
  | dhd::dtl ->
    assert(D.is_value dhd);
    assert(dhd <> D.ELit D.LConflictError);
    let aux (_ : squash (dhd <> D.ELit D.LEmptyError)) : Lemma (False) =
      assert(D.empty_count D.AllEmpty dexceptions == D.empty_count (D.OneNonEmpty dhd) dtl);
      empty_count_non_all_empty_if_one dhd dtl
    in
    Classical.impl_intro aux;
    translation_preserves_typ_exceptions D.empty de dtl dtau;
    translation_preserves_empty_typ dhd dtau;
    translate_list_is_value_list dtl;
    let ltl = translate_exp_list dtl in
    let lhd = translate_exp dhd in
    let lemp = L.ELit (L.LError L.EmptyError) in
    assert(lhd == lemp);
    lift_multiple_l_steps_exceptions_head ltau ltl L.ENone ljust lcons 0 lemp lemp;
    build_default_translation_typing lexceptions L.ENone ljust lcons ltau L.empty;
    assert(take_l_steps ltau (build_default_translation lexceptions L.ENone ljust lcons ltau) 4 ==
      Some (exceptions_head_lift ltau ltl L.ENone ljust lcons lemp));
    let _, n = step_exceptions_head_value ltau ltl L.ENone ljust lcons lemp in
    step_exceptions_head_value_go_through ltau ltl ljust lcons;
    take_l_steps_transitive ltau
      (build_default_translation lexceptions L.ENone ljust lcons ltau)
      (exceptions_head_lift ltau ltl L.ENone ljust lcons lemp)
      4 n;
    assert(take_l_steps ltau (build_default_translation lexceptions L.ENone ljust lcons ltau)
      (4 + n) == Some (build_default_translation ltl L.ENone ljust lcons ltau));
    let n' = translation_correctness_exceptions_no_exceptions_triggered de dtl djust dcons dtau in
    take_l_steps_transitive ltau
      (build_default_translation lexceptions L.ENone ljust lcons ltau)
      (build_default_translation ltl L.ENone ljust lcons ltau)
      (4 + n) n';
    4 + n + n'
#pop-options

#push-options "--fuel 1 --ifuel 1 --z3rlimit 50"
let rec step_exceptions_left_to_right_does_not_depend_on_condition
  (de de': D.exp) (dexceptions: list D.exp{dexceptions << de /\ dexceptions << de'})
  (djust: D.exp{djust << de}) (djust': D.exp{djust' << de'})
  (dcons: D.exp{dcons << de /\ dcons << de'})
  (dtau: D.ty)
    : Lemma (
      match
        D.step_exceptions_left_to_right de dexceptions djust dcons dtau,
        D.step_exceptions_left_to_right de' dexceptions djust' dcons dtau
      with
      | None, None | Some _, Some _ -> True
      | _ -> False
    )
  =
  let e1 = D.step_exceptions_left_to_right de dexceptions djust dcons dtau in
  let e2 = D.step_exceptions_left_to_right de' dexceptions djust' dcons dtau in
  match dexceptions with
  | [] ->
    assert(e1 == None);
    assert(e2 == None)
  | [hd] -> begin
    match D.step hd with
    | Some (D.ELit D.LConflictError) ->
      assert(e1 == Some D.c_err);
      assert(e2 == Some D.c_err)
    | Some hd ->
      assert(e1 == Some ((D.EDefault ([hd]) djust dcons dtau)));
      assert(e2 == Some ((D.EDefault ([hd]) djust' dcons dtau)))
    | _ ->
      assert(e1 == None);
      assert(e2 == None)
  end
  | hd::tl ->
    if D.is_value hd then begin
      step_exceptions_left_to_right_does_not_depend_on_condition
        de de' tl djust djust' dcons dtau;
      match
        D.step_exceptions_left_to_right de tl djust dcons dtau,
        D.step_exceptions_left_to_right de' tl djust' dcons dtau
      with
      | Some _, Some _ -> ()
      | None, None -> ()
    end else begin
      match D.step hd with
      | Some (D.ELit D.LConflictError) ->
        assert(e1 == Some D.c_err);
        assert(e2 == Some D.c_err)
      | Some hd ->
        assert(e1 == Some ((D.EDefault (hd::tl) djust dcons dtau)));
        assert(e2 == Some ((D.EDefault (hd::tl) djust' dcons dtau)))
      | _ ->
        assert(e1 == None);
        assert(e2 == None)
    end
#pop-options

#push-options "--fuel 1 --ifuel 1"
let step_exceptions_does_not_depend_on_condition
  (de de': D.exp) (dexceptions: list D.exp{dexceptions << de /\ dexceptions << de'})
  (djust: D.exp{djust << de}) (djust': D.exp{djust' << de'})
  (dcons: D.exp{dcons << de /\ dcons << de'})
  (dtau: D.ty)
    : Lemma (
      match
        D.step_exceptions de dexceptions djust dcons dtau,
        D.step_exceptions de' dexceptions djust' dcons dtau
      with
      | D.NoStep, D.NoStep | D.IllFormed, D.IllFormed | D.SomeStep _, D.SomeStep _ -> True
      | _ -> False
    )
 =
 if List.Tot.for_all D.is_value dexceptions then begin
   match D.empty_count D.AllEmpty dexceptions with
   | D.AllEmpty -> ()
   | D.OneNonEmpty _ -> ()
   | D.Conflict -> ()
 end else begin
   step_exceptions_left_to_right_does_not_depend_on_condition
     de de' dexceptions djust djust' dcons dtau;
   match
     D.step_exceptions_left_to_right de dexceptions djust dcons dtau,
     D.step_exceptions_left_to_right de' dexceptions djust' dcons dtau
   with
   | None, None -> ()
   | Some _, Some _ -> ()
end
#pop-options

#push-options "--fuel 2 --ifuel 1 --z3rlimit 50"
let rec translation_correctness_step (de: D.exp) (dtau: D.ty)
    : Pure (nat & typed_l_exp (translate_ty dtau) & nat)
      (requires (Some? (D.step de) /\ D.typing D.empty de dtau))
      (ensures (fun (n1, target_e, n2) ->
        translation_preserves_empty_typ de dtau;
        let de' = Some?.v (D.step de) in
        D.preservation de dtau;
        translation_preserves_empty_typ de' dtau;
        take_l_steps (translate_ty dtau) (translate_exp de) n1 == Some target_e /\
        take_l_steps (translate_ty dtau) (translate_exp de') n2 == Some target_e
       ))
      (decreases de)
  =
  let de' = Some?.v (D.step de) in
  translation_preserves_empty_typ de dtau;
  D.preservation de dtau;
  translation_preserves_empty_typ de' dtau;
  let ltau = translate_ty dtau in
  let le : typed_l_exp ltau = translate_exp de in
  let le' : typed_l_exp ltau = translate_exp de' in
  match de with
  | D.EVar _ -> 0, le, 0
  | D.ELit _ -> 0, le, 0
  | D.EAbs _ _ -> 0, le, 0
  | D.EIf de1 de2 de3 ->
    let le1 = translate_exp de1 in
    let le2 = translate_exp de2 in
    let le3 = translate_exp de3 in
    if not (D.is_value de1) then begin
      let de1' = Some?.v (D.step de1) in
      D.preservation de1 D.TBool;
      translation_preserves_empty_typ de1 D.TBool;
      translation_preserves_empty_typ de2 dtau;
      translation_preserves_empty_typ de3 dtau;
      translation_preserves_empty_typ de1' D.TBool;
      let le1' : typed_l_exp L.TBool = translate_exp de1' in
      let n1_e1, target_e1, n2_e1 = translation_correctness_step de1 D.TBool in
      assert(take_l_steps L.TBool le1 n1_e1 == Some target_e1);
      assert(take_l_steps L.TBool le1' n2_e1 == Some target_e1);
      lift_multiple_l_steps L.TBool ltau le1 target_e1 n1_e1
        (if_cond_lift ltau le2 le3);
      lift_multiple_l_steps L.TBool ltau le1' target_e1 n2_e1
        (if_cond_lift ltau le2 le3);
      n1_e1, if_cond_lift ltau le2 le3 target_e1, n2_e1
    end else (1, le', 0)
  | D.EApp de1 de2 dtau_arg ->
    let le1 = translate_exp de1 in
    let le2 = translate_exp de2 in
    let ltau_arg = translate_ty dtau_arg in
    if not (D.is_value de1) then begin
      let de1' = Some?.v (D.step de1) in
      let le1' = translate_exp de1' in
      let n1_e1, target_e1, n2_e1 = translation_correctness_step de1 (D.TArrow dtau_arg dtau) in
      assert(take_l_steps (L.TArrow ltau_arg ltau) le1 n1_e1 == Some target_e1);
      assert(take_l_steps (L.TArrow ltau_arg ltau) le1' n2_e1 == Some target_e1);
      lift_multiple_l_steps (L.TArrow ltau_arg ltau) ltau le1 target_e1 n1_e1
        (app_f_lift ltau_arg ltau le2);
      lift_multiple_l_steps (L.TArrow ltau_arg ltau) ltau le1' target_e1 n2_e1
        (app_f_lift ltau_arg ltau le2);
      n1_e1, app_f_lift ltau_arg ltau le2 target_e1, n2_e1
    end else begin match de1 with
      | D.ELit D.LConflictError -> 1, le', 0
      | D.ELit D.LEmptyError -> 1, le', 0
      | _ ->
        if not (D.is_value de2) then begin
        let de2' = Some?.v (D.step de2) in
        let le2' = translate_exp de2' in
        let n1_e2, target_e2, n2_e2 = translation_correctness_step de2 dtau_arg in
        lift_multiple_l_steps ltau_arg ltau le2 target_e2 n1_e2
          (app_arg_lift ltau_arg ltau le1);
        lift_multiple_l_steps ltau_arg ltau le2' target_e2 n2_e2
          (app_arg_lift ltau_arg ltau le1);
        n1_e2, app_arg_lift ltau_arg ltau le1 target_e2, n2_e2
      end else begin
        match de1, de2 with
        | _, D.ELit D.LConflictError -> 1, le', 0
        | _, D.ELit D.LEmptyError -> 1, le', 0
        | D.EAbs dt1 dbody, _ ->
          substitution_correctness (D.var_to_exp_beta de2) dbody;
          1, le', 0
      end
    end
  | D.EDefault dexceptions djust dcons dtau' ->
    if dtau' <> dtau then 0, le', 0 else begin
    match D.step_exceptions de dexceptions djust dcons dtau with
    | D.SomeStep _ ->
      translation_correctness_exceptions_step de dexceptions djust dcons dtau
        (fun df tf -> translation_correctness_step df tf)
    | D.NoStep ->
      let n1_final =
        translation_correctness_exceptions_no_exceptions_triggered
          de dexceptions djust dcons dtau
      in
      let ltau = translate_ty dtau in
      let ljust = translate_exp djust in
      let lcons = translate_exp dcons in
      let l_err = L.ELit (L.LError L.EmptyError) in
      assert(take_l_steps ltau le n1_final = Some (final_default_subexp ltau ljust lcons));
      if not (D.is_value djust) then begin
       let djust' = Some?.v (D.step djust) in
       D.preservation djust D.TBool;
       translation_preserves_empty_typ djust D.TBool;
       translation_preserves_empty_typ dcons dtau;
       translation_preserves_empty_typ djust' D.TBool;
       let ljust' : typed_l_exp L.TBool = translate_exp djust' in
       let n1_just, target_just, n2_just = translation_correctness_step djust D.TBool in
       assert(take_l_steps L.TBool ljust n1_just == Some target_just);
       assert(take_l_steps L.TBool ljust' n2_just == Some target_just);
       lift_multiple_l_steps L.TBool ltau ljust target_just n1_just
         (if_cond_lift ltau lcons l_err);
       lift_multiple_l_steps L.TBool ltau ljust' target_just n2_just
         (if_cond_lift ltau lcons l_err);
       take_l_steps_transitive ltau le (final_default_subexp ltau ljust lcons) n1_final n1_just;
       assert(take_l_steps ltau le (n1_final + n1_just) == Some (L.EIf target_just lcons l_err));
       assert(de' == D.EDefault dexceptions djust' dcons dtau);
       exceptions_smaller dexceptions djust dcons dtau;
       exceptions_smaller dexceptions djust' dcons dtau;
       step_exceptions_does_not_depend_on_condition
        de de' dexceptions djust djust' dcons dtau;
       let n2_final =
        translation_correctness_exceptions_no_exceptions_triggered
          de' dexceptions djust' dcons dtau
       in
       take_l_steps_transitive ltau le' (final_default_subexp ltau ljust' lcons) n2_final n2_just;
       n1_final + n1_just, L.EIf target_just lcons l_err, n2_final + n2_just
     end else begin
       D.is_bool_value_cannot_be_abs D.empty djust;
       match djust with
       | D.ELit D.LTrue ->
         take_l_steps_transitive ltau le (final_default_subexp ltau ljust lcons) n1_final 1;
         n1_final + 1, lcons, 0
       | D.ELit D.LFalse ->
         take_l_steps_transitive ltau le (final_default_subexp ltau ljust lcons) n1_final 1;
         n1_final + 1, l_err, 0
       | D.ELit D.LConflictError ->
         take_l_steps_transitive ltau le (final_default_subexp ltau ljust lcons) n1_final 1;
         n1_final + 1, L.ELit (L.LError L.ConflictError), 0
       | D.ELit D.LEmptyError ->
         take_l_steps_transitive ltau le (final_default_subexp ltau ljust lcons) n1_final 1;
         n1_final + 1, l_err, 0
     end
  end

(*** Wrap-up theorem  *)

let translation_correctness (de: D.exp) (dtau: D.ty)
    : Lemma
      (requires (D.typing D.empty de dtau))
      (ensures (
        let le = translate_exp de in
        let ltau = translate_ty dtau in
        L.typing L.empty le ltau /\ begin
          if D.is_value de then L.is_value le else begin
            D.progress de dtau;
            D.preservation de dtau;
            let de' = Some?.v (D.step de) in
            translation_preserves_empty_typ de dtau;
            translation_preserves_empty_typ de' dtau;
            let le' : typed_l_exp ltau = translate_exp de' in
            exists (n1 n2:nat) (target: typed_l_exp ltau).
              (take_l_steps ltau le n1 == Some target /\
               take_l_steps ltau le' n2 == Some target)
          end
        end
      ))
 =
 translation_preserves_empty_typ de dtau;
 if D.is_value de then translation_correctness_value de else begin
    D.progress de dtau;
    let n1, target, n2 = translation_correctness_step de dtau in
    ()
 end
