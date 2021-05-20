module Catala.Translation.Helpers

open Catala.LambdaCalculus
module T = FStar.Tactics

(*** Default translation *)

let process_exceptions_f (tau: ty) : Tot exp =
  EAbs (TOption tau) (EAbs (TArrow TUnit tau) (
    EApp (EAbs (TOption tau) (
      EMatchOption (EVar 2) tau
        (EVar 0)
        (EAbs tau (
          EMatchOption (EVar 1) tau
            (EVar 3)
            (EAbs tau (ELit (LError ConflictError)))
        ))
    ))
    (ECatchEmptyError (ESome (EApp (EVar 0) (ELit LUnit) TUnit)) ENone)
    (TOption tau)
  ))

let typ_process_exceptions_f (g: env) (tau: ty)
    : Lemma (typing g (process_exceptions_f tau)
      (TArrow (TOption tau) (TArrow (TArrow TUnit tau) (TOption tau))))
  =
  assert_norm(typing g (process_exceptions_f tau)
      (TArrow (TOption tau) (TArrow (TArrow TUnit tau) (TOption tau))))

let build_default_translation
  (exceptions: list exp)
  (acc: exp)
  (just: exp)
  (cons: exp)
  (tau: ty)
  =
  EMatchOption
    (EFoldLeft
      (process_exceptions_f tau)
      acc (TOption tau)
      (EList exceptions) (TArrow TUnit tau))
    tau
    (EIf
      just cons
      (ELit (LError EmptyError)))
    (EAbs tau (EVar 0))

#push-options "--fuel 1 --ifuel 0"
let build_default_translation_typing
  (exceptions: list exp)
  (acc: exp)
  (just: exp)
  (cons: exp)
  (tau: ty)
  (g: env)
    : Lemma
      (requires (
        typing_list g exceptions (TArrow TUnit tau) /\
        typing g acc (TOption tau) /\
        typing g just TBool /\
        typing g cons tau))
      (ensures (typing g (build_default_translation exceptions acc just cons tau) tau))
  =
  typ_process_exceptions_f g tau;
  assert_norm(typing g (build_default_translation exceptions acc just cons tau) tau)
#pop-options

#push-options "--fuel 9 --ifuel 0"
let process_exceptions_untouched_by_subst (s: var_to_exp) (tau: ty) : Lemma
    (subst s (process_exceptions_f tau) == process_exceptions_f tau)
  =
  ()
#pop-options

(*** Lambda calculus stepping lemma *)

let well_typed_terms_invariant_by_subst (s: var_to_exp) (e: exp) (tau: ty) : Lemma
  (requires (typing empty e tau))
  (ensures (subst s e == e))
  =
  (* Classic lambda calculus result for terms that do not contain free variables *)
  admit()

(*** Step lifting framework *)

let typed_l_exp (tau: ty) = e:exp{typing empty e tau}

let rec take_l_steps (tau: ty) (e: typed_l_exp tau) (fuel: nat)
    : Tot (option (typed_l_exp tau))
      (decreases fuel) =
  if fuel = 0 then Some e else
  match step e with
  | None -> None
  | Some e' ->
    preservation e tau;
    take_l_steps tau e' (fuel - 1)

#push-options "--fuel 2 --ifuel 1"
let rec take_l_steps_transitive (tau: ty) (e1 e2: typed_l_exp tau) (n1 n2: nat)
    : Lemma
      (requires (take_l_steps tau e1 n1 == Some e2))
      (ensures (take_l_steps tau e1 (n1 + n2) == take_l_steps tau e2 n2))
      (decreases n1)
  =
  if n1 = 0 then () else begin
    match step e1 with
      | None -> ()
      | Some e1' ->
        preservation e1 tau;
        take_l_steps_transitive tau e1' e2 (n1 - 1) n2
  end
#pop-options

let not_l_value (tau: ty) = e:exp{not (is_value e) /\ typing empty e tau}
let l_value (tau: ty) = e:exp{is_value e /\ typing empty e tau}

let stepping_context (tau tau': ty) = typed_l_exp tau -> not_l_value tau'

let step_lift_commute_non_value
  (tau tau': ty)
  (f: stepping_context tau tau')
  (e: typed_l_exp tau)
    : prop
  =
  progress e tau;
  if is_value e then true else begin
    preservation e tau;
    step (f e) == Some (f (Some?.v (step e)))
  end

let is_stepping_agnostic_lift
  (tau tau': ty)
  (f:stepping_context tau tau')
    : prop
  =
  forall (e: typed_l_exp tau). step_lift_commute_non_value tau tau' f e

let stepping_agnostic_lift
  (tau tau': ty)
  : Type
  = f:(stepping_context tau tau'){is_stepping_agnostic_lift tau tau' f}

let rec l_values_dont_step (e: exp) : Lemma
    (requires (is_value e))
    (ensures (step e = None))
    (decreases %[e; 1])
  =
  match e with
  | EAbs _ _ -> ()
  | EThunk _ -> ()
  | ELit _ -> ()
  | ENone -> ()
  | ESome e' -> l_values_dont_step e'
  | EList [] -> ()
  | EList l -> l_values_dont_step_list e l
  | _ -> ()
and l_values_dont_step_list (e: exp) (l: list exp{l << e /\ Cons? l}) : Lemma
    (requires (is_value_list l))
    (ensures (step_list e l = Bad))
    (decreases %[e; 0; l])
  =
  match l with
  | [hd] -> l_values_dont_step hd
  | hd::tl ->
    l_values_dont_step hd;
    l_values_dont_step_list e tl

#push-options "--z3rlimit 50 --fuel 2 --ifuel 1"
let rec lift_multiple_l_steps
  (tau tau': ty)
  (e1: typed_l_exp tau)
  (e2: typed_l_exp tau)
  (n: nat)
  (f : stepping_agnostic_lift tau tau')
    : Lemma
      (requires (take_l_steps tau e1 n == Some e2))
      (ensures (take_l_steps tau' (f e1) n == Some (f e2)))
      (decreases n)
  =
  match step e1 with
  | None -> ()
  | Some e1' ->
    progress e1 tau;
    preservation e1 tau;
    if is_value e1 then
      l_values_dont_step e1
    else if n = 0 then
      ()
    else
      lift_multiple_l_steps tau tau' e1' e2 (n-1) f
#pop-options


(*** Lifts *)

let if_cond_lift'
  (tau: ty)
  (e2 e3: typed_l_exp tau)
    : stepping_context TBool tau
  =
  fun e1 -> EIf e1 e2 e3

let if_cond_lift_is_stepping_agnostic
  (tau: ty)
  (e2 e3: typed_l_exp tau)
  (e: typed_l_exp TBool)
    : Lemma
      (requires (True))
      (ensures (step_lift_commute_non_value TBool tau (if_cond_lift' tau e2 e3) e))
  =
  progress e TBool; if is_value e then () else preservation e TBool

let if_cond_lift
  (tau: ty)
  (e2 e3: typed_l_exp tau)
    : stepping_agnostic_lift TBool tau
  =
  Classical.forall_intro (if_cond_lift_is_stepping_agnostic tau e2 e3);
  if_cond_lift' tau e2 e3


let app_f_lift'
  (tau_arg tau: ty)
  (e2: typed_l_exp tau_arg)
    : stepping_context (TArrow tau_arg tau) tau
  =
  fun e1 -> EApp e1 e2 tau_arg

let app_f_lift_is_stepping_agnostic
  (tau_arg tau: ty)
  (e2: typed_l_exp tau_arg)
  (e: typed_l_exp (TArrow tau_arg tau))
    : Lemma
      (requires (True))
      (ensures (
        step_lift_commute_non_value (TArrow tau_arg tau) tau (app_f_lift' tau_arg tau e2) e))
  =
  progress e  (TArrow tau_arg tau);
  if is_value e then () else preservation e  (TArrow tau_arg tau)

let app_f_lift
  (tau_arg tau: ty)
  (e2: typed_l_exp tau_arg)
    : stepping_agnostic_lift (TArrow tau_arg tau) tau
  =
  Classical.forall_intro (app_f_lift_is_stepping_agnostic tau_arg tau e2);
  app_f_lift' tau_arg tau e2

let app_arg_lift'
  (tau_arg tau: ty)
  (e1: l_value (TArrow tau_arg tau))
    : stepping_context tau_arg tau
  =
  fun e2 -> EApp e1 e2 tau_arg

let app_arg_lift_is_stepping_agnostic
  (tau_arg tau: ty)
  (e1: l_value (TArrow tau_arg tau){match e1 with ELit (LError _) -> False | _ -> True})
  (e2: typed_l_exp tau_arg)
    : Lemma
      (requires (True))
      (ensures (
        step_lift_commute_non_value tau_arg tau (app_arg_lift' tau_arg tau e1) e2))
  =
  progress e2 tau_arg;
  if is_value e2 then () else preservation e2 tau_arg

let app_arg_lift
  (tau_arg tau: ty)
  (e1: l_value (TArrow tau_arg tau){match e1 with ELit (LError _) -> False | _ -> True})
    : stepping_agnostic_lift tau_arg tau
  =
  Classical.forall_intro (app_arg_lift_is_stepping_agnostic tau_arg tau e1);
  app_arg_lift' tau_arg tau e1

#push-options "--fuel 9 --ifuel 2 --z3rlimit 30"
let exceptions_head_lift'
  (tau: ty)
  (tl: list exp{typing_list empty tl (TArrow TUnit tau)})
  (acc: typed_l_exp (TOption tau))
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  : stepping_context tau tau
  =
  fun (hd: typed_l_exp tau) ->
    typ_process_exceptions_f empty tau;
    typing_empty_can_be_extended acc (TOption tau) (extend empty (TOption tau));
    typing_empty_can_be_extended acc (TOption tau)
      (extend (extend empty (TOption tau)) tau);
    EMatchOption
    (EFoldLeft
      (process_exceptions_f tau)
      (EApp
        (EAbs (TOption tau) (
          EMatchOption acc tau
            (EVar 0)
            (EAbs tau (
              EMatchOption (EVar 1) tau
                acc
                (EAbs tau (ELit (LError ConflictError)))
            ))
         ))
         (ECatchEmptyError
           (ESome hd) ENone)
         (TOption tau)
      )
      (TOption tau)
      (EList tl) (TArrow TUnit tau))
    tau
    (EIf
      just cons
      (ELit (LError EmptyError)))
    (EAbs tau (EVar 0))
#pop-options

let exceptions_head_lift_is_stepping_agnostic
  (tau: ty)
  (tl: list exp{typing_list empty tl (TArrow TUnit tau)})
  (acc: typed_l_exp (TOption tau))
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  (hd: typed_l_exp tau)
    : Lemma (step_lift_commute_non_value tau tau (exceptions_head_lift' tau tl acc just cons) hd)
  =
  progress hd tau;
  if is_value hd then () else begin
    preservation hd tau;
    assert_norm(step (exceptions_head_lift' tau tl acc just cons hd) == Some
      (exceptions_head_lift' tau tl acc just cons (Some?.v (step hd))))
  end

let exceptions_head_lift
  (tau: ty)
  (tl: list exp{typing_list empty tl (TArrow TUnit tau)})
  (acc: typed_l_exp (TOption tau))
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  : stepping_agnostic_lift tau tau
  =
  Classical.forall_intro (exceptions_head_lift_is_stepping_agnostic tau tl acc just cons);
  exceptions_head_lift' tau tl acc just cons

#push-options "--fuel 3 --ifuel 0"
let exceptions_init_lift'
  (tau: ty)
  (tl: list exp{typing_list empty tl (TArrow TUnit tau)})
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  : stepping_context (TOption tau) tau
  =
  fun (init: typed_l_exp (TOption tau)) ->
    typ_process_exceptions_f empty tau;
    EMatchOption
    (EFoldLeft
      (process_exceptions_f tau)
      init
      (TOption tau)
      (EList tl) (TArrow TUnit tau))
    tau
    (EIf
      just cons
      (ELit (LError EmptyError)))
    (EAbs tau (EVar 0))
#pop-options

let exceptions_init_lift_is_stepping_agnostic
  (tau: ty)
  (tl: list exp{typing_list empty tl (TArrow TUnit tau)})
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  (init: typed_l_exp (TOption tau))
    : Lemma (
      step_lift_commute_non_value (TOption tau) tau (exceptions_init_lift' tau tl just cons) init)
  =
  progress init (TOption tau);
  if is_value init then () else begin
    preservation init (TOption tau);
    assert_norm(step (exceptions_init_lift' tau tl just cons init) == Some
      (exceptions_init_lift' tau tl just cons (Some?.v (step init))))
  end

let exceptions_init_lift
  (tau: ty)
  (tl: list exp{typing_list empty tl (TArrow TUnit tau)})
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  : stepping_agnostic_lift (TOption tau) tau
  =
  Classical.forall_intro (exceptions_init_lift_is_stepping_agnostic tau tl just cons);
  exceptions_init_lift' tau tl just cons

#push-options "--fuel 10 --ifuel 2 --z3rlimit 80"
let stepping_lemma
  (tau: ty)
  (acc: typed_l_exp (TOption tau){is_value acc /\ not (is_error acc)})
  (hd: typed_l_exp tau)
    : Lemma (
      typ_process_exceptions_f empty tau;
      typing_empty_can_be_extended acc (TOption tau) (extend empty (TOption tau));
      typing_empty_can_be_extended acc (TOption tau)
        (extend (extend empty (TOption tau)) tau);
      let e_final =
        EApp (EAbs (TOption tau) (EMatchOption acc tau (EVar 0)
            (EAbs tau (EMatchOption (EVar 1) tau acc (EAbs tau (ELit (LError ConflictError)))))))
            (ECatchEmptyError (ESome (EApp (EThunk hd) (ELit LUnit) TUnit)) ENone) (TOption tau)
      in
      take_l_steps (TOption tau) (EApp
        (EApp (process_exceptions_f tau) acc (TOption tau)) (EThunk hd) (TArrow TUnit tau)) 2 ==
          Some e_final)
  =
  typ_process_exceptions_f empty tau;
  typing_empty_can_be_extended acc (TOption tau) (extend empty (TOption tau));
  typing_empty_can_be_extended acc (TOption tau)
    (extend (extend empty (TOption tau)) tau);
  let e0 =
    EApp (EApp (process_exceptions_f tau) acc (TOption tau)) (EThunk hd) (TArrow TUnit tau)
  in
  let e0' =
    (EApp (subst (var_to_exp_beta acc) (EAbs (TArrow (TUnit) tau) (EApp (EAbs (TOption tau)
       (EMatchOption (EVar 2) tau (EVar 0) (EAbs tau (EMatchOption (EVar 1) tau (EVar 3)
         (EAbs tau (ELit (LError (ConflictError))))))))
         (ECatchEmptyError (ESome (EApp (EVar 0) (ELit (LUnit)) (TUnit))) (ENone))
    (TOption tau)))) (EThunk hd) (TArrow (TUnit) tau))
  in
  let e1 =
    EApp (
      EAbs (TArrow TUnit tau) (
      EApp (EAbs (TOption tau) (
        EMatchOption acc tau
          (EVar 0)
          (EAbs tau (
            EMatchOption (EVar 1) tau
              acc
              (EAbs tau (ELit (LError ConflictError)))
          ))
      ))
      (ECatchEmptyError (ESome (EApp (EVar 0) (ELit LUnit) TUnit)) ENone)
      (TOption tau)
    )) (EThunk hd) (TArrow TUnit tau)
  in
  let e2 =
    EApp (EAbs (TOption tau) (EMatchOption acc tau (EVar 0)
     (EAbs tau (EMatchOption (EVar 1) tau acc (EAbs tau (ELit (LError ConflictError)))))))
     (ECatchEmptyError (ESome (EApp (EThunk hd) (ELit LUnit) TUnit)) ENone) (TOption tau)
  in
  let acc_invariant (s: var_to_exp) : Lemma (subst s acc == acc) [SMTPat (subst s acc)] =
    well_typed_terms_invariant_by_subst s acc (TOption tau)
  in
  assert(step e0 == Some e0') by (
    T.norm [delta_only [
      "Catala.Translation.Helpers.process_exceptions_f";
      "Catala.LambdaCalculus.step";
      "Catala.LambdaCalculus.step_app";
      "Catala.LambdaCalculus.is_value";
    ]; zeta; iota];
    T.smt ()
  );
  assert(e0' == e1);
  assert(step e1 == Some e2);
  preservation e0 (TOption tau);
  preservation e1 (TOption tau);
  take_l_steps_transitive (TOption tau) e0 e1 1 1
#pop-options

#push-options "--fuel 7 --ifuel 2 --z3rlimit 80"
let lift_multiple_l_steps_exceptions_head
  (tau: ty)
  (tl: list exp{typing_list empty tl (TArrow TUnit tau) /\ is_value_list tl})
  (acc: typed_l_exp (TOption tau))
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  (n_hd: nat)
  (hd: typed_l_exp tau)
  (final_hd: typed_l_exp tau)
    : Lemma
      (requires (take_l_steps tau hd n_hd == Some final_hd /\ is_value acc /\ not (is_error acc)))
      (ensures (
        build_default_translation_typing
          ((EThunk hd)::tl) acc just cons tau empty;
        take_l_steps tau
          (build_default_translation ((EThunk hd)::tl) acc just cons tau)
          (n_hd + 4) ==
            Some (exceptions_head_lift tau tl acc just cons final_hd)))
  =
  build_default_translation_typing ((EThunk hd)::tl) acc just cons tau empty;
  typing_empty_can_be_extended acc (TOption tau) (extend empty (TOption tau));
  typing_empty_can_be_extended acc (TOption tau)
   (extend (extend empty (TOption tau)) tau);
  typ_process_exceptions_f empty tau;
  let init0 : typed_l_exp (TOption tau) = EApp
    (EApp (process_exceptions_f tau) acc (TOption tau))
    (EThunk hd) (TArrow TUnit tau)
  in
  let init2 : typed_l_exp (TOption tau) = EApp
    (EAbs (TOption tau) (
      EMatchOption acc tau (EVar 0) (EAbs tau (EMatchOption (EVar 1) tau acc (EAbs tau
        (ELit (LError ConflictError)))))))
    (ECatchEmptyError (ESome (EApp (EThunk hd) (ELit LUnit) TUnit)) ENone) (TOption tau)
  in
  let init3 = EApp (EAbs (TOption tau) (
    EMatchOption acc tau (EVar 0) (EAbs tau (
      EMatchOption (EVar 1) tau acc (EAbs tau
        (ELit (LError ConflictError))
      )
    ))))
    (ECatchEmptyError (ESome hd) ENone) (TOption tau)
  in
  stepping_lemma tau acc hd;
  assert(take_l_steps (TOption tau) init0 2 == Some init2);
  assert(step init2 == Some init3);
  preservation init2 (TOption tau);
  take_l_steps_transitive (TOption tau) init0 init2 2 1;
  let default_translation0 : typed_l_exp tau =
    build_default_translation ((EThunk hd)::tl) acc just cons tau
  in
  let default_translation1 = EMatchOption
    (EFoldLeft
      (process_exceptions_f tau)
      init0 (TOption tau)
      (EList tl) (TArrow TUnit tau))
    tau
    (EIf
      just cons
      (ELit (LError EmptyError)))
    (EAbs tau (EVar 0))
  in
  preservation default_translation0 tau;
  assert(take_l_steps tau default_translation0 1 == Some default_translation1);
  assert(default_translation1 == exceptions_init_lift tau tl just cons init0);
  lift_multiple_l_steps (TOption tau) tau init0 init3 3
    (exceptions_init_lift tau tl just cons);
  take_l_steps_transitive tau default_translation0 default_translation1 1 3;
  assert(take_l_steps tau default_translation0 4 ==
    Some (exceptions_init_lift tau tl just cons init3));
  assert(exceptions_init_lift tau tl just cons init3 ==
    exceptions_head_lift tau tl acc just cons hd);
  lift_multiple_l_steps tau tau hd final_hd n_hd (exceptions_head_lift tau tl acc just cons);
  take_l_steps_transitive tau default_translation0
    (exceptions_head_lift tau tl acc just cons hd) 4 n_hd

(*** Various lambda calculus steps *)

let process_exceptions_applied
  (tau: ty)
  (acc: typed_l_exp (TOption tau))
  (hd: typed_l_exp tau)
    : Tot (typed_l_exp (TOption tau))
  =
  typ_process_exceptions_f empty tau;
  typing_empty_can_be_extended hd tau (extend empty TUnit);
  EApp
   (EApp (process_exceptions_f tau) acc (TOption tau))
   (EThunk hd) (TArrow TUnit tau)


#push-options "--fuel 7 --ifuel 2"
let process_exceptions_applied_stepped
  (tau: ty)
  (acc: typed_l_exp (TOption tau))
  (hd: typed_l_exp tau)
    : Tot (typed_l_exp (TOption tau))
  =
  typing_empty_can_be_extended acc (TOption tau) (extend empty (TOption tau));
  typing_empty_can_be_extended acc (TOption tau)
   (extend (extend empty (TOption tau)) tau);
  EApp (EAbs (TOption tau) (
    EMatchOption acc tau (EVar 0) (EAbs tau (
      EMatchOption (EVar 1) tau acc (EAbs  tau
        (ELit (LError ConflictError))
      )
    ))))
    (ECatchEmptyError (ESome hd) ENone) (TOption tau)
#pop-options

#push-options "--fuel 9 --ifuel 2 --z3rlimit 80"
let process_exceptions_applied_stepping
  (tau: ty)
  (acc: typed_l_exp (TOption tau){is_value acc /\ not (is_error acc)})
  (hd: typed_l_exp tau)
    : Lemma (take_l_steps (TOption tau) (process_exceptions_applied tau acc hd) 3 ==
      Some (process_exceptions_applied_stepped tau acc hd))
  =
  let acc_invariant (s: var_to_exp) : Lemma (subst s acc == acc) [SMTPat (subst s acc)] =
    well_typed_terms_invariant_by_subst s acc (TOption tau)
  in
  [@inline_let]
  let inner_process_exceptions0 (hd: exp) = EApp
    (EAbs (TOption tau) (EMatchOption acc tau (EVar 0) (EAbs tau (
        EMatchOption (EVar 1) tau acc (EAbs tau (ELit (LError ConflictError)))))))
    (ECatchEmptyError (ESome (EApp hd (ELit LUnit) TUnit)) ENone)
    (TOption tau)
  in
  let e1 = EApp
    (EAbs (TArrow TUnit tau) (inner_process_exceptions0 (EVar 0)))
    (EThunk hd) (TArrow TUnit tau)
  in
  let e2 = inner_process_exceptions0 (EThunk hd) in
  assert_norm(step (process_exceptions_applied tau acc hd) == Some e1);
  assert(step e1 == Some e2);
  let hd0 = (ECatchEmptyError (ESome (EApp (EThunk hd) (ELit LUnit) TUnit)) ENone) in
  let hd1 = (ECatchEmptyError (ESome hd) ENone) in
  assert(step hd0 == Some hd1);
  [@inline_let]
  let inner_process_exceptions1 = EApp
    (EAbs (TOption tau) (EMatchOption acc tau (EVar 0) (EAbs tau (
        EMatchOption (EVar 1) tau acc (EAbs tau (ELit (LError ConflictError)))))))
    (ECatchEmptyError (ESome hd) ENone)
    (TOption tau)
  in
  let e3 = inner_process_exceptions1 in
  assert_norm(step e2 == Some e3);
  assert(e3 == process_exceptions_applied_stepped tau acc hd);
  preservation (process_exceptions_applied tau acc hd) (TOption tau);
  preservation e1 (TOption tau);
  preservation e2 (TOption tau);
  take_l_steps_transitive (TOption tau) (process_exceptions_applied tau acc hd) e1 1 1;
  take_l_steps_transitive (TOption tau) (process_exceptions_applied tau acc hd) e2 2 1
#pop-options

#push-options "--fuel 2 --ifuel 1 --z3rlimit 40"
let exceptions_head_lift_steps_to_error
  (tau: ty)
  (tl: list exp{is_value_list tl /\ typing_list empty tl (TArrow TUnit tau)})
  (acc: typed_l_exp (TOption tau){is_value acc})
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
    : Lemma (take_l_steps tau
      (exceptions_head_lift tau tl acc just cons (ELit (LError ConflictError))) 5 ==
        Some (ELit (LError ConflictError)))
  =
  let e = exceptions_head_lift tau tl acc just cons (ELit (LError ConflictError)) in
  let e_plus_3 : typed_l_exp tau =
    exceptions_init_lift tau tl just cons (ELit (LError ConflictError))
  in
  let open FStar.Tactics in
  assert(take_l_steps tau e 3 == Some e_plus_3) by begin
    compute ();
    smt ()
  end;
  let e_plus_4 : typed_l_exp tau = EMatchOption
    (ELit (LError ConflictError))
    tau
    (EIf
      just cons
      (ELit (LError EmptyError)))
    (EAbs tau (EVar 0))
  in
  assert(step e_plus_3 == Some e_plus_4) by begin
    compute ();
    smt ()
  end;
  preservation e_plus_3 tau;
  assert(Some e_plus_4 == take_l_steps tau e_plus_3 1);
  assert(step e_plus_4 ==  Some (ELit (LError ConflictError)));
  assert(take_l_steps tau e_plus_4 1 == Some (ELit (LError ConflictError)));
  take_l_steps_transitive tau e e_plus_3 3 1;
  take_l_steps_transitive tau e e_plus_4 4 1
#pop-options

#push-options "--fuel 4 --ifuel 1 --z3rlimit 20"
let exception_init_lift_conflict_error
  (tau: ty)
  (tl: list exp{is_value_list tl /\ typing_list empty tl (TArrow TUnit tau)})
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
    : Lemma (
      take_l_steps tau (exceptions_init_lift tau tl just cons (ELit (LError ConflictError))) 2 ==
        Some (ELit (LError ConflictError)))
  =
  let c_err = ELit (LError ConflictError) in
  let e0 = exceptions_init_lift tau tl just cons (ELit (LError ConflictError)) in
  let e1 = EMatchOption c_err tau (EIf just cons (ELit (LError EmptyError))) (EAbs tau (EVar 0)) in
  assert(step e0 == Some e1);
  assert(step e1 == Some c_err);
  preservation e0 tau;
  preservation e1 tau;
  take_l_steps_transitive tau e0 e1 1 1
#pop-options

let is_option_value (g: env) (e: exp) (tau: ty)
    : Lemma (requires (is_value e /\ (typing g e (TOption tau))))
      (ensures
        (match e with
          | ESome _ -> True
          | ENone -> True
          | ELit (LError _) -> True
          | _ -> False)) = ()

#push-options "--fuel 8 --ifuel 1 --z3rlimit 150 --quake 10/1"
let step_exceptions_head_value_error
  (tau: ty)
  (tl: list exp{is_value_list tl /\ typing_list empty tl (TArrow TUnit tau)})
  (acc: (typed_l_exp (TOption tau)))
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  (hd_err: err)
    : Pure (typed_l_exp (TOption tau) & nat)
      (requires (is_value acc /\ acc <> ELit (LError EmptyError)))
      (ensures (fun (new_acc, n) ->
        is_value new_acc /\ new_acc <> ELit (LError EmptyError) /\
        take_l_steps tau (exceptions_head_lift tau tl acc just cons (ELit (LError hd_err))) n ==
          Some (exceptions_init_lift tau tl just cons new_acc)
      ))
  =
  typ_process_exceptions_f empty tau;
  typing_empty_can_be_extended acc (TOption tau) (extend empty (TOption tau));
  typing_empty_can_be_extended acc (TOption tau)
    (extend (extend empty (TOption tau)) tau);
  let hd = ELit (LError hd_err) in
  let init0 : typed_l_exp (TOption tau) = EApp (EAbs (TOption tau) (
    EMatchOption acc tau (EVar 0) (EAbs tau (EMatchOption (EVar 1) tau acc (EAbs tau (
      ELit (LError ConflictError))))))) (ECatchEmptyError (ESome hd) ENone) (TOption tau)
  in
  let match_arg0  =
    EFoldLeft (process_exceptions_f tau) init0 (TOption tau) (EList tl) (TArrow TUnit tau)
  in
  let c_err = ELit (LError ConflictError) in
  let match_none0 = EIf just cons (ELit (LError EmptyError)) in
  let match_some0 = EAbs tau (EVar 0) in
  assert(exceptions_head_lift tau tl acc just cons hd ==
    EMatchOption match_arg0 tau match_none0 match_some0);
  assert(exceptions_head_lift tau tl acc just cons hd ==
    exceptions_init_lift tau tl just cons init0);
  let acc_invariant (s: var_to_exp) : Lemma (subst s acc == acc) [SMTPat (subst s acc)] =
    well_typed_terms_invariant_by_subst s acc (TOption tau)
  in
  let init1 = EApp (EAbs (TOption tau) (
    EMatchOption acc tau (EVar 0) (EAbs tau (EMatchOption (EVar 1) tau acc (EAbs tau (
      ELit (LError ConflictError))))))) (ECatchEmptyError hd ENone) (TOption tau)
  in
  assert(step init0 == Some init1);
  match hd_err with
  | ConflictError ->
    let init2 = EApp (EAbs (TOption tau) (
      EMatchOption acc tau (EVar 0) (EAbs tau (EMatchOption (EVar 1) tau acc (EAbs tau (
        ELit (LError ConflictError))))))) hd (TOption tau)
    in
    assert(step init1 == Some init2); assert(step init2 == Some c_err);
    preservation init0 (TOption tau); preservation init1 (TOption tau);
    preservation init2 (TOption tau);
    take_l_steps_transitive (TOption tau) init0 init1 1 1;
    take_l_steps_transitive (TOption tau) init0 init2 2 1;
    lift_multiple_l_steps (TOption tau) tau init0 c_err 3
      (exceptions_init_lift tau tl just cons);
    exception_init_lift_conflict_error tau tl just cons;
    c_err, 3
  | EmptyError ->
    let init2 = EApp (EAbs (TOption tau) (
      EMatchOption acc tau (EVar 0) (EAbs tau (EMatchOption (EVar 1) tau acc (EAbs tau (
        ELit (LError ConflictError))))))) ENone (TOption tau)
    in
    assert(step init1 == Some init2);
    let init3 = EMatchOption acc tau ENone (EAbs tau (EMatchOption ENone tau acc (EAbs tau (
      ELit (LError ConflictError)))))
    in
    assert(step init2 == Some init3);
    is_option_value empty acc tau;
    match acc with
    | ELit (LError _) ->
      assert(step init3 == Some acc);
      preservation init0 (TOption tau); preservation init1 (TOption tau);
      preservation init2 (TOption tau); preservation init3 (TOption tau);
      take_l_steps_transitive (TOption tau) init0 init1 1 1;
      take_l_steps_transitive (TOption tau) init0 init2 2 1;
      take_l_steps_transitive (TOption tau) init0 init3 3 1;
      lift_multiple_l_steps (TOption tau) tau init0 acc 4
        (exceptions_init_lift tau tl just cons);
      acc, 4
    | ENone ->
      assert(step init3 == Some ENone);
      preservation init0 (TOption tau); preservation init1 (TOption tau);
      preservation init2 (TOption tau); preservation init3 (TOption tau);
      take_l_steps_transitive (TOption tau) init0 init1 1 1;
      take_l_steps_transitive (TOption tau) init0 init2 2 1;
      take_l_steps_transitive (TOption tau) init0 init3 3 1;
      lift_multiple_l_steps (TOption tau) tau init0 ENone 4
        (exceptions_init_lift tau tl just cons);
      ENone, 4
    | ESome acc_inner ->
       let init4 = (EApp (EAbs tau (EMatchOption ENone tau acc (EAbs tau (
         ELit (LError ConflictError))))) acc_inner tau)
       in
       assert(step init3 == Some init4);
       let init5 = EMatchOption ENone tau acc (EAbs tau (
         ELit (LError ConflictError)))
       in
       assert(is_value acc_inner);
       assert(not (is_error acc_inner));
       assert(step init4 ==
         Some (subst (var_to_exp_beta acc_inner) (EMatchOption ENone tau acc (EAbs tau (
           ELit (LError ConflictError))))));
       assert(step init4 == Some init5);
       assert(step init5 == Some acc);
       preservation init0 (TOption tau); preservation init1 (TOption tau);
       preservation init2 (TOption tau); preservation init3 (TOption tau);
       preservation init4 (TOption tau); preservation init5 (TOption tau);
       take_l_steps_transitive (TOption tau) init0 init1 1 1;
       take_l_steps_transitive (TOption tau) init0 init2 2 1;
       take_l_steps_transitive (TOption tau) init0 init3 3 1;
       take_l_steps_transitive (TOption tau) init0 init4 4 1;
       take_l_steps_transitive (TOption tau) init0 init5 5 1;
       lift_multiple_l_steps (TOption tau) tau init0 acc 6
        (exceptions_init_lift tau tl just cons);
       acc, 6
#pop-options

#push-options "--fuel 8 --ifuel 1 --z3rlimit 1000 --quake 10/1"
let step_exceptions_head_value_non_error
  (tau: ty)
  (tl: list exp{is_value_list tl /\ typing_list empty tl (TArrow TUnit tau)})
  (acc: (typed_l_exp (TOption tau)))
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  (hd: typed_l_exp tau)
    : Pure (typed_l_exp (TOption tau) & nat)
      (requires (is_value hd /\ not (is_error hd) /\ is_value acc /\ acc <> ELit (LError EmptyError)))
      (ensures (fun (new_acc, n) ->
        is_value new_acc /\ new_acc <> ELit (LError EmptyError) /\
        take_l_steps tau (exceptions_head_lift tau tl acc just cons hd) n ==
          Some (exceptions_init_lift tau tl just cons new_acc)
      ))
  =
  typ_process_exceptions_f empty tau;
  typing_empty_can_be_extended acc (TOption tau) (extend empty (TOption tau));
  typing_empty_can_be_extended acc (TOption tau)
    (extend (extend empty (TOption tau)) tau);
  let init0 : typed_l_exp (TOption tau) = EApp (EAbs (TOption tau) (
    EMatchOption acc tau (EVar 0) (EAbs tau (EMatchOption (EVar 1) tau acc (EAbs tau (
      ELit (LError ConflictError))))))) (ECatchEmptyError (ESome hd) ENone) (TOption tau)
  in
  let match_arg0  =
    EFoldLeft (process_exceptions_f tau) init0 (TOption tau) (EList tl) (TArrow TUnit tau)
  in
  let c_err = ELit (LError ConflictError) in
  let match_none0 = EIf just cons (ELit (LError EmptyError)) in
  let match_some0 = EAbs tau (EVar 0) in
  assert(exceptions_head_lift tau tl acc just cons hd ==
    EMatchOption match_arg0 tau match_none0 match_some0);
  assert(exceptions_head_lift tau tl acc just cons hd ==
    exceptions_init_lift tau tl just cons init0);
  let acc_invariant (s: var_to_exp) : Lemma (subst s acc == acc) [SMTPat (subst s acc)] =
    well_typed_terms_invariant_by_subst s acc (TOption tau)
  in
  let some_hd_invariant (s: var_to_exp) : Lemma (subst s (ESome hd) == ESome hd)
      [SMTPat (subst s (ESome hd))]
    =
    well_typed_terms_invariant_by_subst s (ESome hd) (TOption tau)
  in
  let init1 = EApp (EAbs (TOption tau) (
    EMatchOption acc tau (EVar 0) (EAbs tau (EMatchOption (EVar 1) tau acc (EAbs tau (
      ELit (LError ConflictError))))))) (ESome hd) (TOption tau)
  in
  assert(step init0 == Some init1);
  let init2 = EMatchOption acc tau (ESome hd) (EAbs tau (EMatchOption (ESome hd) tau acc (EAbs tau (
    ELit (LError ConflictError)))))
  in
  assert(step init1 == Some init2);
  is_option_value empty acc tau;
  match acc with
  | ELit (LError _) ->
    assert(step init2 == Some acc);
    preservation init0 (TOption tau); preservation init1 (TOption tau);
    preservation init2 (TOption tau);
    take_l_steps_transitive (TOption tau) init0 init1 1 1;
    take_l_steps_transitive (TOption tau) init0 init2 2 1;
    lift_multiple_l_steps (TOption tau) tau init0 acc 3
      (exceptions_init_lift tau tl just cons);
    acc, 3
  | ENone ->
    assert(step init2 == Some (ESome hd));
    preservation init0 (TOption tau); preservation init1 (TOption tau);
    preservation init2 (TOption tau);
    take_l_steps_transitive (TOption tau) init0 init1 1 1;
    take_l_steps_transitive (TOption tau) init0 init2 2 1;
    lift_multiple_l_steps (TOption tau) tau init0 (ESome hd) 3
      (exceptions_init_lift tau tl just cons);
    ESome hd, 3
  | ESome acc_inner ->
    let init3 = EApp (EAbs tau (EMatchOption (ESome hd) tau acc
      (EAbs tau (ELit (LError ConflictError))))) acc_inner tau
    in
    assert(step init2 == Some init3);
    let init4 = EMatchOption (ESome hd) tau acc (EAbs tau (ELit (LError ConflictError))) in
    assert(is_value acc_inner);
    assert(not (is_error acc_inner));
    assert_norm(step init3 == Some (subst (var_to_exp_beta acc_inner)
      (EMatchOption (ESome hd) tau acc (EAbs tau (ELit (LError ConflictError))))));
    assert(step init3 == Some init4);
    let init5 = EApp (EAbs tau (ELit (LError ConflictError))) hd tau in
    assert(step init4 == Some init5);
    let c_err = ELit (LError ConflictError) in
    assert_norm(step init5 == Some c_err);
    preservation init0 (TOption tau); preservation init1 (TOption tau);
    preservation init2 (TOption tau); preservation init3 (TOption tau);
    preservation init4 (TOption tau); preservation init5 (TOption tau);
    take_l_steps_transitive (TOption tau) init0 init1 1 1;
    take_l_steps_transitive (TOption tau) init0 init2 2 1;
    take_l_steps_transitive (TOption tau) init0 init3 3 1;
    take_l_steps_transitive (TOption tau) init0 init4 4 1;
    take_l_steps_transitive (TOption tau) init0 init5 5 1;
    lift_multiple_l_steps (TOption tau) tau init0 c_err 6
     (exceptions_init_lift tau tl just cons);
    c_err, 6
#pop-options

let step_exceptions_head_value
  (tau: ty)
  (tl: list exp{is_value_list tl /\ typing_list empty tl (TArrow TUnit tau)})
  (acc: (typed_l_exp (TOption tau)))
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  (hd: (typed_l_exp tau))
    : Pure (typed_l_exp (TOption tau) & nat)
      (requires (is_value hd /\ is_value acc /\ acc <> ELit (LError EmptyError)))
      (ensures (fun (new_acc, n) ->
        is_value new_acc /\ new_acc <> ELit (LError EmptyError) /\
        take_l_steps tau (exceptions_head_lift tau tl acc just cons hd) n ==
          Some (exceptions_init_lift tau tl just cons new_acc)
      ))
  =
  match hd with
  | ELit (LError hd_err) -> step_exceptions_head_value_error tau tl acc just cons hd_err
  | _ -> step_exceptions_head_value_non_error tau tl acc just cons hd

let step_exceptions_head_value_same_acc_result
  (tau: ty)
  (tl: list exp{is_value_list tl /\ typing_list empty tl (TArrow TUnit tau)})
  (tl': list exp{is_value_list tl' /\ typing_list empty tl' (TArrow TUnit tau)})
  (acc: typed_l_exp (TOption tau){is_value acc /\ acc <> ELit (LError EmptyError)})
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  (hd: (typed_l_exp tau){is_value hd})
    : Lemma (
      let new_acc, _ = step_exceptions_head_value tau tl acc just cons hd in
      let new_acc', _ = step_exceptions_head_value tau tl' acc just cons hd in
      new_acc == new_acc'
    )
  =
  ()

let step_exceptions_head_value_go_through
  (tau: ty)
  (tl: list exp{is_value_list tl /\ typing_list empty tl (TArrow TUnit tau)})
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))

    : Lemma (
      let new_acc, _ =
        step_exceptions_head_value tau tl ENone just cons (ELit (LError EmptyError)) in
      new_acc = ENone
    )
  =
  ()

let step_exceptions_empty_conflict_error
  (tau: ty)
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
    : Pure nat
      (requires (True))
      (ensures (fun n ->
      build_default_translation_typing [] (ELit (LError ConflictError)) just cons tau empty;
        take_l_steps tau
          (build_default_translation [] (ELit (LError ConflictError)) just cons tau) n ==
            Some (ELit (LError ConflictError))))
  =
  build_default_translation_typing [] (ELit (LError ConflictError)) just cons tau empty;
  assert_norm(take_l_steps tau
          (build_default_translation [] (ELit (LError ConflictError)) just cons tau) 2 ==
            Some (ELit (LError ConflictError)));
  2

#push-options "--fuel 4 --ifuel 1 --z3rlimit 40"
let step_exceptions_empty_some_acc
  (tau: ty)
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  (acc: (typed_l_exp tau))
    : Pure nat
      (requires (is_value acc /\ not (is_error acc)))
      (ensures (fun n ->
      build_default_translation_typing [] (ESome acc) just cons tau empty;
        take_l_steps tau
          (build_default_translation [] (ESome acc) just cons tau) n ==
            Some acc))
  =
  let one_step : typed_l_exp tau =
    EMatchOption (ESome acc) tau
              (EIf just cons (ELit (LError (EmptyError))))
              (EAbs tau (EVar 0))
  in
  build_default_translation_typing [] (ESome acc) just cons tau empty;
  assert(take_l_steps tau
          (build_default_translation [] (ESome acc) just cons tau) 1 ==
            Some one_step);
  let two_step : typed_l_exp tau =
    EApp (EAbs tau (EVar 0)) acc tau
  in
  assert(take_l_steps tau one_step 1 == Some two_step);
  assert(take_l_steps tau two_step 1 == Some acc);
  take_l_steps_transitive tau (build_default_translation [] (ESome acc) just cons tau) one_step 1 1;
  take_l_steps_transitive tau (build_default_translation [] (ESome acc) just cons tau) two_step 2 1;
  3
#pop-options

#push-options "--fuel 4 --ifuel 1 --z3rlimit 40"
let step_exceptions_empty_none
  (tau: ty)
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
    : Pure nat
      (requires (True))
      (ensures (fun n ->
      build_default_translation_typing [] ENone just cons tau empty;
        take_l_steps tau
          (build_default_translation [] ENone just cons tau) n ==
            Some (EIf just cons (ELit (LError (EmptyError))))))
  =
  let one_step : typed_l_exp tau =
    EMatchOption ENone tau
              (EIf just cons (ELit (LError (EmptyError))))
              (EAbs tau (EVar 0))
  in
  build_default_translation_typing [] ENone just cons tau empty;
  assert(take_l_steps tau
          (build_default_translation [] ENone just cons tau) 1 ==
            Some one_step);
  let two_step : typed_l_exp tau =
    (EIf just cons (ELit (LError (EmptyError))))
  in
  assert(take_l_steps tau one_step 1 == Some two_step);
  take_l_steps_transitive tau (build_default_translation [] ENone just cons tau) one_step 1 1;
  2
#pop-options


#push-options "--fuel 4 --ifuel 1 --z3rlimit 40"
let step_exceptions_cons_conflict_error
  (tau: ty)
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  (hd: typed_l_exp tau)
  (tl: list exp)
    : Pure nat
      (requires (is_value hd /\ is_value_list tl /\ typing_list empty tl (TArrow TUnit tau)))
      (ensures (fun n ->
      build_default_translation_typing
        ((EThunk hd)::tl) (ELit (LError ConflictError)) just cons tau empty;
      take_l_steps tau
        (build_default_translation ((EThunk hd)::tl) (ELit (LError ConflictError)) just cons tau) n
          == Some (ELit (LError ConflictError))))
  =
  let l_err: typed_l_exp (TOption tau) = (ELit (LError ConflictError)) in
  build_default_translation_typing
    ((EThunk hd)::tl)
    (ELit (LError ConflictError)) just cons tau empty;
  let one_step : typed_l_exp tau =
    EMatchOption l_err tau
              (EIf just cons (ELit (LError (EmptyError))))
              (EAbs tau (EVar 0))
  in
  assert(take_l_steps tau
    (build_default_translation ((EThunk hd)::tl) (ELit (LError ConflictError)) just cons tau) 1 ==
      Some one_step);
  let l_err': typed_l_exp tau = (ELit (LError ConflictError)) in
  assert(take_l_steps tau one_step 1 == Some l_err');
  take_l_steps_transitive tau
     (build_default_translation ((EThunk hd)::tl) (ELit (LError ConflictError)) just cons tau)
     one_step 1 1;
  2
#pop-options

#push-options "--fuel 4 --ifuel 1 --z3rlimit 40"
let step_exceptions_general_conflict_error
  (tau: ty)
  (just: (typed_l_exp TBool))
  (cons: (typed_l_exp tau))
  (exceptions: list exp)
    : Pure nat
      (requires (is_value_list exceptions /\ typing_list empty exceptions (TArrow TUnit tau)))
      (ensures (fun n ->
      build_default_translation_typing
        exceptions (ELit (LError ConflictError)) just cons tau empty;
      take_l_steps tau
        (build_default_translation exceptions (ELit (LError ConflictError)) just cons tau) n
          == Some (ELit (LError ConflictError))))
  =
  let l_err: typed_l_exp (TOption tau) = (ELit (LError ConflictError)) in
  build_default_translation_typing
    exceptions
    (ELit (LError ConflictError)) just cons tau empty;
  let one_step : typed_l_exp tau =
    EMatchOption l_err tau
              (EIf just cons (ELit (LError (EmptyError))))
              (EAbs tau (EVar 0))
  in
  assert(take_l_steps tau
    (build_default_translation exceptions (ELit (LError ConflictError)) just cons tau) 1 ==
      Some one_step);
  let l_err': typed_l_exp tau = (ELit (LError ConflictError)) in
  assert(take_l_steps tau one_step 1 == Some l_err');
  take_l_steps_transitive tau
     (build_default_translation exceptions (ELit (LError ConflictError)) just cons tau)
     one_step 1 1;
  2
#pop-options
