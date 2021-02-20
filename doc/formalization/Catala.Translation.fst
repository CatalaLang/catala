module Catala.Translation

module L = Catala.LambdaCalculus
module D = Catala.DefaultCalculus

(*** Translation definitions *)


(**** Helpers *)

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

let process_exceptions_f (tau: L.ty) : Tot L.exp =
  L.EAbs (L.TOption tau) (L.EAbs (L.TArrow L.TUnit tau) (
    L.EApp (L.EAbs (L.TOption tau) (
      L.EMatchOption (L.EVar 2) tau
        (L.EVar 0)
        (L.EAbs tau (
          L.EMatchOption (L.EVar 1) tau
            (L.EVar 3)
            (L.EAbs tau (L.ELit (L.LError L.ConflictError)))
        ))
    ))
    (L.ECatchEmptyError (L.ESome (L.EApp (L.EVar 0) (L.ELit L.LUnit) L.TUnit)) L.ENone)
    (L.TOption tau)
  ))

let typ_process_exceptions_f (g: L.env) (tau: L.ty)
    : Lemma (L.typing g (process_exceptions_f tau)
      (L.TArrow (L.TOption tau) (L.TArrow (L.TArrow L.TUnit tau) (L.TOption tau))))
  =
  assert_norm(L.typing g (process_exceptions_f tau)
      (L.TArrow (L.TOption tau) (L.TArrow (L.TArrow L.TUnit tau) (L.TOption tau))))

(**** Main translation *)

let build_default_translation
  (exceptions: list L.exp)
  (acc: L.exp)
  (just: L.exp)
  (cons: L.exp)
  (tau: L.ty)
  =
  L.EMatchOption
    (L.EFoldLeft
      (process_exceptions_f tau)
      acc (L.TOption tau)
      (L.EList exceptions) (L.TArrow L.TUnit tau))
    tau
    (L.EIf
      just cons
      (L.ELit (L.LError L.EmptyError)))
    (L.EAbs tau (L.EVar 0))

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

#push-options "--fuel 1 --ifuel 0"
let build_default_translation_typing
  (exceptions: list L.exp)
  (acc: L.exp)
  (just: L.exp)
  (cons: L.exp)
  (tau: L.ty)
  (g: L.env)
    : Lemma
      (requires (
        L.typing_list g exceptions (L.TArrow L.TUnit tau) /\
        L.typing g acc (L.TOption tau) /\
        L.typing g just L.TBool /\
        L.typing g cons tau))
      (ensures (L.typing g (build_default_translation exceptions acc just cons tau) tau))
  =
  typ_process_exceptions_f g tau;
  assert_norm(L.typing g (build_default_translation exceptions acc just cons tau) tau)
#pop-options

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

(**** Step lifting framework *)

let typed_l_exp (tau: L.ty) = e:L.exp{L.typing L.empty e tau}

let rec take_l_steps (tau: L.ty) (e: typed_l_exp tau) (fuel: nat)
    : Tot (option (typed_l_exp tau))
      (decreases fuel) =
  if fuel = 0 then Some e else
  match L.step e with
  | None -> None
  | Some e' ->
    L.preservation e tau;
    take_l_steps tau e' (fuel - 1)

#push-options "--fuel 2 --ifuel 1"
let rec take_l_steps_transitive (tau: L.ty) (e1 e2: typed_l_exp tau) (n1 n2: nat)
    : Lemma
      (requires (take_l_steps tau e1 n1 == Some e2))
      (ensures (take_l_steps tau e1 (n1 + n2) == take_l_steps tau e2 n2))
      (decreases n1)
  =
  if n1 = 0 then () else begin
    match L.step e1 with
      | None -> ()
      | Some e1' ->
        L.preservation e1 tau;
        take_l_steps_transitive tau e1' e2 (n1 - 1) n2
  end
#pop-options

let not_l_value (tau: L.ty) = e:L.exp{not (L.is_value e) /\ L.typing L.empty e tau}
let l_value (tau: L.ty) = e:L.exp{L.is_value e /\ L.typing L.empty e tau}

let stepping_context (tau tau': L.ty) = typed_l_exp tau -> not_l_value tau'

let step_lift_commute_non_value
  (tau tau': L.ty)
  (f: stepping_context tau tau')
  (e: typed_l_exp tau)
    : prop
  =
  L.progress e tau;
  if L.is_value e then true else begin
    L.preservation e tau;
    L.step (f e) == Some (f (Some?.v (L.step e)))
  end

let is_stepping_agnostic_lift
  (tau tau': L.ty)
  (f:stepping_context tau tau')
    : prop
  =
  forall (e: typed_l_exp tau). step_lift_commute_non_value tau tau' f e

let stepping_agnostic_lift
  (tau tau': L.ty)
  : Type
  = f:(stepping_context tau tau'){is_stepping_agnostic_lift tau tau' f}

let rec l_values_dont_step (e: L.exp) : Lemma
    (requires (L.is_value e))
    (ensures (L.step e = None))
    (decreases %[e; 1])
  =
  match e with
  | L.EAbs _ _ -> ()
  | L.EThunk _ -> ()
  | L.ELit _ -> ()
  | L.ENone -> ()
  | L.EList [] -> ()
  | L.EList l -> l_values_dont_step_list e l
  | _ -> ()
and l_values_dont_step_list (e: L.exp) (l: list L.exp{l << e /\ Cons? l}) : Lemma
    (requires (L.is_value_list l))
    (ensures (L.step_list e l = L.Bad))
    (decreases %[e; 0; l])
  =
  match l with
  | [hd] -> l_values_dont_step hd
  | hd::tl ->
    l_values_dont_step hd;
    l_values_dont_step_list e tl

#push-options "--z3rlimit 50 --fuel 2 --ifuel 1"
let rec lift_multiple_l_steps
  (tau tau': L.ty)
  (e1: typed_l_exp tau)
  (e2: typed_l_exp tau)
  (n: nat)
  (f : stepping_agnostic_lift tau tau')
    : Lemma
      (requires (take_l_steps tau e1 n == Some e2))
      (ensures (take_l_steps tau' (f e1) n == Some (f e2)))
      (decreases n)
  =
  match L.step e1 with
  | None -> ()
  | Some e1' ->
    L.progress e1 tau;
    L.preservation e1 tau;
    if L.is_value e1 then
      l_values_dont_step e1
    else if n = 0 then
      ()
    else
      lift_multiple_l_steps tau tau' e1' e2 (n-1) f
#pop-options

(**** Other helpers *)


#push-options "--fuel 9 --ifuel 0"
let process_exceptions_untouched_by_subst (s: L.var_to_exp) (tau: L.ty) : Lemma
    (L.subst s (process_exceptions_f tau) == process_exceptions_f tau)
  =
  ()
#pop-options

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

(**** Lifts *)

let if_cond_lift'
  (tau: L.ty)
  (e2 e3: typed_l_exp tau)
    : stepping_context L.TBool tau
  =
  fun e1 -> L.EIf e1 e2 e3

let if_cond_lift_is_stepping_agnostic
  (tau: L.ty)
  (e2 e3: typed_l_exp tau)
  (e: typed_l_exp L.TBool)
    : Lemma
      (requires (True))
      (ensures (step_lift_commute_non_value L.TBool tau (if_cond_lift' tau e2 e3) e))
  =
  L.progress e L.TBool; if L.is_value e then () else L.preservation e L.TBool

let if_cond_lift
  (tau: L.ty)
  (e2 e3: typed_l_exp tau)
    : stepping_agnostic_lift L.TBool tau
  =
  Classical.forall_intro (if_cond_lift_is_stepping_agnostic tau e2 e3);
  if_cond_lift' tau e2 e3


let app_f_lift'
  (tau_arg tau: L.ty)
  (e2: typed_l_exp tau_arg)
    : stepping_context (L.TArrow tau_arg tau) tau
  =
  fun e1 -> L.EApp e1 e2 tau_arg

let app_f_lift_is_stepping_agnostic
  (tau_arg tau: L.ty)
  (e2: typed_l_exp tau_arg)
  (e: typed_l_exp (L.TArrow tau_arg tau))
    : Lemma
      (requires (True))
      (ensures (
        step_lift_commute_non_value (L.TArrow tau_arg tau) tau (app_f_lift' tau_arg tau e2) e))
  =
  L.progress e  (L.TArrow tau_arg tau);
  if L.is_value e then () else L.preservation e  (L.TArrow tau_arg tau)

let app_f_lift
  (tau_arg tau: L.ty)
  (e2: typed_l_exp tau_arg)
    : stepping_agnostic_lift (L.TArrow tau_arg tau) tau
  =
  Classical.forall_intro (app_f_lift_is_stepping_agnostic tau_arg tau e2);
  app_f_lift' tau_arg tau e2

let app_arg_lift'
  (tau_arg tau: L.ty)
  (e1: l_value (L.TArrow tau_arg tau))
    : stepping_context tau_arg tau
  =
  fun e2 -> L.EApp e1 e2 tau_arg

let app_arg_lift_is_stepping_agnostic
  (tau_arg tau: L.ty)
  (e1: l_value (L.TArrow tau_arg tau){match e1 with L.ELit (L.LError _) -> False | _ -> True})
  (e2: typed_l_exp tau_arg)
    : Lemma
      (requires (True))
      (ensures (
        step_lift_commute_non_value tau_arg tau (app_arg_lift' tau_arg tau e1) e2))
  =
  L.progress e2 tau_arg;
  if L.is_value e2 then () else L.preservation e2 tau_arg

let app_arg_lift
  (tau_arg tau: L.ty)
  (e1: l_value (L.TArrow tau_arg tau){match e1 with L.ELit (L.LError _) -> False | _ -> True})
    : stepping_agnostic_lift tau_arg tau
  =
  Classical.forall_intro (app_arg_lift_is_stepping_agnostic tau_arg tau e1);
  app_arg_lift' tau_arg tau e1

#push-options "--fuel 9 --ifuel 2 --z3rlimit 30"
let exceptions_head_lift'
  (tau: L.ty)
  (tl: list L.exp{L.typing_list L.empty tl (L.TArrow L.TUnit tau)})
  (acc: typed_l_exp (L.TOption tau))
  (just: (typed_l_exp L.TBool))
  (cons: (typed_l_exp tau))
  : stepping_context tau tau
  =
  fun (hd: typed_l_exp tau) ->
    typ_process_exceptions_f L.empty tau;
    L.typing_empty_can_be_extended acc (L.TOption tau) (L.TOption tau);
    L.EMatchOption
    (L.EFoldLeft
      (process_exceptions_f tau)
      (L.EApp
        (L.EAbs (L.TOption tau) (
          L.EMatchOption acc tau
            (L.EVar 0)
            (L.EAbs tau (
              L.EMatchOption (L.EVar 1) tau
                L.ENone
                (L.EAbs tau (L.ELit (L.LError L.ConflictError)))
            ))
         ))
         (L.ECatchEmptyError
           (L.ESome hd) L.ENone)
         (L.TOption tau)
      )
      (L.TOption tau)
      (L.EList tl) (L.TArrow L.TUnit tau))
    tau
    (L.EIf
      just cons
      (L.ELit (L.LError L.EmptyError)))
    (L.EAbs tau (L.EVar 0))
#pop-options

let exceptions_head_lift_is_stepping_agnostic
  (tau: L.ty)
  (tl: list L.exp{L.typing_list L.empty tl (L.TArrow L.TUnit tau)})
  (acc: typed_l_exp (L.TOption tau))
  (just: (typed_l_exp L.TBool))
  (cons: (typed_l_exp tau))
  (hd: typed_l_exp tau)
    : Lemma (step_lift_commute_non_value tau tau (exceptions_head_lift' tau tl acc just cons) hd)
  =
  L.progress hd tau;
  if L.is_value hd then () else begin
    L.preservation hd tau;
    assert_norm(L.step (exceptions_head_lift' tau tl acc just cons hd) == Some
      (exceptions_head_lift' tau tl acc just cons (Some?.v (L.step hd))))
  end

let exceptions_head_lift
  (tau: L.ty)
  (tl: list L.exp{L.typing_list L.empty tl (L.TArrow L.TUnit tau)})
  (acc: typed_l_exp (L.TOption tau))
  (just: (typed_l_exp L.TBool))
  (cons: (typed_l_exp tau))
  : stepping_agnostic_lift tau tau
  =
  Classical.forall_intro (exceptions_head_lift_is_stepping_agnostic tau tl acc just cons);
  exceptions_head_lift' tau tl acc just cons

#push-options "--fuel 3 --ifuel 0"
let exceptions_init_lift'
  (tau: L.ty)
  (tl: list L.exp{L.typing_list L.empty tl (L.TArrow L.TUnit tau)})
  (just: (typed_l_exp L.TBool))
  (cons: (typed_l_exp tau))
  : stepping_context (L.TOption tau) tau
  =
  fun (init: typed_l_exp (L.TOption tau)) ->
    typ_process_exceptions_f L.empty tau;
    L.EMatchOption
    (L.EFoldLeft
      (process_exceptions_f tau)
      init
      (L.TOption tau)
      (L.EList tl) (L.TArrow L.TUnit tau))
    tau
    (L.EIf
      just cons
      (L.ELit (L.LError L.EmptyError)))
    (L.EAbs tau (L.EVar 0))
#pop-options

let exceptions_init_lift_is_stepping_agnostic
  (tau: L.ty)
  (tl: list L.exp{L.typing_list L.empty tl (L.TArrow L.TUnit tau)})
  (just: (typed_l_exp L.TBool))
  (cons: (typed_l_exp tau))
  (init: typed_l_exp (L.TOption tau))
    : Lemma (
      step_lift_commute_non_value (L.TOption tau) tau (exceptions_init_lift' tau tl just cons) init)
  =
  L.progress init (L.TOption tau);
  if L.is_value init then () else begin
    L.preservation init (L.TOption tau);
    assert_norm(L.step (exceptions_init_lift' tau tl just cons init) == Some
      (exceptions_init_lift' tau tl just cons (Some?.v (L.step init))))
  end

let exceptions_init_lift
  (tau: L.ty)
  (tl: list L.exp{L.typing_list L.empty tl (L.TArrow L.TUnit tau)})
  (just: (typed_l_exp L.TBool))
  (cons: (typed_l_exp tau))
  : stepping_agnostic_lift (L.TOption tau) tau
  =
  Classical.forall_intro (exceptions_init_lift_is_stepping_agnostic tau tl just cons);
  exceptions_init_lift' tau tl just cons


(**** Other helpers *)

let process_exceptions_applied
  (tau: L.ty)
  (acc: typed_l_exp (L.TOption tau))
  (hd: typed_l_exp tau)
    : Tot (typed_l_exp (L.TOption tau))
  =
  typ_process_exceptions_f L.empty tau;
  L.typing_empty_can_be_extended hd tau L.TUnit;
  L.EApp
   (L.EApp (process_exceptions_f tau) acc (L.TOption tau))
   (L.EAbs L.TUnit hd) (L.TArrow L.TUnit tau)


#push-options "--fuel 7 --ifuel 2"
let process_exceptions_applied_stepped
  (tau: L.ty)
  (acc: typed_l_exp (L.TOption tau))
  (hd: typed_l_exp tau)
    : Tot (typed_l_exp (L.TOption tau))
  =
  let e' = 2 in
  let a' = 3 in
  let e'' = 4 in
  L.typing_empty_can_be_extended acc (L.TOption tau) (L.TOption tau);
  L.EApp (L.EAbs (L.TOption tau) (
    L.EMatchOption acc tau (L.EVar 0) (L.EAbs tau (
      L.EMatchOption (L.EVar 1) tau L.ENone (L.EAbs  tau
        (L.ELit (L.LError L.ConflictError))
      )
    ))))
    (L.ECatchEmptyError (L.ESome hd) L.ENone) (L.TOption tau)
#pop-options

#push-options "--fuel 9 --ifuel 2 --z3rlimit 70"
let process_exceptions_applied_stepping
  (tau: L.ty)
  (acc: typed_l_exp (L.TOption tau){L.is_value acc /\ not (L.is_error acc)})
  (hd: typed_l_exp tau)
    : Lemma (take_l_steps (L.TOption tau) (process_exceptions_applied tau acc hd) 3 ==
      Some (process_exceptions_applied_stepped tau acc hd))
  =
  let e1 : L.exp =
    L.EApp
      (L.EAbs (L.TArrow L.TUnit tau) (
    L.EApp (L.EAbs (L.TOption tau) (
      L.EMatchOption acc tau
        (L.EVar 0)
        (L.EAbs  tau (
          L.EMatchOption (L.EVar 1) tau
            acc
            (L.EAbs tau (L.ELit (L.LError L.ConflictError)))
        ))
    ))
    (L.ECatchEmptyError (L.ESome (L.EApp (L.EVar 0) (L.ELit L.LUnit) L.TUnit)) L.ENone)
    (L.TOption tau)
  )
      )
      (L.EThunk hd) (L.TArrow L.TUnit tau)
  in
  let e2 =
    L.EApp (L.EAbs (L.TOption tau) (
      L.EMatchOption acc tau
        (L.EVar 0)
        (L.EAbs tau (
          L.EMatchOption (L.EVar 1) tau
            acc
            (L.EAbs tau (L.ELit (L.LError L.ConflictError)))
        ))
    ))
    (L.ECatchEmptyError (L.ESome (L.EApp (L.EThunk hd) (L.ELit L.LUnit) L.TUnit)) L.ENone)
    (L.TOption tau)

  in
  assume(L.step (process_exceptions_applied tau acc hd) == Some e1);
  assert(L.step e1 == Some e2);
  admit()
#pop-options

#push-options "--fuel 7 --ifuel 2 --z3rlimit 50"
let lift_multiple_l_steps_exceptions_head
  (tau: L.ty)
  (tl: list L.exp{L.typing_list L.empty tl (L.TArrow L.TUnit tau) /\ L.is_value_list tl})
  (acc: typed_l_exp (L.TOption tau))
  (just: (typed_l_exp L.TBool))
  (cons: (typed_l_exp tau))
  (n_hd: nat)
  (hd: typed_l_exp tau)
  (final_hd: typed_l_exp tau)
    : Lemma
      (requires (take_l_steps tau hd n_hd == Some final_hd /\ L.is_value acc))
      (ensures (
        build_default_translation_typing
          ((L.EThunk hd)::tl) acc just cons tau L.empty;
        take_l_steps tau
          (build_default_translation ((L.EThunk hd)::tl) acc just cons tau)
          (n_hd + 4) ==
            Some (exceptions_head_lift tau tl acc just cons final_hd)))
  =
  build_default_translation_typing ((L.EThunk hd)::tl) acc just cons tau L.empty;
  let e' = 2 in
  let a' = 3 in
  let e'' = 4 in
  let init_stepped = L.EApp (L.EAbs (L.TOption tau) (
    L.EMatchOption acc tau (L.EVar 0) (L.EAbs tau (
      L.EMatchOption (L.EVar 1) tau L.ENone (L.EAbs tau
        (L.ELit (L.LError L.ConflictError))
      )
    ))))
    (L.ECatchEmptyError (L.ESome hd) L.ENone) (L.TOption tau)
  in
  let init = L.EApp
    (L.EApp (process_exceptions_f tau) acc (L.TOption tau))
    (L.EThunk hd) (L.TArrow L.TUnit tau)
  in
  admit();
  let open FStar.Tactics in
  assert(take_l_steps (L.TOption tau) init 3 == Some init_stepped) by begin
    compute ()
  end;
  admit();
  let default_translation: typed_l_exp tau =
    build_default_translation ((L.EAbs L.TUnit hd)::tl) acc just cons tau
  in
  let default_translation_stepped = L.EMatchOption
    (L.EFoldLeft
      (process_exceptions_f tau)
      init (L.TOption tau)
      (L.EList tl) (L.TArrow L.TUnit tau))
    tau
    (L.EIf
      just cons
      (L.ELit (L.LError L.EmptyError)))
    (L.EAbs tau (L.EVar 0))
  in
  assert(take_l_steps tau default_translation 1 == Some default_translation_stepped);
  assert(default_translation_stepped == exceptions_init_lift tau tl just cons
    (L.EApp (L.EApp (process_exceptions_f tau) L.ENone (L.TOption tau))
      (L.EAbs L.TUnit hd) (L.TArrow L.TUnit tau)));
  lift_multiple_l_steps (L.TOption tau) tau init init_stepped 3
    (exceptions_init_lift tau tl just cons);
  assert(take_l_steps tau default_translation_stepped 3 ==
    Some (exceptions_head_lift tau tl acc just cons hd));
  take_l_steps_transitive tau default_translation default_translation_stepped 1 3;
  assert(take_l_steps tau default_translation 4 ==
    Some (exceptions_head_lift tau tl acc just cons hd));
  lift_multiple_l_steps tau tau hd final_hd n_hd (exceptions_head_lift tau tl acc just cons);
  assert(take_l_steps tau (exceptions_head_lift tau tl acc just cons hd) n_hd ==
    Some (exceptions_head_lift tau tl acc just cons final_hd));
  take_l_steps_transitive tau default_translation
    (exceptions_head_lift tau tl acc just cons hd) 4 n_hd
#pop-options

#push-options "--fuel 2 --ifuel 1 --z3rlimit 40"
let exceptions_head_lift_steps_to_error
  (tau: L.ty)
  (tl: list L.exp{L.is_value_list tl /\ L.typing_list L.empty tl (L.TArrow L.TUnit tau)})
  (just: (typed_l_exp L.TBool))
  (cons: (typed_l_exp tau))
    : Lemma (take_l_steps tau
      (exceptions_head_lift tau tl L.ENone just cons (L.ELit (L.LError L.ConflictError))) 5 ==
        Some (L.ELit (L.LError L.ConflictError)))
  =
  let e = exceptions_head_lift tau tl L.ENone just cons (L.ELit (L.LError L.ConflictError)) in
  let e_plus_3 : typed_l_exp tau =
    exceptions_init_lift tau tl just cons (L.ELit (L.LError L.ConflictError))
  in
  let open FStar.Tactics in
  assert(take_l_steps tau e 3 == Some e_plus_3) by begin
    compute ();
    smt ()
  end;
  let e_plus_4 : typed_l_exp tau = L.EMatchOption
    (L.ELit (L.LError L.ConflictError))
    tau
    (L.EIf
      just cons
      (L.ELit (L.LError L.EmptyError)))
    (L.EAbs tau (L.EVar 0))
  in
  assert(L.step e_plus_3 == Some e_plus_4) by begin
    compute ();
    smt ()
  end;
  L.preservation e_plus_3 tau;
  assert(Some e_plus_4 == take_l_steps tau e_plus_3 1);
  assert(L.step e_plus_4 ==  Some (L.ELit (L.LError L.ConflictError)));
  assert(take_l_steps tau e_plus_4 1 == Some (L.ELit (L.LError L.ConflictError)));
  take_l_steps_transitive tau e e_plus_3 3 1;
  take_l_steps_transitive tau e e_plus_4 4 1
#pop-options

let step_exceptions_head_value
  (tau: L.ty)
  (tl: list L.exp{L.is_value_list tl /\ L.typing_list L.empty tl (L.TArrow L.TUnit tau)})
  (just: (typed_l_exp L.TBool))
  (cons: (typed_l_exp tau))
  (hd: (typed_l_exp tau))
    : Pure (typed_l_exp (L.TOption tau) & nat)
      (requires (True))
      (ensures (fun (new_acc, n) ->
        take_l_steps tau (exceptions_head_lift tau tl L.ENone just cons hd) n ==
          Some (exceptions_init_lift tau tl just cons new_acc)
      ))
  =
  admit()

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

let rec_lemma_works_for_smaller
  (de: D.exp)
  (de': D.exp{de' << de})
  (lemma: rec_correctness_step_type de)
    : Tot (rec_correctness_step_type de')
  =
  lemma

#push-options "--fuel 2 --ifuel 1 --z3rlimit 150"
let rec translation_correctness_exceptions_left_to_right_step
  (de: D.exp)
  (dexceptions: list D.exp {dexceptions << de})
  (dcurrent: list D.exp{dcurrent << de})
  (djust: D.exp{djust << de})
  (dcons: D.exp{dcons << de})
  (dtau: D.ty)
  (rec_lemma: rec_correctness_step_type de)
    : Pure (nat & typed_l_exp (translate_ty dtau) & nat)
      (requires (
        Some? (D.step de) /\
        de == D.EDefault dexceptions djust dcons dtau /\
        D.typing D.empty de dtau /\
        D.step de == D.step_exceptions_left_to_right de dcurrent djust dcons dtau
      ))
      (ensures (fun (n1, target_e, n2) ->
        translation_preserves_empty_typ de dtau;
        let lcurrent = translate_exp_list dcurrent in
        let ljust = translate_exp djust in
        let lcons = translate_exp dcons in
        let Some de' = D.step_exceptions_left_to_right de dcurrent djust dcons dtau in
        let le' = translate_exp de' in
        D.preservation de dtau;
        let ltau = translate_ty dtau in
        translation_preserves_empty_typ de' dtau;
        admit();
        build_default_translation_typing lcurrent L.ENone ljust lcons ltau L.empty;
        take_l_steps ltau (build_default_translation lcurrent L.ENone ljust lcons ltau)
          n1 == Some target_e /\
        take_l_steps ltau le' n2 == Some target_e
      ))
      (decreases dexceptions)
  =
  admit()

let _ = ()

(*
  let Some de' = D.step_exceptions_left_to_right de dexceptions djust dcons dtau in
  let le = translate_exp de in
  D.preservation de dtau;
  translation_preserves_empty_typ de' dtau;
  translation_preserves_empty_typ de dtau;
  let ltau = translate_ty dtau in
  let le' : typed_l_exp ltau = translate_exp de' in
  let ljust = translate_exp djust in
  let lcons = translate_exp dcons in
  let lexceptions = translate_exp_list dexceptions in
  match dexceptions with
  | [] -> 0, le, 0
  | dhd::dtl ->
    let ltl = translate_exp_list dtl in
    let lhd = translate_exp dhd in
    if D.is_value dhd then begin
      match D.step_exceptions_left_to_right de dtl djust dcons dtau with
      | Some (D.ELit D.LConflictError) ->
        assert(de' == D.ELit (D.LConflictError));
        assert(le' == L.ELit (L.LError (L.ConflictError)));
        let l_err : typed_l_exp ltau = L.ELit (L.LError (L.ConflictError)) in
        translate_list_is_value_list dexceptions;
        build_default_translation_typing_source dexceptions djust dcons dtau D.empty;
        translation_preserves_typ_exceptions D.empty de dexceptions dtau;
        translate_empty_is_empty ();
        assert(L.typing_list L.empty ltl (L.TArrow L.TUnit ltau));
        assert(L.is_value_list ltl);
        translation_preserves_empty_typ dhd dtau;
        lift_multiple_l_steps_exceptions_head ltau ltl ljust lcons 0 lhd lhd;
        assert(take_l_steps ltau le 4 == Some (exceptions_head_lift ltau ltl ljust lcons lhd));
        let detl = D.EDefault dtl djust dcons dtau in
        exceptions_smaller dtl djust dcons dtau;
        tail_default_smaller dhd dtl djust dcons dtau;
        assume(Some? (D.step detl));
        assume(D.step detl == D.step_exceptions_left_to_right detl dtl djust dcons dtau);
        let detl' = Some?.v (D.step detl) in
        D.preservation detl dtau;
        translation_preserves_empty_typ detl' dtau;
        let letl' : typed_l_exp ltau = translate_exp detl' in
        assert(letl' == l_err);
        let n1_tl, target_tl, n2_tl = translation_correctness_exceptions_left_to_right_step
          detl dtl djust dcons dtau rec_lemma
        in
        let n = 6 in
        assume(take_l_steps ltau le n == Some l_err);
        n, le', 0
      | Some (D.EDefault dtl' djust' dcons' dtau') ->
        assert(djust' == djust /\ dcons' == dcons /\ dtau' == dtau);
        admit()
    end else begin
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
         build_default_translation_typing_source dexceptions djust dcons dtau D.empty;
         translate_empty_is_empty ();
         lift_multiple_l_steps_exceptions_head ltau ltl ljust lcons n1_hd lhd l_err;
         assert(take_l_steps ltau
           (build_default_translation ((L.EAbs L.Silent L.TUnit lhd)::ltl)
           ljust lcons ltau) (n1_hd + 4) ==
             Some (exceptions_head_lift ltau ltl ljust lcons l_err));
         exceptions_head_lift_steps_to_error ltau ltl ljust lcons;
         assert(take_l_steps ltau (exceptions_head_lift ltau ltl ljust lcons l_err) 5 ==
           Some l_err);
         assert(le' == l_err);
         take_l_steps_transitive ltau
           (build_default_translation ((L.EAbs L.Silent L.TUnit lhd)::ltl) ljust lcons ltau)
           (exceptions_head_lift ltau ltl ljust lcons l_err)
           (n1_hd + 4)
           5;
         let lexceptions = translate_exp_list dexceptions in
         assert(take_l_steps ltau (build_default_translation lexceptions ljust lcons ltau)
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
         build_default_translation_typing_source dexceptions djust dcons dtau D.empty;
         assert(L.is_value_list ltl);
         translate_empty_is_empty ();
         assert(L.typing_list L.empty ltl (L.TArrow L.TUnit ltau));
         lift_multiple_l_steps_exceptions_head ltau ltl ljust lcons n1_hd lhd target_hd;
         lift_multiple_l_steps_exceptions_head ltau ltl ljust lcons n2_hd lhd' target_hd;
         let target_lexp : typed_l_exp ltau =
           exceptions_head_lift ltau ltl ljust lcons target_hd
         in
         assert(take_l_steps ltau (build_default_translation ((L.EAbs L.Silent L.TUnit lhd)::ltl)
          ljust lcons ltau) (n1_hd + 4) == Some target_lexp);
         lift_multiple_l_steps_exceptions_head ltau ltl ljust lcons n2_hd lhd' target_hd;
         build_default_translation_typing ((L.EAbs L.Silent L.TUnit lhd')::ltl) ljust lcons ltau
           L.empty;
         assert(take_l_steps ltau (build_default_translation ((L.EAbs L.Silent L.TUnit lhd')::ltl)
          ljust lcons ltau) (n2_hd + 4) == Some target_lexp);
         (n1_hd + 4, target_lexp, n2_hd + 4)
    end
  *)
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
        D.step de == D.step_exceptions de dexceptions djust dcons dtau
      ))
      (ensures (fun (n1, target_e, n2) ->
      translation_preserves_empty_typ de dtau;
      let lexceptions = translate_exp_list dexceptions in
      let ljust = translate_exp djust in
      let lcons = translate_exp dcons in
      let Some de' = D.step_exceptions de dexceptions djust dcons dtau in
      let le' = translate_exp de' in
      D.preservation de dtau;
      let ltau = translate_ty dtau in
      translation_preserves_empty_typ de' dtau;
      take_l_steps ltau (build_default_translation lexceptions L.ENone ljust lcons ltau) n1
        == Some target_e /\
      take_l_steps ltau le' n2 == Some target_e
      ))
  =
  if List.Tot.for_all (fun except -> D.is_value except) dexceptions then
    admit()
  else
    translation_correctness_exceptions_left_to_right_step
      de dexceptions dexceptions djust dcons dtau rec_lemma

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
    | Some _ ->
      translation_correctness_exceptions_step de dexceptions djust dcons dtau
        (fun df tf -> translation_correctness_step df tf)
    | None -> admit()
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
