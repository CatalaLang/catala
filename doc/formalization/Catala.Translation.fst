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
  let a = 0 in
  let e = 1 in
  let e' = 2 in
  let a' = 3 in
  let e'' = 4 in
  L.EAbs (L.Named a) (L.TOption tau) (L.EAbs (L.Named e) (L.TArrow L.TUnit tau) (
    L.EApp (L.EAbs (L.Named e') (L.TOption tau) (
      L.EMatchOption (L.EVar a) tau
        (L.EVar e')
        (L.EAbs (L.Named a') tau (
          L.EMatchOption (L.EVar e') tau
            (L.EVar a)
            (L.EAbs (L.Named e'') tau (L.ELit (L.LError L.ConflictError)))
        ))
    ))
    (L.ECatchEmptyError (L.ESome (L.EApp (L.EVar e) (L.ELit L.LUnit) L.TUnit)) L.ENone)
    (L.TOption tau)
  ))

let typ_process_exceptions_f (tau: L.ty)
    : Lemma (L.typing L.empty (process_exceptions_f tau)
      (L.TArrow (L.TOption tau) (L.TArrow (L.TArrow L.TUnit tau) (L.TOption tau))))
  =
  assert_norm(L.typing L.empty (process_exceptions_f tau)
      (L.TArrow (L.TOption tau) (L.TArrow (L.TArrow L.TUnit tau) (L.TOption tau))))

(**** Main translation *)

let build_default_translation_aux
  (e: D.exp)
  (exceptions: list D.exp{exceptions << e})
  (just: D.exp{just << e})
  (cons: D.exp{cons << e})
  (tau: D.ty)
  (translate_exp: (e':D.exp{e' << e}) -> L.exp)
  (translate_exp_list: (l:list D.exp{l << e}) -> list L.exp)
  =
  let tau' = translate_ty tau in
  L.EMatchOption
    (L.EFoldLeft
      (process_exceptions_f tau')
      L.ENone (L.TOption tau')
      (L.EList (translate_exp_list exceptions)) (L.TArrow L.TUnit tau'))
    tau'
    (L.EIf
      (translate_exp just)
      (translate_exp cons)
      (L.ELit (L.LError L.EmptyError)))
    (L.EAbs (L.Named 0) tau' (L.EVar 0))

let rec translate_exp (e: D.exp) : Tot L.exp = match e with
  | D.EVar x -> L.EVar x
  | D.EApp e1 e2 tau_arg ->
    L.EApp (translate_exp e1) (translate_exp e2) (translate_ty tau_arg)
  | D.EAbs x ty body -> L.EAbs (L.Named x) (translate_ty ty) (translate_exp body)
  | D.ELit l -> L.ELit (translate_lit l)
  | D.EIf e1 e2 e3 -> L.EIf
    (translate_exp e1)
    (translate_exp e2)
    (translate_exp e3)
  | D.EDefault exceptions just cons tau ->
    build_default_translation_aux e exceptions just cons tau translate_exp translate_exp_list
and translate_exp_list (l: list D.exp) : Tot (list L.exp) =
  match l with
  | [] -> []
  | hd::tl -> (L.EAbs L.Silent L.TUnit (translate_exp hd))::(translate_exp_list tl)

let build_default_translation
  (exceptions: list D.exp)
  (just: D.exp)
  (cons: D.exp)
  (tau: D.ty)
  =
  let e = D.EDefault exceptions just cons tau in
  assume(exceptions << e);
  assume(just << e);
  assume(cons << e);
  build_default_translation_aux
    e
    exceptions
    just
    cons
    tau
    translate_exp
    translate_exp_list

let translate_env (g: D.env) : Tot L.env =
 FunctionalExtensionality.on_dom L.var_name
   (fun v -> match g v with None -> None | Some t -> Some (translate_ty t))

(*** Typing preservation *)

(**** Helpers and lemmas *)

let extend_translate_commute (g: D.env) (x: D.var) (tau: D.ty)
    : Lemma (L.extend (translate_env g) x (translate_ty tau) == translate_env (D.extend g x tau))
  =
  FunctionalExtensionality.extensionality L.var_name (fun _ -> option L.ty)
    (L.extend (translate_env g) x (translate_ty tau))
    (translate_env (D.extend g x tau))

let translate_empty_is_empty () : Lemma (translate_env D.empty == L.empty) =
  FunctionalExtensionality.extensionality L.var_name (fun _ -> option L.ty)
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
  | D.EAbs x tau_arg body -> begin
    match tau with
    | D.TArrow tau_in tau_out ->
      if tau_in = tau_arg then begin
        translation_preserves_typ (D.extend g x tau_in) body tau_out;
        extend_translate_commute g x tau_in
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
      typ_process_exceptions_f tau';
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
        (L.EAbs (L.Named 0) tau' (L.EVar 0))
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
    assert(L.typing g' (L.EAbs L.Silent L.TUnit hd') thunked_tau')
#pop-options

(*** Translation correctness *)

(**** Helpers *)

let rec l_step_rec (e: L.exp) (fuel: nat) : Tot (option L.exp) (decreases fuel) =
  match L.step e with
  | None -> None
  | Some e' -> if fuel = 0 then Some e' else l_step_rec e' (fuel - 1)

let multiple_l_steps (e1: L.exp) (e2: L.exp) (n: nat) =  l_step_rec e1 n = Some e2

let not_l_value = e:L.exp{not (L.is_value e)}

let step_lift_commute_non_value (f:(L.exp -> not_l_value)) (e: L.exp) =
  if L.is_value e then true else
  match L.step e with
  | None -> L.step (f e) = None
  | Some e' -> L.step (f e) = Some (f e')

let is_stepping_agnostic_lift (f:(L.exp -> not_l_value)) = forall (e: L.exp).
  step_lift_commute_non_value f e

let stepping_agnostic_lift = f:(L.exp -> not_l_value){is_stepping_agnostic_lift f}

#push-options "--fuel 15 --ifuel 2 --z3rlimit 30"
let exceptions_lift
  (hd: D.exp)
  (just: D.exp)
  (cons: D.exp)
  (tau: D.ty)
    : Tot stepping_agnostic_lift
  =
  let tau' = translate_ty tau in
  let aux (tl: L.exp) : not_l_value =
      L.EMatchOption
       (L.EFoldLeft
         (process_exceptions_f tau')
         L.ENone (L.TOption tau')
         (L.EList (match tl with
          | L.EList l -> (translate_exp hd)::l
          | _ -> [translate_exp hd;tl]
         )) (L.TArrow L.TUnit tau'))
      tau'
        (L.EIf
          (translate_exp just)
          (translate_exp cons)
        (L.ELit (L.LError L.EmptyError)))
      (L.EAbs (L.Named 0) tau' (L.EVar 0))
   in
   let aux_lemma (e: L.exp) : Lemma (step_lift_commute_non_value aux e) =
     if L.is_value e then () else
     match e, L.step e with
     | L.EList l, _ -> admit()
     | _, None -> admit()
     | _, Some e' -> admit()
   in
   Classical.forall_intro aux_lemma;
   assert(is_stepping_agnostic_lift aux);
   aux

#push-options "--fuel 15 --ifuel 8 --z3rlimit 150"
let exceptions_lift_lemma
  (hd: D.exp)
  (just: D.exp)
  (cons: D.exp)
  (tau: D.ty)
  (tl: list D.exp)
    : Lemma (
      exceptions_lift hd just cons tau (build_default_translation tl just cons tau) ==
      build_default_translation (hd::tl) just cons tau)
  =
  admit()
#pop-options

let rec l_values_dont_step (e: L.exp) : Lemma
    (requires (L.is_value e))
    (ensures (L.step e = None))
    (decreases %[e; 1])
  =
  match e with
  | L.EAbs _ _ _ -> ()
  | L.ELit _ -> ()
  | L.ENone -> ()
  | L.EList [] -> ()
  | L.EList l -> l_values_dont_step_list e l
  | _ -> ()
and l_values_dont_step_list (e: L.exp) (l: list L.exp{l << e /\ Cons? l}) : Lemma
    (requires (L.is_value_list l))
    (ensures (L.step_list e l = (if l = [] then Some [] else None)))
    (decreases %[e; 0; l])
  =
  match l with
  | [hd] -> l_values_dont_step hd
  | hd::tl ->
    l_values_dont_step hd;
    l_values_dont_step_list e tl

#push-options "--z3rlimit 50 --fuel 1 --ifuel 1"
let rec lift_multiple_l_steps
  (e1: L.exp)
  (e2: L.exp)
  (n: nat)
  (f : stepping_agnostic_lift)
    : Lemma
      (requires (multiple_l_steps e1 e2 n))
      (ensures (multiple_l_steps (f e1) (f e2) n))
      (decreases n)
  =
  match L.step e1 with
  | None -> ()
  | Some e1' ->
    if L.is_value e1 then begin
      l_values_dont_step e1
    end else if n = 0 then
      assert(L.step (f e1) = Some (f e2))
    else lift_multiple_l_steps e1' e2 (n-1) f
#pop-options

#push-options "--fuel 9 --ifuel 0"
let process_exceptions_untouched_by_subst (x: L.var_name) (e: L.exp) (tau: L.ty) : Lemma
    (L.subst x e (process_exceptions_f tau) == process_exceptions_f tau)
  =
  ()
#pop-options

#push-options "--fuel 3 --ifuel 1 --z3rlimit 50"
let rec substitution_correctness (x: D.var) (e_x e: D.exp)
    : Lemma (ensures (
      translate_exp (D.subst x e_x e) == L.subst x (translate_exp e_x) (translate_exp e)))
      (decreases %[e; 1])
  =
  match e with
  | D.EVar y -> ()
  | D.ELit _ -> ()
  | D.EIf e1 e2 e3 ->
    substitution_correctness x e_x e1;
    substitution_correctness x e_x e2;
    substitution_correctness x e_x e3
  | D.EAbs _ _ body ->
    substitution_correctness x e_x body
  | D.EApp e1 e2 _ ->
    substitution_correctness x e_x e1;
    substitution_correctness x e_x e2
  | D.EDefault exceptions just cons tau ->
    substitution_correctness x e_x just;
    substitution_correctness x e_x cons;
    substitution_correctness_list x e_x e exceptions;
    process_exceptions_untouched_by_subst x (translate_exp e_x) (translate_ty tau)
and substitution_correctness_list (x: D.var) (e_x: D.exp) (e: D.exp) (l: list D.exp{l << e})
    : Lemma (ensures (
      translate_exp_list (D.subst_list x e_x l) ==
      L.subst_list x (translate_exp e_x) (translate_exp_list l)))
      (decreases %[e; 0; l])
 =
 match l with
 | [] -> ()
 | hd::tl ->
   substitution_correctness x e_x hd;
   substitution_correctness_list x e_x e tl
#pop-options

(**** Main theorems *)

let translation_correctness_value (e: D.exp) : Lemma
    ((D.is_value e) <==> (L.is_value (translate_exp e)))
  = ()

#push-options "--fuel 2 --ifuel 1 --z3rlimit 50"
let rec translation_correctness_step (e: D.exp) : Pure nat
    (requires (Some? (D.step e)))
    (ensures (fun n -> multiple_l_steps (translate_exp e) (translate_exp (Some?.v (D.step e))) n))
    (decreases %[e; 2])
  =
  let e' = translate_exp e in
  let stepped_e = Some?.v (D.step e) in
  let stepped_e' = translate_exp stepped_e in
  match e with
  | D.EVar _ -> 0
  | D.ELit _ -> 0
  | D.EAbs _ _ _ -> 0
  | D.EIf e1 e2 e3 ->
     let e1' = translate_exp e1 in
     let e2' = translate_exp e2 in
     let e3' = translate_exp e3 in
     if not (D.is_value e1) then begin
       let stepped_e1 = Some?.v (D.step e1) in
       let stepped_e1' = translate_exp stepped_e1 in
       let n_e1 = translation_correctness_step e1 in
       lift_multiple_l_steps e1' stepped_e1' n_e1 (fun e1' -> L.EIf e1' e2' e3');
       n_e1
     end else 0
  | D.EApp e1 e2 tau_arg ->
    let e1' = translate_exp e1 in
    let e2' = translate_exp e2 in
    if not (D.is_value e1) then begin
       let stepped_e1 = Some?.v (D.step e1) in
       let stepped_e1' = translate_exp stepped_e1 in
       let n_e1 = translation_correctness_step e1 in
       lift_multiple_l_steps e1' stepped_e1' n_e1 (fun e1' -> L.EApp e1' e2' (translate_ty tau_arg));
       n_e1
    end else begin match e1 with
      | D.ELit D.LConflictError -> 0
      | D.ELit D.LEmptyError -> 0
      | _ ->
        if not (D.is_value e2) then begin
        let stepped_e2 = Some?.v (D.step e2) in
        let stepped_e2' = translate_exp stepped_e2 in
        let n_e2 = translation_correctness_step e2 in
        lift_multiple_l_steps e2' stepped_e2' n_e2 (fun e2' -> L.EApp e1' e2' (translate_ty tau_arg));
        n_e2
      end else begin
        match e1, e2 with
        | _, D.ELit D.LConflictError -> 0
        | _, D.ELit D.LEmptyError -> 0
        | D.EAbs x1 t1 body, e2 ->
          substitution_correctness x1 e2 body;
          0
      end
    end
  | D.EDefault exceptions just cons tau ->  begin
    match D.step_exceptions e exceptions just cons tau with
    | Some e' ->
       translation_correctness_exceptions_step e exceptions just cons tau
    | None -> admit()
  end

and translation_correctness_exceptions_step
  (e: D.exp)
  (exceptions: list D.exp {exceptions << e})
  (just: D.exp{just << e})
  (cons: D.exp{cons << e})
  (tau: D.ty)
    : Pure nat
      (requires (
        e == D.EDefault exceptions just cons tau /\ Some? (D.step e) /\
        Some? (D.step_exceptions e exceptions just cons tau)
      ))
      (ensures (fun n -> multiple_l_steps (translate_exp e) (translate_exp (Some?.v (D.step e))) n))
      (decreases %[e; 1])
  =
  if List.Tot.for_all (fun except -> D.is_value except) exceptions then
    admit()
  else translation_correctness_exceptions_left_to_right_step e exceptions just cons tau

and translation_correctness_exceptions_left_to_right_step
  (e: D.exp)
  (exceptions: list D.exp {exceptions << e})
  (just: D.exp{just << e})
  (cons: D.exp{cons << e})
  (tau: D.ty)
    : Pure nat
      (requires (
        Some? (D.step_exceptions_left_to_right e exceptions just cons tau)
      ))
      (ensures (fun n ->
        multiple_l_steps
          (build_default_translation exceptions just cons tau)
          (translate_exp
            (Some?.v (D.step_exceptions_left_to_right e exceptions just cons tau))) n
      ))
      (decreases %[e; 0; exceptions])
  =
  match exceptions with
  | [] -> 0
  | hd::tl ->
    if D.is_value hd then begin
      match D.step_exceptions_left_to_right e tl just cons tau with
      | Some (D.ELit D.LConflictError) -> admit()
      | Some (D.EDefault tl' just' cons' tau') ->
        assume(just = just' /\ cons = cons' /\ tau = tau');
        let n_tl = translation_correctness_exceptions_left_to_right_step e tl just cons tau in
        assert(multiple_l_steps
          (build_default_translation tl just cons tau)
          (build_default_translation tl' just cons tau) n_tl);
        lift_multiple_l_steps
          (build_default_translation tl just cons tau)
          (build_default_translation tl' just cons tau)
          n_tl (exceptions_lift hd cons just tau);
        exceptions_lift_lemma hd just cons tau tl;
        exceptions_lift_lemma hd just cons tau tl';
        assert(multiple_l_steps
          (build_default_translation (hd::tl) just cons tau)
          (build_default_translation (hd::tl') just cons tau) n_tl);
        n_tl
      | None -> 0
    end else begin
      admit()
    end

(*** Wrap-up theorem  *)

let translation_correctness (e: D.exp) (tau: D.ty)
    : Lemma
      (requires (D.typing D.empty e tau))
      (ensures (
        let e' = translate_exp e in
        let tau' = translate_ty tau in
        L.typing L.empty e' tau' /\ begin
          if D.is_value e then L.is_value e' else begin
            D.progress e tau;
            let stepped_e = Some?.v (D.step e) in
            let stepped_e' = translate_exp stepped_e in
            exists (n:nat). multiple_l_steps e' stepped_e' n
          end
        end
      ))
 =
 let e' = translate_exp e in
 let tau' = translate_ty tau in
 translation_preserves_typ D.empty e tau;
 translate_empty_is_empty ();
 if D.is_value e then translation_correctness_value e else begin
    D.progress e tau;
    let n = translation_correctness_step e in
    ()
 end
