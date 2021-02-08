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

and translate_exp_list (l: list D.exp) : Tot (list L.exp) =
  match l with
  | [] -> []
  | hd::tl -> (L.EAbs L.Silent L.TUnit (translate_exp hd))::(translate_exp_list tl)

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
  match L.step e, L.is_value e with
  | None, false -> L.step (f e) = None
  | Some e', false -> if L.is_value e' then true else L.step (f e) = Some (f e')
  | _ -> true

let is_stepping_agnostic_lift (f:(L.exp -> not_l_value)) = forall (e: L.exp).
  step_lift_commute_non_value f e

let stepping_agnostic_lift = f:(L.exp -> not_l_value){is_stepping_agnostic_lift f}

let if_lift (e2 e3: L.exp) : stepping_agnostic_lift =
  let f : L.exp -> not_l_value = fun (e1: L.exp) -> L.EIf e1 e2 e3 in
  let aux (e1: L.exp) : Lemma (step_lift_commute_non_value f e1) =
    match L.step e1 with
    | None -> ()
    | Some e' -> ()
  in
  Classical.forall_intro aux;
  f

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
    if L.is_value e1' then begin
      admit()
    end else if n = 0 then
      assert(L.step (f e1) = Some (f e2))
    else lift_multiple_l_steps e1' e2 (n-1) f
#pop-options
(**** Main theorems *)

let translation_correctness_value (e: D.exp) : Lemma
    ((D.is_value e) <==> (L.is_value (translate_exp e)))
  = ()

#push-options "--fuel 2 --ifuel 1 --z3rlimit 50"
let rec translation_correctness_step (e: D.exp) : Pure nat
    (requires (Some? (D.step e)))
    (ensures (fun n -> multiple_l_steps (translate_exp e) (translate_exp (Some?.v (D.step e))) n))
  =
  let e' = translate_exp e in
  let stepped_e = Some?.v (D.step e) in
  let stepped_e' = translate_exp stepped_e in
  match e with
  | D.EVar _ -> 0
  | D.ELit _ -> 0
  | D.EAbs _ _ _ -> 0
  | D.EIf e1 e2 e3 ->
     if not (D.is_value e1) then begin
       let e1' = translate_exp e1 in
       let e2' = translate_exp e2 in
       let e3' = translate_exp e3 in
       let stepped_e1 = Some?.v (D.step e1) in
       let stepped_e1' = translate_exp stepped_e1 in
       let n_e1 = translation_correctness_step e1 in
       lift_multiple_l_steps e1' stepped_e1' n_e1 (fun e1' -> L.EIf e1' e2' e3');
       n_e1
     end else admit()
  | _ -> admit()

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
