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

noeq type env' = {
  tys: D.env;
  max_free: nat
}
let env = g:env'{forall (i:nat{i > g.max_free}). g.tys i = None}

let extend (g: env) (x: D.var) (ty: D.ty) : env =
  let new_tys = D.extend g.tys x ty in
  let new_max_free = if x > g.max_free then x else g.max_free in
  { tys = new_tys; max_free = new_max_free }

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
