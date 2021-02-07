module Catala.Translation

module L = Catala.LambdaCalculus
module D = Catala.DefaultCalculus

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
  L.EAbs a (L.TOption tau) (L.EAbs e (L.TArrow L.TUnit tau) (
    L.EApp (L.EAbs e' (L.TOption tau) (
      L.EMatchOption (L.EVar a) tau
        (L.EVar e')
        (L.EAbs a' tau (
          L.EMatchOption (L.EVar e') tau
            (L.EVar a)
            (L.EAbs 4 tau (L.ELit (L.LError L.ConflictError)))
        ))
    ))
    (L.ECatchEmptyError (L.ESome (L.EApp (L.EVar e) (L.ELit L.LUnit) L.TUnit)) L.ENone)
    tau
  ))

let typ_process_exceptions_f (tau: L.ty)
    : Lemma (L.typing L.empty (process_exceptions_f tau)
      (L.TArrow (L.TOption tau) (L.TArrow (L.TArrow L.TUnit tau) (L.TOption tau))))
  =
  let open FStar.Tactics in
  assert(L.typing L.empty (process_exceptions_f tau)
      (L.TArrow (L.TOption tau) (L.TArrow (L.TArrow L.TUnit tau) (L.TOption tau)))) by begin
    compute ();
    fail "Coucou"
  end;
  admit()

let rec translate_exp (e: D.exp) : Tot L.exp = match e with
  | D.EVar x -> L.EVar x
  | D.EApp e1 e2 tau_arg -> L.EApp (translate_exp e1) (translate_exp e2) (translate_ty tau_arg)
  | D.EAbs x ty body -> L.EAbs x (translate_ty ty) (translate_exp body)
  | D.ELit l -> L.ELit (translate_lit l)
  | D.EIf e1 e2 e3 -> L.EIf (translate_exp e1) (translate_exp e2) (translate_exp e3)
  | _ -> admit()
