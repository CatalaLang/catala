module Catala.LambdaCalculus
open FStar.Mul

module T = FStar.Tactics


/// This whole proof is inspired by FStar/examples/metatheory/StlcStrongdbparsubst.fst

(*** Syntax *)

type ty =
  | TBool : ty
  | TUnit : ty
  | TArrow : tin: ty -> tout: ty -> ty
  | TList: elts:ty -> ty
  | TOption: a: ty -> ty

type var = nat

type err =
  | EmptyError : err
  | ConflictError : err

type lit =
  | LError : err:err -> lit
  | LTrue : lit
  | LFalse : lit
  | LUnit : lit

type exp =
  | EVar : v: var -> exp
  | EApp : fn: exp -> arg: exp -> tau_arg: ty -> exp
  | EAbs : vty: ty -> body: exp -> exp
  | EThunk : body:exp -> exp
  | ELit : l: lit -> exp
  | EIf : test: exp -> btrue: exp -> bfalse: exp -> exp
  | ESome : s:exp -> exp
  | ENone : exp
  | EMatchOption : arg:exp -> tau_some: ty ->  none:exp -> some:exp -> exp
  | EList : l:list exp -> exp
  | ECatchEmptyError: to_try:exp -> catch_with:exp -> exp
  | EFoldLeft : f:exp -> init:exp -> tau_init:ty -> l:exp -> tau_elt:ty -> exp

(*** Operational semantics *)

(**** Helpers *)

let c_err = ELit (LError ConflictError)

let e_err = ELit (LError EmptyError)

val is_value: exp -> Tot bool
let rec is_value e =
  match e with
  | EAbs _ _ | EThunk _ | ELit _ | ENone -> true
  | ESome (ELit (LError _)) -> false
  | ESome e' -> is_value e'
  | EList l -> is_value_list l
  | _ -> false
and is_value_list (es: list exp) : Tot bool =
  match es with
  | [] -> true
  | hd::tl -> is_value hd && is_value_list tl


let var_to_exp = var -> Tot exp

let is_renaming_prop (s: var_to_exp) : prop = forall (x: var). EVar? (s x)

let is_renaming_size (s: var_to_exp)
    : GTot (n:int{(is_renaming_prop s) ==> n = 0 /\ (~(is_renaming_prop s) ==> n = 1)})
  =
  if FStar.StrongExcludedMiddle.strong_excluded_middle (is_renaming_prop s) then 0 else 1

let increment : var_to_exp = fun y -> EVar (y + 1)

let increment_is_renaming (_: unit) : Lemma (is_renaming_prop increment) = ()

let is_var_size (e: exp) : int = if EVar? e then 0 else 1


let rec subst (s: var_to_exp) (e: exp) : Pure exp
      (requires True)
      (ensures (fun e' -> (is_renaming_prop s /\ EVar? e) ==> EVar? e'))
      (decreases %[is_var_size e; is_renaming_size s; 1; e])
  =
  match e with
  | EVar x -> s x
  | EAbs t e1 -> EAbs t (subst (subst_abs s) e1)
  | EThunk e1 -> EThunk (subst s e1)
  | EApp e1 e2 tau_arg -> EApp (subst s e1) (subst s e2) tau_arg
  | ELit l -> ELit l
  | EIf e1 e2 e3 -> EIf (subst s e1) (subst s e2) (subst s e3)
  | ESome e1 -> ESome (subst s e1)
  | ENone -> ENone
  | EMatchOption arg tau_some none some ->
    EMatchOption (subst s arg) tau_some (subst s none) (subst s some)
  | EList l -> EList (subst_list s l)
  | ECatchEmptyError to_try catch_with ->
    ECatchEmptyError (subst s to_try) (subst s catch_with)
  | EFoldLeft f init tau_init l tau_elt ->
    EFoldLeft (subst s f) (subst s init) tau_init (subst s l) tau_elt
and subst_list (s: var_to_exp) (l: list exp) : Tot (list exp)
      (decreases %[1; is_renaming_size s; 1; l]) =
  match l with
  | [] -> []
  | hd :: tl -> (subst s hd) :: (subst_list s tl)
and subst_abs (s: var_to_exp) (y: var) : Tot (e':exp{is_renaming_prop s ==> EVar? e'})
      (decreases %[1; is_renaming_size s; 0])
  =
  if y = 0 then EVar y else subst increment (s (y -1))

let var_to_exp_beta (v: exp) : Tot var_to_exp = fun y ->
  if y = 0 then v else (EVar (y - 1))


(**** Stepping judgment *)

type list_step_result =
  | Good of list exp
  | Error of exp
  | Bad

let is_not_bad (l: list_step_result) : bool = match l with
  | Good _ | Error _ -> true | Bad -> false

let rec step_app (e: exp) (e1: exp{e1 << e}) (e2: exp{e2 << e}) (tau_arg: ty{tau_arg << e})
    : Tot (option exp) (decreases %[ e; 0 ]) =
  if is_value e1
  then
    match e1 with
    | ELit (LError err) -> Some (ELit (LError err))
    | _ ->
      if is_value e2
      then
        match e2 with
        | ELit (LError err) -> Some (ELit (LError err))
        | _ -> begin
          match e1 with
          | EAbs t e' -> Some (subst (var_to_exp_beta e2) e') (* D-Beta *)
          | EThunk e' -> Some e'
          | _ -> None
        end
      else
        (match step e2 with
          | Some e2' -> Some (EApp e1 e2' tau_arg) (* D-Context *)
          | None -> None)
  else
    (match step e1 with
      | Some e1' -> Some (EApp e1' e2 tau_arg) (* D-Context *)
      | None -> None)

and step_if (e: exp) (e1: exp{e1 << e}) (e2: exp{e2 << e}) (e3: exp{e3 << e})
    : Tot (option exp) (decreases %[ e; 1 ]) =
  if is_value e1
  then
    match e1 with
    | ELit (LError err) -> Some (ELit (LError err))
    | ELit LTrue -> Some e2
    | ELit LFalse -> Some e3
    | _ -> None
  else
    match (step e1) with
    | Some e1' -> Some (EIf e1' e2 e3)
    | None -> None

and step_match
  (e: exp)
  (arg: exp{arg << e})
  (tau_some: ty)
  (none: exp{none << e})
  (some: exp{some << e})
    : Tot (option exp) (decreases %[ e; 2 ]) =
  if is_value arg
  then
    match arg with
    | ENone -> Some none
    | ESome s -> Some (EApp some s tau_some)
    | ELit (LError err) -> Some (ELit (LError err))
    | _ -> None
  else
    match (step arg) with
    | Some arg' -> Some (EMatchOption arg' tau_some none some) (* D-Context *)
    | None -> None

and step_list
  (e: exp)
  (l: list exp{l << e})
    : Tot (list_step_result) (decreases %[ e; 3; l ]) =
  match l with
  | [] -> Bad
  | [hd] -> begin
    if is_value hd then Bad else
    match step hd with
    | None -> Bad
    | Some (ELit (LError err)) -> Error (ELit (LError err))
    | Some hd' -> Good ([hd'])
  end
  | hd::tl -> begin
    if is_value hd then
      match step_list e tl with
      | Bad -> Bad
      | Error err -> Error err
      | Good tl' -> Good (hd::tl')
    else
      match step hd with
      | None -> Bad
      | Some (ELit (LError err)) -> Error (ELit (LError err))
      | Some hd' -> Good (hd'::tl)
  end

and step_catch
  (e: exp)
  (to_try: exp{to_try << e})
  (catch_with: exp{catch_with << e})
    : Tot (option exp) (decreases %[ e; 4 ]) =
  if is_value to_try then
    match to_try with
    | ELit (LError EmptyError) -> Some catch_with
    | _ -> Some to_try
  else
    match step to_try with
    | None -> None
    | Some to_try' -> Some (ECatchEmptyError to_try' catch_with)

and step_fold_left
  (e: exp)
  (f: exp{f << e})
  (init: exp{init << e})
  (tau_init: ty)
  (l: exp{l << e})
  (tau_elt: ty)
    : Tot (option exp) (decreases %[ e; 5; l ]) =
  match is_value f, is_value init, is_value l with
  | false, _, _ -> begin
    match step f with
    | None -> None
    | Some f' -> Some (EFoldLeft f' init tau_init l tau_elt)
  end
  | true, false, _ -> begin
    match step init with
    | None -> None
    | Some init' -> Some (EFoldLeft f init' tau_init l tau_elt)
  end
  | true, true, false -> begin
    match step l with
    | None -> None
    | Some l' -> Some (EFoldLeft f init tau_init l' tau_elt)
  end
  | true, true, true -> begin
    match f, init, l with
    | ELit (LError err), _ , _
    | _, ELit (LError err), _
    | _, _, ELit (LError err) -> Some (ELit (LError err))
    | _, _, EList [] -> Some init
    | _, _, EList (hd::tl) ->
      Some (EFoldLeft
        f (EApp (EApp f init tau_init) hd tau_elt)
        tau_init (EList tl) tau_elt
      )
    | _ -> None
  end

and step (e: exp) : Tot (option exp) (decreases %[ e; 6 ]) =
  match e with
  | EApp e1 e2 tau_arg -> step_app e e1 e2 tau_arg
  | EIf e1 e2 e3 -> step_if e e1 e2 e3
  | ESome e1 ->
    if is_value e1 then
      match e1 with
      | ELit (LError err) -> Some (ELit (LError err))
      | _ -> None
    else begin
      match step e1 with
      | None -> None
      | Some e1' -> Some (ESome e1')
    end
  | EMatchOption arg tau_some none some -> step_match e arg tau_some none some
  | EList l -> begin match step_list e l with
    | Bad -> None
    | Error err -> Some err
    | Good l' -> Some (EList l')
  end
  | ECatchEmptyError to_try catch_with -> step_catch e to_try catch_with
  | EFoldLeft f init tau_init l tau_elt -> step_fold_left e f init tau_init l tau_elt
  | _ -> None

(*** Typing *)

(**** Typing helpers *)

type env = FunctionalExtensionality.restricted_t var (fun _ -> option ty)

val empty:env
let empty = FunctionalExtensionality.on_dom var (fun _ -> None)

val extend: env -> ty -> Tot env
let extend g t = FunctionalExtensionality.on_dom var
  (fun x' -> if 0 = x' then Some t else g (x' - 1))

(**** Typing judgment *)

let rec typing (g: env) (e: exp) (tau: ty) : Tot bool (decreases (e)) =
  match e with
  | EVar x -> g x = Some tau
  | EAbs t e1 ->
    (match tau with
      | TArrow tau_in tau_out -> t = tau_in &&
        typing (extend g t) e1 tau_out
      | _ -> false)
  | EThunk e1 ->
    (match tau with
      | TArrow TUnit tau_out -> typing g e1 tau_out
      | _ -> false)
  | EApp e1 e2 tau_arg -> typing g e1 (TArrow tau_arg tau) && typing g e2 tau_arg
  | ELit LTrue -> tau = TBool
  | ELit LFalse -> tau = TBool
  | ELit LUnit -> tau = TUnit
  | ELit (LError _) -> true
  | EIf e1 e2 e3 -> typing g e1 TBool && typing g e2 tau && typing g e3 tau
  | ESome e1 -> begin
    match tau with
    | TOption t' -> typing g e1 t'
    | _ -> false
  end
  | ENone -> begin
    match tau with
    | TOption _ -> true
    | _ -> false
  end
  | EMatchOption arg tau_some none some ->
    typing g arg (TOption tau_some) &&
    typing g none tau &&
    typing g some (TArrow tau_some tau)
  | EList l -> begin
    match tau with
    | TList tau' -> typing_list g l tau'
    | _ -> false
  end
  | ECatchEmptyError to_try catch_with ->
    typing g to_try tau &&
    typing g catch_with tau
  | EFoldLeft f init tau_init l tau_elt ->
    tau_init = tau &&
    typing g l (TList tau_elt) &&
    typing g init tau &&
    typing g f (TArrow tau (TArrow tau_elt tau))
  | _ -> false
and typing_list (g: env) (subs: list exp) (tau: ty) : Tot bool (decreases (subs)) =
  match subs with
  | [] -> true
  | hd :: tl -> typing g hd tau && typing_list g tl tau

(*** Progress *)

(**** Progress lemmas *)

let is_bool_value_cannot_be_abs (g: env) (e: exp)
    : Lemma (requires (is_value e /\ (typing g e TBool)))
      (ensures
        (match e with
          | ELit LUnit -> False
          | ELit _ -> True
          | _ -> False)) = ()

let typing_conserved_by_list_reduction (g: env) (subs: list exp) (tau: ty)
    : Lemma (requires ((typing_list g subs tau)))
      (ensures (Cons? subs ==> (typing_list g (Cons?.tl subs) tau))) = ()

(**** Progress theorem *)

let rec size_for_progress (e: exp) : Tot pos =
  match e with
  | EVar _ -> 1
  | EApp fn arg _ -> size_for_progress fn + size_for_progress arg + 1
  | EAbs _ body -> size_for_progress body + 1
  | EThunk body -> size_for_progress body + 1
  | ELit _ -> 1
  | EIf e1 e2 e3 -> size_for_progress e1 + size_for_progress e2 + size_for_progress e3 + 1
  | ESome s -> size_for_progress s + 1
  | ENone -> 1
  | EMatchOption arg _ none some ->
    size_for_progress arg + size_for_progress none + size_for_progress some + 1
  | EList l -> size_for_progress_list l + 1
  | ECatchEmptyError e1 e2 -> size_for_progress e1 + size_for_progress e2 + 1
  | EFoldLeft f init _ l _ ->
    size_for_progress f + size_for_progress init + size_for_progress l +
    (size_for_progress f) * (size_for_progress l) + 1
and size_for_progress_list (e: list exp) : Tot nat = match e with
  | [] -> 0
  | hd::tl -> size_for_progress hd + size_for_progress_list tl + 10

#restart-solver

#push-options "--fuel 3 --ifuel 0 --z3rlimit 30 --quake 10/1"
let lemma_size_fold_step (f init hd: exp) (tl: list exp) (tau_init tau_elt: ty) : Lemma (
  size_for_progress (EFoldLeft f init tau_init (EList (hd::tl)) tau_elt) >
  size_for_progress (EFoldLeft f
    (EApp (EApp f init tau_init) hd tau_elt) tau_init (EList tl) tau_elt)
) =
  let e = EFoldLeft f init tau_init (EList (hd::tl)) tau_elt in
  let e' = EFoldLeft f (EApp (EApp f init tau_init) hd tau_elt) tau_init (EList tl) tau_elt in
  assert(size_for_progress e =
    size_for_progress f +
    size_for_progress init +
    (size_for_progress hd + size_for_progress_list tl + 10 + 1) +
    (size_for_progress f) * (size_for_progress hd + size_for_progress_list tl + 10 + 1) +
    1);
  assert(size_for_progress e' =
    size_for_progress f +
    (size_for_progress f + size_for_progress init + 1) +
    (size_for_progress hd + 1) +
    (size_for_progress_list tl + 1) +
    (size_for_progress f) * (size_for_progress_list tl + 1) +
    1);
  assert(size_for_progress e - size_for_progress e' =
    - size_for_progress f - 1 - size_for_progress hd - 1
    + size_for_progress hd + 10
    + (size_for_progress f) * (size_for_progress hd + 10)
  );
  assert(size_for_progress e - size_for_progress e' >
  - size_for_progress f + 7
    + (size_for_progress f) * (size_for_progress hd + 10));
  assert(size_for_progress e - size_for_progress e' > 7
    + (size_for_progress f) * (size_for_progress hd + 9))
#pop-options

#push-options "--fuel 1 --ifuel 0 --z3rlimit 30"
let lemma_size_fold_f (f init l: exp) (tau_init tau_elt: ty)
    : Lemma (size_for_progress (EFoldLeft f init tau_init l tau_elt) > size_for_progress f)
  =
  ()
#pop-options

#push-options "--fuel 1 --ifuel 0 --z3rlimit 30"
let lemma_size_fold_init (f init l: exp) (tau_init tau_elt: ty)
    : Lemma (size_for_progress (EFoldLeft f init tau_init l tau_elt) > size_for_progress init)
  =
  ()
#pop-options

#push-options "--fuel 1 --ifuel 0 --z3rlimit 30"
let lemma_size_fold_l (f init l: exp) (tau_init tau_elt: ty)
    : Lemma (size_for_progress (EFoldLeft f init tau_init l tau_elt) > size_for_progress l)
  =
  ()
#pop-options

#push-options "--fuel 3 --ifuel 2 --z3rlimit 50"
let rec progress (e: exp) (tau: ty)
    : Lemma (requires (typing empty e tau))
      (ensures (is_value e \/ (Some? (step e))))
      (decreases %[size_for_progress e; 3]) =
  match e with
  | EApp e1 e2 tau_arg ->
    progress e1 (TArrow tau_arg tau);
    progress e2 tau_arg
  | EIf e1 e2 e3 ->
    progress e1 TBool;
    progress e2 tau;
    progress e3 tau;
    if is_value e1 then is_bool_value_cannot_be_abs empty e1
  | ESome s -> begin
    match tau with
    | TOption tau' -> progress s tau'
    | _ -> ()
  end
  | ENone -> ()
  | EMatchOption arg tau_some none some -> begin
    progress arg (TOption tau_some);
    progress none tau;
    progress some (TArrow tau_some tau);
    if is_value arg then
      match arg with
      | ESome s ->
        let result_exp = EApp some s tau_some in
        progress result_exp tau
      | _ -> ()
    else ()
  end
  | EList [] -> ()
  | EList l -> begin
    match tau with
    | TList tau' ->
      progress_list e l tau'
    | _ -> ()
  end
  | ECatchEmptyError to_try catch_with ->
    progress to_try tau;
    progress catch_with tau
  | EFoldLeft f init tau_init l tau_elt -> begin
     match is_value f, is_value init, is_value l with
     | false, _, _ ->
       lemma_size_fold_f f init l tau_init tau_elt;
       progress f (TArrow tau_init (TArrow tau_elt tau_init))
     | true, false, _ ->
       lemma_size_fold_init f init l tau_init tau_elt;
       progress init tau_init
     | true, true, false ->
       lemma_size_fold_l f init l tau_init tau_elt;
       progress l (TList tau_elt)
     | true, true, true -> begin
       match l with
       | EList [] -> ()
       | EList (hd::tl) ->
         let result_exp = EFoldLeft
          f (EApp (EApp f init tau_init) hd tau_elt)
          tau_init (EList tl) tau_elt
         in
         lemma_size_fold_step f init hd tl tau_init tau_elt;
         progress result_exp tau
       | _ -> ()
     end
  end
  | _ -> ()
and progress_list
  (e: exp)
  (l: list exp{size_for_progress_list l < size_for_progress e /\ l << e}) (tau: ty)
    : Lemma (requires (typing_list empty l tau /\ Cons? l))
      (ensures (is_value_list l \/ (is_not_bad (step_list e l))))
      (decreases %[size_for_progress e; 2; l])
  =
  match l with
  | [hd] -> if is_value hd then () else progress hd tau
  | hd::tl ->
    if is_value hd then progress_list e tl tau else progress hd tau
#pop-options

(*** Preservation *)

(**** Preservation helpers *)


let rec substitution_extensionnal
  (s1: var_to_exp)
  (s2: var_to_exp{FStar.FunctionalExtensionality.feq s1 s2})
  (e: exp)
    : Lemma
      (requires (True))
      (ensures (subst s1 e == subst s2 e))
      [SMTPat (subst s1 e); SMTPat (subst s2 e)]
  =
  match e with
  | EVar _ -> ()
  | ELit _ -> ()
  | EThunk e1 -> substitution_extensionnal s1 s2 e1
  | EAbs t e1 ->
    assert (subst s1 (EAbs t e1) == EAbs t (subst (subst_abs s1) e1))
      by (T.norm [zeta; iota; delta_only [`%subst]]);
    assert (subst s2 (EAbs t e1) == EAbs t (subst (subst_abs s2) e1))
      by (T.norm [zeta; iota; delta_only [`%subst]]; T.smt ());
    substitution_extensionnal (subst_abs s1) (subst_abs s2) e1
  | EApp e1 e2 _ ->
    substitution_extensionnal s1 s2 e1;
    substitution_extensionnal s1 s2 e2
  | EIf e1 e2 e3 ->
    substitution_extensionnal s1 s2 e1;
    substitution_extensionnal s1 s2 e2;
    substitution_extensionnal s1 s2 e3
  | ESome e1 -> substitution_extensionnal s1 s2 e1
  | ENone -> ()
  | EMatchOption arg _ none some ->
    substitution_extensionnal s1 s2 arg;
    substitution_extensionnal s1 s2 none;
    substitution_extensionnal s1 s2 some
  | EList l -> substitution_extensionnal_list s1 s2 l
  | ECatchEmptyError to_try catch_with ->
    substitution_extensionnal s1 s2 to_try;
    substitution_extensionnal s1 s2 catch_with
  | EFoldLeft f init _ l _ ->
    substitution_extensionnal s1 s2 f;
    substitution_extensionnal s1 s2 init;
    substitution_extensionnal s1 s2 l
and substitution_extensionnal_list
  (s1: var_to_exp)
  (s2: var_to_exp{FStar.FunctionalExtensionality.feq s1 s2})
  (l: list exp)
    : Lemma
      (requires (True))
      (ensures (subst_list s1 l == subst_list s2 l))
  =
  match l with
  | [] -> ()
  | hd::tl ->
    substitution_extensionnal s1 s2 hd;
    substitution_extensionnal_list s1 s2 tl

let subst_typing (s: var_to_exp) (g1: env) (g2: env) =
  (x:var) ->  Lemma
      (requires (Some? (g1 x)))
      (ensures (typing g2 (s x) (Some?.v (g1 x))))

let rec substitution_preserves_typing
  (g1: env)
  (e: exp)
  (t: ty)
  (s: var_to_exp)
  (g2: env)
  (s_lemma: subst_typing s g1 g2)
    : Lemma
      (requires (typing g1 e t))
      (ensures (typing g2 (subst s e) t))
      (decreases %[is_var_size e; is_renaming_size s; e])
  =
  match e with
  | EVar x -> s_lemma x
  | EApp e1 e2 t_arg ->
    substitution_preserves_typing g1 e1 (TArrow t_arg t) s g2 s_lemma;
    substitution_preserves_typing g1 e2 t_arg s g2 s_lemma
  | EThunk e1 -> begin
    match t with
    | TArrow TUnit t_out ->
      substitution_preserves_typing g1 e1 t_out s g2 s_lemma
    | _ -> ()
  end
  | EAbs t_arg e1 -> begin
    match t with
    | TArrow t_arg' t_out ->
      if t_arg' <> t_arg then () else
      let s_lemma' : subst_typing increment g2 (extend g2 t_arg) = fun x -> () in
      let s_lemma'' : subst_typing (subst_abs s) (extend g1 t_arg) (extend g2 t_arg) = fun y ->
        if y = 0 then () else
        let n: var = y - 1 in
        s_lemma n;
        assert(typing g2 (s n) (Some?.v (g1 n)));
        substitution_preserves_typing
          g2
          (s n)
          (Some?.v (g1 n))
          increment
          (extend g2 t_arg)
          s_lemma'
      in
      substitution_preserves_typing
        (extend g1 t_arg)
        e1
        t_out
        (subst_abs s)
        (extend g2 t_arg)
        s_lemma''
    | _ -> ()
  end
  | ELit _ -> ()
  | EIf e1 e2 e3 ->
    substitution_preserves_typing g1 e1 TBool s g2 s_lemma;
    substitution_preserves_typing g1 e2 t s g2 s_lemma;
    substitution_preserves_typing g1 e3 t s g2 s_lemma
  | ESome e1 -> begin
    match t with
    | TOption t' ->  substitution_preserves_typing g1 e1 t' s g2 s_lemma
    | _ -> ()
  end
  | ENone -> ()
  | EMatchOption arg tau_some none some ->
    substitution_preserves_typing g1 arg (TOption tau_some) s g2 s_lemma;
    substitution_preserves_typing g1 none t s g2 s_lemma;
    substitution_preserves_typing g1 some (TArrow tau_some t) s g2 s_lemma
  | EList l -> begin
    match t with
    | TList t' -> substitution_preserves_typing_list g1 l t' s g2 s_lemma
    | _ -> ()
  end
  | ECatchEmptyError to_try catch_with ->
    substitution_preserves_typing g1 to_try t s g2 s_lemma;
    substitution_preserves_typing g1 catch_with t s g2 s_lemma
  | EFoldLeft f init tau_init l tau_elt ->
    substitution_preserves_typing g1 f (TArrow tau_init (TArrow tau_elt tau_init)) s g2 s_lemma ;
    substitution_preserves_typing g1 init tau_init s g2 s_lemma;
    substitution_preserves_typing g1 l (TList tau_elt) s g2 s_lemma
and substitution_preserves_typing_list
  (g1: env)
  (l: list exp)
  (t: ty)
  (s: var_to_exp)
  (g2: env)
  (s_lemma: subst_typing s g1 g2)
    : Lemma
      (requires (typing_list g1 l t))
      (ensures (typing_list g2 (subst_list s l) t))
      (decreases %[1; is_renaming_size s; l])
  =
  match l with
  | [] -> ()
  | hd::tl ->
    substitution_preserves_typing g1 hd t s g2 s_lemma;
    substitution_preserves_typing_list g1 tl t s g2 s_lemma

(**** Preservation theorem *)

#push-options "--fuel 3 --ifuel 1 --z3rlimit 20"
let rec preservation (e: exp) (tau: ty)
    : Lemma (requires (typing empty e tau /\ Some? (step e)))
      (ensures (typing empty (Some?.v (step e)) tau))
      (decreases %[ e ]) =
  match e with
  | ELit _ -> ()
  | EVar _ -> ()
  | EIf e1 e2 e3 -> if not (is_value e1) then preservation e1 TBool
  | EApp e1 e2 tau_arg ->
    if is_value e1
    then
      match e1 with
      | ELit (LError _) -> ()
      | _ ->
        if is_value e2
        then
          match e1 with
          | EAbs  _ ebody ->
            let s_lemma : subst_typing (var_to_exp_beta e2) (extend empty tau_arg) empty =
              fun x -> ()
            in
            substitution_preserves_typing (extend empty tau_arg) ebody tau
              (var_to_exp_beta e2) empty s_lemma
          | _ -> ()
        else preservation e2 tau_arg
    else preservation e1 (TArrow tau_arg tau)
  | ENone -> ()
  | ESome s -> let TOption tau' = tau in if not (is_value s) then preservation s tau'
  | EMatchOption arg tau_some none some ->
    if not (is_value arg) then preservation arg (TOption tau_some)
  | EList [] -> ()
  | EList l -> let TList tau' = tau in preservation_list e l tau'
  | ECatchEmptyError to_try catch_with -> if not (is_value to_try) then preservation to_try tau
  | EFoldLeft f init tau_init l tau_elt -> begin
    match is_value f, is_value init, is_value l with
    | false, _, _ -> preservation f (TArrow tau_init (TArrow tau_elt tau_init))
    | true, false, _ -> preservation init tau_init
    | true, true, false -> preservation l (TList tau_elt)
    | true, true, true -> ()
  end
and preservation_list
      (e: exp)
      (l: list exp {l << e})
      (tau: ty)
    : Lemma
      (requires (
        Cons? l /\
        typing empty (EList l) (TList tau) /\
        is_not_bad (step_list e l)
      ))
      (ensures (
        match step_list e l with
        | Good l' -> typing_list empty l' tau
        | Error err -> typing empty err (TList tau)
      ))
      (decreases %[ l ]) =
  match l with
  | [hd] -> begin if is_value hd then () else preservation hd tau end
  | hd :: tl ->
    if is_value hd then begin
      typing_conserved_by_list_reduction empty l tau;
      preservation_list e tl tau
    end else preservation hd tau
#pop-options

(**** Other lemmas *)

let identity_var_to_exp : var_to_exp = fun x -> EVar x

#push-options "--fuel 3 --ifuel 2"
let rec subst_by_identity_is_identity (e: exp) : Lemma (subst identity_var_to_exp e == e) =
  match e with
  | EVar _ -> ()
  | ELit _ -> ()
  | EApp e1 e2 _ ->
    subst_by_identity_is_identity e1;
    subst_by_identity_is_identity e2
  | EThunk e1 ->
    subst_by_identity_is_identity e1
  | EAbs t e1 ->
    subst_by_identity_is_identity e1
  | ENone -> ()
  | ESome e1 ->
    subst_by_identity_is_identity e1
  | EIf e1 e2 e3 ->
    subst_by_identity_is_identity e1;
    subst_by_identity_is_identity e2;
    subst_by_identity_is_identity e3
  | EList l ->
    subst_by_identity_is_identity_list l
  | ECatchEmptyError to_try catch_with ->
    subst_by_identity_is_identity to_try;
    subst_by_identity_is_identity catch_with
  | EFoldLeft f init _ l _ ->
    subst_by_identity_is_identity f;
    subst_by_identity_is_identity init;
    subst_by_identity_is_identity l
  | EMatchOption arg _ none some ->
    subst_by_identity_is_identity arg;
    subst_by_identity_is_identity none;
    subst_by_identity_is_identity some
and subst_by_identity_is_identity_list (l: list exp) : Lemma (subst_list identity_var_to_exp l == l)
  =
  match l with
  | [] -> ()
  | hd::tl ->
    subst_by_identity_is_identity hd;
    subst_by_identity_is_identity_list tl
#pop-options

let typing_empty_can_be_extended (e: exp) (tau: ty) (g: env)
    : Lemma
      (requires (typing empty e tau))
      (ensures (typing g e tau))
  =
  subst_by_identity_is_identity e;
  let s_lemma : subst_typing identity_var_to_exp empty g = fun x -> () in
  substitution_preserves_typing empty e tau identity_var_to_exp g s_lemma

let is_error (e: exp) : bool = match e with ELit (LError _) -> true | _ -> false
