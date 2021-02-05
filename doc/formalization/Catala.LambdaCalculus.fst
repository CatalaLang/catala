module Catala.LambdaCalculus


module DCalc = Catala.DefaultCalculus

(*** Syntax *)

type ty =
  | TBool : ty
  | TUnit : ty
  | TArrow : tin: ty -> tout: ty -> ty
  | TList: elts:ty -> ty
  | TOption: a: ty -> ty

type var = DCalc.var

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
  | EAbs : v: var -> vty: ty -> body: exp -> exp
  | ELit : l: lit -> exp
  | EIf : test: exp -> btrue: exp -> bfalse: exp -> exp
  | ESome : s:exp -> exp
  | ENone : exp
  | EMatchOption : arg:exp -> tau_some: ty ->  none:exp -> some:exp -> exp
  | EList : l:list exp -> exp
  | ECatchEmptyError: to_try:exp -> catch_with:exp -> exp
  | EFoldLeft : f:exp -> init:exp -> l:exp -> tau_elt:ty -> exp

(*** Operational semantics *)

(**** Helpers *)

let c_err = ELit (LError ConflictError)

let e_err = ELit (LError EmptyError)

val is_value: exp -> Tot bool
let rec is_value e =
  match e with
  | EAbs _ _ _ | ELit _ | ENone -> true
  | ESome e' -> is_value e'
  | EList l -> is_value_list l
  | _ -> false
and is_value_list (es: list exp) : Tot bool =
  match es with
  | [] -> true
  | hd::tl -> is_value hd && is_value_list tl


let rec subst (x: var) (e_x e: exp) : Tot exp (decreases e) =
  match e with
  | EVar x' -> if x = x' then e_x else e
  | EAbs x' t e1 -> EAbs x' t (if x = x' then e1 else (subst x e_x e1))
  | EApp e1 e2 tau_arg -> EApp (subst x e_x e1) (subst x e_x e2) tau_arg
  | ELit l -> ELit l
  | EIf e1 e2 e3 -> EIf (subst x e_x e1) (subst x e_x e2) (subst x e_x e3)
  | ESome s -> ESome (subst x e_x s)
  | ENone -> ENone
  | EMatchOption arg tau_some none some ->
    EMatchOption (subst x e_x arg) tau_some (subst x e_x none) (subst x e_x some)
  | EList l -> EList (subst_list x e_x l)
  | ECatchEmptyError to_try catch_with ->
    ECatchEmptyError (subst x e_x to_try) (subst x e_x catch_with)
  | EFoldLeft f init l tau_elt ->
    EFoldLeft (subst x e_x f) (subst x e_x init) (subst x e_x l) tau_elt
and subst_list (x: var) (e_x: exp) (subs: list exp) : Tot (list exp) (decreases subs) =
  match subs with
  | [] -> []
  | hd :: tl -> (subst x e_x hd) :: (subst_list x e_x tl)

(**** Stepping judgment *)

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
          | EAbs x t e' -> Some (subst x e2 e') (* D-Beta *)
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
    | ENone -> Some none
    | _ -> None
  else
    match (step arg) with
    | Some arg' -> Some (EMatchOption arg' tau_some none some) (* D-Context *)
    | None -> None

and step_list (e: exp) (l: list exp{l << e}) : Tot (option (list exp)) (decreases %[ e; 3; l ]) =
  match l with
  | [] -> Some []
  | hd::tl -> begin
    if is_value hd then
      match step_list e tl with
      | None -> None
      | Some tl' -> Some (hd::tl')
    else
      match step hd with
      | None -> None
      | Some hd' -> Some(hd'::tl)
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

and step (e: exp) : Tot (option exp) (decreases %[ e; 5 ]) =
  match e with
  | EApp e1 e2 tau_arg -> step_app e e1 e2 tau_arg
  | EIf e1 e2 e3 -> step_if e e1 e2 e3
  | ESome e1 -> if is_value e1 then None else begin
    match step e1 with
    | None -> None
    | Some (ELit (LError err)) -> Some (ELit (LError err))
    | Some e1' -> Some (ESome e1')
  end
  | EMatchOption arg tau_some none some -> step_match e arg tau_some none some
  | EList l -> begin match step_list e l with
    | None -> None
    | Some l' -> Some (EList l')
  end
  | ECatchEmptyError to_try catch_with -> step_catch e to_try catch_with
  | _ -> None

(*** Typing *)

(**** Typing helpers *)

type env = var -> Tot (option ty)

val empty:env
let empty = fun _ -> None

val extend: env -> var -> ty -> Tot env
let extend g x t = fun x' -> if x = x' then Some t else g x'

(**** Typing judgment *)

let rec typing (g: env) (e: exp) (tau: ty) : Tot bool (decreases (e)) =
  match e with
  | EVar x -> g x = Some tau
  | EAbs x t e1 ->
    (match tau with
      | TArrow tau_in tau_out -> t = tau_in && typing (extend g x t) e1 tau_out
      | _ -> false)
  | EApp e1 e2 tau_arg -> typing g e1 (TArrow tau_arg tau) && typing g e2 tau_arg
  | ELit LTrue -> tau = TBool
  | ELit LFalse -> tau = TBool
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
  | EFoldLeft f init l tau_elt ->
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

#push-options "--fuel 2 --ifuel 1"
let rec progress (e: exp) (tau: ty)
    : Lemma (requires (typing empty e tau))
      (ensures (is_value e \/ (Some? (step e))))
      (decreases %[ e; 3 ]) =
  match e with
  | EApp e1 e2 tau_arg ->
    progress e1 (TArrow tau_arg tau);
    progress e2 tau_arg
  | EIf e1 e2 e3 ->
    progress e1 TBool;
    progress e2 tau;
    progress e3 tau;
    if is_value e1 then is_bool_value_cannot_be_abs empty e1
  | EDefault exceptions just cons -> progress_default e exceptions just cons tau
  | _ -> ()
and progress_default
      (e: exp)
      (exceptions: list exp {exceptions << e})
      (just: exp{just << e})
      (cons: exp{cons << e})
      (tau: ty)
    : Lemma (requires (~(is_value e) /\ e == EDefault exceptions just cons /\ (typing empty e tau)))
      (ensures (Some? (step_default e exceptions just cons)))
      (decreases %[ e; 2 ]) =
  match step_exceptions e exceptions just cons with
  | Some _ -> ()
  | None ->
    if is_value just then
      (is_bool_value_cannot_be_abs empty just;
        match just, cons with
        | ELit LTrue, ELit LEmptyError -> ()
        | ELit LTrue, _ -> progress cons tau
        | ELit LFalse, _ ->  ()
        | ELit LEmptyError, _ -> ()
        | ELit LConflictError, _ -> ())
    else progress just TBool
#pop-options

(*** Preservation *)

(**** Preservation helpers *)

let rec empty_count_preserves_type (acc: empty_count_result) (subs: list exp) (g: env) (tau: ty)
    : Lemma
      (requires
        (typing_list g subs tau /\
          (match acc with
            | OneNonEmpty e' -> typing g e' tau
            | _ -> True)))
      (ensures
        (match empty_count acc subs with
          | OneNonEmpty e' -> typing g e' tau
          | _ -> True))
      (decreases subs) =
  match subs with
  | [] -> ()
  | hd :: tl ->
    match (hd, acc) with
    | ELit LEmptyError, AllEmpty -> empty_count_preserves_type AllEmpty tl g tau
    | ELit LEmptyError, OneNonEmpty e -> empty_count_preserves_type (OneNonEmpty e) tl g tau
    | _, Conflict -> ()
    | _, AllEmpty -> empty_count_preserves_type (OneNonEmpty hd) tl g tau
    | _ -> ()

let rec appears_free_in (x: var) (e: exp) : Tot bool =
  match e with
  | EVar y -> x = y
  | EApp e1 e2 tau_arg -> appears_free_in x e1 || appears_free_in x e2
  | EAbs y _ e1 -> x <> y && appears_free_in x e1
  | EIf e1 e2 e3 -> appears_free_in x e1 || appears_free_in x e2 || appears_free_in x e3
  | EDefault exceptions ejust econs ->
    appears_free_in_list x exceptions || appears_free_in x ejust || appears_free_in x econs
  | ELit _ -> false
and appears_free_in_list (x: var) (subs: list exp) : Tot bool =
  match subs with
  | [] -> false
  | hd :: tl -> appears_free_in x hd || appears_free_in_list x tl

#push-options "--fuel 2 --ifuel 1"
let rec free_in_context (x: var) (e: exp) (g: env) (tau: ty)
    : Lemma (requires (typing g e tau))
      (ensures (appears_free_in x e ==> Some? (g x)))
      (decreases e) =
  match e with
  | EVar _ | ELit _ -> ()
  | EAbs y t e1 ->
    (match tau with | TArrow _ tau_out -> free_in_context x e1 (extend g y t) tau_out)
  | EApp e1 e2 tau_arg ->
    free_in_context x e1 g (TArrow tau_arg tau);
    free_in_context x e2 g tau_arg
  | EIf e1 e2 e3 ->
    free_in_context x e1 g TBool;
    free_in_context x e2 g tau;
    free_in_context x e3 g tau
  | EDefault exceptions ejust econs ->
    free_in_context x ejust g TBool;
    free_in_context x econs g tau;
    free_in_context_list x exceptions g tau
and free_in_context_list (x: var) (subs: list exp) (g: env) (tau: ty)
    : Lemma (requires (typing_list g subs tau))
      (ensures (appears_free_in_list x subs ==> Some? (g x)))
      (decreases subs) =
  match subs with
  | [] -> ()
  | hd :: tl ->
    free_in_context x hd g tau;
    free_in_context_list x tl g tau
#pop-options

let typable_empty_closed (x: var) (e: exp) (tau: ty)
    : Lemma (requires (typing empty e tau))
      (ensures (not (appears_free_in x e)))
      [SMTPat (appears_free_in x e); SMTPat (typing empty e tau)] = free_in_context x e empty tau

(**** Context invariance *)

type equal (g1: env) (g2: env) = forall (x: var). g1 x = g2 x

type equalE (e: exp) (g1: env) (g2: env) = forall (x: var). appears_free_in x e ==> g1 x = g2 x

type equalE_list (subs: list exp) (g1: env) (g2: env) =
  forall (x: var). appears_free_in_list x subs ==> g1 x = g2 x

#push-options "--fuel 2 --ifuel 1"
let rec context_invariance (e: exp) (g g': env) (tau: ty)
    : Lemma (requires (equalE e g g'))
      (ensures (typing g e tau <==> typing g' e tau))
      (decreases %[ e ]) =
  match e with
  | EAbs x t e1 ->
    (match tau with
      | TArrow _ tau_out -> context_invariance e1 (extend g x t) (extend g' x t) tau_out
      | _ -> ())
  | EApp e1 e2 tau_arg ->
    context_invariance e1 g g' (TArrow tau_arg tau);
    context_invariance e2 g g' tau_arg
  | EIf e1 e2 e3 ->
    context_invariance e1 g g' TBool;
    context_invariance e2 g g' tau;
    context_invariance e3 g g' tau
  | EDefault exceptions ejust econs ->
    context_invariance ejust g g' TBool;
    context_invariance econs g g' tau;
    context_invariance_list exceptions g g' tau
  | _ -> ()
and context_invariance_list (exceptions: list exp) (g g': env) (tau: ty)
    : Lemma (requires (equalE_list exceptions g g'))
      (ensures (typing_list g exceptions tau <==> typing_list g' exceptions tau))
      (decreases %[ exceptions ]) =
  match exceptions with
  | [] -> ()
  | hd :: tl ->
    context_invariance hd g g' tau;
    context_invariance_list tl g g' tau
#pop-options

let typing_extensional (g g': env) (e: exp) (tau: ty)
    : Lemma (requires (equal g g')) (ensures (typing g e tau <==> typing g' e tau)) =
  context_invariance e g g' tau

(**** Substitution preservation *)

#push-options "--fuel 1 --ifuel 1 --z3rlimit 10"
let rec substitution_preserves_typing (x: var) (tau_x: ty) (e v: exp) (g: env) (tau: ty)
    : Lemma (requires (typing empty v tau_x /\ typing (extend g x tau_x) e tau))
      (ensures (typing g (subst x v e) tau))
      (decreases %[ e ]) =
  let gx = extend g x tau_x in
  match e with
  | ELit _ -> ()
  | EVar y -> if x = y then context_invariance v empty g tau else context_invariance e gx g tau
  | EApp e1 e2 tau_arg ->
    substitution_preserves_typing x tau_x e1 v g (TArrow tau_arg tau);
    substitution_preserves_typing x tau_x e2 v g tau_arg
  | EIf e1 e2 e3 ->
    substitution_preserves_typing x tau_x e1 v g TBool;
    substitution_preserves_typing x tau_x e2 v g tau;
    substitution_preserves_typing x tau_x e3 v g tau
  | EAbs y t_y e1 ->
    (match tau with
      | TArrow tau_in tau_out ->
        if tau_in = t_y
        then
          let gxy = extend gx y t_y in
          let gy = extend g y t_y in
          if x = y
          then typing_extensional gxy gy e1 tau_out
          else
            let gyx = extend gy x tau_x in
            typing_extensional gxy gyx e1 tau_out;
            substitution_preserves_typing x tau_x e1 v gy tau_out
      | _ -> ())
  | EDefault exceptions ejust econs ->
    substitution_preserves_typing x tau_x ejust v g TBool;
    substitution_preserves_typing x tau_x econs v g tau;
    substitution_preserves_typing_list x tau_x exceptions v g tau
and substitution_preserves_typing_list
      (x: var)
      (tau_x: ty)
      (exceptions: list exp)
      (v: exp)
      (g: env)
      (tau: ty)
    : Lemma (requires (typing empty v tau_x /\ typing_list (extend g x tau_x) exceptions tau))
      (ensures (typing_list g (subst_list x v exceptions) tau))
      (decreases (%[ exceptions ])) =
  match exceptions with
  | [] -> ()
  | hd :: tl ->
    substitution_preserves_typing x tau_x hd v g tau;
    substitution_preserves_typing_list x tau_x tl v g tau
#pop-options

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
      | ELit LConflictError | ELit LEmptyError -> ()
      | _ ->
        if is_value e2
        then
          match e1 with
          | EAbs x _ ebody -> substitution_preserves_typing x tau_arg ebody e2 empty tau
          | _ -> ()
        else preservation e2 tau_arg
    else preservation e1 (TArrow tau_arg tau)
  | EDefault exceptions just cons ->
    if List.Tot.for_all (fun except -> is_value except) exceptions then
      match empty_count AllEmpty exceptions with
      | AllEmpty ->
        begin if not (is_value just) then preservation just TBool else match just with
        | ELit LTrue -> if not (is_value cons) then preservation cons tau
        | _ -> ()
        end
      | OneNonEmpty e' -> empty_count_preserves_type AllEmpty exceptions empty tau
      | Conflict -> ()
    else begin
      match step_exceptions_left_to_right e exceptions just cons with
      | None ->
         begin if not (is_value just) then preservation just TBool else match just with
        | ELit LTrue -> if not (is_value cons) then preservation cons tau
        | _ -> ()
        end
      | Some e' -> preservation_exceptions_left_to_right e exceptions just cons tau
    end
  | _ -> ()
and preservation_exceptions_left_to_right
      (e: exp)
      (exceptions: list exp {exceptions << e})
      (just: exp{just << e})
      (cons: exp{cons << e})
      (tau: ty)
    : Lemma
      (requires (
        typing empty (EDefault exceptions just cons) tau /\
        Some? (step_exceptions_left_to_right e exceptions just cons)
      ))
      (ensures (
        Nil? exceptions \/
        typing empty (Some?.v (step_exceptions_left_to_right e exceptions just cons)) tau
      ))
      (decreases %[ exceptions ]) =
  match exceptions with
  | [] -> ()
  | hd :: tl ->
    if is_value hd
    then
      (typing_conserved_by_list_reduction empty exceptions tau;
        preservation_exceptions_left_to_right e tl just cons tau)
    else
      (preservation hd tau;
        match step hd with
        | Some (ELit LConflictError) -> ()
        | Some hd' -> ())
#pop-options
