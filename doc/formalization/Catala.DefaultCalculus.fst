module Catala.DefaultCalculus

//TODO: change default to have exceptions first
//TODO: change empty error propagation for function application, does not 
//propagate for function argument

(*** Syntax *)

type ty =
  | TBool : ty
  | TUnit : ty
  | TArrow : tin: ty -> tout: ty -> ty

type var = int

type lit =
  | LEmptyError : lit
  | LConflictError : lit
  | LTrue : lit
  | LFalse : lit
  | LUnit : lit

type exp =
  | EVar : v: var -> exp
  | EApp : fn: exp -> arg: exp -> tau_arg: ty -> exp
  | EAbs : v: var -> vty: ty -> body: exp -> exp
  | ELit : l: lit -> exp
  | EIf : test: exp -> btrue: exp -> bfalse: exp -> exp
  | EDefault : just: exp -> cons: exp -> subdefaults: list exp -> exp

(*** Operational semantics *)

(**** Helpers *)

let c_err = ELit LConflictError

let e_err = ELit LEmptyError

val is_value: exp -> Tot bool
let is_value e =
  match e with
  | EAbs _ _ _ | ELit _ -> true
  | _ -> false

let rec subst (x: var) (e_x e: exp) : Tot exp (decreases e) =
  match e with
  | EVar x' -> if x = x' then e_x else e
  | EAbs x' t e1 -> EAbs x' t (if x = x' then e1 else (subst x e_x e1))
  | EApp e1 e2 tau_arg -> EApp (subst x e_x e1) (subst x e_x e2) tau_arg
  | ELit l -> ELit l
  | EIf e1 e2 e3 -> EIf (subst x e_x e1) (subst x e_x e2) (subst x e_x e3)
  | EDefault ejust econd subs ->
    EDefault (subst x e_x ejust) (subst x e_x econd) (subst_list x e_x subs)
and subst_list (x: var) (e_x: exp) (subs: list exp) : Tot (list exp) (decreases subs) =
  match subs with
  | [] -> []
  | hd :: tl -> (subst x e_x hd) :: (subst_list x e_x tl)

type empty_count_result =
  | AllEmpty
  | OneNonEmpty of exp
  | Conflict

let rec empty_count (acc: empty_count_result) (l: list exp) : Tot empty_count_result (decreases l) =
  match l with
  | [] -> acc
  | hd :: tl ->
    match (hd, acc) with
    | ELit LEmptyError, AllEmpty -> empty_count AllEmpty tl
    | ELit LEmptyError, OneNonEmpty e -> empty_count (OneNonEmpty e) tl
    | _, Conflict -> Conflict
    | _, AllEmpty -> empty_count (OneNonEmpty hd) tl
    | _ -> Conflict

(**** Stepping judgment *)

let rec step_app (e: exp) (e1: exp{e1 << e}) (e2: exp{e2 << e}) (tau_arg: ty{tau_arg << e})
    : Tot (option exp) (decreases %[ e; 0 ]) =
  if is_value e1
  then
    match e1 with
    | ELit LConflictError -> Some c_err
    | ELit LEmptyError -> Some e_err
    | _ ->
      if is_value e2
      then
        (match e1 with
          | EAbs x t e' -> Some (subst x e2 e')
          | _ -> None)
      else
        (match step e2 with
          | Some (ELit LConflictError) -> Some (ELit LConflictError)
          | Some (ELit LEmptyError) -> Some (ELit LEmptyError)
          | Some e2' -> Some (EApp e1 e2' tau_arg)
          | None -> None)
  else
    (match step e1 with
      | Some (ELit LConflictError) -> Some c_err
      | Some (ELit LEmptyError) -> Some e_err
      | Some e1' -> Some (EApp e1' e2 tau_arg)
      | None -> None)

and step_if (e: exp) (e1: exp{e1 << e}) (e2: exp{e2 << e}) (e3: exp{e3 << e})
    : Tot (option exp) (decreases %[ e; 1 ]) =
  if is_value e1
  then
    match e1 with
    | ELit LConflictError -> Some c_err
    | ELit LEmptyError -> Some e_err
    | ELit LTrue -> Some e2
    | ELit LFalse -> Some e3
    | _ -> None
  else
    match (step e1) with
    | Some (ELit LConflictError) -> Some c_err
    | Some (ELit LEmptyError) -> Some e_err
    | Some e1' -> Some (EIf e1' e2 e3)
    | None -> None

and step_subdefaults_left_to_right
      (e: exp)
      (just: exp{just << e})
      (cons: exp{cons << e})
      (subs: list exp {subs << e})
    : Tot (option exp) (decreases %[ e; 2; subs ]) =
  match subs with
  | [] -> Some (EDefault just cons [])
  | hd :: tl ->
    if is_value hd
    then
      match step_subdefaults_left_to_right e just cons tl with
      | Some (ELit LConflictError) -> Some c_err
      | Some (EDefault just cons tl') -> Some (EDefault just cons (hd :: tl'))
      | _ -> None
    else
      match step hd with
      | Some (ELit LConflictError) -> Some c_err
      | Some hd' -> Some (EDefault just cons (hd' :: tl))
      | _ -> None

and step_subdefaults_just_false
      (e: exp)
      (just: exp{just << e})
      (cons: exp{cons << e})
      (subs: list exp {subs << e})
    : Tot (option exp) (decreases %[ e; 3 ]) =
  if List.Tot.for_all (fun sub -> is_value sub) subs
  then
    match empty_count AllEmpty subs with
    | AllEmpty -> Some (ELit LEmptyError) (* D-DefaultFalseNoSub *)
    | OneNonEmpty e' -> Some e' (* D-DefaultFalseOneSub *)
    | Conflict ->
      (* D-DefaultFalseSubConflict *)
      Some (ELit LConflictError)
  else
    match step_subdefaults_left_to_right e just cons subs with
    | Some e' -> Some e'
    | _ -> None

and step_default (e: exp) (just: exp{just << e}) (cons: exp{cons << e}) (subs: list exp {subs << e})
    : Tot (option exp) (decreases %[ e; 4 ]) =
  if is_value just
  then
    match just with
    | ELit LConflictError -> Some c_err
    | ELit LEmptyError -> Some e_err
    | _ ->
      match just, cons with
      | EAbs _ _ _, EAbs _ _ _ -> None
      | ELit LTrue, ELit LEmptyError -> Some (EDefault (ELit LFalse) cons subs)
      | ELit LTrue, _ ->
        (* D-DefaultTrueError *)
        (* D-DefaultTrueNoError *)
        if is_value cons
        then Some cons
        else
          (match (step cons) with
            | Some (ELit LConflictError) -> Some c_err
            | Some cons' -> Some (EDefault just cons' subs)
            | None -> None)
      | ELit LFalse, _ -> step_subdefaults_just_false e just cons subs
      | _ -> None
  else
    match (step just) with
    | Some just' -> Some (EDefault just' cons subs)
    | Some (ELit LConflictError) -> Some c_err
    | Some (ELit LEmptyError) -> Some e_err
    | None -> None

and step (e: exp) : Tot (option exp) (decreases %[ e; 5 ]) =
  match e with
  | EApp e1 e2 tau_arg -> step_app e e1 e2 tau_arg
  | EIf e1 e2 e3 -> step_if e e1 e2 e3
  | EDefault just cons subs -> step_default e just cons subs
  | _ -> None

(* Testing *)
let _ =
  let e0 = EApp (EAbs 0 TBool (EIf (EVar 0) (ELit LFalse) (ELit LTrue))) (ELit LTrue) TBool in
  let e1 = EIf (ELit LTrue) (ELit LFalse) (ELit LTrue) in
  let e1' = step e0 in
  assert_norm (e1' == Some e1);
  let e2 = ELit LFalse in
  let e2' = step e1 in
  assert_norm (e2' == Some e2)

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
  | ELit LEmptyError -> true
  | ELit LConflictError -> true
  | EIf e1 e2 e3 -> typing g e1 TBool && typing g e2 tau && typing g e3 tau
  | EDefault ejust econs subs ->
    typing g ejust TBool && typing g econs tau && typing_list g subs tau (* T-Default *)
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
  | EDefault just cons subs -> if is_value e then () else progress_defaults e just cons subs tau
  | _ -> ()
and progress_defaults
      (e: exp)
      (just: exp{just << e})
      (cons: exp{cons << e})
      (subs: list exp {subs << e})
      (tau: ty)
    : Lemma (requires (~(is_value e) /\ e == EDefault just cons subs /\ (typing empty e tau)))
      (ensures (Some? (step_default e just cons subs)))
      (decreases %[ e; 2 ]) =
  progress just TBool;
  if is_value just
  then
    (is_bool_value_cannot_be_abs empty just;
      match just, cons with
      | ELit LTrue, ELit LEmptyError -> ()
      | ELit LTrue, _ -> progress cons tau
      | ELit LFalse, _ -> progress_defaults_just_false e just cons subs tau
      | ELit LEmptyError, _ | ELit LConflictError, _ -> ())
and progress_defaults_just_false
      (e: exp)
      (just: exp{just << e})
      (cons: exp{cons << e})
      (subs: list exp {subs << e})
      (tau: ty)
    : Lemma
      (requires
        (~(is_value e) /\ just == ELit LFalse /\ e == EDefault (ELit LFalse) cons subs /\
          (typing empty e tau)))
      (ensures (Some? (step_subdefaults_just_false e just cons subs)))
      (decreases %[ e; 1 ]) =
  if List.Tot.for_all (fun sub -> is_value sub) subs
  then ()
  else progress_defaults_left_to_right e just cons subs tau
and progress_defaults_left_to_right
      (e: exp)
      (just: exp{just << e})
      (cons: exp{cons << e})
      (subs: list exp {subs << e})
      (tau: ty)
    : Lemma
      (requires
        (~(is_value e) /\ just == ELit LFalse /\ (typing empty (EDefault just cons subs) tau)))
      (ensures (Some? (step_subdefaults_left_to_right e just cons subs)))
      (decreases %[ e; 0; subs ]) =
  match subs with
  | [] -> ()
  | hd :: tl ->
    progress hd tau;
    if is_value hd
    then
      (typing_conserved_by_list_reduction empty subs tau;
        progress_defaults_left_to_right e just cons tl tau)
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
  | EDefault ejust econs subs ->
    appears_free_in x ejust || appears_free_in x econs || appears_free_in_list x subs
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
  | EDefault ejust econs subs ->
    free_in_context x ejust g TBool;
    free_in_context x econs g tau;
    free_in_context_list x subs g tau
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
  | EDefault ejust econs subs ->
    context_invariance ejust g g' TBool;
    context_invariance econs g g' tau;
    context_invariance_list subs g g' tau
  | _ -> ()
and context_invariance_list (subs: list exp) (g g': env) (tau: ty)
    : Lemma (requires (equalE_list subs g g'))
      (ensures (typing_list g subs tau <==> typing_list g' subs tau))
      (decreases %[ subs ]) =
  match subs with
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
  | EDefault ejust econs subs ->
    substitution_preserves_typing x tau_x ejust v g TBool;
    substitution_preserves_typing x tau_x econs v g tau;
    substitution_preserves_typing_list x tau_x subs v g tau
and substitution_preserves_typing_list
      (x: var)
      (tau_x: ty)
      (subs: list exp)
      (v: exp)
      (g: env)
      (tau: ty)
    : Lemma (requires (typing empty v tau_x /\ typing_list (extend g x tau_x) subs tau))
      (ensures (typing_list g (subst_list x v subs) tau))
      (decreases (%[ subs ])) =
  match subs with
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
  | EDefault just cons subs ->
    if not (is_value just)
    then preservation just TBool
    else
      (match just, cons with
        | ELit LTrue, _ -> if not (is_value cons) then preservation cons tau
        | ELit LFalse, _ ->
          if List.Tot.for_all (fun sub -> is_value sub) subs
          then
            match empty_count AllEmpty subs with
            | AllEmpty -> ()
            | OneNonEmpty e' -> empty_count_preserves_type AllEmpty subs empty tau
            | Conflict -> ()
          else preservation_subdefaults_left_to_right e just cons subs tau
        | _ -> ())
  | _ -> ()
and preservation_subdefaults_left_to_right
      (e: exp)
      (just: exp{just << e})
      (cons: exp{cons << e})
      (subs: list exp {subs << e})
      (tau: ty)
    : Lemma
      (requires
        (typing empty (EDefault just cons subs) tau /\
          Some? (step_subdefaults_left_to_right e just cons subs)))
      (ensures
        (Nil? subs \/ typing empty (Some?.v (step_subdefaults_left_to_right e just cons subs)) tau))
      (decreases %[ subs ]) =
  match subs with
  | [] -> ()
  | hd :: tl ->
    if is_value hd
    then
      (typing_conserved_by_list_reduction empty subs tau;
        preservation_subdefaults_left_to_right e just cons tl tau)
    else
      (preservation hd tau;
        match step hd with
        | Some (ELit LConflictError) -> ()
        | Some hd' -> ())
#pop-options

