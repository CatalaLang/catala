module Catala.DefaultCalculus

open FStar.StrongExcludedMiddle

(*** Syntax *)

type ty =
  | TBool : ty
  | TUnit : ty
  | TArrow : tin: ty -> tout: ty -> ty

type var = nat

type lit =
  | LEmptyError : lit
  | LConflictError : lit
  | LTrue : lit
  | LFalse : lit
  | LUnit : lit

type exp =
  | EVar : v: var -> exp
  | EApp : fn: exp -> arg: exp -> tau_arg: ty -> exp
  | EAbs : vty: ty -> body: exp -> exp
  | ELit : l: lit -> exp
  | EIf : test: exp -> btrue: exp -> bfalse: exp -> exp
  | EDefault : exceptions: list exp -> just: exp -> cons: exp -> tau: ty -> exp

(*** Operational semantics *)

(**** Helpers *)

let c_err = ELit LConflictError

let e_err = ELit LEmptyError

val is_value: exp -> Tot bool
let is_value e =
  match e with
  | EAbs _ _ | ELit _ -> true
  | _ -> false

let var_to_exp = var -> Tot exp

let is_renaming_prop (s: var_to_exp) : prop = forall (x: var). EVar? (s x)

let is_renaming_size (s: var_to_exp)
    : GTot (n:int{(is_renaming_prop s) ==> n = 0 /\ (~(is_renaming_prop s) ==> n = 1)})
  =
  if strong_excluded_middle (is_renaming_prop s) then 0 else 1

let increment : var_to_exp = fun y -> EVar (y + 1)

let increment_is_renaming (_: unit) : Lemma (is_renaming_prop increment) = ()

let is_var_size (e: exp) : int = if EVar? e then 0 else 1

let rec subst (s: var_to_exp) (e: exp) : Pure exp
      (requires True)
      (ensures (fun e' -> (is_renaming_prop s /\ EVar? e) ==> EVar? e'))
      (decreases %[is_var_size e; is_renaming_size s; 2; e])
  =
  match e with
  | EVar x -> s x
  | EAbs t e1 -> EAbs t (subst (subst_abs s) e1)
  | EApp e1 e2 tau_arg -> EApp (subst s e1) (subst s e2) tau_arg
  | ELit l -> ELit l
  | EIf e1 e2 e3 -> EIf (subst s e1) (subst s e2) (subst s e3)
  | EDefault exceptions ejust econd tau ->
    EDefault (subst_list s e exceptions) (subst s ejust) (subst s econd) tau
and subst_list (s: var_to_exp) (e: exp) (l: list exp{l << e}) : Tot (list exp)
      (decreases %[is_var_size e; is_renaming_size s; 1; e; l]) =
  match l with
  | [] -> []
  | hd :: tl ->
  (subst s hd) :: (subst_list s e tl)
and subst_abs (s: var_to_exp) (y: var) : Tot (e':exp{is_renaming_prop s ==> EVar? e'})
      (decreases %[1; is_renaming_size s; 0; EVar 0])
  =
  if y = 0 then EVar y else subst increment (s (y -1))

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
    | ELit LConflictError -> Some c_err (* D-ContextConflictError *)
    | ELit LEmptyError -> Some e_err (* D-ContextEmptyError *)
    | _ ->
      if is_value e2
      then
        match e2 with
        | ELit LConflictError -> Some c_err (* D-ContextConflictError *)
        | ELit LEmptyError -> Some e_err (* D-ContextEmptyError *)
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
    | ELit LConflictError -> Some c_err  (* D-ContextConflictError *)
    | ELit LEmptyError -> Some e_err (* D-ContextEmptyError *)
    | ELit LTrue -> Some e2 (* D-CondTrue *)
    | ELit LFalse -> Some e3 (* D-CondFalse*)
    | _ -> None
  else
    match (step e1) with
    | Some e1' -> Some (EIf e1' e2 e3) (* D-Context *)
    | None -> None

and step_exceptions_left_to_right
      (e: exp)
      (exceptions: list exp {exceptions << e})
      (just: exp{just << e})
      (cons: exp{cons << e})
      (tau: ty)
    : Tot (option exp) (decreases %[ e; 2; exceptions ]) =
  match exceptions with
  | [] -> None
  | [hd] -> begin
    match step hd with
    | Some (ELit LConflictError) -> Some c_err  (* D-ContextConflictError *)
    | Some hd' -> Some (EDefault ([hd']) just cons tau) (* D-Context *)
    | _ -> None
  end
  | hd :: tl ->
    if is_value hd
    then
      match step_exceptions_left_to_right e tl just cons tau with
      | Some (ELit LConflictError) -> Some c_err  (* D-ContextConflictError *)
      | Some (EDefault tl' _ _ _) -> Some (EDefault (hd :: tl') just cons tau) (* D-Context *)
      | _ -> None
    else
      match step hd with
      | Some (ELit LConflictError) -> Some c_err  (* D-ContextConflictError *)
      | Some hd' -> Some (EDefault (hd' :: tl) just cons tau) (* D-Context *)
      | _ -> None

and step_exceptions
      (e: exp)
      (exceptions: list exp {exceptions << e})
      (just: exp{just << e})
      (cons: exp{cons << e})
      (tau: ty)
    : Tot (option exp) (decreases %[ e; 3 ]) =
  if List.Tot.for_all (fun except -> is_value except) exceptions
  then
    match empty_count AllEmpty exceptions with
    | AllEmpty -> None
    | OneNonEmpty e' -> Some e' (* D-DefaultOneException *)
    | Conflict -> Some (ELit LConflictError) (* D-DefaultExceptionConflict *)
  else step_exceptions_left_to_right e exceptions just cons tau

and step_default
  (e: exp)
  (exceptions: list exp {exceptions << e})
  (just: exp{just << e})
  (cons: exp{cons << e})
  (tau: ty)
    : Tot (option exp) (decreases %[ e; 4 ]) =
  match step_exceptions e exceptions just cons tau with
  | Some e' -> Some e'
  | None ->
    if is_value just then
      match just with
      | ELit LConflictError -> Some c_err  (* D-ContextConflictError *)
      | ELit LEmptyError -> Some e_err (* D-ContextEmptyError *)
      | _ ->
        match just with
        | ELit LTrue ->
          if is_value cons
          then Some cons
          else
            (match (step cons) with
              | Some (ELit LConflictError) -> Some c_err  (* D-ContextConflictError *)
              | Some cons' -> Some (EDefault exceptions just cons' tau) (* D-DefaultTrueNoExceptions*)
              | None -> None)
        | ELit LFalse -> Some e_err (* D-DefaultFalseNoExceptions *)
        | _ -> None
  else
    match (step just) with
    | Some just' -> Some (EDefault exceptions just' cons tau)
    | Some (ELit LConflictError) -> Some c_err  (* D-ContextConflictError *)
    | Some (ELit LEmptyError) -> Some e_err (* D-ContextEmptyError *)
    | None -> None

and step (e: exp) : Tot (option exp) (decreases %[ e; 5 ]) =
  match e with
  | EApp e1 e2 tau_arg -> step_app e e1 e2 tau_arg
  | EIf e1 e2 e3 -> step_if e e1 e2 e3
  | EDefault just cons subs tau -> step_default e just cons subs tau
  | _ -> None

(*** Typing *)

(**** Typing helpers *)

type env = FunctionalExtensionality.restricted_t var (fun _ -> option ty)

val empty:env
let empty = FunctionalExtensionality.on_dom var (fun _ -> None)

val extend: env -> var -> ty -> Tot env
let extend g x t = FunctionalExtensionality.on_dom var (fun x' -> if x = x' then Some t else g x')

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
  | ELit LUnit -> tau = TUnit
  | ELit LEmptyError -> true
  | ELit LConflictError -> true
  | EIf e1 e2 e3 -> typing g e1 TBool && typing g e2 tau && typing g e3 tau
  | EDefault exceptions ejust econs tau' ->
    tau' = tau &&  typing_list g exceptions tau && typing g ejust TBool && typing g econs tau
    (* T-Default *)
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
  | EDefault exceptions just cons tau' -> progress_default e exceptions just cons tau
  | _ -> ()
and progress_default
      (e: exp)
      (exceptions: list exp {exceptions << e})
      (just: exp{just << e})
      (cons: exp{cons << e})
      (tau: ty)
    : Lemma (requires (
        ~(is_value e) /\
        e == EDefault exceptions just cons tau /\ (typing empty e tau)
      ))
      (ensures (Some? (step_default e exceptions just cons tau)))
      (decreases %[ e; 2 ]) =
  match step_exceptions e exceptions just cons tau with
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
  | EDefault exceptions ejust econs _ ->
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
  | EDefault exceptions ejust econs _ ->
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
  | EDefault exceptions ejust econs _ ->
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
  | EDefault exceptions ejust econs _ ->
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
  | EDefault exceptions just cons tau' ->
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
      match step_exceptions_left_to_right e exceptions just cons tau with
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
        typing empty (EDefault exceptions just cons tau) tau /\
        Some? (step_exceptions_left_to_right e exceptions just cons tau)
      ))
      (ensures (
        Nil? exceptions \/
        typing empty (Some?.v (step_exceptions_left_to_right e exceptions just cons tau)) tau
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
