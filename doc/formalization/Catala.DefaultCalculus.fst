module Catala.DefaultCalculus

open FStar.StrongExcludedMiddle
module T = FStar.Tactics

/// This whole proof is inspired by FStar/examples/metatheory/StlcStrongdbparsubst.fst

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
      (decreases %[is_var_size e; is_renaming_size s; 1; e])
  =
  match e with
  | EVar x -> s x
  | EAbs t e1 -> EAbs t (subst (subst_abs s) e1)
  | EApp e1 e2 tau_arg -> EApp (subst s e1) (subst s e2) tau_arg
  | ELit l -> ELit l
  | EIf e1 e2 e3 -> EIf (subst s e1) (subst s e2) (subst s e3)
  | EDefault exceptions ejust econd tau ->
    EDefault (subst_list s exceptions) (subst s ejust) (subst s econd) tau
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

type empty_count_result =
  | AllEmpty
  | OneNonEmpty of exp
  | Conflict

type exceptions_count_result =
  | NoStep
  | IllFormed
  | SomeStep of exp

let rec empty_count (acc: empty_count_result) (l: list exp) : Tot empty_count_result (decreases l) =
  match l with
  | [] -> acc
  | hd :: tl ->
    match (hd, acc) with
    | ELit LEmptyError, AllEmpty -> empty_count AllEmpty tl
    | ELit LEmptyError, OneNonEmpty e -> empty_count (OneNonEmpty e) tl
    | ELit LConflictError, _ -> Conflict
    | _, Conflict -> Conflict
    | _, AllEmpty -> empty_count (OneNonEmpty hd) tl
    | _, OneNonEmpty _ -> Conflict

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
          | EAbs t e1' -> Some (subst (var_to_exp_beta e2) e1') (* D-Beta *)
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
    : Tot exceptions_count_result (decreases %[ e; 3 ]) =
  if List.Tot.for_all is_value exceptions
  then
    match empty_count AllEmpty exceptions with
    | AllEmpty -> NoStep
    | OneNonEmpty e' -> SomeStep e' (* D-DefaultOneException *)
    | Conflict -> SomeStep (ELit LConflictError) (* D-DefaultExceptionConflict *)
  else
    match step_exceptions_left_to_right e exceptions just cons tau with
    | None -> IllFormed
    | Some e' -> SomeStep e'

and step_default
  (e: exp)
  (exceptions: list exp {exceptions << e})
  (just: exp{just << e})
  (cons: exp{cons << e})
  (tau: ty)
    : Tot (option exp) (decreases %[ e; 4 ]) =
  match step_exceptions e exceptions just cons tau with
  | IllFormed -> None
  | SomeStep e' -> Some e'
  | NoStep ->
    if is_value just then
      match just with
      | ELit LConflictError -> Some c_err  (* D-ContextConflictError *)
      | ELit LEmptyError -> Some e_err (* D-ContextEmptyError *)
      | _ ->
        match just with
        | ELit LTrue -> Some cons (* D-DefaultTrueNoExceptions*)
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

val extend: env -> ty -> Tot env
let extend g t = FunctionalExtensionality.on_dom var (fun x' -> if 0 = x' then Some t else g (x' - 1))

(**** Typing judgment *)

let rec typing (g: env) (e: exp) (tau: ty) : Tot bool (decreases (e)) =
  match e with
  | EVar x -> g x = Some tau
  | EAbs t e1 ->
    (match tau with
      | TArrow tau_in tau_out -> t = tau_in && typing (extend g t) e1 tau_out
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

#push-options "--fuel 2 --ifuel 1 --z3rlimit 20"
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
        e == EDefault exceptions just cons tau /\ (typing empty e tau)
      ))
      (ensures (Some? (step_default e exceptions just cons tau)))
      (decreases %[ e; 2 ]) =
  match step_exceptions e exceptions just cons tau with
  | IllFormed ->
    assert(step_exceptions_left_to_right e exceptions just cons tau == None);
    assert(~(List.Tot.for_all is_value exceptions));
    progress_default_exceptions e exceptions just cons tau
  | SomeStep _ -> ()
  | NoStep ->
    if is_value just then
      (is_bool_value_cannot_be_abs empty just;
        match just, cons with
        | ELit LTrue, ELit LEmptyError -> ()
        | ELit LTrue, _ -> progress cons tau
        | ELit LFalse, _ ->  ()
        | ELit LEmptyError, _ -> ()
        | ELit LConflictError, _ -> ())
    else progress just TBool
and progress_default_exceptions
      (e: exp)
      (exceptions: list exp {exceptions << e})
      (just: exp{just << e})
      (cons: exp{cons << e})
      (tau: ty)
    : Lemma (requires (
        typing_list empty exceptions tau /\
        step_exceptions_left_to_right e exceptions just cons tau == None /\
        ~(List.Tot.for_all is_value exceptions)
      ))
      (ensures (False))
      (decreases %[ e; 1; exceptions ])
  =
  match exceptions with
  | [] -> ()
  | hd::tl ->
    progress hd tau;
    if is_value hd then begin
      progress_default_exceptions e tl just cons tau
    end else ()
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
  | EDefault exceptions just cons _ ->
    substitution_extensionnal_list s1 s2 exceptions;
    substitution_extensionnal s1 s2 just;
    substitution_extensionnal s1 s2 cons
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
  | EDefault exceptions just cons tau ->
    if tau <> t then () else
    substitution_preserves_typing_list g1 exceptions t s g2 s_lemma;
    substitution_preserves_typing g1 just TBool s g2 s_lemma;
    substitution_preserves_typing g1 cons t s g2 s_lemma
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

#push-options "--fuel 4 --ifuel 2 --z3rlimit 100 --quake 10/1"
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
          | EAbs _ ebody ->
            let s_lemma : subst_typing (var_to_exp_beta e2) (extend empty tau_arg) empty =
              fun x -> ()
            in
            substitution_preserves_typing (extend empty tau_arg) ebody tau
              (var_to_exp_beta e2) empty s_lemma
          | _ -> ()
        else preservation e2 tau_arg
    else preservation e1 (TArrow tau_arg tau)
  | EDefault exceptions just cons tau' ->
    if List.Tot.for_all is_value exceptions then
      match empty_count AllEmpty exceptions with
      | AllEmpty ->
        begin if not (is_value just) then begin assert(Some? (step just)); preservation just TBool end else match just with
        | ELit LTrue -> ()
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
