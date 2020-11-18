module Catala.DefaultCalculus

(*** Syntax *)

type ty =
  | TBool  : ty
  | TUnit  : ty
  | TArrow : tin:ty -> tout:ty -> ty

type var = int

type lit =
  | LEmptyError : lit
  | LConflictError : lit
  | LTrue  : lit
  | LFalse : lit
  | LUnit : lit

type exp =
  | EVar    : v:var -> exp
  | EApp    : fn:exp -> arg:exp -> exp
  | EAbs    : v:var -> vty:ty -> body:exp -> exp
  | ELit    : l:lit -> exp
  | EIf     : test:exp -> btrue:exp -> bfalse:exp -> exp
  | EDefault: just:exp -> cons:exp -> subdefaults:list exp -> exp

(*** Operational semantics *)

(**** Helpers *)

let c_err = ELit LConflictError

let e_err = ELit LEmptyError

val is_value : exp -> Tot bool
let is_value e =
  match e with
  | EAbs _ _ _
  | ELit _
  | EDefault (EAbs _ _ _) _ _
    -> true
  | _             -> false

let rec map (#a: Type) (#b: Type) (l:list a) (f: ((x:a{x << l}) -> Tot b)) : Tot (list b)
  =
  match l with
  | [] -> []
  | a::tl -> f a::map tl f

val subst : var -> exp -> e:exp -> Tot exp (decreases e)
let rec subst x e e' =
  match e' with
  | EVar x' -> if x = x' then e else e'
  | EAbs x' t e1 ->
      EAbs x' t (if x = x' then e1 else (subst x e e1))
  | EApp e1 e2 -> EApp (subst x e e1) (subst x e e2)
  | ELit l -> ELit l
  | EIf e1 e2 e3 -> EIf (subst x e e1) (subst x e e2) (subst x e e3)
  | EDefault just cond subs -> EDefault (subst x e just) (subst x e cond) (map subs (subst x e))

type empty_count_result =
  | AllEmpty
  | OneNonEmpty of exp
  | Conflict

let rec empty_count (acc: empty_count_result) (l: list exp) : Tot empty_count_result (decreases l) =
  match l with
  | [] -> acc
  | hd::tl -> begin
    match (hd, acc) with
    | ELit (LEmptyError), AllEmpty -> empty_count AllEmpty tl
    | ELit (LEmptyError), OneNonEmpty e -> empty_count (OneNonEmpty e) tl
    | _, Conflict -> Conflict
    | _, AllEmpty -> empty_count (OneNonEmpty hd) tl
    | _ -> Conflict
  end

(**** Stepping judgment *)

let rec step_app
  (e: exp)
  (e1: exp{e1 << e})
  (e2: exp{e2 << e})
    : Tot (option exp)  (decreases %[e; 0]) =
  if is_value e1 then
    match e1 with
    | ELit LConflictError -> Some c_err
    | ELit LEmptyError -> Some e_err
    | _ ->
      if is_value e2 then
        match e1 with
        | EAbs x t e' -> Some (subst x e2 e')
        | EDefault (EAbs xjust tjust ejust') econs subs -> (* beta_d *)
          Some (EDefault
            (subst xjust e2 ejust')
            (EApp econs e2)
            (map subs (fun sub -> EApp sub e2)))
        | _ -> None
      else
        match step e2 with
        | Some (ELit LConflictError) -> Some (ELit LConflictError)
        | Some (ELit LEmptyError) -> Some (ELit LEmptyError)
        | Some e2' -> Some (EApp e1 e2')
        | None     -> None
    else
      match step e1 with
      | Some (ELit LConflictError) -> Some c_err
      | Some (ELit LEmptyError) -> Some e_err
      | Some e1' -> Some (EApp e1' e2)
      | None     -> None

and step_if
  (e: exp)
  (e1: exp{e1 << e})
  (e2: exp{e2 << e})
  (e3: exp{e3 << e})
    : Tot (option exp)  (decreases %[e; 1])  =
  if is_value e1 then
    match e1 with
    | ELit LConflictError -> Some c_err
    | ELit LEmptyError -> Some e_err
    | ELit LTrue   -> Some e2
    | ELit LFalse  -> Some e3
    | _       -> None
  else
    match (step e1) with
    | Some (ELit LConflictError) -> Some c_err
    | Some (ELit LEmptyError) -> Some e_err
    | Some e1' -> Some (EIf e1' e2 e3)
    | None     -> None

and step_subdefaults_left_to_right
  (e: exp)
  (just:exp{just << e})
  (cons:exp{cons << e})
  (subs: list exp{subs << e})
    : Tot (option exp) (decreases %[e; 2; subs])
  =
  match subs with
  | [] -> Some (EDefault just cons [])
  | hd::tl ->
    if is_value hd then
      match step_subdefaults_left_to_right e just cons tl with
      | Some (ELit LConflictError) -> Some c_err
      | Some (EDefault just cons tl') -> Some (EDefault just cons (hd::tl'))
      | _ -> None
    else
      match step hd with
      | Some (ELit LConflictError) -> Some c_err
      | Some hd' -> Some (EDefault just cons (hd'::tl))
      | _ -> None

and step_subdefaults_just_false
  (e: exp)
  (just:exp{just << e})
  (cons:exp{cons << e})
  (subs: list exp{subs << e}) : Tot (option exp) (decreases %[e; 3]) =
  if List.Tot.for_all (fun sub -> is_value sub) subs then
    match empty_count AllEmpty subs with
    | AllEmpty -> Some (ELit LEmptyError) (* DefaultJustifFalseNoSub *)
    | OneNonEmpty e' -> Some e' (* DefaultJustifFalseOneSub *)
    | Conflict -> Some (ELit LConflictError) (* DefaultJustifFalseSubConflict *)
  else
    match step_subdefaults_left_to_right e just cons subs with
    | Some e' -> Some e'
    | _ -> None

and step_default
  (e: exp)
  (just:exp{just << e})
  (cons:exp{cons << e})
  (subs: list exp{subs << e}) : Tot (option exp)  (decreases %[e; 4]) =
  if is_value just then begin
    match just with
    | ELit LConflictError -> Some c_err
    | ELit LEmptyError -> Some e_err
    | ELit _ | EAbs _ _ _ | EDefault (EAbs _ _ _) _ _ -> begin
      match just, cons with
      | EAbs _ _ _, EAbs _ _ _
      |  EDefault (EAbs _ _ _) _ _,  EDefault (EAbs _ _ _) _ _ ->
        None
      | ELit LTrue, ELit LEmptyError ->
        Some (EDefault (ELit LFalse) cons subs)
        (* DefaultJustifTrueError *)
      | ELit LTrue, _ (* DefaultJustifTrueNoError *) ->
        if is_value cons then
          Some cons
        else begin
          match (step cons) with
          | Some (ELit LConflictError) -> Some c_err
          | Some cons' -> Some (EDefault just cons' subs)
          | None -> None
        end
        | ELit LFalse, _ ->
          step_subdefaults_just_false e just cons subs
          (* here we evaluate the subs from left to right *)
        | _ -> None
      end
    end
  else
    match (step just) with
    | Some just' -> Some (EDefault just' cons subs)
    | Some (ELit LConflictError) -> Some c_err
    | Some (ELit LEmptyError) -> Some e_err
    | None -> None

and step (e: exp) : Tot (option exp) (decreases %[e; 5]) =
  match e with
  | EApp e1 e2 -> step_app e e1 e2
  | EIf e1 e2 e3 -> step_if e e1 e2 e3
  | EDefault just cons subs -> step_default e just cons subs
  | _ -> None

(* Testing *)
let _ =
  let e0 = EApp (EAbs 0 TBool (EIf (EVar 0) (ELit LFalse) (ELit LTrue))) (ELit LTrue) in
  let e1 = EIf (ELit LTrue) (ELit LFalse) (ELit LTrue) in
  let e1' = step e0 in
  assert_norm(e1' == Some e1);
  let e2 = ELit LFalse in
  let e2' = step e1 in
  assert_norm(e2' == Some e2)

(* Testing *)
let _ =
  let e0 = EDefault
    (EAbs 0 TBool (EIf (EVar 0) (ELit LTrue) (ELit LFalse)))
    (EAbs 1 TBool (ELit LTrue))
    [ (EAbs 2 TBool (ELit LEmptyError));  (EAbs 3 TBool (ELit LFalse)) ] in
  assert_norm (step e0 == None);
  let e0 = EApp e0 (ELit LFalse) in
  let e1 = EDefault
    (EIf (ELit LFalse) (ELit LTrue) (ELit LFalse))
    (EApp (EAbs 1 TBool (ELit LTrue)) (ELit LFalse))
    [ (EApp (EAbs 2 TBool (ELit LEmptyError)) (ELit LFalse));
      (EApp (EAbs 3 TBool (ELit LFalse)) (ELit LFalse)) ]
  in
  let e1' = step e0 in (* beta_d *)
  assert_norm(e1' == Some e1);
  let e2 = EDefault
    (ELit LFalse)
    (EApp (EAbs 1 TBool (ELit LTrue)) (ELit LFalse))
    [ (EApp (EAbs 2 TBool (ELit LEmptyError)) (ELit LFalse));
      (EApp (EAbs 3 TBool (ELit LFalse)) (ELit LFalse)) ]
  in
  let e2' = step e1 in (* IfFalse *)
  assert_norm(e2' == Some e2);
  let e3 = EDefault
    (ELit LFalse)
    (EApp (EAbs 1 TBool (ELit LTrue)) (ELit LFalse))
    [ (ELit LEmptyError);
      (EApp (EAbs 3 TBool (ELit LFalse)) (ELit LFalse)) ]
  in
  let e3' = step e2 in (* App *)
  assert_norm(e3' == Some e3);
  let e4 = EDefault
    (ELit LFalse)
    (EApp (EAbs 1 TBool (ELit LTrue)) (ELit LFalse))
    [ (ELit LEmptyError);
      (ELit LFalse) ]
  in
  let e4' = step e3 in (* App *)
  assert_norm(e4' == Some e4);
  let e5 = ELit LFalse in
  let e5' = step e4 in
  assert_norm(e5' == Some e5); (* DefaultJustifFalseOneSub *)
  ()

(*** Typing *)

type env = var -> Tot (option ty)

val empty : env
let empty = fun _ -> None

val extend : env -> var -> ty -> Tot env
let extend g x t = fun x' -> if x = x' then Some t else g x'

let rec for_all_defaults (subs: list exp) (f: (sub:exp{sub << subs}) -> bool) : bool =
  match subs with
  | [] -> true
  | hd::tl ->
    if f hd then for_all_defaults tl f else false

type tyres =
  | TRBool  : tyres
  | TRUnit  : tyres
  | TRArrow : tin:ty -> tout:tyres -> tyres
  | TRAny: tyres

let rec ty_to_res (t: ty) : tyres = match t with
  | TBool -> TRBool
  | TUnit -> TRUnit
  | TArrow tin tout -> TRArrow tin (ty_to_res tout)

let rec unify (t1 t2: tyres) : option tyres =
  match t1, t2 with
  | TRBool, TRBool
  | TRUnit, TRUnit -> Some t1
  | TRAny, TRAny -> Some TRAny
  | TRAny, t2 -> Some t2
  | t1, TRAny -> Some t1
  | TRArrow t11 t12, TRArrow t21 t22 ->
    if t11 = t21 then
      match unify t12 t22 with
      | None -> None
      | Some t -> Some (TRArrow t11 t)
    else None
  | _ -> None

let rec unify_comm (t1 t2: tyres) : Lemma (unify t1 t2 == unify t2 t1) =
  match t1, t2 with
  | TRArrow t11 t12, TRArrow t21 t22 ->
    unify_comm t12 t22
  | _ -> ()

let rec unify_list (g: env) (subs: list exp) : Tot (option tyres) (decreases %[subs]) =
  match subs with
  | [] -> Some TRAny
  | hd::tl -> begin
    let unif_tl = unify_list g tl in
    match unif_tl with
    | None -> None
    | Some unif_tl -> begin
      match typing g hd with
      | None -> None
      | Some thd ->
      unify thd unif_tl
    end
  end

and typing (g: env) (e: exp) : Tot (option tyres) (decreases %[e; 1]) =
  match e with
  | EVar x -> begin
    match g x with
    | None -> None
    | Some t -> Some (ty_to_res t)
  end
  | EAbs x t e1 -> begin
    match typing (extend g x t) e1 with
    | Some t' -> Some (TRArrow t t')
    | None -> None
  end
  | EApp e1 e2 -> begin
    match typing g e1, typing g e2 with
    | Some TRAny, Some t2 ->
      Some TRAny
    | Some (TRArrow t11 t12), Some t2 -> unify (ty_to_res t11) t2
    | _, _ -> None
  end
  | ELit LTrue  -> Some TRBool
  | ELit LFalse -> Some TRBool
  | EIf e1 e2 e3 -> begin
    match typing g e1, typing g e2, typing g e3 with
    | Some TRBool, Some t2, Some t3
    | Some TRAny, Some t2, Some t3 -> unify t2 t3
    | _, _, _ -> None
  end
  | EDefault (EAbs xjust tjust ejust) (EAbs xcons tcons econs) subs -> begin (* DefaultFun *)
     if tjust = tcons then
       match typing (extend g xjust tjust) ejust, typing (extend g xcons tcons) econs with
       | Some tjust', Some tcons' -> begin
         match unify tjust' tcons' with
         | Some tjust' -> begin
           match unify_list g subs with
           | None -> None
           | Some unif_subs -> unify (TRArrow tjust tjust') unif_subs
         end
         | None -> None
       end
       | _,_ -> None
     else None
  end
  | EDefault tjust tcons subs -> begin (* DefaultBase *)
    match typing g tjust, typing g tcons with
    | Some TRBool, Some tcons
    | Some TRAny, Some tcons -> begin
      match unify_list g subs with
      | None -> None
      | Some unif_subs -> unify tcons unif_subs
    end
    | _, _ -> None
  end
  | _ -> None

(*** Progress *)

(**** Progress lemmas *)

let is_bool_value_cannot_be_default_abs (g: env) (e: exp) : Lemma
    (requires (is_value e /\ (match typing g e with
      | Some TRAny | Some TRBool -> True
      | _ -> False
    ))) (ensures (
      match e with
      | ELit LUnit -> False
      | ELit _ -> True
      | _ -> False
    ))
  =
  match e with
  | ELit _ -> ()
  | EDefault (EAbs xjust tjust ejust) (EAbs xcons tcons econs) subs -> begin
    match typing g e with
    | Some TRAny
    | Some TRBool ->
      if tjust = tcons then begin
        match
          typing (extend g xjust tjust) ejust,
          typing (extend g xcons tcons) econs
        with
        | Some tjust', Some tcons' -> begin
          match unify tjust' tcons' with
          | Some tjust' ->
            let te = unify_list g subs in
            assert(typing g e == te)
          | None -> ()
         end
       | _ -> ()
     end else ()
     | _ -> ()
   end
   | _ -> ()

let rec unify_compose_ok (x y z: tyres) : Lemma
   (requires (Some? (unify y z) /\ Some? (unify x (Some?.v (unify y z)))))
   (ensures (Some? (unify x z)))
   =
   match x, y, z with
   | TRArrow x1 x2, TRArrow y1 y2, TRArrow z1 z2 -> unify_compose_ok x2 y2 z2
   | _ -> ()

#push-options "--fuel 3 --ifuel 2 --z3rlimit 20"
let typing_conserved_by_list_reduction
  (g: env)
  (just cons: exp)
  (subs: list exp)
    : Lemma
      (requires (
        ~ (EAbs? just /\ EAbs? cons) /\
        Some? (typing g (EDefault just cons subs))
      ))
      (ensures (Cons? subs ==> Some? (typing g (EDefault just cons (Cons?.tl subs)))))
  =
  match subs with
  | [] -> ()
  | hd1::tl ->
    match typing g just, typing g cons with
    | Some TRBool, Some tcons
    | Some TRAny, Some tcons ->
      let e = EDefault just cons subs in
      let t_out_sub = unify_list g subs in
      let thd1 = Some?.v (typing g hd1) in
      unify_compose_ok tcons thd1 (Some?.v (unify_list g tl))
    | _ -> ()
#pop-options

(**** Progress theorem *)

#push-options "--fuel 2 --ifuel 1 --z3rlimit 20"
val progress : e:exp -> Lemma
      (requires (Some? (typing empty e)))
      (ensures (is_value e \/ (Some? (step e))))
      (decreases %[e; 3])
let rec progress e =
  match e with
  | EApp e1 e2 ->
    progress e1; begin match typing empty e1 with
    | Some TRAny -> if is_value e1 then
        match e1 with
        | ELit (LEmptyError) | ELit (LConflictError) -> ()
        | EDefault just cons subs -> if is_value e2 then () else progress e2
      else ()
    | _ -> progress e2
    end
  | EIf e1 e2 e3 -> progress e1; progress e2; progress e3;
    if is_value e1 then is_bool_value_cannot_be_default_abs empty e1 else ()
  | EDefault just cons subs ->
   if is_value e then () else progress_defaults e just cons subs
  | _ -> ()
and progress_defaults
  (e: exp)
  (just: exp{just << e})
  (cons: exp{cons << e})
  (subs: list exp{subs << e}) : Lemma
    (requires (~ (is_value e) /\ e == EDefault just cons subs /\ Some? (typing empty e)))
    (ensures (Some? (step_default e just cons subs)))
    (decreases %[e; 2])
  =
  progress just;
  if is_value just then begin
    is_bool_value_cannot_be_default_abs empty just;
    match just, cons with
    | ELit LTrue, ELit LEmptyError -> ()
    | ELit LTrue, _ -> progress cons
    | ELit LFalse, _ -> progress_defaults_just_false e just cons subs
  end else ()
and progress_defaults_just_false
  (e: exp)
  (just: exp{just << e})
  (cons: exp{cons << e})
  (subs: list exp{subs << e}) : Lemma
    (requires (
      ~ (is_value e) /\ just == ELit LFalse /\
      e == EDefault (ELit LFalse) cons subs /\ Some? (typing empty e)
    ))
    (ensures (Some? (step_subdefaults_just_false e just cons subs)))
    (decreases %[e; 1])
  =
  if List.Tot.for_all (fun sub -> is_value sub) subs then () else
  progress_defaults_left_to_right e just cons subs
and progress_defaults_left_to_right
  (e: exp)
  (just: exp{just << e})
  (cons: exp{cons << e})
  (subs: list exp{subs << e}) : Lemma
    (requires (
      ~ (is_value e) /\ just == ELit LFalse /\
      Some? (typing empty (EDefault just cons subs))
    ))
    (ensures (Some? (step_subdefaults_left_to_right e just cons subs)))
    (decreases %[e; 0; subs])
  =
  match subs with
  | [] -> ()
  | hd::tl ->
    progress hd;
    if is_value hd then begin
      assert(Some?.v (typing empty just) == TRBool);
      let tcons = Some?.v (typing empty cons) in
      typing_conserved_by_list_reduction empty just cons subs;
      progress_defaults_left_to_right e just cons tl
    end else ()
#pop-options

let rec appears_free_in (x: var) (e: exp) : Tot bool =
  match e with
  | EVar y -> x = y
  | EApp e1 e2 -> appears_free_in x e1 || appears_free_in x e2
  | EAbs y _ e1 -> x <> y && appears_free_in x e1
  | EIf e1 e2 e3 ->
      appears_free_in x e1 || appears_free_in x e2 || appears_free_in x e3
  | EDefault ejust econs subs ->
      appears_free_in x ejust || appears_free_in x econs ||
      appears_free_in_list x subs
  | ELit _ ->  false
and appears_free_in_list (x: var) (subs: list exp) : Tot bool =
  match subs with
  | [] -> false
  | hd::tl -> appears_free_in x hd || appears_free_in_list x tl

let rec free_in_context (x:int) (e:exp) (g:env) : Lemma
      (requires (Some? (typing g e)))
      (ensures (appears_free_in x e ==> Some? (g x)))
  =
  match e with
  | EVar _
  | ELit _ -> ()
  | EAbs y t e1 -> free_in_context x e1 (extend g y t)
  | EApp e1 e2 -> free_in_context x e1 g; free_in_context x e2 g
  | EIf e1 e2 e3 -> free_in_context x e1 g;
                    free_in_context x e2 g; free_in_context x e3 g
  | EDefault ejust econs subs ->
    free_in_context x ejust g;
    free_in_context x econs g;
    free_in_context_list x subs g
and free_in_context_list (x:int) (subs:list exp) (g:env) : Lemma
      (requires (Some? (unify_list g subs)))
      (ensures (appears_free_in_list x subs ==> Some? (g x)))
  =
  match subs with
  | [] -> ()
  | hd::tl ->
    free_in_context x hd g;
    free_in_context_list x tl g

let typable_empty_closed (x:var) (e:exp) : Lemma
      (requires (Some? (typing empty e)))
      (ensures (not(appears_free_in x e)))
      [SMTPat (appears_free_in x e)]
   = free_in_context x e empty

type equal (g1:env) (g2:env) = forall (x:var). g1 x = g2 x

type equalE (e:exp) (g1:env) (g2:env) =
  forall (x:var). appears_free_in x e ==> g1 x = g2 x

type equalE_list (subs:list exp) (g1:env) (g2:env) =
  forall (x:var). appears_free_in_list x subs ==> g1 x = g2 x

let rec context_invariance (e:exp) (g:env) (g':env) : Lemma
  (requires (equalE e g g'))
  (ensures (typing g e == typing g' e))
  =
  match e with
  | EAbs x t e1 ->
     context_invariance e1 (extend g x t) (extend g' x t)
  | EApp e1 e2 ->
     context_invariance e1 g g';
     context_invariance e2 g g'
  | EIf e1 e2 e3 ->
     context_invariance e1 g g';
     context_invariance e2 g g';
     context_invariance e3 g g'
  | EDefault econs ejust subs ->
     context_invariance ejust g g';
     context_invariance econs g g';
     context_invariance_list subs g g'
  | _ -> ()
and context_invariance_list (subs:list exp) (g:env) (g':env) : Lemma
  (requires (equalE_list subs g g'))
  (ensures (unify_list g subs == unify_list g' subs))
  =
  match subs with
  | [] -> ()
  | hd::tl ->
    context_invariance hd g g';
    context_invariance_list tl g g'

let typing_extensional (g:env) (g':env) (e:exp) : Lemma
  (requires (equal g g'))
  (ensures (typing g e == typing g' e))
   = context_invariance e g g'

val substitution_preserves_typing : x:int -> e:exp -> v:exp -> g:env -> Lemma
  (requires (Some? (typing empty v) /\
             Some? (typing (extend g x (Some?.v (typing empty v))) e)))
  (ensures (Some? (typing empty v) /\
            typing g (subst x v e) ==
            typing (extend g x (Some?.v (typing empty v))) e))
let rec substitution_preserves_typing x e v g =
  let Some t_x = typing empty v in
  let gx = extend g x t_x in
  match e with
  | ELit _ -> ()
  | EVar y ->
     if x=y
     then context_invariance v empty g (* uses lemma typable_empty_closed *)
     else context_invariance e gx g
  | EApp e1 e2 ->
     substitution_preserves_typing x e1 v g;
     substitution_preserves_typing x e2 v g
  | EIf e1 e2 e3 ->
     substitution_preserves_typing x e1 v g;
     substitution_preserves_typing x e2 v g;
     substitution_preserves_typing x e3 v g
  | EAbs y t_y e1 ->
     let gxy = extend gx y t_y in
     let gy = extend g y t_y in
     if x=y
     then typing_extensional gxy gy e1
     else
       (let gyx = extend gy x t_x in
        typing_extensional gxy gyx e1;
        substitution_preserves_typing x e1 v gy)

val preservation : e:exp -> Lemma
  (requires (Some? (typing empty e) /\ Some? (step e) ))
  (ensures (Some? (step e) /\
            typing empty (Some?.v (step e)) == typing empty e))
let rec preservation e =
  match e with
  | EApp e1 e2 ->
     if is_value e1
     then (if is_value e2
           then let EAbs x _ ebody = e1 in
                substitution_preserves_typing x ebody e2 empty
           else preservation e2)
     else preservation e1

  | EIf e1 _ _ ->
      if not (is_value e1) then preservation e1
