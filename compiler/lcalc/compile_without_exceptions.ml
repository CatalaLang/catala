(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2022 Inria,
   contributor: Alain Delaët-Tixeuil <alain.delaet--tixeuil@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Catala_utils
module D = Dcalc.Ast
module A = Ast

(** The main idea around this pass is to compile Dcalc to Lcalc without using
    [raise EmptyError] nor [try _ with EmptyError -> _]. To do so, we use the
    same technique as in rust or erlang to handle this kind of exceptions. Each
    [raise EmptyError] will be translated as [None] and each
    [try e1 with EmtpyError -> e2] as
    [match e1 with | None -> e2 | Some x -> x].

    When doing this naively, this requires to add matches and Some constructor
    everywhere. We apply here an other technique where we generate what we call
    `hoists`. Hoists are expression whom could minimally [raise EmptyError]. For
    instance in [let x = <e1, e2, ..., en| e_just :- e_cons> * 3 in x + 1], the
    sub-expression [<e1, e2, ..., en| e_just :- e_cons>] can produce an empty
    error. So we make a hoist with a new variable [y] linked to the Dcalc
    expression [<e1, e2, ..., en| e_just :- e_cons>], and we return as the
    translated expression [let x = y * 3 in x + 1].

    The compilation of expressions is found in the functions
    [translate_and_hoist ctx e] and [translate_expr ctx e]. Every
    option-generating expression when calling [translate_and_hoist] will be
    hoisted and later handled by the [translate_expr] function. Every other
    cases is found in the translate_and_hoist function.

    Problem arise when there is a function application. *)

open Shared_ast

type analysis_mark = {
  pos : Pos.t;
  ty : typ;
  unpure : bool;
  unpure_return : bool option;
}

(* voir sur papier pour voir si ça marche *)

type analysis_info = { unpure_info : bool; unpure_return : bool option }
(* type analysis_ctx = (dcalc, analysis_info) Var.Map.t *)

let make_new_mark (m : typed mark) ?(unpure_return = None) (unpure : bool) :
    analysis_mark =
  match m with
  | Typed m ->
    begin
      match Marked.unmark m.ty, unpure_return with
      | TArrow _, None ->
        Errors.raise_error
          "Internal Error: no pure/unpure return type commentary on a function."
      | _ -> ()
    end;
    { pos = m.pos; ty = m.ty; unpure; unpure_return }

let rec detect_unpure_expr ctx (e : (dcalc, typed mark) gexpr) :
    (dcalc, analysis_mark) boxed_gexpr =
  let m = Marked.get_mark e in
  match Marked.unmark e with
  | EVar x ->
    (* we suppose we don't need any information on a variable containing a
       function, because the only place such variable [f] can appear is in a
       EApp {f; ...} position. Hence it will be matched elsewhere. This is kept
       by the following invariant: *)
    Errors.assert_internal_error
      (match Marked.unmark (Expr.ty e) with TArrow _ -> false | _ -> true)
      "The variable %a should not be a function in this context." Print.var x;
    Expr.make_var (Var.translate x)
      (make_new_mark m (Var.Map.find x ctx).unpure_info)
  | EAbs { binder; tys } ->
    let vars, body = Bindlib.unmbind binder in
    let body' = detect_unpure_expr ctx body in
    let binder' = Expr.bind (Array.map Var.translate vars) body' in
    (* eabs is a value, hence is always pure. However, it is possible the
       function returns something that is pure. In this case the information
       needs to be backpropagated somewhere. *)
    Expr.eabs binder' tys
      (make_new_mark m false
         ~unpure_return:(Some (Marked.get_mark body').unpure))
  | EDefault { excepts; just; cons } ->
    let excepts' = List.map (detect_unpure_expr ctx) excepts in
    let just' = detect_unpure_expr ctx just in
    let cons' = detect_unpure_expr ctx cons in
    (* because of the structural invariant, there is no functions inside an
       default. Hence, there is no need for any verification here. *)
    Expr.edefault excepts' just' cons' (make_new_mark m true)
  | ELit l ->
    Expr.elit l
      (make_new_mark m (match l with LEmptyError -> true | _ -> false))
  | EErrorOnEmpty arg ->
    let arg' = detect_unpure_expr ctx arg in
    (* the result is always pure *)
    Expr.eerroronempty arg' (make_new_mark m false)
  | EApp { f = (EVar x, _) as f; args } ->
    let args' = List.map (detect_unpure_expr ctx) args in
    let unpure =
      args'
      |> List.map (fun arg -> (Marked.get_mark arg).unpure)
      |> List.fold_left ( || ) false
      |> ( || ) (Var.Map.find x ctx).unpure_info
    in
    let f' = detect_unpure_expr ctx f in
    if Option.get (Var.Map.find x ctx).unpure_return then
      Expr.eapp f' args' (make_new_mark m (true || unpure))
    else Expr.eapp f' args' (make_new_mark m unpure)
  | EApp { f = (EAbs _, _) as f; args } ->
    let f' = detect_unpure_expr ctx f in
    let args' = List.map (detect_unpure_expr ctx) args in
    let unpure =
      args'
      |> List.map (fun arg -> (Marked.get_mark arg).unpure)
      |> List.fold_left ( || ) false
      |> ( || ) (Marked.get_mark f').unpure
      ||
      match (Marked.get_mark f').unpure_return with
      | None ->
        Errors.raise_internal_error
          "A function has no information on whenever it is empty or not"
      | Some unpure_return -> unpure_return
    in
    Expr.eapp f' args' (make_new_mark m unpure)
  | EApp { f = EApp { f = EOp { op = Op.Log _; _ }, _; args = _ }, _; _ } ->
    assert false
  | EApp { f = EStructAccess _, _; _ } -> assert false
  (* Now operator application. Those come in multiple shape and forms: either
     the operator is an EOp, or it is an array, ifthenelse, struct, inj, match,
     structAccess, tuple, tupleAccess, Assert.

     Note that for the moment, we consider the ifthenelse an normal if then
     else, and not the selective applicative functor corresponding. *)
  | EApp { f = EOp { op; tys }, opmark; args } ->
    let args' = List.map (detect_unpure_expr ctx) args in
    let unpure =
      args'
      |> List.map (fun arg -> (Marked.get_mark arg).unpure)
      |> List.fold_left ( || ) false
    in
    Expr.eapp
      (Expr.eop op tys (make_new_mark opmark true))
      args' (make_new_mark m unpure)
  | EArray args ->
    let args = List.map (detect_unpure_expr ctx) args in
    let unpure =
      args
      |> List.map (fun arg -> (Marked.get_mark arg).unpure)
      |> List.fold_left ( || ) false
    in
    Expr.earray args (make_new_mark m unpure)
  | EStruct { name; fields } ->
    let fields = StructField.Map.map (detect_unpure_expr ctx) fields in
    let unpure =
      fields
      |> StructField.Map.map (fun field -> (Marked.get_mark field).unpure)
      |> fun ctx -> StructField.Map.fold (fun _ -> ( || )) ctx false
    in
    Expr.estruct name fields (make_new_mark m unpure)
  | EIfThenElse { cond; etrue; efalse } ->
    let cond = detect_unpure_expr ctx cond in
    let etrue = detect_unpure_expr ctx etrue in
    let efalse = detect_unpure_expr ctx efalse in
    let unpure =
      [cond; etrue; efalse]
      |> List.map (fun arg -> (Marked.get_mark arg).unpure)
      |> List.fold_left ( || ) false
    in
    Expr.eifthenelse cond etrue efalse (make_new_mark m unpure)
  | EInj { name; e; cons } ->
    let e = detect_unpure_expr ctx e in
    let unpure = (Marked.get_mark e).unpure in
    Expr.einj e cons name (make_new_mark m unpure)
  | EMatch { name; e; cases } ->
    let e = detect_unpure_expr ctx e in
    let cases = EnumConstructor.Map.map (detect_unpure_expr ctx) cases in
    let unpure =
      e :: List.map snd (EnumConstructor.Map.bindings cases)
      |> List.map (fun arg -> (Marked.get_mark arg).unpure)
      |> List.fold_left ( || ) false
    in
    Expr.ematch e name cases (make_new_mark m unpure)
  | EStructAccess { name; e; field } ->
    let e = detect_unpure_expr ctx e in
    let unpure = (Marked.get_mark e).unpure in
    Expr.estructaccess e field name (make_new_mark m unpure)
  | ETuple args ->
    let args = List.map (detect_unpure_expr ctx) args in
    let unpure =
      args
      |> List.map (fun arg -> (Marked.get_mark arg).unpure)
      |> List.fold_left ( || ) false
    in
    Expr.etuple args (make_new_mark m unpure)
  | ETupleAccess { e; index; size } ->
    let e = detect_unpure_expr ctx e in
    let unpure = (Marked.get_mark e).unpure in
    Expr.etupleaccess e index size (make_new_mark m unpure)
  | EAssert e ->
    let e = detect_unpure_expr ctx e in
    let unpure = (Marked.get_mark e).unpure in
    Expr.eassert e (make_new_mark m unpure)
  (* Those cases should not happend because of the structural invariant on the
     structure of the ast at this point. *)
  | EApp _ -> assert false (* invalid invariant *)
  | EOp _ -> assert false (* invalid invariant *)

let _ = detect_unpure_expr

(** [{
      type struct_ctx_analysis = bool StructField.Map.t StructName.Map.t
    }]

    [{ let rec detect_unpure_expr = assert false }]
    [{ let detect_unpure_scope_let = assert false }]
    [{ let detect_unpure_scope_body = assert false }]
    [{ let detect_unpure_scopes = assert false }]
    [{ let detect_unpure_program = assert false }]
    [{ let detect_unpure_scope_let = assert false }] *)

(* let detect_unpure_code_item (ctx : analysis_ctx) var (code_item : _
   code_item) : analysis_ctx * _ code_item Bindlib.box = match code_item with _
   -> assert false

   let detect_unpure_code_item_list ctx code_items = Scope.fold_map
   ~f:detect_unpure_code_item ~varf:Fun.id ctx code_items *)

type ctx2

let find : ctx2 -> analysis_info = fun _ -> assert false

let add_var : ctx2 -> 'a Bindlib.var -> analysis_info -> ctx2 =
 fun _ -> assert false

let rec is_pure ctx (expr : typed D.expr) =
  match Marked.unmark expr with
  | ELit l -> ( match l with LEmptyError -> true | _ -> false)
  | EApp { f = EOp _, _; args } -> List.for_all (is_pure ctx) args
  | EApp { f = EAbs { binder; _ }, _; args } ->
    let vars, body = Bindlib.unmbind binder in
    let vars = Array.to_list vars in
    let ctx' =
      ListLabels.fold_left2 vars args ~init:ctx ~f:(fun ctx var arg ->
          add_var ctx var
            {
              unpure_info = false;
              unpure_return = Option.map not (return_pure ctx arg);
            })
    in
    is_pure ctx' body
  | EApp { f = (EVar _, _) as f; args } ->
    List.for_all (is_pure ctx) args && Option.get (return_pure ctx f)
  | EVar _ -> not (find ctx).unpure_info
  | EAbs _ -> true
  | EDefault _ -> false
  | EErrorOnEmpty _ -> true
  | EArray _ | EIfThenElse _ | EStruct _ | EInj _ | EMatch _ | ETuple _
  | ETupleAccess _ | EStructAccess _ | EAssert _ ->
    failwith "not implemented yet"
  | EApp { f = EApp { f = EOp { op = Op.Log _; _ }, _; args = _ }, _; _ } ->
    assert false
  | EOp _ | EApp _ -> failwith "wont implement"

and return_pure _ctx _expr = assert false

let _ = is_pure

type 'm hoists = ('m A.expr, 'm D.expr) Var.Map.t
(** Hoists definition. It represent bindings between [A.Var.t] and [D.expr]. *)

type 'm info = { expr : 'm A.expr boxed; var : 'm A.expr Var.t; is_pure : bool }
(** Information about each encontered Dcalc variable is stored inside a context
    : what is the corresponding LCalc variable; an expression corresponding to
    the variable build correctly using Bindlib, and a boolean `is_pure`
    indicating whenever the variable can be an EmptyError and hence should be
    matched (false) or if it never can be EmptyError (true). *)

let pp_info (fmt : Format.formatter) (info : 'm info) =
  Format.fprintf fmt "{var: %a; is_pure: %b}" Print.var info.var info.is_pure

type 'm ctx = {
  decl_ctx : decl_ctx;
  vars : ('m D.expr, 'm info) Var.Map.t;
      (** information context about variables in the current scope *)
}

let _pp_ctx (fmt : Format.formatter) (ctx : 'm ctx) =
  let pp_binding
      (fmt : Format.formatter)
      ((v, info) : 'm D.expr Var.t * 'm info) =
    Format.fprintf fmt "%a: %a" Print.var v pp_info info
  in

  let pp_bindings =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
      pp_binding
  in

  Format.fprintf fmt "@[<2>[%a]@]" pp_bindings (Var.Map.bindings ctx.vars)

(** [find ~info n ctx] is a warpper to ocaml's Map.find that handle errors in a
    slightly better way. *)
let find ?(info : string = "none") (n : 'm D.expr Var.t) (ctx : 'm ctx) :
    'm info =
  try Var.Map.find n ctx.vars
  with Not_found ->
    Errors.raise_spanned_error Pos.no_pos
      "Internal Error: Variable %a was not found in the current environment. \
       Additional informations : %s."
      Print.var n info

(** [add_var pos var is_pure ctx] add to the context [ctx] the Dcalc variable
    var, creating a unique corresponding variable in Lcalc, with the
    corresponding expression, and the boolean is_pure. It is usefull for
    debuging purposes as it printing each of the Dcalc/Lcalc variable pairs. *)
let add_var
    (mark : 'm mark)
    (var : 'm D.expr Var.t)
    (is_pure : bool)
    (ctx : 'm ctx) : 'm ctx =
  let new_var = Var.make (Bindlib.name_of var) in
  let expr = Expr.make_var new_var mark in

  {
    ctx with
    vars =
      Var.Map.update var
        (fun _ -> Some { expr; var = new_var; is_pure })
        ctx.vars;
  }

(** [tau' = translate_typ tau] translate the a dcalc type into a lcalc type.

    Since positions where there is thunked expressions is exactly where we will
    put option expressions. Hence, the transformation simply reduce [unit -> 'a]
    into ['a option] recursivly. There is no polymorphism inside catala. *)
let rec translate_typ (tau : typ) : typ =
  (Fun.flip Marked.same_mark_as)
    tau
    begin
      match Marked.unmark tau with
      | TLit l -> TLit l
      | TTuple ts -> TTuple (List.map translate_typ ts)
      | TStruct s -> TStruct s
      | TEnum en -> TEnum en
      | TOption _ -> assert false
      | TAny -> TAny
      | TArray ts -> TArray (translate_typ ts)
      (* catala is not polymorphic *)
      | TArrow ([(TLit TUnit, _)], t2) -> TOption (translate_typ t2)
      | TArrow (t1, t2) -> TArrow (List.map translate_typ t1, translate_typ t2)
    end

(** [c = disjoint_union_maps cs] Compute the disjoint union of multiple maps.
    Raises an internal error if there is two identicals keys in differnts parts. *)
let disjoint_union_maps (pos : Pos.t) (cs : ('e, 'a) Var.Map.t list) :
    ('e, 'a) Var.Map.t =
  let disjoint_union =
    Var.Map.union (fun _ _ _ ->
        Errors.raise_spanned_error pos
          "Internal Error: Two supposed to be disjoints maps have one shared \
           key.")
  in

  List.fold_left disjoint_union Var.Map.empty cs

(** errorOnEmpty : 'a option -> 'a

    pure var: 'a

    unpure var: 'a option

    default: 'a option list -> bool -> 'a option -> 'a option

    empty: 'a option

    ELit : 'a

    let x = arg in body := bind [arg] (fun x: 'a -> [body])

    app f x1 x2 x3 x4 = bindn [x1; x2; x3; x4] (f x1 x2 x3 x4)

    match arg with case1 case2 := bind arg (fun x -> match x with case1 case2) *)

let monad_return e ~(mark : 'a mark) =
  Expr.einj e Ast.some_constr Ast.option_enum mark

let monad_empty ~(mark : 'a mark) =
  Expr.einj (Expr.elit LUnit mark) Ast.none_constr Ast.option_enum mark

let monad_bind_aux f arg ~(mark : 'a mark) =
  let cases =
    EnumConstructor.Map.of_seq
      (List.to_seq
         [
           ( Ast.none_constr,
             let x = Var.make "x" in
             Expr.eabs
               (Expr.bind [| x |]
                  (Expr.einj (Expr.evar x mark) Ast.none_constr Ast.option_enum
                     mark))
               [TAny, Expr.mark_pos mark]
               mark );
           (* | None x -> None x *)
           ( Ast.some_constr,
             let x = Var.make "x" in
             Expr.eabs
               (Expr.bind [| x |]
                  (Expr.einj (f x) Ast.some_constr Ast.option_enum mark))
               [TAny, Expr.mark_pos mark]
               mark )
           (*| Some x -> Some x *);
         ])
  in
  Expr.ematch arg Ast.option_enum cases mark

let monad_bind_var f x arg ~(mark : 'a mark) =
  let cases =
    EnumConstructor.Map.of_seq
      (List.to_seq
         [
           ( Ast.none_constr,
             let x = Var.make "x" in
             Expr.eabs
               (Expr.bind [| x |]
                  (Expr.einj (Expr.evar x mark) Ast.none_constr Ast.option_enum
                     mark))
               [TAny, Expr.mark_pos mark]
               mark );
           (* | None x -> None x *)
           ( Ast.some_constr,
             Expr.eabs
               (Expr.bind [| x |]
                  (Expr.einj f Ast.some_constr Ast.option_enum mark))
               [TAny, Expr.mark_pos mark]
               mark )
           (*| Some x -> Some x *);
         ])
  in
  Expr.ematch arg Ast.option_enum cases mark

let monad_bind f arg ~(mark : 'a mark) =
  let x = Var.make "x" in
  monad_bind_var f x arg ~mark

let monad_bind_cont f arg ~(mark : 'a mark) =
  let x = Var.make "x" in
  monad_bind_var (f x) x arg ~mark

let monad_mbind_mvar f xs args ~(mark : 'a mark) =
  (* match e1, ..., en with | Some e1', ..., Some en' -> f (e1, ..., en) | _ ->
     None *)
  ListLabels.fold_left2 xs args ~f:(monad_bind_var ~mark)
    ~init:(Expr.eapp f (List.map (fun v -> Expr.evar v mark) xs) mark)

let monad_mbind f args ~(mark : 'a mark) =
  (* match e1, ..., en with | Some e1', ..., Some en' -> f (e1, ..., en) | _ ->
     None *)
  let vars =
    ListLabels.mapi args ~f:(fun i _ -> Var.make (Format.sprintf "e_%i" i))
  in
  monad_mbind_mvar f vars args ~mark

let monad_mbind_cont f args ~(mark : 'a mark) =
  let vars =
    ListLabels.mapi args ~f:(fun i _ -> Var.make (Format.sprintf "e_%i" i))
  in
  monad_mbind_mvar (f vars) vars args ~mark

let monad_map_var f x arg ~(mark : 'a mark) =
  monad_bind_var (monad_return f ~mark) x arg ~mark

let monad_map f x arg ~(mark : 'a mark) =
  let x = Var.make "x" in
  monad_map_var f x arg ~mark

let monad_mmap_mvar f xs args ~(mark : 'a mark) =
  monad_mbind_mvar (monad_return f ~mark) xs args ~mark

let monad_mmap f args ~(mark : 'a mark) =
  let vars =
    ListLabels.mapi args ~f:(fun i _ -> Var.make (Format.sprintf "e_%i" i))
  in
  monad_mmap_mvar f vars args ~mark

let monad_eoe arg ~(mark : 'a mark) =
  let cases =
    EnumConstructor.Map.of_seq
      (List.to_seq
         [
           ( Ast.none_constr,
             let x = Var.make "x" in
             Expr.eabs
               (Expr.bind [| x |] (Expr.eraise NoValueProvided mark))
               [TAny, Expr.mark_pos mark]
               mark );
           (* | None x -> raise NoValueProvided *)
           Ast.some_constr, Expr.eid mark (* | Some x -> x*);
         ])
  in
  Expr.ematch arg Ast.option_enum cases mark

let _ = monad_return
let _ = monad_empty
let _ = monad_bind_aux
let _ = monad_bind_var
let _ = monad_bind
let _ = monad_bind_cont
let _ = monad_mbind_mvar
let _ = monad_mbind
let _ = monad_mbind_cont
let _ = monad_eoe
let trans_var _ctx (x : 'm D.expr Var.t) : 'm Ast.expr Var.t = Var.translate x

(* TODO: louis ? :) *)
let trans_op : (dcalc, 'a) Op.t -> (lcalc, 'a) Op.t = Obj.magic
let admit = assert false

let rec trans ctx (e : 'm D.expr) : (lcalc, 'm mark) boxed_gexpr =
  let m = Marked.get_mark e in
  let mark = m in
  let pos = Expr.pos e in
  match Marked.unmark e with
  | EVar x ->
    if (Var.Map.find x ctx).is_pure then
      monad_return (Expr.evar (trans_var ctx x) m) ~mark
    else Expr.evar (trans_var ctx x) m
  | EApp { f = EVar v, _; args = [(ELit LUnit, _)] } ->
    (* lazy *)
    assert (not (Var.Map.find v ctx).is_pure);
    Expr.evar (trans_var ctx v) m
  | EAbs { binder; tys } ->
    (* this is to be used with monad_bind. *)
    let vars, body = Bindlib.unmbind binder in
    let ctx' =
      assert
        false (* addvars vars (ArrayLabels.map vars ~f:(Fun.const true)) ctx *)
    in
    let body' = trans ctx' body in
    let binder' = Expr.bind (Array.map Var.translate vars) body' in
    Expr.eabs binder' tys m
  | EDefault { excepts; just; cons } ->
    let excepts' = List.map (trans ctx) excepts in
    let just' = trans ctx just in
    let cons' = trans ctx cons in

    (* for each e in excepts, e: 'a option. just: bool. cons': 'a option.
       Result: 'a option*)
    let m' = match m with Typed m -> Typed { m with ty = TAny, pos } in
    (* App Handle_default excepts just cons *)
    Expr.make_app
      (Expr.make_var (Var.translate A.handle_default_opt) m')
      [Expr.earray excepts' m; just'; cons']
      pos
  | ELit l -> begin
    match l with
    | LEmptyError -> monad_empty ~mark
    (* gadts cannot infer l is in fact lcalc glit. Hence, we explicit it. *)
    | (LBool _ | LInt _ | LRat _ | LMoney _ | LUnit | LDate _ | LDuration _) as
      l ->
      monad_return ~mark (Expr.elit l m)
  end
  | EErrorOnEmpty arg ->
    let arg' = trans ctx arg in
    monad_eoe arg' ~mark
  | EApp { f = (EVar _, _) as f; args } ->
    (* INVARIANT: functions are always encoded using this function. *)
    let f_var = Var.make "f" in
    monad_bind_var ~mark (trans ctx f) f_var
      (monad_mbind (Expr.evar f_var mark) (List.map (trans ctx) args) ~mark)
  | EApp { f = EAbs { binder; tys }, _; args } ->
    let var, body = Bindlib.unmbind binder in
    let[@warning "-8"] [| var |] = var in
    let var' = Var.translate var in
    let[@warning "-8"] [arg] = args in
    let ctx' = assert false in
    monad_bind_var (trans ctx arg) var' (trans ctx' body) ~mark
  | EApp { f = EApp { f = EOp { op = Op.Log _; _ }, _; args = _ }, _; _ } ->
    assert false
  | EApp { f = EStructAccess _, _; _ } -> assert false
  | EApp { f = EOp { op; tys }, opmark; args } ->
    monad_mbind
      (Expr.eop (trans_op op) tys opmark)
      (List.map (trans ctx) args)
      ~mark
  | EMatch { name; e; cases } ->
    let cases =
      EnumConstructor.MapLabels.map cases ~f:(fun case ->
          match Marked.unmark case with
          | EAbs { binder; tys } ->
            let vars, body = Bindlib.unmbind binder in
            let ctx' =
              addvars vars (ArrayLabels.map vars ~f:(Fun.const true)) ctx
            in
            let binder =
              Expr.bind (Array.map Var.translate vars) (trans ctx' body)
            in
            Expr.eabs binder tys m
          | _ -> assert false)
    in
    monad_bind_cont
      (fun e -> Expr.ematch (Expr.evar e m) name cases m)
      (trans ctx e) ~mark
  | EArray args ->
    monad_mbind_cont ~mark
      (fun vars -> Expr.earray (List.map (fun v -> Expr.evar v m) vars) mark)
      (List.map (trans ctx) args)
  | EStruct { name; fields } ->
    (* TODO: since ocaml is determinisitc, the fields will always be enumerated
       in the same order on the fields. *)
    let fields_name, fields = List.split (StructField.Map.bindings fields) in
    monad_mbind_cont
      (fun xs ->
        let fields =
          ListLabels.fold_right2 fields_name
            (List.map (fun x -> Expr.evar x mark) xs)
            ~f:StructField.Map.add ~init:StructField.Map.empty
        in
        Expr.estruct name fields mark)
      (List.map (trans ctx) fields)
      ~mark
  | EIfThenElse { cond; etrue; efalse } ->
    (* semantic one : ife is not rendondant with defaults. *)
    (* monad_bind_cont ~mark (fun cond -> monad_bind_cont (fun etrue ->
       monad_bind_cont ~mark (fun efalse -> Expr.eifthenelse (Expr.evar cond m)
       (Expr.evar etrue m) (Expr.evar efalse m) m) (trans ctx efalse)) (trans
       ctx etrue) ~mark) (trans ctx cond) *)

    (* semantic two: ife is redondant with defaults. *)
    monad_bind_cont ~mark
      (fun cond ->
        Expr.eifthenelse (Expr.evar cond mark) (trans ctx etrue)
          (trans ctx efalse) mark)
      (trans ctx cond)
  | EInj { name; cons; e } ->
    monad_bind_cont
      (fun e -> Expr.einj (Expr.evar e mark) cons name mark)
      (trans ctx e) ~mark
  | EStructAccess { name; e; field } ->
    monad_bind_cont
      (fun e -> Expr.estructaccess (Expr.evar e mark) field name mark)
      (trans ctx e) ~mark
  | ETuple args ->
    monad_mbind_cont
      (fun xs -> Expr.etuple (List.map (fun x -> Expr.evar x mark) xs) mark)
      (List.map (trans ctx) args)
      ~mark
  | ETupleAccess { e; index; size } ->
    monad_bind_cont
      (fun e -> Expr.etupleaccess (Expr.evar e mark) index size mark)
      (trans ctx e) ~mark
  | EAssert e ->
    monad_bind_cont
      (fun e -> Expr.eassert (Expr.evar e mark) mark)
      (trans ctx e) ~mark
  | EApp _ -> assert false
  | EOp _ -> assert false

let _ = trans

let rec translate_scope_let (ctx : 'm ctx) (lets : 'm D.expr scope_body_expr) :
    'm A.expr scope_body_expr Bindlib.box =
  match lets with
  | Result e ->
    Bindlib.box_apply
      (fun e -> Result e)
      (Expr.Box.lift (translate_expr ~append_esome:false ctx e))
  | ScopeLet
      {
        scope_let_kind = SubScopeVarDefinition;
        scope_let_typ = typ;
        scope_let_expr = EAbs { binder; _ }, emark;
        scope_let_next = next;
        scope_let_pos = pos;
      } ->
    (* special case : the subscope variable is thunked (context i/o). We remove
       this thunking. *)
    let _, expr = Bindlib.unmbind binder in

    let var_is_pure = true in
    let var, next = Bindlib.unbind next in
    (* Cli.debug_print @@ Format.asprintf "unbinding %a" Print.var var; *)
    let vmark = Expr.with_ty emark ~pos typ in
    let ctx' = add_var vmark var var_is_pure ctx in
    let new_var = (find ~info:"variable that was just created" var ctx').var in
    let new_next = translate_scope_let ctx' next in
    Bindlib.box_apply2
      (fun new_expr new_next ->
        ScopeLet
          {
            scope_let_kind = SubScopeVarDefinition;
            scope_let_typ = translate_typ typ;
            scope_let_expr = new_expr;
            scope_let_next = new_next;
            scope_let_pos = pos;
          })
      (Expr.Box.lift (translate_expr ctx ~append_esome:false expr))
      (Bindlib.bind_var new_var new_next)
  | ScopeLet
      {
        scope_let_kind = SubScopeVarDefinition;
        scope_let_typ = typ;
        scope_let_expr = (EErrorOnEmpty _, emark) as expr;
        scope_let_next = next;
        scope_let_pos = pos;
      } ->
    (* special case: regular input to the subscope *)
    let var_is_pure = true in
    let var, next = Bindlib.unbind next in
    (* Cli.debug_print @@ Format.asprintf "unbinding %a" Print.var var; *)
    let vmark = Expr.with_ty emark ~pos typ in
    let ctx' = add_var vmark var var_is_pure ctx in
    let new_var = (find ~info:"variable that was just created" var ctx').var in
    Bindlib.box_apply2
      (fun new_expr new_next ->
        ScopeLet
          {
            scope_let_kind = SubScopeVarDefinition;
            scope_let_typ = translate_typ typ;
            scope_let_expr = new_expr;
            scope_let_next = new_next;
            scope_let_pos = pos;
          })
      (Expr.Box.lift (translate_expr ctx ~append_esome:false expr))
      (Bindlib.bind_var new_var (translate_scope_let ctx' next))
  | ScopeLet
      {
        scope_let_kind = SubScopeVarDefinition;
        scope_let_pos = pos;
        scope_let_expr = expr;
        _;
      } ->
    Errors.raise_spanned_error pos
      "Internal Error: found an SubScopeVarDefinition that does not satisfy \
       the invariants when translating Dcalc to Lcalc without exceptions: \
       @[<hov 2>%a@]"
      (Expr.format ctx.decl_ctx) expr
  | ScopeLet
      {
        scope_let_kind = kind;
        scope_let_typ = typ;
        scope_let_expr = expr;
        scope_let_next = next;
        scope_let_pos = pos;
      } ->
    let var_is_pure =
      match kind with
      | DestructuringInputStruct -> (
        (* Here, we have to distinguish between context and input variables. We
           can do so by looking at the typ of the destructuring: if it's
           thunked, then the variable is context. If it's not thunked, it's a
           regular input. *)
        match Marked.unmark typ with
        | TArrow ([(TLit TUnit, _)], _) -> false
        | _ -> true)
      | ScopeVarDefinition | SubScopeVarDefinition | CallingSubScope
      | DestructuringSubScopeResults | Assertion ->
        true
    in
    let var, next = Bindlib.unbind next in
    (* Cli.debug_print @@ Format.asprintf "unbinding %a" Print.var var; *)
    let vmark = Expr.with_ty (Marked.get_mark expr) ~pos typ in
    let ctx' = add_var vmark var var_is_pure ctx in
    let new_var = (find ~info:"variable that was just created" var ctx').var in
    Bindlib.box_apply2
      (fun new_expr new_next ->
        ScopeLet
          {
            scope_let_kind = kind;
            scope_let_typ = translate_typ typ;
            scope_let_expr = new_expr;
            scope_let_next = new_next;
            scope_let_pos = pos;
          })
      (Expr.Box.lift (translate_expr ctx ~append_esome:false expr))
      (Bindlib.bind_var new_var (translate_scope_let ctx' next))

let translate_scope_body
    (scope_pos : Pos.t)
    (ctx : 'm ctx)
    (body : typed D.expr scope_body) : 'm A.expr scope_body Bindlib.box =
  match body with
  | {
   scope_body_expr = result;
   scope_body_input_struct = input_struct;
   scope_body_output_struct = output_struct;
  } ->
    let v, lets = Bindlib.unbind result in
    let vmark =
      let m =
        match lets with
        | Result e | ScopeLet { scope_let_expr = e; _ } -> Marked.get_mark e
      in
      Expr.map_mark (fun _ -> scope_pos) (fun ty -> ty) m
    in
    let ctx' = add_var vmark v true ctx in
    let v' = (find ~info:"variable that was just created" v ctx').var in
    Bindlib.box_apply
      (fun new_expr ->
        {
          scope_body_expr = new_expr;
          scope_body_input_struct = input_struct;
          scope_body_output_struct = output_struct;
        })
      (Bindlib.bind_var v' (translate_scope_let ctx' lets))

let translate_code_items (ctx : 'm ctx) (scopes : 'm D.expr code_item_list) :
    'm A.expr code_item_list Bindlib.box =
  let _ctx, scopes =
    Scope.fold_map
      ~f:
        (fun ctx var -> function
          | Topdef (name, ty, e) ->
            ( add_var (Marked.get_mark e) var true ctx,
              Bindlib.box_apply
                (fun e -> Topdef (name, ty, e))
                (Expr.Box.lift (translate_expr ~append_esome:false ctx e)) )
          | ScopeDef (scope_name, scope_body) ->
            ( ctx,
              let scope_pos = Marked.get_mark (ScopeName.get_info scope_name) in
              Bindlib.box_apply
                (fun body -> ScopeDef (scope_name, body))
                (translate_scope_body scope_pos ctx scope_body) ))
      ~varf:Var.translate ctx scopes
  in
  scopes

let translate_program (prgm : typed D.program) : 'm A.program =
  let inputs_structs =
    Scope.fold_left prgm.code_items ~init:[] ~f:(fun acc def _ ->
        match def with
        | ScopeDef (_, body) -> body.scope_body_input_struct :: acc
        | Topdef _ -> acc)
  in
  (* Cli.debug_print @@ Format.asprintf "List of structs to modify: [%a]"
     (Format.pp_print_list D.StructName.format_t) inputs_structs; *)
  let decl_ctx =
    {
      prgm.decl_ctx with
      ctx_enums =
        prgm.decl_ctx.ctx_enums
        |> EnumName.Map.add A.option_enum A.option_enum_config;
    }
  in
  let decl_ctx =
    {
      decl_ctx with
      ctx_structs =
        prgm.decl_ctx.ctx_structs
        |> StructName.Map.mapi (fun n str ->
               if List.mem n inputs_structs then
                 StructField.Map.map translate_typ str
                 (* Cli.debug_print @@ Format.asprintf "Input type: %a"
                    (Print.typ decl_ctx) tau; Cli.debug_print @@ Format.asprintf
                    "Output type: %a" (Print.typ decl_ctx) (translate_typ
                    tau); *)
               else str);
    }
  in

  let code_items =
    Bindlib.unbox
      (translate_code_items { decl_ctx; vars = Var.Map.empty } prgm.code_items)
  in
  { decl_ctx; code_items }
