(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Nicolas Chataing
   <nicolas.chataing@ens.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Lambda_ast

type uid = int

type scope_uid = int

module UidMap = Uid.UidMap
module UidSet = Uid.UidSet

type exec_context = Lambda_ast.untyped_term UidMap.t

let empty_exec_ctxt = UidMap.empty

let raise_default_conflict (id : string Pos.marked) (true_pos : Pos.t list) (false_pos : Pos.t list)
    =
  if List.length true_pos = 0 then
    let justifications : (string option * Pos.t) list =
      List.map (fun pos -> (Some "This justification is false:", pos)) false_pos
    in
    Errors.raise_multispanned_error
      (Printf.sprintf "Default logic error for variable %s: no justification is true."
         (Pos.unmark id))
      ( ( Some (Printf.sprintf "The error concerns this variable %s" (Pos.unmark id)),
          Pos.get_position id )
      :: justifications )
  else
    let justifications : (string option * Pos.t) list =
      List.map (fun pos -> (Some "This justification is true:", pos)) true_pos
    in
    Errors.raise_multispanned_error
      "Default logic conflict, multiple justifications are true but are not related by a precedence"
      ( ( Some (Printf.sprintf "The conflict concerns this variable %s" (Pos.unmark id)),
          Pos.get_position id )
      :: justifications )

let rec eval_term (top_uid : Uid.t) (exec_ctxt : exec_context) (term : Lambda_ast.term) :
    Lambda_ast.term =
  let (term, pos), typ = term in
  let evaled_term =
    match term with
    | EFun _ | EInt _ | EDec _ | EBool _ | EOp _ -> term (* already a value *)
    | EVar uid -> (
        match UidMap.find_opt uid exec_ctxt with
        | Some t -> t
        | None ->
            Errors.raise_spanned_error
              (Printf.sprintf "Variable %s is not defined" (Uid.get_ident uid))
              pos )
    | EApp (f, args) -> (
        (* First evaluate and match the function body *)
        let f = f |> eval_term top_uid exec_ctxt |> Lambda_ast.untype in
        match f with
        | EFun (bindings, body) ->
            let exec_ctxt =
              List.fold_left2
                (fun ctxt arg (uid, _) ->
                  UidMap.add uid (arg |> eval_term top_uid exec_ctxt |> Lambda_ast.untype) ctxt)
                exec_ctxt args bindings
            in
            eval_term top_uid exec_ctxt body |> Lambda_ast.untype
        | EOp op -> (
            let args =
              List.map (fun arg -> arg |> eval_term top_uid exec_ctxt |> Lambda_ast.untype) args
            in
            match op with
            | Binop binop -> (
                match binop with
                | And | Or ->
                    let b1, b2 =
                      match args with [ EBool b1; EBool b2 ] -> (b1, b2) | _ -> assert false
                    in
                    EBool (if binop = And then b1 && b2 else b1 || b2)
                | _ -> (
                    let i1, i2 =
                      match args with [ EInt i1; EInt i2 ] -> (i1, i2) | _ -> assert false
                    in
                    match binop with
                    | Add | Sub | Mult | Div ->
                        let op_arith =
                          match binop with
                          | Add -> ( + )
                          | Sub -> ( - )
                          | Mult -> ( * )
                          | Div -> ( / )
                          | _ -> assert false
                        in
                        EInt (op_arith i1 i2)
                    | _ ->
                        let op_comp =
                          match binop with
                          | Lt -> ( < )
                          | Lte -> ( <= )
                          | Gt -> ( > )
                          | Gte -> ( >= )
                          | Eq -> ( = )
                          | Neq -> ( <> )
                          | _ -> assert false
                        in
                        EBool (op_comp i1 i2) ) )
            | Unop Minus -> ( match args with [ EInt i ] -> EInt (-i) | _ -> assert false )
            | Unop Not -> ( match args with [ EBool b ] -> EBool (not b) | _ -> assert false ) )
        | _ -> assert false )
    | EIfThenElse (t_if, t_then, t_else) ->
        ( match eval_term top_uid exec_ctxt t_if |> Lambda_ast.untype with
        | EBool b ->
            if b then eval_term top_uid exec_ctxt t_then else eval_term top_uid exec_ctxt t_else
        | _ -> assert false )
        |> Lambda_ast.untype
    | EDefault t -> (
        match eval_default_term top_uid exec_ctxt t with
        | Ok value -> value |> Lambda_ast.untype
        | Error (true_pos, false_pos) ->
            raise_default_conflict (Uid.get_ident top_uid, Uid.get_pos top_uid) true_pos false_pos )
  in
  ((evaled_term, pos), typ)

(* Evaluates a default term : see the formalization for an insight about this operation *)
and eval_default_term (top_uid : Uid.t) (exec_ctxt : exec_context) (term : Lambda_ast.default_term)
    : (Lambda_ast.term, Pos.t list * Pos.t list) result =
  (* First filter out the term which justification are false *)
  let candidates : 'a IntMap.t =
    IntMap.filter
      (fun _ (cond, _) ->
        match eval_term top_uid exec_ctxt cond |> Lambda_ast.untype with
        | EBool b -> b
        | _ -> assert false)
      term.defaults
  in
  (* Now filter out the terms that have a predecessor which justification is true *)
  let module ISet = Set.Make (Int) in
  let key_candidates = IntMap.fold (fun x _ -> ISet.add x) candidates ISet.empty in
  let chosen_one =
    List.fold_left
      (fun set (lo, hi) -> if ISet.mem lo set && ISet.mem hi set then ISet.remove hi set else set)
      key_candidates term.ordering
  in
  match ISet.elements chosen_one with
  | [ x ] ->
      let _, cons = IntMap.find x term.defaults in
      Ok (eval_term top_uid exec_ctxt cons)
  | xs ->
      let true_pos =
        xs |> List.map (fun x -> IntMap.find x term.defaults |> fst |> Lambda_ast.get_pos)
      in
      let false_pos : Pos.t list =
        let bindings : (scope_uid * (term * term)) list = term.defaults |> IntMap.bindings in
        List.map (fun (_, (cond, _)) -> Lambda_ast.get_pos cond) bindings
      in
      Error (true_pos, false_pos)
