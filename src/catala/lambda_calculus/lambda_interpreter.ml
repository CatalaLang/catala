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

module ExecContextKey = struct
  type t = LocalVar of Uid.LocalVar.t | ScopeVar of var_prefix * Uid.Var.t

  let compare x y =
    match (x, y) with
    | LocalVar x, LocalVar y -> Uid.LocalVar.compare x y
    | ScopeVar (x1, x2), ScopeVar (y1, y2) -> (
        match (x1, y1) with
        | NoPrefix, NoPrefix | CallerPrefix _, CallerPrefix _ -> Uid.Var.compare x2 y2
        | SubScopePrefix x1, SubScopePrefix y1 ->
            let sub_comp = Uid.SubScope.compare x1 y1 in
            if sub_comp = 0 then Uid.Var.compare x2 y2 else sub_comp
        | _ -> compare x y )
    | _ -> compare x y

  let format_t (x : t) : string =
    match x with
    | LocalVar x -> Uid.LocalVar.format_t x
    | ScopeVar (prefix, var) -> Format_lambda.print_prefix prefix ^ Uid.Var.format_t var
end

module ExecContext = Map.Make (ExecContextKey)

type exec_context = Lambda_ast.untyped_term ExecContext.t

let format_exec_context (ctx : exec_context) =
  String.concat "\n"
    (List.map
       (fun (key, value) ->
         Printf.sprintf "%s -> %s" (ExecContextKey.format_t key)
           (Format_lambda.print_term ((value, Pos.no_pos), TDummy)))
       (ExecContext.bindings ctx))

let empty_exec_ctxt = ExecContext.empty

let raise_default_conflict (def : Uid.ScopeDef.t) (true_pos : Pos.t list) (false_pos : Pos.t list) =
  let var_str = Uid.ScopeDef.format_t def in
  let var_pos =
    match def with
    | Uid.ScopeDef.SubScopeVar (_, v) | Uid.ScopeDef.Var v -> Pos.get_position (Uid.Var.get_info v)
  in

  if List.length true_pos = 0 then
    let justifications : (string option * Pos.t) list =
      List.map (fun pos -> (Some "This justification is false:", pos)) false_pos
    in
    Errors.raise_multispanned_error
      (Printf.sprintf "Default logic error for variable %s: no justification is true." var_str)
      ( (Some (Printf.sprintf "The error concerns this variable %s" var_str), var_pos)
      :: justifications )
  else
    let justifications : (string option * Pos.t) list =
      List.map (fun pos -> (Some "This justification is true:", pos)) true_pos
    in
    Errors.raise_multispanned_error
      "Default logic conflict, multiple justifications are true but are not related by a precedence"
      ( (Some (Printf.sprintf "The conflict concerns this variable %s" var_str), var_pos)
      :: justifications )

let rec eval_term (top_uid : Uid.ScopeDef.t) (exec_ctxt : exec_context) (term : Lambda_ast.term) :
    Lambda_ast.term =
  let (term, pos), typ = term in
  let evaled_term =
    match term with
    | EFun _ | EInt _ | EDec _ | EBool _ | EOp _ -> term (* already a value *)
    | ELocalVar uid -> (
        let ctxt_key = ExecContextKey.LocalVar uid in
        match ExecContext.find_opt ctxt_key exec_ctxt with
        | Some t -> t
        | None ->
            Errors.raise_spanned_error
              (Printf.sprintf "Local Variable %s is not defined" (Uid.LocalVar.format_t uid))
              pos )
    | EVar (prefix, uid) -> (
        let ctxt_key = ExecContextKey.ScopeVar (prefix, uid) in
        match ExecContext.find_opt ctxt_key exec_ctxt with
        | Some t -> t
        | None ->
            Errors.raise_spanned_error
              (Printf.sprintf "Variable %s is not defined" (Uid.Var.format_t uid))
              pos )
    | EApp (f, args) -> (
        (* First evaluate and match the function body *)
        let f = f |> eval_term top_uid exec_ctxt |> Lambda_ast.untype in
        match f with
        | EFun (bindings, body) ->
            let exec_ctxt =
              List.fold_left2
                (fun ctxt arg (uid, _) ->
                  ExecContext.add (ExecContextKey.LocalVar uid)
                    (arg |> eval_term top_uid exec_ctxt |> Lambda_ast.untype)
                    ctxt)
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
                      (* should not happen *)
                    in
                    EBool (if binop = And then b1 && b2 else b1 || b2)
                | _ -> (
                    let i1, i2 =
                      match args with [ EInt i1; EInt i2 ] -> (i1, i2) | _ -> assert false
                      (* should not happen *)
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
                          (* should not happen *)
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
                          (* should not happen *)
                        in
                        EBool (op_comp i1 i2) ) )
            | Unop Minus -> (
                match args with
                | [ EInt i ] -> EInt (-i)
                | _ -> assert false (* should not happen *) )
            | Unop Not -> (
                match args with
                | [ EBool b ] -> EBool (not b)
                | _ -> assert false (* should not happen *) ) )
        | _ -> assert false )
    | EIfThenElse (t_if, t_then, t_else) ->
        ( match eval_term top_uid exec_ctxt t_if |> Lambda_ast.untype with
        | EBool b ->
            if b then eval_term top_uid exec_ctxt t_then else eval_term top_uid exec_ctxt t_else
        | _ -> assert false (* should not happen *) )
        |> Lambda_ast.untype
    | EDefault t -> (
        match eval_default_term top_uid exec_ctxt t with
        | Ok value -> value |> Lambda_ast.untype
        | Error (true_pos, false_pos) -> raise_default_conflict top_uid true_pos false_pos )
  in
  ((evaled_term, pos), typ)

(* Evaluates a default term : see the formalization for an insight about this operation *)
and eval_default_term (top_uid : Uid.ScopeDef.t) (exec_ctxt : exec_context)
    (term : Lambda_ast.default_term) : (Lambda_ast.term, Pos.t list * Pos.t list) result =
  (* First filter out the term which justification are false *)
  let defaults_numbered : (int * (term * term)) list =
    List.mapi (fun (x : int) (y : term * term) -> (x, y)) term.defaults
  in
  let candidates : 'a list =
    List.filter
      (fun (_, (cond, _)) ->
        match eval_term top_uid exec_ctxt cond |> Lambda_ast.untype with
        | EBool b -> b
        | _ -> assert false
        (* should not happen *))
      defaults_numbered
  in
  (* Now filter out the terms that have a predecessor which justification is true *)
  let module ISet = Set.Make (Int) in
  let key_candidates = List.fold_left (fun acc (x, _) -> ISet.add x acc) ISet.empty candidates in
  let chosen_one =
    List.fold_left
      (fun set (lo, hi) -> if ISet.mem lo set && ISet.mem hi set then ISet.remove hi set else set)
      key_candidates term.ordering
  in
  match ISet.elements chosen_one with
  | [ x ] ->
      let _, (_, cons) = List.find (fun (i, _) -> i = x) defaults_numbered in
      Ok (eval_term top_uid exec_ctxt cons)
  | xs ->
      let true_pos =
        xs
        |> List.map (fun x ->
               List.find (fun (i, _) -> i = x) defaults_numbered |> snd |> fst |> Lambda_ast.get_pos)
      in
      let false_pos : Pos.t list =
        List.map (fun (_, (cond, _)) -> Lambda_ast.get_pos cond) defaults_numbered
      in
      Error (true_pos, false_pos)
