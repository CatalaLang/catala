(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the license *)

(** Processes scope use to :

    - regroup rules or definitions (aka defaults) of the same variable under the same hood
    - push scope use conditions into the justification of the defaults **)

module UidMap = Context.UidMap
module UidSet = Context.UidSet

let check_qident (context : Context.context) (scope_uid : Context.uid) (qid : Ast.qident) :
    Context.qident * Context.typ option =
  let rec remove_marks = function [] -> [] | x :: xx -> Pos.unmark x :: remove_marks xx in

  let qid = remove_marks qid.qident_path in

  let rec check_inclusion parent_uid = function
    | [] -> parent_uid
    | ident :: id_list -> (
        match parent_uid with
        | None -> (
            (* base case, we need to check that the ident is in the scope *)
            let uids = UidSet.of_list (Context.find_uid_list ident context.ident_to_uid) in
            let scope_params = UidMap.find scope_uid context.scope_decl in
            match UidSet.choose_opt (UidSet.inter uids scope_params) with
            | None -> raise (Context.ContextError "...")
            | Some uid -> check_inclusion (Some uid) id_list )
        | Some parent_uid -> Some parent_uid )
  in

  let uid =
    match check_inclusion None qid with
    | None -> raise (Context.ContextError "???")
    | Some uid -> uid
  in
  let typ = (UidMap.find uid context.uid_data).uid_typ in
  (qid, typ)
