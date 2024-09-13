(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2024 Inria, contributor:
   Louis Gesbert <louis.gesbert@inria.fr>

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
open Shared_ast
open Ast
module Runtime = Runtime_ocaml.Runtime
module D = Dcalc.Ast
module L = Lcalc.Ast

let rec get_vars e =
  match Mark.remove e with
  | EVar v -> VarName.Set.singleton v
  | EFunc _ | ELit _ | EPosLit | EExternal _ -> VarName.Set.empty
  | EStruct str ->
    StructField.Map.fold (fun _ e -> VarName.Set.union (get_vars e)) str.fields VarName.Set.empty
  | EStructFieldAccess { e1; _ } | ETupleAccess { e1; _ } | EInj { e1; _ }->
    get_vars e1
  | ETuple el | EArray el  | EAppOp { args = el; _ } ->
    List.fold_left (fun acc e -> VarName.Set.union acc (get_vars e)) VarName.Set.empty el
  | EApp { f; args } ->
    List.fold_left (fun acc e -> VarName.Set.union acc (get_vars e))
      (get_vars f) args

let rec subst_expr v e within_expr =
  let m = Mark.get within_expr in
  match Mark.remove within_expr with
  | EVar v1 -> if VarName.equal v v1 then e else within_expr
  | EFunc _ | ELit _ | EPosLit | EExternal _ -> within_expr
  | EStruct str ->
    ( EStruct
        { str with fields = StructField.Map.map (subst_expr v e) str.fields },
      m )
  | EStructFieldAccess sfa ->
    EStructFieldAccess { sfa with e1 = subst_expr v e sfa.e1 }, m
  | ETuple el -> ETuple (List.map (subst_expr v e) el), m
  | ETupleAccess ta -> ETupleAccess { ta with e1 = subst_expr v e ta.e1 }, m
  | EInj i -> EInj { i with e1 = subst_expr v e i.e1 }, m
  | EArray el -> EArray (List.map (subst_expr v e) el), m
  | EApp { f; args } ->
    EApp { f = subst_expr v e f; args = List.map (subst_expr v e) args }, m
  | EAppOp ao -> EAppOp { ao with args = List.map (subst_expr v e) ao.args }, m

let rec subst_stmt v e stmt =
  match stmt with
  | SInnerFuncDef ifd ->
    SInnerFuncDef
      {
        ifd with
        func = { ifd.func with func_body = subst_block v e ifd.func.func_body };
      }
  | SLocalDecl _ -> stmt
  | SLocalInit li -> SLocalInit { li with expr = subst_expr v e li.expr }
  | SLocalDef ld -> SLocalDef { ld with expr = subst_expr v e ld.expr }
  | SFatalError fe ->
    SFatalError { fe with pos_expr = subst_expr v e fe.pos_expr }
  | SIfThenElse { if_expr; then_block; else_block } ->
    SIfThenElse
      {
        if_expr = subst_expr v e if_expr;
        then_block = subst_block v e then_block;
        else_block = subst_block v e else_block;
      }
  | SSwitch sw ->
    let switch_var =
      if VarName.equal sw.switch_var v then
        match e with EVar v1, _ -> v1 | _ -> raise Exit
      else sw.switch_var
    in
    SSwitch
      {
        sw with
        switch_var;
        switch_cases =
          List.map
            (fun c -> { c with case_block = subst_block v e c.case_block })
            sw.switch_cases;
      }
  | SReturn e1 -> SReturn (subst_expr v e e1)
  | SAssert { pos_expr; expr } ->
    SAssert { pos_expr = subst_expr v e pos_expr; expr = subst_expr v e expr }
  | _ -> .

and subst_block v e block =
  List.map (fun (stmt, pos) -> subst_stmt v e stmt, pos) block

let subst_block v expr typ pos block =
  try subst_block v expr block
  with Exit -> (SLocalInit { name = v, pos; typ; expr }, pos) :: block

let rec find_block pred = function
  | [] -> None
  | stmt :: _ when pred stmt -> Some stmt
  | (SIfThenElse { then_block; else_block; _ }, _) :: r ->
    (match find_block pred then_block with
     | None ->
       (match find_block pred else_block with
        | None -> find_block pred r
        | some -> some)
     | some -> some)
  | (SSwitch { switch_cases; _ }, _) :: r ->
    (match
       List.find_map (fun case ->
           find_block pred case.case_block)
         switch_cases
     with
     | None -> find_block pred r
     | some -> some)
  | _ :: r -> find_block pred r
