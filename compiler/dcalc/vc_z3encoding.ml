(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2022 Inria, contributor: Aymeric Fromherz
   <aymeric.fromherz@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils
open Ast 
open Z3

(** [translate_expr] translate the expression [vc] to a Z3 formula **)
let translate_expr (_ctx:context) (vc : expr Pos.marked) : Expr.expr =
  match Pos.unmark vc with
  | EVar _ -> failwith "[Z3 encoding] EVar unsupported"
  | ETuple _ -> failwith "[Z3 encoding] ETuple unsupported"
  | ETupleAccess _ -> failwith "[Z3 encoding] ETupleAccess unsupported"
  | EInj _ -> failwith "[Z3 encoding] EInj unsupported"
  | EMatch _ -> failwith "[Z3 encoding] EMatch unsupported"
  | EArray _ -> failwith "[Z3 encoding] EArray unsupported"
  | ELit _ -> failwith "[Z3 encoding] ELit unsupported"
  | EAbs _ -> failwith "[Z3 encoding] EAbs unsupported"
  | EApp _ -> failwith "[Z3 encoding] EApp unsupported"
  | EAssert _ ->  failwith "[Z3 encoding] EAssert unsupported"
  | EOp _ -> failwith "[Z3 encoding] EOp unsupported"
  | EDefault _ -> failwith "[Z3 encoding] EDefault unsupported"
  | EIfThenElse _ ->  failwith "[Z3 encoding] EIfThenElse unsupported"
  | ErrorOnEmpty _ -> failwith "[Z3 encoding] ErrorOnEmpty unsupported"

(** [solve_vc] is the main entry point of this module.
    It takes a list of expressions [vcs] corresponding to verification conditions
    that must be discharged by Z3, and attempts to solve them **)
let solve_vc (vcs : expr Pos.marked list) : unit =
  Printf.printf "Running Z3 version %s\n" Version.to_string ;

  let cfg = [("model", "true"); ("proof", "false")] in
	let ctx = (mk_context cfg) in
  
  let solver = Solver.mk_solver ctx None in

  let z3_vcs = List.map (translate_expr ctx) vcs in

  List.iter (fun vc -> Printf.printf "Generated VC: %s\n" (Expr.to_string vc)) z3_vcs;

  Solver.add solver z3_vcs;

  if (Solver.check solver []) = SATISFIABLE then
    Printf.printf "Success: Empty unreachable\n"
  else
    (* TODO: Print model as error message for Catala debugging purposes *) 
    Printf.printf "Failure: Empty reachable\n"

