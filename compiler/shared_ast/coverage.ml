(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2025 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>, Florian Angeletti
   <florian.angeletti@inria.fr>

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

let glob = ref None
let is_recorded () = !glob <> None

let mark_position add p =
  Option.iter (fun map -> glob := Some (add p map)) !glob

let mark pol e = mark_position pol (Expr.mark_pos (Mark.get e))
let mark_pos p = mark Pos_map.pos p
let mark_neg p = mark Pos_map.neg p

let mark_all add e =
  Option.iter
    (fun m ->
      let m =
        List.fold_left (fun m x -> add (Expr.mark_pos @@ Mark.get x) m) m e
      in
      glob := Some m)
    !glob

open Definitions

let rec reachable e map =
  let m = Mark.get e in
  let loc = Expr.mark_pos m in
  let map = Pos_map.reachable loc map in
  Expr.shallow_fold reachable e map

let compute_reachable_dcalc (p : (dcalc, 'm) gexpr program) =
  let htbl = Hashtbl.create 13 in
  let add p x =
    Format.eprintf "adding %a@." Pos.format_loc_text p;
    Hashtbl.add htbl p x
  in
  let rec loop (e : (dcalc, 'm) gexpr) =
    let pos = Expr.mark_pos (Mark.get e) in
    let e = Mark.remove e in
    match e with
    (* atoms *)
    | ELit _ -> add pos Pos_map.Reach
    | EPos _ -> add pos Pos_map.Reach
    | EVar _ -> add pos Pos_map.Reach
    | EExternal _ -> add pos Pos_map.Reach
    (* | ELit _ -> add pos Pos_map.Pos *)
    (* | EPos _ -> add pos Pos_map.Neg *)
    (* | EVar _ -> add pos Pos_map.Fulf *)
    (* | EExternal _ -> add pos Pos_map.Reach *)
    (**** ignored ****)
    | ELocation _ | EFatalError _ | EEmpty (* | ECustom _ *) -> ()
    (**** direct recursion ****)
    | EInj { e; _ }
    | EPureDefault e
    | EErrorOnEmpty e
    | ETupleAccess { e; _ }
    (* | EDStructAccess { e; _ } *)
    | EStructAccess { e; _ }
    | EAssert e ->
      loop e
    (**** non-trivial ****)
    | EApp { f = e; args; _ } ->
      loop e;
      List.iter loop args
    | EAbs { binder; pos = _; tys = _ } ->
      let _, body = Bindlib.unmbind binder in
      loop body
    | EArray args | ETuple args -> List.iter loop args
    (* | EScopeCall { args; _ } -> ScopeVar.Map.iter (fun _ (_p, e) -> loop e)
       args *)
    | EStruct { fields; _ } -> StructField.Map.iter (fun _ e -> loop e) fields
    | EAppOp { args; _ } -> List.iter loop args
    | EIfThenElse { cond; etrue; efalse } ->
      loop cond;
      loop etrue;
      loop efalse
    | EDefault { excepts; just; cons } ->
      List.iter loop excepts;
      loop just;
      loop cons
    (* | EDStructAmend { e; fields; _ } -> *)
    (*   loop e; *)
    (*   Ident.Map.iter (fun _ e -> loop e) fields *)
    | EMatch { e; cases; _ } ->
      loop e;
      EnumConstructor.Map.iter (fun _ e -> loop e) cases
  in
  Program.fold_exprs ~f:(fun () e _typ -> loop e) ~init:() p;
  htbl

let from_new h =
  Hashtbl.fold (fun p x acc -> Pos_map.add p x acc) h Pos_map.empty
