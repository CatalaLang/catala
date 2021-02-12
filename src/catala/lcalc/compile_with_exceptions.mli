(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils

type ctx = Ast.expr Pos.marked Bindlib.box Dcalc.Ast.VarMap.t

val handle_default : Pos.t -> Ast.expr Pos.marked Bindlib.box

val translate_lit : Dcalc.Ast.lit -> Ast.expr

val thunk_expr : Ast.expr Pos.marked Bindlib.box -> Pos.t -> Ast.expr Pos.marked Bindlib.box

val translate_default :
  ctx ->
  Dcalc.Ast.expr Pos.marked list ->
  Dcalc.Ast.expr Pos.marked ->
  Dcalc.Ast.expr Pos.marked ->
  Pos.t ->
  Ast.expr Pos.marked Bindlib.box

val translate_expr : ctx -> Dcalc.Ast.expr Pos.marked -> Ast.expr Pos.marked Bindlib.box

val translate_program : Dcalc.Ast.program -> Ast.program
