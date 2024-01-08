(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Denis Merigoux <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Shared_ast

let add_option_type ctx =
  {
    ctx with
    ctx_enums =
      EnumName.Map.add Expr.option_enum Expr.option_enum_config ctx.ctx_enums;
  }

let add_option_type_program prg =
  { prg with decl_ctx = add_option_type prg.decl_ctx }

let translate_program_with_exceptions =
  Compile_with_exceptions.translate_program

let translate_program_without_exceptions prg =
  let prg = add_option_type_program prg in
  Compile_without_exceptions.translate_program prg
