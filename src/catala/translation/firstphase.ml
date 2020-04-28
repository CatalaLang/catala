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

(* This file intends to translate from the parser ast in parsing/ast.ml to the first intermediate
   representation in ir/ir.ml *)

let struct_decl_translation (prgm : Ir.program) (_struct_decl : Ast.struct_decl) = prgm

let enum_decl_translation (prgm : Ir.program) (_enum_decl : Ast.enum_decl) = prgm

let field_decl_translation (prgm : Ir.program) (_field_decl : Ast.field_decl) = prgm

let field_use_translation (prgm : Ir.program) (_field_use : Ast.field_use) = prgm

let code_item_translation (prgm : Ir.program) (code_item : Ast.code_item) =
  match code_item with
  | Ast.FieldUse field_use -> field_use_translation prgm field_use
  | Ast.FieldDecl field_decl -> field_decl_translation prgm field_decl
  | Ast.StructDecl struct_decl -> struct_decl_translation prgm struct_decl
  | Ast.EnumDecl enum_decl -> enum_decl_translation prgm enum_decl

let code_block_translation : Ir.program -> Ast.code_block -> Ir.program =
  List.fold_left (fun prgm item -> code_item_translation prgm (Pos.unmark item))

let program_item_translation (prgm : Ir.program) (item : Ast.program_item) =
  match item with
  | Ast.MetadataBlock (block, _) | Ast.CodeBlock (block, _) -> code_block_translation prgm block
  | _ -> prgm

let translation (prgm : Ast.program) =
  let empty_prgm : Ir.program =
    { enums = Ir.EnumMap.empty; fields = Ir.FieldMap.empty; structs = Ir.StructMap.empty }
  in
  List.fold_left program_item_translation empty_prgm prgm.program_items
