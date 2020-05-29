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

exception Desugaring of string

(* Checks that types of the form Named constructor refer to existing structs or enum cases *)
let check_typ_well_formed (_context : Ir.context) (_typ : Ir.typ) : unit = ()

(* Checks that the ident is well formed and transform it *)
let desugar_qident (_context : Ir.context) (_scope : Ir.scope) (_qident : Ast.qident) : Ir.qident =
  []

(* Process a struct declaration, checking for wellformedness of types *)
let process_struct_decl (context : Ir.context) (_struct : Ast.struct_decl) : Ir.context = context

(* Process an enum declaration, checking for wellformedness of types *)
let process_enum_decl (context : Ir.context) (_enum : Ast.enum_decl) : Ir.context = context

(* Process a scope declaration, checking for wellformedness of types *)
let process_scope_decl (context : Ir.context) (_scope : Ast.scope_decl) : Ir.context = context

(* Process one declaration, checking for wellformedness of types *)
let process_decl (context : Ir.context) (decl : Ast.code_item) : Ir.context =
  match decl with
  | ScopeUse _ -> context
  | ScopeDecl decl -> process_scope_decl context decl
  | StructDecl decl -> process_struct_decl context decl
  | EnumDecl decl -> process_enum_decl context decl

(* Process all declarations of a given program to form its context *)
let form_context (prgm : Ast.program) : Ir.context =
  let context : Ir.context =
    {
      counter = ref 0;
      ident_to_uid = Hashtbl.create 1000;
      struct_decl = Ir.IdentMap.empty;
      enum_decl = Ir.IdentMap.empty;
      enum_cases = Ir.IdentMap.empty;
      scope_decl = Ir.IdentMap.empty;
      uid_data = Ir.UidMap.empty;
    }
  in

  let process_program_item context = function
    | Ast.MetadataBlock (block, _) ->
        List.fold_left (fun context item -> process_decl context (Pos.unmark item)) context block
    | _ -> context
  in

  List.fold_left process_program_item context prgm.program_items
