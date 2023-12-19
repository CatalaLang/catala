(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
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
open Ast
open Catala_utils

module TypMap = Map.Make (struct
  type t = naked_typ

  let compare x y = Type.compare (x, Pos.no_pos) (y, Pos.no_pos)
  let format fmt x = Print.typ_debug fmt (x, Pos.no_pos)
end)

type option_instance = {
  name : EnumName.t;
  some_cons : EnumConstructor.t;
  some_typ : naked_typ;
  none_cons : EnumConstructor.t;
}

type tuple_instance = {
  name : StructName.t;
  fields : (StructField.t * naked_typ) list;
}

type monorphized_instances = {
  (* The keys are the types inside the [TOption] (before monomorphization). *)
  options : option_instance TypMap.t;
  (* The keys are the [TTuple] types themselves (before monomorphization). *)
  tuples : tuple_instance TypMap.t;
}

let program (prg : typed program) : typed program =
  let monomorphized_instances : monorphized_instances =
    let option_instances_counter = ref 0 in
    let tuple_instances_counter = ref 0 in
    let rec monomorphize_typ acc typ =
      match Mark.remove typ with
      | TStruct _ | TEnum _ | TAny | TClosureEnv | TLit _ -> acc
      | TTuple args ->
        let new_acc =
          {
            acc with
            tuples =
              TypMap.update (Mark.remove typ)
                (fun monomorphized_name ->
                  match monomorphized_name with
                  | Some e -> Some e
                  | None ->
                    incr tuple_instances_counter;
                    Some
                      {
                        fields =
                          List.mapi
                            (fun i arg ->
                              ( StructField.fresh
                                  ("elt_" ^ string_of_int i, Pos.no_pos),
                                Mark.remove arg ))
                            args;
                        name =
                          StructName.fresh []
                            ( "tuple_" ^ string_of_int !option_instances_counter,
                              Pos.no_pos );
                      })
                acc.tuples;
          }
        in
        List.fold_left monomorphize_typ new_acc args
      | TArray t | TDefault t -> monomorphize_typ acc t
      | TArrow (args, ret) ->
        List.fold_left monomorphize_typ (monomorphize_typ acc ret) args
      | TOption t ->
        let new_acc =
          {
            acc with
            options =
              TypMap.update (Mark.remove t)
                (fun monomorphized_name ->
                  match monomorphized_name with
                  | Some e -> Some e
                  | None ->
                    incr option_instances_counter;
                    Some
                      {
                        some_cons =
                          EnumConstructor.fresh
                            ( "some_" ^ string_of_int !option_instances_counter,
                              Pos.no_pos );
                        none_cons =
                          EnumConstructor.fresh
                            ( "none_" ^ string_of_int !option_instances_counter,
                              Pos.no_pos );
                        some_typ = Mark.remove t;
                        name =
                          EnumName.fresh []
                            ( "option_" ^ string_of_int !option_instances_counter,
                              Pos.no_pos );
                      })
                acc.options;
          }
        in
        monomorphize_typ new_acc t
    in
    let rec monomorphize_expr acc e =
      let acc = monomorphize_typ acc (Expr.maybe_ty (Mark.get e)) in
      Expr.shallow_fold (fun e acc -> monomorphize_expr acc e) e acc
    in
    let acc =
      Scope.fold_left
        ~init:{ options = TypMap.empty; tuples = TypMap.empty }
        ~f:(fun acc item _ ->
          match item with
          | Topdef (_, typ, e) -> monomorphize_typ (monomorphize_expr acc e) typ
          | ScopeDef (_, body) ->
            let _, body = Bindlib.unbind body.scope_body_expr in
            Scope.fold_left_lets ~init:acc
              ~f:(fun acc { scope_let_typ; scope_let_expr; _ } _ ->
                monomorphize_typ
                  (monomorphize_expr acc scope_let_expr)
                  scope_let_typ)
              body)
        prg.code_items
    in
    EnumName.Map.fold
      (fun _ constructors acc ->
        EnumConstructor.Map.fold
          (fun _ t acc -> monomorphize_typ acc t)
          constructors acc)
      prg.decl_ctx.ctx_enums
      (StructName.Map.fold
         (fun _ fields acc ->
           StructField.Map.fold
             (fun _ t acc -> monomorphize_typ acc t)
             fields acc)
         prg.decl_ctx.ctx_structs acc)
  in
  (* First we augment the [decl_ctx] with the option instances *)
  let prg =
    {
      prg with
      decl_ctx =
        {
          prg.decl_ctx with
          ctx_enums =
            TypMap.fold
              (fun _ (option_instance : option_instance) (ctx_enums : enum_ctx) ->
                EnumName.Map.add option_instance.name
                  (EnumConstructor.Map.add option_instance.none_cons
                     (TLit TUnit, Pos.no_pos)
                     (EnumConstructor.Map.singleton option_instance.some_cons
                        (option_instance.some_typ, Pos.no_pos)))
                  ctx_enums)
              monomorphized_instances.options prg.decl_ctx.ctx_enums;
          ctx_structs =
            TypMap.fold
              (fun _ (tuple_instance : tuple_instance)
                   (ctx_structs : struct_ctx) ->
                StructName.Map.add tuple_instance.name
                  (List.fold_left
                     (fun acc (field, typ) ->
                       StructField.Map.add field (typ, Pos.no_pos) acc)
                     StructField.Map.empty tuple_instance.fields)
                  ctx_structs)
              monomorphized_instances.tuples prg.decl_ctx.ctx_structs;
        };
    }
  in
  (* TODO replace types in exprs *)
  prg
