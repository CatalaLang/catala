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

type array_instance = {
  name : StructName.t;
  len_field : StructField.t;
  content_field : StructField.t;
  content_typ : naked_typ;
}

type monomorphized_instances = {
  (* The keys are the types inside the [TOption] (before monomorphization). *)
  options : option_instance Type.Map.t;
  (* The keys are the [TTuple] types themselves (before monomorphization). *)
  tuples : tuple_instance Type.Map.t;
  (* The keys are the types inside the [TArray] (before monomorphization). *)
  arrays : array_instance Type.Map.t;
}

let empty_instances =
  { options = Type.Map.empty; tuples = Type.Map.empty; arrays = Type.Map.empty }

let collect_monomorphized_instances (prg : typed program) :
    monomorphized_instances =
  let option_instances_counter = ref 0 in
  let tuple_instances_counter = ref 0 in
  let array_instances_counter = ref 0 in
  let rec collect_typ acc typ =
    match Mark.remove typ with
    | TTuple args when List.for_all (fun t -> Mark.remove t <> TAny) args ->
      let new_acc =
        {
          acc with
          tuples =
            Type.Map.update typ
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
                                ("elt_" ^ string_of_int i, Pos.void),
                              Mark.remove arg ))
                          args;
                      name =
                        StructName.fresh []
                          ( "tuple_" ^ string_of_int !tuple_instances_counter,
                            Pos.void );
                    })
              acc.tuples;
        }
      in
      List.fold_left collect_typ new_acc args
    | TArray t ->
      let new_acc =
        {
          acc with
          arrays =
            Type.Map.update typ
              (fun monomorphized_name ->
                match monomorphized_name with
                | Some e -> Some e
                | None ->
                  incr array_instances_counter;
                  Some
                    {
                      len_field = StructField.fresh ("length", Pos.void);
                      content_field = StructField.fresh ("content", Pos.void);
                      content_typ = Mark.remove t;
                      name =
                        StructName.fresh []
                          ( "array_" ^ string_of_int !array_instances_counter,
                            Pos.void );
                    })
              acc.arrays;
        }
      in
      collect_typ new_acc t
    | TDefault t -> collect_typ acc t
    | TArrow (args, ret) ->
      List.fold_left collect_typ (collect_typ acc ret) args
    | TOption t when Mark.remove t <> TAny ->
      let new_acc =
        {
          acc with
          options =
            Type.Map.update typ
              (fun monomorphized_name ->
                match monomorphized_name with
                | Some e -> Some e
                | None ->
                  incr option_instances_counter;
                  Some
                    {
                      some_cons =
                        EnumConstructor.fresh
                          ( "Some_" ^ string_of_int !option_instances_counter,
                            Pos.void );
                      none_cons =
                        EnumConstructor.fresh
                          ( "None_" ^ string_of_int !option_instances_counter,
                            Pos.void );
                      some_typ = Mark.remove t;
                      name =
                        EnumName.fresh []
                          ( "option_" ^ string_of_int !option_instances_counter,
                            Pos.void );
                    })
              acc.options;
        }
      in
      collect_typ new_acc t
    | TStruct _ | TEnum _ | TAny | TClosureEnv | TLit _ -> acc
    | TOption _ | TTuple _ ->
      Message.error ~internal:true ~pos:(Mark.get typ)
        "Some types in tuples or option have not been resolved by the \
         typechecking before monomorphization."
  in
  let rec collect_expr e acc =
    Expr.shallow_fold collect_expr e (collect_typ acc (Expr.ty e))
  in
  let acc =
    Scope.fold_exprs prg.code_items ~init:empty_instances ~f:(fun acc e typ ->
        collect_typ (collect_expr e acc) typ)
  in
  EnumName.Map.fold
    (fun _ constructors acc ->
      EnumConstructor.Map.fold
        (fun _ t acc -> collect_typ acc t)
        constructors acc)
    prg.decl_ctx.ctx_enums
    (StructName.Map.fold
       (fun _ fields acc ->
         StructField.Map.fold (fun _ t acc -> collect_typ acc t) fields acc)
       prg.decl_ctx.ctx_structs acc)

let rec monomorphize_typ
    (monomorphized_instances : monomorphized_instances)
    (typ : typ) : typ =
  match Mark.remove typ with
  | TStruct _ | TEnum _ | TAny | TClosureEnv | TLit _ -> typ
  | TArray _ ->
    ( TStruct (Type.Map.find typ monomorphized_instances.arrays).name,
      Mark.get typ )
  | TDefault t1 ->
    TDefault (monomorphize_typ monomorphized_instances t1), Mark.get typ
  | TArrow (t1s, t2) ->
    ( TArrow
        ( List.map (monomorphize_typ monomorphized_instances) t1s,
          monomorphize_typ monomorphized_instances t2 ),
      Mark.get typ )
  | TTuple _ ->
    ( TStruct (Type.Map.find typ monomorphized_instances.tuples).name,
      Mark.get typ )
  | TOption _ ->
    TEnum (Type.Map.find typ monomorphized_instances.options).name, Mark.get typ

let is_some c =
  EnumConstructor.equal Expr.some_constr c
  ||
  (assert (EnumConstructor.equal Expr.none_constr c);
   false)

let rec monomorphize_expr
    (monomorphized_instances : monomorphized_instances)
    (e0 : typed expr) : typed expr boxed =
  let ty0 = Expr.ty e0 in
  (* Keys in [monomorphized_instances] are before monomorphization, so collect
     this top-down *)
  let f_expr = monomorphize_expr monomorphized_instances in
  let f_ty = monomorphize_typ monomorphized_instances in
  (* Proceed bottom-up: apply first to the sub-terms *)
  let e = Expr.map ~f:f_expr ~typ:f_ty ~op:Fun.id e0 in
  let m = Mark.get e in
  let map_box f = Expr.Box.app1 e (fun e -> f (Mark.remove e)) m in
  map_box
  @@ function
  | ETuple args ->
    let tuple_instance = Type.Map.find ty0 monomorphized_instances.tuples in
    EStruct
      {
        name = tuple_instance.name;
        fields =
          StructField.Map.of_list
          @@ List.map2
               (fun (tuple_field, _) arg -> tuple_field, arg)
               tuple_instance.fields args;
      }
  | ETupleAccess { e; index; _ } ->
    (* The type of the tuple needs to be recovered from the untransformed
       expr *)
    let tup_ty =
      match e0 with ETupleAccess { e; _ }, _ -> Expr.ty e | _ -> assert false
    in
    let tuple_instance = Type.Map.find tup_ty monomorphized_instances.tuples in
    EStructAccess
      {
        name = tuple_instance.name;
        e;
        field = fst (List.nth tuple_instance.fields index);
      }
  | EMatch { name; e; cases } when EnumName.equal name Expr.option_enum ->
    let opt_ty =
      match e0 with EMatch { e; _ }, _ -> Expr.ty e | _ -> assert false
    in
    let option_instance =
      Type.Map.find opt_ty monomorphized_instances.options
    in
    EMatch
      {
        name = option_instance.name;
        e;
        cases =
          EnumConstructor.Map.fold
            (fun c ->
              EnumConstructor.Map.add
                (if is_some c then option_instance.some_cons
                 else option_instance.none_cons))
            cases EnumConstructor.Map.empty;
      }
  | EInj { name; e; cons } when EnumName.equal name Expr.option_enum ->
    let option_instance = Type.Map.find ty0 monomorphized_instances.options in
    EInj
      {
        name = option_instance.name;
        e;
        cons =
          (if is_some cons then option_instance.some_cons
           else option_instance.none_cons);
      }
  | EArray elts as e ->
    let elt_ty =
      match Mark.remove ty0 with TArray t -> t | _ -> assert false
    in
    let array_instance = Type.Map.find ty0 monomorphized_instances.arrays in
    EStruct
      {
        name = array_instance.name;
        fields =
          StructField.Map.of_list
            [
              ( array_instance.len_field,
                ( ELit (LInt (Runtime.integer_of_int (List.length elts))),
                  Expr.with_ty m (TLit TInt, Expr.mark_pos m) ) );
              ( array_instance.content_field,
                (e, Expr.with_ty m (TArray (f_ty elt_ty), Expr.mark_pos m)) );
            ];
      }
  | e -> e

let program (prg : typed program) : typed program * TypeIdent.t list =
  let monomorphized_instances = collect_monomorphized_instances prg in
  let decl_ctx = prg.decl_ctx in
  (* First we remove the polymorphic option type *)
  let ctx_enums = EnumName.Map.remove Expr.option_enum decl_ctx.ctx_enums in
  let ctx_structs = decl_ctx.ctx_structs in
  (* Then we replace all hardcoded types and expressions with the monomorphized
     instances *)
  let ctx_enums =
    EnumName.Map.map
      (EnumConstructor.Map.map (monomorphize_typ monomorphized_instances))
      ctx_enums
  in
  let ctx_structs =
    StructName.Map.map
      (StructField.Map.map (monomorphize_typ monomorphized_instances))
      ctx_structs
  in
  (* Then we augment the [decl_ctx] with the monomorphized instances *)
  let ctx_enums =
    Type.Map.fold
      (fun _ (option_instance : option_instance) (ctx_enums : enum_ctx) ->
        EnumName.Map.add option_instance.name
          (EnumConstructor.Map.add option_instance.none_cons
             (TLit TUnit, Pos.void)
             (EnumConstructor.Map.singleton option_instance.some_cons
                (monomorphize_typ monomorphized_instances
                   (option_instance.some_typ, Pos.void))))
          ctx_enums)
      monomorphized_instances.options ctx_enums
  in
  let ctx_structs =
    Type.Map.fold
      (fun _ (tuple_instance : tuple_instance) (ctx_structs : struct_ctx) ->
        StructName.Map.add tuple_instance.name
          (List.fold_left
             (fun acc (field, typ) ->
               StructField.Map.add field
                 (monomorphize_typ monomorphized_instances (typ, Pos.void))
                 acc)
             StructField.Map.empty tuple_instance.fields)
          ctx_structs)
      monomorphized_instances.tuples
      (Type.Map.fold
         (fun _ (array_instance : array_instance) (ctx_structs : struct_ctx) ->
           StructName.Map.add array_instance.name
             (StructField.Map.add array_instance.content_field
                ( TArray
                    (monomorphize_typ monomorphized_instances
                       (array_instance.content_typ, Pos.void)),
                  Pos.void )
                (StructField.Map.singleton array_instance.len_field
                   (TLit TInt, Pos.void)))
             ctx_structs)
         monomorphized_instances.arrays ctx_structs)
  in
  let decl_ctx = { decl_ctx with ctx_structs; ctx_enums } in
  let code_items =
    Bindlib.unbox
    @@ Scope.map_exprs prg.code_items
         ~typ:(monomorphize_typ monomorphized_instances)
         ~varf:Fun.id
         ~f:(monomorphize_expr monomorphized_instances)
  in
  ( { prg with decl_ctx; code_items },
    Scopelang.Dependency.check_type_cycles ctx_structs ctx_enums )
