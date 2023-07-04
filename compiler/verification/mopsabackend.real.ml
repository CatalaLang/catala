(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria,
   contributors: Raphaël Monat <raphael.monat@inria.fr>, Denis Merigoux
   <denis.merigoux@inria.fr>

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

let simplified_string_of_pos pos =
  let basename =
    Filename.basename (Pos.get_file pos) |> Filename.chop_extension
  in
  Format.asprintf "%s_%d-%d_%d-%d" basename (Pos.get_start_line pos)
    (Pos.get_start_column pos) (Pos.get_end_line pos) (Pos.get_end_column pos)

let rec vars_used_in_vc vc_scope_ctx (e : typed Dcalc.Ast.expr) :
    (typed Dcalc.Ast.expr, typ) Var.Map.t =
  let rec free_vars_with_types :
      ('a, 't) gexpr -> (('a, 't) gexpr, typ) Var.Map.t =
   fun e ->
    match e with
    | EVar v, _ -> Var.Map.singleton v (Expr.ty e)
    | EAbs { binder; _ }, _ ->
      let vs, body = Bindlib.unmbind binder in
      Array.fold_right Var.Map.remove vs (free_vars_with_types body)
    | e ->
      Expr.shallow_fold
        (fun e ->
          Var.Map.union (fun _ _ -> assert false) (free_vars_with_types e))
        e Var.Map.empty
  in
  let free_vars = free_vars_with_types e in
  (* We search recursively in the possible definitions of each free variable. *)
  let possible_values_of_free_vars =
    Var.Map.filter
      (fun v _ -> Var.Map.mem v free_vars)
      vc_scope_ctx.Conditions.vc_scope_possible_variable_values
  in
  Var.Map.fold
    (fun _ possible_values vars_used ->
      List.fold_left
        (fun vars_used possible_value ->
          Var.Map.union
            (fun _ _ -> assert false)
            (vars_used_in_vc vc_scope_ctx possible_value)
            vars_used)
        vars_used possible_values)
    possible_values_of_free_vars free_vars

type mopsa_variable_declaration_value =
  | Unknown of typ
  | Known of typed Dcalc.Ast.expr

type mopsa_program = {
  initial_guard : typed Dcalc.Ast.expr;
  main_guard : typed Dcalc.Ast.expr;
  declared_variables :
    ((dcalc, typed) gexpr Var.t * mopsa_variable_declaration_value) list;
}

type 'a mopsa_variable_declaration_boxed_value =
  | BoxedUnknown of typ
  | BoxedKnown of ('a, typed) marked

module TypedValuesDcalcVarMap = struct
  type 'x t =
    (typed Dcalc.Ast.expr, 'x mopsa_variable_declaration_boxed_value) Var.Map.t

  let map (f : 'a -> 'b) (m : 'a t) : 'b t =
    Var.Map.map
      (fun v ->
        match v with
        | BoxedUnknown ty -> BoxedUnknown ty
        | BoxedKnown (e, te) -> BoxedKnown (f e, te))
      m
end

module TypedValuesDcalcVarMapBoxLifting = Bindlib.Lift (TypedValuesDcalcVarMap)

type mopsa_variable_declarations =
  (typed Dcalc.Ast.expr, mopsa_variable_declaration_value) Var.Map.t

module VarVertex = struct
  include Var

  type t = typed Dcalc.Ast.expr Var.t

  let hash x = Bindlib.uid_of x
  let equal x y = Bindlib.eq_vars x y
end

module VarDependencies =
  Graph.Persistent.Digraph.ConcreteBidirectional (VarVertex)

module VarDependenciesTraversal = Graph.Topological.Make (VarDependencies)

let declaration_reverse_dependency_ordering (d : mopsa_variable_declarations) :
    typed Dcalc.Ast.expr Var.t list =
  let g = VarDependencies.empty in
  let g =
    Var.Map.fold
      (fun (v : typed Dcalc.Ast.expr Var.t)
           (e : mopsa_variable_declaration_value) g ->
        let g = VarDependencies.add_vertex g v in
        match e with
        | Unknown _ -> g
        | Known e ->
          let used = Expr.free_vars e in
          Var.Set.fold (fun used g -> VarDependencies.add_edge g used v) used g)
      d g
  in
  List.rev (VarDependenciesTraversal.fold (fun v acc -> v :: acc) g [])

(** To better split the load for MOPSA and improve debugging, we split the VC
    into smaller parts by creating supplementary variables. *)
let rec split_expression_into_atomic_parts (e : typed Dcalc.Ast.expr) :
    ( typed Dcalc.Ast.expr,
      (dcalc, typed) naked_gexpr Bindlib.box
      mopsa_variable_declaration_boxed_value )
    Var.Map.t
    * (dcalc, typed) boxed_gexpr =
  match Mark.remove e with
  | EApp
      {
        f =
          ( EOp
              {
                op =
                  ( Op.Add_dat_dur Dates_calc.Dates.AbortOnRound
                  | Op.FirstDayOfMonth );
                tys = _;
              },
            _ ) as f;
        args;
      } ->
    let acc, args =
      List.fold_left_map
        (fun acc arg ->
          let toadd, arg = split_expression_into_atomic_parts arg in
          (Var.Map.union (fun _ _ _ -> assert false)) acc toadd, arg)
        Var.Map.empty args
    in
    let dummy_var =
      let pos = Expr.pos e in
      Var.make ("var_" ^ simplified_string_of_pos pos)
    in
    let new_e = Expr.eapp (Expr.box f) args (Mark.get e) in
    ( Var.Map.add dummy_var (BoxedKnown new_e) acc,
      Expr.evar dummy_var (Mark.get e) )
  | _ ->
    Expr.map_gather ~acc:Var.Map.empty
      ~join:(Var.Map.union (fun _ _ _ -> assert false))
      ~f:split_expression_into_atomic_parts e

(** Sometimes VC contains struct accesses from larger values which are defined
    elsewhere. Since MOPSA does not currently handle structs, etc., we prefer
    replacing the struct access with its own variable coming with an unknown
    value. *)
let rec transform_field_accesses_into_variables
    ~(is_vc_var : (dcalc, typed) naked_gexpr Bindlib.var -> bool)
    (e : typed Dcalc.Ast.expr) :
    (typed Dcalc.Ast.expr, typ) Var.Map.t * (dcalc, typed) boxed_gexpr =
  match Mark.remove e with
  | EStructAccess { e = EVar v, _; field; _ } when is_vc_var v ->
    let var =
      Var.make (Format.asprintf "%a__%a" Print.var v StructField.format_t field)
    in
    Var.Map.singleton var (Expr.ty e), Expr.evar var (Mark.get e)
  | _ ->
    Expr.map_gather ~acc:Var.Map.empty
      ~join:(Var.Map.union (fun _ _ _ -> assert false))
      ~f:(transform_field_accesses_into_variables ~is_vc_var)
      e

(* The goal of this part is to extract additions from expressions, and perform
   them before as assignments. This will simplify the analysis and communication
   with Mopsa. *)
let translate_expr
    (vc_scope_ctx : Conditions.verification_conditions_scope)
    (original_vc_guard : typed Dcalc.Ast.expr) : mopsa_program =
  let vars_used_in_vc = vars_used_in_vc vc_scope_ctx original_vc_guard in
  let vars_used_in_vc_with_known_values =
    Var.Map.filter
      (fun v _ ->
        Var.Map.mem v vc_scope_ctx.Conditions.vc_scope_possible_variable_values)
      vars_used_in_vc
  in
  let vc_variables_declarations_unknown =
    Var.Map.fold
      (fun v ty declarations -> Var.Map.add v (Unknown ty) declarations)
      (Var.Map.filter
         (fun v _ -> not (Var.Map.mem v vars_used_in_vc_with_known_values))
         vars_used_in_vc)
      Var.Map.empty
  in
  let vc_variables_declarations_defined =
    Var.Map.mapi
      (fun var values ->
        if List.length values <> 1 then
          Message.emit_debug
            "Only the first possible value was taken into account for %a"
            Print.var_debug var;
        (* FIXME: take into account multiple possible values *)
        Known (List.hd values))
      (Var.Map.filter
         (fun v _ -> Var.Map.mem v vars_used_in_vc)
         vc_scope_ctx.vc_scope_possible_variable_values)
  in
  let vc_variables_declarations : mopsa_variable_declarations =
    Var.Map.union
      (fun _ _ -> assert false)
      vc_variables_declarations_unknown vc_variables_declarations_defined
  in
  let new_vars_for_struct_accesses, vc_guard =
    transform_field_accesses_into_variables
      ~is_vc_var:(fun v ->
        Var.Map.exists
          (fun v2 _ -> Var.compare v v2 = 0)
          vc_variables_declarations)
      original_vc_guard
  in
  let new_vars_for_struct_accesses : mopsa_variable_declarations =
    Var.Map.map (fun ty -> Unknown ty) new_vars_for_struct_accesses
  in
  let vc_guard = Expr.unbox vc_guard in
  let new_vars_for_atomic_splitting, (vc_guard, vc_guard_mark) =
    split_expression_into_atomic_parts vc_guard
  in
  let new_vars_for_atomic_splitting, vc_guard =
    (* This manipulation is done so that we only unbox once to keep all
       variables in sync. *)
    Bindlib.unbox
      (Bindlib.box_apply2
         (fun new_vars_for_atomic_splitting vc_guard ->
           new_vars_for_atomic_splitting, (vc_guard, vc_guard_mark))
         (TypedValuesDcalcVarMapBoxLifting.lift_box
            new_vars_for_atomic_splitting)
         vc_guard)
  in
  let new_vars_for_atomic_splitting : mopsa_variable_declarations =
    Var.Map.map
      (fun v ->
        match v with BoxedUnknown ty -> Unknown ty | BoxedKnown e -> Known e)
      new_vars_for_atomic_splitting
  in
  let vc_variables_declarations =
    Var.Map.union
      (fun _ _ -> assert false)
      vc_variables_declarations
      (Var.Map.union
         (fun _ _ -> assert false)
         new_vars_for_struct_accesses new_vars_for_atomic_splitting)
  in
  let declaration_reverse_dependency_ordering =
    declaration_reverse_dependency_ordering vc_variables_declarations
  in
  {
    initial_guard = original_vc_guard;
    main_guard = vc_guard;
    declared_variables =
      (* Careful with the order! these definitions must be laid out in reverse
         dependency order *)
      List.map
        (fun v -> v, Var.Map.find v vc_variables_declarations)
        declaration_reverse_dependency_ordering;
  }

let print_encoding (prog : mopsa_program) =
  let fmt = Format.str_formatter in
  let () =
    Format.fprintf fmt "%aassert(sync(%a));@."
      (Format.pp_print_list
         ~pp_sep:(fun _ () -> ())
         (fun fmt (var, oexpr) ->
           match oexpr with
           | Unknown ty -> (
             let make_random =
               match Mark.remove ty with
               | TLit TDate -> Some "date()"
               | TLit TDuration -> Some "duration_ym()"
               | TLit TBool -> Some "bool()"
               | TLit TInt -> Some "int()"
               | _ -> None
             in
             match make_random with
             | Some make_random ->
               Format.fprintf fmt "%a %a = make_random_%s;@." Print.typ_debug ty
                 Print.var var make_random
             | None ->
               Message.emit_debug "Ignoring type declaration of var %a : %a"
                 Print.var_debug var Print.typ_debug ty)
           | Known expr ->
             Format.fprintf fmt "%a %a = %a;@." Print.typ_debug (Expr.ty expr)
               Print.var var
               (Print.expr ~debug:false ())
               expr))
      prog.declared_variables
      (Print.expr ~debug:false ())
      prog.main_guard
  in
  let str = Format.flush_str_formatter () in
  String.to_ascii str

module Backend : Io.Backend = struct
  type vc_encoding = mopsa_program

  let init_backend () = ()

  type backend_context = unit

  let make_context _ = ()

  let translate_expr
      (scope_vcs : Conditions.verification_conditions_scope)
      ()
      (e : typed Dcalc.Ast.expr) : unit * mopsa_program =
    (), translate_expr scope_vcs e

  let print_encoding _ctx prog = print_encoding prog

  type model = Yojson.Basic.t (* yeah, I'll have to fix that *)
  type solver_result = ProvenTrue | ProvenFalse of model option | Unknown

  let solve_vc_encoding ctx mopsa_program =
    let prog_name =
      "proof_obligation_"
      ^ simplified_string_of_pos (Expr.pos mopsa_program.main_guard)
      ^ ".u"
    in
    let prog_channel = open_out prog_name in
    Printf.fprintf prog_channel "%s" (print_encoding ctx mopsa_program);
    close_out prog_channel;
    Message.emit_debug "Generated new Mopsa program at %s" prog_name;
    let process_mopsas_json j =
      let open Yojson.Basic.Util in
      if j |> member "success" |> to_bool then
        let alarms = j |> member "checks" |> to_list in
        (* FIXME: if list.hd fails the program is safe *)
        let toparse = List.hd alarms |> member "messages" |> to_string in
        let pos = Str.search_forward (Str.regexp_string "Hints: ") toparse 0 in
        let sub_until_end s from = String.sub s from (String.length s - from) in
        let new_json_to_parse =
          sub_until_end toparse (pos + String.length "Hints: ")
        in
        Some (Yojson.Basic.from_string new_json_to_parse)
      else
        let () =
          Message.emit_warning "Something went wrong with Mopsa: %s"
            (j |> member "exception" |> to_string)
        in
        None
    in
    (* I wanted to use mopsa as a library, but we have a small issue to fix
       there first *)
    let args =
      [|
        "mopsa-universal";
        "-config=universal/ymd_poly_powerint_markerset.json";
        (* "-debug=_"; *)
        "-max-set-size=7";
        "-numeric=polkagrid";
        "-format=json";
        "-silent";
        "-output=tmp.json";
        prog_name;
      |]
    in
    (* I wanted to use mopsa as a library, but we have a small issue to fix
       there first *)
    (* let open Mopsa_analyzer.Framework.Runner in *)
    (* let _ = *)
    (*   try parse_options args analyze_files () *)
    (*   with *)
    (*   | Mopsa_utils.Exceptions.Panic (s, t) -> *)
    (*     Message.emit_warning "Mopsa failed :(@.%s@%s" s t ; 0 *)
    (*   | _ -> *)
    (*     Message.emit_warning "Mopsa failed :("; 0 *)
    (* in *)
    let () = try Unix.unlink "tmp.json" with Unix.Unix_error _ -> () in
    let _ =
      Unix.system (Array.fold_left (fun acc s -> acc ^ " " ^ s) "" args)
    in
    match process_mopsas_json (Yojson.Basic.from_file "tmp.json") with
    | Some hints -> ProvenFalse (Some hints)
    | None -> Unknown

  let print_model _ m : string = Yojson.Basic.pretty_to_string ~std:true m
  let init_backend () = Message.emit_debug "Running Mopsa"
  let is_model_empty _ = false
  let encode_asserts _ _ _ = assert false
end

module Io = Io.MakeBackendIO (Backend)
