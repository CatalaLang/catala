(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria.

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
module IntSet = Set.Make (Int)

type column = {
  col_enum : EnumName.t;
  col_constr : EnumConstructor.t;
  col_vars : (Ast.expr Var.t * Pos.t) list;
  col_pos : Pos.t;
}

type row = {
  row_cols : column option list;
  row_body : Ast.expr boxed;
  row_pos : Pos.t;
}

let is_wildcard row = List.for_all Option.is_none row.row_cols
let head_col (_, row) = List.hd row.row_cols
let pop (i, row) = i, { row with row_cols = List.tl row.row_cols }

let subst_vars vars args (body : Ast.expr boxed) : Ast.expr boxed =
  let bnd = Expr.bind (Array.of_list vars) body in
  let args = Bindlib.box_list (List.map Mark.remove args) in
  ( Bindlib.box_apply2
      (fun bnd args -> Mark.remove (Bindlib.msubst bnd (Array.of_list args)))
      bnd args,
    Mark.get body )

let payload_args col cell_type payload_var body_pos : Ast.expr boxed list =
  match col.col_vars with
  | [] -> []
  | [_] -> [Expr.evar payload_var (Untyped { pos = col.col_pos })]
  | vars ->
    let arity = List.length vars in
    (match cell_type with
    | TTuple tl, _ when List.length tl <> arity ->
      let n = List.length tl in
      Message.error
        ~pos:(List.fold_left (fun acc (_, p) -> Pos.join acc p) Pos.void vars)
        "This pattern has %d argument%s, while %d %s expected by constructor@ \
         %a."
        arity
        (if arity = 1 then "" else "s")
        n
        (if n = 1 then "is" else "are")
        EnumConstructor.format col.col_constr
    | _ -> ());
    let m = Untyped { pos = body_pos } in
    List.init arity (fun index ->
        Expr.etupleaccess ~e:(Expr.evar payload_var m) ~index ~size:arity m)

let payload_type pendings cell_type =
  match cell_type with
  | TForAll _, _ -> (
    assert (Type.is_universal cell_type);
    match
      List.find_map
        (fun p ->
          match head_col p with
          | Some { col_vars = _ :: _ :: _ as vars; _ } -> Some vars
          | _ -> None)
        pendings
    with
    | Some vars ->
      ( TTuple (List.map (fun (_, p) -> Type.fresh_var p) vars),
        List.fold_left (fun acc (_, p) -> Pos.join acc p) Pos.void vars )
    | None -> cell_type)
  | _ -> cell_type

let payload_name pendings =
  Option.value ~default:"_"
  @@ List.find_map
       (fun p ->
         match head_col p with
         | Some { col_vars = [(v, _)]; _ } -> Some (Bindlib.name_of v)
         | Some { col_vars = _ :: _ :: _; _ } -> Some "payload"
         | _ -> None)
       pendings

let payload_pos pendings =
  Option.value ~default:Pos.void
  @@ List.find_map
       (fun p ->
         match head_col p with
         | Some { col_vars = _ :: _ as vars; _ } ->
           Some
             (List.fold_left (fun acc (_, p) -> Pos.join acc p) Pos.void vars)
         | _ -> None)
       pendings

let format_case fmt case =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
    (fun fmt -> function
      | None -> Format.pp_print_string fmt "anything"
      | Some c -> EnumConstructor.format fmt c)
    fmt case

let check_column_enums ncols rows =
  ignore
    (List.fold_left
       (fun seen row ->
         List.map2
           (fun seen col ->
             match seen, col with
             | None, Some col -> Some col.col_enum
             | Some prev, Some col when not (EnumName.equal prev col.col_enum)
               ->
               Message.error ~pos:col.col_pos
                 "This case matches a constructor of enumeration@ %a@ but@ \
                  previous@ cases@ were@ matching@ constructors@ of@ \
                  enumeration@ %a."
                 EnumName.format prev EnumName.format col.col_enum
             | seen, _ -> seen)
           seen row.row_cols)
       (List.init ncols (fun _ -> None))
       rows)

let same_columns r1 r2 =
  List.for_all2
    (fun c1 c2 ->
      match c1, c2 with
      | None, None -> true
      | Some c1, Some c2 -> EnumConstructor.equal c1.col_constr c2.col_constr
      | _ -> false)
    r1.row_cols r2.row_cols

let check_unused_rows ~no_wildcard_warning ncols rows reached =
  ignore
    (List.fold_left
       (fun acc (i, row) ->
         (if not (IntSet.mem i reached) then
            if is_wildcard row then (
              if not no_wildcard_warning then
                match
                  List.find_map
                    (fun (_, row) ->
                      List.find_map
                        (Option.map (fun c -> c.col_enum))
                        row.row_cols)
                    rows
                with
                | Some name when ncols = 1 ->
                  Message.warning ~pos:row.row_pos
                    "Unreachable match case, all constructors of the \
                     enumeration@ %a@ are@ already@ specified."
                    EnumName.format name
                | _ ->
                  Message.warning ~pos:row.row_pos
                    "Unreachable match case, all combinations of constructors@ \
                     are@ already@ specified.")
            else
              match
                List.find_opt
                  (fun r -> (not (is_wildcard r)) && same_columns r row)
                  acc
              with
              | Some r -> (
                let extra_pos =
                  ["", Expr.pos row.row_body; "", Expr.pos r.row_body]
                in
                match row.row_cols with
                | [Some col] ->
                  Message.error ~extra_pos
                    "The constructor %a@ has@ been@ matched@ twice."
                    EnumConstructor.format col.col_constr
                | _ ->
                  Message.error ~extra_pos
                    "This combination of constructors has been matched twice.")
              | None ->
                Message.warning ~pos:row.row_pos
                  "Unreachable match case, it is already covered by the \
                   previous cases.");
         row :: acc)
       [] rows)

let compile
    (ctxt : Name_resolution.context)
    ~(scrutinees : Ast.expr boxed list)
    ~(no_wildcard_warning : bool)
    (emark : untyped mark)
    (rows : row list) : Ast.expr boxed =
  let pos = Expr.mark_pos emark in
  let ncols = List.length scrutinees in
  check_column_enums ncols rows;
  let rec compile_from path pendings = function
    | [] -> (
      match pendings with
      | [] -> assert false
      | (i, row) :: _ -> row.row_body, IntSet.singleton i)
    | scrutinee :: scrutinees -> (
      match List.find_map head_col pendings with
      | None -> compile_from (None :: path) (List.map pop pendings) scrutinees
      | Some { col_enum; _ } ->
        let constructors =
          fst (EnumName.Map.find col_enum ctxt.Name_resolution.enums)
        in
        let cases, reached =
          EnumConstructor.Map.fold
            (fun c_uid cell_type (cases, reached) ->
              let matching =
                List.filter
                  (fun p ->
                    match head_col p with
                    | None -> true
                    | Some col -> EnumConstructor.equal col.col_constr c_uid)
                  pendings
              in
              match matching with
              | [] ->
                if ncols = 1 then cases, reached
                else
                  Message.error ~pos
                    "This pattern matching is not exhaustive, the following \
                     case is not covered:@ @[<hov>%a@]"
                    format_case
                    (List.rev_append path
                       (Some c_uid :: List.map (fun _ -> None) scrutinees))
              | _ ->
                let payload_var = Var.make (payload_name matching) in
                let pos_binder = payload_pos matching in
                let payload_ty = payload_type matching cell_type in
                let matching =
                  List.map
                    (fun ((i, row) as p) ->
                      match head_col p with
                      | Some ({ col_vars = _ :: _; _ } as col) ->
                        let args =
                          payload_args col cell_type payload_var
                            (Expr.pos row.row_body)
                        in
                        let row_body =
                          subst_vars
                            (List.map fst col.col_vars)
                            args row.row_body
                        in
                        pop (i, { row with row_body })
                      | _ -> pop p)
                    matching
                in
                let body, reached_c =
                  compile_from (Some c_uid :: path) matching scrutinees
                in
                let body =
                  Expr.etag
                    (Branching (Some (EnumConstructor.to_string c_uid)))
                    body
                in
                ( EnumConstructor.Map.add c_uid
                    (Expr.eabs
                       (Expr.bind [| payload_var |] body)
                       [pos_binder] [payload_ty] (Mark.get body))
                    cases,
                  IntSet.union reached reached_c ))
            constructors
            (EnumConstructor.Map.empty, IntSet.empty)
        in
        Expr.ematch ~e:scrutinee ~name:col_enum ~cases emark, reached)
  in
  let rows = List.mapi (fun i row -> i, row) rows in
  let expr, reached = compile_from [] rows scrutinees in
  check_unused_rows ~no_wildcard_warning ncols rows reached;
  expr
