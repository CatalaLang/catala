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

type 'm naked_expr = (lcalc, 'm) naked_gexpr
and 'm expr = (lcalc, 'm) gexpr

type 'm program = 'm expr Shared_ast.program

module OptionMonad = struct
  let return ~(mark : 'a mark) e =
    Expr.einj ~e ~cons:Expr.some_constr ~name:Expr.option_enum mark

  let empty ~(mark : 'a mark) =
    Expr.einj ~e:(Expr.elit LUnit mark) ~cons:Expr.none_constr ~name:Expr.option_enum mark

  let bind_var ~(mark : 'a mark) f x arg =
    let cases =
      EnumConstructor.Map.of_list
        [
          ( Expr.none_constr,
            let x = Var.make "_" in
            Expr.eabs
              (Expr.bind [| x |]
                 (Expr.einj ~e:(Expr.evar x mark) ~cons:Expr.none_constr ~name:Expr.option_enum
                    mark))
              [TLit TUnit, Expr.mark_pos mark]
              mark );
          (* | None x -> None x *)
          ( Expr.some_constr,
            Expr.eabs (Expr.bind [| x |] f) [TAny, Expr.mark_pos mark] mark )
          (*| Some x -> f (where f contains x as a free variable) *);
        ]
    in
    Expr.ematch ~e:arg ~name:Expr.option_enum ~cases mark

  let bind ~(mark : 'a mark) ~(var_name : string) f arg =
    let x = Var.make var_name in
    (* todo modify*)
    bind_var f x arg ~mark

  let bind_cont ~(mark : 'a mark) ~(var_name : string) f arg =
    let x = Var.make var_name in
    bind_var (f x) x arg ~mark

  let mbind_mvar ~(mark : 'a mark) f xs args =
    (* match e1, ..., en with | Some e1', ..., Some en' -> f (e1, ..., en) | _
       -> None *)
    ListLabels.fold_left2 xs args ~f:(bind_var ~mark)
      ~init:(Expr.eapp f (List.map (fun v -> Expr.evar v mark) xs) mark)

  let mbind ~(mark : 'a mark) ~(var_name : string) f args =
    (* match e1, ..., en with | Some e1', ..., Some en' -> f (e1, ..., en) | _
       -> None *)
    let vars =
      ListLabels.mapi args ~f:(fun i _ ->
          Var.make (Format.sprintf "%s_%i" var_name i))
    in
    mbind_mvar f vars args ~mark

  let mbind_cont ~(mark : 'a mark) ~(var_name : string) f args =
    let vars =
      ListLabels.mapi args ~f:(fun i _ ->
          Var.make (Format.sprintf "%s_%i" var_name i))
    in
    ListLabels.fold_left2 vars args ~f:(bind_var ~mark) ~init:(f vars)
  (* mbind_mvar (f vars) vars args ~mark *)

  let mmap_mvar ~(mark : 'a mark) f xs args =
    (* match e1, ..., en with | Some e1', ..., Some en' -> f (e1, ..., en) | _
       -> None *)
    ListLabels.fold_left2 xs args ~f:(bind_var ~mark)
      ~init:
        (Expr.einj
           ~e:(Expr.eapp f (List.map (fun v -> Expr.evar v mark) xs) mark)
           ~cons:Expr.some_constr ~name:Expr.option_enum mark)

  let map_var ~(mark : 'a mark) f x arg = mmap_mvar f [x] [arg] ~mark

  let map ~(mark : 'a mark) ~(var_name : string) f arg =
    let x = Var.make var_name in
    map_var f x arg ~mark

  let mmap ~(mark : 'a mark) ~(var_name : string) f args =
    let vars =
      ListLabels.mapi args ~f:(fun i _ ->
          Var.make (Format.sprintf "%s_%i" var_name i))
    in
    mmap_mvar f vars args ~mark

  let error_on_empty
      ~(mark : 'a mark)
      ~(var_name : string)
      ?(toplevel = false)
      arg =
    let cases =
      EnumConstructor.Map.of_list
        [
          ( Expr.none_constr,
            let x = Var.make "_" in
            Expr.eabs
              (Expr.bind [| x |] (Expr.eraise NoValueProvided mark))
              [TAny, Expr.mark_pos mark]
              mark );
          (* | None x -> raise NoValueProvided *)
          Expr.some_constr, Expr.fun_id ~var_name mark (* | Some x -> x*);
        ]
    in
    if toplevel then Expr.ematch ~e:arg ~name:Expr.option_enum ~cases mark
    else return ~mark (Expr.ematch ~e:arg ~name:Expr.option_enum ~cases mark)
end
