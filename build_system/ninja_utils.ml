(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020 Inria,
   contributor: Emile Rolley <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

module Expr = struct
  type t = Lit of string | Var of string | Seq of t list

  let rec format fmt = function
    | Lit s -> Format.fprintf fmt "%s" s
    | Var s -> Format.fprintf fmt "$%s" s
    | Seq ls -> format_list fmt ls

  and format_list fmt = function
    | hd :: tl ->
      Format.fprintf fmt "%a%a" format hd
        (fun fmt tl ->
          tl |> List.iter (fun s -> Format.fprintf fmt " %a" format s))
        tl
    | [] -> ()
end

module Rule = struct
  type t = { name : string; command : Expr.t; description : Expr.t option }

  let make name ~command ~description =
    { name; command; description = Option.some description }

  let format fmt rule =
    let format_description fmt = function
      | Some e -> Format.fprintf fmt "  description = %a\n" Expr.format e
      | None -> Format.fprintf fmt "\n"
    in
    Format.fprintf fmt "rule %s\n  command = %a\n%a" rule.name Expr.format
      rule.command format_description rule.description
end

module Build = struct
  type t = {
    outputs : Expr.t list;
    rule : string;
    inputs : Expr.t list option;
    vars : (string * Expr.t) list;
  }

  let make ~outputs ~rule = { outputs; rule; inputs = Option.none; vars = [] }

  let make_with_vars ~outputs ~rule ~vars =
    { outputs; rule; inputs = Option.none; vars }

  let make_with_inputs ~outputs ~rule ~inputs =
    { outputs; rule; inputs = Option.some inputs; vars = [] }

  let make_with_vars_and_inputs ~outputs ~rule ~inputs ~vars =
    { outputs; rule; inputs = Option.some inputs; vars }

  let empty = make ~outputs:[ Expr.Lit "empty" ] ~rule:"phony"

  let unpath ?(sep = "-") path =
    Re.Pcre.(substitute ~rex:(regexp "/") ~subst:(fun _ -> sep)) path

  let format fmt build =
    let format_inputs fmt = function
      | Some exs -> Format.fprintf fmt " %a" Expr.format_list exs
      | None -> ()
    and format_vars fmt vars =
      List.iter
        (fun (name, exp) ->
          Format.fprintf fmt "  %s = %a\n" name Expr.format exp)
        vars
    in
    Format.fprintf fmt "build %a: %s%a\n%a" Expr.format_list build.outputs
      build.rule format_inputs build.inputs format_vars build.vars
end

module RuleMap : Map.S with type key = String.t = Map.Make (String)
module BuildMap : Map.S with type key = String.t = Map.Make (String)

type ninja = { rules : Rule.t RuleMap.t; builds : Build.t BuildMap.t }

let empty = { rules = RuleMap.empty; builds = BuildMap.empty }

let format fmt ninja =
  let format_for_all iter format =
    iter (fun _name rule -> Format.fprintf fmt "%a\n" format rule)
  in
  format_for_all RuleMap.iter Rule.format ninja.rules;
  format_for_all BuildMap.iter Build.format ninja.builds
