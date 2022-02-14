(* This file is part of the Catala build system, a specification language for tax and social
   benefits computation rules. Copyright (C) 2020 Inria, contributor: Emile Rolley
   <emile.rolley@tuta.io>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

module Expr = struct
  type t = Seq of t list | Lit of string | Var of string

  let rec to_string = function
    | Lit s -> s
    | Var s -> "$" ^ s
    | Seq ls -> List.fold_left (fun acc s -> acc ^ " " ^ to_string s) "" ls

  let list_to_string ?(sep = " ") ls = ls |> List.map to_string |> String.concat sep
end

module Rule = struct
  type t = { name : string; command : Expr.t; description : Expr.t option }

  let make name ~command ~description = { name; command; description = Option.some description }

  let to_string rule =
    Printf.sprintf "rule %s\n  command =%s\n" rule.name (Expr.to_string rule.command)
    ^ (rule.description
      |> Option.fold ~some:(fun e -> "  description =" ^ Expr.to_string e ^ "\n") ~none:"")
end

module Build = struct
  type t = {
    outputs : Expr.t list;
    rule : string;
    inputs : Expr.t list option;
    vars : (string * Expr.t) list;
  }

  let make ~outputs ~rule = { outputs; rule; inputs = Option.none; vars = [] }

  let make_with_vars ~outputs ~rule ~vars = { outputs; rule; inputs = Option.none; vars }

  let make_with_inputs ~outputs ~rule ~inputs =
    { outputs; rule; inputs = Option.some inputs; vars = [] }

  let make_with_vars_and_inputs ~outputs ~rule ~inputs ~vars =
    { outputs; rule; inputs = Option.some inputs; vars }

  let empty = make ~outputs:[ Expr.Lit "empty" ] ~rule:"phony"

  let unpath path = Re.Pcre.(substitute ~rex:(regexp "/") ~subst:(fun _ -> "-")) path

  let to_string build =
    Printf.sprintf "build %s: %s" (Expr.list_to_string build.outputs) build.rule
    ^ (build.inputs |> Option.fold ~some:(fun ls -> " " ^ Expr.list_to_string ls) ~none:"")
    ^ "\n"
    ^ List.fold_left
        (fun acc (name, exp) -> acc ^ Printf.sprintf "  %s = %s\n" name (Expr.to_string exp))
        "" build.vars
end

module RuleMap : Map.S with type key = String.t = Map.Make (String)

module BuildMap : Map.S with type key = String.t = Map.Make (String)

type ninja = { rules : Rule.t RuleMap.t; builds : Build.t BuildMap.t }

let empty = { rules = RuleMap.empty; builds = BuildMap.empty }

let write out ninja =
  let write_for_all iter to_string =
    iter (fun _name rule -> Printf.fprintf out "%s\n" (to_string rule))
  in
  write_for_all RuleMap.iter Rule.to_string ninja.rules;
  write_for_all BuildMap.iter Build.to_string ninja.builds
