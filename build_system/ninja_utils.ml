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

(** Ninja variable names *)
module Var = struct
  type t = V of string

  let make s = V s
  let name (V v) = v
  let v (V v) = Printf.sprintf "${%s}" v
end

module Expr = struct
  type t = string list

  let format =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ' ')
      (fun fmt s ->
        Format.pp_print_string fmt
          (Re.replace_string Re.(compile space) ~by:"$ " s))
end

module Binding = struct
  type t = Var.t * Expr.t

  let make var e = var, e

  let format ~global ppf (v, e) =
    if not global then Format.pp_print_string ppf "  ";
    Format.fprintf ppf "%s = %a" (Var.name v) Expr.format e;
    if global then Format.pp_print_newline ppf ()

  let format_list ~global ppf l =
    Format.pp_print_list ~pp_sep:Format.pp_print_newline (format ~global) ppf l
end

module Rule = struct
  type t = {
    name : string;
    command : Expr.t;
    description : Expr.t option;
    vars : Binding.t list;
  }

  let make ?(vars = []) name ~command ~description =
    { name; command; description = Option.some description; vars }

  let format fmt rule =
    let bindings =
      Binding.make (Var.make "command") rule.command
      :: Option.(
           to_list
             (map
                (fun d -> Binding.make (Var.make "description") d)
                rule.description))
      @ rule.vars
    in
    Format.fprintf fmt "rule %s\n%a" rule.name
      (Binding.format_list ~global:false)
      bindings
end

module Build = struct
  type t = {
    rule : string;
    inputs : Expr.t option;
    implicit_in : Expr.t;
    outputs : Expr.t;
    implicit_out : Expr.t option;
    vars : Binding.t list;
  }

  let make ?inputs ?(implicit_in = []) ~outputs ?implicit_out ?(vars = []) rule
      =
    { rule; inputs; implicit_in; outputs; implicit_out; vars }

  let empty = make ~outputs:["empty"] "phony"

  let unpath ?(sep = "-") path =
    Re.Pcre.(substitute ~rex:(regexp "/") ~subst:(fun _ -> sep)) path

  let format fmt t =
    Format.fprintf fmt "build %a%a: %s%a%a%a%a" Expr.format t.outputs
      (Format.pp_print_option (fun fmt i ->
           Format.pp_print_string fmt " | ";
           Expr.format fmt i))
      t.implicit_out t.rule
      (Format.pp_print_option (fun ppf e ->
           Format.pp_print_char ppf ' ';
           Expr.format ppf e))
      t.inputs
      (fun ppf -> function
        | [] -> ()
        | e ->
          Format.pp_print_string ppf " | ";
          Expr.format ppf e)
      t.implicit_in
      (if t.vars = [] then fun _ () -> () else Format.pp_print_newline)
      ()
      (Binding.format_list ~global:false)
      t.vars
end

module Default = struct
  type t = Expr.t

  let make rules = rules
  let format ppf t = Format.fprintf ppf "default %a" Expr.format t
end

type def =
  | Comment of string
  | Binding of Binding.t
  | Rule of Rule.t
  | Build of Build.t
  | Default of Default.t

let comment s = Comment s
let binding v e = Binding (Binding.make v e)

let rule ?vars name ~command ~description =
  Rule (Rule.make ?vars name ~command ~description)

let build ?inputs ?implicit_in ~outputs ?implicit_out ?vars rule =
  Build (Build.make ?inputs ?implicit_in ~outputs ?implicit_out ?vars rule)

let default rules = Default (Default.make rules)

let format_def ppf def =
  let () =
    match def with
    | Comment s ->
      Format.pp_print_list ~pp_sep:Format.pp_print_newline
        (fun ppf s ->
          if s <> "" then Format.pp_print_string ppf "# ";
          Format.pp_print_string ppf s)
        ppf
        (String.split_on_char '\n' s)
    | Binding b -> Binding.format ~global:true ppf b
    | Rule r ->
      Rule.format ppf r;
      Format.pp_print_newline ppf ()
    | Build b -> Build.format ppf b
    | Default d -> Default.format ppf d
  in
  Format.pp_print_flush ppf ()

type ninja = def Seq.t

let format ppf t =
  Format.pp_print_seq ~pp_sep:Format.pp_print_newline format_def ppf t;
  Format.pp_print_newline ppf ()
