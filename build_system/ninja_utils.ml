(** Expression containing variable references. *)
module Expr = struct
  type t =
    (* Sequence of expressions. *)
    | Seq of t list
    (* Literal string. *)
    | Lit of string
    (* Variable reference. *)
    | Var of string

  let rec to_string = function
    | Lit s -> s
    | Var s -> "$" ^ s
    | Seq ls -> List.fold_left (fun acc s -> acc ^ " " ^ to_string s) "" ls
end

module Rule = struct
  (* NOTE: is name is really needed despite the use of Map? *)
  type t = { name : string; command : Expr.t; description : Expr.t option }

  let make name ~command ~description = { name; command; description = Option.some description }

  let to_string rule =
    Printf.sprintf "rule %s\n  command = %s\n" rule.name (Expr.to_string rule.command)
    ^ (rule.description
      |> Option.fold ~some:(fun e -> "  description = " ^ Expr.to_string e ^ "\n") ~none:"")
end

module RuleMap : Map.S with type key = String.t = Map.Make (String)


type ninja = { rules : Rule.t RuleMap.t (* builds : Build.t BuildMap.t *) }

let empty = { rules = RuleMap.empty (* ; builds = BuildMap.empty *) }

let write out ninja =
  RuleMap.iter (fun name rule -> Printf.fprintf out "%s" (Rule.to_string rule)) ninja.rules
