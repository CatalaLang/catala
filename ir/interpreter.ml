(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

open Ir

type ctx = {
  int_values: Int64.t Ast.IntVariableMap.t;
  bool_values : bool Ast.BoolVariableMap.t;
}

let empty_ctx = {
  int_values = Ast.IntVariableMap.empty;
  bool_values = Ast.BoolVariableMap.empty;
}

let interpret_int_literal (l: int_literal Pos.marked) (ctx: ctx) : Int64.t =
  match Pos.unmark l with
  | Int i -> i
  | IntVar v -> Ast.IntVariableMap.find v ctx.int_values

let interpret_bool_literal (l: bool_literal Pos.marked) (ctx: ctx) : bool =
  match Pos.unmark l with
  | Bool b -> b
  | BoolVar v -> Ast.BoolVariableMap.find v ctx.bool_values


let interpret_logical_expression (e: logical_expression Pos.marked) (ctx: ctx) : bool =
  match Pos.unmark e with
  | Comparison (op, v1, v2)  ->
    let v1 = interpret_int_literal v1 ctx in
    let v2 = interpret_int_literal v2 ctx in
    begin match Pos.unmark op with
      | Ast.Lt -> v1 < v2
      | Ast.Lte -> v1 <= v2
      | Ast.Gt -> v1 > v2
      | Ast.Gte -> v1 >= v2
      | Ast.Neq -> v1 <> v2
      | Ast.Eq -> v1 = v2
    end
  | LogicalBinop (op, v1, v2) ->
    let v1 = interpret_bool_literal v1 ctx in
    let v2 = interpret_bool_literal v2 ctx in
    begin match Pos.unmark op with
      | Ast.And -> v1 && v2
      | Ast.Or -> v1 || v2
    end
  | LogicalNot v1 ->
    let v1 = interpret_bool_literal v1 ctx in
    not v1
  | BoolLiteral v -> interpret_bool_literal v ctx

let interpret_arithmetic_expression (e: arithmetic_expression Pos.marked) (ctx: ctx) : Int64.t =
  match Pos.unmark e with
  | ArithmeticBinop (op, v1, v2) ->
    let v1 = interpret_int_literal v1 ctx in
    let v2 = interpret_int_literal v2 ctx in
    begin match Pos.unmark op with
      | Ast.Add -> Int64.add v1 v2
      | Ast.Sub -> Int64.sub v1 v2
      | Ast.Mul -> Int64.mul v1 v2
      | Ast.Div -> Int64.div v1 v2
    end
  | ArithmeticMinus v1 ->
    let v1 = interpret_int_literal v1 ctx in
    Int64.sub Int64.zero v1
  | Conditional (v1, v2, v3) ->
    let v1 = interpret_bool_literal v1 ctx in
    let v2 = interpret_int_literal v2 ctx in
    let v3 = interpret_int_literal v3 ctx in
    if v1 then v2 else v3
  | IntLiteral v -> interpret_int_literal v ctx

let interpret_command (cmd: command) (ctx: ctx) : ctx =
  match cmd with
  | BoolDef (var, e) ->
    let v = interpret_logical_expression e ctx in
    { ctx with bool_values = Ast.BoolVariableMap.add var v ctx.bool_values }
  | IntDef (var, e) ->
    let v = interpret_arithmetic_expression e ctx in
    { ctx with int_values = Ast.IntVariableMap.add var v ctx.int_values }
  | Constraint e ->
    if not (interpret_logical_expression e ctx) then
      raise
        (Errors.VerifiscRuntimeError
           (Printf.sprintf "assertion violated %s"
              (Pos.format_position (Pos.get_position e))))
    else ctx

let interpret_function (f_var : Ast.FunctionVariable.t) (f: func) (ctx: ctx) : ctx =
  List.iter (fun var ->
      if not (Ast.IntVariableMap.mem var ctx.int_values) then
        raise
          (Errors.VerifiscRuntimeError
             (Printf.sprintf "missing input value for function %s: %s"
                (Pos.unmark f_var.Ast.FunctionVariable.name)
                (Pos.unmark var.Ast.IntVariable.name)
             ))
    ) (fst f.inputs);
  List.iter (fun var ->
      if not (Ast.BoolVariableMap.mem var ctx.bool_values) then
        raise
          (Errors.VerifiscRuntimeError
             (Printf.sprintf "missing input value for function %s: %s"
                (Pos.unmark f_var.Ast.FunctionVariable.name)
                (Pos.unmark var.Ast.BoolVariable.name)
             ))
    ) (snd f.inputs);
  let ctx = List.fold_left (fun ctx cmd ->
      interpret_command cmd ctx
    ) ctx f.body
  in
  List.iter (fun var ->
      if not (Ast.IntVariableMap.mem var ctx.int_values) then
        raise
          (Errors.VerifiscRuntimeError
             (Printf.sprintf "missing output value for function %s: %s"
                (Pos.unmark f_var.Ast.FunctionVariable.name)
                (Pos.unmark var.Ast.IntVariable.name)
             ))
    ) (fst f.outputs);
  List.iter (fun var ->
      if not (Ast.BoolVariableMap.mem var ctx.bool_values) then
        raise
          (Errors.VerifiscRuntimeError
             (Printf.sprintf "missing output value for function %s: %s"
                (Pos.unmark f_var.Ast.FunctionVariable.name)
                (Pos.unmark var.Ast.BoolVariable.name)
             ))
    ) (snd f.outputs);
  ctx

let read_inputs_from_stdin (f: func) (mult_factor: int): ctx =
  Printf.printf "Enter the input values of the program and press [Enter]\n";
  let ctx = empty_ctx in
  let ctx = List.fold_left (fun ctx var ->
      Printf.printf "%s : float := " (Pos.unmark var.Ast.IntVariable.name);
      let input = ref None in
      while !input = None do
        match read_float_opt () with
        | None -> Printf.printf "Please enter an integer!\n"
        | Some f -> input := Some (Int64.of_float (f *. (float_of_int mult_factor)))
      done;
      match !input with
      | None -> assert false (* should not happen *)

      | Some i ->
        { ctx with int_values = Ast.IntVariableMap.add var i ctx.int_values }
    ) ctx (fst f.inputs) in
  let ctx = List.fold_left (fun ctx var ->
      Printf.printf "%s : bool := " (Pos.unmark var.Ast.BoolVariable.name);
      let input = ref None in
      while !input = None do
        let str = read_line () in
        if str = "true" then
          input := Some true
        else if str = "false" then
          input := Some false
        else
          Printf.printf "Please enter an integer!\n"
      done;
      match !input with
      | None -> assert false (* should not happen *)
      | Some b -> { ctx with bool_values = Ast.BoolVariableMap.add var b ctx.bool_values }
    ) ctx (snd f.inputs) in
  ctx

let repl_interpreter (p: program) : unit =
  Printf.printf "Here is the list of functions in the program:\n%s\n"
    (String.concat "\n" (List.map (fun (f, _) ->
         Printf.sprintf "[%d] %s (%s)"
           (f.Ast.FunctionVariable.id)
           (Pos.unmark f.Ast.FunctionVariable.name)
           (Pos.unmark f.Ast.FunctionVariable.descr)
       ) (Ast.FunctionVariableMap.bindings p.program_functions)));
  Printf.printf "Please enter the number of the function you wish to execute and press [Enter]:\n";
  let input = ref None in
  while !input = None do
    match read_int_opt () with
    | None -> Printf.printf "Please enter an integer!\n"
    | Some i -> input := Some i
  done;
  match !input with
  | None -> assert false (* should not happen *)
  | Some i ->
    let f, func =
      Ast.FunctionVariableMap.choose
        (Ast.FunctionVariableMap.filter
           (fun f _ -> f.Ast.FunctionVariable.id = i)
           p.program_functions)
    in
    let ctx = read_inputs_from_stdin func p.program_mult_factor in
    let ctx = interpret_function f func ctx in
    Printf.printf "Here are the functions outputs:\n";
    List.iter (fun var ->
        Printf.printf "%s : float = %f\n"
          (Pos.unmark var.Ast.IntVariable.name)
          ((Int64.to_float (Ast.IntVariableMap.find var ctx.int_values)) /.
           (float_of_int p.program_mult_factor))
      ) (fst func.outputs);
    List.iter (fun var ->
        Printf.printf "%s : bool = %b\n"
          (Pos.unmark var.Ast.BoolVariable.name)
          (Ast.BoolVariableMap.find var ctx.bool_values)
      ) (snd func.outputs)
