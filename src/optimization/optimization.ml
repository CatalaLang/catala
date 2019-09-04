(*
Copyright (C) 2019 Inria, contributor: Denis Merigoux <denis.merigoux@inria.fr>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

open Ir

let optimize
    (program: program)
  : program =

  let program = ref program in
  let nb_removed = ref max_int in

  while !nb_removed > 0 do
    let new_program = Global_value_numbering.optimize !program in
    let new_program = Partial_evaluation.optimize new_program in
    let new_program = Dead_code_elimination.optimize new_program in
    let new_nb_removed =
      nb_commands !program -
      nb_commands new_program
    in
    program := new_program;
    nb_removed := new_nb_removed;
  done;
  !program
