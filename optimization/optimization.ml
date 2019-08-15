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
