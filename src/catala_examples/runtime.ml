(* This file is part of the Catala compiler, a specification language for tax and social benefits
   computation rules. Copyright (C) 2020 Inria, contributor: Denis Merigoux
   <denis.merigoux@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
   in compliance with the License. You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software distributed under the License
   is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
   or implied. See the License for the specific language governing permissions and limitations under
   the License. *)

open Utils

(** {1 Types} *)

type money = Z.t

type integer = Z.t

type decimal = Q.t

type date = CalendarLib.Date.t

type duration = CalendarLib.Date.Period.t

(**{1 Constructors} *)

let money_of_cent_string (cents : string) : money = Z.of_string cents

let decimal_of_string (d : string) : decimal = Q.of_string d

let integer_of_string (i : string) : integer = Z.of_string i

(**{1 Logging} *)

let log_entry : 'a. string -> 'a -> 'a =
 fun msg arg ->
  Printf.printf "%s\n" msg;
  arg

(**{1 Exceptions and defaults} *)

exception EmptyError

let error_empty : 'a. 'a -> 'a =
 fun x -> try x with EmptyError -> Errors.raise_error "empty value found!"

let handle_default : 'a. (unit -> 'a) list -> (unit -> bool) -> (unit -> 'a) -> 'a =
 fun exceptions just cons ->
  let except =
    List.fold_left
      (fun acc except ->
        let new_val = try Some (except ()) with EmptyError -> None in
        match (acc, new_val) with
        | None, _ -> new_val
        | Some _, None -> acc
        | Some _, Some _ -> Errors.raise_error "conflict!")
      None exceptions
  in
  match except with Some x -> x | None -> if just () then cons () else raise EmptyError

(**{1 Operators} *)

let ( *$ ) (_m : money) (_d : decimal) : money = assert false

let ( +$ ) (_m1 : money) (_m2 : money) : money = assert false

let ( -$ ) (_m1 : money) (_m2 : money) : money = assert false

let ( <=$ ) (_m1 : money) (_m2 : money) : bool = assert false

let ( >=! ) (_m1 : integer) (_m2 : integer) : bool = assert false
