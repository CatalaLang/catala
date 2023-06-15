(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2022 Inria, contributor:
   Aymeric Fromherz <aymeric.fromherz@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Replicating the interface, with no actual implementation for compiling
    without the expected backend. All functions print an error message and exit *)

let dummy () =
  Catala_utils.Message.raise_error
    "This instance of Catala was compiled without Z3 support."

module Io = struct
  let init_backend () = dummy ()

  type backend_context = unit

  let make_context _ = dummy ()

  type vc_encoding = unit

  let translate_expr _ _ = dummy ()
  let encode_asserts _ _ = dummy ()

  type model = unit
  type vc_encoding_result = Success of model * model | Fail of string

  let print_negative_result _ _ _ = dummy ()
  let encode_and_check_vc _ _ = dummy ()
end
