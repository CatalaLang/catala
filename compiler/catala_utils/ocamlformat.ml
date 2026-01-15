(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria, contributor:
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

open Ocamlformat_lib

let conf = Conf.default

let write filename contents =
  File.with_out_channel filename @@ fun oc -> output_string oc contents

let format input_name =
  let source = File.contents input_name in
  match
    Translation_unit.parse_and_format Syntax.Use_file conf ~input_name ~source
  with
  | Ok formatted -> write input_name formatted
  | Error e ->
    let content =
      Message.Content.of_message (fun fmt -> Translation_unit.Error.print fmt e)
    in
    raise (Message.CompilerError content)
