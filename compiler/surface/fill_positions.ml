(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
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

open Catala_utils

let fill_pos_with_legislative_info (p : Ast.program) : Ast.program =
  let visitor =
    object
      inherit [_] Ast.map as super

      method! visit_pos f env x =
        f env (Mark.remove x), Pos.overwrite_law_info (Mark.get x) env

      method! visit_LawHeading
          (env : string list)
          (heading : Ast.law_heading)
          (children : Ast.law_structure list) =
        let env = Mark.remove heading.law_heading_name :: env in
        Ast.LawHeading
          ( super#visit_law_heading env heading,
            List.map (fun child -> super#visit_law_structure env child) children
          )
    end
  in
  visitor#visit_program [] p
