(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020-2026 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Louis Gesbert
   <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

let dev_mode_env_varname = "CATALA_DEVELOPER"

let is_dev =
  Sys.argv.(0) = "catala"
  &&
  match Sys.getenv_opt dev_mode_env_varname with
  | None | Some ("" | "0" | "no" | "reentrant") -> false
  | _ -> true

let in_catala_src () =
  let open Catala_utils.File in
  match String.trim (process_out "git" ["rev-parse"; "--show-toplevel"]) with
  | dir -> exists (dir / "catala.opam")
  | exception Failure _ -> false

let () =
  if is_dev && in_catala_src () then (
    let env = Unix.environment () in
    Array.iteri
      (fun i v ->
        if String.starts_with ~prefix:(dev_mode_env_varname ^ "=") v then
          env.(i) <- dev_mode_env_varname ^ "=reentrant")
      env;
    let dune = Catala_utils.File.check_exec "dune" in
    Unix.execve dune (Array.append [| dune; "exec"; "--" |] Sys.argv) env)
  else Driver.main ()
