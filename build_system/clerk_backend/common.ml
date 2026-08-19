(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2020-2025 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Emile Rolley
   <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

open Clerk_utils
open Catala_utils
include Sig

type t = (module Sig.S)

let backends : (Clerk_config.backend, t) Hashtbl.t = Hashtbl.create 7

let register (module B : S) =
  Clerk_config.register_backend ~name:B.name B.config_backend;
  Hashtbl.add backends B.config_backend (module B)

let get bk = Hashtbl.find backends bk
let all () = Hashtbl.to_seq_values backends |> List.of_seq
let name (module B : S) = B.name
let id (module B : S) = B.config_backend

open File
open Var
module Nj = Ninja_utils

let static_base_rules =
  [
    Ninja_utils.rule "copy"
      ~command:
        (if Sys.win32 then
           [
             Raw "cmd";
             Raw "/c";
             Raw "copy";
             Raw "/by";
             Raw ">nul";
             !!input;
             Raw "+nul";
             !!output;
           ] (* The "+nul" forces the timestamp of the new file to be updated *)
         else [Word "cp"; Word "-f"; !!input; !!output])
      ~description:[Word "<copy>"; !!input];
  ]

let extern_src ~filename ~name:backend ~ext ~missing =
  let f = filename -.- ext in
  match check_file f with
  | Some f -> f, missing
  | None -> (
    match
      check_file
        ((dirname f / backend / String.to_id (remove_extension (basename f)))
        -.- ext)
    with
    | Some f -> f, missing
    | None -> f, f :: missing)

let target ?name:backend (* ?(is_stdlib=false)  *) ext =
  let ext =
    match ext.[0] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> "." ^ ext
    | _ -> ext
  in
  let dir = !Var.tdir in
  (* let dir = if is_stdlib then dir / Scan.libcatala else dir in *)
  let dir = match backend with Some b -> dir / b | None -> dir in
  Nj.Expr.Word ((dir / !Var.dst) ^ ext)

let catala_obj_target modname =
  Nj.Expr.Word ("@catala/obj/" ^ String.to_id modname)

let interface_dep ~name:backend modname =
  Nj.Expr.Word ("@" ^ backend ^ "/interface/" ^ String.to_id modname)

let src_dep ~name:backend modname =
  Nj.Expr.Word ("@" ^ backend ^ "/src/" ^ String.to_id modname)

let catala_obj_dep item =
  match item.Scan.module_def with
  | Some (m, _) -> Nj.Expr.Word ("@catala/obj/" ^ String.to_id m)
  | None ->
    Nj.Expr.Word
      ("@catala/obj/"
      ^ (dirname item.file_name / String.to_id (basename item.file_name -.- ""))
      )

let obj_dep ~name:backend item =
  match item.Scan.module_def with
  | Some (m, _) -> Nj.Expr.Word ("@" ^ backend ^ "/obj/" ^ String.to_id m)
  | None ->
    Nj.Expr.Word
      ("@"
      ^ backend
      ^ "/obj/"
      ^ (dirname item.file_name / String.to_id (basename item.file_name -.- ""))
      )

module Make_backend (A : Sig.Spec) : Sig.S = struct
  module Backend = struct
    include A

    type Clerk_config.backend += T

    let config_backend = T
    let () = Clerk_config.register_backend ~name config_backend

    let current_target item ext =
      if item.Scan.is_stdlib then
        Nj.Expr.Word ((!Var.tdir / name / stdlib_subdir / !Var.dst) -.- ext)
      else target ~name ext

    let interface_dep = interface_dep ~name
    let src_dep = src_dep ~name
    let obj_dep = obj_dep ~name

    let runtime_targets ~only_source =
      if only_source then ["@" ^ name ^ "/runtime/src"]
      else ["@" ^ name ^ "/runtime/obj"]

    let external_copy item =
      let catala_src = Nj.Expr.Word (!Var.tdir / !Var.src) in
      let srcs, _missing =
        (* Note: `missing` is ignored here, because we can't trigger an error on
           a backend that may not be enabled for the given item. A second check
           is done at the end of Clerk_rules.run_ninja once we have the info *)
        List.fold_right
          (fun ext (srcs, missing) ->
            let src, missing =
              extern_src ~filename:item.Scan.file_name ~name ~ext ~missing
            in
            Seq.cons (Nj.Expr.Word src, ext) srcs, missing)
          src_extensions (Seq.empty, [])
      in
      Seq.map
        (fun (src, ext) ->
          let output =
            if item.is_stdlib then
              (!Var.tdir / name / stdlib_subdir / !Var.dst) -.- ext
            else (!Var.tdir / name / !Var.dst) -.- ext
          in
          Ninja_utils.build "copy" ~implicit_in:[catala_src] ~inputs:[src]
            ~outputs:[Word output])
        srcs
  end

  let () = register (module Backend)

  include Backend
end
