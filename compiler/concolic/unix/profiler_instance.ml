(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Profiler
module BackendMap = Map.Make (String)
module VerbosityMap = Map.Make (String)

type instance_maker =
  verbosity:Profiler.verbosity ->
  directory:string ->
  name:string ->
  Profiler.instance

type 'config driver = (module Profiler.DRIVER with type config = 'config)

type 'instance_maker backend_infos = {
  instance_maker : 'instance_maker;
  view : view;
}

let registered_backends : instance_maker backend_infos BackendMap.t ref =
  ref BackendMap.empty

let register_backend : type config.
    string list -> (config driver -> instance_maker) -> config driver -> unit =
 fun env instance_maker driver ->
  let module Driver = (val driver : DRIVER with type config = config) in
  (* Wrapping the kind in a View to avoid internal types of the Driver escaping
     their scope *)
  let view = Profiler.View Driver.kind in
  match List.find_opt (fun k -> BackendMap.mem k !registered_backends) env with
  | Some k ->
    Format.ksprintf failwith
      "Profiler backend already registered for value \"%s\"" k
  | None ->
    registered_backends :=
      List.fold_left
        (fun acc k ->
          BackendMap.add k { instance_maker = instance_maker driver; view } acc)
        !registered_backends env

let verbosity = Debug

let wrap_backend_verbosity instance_maker ~directory ~name =
  Some (instance_maker ~verbosity ~directory ~name)

type wrapped_instance_maker =
  directory:string -> name:string -> Profiler.instance option

let split_no_empty delim ?(limit = max_int) path =
  let l = String.length path in
  let rec do_slashes acc limit i =
    if i >= l then List.rev acc
    else if path.[i] = delim then do_slashes acc limit (i + 1)
    else do_split acc limit i
  and do_split acc limit i =
    if limit <= 0 then
      if i = l then List.rev acc else List.rev (String.sub path i (l - i) :: acc)
    else do_component acc (pred limit) i i
  and do_component acc limit i j =
    if j >= l then
      if i = j then List.rev acc else List.rev (String.sub path i (j - i) :: acc)
    else if path.[j] = delim then
      do_slashes (String.sub path i (j - i) :: acc) limit j
    else do_component acc limit i (j + 1)
  in
  if limit > 0 then do_slashes [] limit 0 else [path]

let selected_backends () =
  let fail s =
    Format.kasprintf failwith
      "@[<v 2>%s.@,\
       You can set backends with PROFILING_BACKENDS=<list of ; separated \
       backends>.@,\
       @[<v 2>Available backends are:@,\
       %a@."
      s
      (Format.pp_print_list ~pp_sep:Format.pp_print_cut Format.pp_print_string)
      (BackendMap.fold
         (fun k _ acc -> (if k = "" then "\"\"" else k) :: acc)
         !registered_backends [])
  in
  match
    Sys.getenv_opt "PROFILING_BACKENDS" |> Option.map String.lowercase_ascii
  with
  | None -> (
    match Sys.getenv_opt "PROFILING" with
    | None -> None
    | Some _ ->
      Format.sprintf
        "No backend selected but profilers were enabled in PROFILING."
      |> fail)
  | Some backends ->
    split_no_empty ';' backends
    |> List.fold_left
         (fun acc backend ->
           match
             BackendMap.find_opt (String.trim backend) !registered_backends
           with
           | Some { instance_maker; view } ->
             { instance_maker = wrap_backend_verbosity instance_maker; view }
             :: acc
           | None ->
             Format.sprintf "No backend registered for value \"%s\"" backend
             |> fail)
         []
    |> Option.some

let () =
  if
    (true [@profiler.overwrite false])
    && Sys.getenv_opt "PROFILING_BACKENDS" <> None
  then
    Format.ksprintf failwith
      "The profiling has been enabled with PROFILING_BACKENDS='...' but the \
       program hasn't been compiled with TEZOS_PPX_PROFILER='...'"
  else ()
