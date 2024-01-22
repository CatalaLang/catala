(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2020 Inria, contributor:
   Emile Rolley <emile.rolley@tuta.io>, Louis Gesbert <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

type t = string

(** Run finaliser [f] unconditionally after running [k ()], propagating any
    raised exception. *)
let finally f k =
  match k () with
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    f ();
    Printexc.raise_with_backtrace e bt
  | r ->
    f ();
    r

let temp_file pfx sfx =
  let f = Filename.temp_file pfx sfx in
  if not Cli.globals.debug then
    at_exit (fun () -> try Sys.remove f with _ -> ());
  f

let with_out_channel filename f =
  let oc = open_out filename in
  finally (fun () -> close_out oc) (fun () -> f oc)

let with_in_channel filename f =
  let oc = open_in filename in
  finally (fun () -> close_in oc) (fun () -> f oc)

let with_formatter_of_out_channel oc f =
  let fmt = Message.formatter_of_out_channel oc in
  finally (fun () -> Format.pp_print_flush fmt ()) @@ fun () -> f fmt

let with_formatter_of_file filename f =
  with_out_channel filename (fun oc -> with_formatter_of_out_channel oc f)

let with_formatter_of_opt_file filename_opt f =
  match filename_opt with
  | None -> finally (fun () -> flush stdout) (fun () -> f Format.std_formatter)
  | Some filename -> with_formatter_of_file filename f

let get_out_channel ~source_file ~output_file ?ext () =
  match output_file, ext with
  | Some "-", _ | None, None -> None, fun f -> f stdout
  | Some f, _ -> Some f, with_out_channel f
  | None, Some ext ->
    let src = Cli.input_src_file source_file in
    let f = Filename.remove_extension src ^ ext in
    Some f, with_out_channel f

let get_formatter_of_out_channel ~source_file ~output_file ?ext () =
  let f, with_ = get_out_channel ~source_file ~output_file ?ext () in
  f, fun fmt -> with_ (fun oc -> with_formatter_of_out_channel oc fmt)

let with_temp_file pfx sfx ?contents f =
  let filename = temp_file pfx sfx in
  finally (fun () -> Sys.remove filename)
  @@ fun () ->
  Option.iter
    (fun contents ->
      with_out_channel filename (fun oc -> output_string oc contents))
    contents;
  f filename

let contents filename =
  with_in_channel filename (fun ic ->
      really_input_string ic (in_channel_length ic))

let process_out ?check_exit cmd args =
  let check_exit =
    let default n =
      if n <> 0 then
        Printf.ksprintf failwith "Sub-process %s returned with status %d" cmd n
    in
    Option.value check_exit ~default
  in
  let aargs = Array.of_list (cmd :: args) in
  let ic =
    try Unix.open_process_args_in cmd aargs
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      Printf.ksprintf failwith "ERROR: program %s not found" cmd
  in
  let buf = Buffer.create 4096 in
  finally (fun () ->
      match Unix.close_process_in ic with
      | Unix.WEXITED n -> check_exit n
      | Unix.WSIGNALED n | Unix.WSTOPPED n ->
        Printf.ksprintf failwith "Sub-process %s was killed (%d)" cmd n)
  @@ fun () ->
  try
    while true do
      Buffer.add_channel buf ic 4096
    done;
    assert false
  with End_of_file -> Buffer.contents buf

let check_directory d =
  try
    let d = Unix.realpath d in
    if Sys.is_directory d then Some d else None
  with Unix.Unix_error _ | Sys_error _ -> None

let check_file f =
  try if Sys.is_directory f then None else Some f
  with Unix.Unix_error _ | Sys_error _ -> None

let get_command t =
  String.trim
  @@ process_out
       ~check_exit:(function 0 -> () | _ -> raise Not_found)
       "/bin/sh"
       ["-c"; "command -v " ^ Filename.quote t]

let dir_sep_char = Filename.dir_sep.[0]

let check_exec t =
  try if String.contains t dir_sep_char then Unix.realpath t else get_command t
  with Unix.Unix_error _ | Sys_error _ ->
    Message.raise_error
      "Could not find the @{<yellow>%s@} program, please fix your installation"
      (Filename.quote t)

let ( / ) a b = if a = Filename.current_dir_name then b else Filename.concat a b
let dirname = Filename.dirname
let ( /../ ) a b = dirname a / b
let ( -.- ) file ext = Filename.chop_extension file ^ "." ^ ext

let path_to_list path =
  String.split_on_char dir_sep_char path
  |> List.filter (function "" | "." -> false | _ -> true)

let equal a b =
  String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)

let compare a b =
  String.compare (String.lowercase_ascii a) (String.lowercase_ascii b)

let format ppf t = Format.fprintf ppf "\"@{<cyan>%s@}\"" t

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare
  let format = format
end)

let scan_tree f t =
  let is_dir t =
    try Sys.is_directory t
    with Sys_error _ ->
      Message.emit_debug "Cannot read %s, skipping" t;
      false
  in
  let not_hidden t = match t.[0] with '.' | '_' -> false | _ -> true in
  let rec do_dir d =
    Sys.readdir d
    |> Array.to_list
    |> List.filter not_hidden
    |> List.map (fun t -> d / t)
    |> do_files d
  and do_files d flist =
    let dirs, files =
      flist |> List.sort (fun a b -> -compare a b) |> List.partition is_dir
    in
    let rec gather_subdirs subdirs_list_acc subdirs_seq () =
      match subdirs_seq () with
      | Seq.Nil -> (
        match List.rev subdirs_list_acc, List.filter_map f files with
        | [], [] -> Seq.Nil
        | sdirs, items -> Seq.return (d, sdirs, items) ())
      | Seq.Cons (subdir_name, subdir_next) -> (
        match do_dir subdir_name () with
        | Seq.Nil -> gather_subdirs subdirs_list_acc subdir_next ()
        | Seq.Cons (sd0, sds) ->
          Seq.Cons
            ( sd0,
              Seq.append sds
                (gather_subdirs (subdir_name :: subdirs_list_acc) subdir_next)
            ))
    in
    gather_subdirs [] (List.to_seq dirs)
  in
  if is_dir t then do_dir t else Seq.return (dirname t, [], Option.to_list (f t))

module Tree = struct
  type path = t

  type item = F | D of t
  and t = (path * item) Map.t Lazy.t

  let empty = lazy Map.empty

  let rec build path =
    lazy
      (let entries = try Sys.readdir path with Sys_error _ -> [||] in
       Array.fold_left
         (fun m f ->
           let path = path / f in
           match Sys.is_directory path with
           | true -> Map.add f (path, D (build path)) m
           | false -> Map.add f (path, F) m
           | exception Sys_error _ -> m)
         Map.empty entries)

  let subtree t path =
    let rec aux t = function
      | [] -> t
      | dir :: path -> (
        match Map.find_opt dir (Lazy.force t) with
        | Some (_, D sub) -> aux sub path
        | Some (_, F) | None -> raise Not_found)
    in
    aux t (path_to_list path)

  let lookup t path =
    try
      let t = subtree t (dirname path) in
      match Map.find_opt (Filename.basename path) (Lazy.force t) with
      | Some (path, F) -> Some path
      | Some (_, D _) | None -> None
    with Not_found -> None

  let union t1 t2 =
    lazy (Map.union (fun _ x _ -> Some x) (Lazy.force t1) (Lazy.force t2))
end
