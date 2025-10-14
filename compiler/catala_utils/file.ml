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

let format ppf t = Format.fprintf ppf "\"@{<cyan>%s@}\"" t

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

let original_cwd = Sys.getcwd ()
let dir_sep_char = Filename.dir_sep.[0]

let ( / ) a b =
  if a = Filename.current_dir_name then b
  else if a = "" then Filename.dir_sep ^ b
  else Filename.concat a b

let clean_path p =
  let ( / ) a b = if b = "" then a else a / b in
  let nup, p =
    List.fold_right
      (fun d (nup, acc) ->
        if d = Filename.current_dir_name then nup, acc
        else if d = Filename.parent_dir_name then nup + 1, acc
        else if nup > 0 then nup - 1, acc
        else nup, d / acc)
      (String.split_on_char dir_sep_char p)
      (0, "")
  in
  let p =
    if nup = 0 then p
    else
      String.concat Filename.dir_sep
        (List.init nup (fun _ -> Filename.parent_dir_name))
      / p
  in
  if p = "" then "." else p

let make_absolute p =
  clean_path @@ if Filename.is_relative p then Sys.getcwd () / p else p

let remove_prefix prefix f0 =
  let prefix = make_absolute prefix in
  let f = make_absolute f0 in
  let suf = String.remove_prefix ~prefix f in
  if suf = "" then Filename.current_dir_name
  else if suf <> f && suf.[0] = dir_sep_char then
    String.sub suf 1 (String.length suf - 1)
  else f0

let rel_original_cwd () = remove_prefix (Sys.getcwd ()) original_cwd

let temp_file pfx sfx =
  let f = Filename.temp_file pfx sfx in
  if not Global.options.debug then
    at_exit (fun () -> try Sys.remove f with _ -> ());
  f

let rec parent f =
  let base = Filename.basename f in
  if base = Filename.parent_dir_name || base = Filename.current_dir_name then
    parent (Filename.dirname f) / base
  else Filename.dirname f

let exists = Sys.file_exists

let rec ensure_dir dir =
  match Sys.is_directory dir with
  | true -> ()
  | false ->
    Message.error "Directory %a exists but is not a directory" format dir
  | exception Sys_error _ ->
    let pdir = parent dir in
    if pdir <> dir then ensure_dir pdir;
    Sys.mkdir dir
      0o777 (* will be affected by umask, most likely restricted to 0o755 *)

let path_to_list path =
  String.split_on_char dir_sep_char path
  |> List.filter (function "" | "." -> false | _ -> true)

let list_to_path = function
  | [] -> Filename.current_dir_name
  | l -> String.concat Filename.dir_sep l

let common_prefix f1 f2 =
  let rec aux p1 p2 =
    match p1, p2 with
    | d1 :: p1, d2 :: p2 when d1 = d2 -> d1 :: aux p1 p2
    | _ -> []
  in
  "" :: aux (path_to_list (make_absolute f1)) (path_to_list (make_absolute f2))
  |> list_to_path

let make_relative_to ~dir:dir0 f0 =
  let dir = make_absolute dir0 in
  let f = make_absolute f0 in
  let prefix = common_prefix dir f in
  let dir = remove_prefix prefix dir in
  let f = remove_prefix prefix f in
  list_to_path (List.map (fun _ -> Filename.parent_dir_name) (path_to_list dir))
  / f
  |> clean_path

let reverse_path ?(from_dir = Sys.getcwd ()) ~to_dir f =
  clean_path
  @@
  if Filename.is_relative from_dir then invalid_arg "File.reverse_path"
  else
    let f =
      if Filename.is_relative f then f else make_relative_to ~dir:from_dir f
    in
    let to_dir =
      if Filename.is_relative to_dir then to_dir
      else make_relative_to ~dir:from_dir to_dir
    in
    let rec aux acc rbase = function
      | [] -> acc
      | dir :: p -> (
        if dir = Filename.parent_dir_name then
          match rbase with
          | base1 :: rbase -> aux (base1 :: acc) rbase p
          | [] -> aux acc [] p
        else
          match acc with
          | dir1 :: acc when dir1 = dir -> aux acc rbase p
          | _ -> aux (Filename.parent_dir_name :: acc) rbase p)
    in
    let rbase = List.rev (path_to_list from_dir) in
    list_to_path (aux (path_to_list f) rbase (path_to_list to_dir))

let find_in_parents ?cwd predicate =
  let cwd = match cwd with None -> Sys.getcwd () | Some cwd -> cwd in
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  let rec lookup dir rel =
    match predicate dir with
    | true -> Some dir, rel
    | exception (Unix.Unix_error _ | Sys_error _) ->
      None, Filename.current_dir_name
    | false when dir = home -> None, Filename.current_dir_name
    | false ->
      let parent = Filename.dirname dir in
      if parent = dir then None, Filename.current_dir_name
      else lookup parent (rel / Filename.parent_dir_name)
  in
  match lookup cwd Filename.current_dir_name with
  | Some dir, rel -> Some (dir, rel)
  | None, _ -> None

let with_out_channel filename f =
  ensure_dir (Filename.dirname filename);
  let oc = open_out_bin filename in
  finally (fun () -> close_out oc) (fun () -> f oc)

let with_in_channel filename f =
  let oc = open_in_bin filename in
  finally (fun () -> close_in oc) (fun () -> f oc)

let with_formatter_of_out_channel ?nocolor oc f =
  let fmt = Message.formatter_of_out_channel ?nocolor oc () in
  finally (fun () -> Format.pp_print_flush fmt ()) @@ fun () -> f fmt

let with_formatter_of_file filename f =
  with_out_channel filename (fun oc ->
      with_formatter_of_out_channel ~nocolor:true oc f)

let with_formatter_of_opt_file filename_opt f =
  match filename_opt with
  | None -> finally (fun () -> flush stdout) (fun () -> f Format.std_formatter)
  | Some filename -> with_formatter_of_file filename f

let ( -.- ) file ext =
  let base = Filename.remove_extension file in
  match ext with "" -> base | ext -> base ^ "." ^ ext

let get_main_out_channel ~source_file ~output_file ?ext () =
  match output_file, ext with
  | Some "-", _ | None, None -> None, fun f -> f stdout
  | Some f, _ -> Some f, with_out_channel f
  | None, Some ext ->
    let src = Global.input_src_file source_file in
    let f = src -.- ext in
    Some f, with_out_channel f

let get_main_out_formatter ~source_file ~output_file ?ext () =
  let f, with_ = get_main_out_channel ~source_file ~output_file ?ext () in
  let nocolor = match output_file with Some "-" | None -> false | _ -> true in
  f, fun fmt -> with_ (fun oc -> with_formatter_of_out_channel ~nocolor oc fmt)

let with_secondary_out_channel ~output_file ~ext f =
  match output_file with
  | None | Some "-" ->
    Message.debug
      "Ignoring secondary %a output to since main output is to stdout" format
      ext;
    f None (Format.make_formatter (fun _ _ _ -> ()) ignore)
  | Some file ->
    let file =
      match ext.[0] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> file -.- ext
      | _ -> Filename.remove_extension file ^ ext
    in
    Message.debug "Secondary output to %a" format file;
    with_formatter_of_file file (fun ppf -> f (Some file) ppf)

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

let copy ~src ~dst =
  let chunk = 4096 in
  let buf = Bytes.create chunk in
  let ic = open_in_bin src in
  let oc = open_out_bin dst in
  let rec loop () =
    match input ic buf 0 chunk with
    | 0 -> ()
    | n ->
      output oc buf 0 n;
      loop ()
  in
  Fun.protect
    ~finally:(fun () ->
      close_out oc;
      close_in ic)
    loop

let copy_in ~src ~dir = copy ~src ~dst:(dir / Filename.basename src)

let newer f1 f2 =
  let open Unix in
  try (stat f1).st_mtime > (stat f2).st_mtime with Unix.Unix_error _ -> true

let rec copy_dir
    ?(filter = fun _ -> true)
    ?(newer_only = false)
    ~src
    ~dst:dst0
    () =
  Array.iter
    (fun base ->
      let src = src / base and dst = dst0 / base in
      if Sys.is_directory src then copy_dir ~filter ~src ~dst ()
      else if filter base then (
        ensure_dir dst0;
        if (not newer_only) || newer src dst then copy ~src ~dst))
    (Sys.readdir src)

let rec remove t =
  match Unix.lstat t with
  (* Don't use Sys.file_exists or Sys.is_directory here, they would follow
     symlinks *)
  | { Unix.st_kind = Unix.S_DIR; _ } ->
    (* directory *)
    let files = Sys.readdir t in
    Array.iter (fun f -> remove (t / f)) files;
    Sys.rmdir t
  | _ ->
    (* anything else, including symlinks *)
    Sys.remove t
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
    (* already does not exist *)
    ()

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

(* SIDE EFFECT AT MODULE LOAD: sets up a signal handler on SIGWINCH (window
   resize) *)
let () =
  let default = 80 in
  let get_terminal_cols () =
    let from_env () =
      try int_of_string (Sys.getenv "COLUMNS") with Not_found | Failure _ -> 0
    in
    let count =
      if not Unix.(isatty stdin) then from_env ()
      else if Sys.win32 then default
      else
        try
          (* terminfo *)
          process_out "tput" ["cols"] |> String.trim |> int_of_string
        with Failure _ -> (
          try
            (* stty *)
            process_out "stty" ["size"]
            |> String.trim
            |> fun s ->
            let i = String.rindex s ' ' + 1 in
            String.sub s i (String.length s - i) |> int_of_string
          with Failure _ | Not_found | Invalid_argument _ -> from_env ())
    in
    if count > 0 then count else default
  in
  let width = ref None in
  let () =
    try
      Sys.set_signal 28 (* SIGWINCH *)
        (Sys.Signal_handle (fun _ -> width := None))
    with Invalid_argument _ -> ()
  in
  Message.set_terminal_width_function (fun () ->
      match !width with
      | Some n -> n
      | None ->
        let r = get_terminal_cols () in
        width := Some r;
        r)

let check_directory d =
  try
    let d = Unix.realpath d in
    if Sys.is_directory d then Some d else None
  with Unix.Unix_error _ | Sys_error _ -> None

let check_file f =
  try if Sys.is_directory f then None else Some f
  with Unix.Unix_error _ | Sys_error _ -> None

let get_command t =
  let cmd, args =
    if Sys.win32 then
      let t_exe =
        if not (Filename.check_suffix t ".exe") then t ^ ".exe" else t
      in
      "where.exe", [t_exe]
    else "/bin/sh", ["-c"; "command -v " ^ Filename.quote t]
  in
  String.trim
  @@ process_out
       ~check_exit:(function 0 -> () | _ -> raise Not_found)
       cmd args

let check_exec t =
  try
    if String.contains t dir_sep_char then Unix.realpath t else get_command t
  with
  | Unix.Unix_error _ | Sys_error _ ->
    Message.error
      "Could not find the @{<yellow>%s@} program, please fix your installation."
      (Filename.quote t)
  | Not_found ->
    Message.error
      "@{<yellow>%s@} is not a valid executable, it cannot be used by clerk."
      (Filename.quote t)

let dirname = Filename.dirname
let basename = Filename.basename
let extension t = String.remove_prefix ~prefix:"." (Filename.extension t)
let ( /../ ) a b = parent a / b

let equal a b =
  String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)

let compare a b =
  String.compare (String.lowercase_ascii a) (String.lowercase_ascii b)

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
      Message.debug "Cannot read %s, skipping" t;
      false
  in
  let not_hidden t = match t.[0] with '.' | '_' -> false | _ -> true in
  let rec do_dir d =
    (try Sys.readdir d
     with Sys_error _ ->
       Message.debug "Cannot read %s, skipping" t;
       [||])
    |> Array.to_list
    |> List.filter not_hidden
    |> List.map (fun t -> d / t)
    |> do_files d
  and do_files d flist =
    let dirs, files =
      flist |> List.sort (fun a b -> compare a b) |> List.partition is_dir
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
      let fname = String.to_id (Filename.basename path) in
      let matches =
        Map.filter_map
          (fun s m ->
            match equal (String.to_id s) fname, m with
            | true, (path, F) -> Some path
            | _ -> None)
          (Lazy.force t)
      in
      match Map.cardinal matches with
      | 0 -> None
      | 1 -> Some (snd (Map.choose matches))
      | _ ->
        Message.error
          "Multiple files match the same module name:@ @[<v>%a@]@,\
           @{<bold>Hint:@} Rename your modules to avoid conflicts. You may \
           need to run `clerk clean`"
          (Format.pp_print_list format)
          (List.map fst (Map.bindings matches))
    with Not_found -> None

  let union t1 t2 =
    lazy (Map.union (fun _ x _ -> Some x) (Lazy.force t1) (Lazy.force t2))
end
