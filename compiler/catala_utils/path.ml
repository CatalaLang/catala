(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2026 Inria,
   contributors: Romain Primet <romain.primet@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(* Pure path/URL helpers. The target OS is read from [win32] (defaulting to the
   host); tests override it to exercise Windows behaviour on any host. Internal:
   [File]/[Message] expose the host-specialised versions. *)

let win32 = ref Sys.win32
let current = Filename.current_dir_name
let parent = Filename.parent_dir_name
let dir_sep () = if !win32 then "\\" else "/"
let dir_sep_re_win = Re.(compile (set "/\\"))
let dir_sep_re_unix = Re.(compile (char '/'))
let dir_sep_re () = if !win32 then dir_sep_re_win else dir_sep_re_unix
let is_dir_sep c = c = '/' || (!win32 && (c = '\\' || c = ':'))

(* Separator between entries of a path-list env var (PATH, PYTHONPATH,
   CLASSPATH). *)
let list_sep () = if !win32 then ";" else ":"

let is_relative n =
  if !win32 then
    (String.length n < 1 || (n.[0] <> '/' && n.[0] <> '\\'))
    && (String.length n < 2 || n.[1] <> ':')
  else String.length n < 1 || n.[0] <> '/'

let concat a b =
  let l = String.length a in
  if l = 0 || is_dir_sep a.[l - 1] then a ^ b else a ^ dir_sep () ^ b

let join a b =
  if a = current then b else if a = "" then dir_sep () ^ b else concat a b

let to_list path =
  let p = String.re_split_delim (dir_sep_re ()) path in
  let drive, p =
    if not !win32 then None, p
    else
      match p with
      | drive :: p when String.length drive >= 2 && drive.[1] = ':' ->
        ( Some (String.sub drive 0 2),
          String.sub drive 2 (String.length drive - 2) :: p )
      | _ -> None, p
  in
  match p with
  | [] | [""] | ["."] -> drive, []
  | p1 :: p ->
    drive, p1 :: List.filter (function "" | "." -> false | _ -> true) p

let from_list = function
  | None, [] -> current
  | Some drive, [] -> drive
  | drive, [""] -> Option.value drive ~default:"" ^ dir_sep ()
  | drive, p1 :: p -> Option.value drive ~default:"" ^ List.fold_left join p1 p

(* Removes redundant "." segments, folds ".." when possible (keeping extra
   leading ".."), preserves absolute/relative and the drive/root syntax. Purely
   lexical: no symlink or case resolution. *)
let clean p =
  let drive, p = to_list p in
  let nup, p =
    List.fold_right
      (fun d (nup, acc) ->
        if d = parent then nup + 1, acc
        else if nup > 0 then nup - 1, acc
        else 0, d :: acc)
      p (0, [])
  in
  let p = List.init nup (fun _ -> parent) @ p in
  from_list (drive, p)

let make_absolute ~cwd p =
  clean
  @@
  if is_relative p then join cwd p
  else if !win32 && String.starts_with ~prefix:(dir_sep ()) p then
    (* absolute but without drive letter: borrow the cwd's drive *)
    String.sub cwd 0 2 ^ p
  else p

let compat_drives d1 d2 =
  match d1, d2 with
  | Some l1, Some l2 -> String.lowercase_ascii l1 = String.lowercase_ascii l2
  | _ -> true

let remove_prefix ~cwd prefix f0 =
  let prefix = make_absolute ~cwd prefix in
  let f = make_absolute ~cwd f0 in
  (* Windows path comparison is case-insensitive and VS Code lower-cases the
     drive letter, so match the prefix case-insensitively; the suffix keeps
     [f]'s original case. *)
  let n = String.length prefix in
  let matches =
    String.length f >= n
    &&
    let pre = String.sub f 0 n in
    if !win32 then
      String.equal (String.lowercase_ascii pre) (String.lowercase_ascii prefix)
    else String.equal pre prefix
  in
  let suf = if matches then String.sub f n (String.length f - n) else f in
  if suf = "" then current
  else if suf <> f && Re.execp ~len:1 (dir_sep_re ()) suf then
    String.sub suf 1 (String.length suf - 1)
  else f0

let common_prefix ~cwd f1 f2 =
  let rec aux p1 p2 =
    match p1, p2 with
    | d1 :: p1, d2 :: p2 when d1 = d2 -> d1 :: aux p1 p2
    | _ -> []
  in
  let drive1, f1 = to_list (make_absolute ~cwd f1) in
  let drive2, f2 = to_list (make_absolute ~cwd f2) in
  if not (compat_drives drive1 drive2) then ""
  else
    match aux f1 f2 with
    | [""] -> "" (* this is the fs root *)
    | pfx -> from_list (drive1, pfx)

let make_relative_to ~cwd ~dir:dir0 f0 =
  let dir = make_absolute ~cwd dir0 in
  let f = make_absolute ~cwd f0 in
  let prefix = common_prefix ~cwd dir f in
  if prefix = "" then f0
  else
    let dir = remove_prefix ~cwd prefix dir in
    let f = remove_prefix ~cwd prefix f in
    let ddrive, dlist = to_list dir in
    join (from_list (ddrive, List.map (fun _ -> parent) dlist)) f |> clean

let reverse ~cwd ~from_dir ~to_dir f =
  clean
  @@
  if is_relative from_dir then invalid_arg "Path.reverse"
  else
    let f =
      if is_relative f then f else make_relative_to ~cwd ~dir:from_dir f
    in
    let to_dir =
      if is_relative to_dir then to_dir
      else make_relative_to ~cwd ~dir:from_dir to_dir
    in
    let rec aux acc rbase = function
      | [] -> acc
      | dir :: p -> (
        if dir = parent then
          match rbase with
          | base1 :: rbase -> aux (base1 :: acc) rbase p
          | [] -> aux acc [] p
        else
          match acc with
          | dir1 :: acc when dir1 = dir -> aux acc rbase p
          | _ -> aux (parent :: acc) rbase p)
    in
    let _, frompath = to_list from_dir in
    let todrive, topath = to_list to_dir in
    let fdrive, fpath = to_list f in
    if compat_drives todrive fdrive then
      from_list (todrive, aux fpath (List.rev frompath) topath)
    else make_absolute ~cwd f

(* Leading slash before "C:" (else "C:" reads as the URL authority); a UNC
   path's server IS the authority, so its two leading slashes collapse to none. *)
let url_of_absolute path =
  if not !win32 then path
  else
    let p = String.map (function '\\' -> '/' | c -> c) path in
    if String.length p >= 2 && p.[0] = '/' && p.[1] = '/' then
      String.sub p 2 (String.length p - 2)
    else "/" ^ p
