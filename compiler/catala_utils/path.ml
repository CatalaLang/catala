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

(* Pure path/URL helpers parameterized by [~win32] (and [~cwd]) so Windows
   behaviour is testable on any host. Internal: [File]/[Message] expose the
   host-specialised versions. *)

let current = Filename.current_dir_name
let parent = Filename.parent_dir_name
let dir_sep ~win32 = if win32 then "\\" else "/"
let dir_sep_re_win = Re.(compile (set "/\\"))
let dir_sep_re_unix = Re.(compile (char '/'))
let dir_sep_re ~win32 = if win32 then dir_sep_re_win else dir_sep_re_unix
let is_dir_sep ~win32 c = c = '/' || (win32 && (c = '\\' || c = ':'))

let is_relative ~win32 n =
  if win32 then
    (String.length n < 1 || (n.[0] <> '/' && n.[0] <> '\\'))
    && (String.length n < 2 || n.[1] <> ':')
  else String.length n < 1 || n.[0] <> '/'

let concat ~win32 a b =
  let l = String.length a in
  if l = 0 || is_dir_sep ~win32 a.[l - 1] then a ^ b else a ^ dir_sep ~win32 ^ b

let join ~win32 a b =
  if a = current then b
  else if a = "" then dir_sep ~win32 ^ b
  else concat ~win32 a b

let to_list ~win32 path =
  let p = String.re_split_delim (dir_sep_re ~win32) path in
  let drive, p =
    if not win32 then None, p
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

let from_list ~win32 = function
  | None, [] -> current
  | Some drive, [] -> drive
  | drive, [""] -> Option.value drive ~default:"" ^ dir_sep ~win32
  | drive, p1 :: p ->
    Option.value drive ~default:"" ^ List.fold_left (join ~win32) p1 p

(* Removes redundant "." segments, folds ".." when possible (keeping extra
   leading ".."), preserves absolute/relative and the drive/root syntax. Purely
   lexical: no symlink or case resolution. *)
let clean ~win32 p =
  let drive, p = to_list ~win32 p in
  let nup, p =
    List.fold_right
      (fun d (nup, acc) ->
        if d = parent then nup + 1, acc
        else if nup > 0 then nup - 1, acc
        else 0, d :: acc)
      p (0, [])
  in
  let p = List.init nup (fun _ -> parent) @ p in
  from_list ~win32 (drive, p)

let make_absolute ~win32 ~cwd p =
  clean ~win32
  @@
  if is_relative ~win32 p then join ~win32 cwd p
  else if win32 && String.starts_with ~prefix:(dir_sep ~win32) p then
    (* absolute but without drive letter: borrow the cwd's drive *)
    String.sub cwd 0 2 ^ p
  else p

let compat_drives d1 d2 =
  match d1, d2 with
  | Some l1, Some l2 -> String.lowercase_ascii l1 = String.lowercase_ascii l2
  | _ -> true

let remove_prefix ~win32 ~cwd prefix f0 =
  let prefix = make_absolute ~win32 ~cwd prefix in
  let f = make_absolute ~win32 ~cwd f0 in
  (* Windows path comparison is case-insensitive and VS Code lower-cases the
     drive letter, so match the prefix case-insensitively; the suffix keeps
     [f]'s original case. *)
  let n = String.length prefix in
  let matches =
    String.length f >= n
    &&
    let pre = String.sub f 0 n in
    if win32 then
      String.equal (String.lowercase_ascii pre) (String.lowercase_ascii prefix)
    else String.equal pre prefix
  in
  let suf = if matches then String.sub f n (String.length f - n) else f in
  if suf = "" then current
  else if suf <> f && Re.execp ~len:1 (dir_sep_re ~win32) suf then
    String.sub suf 1 (String.length suf - 1)
  else f0

let common_prefix ~win32 ~cwd f1 f2 =
  let rec aux p1 p2 =
    match p1, p2 with
    | d1 :: p1, d2 :: p2 when d1 = d2 -> d1 :: aux p1 p2
    | _ -> []
  in
  let drive1, f1 = to_list ~win32 (make_absolute ~win32 ~cwd f1) in
  let drive2, f2 = to_list ~win32 (make_absolute ~win32 ~cwd f2) in
  if not (compat_drives drive1 drive2) then ""
  else
    match aux f1 f2 with
    | [""] -> "" (* this is the fs root *)
    | pfx -> from_list ~win32 (drive1, pfx)

let make_relative_to ~win32 ~cwd ~dir:dir0 f0 =
  let dir = make_absolute ~win32 ~cwd dir0 in
  let f = make_absolute ~win32 ~cwd f0 in
  let prefix = common_prefix ~win32 ~cwd dir f in
  if prefix = "" then f0
  else
    let dir = remove_prefix ~win32 ~cwd prefix dir in
    let f = remove_prefix ~win32 ~cwd prefix f in
    let ddrive, dlist = to_list ~win32 dir in
    join ~win32 (from_list ~win32 (ddrive, List.map (fun _ -> parent) dlist)) f
    |> clean ~win32

let reverse ~win32 ~cwd ~from_dir ~to_dir f =
  clean ~win32
  @@
  if is_relative ~win32 from_dir then invalid_arg "Path.reverse"
  else
    let f =
      if is_relative ~win32 f then f
      else make_relative_to ~win32 ~cwd ~dir:from_dir f
    in
    let to_dir =
      if is_relative ~win32 to_dir then to_dir
      else make_relative_to ~win32 ~cwd ~dir:from_dir to_dir
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
    let _, frompath = to_list ~win32 from_dir in
    let todrive, topath = to_list ~win32 to_dir in
    let fdrive, fpath = to_list ~win32 f in
    if compat_drives todrive fdrive then
      from_list ~win32 (todrive, aux fpath (List.rev frompath) topath)
    else make_absolute ~win32 ~cwd f

(* Leading slash before "C:" (else "C:" reads as the URL authority); a UNC
   path's server IS the authority, so its two leading slashes collapse to none. *)
let url_of_absolute ~win32 path =
  if not win32 then path
  else
    let p = String.map (function '\\' -> '/' | c -> c) path in
    if String.length p >= 2 && p.[0] = '/' && p.[1] = '/' then
      String.sub p 2 (String.length p - 2)
    else "/" ^ p
