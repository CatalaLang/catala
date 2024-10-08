(* This file is part of the Catala build system, a specification language for
   tax and social benefits computation rules. Copyright (C) 2024 Inria,
   contributors: Louis Gesbert <louis.gesbert@inria.fr>

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
module S = Set.Make (String)
module M = Map.Make (String)

type _ descr =
  | String : string descr
  | List : 'a descr -> 'a list descr
  | Union : { cases : 'a case list } -> 'a descr
  | Tup : 'a descr -> 'a descr
  | Tups : 'a descr * 'b descr -> ('a * 'b) descr
  | Conv : { proj : 'a -> 'b; inj : 'b -> 'a; descr : 'b descr } -> 'a descr
  | Obj : 'a field -> 'a descr
  | Objs : 'a descr * 'b descr -> ('a * 'b) descr

and _ field =
  | Req : { name : string; descr : 'a descr } -> 'a field
  | Opt : { name : string; descr : 'a descr } -> 'a option field
  | Dft : { name : string; descr : 'a descr; default : 'a } -> 'a field

and _ case =
  | Case : {
      descr : 'a descr;
      name : string;
      proj : 't -> 'a option;
      inj : 'a -> 't option;
    }
      -> 't case

and _ table_descr =
  | Table_req : { name : string; descr : 'a descr } -> 'a table_descr
  | Table_opt : { name : string; descr : 'a descr } -> 'a option table_descr
  | Multi_table : { name : string; descr : 'a descr } -> 'a list table_descr
  | Tables : 'a table_descr * 'b table_descr -> ('a * 'b) table_descr
  | ConvT : {
      proj : 'a -> 'b;
      inj : 'b -> 'a;
      descr : 'b table_descr;
    }
      -> 'a table_descr

type 'a t = 'a table_descr

let string = String
let list d = List d

(* TODO: also provide tuples constructors *)
let pair a b = Tups (Tup a, Tup b)

let merge_objs l r =
  let rec check_name_clash : type a. S.t -> a descr -> S.t =
   fun acc -> function
    | String | Tup _ | Tups _ | List _ | Union _ -> acc
    | Conv { descr; _ } -> check_name_clash acc descr
    | Objs (l, r) -> (
      let l = check_name_clash S.empty l in
      let r = check_name_clash S.empty r in
      if S.disjoint l r then S.union l r
      else
        let inter = S.inter l r in
        try
          Message.error
            "Invalid TOML encoding: different fields have an identical name %a."
            Format.(
              pp_print_list
                ~pp_sep:(fun fmt () -> fprintf fmt ", ")
                (fun fmt s -> fprintf fmt "@{<red>'%s'@}" s))
            (S.elements inter)
        with Message.CompilerError content ->
          (* This is triggered when the descriptor gets evaluated, it's not
             caught under the driver's main loop: we display it directly as it
             should be unreachable in a release. *)
          Message.Content.emit content Error;
          exit Cmdliner.Cmd.Exit.some_error)
    | Obj (Req { name; _ }) | Obj (Opt { name; _ }) | Obj (Dft { name; _ }) ->
      S.add name acc
  in
  ignore @@ check_name_clash S.empty (Objs (l, r));
  Objs (l, r)

let obj1 f1 = Obj f1
let obj2 f2 f1 = merge_objs (obj1 f2) (obj1 f1)
let obj3 f3 f2 f1 = merge_objs (obj1 f3) (obj2 f2 f1)
let obj4 f4 f3 f2 f1 = merge_objs (obj2 f4 f3) (obj2 f2 f1)
let obj5 f5 f4 f3 f2 f1 = merge_objs (obj1 f5) (obj4 f4 f3 f2 f1)

let merge_tables l r =
  let rec check_name_clash : type a. S.t -> a table_descr -> S.t =
   fun acc -> function
    | ConvT { descr; _ } -> check_name_clash acc descr
    | Tables (l, r) -> (
      let l = check_name_clash S.empty l in
      let r = check_name_clash S.empty r in
      if S.disjoint l r then S.union l r
      else
        let inter = S.inter l r in
        try
          Message.error
            "Invalid TOML encoding: different tables have an identical name %a."
            Format.(
              pp_print_list
                ~pp_sep:(fun fmt () -> fprintf fmt ", ")
                (fun fmt s -> fprintf fmt "@{<red>'%s'@}" s))
            (S.elements inter)
        with Message.CompilerError content ->
          (* This is triggered when the descriptor gets evaluated, it's not
             caught under the driver's main loop: we display it directly as it
             should be unreachable in a release. *)
          Message.Content.emit content Error;
          exit Cmdliner.Cmd.Exit.some_error)
    | Table_req { name; _ } | Table_opt { name; _ } | Multi_table { name; _ } ->
      S.add name acc
  in
  ignore @@ check_name_clash S.empty (Tables (l, r));
  Tables (l, r)

let table_req ~name descr = Table_req { name; descr }
let table_opt ~name descr = Table_opt { name; descr }
let multi_table ~name descr = Multi_table { name; descr }
let table2 f2 f1 = merge_tables f2 f1
let table3 f3 f2 f1 = merge_tables f3 (table2 f2 f1)
let table4 f4 f3 f2 f1 = merge_tables (table2 f4 f3) (table2 f2 f1)
let table5 f5 f4 f3 f2 f1 = merge_tables f5 (table4 f4 f3 f2 f1)
let conv proj inj descr = Conv { proj; inj; descr }
let conv3 ty = conv (fun (c, b, a) -> c, (b, a)) (fun (c, (b, a)) -> c, b, a) ty

let conv4 ty =
  conv
    (fun (d, c, b, a) -> (d, c), (b, a))
    (fun ((d, c), (b, a)) -> d, c, b, a)
    ty

let conv5 ty =
  conv
    (fun (e, d, c, b, a) -> e, ((d, c), (b, a)))
    (fun (e, ((d, c), (b, a))) -> e, d, c, b, a)
    ty

let obj3 f3 f2 f1 = conv3 (obj3 f3 f2 f1)
let obj4 f4 f3 f2 f1 = conv4 (obj4 f4 f3 f2 f1)
let obj5 f5 f4 f3 f2 f1 = conv5 (obj5 f5 f4 f3 f2 f1)
let convt proj inj descr = ConvT { proj; inj; descr }

let convt3 ty =
  convt (fun (c, b, a) -> c, (b, a)) (fun (c, (b, a)) -> c, b, a) ty

let convt4 ty =
  convt
    (fun (d, c, b, a) -> (d, c), (b, a))
    (fun ((d, c), (b, a)) -> d, c, b, a)
    ty

let convt5 ty =
  convt
    (fun (e, d, c, b, a) -> e, ((d, c), (b, a)))
    (fun (e, ((d, c), (b, a))) -> e, d, c, b, a)
    ty

let table3 f3 f2 f1 = convt3 (table3 f3 f2 f1)
let table4 f4 f3 f2 f1 = convt4 (table4 f4 f3 f2 f1)
let table5 f5 f4 f3 f2 f1 = convt5 (table5 f5 f4 f3 f2 f1)
let req_field ~name descr = Req { name; descr }
let opt_field ~name descr = Opt { name; descr }
let dft_field ~name ~default descr = Dft { name; default; descr }
let case ~info ~proj ~inj descr = Case { name = info; proj; inj; descr }

let string_cases (l : (string * 'a) list) =
  List.map
    (fun (str, value) ->
      Case
        {
          descr = String;
          name = str;
          proj = (fun v -> if v = value then Some str else None);
          inj = (fun s -> if s <> str then None else Some value);
        })
    l

let proj_empty_list = function [] -> None | l -> Some l
let inj_empty_list = function None -> [] | Some l -> l
let union cases = Union { cases }

let rec tups_depth : type a. a descr -> int = function
  | Tups (l, r) -> tups_depth l + tups_depth r
  | _ -> 1

let split_list l n =
  let rec loop left n l =
    match l with
    | [] -> List.rev left, []
    | h :: t when n > 0 -> loop (h :: left) (pred n) t
    | _ -> List.rev left, l
  in
  loop [] n l

let pp_toml_type fmt toml =
  Format.pp_print_string fmt
  @@
  match toml with
  | Otoml.TomlString _ -> "a string"
  | TomlInteger _ -> "an integer"
  | TomlFloat _ -> "a float"
  | TomlBoolean _ -> "a boolean"
  | TomlOffsetDateTime _ -> "an offsetdatetime"
  | TomlLocalDateTime _ -> "a localdatetime"
  | TomlLocalDate _ -> "a localdate"
  | TomlLocalTime _ -> "a localtime"
  | TomlTableArray _ | TomlArray _ -> "an array"
  | TomlInlineTable _ | TomlTable _ -> "a table"

type target_kind = Table of string | MTable of string

type scope = {
  target : target_kind;
  name : string option;
  rev_keys : string list;
}

let pp_target fmt (target_kind, name) =
  let open Format in
  (match target_kind with
  | Table tname -> fprintf fmt "@{<bold>[%s]@}" tname
  | MTable tname -> fprintf fmt "@{<bold>[[%s]]@}" tname);
  Option.iter (fun name -> fprintf fmt " @{<yellow>%s@}" name) name

let pp_scope fmt scope =
  let open Format in
  match scope with
  | { target; name; rev_keys = [] } ->
    fprintf fmt "in table %a" pp_target (target, name)
  | { target; name; rev_keys } ->
    fprintf fmt "in table %a at key @{<yellow>%a@}" pp_target (target, name)
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ".") pp_print_string)
      (List.rev rev_keys)

let key_names (descr : _ descr) : S.t =
  let rec loop : type a. S.t -> a descr -> S.t =
   fun acc -> function
    | Objs (l, r) -> S.union (loop acc l) (loop S.empty r)
    | Obj (Req { name; _ }) | Obj (Dft { name; _ }) | Obj (Opt { name; _ }) ->
      S.add name acc
    | _ -> acc
  in
  loop S.empty descr

let error scope ?found pp =
  match found with
  | None ->
    Message.error "While parsing the TOML configuration file, %a:@\n%t."
      pp_scope scope pp
  | Some toml ->
    Message.error
      "While parsing the TOML configuration file, %a:@\nParsed @{<red>%a@}, %t."
      pp_scope scope pp_toml_type toml pp

let check_obj scope bindings descr =
  let valid_keys = key_names descr in
  let given_keys = List.map fst bindings |> S.of_list in
  let diff = S.diff given_keys valid_keys in
  if not (S.is_empty diff) then
    error scope (fun fmt ->
        Format.fprintf fmt
          "Detected invalid keys present in table: %a.@\nAllowed keys are: %a"
          Format.(
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt ", ")
              (fun fmt s -> Format.fprintf fmt "@{<red>%s@}" s))
          (S.elements diff)
          Format.(
            pp_print_list
              ~pp_sep:(fun fmt () -> fprintf fmt ", ")
              pp_print_string)
          (S.elements valid_keys))

let decode_descr (target : target_kind) toml descr =
  let open Otoml in
  let open Format in
  let rec loop :
      type a. ?first_obj:bool -> scope:scope -> Otoml.t -> a descr -> a =
   fun ?(first_obj = true) ~scope toml (descr as current) ->
    match toml, descr with
    | _, Union { cases } -> (
      match
        List.find_map
          (fun (Case { descr; inj; _ }) ->
            try inj @@ loop ~first_obj ~scope toml descr
            with Catala_utils.Message.CompilerError _ -> None)
          cases
      with
      | None ->
        error scope (fun fmt ->
            fprintf fmt
              "@[<v 2>the provided value do not match any of the possible \
               cases:@ %a@]"
              (pp_print_list ~pp_sep:pp_print_cut (fun fmt s ->
                   fprintf fmt "- %s" s))
              (List.map (fun (Case case) -> case.name) cases))
      | Some x -> x)
    | TomlString s, String -> s
    | _, String ->
      error ~found:toml scope (fun fmt ->
          fprintf fmt "expected @{<bold>a string@}")
    | TomlArray l, List descr -> List.map (fun x -> loop ~scope x descr) l
    | toml, Conv { inj; descr; _ } -> inj (loop ~scope toml descr)
    | TomlArray [x], Tup d -> loop ~scope x d
    | TomlArray arr, Tups (l, r) ->
      let sub_left, sub_right = split_list arr (tups_depth l) in
      loop ~scope (TomlArray sub_left) l, loop ~scope (TomlArray sub_right) r
    | _, List _ | _, Tup _ | _, Tups _ ->
      error ~found:toml scope (fun fmt ->
          fprintf fmt "expected @{<bold>an array of values@}")
    | TomlTable bindings, Obj (Req { name; descr }) -> (
      if first_obj then check_obj scope bindings current;
      match List.assoc_opt name bindings with
      | Some b ->
        loop ~scope:{ scope with rev_keys = name :: scope.rev_keys } b descr
      | None ->
        error scope (fun fmt ->
            fprintf fmt "the required key @{<yellow>%s@} is missing" name))
    | TomlTable bindings, Obj (Opt { name; descr }) -> (
      if first_obj then check_obj scope bindings current;
      match List.assoc_opt name bindings with
      | Some b ->
        Some
          (loop ~scope:{ scope with rev_keys = name :: scope.rev_keys } b descr)
      | None -> None)
    | TomlTable bindings, Obj (Dft { name; descr; default }) -> (
      if first_obj then check_obj scope bindings current;
      match List.assoc_opt name bindings with
      | Some b ->
        loop ~scope:{ scope with rev_keys = name :: scope.rev_keys } b descr
      | None -> default)
    | TomlTable bindings, Objs (l, r) ->
      if first_obj then check_obj scope bindings current;
      loop ~first_obj:false ~scope toml l, loop ~first_obj:false ~scope toml r
    | _, Obj _ | _, Objs _ ->
      error ~found:toml scope (fun fmt ->
          fprintf fmt "expected a @{<bold><key> = <value> table@}")
  in
  let name =
    match toml with
    | TomlTable bindings -> (
      match List.assoc_opt "name" bindings with
      | Some (TomlString n) -> Some n
      | _ -> None)
    | _ -> None
  in
  loop ~scope:{ target; name; rev_keys = [] } toml descr

let table_names (tables_descr : _ table_descr) : string list * S.t =
  let rec loop : type a. string list * S.t -> a table_descr -> string list * S.t
      =
   fun ((t, mt) as acc) -> function
    | Table_req { name; _ } -> name :: t, mt
    | Table_opt { name; _ } -> name :: t, mt
    | Multi_table { name; _ } -> t, S.add name mt
    | ConvT { descr; _ } -> loop acc descr
    | Tables (l, r) ->
      let acc = loop acc l in
      loop acc r
  in
  loop ([], S.empty) tables_descr

let check_table_consistency (toml : Otoml.t) (table_descr : _ table_descr) :
    unit =
  let tables, mtables = table_names table_descr in
  let pp_valids ~is_table fmt =
    let open Format in
    let l = List.rev (if is_table then tables else S.elements mtables) in
    let pp_table fmt s =
      if is_table then fprintf fmt "@{<yellow>[%s]@}" s
      else fprintf fmt "@{<yellow>[[%s]]@}" s
    in
    fprintf fmt "@\nThose are valid at this point: %a."
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_table)
      l
  in
  match toml with
  | TomlTable bindings ->
    List.iter
      (fun (key, value) ->
        match value with
        | Otoml.TomlTable _ when not (List.mem key tables) ->
          Message.error
            "While parsing the TOML configuration file: found an unexpected \
             table @{<red>[%s]@}.%t"
            key (pp_valids ~is_table:true)
        | TomlTableArray _ when not (S.mem key mtables) ->
          Message.error
            "While parsing the TOML configuration file: found an unexpected \
             table @{<red>[[%s]]@}.%t"
            key
            (pp_valids ~is_table:false)
        | TomlTable _ | TomlTableArray _ -> ()
        | _ ->
          Message.error
            "While parsing the TOML configuration file: found an unexpected \
             key @{<red>%s@}."
            key)
      bindings
  | _ ->
    Message.error
      "While parsing the TOML configuration file: no toplevel table found."

let decode (toml : Otoml.t) (descr : _ table_descr) : 'a =
  let open Otoml in
  check_table_consistency toml descr;
  let rec decode_tables : type a. (string * Otoml.t) list -> a table_descr -> a
      =
   fun otables (tables_descr : _ table_descr) ->
    let open Otoml in
    match tables_descr with
    | Multi_table { name; descr } -> (
      List.find_map
        (function
          | name', TomlTableArray l ->
            if name = name' then
              Some (List.map (fun l -> decode_descr (MTable name) l descr) l)
            else None
          | _ -> None)
        otables
      |> function None -> [] | Some l -> l)
    | Table_opt { name; descr } ->
      List.find_map
        (function
          | name', TomlTable x ->
            if name = name' then
              Some
                (decode_descr (Table name) (TomlTable x)
                   (* keep the table toml structure *) descr)
            else None
          | _ -> None)
        otables
    | Table_req { name; descr } -> (
      let res =
        List.find_map
          (function
            | name', TomlTable x ->
              if name = name' then
                Some
                  (decode_descr (Table name) (TomlTable x)
                     (* keep the table toml structure *) descr)
              else None
            | _ -> None)
          otables
      in
      match res with
      | Some x -> x
      | None ->
        Message.error
          "While parsing the TOML configuration file: the expected  \
           @{<yellow>[%s]@} table is missing."
          name)
    | Tables (l, r) -> decode_tables otables l, decode_tables otables r
    | ConvT { descr; inj; _ } -> inj @@ decode_tables otables descr
  in
  let tables =
    match toml with
    | TomlTable l -> l
    | _ -> (* ensured by the consistency check *) assert false
  in
  decode_tables tables descr

let rec encode_descr : type a. a -> a descr -> Otoml.t =
 fun v -> function
  | String -> TomlString v
  | List descr -> TomlArray (List.map (fun v -> encode_descr v descr) v)
  | Union { cases } -> (
    List.find_map
      (function
        | Case { proj; descr; _ } ->
          Option.map (fun v -> encode_descr v descr) (proj v))
      cases
    |> function None -> assert false | Some v -> v)
  | Tup t -> TomlArray [encode_descr v t]
  | Tups (l, r) -> (
    let left, right = v in
    match encode_descr left l, encode_descr right r with
    | TomlArray l, TomlArray r -> TomlArray (l @ r)
    | _ -> assert false)
  | Conv { proj; descr; _ } -> encode_descr (proj v) descr
  | Obj (Req { name; descr }) -> TomlTable [name, encode_descr v descr]
  | Obj (Dft { name; descr; _ }) -> TomlTable [name, encode_descr v descr]
  | Obj (Opt { name; descr; _ }) -> (
    match v with
    | None -> TomlTable []
    | Some v -> TomlTable [name, encode_descr v descr])
  | Objs (l, r) -> (
    let left, right = v in
    match encode_descr left l, encode_descr right r with
    | TomlTable l, TomlTable r -> TomlTable (l @ r)
    | _ -> assert false)

let encode v table_descr : Otoml.t =
  let open Otoml in
  let rec encode_table :
      type a.
      Otoml.t M.t * Otoml.t M.t ->
      a ->
      a table_descr ->
      Otoml.t M.t * Otoml.t M.t =
   fun ((t, mt) as acc) v table_descr ->
    match table_descr with
    | ConvT { proj; descr; _ } -> encode_table acc (proj v) descr
    | Tables (l, r) ->
      let vl, vr = v in
      let acc = encode_table acc vl l in
      encode_table acc vr r
    | Table_opt { name; descr } -> (
      match v with
      | None -> acc
      | Some v -> M.add name (encode_descr v descr) t, mt)
    | Table_req { name; descr } -> M.add name (encode_descr v descr) t, mt
    | Multi_table { name; descr } ->
      ( t,
        M.add name
          (TomlTableArray (List.map (fun v -> encode_descr v descr) v))
          mt )
  in
  let tables, mtables = encode_table (M.empty, M.empty) v table_descr in
  TomlTable (M.bindings tables @ M.bindings mtables)
