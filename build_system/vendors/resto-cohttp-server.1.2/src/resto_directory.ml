(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(*  Copyright (C) 2016, OCamlPro.                                            *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Resto

let map_option f = function None -> None | Some x -> Some (f x)

let ( >>= ) = Lwt.bind

let ( >|= ) = Lwt.( >|= )

let ( >>? ) = Result.bind

let ( >>=? ) = Lwt_result.bind

module Answer = struct
  (** Return type for service handler *)
  type ('o, 'e) t =
    [ `Ok of 'o (* 200 *)
    | `OkChunk of 'o (* 200 *)
    | `OkStream of 'o stream (* 200 *)
    | `Created of string option (* 201 *)
    | `No_content (* 204 *)
    | `Unauthorized of 'e option (* 401 *)
    | `Forbidden of 'e option (* 403 *)
    | `Not_found of 'e option (* 404 *)
    | `Conflict of 'e option (* 409 *)
    | `Gone of 'e option (* 410 *)
    | `Error of 'e option (* 500 *) ]

  and 'a stream = {next : unit -> 'a option Lwt.t; shutdown : unit -> unit}

  let return x = Lwt.return (`Ok x)

  let return_stream x = Lwt.return (`OkStream x)
end

module Make (Encoding : ENCODING) = struct
  module Service = Resto.MakeService (Encoding)

  module Curry = struct
    type (_, _, _, _, _, _) conv =
      | Z : (unit, 'g, 'g, unit, 'f, 'f) conv
      | S :
          ('t, 'g, 'b * 's, 'rt, 'f, 'r) conv
          -> ('t * 'b, 'g, 's, 'a * 'rt, 'a -> 'f, 'r) conv

    let reverse : type a c d e f. (a, c, unit, d, e, f) conv -> a -> c =
     fun c v ->
      let rec reverse : type a c d e f g. (a, c, d, e, f, g) conv -> a -> d -> c
          =
       fun c v acc ->
        match (c, v) with Z, _ -> acc | S c, (v, x) -> reverse c v (x, acc)
      in
      reverse c v ()

    let rec curry : type a b c d e f. (a, b, c, d, e, f) conv -> e -> d -> f =
     fun c f ->
      match c with Z -> fun () -> f | S c -> fun (v, x) -> curry c (f v) x

    let curry c f =
      let f = curry c f in
      fun x -> f (reverse c x)
  end

  type step =
    | Static of string
    | Dynamic of Arg.descr
    | DynamicTail of Arg.descr

  type conflict =
    | CService of meth
    | CDir
    | CBuilder
    | CDynDescr of string * string
    | CTail
    | CTypes of Arg.descr * Arg.descr
    | CType of Arg.descr * string list

  exception Conflict of step list * conflict

  open Resto.Internal

  type lookup_error =
    [ `Not_found (* 404 *)
    | `Method_not_allowed of meth list (* 405 *)
    | `Cannot_parse_path of string list * Arg.descr * string (* 400 *) ]

  type ('query, 'input, 'output, 'error) types =
        ('query, 'input, 'output, 'error) Service.Internal.types = {
    query : 'query Resto.Query.t;
    input : 'input Service.input;
    output : 'output Encoding.t;
    error : 'error Encoding.t;
  }

  type 'key t =
    | Empty : 'key t
    | Static : 'key static_directory -> 'key t
    | Dynamic : string option * ('key -> 'key directory Lwt.t) -> 'key t
    | DynamicTail : 'a arg * ('key * 'a list) t -> 'key t

  and 'key directory = 'key t

  and 'key static_directory = {
    services : 'key registered_service_builder MethMap.t;
    subdirs : 'key static_subdirectories option;
  }

  and _ static_subdirectories =
    | Suffixes : 'key directory StringMap.t -> 'key static_subdirectories
    | Arg :
        'a Resto.Internal.arg * ('key * 'a) directory
        -> 'key static_subdirectories

  and registered_service =
    | Service : {
        types : ('q, 'i, 'o, 'e) types;
        handler : 'q -> 'i -> ('o, 'e) Answer.t Lwt.t;
      }
        -> registered_service

  and 'key registered_service_builder = {
    meth : Resto.meth;
    description : Encoding.schema Description.service;
    builder : 'key -> registered_service Lwt.t;
  }

  let empty = Empty

  let rec map_directory : type a b. (a -> b Lwt.t) -> b directory -> a directory
      =
   fun f t ->
    match t with
    | Empty -> Empty
    | Dynamic (descr, builder) ->
        let builder a = f a >>= builder >|= map_directory f in
        Dynamic (descr, builder)
    | DynamicTail (arg, dir) ->
        DynamicTail
          (arg, map_directory (fun (x, l) -> f x >|= fun x -> (x, l)) dir)
    | Static dir -> Static (map_static_directory f dir)

  and map_static_directory :
      type a b. (a -> b Lwt.t) -> b static_directory -> a static_directory =
   fun f t ->
    {
      services = MethMap.map (map_registered_service f) t.services;
      subdirs = map_option (map_static_subdirectories f) t.subdirs;
    }

  and map_static_subdirectories :
      type a b.
      (a -> b Lwt.t) -> b static_subdirectories -> a static_subdirectories =
   fun f t ->
    match t with
    | Suffixes map -> Suffixes (StringMap.map (map_directory f) map)
    | Arg (arg, dir) ->
        let dir = map_directory (fun (a, x) -> f a >|= fun a -> (a, x)) dir in
        Arg (arg, dir)

  and map_registered_service :
      type a b.
      (a -> b Lwt.t) ->
      b registered_service_builder ->
      a registered_service_builder =
   fun f rs -> {rs with builder = (fun p -> f p >>= fun p -> rs.builder p)}

  let map = map_directory

  let prefix : type p pr. (pr, p) Path.path -> p directory -> pr directory =
   fun path dir ->
    let rec prefix :
        type k pr. (pr, k) Resto.Internal.path -> k directory -> pr directory =
     fun path dir ->
      match path with
      | Root -> dir
      | Static (path, name) ->
          let subdirs = Suffixes (StringMap.singleton name dir) in
          prefix
            path
            (Static {subdirs = Some subdirs; services = MethMap.empty})
      | Dynamic (path, arg) ->
          let subdirs = Arg (arg, dir) in
          prefix
            path
            (Static {subdirs = Some subdirs; services = MethMap.empty})
      | DynamicTail _ -> invalid_arg "RestoDirectory.prefix"
    in
    prefix (Resto.Internal.to_path path) dir

  let conflict steps kind = raise (Conflict (steps, kind))

  (*********)
  (* Merge *)
  (*********)

  let rec merge_loop :
      type p.
      [`Raise | `Pick_left | `Pick_right] ->
      step list ->
      p directory ->
      p directory ->
      p directory =
   fun strategy path left right ->
    match (left, right) with
    | Empty, t -> t
    | t, Empty -> t
    | Static l, Static r -> Static (merge_static_directory strategy path l r)
    | Dynamic (l_desc, l_builder), Dynamic (r_desc, r_builder) ->
        let sub_desc, sub_dir =
          merge_dynamic_directories
            strategy
            path
            l_desc
            l_builder
            r_desc
            r_builder
        in
        Dynamic (sub_desc, sub_dir)
    | DynamicTail (argl, subtl), DynamicTail (argr, subtr) -> (
        try
          let Eq = Ty.eq argl.id argr.id in
          let subt =
            merge_loop strategy (DynamicTail argl.descr :: path) subtl subtr
          in
          DynamicTail (argl, subt)
        with Ty.Not_equal -> conflict path (CTypes (argl.descr, argr.descr)))
    | Dynamic _, _ | _, Dynamic _ -> conflict path CBuilder
    | DynamicTail _, _ | _, DynamicTail _ -> conflict path CTail

  and select_using_strategy :
      type p.
      [`Raise | `Pick_left | `Pick_right] ->
      step list ->
      conflict ->
      p ->
      p ->
      p option =
   fun strategy path cfct left right ->
    match strategy with
    | `Raise -> conflict path cfct
    | `Pick_right -> Some right
    | `Pick_left -> Some left

  and merge_static_directory :
      type p.
      [`Raise | `Pick_left | `Pick_right] ->
      step list ->
      p static_directory ->
      p static_directory ->
      p static_directory =
   fun strategy path left right ->
    let subdirs =
      match (left.subdirs, right.subdirs) with
      | None, None -> None
      | None, Some dir | Some dir, None -> Some dir
      | Some dl, Some dr -> (
          match (dl, dr) with
          | Suffixes ml, Suffixes mr ->
              let merge =
                StringMap.union (fun name left_dir right_dir ->
                    Option.some
                    @@ merge_loop
                         strategy
                         (Static name :: path)
                         left_dir
                         right_dir)
              in
              Some (Suffixes (merge ml mr))
          | Arg (argl, subtl), Arg (argr, subtr) -> (
              try
                let Eq = Ty.eq argl.id argr.id in
                let subt =
                  merge_loop strategy (Dynamic argl.descr :: path) subtl subtr
                in
                Some (Arg (argl, subt))
              with Ty.Not_equal ->
                conflict path (CTypes (argl.descr, argr.descr)))
          | Arg (arg, _), Suffixes m ->
              conflict
                path
                (CType (arg.descr, List.map fst (StringMap.bindings m)))
          | Suffixes m, Arg (arg, _) ->
              conflict
                path
                (CType (arg.descr, List.map fst (StringMap.bindings m))))
    in
    let services =
      MethMap.union
        (fun meth -> select_using_strategy strategy path @@ CService meth)
        left.services
        right.services
    in
    {subdirs; services}

  and merge_dynamic_directories :
      type p.
      [`Raise | `Pick_left | `Pick_right] ->
      step list ->
      string option ->
      (p -> p directory Lwt.t) ->
      string option ->
      (p -> p directory Lwt.t) ->
      string option * (p -> p directory Lwt.t) =
   fun strategy path left_desc left_builder right_desc right_builder ->
    let merged_description =
      match (left_desc, right_desc) with
      | Some l, Some r ->
          select_using_strategy strategy path (CDynDescr (l, r)) l r
      | Some l, None -> Some l
      | None, Some r -> Some r
      | None, None -> None
    in
    let merged_subdirs key =
      left_builder key >>= fun left_subdir ->
      right_builder key >>= fun right_subdir ->
      Lwt.return @@ merge_loop strategy path left_subdir right_subdir
    in
    (merged_description, merged_subdirs)

  let merge ?(strategy = `Raise) left right = merge_loop strategy [] left right

  (*************************)
  (* Directory description *)
  (*************************)

  let rec describe_directory :
      type a.
      recurse:bool ->
      ?arg:a ->
      a directory ->
      Encoding.schema Description.directory Lwt.t =
   fun ~recurse ?arg dir ->
    match dir with
    | Empty -> Lwt.return Description.Empty
    | Dynamic (descr, builder) -> (
        match arg with
        | None ->
            Lwt.return (Dynamic descr : Encoding.schema Description.directory)
        | Some arg -> builder arg >>= fun dir -> describe_directory ~recurse dir
        )
    | DynamicTail (_, dir) -> describe_directory ~recurse dir
    | Static dir ->
        describe_static_directory recurse dir >>= fun dir ->
        Lwt.return (Static dir : Encoding.schema Description.directory)

  and describe_static_directory :
      type a.
      bool ->
      a static_directory ->
      Encoding.schema Description.static_directory Lwt.t =
   fun recurse dir ->
    let services = MethMap.map describe_service dir.services in
    (if recurse then
     match dir.subdirs with
     | None -> Lwt.return_none
     | Some subdirs ->
         describe_static_subdirectories subdirs >>= fun dirs ->
         Lwt.return (Some dirs)
    else Lwt.return_none)
    >>= fun subdirs ->
    Lwt.return
      ({services; subdirs} : Encoding.schema Description.static_directory)

  and describe_static_subdirectories :
      type a.
      a static_subdirectories ->
      Encoding.schema Description.static_subdirectories Lwt.t =
   fun dir ->
    match dir with
    | Suffixes map ->
        StringMap.fold
          (fun key dir map ->
            map >>= fun map ->
            describe_directory ~recurse:true dir >>= fun dir ->
            Lwt.return (StringMap.add key dir map))
          map
          (Lwt.return StringMap.empty)
        >>= fun map ->
        Lwt.return
          (Suffixes map : Encoding.schema Description.static_subdirectories)
    | Arg (arg, dir) ->
        describe_directory ~recurse:true dir >>= fun dir ->
        Lwt.return
          (Arg (arg.descr, dir)
            : Encoding.schema Description.static_subdirectories)

  and describe_service :
      type a.
      a registered_service_builder -> Encoding.schema Description.service =
   fun {description; _} -> description

  and describe_query :
      type a. a Resto.Internal.query -> Description.query_item list =
   fun (Fields (fields, _)) ->
    let rec loop : type a b. (a, b) query_fields -> _ = function
      | F0 -> []
      | F1 (f, fs) ->
          {
            Description.name = field_name f;
            description = field_description f;
            kind = field_kind f;
          }
          :: loop fs
    in
    loop fields

  (****************************************************************************
   * Lookup
   ****************************************************************************)

  type resolved_directory =
    | Dir : 'a static_directory * 'a -> resolved_directory

  let rec resolve :
      type a.
      string list ->
      a directory ->
      a ->
      string list ->
      (resolved_directory, _) result Lwt.t =
   fun prefix dir args path ->
    match (path, dir) with
    | _, Empty -> Lwt.return_error `Not_found
    | path, Dynamic (_, builder) ->
        builder args >>= fun dir -> resolve prefix dir args path
    | path, DynamicTail (arg, dir) -> (
        match
          List.fold_right
            (fun e acc ->
              match acc with
              | Error _ as err -> err
              | Ok (prefix, path) -> (
                  match arg.destruct e with
                  | Ok s -> Ok (e :: prefix, s :: path)
                  | Error msg ->
                      Error
                        (`Cannot_parse_path
                          (List.rev (e :: prefix), arg.descr, msg))))
            path
            (Ok (prefix, []))
        with
        | Ok (prefix, path) -> resolve prefix dir (args, path) []
        | Error _ as err -> Lwt.return err)
    | [], Static sdir -> Lwt.return_ok (Dir (sdir, args))
    | _name :: _path, Static {subdirs = None; _} -> Lwt.return_error `Not_found
    | name :: path, Static {subdirs = Some (Suffixes static); _} -> (
        match StringMap.find name static with
        | exception Not_found -> Lwt.return_error `Not_found
        | dir -> resolve (name :: prefix) dir args path)
    | name :: path, Static {subdirs = Some (Arg (arg, dir)); _} -> (
        match arg.destruct name with
        | Ok x -> resolve (name :: prefix) dir (args, x) path
        | Error msg ->
            Lwt.return_error
            @@ `Cannot_parse_path (List.rev (name :: prefix), arg.descr, msg))

  let lookup :
      type a.
      a directory ->
      a ->
      meth ->
      string list ->
      (registered_service, lookup_error) result Lwt.t =
   fun dir args meth path ->
    resolve [] dir args path >>= function
    | Error _ as err -> Lwt.return err
    | Ok (Dir (dir, args)) -> (
        match MethMap.find meth dir.services with
        | exception Not_found -> (
            match MethMap.bindings dir.services with
            | [] -> Lwt.return_error `Not_found
            | l -> Lwt.return_error (`Method_not_allowed (List.map fst l)))
        | rs -> rs.builder args >>= Lwt.return_ok)

  let lookup =
    (lookup
      : _ -> _ -> _ -> _ -> (_, lookup_error) result Lwt.t
      :> _ -> _ -> _ -> _ -> (_, [> lookup_error]) result Lwt.t)

  let rec resolve_uri_desc :
      type a.
      string list ->
      a directory ->
      a ->
      string list ->
      (string list * resolved_directory, _) result Lwt.t =
   fun prefix dir args path ->
    let fmt_desc desc = "<" ^ desc ^ ">" in
    match (path, dir) with
    | _, Empty -> Lwt.return_error `Not_found
    | _, Dynamic (_, builder) ->
        builder args >>= fun dir -> resolve_uri_desc prefix dir args path
    | _, DynamicTail (arg, adir) ->
        Lwt.return
        @@ List.fold_right
             (fun name acc ->
               acc >>? fun (prefix, xs) ->
               match arg.destruct name with
               | Ok x -> Ok (fmt_desc arg.descr.name :: prefix, x :: xs)
               | Error msg ->
                   Error
                     (`Cannot_parse_path
                       (List.rev (name :: prefix), arg.descr, msg)))
             path
             (Ok (prefix, []))
        >>=? fun (prefix, xs) -> resolve_uri_desc prefix adir (args, xs) []
    | [], Static sdir -> Lwt.return_ok (prefix, Dir (sdir, args))
    | _, Static {subdirs = None; _} -> Lwt.return_error `Not_found
    | name :: remaining, Static {subdirs = Some (Suffixes static); _} -> (
        match StringMap.find name static with
        | exception Not_found -> Lwt.return_error `Not_found
        | dir -> resolve_uri_desc (name :: prefix) dir args remaining)
    | name :: remaining, Static {subdirs = Some (Arg (arg, adir)); _} -> (
        match arg.destruct name with
        | Ok x ->
            resolve_uri_desc
              (fmt_desc arg.descr.name :: prefix)
              adir
              (args, x)
              remaining
        | Error msg ->
            Lwt.return_error
            @@ `Cannot_parse_path (List.rev (name :: prefix), arg.descr, msg))

  let lookup_uri_desc :
      type a.
      a directory ->
      a ->
      meth ->
      string list ->
      (string, lookup_error) result Lwt.t =
   fun dir args meth path ->
    resolve_uri_desc [] dir args path >>=? fun (path, Dir (dir, _)) ->
    match MethMap.bindings dir.services with
    | [] -> Lwt.return_error `Not_found
    | l -> (
        match MethMap.find meth dir.services with
        | exception Not_found ->
            Lwt.return_error (`Method_not_allowed (List.map fst l))
        | _ -> Lwt.return_ok @@ "/" ^ String.concat "/" (List.rev path))

  let lookup_uri_desc =
    (lookup_uri_desc
      : _ -> _ -> _ -> _ -> (_, lookup_error) result Lwt.t
      :> _ -> _ -> _ -> _ -> (_, [> lookup_error]) result Lwt.t)

  let allowed_methods :
      type a.
      a directory ->
      a ->
      string list ->
      (Resto.meth list, lookup_error) result Lwt.t =
   fun dir args path ->
    resolve [] dir args path >>= function
    | Error err -> Lwt.return_error err
    | Ok (Dir (dir, _)) -> (
        match MethMap.bindings dir.services with
        | [] -> Lwt.return_error `Not_found
        | l -> Lwt.return_ok (List.map fst l))

  let allowed_methods =
    (allowed_methods
      : _ -> _ -> _ -> (_, lookup_error) result Lwt.t
      :> _ -> _ -> _ -> (_, [> lookup_error]) result Lwt.t)

  let rec build_dynamic_dir : type p. p directory -> p -> p directory Lwt.t =
   fun dir args ->
    match dir with
    | Dynamic (_, builder) ->
        builder args >>= fun dir -> build_dynamic_dir dir args
    | _ -> Lwt.return dir

  let rec transparent_resolve :
      type pr p. pr directory -> (pr, p) path -> p -> p directory option Lwt.t =
   fun dir path rargs ->
    match path with
    | Root -> Lwt.return_some dir
    | Static (path, name) -> (
        transparent_resolve dir path rargs >>= function
        | None -> Lwt.return_none
        | Some dir -> (
            build_dynamic_dir dir rargs >>= function
            | Dynamic (_, _) -> assert false (* should not happen. *)
            | Static {subdirs = Some (Suffixes s); _} ->
                Lwt.return_some (StringMap.find name s)
            | Empty -> Lwt.return_none
            | Static _ -> Lwt.return_none
            | DynamicTail _ -> Lwt.return_none))
    | Dynamic (ipath, iarg) -> (
        transparent_resolve dir ipath (fst rargs) >>= function
        | None -> Lwt.return_none
        | Some dir -> (
            build_dynamic_dir dir (fst rargs) >>= function
            | Dynamic (_, _) -> assert false (* should not happen. *)
            | Static {subdirs = Some (Arg (arg, dir)); _} -> (
                match Ty.eq iarg.id arg.id with
                | exception Ty.Not_equal -> Lwt.return_none
                | Eq -> Lwt.return_some (dir : (_ * _) directory :> p directory)
                )
            | Empty -> Lwt.return_none
            | Static _ -> Lwt.return_none
            | DynamicTail _ -> Lwt.return_none))
    | DynamicTail (path, arg) -> (
        transparent_resolve dir path (fst rargs) >>= function
        | None -> Lwt.return_none
        | Some dir -> (
            build_dynamic_dir dir (fst rargs) >>= function
            | Dynamic (_, _) -> assert false (* should not happen. *)
            | DynamicTail (iarg, dir) -> (
                match Ty.eq iarg.id arg.id with
                | exception Ty.Not_equal -> Lwt.return_none
                | Eq -> Lwt.return_some (dir : (_ * _) directory :> p directory)
                )
            | Empty -> Lwt.return_none
            | Static _ -> Lwt.return_none))

  let transparent_lookup :
      type prefix params query input output error.
      prefix directory ->
      (_, prefix, params, query, input, output, error) Service.t ->
      params ->
      query ->
      input ->
      (output, error) Answer.t Lwt.t =
   fun dir service params query body ->
    let service = Service.Internal.to_service service in
    transparent_resolve dir service.path params >>= function
    | None -> Lwt.return (`Not_found None)
    | Some (Static {services; _}) -> (
        try
          (MethMap.find service.meth services).builder params
          >>= fun (Service {handler; types}) ->
          match Service.Internal.eq types service.types with
          | exception Service.Internal.Not_equal -> Lwt.return (`Not_found None)
          | Service.Internal.Eq ->
              (handler query body
                : (_, _) Answer.t Lwt.t
                :> (output, error) Answer.t Lwt.t)
        with Not_found -> Lwt.return (`Not_found None))
    | Some _ -> Lwt.return (`Not_found None)

  let transparent_lookup =
    (transparent_lookup
      : _ ->
        (Resto.meth, _, _, _, _, _, _) Service.t ->
        _ ->
        _ ->
        _ ->
        (_, _) Answer.t Lwt.t
      :> _ ->
         ([< Resto.meth], _, _, _, _, _, _) Service.t ->
         _ ->
         _ ->
         _ ->
         [> (_, _) Answer.t] Lwt.t)

  let rec describe_rpath :
      type a b.
      Description.path_item list -> (a, b) path -> Description.path_item list =
   fun acc path ->
    match path with
    | Root -> acc
    | Static (rpath, name) -> describe_rpath (PStatic name :: acc) rpath
    | Dynamic (rpath, arg) -> describe_rpath (PDynamic arg.descr :: acc) rpath
    | DynamicTail (rpath, arg) ->
        describe_rpath (PDynamicTail arg.descr :: acc) rpath

  (****************************************************************************
   * Registration
   ****************************************************************************)

  let rec step_of_path : type p rk. (rk, p) path -> step list -> step list =
   fun path acc ->
    match path with
    | Root -> acc
    | Static (path, name) -> step_of_path path (Static name :: acc)
    | Dynamic (path, arg) -> step_of_path path (Dynamic arg.descr :: acc)
    | DynamicTail (path, arg) -> step_of_path path (DynamicTail arg.descr :: acc)

  let step_of_path p = step_of_path p []

  let conflict path kind = raise (Conflict (step_of_path path, kind))

  let rec insert :
      type k rk.
      (rk, k) path ->
      rk directory ->
      k directory * (k directory -> rk directory) =
   fun path dir ->
    match path with
    | Root -> (dir, fun x -> x)
    | Static (subpath, name) ->
        let subdir, rebuild = insert subpath dir in
        let dirmap, services =
          match subdir with
          | Empty -> (StringMap.empty, MethMap.empty)
          | Static {subdirs = None; services} -> (StringMap.empty, services)
          | Static {subdirs = Some (Suffixes m); services} -> (m, services)
          | Static {subdirs = Some (Arg (arg, _)); _} ->
              conflict path (CType (arg.descr, [name]))
          | Dynamic _ -> conflict path CBuilder
          | DynamicTail _ -> conflict path CTail
        in
        let dir = try StringMap.find name dirmap with Not_found -> empty in
        let rebuild s =
          let subdirs = Some (Suffixes (StringMap.add name s dirmap)) in
          rebuild (Static {subdirs; services})
        in
        (dir, rebuild)
    | Dynamic (subpath, arg) ->
        let subdir, rebuild = insert subpath dir in
        let dir, services =
          match subdir with
          | Empty -> (Empty, MethMap.empty)
          | Static {subdirs = None; services} -> (Empty, services)
          | Static {subdirs = Some (Arg (arg', dir)); services} -> (
              try
                let Eq = Ty.eq arg.id arg'.id in
                ((dir :> k directory), services)
              with Ty.Not_equal ->
                conflict path (CTypes (arg.descr, arg'.descr)))
          | Static {subdirs = Some (Suffixes m); _} ->
              conflict
                path
                (CType (arg.descr, List.map fst (StringMap.bindings m)))
          | Dynamic _ -> conflict path CBuilder
          | DynamicTail _ -> conflict path CTail
        in
        let rebuild s =
          let subdirs = Some (Arg (arg, s)) in
          rebuild (Static {subdirs; services})
        in
        (dir, rebuild)
    | DynamicTail (subpath, arg) -> (
        let subdir, rebuild = insert subpath dir in
        match subdir with
        | Empty ->
            let rebuild s = rebuild (DynamicTail (arg, s)) in
            (empty, rebuild)
        | Static {subdirs = None; services} ->
            conflict path (CService (fst (MethMap.min_binding services)))
        | Static {subdirs = Some (Arg (arg, _)); _} ->
            conflict path (CType (arg.descr, []))
        | Static {subdirs = Some (Suffixes m); _} ->
            conflict
              path
              (CType (arg.descr, List.map fst (StringMap.bindings m)))
        | Dynamic _ -> conflict path CBuilder
        | DynamicTail _ -> conflict path CTail)

  let register :
      type p q i o e pr.
      pr directory ->
      (_, pr, p, q, i, o, e) Service.t ->
      (p -> q -> i -> (o, e) Answer.t Lwt.t) ->
      pr directory =
   fun root s handler ->
    let s = Service.Internal.to_service s in
    let register :
        type k.
        (pr, k) path -> (k -> q -> i -> (o, e) Answer.t Lwt.t) -> pr directory =
     fun path handler ->
      let dir, insert = insert path root in
      let rs =
        let description : _ Description.service =
          {
            meth = s.meth;
            path = describe_rpath [] path;
            description = s.description;
            query = describe_query (Resto.Internal.to_query s.types.query);
            input =
              (match s.types.input with
              | Service.No_input -> None
              | Service.Input input -> Some (lazy (Encoding.schema input)));
            output = lazy (Encoding.schema s.types.output);
            error = lazy (Encoding.schema s.types.error);
          }
        in
        let builder key =
          Lwt.return (Service {types = s.types; handler = handler key})
        in
        {meth = s.meth; description; builder}
      in
      match dir with
      | Empty ->
          insert
            (Static {services = MethMap.singleton s.meth rs; subdirs = None})
      | Static ({services; _} as dir) when not (MethMap.mem s.meth services) ->
          insert (Static {dir with services = MethMap.add s.meth rs services})
      | Static _ -> conflict path (CService s.meth)
      | Dynamic _ -> conflict path CBuilder
      | DynamicTail _ -> conflict path CTail
    in
    register s.path handler

  let register =
    (register
      : _ ->
        (Resto.meth, _, _, _, _, _, _) Service.t ->
        (_ -> _ -> _ -> (_, _) Answer.t Lwt.t) ->
        _
      :> _ ->
         ([< Resto.meth], _, _, _, _, _, _) Service.t ->
         (_ -> _ -> _ -> [< (_, _) Answer.t] Lwt.t) ->
         _)

  let register_dynamic_directory :
      type pr a.
      ?descr:string ->
      pr directory ->
      (pr, a) Path.path ->
      (a -> a directory Lwt.t) ->
      pr directory =
   fun ?descr root path builder ->
    let path = Resto.Internal.to_path path in
    let register :
        type k. (pr, k) path -> (k -> k directory Lwt.t) -> pr directory =
     fun path builder ->
      let dir, insert = insert path root in
      match dir with
      | Empty -> insert (Dynamic (descr, builder))
      | Static {services; subdirs = None} ->
          conflict path (CService (fst (MethMap.choose services)))
      | Static {subdirs = Some _; _} -> conflict path CDir
      | Dynamic _ -> conflict path CBuilder
      | DynamicTail _ -> conflict path CTail
    in
    register path builder

  let register_describe_directory_service :
      type pr.
      pr directory -> (pr, pr, _) Service.description_service -> pr directory =
   fun root service ->
    let dir = ref root in
    let lookup (args, path) {Description.recurse} () =
      resolve [] root args path >>= function
      | Error `Not_found | Error (`Cannot_parse_path _) ->
          Lwt.return (`Not_found None)
      | Ok (Dir (dir, arg)) -> (
          describe_directory ~recurse ~arg (Static dir) >>= function
          | Static {services; _} when (not recurse) && MethMap.is_empty services
            ->
              Lwt.return (`Not_found None)
          | d -> Lwt.return (`Ok d))
    in
    dir := register root service lookup ;
    !dir

  (****************************************************************************
   * Pretty-printing conflicts
   ****************************************************************************)

  let string_of_step : step -> string = function
    | Static s -> s
    | Dynamic arg -> Printf.sprintf "<%s>" arg.name
    | DynamicTail arg -> Printf.sprintf "<%s...>" arg.name

  let string_of_conflict_kind = function
    | CDir -> "Directory conflict"
    | CBuilder -> "Builder conflict"
    | CDynDescr (l, r) ->
        Printf.sprintf
          "Conflict between dynamic directories description %s and %s"
          l
          r
    | CTail -> "Tail conflict"
    | CService meth ->
        Printf.sprintf
          "Service conflict for method %s"
          (Resto.string_of_meth meth)
    | CTypes (arg1, arg2) ->
        Printf.sprintf
          "Type conflict between arguments: found type %s but type %s was \
           expected"
          arg2.name
          arg1.name
    | CType (arg, names) ->
        Printf.sprintf
          "Type conflict for %s with argument %s"
          (String.concat ", " names)
          arg.name

  let string_of_conflict (steps, kind) =
    Printf.sprintf
      "%s in /%s"
      (string_of_conflict_kind kind)
      (String.concat "/" @@ List.map string_of_step steps)

  (* Register a special printer for conflicts *)
  let () =
    Printexc.register_printer @@ function
    | Conflict (steps, conflict) ->
        Format.ksprintf
          Option.some
          "Conflict in registration of service: %s"
          (string_of_conflict (steps, conflict))
    | _ -> None

  (****************************************************************************
   * Let's currify!
   ****************************************************************************)

  open Curry

  let register0 root s f = register root s (curry Z f)

  let register1 root s f = register root s (curry (S Z) f)

  let register2 root s f = register root s (curry (S (S Z)) f)

  let register3 root s f = register root s (curry (S (S (S Z))) f)

  let register4 root s f = register root s (curry (S (S (S (S Z)))) f)

  let register5 root s f = register root s (curry (S (S (S (S (S Z))))) f)

  let register_dynamic_directory1 ?descr root s f =
    register_dynamic_directory ?descr root s (curry (S Z) f)

  let register_dynamic_directory2 ?descr root s f =
    register_dynamic_directory ?descr root s (curry (S (S Z)) f)

  let register_dynamic_directory3 ?descr root s f =
    register_dynamic_directory ?descr root s (curry (S (S (S Z))) f)
end
