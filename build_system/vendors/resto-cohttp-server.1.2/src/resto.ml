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

module Utils = struct
  let split_path path =
    let l = String.length path in
    let rec do_slashes acc i =
      if i >= l then List.rev acc
      else if path.[i] = '/' then do_slashes acc (i + 1)
      else do_component acc i i
    and do_component acc i j =
      if j >= l then
        if i = j then List.rev acc
        else List.rev (String.sub path i (j - i) :: acc)
      else if path.[j] = '/' then
        do_slashes (String.sub path i (j - i) :: acc) j
      else do_component acc i (j + 1)
    in
    do_slashes [] 0

  let decode_split_path path = path |> split_path |> List.map Uri.pct_decode
end

let bool_of_string s =
  match String.lowercase_ascii s with
  | "" | "true" | "t" | "yes" | "y" -> Ok true
  | "false" | "f" | "no" | "n" -> Ok false
  | _ -> Error "Cannot parse boolean value"

type meth = [`GET | `POST | `DELETE | `PUT | `PATCH]

let string_of_meth = function
  | `GET -> "GET"
  | `POST -> "POST"
  | `DELETE -> "DELETE"
  | `PUT -> "PUT"
  | `PATCH -> "PATCH"

let meth_of_string = function
  | "GET" -> Some `GET
  | "POST" -> Some `POST
  | "DELETE" -> Some `DELETE
  | "PUT" -> Some `PUT
  | "PATCH" -> Some `PATCH
  | _ -> None

module MethMap = Map.Make (struct
  type t = meth

  let compare = compare
end)

module StringMap = Map.Make (String)

type (_, _) eq = Eq : ('a, 'a) eq

module Internal = struct
  module Ty = struct
    type 'a witness = ..

    exception Not_equal

    module type Ty = sig
      type t

      val witness : t witness

      val eq : 'a witness -> ('a, t) eq
    end

    type 'a id = (module Ty with type t = 'a)

    let new_id (type a) () =
      let module Ty = struct
        type t = a

        type 'a witness += Ty : t witness

        let witness = Ty

        let eq (type b) : b witness -> (b, t) eq = function
          | Ty -> Eq
          | _ -> raise Not_equal
      end in
      (module Ty : Ty with type t = a)

    let eq : type a b. a id -> b id -> (a, b) eq =
     fun (module TyA) (module TyB) -> TyB.eq TyA.witness
  end

  type descr = {name : string; descr : string option}

  type 'a arg = {
    id : 'a Ty.id;
    destruct : string -> ('a, string) result;
    construct : 'a -> string;
    descr : descr;
  }

  let from_arg x = x

  let to_arg x = x

  type (_, _) path =
    | Root : ('rkey, 'rkey) path
    | Static : ('rkey, 'key) path * string -> ('rkey, 'key) path
    | Dynamic : ('rkey, 'key) path * 'a arg -> ('rkey, 'key * 'a) path
    | DynamicTail : ('rkey, 'key) path * 'a arg -> ('rkey, 'key * 'a list) path

  let rec subst0 : type a b. (a, a) path -> (b, b) path = function
    | Root -> Root
    | Static (path, name) -> Static (subst0 path, name)
    | Dynamic _ -> assert false (* impossible *)
    | DynamicTail _ -> assert false

  (* impossible *)

  let rec subst1 : type a b c. (a, a * c) path -> (b, b * c) path = function
    | Root -> assert false (* impossible *)
    | Static (path, name) -> Static (subst1 path, name)
    | Dynamic (path, arg) -> Dynamic (subst0 path, arg)
    | DynamicTail (path, arg) -> DynamicTail (subst0 path, arg)

  let rec subst2 : type a b c d. (a, (a * c) * d) path -> (b, (b * c) * d) path
      = function
    | Root -> assert false (* impossible *)
    | Static (path, name) -> Static (subst2 path, name)
    | Dynamic (path, arg) -> Dynamic (subst1 path, arg)
    | DynamicTail (path, arg) -> DynamicTail (subst1 path, arg)

  let rec subst3 :
      type a b c d e. (a, ((a * c) * d) * e) path -> (b, ((b * c) * d) * e) path
      = function
    | Root -> assert false (* impossible *)
    | Static (path, name) -> Static (subst3 path, name)
    | Dynamic (path, arg) -> Dynamic (subst2 path, arg)
    | DynamicTail (path, arg) -> DynamicTail (subst2 path, arg)

  let from_path x = x

  let to_path x = x

  type 'a query =
    (* inspired from Irmin.Ty.record. *)
    | Fields : ('a, 'b) query_fields * 'b -> 'a query

  and ('a, 'b) query_fields =
    | F0 : ('a, 'a) query_fields
    | F1 :
        ('a, 'b) query_field * ('a, 'c) query_fields
        -> ('a, 'b -> 'c) query_fields

  and ('a, 'b) query_field =
    | Single : {
        name : string;
        description : string option;
        ty : 'b arg;
        default : 'b;
        get : 'a -> 'b;
      }
        -> ('a, 'b) query_field
    | Opt : {
        name : string;
        description : string option;
        ty : 'b arg;
        get : 'a -> 'b option;
      }
        -> ('a, 'b option) query_field
    | Flag : {
        name : string;
        description : string option;
        get : 'a -> bool;
      }
        -> ('a, bool) query_field
    | Multi : {
        name : string;
        description : string option;
        ty : 'b arg;
        get : 'a -> 'b list;
      }
        -> ('a, 'b list) query_field

  type query_kind =
    | Single of descr
    | Optional of descr
    | Flag
    | Multi of descr

  let field_name (type t) : (_, t) query_field -> _ = function
    | Single {name; _} -> name
    | Opt {name; _} -> name
    | Flag {name; _} -> name
    | Multi {name; _} -> name

  let field_description (type t) : (_, t) query_field -> _ = function
    | Single {description; _} -> description
    | Opt {description; _} -> description
    | Flag {description; _} -> description
    | Multi {description; _} -> description

  let field_kind (type t) : (_, t) query_field -> query_kind = function
    | Single {ty; _} -> Single ty.descr
    | Opt {ty; _} -> Optional ty.descr
    | Flag _ -> Flag
    | Multi {ty; _} -> Multi ty.descr

  let from_query x = x

  let to_query x = x
end

open Internal

module Arg = struct
  type descr = Internal.descr = {name : string; descr : string option}

  type 'a t = 'a Internal.arg

  type 'a arg = 'a t

  let make ?descr ~name ~destruct ~construct () =
    let id = Ty.new_id () in
    let descr = {name; descr} in
    {descr; id; construct; destruct}

  let like arg ?descr name = {arg with id = Ty.new_id (); descr = {name; descr}}

  let descr (ty : 'a arg) = ty.descr

  let bool : bool arg =
    let string_of_bool = function true -> "yes" | false -> "no" in
    make ~name:"bool" ~destruct:bool_of_string ~construct:string_of_bool ()

  let int =
    let int_of_string s =
      try Ok (int_of_string s)
      with Failure _ -> Error "Cannot parse integer value"
    in
    make ~name:"int" ~destruct:int_of_string ~construct:string_of_int ()

  let float =
    let float_of_string s =
      try Ok (float_of_string s)
      with Failure _ -> Error "Cannot parse float value"
    in
    make ~name:"float" ~destruct:float_of_string ~construct:string_of_float ()

  let int32 =
    let int32_of_string s =
      try Ok (Int32.of_string s)
      with Failure _ -> Error "Cannot parse int32 value"
    in
    make ~name:"int32" ~destruct:int32_of_string ~construct:Int32.to_string ()

  let int64 =
    let int64_of_string s =
      try Ok (Int64.of_string s)
      with Failure _ -> Error "Cannot parse int64 value"
    in
    make ~name:"int64" ~destruct:int64_of_string ~construct:Int64.to_string ()

  let string =
    make ~name:"string" ~destruct:(fun x -> Ok x) ~construct:(fun x -> x) ()

  let eq a1 a2 =
    try Some (Ty.eq a1.id a2.id) with Internal.Ty.Not_equal -> None
end

module Path = struct
  type ('a, 'b) t = ('a, 'b) Internal.path

  type ('a, 'b) path = ('a, 'b) Internal.path

  type 'prefix context = ('prefix, 'prefix) path

  let root = Root

  let open_root = Root

  let add_suffix (type p pr) (path : (p, pr) path) name =
    match path with
    | DynamicTail _ -> invalid_arg "Resto.Path.add_suffix"
    | path -> Static (path, name)

  let add_arg (type p pr) (path : (p, pr) path) arg =
    match path with
    | DynamicTail _ -> invalid_arg "Resto.Path.add_arg"
    | path -> Dynamic (path, arg)

  let add_final_args (type p pr) (path : (p, pr) path) arg =
    match path with
    | DynamicTail _ -> invalid_arg "Resto.Path.add_final_arg"
    | path -> DynamicTail (path, arg)

  let prefix : type p pr a. (pr, a) path -> (a, p) path -> (pr, p) path =
   fun p1 p2 ->
    let rec prefix : type pr a k. (pr, a) path -> (a, k) path -> (pr, k) path =
     fun p1 p2 ->
      match p2 with
      | Root -> p1
      | Static (path, name) -> add_suffix (prefix p1 path) name
      | Dynamic (path, arg) -> add_arg (prefix p1 path) arg
      | DynamicTail (path, arg) -> add_final_args (prefix p1 path) arg
    in
    match p1 with
    | DynamicTail _ -> invalid_arg "Resto.Path.prefix"
    | _ -> prefix p1 p2

  let ( / ) = add_suffix

  let ( /: ) = add_arg

  let ( /:* ) = add_final_args

  let subst0 = Internal.subst0

  let subst1 = Internal.subst1

  let subst2 = Internal.subst2

  let subst3 = Internal.subst3

  let to_segments : type pr p. (pr, p) path -> string list =
   fun path ->
    let rec flatten_rev : type pr p. (pr, p) path -> string list = function
      | Root -> []
      | Static (p, s) -> s :: flatten_rev p
      | Dynamic (p, arg) ->
          Printf.sprintf "<%s>" arg.descr.name :: flatten_rev p
      | DynamicTail (p, arg) ->
          Printf.sprintf "<%s>*" arg.descr.name :: flatten_rev p
    in
    List.rev @@ flatten_rev path

  let to_string path = "/" ^ String.concat "/" (to_segments path)
end

module Query = struct
  type 'a t = 'a Internal.query

  type 'a query = 'a Internal.query

  type ('a, 'b) field = ('a, 'b) Internal.query_field

  type ('a, 'b, 'c) open_query =
    ('a, 'c) query_fields -> 'b * ('a, 'b) query_fields

  let field ?descr name ty default get : (_, _) query_field =
    Single {name; description = descr; ty; default; get}

  let opt_field ?descr name ty get : (_, _) query_field =
    Opt {name; description = descr; ty; get}

  let flag ?descr name get : (_, _) query_field =
    Flag {name; description = descr; get}

  let multi_field ?descr name ty get : (_, _) query_field =
    Multi {name; description = descr; ty; get}

  let query : 'b -> ('a, 'b, 'b) open_query = fun c fs -> (c, fs)

  let app :
      type a b c d.
      (a, b, c -> d) open_query -> (a, c) query_field -> (a, b, d) open_query =
   fun r f fs ->
    let c, fs = r (F1 (f, fs)) in
    (c, fs)

  let seal : type a b. (a, b, a) open_query -> a t =
   fun r ->
    let c, fs = r F0 in
    Fields (fs, c)

  let ( |+ ) = app

  let empty = Fields (F0, ())

  type 'a efield = Field : ('a, 'b) query_field -> 'a efield

  let fold_fields (type fs) ~f ~init fs =
    let rec loop : type f. _ -> (fs, f) query_fields -> _ =
     fun acc -> function
      | F0 -> acc
      | F1 (field, fs) -> loop (f acc (Field field)) fs
    in
    loop init fs

  type 'a parsed_field =
    | Parsed : ('a, 'b) query_field * 'b option -> 'a parsed_field

  let rec rebuild : type fs f. _ -> (fs, f) query_fields -> f -> fs =
   fun map fs f ->
    match fs with
    | F0 -> f
    | F1 (Single field, fs) -> (
        match StringMap.find field.name map with
        | Parsed (Single field', v) ->
            let Eq = Ty.eq field.ty.id field'.ty.id in
            let v = match v with None -> field.default | Some v -> v in
            rebuild map fs (f v)
        | Parsed _ -> assert false)
    | F1 (Opt field, fs) -> (
        match StringMap.find field.name map with
        | Parsed (Opt field', v) ->
            let Eq = Ty.eq field.ty.id field'.ty.id in
            let v = match v with None -> None | Some v -> v in
            rebuild map fs (f v)
        | Parsed _ -> assert false)
    | F1 (Flag field, fs) -> (
        match StringMap.find field.name map with
        | Parsed (Flag _, v) ->
            let v = match v with None -> false | Some v -> v in
            rebuild map fs (f v)
        | Parsed _ -> assert false)
    | F1 (Multi field, fs) -> (
        match StringMap.find field.name map with
        | Parsed (Multi field', v) ->
            let Eq = Ty.eq field.ty.id field'.ty.id in
            let v = match v with None -> [] | Some v -> v in
            rebuild map fs (f v)
        | Parsed _ -> assert false)

  exception Invalid of string

  type untyped = (string * string) list

  let parse (Fields (fs, f)) =
    let fields =
      fold_fields
        ~f:(fun map (Field f) ->
          StringMap.add (field_name f) (Parsed (f, None)) map)
        ~init:StringMap.empty
        fs
    in
    fun query ->
      let fail fmt = Format.kasprintf (fun s -> raise (Invalid s)) fmt in
      let fields =
        List.fold_left
          (fun fields (name, value) ->
            match StringMap.find name fields with
            | exception Not_found -> fields
            | Parsed (Single _, Some _) ->
                fail "Duplicate argument '%s' in query string." name
            | Parsed (Opt _, Some _) ->
                fail "Duplicate argument '%s' in query string." name
            | Parsed (Flag _, Some _) ->
                fail "Duplicate argument '%s' in query string." name
            | Parsed (Single f, None) -> (
                match f.ty.destruct value with
                | Error error ->
                    fail
                      "Failed to parse argument '%s' (%S): %s"
                      name
                      value
                      error
                | Ok v -> StringMap.add name (Parsed (Single f, Some v)) fields)
            | Parsed (Opt f, None) -> (
                match f.ty.destruct value with
                | Error error ->
                    fail
                      "Failed to parse argument '%s' (%S): %s"
                      name
                      value
                      error
                | Ok v ->
                    StringMap.add name (Parsed (Opt f, Some (Some v))) fields)
            | Parsed (Flag f, None) -> (
                match bool_of_string value with
                | Ok v -> StringMap.add name (Parsed (Flag f, Some v)) fields
                | Error error ->
                    fail
                      "Failed to parse argument '%s' (%S): %s"
                      name
                      value
                      error)
            | Parsed (Multi f, previous) -> (
                match f.ty.destruct value with
                | Error error ->
                    fail
                      "Failed to parse argument '%s' (%S): %s"
                      name
                      value
                      error
                | Ok v ->
                    let v =
                      match previous with None -> [v] | Some l -> v :: l
                    in
                    StringMap.add name (Parsed (Multi f, Some v)) fields))
          fields
          query
      in
      rebuild fields fs f
end

module Description = struct
  type request = {recurse : bool}

  let request_query =
    let open Query in
    query (fun recurse -> {recurse})
    |+ field "recurse" Arg.bool false (fun t -> t.recurse)
    |> seal

  type nonrec query_kind = query_kind =
    | Single of Arg.descr
    | Optional of Arg.descr
    | Flag
    | Multi of Arg.descr

  [@@@ocaml.warning "-30"]

  type 'schema service = {
    description : string option;
    path : path_item list;
    meth : meth;
    query : query_item list;
    input : 'schema Lazy.t option;
    output : 'schema Lazy.t;
    error : 'schema Lazy.t;
  }

  and path_item =
    | PStatic of string
    | PDynamic of Arg.descr
    | PDynamicTail of Arg.descr

  and query_item = {
    name : string;
    description : string option;
    kind : query_kind;
  }

  type 'schema directory =
    | Empty
    | Static of 'schema static_directory
    | Dynamic of string option

  and 'schema static_directory = {
    services : 'schema service MethMap.t;
    subdirs : 'schema static_subdirectories option;
  }

  and 'schema static_subdirectories =
    | Suffixes of 'schema directory Map.Make(String).t
    | Arg of Arg.descr * 'schema directory

  let rec pp_print_directory ppf =
    let open Format in
    function
    | Empty -> fprintf ppf "<empty>"
    | Static dir -> fprintf ppf "@[%a@]" pp_print_static_directory dir
    | Dynamic None -> fprintf ppf "<dyntree>"
    | Dynamic (Some descr) -> fprintf ppf "<dyntree> : %s" descr

  and pp_print_static_directory ppf =
    let open Format in
    function
    | {services; subdirs = None} when MethMap.is_empty services ->
        fprintf ppf "{}"
    | {services; subdirs = None} ->
        fprintf ppf "@[<v>%a@]" pp_print_dispatch_services services
    | {services; subdirs = Some subdirs} when MethMap.is_empty services ->
        fprintf ppf "%a" pp_print_static_subdirectories subdirs
    | {services; subdirs = Some subdirs} ->
        fprintf
          ppf
          "@[<v>%a@ %a@]"
          pp_print_dispatch_services
          services
          pp_print_static_subdirectories
          subdirs

  and pp_print_static_subdirectories ppf =
    let open Format in
    function
    | Suffixes map ->
        let print_binding ppf (name, tree) =
          fprintf ppf "@[<hov 2>%s:@ %a@]" name pp_print_directory tree
        in
        fprintf
          ppf
          "@[<v>%a@]"
          (pp_print_list ~pp_sep:pp_print_cut print_binding)
          (StringMap.bindings map)
    | Arg (arg, tree) ->
        fprintf ppf "@[<hov 2>[:%s:]@ @[%a@]@]" arg.name pp_print_directory tree

  and pp_print_dispatch_services ppf services =
    MethMap.iter
      (fun _ s ->
        match s with
        | {description = None; meth; _} ->
            Format.fprintf ppf "<%s>" (string_of_meth meth)
        | {description = Some descr; meth; _} ->
            Format.fprintf ppf "<%s> : %s" (string_of_meth meth) descr)
      services
end

module type ENCODING = sig
  type 'a t

  type schema

  val unit : unit t

  val untyped : string t

  val conv : ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

  val schema : ?definitions_path:string -> 'a t -> schema

  val description_request_encoding : Description.request t

  val description_answer_encoding : schema Description.directory t
end

module MakeService (Encoding : ENCODING) = struct
  module Internal = struct
    include Internal

    type ('query, 'input, 'output, 'error) types = {
      query : 'query query;
      input : 'input input;
      output : 'output Encoding.t;
      error : 'error Encoding.t;
    }

    and _ input =
      | No_input : unit input
      | Input : 'input Encoding.t -> 'input input

    type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) iservice = {
      description : string option;
      meth : 'meth;
      path : ('prefix, 'params) path;
      types : ('query, 'input, 'output, 'error) types;
    }
      constraint 'meth = [< meth]

    let from_service x = x

    let to_service x = x

    type (_, _) eq =
      | Eq
          : ( ('query, 'input, 'output, 'error) types,
              ('query, 'input, 'output, 'error) types )
            eq

    exception Not_equal

    let eq :
        type query1 input1 output1 error1 query2 input2 output2 error2.
        (query1, input1, output1, error1) types ->
        (query2, input2, output2, error2) types ->
        ( (query1, input1, output1, error1) types,
          (query2, input2, output2, error2) types )
        eq =
     fun x y ->
      if Obj.magic x == Obj.magic y then Obj.magic Eq (* FIXME *)
      else raise Not_equal
  end

  include Internal
  open Path

  type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) t =
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) Internal.iservice

  type (+'meth, 'prefix, 'params, 'query, 'input, 'output, 'error) service =
    ('meth, 'prefix, 'params, 'query, 'input, 'output, 'error) t

  let get_service ?description ~query ~output ~error path =
    let input = No_input in
    {meth = `GET; description; path; types = {query; input; output; error}}

  let post_service ?description ~query ~input ~output ~error path =
    let input = Input input in
    {meth = `POST; description; path; types = {query; input; output; error}}

  let delete_service ?description ~query ~output ~error path =
    let input = No_input in
    {meth = `DELETE; description; path; types = {query; input; output; error}}

  let put_service ?description ~query ~input ~output ~error path =
    let input = Input input in
    {meth = `PUT; description; path; types = {query; input; output; error}}

  let patch_service ?description ~query ~input ~output ~error path =
    let input = Input input in
    {meth = `PATCH; description; path; types = {query; input; output; error}}

  let prefix path s = {s with path = Path.prefix path s.path}

  let subst0 s = {s with path = Internal.subst0 s.path}

  let subst1 s = {s with path = Internal.subst1 s.path}

  let subst2 s = {s with path = Internal.subst2 s.path}

  let subst3 s = {s with path = Internal.subst3 s.path}

  let meth {meth; _} = meth

  let query : type pr p i q o e. (_, pr, p, q, i, o, e) service -> q Query.t =
   fun {types; _} -> types.query

  let input_encoding :
      type pr p i q o e. (_, pr, p, q, i, o, e) service -> i input =
   fun {types; _} -> types.input

  let output_encoding :
      type pr p i q o e. (_, pr, p, q, i, o, e) service -> o Encoding.t =
   fun {types; _} -> types.output

  let error_encoding :
      type pr p i q o e. (_, pr, p, q, i, o, e) service -> e Encoding.t =
   fun {types; _} -> types.error

  type ('prefix, 'params, 'error) description_service =
    ( [`GET],
      'prefix,
      'params * string list,
      Description.request,
      unit,
      Encoding.schema Description.directory,
      'error )
    service

  let description_service ?description error path =
    let description =
      match description with Some descr -> descr | None -> "<TODO>"
    in
    get_service
      ~description
      ~query:Description.request_query
      ~output:Encoding.description_answer_encoding
      ~error
      Path.(path /:* Arg.string)

  type 'input request = {meth : meth; uri : Uri.t; input : 'input input}

  let forge_request_args : type pr p. (pr, p) path -> p -> string list =
   fun path args ->
    let rec forge_request_args :
        type k. (pr, k) path -> k -> string list -> string list =
     fun path args acc ->
      match (path, args) with
      | Root, _ -> acc
      | Static (path, name), args -> forge_request_args path args (name :: acc)
      | Dynamic (path, arg), (args, x) ->
          forge_request_args path args (arg.construct x :: acc)
      | DynamicTail (path, arg), (args, xs) ->
          forge_request_args
            path
            args
            (List.fold_right (fun x acc -> arg.construct x :: acc) xs acc)
    in
    forge_request_args path args []

  let forge_request_query : type q. q query -> q -> (string * string) list =
   fun (Fields (fields, _)) q ->
    let rec loop : type t. (q, t) query_fields -> _ = function
      | F0 -> []
      | F1 (Single {name; ty; get; _}, fields) ->
          (name, ty.construct (get q)) :: loop fields
      | F1 (Opt {name; ty; get; _}, fields) -> (
          match get q with
          | None -> loop fields
          | Some v -> (name, ty.construct v) :: loop fields)
      | F1 (Flag {name; get; _}, fields) -> (
          match get q with
          | false -> loop fields
          | true -> (name, "true") :: loop fields)
      | F1 (Multi {name; ty; get; _}, fields) -> (
          match get q with
          | [] -> loop fields
          | l ->
              List.fold_right
                (fun v acc -> (name, ty.construct v) :: acc)
                l
                (loop fields))
    in
    loop fields

  let forge_partial_request :
      type pr p i q o e.
      (_, pr, p, q, i, o, e) service -> ?base:Uri.t -> p -> q -> i request =
   fun s ?base:(uri = Uri.empty) args query ->
    let path = String.concat "/" (forge_request_args s.path args) in
    let prefix = Uri.path uri in
    let prefixed_path = if prefix = "" then path else prefix ^ "/" ^ path in
    let uri = Uri.with_path uri prefixed_path in
    let uri = Uri.with_query' uri (forge_request_query s.types.query query) in
    {meth = s.meth; uri; input = s.types.input}

  let forge_partial_request =
    (forge_partial_request
      : (meth, _, _, _, _, _, _) service -> _
      :> ([< meth], _, _, _, _, _, _) service -> _)

  let forge_request =
    (forge_partial_request
      : (meth, _, _, _, _, _, _) service -> _
      :> ([< meth], unit, _, _, _, _, _) service -> _)
end
