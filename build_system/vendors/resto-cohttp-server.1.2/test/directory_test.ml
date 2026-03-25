(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Directory = Resto_directory.Make (Resto_json.Encoding)
module Service = Resto.MakeService (Resto_json.Encoding)

let ( let* ) = Lwt.bind

let ( let*? ) = Lwt_result.bind

let traverse = function
  | Ok p ->
      let* r = p in
      Lwt.return @@ Ok r
  | Error _ as err -> Lwt.return err

let get_or_else f = function Ok i -> i | Error err -> f err

module Resolve_uri_desc = struct
  let uri_desc_res_testable =
    let meth_pp fmt m = Fmt.pf fmt "%s" @@ Resto.string_of_meth m in
    let arg_descr_pp fmt Resto.Arg.{name; descr} =
      Fmt.pf fmt "{name: %s; descr: %a}" name (Fmt.option Fmt.string) descr
    in
    let lookup_error_pp fmt = function
      | `Not_found -> Fmt.string fmt "Not found"
      | `Method_not_allowed ms ->
          Fmt.pf fmt "Method not allowed: %a" (Fmt.list meth_pp) ms
      | `Cannot_parse_path (path, arg, name) ->
          Fmt.pf
            fmt
            "Cannot parse path: %a with arg %a and name %s"
            (Fmt.list Fmt.string)
            path
            arg_descr_pp
            arg
            name
    in
    (* Just checking we are in the right case of error to implement
       tests easily. *)
    let lookup_error_eq a b =
      match (a, b) with
      | `Not_found, `Not_found
      | `Method_not_allowed _, `Method_not_allowed _
      | `Cannot_parse_path (_, _, _), `Cannot_parse_path (_, _, _) ->
          true
      | _ -> false
    in
    let lookup_error_testable =
      Alcotest.testable lookup_error_pp lookup_error_eq
    in
    Alcotest.(result string lookup_error_testable)

  let not_found_case = `Not_found

  let cannot_parse_case = `Cannot_parse_path ([], Resto.Arg.(descr int), "")

  let method_not_allowed_case = `Method_not_allowed [`GET]

  let test ?(title = "") ~exptd res =
    let* r = res in
    Lwt.return @@ Alcotest.check uri_desc_res_testable title exptd r

  let tests =
    [
      ( "succeed to resolve static paths",
        fun () ->
          test
            ~exptd:(Ok "/bar/<int>/<float>/patch")
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `POST
               ["bar"; "2"; "1.7"; "patch"]) );
      ( "succeed to resolve dynamic paths",
        fun () ->
          test
            ~exptd:(Ok "/tartine/<float>/chaussure/<int>/minus")
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `POST
               ["tartine"; "1."; "chaussure"; "7"; "minus"]) );
      ( "succeed to resolve dynamic tails",
        fun () ->
          test
            ~exptd:(Ok "/foobar/<int>/<int>/<int>/<int>/<int>")
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `GET
               ["foobar"; "1"; "2"; "3"; "4"; "5"]) );
      ( "fail to retrieve non existant paths",
        fun () ->
          test
            ~exptd:(Error not_found_case)
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `GET
               ["toto"; "1"; "tata"; "2"; "tutu"; "3"]) );
      ( "fail to retrieve incomplete paths",
        fun () ->
          test
            ~exptd:(Error not_found_case)
            (Directory.lookup_uri_desc Fixtures.Directory.dir () `POST ["foo"])
      );
      ( "fail if an argument can't be serialized",
        fun () ->
          test
            ~exptd:(Error cannot_parse_case)
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `POST
               ["foo"; "surely_not_an_int"; "add"]) );
      ( "fail if a service exist but the method is not good",
        fun () ->
          test
            ~exptd:(Error method_not_allowed_case)
            (Directory.lookup_uri_desc
               Fixtures.Directory.dir
               ()
               `GET
               ["foo"; "1"; "add"]) );
    ]
end

module Merge = struct
  let dir1 = Resto.Path.(root / "dir1")

  let dir2 = Resto.Path.(root / "dir2")

  let dir3 = Resto.Path.(dir1 / "dir3")

  let dir4 = Resto.Path.(dir1 / "dir4")

  let dir5 = Resto.Path.(dir3 / "dir5")

  let dir6 = Resto.Path.(open_root / "dir6" /:* Resto.Arg.int)

  let dyn1 = Resto.Path.(root / "dyn1" /: Resto.Arg.int)

  let get_service =
    Service.get_service
      ~query:Resto.Query.empty
      ~output:Json_encoding.int
      ~error:Json_encoding.empty

  let open_root_service = get_service Resto.Path.open_root

  let s1 = get_service dir2

  let s2 = get_service dir1

  let s3 = get_service dir5

  let s4 = get_service dir4

  let s5 = open_root_service

  let s6 = open_root_service

  let s7 = open_root_service

  let s8 = get_service dir6

  let int_handler v () () = Lwt.return @@ `Ok v

  let register_list registration = List.fold_left registration Directory.empty

  let register0 v service directory =
    Directory.register0 directory service @@ int_handler v

  let register1 v service directory =
    Directory.register1 directory service @@ fun _ -> int_handler v

  let register2 v service directory =
    Directory.register2 directory service @@ fun _ _ -> int_handler v

  let register0_list v = register_list (fun d s -> register0 v s d)

  (* root
      ├── dir1
      │   ├── dir3
      │   │   └── dir5
      │   │       └── s3 ----> 0
      │   └── s2 ------------> 0
      ├── dir2
      │   └── s1 ------------> 0
      └── dyn1
          ├── <0>
          │    └── s5 -------> 0
          └── <2>
               └── s7 -------> 0 *)
  let left_dir =
    let v = 0 in
    let d = register0_list v [s1; s2; s3] in
    Directory.register_dynamic_directory1 d dyn1 (function
        | 0 -> Lwt.return @@ register1 v s5 Directory.empty
        | 2 -> Lwt.return @@ register1 v s7 Directory.empty
        | _ -> Lwt.return @@ Directory.empty)

  (* root
     ├── dir1
     │   ├── dir3
     │   │   └── dir5
     │   │       └── s3 ----> 1
     │   ├── dir4
     │   │   └── s4 --------> 1
     │   └── s2 ------------> 1
     └── dyn1
         ├── <1>
         │    └── s6 -------> 1
         └── <2>
              ├── dir6
              │   └── <_>*
              │        └── s8 ---> 1
              └── s7 -------> 1 *)
  let right_dir =
    let v = 1 in
    let d = register0_list v [s2; s3; s4] in
    Directory.register_dynamic_directory1 d dyn1 (function
        | 1 -> Lwt.return @@ register1 v s6 Directory.empty
        | 2 -> register1 v s7 Directory.empty |> register2 v s8 |> Lwt.return
        | _ -> Lwt.return @@ Directory.empty)

  let path_of_service s =
    let {Service.uri; _} = Service.forge_request s () () in
    uri

  let request dir uri =
    let string_uri = Uri.to_string uri in
    let path = Resto.Utils.decode_split_path @@ Uri.path uri in
    let* service = Directory.lookup dir () `GET path in
    let (Directory.Service s) =
      get_or_else
        (fun _ ->
          Alcotest.fail
            (Printf.sprintf "Could not locate the service %s" string_uri))
        service
    in
    let query = Resto.Query.parse s.types.query [] in
    let* answer =
      match s.types.input with
      | Service.No_input -> s.handler query ()
      | Service.Input input ->
          s.handler query (Json_encoding.destruct input (`O []))
    in
    match answer with
    | `Ok a ->
        let encoded_output = Json_encoding.construct s.types.output a in
        let decoded_output =
          try Json_encoding.(destruct int encoded_output)
          with _ ->
            Alcotest.fail
              (Printf.sprintf
                 "The output for sercive %s could not be deserialized as an int"
                 string_uri)
        in
        Lwt.return decoded_output
    | _ ->
        Alcotest.fail
          (Printf.sprintf
             "The query for service %s must be successful by precondition."
             string_uri)

  let check_call_results dir =
    let do_check (path, msg, expd) =
      let* answer = request dir path in
      Lwt.return @@ Alcotest.(check int) msg expd answer
    in
    Lwt_list.iter_s do_check

  let check_raises f =
    match f () with
    | exception _ -> Lwt.return_unit
    | _ -> Alcotest.fail "An exception was expected to raise"

  let tests =
    [
      ( "succeed to pick left",
        fun () ->
          (* root
             ├── dir1
             │   ├── dir3
             │   │   └── dir5
             │   │       └── s3 --------> 0 (Already existing in left)
             │   ├── dir4
             │   │   └── s4 ------------> 1 (Added from right)
             │   └── s2 ----------------> 0 (Already existing in left)
             ├── dir2
             │   └── s1 ----------------> 0 (Already existing in left)
             └── dyn1
                 ├── <0>
                 │    └── s5 ------------> 0 (Already existing in left)
                 ├── <1>
                 │    └── s6 ------------> 1 (Added from right)
                 └── <2>
                      ├── dir6
                      │   └── <_>*
                      │        └── s8 ---> 1 (Added from right)
                      └── s7 ------------> 0 (Already existing in left) *)
          let merged =
            Directory.merge ~strategy:`Pick_left left_dir right_dir
          in
          let grid =
            [
              (path_of_service s1, "s1", 0);
              (path_of_service s2, "s2", 0);
              (path_of_service s3, "s3", 0);
              (path_of_service s4, "s4", 1);
              (Uri.of_string "/dyn1/0", "s5", 0);
              (Uri.of_string "/dyn1/1", "s6", 1);
              (Uri.of_string "/dyn1/2", "s7", 0);
              (Uri.of_string "/dyn1/2/dir6", "s8", 1);
              (Uri.of_string "/dyn1/2/dir6/1/2/3/4", "s8*", 1);
            ]
          in
          check_call_results merged grid );
      ( "succeed to pick right",
        fun () ->
          (* root
             ├── dir1
             │   ├── dir3
             │   │   └── dir5
             │   │       └── s3 ---------> 1 (Already existing in right)
             │   ├── dir4
             │   │   └── s4 -------------> 1 (Already existing in right)
             │   └── s2 -----------------> 1 (Already existing in right)
             ├── dir2
             │   └── s1 -----------------> 0 (Added from left)
             └── dyn1
                 ├── <0>
                 │    └── s5 ------------> 0 (Added from left)
                 ├── <1>
                 │    └── s6 ------------> 1 (Already existing in right)
                 └── <2>
                      ├── dir6
                      │   └── <_>*
                      │        └── s8 ---> 1 (Added from right)
                      └── s7 ------------> 1 (Already existing in right) *)
          let merged =
            Directory.merge ~strategy:`Pick_right left_dir right_dir
          in
          let grid =
            [
              (path_of_service s1, "s1", 0);
              (path_of_service s2, "s2", 1);
              (path_of_service s3, "s3", 1);
              (path_of_service s4, "s4", 1);
              (Uri.of_string "/dyn1/0", "s5", 0);
              (Uri.of_string "/dyn1/1", "s6", 1);
              (Uri.of_string "/dyn1/2", "s7", 1);
              (Uri.of_string "/dyn1/2/dir6", "s8", 1);
              (Uri.of_string "/dyn1/2/dir6/1/2/3/4", "s8*", 1);
            ]
          in
          check_call_results merged grid );
      ( "fail with Conflict exception when requiring `Raise",
        fun () ->
          check_raises @@ fun () ->
          Directory.merge ~strategy:`Raise left_dir right_dir );
      ( "fail with Conflict exception by default",
        fun () -> check_raises @@ fun () -> Directory.merge left_dir right_dir
      );
    ]
end

module Conflicts = struct
  let test_conflict () =
    Lwt.return
    @@ Alcotest.check_raises
         "conflict is detected"
         (Fixtures.Directory.Conflict
            ([Static "foobar"; DynamicTail {name = "int"; descr = None}], CTail))
         Fixtures.Directory.add_tail_conflict

  let test_print_conflict () =
    Lwt.return
    @@
    try Fixtures.Directory.add_tail_conflict ()
    with exn ->
      let exn_str = Stdlib.Printexc.to_string exn in
      let exn_default_str = Stdlib.Printexc.to_string_default exn in
      Alcotest.(check @@ neg string)
        "exception string is not the default"
        exn_str
        exn_default_str

  let test_print_conflict_explanation () =
    Lwt.return
    @@
    try Fixtures.Directory.add_type_conflict ()
    with exn ->
      let exn_str = Stdlib.Printexc.to_string exn in
      let expected_str =
        "Conflict in registration of service: Type conflict between arguments: \
         found type int but type float was expected in /bar/<float>"
      in
      Alcotest.(check @@ string)
        "exception string is correct explanation"
        exn_str
        expected_str

  let tests =
    [
      ("conflict is detected", test_conflict);
      ("conflict is pretty-printed", test_print_conflict);
      ("conflict is correctly pretty-printed", test_print_conflict_explanation);
    ]
end

let () =
  Lwt_main.run
  @@ Alcotest_lwt.run
       "directory"
       [
         ("resolve_uri_desc", Util.do_test_lwt Resolve_uri_desc.tests);
         ("merge", Util.do_test_lwt Merge.tests);
         ("conflicts", Util.do_test_lwt Conflicts.tests);
       ]
