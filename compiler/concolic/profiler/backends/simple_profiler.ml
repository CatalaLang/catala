(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

open Profiler
module Encoding = Json_encoding.Make (Json_repr.Yojson)

let time ~cpu () =
  {
    wall = Unix.gettimeofday ();
    cpu = (if compute_cpu ~cpu then Some (Sys.time ()) else None);
  }

type stack_item = {
  id : id;
  time : time;
  report : report;
  verbosity : verbosity;
  cpu : bool option;
}

type stack = Toplevel of report | Cons of stack_item * stack
type scope = { id : id; verbosity : verbosity; time : time; cpu : bool option }
type state = { stack : stack; scopes : scope list; max_verbosity : verbosity }

let empty max_verbosity =
  {
    stack = Toplevel { aggregated = IdMap.empty; recorded = [] };
    scopes = [];
    max_verbosity;
  }

let aggregate ~cpu state verbosity id =
  {
    state with
    scopes = { id; verbosity; time = time ~cpu (); cpu } :: state.scopes;
  }

let record ~cpu state verbosity id =
  if state.scopes <> [] then aggregate ~cpu state verbosity id
  else
    let stack =
      Cons
        ( {
            id;
            time = time ~cpu ();
            report = { recorded = []; aggregated = IdMap.empty };
            verbosity;
            cpu;
          },
          state.stack )
    in
    { state with stack }

let rec merge
    {
      count = na;
      total = Span ta;
      children = contentsa;
      node_verbosity = verbosity_a;
    }
    {
      count = nb;
      total = Span tb;
      children = contentsb;
      node_verbosity = verbosity_b;
    } =
  {
    count = na + nb;
    total = Span (ta +* tb);
    children = merge_maps contentsa contentsb;
    node_verbosity = min verbosity_a verbosity_b;
  }

and merge_maps amap bmap =
  IdMap.merge
    (fun _ a b ->
      match a, b with
      | Some v, None | None, Some v -> Some v
      | None, None -> None
      | Some a, Some b -> Some (merge a b))
    amap bmap

let rec filter_verbosity_to_aggregated verbosity aggregated =
  IdMap.fold
    (fun id node acc ->
      let children = filter_verbosity_to_aggregated verbosity node.children in
      if node.node_verbosity <= verbosity then
        IdMap.add id { node with children } acc
      else merge_maps acc children)
    aggregated IdMap.empty

let rec aggregate_report { aggregated; recorded } =
  List.fold_left
    (fun acc (id, { start = _; duration; item_verbosity; contents }) ->
      let children = aggregate_report contents in
      let node =
        {
          count = 1;
          total = duration;
          node_verbosity = item_verbosity;
          children;
        }
      in
      IdMap.add id node acc)
    aggregated recorded

let rec filter_verbosity verbosity { aggregated; recorded } =
  let aggregated = filter_verbosity_to_aggregated verbosity aggregated in
  let aggregated, recorded =
    List.fold_left
      (fun (aggregated, recorded) (id, item) ->
        let filtered_contents = filter_verbosity verbosity item.contents in
        if item.item_verbosity <= verbosity then
          ( aggregated,
            (id, { item with contents = filtered_contents }) :: recorded )
        else
          merge_maps aggregated (aggregate_report filtered_contents), recorded)
      (aggregated, []) recorded
  in
  { aggregated; recorded = List.rev recorded }

let inc state report =
  let inc_report report preport =
    {
      aggregated = merge_maps report.aggregated preport.aggregated;
      recorded = List.rev_append report.recorded preport.recorded;
    }
  in
  let stack =
    match state.stack with
    | Toplevel preport -> Toplevel (inc_report report preport)
    | Cons (item, rest) ->
      Cons ({ item with report = inc_report report item.report }, rest)
  in
  { state with stack }

let span ~cpu state verbosity d ((ids, metadata) : string list * metadata) =
  let zero_time = zero_time ~cpu in
  let tids =
    List.rev_append
      (List.map (fun { id; _ } -> id, zero_time) state.scopes)
      (List.map (fun id -> (id, metadata), d) ids)
  in
  match ids with
  | [] -> (* Shhh, everything will be alright. *) state
  | _ :: _ ->
    let rec build_node = function
      | [] -> IdMap.empty
      | (id, d) :: tids ->
        let children = build_node tids in
        let count, total = if tids = [] then 1, d else 0, zero_time in
        IdMap.singleton id
          { count; total; children; node_verbosity = verbosity }
    in
    inc state { recorded = []; aggregated = build_node tids }

let mark state verbosity ids =
  span ~cpu:None state verbosity (zero_time ~cpu:None) ids

let stop_aggregate state verbosity d id scopes cpu =
  let tids =
    let s_scopes = List.map (fun { id; _ } -> id, d) scopes in
    List.rev ((id, d) :: s_scopes)
  in
  let rec build_node = function
    | [] -> IdMap.empty
    | (id, d) :: tids ->
      let children = build_node tids in
      let count, total = if tids = [] then 1, d else 0, zero_time ~cpu in
      IdMap.singleton id { count; total; children; node_verbosity = verbosity }
  in
  inc state { recorded = []; aggregated = build_node tids }

let stop : state -> state =
 fun state ->
  match state.scopes with
  | { id; verbosity; time = t0; cpu } :: scopes ->
    let d = Span (time ~cpu () -* t0) in
    let state = { state with scopes } in
    stop_aggregate state verbosity d id scopes cpu
  | [] ->
    let stop_report id start contents report item_verbosity cpu =
      let contents = { contents with recorded = List.rev contents.recorded } in
      let duration = Span (time ~cpu () -* start) in
      let recorded =
        (id, { start; duration; contents; item_verbosity }) :: report.recorded
      in
      { report with recorded }
    in
    let stack =
      match state.stack with
      | Cons
          ( { id; time = start; report = contents; verbosity; cpu },
            Cons
              ( {
                  id = pid;
                  time = pt0;
                  report;
                  verbosity = p_verbosity;
                  cpu = _;
                },
                rest ) ) ->
        Cons
          ( {
              id = pid;
              time = pt0;
              report = stop_report id start contents report verbosity cpu;
              verbosity = p_verbosity;
              cpu;
            },
            rest )
      | Cons
          ( { id; time = start; report = contents; verbosity; cpu },
            Toplevel report ) ->
        Toplevel (stop_report id start contents report verbosity cpu)
      | Toplevel _ -> (* Shhh, everything will be alright. *) state.stack
    in
    { state with stack }

let stamp ~cpu state verbosity id = stop (record ~cpu state verbosity id)
let time_pp_hum ppf t = Ptime.pp_rfc3339 ~frac_s:3 () ppf t

let pp_line ?toplevel_timestamp nindent ppf (id, metadata) n t =
  let id =
    match
      List.filter_map (function "text", v -> Some v | _ -> None) metadata
    with
    | [] -> (* format: id *) id
    | metadata ->
      (* format: id(m1;m2) *)
      let pp_sep fmt () = Format.pp_print_char fmt ';' in
      let pp_metadata fmt metadata =
        Format.pp_print_list ~pp_sep Format.pp_print_string fmt metadata
      in
      Format.asprintf "%s(%a)" id pp_metadata metadata
  in
  let indent = Stdlib.List.init nindent (fun _ -> "  ") in
  let () =
    Option.iter
      (fun t ->
        let time = Option.get (Ptime.of_float_s t.wall) in
        Format.fprintf ppf "%a@," time_pp_hum time)
      toplevel_timestamp
  in
  let indentsym =
    String.concat ""
      (indent
      @ [
          id;
          " ......................................................";
          "......................................................";
          "......................................................";
        ])
  in
  Format.fprintf ppf "%s %-7i " (String.sub indentsym 0 60) n;
  if t.wall = 0. then Format.fprintf ppf "                 "
  else if Option.is_some t.cpu then
    Format.fprintf ppf "% 10.3fms %3d%%" (t.wall *. 1000.)
      (int_of_float (100. *. (Option.value ~default:0. t.cpu /. t.wall)))
  else Format.fprintf ppf "% 10.3fms" (t.wall *. 1000.);
  Format.fprintf ppf "@,"

let rec pp_report ?(toplevel_call = true) t0 nident ppf { aggregated; recorded }
    =
  List.iter
    (fun (id, { start = t; duration = Span d; contents; item_verbosity = _ }) ->
      let toplevel_timestamp = if toplevel_call then Some t else None in
      pp_line ?toplevel_timestamp nident ppf id 1 d;
      pp_report ~toplevel_call:false t (nident + 1) ppf contents)
    recorded;
  IdMap.iter
    (fun id { count = n; total = Span d; children; node_verbosity = _ } ->
      pp_line nident ppf id n d;
      pp_report ~toplevel_call:false t0 (nident + 1) ppf
        { recorded = []; aggregated = children })
    aggregated

let pp_report ?t0 ppf report =
  let t0 =
    match t0 with
    | Some t0 -> t0
    | None -> (
      match report.recorded with
      | (_, { start; _ }) :: _ -> start
      | [] -> { wall = 0.; cpu = Some 0. })
  in
  Format.fprintf ppf "@[<v 0>%a@]" (pp_report ~toplevel_call:false t0 0) report

module Base (P : sig
  type t

  val get_state : t -> state
  val set_state : t -> state -> unit
  val output_report : (t -> report -> unit) option
end) =
struct
  let time ~cpu _ = time ~cpu ()

  let record ~cpu t verbosity id =
    P.set_state t @@ record ~cpu (P.get_state t) verbosity id

  let aggregate ~cpu t verbosity id =
    P.set_state t @@ aggregate ~cpu (P.get_state t) verbosity id

  let report ~cpu:_ t =
    match (P.get_state t).stack with
    | Toplevel { aggregated; recorded }
      when IdMap.cardinal aggregated > 0 || recorded <> [] ->
      P.set_state t @@ empty (P.get_state t).max_verbosity;
      let report = { aggregated; recorded = List.rev recorded } in
      Some (filter_verbosity (P.get_state t).max_verbosity report)
    | _ -> None

  let may_output =
    match P.output_report with
    | Some fn -> fun t -> Option.iter (fun r -> fn t r) (report ~cpu:None t)
    | None -> fun _ -> ()

  let stamp ~cpu t verbosity id =
    P.set_state t @@ stamp ~cpu (P.get_state t) verbosity id;
    may_output t

  let inc t report =
    P.set_state t @@ inc (P.get_state t) report;
    may_output t

  let mark t verbosity id =
    P.set_state t @@ mark (P.get_state t) verbosity id;
    may_output t

  let span ~cpu t verbosity d id =
    P.set_state t @@ span ~cpu (P.get_state t) verbosity d id;
    may_output t

  let stop t =
    P.set_state t @@ stop (P.get_state t);
    may_output t
end

type (_, _) Profiler.kind += Headless : (verbosity, state ref) Profiler.kind

module Headless = struct
  type nonrec state = state ref
  type config = verbosity

  let kind = Headless

  let encoding_case =
    let open Json_encoding in
    case ~title:"headless" ~description:"Headless driver" string
      (function View Headless -> Some "headless" | _ -> None)
      (fun _ -> View Headless)

  let create verbosity = ref (empty verbosity)

  include Base (struct
    type t = state

    let[@inline] get_state t = !t
    let[@inline] set_state t s = t := s
    let output_report = None
  end)

  let close _ = ()
end

let headless = (module Headless : DRIVER with type config = verbosity)

type output =
  | Closed of string
  | Open of string * out_channel * Format.formatter

type auto_writer_state = {
  mutable profiler_state : state;
  mutable output : output;
  time : time;
}

type file_format = Plain_text | Json

type (_, _) Profiler.kind +=
  | Auto_write_to_file :
      file_format
      -> (string * verbosity, auto_writer_state) Profiler.kind

let make_driver ~file_format =
  (module struct
    type nonrec state = auto_writer_state
    type config = string * verbosity

    let kind = Auto_write_to_file file_format

    let encoding_case =
      let title =
        match file_format with Plain_text -> "plain_text" | Json -> "json"
      in
      Json_encoding.(
        case ~title
          ~description:(Printf.sprintf "%s driver" title)
          (constant title)
          (function
            | View (Auto_write_to_file format) when format = file_format ->
              Some ()
            | _ -> None)
          (fun () -> View (Auto_write_to_file file_format)))

    let create (file_name, verbosity) =
      {
        profiler_state = empty verbosity;
        time = time ~cpu:None ();
        output = Closed file_name;
      }

    include Base (struct
      type t = state

      let[@inline] get_state t = t.profiler_state
      let[@inline] set_state t s = t.profiler_state <- s

      let writer_of =
        match file_format with
        | Plain_text ->
          fun formatter report time ->
            Format.fprintf formatter "%a%!" (pp_report ~t0:time) report
        | Json ->
          fun formatter report _time ->
            let encoded_report =
              Encoding.construct Profiler.report_encoding report
            in
            Yojson.Safe.pretty_print formatter encoded_report;
            Format.pp_print_newline formatter ()

      let output_report =
        Some
          (fun state report ->
            let ppf =
              match state.output with
              | Open (_, _, ppf) -> ppf
              | Closed fn ->
                let fp = open_out fn in
                let ppf = Format.formatter_of_out_channel fp in
                state.output <- Open (fn, fp, ppf);
                ppf
            in
            writer_of ppf report state.time)
    end)

    let close ({ output; _ } as state) =
      match output with
      | Open (fn, fp, ppf) ->
        Format.pp_print_newline ppf ();
        close_out fp;
        state.output <- Closed fn
      | Closed _ -> ()
  end : DRIVER
    with type config = string * verbosity)

let auto_write_as_txt_to_file = make_driver ~file_format:Plain_text
let auto_write_as_json_to_file = make_driver ~file_format:Json

(** Default profilers. *)
let rec create_dir ?(perm = 0o755) dir =
  let stat =
    try Some (Unix.stat dir) with Unix.Unix_error (ENOENT, _, _) -> None
  in
  match stat with
  | Some { st_kind = S_DIR; _ } -> ()
  | Some _ -> Stdlib.failwith "Not a directory"
  | None -> (
    create_dir ~perm (Filename.dirname dir);
    try Unix.mkdir dir perm
    with Unix.Unix_error (EEXIST, _, _) ->
      (* This is the case where the directory has been created at the same
         time. *)
      ())

let profiler
    ?(suffix = "")
    (backend : (module DRIVER with type config = string * verbosity))
    ~verbosity
    ~directory
    ~name =
  let output_dir =
    (* If [PROFILING_OUTPUT_DIR] environment variable is set, it overwrites the
       directory provided by the application *)
    let output_dir =
      Sys.getenv_opt "PROFILING_OUTPUT_DIR" |> Option.value ~default:directory
    in
    match Sys.is_directory output_dir with
    | true -> output_dir
    | false ->
      Format.ksprintf failwith
        "Error: Profiling output directory '%s' is not a directory." output_dir
    | exception Sys_error _ ->
      create_dir ~perm:0o777 output_dir;
      output_dir
  in
  let ( / ) = Filename.concat in
  Profiler.instance backend
    (output_dir / (name ^ "_profiling" ^ suffix), verbosity)

let () =
  Profiler_unix.Profiler_instance.register_backend ["text"; "txt"]
    (profiler ~suffix:".txt") auto_write_as_txt_to_file
