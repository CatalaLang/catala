(* This file is part of the Catala compiler, a specification language for tax
   and social benefits computation rules. Copyright (C) 2023 Inria,
   contributors: Denis Merigoux <denis.merigoux@inria.fr>, Louis Gesbert
   <louis.gesbert@inria.fr>

   Licensed under the Apache License, Version 2.0 (the "License"); you may not
   use this file except in compliance with the License. You may obtain a copy of
   the License at

   http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
   License for the specific language governing permissions and limitations under
   the License. *)

(** Error formatting and helper functions *)

(**{1 Terminal formatting}*)

(* Adds handling of color tags in the formatter *)
let color_formatter ppf =
  Ocolor_format.prettify_formatter ppf;
  ppf

(* Sets handling of tags in the formatter to ignore them (don't print any color
   codes) *)
let unstyle_formatter ppf =
  Format.pp_set_mark_tags ppf false;
  ppf

(* SIDE EFFECT AT MODULE LOAD: this turns on handling of tags in
   [Format.sprintf] etc. functions (ignoring them) *)
let () = ignore (unstyle_formatter Format.str_formatter)

(* Note: we could do the same for std_formatter, err_formatter... but we'd
   rather promote the use of the formatting functions of this module and the
   below std_ppf / err_ppf *)

let terminal_columns, set_terminal_width_function =
  let get_cols = ref (fun () -> 80) in
  (fun () -> !get_cols ()), fun f -> get_cols := f

let has_color_raw ~(tty : bool Lazy.t) =
  match Global.options.color with
  | Global.Never -> false
  | Always -> true
  | Auto -> Lazy.force tty

let has_color oc =
  has_color_raw ~tty:(lazy Unix.(isatty (descr_of_out_channel oc)))

(* Here we create new formatters to stderr/stdout that remain separate from the
   ones used by [Format.printf] / [Format.eprintf] (which remain unchanged) *)

let formatter_of_out_channel ?(nocolor = false) oc =
  let tty = lazy Unix.(isatty (descr_of_out_channel oc)) in
  let ppf =
    lazy
      (let ppf = Format.formatter_of_out_channel oc in
       if (not nocolor) && has_color_raw ~tty then color_formatter ppf
       else unstyle_formatter ppf)
  in
  fun () ->
    let ppf = Lazy.force ppf in
    if Lazy.force tty then Format.pp_set_margin ppf (terminal_columns ());
    ppf

let std_ppf =
  let ppf = lazy (formatter_of_out_channel stdout ()) in
  fun () -> Lazy.force ppf

let err_ppf =
  let ppf = lazy (formatter_of_out_channel stderr ()) in
  fun () -> Lazy.force ppf

let ignore_ppf =
  let ppf = lazy (Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())) in
  fun () -> Lazy.force ppf

let unformat (f : Format.formatter -> unit) : string =
  let buf = Buffer.create 1024 in
  let ppf = unstyle_formatter (Format.formatter_of_buffer buf) in
  Format.pp_set_margin ppf max_int;
  (* We won't print newlines anyways, but better not have them in the first
     place (this wouldn't remove cuts in a vbox for example) *)
  let out_funs = Format.pp_get_formatter_out_functions ppf () in
  Format.pp_set_formatter_out_functions ppf
    {
      out_funs with
      Format.out_newline = (fun () -> out_funs.out_string " " 0 1);
      Format.out_indent = (fun _ -> ());
    };
  f ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let pad n s ppf = Pos.pad_fmt n s ppf

(** {2 Message types and output helpers} *)

type level = Error | Warning | Debug | Log | Result

let get_ppf = function
  | Result -> std_ppf ()
  | Debug when not Global.options.debug -> ignore_ppf ()
  | Warning when Global.options.disable_warnings -> ignore_ppf ()
  | Error | Log | Debug | Warning -> err_ppf ()

(**{3 Markers}*)

let print_time_marker =
  let time : float ref = ref (Unix.gettimeofday ()) in
  fun ppf () ->
    let new_time = Unix.gettimeofday () in
    let old_time = !time in
    time := new_time;
    let delta = (new_time -. old_time) *. 1000. in
    if delta > 50. then
      Format.fprintf ppf
        "[@{<bold;magenta>DEBUG@}] @{<bold;black>- %.0fms elapsed -@}@," delta

let pp_marker ?extra_label target ppf =
  let open Ocolor_types in
  let color, str =
    match target with
    | Debug -> magenta, "DEBUG"
    | Error -> red, "ERROR"
    | Warning -> yellow, "WARNING"
    | Result -> green, "RESULT"
    | Log -> black, "LOG"
  in
  Format.pp_open_stag ppf (Ocolor_format.Ocolor_style_tag (Fg (C4 color)));
  Format.pp_open_stag ppf (Ocolor_format.Ocolor_style_tag Bold);
  Format.pp_print_string ppf str;
  Format.pp_close_stag ppf ();
  extra_label
  |> Option.iter (fun lbl ->
         Format.pp_print_char ppf ' ';
         Format.pp_print_string ppf lbl);
  Format.pp_close_stag ppf ()

(**{2 Printers}*)

(** {1 Message content} *)

module Content = struct
  type message = Format.formatter -> unit
  type position = { pos_message : message option; pos : Pos.t }

  type message_element =
    | MainMessage of message
    | Position of position
    | Suggestion of string list
    | Outcome of message

  type t = message_element list

  let of_message (message : message) : t = [MainMessage message]
  let of_result (message : message) : t = [Outcome message]
  let prepend_message (content : t) prefix : t = MainMessage prefix :: content

  let to_internal_error (content : t) : t =
    let internal_error_prefix ppf =
      Format.pp_print_string ppf
        "Internal Error, please report to \
         https://github.com/CatalaLang/catala/issues."
    in
    prepend_message content internal_error_prefix

  let add_suggestion (content : t) (suggestion : string list) =
    content @ [Suggestion suggestion]

  let add_position (content : t) ?(message : message option) (position : Pos.t)
      =
    content @ [Position { pos = position; pos_message = message }]

  let of_string (s : string) : t =
    [MainMessage (fun ppf -> Format.pp_print_text ppf s)]

  let basic_msg ?header ppf target content =
    let pp_header ppf = Option.iter (Format.fprintf ppf " %s: ") header in
    Format.pp_open_vbox ppf 0;
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,@,")
      (fun ppf -> function
        | Position pos ->
          Option.iter
            (fun msg -> Format.fprintf ppf "@[<hov>%t@]@," msg)
            pos.pos_message;
          Pos.format_loc_text ppf pos.pos
        | MainMessage msg ->
          if target = Debug then print_time_marker ppf ();
          Format.fprintf ppf "@[<hov 2>[%t] %t%t@]" (pp_marker target) pp_header
            msg
        | Outcome msg ->
          Format.fprintf ppf "@[<hov>[%t]@ %t%t@]" (pp_marker target) pp_header
            msg
        | Suggestion suggestions_list -> Suggestions.format ppf suggestions_list)
      ppf content;
    Format.pp_close_box ppf ();
    Format.pp_print_newline ppf ()

  let fancy_msg ?header ppf target content =
    let ppf_out_fcts = Format.pp_get_formatter_out_functions ppf () in
    let restore_ppf () =
      Format.pp_print_flush ppf ();
      Format.pp_set_formatter_out_functions ppf ppf_out_fcts
    in
    let getcolorstr pp =
      let buf = Buffer.create 17 in
      let ppfb = Format.formatter_of_buffer buf in
      Format.pp_set_formatter_stag_functions ppfb
        (Format.pp_get_formatter_stag_functions ppf ());
      Format.pp_set_mark_tags ppfb (Format.pp_get_mark_tags ppf ());
      pp ppfb;
      Format.pp_print_flush ppfb ();
      Buffer.contents buf
    in
    (* The following adds a blue line on the left *)
    Format.pp_set_formatter_out_functions ppf
      {
        ppf_out_fcts with
        out_indent =
          (fun n ->
            let lead =
              getcolorstr (fun ppf -> Format.fprintf ppf "@{<blue>@<1>%s@}" "│")
            in
            if n >= 1 then ppf_out_fcts.out_string lead 0 (String.length lead);
            if n >= 2 then ppf_out_fcts.out_indent (n - 1));
      };
    Format.pp_open_vbox ppf 1;
    Format.fprintf ppf "@{<blue>@<2>%s[%t]@<2>%s@}" "┌─" (pp_marker target) "─";
    Option.iter (fun h -> Format.fprintf ppf " %s @{<blue>─@}" h) header;
    (* Returns true when a finaliser is needed *)
    let print_elt ppf ?(islast = false) = function
      | MainMessage msg ->
        Format.fprintf ppf "@,@[<v 2>@,@[<hov>%t@]@]" msg;
        if islast then Format.pp_print_cut ppf ();
        true
      | Position pos ->
        Format.pp_print_cut ppf ();
        if Pos.get_file pos.pos = "" then (
          Format.pp_print_break ppf 0 (-1);
          restore_ppf ();
          Format.fprintf ppf "@{<blue>@<3>%s@}%s" "└─"
            (if Global.options.debug then " (no position information)" else "");
          false)
        else (
          Option.iter
            (fun msg -> Format.fprintf ppf "@[<v 1>@,@[<hov 2>%t@]@]" msg)
            pos.pos_message;
          Format.pp_print_break ppf 0 (-1);
          let pr_head, pr_context, pr_legal =
            Pos.format_loc_text_parts pos.pos
          in
          Format.pp_open_vbox ppf 2;
          Format.fprintf ppf "@{<blue>@<1>%s@}%t" "├" pr_head;
          pr_context ppf;
          Format.pp_close_box ppf ();
          match pr_legal with
          | None -> true
          | Some pr_legal ->
            Format.pp_print_break ppf 0 (-1);
            if islast then (
              restore_ppf ();
              Format.pp_open_vbox ppf 3;
              Format.fprintf ppf "@{<blue>@<3>%s@}%t" "└─ " pr_legal)
            else (
              Format.pp_open_vbox ppf 3;
              Format.fprintf ppf "@{<blue>@<3>%s@}%t" "├─ " pr_legal);
            Format.pp_close_box ppf ();
            not islast)
      | Outcome msg ->
        Format.fprintf ppf "@;<0 1>@[<v 1>@[<hov 2>%t@]@]" msg;
        true
      | Suggestion suggestions_list ->
        Format.fprintf ppf "@,@[<v 1>@,@[<hov 2>%a@]@]" Suggestions.format
          suggestions_list;
        true
    in
    let rec print_lines ppf = function
      | [elt] ->
        let finalise = print_elt ppf ~islast:true elt in
        Format.pp_close_box ppf ();
        if finalise then Format.fprintf ppf "@,@{<blue>@<2>%s@}" "└─"
      | elt :: r ->
        let _ = print_elt ppf elt in
        print_lines ppf r
      | [] ->
        Format.pp_close_box ppf ();
        Format.pp_print_cut ppf ()
    in
    print_lines ppf content;
    Format.pp_close_box ppf ();
    restore_ppf ();
    Format.pp_print_newline ppf ()

  let gnu_msg ?header ppf target content =
    (* The top message doesn't come with a position, which is not something the
       GNU standard allows. So we look the position list and put the top message
       everywhere there is not a more precise message. If we can't find a
       position without a more precise message, we just take the first position
       in the list to pair with the message. *)
    let first_pos_elt =
      List.find_map
        (function
          | Position { pos_message = None; pos } as e -> Some (pos, e)
          | _ -> None)
        content
      |> function
      | None ->
        List.find_map
          (function
            | Position { pos_message = _; pos } as e -> Some (pos, e)
            | _ -> None)
          content
      | some -> some
    in
    List.iter
      (fun elt ->
        let pos, message =
          match elt with
          | MainMessage m -> Option.map fst first_pos_elt, Some m
          | Position { pos_message; pos } ->
            if
              List.exists
                (function MainMessage _ -> true | _ -> false)
                content
              && Some elt = Option.map snd first_pos_elt
            then None, None (* Avoid redundant positions *)
            else Some pos, pos_message
          | Outcome m -> None, Some m
          | Suggestion sl -> None, Some (fun ppf -> Suggestions.format ppf sl)
        in
        if pos = None && message = None then ()
        else (
          Option.iter
            (fun pos ->
              Format.fprintf ppf "@{<blue>%s@}: " (Pos.to_string_short pos))
            pos;
          Format.fprintf ppf "[%t]" (pp_marker target);
          Option.iter (fun h -> Format.fprintf ppf " %s" h) header;
          Option.iter
            (fun message ->
              if header <> None then Format.pp_print_char ppf ':';
              Format.pp_print_char ppf ' ';
              Format.pp_print_string ppf (unformat message))
            message;
          Format.pp_print_newline ppf ()))
      content

  let lsp_msg ppf content =
    (* Hypothesis: [MainMessage] is always part of a content list. *)
    let rec retrieve_message acc = function
      | [] -> acc
      | MainMessage m :: _ -> Some m
      | Outcome m :: t ->
        retrieve_message (match acc with None -> Some m | _ -> acc) t
      | (Position _ | Suggestion _) :: t -> retrieve_message acc t
    in
    let msg = retrieve_message None content in
    Option.iter (fun msg -> Format.fprintf ppf "%s" (unformat msg)) msg

  let emit_raw ?ppf ?header (content : t) (target : level) : unit =
    let ppf = Option.value ~default:(get_ppf target) ppf in
    match Global.options.message_format with
    | Global.Human -> (
      match target with
      | Debug | Log -> basic_msg ?header ppf target content
      | Result | Warning | Error -> fancy_msg ?header ppf target content)
    | GNU -> gnu_msg ?header ppf target content
    | Lsp -> lsp_msg ppf content

  let emit_n ?ppf (errs : t list) (target : level) =
    match errs with
    | [content] -> emit_raw ?ppf content target
    | contents ->
      let ppf = Option.value ~default:(get_ppf target) ppf in
      let len = List.length contents in
      List.iteri
        (fun i c ->
          let header = Printf.sprintf "%d/%d" (succ i) len in
          emit_raw ~ppf ~header c target)
        contents

  let emit ?ppf (content : t) (target : level) = emit_raw ?ppf content target
end

open Content

(** {1 Error exception} *)

exception CompilerError of Content.t
exception CompilerErrors of Content.t list

type lsp_error_kind =
  | Lexing
  | Parsing
  | Typing
  | Generic
  | Warning
  | AssertFailure

type lsp_error = {
  kind : lsp_error_kind;
  message : Format.formatter -> unit;
  pos : Pos.t option;
  suggestion : string list option;
}

let global_error_hook = ref None

let register_lsp_error_notifier f =
  global_error_hook :=
    Some
      (fun err ->
        f err;
        true)

let register_lsp_error_absorber f = global_error_hook := Some f

(** {1 Error printing} *)

type ('a, 'b) emitter =
  ?header:Content.message ->
  ?internal:bool ->
  ?pos:Pos.t ->
  ?pos_msg:Content.message ->
  ?extra_pos:(string * Pos.t) list ->
  ?fmt_pos:(Content.message * Pos.t) list ->
  ?outcome:Content.message list ->
  ?suggestion:string list ->
  ('a, Format.formatter, unit, 'b) format4 ->
  'a

let make
    ?header
    ?(internal = false)
    ?pos
    ?pos_msg
    ?extra_pos
    ?fmt_pos
    ?(outcome = [])
    ?(suggestion = [])
    ~cont
    ~level =
  match level with
  | Debug when not Global.options.debug ->
    Format.ikfprintf (fun _ -> cont [] level) (ignore_ppf ())
  | Warning when Global.options.disable_warnings ->
    Format.ikfprintf (fun _ -> cont [] level) (ignore_ppf ())
  | _ ->
    Format.kdprintf
    @@ fun message ->
    let t =
      match level with Result -> of_result message | _ -> of_message message
    in
    let t = match header with Some h -> prepend_message t h | None -> t in
    let t = if internal then to_internal_error t else t in
    let t =
      match outcome with [] -> t | o -> t @ List.map (fun o -> Outcome o) o
    in
    let t =
      match pos with Some p -> add_position t ?message:pos_msg p | None -> t
    in
    let t =
      match extra_pos with
      | Some pl ->
        List.fold_left
          (fun t (message, p) ->
            let message =
              if message = "" then None
              else Some (fun ppf -> Format.pp_print_text ppf message)
            in
            add_position t ?message p)
          t pl
      | None -> t
    in
    let t =
      match fmt_pos with
      | Some pl ->
        List.fold_left
          (fun t (message, p) ->
            let message = if message == ignore then None else Some message in
            add_position t ?message p)
          t pl
      | None -> t
    in
    let t = match suggestion with [] -> t | s -> add_suggestion t s in
    cont t level

let debug = make ~level:Debug ~cont:emit
let log = make ~level:Log ~cont:emit
let result = make ~level:Result ~cont:emit

let results ?title r =
  emit_raw ?header:title (List.flatten (List.map of_result r)) Result

let join_pos ~pos ~fmt_pos ~extra_pos =
  (* Error positioning might be provided using multiple options. Thus, we look
     for each of them and prioritize in this order [fmt_pos] > [extra_pos] >
     [pos] if multiple positions are present. *)
  match fmt_pos, extra_pos, pos with
  | Some ((_, pos) :: _), _, _ | _, Some ((_, pos) :: _), _ | _, _, Some pos ->
    Some pos
  | _ -> None

let warning
    ?header
    ?internal
    ?pos
    ?pos_msg
    ?extra_pos
    ?fmt_pos
    ?outcome
    ?suggestion
    fmt =
  make ?header ?internal ?pos ?pos_msg ?extra_pos ?fmt_pos ?outcome ?suggestion
    fmt ~level:Warning ~cont:(fun m x ->
      Option.iter
        (fun f ->
          let message ppf = Content.emit ~ppf m Warning in
          let pos = join_pos ~pos ~fmt_pos ~extra_pos in
          ignore (f { kind = Warning; message; pos; suggestion }))
        !global_error_hook;
      emit m x)

let error ?(kind = Generic) : ('a, 'exn) emitter =
 fun ?header ?internal ?pos ?pos_msg ?extra_pos ?fmt_pos ?outcome ?suggestion
     fmt ->
  make ?header ?internal ?pos ?pos_msg ?extra_pos ?fmt_pos ?outcome ?suggestion
    fmt ~level:Error ~cont:(fun m _ ->
      Option.iter
        (fun f ->
          let message ppf = Content.emit ~ppf m Error in
          let pos = join_pos ~pos ~fmt_pos ~extra_pos in
          ignore (f { kind; message; pos; suggestion }))
        !global_error_hook;
      raise (CompilerError m))

(* Multiple errors handling *)

type global_errors = {
  mutable errors : t list option;
  mutable stop_on_error : bool;
}

let global_errors = { errors = None; stop_on_error = false }

let delayed_error ?(kind = Generic) x : ('a, 'exn) emitter =
 fun ?header ?internal ?pos ?pos_msg ?extra_pos ?fmt_pos ?outcome ?suggestion
     fmt ->
  make ?header ?internal ?pos ?pos_msg ?extra_pos ?fmt_pos ?outcome ?suggestion
    fmt ~level:Error ~cont:(fun m _ ->
      let register_error =
        match !global_error_hook with
        | Some f ->
          let message ppf = Content.emit ~ppf m Error in
          let pos = join_pos ~pos ~fmt_pos ~extra_pos in
          f { kind; message; pos; suggestion }
        | None -> true
      in
      if register_error then (
        if global_errors.stop_on_error then raise (CompilerError m);
        match global_errors.errors with
        | None ->
          error ~internal:true
            "delayed error called outside scope: encapsulate using \
             'with_delayed_errors' first"
        | Some l ->
          global_errors.errors <- Some (m :: l);
          x)
      else x)

let with_delayed_errors
    ?(stop_on_error = Global.options.stop_on_error)
    (f : unit -> 'a) : 'a =
  (match global_errors.errors with
  | None -> global_errors.errors <- Some []
  | Some _ -> error ~internal:true "nested call to 'with_delayed_errors'");
  global_errors.stop_on_error <- stop_on_error;
  let result =
    match f () with
    | r -> Either.Left r
    | exception CompilerError err ->
      let bt = Printexc.get_raw_backtrace () in
      Either.Right (err, bt)
    | exception e ->
      global_errors.errors <- None;
      raise e
  in
  let errs = global_errors.errors in
  global_errors.errors <- None;
  match errs, result with
  | (None | Some []), Either.Right (e, bt) ->
    Printexc.raise_with_backtrace (CompilerError e) bt
  | None, Either.Left _ ->
    error ~internal:true "intertwined delayed error scope"
  | Some [], Either.Left result -> result
  | Some [err], Either.Left _ -> raise (CompilerError err)
  | Some errs, Either.Left _ -> raise (CompilerErrors (List.rev errs))
  | Some errs, Either.Right (err, bt) ->
    Printexc.raise_with_backtrace (CompilerErrors (List.rev (err :: errs))) bt
