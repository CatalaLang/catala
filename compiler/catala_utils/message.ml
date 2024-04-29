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

let terminal_columns, set_terminal_width_function =
  let get_cols = ref (fun () -> 80) in
  (fun () -> !get_cols ()), fun f -> get_cols := f

(* Note: we could do the same for std_formatter, err_formatter... but we'd
   rather promote the use of the formatting functions of this module and the
   below std_ppf / err_ppf *)

let has_color_raw ~(tty : bool Lazy.t) =
  match Global.options.color with
  | Global.Never -> false
  | Always -> true
  | Auto -> Lazy.force tty

let has_color oc =
  has_color_raw ~tty:(lazy Unix.(isatty (descr_of_out_channel oc)))

(* Here we create new formatters to stderr/stdout that remain separate from the
   ones used by [Format.printf] / [Format.eprintf] (which remain unchanged) *)

let formatter_of_out_channel oc =
  let tty = lazy Unix.(isatty (descr_of_out_channel oc)) in
  let ppf = Format.formatter_of_out_channel oc in
  let ppf =
    if has_color_raw ~tty then color_formatter ppf else unstyle_formatter ppf
  in
  let out, flush = Format.pp_get_formatter_output_functions ppf () in
  let flush () =
    if Lazy.force tty then Format.pp_set_margin ppf (terminal_columns ());
    flush ()
  in
  Format.pp_set_formatter_output_functions ppf out flush;
  ppf

let std_ppf = lazy (formatter_of_out_channel stdout)
let err_ppf = lazy (formatter_of_out_channel stderr)
let ignore_ppf = lazy (Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()))

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

(**{2 Message types and output helpers *)

type level = Error | Warning | Debug | Log | Result

let get_ppf = function
  | Result -> Lazy.force std_ppf
  | Debug when not Global.options.debug -> Lazy.force ignore_ppf
  | Warning when Global.options.disable_warnings -> Lazy.force ignore_ppf
  | Error | Log | Debug | Warning -> Lazy.force err_ppf

(**{3 Markers}*)

let print_time_marker =
  let time : float ref = ref (Unix.gettimeofday ()) in
  fun ppf () ->
    let new_time = Unix.gettimeofday () in
    let old_time = !time in
    time := new_time;
    let delta = (new_time -. old_time) *. 1000. in
    if delta > 50. then
      Format.fprintf ppf "@{<bold;black>[TIME] %.0fms@}@\n" delta

let pp_marker target ppf =
  let open Ocolor_types in
  let tags, str =
    match target with
    | Debug -> [Bold; Fg (C4 magenta)], "[DEBUG]"
    | Error -> [Bold; Fg (C4 red)], "[ERROR]"
    | Warning -> [Bold; Fg (C4 yellow)], "[WARNING]"
    | Result -> [Bold; Fg (C4 green)], "[RESULT]"
    | Log -> [Bold; Fg (C4 black)], "[LOG]"
  in
  if target = Debug then print_time_marker ppf ();
  Format.pp_open_stag ppf (Ocolor_format.Ocolor_styles_tag tags);
  Format.pp_print_string ppf str;
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

  let emit (content : t) (target : level) : unit =
    match Global.options.message_format with
    | Global.Human ->
      let ppf = get_ppf target in
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
            Format.fprintf ppf "@[<hov 2>%t %t@]" (pp_marker target) msg
          | Outcome msg ->
            Format.fprintf ppf "@[<hov>%t@ %t@]" (pp_marker target) msg
          | Suggestion suggestions_list ->
            Suggestions.format ppf suggestions_list)
        ppf content;
      Format.pp_close_box ppf ();
      Format.pp_print_newline ppf ()
    | Global.GNU ->
      (* The top message doesn't come with a position, which is not something
         the GNU standard allows. So we look the position list and put the top
         message everywhere there is not a more precise message. If we can't
         find a position without a more precise message, we just take the first
         position in the list to pair with the message. *)
      let ppf = get_ppf target in
      Format.pp_print_list ~pp_sep:Format.pp_print_newline
        (fun ppf elt ->
          let pos, message =
            match elt with
            | MainMessage m ->
              let pos =
                List.find_map
                  (function
                    | Position { pos_message = None; pos } -> Some pos
                    | _ -> None)
                  content
                |> function
                | None ->
                  List.find_map
                    (function
                      | Position { pos_message = _; pos } -> Some pos
                      | _ -> None)
                    content
                | some -> some
              in
              pos, Some m
            | Position { pos_message; pos } -> Some pos, pos_message
            | Outcome m -> None, Some m
            | Suggestion sl -> None, Some (fun ppf -> Suggestions.format ppf sl)
          in
          Option.iter
            (fun pos ->
              Format.fprintf ppf "@{<blue>%s@}: " (Pos.to_string_short pos))
            pos;
          pp_marker target ppf;
          match message with
          | Some message ->
            Format.pp_print_char ppf ' ';
            Format.pp_print_string ppf (unformat message)
          | None -> ())
        ppf content;
      Format.pp_print_newline ppf ()
end

open Content

(** {1 Error exception} *)

exception CompilerError of Content.t

(** {1 Error printing} *)

type ('a, 'b) emitter =
  ?header:Content.message ->
  ?internal:bool ->
  ?pos:Pos.t ->
  ?pos_msg:Content.message ->
  ?extra_pos:(string * Pos.t) list ->
  ?fmt_pos:(Content.message * Pos.t) list ->
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
    ?(suggestion = [])
    ~cont
    ~level =
  Format.kdprintf
  @@ fun message ->
  let t =
    match level with Result -> of_result message | _ -> of_message message
  in
  let t = match header with Some h -> prepend_message t h | None -> t in
  let t = if internal then to_internal_error t else t in
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
let warning = make ~level:Warning ~cont:emit
let error = make ~level:Error ~cont:(fun m _ -> raise (CompilerError m))
