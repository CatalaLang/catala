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

let has_color oc =
  match Cli.globals.color with
  | Cli.Never -> false
  | Always -> true
  | Auto -> Unix.(isatty (descr_of_out_channel oc))

(* Here we create new formatters to stderr/stdout that remain separate from the
   ones used by [Format.printf] / [Format.eprintf] (which remain unchanged) *)

let formatter_of_out_channel oc =
  let ppf = Format.formatter_of_out_channel oc in
  if has_color oc then color_formatter ppf else unstyle_formatter ppf

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

type content_type = Error | Warning | Debug | Log | Result

let get_ppf = function
  | Result -> Lazy.force std_ppf
  | Debug when not Cli.globals.debug -> Lazy.force ignore_ppf
  | Warning when Cli.globals.disable_warnings -> Lazy.force ignore_ppf
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
    | Result of message

  type t = message_element list

  let of_message (message : message) : t = [MainMessage message]
  let of_result (message : message) : t = [Result message]
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

  let of_string (s : string) : t =
    [MainMessage (fun ppf -> Format.pp_print_string ppf s)]

  let emit (content : t) (target : content_type) : unit =
    match Cli.globals.message_format with
    | Cli.Human ->
      let ppf = get_ppf target in
      Format.fprintf ppf "@[<hv>%t%t%a@]@." (pp_marker target)
        (fun (ppf : Format.formatter) ->
          match content, target with
          | MainMessage _ :: _, (Result | Error) -> Format.pp_print_space ppf ()
          | _ -> Format.pp_print_char ppf ' ')
        (fun (ppf : Format.formatter) (message_elements : t) ->
          Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,@,")
            (fun ppf (elt : message_element) ->
              match elt with
              | Position pos ->
                Option.iter
                  (fun msg -> Format.fprintf ppf "%t@," msg)
                  pos.pos_message;
                Pos.format_loc_text ppf pos.pos
              | MainMessage msg -> msg ppf
              | Result msg -> msg ppf
              | Suggestion suggestions_list ->
                Suggestions.format ppf suggestions_list)
            ppf message_elements)
        content
    | Cli.GNU ->
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
              pos, m
            | Position { pos_message; pos } ->
              let message =
                match pos_message with Some m -> m | None -> fun _ -> ()
              in
              Some pos, message
            | Result m -> None, m
            | Suggestion sl -> None, fun ppf -> Suggestions.format ppf sl
          in
          Option.iter
            (fun pos ->
              Format.fprintf ppf "@{<blue>%s@}: " (Pos.to_string_short pos))
            pos;
          pp_marker target ppf;
          Format.pp_print_char ppf ' ';
          Format.pp_print_string ppf (unformat message))
        ppf content;
      Format.pp_print_newline ppf ()
end

open Content

(** {1 Error exception} *)

exception CompilerError of Content.t

(** {1 Error printing} *)

let raise_spanned_error
    ?(span_msg : Content.message option)
    ?(suggestion = ([] : string list))
    (span : Pos.t)
    format =
  let continuation (message : Format.formatter -> unit) =
    raise
      (CompilerError
         ([MainMessage message; Position { pos_message = span_msg; pos = span }]
         @ match suggestion with [] -> [] | sugg -> [Suggestion sugg]))
  in
  Format.kdprintf continuation format

let raise_multispanned_error_full
    ?(suggestion = ([] : string list))
    (spans : (Content.message option * Pos.t) list)
    format =
  Format.kdprintf
    (fun message ->
      raise
        (CompilerError
           (MainMessage message
            :: List.map
                 (fun (pos_message, pos) -> Position { pos_message; pos })
                 spans
           @ match suggestion with [] -> [] | sugg -> [Suggestion sugg])))
    format

let raise_multispanned_error
    ?(suggestion = ([] : string list))
    (spans : (string option * Pos.t) list)
    format =
  raise_multispanned_error_full ~suggestion
    (List.map
       (fun (msg, pos) ->
         Option.map (fun s ppf -> Format.pp_print_string ppf s) msg, pos)
       spans)
    format

let raise_error format =
  Format.kdprintf
    (fun message -> raise (CompilerError [MainMessage message]))
    format

let raise_internal_error format =
  Format.kdprintf
    (fun message ->
      raise (CompilerError (Content.to_internal_error [MainMessage message])))
    format

(** {1 Warning printing}*)

let assert_internal_error condition fmt =
  if condition then raise_internal_error ("assertion failed: " ^^ fmt)
  else Format.ifprintf (Format.formatter_of_out_channel stdout) fmt

let emit_multispanned_warning
    (pos : (Content.message option * Pos.t) list)
    format =
  Format.kdprintf
    (fun message ->
      Content.emit
        (MainMessage message
        :: List.map
             (fun (pos_message, pos) -> Position { pos_message; pos })
             pos)
        Warning)
    format

let emit_spanned_warning
    ?(span_msg : Content.message option)
    (span : Pos.t)
    format =
  emit_multispanned_warning [span_msg, span] format

let emit_warning format = emit_multispanned_warning [] format

let emit_log format =
  Format.kdprintf (fun message -> Content.emit [MainMessage message] Log) format

let emit_debug format =
  Format.kdprintf
    (fun message -> Content.emit [MainMessage message] Debug)
    format

let emit_result format =
  Format.kdprintf
    (fun message -> Content.emit [MainMessage message] Result)
    format
