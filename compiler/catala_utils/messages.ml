(** Error formatting and helper functions *)

(**{1 Terminal formatting}*)

(**{2 Markers}*)

let time : float ref = ref (Unix.gettimeofday ())

let time_marker ppf () =
  let new_time = Unix.gettimeofday () in
  let old_time = !time in
  time := new_time;
  let delta = (new_time -. old_time) *. 1000. in
  if delta > 50. then
    Cli.format_with_style
      [ANSITerminal.Bold; ANSITerminal.black]
      ppf
      (Format.sprintf "[TIME] %.0fms@\n" delta)

(** Prints [\[DEBUG\]] in purple on the terminal standard output *)
let debug_marker ppf () =
  time_marker ppf ();
  Cli.format_with_style [ANSITerminal.Bold; ANSITerminal.magenta] ppf "[DEBUG] "

(** Prints [\[ERROR\]] in red on the terminal error output *)
let error_marker ppf () =
  Cli.format_with_style [ANSITerminal.Bold; ANSITerminal.red] ppf "[ERROR] "

(** Prints [\[WARNING\]] in yellow on the terminal standard output *)
let warning_marker ppf () =
  Cli.format_with_style
    [ANSITerminal.Bold; ANSITerminal.yellow]
    ppf "[WARNING] "

(** Prints [\[RESULT\]] in green on the terminal standard output *)
let result_marker ppf () =
  Cli.format_with_style [ANSITerminal.Bold; ANSITerminal.green] ppf "[RESULT] "

(** Prints [\[LOG\]] in red on the terminal error output *)
let log_marker ppf () =
  Cli.format_with_style [ANSITerminal.Bold; ANSITerminal.black] ppf "[LOG] "

(**{2 Printers}*)

(** All the printers below print their argument after the correct marker *)

let debug_format (format : ('a, Format.formatter, unit) format) =
  if !Cli.debug_flag then
    Format.printf ("%a@[<hov>" ^^ format ^^ "@]@.") debug_marker ()
  else Format.ifprintf Format.std_formatter format

let error_format (format : ('a, Format.formatter, unit) format) =
  Format.print_flush ();
  (* Flushes previous warnings *)
  Format.printf ("%a" ^^ format ^^ "\n%!") error_marker ()

let warning_format format =
  if !Cli.disable_warnings_flag then Format.ifprintf Format.std_formatter format
  else Format.printf ("%a" ^^ format ^^ "\n%!") warning_marker ()

let result_format format =
  Format.printf ("%a" ^^ format ^^ "\n%!") result_marker ()

let log_format format =
  Format.printf ("%a@[<hov>" ^^ format ^^ "@]@.") log_marker ()

(** {1 Message content} *)

module Content = struct
  type position = { message : string option; position : Pos.t }
  type t = { message : string; positions : position list }

  let of_message (s : string) : t = { message = s; positions = [] }
end

let internal_error_prefix =
  "Internal Error, please report to \
   https://github.com/CatalaLang/catala/issues: "

let to_internal_error (content : Content.t) : Content.t =
  { content with message = internal_error_prefix ^ content.message }

type content_type = Error | Warning | Debug | Log | Result

let emit_content (content : Content.t) (typ : content_type) : unit =
  let { Content.message = msg; positions = pos } = content in
  match !Cli.message_format_flag with
  | Cli.Human ->
    (match typ with
    | Warning -> warning_format
    | Error -> error_format
    | Debug -> debug_format
    | Log -> log_format
    | Result -> result_format)
      "%s%s%s" msg
      (if pos = [] then "" else "\n\n")
      (String.concat "\n\n"
         (List.map
            (fun (pos : Content.position) ->
              Printf.sprintf "%s%s"
                (match pos.message with None -> "" | Some msg -> msg ^ "\n")
                (Pos.retrieve_loc_text pos.position))
            pos))
  | Cli.GNU ->
    let remove_new_lines s =
      Re.replace ~all:true
        (Re.compile (Re.seq [Re.char '\n'; Re.rep Re.blank]))
        ~f:(fun _ -> " ")
        s
    in
    let severity =
      Format.asprintf "%a"
        (match typ with
        | Warning -> warning_marker
        | Error -> error_marker
        | Debug -> debug_marker
        | Log -> log_marker
        | Result -> result_marker)
        ()
    in
    (* The top message doesn't come with a position, which is not something the
       GNU standard allows. So we look the position list and put the top message
       everywhere there is not a more precise message. If we can'r find a
       position without a more precise message, we just take the first position
       in the list to pair with the message. *)
    (match typ with
    | Error -> Format.eprintf
    | Warning | Log | Debug | Result -> Format.printf)
      "%s%s\n"
      (if
       pos != []
       && List.for_all
            (fun (pos' : Content.position) -> Option.is_some pos'.message)
            pos
      then
       Format.asprintf "%a: %s %s\n"
         (Cli.format_with_style [ANSITerminal.blue])
         (Pos.to_string_short (List.hd pos).position)
         severity (remove_new_lines msg)
      else "")
      (String.concat "\n"
         (List.map
            (fun pos' ->
              Format.asprintf "%a: %s %s"
                (Cli.format_with_style [ANSITerminal.blue])
                (Pos.to_string_short pos'.Content.position)
                severity
                (match pos'.message with
                | None -> remove_new_lines msg
                | Some msg' -> remove_new_lines msg'))
            pos))

(** {1 Error exception} *)

exception CompilerError of Content.t

(** {1 Error printing} *)

let raise_spanned_error ?(span_msg : string option) (span : Pos.t) format =
  Format.kasprintf
    (fun msg ->
      raise
        (CompilerError
           {
             message = msg;
             positions = [{ message = span_msg; position = span }];
           }))
    format

let raise_multispanned_error (spans : (string option * Pos.t) list) format =
  Format.kasprintf
    (fun msg ->
      raise
        (CompilerError
           {
             message = msg;
             positions =
               List.map
                 (fun (message, position) -> { Content.message; position })
                 spans;
           }))
    format

let raise_error format =
  Format.kasprintf
    (fun msg -> raise (CompilerError { message = msg; positions = [] }))
    format

let raise_internal_error format =
  raise_error ("%s" ^^ format) internal_error_prefix

(** {1 Warning printing}*)

let assert_internal_error condition fmt =
  if condition then raise_internal_error ("assertion failed: " ^^ fmt)
  else Format.ifprintf (Format.formatter_of_out_channel stdout) fmt

let emit_multispanned_warning (pos : (string option * Pos.t) list) format =
  Format.kasprintf
    (fun msg ->
      emit_content
        {
          message = msg;
          positions =
            List.map
              (fun (msg, pos) -> { Content.message = msg; position = pos })
              pos;
        }
        Warning)
    format

let emit_spanned_warning ?(span_msg : string option) (span : Pos.t) format =
  emit_multispanned_warning [span_msg, span] format

let emit_warning format = emit_multispanned_warning [] format

let emit_log format =
  Format.kasprintf
    (fun msg -> emit_content { message = msg; positions = [] } Log)
    format

let emit_debug format =
  Format.kasprintf
    (fun msg -> emit_content { message = msg; positions = [] } Debug)
    format

let emit_result format =
  Format.kasprintf
    (fun msg -> emit_content { message = msg; positions = [] } Result)
    format
