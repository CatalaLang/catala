(** Error formatting and helper functions *)

(** {1 Message content} *)

type message_position = { message : string option; position : Pos.t }
type message_content = { message : string; positions : message_position list }

let internal_error_prefix =
  "Internal Error, please report to \
   https://github.com/CatalaLang/catala/issues: "

let to_internal_error (content : message_content) : message_content =
  { content with message = internal_error_prefix ^ content.message }

type content_type = Error | Warning | Debug | Log

let emit_content (content : message_content) (typ : content_type) : unit =
  let { message = msg; positions = pos } = content in
  match !Cli.message_format_flag with
  | Cli.Human ->
    (match typ with
    | Warning -> Cli.warning_print
    | Error -> Cli.error_print
    | Debug -> Cli.debug_print
    | Log -> Cli.log_print)
      "%s%s%s" msg
      (if pos = [] then "" else "\n\n")
      (String.concat "\n\n"
         (List.map
            (fun (pos : message_position) ->
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
        | Warning -> Cli.warning_marker
        | Error -> Cli.error_marker
        | Debug -> Cli.debug_marker
        | Log -> Cli.log_marker)
        ()
    in
    (* The top message doesn't come with a position, which is not something the
       GNU standard allows. So we look the position list and put the top message
       everywhere there is not a more precise message. If we can'r find a
       position without a more precise message, we just take the first position
       in the list to pair with the message. *)
    (match typ with
    | Error -> Format.eprintf
    | Warning | Log | Debug -> Format.printf)
      "%s%s\n"
      (if
       pos != []
       && List.for_all
            (fun (pos' : message_position) -> Option.is_some pos'.message)
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
                (Pos.to_string_short pos'.position)
                severity
                (match pos'.message with
                | None -> remove_new_lines msg
                | Some msg' -> remove_new_lines msg'))
            pos))

(** {1 Error exception} *)

exception CompilerError of message_content

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
               List.map (fun (message, position) -> { message; position }) spans;
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
            List.map (fun (msg, pos) -> { message = msg; position = pos }) pos;
        }
        Warning)
    format

let emit_spanned_warning ?(span_msg : string option) (span : Pos.t) format =
  emit_multispanned_warning [span_msg, span] format

let emit_warning format = emit_multispanned_warning [] format
