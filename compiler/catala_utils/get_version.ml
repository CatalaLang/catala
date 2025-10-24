(** This trivial binary is run at build-time to get the correct version from the
    build environment (either the CATALA_VERSION) environment variable if
    defined, or `git describe`, or resorting to just "dev" if none of these can
    be found *)

let v =
  match Sys.getenv_opt "CATALA_VERSION" with
  | None | Some "" -> (
    try
      let ic = Unix.open_process_in "git describe --tags --dirty 2>/dev/null" in
      let v = input_line ic in
      match Unix.close_process_in ic with Unix.WEXITED 0 -> Some v | _ -> None
    with _ -> None)
  | Some v -> Some v

let () =
  print_string "let v = ";
  print_string
    (match v with
    | None -> "None"
    | Some v -> "Some \"" ^ String.escaped v ^ "\"");
  print_endline ""
