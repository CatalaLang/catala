(** This trivial binary is run at build-time to get the correct version from the
    build environment (either the CATALA_VERSION) environment variable if
    defined, or `git describe`, or resorting to just "dev" if none of these can
    be found *)

let v =
  match Sys.getenv_opt "CATALA_VERSION" with
  | None | Some "" -> (
    let ic = Unix.open_process_in "git describe --tags --dirty 2>/dev/null" in
    let v = try input_line ic with _ -> "dev" in
    match Unix.close_process_in ic with Unix.WEXITED 0 -> v | _ -> "dev")
  | Some v -> v

let () =
  print_string "let v = \"";
  print_string (String.escaped v);
  print_endline "\""
