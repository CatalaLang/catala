let _ =
  Runtime_ocaml.Runtime.reset_log ();
  let _result = Scope_call3.whole_computation () in
  let log = Runtime_ocaml.Runtime.retrieve_log () in
  Format.printf "\nLog length: %d\n" (List.length log);
  let struct_log = Runtime_ocaml.Runtime.EventParser.parse_raw_events log in
  Runtime_ocaml.Runtime.pp_events Format.std_formatter struct_log;
  Format.printf "Finished!\n"
