let () =
  let open Re in
  if Array.length Sys.argv <> 2 then
    failwith "expected either ar or gmp_flags as argument"
  else
    match Sys.argv.(1) with
    | "ar" ->
      (* On mingw's opam windows install gcc is named: x86_64-w64-mingw32-gcc,
         and, ar is named: x86_64-w64-mingw32-ar. We substitute gcc by ar from
         the c compiler retrieved in the OCaml compiler's config to make the
         build system handle it properly. *)
      let r = compile (seq [str "gcc"; opt (str ".exe"); eol]) in
      if (Sys.win32 || Sys.cygwin) && Re.execp r Config.c_compiler then
        Config.c_compiler |> replace_string r ~by:"ar" |> print_string
      else print_string "ar"
    | "gmp_flags" ->
      if Sys.win32 || Sys.cygwin then
        (* On cygwin/windows, pkg-config is not aware of gmp but opam is aware
           of the include dir. *)
        ()
      else
        let _ = Sys.command "pkg-config --cflags gmp" in
        (* If pkg-config fails to find gmp's header location, let it fail and
           print its message on stderr then try without any flag. *)
        exit 0
    | s -> Format.ksprintf failwith "unknown command: %s" s
