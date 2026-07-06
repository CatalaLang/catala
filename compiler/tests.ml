module File = Catala_utils.File
module Message = Catala_utils.Message
module Common = Clerk_backends.Common

let check = Alcotest.(check string)
let check_list = Alcotest.(check (list string))

(* These emulate Windows path handling on any host through the [~win32] flag, so
   they run in the Linux CI. The scenario is VS Code launching clerk: the cwd
   carries a LOWER-case drive letter ("c:\\...", from [Uri.fsPath]) while the
   filesystem's canonical drive is UPPER case ("C:\\..."). The case-sensitive
   prefix compare that [File.Path.remove_prefix] used to do then failed to
   relativize, corrupting clerk's include_dirs into drive-stripped absolutes. *)

let test_remove_prefix_drive_case () =
  check "lower-case-drive prefix still removed from upper-case-drive path"
    {|lib\src\data|}
    (File.Path.remove_prefix ~win32:true ~cwd:{|c:\proj|}
       {|c:\proj|} {|C:\proj\lib\src\data|})

let test_reverse_path_no_drive_strip () =
  (* The exact clerk include_dir case: from a lower-case-drive cwd an
     upper-case-drive project dir must relativize to a clean relative path, not
     a drive-stripped absolute ("\\proj\\..."). *)
  check "include-dir relativized to project root (drive not stripped)"
    {|lib\src\data\enums|}
    (File.Path.reverse_path ~win32:true ~cwd:{|c:\proj|}
       ~from_dir:{|c:\proj|} ~to_dir:{|.|}
       {|C:\proj\lib\src\data\enums|})

let test_remove_prefix_matching_case () =
  (* No change when the drive cases already agree. *)
  check "matching-case prefix removal"
    {|lib\src|}
    (File.Path.remove_prefix ~win32:true ~cwd:{|C:\proj|} {|C:\proj|}
       {|C:\proj\lib\src|})

let test_reverse_path_unix () =
  (* Non-Windows behaviour is unaffected. *)
  check "unix relativization"
    "a/b"
    (File.Path.reverse_path ~win32:false ~cwd:"/home/x" ~from_dir:"/home/x"
       ~to_dir:"." "/home/x/a/b")

let test_make_relative_to_drive_case () =
  (* Same lower/upper drive mismatch, through make_relative_to. *)
  check "make_relative_to across a drive-case mismatch"
    {|b\c|}
    (File.Path.make_relative_to ~win32:true ~cwd:{|c:\proj|} ~dir:{|C:\proj\a|}
       {|C:\proj\a\b\c|})

(* file:// URL construction (Message.url_path_of_absolute). The clickable-link
   fix: a drive path needs a leading slash before "C:" (else it is read as the
   URL authority), and a UNC path's server IS the authority so it must keep
   exactly two slashes total after "file:" — i.e. no extra leading slash. *)

let test_file_url_drive () =
  check "windows drive path -> /C:/..."
    "/C:/proj/file.catala_en"
    (Message.url_path_of_absolute ~win32:true {|C:\proj\file.catala_en|})

let test_file_url_unc () =
  check "windows UNC path -> server/share/... (server is the authority)"
    "server/share/dir/file.catala_en"
    (Message.url_path_of_absolute ~win32:true
       {|\\server\share\dir\file.catala_en|})

let test_file_url_unix () =
  check "unix path is already URL-shaped"
    "/home/x/file.catala_en"
    (Message.url_path_of_absolute ~win32:false "/home/x/file.catala_en")

(* Clerk's OCaml/C backends emit '-I <dir>' flags into a ninja variable later
   expanded onto the compiler command line. Ninja's file-syntax escaping ($ , $:)
   is UN-escaped before the command runs, so an absolute include dir containing a
   space (e.g. C:\Program Files\Catala\toolchain\lib\zarith) must ALSO be
   shell-quoted or the compiler's argv parser word-splits it ("Don't know what to
   do with Files\Catala..."). These pin the shell-quoting. Space-free paths are
   byte-identical after unquoting, so Linux is unaffected. *)

let test_includes_quote_absolute () =
  check_list "includes: absolute -I dir is shell-quoted"
    ["-I"; {|"/opt/some dir/zarith"|}]
    (Common.Flags.includes [{|/opt/some dir/zarith|}])

let test_include_flags_quote_absolute () =
  check_list "include_flags: each -I dir is shell-quoted"
    ["-I"; {|"${tdir}/ocaml"|}; "-I"; {|"/opt/some dir/ocaml"|}]
    (Common.Flags.include_flags ~backend:"ocaml" [{|/opt/some dir|}])

let test_c_backend_runtime_include_quoted () =
  (* The C backend emits its own runtime include ("-I ${builddir}/libcatala/c")
     directly (c.ml), not via the shared helper, so it needs its own check. Drive
     the public Backend.Flags.default and grab the C_INCLUDE_FLAGS binding (the
     only one whose value starts with "-I" when include_dirs is empty). *)
  let bindings =
    Clerk_backends.C.Backend.Flags.default ~variables:[] ~autotest:false
      ~use_default_flags:true ~test_flags:[] ~include_dirs:[]
  in
  let c_include =
    Option.get
      (List.find_map
         (fun (_v, value) ->
           match value with "-I" :: _ -> Some value | _ -> None)
         bindings)
  in
  check_list "C backend runtime -I is shell-quoted"
    ["-I"; {|"${builddir}/libcatala/c"|}]
    c_include

let () =
  let open Alcotest in
  run "Catala-utils"
    [
      ( "Iota-reduction",
        [
          test_case "#1" `Quick Shared_ast.Optimizations.test_iota_reduction_1;
          test_case "#2" `Quick Shared_ast.Optimizations.test_iota_reduction_2;
        ] );
      ( "File-paths (Windows drive-case)",
        [
          test_case "remove_prefix drive-case" `Quick
            test_remove_prefix_drive_case;
          test_case "reverse_path no drive strip" `Quick
            test_reverse_path_no_drive_strip;
          test_case "remove_prefix matching case" `Quick
            test_remove_prefix_matching_case;
          test_case "reverse_path unix" `Quick test_reverse_path_unix;
          test_case "make_relative_to drive-case" `Quick
            test_make_relative_to_drive_case;
        ] );
      ( "File URLs (Windows drive + UNC)",
        [
          test_case "file_url drive path" `Quick test_file_url_drive;
          test_case "file_url UNC path" `Quick test_file_url_unc;
          test_case "file_url unix path" `Quick test_file_url_unix;
        ] );
      ( "Clerk include-dir quoting (spaces in install dir)",
        [
          test_case "includes quotes absolute dir" `Quick
            test_includes_quote_absolute;
          test_case "include_flags quotes dirs" `Quick
            test_include_flags_quote_absolute;
          test_case "C backend runtime -I quoted" `Quick
            test_c_backend_runtime_include_quoted;
        ] );
    ]
