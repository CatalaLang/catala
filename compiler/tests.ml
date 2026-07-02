module File = Catala_utils.File

let check = Alcotest.(check string)

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
        ] );
    ]
