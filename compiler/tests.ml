module Path = Catala_utils.Path
module Common = Clerk_backends.Common
module Backend_paths = Clerk_backends.Backend_paths

let check = Alcotest.(check string)
let check_list = Alcotest.(check (list string))

(* Runs [f] with the target OS forced, so Windows behaviour is exercised on the
   Linux CI — where the reftests, running with '/', cannot catch it. Restores
   [Path.win32] afterwards so tests don't leak the setting to one another. *)
let with_os win32 f =
  let saved = !Path.win32 in
  Path.win32 := win32;
  Fun.protect ~finally:(fun () -> Path.win32 := saved) f

(* [include_flags]/[classpath] join with [Filename.concat], so on Windows a
   joined separator is '\'. These tests check quoting and the path separator, not
   slash direction; normalise '\' -> '/' first (no-op on Linux). *)
let fwd s = String.map (function '\\' -> '/' | c -> c) s

(* VS Code launches clerk with a lower-case drive cwd ("c:\\...") while the fs
   drive is upper-case; a case-sensitive prefix compare then failed to
   relativize, corrupting include_dirs into drive-stripped absolutes. *)

let test_remove_prefix_drive_case () =
  with_os true
  @@ fun () ->
  check "lower-case-drive prefix removed from upper-case-drive path"
    {|lib\src\data|}
    (Path.remove_prefix ~cwd:{|c:\proj|} {|c:\proj|}
       {|C:\proj\lib\src\data|})

let test_reverse_path_no_drive_strip () =
  with_os true
  @@ fun () ->
  check "include-dir relativized to project root (drive not stripped)"
    {|lib\src\data\enums|}
    (Path.reverse ~cwd:{|c:\proj|} ~from_dir:{|c:\proj|}
       ~to_dir:{|.|} {|C:\proj\lib\src\data\enums|})

let test_make_relative_to_drive_case () =
  with_os true
  @@ fun () ->
  check "make_relative_to across a drive-case mismatch" {|b\c|}
    (Path.make_relative_to ~cwd:{|c:\proj|} ~dir:{|C:\proj\a|} {|C:\proj\a\b\c|})

(* A drive path needs a leading slash before "C:" (else "C:" is the URL
   authority); a UNC path's server IS the authority, so no extra leading slash. *)

let test_file_url_drive () =
  with_os true
  @@ fun () ->
  check "windows drive path -> /C:/..." "/C:/proj/file.catala_en"
    (Path.url_of_absolute {|C:\proj\file.catala_en|})

let test_file_url_unc () =
  with_os true
  @@ fun () ->
  check "windows UNC path -> server/share/... (server is the authority)"
    "server/share/dir/file.catala_en"
    (Path.url_of_absolute {|\\server\share\dir\file.catala_en|})

(* Ninja un-escapes its file-syntax quoting before running the command, so an
   include dir with a space (C:\Program Files\...) must ALSO be shell-quoted or
   the compiler's argv parser word-splits it. *)

let test_include_flags_quote_absolute () =
  check_list "include_flags: each -I dir is shell-quoted"
    ["-I"; {|"${tdir}/ocaml"|}; "-I"; {|"/opt/some dir/ocaml"|}]
    (List.map fwd
       (Common.Flags.include_flags ~backend:"ocaml" [{|/opt/some dir|}]))

(* The separator differs by OS; the reftests only run on Linux, so the Windows
   case needs a unit test. *)

let test_classpath_separator () =
  with_os true (fun () ->
      check "Java classpath: ';' on Windows" {|${tdir}/java;/opt/lib a/java|}
        (fwd (Backend_paths.classpath ~backend:"java" [{|/opt/lib a|}])));
  with_os false (fun () ->
      check "Java classpath: ':' on Unix" {|${tdir}/java:/opt/lib a/java|}
        (fwd (Backend_paths.classpath ~backend:"java" [{|/opt/lib a|}])))

let test_pythonpath_separator () =
  with_os true (fun () ->
      check "PYTHONPATH: ';' on Windows" {|C:/build/python;C:/proj/tests|}
        (Backend_paths.pythonpath [{|C:/build/python|}; {|C:/proj/tests|}]));
  with_os false (fun () ->
      check "PYTHONPATH: ':' on Unix" {|/build/python:/proj/tests|}
        (Backend_paths.pythonpath [{|/build/python|}; {|/proj/tests|}]))

(* A position literal embeds the source filename; on Windows its backslashes are
   an illegal escape (Java/Python) or silently wrong (C/OCaml) in the target
   string literal unless [format_pos] escapes them. *)

let contains ~sub s =
  let n = String.length sub and m = String.length s in
  let rec go i = i + n <= m && (String.sub s i n = sub || go (i + 1)) in
  n = 0 || go 0

let pos_escaped fmt_pos =
  let pos = Catala_utils.Pos.from_info {|C:\proj\mod.catala_fr|} 1 2 3 4 in
  contains ~sub:{|C:\\proj\\mod.catala_fr|} (Format.asprintf "%a" fmt_pos pos)

let backends_format_pos =
  [
    "Java", Scalc.To_java.format_pos;
    "Python", Scalc.To_python.format_pos;
    "C", Scalc.To_c.format_pos;
    "OCaml", Lcalc.To_ocaml.format_pos;
  ]

(* Thousands of '-C <dir> <file>' pairs overflow the Windows command-line limit,
   so clerk spills them to a jar argfile; backslash escapes there, so Windows
   paths must be forward-slashed and quoted for spaces. *)

let test_jar_argfile_escaping () =
  check "jar argfile forward-slashes and quotes Windows paths"
    {|-C
"C:/Program Files/build/app/java"
"Outer$Inner.class"|}
    (Backend_paths.jar_argfile_content
       [{|C:\Program Files\build\app\java|}, {|Outer$Inner.class|}])

let () =
  let open Alcotest in
  run "Catala-utils"
    [
      ( "Iota-reduction",
        [
          test_case "#1" `Quick Shared_ast.Optimizations.test_iota_reduction_1;
          test_case "#2" `Quick Shared_ast.Optimizations.test_iota_reduction_2;
        ] );
      ( "File paths (Windows drive-case)",
        [
          test_case "remove_prefix drive-case" `Quick
            test_remove_prefix_drive_case;
          test_case "reverse_path no drive strip" `Quick
            test_reverse_path_no_drive_strip;
          test_case "make_relative_to drive-case" `Quick
            test_make_relative_to_drive_case;
        ] );
      ( "File URLs (Windows drive + UNC)",
        [
          test_case "file_url drive path" `Quick test_file_url_drive;
          test_case "file_url UNC path" `Quick test_file_url_unc;
        ] );
      ( "Clerk include-dir quoting (spaces in install dir)",
        [
          test_case "include_flags shell-quotes each -I dir" `Quick
            test_include_flags_quote_absolute;
        ] );
      ( "Backend path separators (Windows drive-colon)",
        [
          test_case "classpath separator" `Quick test_classpath_separator;
          test_case "PYTHONPATH separator" `Quick test_pythonpath_separator;
        ] );
      ( "Backend position-filename escaping (Windows backslash)",
        List.map
          (fun (name, fmt_pos) ->
            test_case (name ^ " escapes backslashes") `Quick (fun () ->
                Alcotest.(check bool) name true (pos_escaped fmt_pos)))
          backends_format_pos );
      ( "Java jar @argfile (command-line length + path escaping)",
        [
          test_case "argfile escapes a Windows path" `Quick
            test_jar_argfile_escaping;
        ] );
    ]
