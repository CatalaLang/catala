module Path = Catala_utils.Path
module Common = Clerk_backend
module Backend_paths = Clerk_backend.Backend_paths

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
    (Path.remove_prefix ~cwd:{|c:\proj|} {|c:\proj|} {|C:\proj\lib\src\data|})

let test_reverse_path_no_drive_strip () =
  with_os true
  @@ fun () ->
  check "include-dir relativized to project root (drive not stripped)"
    {|lib\src\data\enums|}
    (Path.reverse ~cwd:{|c:\proj|} ~from_dir:{|c:\proj|} ~to_dir:{|.|}
       {|C:\proj\lib\src\data\enums|})

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

(* The override border guards must fail loudly on any platform, not just
   spaced-dir Windows: stored quote chars double-quote at emission, refs
   would expand in direct exec but quote-glue at emission. (The old
   check_path property — no vector ref in a path — is now static:
   Var.ref only accepts scalars, and emission rejects Splice in paths.) *)

module CVar = Clerk_utils.Var

let raises_compiler_error f =
  try
    ignore (f ());
    false
  with Catala_utils.Message.CompilerError _ -> true

let test_override_rejects_quote () =
  Alcotest.(check bool)
    "quote char in an override value is rejected" true
    (raises_compiler_error (fun () ->
         CVar.binding_of_words_override CVar.catala_flags [{|"boom"|}]))

let test_override_rejects_ref () =
  Alcotest.(check bool)
    "variable reference in an override value is rejected" true
    (raises_compiler_error (fun () ->
         CVar.binding_of_words_override CVar.catala_flags ["${builddir}/x"]))

let test_override_accepts_clean () =
  check_list "clean vector override passes through" ["-O"; "--trace"]
    (CVar.binding_to_words
       (CVar.binding_of_words_override CVar.catala_flags ["-O"; "--trace"]))

(* the CLI splits override values on spaces before kinds are known, so a
   scalar must rejoin them: a spaced path is one value, not two words *)
let test_override_rejoins_spaced_scalar () =
  check_list "spaced scalar value is kept whole" ["/a b/catala"]
    (CVar.binding_to_words
       (CVar.binding_of_words_override CVar.catala_exe ["/a"; "b/catala"]))

let test_override_accepts_single_scalar () =
  check_list "single-word scalar value passes through" ["catala.exe"]
    (CVar.binding_to_words
       (CVar.binding_of_words_override CVar.catala_exe ["catala.exe"]))

(* include_flags feeds rule-scoped ninja bindings spliced into compile
   commands, which the shell re-parses: each dir must stay a single Word so
   that a spaced path (C:\Program Files\...) is quoted as one shell word at
   emission. *)

let expr_words e =
  List.map
    (function
      | Ninja_utils.Expr.Word w -> w
      | Ninja_utils.Expr.Splice v -> "${" ^ CVar.name v ^ "}"
      | Ninja_utils.Expr.Raw s -> s)
    e

let test_include_flags_single_words () =
  check_list "include_flags: one Word per flag and per dir"
    ["-I"; "${tdir}/ocaml"; "-I"; "/opt/some dir/ocaml"]
    (List.map fwd
       (expr_words
          (Common.Flags.include_flags ~name:"ocaml" [{|/opt/some dir|}])))

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

(* javac decodes source with the platform encoding (cp1252 on Windows), so a
   generated Java string literal must be pure ASCII. [\uXXXX] is translated back
   before lexing, so the value is unchanged; above the BMP Java wants a UTF-16
   surrogate pair. [format_pos] is the seam: it embeds the source path. *)

let test_java_literal_ascii_only () =
  let pos = Catala_utils.Pos.from_info {|C:\Impôts\🎯\x.catala_fr|} 1 2 3 4 in
  let s = Format.asprintf "%a" Scalc.To_java.format_pos pos in
  Alcotest.(check bool)
    "literal is pure ASCII" true
    (String.for_all (fun c -> Char.code c < 0x80) s);
  Alcotest.(check bool) "BMP char escaped" true (contains ~sub:{|\u00f4|} s);
  Alcotest.(check bool)
    "astral char becomes a surrogate pair" true
    (contains ~sub:{|\ud83c\udfaf|} s)

(* Same hazard in the trace the runtime emits: an unescaped backslash makes the
   JSON unparseable, so the trace viewer rejects every Windows trace. *)

let test_trace_json_escaping () =
  let pos =
    Catala_runtime.
      {
        filename = {|baremes\tests\N007.catala_fr|};
        start_line = 1;
        start_column = 2;
        end_line = 3;
        end_column = 4;
        law_headings = [];
      }
  in
  let json =
    Catala_runtime.Json.trace
      [{ kind = BranchingCondition; pos; value = None; sub_trace = [] }]
  in
  Alcotest.(check bool)
    "trace escapes backslashes in the position filename" true
    (contains ~sub:{|"file":"baremes\\tests\\N007.catala_fr"|} json)

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

(* Only emitted under [Sys.win32], out of reach of the Linux testsuite. *)

let test_file_list_roundtrip () =
  let files =
    [
      {|${builddir}\a@test|};
      {|C:\spaced dir\DPC_+_6, é & co@test|};
      {|C:\Résidence, nationalité\t=1;2^3%PATH%@test|};
    ]
  in
  check_list "dir-tests file list round-trips" files
    (CVar.file_list_of_string (CVar.file_list_to_string files))

(* [File.Tree.lookup] against its rule, spelled out over real folders: [to_id]
   and case-insensitive, plain files only, left folder wins on a same name,
   several matches are an error *)
let test_tree_lookup_matches_reference () =
  let module File = Catala_utils.File in
  (* not [Filename.temp_dir]: needs OCaml 5.1 *)
  let root = Filename.temp_file "catala-tree" "" in
  Sys.remove root;
  Sys.mkdir root 0o755;
  let ( / ) = Filename.concat in
  let files =
    [
      "a" / "Foo.catala_en";
      "a" / "Bär_baz.catala_fr";
      "a" / "Dup.catala_en";
      "a" / "dûp.catala_en";
      "a" / "Same.catala_en";
      "a" / "Cross.catala_en";
      "a" / "sub" / "Inner.catala_en";
      "b" / "Same.catala_en";
      "b" / "cröss.catala_en";
    ]
  in
  List.iter (fun d -> Sys.mkdir (root / d) 0o755) ["a"; "a" / "sub"; "b"];
  List.iter (fun f -> close_out (open_out (root / f))) files;
  Fun.protect ~finally:(fun () ->
      List.iter (fun f -> Sys.remove (root / f)) files;
      List.iter (fun d -> Sys.rmdir (root / d)) ["a" / "sub"; "a"; "b"; ""])
  @@ fun () ->
  let dirs = [root / "a"; root / "b"] in
  let id s = String.lowercase_ascii (Catala_utils.String.to_id s) in
  let reference query =
    let merged =
      List.fold_left
        (fun acc dir ->
          Array.fold_left
            (fun acc f ->
              if List.exists (fun (g, _) -> File.equal f g) acc then acc
              else acc @ [f, dir / f])
            acc (Sys.readdir dir))
        [] dirs
    in
    let entries, name =
      match String.split_on_char '/' query with
      | [sub; name] -> (
        match List.find_opt (fun (g, _) -> File.equal g sub) merged with
        | Some (_, p) when Sys.is_directory p ->
          Array.to_list (Sys.readdir p) |> List.map (fun f -> f, p / f), name
        | _ -> [], name)
      | _ -> merged, query
    in
    List.filter
      (fun (f, p) -> (not (Sys.is_directory p)) && id f = id name)
      entries
    |> function [] -> "none" | [(f, _)] -> f | _ -> "ambiguous"
  in
  let tree =
    File.Tree.union
      (File.Tree.build (root / "a"))
      (File.Tree.build (root / "b"))
  in
  let actual query =
    match File.Tree.lookup tree query with
    | Some p -> Filename.basename p
    | None -> "none"
    | exception Catala_utils.Message.CompilerError _ -> "ambiguous"
  in
  let queries =
    [
      "Foo.catala_en";
      "foo.catala_en";
      "Bar_baz.catala_fr";
      "BAR_BAZ.catala_fr";
      "Dup.catala_en";
      "Missing.catala_en";
      "sub";
      "Inner.catala_en";
      "sub/Inner.catala_en";
      "Same.catala_en";
      "Cross.catala_en";
    ]
  in
  (* twice: the second round uses the built index *)
  List.iter (fun q -> check q (reference q) (actual q)) (queries @ queries);
  check "accented file found from its ASCII name" "Bär_baz.catala_fr"
    (actual "Bar_baz.catala_fr");
  check "same name in two folders: left wins"
    (root / "a" / "Same.catala_en")
    (Option.get (File.Tree.lookup tree "Same.catala_en"));
  check "different names, same id, across folders: an error" "ambiguous"
    (actual "Cross.catala_en");
  check "path through a subfolder" "Inner.catala_en"
    (actual "sub/Inner.catala_en")

let () =
  let open Alcotest in
  run "Unit tests"
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
      ( "File trees",
        [
          test_case "lookup matches the reference rule" `Quick
            test_tree_lookup_matches_reference;
        ] );
      ( "File URLs (Windows drive + UNC)",
        [
          test_case "file_url drive path" `Quick test_file_url_drive;
          test_case "file_url UNC path" `Quick test_file_url_unc;
        ] );
      ( "Clerk override border guards",
        [
          test_case "override rejects quote char" `Quick
            test_override_rejects_quote;
          test_case "override rejects variable ref" `Quick
            test_override_rejects_ref;
          test_case "override passes clean vector words" `Quick
            test_override_accepts_clean;
          test_case "override rejoins spaced scalar" `Quick
            test_override_rejoins_spaced_scalar;
          test_case "override passes single-word scalar" `Quick
            test_override_accepts_single_scalar;
        ] );
      ( "Clerk include-dir quoting (spaces in install dir)",
        [
          test_case "include_flags keeps each -I dir a single word" `Quick
            test_include_flags_single_words;
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
      ( "Java source encoding (non-ASCII in generated literals)",
        [
          test_case "position literal is pure ASCII" `Quick
            test_java_literal_ascii_only;
        ] );
      ( "Runtime trace JSON escaping (Windows backslash)",
        [
          test_case "trace escapes a Windows path" `Quick
            test_trace_json_escaping;
        ] );
      ( "Java jar @argfile (command-line length + path escaping)",
        [
          test_case "argfile escapes a Windows path" `Quick
            test_jar_argfile_escaping;
        ] );
      ( "Windows test-report file list",
        [test_case "round-trips awkward paths" `Quick test_file_list_roundtrip]
      );
    ]
