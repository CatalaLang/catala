(* Stdlib files embedded for web use. Uses jsoo's /static/ virtual filesystem
   which supports both file I/O and directory operations (Sys.readdir,
   Sys.is_directory). *)

open Js_of_ocaml

let stdlib_path = "/static/stdlib"

let register_stdlib () =
  List.iter
    (fun name ->
      match Stdlib_files.read name with
      | Some content ->
        let path = stdlib_path ^ "/" ^ name in
        Sys_js.create_file ~name:path ~content
      | None -> failwith ("Missing embedded stdlib file: " ^ name))
    Stdlib_files.file_list
