(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime

module String = ExternalType(struct
    type t = string
    let name = "String"
    let equal _pos t1 t2 = String.equal t1 t2
    let compare _pos t1 t2 = String.compare t1 t2
    let print t = Printf.sprintf "%S" t
    let to_json t = Printf.sprintf "%S" t
    let from_json s = Scanf.sscanf s "%S" Fun.id
end)


(* Toplevel def loc *)
let loc : code_location array =
  [|{filename="tests/modules/good/string.catala_en";
     start_line=8; start_column=13; end_line=8; end_column=16;
     law_headings=["String types in Catala"]};
    {filename="tests/modules/good/string.catala_en";
     start_line=9; start_column=13; end_line=9; end_column=16;
     law_headings=["String types in Catala"]};
    {filename="tests/modules/good/string.catala_en";
     start_line=10; start_column=13; end_line=10; end_column=19;
     law_headings=["String types in Catala"]}|]

(* Toplevel def foo *)
let foo : String.t = "foo"

(* Toplevel def bar *)
let bar : String.t = "bar"

let fortytwo = "42"

(* Toplevel def of_int *)
let of_int : integer -> String.t = integer_to_string

let () =
  Catala_runtime.register_module "String"
    [ "foo", Stdlib.Obj.repr (foo);
      "bar", Stdlib.Obj.repr (bar);
      "fortytwo", Stdlib.Obj.repr (fortytwo);
      "of_int", Stdlib.Obj.repr (of_int) ]
    ~types:["String", (module String)]
    "*external*"
