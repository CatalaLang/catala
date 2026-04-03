(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime


module String = struct
  type t
  let rtype = Value.External {
    name = "String";
    equal = fun pos t1 t2 -> assert false;
    compare = fun pos t1 t2 -> assert false;
    to_json = Some (fun t -> "\"<String>\"");
    to_string = fun t -> "<String>";
  }
end


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
let foo : String.t =
  raise
  (Error (Impossible, [loc.(0)], None))

(* Toplevel def bar *)
let bar : String.t =
  raise
  (Error (Impossible, [loc.(1)], None))

(* Toplevel def of_int *)
let of_int : integer -> String.t =
  fun (_: integer) -> raise (Error (Impossible, [loc.(2)], None))

let () =
  Catala_runtime.register_module "String"
    [ "foo", Stdlib.Obj.repr (foo);
      "bar", Stdlib.Obj.repr (bar);
      "of_int", Stdlib.Obj.repr (of_int) ]
    "*external*"
