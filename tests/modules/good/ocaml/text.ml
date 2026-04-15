(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime

module Text = ExternalType(struct
    module String = Stdlib.String
    type t = string
    let name = "Text"
    let equal _pos t1 t2 = String.equal t1 t2
    let compare _pos t1 t2 = String.compare t1 t2
    let print t = Printf.sprintf "%S" t
    let to_json t =
      let buf = Buffer.create ((2 * String.length t) + 2) in
      Buffer.add_char buf '"';
      String.iter
        (function
          | ('"' | '\\') as c ->
            Buffer.add_char buf '\\';
            Buffer.add_char buf c
          | '\b' -> Buffer.add_string buf "\\b"
          | '\x0c' -> Buffer.add_string buf "\\f"
          | '\n' -> Buffer.add_string buf "\\n"
          | '\r' -> Buffer.add_string buf "\\r"
          | '\t' -> Buffer.add_string buf "\\t"
          | c -> Buffer.add_char buf c)
        t;
      Buffer.add_char buf '"';
      Buffer.contents buf
    let from_json pos s =
      let err() =
        raise (Error (AssertionFailed, [pos], Some "Invalid JSON input for type string"))
      in
      let len = String.length s in
      if len < 2 || s.[0] <> '"' || s.[len - 1] <> '"' then err();
      let buf = Buffer.create len in
      let rec iter i =
        if i < len - 1 then
          match s.[i] with
          | '\\' ->
            let i = i + 1 in
            if i >= len - 1 then err();
            (match s.[i] with
             | 'b' -> Buffer.add_char buf '\b'; iter (i+1)
             | 'f' -> Buffer.add_char buf '\x0c'; iter (i+1)
             | 'n' -> Buffer.add_char buf '\n'; iter (i+1)
             | 'r' -> Buffer.add_char buf '\r'; iter (i+1)
             | 't' -> Buffer.add_char buf '\t'; iter (i+1)
             | '"' | '/' | '\\' as c -> Buffer.add_char buf c; iter (i+1)
             | 'u' ->
               if i >= len - 5 then err ();
               let () =
                 match int_of_string_opt ("0x" ^ String.sub s (i+1) 4) with
                 | None -> err()
                 | Some n ->
                   try Buffer.add_utf_8_uchar buf (Uchar.of_int n)
                   with Invalid_argument _ -> err()
               in
               iter (i + 4)
             | _ -> err())
          | c -> Buffer.add_char buf c; iter (i+1)
      in
      iter 1;
      Buffer.contents buf
end)

(* Toplevel def loc *)
let loc : code_location array =
  [|{filename="tests/modules/good/text.catala_en";
     start_line=8; start_column=13; end_line=8; end_column=16;
     law_headings=["Text types in Catala"]};
    {filename="tests/modules/good/text.catala_en";
     start_line=9; start_column=13; end_line=9; end_column=16;
     law_headings=["Text types in Catala"]};
    {filename="tests/modules/good/text.catala_en";
     start_line=10; start_column=13; end_line=10; end_column=19;
     law_headings=["Text types in Catala"]}|]

(* Toplevel def foo *)
let foo : Text.t = "foo\\"

(* Toplevel def bar *)
let bar : Text.t = "bąr"

let fortytwo = "42"

(* Toplevel def of_int *)
let of_int : integer -> Text.t = integer_to_string

let () =
  Catala_runtime.register_module "Text"
    [ "foo", Stdlib.Obj.repr (foo);
      "bar", Stdlib.Obj.repr (bar);
      "fortytwo", Stdlib.Obj.repr (fortytwo);
      "of_int", Stdlib.Obj.repr (of_int) ]
    ~types:["Text", (module Text)]
    "*external*"
