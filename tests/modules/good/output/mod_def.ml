
(** This file has been generated by the Catala compiler, do not edit! *)

open Runtime_ocaml.Runtime

[@@@ocaml.warning "-4-26-27-32-41-42"]

module Enum1 = struct
 type t =
    | Yes of unit
    | No of unit
    | Maybe of unit
  end

module S = struct
  type t = {sr: money; e1: Enum1.t}
end

module Str1 = struct
  type t = {fld1: Enum1.t; fld2: integer}
end

module S_in = struct
  type t = unit
end


let s (s_in: S_in.t) : S.t =
  let sr_: money =
    try
      (handle_default
         [|{filename="tests/modules/good/mod_def.catala_en";
            start_line=26; start_column=24; end_line=26; end_column=30;
            law_headings=["Test modules + inclusions 1"]}|]
         ([|(fun (_: unit) ->
               handle_default [||] ([||]) (fun (_: unit) -> true)
                 (fun (_: unit) -> money_of_cents_string "100000"))|])
         (fun (_: unit) -> false) (fun (_: unit) -> raise Empty))
    with Empty ->
    (raise
    (Runtime_ocaml.Runtime.Error (NoValue, [{filename="tests/modules/good/mod_def.catala_en";
                                             start_line=16; start_column=10;
                                             end_line=16; end_column=12;
                                             law_headings=["Test modules + inclusions 1"]}])))
    in
  let e1_: Enum1.t =
    try
      (handle_default
         [|{filename="tests/modules/good/mod_def.catala_en";
            start_line=27; start_column=24; end_line=27; end_column=29;
            law_headings=["Test modules + inclusions 1"]}|]
         ([|(fun (_: unit) ->
               handle_default [||] ([||]) (fun (_: unit) -> true)
                 (fun (_: unit) -> Enum1.Maybe ()))|])
         (fun (_: unit) -> false) (fun (_: unit) -> raise Empty))
    with Empty ->
    (raise
    (Runtime_ocaml.Runtime.Error (NoValue, [{filename="tests/modules/good/mod_def.catala_en";
                                             start_line=17; start_column=10;
                                             end_line=17; end_column=12;
                                             law_headings=["Test modules + inclusions 1"]}])))
    in
  {S.sr = sr_; S.e1 = e1_}

let half_ : integer -> decimal =
  fun (x_: integer) ->
    o_div_int_int
      {filename="tests/modules/good/mod_def.catala_en";
       start_line=21; start_column=14; end_line=21; end_column=15;
       law_headings=["Test modules + inclusions 1"]} x_ (integer_of_string
      "2")

let () =
  Runtime_ocaml.Runtime.register_module "Mod_def"
    [ "S", Obj.repr s;
      "half", Obj.repr half_ ]
    "todo-module-hash"