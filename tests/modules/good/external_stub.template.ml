(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

open Runtime_ocaml.Runtime

[@@@ocaml.warning "-4-26-27-32-41-42"]


module Str = struct
  type t = {fld: integer}
end

module En = struct type t = Str of Str.t | Empty of unit end


(* Toplevel def str_with_default *)
let str_with_default : En.t -> Str.t =
  fun (_: En.t) -> raise
    (Runtime_ocaml.Runtime.Error (Impossible, [{filename="tests/modules/good/external_stub.catala_en";
                                                start_line=11; start_column=13;
                                                end_line=11; end_column=29;
                                                law_headings=[]}]))

let () =
  Runtime_ocaml.Runtime.register_module "External_stub"
    [ "str_with_default", Obj.repr str_with_default ]
    "*external*"
