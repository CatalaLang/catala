(* This is a template file following the expected interface and declarations to
 * implement the corresponding Catala module.
 *
 * You should replace all `raise (Error (Impossible))` place-holders with your
 * implementation and rename it to remove the ".template" suffix. *)

[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime

module M = Map.Make(Z)

module Dictionnary = ExternalType(struct
  type t = money M.t
  let name = "Dictionnary"
  let equal _ = M.equal (=)
  let compare pos = M.compare Stdlib.compare
  let print t = "<Text_dict>"
  let to_json t = Printf.sprintf "%S" (print t)
  let from_json pos t = assert false
end)

(* Toplevel def empty *)
let empty : Dictionnary.t =  M.empty

(* Toplevel def store *)
let store : Dictionnary.t -> integer -> money -> Dictionnary.t =
  fun (d: Dictionnary.t) (k: integer) (v: money) ->
  M.add k v d

(* Toplevel def find *)
let find : Dictionnary.t -> integer -> money Optional.t =
  fun (d: Dictionnary.t) (k: integer) ->
  match M.find_opt k d with
  | Some v -> Optional.Present v
  | None -> Optional.Absent

let () =
  Catala_runtime.register_module "Dictionnary_ext"
    [ "empty", Stdlib.Obj.repr empty;
      "store", Stdlib.Obj.repr store;
      "find", Stdlib.Obj.repr find ]
    "*external*"
