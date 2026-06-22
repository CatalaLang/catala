[@@@ocaml.warning "-4-26-27-32-33-34-37-41-42-69"]

open Catala_runtime

module Mod_def = Mod_def
module Text = Text

module M = Map.Make(struct
    type t = Text.Text.t
    let compare a b = Value.compare ({filename=__FILE__; start_line=0; start_column=0;end_line=0;end_column=0; law_headings=[]})
        (Value.embed Text.Text.rtype a)
        (Value.embed Text.Text.rtype b)
  end)

module Text_dict = ExternalType(struct
  type t = Mod_def.Mod_def.t M.t
  let name = "Text_dict"
  let equal pos = M.equal (equal Mod_def.Mod_def.rtype pos)
  let compare pos = M.compare (compare Mod_def.Mod_def.rtype pos)
  let print t = "<Text_dict>"
  let to_json t = Printf.sprintf "%S" (print t)
  let from_json pos t = assert false
end)

(* Toplevel def empty *)
let empty : Text_dict.t = M.empty

(* Toplevel def store *)
let store : Text_dict.t -> Text.Text.t -> Mod_def.Mod_def.t -> Text_dict.t =
  fun (d: Text_dict.t) (k: Text.Text.t) (v: Mod_def.Mod_def.t) ->
  M.add k v d

(* Toplevel def find *)
let find : Text_dict.t -> Text.Text.t -> Mod_def.Mod_def.t Optional.t =
  fun (d: Text_dict.t) (k: Text.Text.t) ->
  match M.find_opt k d with
  | Some v -> Optional.Present v
  | None -> Optional.Absent

let () =
  Catala_runtime.register_module "Text_dict"
    [ "empty", Stdlib.Obj.repr empty;
      "store", Stdlib.Obj.repr store;
      "find", Stdlib.Obj.repr find ]
    "*external*"
