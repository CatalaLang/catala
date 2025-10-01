open Catala_runtime

(* Toplevel def sequence *)
let sequence : integer -> integer -> (integer array) =
  fun start stop ->
  let len = Z.sub stop start in
  if Z.sign len <> 1 then
    [||]
  else
    Array.init (Z.to_int len)
      (fun i -> Z.add start (Z.of_int i))

(* Toplevel def element *)
let element : ('t array) -> integer -> ('t) Optional.t =
  fun arr n ->
  let n = Z.to_int n - 1 in
  if 0 <= n && n < Array.length arr then Optional.Present arr.(n)
  else Optional.Absent ()

let () =
  Catala_runtime.register_module "List_internal"
    [ "sequence", Obj.repr sequence;
      "element", Obj.repr element ]
    "*external*"
