open Catala_runtime

(* Toplevel def sequence *)
let sequence : integer -> integer -> integer array =
 fun start stop ->
  let len = Z.sub stop start in
  if Z.sign len <> 1 then [||]
  else Array.init (Z.to_int len) (fun i -> Z.add start (Z.of_int i))

(* Toplevel def element *)
let nth_element : 't array -> integer -> 't Optional.t =
 fun arr n ->
  let n = Z.to_int n - 1 in
  if 0 <= n && n < Array.length arr then Optional.Present arr.(n)
  else Optional.Absent

(* Toplevel def remove_nth_element *)
let remove_nth_element : 't array -> integer -> 't array =
 fun arr n ->
  let n = Z.to_int n - 1 in
  let len = Array.length arr in
  if n < 0 || len <= n then arr
  else
    let ret = Array.make (Array.length arr - 1) arr.(0) in
    if n > 0 then Array.blit arr 0 ret 0 n;
    if n < len - 1 then Array.blit arr (n + 1) ret n (len - n - 1);
    ret

(* Toplevel def reverse *)
let reverse : 't array -> 't array =
 fun arr ->
  let len = Array.length arr in
  if len <= 0 then arr
  else
    let ret = Array.make len arr.(len - 1) in
    for i = 1 to len - 1 do
      ret.(i) <- arr.(len - 1 - i)
    done;
    ret

let () =
  Catala_runtime.register_module "List_internal"
    [
      "sequence", Obj.repr sequence;
      "nth_element", Obj.repr nth_element;
      "remove_nth_element", Obj.repr remove_nth_element;
      "reverse", Obj.repr reverse;
    ]
    "*external*"
