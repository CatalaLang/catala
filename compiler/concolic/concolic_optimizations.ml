open Path_constraint.PathConstraint

type flag = OTrivial

let optim_list = ["trivial", OTrivial]
let trivial : flag list -> bool = List.mem OTrivial

let remove_trivial_constraints opt (pcs : naked_path) : naked_path =
  if not (trivial opt) then pcs
  else begin
    let f pc =
      match pc.expr with Pc_z3 s -> not (Z3.Boolean.is_true s) | _ -> true
    in
    List.filter f pcs
  end
