open Path_constraint.PathConstraint

type flag = OTrivial | OLazyDefault

let optim_list = ["trivial", OTrivial; "lazy-default", OLazyDefault]
let trivial : flag list -> bool = List.mem OTrivial
let lazy_default : flag list -> bool = List.mem OLazyDefault

let remove_trivial_constraints opt (pcs : naked_path) : naked_path =
  if not (trivial opt) then pcs
  else begin
    let f pc =
      match pc.expr with Pc_z3 s -> not (Z3.Boolean.is_true s) | _ -> true
    in
    List.filter f pcs
  end
