module Ren = struct
  module Set = Set.Make (String)

  type ctxt = Set.t

  let skip_constant_binders = true
  let reset_context_for_closed_terms = true
  let constant_binder_name = None
  let empty_ctxt = Set.empty
  let reserve_name n s = Set.add n s
  let new_name n s = n, Set.add n s
end

module Ctx = Bindlib.Ctxt (Ren)

let fv b = Ren.Set.elements (Ctx.free_vars b)